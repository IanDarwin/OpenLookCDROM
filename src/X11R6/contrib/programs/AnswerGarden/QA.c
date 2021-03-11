/**********************************************************

  The ANSWER GARDEN PROJECT

  QA.c   Code for the QA-Node node type

 -- Copyright (c) 1994 Regents of the University of California.
  -- All rights reserved.
  --
  -- This software was developed by the Answer Garden project
  -- at the University of California, Irvine.
  --
  -- Redistribution and use in source and binary forms are permitted
  -- provided that the above copyright notice and this paragraph are
  -- duplicated in all such forms and that any documentation,
  -- advertising materials, and other materials related to such
  -- distribution and use acknowledge that the software was developed
  -- by the University of California, Irvine.  The name of the
  -- University may not be used to endorse or promote products derived
  -- from this software without specific prior written permission.
  -- THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
  -- IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
  -- WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
  
  -- Answer Garden is a trademark of the Regents of the University of
  -- California.  All rights reserved.

  Mark Ackerman
  Information and Computer Science
  University of California, Irvine

  formerly -
  MIT/Center for Coordination Science

  ackerman@ics.uci.edu

**********************************************************************/

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Viewport.h>

#include <ctype.h>
#include <stdio.h>
#include "Layout.h"
#include "AG.h"


#define QAIndexColumns 1
#define QABodyColumns  80
#define QAVertLeading  2
#define QAHorizMargin  6
#define MaxQAIndexCharWidth 3
extern UserSettings user_settings;
extern HistoryNode *history_list;
extern HistoryNode *current_history_node;


  /**** need node name ***/
typedef struct
{
  char *filename; /* could remove */
  char *index_buffer;
  char *body_buffer;
  NodeInfoPtr node_info;
} QAInfo;

#define EndLine '\n'

extern GlobalInfo global_info;

static QAInfo *QA_Initialize();
static void QA_Destroy();
static Widget QA_Open();
static void QA_Put_Error();

#ifdef OLDAPI
Widget Create_QA(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
#else
Widget Create_QA(w,node_info,geometry,misc_info,num_info)
     Widget w;
     NodeInfoPtr node_info;
     String geometry;
     String *misc_info;
     int num_info;
#endif

{
    char filestring[MaxString];
    char *file_body_buffer;
    QAInfo *qa_info;
    int n_lines;
    char msg[MaxString];
#ifdef OLDAPI
    NodeInfoPtr node_info = (NodeInfoPtr) client_data;
#endif
    char *filename;
    Widget shell;


    filename = NodeService_Get_Location (node_info);
    if (filename == NULL)
      {
	 QA_Put_Error(w,
		      "null name passed to discussion node.  Continuing...");
	 return(NULL);
      }

    Form_Filename(filestring,filename);
    if (! NodeService_Open_File(node_info,filestring))
      {
	 sprintf(msg,"error in discussion file %s.  Continuing...",filename);
	 QA_Put_Error(w,msg);
	 NodeService_Close_File(node_info);
	 return (NULL);
      }

       
    file_body_buffer = NodeService_Get_Text(node_info);
    if (file_body_buffer)
      qa_info = QA_Initialize(file_body_buffer,&n_lines);

    qa_info->node_info = node_info;
    qa_info->filename = filename;

    if (!file_body_buffer || qa_info == NULL)
      {
	QA_Put_Error(w,filename);
	return(NULL);
      }
    else
      {
#ifdef OLDAPI
	shell = QA_Open(qa_info,n_lines,call_data);
#else
	shell = QA_Open(qa_info,n_lines,geometry);
#endif
	NodeService_Free_Buffer(node_info);
	return(shell);
      }
  }


#define NDelimiters  4
static char startDelimiters[NDelimiters] = {'{','[','(','<'};
static char endDelimiters[NDelimiters] = {'}',']',')','>'};
static char *begin_string = "begin";
static char *end_string = "end";

static QAInfo *QA_New()
{
  QAInfo *qa_info;
  return( (QAInfo *) XtMalloc(sizeof(QAInfo)));
}
    
static void QA_Destroy(qa_info)
  QAInfo *qa_info;
{
  if (!qa_info)
    return;
  NodeService_Close_File(qa_info->node_info);
  if (qa_info->body_buffer)
    XtFree((char *)qa_info->body_buffer);
  if (qa_info->index_buffer)
    XtFree((char *)qa_info->index_buffer);
  XtFree((char *)qa_info);
}
    
static void QA_Put_Error(w,filename)
     Widget w;
     char *filename;
{
  char msg[MaxString];
  sprintf(msg,"error in QA node %s.  Continuing....",filename);
  XtWarning(msg);
}

char *Util_Remove_WhiteSpace();

static char QA_Process_Control_Info(control_buffer)
     char *control_buffer;
{
  char *temp;
  char tempstr[MaxString];
  
  temp = Util_Remove_WhiteSpace(control_buffer,tempstr);

  if (!strncmplo(temp,begin_string,AGstrlen(begin_string)))
    {
      /* find out which; this is a kludge */
      temp += AGstrlen(begin_string) + 1; /* get the delimiter */
      if (*temp == 'q' || *temp == 'Q')
	return('Q');
      else if (*temp == 'a' || *temp == 'A')
	return('A');
      else
	 return(*temp);
    }
  
  
  if (!strncmplo(temp,end_string,AGstrlen(end_string)))
    {
      /* eat it for now */
      return(NULL);
    }
  return('@');  /* some random @ sign */
}


#define QAPutChar(ptr,chr)  *(ptr++) = chr
#define QAPutString(ptr,string) AGstrcpy(ptr,string); \
		                ptr += AGstrlen(string);

static QAInfo *QA_Initialize(buffer,n_lines)
     char *buffer;
     int *n_lines;
{
  int i;
  int len;
  char *ptr,*endptr,*indexptr,*bodyptr,*controlptr;
  Boolean control_flag;
  char control_buffer[MaxString];
  char *index_buffer;
  char *body_buffer;
  QAInfo *qa_info;
  char index_char;

  if ((qa_info = QA_New()) == NULL)
    return(NULL);

  len = AGstrlen(buffer);
  /* let's be a little wasteful here in prototyping */

  for (i=0,*n_lines=0;i<len;i++)
    if (buffer[i] == EndLine)
      (*n_lines)++;

    /* This is a pretty arbitrary number.  I think it should
       be n_lines*2+1, but I'm upping it just to make sure. */
  if ((qa_info->index_buffer = 
       XtMalloc( (*n_lines)*(MaxQAIndexCharWidth+1)+1 )) == NULL)
    {
      QA_Destroy(qa_info);
      return(NULL);
    }
  if ((qa_info->body_buffer = XtMalloc(len+1)) == NULL)
    {
      QA_Destroy(qa_info);
      return(NULL);
    }
  
  ptr = buffer;
  endptr = (char *)(buffer+len);
  bodyptr = qa_info->body_buffer;
  indexptr = qa_info->index_buffer;
  controlptr = control_buffer;

  control_flag = False;

  for (; ptr < endptr; ptr++)
    switch (*ptr)
      {
      case EndLine:
	if (control_flag)
	  {
	    control_flag = False;
	    QAPutChar(controlptr,EOS);
	    index_char = QA_Process_Control_Info(control_buffer);
	    switch (index_char)
	      {

#ifdef NOPE
	      case 'Q':
		/*QAPutChar(bodyptr,EndLine);*/
		  /* Using 3 putchar instead of a putstring so can
		     avoid problem with having an EOS (or not) on the 
		     string */
		QAPutChar(indexptr,'Q');
		/*QAPutChar(indexptr,EndLine);*/
		break;
	      case 'A':
		/*QAPutChar(bodyptr,EndLine);*/
		QAPutChar(indexptr,'A');
/*		QAPutChar(indexptr,EndLine);*/
		break;
#endif
	      case '@': /* it was a random @ sign - put it in the
			   body */
		QAPutChar(bodyptr,'@');
		QAPutString(bodyptr,control_buffer);
		QAPutChar(bodyptr,'\n');
		QAPutChar(indexptr,'\n');
		break;
	      case NULL:
		break;
	      default:
		if (isprint(index_char))
		   QAPutChar(indexptr,index_char);
		break;
	      }
	    controlptr = control_buffer;
	  }
	else
	  {
	    QAPutChar(bodyptr,EndLine);
	    QAPutChar(indexptr,EndLine);
	  }
	break;
      default:
	if (control_flag)
	  QAPutChar(controlptr,*ptr);
	else
	  {
	    if (*ptr == '@' && *(ptr+1) != '@')
	      {
		control_flag = True;
	      }
	    else
	      QAPutChar(bodyptr,*ptr);
	  }
	break;
      }
  QAPutChar(indexptr,EOS);
  QAPutChar(bodyptr,EOS);
  return(qa_info);
}  

#undef QAPutChar
#undef QAPutString


static void QA_Destructor_Callback(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  QAInfo *qa_info = (QAInfo *)call_data;
  QA_Destroy(qa_info);
}

static Widget QA_Open(qa_info,n_lines,call_data)
     QAInfo *qa_info;
     int n_lines;
     XtPointer call_data;
{
    Widget shell;
    Widget body_text;
    Widget index_text;
    Widget form;
    Widget layout;
    Widget viewport;
    Widget button_form;
    Widget text_form;
    Widget label;
    int num_buttons;
    Widget *buttons;
    Arg arglist[10];
    XFontStruct *font_struct;


    if (call_data == NULL)
      shell = XtVaCreatePopupShell(
				NodeService_Get_Node_Name(qa_info->node_info),
				   topLevelShellWidgetClass,
				   global_info.main_shell,
				   XtNinput,(XtArgVal)True,
				   XtNallowShellResize, (XtArgVal)True,
				   XtNtitle, (XtArgVal)
				NodeService_Get_Label(qa_info->node_info),
				   NULL);
    else
      shell = XtVaCreatePopupShell(
				NodeService_Get_Node_Name(qa_info->node_info),
				   topLevelShellWidgetClass,
				   global_info.main_shell,
				   XtNinput,(XtArgVal)True,
				   XtNallowShellResize, (XtArgVal)True,
				   XtNgeometry, (XtArgVal)call_data,
				   XtNtitle, (XtArgVal)
				NodeService_Get_Label(qa_info->node_info),
				   NULL);

    form = XtVaCreateManagedWidget("qAForm",formWidgetClass,shell,
				   XtNborderWidth, (XtArgVal) 10,
				   NULL);
    
    label = XtVaCreateManagedWidget("qALabel",labelWidgetClass,form,
				    NULL);


    viewport = XtVaCreateManagedWidget("qAViewport",viewportWidgetClass,
				       form,
/*				       XtNborderWidth, (XtArgVal)0,*/
				       XtNallowHoriz,(XtArgVal)True,
				       XtNallowVert,(XtArgVal)True,
				       XtNfromVert, (XtArgVal) label,
				       NULL);
    
  
    /* Todo: Get rid of this by making knedit subclass of form */
    text_form =  XtVaCreateManagedWidget("qAInnerForm",formWidgetClass,
					  viewport,
					  XtNborderWidth, (XtArgVal)0,
					  NULL);
  
  
  
    index_text = 
      XtVaCreateManagedWidget("qAIndexText",asciiTextWidgetClass,
				   text_form,
				   XtNtype, (XtArgVal)XawAsciiString,
				   XtNuseStringInPlace, (XtArgVal)True,
			           XtNdisplayCaret, (XtArgVal)False,
				   XtNlength, 
				     (XtArgVal)AGstrlen(qa_info->index_buffer),
				   XtNstring,(XtArgVal)qa_info->index_buffer,
				   XtNeditType, (XtArgVal)XawtextRead,
				   XtNscrollHorizontal,  
				   (XtArgVal)XawtextScrollNever,
				   XtNscrollVertical, 
				   (XtArgVal)XawtextScrollNever,
				   XtNfromVert, (XtArgVal)NULL,
				   XtNfromHoriz, (XtArgVal)NULL,
				   NULL);

    body_text = 
      XtVaCreateManagedWidget("qABodyText",asciiTextWidgetClass,
			      text_form,
			      XtNtype, (XtArgVal)XawAsciiString,
			      XtNuseStringInPlace, (XtArgVal)True,
			      XtNdisplayCaret, (XtArgVal)False,
		      XtNlength,(XtArgVal)AGstrlen(qa_info->body_buffer),
			      XtNstring,(XtArgVal)qa_info->body_buffer,
			      XtNeditType, (XtArgVal)XawtextRead,
			      XtNscrollHorizontal, 
			       (XtArgVal)XawtextScrollNever,
			      XtNscrollVertical, 
			       (XtArgVal)XawtextScrollNever,
			      XtNfromVert, (XtArgVal)NULL,
			      XtNfromHoriz, (XtArgVal)index_text,
			      NULL);
  
    XtVaGetValues(index_text,XtNfont,&font_struct,NULL);
    XtVaSetValues(index_text,
		  XtNwidth, (XtArgVal)(font_struct->max_bounds.width
				       * QAIndexColumns
				       + QAHorizMargin),
		  XtNheight, (XtArgVal)((font_struct->max_bounds.ascent +
					font_struct->max_bounds.descent +
					QAVertLeading)*n_lines),
		  NULL);

    /* index's font is probably bold, so get the font again */
    XtVaGetValues(body_text,XtNfont,&font_struct,NULL);
    XtVaSetValues(body_text,
		  XtNwidth, (XtArgVal)(font_struct->max_bounds.width
				       * QABodyColumns + QAHorizMargin),
		  XtNheight, (XtArgVal)((font_struct->max_bounds.ascent +
					font_struct->max_bounds.descent +
					QAVertLeading)*n_lines),
		  NULL);

    Util_Open_ButtonBox_With_Bindings(form,&button_form,True,&buttons,
				      &num_buttons,
				      (XtPointer)qa_info->node_info);

    Util_Add_Button_With_Bindings(button_form,buttons,&buttons,&num_buttons,
				  "forwButton",
				  NULL,(XtPointer)NULL); 
    Util_Add_Button_With_Bindings(button_form,buttons,&buttons,&num_buttons,
				  "backButton",
				  NULL,(XtPointer)NULL);
    XtFree((char *)buttons);
 

    XtVaSetValues(button_form,XtNfromVert,(XtArgVal)viewport,NULL);

    XtAddCallback(shell,XtNdestroyCallback,QA_Destructor_Callback,
		  (XtPointer)qa_info);

    AG_Geometry(shell,qa_info->node_info);
    XtRealizeWidget(shell);
    XtPopup(shell,XtGrabNone);
    return(shell);
}


  









