/*		Character grid hypertext object
**		===============================
**              Converted for use with tkWWW
*/

#include <assert.h>
#include <ctype.h>
#include <tcp.h>
#include "HTUtils.h"
#include "HTString.h"
#include "HTextDef.h"


struct _HTStream {			/* only know it as object */
    CONST HTStreamClass *	isa;
    /* ... */
};

/*			Creation Method
**			---------------
*/
PUBLIC HText *	HText_new ARGS1(HTParentAnchor *,anchor)
{
    HText * self = (HText *) malloc(sizeof(*self));
    if (!self) return self;

    self->node_anchor = anchor;
    self->output = HTChunkCreate(1024);
    self->execute_pointer = 0;
    self->error_code = TCL_OK;
    HTAnchor_setDocument(anchor, (HyperDoc *)self);
    return self;
}

/*			Creation Method 2
**			---------------
**
**	Stream is assumed open and left open.
*/
PUBLIC HText *	HText_new2 ARGS2(
		HTParentAnchor *,	anchor,
		HTStream*,		stream)

{
    HText * this = HText_new(anchor);
        
    if (stream) {
        this->target = stream;
	this->targetClass = *stream->isa;	/* copy action procedures */
    }    
    return this;
}


/*	Free Entire Text
**	----------------
*/
PUBLIC void 	HText_free ARGS1(HText *,self)
{
    HTAnchor_setDocument(self->node_anchor, (HyperDoc *)0);
    HTChunkClear(self->output);
}

/*			Object Building methods
**			-----------------------
**
**	These are used by a parser to build the text in an object
*/

/*	Append a character to the text object
**	-------------------------------------
*/
PUBLIC void HText_appendCharacter ARGS2(HText *,text, char,ch)
{
  switch (ch) {
  case '{': /* add an escape before braces */
  case '}':
  case '[':
  case ']':
  case '\"':
  case '\\':
  case '$':
    HText_putc(text, '\\');
    break;
  }
  HText_putc(text, ch);
}

/*		Anchor handling
**		---------------
*/
/*	Start an anchor field
*/

PUBLIC void HText_beginAnchor ARGS2(HText *,text, HTChildAnchor *,anc)
{
  HText_puts(text, "\"\n");
}

PUBLIC void HText_endAnchor ARGS1(HText *,text)
{
  HText_puts(text, "\"\n");
}

PUBLIC void HText_appendText ARGS2(HText *,text, CONST char *,str)
{
  CONST char * p;
  for(p=str; *p; p++)
    HText_appendCharacter(text, *p);
}

PUBLIC void HText_puts ARGS2(HText *,text, CONST char *, s)
{
  HTChunkPuts(text->output, s);
}


PUBLIC void HText_putc ARGS2(HText *,text, CONST char, c)
{
  HTChunkPutc(text->output, c);
}

PUBLIC void HText_executeTCL ARGS1(HText *,text)
{
  if (text->error_code != TCL_OK) 
    return;

  /* check there is enough in the buffer to execute */
  if (text->output->size > 0) {
    int value;
    char *cmd_start;
    cmd_start = text->output->data + text->execute_pointer;

    /* never access directly, it crash when chunk is just full 
       use that instead :                     -- dl           */
    HTChunkPutc(text->output,0) ;
    /* subtract one from pointer so that the next write to the
       chunk overwrites the null at the end.

       WARNING: This breaks an abstraction barrier.  Unfortunately, 
       I don't know of a better way of doing this.  

       Simply putting a NULL at the end of the string will cause problems
       if it is at the end of an allocated space.  So I have to use
       HTChunkPutc(text->output,0);

       There should be a HTChunkRewind
     */

    text->output->size--;
    
    if (Tcl_CommandComplete(cmd_start)) {
      text->execute_pointer = text->output->size;
      value = Tcl_Eval(HtTclInterp, cmd_start);
      if (value != TCL_OK) 
	text->error_code = value;
    }
  }
}

PUBLIC void HText_endAppend ARGS1(HText *,text)
{
  HTChunkTerminate(text->output);
  HtTclErrorCode = text->error_code;
}



/* 	Dump diagnostics to stderr
*/
PUBLIC void HText_dump ARGS1(HText *,text)
{
  fprintf(stderr, "HText: Dump called\n");
}
	

/*	Return the anchor associated with this node
*/
PUBLIC HTParentAnchor * HText_nodeAnchor ARGS1(HText *,text)
{
  return text->node_anchor;
}

/*		Browsing functions
**		==================
** This procedure is called if a node is already loaded.  In this case
** all we need to do is to execute the cache of TCL commands
*/

PUBLIC BOOL HText_select ARGS1(HText *,text)
{
  if (Tcl_Eval(HtTclInterp, text->output->data) == TCL_OK)
    return YES;
  else
    return NO;
}

PUBLIC BOOL HText_selectAnchor ARGS2(HText *,text, HTChildAnchor *,anchor)
{
  return YES;
}
 

/*		Editing functions		- NOT IMPLEMENTED
**		=================
**
**	These are called from the application. There are many more functions
**	not included here from the orginal text object.
*/

/*	Style handling:
*/
/*	Apply this style to the selection
*/
PUBLIC void HText_applyStyle ARGS2(HText *, me, HTStyle *,style)
{
}


/*	Update all text with changed style.
*/
PUBLIC void HText_updateStyle ARGS2(HText *, me, HTStyle *,style)
{
}


/*	Return style of  selection
*/
PUBLIC HTStyle * HText_selectionStyle ARGS2(
	HText *,me,
	HTStyleSheet *,sheet)
{
    return 0;
}


/*	Paste in styled text
*/
PUBLIC void HText_replaceSel ARGS3(
	HText *,me,
	CONST char *,aString, 
	HTStyle *,aStyle)
{
}


/*	Apply this style to the selection and all similarly formatted text
**	(style recovery only)
*/
PUBLIC void HTextApplyToSimilar ARGS2(HText *,me, HTStyle *,style)
{
    
}

 
/*	Select the first unstyled run.
**	(style recovery only)
*/
PUBLIC void HTextSelectUnstyled ARGS2(HText *,me, HTStyleSheet *,sheet)
{
}


/*	Anchor handling:
*/
PUBLIC void		HText_unlinkSelection ARGS1(HText *,me)
{    
}

PUBLIC HTAnchor *	HText_referenceSelected ARGS1(HText *,me)
{
     return 0;   
}

PUBLIC HTAnchor *	HText_linkSelTo ARGS2(HText *,me, HTAnchor *,anchor)
{
    return 0;
}


PUBLIC void HText_setStyle ARGS2(HText *, me, HTStyle *, style) {
}

PUBLIC void HText_beginAppend ARGS1(HText *, text) {
}


