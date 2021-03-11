/**********************************************************

  The ANSWER GARDEN PROJECT

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

  AnswerGarden.c

  Mark Ackerman
  MIT/Center for Coordination Science
  MIT/Project Athena

  July, 1989

***********************************************************/
#include <stdio.h>
#include "AG.h"
#include "FuncObj.h"
#include "StackObj.h"
#include <X11/Xaw/Command.h>

#define MAXSHORT  32767

/***********************************

  USER SETTINGS

***********************************/

UserSettings user_settings =
{ 
    TRUE, 
    FALSE, 
};


/**************************************

  SORTA OBJECTS

  SortaObj's -- How node types
  get hooked into AG.  For each
  node type there must be a SortaObj
  record here.

  10/19/92 - NOW, BRAND NEW!
  called NodeTypes instead.  Why?
  It sounds better in a dissertation....

******************************************/
extern Widget Open_SBrowser();
extern Widget Show_Grapher();
extern Widget Discuss_Create();
extern Widget Create_QA();
extern Widget Code_Create();
extern Widget Ascii_Create();

#ifdef SAO
extern Widget Open_ParamEditor();
extern Widget Open_PackageLoader();
extern Widget Open_LockGrapher();
extern void Init_ParamEditor();
#endif

#ifdef LIVEDOC
extern Widget Doc_Create();
#endif

NodeType sBrowser_obj = { "SBrowser", 0, NULLQUARK,  Open_SBrowser, 
			    NULL, NULL, False,
			    False,NULL, NULL,NULL, NULL,False};

NodeType grapher_obj = {"Grapher", 0, NULLQUARK,  Show_Grapher, 
			  NULL, NULL, False,
			    False, NULL, NULL,NULL,NULL, False};
NodeType discussion_obj = {"Discussion", 0, NULLQUARK,  Discuss_Create, 
			     NULL, NULL, False,
			    False, NULL, NULL,NULL,NULL, False};

NodeType qa_obj = {"QA-Node", 0, NULLQUARK,  Create_QA, 
		     NULL, NULL, False,
		     False, NULL, NULL,NULL, NULL, False};
NodeType code_obj = {"Code",0, NULLQUARK, Code_Create, 
		       NULL, NULL, False,
		       False, NULL, NULL,NULL, NULL, False};

NodeType ascii_obj = { "Ascii-Node", 0, NULLQUARK, Ascii_Create,
                      NULL, NULL, False,
                      False, NULL, NULL,NULL,NULL,True};

#ifdef SAO
/* ParamEdit stuff */
NodeType pfedit_obj = {"ParamEditor", 0, NULLQUARK, Open_ParamEditor, 
			 NULL, Init_ParamEditor,True, 
		         False, NULL, NULL,NULL,NULL,False};
NodeType pfhelp_obj = {"HelpPE", 0, NULLQUARK, Open_ParamEditor, 
			 NULL, NULL, True, 
		         False, NULL, NULL,NULL,NULL,False};
NodeType pfload_obj = {"PackageLoader", 0, NULLQUARK, Open_PackageLoader, 
			 NULL, NULL, True, 
		         False, NULL, NULL,NULL,NULL,False};
#endif


#ifdef JOURNAL
extern Widget Journal_Create();
NodeType journal_obj = {"Journal",0, NULLQUARK, Journal_Create,
			  NULL, NULL, False,
			  False, NULL, NULL,NULL,NULL,False};
#endif JOURNAL
#ifdef CISTK
extern Widget Cistk_Create();
NodeType cistk_obj = {"Cistk",0, NULLQUARK, Cistk_Create,
			  NULL, NULL, False,
			  False, NULL, NULL,NULL,NULL,False};
#endif CISTK
#ifdef LIVEDOC
NodeType doc_obj = { "RTF-Reader", 0, NULLQUARK, Doc_Create,
                      NULL, NULL, False,
                      False, NULL, NULL,NULL,NULL,True};
#endif

  /* The array of node types known to the Answer Garden */
  /* To create a new type, add it in here.  5/6/91  */
  /* Make sure that noaction_obj is always present */
NodeType *global_node_types[] = 
{
  &sBrowser_obj,
  &grapher_obj,
  &discussion_obj,
  &qa_obj,
  &code_obj,
  &ascii_obj,
#ifdef SAO
  &pfedit_obj,
  &pfhelp_obj,
  &pfload_obj,
#endif
#ifdef JOURNAL
  &journal_obj,
#endif
#ifdef CISTK
  &cistk_obj,
#endif
#ifdef LIVEDOC
  &doc_obj,
#endif
};


/************************

  GLOBALS 

************************/

GlobalInfo global_info;

#define offset(field) XtOffsetOf(GlobalInfo, field)
static XtResource resources[] = { 
    {XtNdirectory, XtCDirectory, XtRString, sizeof(String),
	 offset(directory), XtRString, (XtPointer)NULL},
    {XtNmailerType, XtCMailerType, XtRString, sizeof(String),
	 offset(mailer_type), XtRString, (XtPointer)"mh"},
    {XtNeditMode, XtCEditMode, XtRBoolean, sizeof(Boolean),
	 offset(edit_mode), XtRString, (XtPointer)"False"},
    {XtNstatisticsFile, XtCStatisticsFile, XtRString, sizeof(String),
         offset(statistics_file), XtRString, (XtPointer)NULL},
    {XtNglobalExpertList, XtCExpertList, XtRString, sizeof(String),
         offset(global_expert_list), XtRString, (XtPointer)NULL},
    {XtNorganization, XtCOrganization, XtRString, sizeof(String),
         offset(organization), XtRString, (XtPointer)NULL},
    {XtNmailName, XtCMailName, XtRString, sizeof(String),
         offset(mail_name), XtRString, (XtPointer)NULL},
    {XtNmailMachineName, XtCMailMachineName, XtRString, sizeof(String),
         offset(mail_machine_name), XtRString, (XtPointer)NULL},
    {XtNdomainName, XtCDomainName, XtRString, sizeof(String),
         offset(domain_name), XtRString, (XtPointer)NULL},
    {XtNnodeExpert, XtCNodeExpert, XtRString, sizeof(String),
         offset(node_expert), XtRString, (XtPointer)NULL},
    {XtNstartupNode, XtCStartupNode, XtRString, sizeof(String),
         offset(startup_node), XtRString, (XtPointer)"Control"}, /**092092**/
    {XtNgrapherMaxHeight, XtCHeight, XtRDimension, sizeof(Dimension),
         offset(grapher_max_height), XtRImmediate, (XtPointer)MAXSHORT},
    {XtNgrapherMaxWidth, XtCWidth,  XtRDimension, sizeof(Dimension),
         offset(grapher_max_width), XtRImmediate, (XtPointer)MAXSHORT},
    {XtNlocalExpertList, XtCExpertList, XtRString, sizeof(String),
         offset(local_expert_list), XtRString, (XtPointer)NULL},
    {XtNuseMail, XtCUseMail, XtRBoolean, sizeof(Boolean),
	 offset(use_mail), XtRString, (XtPointer)"True"},
    {XtNuseLocalExpert, XtCUseLocalExpert, XtRBoolean, sizeof(Boolean),
	 offset(use_local_expert), XtRString, (XtPointer)"False"},
    {XtNtrackingServerAddress, XtCServerAddress, XtRString, sizeof(String),
	 offset(tracking_server_address), XtRString, (XtPointer)NULL},
    {XtNnStacks, XtCNStacks, XtRInt, sizeof(int),
       offset(n_stacks),XtRImmediate,(XtPointer)0},
    {XtNwmProtocolsTranslations,XtCWMProtocolsTranslations,
       XtRTranslationTable, sizeof(XtTranslations),
       offset(wm_protocols_translations),XtRString,
       "<Message>WM_PROTOCOLS: AGWMProtocols()\n"},
    {XtNrootGrapher, XtCRoot, XtRString, sizeof(String),
	 offset(root_grapher), XtRString, (XtPointer)"RootGrapher"}, 
    {XtNroot, XtCRoot, XtRString, sizeof(String),
	 offset(root), XtRString, (XtPointer)"Root"},
    /* 6/2/94 MSA */
    {XtNlockedText, XtCText, XtRString, sizeof(String),
	 offset(locked_text), XtRString, (XtPointer)"Locked"}, 
    {XtNunlockedText, XtCText, XtRString, sizeof(String),
	 offset(unlocked_text), XtRString, (XtPointer)"Unlocked"}, 
    {XtNnumberGeneralButtons, XtCNumberGeneralButtons, XtRInt, sizeof(int),
	 offset(number_general_buttons), XtRImmediate, (XtPointer)3}, 
};
#undef offset

extern void General_Close_Callback_With_Shell();

/* MSA 5/13/94 */

static FuncSubstitutionRec substitution_table[] = 
{
  {"@DIRECTORY", ""},
  {"@DIR", ""},
  {"@PRINTER",""},
  {"@EDITOR",""},
};

#define SUBST_DIRECTORY 0
#define SUBST_DIR 1
#define SUBST_PRINTER 2
#define SUBST_EDITOR 3
#define NSUBSTITUTIONS 4

#if NeedFunctionPrototypes
Globals_Initialize(Widget shell, String progname)
#else
Globals_Initialize(shell,progname)
     Widget shell;
     String progname;
#endif
{
  int i;
  char buffer[MaxString];
  char *ptr;

  /* Set up the global_info structure.  We need to copy 
     the Directory and mailer type into our space.  Directory
     is where to find the information-base for AG; mailer
     type is which mailer to use. */
  XtGetApplicationResources(shell,(XtPointer) &global_info,
			    resources,XtNumber(resources), NULL, 0);

  global_info.directory = XtNewString(global_info.directory);
  global_info.program_name = XtNewString(progname);
  global_info.class_name = AGSClassName;

  /* MSA 6/2/94 SAO mod */
  if (global_info.directory == NULL)
    {
      sprintf(buffer,"This session with %s cannot find the directory",
	      global_info.program_name);
      XtWarning(buffer);
      XtWarning("\twith the data files.  Check the XAPPLRESDIR environment");
      XtWarning("\tvariable (which should point at the application resource");
      sprintf(buffer,"\tcalled %s).  If that is correct, then check",
	      global_info.class_name);

      XtWarning(buffer);
      XtWarning("\tto make sure that the Directory (or directory) resource");
      sprintf(buffer,"\tis set to the correct directory for %s ",
	      global_info.program_name);
      XtWarning(buffer);
      XtWarning("\tinformation files.");
      sprintf(buffer,"%s is exiting...\n",
	      global_info.program_name);
      XtWarning(buffer);
      exit(127);
    }
  


  global_info.main_shell = shell;
  global_info.mailer_type = XtNewString(global_info.mailer_type);
  global_info.statistics_file = XtNewString(global_info.statistics_file);
  global_info.global_expert_list = XtNewString(global_info.global_expert_list);
  global_info.organization = XtNewString(global_info.organization);
  global_info.mail_name = XtNewString(global_info.mail_name);
  global_info.mail_machine_name = XtNewString(global_info.mail_machine_name);
  global_info.domain_name = XtNewString(global_info.domain_name);
  global_info.node_expert = XtNewString(global_info.node_expert);
  global_info.startup_node = XtNewString(global_info.startup_node);
  global_info.local_expert_list = XtNewString(global_info.local_expert_list);
  global_info.tracking_server_address = 
    XtNewString(global_info.tracking_server_address);
  global_info.root = XtNewString(global_info.root);
  global_info.root_grapher = XtNewString(global_info.root_grapher);
  global_info.locked_text = XtNewString(global_info.locked_text);
  global_info.unlocked_text = XtNewString(global_info.unlocked_text);

  if (global_info.n_stacks < 0)
    global_info.n_stacks = 0;
  if (global_info.n_stacks > 0)
    global_info.stacks = (Widget *)
      XtMalloc(sizeof(Widget) * global_info.n_stacks);
  for (i=0;i<global_info.n_stacks;i++)
    {
      sprintf(buffer,"stack%d",i+1);
      global_info.stacks[i] = 
	XtVaCreateWidget(buffer,stackObjClass,
			 global_info.main_shell,
			 NULL);
      XtAddCallback(global_info.stacks[i],XtNautoRemovalCallback, 
		    General_Close_Callback_With_Shell,NULL); /**092092**/

    }
  
  /* 6/2/94 MSA name of AG_quark changed and string changed to be
     program_name from "AnswerGarden" */
  global_info.AGS_quark = XrmStringToQuark(global_info.program_name);
  global_info.icon_context = XUniqueContext();
  global_info.n_global_node_types = XtNumber(global_node_types);
  global_info.history_list = NULL;
  /* let's see if I can get away with this */
  
  /* for now MSA 5/13/94 */ /* Yes, these should all be resources */
  substitution_table[SUBST_DIRECTORY].new_string = global_info.directory;
  substitution_table[SUBST_DIR].new_string 
    = XtNewString(global_info.directory);
  if (global_info.directory != NULL) /* should be safe here, but we'll
					make sure */
    {
      substitution_table[SUBST_DIR].
	new_string[AGstrlen(global_info.directory)-1] =
	  EOS; /* make it a character shorter; ie, get rid of the trailing / */
    }
  if ((ptr = getenv ("EDITOR")) != NULL)
    substitution_table[SUBST_EDITOR].new_string = XtNewString(ptr);
  else
    substitution_table[SUBST_EDITOR].new_string = XtNewString("vi");
  if ((ptr = getenv ("PRINTER")) != NULL)
    substitution_table[SUBST_PRINTER].new_string = XtNewString(ptr);
  else
    substitution_table[SUBST_PRINTER].new_string = XtNewString("");

  global_info.global_func_obj = XtVaCreateWidget("GlobalFuncObj",
						 funcObjClass,
						 global_info.main_shell,
						 XtNsubstitutionTable,
						 (XtArgVal)substitution_table,
						 XtNsubstitutionTableNumber,
						 (XtArgVal) NSUBSTITUTIONS,
						 NULL);
					       
  global_info.wm_protocols = XInternAtom(XtDisplay(shell),"WM_PROTOCOLS",
					 False);
  
  global_info.wm_delete_window = XInternAtom(XtDisplay(shell),
					     "WM_DELETE_WINDOW",
					     False);

  /* 10/19/92 default button class for all nodes' help, question, and
     quit buttons */
  global_info.general_button_class = (WidgetClass)commandWidgetClass;
  /* 10/19/92 force at least 3 buttons on each node */
  if (global_info.number_general_buttons < 3)
    global_info.number_general_buttons = 3;

}
