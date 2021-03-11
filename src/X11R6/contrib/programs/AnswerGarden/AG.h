/**********************************************************

  The ANSWER GARDEN PROJECT

  AG.h  Answer Garden main module

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

***********************************************************/
#ifndef _AnswerG_h
#define _AnswerG_h

#include <ctype.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include "AGmacros.h" /* some string macros to handle Suns */

  /* where to temporarily store the mail message */
#define MailUserFile ".agdraft"


#define XtNdirectory "directory"
#define XtNmailerType "mailerType"
#define XtNmailOrganizationName "mailOrganizationName"
#define XtNeditMode "editMode"
#define XtNstatisticsFile "statisticsFile"
#define XtNglobalExpertList "globalExpertList"
#define XtNorganization "organization"
#define XtNmailName "mailName"
#define XtNmailMachineName "mailMachineName"
#define XtNdomainName "domainName"
#define XtNnodeExpert "nodeExpert"
#define XtNstartupNode "startupNode"
#define XtNcloseCallback "closeCallback"
#define XtNhelpCallback "helpCallback"
#define XtNquestionCallback "questionCallback"
#define XtNgrapherMaxHeight "grapherMaxHeight"
#define XtNgrapherMaxWidth "grapherMaxWidth"
#define XtNuseMail "useMail"
#define XtNlocalExpertList "localExpertList"
#define XtNuseLocalExpert "useLocalExpert"
#define XtNtrackingServerAddress "trackingServerAddress"
#define XtNstack "stack"
#define XtNnStacks "nStacks"
#define XtNwmProtocolsTranslations "wmProtocolsTranslations"
#define XtNrootGrapher "rootGrapher"
#define XtNroot "root"
#define XtNbackCallback "backCallback" /**092192**/
#define XtNlockedCallback "lockedCallback" /**092192**/
#define XtNlocked "locked" /**092192**/
#define XtNnumberGeneralButtons "numberGeneralButtons" /**101992**/
#define XtNunlockedText "unlockedText" /* 060294*/
#define XtNlockedText "lockedText" /*060294*/

#define XtCDirectory "Directory"
#define XtCMailerType "MailerType"
#define XtCMailOrganizationName "MailOrganizationName"
#define XtCEditMode "EditMode"
#define XtCStatisticsFile "StatisticsFile"
#define XtCOrganization "Organization"
#define XtCMailName "MailName"
#define XtCMailMachineName "MailMachineName"
#define XtCDomainName "DomainName"
#define XtCNodeExpert "NodeExpert"
#define XtCStartupNode "StartupNode"
#define XtCExpertList "ExpertList"
#define XtCUseMail "UseMail"
#define XtCUseLocalExpert "UseLocalExpert"
#define XtCServerAddress "ServerAddress"
#define XtCStack "Stack"
#define XtCNStacks "NStacks"
#define XtCWMProtocolsTranslations "WMProtocolsTranslations"
#define XtCRoot "Root"
#define XtCLocked "Locked" /**092192**/
#define XtCNumberGeneralButtons "NumberGeneralButtons" /**101992**/
#define XtCText "Text" /*060294*/

/* For data collection */
#define MailQuestionNotificationList "ackerman=ag-notification@ics.uci.edu"
#define AGVersionNumber "1.10"
#define AGSClassName "AGS"


/* Some useful const's */
#define MaxString 256
#define EOS '\0'
#define AGNodeTypes int
#define AGTypeToken XrmQuark


/* some generic definitions that hide the actual definitions for
   the modules */
typedef struct _FileInfo FileInfo, *FileInfoPtr;

typedef struct _NodeServiceListMember NodeInfo, *NodeInfoPtr;

typedef struct _EditInfo EditInfo, *EditInfoPtr;

/********************** General Data Structures **********************/

/* used to keep track of user history */
typedef struct _history_node
{
  char *node_name;
  char *type_name;
  char time[22]; /* could be 19 or even 18 */
  char *next;    /* link to next node */
} HistoryNode;


  /* The user settings -- will be expanded.  See AnswerGarden.c for
     defaults - these will mutate into application resources. */
typedef struct _user_settings
{
    Boolean auto_placement;
    Boolean auto_delete;
} UserSettings;


typedef enum {
  ShowNone,
  Organization,
  Author
} ShowAuthor;

typedef enum {
  NotMeaningful,
  Low,
  High
} AuthorityLevel;

typedef enum {
/*  NotMeaningful,*/  /**????***/
  ShortAnswer,
  Discourse
} ViewType;

typedef struct _AG_Nonpredefined_Header
{
  XtPointer magic;
  char *label;
  char *value;
  struct _AG_Nonpredefined_Header *next;
} AG_Nonpredefined_Header;

/*
The header should contain at least the following labels:

static char *header_array =
{
  "author",
  "show_author",
  "author_organization",
  "node_expert",
  "expiration_date",
  "last_modifier",
  "last_mod_date",
  "mod_num",

};

This array is contained in FileServ.c.
*/

#define FileServHeaderAuthor 0
#define FileServHeaderShowAuthor 1
#define FileServHeaderOrganization 2
#define FileServHeaderNodeExpert  3
#define FileServHeaderExpirationDate 4
#define FileServHeaderLastModifier  5
#define FileServHeaderLastModDate  6
#define FileServHeaderModNum  7

#define NFileServHeaderValues 8





/* used only by routines in Util.c to add new buttons and their
   callbacks to each node and with a Dynamic routine in AG.c */
typedef struct _AGGeneralButtonStruct 
{
  int id;
  XtPointer client_data;
}   AGGeneralButtonStruct;



typedef Widget (*AGSCreateProc)();
typedef void (*AGSClassCreateProc)();
typedef String* (*AGSListProc)();
typedef struct
{
  char *name;
  AGTypeToken token;
  XrmQuark name_quark;
  AGSCreateProc create_function;
  AGSClassCreateProc class_create_function;
  AGSClassCreateProc initialize_function;
  Boolean virtual_node;
  Boolean class_created;
  XtCallbackProc help_callback;
  XtCallbackProc question_callback;
  XtCallbackProc close_callback;
  AGSListProc search_function;
  Boolean get_file_immediate;
} NodeType; /* once called SortaObj because that's what it is */

typedef struct 
{
    String directory;    /* where to find the information database */
    String mailer_type;  /* what mailer to use (e.g., /bin/mail or mh) */
    Boolean edit_mode;   /* want to allow editing? */
    Widget main_shell;   /* the shell for the first node */
    int n_global_node_types;    /* how many node types */
    String statistics_file; /* the name of the file for the user stats */
    String organization;    /* what company/group (from the resource file) */
    String global_expert_list; /* default experts (from the resource file) */
    String mail_name;       
    String mail_machine_name;
    String domain_name;
    String node_expert;
    String startup_node;   /* what node to start up from */
    XrmQuark AGS_quark;     /* name changed 6/2/94 MSA */
    XContext icon_context; /* for watching iconification and deiconification */
    Widget  global_func_obj;  /* the default funcObj for @ commands in AG */
    Dimension grapher_max_height;  /* you guessed it */
    Dimension grapher_max_width;
    Boolean use_mail;          /* allow mail? */
    Boolean use_local_expert;  /* send questions to the local expert first? */
    String local_expert_list;  
    String tracking_server_address; /* where is the tracking server? */
    Widget history_list;       
    int n_stacks;              /* for stack operations (window geometry) */
    Widget *stacks;            
    Atom wm_delete_window;     /* to handle WM_DELETE_WINDOW requirements */
    Atom wm_protocols;
    XtTranslations wm_protocols_translations;
    String root_grapher;
    String root;
    Boolean locking_enabled;
    int number_general_buttons; /* added 10/19/92, to handle a node type
				   wanting additional buttons on all nodes */
    WidgetClass general_button_class;  /* added 10/19/92 to allow overrides
					  of button classes on all nodes'
					  help, quesiton, and close buttons */
    String class_name; /* 10/21/92 the class name for this application */
    String program_name; /* 10/21/92 the program name for this app */
    String locked_text; /* 6/2/94 MSA */
    String unlocked_text; /* 6/2/94 MSA */
} GlobalInfo;

/******* misc ************/



void General_Close_Callback();
void General_Help_Callback();
void General_Question_Callback();
void General_Back_Callback();
void General_Locked_Callback();

void Dynamic_Close_Callback();
void Dynamic_Help_Callback();
void Dynamic_Question_Callback();
void Dynamic_Back_Callback();
void Dynamic_Locked_Callback();

Boolean Dynamic_Add_Callback();

Widget AG_Create_Node();
void AG_Geometry();

void General_Question_Callback();
void General_Close_Callback();
char *getenv();
void AG_Raise_Shell();

/***** NODESERV.C *****/

char *NodeService_Get_Node_Name();
char *NodeService_Get_Label();
char *NodeService_Get_TypeString();
char *NodeService_Get_Location();
AGNodeTypes NodeService_Get_Type();
AGNodeTypes NodeService_Get_Type_From_String();
char **NodeService_Get_Defined_Headers();
char *NodeService_Get_Text();
Widget NodeService_Get_Shell();
EditInfo *NodeService_Get_EditNIPInfo();
EditInfo *NodeService_Get_EditFileInfo();
FileInfoPtr NodeService_Get_FileInfo();
Boolean NodeService_Set_Defined_Headers();
char *NodeService_Get_String_From_Type();
AG_Nonpredefined_Header *NodeService_Get_Nonpredefined_Headers();


char *NodeSerice_Node_String_From_Type();
int NodeService_Node_Int_From_Type();
int NodeService_Node_Type_From_Int();

int NodeService_Request_Fields();
int NodeService_Request_By_Name();

void NodeService_Register_Open_Node();
void NodeService_Register_Open_File();
void NodeService_Register_Closed_Node();
void NodeService_Register_Closed_Edit();
Boolean NodeService_Has_Node_Changed();
NodeInfoPtr NodeService_Find_Open_Node();


NodeInfoPtr NodeService_Add_Node();
int NodeService_Replace_Node();


Boolean NodeService_Save_File();
Boolean NodeService_Save_New_File();
void NodeService_Close_File();
Boolean NodeService_Open_File();

Boolean NodeService_Check_NodeInfo();



/**** FILESERV.C *****/

int AG_File_Filename_Set();
void Form_Filename();
Boolean Check_Node_Physical_Existance();
FileInfo *AG_File_Open();
char *AG_File_Extract_Text();


/**** UTIL.C ****/
void Util_Open_ButtonBox();
void Util_Open_Minimal_ButtonBox();
void Util_Open_ButtonBox_With_Bindings();
void Util_Add_Button_With_Bindings();


char *Util_Remove_Leading_WhiteSpace();
char *Util_Remove_WhiteSpace();
void Util_Get_Date();
void Util_Replace_String();
void Util_Get_UserName_With_Machine();
void User_History_List_Add_Member();
void Util_Get_UserInfo();

char *Remove_WhiteSpace();  
char *Remove_Control_Marks();
char *Remove_Tabs();

/*** STACKOBJ.C ***/

Boolean AG_Stack_Set_Locked();
Boolean AG_Stack_Get_Locked();


#endif /*_AnswerG_h*/



