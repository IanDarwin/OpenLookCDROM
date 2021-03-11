/***************************************************************************

  NodeService.c    


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


  Mark Ackerman
  Information and Computer Science
  University of California, Irvine

  formerly -
  MIT/Center for Coordination Science

  ackerman@ics.uci.edu


************************************************************************/


#include <stdio.h>

#include <X11/Intrinsic.h>
#include "AG.h"
#include "FileServ.h"
#include "Grapher.h"

extern NodeType *global_node_types[];
extern GlobalInfo global_info;

char *Util_Remove_WhiteSpace();
Widget Edit_Get_EditNIP_Shell();
Widget Edit_Get_EditFile_Shell();


#define NodeServiceDelimiter '#'
#define NSCheckField 'n'

#define AGNodeServiceFile "AGNodeFile"  /* "AGNodeServiceFile" was too long */

    /* This will become a hash table...tomorrow.*/
typedef struct _NodeServiceListMember{
    char check_field;
#ifdef TEMPORARY_NODES  
    char ns_status;
#endif
    struct _NodeServiceListMember *next;
    char *node_name;
    char *label;
    AGNodeTypes node_type;
    char *location;
    FileInfo *file_info;
} NodeServiceListMember;

static struct _nodeService_globals
{
    int  default_node;
    int  no_action_node;
    char *default_node_string;
    char *no_action_string;
    AGTypeToken default_node_token;
    AGTypeToken no_action_token;
    NodeServiceListMember *nodeService_root;
    NodeServiceListMember *nodeService_last;
} nodeService_globals = 
{
    0,           
    0,
    "noaction",  /* lower case! */
    "sbrowser",  /* lower case! */
    0,
    0,
    NULL,
    NULL,
};

#ifdef TEMPORARY_NODES  
#define NSNodeIsReal (char)1
#define NSNodeIsTemporary (char)0
#endif


/****************************************

  ROUTINES TO PROVIDE INFO ABOUT
  TYPES IN NODESERVICE

****************************************/

char *NodeService_Get_String_From_Type(type)
     AGNodeTypes type;
{
  int i;
  if (type >= 0)
    return(global_node_types[type]->name);
  return(NULL);
}

int NodeService_Node_Int_From_Type(type)
     AGNodeTypes type;
{
  return(type);
}
     
int NodeService_Node_Type_From_Int(i)
     int i;
{
  return(i);
}


static AGTypeToken NS_Get_Type_Token(string)
     char *string;
{
    char string2[MaxString];
    AGTypeToken temp_quark;

    XmuCopyISOLatin1Lowered(string2,string);
    temp_quark = XrmStringToQuark(string2);
    return(temp_quark);
}

static int Check_NodeTokens(this_token)
     AGNodeTypes this_token;
{
    int i;
    for (i=0;i<global_info.n_global_node_types;i++)
      if (global_node_types[i]->token == this_token)
	return(i);
    return(-1);
}



AGNodeTypes NodeService_Get_Type_From_String(string)
     char *string;
{
    int i;
    AGTypeToken temp_token;

    temp_token = NS_Get_Type_Token(string);

    for (i=0;i<global_info.n_global_node_types;i++)
      if (global_node_types[i]->token == temp_token)
	return(i);
    return(nodeService_globals.no_action_node);
}


/*********************************

  NODESERVICE INTERNAL ROUTINES

*********************************/

static char tempstr[MaxString];


#define GetString(string,first_char,temp) \
    { \
	    AGstrncpy(string,first_char,temp+1-first_char); \
	    string[temp-first_char] = EOS; \
	    }

#define   FindDelimiterAndIgnoreBlankLine(loc,next_loc) \
    first_char = loc; \
    if ((next_loc = AGindex(first_char,NodeServiceDelimiter)) == 0) \
	return(0);	/* treat as blank line */

static Boolean No_NodeService_String(first_char,temp)
     char *first_char;
     char *temp;
{
    char *ptr;
    for (ptr=first_char+1;ptr<temp;ptr++)
	if (*ptr != '\t' && *ptr != ' ')
	    return(False);
    return(True);
}


static NodeServiceListMember *NS_New()
{
   NodeServiceListMember *new;

   if ((new = (NodeServiceListMember *)XtMalloc(
				sizeof(NodeServiceListMember))) == NULL)
	return(NULL);
   new->next = NULL;
   new->node_name = NULL;
   new->label = NULL;
   new->location = NULL;
   new->node_type = (AGNodeTypes) -1;
   new->check_field = NSCheckField;
   new->file_info = NULL;
#ifdef TEMPORARY_NODES
   new->ns_status = NSNodeIsReal;
#endif
   return(new);
}

#define NSFree(field)  if (field != NULL) XtFree(field);

static Boolean NS_Destroy(node_info)
     NodeInfo *node_info;
{
  /* refuse to destroy an open node */
  if (node_info->file_info != NULL) 
    return(False);

  NSFree(node_info->node_name);
  NSFree(node_info->label);
  NSFree(node_info->location);
  XtFree((char *)node_info);
  return(True);
}
 
#undef NSFree 
  
/*
node name    #node type    #node label     #physical location

whatever     #include      #reserved       #physical location
*/

/* 10/29/91:
   Note that the return code is used by the calling function.
    < 0           error condition
    1             normal node found
    2             include directive found
*/
static int Parse_NodeService_Buffer(buffer,current_node,
				    current_location,include_location)
     char *buffer;
     NodeServiceListMember **current_node;
     char *current_location; /* used if more than 1 subdirectory */
     char *include_location; /* valid only if rc = 2 */
{
    char *first_char;
    char *temp;
    int len;
    char type[MaxString];
    char name[MaxString];
    char label[MaxString];
    AGNodeTypes node_type;
    char location[MaxString];
    char temp_buffer[MaxString];
    char type_without_whitespace[MaxString];

      /* name */
/*** 6/29/91 macros need to be rewritten to include first_char.
     first_char is set as a side effect of the FindDelimiter... macro */
    FindDelimiterAndIgnoreBlankLine(buffer,temp);
    GetString(name,first_char,temp);
    if (AGstrlen(name) < 1)
	return(0);

      /* type */
    first_char = temp+1;
    FindDelimiterAndIgnoreBlankLine(temp+1,temp);
    if (No_NodeService_String(first_char,temp)) /* no type */
	AGstrcpy(type, nodeService_globals.default_node_string);
    else
	GetString(type,first_char,temp);

      /* label */
    FindDelimiterAndIgnoreBlankLine(temp+1,temp);
    if (No_NodeService_String(first_char,temp)) /* no label */
	AGstrcpy(label,name);
    else
	GetString(label,first_char,temp);

      /* source location */
    first_char = temp+1;
    len = AGstrlen(first_char);
    if (len < 2) /* only newline */
	    /* assume name of node is also name of file */
	AGstrcpy(location,name);
    else
	AGstrcpy(location,first_char);

    /* 10/29/91:  if type is #include, then report back the
       location of the new AGNodeFile, and return rc 2 */
    /* 11/4/91:  moved Util_Remove_WhiteSpace call here to
       get rid of leading or trailing whitespace before
       compare for "include" */
    if (!strcmplo(Util_Remove_WhiteSpace(type,type_without_whitespace),
		  "include"))
      {
	*current_node = NULL;
	AGstrcpy(include_location,location);
	/* MSA 11/4/91:  put final / on end of new location */
	AGstrcat(include_location,"/");
	return(2);
      }
    
    if ((*current_node = NS_New()) == NULL)
	return(-1);

    nodeService_globals.nodeService_last = *current_node;

    (*current_node)->node_name = 
       XtNewString(Util_Remove_WhiteSpace(name,tempstr));
    (*current_node)->label = XtNewString(Remove_Tabs(label,tempstr));

    /* 10/28/91 put current location (if specified by a #include
       directive) on the front of the physical location */
    Util_Remove_Leading_WhiteSpace(location,tempstr);
    if (current_location != NULL && current_location[0] != EOS)
      {
	AGstrcpy(temp_buffer,current_location);
	AGstrcat(temp_buffer,tempstr);
      }
    else
      AGstrcpy(temp_buffer,tempstr);
    (*current_node)->location = XtNewString(temp_buffer);

    /*** MSA 11/4/91 moved Util_Remove_Whitespace call up
      higher ***/
    node_type = 
      NodeService_Get_Type_From_String(type_without_whitespace);
    (*current_node)->node_type = node_type;
    (*current_node)->next = NULL;

    return(1);
}
    
#undef GetString     
#undef FindDelimiterAndIgnoreBlankLine

static Boolean NS_Delete_Node_From_List(node_info)
     NodeInfoPtr node_info;
{
#ifdef NEXT
  NodeInfo *temp;
  node_info->prev->next = node_info->next;
  if (nodeService_globals.nodeService_last == node_info) /* not last */
    nodeService_globals.nodeService_last = node_info->prev;
  
  node_info->next->prev = node_info->prev;
  if (nodeService_globals.nodeService_root == node_info ) /* not first */
    node_info->next->prev = node_info->prev;
  
  NS_Destroy(node_info);
#endif
  return(True);
}

static NodeServiceListMember 
*NS_Add_Node_To_List(node_name,label,type,location)
     char *node_name;
     char *label;
     AGNodeTypes type;
     char *location;
{
  NodeServiceListMember *current;
  
  if ((current = NS_New()) == NULL)
    return(NULL);
  nodeService_globals.nodeService_last->next = current;
  nodeService_globals.nodeService_last = current;
   /* Potential for memory leak */
  current->node_name = XtNewString(node_name);  
  current->label = XtNewString(label);
  current->node_type = type;
  current->location = XtNewString(location);
  return(current);
}

/**** This will get turned into a hash table, soon .... ****/
static NodeServiceListMember *NS_Find_First_Member()
{
  NodeServiceListMember *current;

  if ((current = nodeService_globals.nodeService_root) == NULL)
    {
      XtWarning("no nodes in node service");
      return(NULL);
    }
  return(current);
}


static NodeServiceListMember *NS_Find_Next_Member(current)
  NodeServiceListMember *current;
{
  NodeServiceListMember *next_member;

  if (current == NULL)
    {
      XtWarning("in error state in NS_Find_Next_Member.  \
Attempting to continue.");
      return(NULL);
    }

  next_member = current->next;
  return(next_member);
}


static NodeServiceListMember *NS_Match_Member_Name(node_name)
  char *node_name;
{
  NodeServiceListMember *current;
  char buffer[MaxString];
  

  if ((current = NS_Find_First_Member()) == NULL)
    {
      return(NULL);
    }
  
  while (current)
    {
      if (strcmplo(node_name,current->node_name) == 0)
	  return(current);
      current = NS_Find_Next_Member(current);
    }
  return(NULL);
}



/*******************************

  I/O ROUTINES

  *** These must be changed to 
      use the FileService (4/4/91)

**********************************/

/* 10/29/91: changed to open AGNodeFile in a specified subdirectory
   (actually specified any-location relative to directory resource) */
static FILE *NS_Open_File(access_mode,subdirectory)
     char *access_mode;
     char *subdirectory;
{
    char filestring[MaxString];
    char buffer[MaxString];
    FILE *fp;
    
    if (subdirectory != NULL && subdirectory[0] != EOS)
      {
	AGstrcpy(buffer,subdirectory);
	AGstrcat(buffer,AGNodeServiceFile);
      }
    else
      AGstrcpy(buffer,AGNodeServiceFile);

    Form_Filename(filestring,buffer);
    
    if ((fp = fopen(filestring,access_mode)) == NULL)
      {
	XtWarning("could not open control file for NodeService");
	sprintf(buffer,"unable to open %s for accessing nodes info\n",
		filestring);
	XtWarning(buffer);
	Util_Debug(buffer);
	return(NULL);
      }
    return(fp);
  }


static void NS_Close_File(fp)
     FILE *fp;
{
  fclose(fp);
}

static Boolean NS_Backup_File()
{
  char filestring[MaxString];
  char filestring2[MaxString];
  char buffer[MaxString*2];
  char msg[MaxString];

  Form_Filename(filestring,AGNodeServiceFile);
  Form_Filename(filestring2,AGNodeServiceFile);
  strcat(filestring2,".bak");

  sprintf(msg,"Backing up previous NodeService File (%s)",AGNodeServiceFile);
  XtWarning(msg);
  XtWarning("   This operation may take a few moments");
  sprintf(buffer,"mv %s %s",filestring,filestring2);
  /* The man page for "system" doth sayeth that 
     "Exit status 127 (may be displayed as "32512") indicates
     the shell could not be executed."  */
  if (system(buffer) == 127) 
    return(False);
  XtWarning("Backup done.");
  return(True);
}
  

static void NS_Write_Member(fp,current)
     FILE *fp;
     NodeServiceListMember *current;
{
  char *type_string;

  /* assuming 32 characters for node name
              16 characters for type
	      32 characters for label 
	      32 characters for location
     (more allowed) */

#ifdef TEMPORARY_NODES
  if (current->ns_status == NSNodeIsTemporary)
    return;

#endif
  type_string = NodeService_Get_String_From_Type(current->node_type);
  if (AGstrlen(current->node_name) <= 32 &&
      AGstrlen(type_string) <= 16 &&
      AGstrlen(current->label) <= 32 &&
      AGstrlen(current->location) <= 32)
    fprintf(fp,"%-32s#%-16s#%-32s#%s\n",
	    current->node_name,
	    type_string,
	    current->label,
	    current->location);
  else
    fprintf(fp,"%s\t\t#%s\t\t#%s\t\t\t#%s\n",
	    current->node_name,
	    type_string,
	    current->label,
	    current->location);
}



static Boolean NS_Write_All_Members(fp)
     FILE *fp;
{
  NodeServiceListMember *current;
  char buffer[MaxString];
  
  if ((current = NS_Find_First_Member()) == NULL)
    {
      return(False);
    }
  
  while (current)
    {
      NS_Write_Member(fp,current);
      current = NS_Find_Next_Member(current);
    }
  return(True);
}

  

/*********************************

  "PUBLIC" ROUTINES

*********************************/

/* 10/29/91:  Moved out of NodeService_Initialize so it can
   become recursive for #include's */
static int NS_Initialize(n,current_location,prev_node)
     int *n;
     char *current_location;
     NodeServiceListMember **prev_node;
{
  FILE *fp;
  char buffer[MaxString];
  int len;
  char include_location[MaxString];
  NodeServiceListMember *current_node;
  int rc;

  if ((fp = NS_Open_File("r",current_location)) == NULL)
     {
	return(-1);
     }
  while (fgets(buffer,MaxString,fp) != NULL)
     {
	  /* allow comments lines that start with ! like resource files */
	if (buffer[0] == '!')
	   continue;
	len = AGstrlen(buffer);
	buffer[--len] = EOS; /* get rid of \n */
	if ((rc = 
	     Parse_NodeService_Buffer(buffer,&current_node,
				      current_location,include_location)) 
	    < 0)
	   return(rc);
	if (rc > 0) /* not blank line */
	  {
	    if (rc == 2 && include_location != NULL &&
		include_location[0] != EOS) /* include directive */
	      NS_Initialize(n,include_location,prev_node);
	    else
	      {
		if (*prev_node)
		  (*prev_node)->next = current_node;
		else
		  nodeService_globals.nodeService_root = current_node;
		*prev_node = current_node;
		(*n)++;
	      }
	  }
      }
  NS_Close_File(fp);
  return(1);
}


int NodeService_Initialize()
{
  char filestring[MaxString];
  int rc;
  int n;
  char string2[MaxString];
  NodeServiceListMember *prev_node;


  nodeService_globals.default_node_token = 
      NS_Get_Type_Token(nodeService_globals.default_node_string);
  nodeService_globals.no_action_token = 
      NS_Get_Type_Token(nodeService_globals.no_action_string);
  
  
  nodeService_globals.no_action_node = -1;
  

    /* Get tokens for all of the global_node_types */
  for (n=0;n<global_info.n_global_node_types;n++)
    {
      if (global_node_types[n]->name)
	global_node_types[n]->token = 
	  NS_Get_Type_Token(global_node_types[n]->name);
      else
	global_node_types[n]->token = (AGTypeToken)NULL;  /*better not happen*/
      if (global_node_types[n]->token == 
	  nodeService_globals.default_node_token)
	  nodeService_globals.default_node = n;

    }
  
  n = -1;
  prev_node = NULL;
  /* 10/29/91:  start off reading the AGNodeFile in the
     directory specified by the Directory resource (hence the
     NULL) */
  if (NS_Initialize(&n,NULL,&prev_node) < 0)
    return(-1);
  return(n);
}


NodeInfoPtr NodeService_Add_Node(node_name,label,type,location)
     char *node_name;
     char *label;
     AGNodeTypes type;
     char *location;
{
    NodeServiceListMember *current;
    char filestring[MaxString];
    FILE *fp;

    if (node_name == NULL || node_name[0] == EOS || 
	AGstrlen(node_name) >= MaxString)
      {
	Util_Debug("NodeService_Add_Node:  invalid name");
	return(NULL);
      }

    if ((current = NS_Add_Node_To_List(node_name,label,type,location))
	== NULL)
      return(NULL);

      /* Open up the AGNodeServiceFile, but only in the current
	 directory */
    if ((fp = NS_Open_File("a",NULL)) == NULL)
      {
	return(NULL);
      }
    NS_Write_Member(fp,current);
    NS_Close_File(fp);

    return(current);
}

#define NSReplaceElement(old_string,replace_string) \
                            XtFree(old_string); \
                            old_string = XtNewString(replace_string);

#define NSCheckMacro(node_info) \
    (node_info != NULL && node_info->check_field == NSCheckField)


int NodeService_Replace_Node(node_info,node_name,label,type,location)
     NodeInfoPtr node_info;
     char *node_name;
     char *label;
     AGNodeTypes type;
     char *location;
{
    char buffer[MaxString];
    FILE *fp;

    if (!NSCheckMacro(node_info))
      {
	Util_Debug("NodeService_Replace_Node: invalid node info");
	return(-1);
      }
    
    NSReplaceElement(node_info->node_name,node_name);
    NSReplaceElement(node_info->label,label);
    NSReplaceElement(node_info->location,location);
    node_info->node_type = type;  /* shouldn't be replacing this, tho */
    if (!NS_Backup_File())
      {
	XtWarning("node service has new information but it has not been \
written out.  Continuing....");
	return(-1);
      }
    /* changed from "a" to "w" 9/3/91 ****/
    /* 10/29/91:  changed call to open AGNodeFile in Directory directory */
    if ((fp = NS_Open_File("w",NULL)) == NULL)
      {
	XtWarning("node service has new information but it has not been \
written out.  Continuing....");
	return(-1);
      }
    NS_Write_All_Members(fp);
    NS_Close_File(fp);
    return(1);
}      
  
#undef NSReplaceElement

int NodeService_Delete_Node(node_info)
     NodeInfoPtr node_info;
{
    char buffer[MaxString];
    FILE *fp;

    if (!NSCheckMacro(node_info))
      {
	Util_Debug("NodeService_Delete_Node: invalid node info");
	return(-1);
      }

    /* refuse to delete an open node */
    if (node_info->file_info)
      return(-2);
    
    NS_Delete_Node_From_List(node_info);

    if (!NS_Backup_File())
      {
	XtWarning("node service has new information but it has not been \
written out.  Continuing....");
	return(-1);
      }
    /* 10/29/91:  using AGNodeFile in Directory directory */
    if ((fp = NS_Open_File("w",NULL)) == NULL)
      {
	XtWarning("node service has new information but it has not been \
written out.  Continuing....");
	return(-1);
      }
    NS_Write_All_Members(fp);
    NS_Close_File(fp);
    return(1);
}      
  
#ifdef TEMPORARY_NODES
NodeInfoPtr NodeService_Add_Temporary_Node(node_name,label,type,location)
     char *node_name;
     char *label;
     AGNodeTypes type;
     char *location;
{
    NodeServiceListMember *current;
    char filestring[MaxString];

    if (node_name == NULL || node_name[0] == EOS || 
	AGstrlen(node_name) >= MaxString)
      {
	Util_Debug("NodeService_Add_Node:  invalid name");
	return(NULL);
      }

    if ((current = NS_Add_Node_To_List(node_name,label,type,location))
	== NULL)
      return(NULL);

    current->ns_status = NSNodeIsTemporary;

    return(current);
}

#define NSReplaceElement(old_string,replace_string) \
                            XtFree(old_string); \
                            old_string = XtNewString(replace_string);



int NodeService_Replace_Temporary_Node(node_info,node_name,label,type,location)
     NodeInfoPtr node_info;
     char *node_name;
     char *label;
     AGNodeTypes type;
     char *location;
{
    char buffer[MaxString];
    FILE *fp;

    if (!NSCheckMacro(node_info))
      {
	Util_Debug("NodeService_Replace_Node: invalid node info");
	return(-1);
      }
    
    NSReplaceElement(node_info->node_name,node_name);
    NSReplaceElement(node_info->label,label);
    NSReplaceElement(node_info->location,location);
    node_info->node_type = type;  /* shouldn't be replacing this, tho */
    return(1);
}      
  
#undef NSReplaceElement

int NodeService_Delete_Temporary_Node(node_info)
     NodeInfoPtr node_info;
{
    char buffer[MaxString];
    FILE *fp;

    if (!NSCheckMacro(node_info))
      {
	Util_Debug("NodeService_Delete_Node: invalid node info");
	return(-1);
      }

    /* refuse to delete an open node */
    if (node_info->file_info)
      return(-2);
    
    NS_Delete_Node_From_List(node_info);

    return(1);
}      
#endif  

int NodeService_Request_Fields(node_info,node_name,label,type,location)
     NodeInfoPtr node_info;
     char **node_name;
     char **label;
     AGNodeTypes *type;
     char **location;
{
    NodeServiceListMember *current;
    char buffer[MaxString];


  if (!NSCheckMacro(node_info))
    {
      Util_Debug("NodeService_Request_Fields: invalid node info");
      return(-1);
    }


    *label = node_info->label;
    *location = node_info->location;
    *type = node_info->node_type;
    *node_name = node_info->node_name;
    return(1);
}


int NodeService_Request_By_Name(node_name,node_info,label,type,location)
     char *node_name;
     NodeInfoPtr *node_info;
     char **label;
     AGNodeTypes *type;
     char **location;
{
    NodeServiceListMember *current;
    char buffer[MaxString];

      /* 7/4/91 - make sure node name isn't messed up.  Check
	 whether it's the NULL pointer, an empty string, or way
	 too long (probably garbage or not null terminated).  This
	 to keep it from core-dumping later */
    if (node_name == NULL || node_name[0] == EOS || 
	AGstrlen(node_name) >= MaxString)
      {
	Util_Debug("NodeService_Request_By_Name:  invalid name");
	return(-1);
      }

    if ((current = NS_Match_Member_Name(node_name)) != NULL)
      {
	*label = current->label;
	*location = current->location;
	*type = current->node_type;
	*node_info = current;
	return(1);
      }

    /* missing node */
    *label = nodeService_globals.no_action_string;
    *location = nodeService_globals.no_action_string;
    *type = nodeService_globals.no_action_node;
    *node_info = NULL;
    return(-2);
}
    
char *NodeService_Request_Node_Label(node_name)
     char *node_name;
{
    NodeServiceListMember *current;
    int len;

    if (node_name == NULL || node_name[0] == EOS || 
	AGstrlen(node_name) >= MaxString)
      {
	Util_Debug("NodeService_Request_Node_Label:  invalid name");
	return("Invalid Name");
      }

    if ((current = NS_Match_Member_Name(node_name)) != NULL)
      return(current->label);
    /* missing node */
    return(nodeService_globals.no_action_string);
}
    
Boolean NodeService_Check_NodeInfo(node_info)
     NodeInfo *node_info;
{
  return(NSCheckMacro(node_info));
}


/******************************

  BOOK KEEPING ROUTINES

******************************/


void NodeService_Register_Open_Node(node_info,shell,editFile_info,
				      editNIP_info)
     NodeInfo *node_info;
     Widget shell;
     EditInfoPtr editFile_info;
     EditInfoPtr editNIP_info;
{
  Widget edit_shell;

  if (!NSCheckMacro(node_info))
    {
      Util_Debug("NodeService_Register_Open_Node: invalid node info");
      return;
    }

  if (node_info->file_info)
    FileService_Register_Open_Node(node_info->file_info,shell,editFile_info,
				   editNIP_info);
  else /* a non-FileService node */
    node_info->file_info =
      FileService_Register_Open_NonFS_Node(node_info,shell,
					   editFile_info,editNIP_info);
  if (shell != NULL)
    AG_Register_Open_Node(shell);
  if (editNIP_info != NULL)
    if ((edit_shell = Edit_Get_EditNIP_Shell(editNIP_info)) != NULL)
      AG_Register_Open_Node(edit_shell);
  if (editFile_info != NULL)
    if ((edit_shell = Edit_Get_EditFile_Shell(editFile_info)) != NULL)
      AG_Register_Open_Node(edit_shell);

}


  

void NodeService_Register_Closed_Node(node_info,shell,editFile_info,
				      editNIP_info)
     NodeInfo *node_info;
     Widget shell;
     EditInfoPtr editFile_info;
     EditInfoPtr editNIP_info;
{
  if (!NSCheckMacro(node_info))
    {
      Util_Debug("NodeService_Register_Closed_Node: invalid node info");
      return;
    }

  if (node_info->file_info) /* may have already been closed */
    if (FileService_Register_Closed_File(node_info->file_info,
					 shell))
      node_info->file_info = NULL; /* last one closed */

}

void NodeService_Register_Closed_Edit(node_info,edit_info)
     NodeInfo *node_info;
     EditInfoPtr edit_info;
{
  if (!NSCheckMacro(node_info))
    {
      Util_Debug("NodeService_Register_Closed_Node: invalid node info");
      return;
    }

  if (node_info->file_info) /* may have already been closed */
    if (FileService_Register_Closed_Edit(node_info->file_info,
					 edit_info))
      node_info->file_info = NULL; /* last one closed */

}



void NodeService_Register_Open_File(node_info,file_info)
     NodeInfo *node_info;
     FileInfo *file_info;
{
  if (NSCheckMacro(node_info))
    {
      node_info->file_info = file_info;    
    }
  else
    Util_Debug("NodeService_Register_Open_File: invalid parameters");
  
}

/************************************

  FILE I/O FOR NODES

************************************/

Boolean NodeService_Is_Node_Type_Without_Headers(type)
     AGNodeTypes type;
{
  /* purpose:  determine whether the node type does not have AGS
     headers and the file is a non-AGS file (eg, for Ascii-Nodes) */
  if (type <=   global_info.n_global_node_types)
    return(global_node_types[type]->get_file_immediate);
  return(False);
}
      


Boolean NodeService_Open_File(node_info,filename)
     NodeInfoPtr node_info;
     char *filename;
{
  if (NSCheckMacro(node_info))
    {

      if (NodeService_Is_Node_Type_Without_Headers(node_info->node_type) 
	  == False)
	{
	  if ((node_info->file_info = AG_File_Open(node_info,filename)) !=
	      NULL)
	    return(True);
	  else
	    return(False);
	}
      else
	{
	  if ((node_info->file_info = 
	       FileService_Open_Ascii(node_info,filename)) != NULL)
	    return(True);
	  else
	    return(False);
	}
    }
  return(False);
}


#ifdef NOPE
Boolean NodeService_Open_New_File(node_info)
     NodeInfoPtr node_info;
{
  if (NSCheckMacro(node_info))
    {
      if ((node_info->file_info = AG_File_Open_NewFile(node_info)) !=
	  NULL)
	return(True);
    }
  return(False);
}
#endif

Boolean NodeService_Save_File(node_info,text_buffer)
     NodeInfoPtr node_info;
     char *text_buffer;
{
  if (NSCheckMacro(node_info))
    {
      return(AG_File_Save(node_info->file_info,text_buffer));
    }
  return(False);
}


/* Set the defined headers (ie, node expert and author) */
Boolean NodeService_Set_Defined_Headers(node_info,header_values)
     NodeInfo *node_info;
     char **header_values;
{
  if (!NSCheckMacro(node_info)) 
    { 
      Util_Debug("NodeService Set Defined Headers: invalid node info"); 
      return(False);
    } 
  return(FileService_Set_Defined_Headers(node_info->file_info,
					 header_values));
}

Boolean NodeService_Save_New_File(node_info,filename,header_values,text_buffer)
     NodeInfoPtr node_info;
     char *filename;
     char **header_values;
     char *text_buffer;
{
  if (NSCheckMacro(node_info))
    {
      if ((node_info->file_info = AG_File_Open_NewFile(node_info)) !=
	  NULL)
	{
	  AG_File_Filename_Set(node_info->file_info,filename);
	  if (FileService_Set_Defined_Headers(node_info->file_info,
					 header_values))
	    return(AG_File_Save(node_info->file_info,text_buffer));
	  else
	    return(False);
	}
    }
  return(False);
}

/* This is a routine to close a non-registered file.  Why should
   there be a non-registered file?  Only when you have opened
   it to write it back out without displaying it.  */
void NodeService_Close_File(node_info)
     NodeInfoPtr node_info;
{
  if (NSCheckMacro(node_info))
    {
      AG_File_Close(node_info->file_info);
      node_info->file_info = NULL;  /* do it in any case */
    }
}


/**************************************************

  Get/Set ROUTINES FOR DATA STRUCTURES

***************************************************/


/*
  There are 2 sets of routines:

  1.  Routines to retrieve the values of a specific node given
      a NodeInfo pointer.
  2.  Routines to retrieve the values of a specific file given
      a NodeInfo pointer.

  Node information = { node_name, label, type, and location}

  The NodeService hands the FileService any request for (2).
*/


/* Note to any readers:  If you have a better way to do this
   (and stay in C), let me know. -- Ack */



#define NSGetMacro(Field,field,dead_value)    \
  if (!NSCheckMacro(node_info)) \
     { \
        Util_Debug("NodeService Get Field: invalid node info"); \
        return(dead_value); \
      } \
  return(node_info->field); 


#define NSGetFSMacro(Field,field,dead_value)    \
  if (!NSCheckMacro(node_info)) \
    { \
      Util_Debug("NodeService pass to FileService: invalid node info"); \
      return(dead_value); \
    } 




char *NodeService_Get_Location(node_info)
     NodeInfo *node_info;
{
  NSGetMacro(Location,location,NULL);
}


char *NodeService_Get_Label(node_info)
     NodeInfo *node_info;
{
  NSGetMacro(Label,label,NULL);
}

char *NodeService_Get_Node_Name(node_info)
     NodeInfo *node_info;
{
  NSGetMacro(Node_Name,node_name,NULL);
}

AGNodeTypes NodeService_Get_Type(node_info)
     NodeInfo *node_info;
{
  NSGetMacro(Type,node_type,((AGNodeTypes)NULL));
}

Boolean NodeService_Has_Node_Changed(node_info)
     NodeInfo *node_info;
{
  NSGetFSMacro(Dirty,dirty,((Boolean)NULL));
  return(FileService_Has_Node_Changed(node_info->file_info));
}

Widget NodeService_Get_Shell(node_info)
     NodeInfo *node_info;
{
  NSGetFSMacro(Shell,shell,NULL);
  return(FileService_Get_Shell(node_info->file_info));
}

char *NodeService_Get_Text(node_info)
     NodeInfo *node_info;
{
  NSGetFSMacro(Text,text,NULL);
  return(FileService_Get_Text(node_info->file_info));
}

char **NodeService_Get_Defined_Headers(node_info)
     NodeInfo *node_info;
{
  NSGetFSMacro(Defined_Headers,header_values,NULL);
  return(FileService_Get_Defined_Headers(node_info->file_info));
}

AG_Nonpredefined_Header *NodeService_Get_Nonpredefined_Headers(node_info)
     NodeInfo *node_info;
{
  NSGetFSMacro(Nonpredefined_Headers,nonpredefined_header_root,NULL);
  return(FileService_Get_Nonpredefined_Headers(node_info->file_info));
}


FileInfo *NodeService_Get_FileInfo(node_info)
     NodeInfo *node_info;
{
  NSGetMacro(FileInfo,file_info,NULL);
}

/* These are just plucking the field out of the file info struct,
   so I'm not calling a routine to get them. */
EditInfo *NodeService_Get_EditFileInfo(node_info)
     NodeInfo *node_info;
{
  NSGetFSMacro(EditFileInfo,editFile_info,NULL);
  return(FileService_Get_EditFileInfo(node_info->file_info));
}

/* These are just plucking the field out of the file info struct,
   so I'm not calling a routine to get them. */
EditInfo *NodeService_Get_EditNIPInfo(node_info)
     NodeInfo *node_info;
{
  NSGetFSMacro(EditNIPInfo,editNIP_info,NULL);
  return(FileService_Get_EditNIPInfo(node_info->file_info));
}

int NodeService_Free_Buffer(node_info)
     NodeInfo *node_info;
{
  NSGetFSMacro(FreeBuffer,buffer,((int)NULL));
  AG_File_Free_Buffer(node_info->file_info);
  return(1);
}



NodeInfoPtr NodeService_Find_Open_Node(shell)
     Widget shell;
{
  return(FileService_Find_Open_Node(shell));
}


