/***************************************************************************


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

  FileService.c    


  Todo:
     This ought to create a separate file object.  Not clear whether
        this should be an Xt Obj tho.

************************************************************************/


#include <stdio.h>
#include <X11/Intrinsic.h>
#include "AG.h"
#include "FileServ.h"

extern GlobalInfo global_info;

/* 8/3/91 MSA

   Some notes on the FileService.  

   The FileService is responsible for accessing physical storage.
   Right now, the FileService is in transition.  It was essentially
   an independent service, and only dealt with Unix flat files.

   It is moving to a dependent service of the NodeService, since
   the NodeService is in the best position to determine how to
   access any given node - since that information is in the 
   possession of the NodeService structures.

   Eventually, any publicly accessible routines should be called
   only from NodeService routines.

*/



/*   
  8/3/91  MSA

  This all assumes that the files are Answer Garden files.  Each
  Answer Garden (AG) file looks like:

         header info
	 body of file

  where the header info starts with a 
  
         @begin(header)

  and ends with a 
  
         @end(header)

  Header info consists of

        label: value

  where "label" is any arbitrary string and "value" is also any
  arbitrary string.  The delimiter ':' is hard-wired right now, but
  shouldn't be.

  Header info comes in two flavors, pre-defined and not pre-defined.
  Predefined headers are given in the array called "header_array" 
  (defined below).  These routines write out these strings and read
  in the same strings.  Only they need to know what the strings are;
  all other parts of AG refer them with the symbolic names provided
  in AG.h.  They include author, modification number, node expert,
  last modification date, and so on.  See AG.h for the ones currently
  defined.  They are stored in file_info->header_values for any
  open file.
  
  Anyone can include any header info (label, value) pair in any AG
  file.  These will be read into and written from a linked list starting at
  file_info->nonpredefined_header_root.  

  Nothing in the FileService currently does anything with the header
  values per se.  Many parts of AG use the predefined header values.
  New node types can use the non-predefined header values as they
  wish - that is their function - to provide extensibility for new
  node types.

*/


typedef struct _FileInfo
{
  char check_field;          /* always set to FSCheckField if valid FileInfo */
  struct _FileInfo *next;
  struct _FileInfo *prev;
  NodeInfoPtr node_info;     /* back pointer to node information */
  EditInfoPtr editFile_info; /* pointer to first editing info */
  EditInfoPtr editNIP_info;  /* pointer to second editing info */
  Widget shell;   
  Boolean dirty;             /* has file been written out since last read */
  char *filename;            /* filename of physical file */
  char *buffer;              /* entire buffer of file that has been read */
  char *text;                /* ptr to body (after header info) of buffer */
  int buffer_length;
  char *header_values[NFileServHeaderValues];
  AG_Nonpredefined_Header *nonpredefined_header_root;
};







FileInfo *root_file_info = NULL;
FileInfo *last_file_info = NULL;


static char *FS_Handle_Header();
char *Util_Remove_WhiteSpace_Until_LineEnd();
void AG_File_Partial_Close();

#define HeaderDelimiter ':'
#define FSCheckField 'f'

#define FSCheckMacro(file_info)   \
  (file_info != NULL && file_info->check_field == FSCheckField)


   /* Concatenate the absolute location of the files to the
      filename. */
void Form_Filename(filestring,filename)
     char *filestring;
     char *filename;
{
  /* if a rooted pathname was specified, just use that */
  /* SAO Mod */
  if ( *filename == '/' || *filename == '.' )
    AGstrcpy(filestring,filename);
  else
    {
      AGstrcpy(filestring,global_info.directory);
      strcat(filestring,filename);
    }

}


   /* This should get re-implemented on slower systems that have
      a system to call to check for a file's existance. */
Boolean Check_Node_Physical_Existance(location)
     char *location;
{
    char filename[MaxString];
    FILE *fp;

    Form_Filename(filename,location);
    if ((fp = fopen(filename,"r")) == NULL)
	{
	    return(False);
	}
    fclose(fp);
    return(True);
}


  /* There must be some system call for this ***/
Boolean AG_File_Check_Write_Permission(location)
     char *location;
{
    char filename[MaxString];
    FILE *fp;

    Form_Filename(filename,location);
    if ((fp = fopen(filename,"a")) == NULL)
	{
	    return(False);
	}
    fclose(fp);
    return(True);
}

static void FileInfo_Attach(current)
     FileInfo *current;
{
  if (last_file_info == NULL) /* first one being made */
    {
      current->prev = NULL;
      last_file_info = current;
      root_file_info = current;
    }
  else
    {
      last_file_info->next = current;
      current->prev = last_file_info;
      last_file_info = current;
    }
}

static void FileInfo_Detach(current)
     FileInfo *current;
{
  if (root_file_info == current)  /* Is it the first? */
    {
      if (last_file_info == current) /* Is it the only? */
	{
	  root_file_info = NULL;
	  last_file_info = NULL;
	}
      else
	{
	  root_file_info = current->next;
	  root_file_info->prev = NULL;
	}
    }
  else /* not the first */
    {
      if (last_file_info == current) /* Is it the last? */
	{
	  last_file_info = current->prev;
	  last_file_info->next = NULL;
	}
      else
	{
	  current->next->prev = current->prev;
	  current->prev->next = current->next;
	}
    }
}

static FileInfo *FileInfo_New(node_info,filename)
     NodeInfoPtr node_info;
     char *filename;
{
  FileInfo *fileInfo;
  int i;
  
  if ((fileInfo = (FileInfo *)XtMalloc(sizeof(FileInfo))) == NULL)
    {
      XtWarning("Unable to allocate memory in Answer Garden");
      return(NULL);
    }
  else
    {
      fileInfo->buffer = NULL;
      fileInfo->text = NULL;
      fileInfo->buffer_length = 0;
      
      for (i=0;i<NFileServHeaderValues;i++)
	fileInfo->header_values[i] = NULL;
      fileInfo->nonpredefined_header_root = NULL;

      fileInfo->filename = XtNewString(filename);
      fileInfo->node_info = node_info;
      FileInfo_Attach(fileInfo);
      fileInfo->next = NULL;
      fileInfo->shell = NULL;
      fileInfo->editFile_info = NULL;
      fileInfo->editNIP_info = NULL;
      fileInfo->dirty = False;
      fileInfo->check_field = FSCheckField;
    }
  return(fileInfo);
}


FileInfo *AG_File_Open_NewFile(node_info)
     NodeInfoPtr node_info;
{
   FileInfo *fileInfo;
   
   return(fileInfo = FileInfo_New(node_info,NULL));
}


/* check file's existance should move into here to avoid extra file open */
static FileInfo *FS_Open_File_Internal(node_info,filename,is_ascii_file)
     NodeInfoPtr node_info;
     char *filename;
     Boolean is_ascii_file;
{
    FILE *fp;
    long pos;
    int file_length;
    int rc;
    FileInfo *fileInfo;
    char error_buffer[MaxString];
    int i;
    Boolean existing;
    char *text_ptr;

    if (filename == NULL)
      {
	XtWarning("Answer Garden FileService given a null filename. Continuing...");
	return(NULL);
      }

    if (!NodeService_Check_NodeInfo(node_info))
      {
	XtWarning("Unable to provide file service.  Continuing...");
	Util_Debug("AG_File_Open:  invalid node info"); 
	return(NULL);
      }

    existing = False;
    if ((fileInfo = NodeService_Get_FileInfo(node_info)) != NULL)
	if (FSCheckMacro(fileInfo))
	  existing = True;

    if (!existing)
      {
	if ((fileInfo = FileInfo_New(node_info,filename)) == NULL)
	  return(NULL);
      }
    else
      {
	if (fileInfo->buffer == NULL || fileInfo->text == NULL 
	    || fileInfo->dirty )
	  {
	    AG_File_Partial_Close(fileInfo);
	    fileInfo->dirty = False;
	  }
	else
	  return(fileInfo);
      }

    if ((fp = fopen(filename,"r")) == NULL)
	{   
	    /* No file?  Get upset. */
	    sprintf(error_buffer,
		    "Answer Garden cannot open file %s.  Continuing....",
		    filename);
	    XtWarning(error_buffer);
	    AG_File_Close(fileInfo);
	    return(NULL);
	}

    fseek(fp,0L,2);
    pos = ftell(fp);
    file_length = (int)pos;

    if ((fileInfo->buffer = XtMalloc((Cardinal)file_length+1)) == NULL)
      {
	AG_File_Close(fileInfo);
	return(NULL);
      }
    rewind(fp);

    if ((rc = fread(fileInfo->buffer,1,(int)file_length,fp)) 
	!= (int)file_length)
      {
	sprintf(error_buffer,
		"Answer Garden file i/o error (%d).  Continuing...",
		rc);
	XtWarning(error_buffer);
	AG_File_Close(fileInfo);
	return(NULL);
      }

    fclose(fp);
    fileInfo->buffer[file_length] = EOS;
    fileInfo->buffer_length = file_length;

    if (is_ascii_file)
      {
	if ((fileInfo->text = FS_Handle_Header(fileInfo)) == NULL)
	  {
	    AG_File_Close(fileInfo);
	    return(NULL);
	  }
      }
    else
      fileInfo->text = fileInfo->buffer; /* no headers on ascii file */

    /* This should go after everyone switches to NodeService_Open_File */
    NodeService_Register_Open_File(node_info,fileInfo);

    return(fileInfo);
}

FileInfo *AG_File_Open(node_info,filename)
     NodeInfoPtr node_info;
     char *filename;
{
  return(FS_Open_File_Internal(node_info,filename,True));
}

FileInfo *FileService_Open_Ascii(node_info,filename)
     NodeInfoPtr node_info;
     char *filename;
{
  /* send False, so headers not processed */
  return(FS_Open_File_Internal(node_info,filename,False));
}

/*******************************

  HEADER ROUTINES

*******************************/


/**** PRIVATE:  Initial Processing ****/


static void FS_Nonpredefined_Header_Close(file_info)
     FileInfo *file_info;
{
  AG_Nonpredefined_Header *tempptr;
  AG_Nonpredefined_Header *next;
  
  
  tempptr = file_info->nonpredefined_header_root;
  while (tempptr)
    {
      next = tempptr->next;
      if (tempptr->label)
	XtFree(tempptr->label);
      if (tempptr->value)
	XtFree(tempptr->value);
      XtFree((char *)tempptr);
      tempptr = next;
    }
}

static void FS_Predefined_Header_Close(file_info)
     FileInfo *file_info;
{
  int i;

  for (i=0;i<NFileServHeaderValues;i++)
    if (file_info->header_values[i])
      XtFree(file_info->header_values[i]);
}

  /* This is a pointer to the current end of the nonpredefined values
     for the header (see AG.h).  Why is it a global?  It was becoming
     a struct ***, which was ridiculous. */
static AG_Nonpredefined_Header **nonpredefined_values_ptr;

static AG_Nonpredefined_Header *FS_Nonpredefined_New(file_info,label,value)
     FileInfo *file_info;
     char *label;
     char *value;
{
  AG_Nonpredefined_Header *tempptr;
  if ((tempptr = (AG_Nonpredefined_Header *)
       XtMalloc(sizeof(AG_Nonpredefined_Header))) == NULL)
      return(NULL);
  tempptr->magic = (XtPointer)tempptr;
  tempptr->label = XtNewString(label);
  tempptr->value = XtNewString(value);
  tempptr->next = NULL;
  return(tempptr);
}
  


static FS_Do_Value(buffer,ptr,value_slot)
  char *buffer;
  char *ptr;
  char **value_slot;
{
  char temp_string[MaxString];

  Util_Remove_Leading_WhiteSpace(ptr,temp_string);
  *value_slot = XtNewString(temp_string);
}

static FS_Do_Nonpredefined_Value(file_info,buffer,value_ptr,label)
     FileInfo *file_info;
     char *buffer;
     char *value_ptr;
     char *label;
{
  AG_Nonpredefined_Header *tempptr;
  char value[MaxString];
  
  Util_Remove_Leading_WhiteSpace(value_ptr,value);
  if ((tempptr = FS_Nonpredefined_New(file_info,label,value))
      == NULL)
    return(-1);
  *nonpredefined_values_ptr = tempptr;
  nonpredefined_values_ptr = &(tempptr->next);
  return(1);
}


  /* This is the array to retrieve the values in the header correctly.
     This is an implicit order in that the header_values array of
     char ptrs in the fileinfo structure will contain the values for
     this header labels.

     See the #define's in the AG.h file.
  */
static char *header_array[NFileServHeaderValues] =
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
  

static FS_Do_Header_Line(fileInfo,buffer,header_values)
     FileInfo *fileInfo;
     char *buffer;
     char *header_values[NFileServHeaderValues];
{ 
  char *ptr;
  char token[MaxString];
  char token2[MaxString];
  char value_token[MaxString];
  char value_token2[MaxString];
  int i;
  int len;

  if ((ptr = AGindex(buffer,HeaderDelimiter)) == NULL)
    {
      XtWarning("broken header line in file.  continuing...\n");
      return;
    }
  
  len = (int)(ptr-buffer);
  Util_Move_Token_To_String(token,buffer,
			    (len >= MaxString) ? MaxString-1 : len);
  Util_Remove_WhiteSpace(token,token2);
  
  if ((i = Util_Search_Array(header_array,token2,NFileServHeaderValues)) < 0)
    FS_Do_Nonpredefined_Value(fileInfo,buffer,ptr+1,token2);
  else
    {
      if (header_values[i] != NULL)
	{
	  XtFree(header_values[i]);
	}
      FS_Do_Value(buffer,ptr+1,&(header_values[i]));
    }
}
  
  

#define  FS_Header_Begin "@begin(header)"
#define FS_Header_End "@end(header)"

static char *FS_Handle_Header(fileInfo)
     FileInfo *fileInfo;
{
  char *pos;
  char *next;
  char temp_buffer[MaxString];
  char *loc;
  char *temp_pos;

  /* look for the header */
  
  pos = Util_Remove_WhiteSpace_Until_LineEnd(fileInfo->buffer,temp_buffer);
  while (AGstrlen(temp_buffer) == 0)
     {
	temp_pos = pos + 1;
	pos = Util_Remove_WhiteSpace_Until_LineEnd(temp_pos,temp_buffer);
     }
  if (strcmplo("@begin(header)",temp_buffer))
	{
	  /* No header?  Get upset. */
	  XtWarning("missing header in Answer Garden.  Continuing...");
	  return(NULL);
	}

  nonpredefined_values_ptr = &(fileInfo->nonpredefined_header_root);
    /* Prowl through the file looking for the end of the
       header */
  loc = fileInfo->buffer + fileInfo->buffer_length;
  for (pos++ ; pos <= loc; pos++)
/*    if (*pos == '@' && *(pos+1) != '@') */
      {
	/* Take the hit for now of sequential search twice.  Needs to get
	   redone */
	temp_pos = Util_Remove_WhiteSpace_Until_LineEnd(pos,temp_buffer);  
	if (!strcmplo(FS_Header_End,temp_buffer))
	  break;
	FS_Do_Header_Line(fileInfo,pos,fileInfo->header_values);
	pos = temp_pos;
      }
  if (pos > loc)
    {
      XtWarning("Mangled header in Answer Garden.");
      return(NULL);
    }
  /* this is a kludge until I have a real parser */
  pos = temp_pos + 1;
  return(pos);
}

/***** PRIVATE:  routines for get/set **************/

static AG_Nonpredefined_Header *
  FS_Find_Nonpredefined(file_info,field_name)
     FileInfo *file_info;
     char *field_name;
{
  AG_Nonpredefined_Header *temp;

  if (field_name == NULL || field_name[0] == EOS)
    {
      Util_Debug("FS FindNonpredefined:  bad field name");
      return(NULL);
    }

  temp = file_info->nonpredefined_header_root;
  while (temp != NULL)
    {
      if (!AGstrcmp(temp->value,field_name))
	return(temp);
      temp = temp->next;
    }
  return(NULL);
}


static Boolean FS_Insert_Nonpredefined(file_info,field_name,value)
     FileInfo *file_info;
     char *field_name;
     char *value;
{
  AG_Nonpredefined_Header *temp;
  

  if (field_name == NULL || field_name[0] == EOS)
    {
      Util_Debug("FS InsertNonpredefined:  bad field name");
      return(False);
    }
  
  if ((temp = FS_Nonpredefined_New(file_info,field_name,value)) == NULL)
    return(False);

  file_info->nonpredefined_header_root = temp;
  return(True);
}
    
static Boolean FS_Delete_Nonpredefined(file_info,nptr)
     FileInfo *file_info;
     AG_Nonpredefined_Header *nptr;
{
  AG_Nonpredefined_Header *next;
  AG_Nonpredefined_Header *prev;
  AG_Nonpredefined_Header *temp;
  
  if (nptr == NULL)
    return(False);
  
  next = nptr->next;
  
  if (nptr == file_info->nonpredefined_header_root)
    {
      file_info->nonpredefined_header_root = next;
    }
  else
    {
      temp = file_info->nonpredefined_header_root;
      while (temp != nptr && temp)
	{
	  prev = temp;
	  temp = temp->next;
	}
      if (temp == NULL)
	{
	  Util_Debug("FS DeleteNonpredefined:  at end of list without match");
	  return(False);
	}
      prev->next = next;
    }
  XtFree((char *)nptr);
  return(True);
}

/******* PUBLIC:  get/set *********/
  
Boolean FileService_Get_Nonpredefined_Value(file_info,field_name,value)
     FileInfo *file_info;
     char *field_name;
     char **value;
{
  AG_Nonpredefined_Header *vptr;
  if (!FSCheckMacro(file_info))
    {
      Util_Debug("FS GetNonpredefinedValue:  bad file info");
    }
  else
    if ((vptr = FS_Find_Nonpredefined(file_info,field_name))
	== NULL)
      return(False);
    else
      {
	*value = vptr->value;
	return(True);
      }
      return(False);
}

AG_Nonpredefined_Header *
  FileService_Get_Nonpredefined_Struct(file_info,field_name,value)
     FileInfo *file_info;
     char *field_name;
     char **value;
{
  AG_Nonpredefined_Header *vptr;
  if (!FSCheckMacro(file_info))
    {
      Util_Debug("FS GetNonpredefinedStruct:  bad fileinfo ");
    }
  else
    return(vptr = FS_Find_Nonpredefined(file_info,field_name));
}

Boolean FileService_Set_Nonpredefined_By_Magic(file_info,magic,value,
					    replace_only)
     FileInfo *file_info;
     XtPointer magic;
     char *value;
     Boolean replace_only;
{
  AG_Nonpredefined_Header *temp =  
    (AG_Nonpredefined_Header *) magic;

  if (!FSCheckMacro(file_info))
    {
      Util_Debug("FS SetNonpredefinedByMagic:  bad file info");
    }
  else
    if (temp->magic != magic)
      {
	Util_Debug("FS SetNonpredefinedByMagic:  bad magic");
	return(False);
      }
    else
      {
	Util_Replace_String(&temp->value,value);
	return(True);
      }
  return(False);
}

Boolean FileService_Set_Nonpredefined_By_Name(file_info,field_name,value,
					    replace_only)
     FileInfo *file_info;
     char *field_name;
     char *value;
     Boolean replace_only;
{
  AG_Nonpredefined_Header *vptr;
  if (!FSCheckMacro(file_info))
    {
      Util_Debug("FS SetNonpredefinedByName:  bad file info");
    }
  else
    if ((vptr = FS_Find_Nonpredefined(file_info,field_name))
	== NULL)
      {
	if (!replace_only)
	  {
	    FS_Insert_Nonpredefined(file_info,field_name,value);
	    return(True);
	  }
	else
	  {
	    Util_Debug("FS SetNonpredefinedByName:  create on replace only");
	    return(False);
	  }
      }
    else
      {
	Util_Replace_String(&vptr->value,value);
	return(True);
      }
  return(False);
}


Boolean FileService_Delete_Nonpredefined_By_Name(file_info,field_name)
     FileInfo *file_info;
     char *field_name;
{
  AG_Nonpredefined_Header *vptr;
  if (!FSCheckMacro(file_info))
    {
      Util_Debug("FS DeleteNonpredefinedByName:  bad file info");
    }
  else
    if ((vptr = FS_Find_Nonpredefined(file_info,field_name))
	== NULL)
      {
	Util_Debug("FS DeleteNonpredefinedByName:  can't find record");
	return(False);
      }
    else
      {
	FS_Delete_Nonpredefined(file_info,vptr);
	return(True);
      }
  return(False);
}

/* Routines defined below

char **FileService_Get_Defined_Headers(file_info)

AG_Nonpredefined_Header 
  *FileService_Get_Nonpredefined_Headers(file_info)

*/

Boolean FileService_Set_Defined_Headers(file_info,header_values)
     FileInfo *file_info;
     char **header_values;
{
  int i;
  if (FSCheckMacro(file_info) && header_values != (char **)NULL)
    {
      for (i=0;i< NFileServHeaderValues;i++)
	Util_Replace_String(&file_info->header_values[i],header_values[i]);
      return(True);
    }
  else
    {
      Util_Debug("FS SetDefinedHeaders:  bad fileinfo or null header values ");
      return(False);
    }
}





/*****************************

  MISC ROUTINES:  Should be 
  used only by NodeService
  routines

*****************************/
void AG_File_Free_Buffer(fileInfo)
     FileInfo *fileInfo;
{
  if (!FSCheckMacro(fileInfo))
    {
      Util_Debug("AG_File_Free_Buffer:  invalid file info");
      return;
    }

  if (fileInfo->buffer)
    XtFree(fileInfo->buffer);
  /* mark text as bogus */
  fileInfo->buffer = NULL;
  fileInfo->text = NULL;
}


AG_File_Filename_Set(fileInfo,filename)
     FileInfo *fileInfo;
     char *filename;
{
  if (!FSCheckMacro(fileInfo))
    {
      Util_Debug("AG_File_Filename_Set:  invalid file info");
      return;
    }

  if (filename)
    fileInfo->filename = XtNewString(filename);
  else
    Util_Debug("AG_File_Filename_Set:  null filename");
}


  /* If a node show up without a header, then I own
     it. */
static char *special_conversion_array[] =
{
  "Mark S. Ackerman",
  "ShowNone",
  "MIT Center for Coordination Science",
  "ack@athena.mit.edu",
  "12/31/93",
  "Mark S. Ackerman",
  "6/1/91",
  "1",
};
  

static File_Header_Conversion(fileInfo)
     FileInfo *fileInfo;
{
#ifdef NOPE
  "author",
  "show_author",
  "author_organization",
  "node_expert",
  "expiration_date",
  "last_modifier",
  "last_mod_date",
  "mod_num",

#endif
  int i;
  
  for (i=0;i<NFileServHeaderValues;i++)
   fileInfo->header_values[i] = XtNewString(special_conversion_array[i]);

}


static File_Save_Header(fp,fileInfo)
     FILE *fp;
     FileInfo *fileInfo;
{
   int j;
   int i;
   int mod_num;
   char date[MaxString];
   char temp[MaxString];


   j = 0;
   for (i=0;i<NFileServHeaderValues;i++)
      if (fileInfo->header_values[i])
	 j++;
   if (j == 0)
      File_Header_Conversion(fileInfo);
   
   Util_Get_Date(date);
   Util_Replace_String(&(fileInfo->header_values[FileServHeaderLastModDate]),
		       date);

   /* up the mod count */
   if (fileInfo->header_values[FileServHeaderModNum])
      {
	 j = atoi(fileInfo->header_values[FileServHeaderModNum]);
	 sprintf(temp,"%d",++j);
	 Util_Replace_String(&(fileInfo->header_values[FileServHeaderModNum]),
			     temp);
      }
   else /* must make it an allocated string for the destroy routine */
      fileInfo->header_values[FileServHeaderModNum] = XtNewString("1");

   Util_Get_UserName_With_Machine(temp);
   if (fileInfo->header_values[FileServHeaderLastModifier])
	Util_Replace_String(&(fileInfo->header_values[FileServHeaderLastModifier]),
			     temp);
   else
      fileInfo->header_values[FileServHeaderLastModifier] = XtNewString(temp);

   for (i=0;i<NFileServHeaderValues;i++)
      if (fileInfo->header_values[i])
	 fprintf(fp,"%s: %s\n",header_array[i],fileInfo->header_values[i]);
   
}


Boolean AG_File_Save(fileInfo,body_buffer)
     FileInfo *fileInfo;
     char *body_buffer;
{
  FILE *fp;
  AG_Nonpredefined_Header *next_ptr;
  char error_buffer[MaxString];

  if (!FSCheckMacro(fileInfo))
    {
      Util_Debug("AG_File_Save:  invalid file info");
      return(False);
    }

  if ((fp = fopen(fileInfo->filename,"w")) == NULL)
    {   
      /* No file?  Get upset. */
      sprintf(error_buffer,
	      "Answer Garden cannot open file %s.  Continuing....",
	      fileInfo->filename);
      XtWarning(error_buffer);
      /* AG_File_Close(fileInfo); */ /* Don't wipe out the node info tho */
      return(False);
    }
  
  fprintf(fp,"@begin(header)\n");

  File_Save_Header(fp,fileInfo);
  
  next_ptr = fileInfo->nonpredefined_header_root;
  while (next_ptr)
    {
      fprintf(fp,"%s: %s\n",next_ptr->label,next_ptr->value);
      next_ptr = next_ptr->next;
    }
  fprintf(fp,"@end(header)\n");

  fwrite(body_buffer,1,AGstrlen(body_buffer),fp);

  fclose(fp);

#ifdef OLD
  if (fileInfo->buffer != NULL)
    {
      fileInfo->dirty = True;
      AG_File_Free_Buffer(fileInfo);
    }
#endif
  fileInfo->dirty = True;

  return(True);
}


static void AG_File_Common_Close(fileInfo)
     FileInfo *fileInfo;
{
  AG_Nonpredefined_Header *temp_ptr;
  AG_Nonpredefined_Header *next_ptr;
  int i;

  if (!FSCheckMacro(fileInfo))
    {
      Util_Debug("AG_File_Partial_Close:  invalid file info");
      return;
    }

  FS_Nonpredefined_Header_Close(fileInfo);
  FS_Predefined_Header_Close(fileInfo);
}

void AG_File_Partial_Close(file_info)
     FileInfo *file_info;
{
  int i;
  AG_File_Common_Close(file_info);
  for (i=0;i<NFileServHeaderValues;i++)
    file_info->header_values[i] = NULL;
  file_info->nonpredefined_header_root = NULL;
}
  

void AG_File_Close(fileInfo)
     FileInfo *fileInfo;
{
  AG_Nonpredefined_Header *temp_ptr;
  AG_Nonpredefined_Header *next_ptr;
  int i;

  if (!FSCheckMacro(fileInfo))
    {
      Util_Debug("AG_File_Close:  invalid file info");
      return;
    }

  if (fileInfo->shell || fileInfo->editFile_info 
      || fileInfo->editNIP_info)
    return;

  AG_File_Free_Buffer(fileInfo);

  if (fileInfo->filename)
    XtFree(fileInfo->filename);

  AG_File_Common_Close(fileInfo);

  FileInfo_Detach(fileInfo);

  XtFree((char *)fileInfo);
}

/******************************

  BOOK KEEPING ROUTINES

******************************/



void FileService_Register_Open_Node(file_info,shell,editFile_info,
				    editNIP_info)
     FileInfo *file_info;
     Widget shell;
     EditInfoPtr editFile_info;
     EditInfoPtr editNIP_info;
{
  if (FSCheckMacro(file_info))
    {
      if (shell != NULL)
	file_info->shell = shell;
      if (editFile_info)
	file_info->editFile_info = editFile_info;
      if (editNIP_info)
	file_info->editNIP_info = editNIP_info;
    }
  else 
    Util_Debug("FS_Register_Open_Node:  invalid fileinfo");
}


FileInfo *FileService_Register_Open_NonFS_Node(node_info,shell,editFile_info,
					  editNIP_info)
     NodeInfo *node_info;
     Widget shell;
     EditInfoPtr editFile_info;
     EditInfoPtr editNIP_info;
{
  FileInfo *file_info;
  if ((file_info = AG_File_Open_NewFile(node_info)) == NULL)
    {
      Util_Debug("NService_Register_Open_NonFS_Node: can't create fileinfo");
      return(NULL);
    }

  FileService_Register_Open_Node(file_info,shell,NULL,NULL);
  return(file_info);
}

NodeInfo *FileService_Find_Open_Node(shell)
     Widget shell;
{
  FileInfo *tempptr;
  if (root_file_info == NULL || shell == NULL)
    return(NULL);
  tempptr = root_file_info;
  while (tempptr)
    if (tempptr->shell == shell)
      return(tempptr->node_info);
    else
      tempptr = tempptr->next; /**092092**/

  return(NULL);
}

Boolean FileService_Register_Closed_File(file_info,shell)
     FileInfo *file_info;
     Widget shell;
{
  if (FSCheckMacro(file_info))
    {
      if (shell != NULL)
	file_info->shell = NULL;
      if (file_info->shell || file_info->editFile_info || 
	  file_info->editNIP_info)
	;
      else
	{
	  AG_File_Close(file_info);
	  return(True);
	}
    }
  else 
    Util_Debug("FS_Register_Closed_Node:  invalid fileinfo");
  return(False);
}

Boolean FileService_Register_Closed_Edit(file_info,edit_info)
     FileInfo *file_info;
     EditInfoPtr edit_info;
{
  if (FSCheckMacro(file_info))
    {
      if (edit_info)
	{
	  if (file_info->editFile_info == edit_info)
	    file_info->editFile_info = NULL;
	  else if (file_info->editNIP_info == edit_info)
	    file_info->editNIP_info = NULL;
	  else
	    {
	      Util_Debug("FS Register Closed Edit:  can't find editinfo");
	      return(False);
	    }
	}
      if (file_info->shell || file_info->editFile_info || 
	  file_info->editNIP_info)
	;
      else
	{
	  AG_File_Close(file_info);
	  return(True);
	}
    }
  else 
    Util_Debug("FS RegisterClosedFile:  invalid fileinfo");
  return(False);
}


/***********************************

  GET routines

***********************************/

#define FSGetMacro(field,dead_value) \
  if (FSCheckMacro(file_info)) \
    { \
      return(file_info->field); \
    } \
  else \
    { \
      Util_Debug("FileService Get field:  invalid fileinfo"); \
      return(dead_value); \
    }
      

char **FileService_Get_Defined_Headers(file_info)
     FileInfo *file_info;
{
  FSGetMacro(header_values,NULL);
}


AG_Nonpredefined_Header 
  *FileService_Get_Nonpredefined_Headers(file_info)
     FileInfo *file_info;
{
  FSGetMacro(nonpredefined_header_root,NULL);
}


Widget FileService_Get_Shell(file_info)
     FileInfo *file_info;
{
  if (file_info == NULL)
    return(NULL);
  FSGetMacro(shell,NULL);
}

EditInfoPtr FileService_Get_EditFile_Info(file_info)
     FileInfo *file_info;
{
  FSGetMacro(editFile_info,NULL);
}

EditInfoPtr FileService_Get_EditNIP_Info(file_info)
     FileInfo *file_info;
{
  FSGetMacro(editNIP_info,NULL);
}

char *FileService_Get_Text(file_info)
     FileInfo *file_info;
{
  FSGetMacro(text,NULL);
}


EditInfo
  *FileService_Get_EditFileInfo(file_info)
     FileInfo *file_info;
{
  if (file_info == NULL)
    return(NULL);
  FSGetMacro(editFile_info,NULL);
}

EditInfo
  *FileService_Get_EditNIPInfo(file_info)
     FileInfo *file_info;
{
  if (file_info == NULL)
    return(NULL);
  FSGetMacro(editNIP_info,NULL);
}



Boolean FileService_Has_Node_Changed(file_info)
     FileInfo *file_info;
{
  FSGetMacro(dirty,((Boolean)NULL));
}


