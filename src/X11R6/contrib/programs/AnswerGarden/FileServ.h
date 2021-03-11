/**********************************************************

  The ANSWER GARDEN PROJECT

  FILESERV.H Some declarations for the File Service only

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

FileInfo *AG_File_Open_NewFile();
FileInfo *AG_File_Open();
FileInfo *FileService_Open_Ascii();
void AG_File_Free_Buffer();
Boolean AG_File_Check_Write_Permission();
Boolean AG_File_Save();
void AG_File_Close();
void FileService_Register_Open_Node();
FileInfo *FileService_Register_Open_NonFS_Node();
Boolean FileService_Register_Closed_File();
Boolean FileService_Register_Closed_Edit();
NodeInfo *FileService_Find_Open_Node();

Boolean FileService_Has_Node_Changed();

Boolean FileService_Set_Defined_Headers();

EditInfo *FileService_Get_EditNIPInfo();
EditInfo *FileService_Get_EditFileInfo();

char *FileService_Get_Text();
Widget FileService_Get_Shell();
char **FileService_Get_Defined_Headers();
AG_Nonpredefined_Header *FileService_Get_Nonpredefined_Headers();

