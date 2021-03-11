/* File srctextv.ch created by R L Quinn
 
   srctextv.ch: Text View subclass specialized for dealing with source code text views. */
/* Copyright 1988,1994 Carnegie Mellon University and IBM. All rights reserved.
  $Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/
class srctextview[srctextv]: textview[textv] {
 
  overrides:
    PrepareInsertion(boolean insertingNewLine);
    PostMenus(struct menulist *menulist);
    ReceiveInputFocus();
    SetDataObject(struct srctext *dataobj);

  methods:
    Compress();
    CompressAll();
    ConfirmReadOnly() returns boolean;
    DecompressAll(long dummy);
    DeleteMenuItem(char menuitem[]);
    EndComment(char key);
    FindSubFile(char *filename, char *bufname, char *procname, char *searchpath) returns boolean;
    ForceUpperOn();
    ForceUpperOff();
    GotoColumn(int rock);
    HandleEndOfLineStyle(long pos);
    HandleNewlineAndRetrn(long key,boolean reindentThisLine,boolean preindentNewLine);
    InsertComment();
    InsertLineComment();
    MatchParens(char key);
    Paren(char key);
    PrependKeyState() returns struct keystate *;
    PutFileIntoNewWindow(char *bufname, char *proc, char *filename);
    RedoStyles();
    Reformat();
    Reindent();
    RenameIdent();
    SelfInsert(char key);
    SelfInsertReindent(char key);
    StartComment(char key);
    StartLineComment(char key);
    StyleLabel(char key);
    StyleString(char key);
    WaitCursorOn();
    WaitCursorOff();
    WhatColumn();

  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct srctextview *self) returns boolean;
    FinalizeObject(struct srctextview *self);
 
  macromethods:

  data:
    struct keystate *src_state;
    struct menulist *src_menus;    
};
