/* HTML text, first run -*-c-*- */
/* $Id: html.ch,v 1.6 1994/05/16 15:50:48 rr2b Exp $ */
/*
 * Copyright 1993, City University
 * Copyright 1993, 1994, Nick Williams. 
 * 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 *
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#include <mark.ih>

class html : text {
  overrides:
    Clear();
    Read(FILE* file, long id) returns long;
    ReadSubString(long pos, FILE* file, int quoteCharacters) returns long;
    Write(FILE* file, long writeID, int level) returns long;
    WriteSubString(long pos, long len, FILE* file, int quoteChars);

  methods:
    EnvStart(char* buf, char* envname, int* wantpar, int* br, int* newlines) returns char* ;
    EnvEnd(char* buf, char* envname, int* par, int* br) returns char* ;

    ChangeTitle(char* name);
    ChangeIndexable(int flag);

    AddLink(long pos, long len, char* uri);
    AddEntity(long pos, long len, char* name, char* vars);
      AddImage(long pos, char* path);
    GetAnchorDest(long pos) returns char*;
    GetAttributeList(struct environment* env, char** list, int* count);
    ChangeAttribute(struct view* tv, struct environment* env, char* attr, char* value);
    GetEntityEnvironment(long pos, struct environment* env) returns struct environment* ;
    GetAttribute(struct environment* env, char* attr) returns char*;
    TagItem(long pos, long len, char* text, char* styleName, struct style* extra) returns boolean;
    UntagItem(long pos) returns int;
      HasErrors() returns boolean;
      Inform(char*msg);

  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct html* self) returns boolean;
    FinalizeObject(struct html* self);

  macromethods:
    GetTitle() ((self)->title)
    GetIsIndex() ((self)->isindex)
    SetView(x) ((self)->view = (x))

  data:
    char* base;     /* The base of the URI */
    char* title;    /* The title of the document */
    long lineNumber; /* when reading, used for displaying error lines */
    int nextid;     /* Next identifier to use for anchor */
    int isindex;    /* If document is searchable */
    int	withinPar;  /* When parsing, indicates if within a paragraph */
    int parbreak;   /* When writing, indicates if parbreak is pending */
    int adornment;  /* When writing, indicates if stuff really exists */
      struct buffer* errorBuffer;
    struct htmlview* view; /* For debugging, mainly */
    struct entityElement* entities; /* stack of current ents when parsing */
      struct style* tagStyle;
      struct style* itemStyle;
};

