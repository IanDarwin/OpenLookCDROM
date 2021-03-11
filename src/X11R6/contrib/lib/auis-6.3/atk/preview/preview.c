/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
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
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/preview/RCS/preview.c,v 2.33 1994/01/30 06:00:54 rr2b Exp $";
#endif


/* 
*
*	BE2 preview.
*		A program for previewing dvitroff input
*
*/

#include <andrewos.h>
#include <ctype.h>
#include <stdio.h>
#include <class.h>
#include <menulist.ih>
#include <scroll.ih>
#include <im.ih>
#include <graphic.ih>
#include <keystate.ih>
#include <environ.ih>
#include <proctbl.ih>
#include <cursor.ih>
#include <fontdesc.ih>
#include <keymap.ih>
#include <print.ih>
#ifdef USEFRAME
#include <message.ih>
#endif /* USEFRAME */
#include <preview.eh>

#define ResetOffsets(self) if(! self->DoScaling ) self->yoff = self->xoff = 0;
static SetTitle(self)
struct preview *self;
{
    if(preview_GetIM(self) == NULL) return;
    if(self->FindFirstPage || self->CurrentPageTableIndex == 0)
	sprintf(self->WindowTitle,"Initializing %s",self->DviBaseName);
    else if(self->PageTable[self->CurrentPageTableIndex].PageNumber < 1)
		sprintf(self->WindowTitle,"Processing %s",self->DviBaseName);
    else if (self->nowreading != -1) {
	sprintf(self->WindowTitle,"Processing %s/%d", 
		self->DviBaseName,
		self->PageTable[self->CurrentPageTableIndex].PageNumber);
    }
    else 
	sprintf(self->WindowTitle,"%s/%d of %d", 
		self->DviBaseName,
		self->PageTable[self->CurrentPageTableIndex].PageNumber,
		self->PageTable[self->NumberofPageTableEntries - 1].PageNumber
		);
    im_SetTitle(preview_GetIM(self),self->WindowTitle);
}
static DisplayPage(self,n)
struct preview *self;
preview_pagetableindex n;
{
   if (self->debug)
      fprintf(stderr, "DisplayPage %d\n", n);

   /* Change the title to reflect the new page number */
   SetTitle(self);
   preview_EraseVisualRect(self);

   if (self->WindowWidth < self->minWidth
   	 || self->WindowHeight < self->minHeight)
       return;

   DrawBorder(self);
   if (self->FindFirstPage){
      DoFindFirstPage(self);
        if(self->CurrentPageTableIndex <= 1) return; 
	if(! self->FindFirstPage){
/*	    preview_EraseVisualRect(self); */
	    if(n < self->CurrentPageTableIndex ) n = self->CurrentPageTableIndex;
	} 
   }
   if(self->FindFirstPage) return;
   if (0 <= n && n < self->NumberofPageTableEntries)
	    {
	       fseek(self->DviFileIn, self->PageTable[n].FileOffset, 0);
	       preview_DviToDisplay(self);
	    }
}
void preview__WantUpdate(self, requestor)
    struct preview *self;
    struct view *requestor;
{
    if (self->RedrawRequested && (struct view *)self == requestor) return;
    super_WantUpdate(self, requestor);
    if((struct view *)self == requestor) self->RedrawRequested = TRUE;
}

void preview__Update(self)
struct preview *self;
{
   if (self->debug)
      fprintf(stderr, "DoRedraw\n");
   self->RedrawRequested = FALSE;
   if(self->CurrentPageTableIndex == self->nowreading) return;
    if(self->quitpending && self->CursorChanged){
        SetCursor(self);
        return;
    }
/*    preview_EraseVisualRect(self);
 */   if (self->SizeChanged){
	 self->SizeChanged = FALSE;
	self->WindowWidth = preview_GetLogicalRight(self);
	self->WindowHeight = preview_GetLogicalBottom(self);

	 if (self->WindowWidth < self->minWidth 
		|| self->WindowHeight < self->minHeight)
	    return;

	 if (!self->DoScaling)
	     self->DisplayResolution = preview_DISPLAY_RESOLUTION;
	 else {
		self->DisplayResolution = self->WindowHeight/11;
		if (self->DisplayResolution > 2*self->WindowWidth/17)
			self->DisplayResolution = 2*self->WindowWidth/17;
	      }

   	self->xPixelsPerPage = self->DisplayResolution * 17 / 2;
   	self->yPixelsPerPage = self->DisplayResolution * 11;

	/*  RecomputeOffsets(self);  */
     }
   DisplayPage(self,self->CurrentPageTableIndex);
   SetCursor(self);
}
void preview__FullUpdate(self,type,left, top,  width,  right)
struct preview *self;
enum view_UpdateType type;
long left, top,  width,  right;
{
	self->SizeChanged = TRUE;
	preview_Update(self);
}
void preview__ReceiveInputFocus(self)
    struct preview *self;
{
    self->hasInputFocus = TRUE;
    self->keystate->next = NULL;
    preview_PostKeyState(self, self->keystate);
    preview_PostMenus(self, self->menulist);
}

void preview__LoseInputFocus(self)
    struct preview *self;
{
    self->hasInputFocus = FALSE;
}
static SetScale(self)
struct preview *self;
{

	/* change menu */
	if (self->DoScaling) return;
	self->DoScaling = TRUE;
	menulist_AddToML(self->menulist,"Full Size~10",self->FullSizeProc,NULL,0);
	menulist_DeleteFromML(self->menulist,"Scale~10");
        self->SizeChanged = TRUE;
 	if(self->hasInputFocus == TRUE) preview_PostMenus(self, self->menulist);
	preview_WantUpdate(self,self);
}
static SetFullSize(self)
struct preview *self;
{

	/* change menu */
	if (!self->DoScaling) return;
	self->DoScaling = FALSE;
	menulist_AddToML(self->menulist,"Scale~10",self->ScaleProc,NULL,0);
	menulist_DeleteFromML(self->menulist,"Full Size~10");
        self->SizeChanged = TRUE;
	if(self->hasInputFocus == TRUE)preview_PostMenus(self, self->menulist);
	preview_WantUpdate(self,self);

}
static LastPage(self)
struct preview *self;
{
/*	       if (self->CurrentPageTableIndex > self->LowestNonBlankPageIndex){ */
	       if (self->CurrentPageTableIndex > 1){
                   ResetOffsets(self);
                   self->CurrentPageTableIndex -= 1;
		   preview_WantUpdate(self,self);
	}
}
static NextPage(self)
struct preview *self;
{
	if (!self->DviFileComplete 
	   || self->CurrentPageTableIndex < self->NumberofPageTableEntries-1){
               ResetOffsets(self);
		self->CurrentPageTableIndex += 1;
		preview_WantUpdate(self,self);
	}
}
static SetPage(self,pagenum)
struct preview *self;
long pagenum;
{
	if(pagenum < self->NumberofPageTableEntries &&
/*	    pagenum >= self->LowestNonBlankPageIndex && */
	    pagenum >= 1 &&
	    pagenum != self->CurrentPageTableIndex){
                ResetOffsets(self);
		self->CurrentPageTableIndex = pagenum;
		preview_WantUpdate(self,self);
	}
}
static PrintCmd(self)	
struct preview *self; 
{
    DoPrintCmd(self,-1);
}
static PrintPageCmd(self)
struct preview *self; 
{
    DoPrintCmd(self,self->CurrentPageTableIndex);
}
static DeleteWindowCmd(self)	
struct preview *self; 
{
    /* Since preview always runs in a single window and
      a seperate process, we just exit.
      previewapp will then call preview_ReadyToQuit */
    im_KeyboardExit();
}
int preview__ReadyToQuit(self)
struct preview *self;
{
#ifdef USEFRAME
    char answer[50];
    if(!self->printpending) return(TRUE);
    if((message_AskForString(self,0,"Print request won't be processed; exit anyway [n]? ", NULL, answer, sizeof(answer)) == -1) || *answer != 'y') return(FALSE);
    return(TRUE);
#else /* USEFRAME */
    if(self->printpending){
        self->quitpending = TRUE;
        self->CursorChanged = TRUE;
        preview_WantUpdate(self,self);
        return(FALSE);
    }
    return(TRUE);
#endif /* USEFRAME */
}

static struct keymap *keymap;

boolean preview__InitializeObject(classID,self)
struct classheader *classID;
struct preview *self;
{
    struct proctable_Entry *tempProc;
    tmpnam(self->DviFileName);
    self->NWMFonts = 0;
    self->DoScaling = 0;
    self->DviFileIn = self->DviFileOut = NULL;
    self->DviFileLength = 0;
    self->CharactersOnThisPage = 0;
    self->SizeChanged = 0;
    self->WindowWidth = self->WindowHeight = 0;
    self->minWidth = self->minHeight = 3;
    self->PollCount = self->peekc = 0;
    self->NWMFonts = 0;
    self->CurrentCursor = '\0';
    self->PhysicalX = self->PhysicalY = 0;
    self->LogicalX = self->LogicalY = 0;
    self->Centre = self->CentreY = 0;
    self->InputResolution = 0;
    self->curfont = 2;
    self->cursize = 10 ;
    self->slant = 0;
    self->hasInputFocus = FALSE;
    self->DviFileComplete = FALSE;
    self->CreatedTemp = FALSE;
    self->FindFirstPage = TRUE;
    self->LowestNonBlankPageIndex = 0;
    self->CurrentPageTableIndex = 0;
    self->nowreading = 0;
    self->lastc = '\n';
    self->menulist = menulist_Create(self);
    keymap = keymap_New();
    self->DisplayResolution = preview_DISPLAY_RESOLUTION;
    self->xPixelsPerPage =self-> DisplayResolution * 17 / 2;
    self->yPixelsPerPage = self->DisplayResolution * 11;
    self->RedrawRequested = TRUE;
    self->menupage = -1;
    self->printpending = FALSE;
    self->quitpending = FALSE;
    self->cursor = cursor_Create(self);
    self->CursorChanged = TRUE;
/*    self->scroll = textview_NoScroll;
    self->scrollLine = 0;
    self->scrollDist = -1; */
    self->debug = FALSE;
    self->yoff = self->xoff = 0;
    InitPageMap(self);
     self->keystate = keystate_Create(self, keymap);
    self->ScaleProc = proctable_DefineProc("preview-Scale", SetScale, &preview_classinfo, NULL, "Scale preview display");
    self->FullSizeProc = proctable_DefineProc("preview-FullSize", SetFullSize, &preview_classinfo, NULL, "Full size preview display");
    self->SetPageProc = proctable_DefineProc("preview-SetPage", SetPage, &preview_classinfo, NULL, "Set Page");
    tempProc = proctable_DefineProc("preview-NextPage", NextPage, &preview_classinfo, NULL, "Next Page");
    /* Bind Next Page to space, and ^v keys */
    keymap_BindToKey(keymap, " ", tempProc, NULL);
    keymap_BindToKey(keymap, "\026", tempProc, NULL);
    menulist_AddToML(self->menulist, "Next Page~20", tempProc, NULL, 0);
    tempProc = proctable_DefineProc("preview-PreviousPage", LastPage, &preview_classinfo, NULL, "Last Page");
    /* Bind Previous Page to b, backspace, del, and M-v keys */
    keymap_BindToKey(keymap, "b", tempProc, NULL);
    keymap_BindToKey(keymap, "\010", tempProc, NULL);
    keymap_BindToKey(keymap, "\177", tempProc, NULL);
    keymap_BindToKey(keymap, "\033v", tempProc, NULL);
    menulist_AddToML(self->menulist, "Previous Page~21", tempProc, NULL, 0);
    tempProc = proctable_DefineProc("Preview-Print", PrintCmd, &preview_classinfo, NULL, "Prints preview document");
    menulist_AddToML(self->menulist, "Print~30", tempProc, NULL, 0);
    tempProc = proctable_DefineProc("Preview-PrintPage", PrintPageCmd, &preview_classinfo, NULL, "Prints current page of preview document");
    menulist_AddToML(self->menulist, "Print Page~31", tempProc, NULL, 0);
 
    tempProc = proctable_DefineProc("Preview-DeleteWindow", DeleteWindowCmd, &preview_classinfo, NULL, "Delete Preview Window");
    menulist_AddToML(self->menulist, "Delete Window~89", tempProc, NULL, 0);

    return TRUE;
}

static updatemenu(self)
struct preview *self;
{
	if(self->menupage >= 0) {
		menulist_AddToML(self->menulist, self->menubuf, self->SetPageProc, self->menupage, 0);
		if(self->hasInputFocus == TRUE)preview_PostMenus(self, self->menulist);
		self->menupage = -1;
	}
}

static DeclarePage(self,n, FilePosition)
struct preview *self;
preview_pagenumber    n;
long  FilePosition;
{
   preview_pagetableindex i;
/*     int RedrawRequested = FALSE;
 */
   if (self->debug)
      fprintf(stderr, "DeclarePage %d at %d\n", n, FilePosition);

   /* finish the completion work for the last page */
/*    if (self->LowestNonBlankPageIndex == self->NumberofPageTableEntries)
      RedrawRequested = TRUE;
   if (self->CurrentPageTableIndex == self->NumberofPageTableEntries)
      RedrawRequested = TRUE;
 */
   /* and then start work for the next page */
   i = self->NumberofPageTableEntries + 1;
   if (i >= preview_MAXPageTable)
	   {
		fprintf(stderr,"Too many pages (> %d)\n",preview_MAXPageTable);
		exit(1);
	   }
   self->PageTable[i].PageNumber = n;
   self->PageTable[i].FileOffset = FilePosition;
   if (self->debug)
      fprintf(stderr, "Put page %d at %d\n", n, i);

   if (n != 0 || FilePosition != 0)
   {
	
	preview_pagenumber m;
	updatemenu(self); /* add the last page to the menu */
	m = n/10*10;
	/* set up menu info for this page */
   	sprintf(self->menubuf, "Pages %d-%d~%d,Page %d~%d", (m == 0)? 1:m, m+9,(m / 10) +10, n,n - m + 10);
	self->menupage = i;
   }
   self->NumberofPageTableEntries = i;

 /*   if (RedrawRequested) preview_WantUpdate(self,self); */
}

static InitPageMap(self)
struct preview *self;
{
   preview_pagetableindex    i;

   for (i = 1; i < preview_MAXPageTable; i++)
      self->PageTable[i].PageNumber = -1;
   /* page zero must be at offset zero; this allows us to pick
	up the troff dvi header information before the first
	page, like the resolution of the input */
   self->PageTable[0].PageNumber = 0;
   self->PageTable[0].FileOffset = 0;
   self->NumberofPageTableEntries = 0;
}
static char testdvi[] = "x T ";
#define TESTDVILEN 4

notdvifile(self)
struct preview *self;
{
    fprintf(stderr,"The input file to preview is not a dvi file\n");
    fflush(stderr);
    self->printpending = FALSE;
    im_KeyboardExit();
}
static MakePageMap(filein,self)
FILE *filein;
struct preview *self;
{
   register int   c;
   register long  FilePosition;
   register    FILE * f = self->DviFileOut;
   register int lastc = self->lastc;
    int lastread;
   FilePosition = self->DviFileLength;
   while ((c = getc(filein)) != EOF)
      {
	 putc(c, f);
          if(FilePosition < TESTDVILEN && c != testdvi[FilePosition]){
              notdvifile(self);
              return;
          }
	 if (c == 'p' && lastc == '\n')
	    {
	       preview_pagenumber   n = 0;
	       while ((c = getc(filein)) != EOF && isdigit(c))
		  {
		     n = n * 10 + c - '0';
		     FilePosition += 1;
		     putc(c, f);
		  }
	       putc(c, f);
	       FilePosition += 1;
	       fflush(f);
		lastread = self->nowreading;
		self->nowreading = n;
	       DeclarePage(self,n, FilePosition);
		if(self->CurrentPageTableIndex == lastread || self->FindFirstPage){
       			preview_WantUpdate(self,self);
		}
	    }
	 lastc = c;
	 FilePosition += 1;
	 if (self->SizeChanged)
	    break;
	 if (FILE_HAS_IO(filein)<=0 && !self->DviFileComplete)

	    break;
      }
   self->lastc = lastc;
   self->DviFileLength = FilePosition;

   if (c != EOF)
      return;

   /* have reached the end of standard input, and a DVI file has been
      created; set everything up as if it had been here all the time. 
   */
   fclose(self->DviFileOut);
   DeclarePage(self,0, 0);
   self->DviFileComplete = TRUE;
   if (self->debug)
      fprintf(stderr, "DviFile Completed\n");
   lastread = self->nowreading;
   self->nowreading = -1;
   updatemenu(self);
   SetTitle(self);
   if(self->NumberofPageTableEntries <= self->CurrentPageTableIndex){
       lastread = self->CurrentPageTableIndex = self->NumberofPageTableEntries - 1;
   }
   if(self->CurrentPageTableIndex == lastread || self->FindFirstPage) {
       			preview_WantUpdate(self,self);
		}
    im_RemoveFileHandler(filein);
    fclose(filein);
    if(self->printpending) PrintCmd(self);
}

static MakePageMapWithoutCopying(filein,self)
FILE *filein;
struct preview *self;
{
   register int   c;
   register int   lastc;
   register long  FilePosition;

   self->DviFileComplete = FALSE;

   FilePosition = 0;
   lastc = '\n';
   while ((c = getc(filein)) != EOF)
      {
          if(FilePosition < TESTDVILEN && c != testdvi[FilePosition]){
              notdvifile(self);
              return;
          }
	 if (c == 'p' && lastc == '\n')
	    {
	       /* get page number */
	       preview_pagenumber   n = 0;
	       while ((c = getc(filein)) != EOF && isdigit(c))
		  {
		     n = n * 10 + c - '0';
		     FilePosition += 1;
		  }
	       FilePosition += 1;
	       DeclarePage(self,n, FilePosition);
	    }
	 lastc = c;
	 FilePosition += 1;
      }
   self->DviFileLength = FilePosition;
   DeclarePage(self,0, 0);
   self->DviFileComplete = TRUE;
   self->nowreading = -1;

   if (self->FindFirstPage ){
       			preview_WantUpdate(self,self);
		}
   updatemenu(self);
}

static DrawBorder(self)
struct preview *self;
{
   Boolean leftside = self->xoff > 0;
   Boolean topside = self->yoff > 0;
   preview_coordinate   right = self->xoff + self->xPixelsPerPage;
   preview_coordinate   bottom = self->yoff + self->yPixelsPerPage;
   Boolean rightside = right < self->WindowWidth;
   Boolean bottomside = bottom < self->WindowHeight;

   if (leftside)
      {
	 preview_MoveTo(self,self->xoff,(topside) ? self->yoff : 0);
	 preview_DrawLineTo(self,self->xoff,(bottomside) ? bottom : self->WindowHeight);
      }
   if (topside)
      {
	 preview_MoveTo(self,(leftside) ? self->xoff : 0, self->yoff);
	 preview_DrawLineTo(self,(rightside) ? right : self->WindowWidth,self->yoff);
      }
   if (rightside)
      {
	 preview_MoveTo(self,right,(topside) ? self->yoff : 0);
	 preview_DrawLineTo(self,right,(bottomside) ? bottom : self->WindowHeight);
      }
   if (bottomside)
      {
	 preview_MoveTo(self,(leftside) ? self->xoff : 0, bottom);
	 preview_DrawLineTo(self,(rightside) ? right : self->WindowWidth, bottom);
      }
}

static SetCursor(self)
struct preview *self;
{
    
    if(self->CursorChanged){
        self->CursorChanged = FALSE;
        if(self->quitpending){ 
/* Put up a wait sign it a print is pending and a quit is requested */
            struct fontdesc *fd = fontdesc_Create("icon",fontdesc_Plain,12);
            cursor_SetGlyph(self->cursor,fd,'W');
            /* cursor_SetStandard(self->cursor,Cursor_DangerousBend); */
        }
        else if(self->FindFirstPage) 
/* Put up a clock until the first page is ready to display */
            cursor_SetStandard(self->cursor,Cursor_Wait);
	else {
	    if (cursor_IsPosted(self->cursor))
		preview_RetractCursor(self, self->cursor);
	    return;
	}
	if(!cursor_IsPosted(self->cursor)){
	    struct rectangle tr;
	    preview_GetVisualBounds(self,&tr);
	    preview_PostCursor(self,&tr,self->cursor);
	}
    }
}

static DoFindFirstPage(self)
struct preview *self;
{
   preview_pagetableindex cp;

   /* if we are looking for the first real page and this one was
      blank, increment the page number until we find another candidate
      */

   cp = self->LowestNonBlankPageIndex;
   while (cp < self->NumberofPageTableEntries)
      {
	       if (self->debug) fprintf(stderr,"Try index %d\n",cp);
	       fseek(self->DviFileIn, self->PageTable[cp].FileOffset, 0);
	       preview_DviToDisplay(self);
	       if (self->CharactersOnThisPage)
		  {
		     self->FindFirstPage = FALSE;
                      self->CursorChanged = TRUE;
		     if (self->debug) fprintf(stderr,"Found First %d\n",cp); 
		     self->LowestNonBlankPageIndex = cp;
		     if (self->CurrentPageTableIndex < self->LowestNonBlankPageIndex)
				 self->CurrentPageTableIndex = self->LowestNonBlankPageIndex;
   		    /* Change the title to the new page number */
		     SetTitle(self);
		     return;
		  }
	 cp += 1;
      }
   self->LowestNonBlankPageIndex = cp;
   if (self->DviFileComplete) self->FindFirstPage = FALSE;
}


static insert(src,c)
char *src,*c;
{   /* inserts string src into the begining of string c , assumes enough space */
    char *p,*enddest;
    enddest = c + strlen(c);
    p = enddest + strlen(src);
    while(enddest >= c) *p-- = *enddest-- ;
    for(p = src; *p != '\0';p++)
	*c++ = *p;
}
static void normalize(s)
char *s;
{
    register char *c;
    for(c = s + strlen(s) - 1; c >= s; c--){
	if(!isalnum(*c)){
	    insert("\\",c);
	}
    }
}

static DoPrintCmd(self,page)	
struct preview *self; 
int page;
{
   int processid;
   Preview_Line PrintCommandFormat;
   Preview_Line PrintCommand;
   char *p;
   char BaseName[2048];
   strcpy(BaseName,self->DviBaseName);
   normalize(BaseName);
   p = print_GetPrintCmd(print_PRINTTROFF);
   strcpy(PrintCommandFormat,p);
   sprintf(PrintCommand,PrintCommandFormat,BaseName,BaseName,BaseName);
   if(page != -1){
       register FILE *fi,*fo;
       long n;
       register long c,diff,lastc;
       char buf[512];
       n = self->PageTable[page].FileOffset;
       if((fo = popen(PrintCommand,"w")) == NULL){
	   fprintf(stderr,"Can't execute %s",PrintCommand);
	   exit(1);
       }
       if((fi = fopen(self->DviFileName,"r")) == NULL){
	   fprintf(stderr,"Can't open %s",self->DviFileName);
	   exit(1);
       }
       diff = self->PageTable[1].FileOffset;
       while((c = getc(fi)) != EOF){
	   putc(c,fo);
	   if(--diff < 0) break;
       }
       if(page != 1){
	   fseek(fi, n , 0);
	   fgets(buf,512,fi);
       }
       lastc = '\n';
       while((c = getc(fi)) != EOF){
	   if(lastc == '\n' && c == 'p') break;
	   putc(c,fo);
	   lastc = c;
       }
       if(c != EOF)
	   fprintf(fo,"x trailer\nx stop\n");
       fflush(fo);
       pclose(fo);
       close(fi);
       return;
   }

   /* 
    Send the DviFile to be printed.
 */
 
   if (self->debug) fprintf(stderr, "Fork Print Process\n");
   
   if(!self->DviFileComplete){
       self->printpending = TRUE;
#ifdef USEFRAME
       message_DisplayString(self,0,"print pending");
#endif /* USEFRAME */
       return;
   }

   while (wait(0) > 0);

   self->printpending = FALSE;
#ifdef USEFRAME
   message_DisplayString(self,0,"Initiating print process");
#endif /* USEFRAME */
   if (osi_vfork() != 0) {  /* if parent process */
       if(self->quitpending) {
           wait(0); /* insures that the child gets to open the tmp files before the parent deletes it */
           im_KeyboardExit();
       }
       return;
   }

   /* To avoid dieing when the parent quits; reset process
	group id; set it to processid to be unique */
   processid = getpid();
   NEWPGRP();
  /* form the print command */
   close(0);
   open(self->DviFileName, O_RDONLY, 0);

   /* now execute the printcommand */
   execlp("/bin/sh", "sh", "-c", PrintCommand, 0);
   exit(0);

}


/* Scroll stuff. */
static void hgetinfo(), hsetframe(),vgetinfo(), vsetframe();
static long vwhatisat(),hwhatisat();

struct view *preview__GetApplicationLayer(self)
    struct preview *self;
{
   
     return (struct view *) scroll_Create(self, scroll_LEFT | scroll_BOTTOM);
    
}

void preview__DeleteApplicationLayer(self, scrollbar)
    struct preview *self;
    struct scroll *scrollbar;
{

    scroll_Destroy(scrollbar);
}
#define RANGE(T,A) ((T->beg > A)? T->beg : ((T->end < A)? T->end : A))
static void vgetinfo(self, total, seen, dot)
    struct preview *self;
    struct range *total, *seen, *dot;
{

    total->beg = 0;
    total->end =self->yPixelsPerPage;
    seen->beg = RANGE(total,-self->yoff);
    seen->end = RANGE(total,self->WindowHeight - self->yoff);
    if(seen->end < 0) seen->end = 0;
    dot->beg = 0;
    dot->end = -1;
}
static void hgetinfo(self, total, seen, dot)
    struct preview *self;
    struct range *total, *seen, *dot;
{

    total->beg = 0;
    total->end =self->xPixelsPerPage;
    seen->beg = RANGE(total,-self->xoff);
    seen->end = RANGE(total,self->WindowWidth - self->xoff);
    if(seen->end < 0) seen->end = 0;
    dot->beg = 0;
    dot->end = -1;
}
static long hwhatisat(self, numerator, denominator)
    struct preview *self;
    long numerator, denominator;
{
return numerator - self->xoff;

}
static long vwhatisat(self, numerator, denominator)
    struct preview *self;
    long numerator, denominator;
{
return numerator - self->yoff;

}static void vsetframe(self, position, numerator, denominator)
    struct preview *self;
    long position,  numerator, denominator;
{
    self->yoff =   numerator-position ;
    preview_WantUpdate(self,self);
}
static void hsetframe(self, position, numerator, denominator)
    struct preview *self;
    long position,  numerator, denominator;
{
    self->xoff =  numerator -position;
    preview_WantUpdate(self,self);
}
static struct scrollfns vscrollInterface = {vgetinfo, vsetframe, NULL, vwhatisat};
static struct scrollfns hscrollInterface = {hgetinfo, hsetframe, NULL, hwhatisat};

char *preview__GetInterface(self, interfaceName)
    struct preview *self;
    char *interfaceName;
{

    if (strcmp(interfaceName, "scroll,vertical") == 0)
        return (char *) &vscrollInterface;
    if (strcmp(interfaceName, "scroll,horizontal") == 0)
        return (char *) &hscrollInterface;
    return NULL;
}

struct preview *preview__Create(classID,f,fname,fbase,compleated,scale)
struct classheader *classID;
FILE *f;
char *fname, *fbase;
boolean compleated, scale;
{
    struct preview *self = preview_New();

    self->DviFileComplete = compleated;
    if(fbase)
	strcpy(self->DviBaseName, fbase);
    else
	*(self->DviBaseName) = '\0';
    self->DoScaling = scale;
    if(self->DoScaling)
	menulist_AddToML(self->menulist, "Full Size~10",
			 self->FullSizeProc, NULL, 0);
    else
	menulist_AddToML(self->menulist, "Scale~10", self->ScaleProc,
			 NULL, 0);
    if(compleated == FALSE) {
	int DviFD = open(self->DviFileName,
		     O_WRONLY | O_CREAT | O_EXCL,
		     0600);
	if(DviFD < 0){
	    fprintf(stderr, "Can't create tmp file %s\n", self->DviFileName);
	    return(NULL);
	}
	close(DviFD);
	self->DviFileOut = fopen(self->DviFileName, "w");
	if (self->DviFileOut == (FILE *) NULL) {
	    fprintf(stderr, "Can't write dvi file %s\n", self->DviFileName);
	    return(NULL);
	}
	self->CreatedTemp = TRUE;
    }
    self->DviFileIn = fopen(self->DviFileName, "r");
    if (self->DviFileIn == (FILE *) NULL) {
	fprintf(stderr, "Can't read dvi file %s\n", self->DviFileName);
	return(NULL);
    }
    SetTitle(self);
    if(compleated)
	MakePageMapWithoutCopying(f, self);
    else
	im_AddFileHandler(f, MakePageMap, self, 6);
    return(self);
}
struct preview *preview__Hit(self, action, x, y, numberOfClicks)
    struct preview *self;
    enum view_MouseAction action;
    long x;
    long y;
    long numberOfClicks;
{

 	if (! self->hasInputFocus)
	    preview_WantInputFocus(self, self);
	return(self);
}
void preview__FinalizeObject(classID,self)
struct classheader *classID;
struct preview *self;
{
   if (self->CreatedTemp)
      unlink(self->DviFileName);
}
