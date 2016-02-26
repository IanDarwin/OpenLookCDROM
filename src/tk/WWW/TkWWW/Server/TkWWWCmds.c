/* TkWWWCmds.c Adds World Wide Web commands to a Tcl interpreter
** ===============
**
** Authors:
** Joseph Wang, Department of Astronomy, University of Texas at Austin
** (joe@astro.as.utexas.edu)
**
** Copyright:
** Copyright (C) 1992-1994
** Globewide Network Academy
** Macvicar Institute for Educational Software Development
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Include c stuff
** ---------------
*/

#include <assert.h>
#include <stdio.h>
#include <ctype.h>

#if STDC_HEADERS
#include <sys/types.h>
#include <stdlib.h>
#endif 

/* Include World Wide Web stuff
** ----------------------------
*/

#include <HTAnchor.h>
#include <HTParse.h>
#include <HTAccess.h>
#include <HTML.h>
#include <HText.h>
#include <HTTCP.h>
#include <HTStream.h>
#include <HTFile.h>
#include <HTFWriter.h>
#include <HTextDef.h>
#include <HTAtom.h>
#include <HTPlain.h>
#include <HTMLGen.h>
#include <HTFormat.h>
#include <HTMIME.h>
#include <HTList.h>
#include <HTInit.h>
#include <HTAlert.h>
#include <TkWWWCmds.h>
#include <TkWWWVersion.h>

/* Some Global Variables
** ---------------------
*/

PUBLIC char * HTAppName = "tkWWW";	/* Application name */
PUBLIC char * HTAppVersion = SERVER_VERSION; 	/* Application version */
PUBLIC Tcl_Interp* HtTclInterp = 0; /* @@@@@@@@ */
PUBLIC int HtTclErrorCode = TCL_OK;
PUBLIC HText* HtTclExecText = 0;

EXTERN HTPresentation* default_presentation;
					   
/* Macro to check arguments
** ------------------------
** Assumes interpreter is in interp and number of arguments is in argc
*/

#define HtCheckArgc(min,max,function_name) \
    if ((argc < (min)) || (argc > (max))) { \
       Tcl_AppendResult(interp, (function_name), \
			": Incorrect number of arguments" , NULL); \
       return (TCL_ERROR); \
       }

/* Procedures called by tkWWW functions
** ------------------------------------
*/

PRIVATE int 
HtLoadCmd(dummy, interp, argc, argv)
     ClientData dummy;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
  HTRequest *request;
  HtCheckArgc(2,2, "HtLoad");
  HtTclInterp = interp;
  HtTclErrorCode = TCL_OK;
  HtTclExecText = NULL;

  request = HTRequest_new();


  if (!HTLoadAbsolute(argv[1], request)) {
    if (*interp->result == '\0') {
      HTLoadError(request, 500, "Unable to access document.");
    }
    HTRequest_delete (request);
    return (TCL_ERROR);
  }

  if (HtTclExecText)
    HtTclErrorCode = Tcl_Eval(interp, HtTclExecText->output->data);

  HTRequest_delete (request);
  return (HtTclErrorCode);
}

PRIVATE int 
HtParseNameCmd(dummy, interp, argc, argv)
     ClientData dummy;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
  char *current_address = NULL;
  
  HtCheckArgc(2,3,"HtParseName");
  if (argc == 3 && argv[2] && *(argv[2])) 
    StrAllocCopy(current_address, argv[2]);
  else {
    StrAllocCopy(current_address, "file:");

/* The following mess is to get the current working directory in a manner
 * that will work on most platforms 
 */

#ifndef MAXPATHLEN
#define NO_GETWD		/* Assume no  getwd() if no MAXPATHLEN */
#endif

#ifdef NO_GETWD  		/* No getwd() on this machine */
#ifdef HAS_GETCWD		/* System V variant SIGN CHANGED TBL 921006 !! */

    {
      char wd[1024];			/*!! Arbitrary*/
      extern char * getcwd();
      char * result = getcwd(wd, sizeof(wd)); 
      if (result) {

#ifdef vms  /* convert directory name to Unix-style syntax */
	char * disk = strchr (wd, ':');
	char * dir = strchr (wd, '[');
	if (disk) {
	  *disk = '\0';
	  StrAllocCat (current_address, "/");  /* needs delimiter */
	  StrAllocCat (current_address, wd);
	}
	if (dir) {
	  char *p;
	  *dir = '/';  /* Convert leading '[' */
	  for (p = dir ; *p != ']'; ++p)
	    if (*p == '.') *p = '/';
	  *p = '\0';  /* Cut on final ']' */
	  StrAllocCat (current_address, dir);
	}
#else  /* not VMS */
	StrAllocCat (current_address, wd);
#endif  /* not VMS */
      } else {
	fprintf(stderr,
		"HTBrowse: Can't read working directory (getcwd).\n");
      }
    }  /* end if good getcwd result */
	
#else   /* has NO getcwd */

    if (TRACE) 
      fprintf(stderr,
	      "HTBrowse: This platform does not support getwd() or getcwd()\n");
#endif	/* has no getcwd */

#else   /* has getwd */
    {
      char wd[MAXPATHLEN];
      extern char * getwd();
      char * result = getwd(wd);
      if (result) {
	StrAllocCat(current_address, wd);
      } else {
	fprintf(stderr, "HTBrowse: Can't read working directory.\n");
      }
    }
#endif
		
#ifdef vms
    StrAllocCat(current_address, "default.html");
#else
    StrAllocCat(current_address, "/default.html");
#endif
  }

  Tcl_SetResult(interp, HTParse(argv[1], current_address, PARSE_ALL), 
		TCL_DYNAMIC);
  free(current_address);
  
  return (TCL_OK);
}

PRIVATE int 
HtUncacheCmd (dummy, interp, argc, argv) 
     ClientData dummy;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
  HTParentAnchor *parent_anchor;
  HTAnchor *anchor;
  HText *document;
  HtCheckArgc(2, 2, "HtUncache");

  anchor = HTAnchor_findAddress(argv[1]);
  if (anchor) {
    parent_anchor = HTAnchor_parent(anchor);
    document = (HText *)HTAnchor_document(parent_anchor);
    if (document) 
      HText_free(document);
  }
  return (TCL_OK);
}

  
/* HtVersion
** ------------
** returns the version of the tkWWW server
*/

PRIVATE int 
HtVersionCmd(dummy, interp, argc, argv)
     ClientData dummy;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
  
  Tcl_SetResult(interp, SERVER_VERSION, TCL_STATIC);
  return (TCL_OK);
}

PUBLIC void 
HTTkSetOutputFile(request, format, fnam, encoding)
     HTRequest *request;
     HTFormat format;
     CONST char *fnam;
     HTFormat encoding;
 {
  char *address;
  HText *text = HText_new(request->anchor);
  HTChunkPuts(text->output, "tkW3ConfigDisplayFile ");
  HTChunkPuts(text->output, HTAtom_name(format));
  HTChunkPuts(text->output, " ");
  HTChunkPuts(text->output, fnam);
  HTChunkPuts(text->output, " ");
  address = HTAnchor_address((HTAnchor *) request->anchor);
  HTChunkPuts(text->output, address);
  if (encoding) {
    HTChunkPuts(text->output, " ");
    HTChunkPuts(text->output, HTAtom_name(encoding));
  }
  HTChunkPuts(text->output, "\n");
  free(address);
  HTChunkTerminate(text->output);
  HtTclExecText = text;
}

PUBLIC HTStream* 
HTTkDisplay ARGS5(
        HTRequest *,            request,
        void *,                 param,
        HTFormat,               input_format,
        HTFormat,               output_format,
        HTStream *,             output_stream)
{
  char *fnam;
  CONST char * suffix;
  HTStream* this;
  FILE *fp;
  HTFormat encoding;

  encoding = request->content_encoding ? request->content_encoding : 
    WWW_BINARY;

  /* Save the file under a suitably suffixed name */
  suffix = HTFileSuffix(input_format);

  fnam = tempnam (NULL, "www");
  if (suffix) strcat(fnam, suffix);
    
  fp = fopen (fnam, "w");
  if (!fp) {
    HTAlert("Can't open temporary file!");
    free(fnam);
    return NULL;
  }

  this = HTFWriter_new(fp, FALSE);

  HTTkSetOutputFile(request, input_format, fnam, encoding);
  free (fnam);
  return this;
}

PRIVATE BOOL 
wild_match ARGS2(HTAtom *, template, HTAtom *, actual)
{
    char *t, *a, *st, *sa;
    BOOL match = NO;

    if (template && actual && (t = HTAtom_name(template))) {
	if (!strcmp(t, "*"))
	    return YES;

	if (strchr(t, '*') &&
	    (a = HTAtom_name(actual)) &&
	    (st = strchr(t, '/')) && (sa = strchr(a,'/'))) {

	    *sa = 0;
	    *st = 0;

	    if ((*(st-1)=='*' &&
		 (*(st+1)=='*' || !strcasecomp(st+1, sa+1))) ||
		(*(st+1)=='*' && !strcasecomp(t,a)))
		match = YES;

	    *sa = '/';
	    *st = '/';
	}    
    }
    return match;
}


PUBLIC BOOL 
HTTkUseInPlace(rep_in, request)
     HTFormat rep_in;
     HTRequest *request;
{
  HTFormat rep_out = request->output_format; 
  HTList * conversion[2];
  int which_list;
  float best_quality = -1e30;         /* Pretty bad! */
  HTPresentation *pres, *match, *best_match=0;

  conversion[0] = request->conversions;
  conversion[1] = HTConversions;

  for(which_list = 0; which_list<2; which_list++) {
    HTList * cur = conversion[which_list];
        
    while ((pres = (HTPresentation*)HTList_nextObject(cur))) {
      if  ((pres->rep == rep_in || wild_match(pres->rep, rep_in)) &&
	   (pres->rep_out == rep_out || wild_match(pres->rep_out, rep_out))
) {
	if (pres->quality > best_quality) {
	  best_match = pres;
	  best_quality = pres->quality;
	}
      }
    }
    match = best_match ? best_match : NULL;
    if (match) {
      return (match->converter == HTTkDisplay);
    }
  }
   return FALSE;
}

PRIVATE int 
HtAddEncodingCmd(dummy, interp, argc, argv)
     ClientData dummy;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
  float priority;
  HtCheckArgc(3, 4, "HtAddEncoding");
  priority  = (argc==3) ? 1.0 : atof(argv[3]);
  HTAddEncoding(argv[1], argv[2], priority);
  return (TCL_OK);
}

PRIVATE int 
HtAddTypeCmd(dummy, interp, argc, argv)
     ClientData dummy;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
  float priority;
  HtCheckArgc(4, 5, "HtAddType");
  priority  = (argc==4) ? 1.0 : atof(argv[4]);
  HTAddType(argv[1], argv[2], argv[3], priority);
  return (TCL_OK);
}

/* Add tkWWW commands to a tcl interpreter init_tkWWW(interp)
** -------------
*/

int 
WWW_AppInit(interp)
     Tcl_Interp *interp;
{
  if (!HTConversions) 
    HTConversions = HTList_new();
  Tcl_CreateCommand(interp, "HtAddType", HtAddTypeCmd,
		    (ClientData) NULL, (void (*)()) NULL);
  Tcl_CreateCommand(interp, "HtAddEncoding", HtAddEncodingCmd,
		    (ClientData) NULL, (void (*)()) NULL);
  Tcl_CreateCommand(interp, "HtLoad", HtLoadCmd, 
		    (ClientData) NULL, (void (*)()) NULL);
  Tcl_CreateCommand(interp, "HtParseName", HtParseNameCmd,
		    (ClientData) NULL, (void (*)()) NULL);
  Tcl_CreateCommand(interp, "HtUncache", HtUncacheCmd,
		    (ClientData) NULL, (void (*)()) NULL);
  Tcl_CreateCommand(interp, "HtVersion", HtVersionCmd,
		    (ClientData) NULL, (void (*)()) NULL);


  HTSetConversion(HTConversions, "www/mime",  "*/*",           
		   HTMIMEConvert, 	1.0, 0.0, 0.0);
  HTSetConversion(HTConversions, "text/html", "text/x-c",    
		   HTMLToC, 	        0.5, 0.0, 0.0);
  HTSetConversion(HTConversions, "text/html", "text/plain",  
		   HTMLToPlain, 	0.5, 0.0, 0.0);
  HTSetConversion(HTConversions, "text/html", "www/present", 
		   HTMLPresent, 	1.0, 0.0, 0.0);
  HTSetConversion(HTConversions, "text/plain", "text/html",  
		   HTPlainToHTML,	1.0, 0.0, 0.0);
  HTSetConversion(HTConversions, "*/*", "www/present",
		   HTTkDisplay,      0.3, 0.0, 0.0);

  return (TCL_OK);
}
