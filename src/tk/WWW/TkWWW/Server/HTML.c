/*		Structured stream to Rich hypertext converter
**		============================================
**
**	This generates of a hypertext object.  It converts from the
**	structured stream interface fro HTMl events into the style-
**	oriented iunterface of the HText.h interface.  This module is
**	only used in clients and shouldnot be linked into servers.
**
**	Override this module if making a new GUI browser.
**
*/

#include "HTML.h"
#include <ctype.h>
#include <stdio.h>

#include "HTAtom.h"
#include "HTextDef.h"

#include "HTAlert.h"
#include "HTMLGen.h"
#include "HTParse.h"

#include "TkWWWCmds.h"

PUBLIC HTStyleSheet *styleSheet = NULL;

/*		HTML Object
**		-----------
*/

struct _HTStructured {
    CONST HTStructuredClass * 	isa;
    HTParentAnchor * 		node_anchor;
    HText * 			text;

    HTStream*			target;			/* Output stream */
    HTStreamClass		targetClass;		/* Output routines */

    HTChunk 			title;		/* Grow by 128 */
    
    char *			comment_start;	/* for literate programming */
    char *			comment_end;

    CONST SGML_dtd*		dtd;

    BOOL                        in_title;
    BOOL                        in_comment;
    BOOL			in_word;  /* Have just had a non-white char */
    BOOL                        in_verbatim;
    BOOL                        in_string;
    BOOL                        error;
};

struct _HTStream {
    CONST HTStreamClass *	isa;
    /* .... */
};

static char * ISO_Latin1[] = {
        "\306", /* capital AE diphthong (ligature) */ 
        "\301", /* capital A, acute accent */ 
        "\302", /* capital A, circumflex accent */ 
        "\300", /* capital A, grave accent */ 
        "\305", /* capital A, ring */ 
        "\303", /* capital A, tilde */ 
        "\304", /* capital A, dieresis or umlaut mark */ 
        "\307", /* capital C, cedilla */ 
        "\320", /* capital Eth, Icelandic */ 
        "\311", /* capital E, acute accent */ 
        "\312", /* capital E, circumflex accent */ 
        "\310", /* capital E, grave accent */ 
        "\313", /* capital E, dieresis or umlaut mark */ 
        "\315", /* capital I, acute accent */ 
        "\316", /* capital I, circumflex accent */ 
        "\314", /* capital I, grave accent */ 
        "\317", /* capital I, dieresis or umlaut mark */ 
        "\321", /* capital N, tilde */ 
        "\323", /* capital O, acute accent */ 
        "\324", /* capital O, circumflex accent */ 
        "\322", /* capital O, grave accent */ 
        "\330", /* capital O, slash */ 
        "\325", /* capital O, tilde */ 
        "\326", /* capital O, dieresis or umlaut mark */ 
        "\336", /* capital THORN, Icelandic */ 
        "\332", /* capital U, acute accent */ 
        "\333", /* capital U, circumflex accent */ 
        "\331", /* capital U, grave accent */ 
        "\334", /* capital U, dieresis or umlaut mark */ 
        "\335", /* capital Y, acute accent */ 
        "\341", /* small a, acute accent */ 
        "\342", /* small a, circumflex accent */ 
        "\346", /* small ae diphthong (ligature) */ 
        "\340", /* small a, grave accent */ 
        "\046", /* ampersand */ 
        "\345", /* small a, ring */ 
        "\343", /* small a, tilde */ 
        "\344", /* small a, dieresis or umlaut mark */ 
        "\347", /* small c, cedilla */ 
        "\351", /* small e, acute accent */ 
        "\352", /* small e, circumflex accent */ 
        "\350", /* small e, grave accent */ 
        "\360", /* small eth, Icelandic */ 
        "\353", /* small e, dieresis or umlaut mark */ 
        "\076", /* greater than */ 
        "\355", /* small i, acute accent */ 
        "\356", /* small i, circumflex accent */ 
        "\354", /* small i, grave accent */ 
        "\357", /* small i, dieresis or umlaut mark */ 
        "\074", /* less than */ 
        "\361", /* small n, tilde */ 
        "\363", /* small o, acute accent */ 
        "\364", /* small o, circumflex accent */ 
        "\362", /* small o, grave accent */ 
        "\370", /* small o, slash */ 
        "\365", /* small o, tilde */ 
        "\366", /* small o, dieresis or umlaut mark */ 
        "\337", /* small sharp s, German (sz ligature) */ 
        "\376", /* small thorn, Icelandic */ 
        "\372", /* small u, acute accent */ 
        "\373", /* small u, circumflex accent */ 
        "\371", /* small u, grave accent */ 
        "\374", /* small u, dieresis or umlaut mark */ 
        "\375", /* small y, acute accent */ 
        "\377", /* small y, dieresis or umlaut mark */ 
      };

/* 	Entity values -- for ISO Latin 1 local representation
**
**	This MUST match exactly the table referred to in the DTD!
*/

/*		Set character set
**		----------------
*/

PRIVATE char** p_entity_values = ISO_Latin1;	/* Pointer to translation */

PUBLIC void HTMLUseCharacterSet ARGS1(HTMLCharacterSet, i)
{
    p_entity_values = ISO_Latin1;
}

/*_________________________________________________________________________
**
**			A C T I O N 	R O U T I N E S
*/

PRIVATE void begin_string ARGS1(HTStructured *, me) 
{
  if (!me->in_string) {
    HText_puts(me->text, "tkW3HtAdd \"");
    me->in_string = YES;
  }
}

PRIVATE void begin_command ARGS1(HTStructured *, me)
{
  if (me->in_string) {
    HText_puts(me->text, "\"\n");
    me->in_string = NO;
  }
}


/*	Character handling
**	------------------
*/
PRIVATE void HTML_put_character ARGS2(HTStructured *, me, char, c)
{

  if (me->in_comment)
    return;
	
  if (me->in_title) {
    HTChunkPutc(&me->title, c);
    return;
  }

  if (me->in_verbatim) {
    begin_string(me);
    HText_appendCharacter(me->text, c);
    return;
  }

  if (isspace(c)) {
    if (me->in_word) {
      begin_string (me);
      HText_appendCharacter(me->text, ' ');
      me->in_word = NO;
    }
  } else {
    begin_string(me);
    HText_appendCharacter(me->text, c);
    me->in_word = YES;
  }

}






/*	String handling
**	---------------
**
**	This is written separately from put_character becuase the loop can
**	in some cases be promoted to a higher function call level for speed.
*/
PRIVATE void HTML_put_string ARGS2(HTStructured *, me, CONST char*, s)
{
  CONST char *p;
  begin_string(me);

  if (me->in_comment)    
    return;
	
  if (me->in_title) {
    HTChunkPuts(&me->title, s);
    return;
  }

  if (me->in_verbatim) {
    begin_string(me);
    HText_appendText(me->text, s);
    return;
  }

  for(p=s; *p; p++) {
    if (isspace(*p)) {
      if (me->in_word) {
	begin_string(me);
	HText_appendCharacter(me->text, ' ');
	me->in_word = NO;
      }
    } else {
      begin_string(me);
      HText_appendCharacter(me->text, *p);
      me->in_word = YES;
    }
  } /* for */
}


/*	Buffer write
**	------------
*/
PRIVATE void HTML_write ARGS3(HTStructured *, me, CONST char*, s, int, l)
{
    CONST char* p;
    CONST char* e = s+l;
    for (p=s; s<e; p++) HTML_put_character(me, *p);
}

/*
**
*/

PRIVATE void beginAnchor ARGS3(HTStructured *, me, CONST char*, name, 
			       CONST char *, href)
{
  begin_command(me);
  HText_puts(me->text, "tkW3HtBeginAnc \"");
  HText_puts(me->text, name);
  HText_puts(me->text, "\" \"");
  HText_puts(me->text, href);
  HText_puts(me->text, "\"\n");
  HText_executeTCL(me->text);
}


PRIVATE void endAnchor ARGS1(HTStructured *, me)
{
  begin_command(me);
  HText_puts(me->text, "tkW3HtEndAnc\n");
  HText_executeTCL(me->text);
}

PRIVATE void setImage ARGS3(HTStructured *, me, CONST char*, name,
			    BOOL, is_map) {
  begin_command(me);
  HText_puts(me->text, "tkW3HtSetImg ");
  HText_puts(me->text, name);
  HText_puts(me->text, " ");
  HText_puts(me->text, is_map ? "1" : "0");
  HText_puts(me->text, "\n");
  HText_executeTCL(me->text);
}

PRIVATE void addCommand ARGS3(HTStructured *, me, CONST char *, command,
			      int, elem) {
  begin_command(me);
  HText_puts(me->text, command);
  HText_putc(me->text, ' ');
  HText_puts(me->text, me->dtd->tags[elem].name);  
  HText_puts(me->text, "\n");
  HText_executeTCL(me->text);
}

#define beginParaStyle(me, elem)  addCommand((me), "tkW3HtBegin para", (elem));
#define endParaStyle(me, elem)    addCommand((me), "tkW3HtEnd para", (elem));
#define beginCharStyle(me, elem) addCommand((me), "tkW3HtBegin char", (elem));
#define endCharStyle(me, elem)   addCommand((me), "tkW3HtEnd char", (elem));
#define beginListStyle(me, elem) addCommand((me), "tkW3HtBegin list", (elem));
#define endListStyle(me, elem)   addCommand((me), "tkW3HtEnd list", (elem));
#define appendBullet(me, elem)   addCommand((me), "tkW3HtAddBul", (elem));

/*	Start Element
**	-------------
*/
PRIVATE void HTML_start_element ARGS4(
	HTStructured *, 	me,
	int,		element_number,
	CONST BOOL*,	 	present,
	CONST char **,	value)
{
    switch (element_number) {
    case HTML_A:
	{
	  CONST char * id = "";
	  char * href = NULL;
	  if (present[HTML_A_HREF]) {
	    StrAllocCopy(href, value[HTML_A_HREF]);
	    HTSimplify(href);
	  }
	  if (present[HTML_A_ID]) 
	    id = value[HTML_A_ID];
	  else if (present[HTML_A_NAME]) 
	    id = value[HTML_A_NAME];
	  beginAnchor(me, id,  present[HTML_A_HREF] ? href: "");
	  HText_executeTCL(me->text);
	}
    	break;
      case HTML_LINK:
	begin_command(me);
	HText_puts(me->text, "tkW3HtSetLink \"");
	HText_puts(me->text, 
		   present[HTML_A_REL] ? value[HTML_A_REL] : "");
	HText_puts(me->text, "\" \"");
	HText_puts(me->text,
		   present[HTML_A_HREF] ? value[HTML_A_HREF] : "");
	HText_puts(me->text, "\"\n");
	HText_executeTCL(me->text);
	break;

      case HTML_IMG:
	if (present[HTML_IMG_SRC]) 
	  setImage(me, value[HTML_IMG_SRC], present[HTML_IMG_ISMAP]);
	break;

      case HTML_TITLE:
	me->in_title = YES;
        HTChunkClear(&me->title);
	break;
	
      case HTML_NEXTID:
	if (present[HTML_NEXTID_N]) {
	  begin_command(me);
	  HText_puts(me->text, "tkW3HtSetNextId \"");
	  HText_puts(me->text, value[HTML_NEXTID_N]);
	  HText_puts(me->text, "\"\n");
	  HText_executeTCL(me->text);
	}
    	break;
	
/*      case HTML_BASE:
	if (present[HTML_BASE_HREF]) {
	  begin_command(me);
	  HText_puts(me->text, "tkW3HtSetBase \"");
	  HText_puts(me->text, value[HTML_BASE_HREF]);
	  HText_puts(me->text, "\"\n");
	}
	break;
	*/
      case HTML_ISINDEX:
   	HTAnchor_setIndex(me->node_anchor);
	begin_command(me);
	HText_puts(me->text, "tkW3HtSetInd \"");
/*	if (present[HTML_BASE_HREF])
	  HText_puts(me->text, value[HTML_BASE_HREF]); */
	HText_puts(me->text, "\"\n");
	HText_executeTCL(me->text);
	break;
	
      case HTML_DL:
      case HTML_UL:
      case HTML_OL:
      case HTML_MENU:
      case HTML_DIR:
	beginListStyle(me, element_number);
	break;
	
      case HTML_DT:
      case HTML_DD:
      case HTML_LI:
      case HTML_P:
      case HTML_BR:
      case HTML_HR:
	appendBullet(me, element_number);
	me->in_word = NO;
	break;

	
      case HTML_LISTING:				/* Litteral text */
      case HTML_XMP:
      case HTML_PLAINTEXT:
      case HTML_PRE:
      case HTML_LIT:
	me->in_verbatim = YES; /* @@@@@@@ Assumes these do not nest */

      case HTML_ABSTRACT:
      case HTML_BYLINE:
      case HTML_H1:
      case HTML_H2:
      case HTML_H3:
      case HTML_H4:
      case HTML_H5:
      case HTML_H6:
      case HTML_H7:
      case HTML_ADDRESS:
      case HTML_BLOCKQUOTE:
      case HTML_FOOTNOTE:
      case HTML_MARGIN:
    	beginParaStyle(me, element_number);
	break;

      case HTML_TT:			/* Physical character highlighting */
      case HTML_B:
      case HTML_I:
      case HTML_U:

      case HTML_EM:			/* Logical character highlighting */
      case HTML_STRONG:
      case HTML_CODE:
      case HTML_SAMP:
      case HTML_KBD:
      case HTML_VAR:
      case HTML_DFN:
      case HTML_CITE:
      case HTML_Q:
      case HTML_PERSON:
      case HTML_ACRONYM:
      case HTML_ABBREV:
      case HTML_CMD:
      case HTML_ARG:
	beginCharStyle(me, element_number);
	break;


      case HTML_COMMENT:
	me->in_comment = YES;


    } /* end switch */
}


/*		End Element
**		-----------
**
*/

PRIVATE void HTML_end_element ARGS2(HTStructured *, me, int , element_number)
{
  switch(element_number) {

  case HTML_A:
    endAnchor(me);
    break;

  case HTML_TITLE:
    HTChunkTerminate(&me->title);
    HTAnchor_setTitle(me->node_anchor, me->title.data);
    me->in_title = NO;
    break;

  case HTML_COMMENT:
    me->in_comment = NO;
    break;

  case HTML_TT:			/* Physical character highlighting */
  case HTML_B:
  case HTML_I:
  case HTML_U:

  case HTML_EM:			/* Logical character highlighting */
  case HTML_STRONG:
  case HTML_CODE:
  case HTML_SAMP:
  case HTML_KBD:
  case HTML_VAR:
  case HTML_DFN:
  case HTML_CITE:
  case HTML_Q:
  case HTML_PERSON:
  case HTML_ACRONYM:
  case HTML_ABBREV:
  case HTML_CMD:
  case HTML_ARG:
    endCharStyle(me, element_number);
    break;

  case HTML_DL:
  case HTML_UL:
  case HTML_OL:
  case HTML_MENU:
  case HTML_DIR:
    endListStyle(me, element_number);
    break;

  case HTML_LISTING:				/* Litteral text */
  case HTML_XMP:
  case HTML_PLAINTEXT:
  case HTML_PRE:
  case HTML_LIT:
    me->in_verbatim = NO; /* @@@@@@@ Assumes these do not nest */

  case HTML_ABSTRACT:
  case HTML_BYLINE:
  case HTML_H1:
  case HTML_H2:
  case HTML_H3:
  case HTML_H4:
  case HTML_H5:
  case HTML_H6:
  case HTML_H7:
  case HTML_ADDRESS:
  case HTML_BLOCKQUOTE:
  case HTML_FOOTNOTE:
  case HTML_MARGIN:
    endParaStyle(me, element_number);
    break;
  default:
    break;
  } /* switch */
}


/*		Expanding entities
**		------------------
*/
/*	(In fact, they all shrink!)
*/

PRIVATE void HTML_put_entity ARGS2(HTStructured *, me, int, entity_number)
{
  begin_string(me);
  /* @@ Other representations */
  HTML_put_string(me, ISO_Latin1[entity_number]);
}



/*	Free an HTML object
**	-------------------
**
** If the document is empty, the text object will not yet exist.
   So we could in fact abandon creating the document and return
   an error code.  In fact an empty document is an important type
   of document, so we don't.
**
**	If non-interactive, everything is freed off.   No: crashes -listrefs
**	Otherwise, the interactive object is left.	
*/
PUBLIC void HTML_free ARGS1(HTStructured *, me)
{
  char *address;
  CONST char *title;
  if (me->comment_end)
    HTML_put_string(me,me->comment_end);
  begin_command(me);
  HText_puts(me->text, "tkW3HtSetName \"");
  address = HTAnchor_address((HTAnchor *) me->node_anchor);
  HText_appendText(me->text, address);
  free(address);
  HText_puts(me->text, "\" \"");
  title = HTAnchor_title(me->node_anchor);
  if (title)
    HText_appendText(me->text, title);
  HText_puts(me->text, "\"\ntkW3HtEndDoc\n");
  HText_endAppend(me->text);
  HText_executeTCL(me->text);

  if (me->target)
    (*me->targetClass.free)(me->target);
  free(me);
}


PRIVATE void HTML_abort ARGS2(HTStructured *, me, HTError, e)

{
    if (me->target) {
        (*me->targetClass.abort)(me->target, e);
    }
    free(me);
}

/*				P U B L I C
*/

/*	Structured Object Class
**	-----------------------
*/
PUBLIC CONST HTStructuredClass HTMLPresentation = /* As opposed to print etc */
{		
	"text/html",
	HTML_free,
	HTML_abort,
	HTML_put_character, 	HTML_put_string,  HTML_write,
	HTML_start_element, 	HTML_end_element,
	HTML_put_entity
}; 


/*		New Structured Text object
**		--------------------------
**
**	The strutcured stream can generate either presentation,
**	or plain text, or HTML.
*/
PUBLIC HTStructured* HTML_new ARGS5(
	HTRequest *,		request,
	void *,			param,
	HTFormat,		input_format,
	HTFormat,		output_format,
	HTStream *,		output_stream)
{

    HTStructured * me;
    
    me = (HTStructured*) malloc(sizeof(*me));
    if (me == NULL) outofmem(__FILE__, "HTML_new");
    if (output_format != WWW_PLAINTEXT
    	&& output_format != WWW_PRESENT
	&& output_format != HTAtom_for("text/x-c")) {
      HTStream * intermediate = HTStreamStack(WWW_HTML, request);
      if (intermediate) return HTMLGenerator(intermediate);
      fprintf(stderr, "** Internal error: can't parse HTML to %s\n",
	      HTAtom_name(output_format));
      exit (-99);
    }
    
    me->isa = &HTMLPresentation;
    me->dtd = &DTD;
    me->node_anchor =  request->anchor;
    me->title.size = 0;
    me->title.growby = 128;
    me->title.allocated = 0;
    me->title.data = 0;

    
    me->comment_start = NULL;
    me->comment_end = NULL;
    me->target = output_stream;
    me->in_verbatim = NO;
    me->in_word = NO;
    me->in_comment = NO;
    me->in_title = NO;
    me->in_string = NO;
    if (output_stream) me->targetClass = *output_stream->isa;	/* Copy pointers */
    me->text = HText_new2(request->anchor, output_stream);
    HText_puts(me->text, "tkW3HtBeginDoc\n");
    HText_executeTCL(me->text);
    return (HTStructured*) me;
}

/*	HTConverter for HTML to plain text
**	----------------------------------
**
**	This will convert from HTML to presentation or plain text.
*/
PUBLIC HTStream* HTMLToPlain ARGS5(
	HTRequest *,		request,
	void *,			param,
	HTFormat,		input_format,
	HTFormat,		output_format,
	HTStream *,		output_stream)
{
    return SGML_new(&DTD, HTML_new(
    	request, NULL, input_format, output_format, output_stream));
}


/*	HTConverter for HTML to C code
**	------------------------------
**
**	C copde is like plain text but all non-preformatted code
**	is commented out.
**	This will convert from HTML to presentation or plain text.
*/
PUBLIC HTStream* HTMLToC ARGS5(
	HTRequest *,		request,
	void *,			param,
	HTFormat,		input_format,
	HTFormat,		output_format,
	HTStream *,		output_stream)
{
    
    HTStructured * html;
    
    (*output_stream->isa->put_string)(output_stream, "/* "); /* Before even title */
    html = HTML_new(request, NULL, input_format, output_format, output_stream);
    html->comment_start = "/* ";
    html->dtd = &DTD;
    html->comment_end = " */\n";	/* Must start in col 1 for cpp */
/*    HTML_put_string(html,html->comment_start); */
    return SGML_new(&DTD, html);
}


/*	Presenter for HTML
**	------------------
**
**	This will convert from HTML to presentation or plain text.
**
**	Override this if you have a windows version
*/
#ifndef GUI
PUBLIC HTStream* HTMLPresent ARGS5(
	HTRequest *,		request,
	void *,			param,
	HTFormat,		input_format,
	HTFormat,		output_format,
	HTStream *,		output_stream)
{
    return SGML_new(&DTD, HTML_new(
    	request, NULL, input_format, output_format, output_stream));
}
#endif


/*	Record error message as a hypertext object
**	------------------------------------------
**
**	The error message should be marked as an error so that
**	it can be reloaded later.
**	This implementation just throws up an error message
**	and leaves the document unloaded.
**	A smarter implementation would load an error document,
**	marking at such so that it is retried on reload.
**
** On entry,
**	sink 	is a stream to the output device if any
**	number	is the HTTP error number
**	message	is the human readable message.
**
** On exit,
**	returns	a negative number to indicate lack of success in the load.
*/



PUBLIC int HTLoadError ARGS3(
	HTRequest *, 	req,
	int,		number,
	CONST char *,	message)
{
  char *err = "Oh I screwed up!";   
  Tcl_SetResult(HtTclInterp, (char *) message, TCL_VOLATILE);
  if (req && req->output_stream)
    (*req->output_stream->isa->abort)(req->output_stream, err);
  return -number;
} 
