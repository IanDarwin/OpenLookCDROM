/*                                                     HTML to rich text converter for libwww
                         THE HTML TO STYLED TEXT OBJECT CONVERTER
                                             
   This interprets the HTML semantics and some HTMLPlus.
   
   Part of libwww . Implemented by HTML.c
   
 */
#ifndef HTML_H
#define HTML_H

#include "HTUtils.h"
#include "HTFormat.h"
#include "HTAnchor.h"
#include "HTMLPDTD.h"

#define DTD HTMLP_dtd

#ifdef SHORT_NAMES
#define HTMLPresentation        HTMLPren
#define HTMLPresent             HTMLPres
#endif

extern CONST HTStructuredClass HTMLPresentation;

/*

HTML_new: A structured stream to parse HTML

   When this routine is called, the request structure may contain a childAnchor value.  I
   that case it is the responsability of this module to select  the anchor.
   
   
   
 */
extern HTStructured* HTML_new PARAMS((HTRequest * request,
                                        void *   param,
                                        HTFormat input_format,
                                        HTFormat output_format,
                                        HTStream * output_stream));

/*

  REOPEN
  
   Reopening an existing HTML object allows it to be retained (for example by the styled
   text object) after the structured stream has been closed. To be actually deleted, the
   HTML object must  be closed once more times than it has been reopened.
   
 */
extern void HTML_reopen PARAMS((HTStructured * me));

/*

Converters

 */
extern HTConverter HTMLToPlain, HTMLToC, HTMLPresent, HTMLToTeX;


/*

Selecting internal character set representations

 */
typedef enum _HTMLCharacterSet {
        HTML_ISO_LATIN1,
        HTML_NEXT_CHARS,
        HTML_PC_CP950
} HTMLCharacterSet;

extern void HTMLUseCharacterSet PARAMS((HTMLCharacterSet i));

/*

Record error message as a hypertext object

   The error message should be marked as an error so that it can be reloaded later. This
   implementation just throws up an error message and leaves the document unloaded.
   
  ON ENTRY,
  
  sink                    is a stream to the output device if any
                         
  number                  is the HTTP error number
                         
  message                 is the human readable message.
                         
  ON EXIT,
  
   a return code like HT_LOADED if object exists else < 0
   
 */
PUBLIC int HTLoadError PARAMS((
        HTRequest *     req,
        int             number,
        CONST char *    message));

#endif


/*

   end  */
