/*                                                      HTAccess:  Access manager  for libwww
                                      ACCESS MANAGER
                                             
   This module keeps a list of valid protocol (naming scheme) specifiers with associated
   access code.  It allows documents to be loaded given various combinations of
   parameters. New access protocols may be registered at any time.
   
   Note: HTRequest defined and request parametsr added to almost all calls 18 Nov 1993.
   
   Part of the libwww library .  Implemented by HTAcces.c
   
 */
#ifndef HTACCESS_H
#define HTACCESS_H

/*      Definition uses:
*/
#include "HTUtils.h"
#include "HTList.h"
#include "tcp.h"


#ifdef SHORT_NAMES
#define HTClientHost            HTClHost
#define HTSearchAbsolute        HTSeAbso
#define HTOutputStream          HTOuStre
#define HTOutputFormat          HTOuForm
#endif

typedef enum {
        METHOD_INVALID  = 0,
        METHOD_GET      = 1,
        METHOD_HEAD,
        METHOD_POST,
        METHOD_PUT,
        METHOD_DELETE,
        METHOD_CHECKOUT,
        METHOD_CHECKIN,
        METHOD_SHOWMETHOD,
        METHOD_LINK,
        METHOD_UNLINK,
        MAX_METHODS
} HTMethod;


/*

Methods

 */


/*      Get method enum value
**      ---------------------
*/
PUBLIC HTMethod HTMethod_enum PARAMS((char * name));


/*      Get method name
**      ---------------
*/
PUBLIC char * HTMethod_name PARAMS((HTMethod method));


/* PUBLIC                                               HTMethod_inList()
**              IS A METHOD IN A LIST OF METHOD NAMES
** ON ENTRY:
**      method          is the method to look for.
**      list            is a list of method names.
**
** ON EXIT:
**      returns         YES, if method was found.
**                      NO, if not found.
*/
PUBLIC BOOL HTMethod_inList PARAMS((HTMethod    method,
                                    HTList *    list));
/*

Match Template Against Filename

 */

/* PUBLIC                                               HTAA_templateMatch()
**              STRING COMPARISON FUNCTION FOR FILE NAMES
**                 WITH ONE WILDCARD * IN THE TEMPLATE
** NOTE:
**      This is essentially the same code as in HTRules.c, but it
**      cannot be used because it is embedded in between other code.
**      (In fact, HTRules.c should use this routine, but then this
**       routine would have to be more sophisticated... why is life
**       sometimes so hard...)
**
** ON ENTRY:
**      template        is a template string to match the file name
**                      agaist, may contain a single wildcard
**                      character * which matches zero or more
**                      arbitrary characters.
**      filename        is the filename (or pathname) to be matched
**                      agaist the template.
**
** ON EXIT:
**      returns         YES, if filename matches the template.
**                      NO, otherwise.
*/
PUBLIC BOOL HTAA_templateMatch PARAMS((CONST char * template,
                                       CONST char * filename));


/*

   The following have to be defined in advance of the other include files because of
   circular references.
   
 */
typedef struct _HTRequest HTRequest;

/*
** Callback to call when username and password
** have been prompted.
*/
typedef int (*HTRetryCallbackType) PARAMS((HTRequest * req));


#include "HTAnchor.h"
#include "HTFormat.h"
#include "HTAAUtil.h"           /* HTAAScheme, HTAAFailReason */
#include "HTAABrow.h"           /* HTAASetup */


/*      Return codes from load routines:
**
**      These codes may be returned by the protocol modules,
**      and by the HTLoad routines.
**      In general, positive codes are OK and negative ones are bad.
*/

#define HT_NO_DATA -9999        /* return code: OK but no data was loaded */
                                /* Typically, other app started or forked */

/*

Default Addresses

   These control the home page selection. To mess with these for normal browses is asking
   for user confusion.
   
 */
#define LOGICAL_DEFAULT "WWW_HOME"  /* Defined to be the home page */

#ifndef PERSONAL_DEFAULT
#define PERSONAL_DEFAULT "WWW/default.html"     /* in home directory */
#endif
#ifndef LOCAL_DEFAULT_FILE
#define LOCAL_DEFAULT_FILE "/usr/local/lib/WWW/default.html"
#endif
/*  If one telnets to a www access point,
    it will look in this file for home page */
#ifndef REMOTE_POINTER
#define REMOTE_POINTER  "/etc/www-remote.url"  /* can't be file */
#endif
/* and if that fails it will use this. */
#ifndef REMOTE_ADDRESS
#define REMOTE_ADDRESS  "http://info.cern.ch/remote.html"  /* can't be file */
#endif

/* If run from telnet daemon and no -l specified, use this file:
*/
#ifndef DEFAULT_LOGFILE
#define DEFAULT_LOGFILE "/usr/adm/www-log/www-log"
#endif

/*      If the home page isn't found, use this file:
*/
#ifndef LAST_RESORT
#define LAST_RESORT     "http://info.cern.ch/default.html"
#endif

/*      This is the default cache directory:
*/
#ifndef CACHE_HOME_DIR
#define CACHE_HOME_DIR          "/tmp/"
#endif

/*      The default directory for "save locally" and "save and execute" files:
*/
#ifndef SAVE_LOCALLY_HOME_DIR
#define SAVE_LOCALLY_HOME_DIR   "/tmp/"
#endif

/*

The Request structure

   When a request is handled, all kinds of things about it need to be passed along.  These
   are all put into a HTRequest structure.  Note there is also a global list of converters
   .
   
 */
struct _HTRequest {

/*

   The elements of the request structure are as follows.
   
  SET BY THE CALLER OF HTACCESS:
  
    Conditions of the request itself:
    
 */
        HTMethod        method;

/*

   An atom for the name of the operation using HTTP method names .
   
 */
        HTList *        conversions ;
/*

   NULL, or a list of conversions which the format manager can do in order to fulfill the
   request.  This is set by the caller of HTAccess. Typically points to a list set up an
   initialisation time for example by HTInit.
   
 */
        HTList *        encodings;      /* allowed content-encodings      */

/*

   The list of encodings acceptablein the output stream.
   
 */
        HTList *        languages;      /* accepted content-languages     */

/*

   The list of (human) language values acceptable in the response
   
 */
        BOOL (* callback ) PARAMS((
                                struct _HTRequest* request,
                                void *param));

/*

   A function to be called back in the event that a file has been saved to disk by
   HTSaveAndCallBack for example.
   
 */
        void *          context;        /* caller data -- HTAccess unaware */

/*

   An arbitrary pointer passed to HTAccess and passed back as a parameter to the callback
   .
   
 */
        HTStream*       output_stream;

/*

   NULL in the case of display to the user, or if a specific output stream is required,
   the stream.
   
 */
        HTAtom *        output_format;

/*

   The output format required, or a generic format such as www/present (present to user).
   
    Information about the requester
    
 */
        char *          from;

/*

   Email format address of person responible for request
   
  SET BY HTACCESS
  
   None of the bits below may be looked at by a client application except in the callback
   routine, when the anchor may be picked out.
   
 */
        HTParentAnchor* anchor;

/*

   The anchor for the object in question. Set immediately by HTAcesss.  Used by the
   protocol and parsing modules. Valid thoughout the access.
   
 */
        HTChildAnchor * childAnchor;    /* For element within the object  */

/*

   T he anchor for the sub object if any.  The object builder should ensure that htis is
   selected, highlighted, etc when the object is loaded. NOTE: Set by HTAccess.
   
 */
        void *          using_cache;

/*

   pointer to cache element if cache hit
   
  SERVER SIDE
  
 */

        HTAtom *        content_type;   /* Content-Type:                  */
        HTAtom *        content_language;/* Language                      */
        HTAtom *        content_encoding;/* Encoding                      */
        int             content_length; /* Content-Length:                */
        HTInputSocket * isoc;           /* InputSocket object for reading */
        char *          authorization;  /* Authorization: field           */
        HTAAScheme      scheme;         /* Authentication scheme used     */
/*

  CLIENT SIDE
  
 */

        HTList *        valid_schemes;  /* Valid auth.schemes             */
        HTAssocList **  scheme_specifics;/* Scheme-specific parameters    */
        char *          prot_template;  /* WWW-Protection-Template: field */
        HTAASetup *     setup;          /* Doc protection info            */
        HTAARealm *     realm;          /* Password realm                 */
        char *          dialog_msg;     /* Authentication prompt (client) */
        HTRetryCallbackType
                        retry_callback; /* Called when password entered   */
};

/*

   Just to make things easier especially for clients, here is a function to return a new
   blank request:
   
Create blank request

   This request has defaults in -- in most cases it will need some information added
   before being passed to HTAccess, but it will work as is for a simple request.
   
 */

PUBLIC HTRequest * HTRequest_new NOPARAMS;


/*

Delete request structure

   Frees also conversion list hanging from req->conversions.
   
 */

PUBLIC void HTRequest_delete PARAMS((HTRequest * req));


/*

Flags which may be set to control this module

 */

extern char * HTSaveLocallyDir;         /* Dir home for "save locally" files */
extern char * HTCacheDir;               /* Cache dir, default NULL: no cache */
extern char * HTClientHost;             /* Name or number of telnetting host */
extern FILE * logfile;                  /* File to output one-liners to */
extern BOOL HTSecure;                   /* Disable security holes? */
extern BOOL HTImServer;                 /* If I'm cern_httpd */
extern BOOL HTImProxy;                  /* If I'm cern_httpd as a proxy */
extern BOOL HTForceReload;              /* Force reload from cache or net */

/*

Load a document from relative name

  ON ENTRY,
  
  relative_name           The relative address of the file to be accessed.
                         
  here                    The anchor of the object being searched
                         
  request->anchor         not filled in yet
                         
  ON EXIT,
  
  returns    YES          Success in opening file
                         
  NO                      Failure
                         
 */
extern  BOOL HTLoadRelative PARAMS((
                CONST char *            relative_name,
                HTParentAnchor *        here,
                HTRequest *             request));


/*

Load a document from absolute name

  ON ENTRY,
  
  addr                    The absolute address of the document to be accessed.
                         
  filter                  if YES, treat document as HTML
                         
  request->anchor         not filled in yet
                         
 */

/*

  ON EXIT,
  
 */

/*

  returns YES             Success in opening document
                         
  NO                      Failure
                         
 */
extern BOOL HTLoadAbsolute PARAMS((CONST char * addr,
                HTRequest *             request));


/*

Load a document from absolute name to a stream

  ON ENTRY,
  
  addr                    The absolute address of the document to be accessed.
                         
  filter                  if YES, treat document as HTML
                         
  request->anchor         not filled in yet
                         
  ON EXIT,
  
  returns YES             Success in opening document
                         
  NO                      Failure
                         
   Note: This is equivalent to HTLoadDocument
   
 */
extern BOOL HTLoadToStream PARAMS((
                CONST char *            addr,
                BOOL                    filter,
                HTRequest *             request));


/*

Load if necessary, and select an anchor

   The anchor parameter may be a child anchor. The anchor in the request is set to the
   parent anchor.
   
  ON ENTRY,
  
  anchor                  may be a child or parenet anchor or 0 in which case there is no
                         effect.
                         
  request->anchor            Not set yet.
                         
  ON EXIT,
  
 */

/*

  returns YES             Success
                         
  returns NO              Failure
                         
  request->anchor         The parenet anchor.
                         
 */
extern BOOL HTLoadAnchor PARAMS((HTAnchor * a,
                        HTRequest *             request));


/*

Load a Document

   This is an internal routine, which has an address AND a matching anchor.  (The public
   routines are called with one OR the other.) This is, however, recursively called from
   file load module to try ftp.
   
  ON ENTRY,
  
   request->
   
                          anchor
                         
                         
                          a parent anchor with fully qualified
                         
                         
                         
                              hypertext reference as its address set
                         
   output_format
                          valid
                         
   output_stream
                          valid on NULL
                         
  ON EXIT,
  
   returns
   
                           Error has occured.
                         
   HT_LOADED
                          Success
                         
   HT_NO_DATA
                          Success, but no document loaded.
                         
                         
                         
                         
                         (telnet sesssion started etc)
                         
 */

PUBLIC int HTLoad PARAMS((HTRequest * request));
/*

Bind an anchor to a request structure without loading

   The anchor parameter may be a child anchor. The anchor in the request is set to the
   parent anchor. This is useful in non-interactive mode if no home-anchor is known.
   Actually the same as HTLoadAnchor(), but without loading
   
  ON ENTRY,
  
  anchor                  may be a child or parenet anchor or 0 in which case there is no
                         effect.
                         
  request->anchor         Not set yet.
                         
  ON EXIT,
  
 */

/*

   returns YES   Success
   
   returns NO    Failure
   
   request->anchor       The parenet anchor.
   
 */

extern BOOL HTBindAnchor PARAMS((HTAnchor *anchor, HTRequest *request));


/*

Make a stream for Saving object back

  ON ENTRY,
  
  request->anchor         is valid anchor which has previously beeing loaded
                         
  ON EXIT,
  
  returns                 0 if error else a stream to save the object to.
                         
 */


extern HTStream * HTSaveStream PARAMS((HTRequest * request));


/*

Search

   Performs a search on word given by the user. Adds the search words to the end of the
   current address and attempts to open the new address.
   
  ON ENTRY,
  
  *keywords               space-separated keyword list or similar search list
                         
  here                    The anchor of the object being searched
                         
 */
extern BOOL HTSearch PARAMS((
                CONST char *            keywords,
                HTParentAnchor*         here,
                HTRequest *             request));


/*

Search Given Indexname

   Performs a keyword search on word given by the user. Adds the keyword to  the end of
   the current address and attempts to open the new address.
   
  ON ENTRY,
  
  *keywords               space-separated keyword list or similar search list
                         
  *indexname              is name of object search is to be done on.
                         
 */
extern BOOL HTSearchAbsolute PARAMS((
        CONST char *            keywords,
        CONST char *            indexname,
        HTRequest *             request));


/*

Register an access method

   An access method is defined by an HTProtocol structure which point to the routines for
   performing the various logical operations on an object: in HTTP terms,  GET, PUT, and
   POST.
   
   Each of these routine takes as a parameter a request structure containing details ofthe
   request.  When the protocol class routine is called, the anchor elemnt in the request
   is already valid (made valid by HTAccess).
   
 */
typedef struct _HTProtocol {
        char * name;
        
        int (*load)PARAMS((HTRequest *  request));
                
        HTStream* (*saveStream)PARAMS((HTRequest *      request));

        HTStream* (*postStream)PARAMS((
                                HTRequest *     request,
                                HTParentAnchor* postTo));

} HTProtocol;

extern BOOL HTRegisterProtocol PARAMS((HTProtocol * protocol));


/*

Generate the anchor for the home page

 */

/*

   As it involves file access, this should only be done once when the program first runs.
   This is a default algorithm -- browser don't HAVE to use this.
   
 */
extern HTParentAnchor * HTHomeAnchor NOPARAMS;

#endif /* HTACCESS_H */

/*

   end of HTAccess */
