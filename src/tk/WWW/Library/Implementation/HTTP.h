/*                     /Net/dxcern/userd/timbl/hypertext/WWW/Library/Implementation/HTTP.html
                                HYPERTEXT TRANFER PROTOCOL
                                             
 */
#ifndef HTTP_H
#define HTTP_H

#include "HTAccess.h"

/*

  CACHE CONTROL FLAG
  
   Turn this off if you don't want HTTP protocol fetches to leave cache files.  extern
   BOOL  HTCacheHTTP; This variable is now replaced by the (char *) HTCacheDir in
   HTAccess.html Henrik 09/03-94
   
  PROTOCOL
  
 */
GLOBALREF HTProtocol HTTP;

#endif /* HTTP_H */

/*

   end of HTTP module definition */
