/*                                                               FTP access module for libwww
                                   FTP ACCESS FUNCTIONS
                                             
   This isn't really  a valid protocol module -- it is lumped together with HTFile . That
   could be changed easily.
   
   Author: Tim Berners-Lee. Public Domain. Please mail changes to timbl@info.cern.ch
   
 */
#ifndef HTFTP_H
#define HTFTP_H

#include "HTUtils.h"
#include "HTAnchor.h"
#include "HTStream.h"

/*

Retrieve File from Server

  ON EXIT,
  
  returns                 Socket number for file if good.<0 if bad.
                         
 */
extern int HTFTPLoad PARAMS((HTRequest * request));
#ifdef OLD_PARAMS
((
  HTRequest *           request,
  void *                param,
  CONST char *          name,
  HTParentAnchor *      anchor,
  HTFormat              format_out,
  HTStream*             sink
));
#endif

/*

Return Host Name

 */
extern CONST char * HTHostName NOPARAMS;

#endif

/*

   end */
