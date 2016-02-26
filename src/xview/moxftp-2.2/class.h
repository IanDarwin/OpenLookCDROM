/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * $Id: class.h,v 1.1 1994/03/14 18:57:41 jones Exp jones $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/class.h,v $
 *
 * $Log: class.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 */

#if defined(XAW)
#define XFTPNAME            "xftp"
#define XFTPCLASS           "Xftp"
#define XFTPRESOURCE         XFTPCLASS 
#define XFTPRESOURCE_DEBUG  "Xftp.ad"
#define GENERAL_HELP	    "help_General"
#define XFTPICONNAME	    "XFTP"
#endif
#if defined(MOTIF)
#define XFTPNAME            "mftp"
#define XFTPCLASS           "Mftp"
#define XFTPRESOURCE         XFTPCLASS 
#define XFTPRESOURCE_DEBUG  "Mftp.ad"
#define GENERAL_HELP	    "help_General"
#define XFTPICONNAME	    "MFTP"
#endif
#if defined(OPENWINDOW)
#define XFTPNAME            "oftp"
#define XFTPCLASS           "Oftp"
#define XFTPRESOURCE         XFTPCLASS 
#define XFTPRESOURCE_DEBUG  "Oftp.ad"
#define GENERAL_HELP	    "help_General"
#define XFTPICONNAME	    "OFTP"
#endif

