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
 * $Id: ftp.h,v 1.1 1994/03/14 18:57:41 jones Exp $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/ftp.h,v $
 *
 * $Log: ftp.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 */


#define FTP_STD_ERROR      0
#define FTP_STD_NO_ERROR   1
#define FTP_STD_RESTART	   3
#define FTP_OK              00001
#define FTP_NOOP            00002 
#define FTP_ERROR           00004
#define FTP_NEED_LOGIN      00010
#define FTP_NEED_ACCOUNT    00020
#define FTP_NEED_RESTART    00040
#define FTP_ERROR_OK        00400
#define FTP_ERROR_RESTART   01000
#define FTP_ERROR_RECONN    02000

struct _ftp_response {
	char *code;
	int  status;
	char  *message;
};

extern int NUM_RESPONSE;

extern struct _ftp_response ftp_response[];
