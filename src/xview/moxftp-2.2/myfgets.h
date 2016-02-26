/*
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
 * $Id: myfgets.h,v 1.1 1994/03/14 18:57:41 jones Exp $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/myfgets.h,v $
 *
 * $Log: myfgets.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 */

#define BUFFSIZE 4096
struct _buffs {
    char   buff[BUFFSIZE +1];
    int    state;
    int    func;
    int    count;
    char   *ptr;
};

