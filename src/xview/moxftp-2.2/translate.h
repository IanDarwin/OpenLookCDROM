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
 * $Id: translate.h,v 1.1 1994/03/14 18:57:41 jones Exp $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/translate.h,v $
 *
 * $Log: translate.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 */


#define NUM_TRAN 20
#define FILE_NAME    1
#define FILE_SIZE    2
#define FILE_DATE    3
#define FILE_TIME    4
#define FILE_OWNER   5
#define FILE_PROT    6
#define FILE_GROUP   7
#define FILE_P1      8
#define FILE_P2      9

#define TRAN_OPTION_USER   001

#define UPPER 1
#define LOWER 2

#define N_T 50
#define N_E 100
#define N_LISTING 9
#define N_GUESS 50
#define N_TYPE  50

#undef unix
struct _translate {
 char   *guess[N_GUESS]; /* Guess system type based on login greeting */
 char   *guess_cwd;	 /* Guess system type based in illegal cwd */
 char   *type[N_TYPE];	 /* Sete system type base on syst type */
 char   *system;	 /* System name */
 char   *system_note;    /* System note */
 char   *init;		 /* Command to do on initial connection */
 char   *up;		 /* Text string to go up a directory tree */
 char   *up_exp;	 /* Regular expression to use to write remote_dir */
			 /* to go up directory tree */
 regexp *up_regexp;	 
 char   *up_source;     
 char   *cd_exp;	 /* Regular expression to use to extract info from */
			 /* remote_dir to do a change dir (A20 hack) */
 regexp *cd_regexp; 
 char   *cd_source;
 char   *file_name;      /*  Not Used */
 char   *file_size;
 char   *file_date;
 char   *file_owner;
 char   *file_prot;
 char   *file_group;
 char   *file_p1;
 char   *file_p2;
 char   *format_m;
 char   *format_l;
 char   *unix_examples[N_E];  /* Local unix  system example file name */
 char   *remote_examples[N_E]; /* Remote system example file name */
 int	 options;
 int	 remote;
 int	 local;
 int	 indent;
 int	 unix;
 int     ibm_rt;
 int	 apollo;
 int	 notype;
 int 	 no_pwd;
 char   *convert_size_exp;    /* Regular expression to apply to size field */
 regexp *convert_size_regexp;
 char   *convert_size_source;
 char   *is_dir;	      /* Regular expression to figure out file is a 
			      /* directory */
 regexp *is_dir_regexp;
 int     is_dir_field;
 char   *dir_name_exp;        /* Regular expression to extarct directory */
                              /* name  */
 regexp *dir_name_regexp;
 char   *mode_exp[N_E];	      /* Regular expression to extract */
			      /*  mode/protection fields */
 regexp *mode_regexp[N_E];
 char   *mode[N_E];
 char   *exp;		      /* Regular expression to identify valid */
			      /* filename entry */
 regexp *regexp;
 int     map[10];	      /* Valid fields */
 int     listing_l[N_LISTING]; /* Long listing format */
 int     listing_m[N_LISTING]; /* Short listing format */
 char   *dir_exp[N_T];        /* Regular expression to identify a directory */
 regexp *dir_regexp[N_T];
 char   *dir_source[N_T];
 int     map_index;
 char   *use_other; 	        /* Use his translation table for files */
 char   *file_to_unix_exp[N_T]; 
 regexp *file_to_unix_regexp[N_T];
 char   *file_to_unix_source[N_T];
 int     file_to_unix_mode[N_T];
 char   *file_to_other_exp[N_T];
 regexp *file_to_other_regexp[N_T];
 char   *file_to_other_source[N_T];
 int     file_to_other_mode[N_T];
 char   *remote_to_get_rewrite_exp[N_T];
 regexp *remote_to_get_rewrite_regexp[N_T];
 char   *remote_to_get_rewrite_source[N_T];
};

