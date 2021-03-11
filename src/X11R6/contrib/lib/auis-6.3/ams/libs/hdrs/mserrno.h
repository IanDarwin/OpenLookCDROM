/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


/*
	mserrno.h -- Error codes for message server & clients

	This include file attempts to categorize possible error
	exit statuses for the message server and its clients.
	It is intended to be compatible with sysexits.h.

	Error numbers begin at EMSBASE to reduce the possibility of
	clashing with other exit statuses that random programs may
	already return and with those defined in sysexits.h.

**********************************************************************************
				IMPORTANT NOTE
**********************************************************************************
	All additions to this file MUST be accompanied by a matching
	change to the arrays of diagnostic messages in amserr.c.

			DO NOT CHANGE ONE WITHOUT THE OTHER

**********************************************************************************
*/



#include <errno.h>

extern int errno;

#ifndef NODECLAREMSERRCODE
extern long mserrcode;
#endif /* NODECLAREMSERRCODE */

extern char *ErrDebugString;

/* Constant for ERR_CRITICAL messages that will cause termination */
#define ERR_FATAL ERR_CRITICAL - 1

#define AMS_ERRNO	((mserrcode >> 16) & 0xFF)
#define AMS_ERRCAUSE	((mserrcode >> 8) & 0xFF)
#define AMS_ERRVIA	(mserrcode & 0xFF)
#define AMS_RPCERRNO	((mserrcode >>24) & 0xFF)
#define AMS_RETURN_ERRCODE(a, b, c)\
	{debug(64, (ErrDebugString, a, b, c));\
	mserrcode = (((a) & 0xFF) << 16) | (((b) & 0xFF) << 8)  | ((c) & 0xFF);\
	return(mserrcode);}

#define AMS_RETURN_SNAP_ERRCODE(a)\
	{mserrcode = ((a) & 0xFF) << 24;\
	return(mserrcode);}

#define AMS_SET_ERRCODE(a, b, c)\
	mserrcode = (((a) & 0xFF) << 16) |\
			     (((b) & 0xFF) << 8)  |\
			     ((c) & 0xFF)

/* Return code for RPC errors */
#define CUI_RPC_ERROR(code) (mserrcode = ((code) &0xFF) << 24)

#define EMSBASE	128	/* base value for MS error messages */

#define EMSVICE	128	/* Vice Error of some sort */
#define EMSBOGUS 129	/* Error we just can't cope with */
#define EMSBUFSIZE 130	/* Buffer is not long enough */
#define EMSWOULDOVERWRITE 131	/* Operation would trash file */
#define EMSBADMSGFORMAT 132 	/* Doesn't look like a message */
#define EMSBADDIRFORMAT 133	/* Doesn't look like a msg dir */
#define EMSUNKNOWN 134		/* Unknown error */
#define EMSBADDIRSPEC 135	/* Directory parse spec program is invalid */
#define EMSORPHANMSG 136	/* Dir parse spec program yields no answers */
#define EMSBIGDIRSPECLINE 137	/* Dir parse spec program lines too long */
#define EMSDIRNEST 138		/* Dir spec program incorrectly nested */
#define EMSDIRNAMECOLLISION 139 /* Directory names should not be the same */
#define EMSBIGPROFLINE 140	/* Profile line is too big */
#define EMSNOSUCHUSER 141	/* Bogus ~ reference */
#define EMSNOHOME 142
#define EMSNOSUCHMESSAGE 143	/* No message for referenced msg id */
#define EMSCANNOTPRINT 144	/* Print exec failed or something liek that */
#define EMSRISKYDELIVERY 145	/* Risky delivery, success unknowable */
#define EMSNOPARENT 146		/* Directory has no parent */
#define EMSTOOMANYDIRS 147	/* Message would be posted on too many directoreis at once */
#define EMSEXPLAINED 148	/* Message server returned explanation text */
#define EMSTRYMAILERR 149	/* Unparsable return from trymail */
#define EMSYOUNGMAIL 150	/* Empty file in mailbox is very new */
#define EMSDIRNOTOPEN 151	/* MS Directory was not yet open */
#define EMSDIRALREADYOPEN 152	/* MS Directory is already open */
#define EMSWPOPENFAIL 153	/* Cannot open white pages */
#define EMSNOUNQUOTE 154	/* Cannot unquote address */
#define EMSWPSETUP 155		/* White pages setup failed */
#define EMSWPREAD 156		/* White pages read failed */
#define EMSMULTIPLECLOSEERRS 157	/* Failed to close several dirs */
#define EMSWPLIES 158 /* White pages answer makes no sense */
#define EMSUNAUTH 159 /* User appears to be unauthenticated */
#define EMSUNIMPL 160	/* Unimplemented feature */
#define EMSDIRHASKIDS 161 /* Message directory has children, cannot delete */
#define EMSRECONFAILED 162 /* Directory reconstruction failed */
#define EMSFILEERR 163	/* Generic file error from ferror */
#define EMSNOSUBJ 164
#define EMSDELIVERYFAIL 165
#define EMSBIGALIAS 166
#define EMSALREADYTHERE 167
#define EMSNOSPECFILE 168
#define EMSNOSUCHVAR 169
#define EMSDROPOFFWARN 170
#define EMSDROPOFFLOCALQUEUE 171
#define EMSDROPOFFNOQUEUE 172
#define EMSDROPOFFBADPARMS 173
#define EMSDROPOFFTEMPFAIL 174
#define EMSDROPOFFBADMSGFILE 175
#define EMSDROPOFFOSERR 176
#define EMSBADLOCALSYNTAX 177
#define EMSWPLOOKUPFAIL 178
#define EMSPREMATUREEOF 179
#define EMSTOOMANYHEADS 180
#define EMSBADDIRNAME 181
#define EMSSYNTAX 182
#define EMSYOUNGLOCK 183
#define EMSFASCISTSUBSCRIPTION 184
#define EMSENDOFLIST 185
#define EMSNONASCIIMAIL 186
#define EMSLONGLINES 187
#define EMSATTREXISTS 188
#define EMSGUARDIANERR 189
#define EMSSNAPAUTH 190
#define EMSNOVUID 191
#define EMSNOTTREEROOT 192
#define EMSNOMEM 193
#define EMSBADPRINTER 194
#define EMSWPCORRUPTION 195
#define EMSSNAPINIT 196
#define EMSTOOMANYPRINTS 197
#define EMSBADFILESIZE 198
#define EMSELISYNTAX 199
#define EMSELIUNBOUND 200
#define EMSELIUNDEF 201
#define EMSELIBADPARAM 202
#define EMSELIBADARGS 203
#define EMSELINOSYM 204
#define EMSELIARGLISTSIZE 205
#define EMSELIUSERERROR 206
#define EMSNODELIVERY 207
#define EMSUNSUPPORTED 208
#define EMSEVALBOGUS 209
#define EMSHOLDSET 210
#define EMSCLOCKBOGUS 211
#define EMSELISYSERROR 212
#define EMSLASTERROR 212 /***************/

#define EIN_UNKNOWN 0
#define EIN_FOPEN 1
#define EIN_FCLOSE 2
#define EIN_OPEN 3
#define EIN_CLOSE 4
#define EIN_STAT 5
#define EIN_READ 6
#define EIN_WRITE 7
#define EIN_FREAD 8
#define EIN_FWRITE 9
#define EIN_MALLOC 10
#define EIN_FSTAT 11
#define EIN_PARAMCHECK 12 /* Parameters to routine requested failure
				in the current state */
#define EIN_OPENDIR 13
#define EIN_SIZECHECK 14	/* Size of system file is wrong */
#define EIN_BCMP 15
#define EIN_MKDIR 16
#define EIN_LSEEK 17
#define EIN_DIRSPECPARSE 18
#define EIN_DIRECTORYLOCK 19
#define EIN_ELSEPARSE 20
#define EIN_ENDIFPARSE 21
#define EIN_PUSH 22
#define EIN_POP 23
#define EIN_COMMANDPARSE 24
#define EIN_DISAMB 25
#define EIN_VCLOSE 26
#define EIN_VFCLOSE 27
#define EIN_POPEN 28
#define EIN_PCLOSE 29
#define EIN_GETPWNAM 30
#define EIN_ACCESS 31
#define EIN_PATHSEARCH 32
#define EIN_GETENV 33
#define EIN_DUP2 34
#define EIN_FLOCK 35
#define EIN_WAIT 36
#define EIN_FORK 37
#define EIN_RINDEX 38
#define EIN_FERROR 39
#define EIN_RENAME 40
#define EIN_SETSOCKOPT 41
#define EIN_T2OPEN 42
#define EIN_TRUNCATE 43
#define EIN_FTRUNCATE 44
#define EIN_FGETS 45
#define EIN_PARSEADDR 46
#define EIN_UNPARSEADDR 47
#define EIN_HANDLEADDRESS 48
#define EIN_CLOSEMSDIR 49
#define EIN_INDEX 50
#define EIN_RETRY 51
#define EIN_UNLINK 52
#define EIN_RMDIR 53
#define EIN_RECON 54
#define EIN_UTIMES 55
#define EIN_QOPEN 56
#define EIN_QCLOSE 57
#define EIN_VFORK 58
#define EIN_CHMOD 59
#define EIN_PIOCTL 60
#define EIN_MSPATHCHECK 61
#define EIN_DROPOFF 62
#define EIN_LAKIND 63
#define EIN_UNSCRIBE 64
#define EIN_PARSEDATE 65
#define EIN_SETPROF 66
#define EIN_FDOPEN 67
#define EIN_VMOPEN 68
#define EIN_UCBMAILLOCK 69
#define EIN_GETVUID 70
#define EIN_GETVPWUID 71
#define EIN_SNAPCLIENTINIT 72
#define EIN_SNAPBEGINCONV 73
#define EIN_GETCELLFROMFILE 74
#define EIN_GETCELLFROMUSER 75
#define EIN_GETCELLFROMWS 76
#define EIN_SYSTEM 77
#define EIN_LOCALNAMEDB 78
#define EIN_ELI 79
#define EIN_MSELIEVAL 80
#define EIN_READLINK 81
#define EIN_READFLAMES 82
#define EIN_LASTERRORLOCATION 82 /***********/

#define EVIA_UNKNOWN 0
#define EVIA_PROCNEWMSGS 1
#define EVIA_GETPARTIALFILE 2
#define EVIA_CREATENEWMESSAGEDIRECTORY 3
#define EVIA_CREATENEWMSDIRECTORY 4
#define EVIA_DESTRUCTIVELYWRITEDIR 5
#define EVIA_READOLDMSDIR 6
#define EVIA_WRITEPUREFILE 7
#define EVIA_READRAWFILE 8
#define EVIA_CHOOSEDIRECTORIES 9
#define EVIA_BUILDREPLY 10
#define EVIA_BUILDWIDEREPLY 11
#define EVIA_PARSEMSGFROMRAWBODY 12
#define EVIA_ADDTODIRCACHE 13
#define EVIA_PROCESSCOMMAND 14
#define EVIA_SETASSOCIATEDTIME 15
#define EVIA_GETASSOCIATEDTIME 16
#define EVIA_READPROFILE 17
#define EVIA_WRITEPROFILE 18
#define EVIA_HEADERSSINCE 19
#define EVIA_PRINTMESSAGE 20
#define EVIA_DISAMB 21
#define EVIA_GETSEARCHPATHENTRY 22
#define EVIA_NAMESUBSCRIPTIONMAP 23
#define EVIA_SUBSTREEWALK 24
#define EVIA_WRITESUBS 25
#define EVIA_READSUBS 26
#define EVIA_OPENDEBUGGINGPIPESCRIPT 27
#define EVIA_NAMEREPLYFILE 28
#define EVIA_REBUILDSUBSCRIPTIONMAP 29
#define EVIA_GETSNAPSHOTFROMDIR 30
#define EVIA_PURGEDELETEDMESSAGES 31
#define EVIA_DELETETHROUGH 32
#define EVIA_EPOCH 33
#define EVIA_REJECTMESSAGE 34
#define EVIA_GETHEADERCONTENTS 35
#define EVIA_SUBMITMESSAGE 36
#define EVIA_LOCKPROFILE 37
#define EVIA_UNLOCKPROFILE 38
#define EVIA_STOREPARTIALFILE 39
#define EVIA_ADDPARENTAL 40
#define EVIA_CHECKOPENMSDIR 41
#define EVIA_MSCUIINIT 42
#define EVIA_REWRITEADDRESS 43
#define EVIA_APPENDMESSAGETOMSDIR 44
#define EVIA_READORFINDMSDIR 45
#define EVIA_RECONSTRUCTDIRECTORY 46
#define EVIA_BUILDCAPTIONFIELD 47
#define EVIA_CHECKMAILBOXES 48
#define EVIA_OPENMSDIR 49
#define EVIA_CLOSEMSDIR 50
#define EVIA_GETSNAPSHOTBYID 51
#define EVIA_GETSNAPSHOTBYNUMBER 52
#define EVIA_REWRITESNAPSHOTINDIR 53
#define EVIA_PLANTOCLOSEDIR 54
#define EVIA_CLOSEDIRSTHATNEEDIT 55
#define EVIA_CLONEMESSAGE 56
#define EVIA_COPYMESSAGEBODY 57
#define EVIA_MERGEDIRS 58
#define EVIA_GETDIRINFO 59
#define EVIA_REMOVEDIR 60
#define EVIA_UNLINKFILE 61
#define EVIA_EDITMESSAGE 62
#define EVIA_CONVERTOLD 63
#define EVIA_ENSUREINSUBS 64
#define EVIA_CHECKPERSONALALIAS 65
#define EVIA_RENAMEDIR 66
#define EVIA_INITSEARCHPATHS 67
#define EVIA_PREFETCH 68
#define EVIA_PREFETCHMSG 69
#define EVIA_CHECKMISSINGFOLDER 70
#define EVIA_UPDATEUPDATES 71
#define EVIA_FINDTREEROOT 72
#define EVIA_LOCKMASTERFILE 73
#define EVIA_UNLOCKMASTERFILE 74
#define EVIA_WRITECHANGEDSUBS 75
#define EVIA_REBUILDMASTERUPS 76
#define EVIA_SETSUBSENTRY 77
#define EVIA_VALCHUNK 78
#define EVIA_WRITEUNSCRIBED 79
#define EVIA_DOIHAVEMAIL 80
#define EVIA_PARSEDATE 81
#define EVIA_WRITEALLMATCHES 82
#define EVIA_HANDLEPREFERENCE 83
#define EVIA_CHECKAUTH 84
#define EVIA_CONVERTINCOMING 85
#define EVIA_RESENDMESSAGE 86
#define EVIA_GETDIRATTRS 87
#define EVIA_ADDATTR 88
#define EVIA_DELATTR 89
#define EVIA_UNFORMATMSG 90
#define EVIA_GUARANTEEFULLBODY 91
#define EVIA_GETHEADERSIZE 92
#define EVIA_EMITBE2PREFIX 93
#define EVIA_MACHINEINIT 94
#define EVIA_MARKINPROGRESS 95
#define EVIA_SETCHAIN 96
#define EVIA_FLAMES_HANDLENEW 97
#define EVIA_CUISNAP 98
#define EVIA_MSSNAP 99
#define EVIA_MSELI 100
#define EVIA_MSELILISP 101
#define EVIA_MATCHFOLDERNAME 102
#define EVIA_SCAVENGE 103
#define EVIA_BUILDDATE 104

#define EVIA_LASTERROR 104 		/*************/

/* RPC error values */

	/* Call went ok */
#define RPC_OK			1
	/* The opcode was unrecognized */
#define RPC_BAD_OPCODE		2

/******************************************************************\
* 								   *
*    These next 4 errors all have to do with buffer size problems  *
* 								   *
\******************************************************************/

	/* The arguments to an RPC from the client exceeded the client buffer size */
#define RPC_BAD_CALL_LENGTH_1	3
	/* The buffer from the client exceeded the server buffer size */
#define RPC_BAD_CALL_LENGTH_2	4
	/* The return args from the server exceeded the server buffer size */
#define RPC_BAD_CALL_LENGTH_3	5
	/* The buffer from the server exceeded the client buffer size */
#define RPC_BAD_CALL_LENGTH_4	6

	/* The return packet was never received */
#define RPC_TIMEOUT		7

	/* The Server could not initialize but hung on long enough to 
	    try to tell the client  */

#define RPC_SERVERDOA		8

	/**************************/
#define RPC_LASTERROR		8
