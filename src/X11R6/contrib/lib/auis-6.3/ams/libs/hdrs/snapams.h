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
		mscui.h -- Message server/common user interface SNAP include stuff
*/

/* MS op codes */

/*
	Keep to under 32 chars -- make life easy for PC

	 1111111111222222222233
1234567890123456789012345678901
*/
#define OP_MS_CREATE_NEW_MESSAG		1
#define OLDOP_MS_PROCESS_NEW_MESSA      2 /* Replaced -- bobg, 26-Oct-88 */
#define OP_MS_HEADERS_SINCE		3
#define OP_MS_GET_PARTIAL_FILE		4
#define OP_MS_GET_PARTIAL_BODY		5
#define OP_MS_SET_ASSOCIATED_TI		6
#define OP_MS_GET_ASSOCIATED_TI		7
#define OP_OLD_MS_PRINT_MESSAGE		8 /* Obsolete */
#define OP_MS_GET_VERSION		9
#define OP_MS_DEBUG_MODE		10
#define OP_MS_DISAMBIGUATE_FILE		11
#define OP_MS_GET_SEARCH_PATH_E		12
#define OP_MS_NAME_SUBSCRIPTION		13
#define OP_MS_SET_SUBSCRIPTION_		14
#define OP_MS_GET_SUBSCRIPTION_		15
#define OP_MS_DIE			16
#define OP_MS_OPEN_DEBUGGING_PI		17
#define OP_MS_REBUILD_SUBSCRIPT		18
#define OP_MS_NAME_REPLY_FILE		19
#define OP_MS_EPOCH			20
#define OP_MS_PURGE_DELETED_MES		21
#define OP_MS_ALTER_SNAPSHOT		22
#define OP_MS_GET_SNAPSHOT		23
#define OP_MS_GET_HEADER_CONTEN		24
#define OLDOP_SUBMIT_MESSAGE		25 /* OBSOLETE VALUE */
#define OP_MS_UPDATE_STATE		26
#define OP_MS_STORE_PARTIAL_FIL		27
#define OP_MS_INSTALL_WELCOME_M		28
#define OP_MS_REINITIALIZE		29
#define OLDOP_REWRITEHEADERLINE		30 /* OBSOLETE */
#define OP_MS_RECONSTRUCTDIRECT		31
#define OP_MS_FIND_MAILBOX		32
#define OP_MS_CLONE_MESSAGE		33
#define OP_MS_MERGE_DIRECTORIES		34
#define OP_MS_GETDIRINFO		35
#define OP_MS_REMOVE_DIRECTORY		36
#define OP_MS_UNLINK_FILE		37
#define OP_MS_EDIT_MESSAGE		38
#define OP_MS_GET_NEXT_SUBSENT		39
#define OP_MS_CONVERT_OLD_MAIL		40
#define OP_MS_RENAME_DIR		41
#define OP_MS_GET_NTH_SNAPSHOT		42
#define OP_MS_GET_NEW_MSG_CT		43
#define OLDOP_MS_PREFETCH_MADLY		44 /* OBSOLETE, UNSUPPORTED */
#define OP_MS_PREFETCH_MSG		45
#define OP_MS_FAST_UPDATE		46
#define OP_MS_CHECK_MISSING		47
#define OP_MS_REBUILDMASTERUP_OLD	48 /* OBSOLETE */
#define OP_MS_NAMECHANGEDMAP		49
#define OP_MS_SUBMIT_MESSAGE		50
#define OP_MS_WRITE_ALL_MATCHES		51
#define OP_MS_VALIDATE_CHUNK		52
#define OP_MS_WRITE_UNSCRIBED		53
#define OP_MS_DO_I_HAVE_MAIL		54
#define OP_MS_SET_DEATHKNELL		55
#define OP_MS_PARSE_DATE		56
#define OP_MS_HANDLE_PREFERENCE		57
#define	OP_MS_APPENDFILEFOLDER		58
#define OP_MS_CHECKAUTHENTICATION       59
#define	OP_MS_TAKEHINTS			60
#define	OP_MS_GETDIRATTRS		61
#define	OP_MS_DELETEATTR		62
#define	OP_MS_ADDATTR			63
#define OP_MS_REBUILD_ONE		64
#define OP_MS_REINDEX_ONE_OLD		65 /* OBSOLETE */
#define OP_MS_PRINT_MESSAGE		66
#define	OP_MS_GEN_TEMP_NAME		67
#define	OP_MS_REBUILDMASTERUP		68
#define OP_MS_REINDEX_ONE		69
#define	OP_MS_GETCONFIGPARMS		70
#define	OP_OLD_MS_ELI_EVAL		71 /* OBSOLETE */
#define	OP_MS_ANDREWDIR			72
#define OP_MS_PROCESS_NEW_MESSA         73
#define	OP_MS_MATCH_FOLDER_NAME		74
#define OP_MS_GET_VERSION_CONFIG	75
#define	OP_MS_DOMAIN_FORMAT		76
#define	OP_MS_SCAVENGE			77
#if 0

/* Layout of RPC call packet -- for documentation only */
struct {
    unsigned	Length:32;	/* Length (in bytes) of packet, including this field */
    unsigned	OpCode:32;	/* Op code -- routine to execute */
    unsigned	Args:*;		/* Space for arguments (unbounded size) */
} CallPacket;

/* Layout of RPC return packet -- documentation only */
struct {
    unsigned	Length:32;	/* Length (in bytes) of packet, including this field */
    unsigned	Error:32	/* Error from stubs at other end (not user routine) */
    unsigned	ReturnValue:32;	/* Return value from routine */
    unsigned	Args:*;		/* Space for return arguments (unbounded size) */
ReturnPacket;

#endif /* 0 */

#define CALL_PACKET_HEADER_LENGTH	8
#define RETURN_PACKET_HEADER_LENGTH	12

typedef char CallPacket;
typedef char ReturnPacket;

/* Macros for accessing fields */
#define CallPacketLength(p)		((long *) (p))
#define CallPacketOpCode(p)		((long *) ((char *) (p) + 4))
#define CallPacketArgs(p)		((char *) (p) + 8)

#define ReturnPacketLength(r)		((long *) (r))
#define ReturnPacketError(r)		((long *) ((char *) (r) + 4))
#define ReturnPacketReturnValue(r)	((long *) ((char *) (r) + 8))
#define ReturnPacketArgs(r)		((char *) (r) + 12)

/* Some SNAP stuff */

/* Total lengthof time for snap client to wait */
#define AMS_SNAP_TIMEOUT 80

#ifdef LINT_ARGS /* for PC compiler */
#define LINT 1
#endif /* LINT_ARGS  */

#ifdef LINT
#define getint(x,y) SNAP_ExtractIntFromMsg((char *) (x),  (y))
#define getstr(x,y) SNAP_ExtractStringFromMsg((char *) (x), (y))
#define getbytes(x,y) SNAP_ExtractBytesFromMsg((char *) (x), (y))

#define putint(x,y) SNAP_AppendIntToMsg((char *) (x), (y))
#define putstr(x,y) SNAP_AppendStringToMsg((char *) (x), (y))
#define putbytes(x,y, z) SNAP_AppendBytesToMsg((char *) (x), (y), (z))
#else /* LINT */
#define getint		SNAP_ExtractIntFromMsg
#define getstr		SNAP_ExtractStringFromMsg
#define getbytes	SNAP_ExtractBytesFromMsg

#define putint		SNAP_AppendIntToMsg
#define putstr		SNAP_AppendStringToMsg
#define putbytes	SNAP_AppendBytesToMsg
#endif /* LINT */
