/* $XConsortium: XIMProto.h,v 1.5 92/04/14 13:28:34 rws Exp $ */
/******************************************************************

              Copyright 1991, 1992 by FUJITSU LIMITED
              Copyright 1991, 1992 by Sun Microsystems, Inc.

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of FUJITSU LIMITED
and Sun Microsystems, Inc.not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission. FUJITSU LIMITED and Sun Microsystems, Inc. makes no
representations about the suitability of this software for any purpose. 
It is provided "as is" without express or implied warranty.

FUJITSU LIMITED AND SUN MICROSYSTEMS, INC DISCLAIM ALL WARRANTIES WITH 
REGARD TO THIS SOFTWARE,INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS, IN NO EVENT SHALL FUJITSU LIMITED BE LIABLE FOR ANY SPECIAL, 
INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM 
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE
OR PERFORMANCE OF THIS SOFTWARE.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
                               fujiwara@a80.tech.yk.fujitsu.co.jp
          Hideki Hiura         Sun Microsystems, Inc.
	                       hhiura@Sun.COM
******************************************************************/

#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)XIMProto.h	2.1 93/10/04 SMI";
#endif
#endif

/* Ximp implementation revision */
#define XIMP_REVISION "Ximp Revision 3.4"

/* Ximp Protocol Version */
#ifdef XIMP_40
#define XIMP_PROTOCOL_VERSION "XIMP.4.0"
#else
#define XIMP_PROTOCOL_VERSION "XIMP.3.5"
#endif /* XIMP_40 */

/* Ximp Protocol Version Number */
#ifdef XIMP_40
#define XIMP_VERSION_NUMBER 40
#endif /* XIMP_40 */

/* Input Context ID */
typedef unsigned long	ICID;

/* ClientMessage No.  Ximp 4.0 */

/* client <=> frontend  */
#define  XIMP_KEYRELEASE4               100
#define  XIMP_KEYPRESS4			101
/* client ==> frontend  * Base Protocol */
#define  XIMP_CREATE4			102
#define  XIMP_DESTROY4			103
#define  XIMP_REG_KEY_PRESSED4		104
#define  XIMP_SETFOCUS4			105
#define  XIMP_UNSETFOCUS4		106
#define  XIMP_CLIENT_WINDOW4		107
#define  XIMP_FOCUS_WINDOW4		108
#define  XIMP_MOVE4			109
#define  XIMP_RESET4			110
#define  XIMP_SETVALUE4			111
#define  XIMP_GETVALUE4			112
/*                      * Callback  Protocol */
#define  XIMP_PREEDITSTART_RETURN4	120
#define  XIMP_PREEDITCARET_RETURN4	121
/* client <== frontend  * Base Protocol  */
#define  XIMP_SPROC_STARTED4		130
#define  XIMP_SPROC_STOPPED4		131
#define  XIMP_READPROP4			132
#define  XIMP_CLIENT_WINDOW_RETURN4	133
#define  XIMP_FOCUS_WINDOW_RETURN4	134
#define  XIMP_GETVALUE_RETURN4		135
#define  XIMP_RESET_RETURN4		136
#define  XIMP_CREATE_RETURN4		137
#define  XIMP_KEYPRESS_RETURN4		138
#define  XIMP_KEYRELEASE_RETURN4        139
/*                      * Callback  Protocol */
#define  XIMP_GEOMETRY4			140
#define  XIMP_PREEDITSTART4		150
#define  XIMP_PREEDITDONE4		151
#define  XIMP_PREEDITDRAW4		152
#define  XIMP_PREEDITDRAW_CM4		153
#define  XIMP_PREEDITDRAW_CM_TINY4	154
#define  XIMP_PREEDITDRAW_CM_FEEDBACK4	155
#define  XIMP_PREEDITCARET4		156
#define  XIMP_STATUSSTART4		160
#define  XIMP_STATUSDONE4		161
#define  XIMP_STATUSDRAW4		162
#define  XIMP_STATUSDRAW_CM4		163
/* client => IM Server for frontend method */
#define  XIMP_EVENTMASK_NOTIFY4         200
/* client <= IM Server for frontend method */
#define  XIMP_EVENTMASK_NOTIFY_RETURN4  201
/* Extension Protocol */
#define  XIMP_EXTENSION4		500
/* client <== frontend  * Error Protocol */
#define  XIMP_ERROR4			999

/* ClientMessage No.  Ximp 3.5 */
/* client <=> frontend  */
#define  XIMP_KEYPRESS3		 1
/* client ==> frontend  * Base Protocol */
#define  XIMP_CREATE3			 2
#define  XIMP_DESTROY3			 3
#define  XIMP_BEGIN3			 4
#define  XIMP_END3			 5
#define  XIMP_SETFOCUS3			 6
#define  XIMP_UNSETFOCUS3		 7
#define  XIMP_CHANGE3			 8
#define  XIMP_MOVE3			 9
#define  XIMP_RESET3			10
#define  XIMP_SETVALUE3			11
#define  XIMP_GETVALUE3			12
/*                      * Callback  Protocol */
#define  XIMP_PREEDITSTART_RETURN3	20
#define  XIMP_PREEDITCARET_RETURN3	21
/* client <== frontend  * Base Protocol  */
#define  XIMP_CREATE_RETURN3		 2
#define  XIMP_CONVERSION_BEGIN3		30
#define  XIMP_PROCESS_BEGIN3		30
#define  XIMP_CONVERSION_END3		31
#define  XIMP_PROCESS_END3		31
#define  XIMP_READPROP3			32
#define  XIMP_GETVALUE_RETURN3		33
#define  XIMP_RESET_RETURN3		34
/*                      * Callback  Protocol */
#define  XIMP_GEOMETRY3			40
#define  XIMP_PREEDITSTART3		41
#define  XIMP_PREEDITDONE3		42
#define  XIMP_PREEDITDRAW3		43
#define  XIMP_PREEDITDRAW_CM3		44
#define  XIMP_PREEDITCARET3		45
#define  XIMP_STATUSSTART3		46
#define  XIMP_STATUSDONE3		47
#define  XIMP_STATUSDRAW3		48
#define  XIMP_STATUSDRAW_CM3		49
#define  XIMP_PREEDITDRAW_TINY3		50
/* Extension Protocol */
#define  XIMP_EXTENSION3		90
/* client <== frontend  * Error Protocol */
#define  XIMP_ERROR3			99

/* For Backward Compatibility */
#ifndef XIMP_40
#define  XIMP_KEYPRESS			XIMP_KEYPRESS3
#define  XIMP_CREATE			XIMP_CREATE3
#define  XIMP_DESTROY			XIMP_DESTROY3
#define  XIMP_BEGIN			XIMP_BEGIN3
#define  XIMP_END			XIMP_END3
#define  XIMP_SETFOCUS			XIMP_SETFOCUS3
#define  XIMP_UNSETFOCUS		XIMP_UNSETFOCUS3
#define  XIMP_CHANGE			XIMP_CHANGE3
#define  XIMP_MOVE			XIMP_MOVE3
#define  XIMP_RESET			XIMP_RESET3
#define  XIMP_SETVALUE			XIMP_SETVALUE3
#define  XIMP_GETVALUE			XIMP_GETVALUE3
#define  XIMP_PREEDITSTART_RETURN	XIMP_PREEDITSTART_RETURN3
#define  XIMP_PREEDITCARET_RETURN	XIMP_PREEDITCARET_RETURN3
#define  XIMP_CREATE_RETURN		XIMP_CREATE_RETURN3
#define  XIMP_CONVERSION_BEGIN		XIMP_CONVERSION_BEGIN3
#define  XIMP_PROCESS_BEGIN		XIMP_PROCESS_BEGIN3
#define  XIMP_CONVERSION_END		XIMP_CONVERSION_END3
#define  XIMP_PROCESS_END		XIMP_PROCESS_END3
#define  XIMP_READPROP			XIMP_READPROP3
#define  XIMP_GETVALUE_RETURN		XIMP_GETVALUE_RETURN3
#define  XIMP_RESET_RETURN		XIMP_RESET_RETURN3
#define  XIMP_GEOMETRY			XIMP_GEOMETRY3
#define  XIMP_PREEDITSTART		XIMP_PREEDITSTART3
#define  XIMP_PREEDITDONE		XIMP_PREEDITDONE3
#define  XIMP_PREEDITDRAW		XIMP_PREEDITDRAW3
#define  XIMP_PREEDITDRAW_CM		XIMP_PREEDITDRAW_CM3
#define  XIMP_PREEDITCARET		XIMP_PREEDITCARET3
#define  XIMP_STATUSSTART		XIMP_STATUSSTART3
#define  XIMP_STATUSDONE		XIMP_STATUSDONE3
#define  XIMP_STATUSDRAW		XIMP_STATUSDRAW3
#define  XIMP_STATUSDRAW_CM		XIMP_STATUSDRAW_CM3
#define  XIMP_PREEDITDRAW_TINY		XIMP_PREEDITDRAW_TINY3
#define  XIMP_EXTENSION			XIMP_EXTENSION3
#define  XIMP_ERROR			XIMP_ERROR3
#endif /* !XIMP_40 */


/* Error Notify from IM Server */
/*  Detail Error Number */
#define  XIMP_NoError			0	/* No Error */
#define  XIMP_BadAlloc			1	/* Memeory Alloc Fail */
#define  XIMP_BadStyle         		2	/* Unspported Input Style */
#define  XIMP_BadClientWindow         	3	/* Invalid Client Window */
#define  XIMP_BadFocusWindow		4	/* Invalid Focus Window */
#define  XIMP_BadArea			5	/* Invalid Area */
#define  XIMP_BadSpotLocation		6	/* SpotLocation Out Of Range */
#define  XIMP_BadColormap		7	/* Invalid Colormap ID */
#define  XIMP_BadAtom			8	/* Invalid Atom ID */
#define  XIMP_BadPixel			9	/* Invalid Pixel Value */
#define  XIMP_BadPixmap			10	/* Invalid Pixmap Value */
#define  XIMP_BadName			11	/* Invalid Font Name */
#define  XIMP_BadCursor			12	/* Invalid Cursor ID */
#define  XIMP_BadProtocol		13	/* Invalid Protocol ID */
#define  XIMP_BadProperty		14	/* Invalid Property Name */
#define  XIMP_BadPropertyType		15	/* Invalid Property Type */

/* Property Name */
#define  _XIMP_PROTOCOL		"_XIMP_PROTOCOL"
#define  _XIMP_BASE		"_XIMP_" /* Root Window _XIP_<locale_name> */

/* IMS Window Property Name */
#define  _XIMP_VERSION			"_XIMP_VERSION"
#define  _XIMP_STYLE			"_XIMP_STYLE"
#define  _XIMP_SPROC_STARTED_KEYS	"_XIMP_SPROC_STARTED_KEYS"
#define  _XIMP_SPROC_STOPPED_KEYS	"_XIMP_SPROC_STOPPED_KEYS"
#define  _XIMP_KEYS			"_XIMP_KEYS"
#define  _XIMP_SERVERNAME		"_XIMP_SERVERNAME"
#define  _XIMP_SERVERVERSION		"_XIMP_SERVERVERSION"
#define  _XIMP_EXTENSIONS		"_XIMP_EXTENSIONS"
#define  _XIMP_PREEDITMAXSIZE		"_XIMP_PREEDITMAXSIZE"
#define  _XIMP_VENDORNAME		"_XIMP_VENDORNAME"
#define  _XIMP_TYPE			"_XIMP_TYPE"

/* Client Window Property Name */
#define  _XIMP_LIBVERSION	"_XIMP_VERSION"
#define  _XIMP_FOCUS		"_XIMP_FOCUS"
#define  _XIMP_PREEDIT		"_XIMP_PREEDIT"
#define  _XIMP_STATUS		"_XIMP_STATUS"
#define  _XIMP_PREEDITFONT	"_XIMP_PREEDITFONT"
#define  _XIMP_STATUSFONT	"_XIMP_STATUSFONT"

#define  _XIMP_CTEXT		"_XIMP_CTEXT"

/* CallBack Property Name */
#define  _XIMP_PREEDIT_DRAW_DATA		"_XIMP_PREEDIT_DRAW_DATA"
#define  _XIMP_FEEDBACKS			"_XIMP_FEEDBACKS"
#define  _XIMP_PREEDITDRAWLENGTH 		"_XIMP_PREEDITDRAWLENGTH"
#define  _XIMP_PREEDITDRAWSTRING 		"_XIMP_PREEDITDRAWSTRING"
#define  _XIMP_PREEDITDRAWFEEDBACK 		"_XIMP_PREEDITDRAWFEEDBACK"

#define  _XIMP_EXT_XIMP_CHOICE_START_REQ	"_XIMP_EXT_XIMP_CHOICE_START_REQ"
#define  _XIMP_EXT_XIMP_CHOICE_START_REP	"_XIMP_EXT_XIMP_CHOICE_START_REP"
#define  _XIMP_EXT_XIMP_CHOICE_DRAW_REQ		"_XIMP_EXT_XIMP_CHOICE_DRAW_REQ"
#define  _XIMP_EXT_XIMP_CHOICE_PROC_REQ		"_XIMP_EXT_XIMP_CHOICE_PROC_REQ"
#define  _XIMP_EXT_XIMP_CHOICE_PROC_REP		"_XIMP_EXT_XIMP_CHOICE_PROC_REP"
#define  _XIMP_EXT_XIMP_LOOKUPCHOICES		"_XIMP_EXT_XIMP_LOOKUPCHOICES"

/* Lookup choices REQ and REP */
#define  LOOKUP_CHOICES_BEGIN	        0
#define  LOOKUP_CHOICES_START_REQ	1
#define  LOOKUP_CHOICES_START_REP	2
#define  LOOKUP_CHOICES_PROCESS_REQ	3
#define  LOOKUP_CHOICES_PROCESS_REP	4
#define  LOOKUP_CHOICES_DRAW_REQ	5
#define  LOOKUP_CHOICES_DONE_REQ	6


/* mask (XIMP_CREATE, XIMP_SETVALUE, XIMP_GETVALUE) */
/* Ximp 4.0 */
#define XIMP_FOCUS_WIN_MASK4		(1L <<  0)
#define XIMP_PRE_AREA_MASK4		(1L <<  1)
#define XIMP_PRE_AREANEED_MASK4		(1L <<  2)
#define XIMP_PRE_COLORMAP_MASK4		(1L <<  3)
#define XIMP_PRE_STD_COLORMAP_MASK4	(1L <<  4)
#define XIMP_PRE_FG_MASK4		(1L <<  5)
#define XIMP_PRE_BG_MASK4		(1L <<  6)
#define XIMP_PRE_BGPIXMAP_MASK4		(1L <<  7)
#define XIMP_PRE_LINESP_MASK4		(1L <<  8)
#define XIMP_PRE_CURSOR_MASK4		(1L <<  9)
#define XIMP_PRE_SPOTL_MASK4		(1L << 10)
#define XIMP_STS_AREA_MASK4		(1L << 11)
#define XIMP_STS_AREANEED_MASK4		(1L << 12)
#define XIMP_STS_COLORMAP_MASK4		(1L << 13)
#define XIMP_STS_STD_COLORMAP_MASK4	(1L << 14)
#define XIMP_STS_FG_MASK4		(1L << 15)
#define XIMP_STS_BG_MASK4		(1L << 16)
#define XIMP_STS_BGPIXMAP_MASK4		(1L << 17)
#define XIMP_STS_LINESP_MASK4		(1L << 18)
#define XIMP_STS_CURSOR_MASK4		(1L << 19)
#define XIMP_STS_WINDOW_MASK4		(1L << 20)
#define XIMP_PRE_FONT_MASK4		(1L << 21)
#define XIMP_STS_FONT_MASK4		(1L << 22)
#define XIMP_SERVERTYPE_MASK4		(1L << 23)

/* Ximp 3.5 */
#define XIMP_FOCUS_WIN_MASK3          (1L <<  0)
#define XIMP_PRE_AREA_MASK3           (1L <<  1)
#define XIMP_PRE_FG_MASK3             (1L <<  2)
#define XIMP_PRE_BG_MASK3             (1L <<  3)
#define XIMP_PRE_COLORMAP_MASK3       (1L <<  4)
#define XIMP_PRE_BGPIXMAP_MASK3       (1L <<  5)
#define XIMP_PRE_LINESP_MASK3         (1L <<  6)
#define XIMP_PRE_CURSOR_MASK3         (1L <<  7)
#define XIMP_PRE_AREANEED_MASK3       (1L <<  8)
#define XIMP_PRE_SPOTL_MASK3          (1L <<  9)
#define XIMP_STS_AREA_MASK3           (1L << 10)
#define XIMP_STS_FG_MASK3             (1L << 11)
#define XIMP_STS_BG_MASK3             (1L << 12)
#define XIMP_STS_COLORMAP_MASK3       (1L << 13)
#define XIMP_STS_BGPIXMAP_MASK3       (1L << 14)
#define XIMP_STS_LINESP_MASK3         (1L << 15)
#define XIMP_STS_CURSOR_MASK3         (1L << 16)
#define XIMP_STS_AREANEED_MASK3       (1L << 17)
#define XIMP_STS_WINDOW_MASK3         (1L << 18)
#define XIMP_PRE_FONT_MASK3           (1L << 19)
#define XIMP_STS_FONT_MASK3           (1L << 20)

/* mask (For Backward Compatibility) */
#ifndef XIMP_40
#define XIMP_FOCUS_WIN_MASK	 	XIMP_FOCUS_WIN_MASK3
#define XIMP_PRE_AREA_MASK		XIMP_PRE_AREA_MASK3
#define XIMP_PRE_FG_MASK		XIMP_PRE_FG_MASK3
#define XIMP_PRE_BG_MASK		XIMP_PRE_BG_MASK3
#define XIMP_PRE_COLORMAP_MASK		XIMP_PRE_COLORMAP_MASK3
#define XIMP_PRE_BGPIXMAP_MASK		XIMP_PRE_BGPIXMAP_MASK3
#define XIMP_PRE_LINESP_MASK		XIMP_PRE_LINESP_MASK3
#define XIMP_PRE_CURSOR_MASK		XIMP_PRE_CURSOR_MASK3
#define XIMP_PRE_AREANEED_MASK		XIMP_PRE_AREANEED_MASK3
#define XIMP_PRE_SPOTL_MASK		XIMP_PRE_SPOTL_MASK3
#define XIMP_STS_AREA_MASK		XIMP_STS_AREA_MASK3
#define XIMP_STS_FG_MASK		XIMP_STS_FG_MASK3
#define XIMP_STS_BG_MASK		XIMP_STS_BG_MASK3
#define XIMP_STS_COLORMAP_MASK		XIMP_STS_COLORMAP_MASK3
#define XIMP_STS_BGPIXMAP_MASK		XIMP_STS_BGPIXMAP_MASK3
#define XIMP_STS_LINESP_MASK		XIMP_STS_LINESP_MASK3
#define XIMP_STS_CURSOR_MASK		XIMP_STS_CURSOR_MASK3
#define XIMP_STS_AREANEED_MASK		XIMP_STS_AREANEED_MASK3
#define XIMP_STS_WINDOW_MASK		XIMP_STS_WINDOW_MASK3
#define XIMP_PRE_FONT_MASK		XIMP_PRE_FONT_MASK3
#define XIMP_STS_FONT_MASK		XIMP_STS_FONT_MASK3
#endif /* !XIMP_40 */

/* MODE(FRONTEND or BACKEND), TYPE(Type1,2,3) and SYNC/ASYNC */
#define XIMP_FRONTEND4		(1L << 0)
#define XIMP_BACKEND4		(1L << 1)
#define XIMP_TYPE1		(1L << 2)
#define XIMP_TYPE2		(1L << 3)
#define XIMP_TYPE3		(1L << 4)
#define XIMP_SYNC		(1L << 5)
#define XIMP_FE_TYPE1		(XIMP_FRONTEND4 | XIMP_TYPE1)
#define XIMP_FE_TYPE2		(XIMP_FRONTEND4 | XIMP_TYPE2)
#define XIMP_FE_TYPE3		(XIMP_FRONTEND4 | XIMP_TYPE3)
#define XIMP_BE_TYPE1		(XIMP_BACKEND4  | XIMP_TYPE1)
#define XIMP_BE_TYPE2		(XIMP_BACKEND4  | XIMP_TYPE2)
#define XIMP_SYNC_BE_TYPE1	(XIMP_SYNC      | XIMP_BE_TYPE1)
#define XIMP_SYNC_BE_TYPE2	(XIMP_SYNC      | XIMP_BE_TYPE2)

/* MODE(FRONTEND or BACKEND) For Backward Compatibility */
#define XIMP_FRONTEND_BC_MASK	(1L << 0)
#define XIMP_BACKEND_BC_MASK	(1L << 1)

/* mask (For Backward Compatibility) */
#define XIMP_FRONTEND		0
#define XIMP_BACKEND		1

/*  XIMP_PREEDITDRAW_CM status value
 * post Ximp 3.4 protocol maybe compliant. 
 * XIMP status flag will may contain the supplementary infomations to 
 * reassemble the XIMPreeditDrawCallbackStruct.
 *	  +-----------------------------------------+
 *	0 | XIMP_PREEDITDRAW_CM                     |
 *	  +-----------------------------------------+
 *	4 | ICID                                    |
 *	  +-------------------+---------------------+
 *	8 |PreeditDrawCBStatus|       caret         |
 *	  +-------------------+---------------------+
 *	12|      chg_first    |      chg_length     |
 *	  +-------------------+---------------------+
 *	16|               feedback                  |
 *	  +-----------------------------------------+
 * PreeditDrawCBStatus:
 *    0x0001 no_text:  if 1, string == NULL (no following client message.)
 *    0x0002 no_feedback: if 1 feedback == NULL
 *    0x0004 feedbacks_via_property: if 1 , feedback field is property atom#
 **/

#define XIMP_PDCBSTATUS_NOTEXT 			0x0001
#define XIMP_PDCBSTATUS_NOFEEDBACK 		0x0002
#define XIMP_PDCBSTATUS_FEEDBACKS_VIA_PROP	0x0004

/* _XIMP_KEYS   struct  */

typedef struct {
	unsigned long		modifier;
	unsigned long		modifier_mask;
	KeySym			keysym;
} Ximp_Key;

typedef struct {
	unsigned short		 count_keys;
	Ximp_Key		*keys_list;
} Ximp_KeyList;

typedef struct _Ximp_Area {
	long		x;
	long		y;
	long		width;
	long		height;
} Ximp_AreaRec;

typedef struct _Ximp_Point {
	long		x;
	long		y;
} Ximp_PointRec;

typedef struct _Ximp_Size {
	long		width;
	long		height;
} Ximp_SizeRec;

/* kana-kanji conversion window attributes */

#ifdef XIMP_40

#define XIMP_PREEDIT_MAX_LONG4 15
#define XIMP_PREEDIT_MAX_CHAR4 60

typedef struct  _Ximp_Preedit4 {
	Ximp_AreaRec	Area;
	Ximp_SizeRec	AreaNeeded;
	Ximp_PointRec   SpotLocation;
	Colormap	Colormap;
	Atom		StdColormap;
	unsigned long   Foreground;
	unsigned long   Background;
	Pixmap		Bg_Pixmap;
	long		LineSpacing;
	Cursor		Cursor;
} Ximp_PreeditPropRec4;

#define XIMP_PREEDIT_MAX_LONG3 14
#define XIMP_PREEDIT_MAX_CHAR3 56

typedef struct  _Ximp_Preedit3 {
        Ximp_AreaRec    Area;
        unsigned long   Foreground;
        unsigned long   Background;
        Colormap        Colormap;
        Pixmap          Bg_Pixmap;
        long            LineSpacing;
        Cursor          Cursor;
        Ximp_SizeRec    AreaNeeded;
        Ximp_PointRec   SpotLocation;
} Ximp_PreeditPropRec3;

#define XIMP_STATUS_MAX_LONG4 14
#define XIMP_STATUS_MAX_CHAR4 56
#define XIMP_STATUS_MAX_LONG4 14
#define XIMP_STATUS_MAX_CHAR4 56

typedef struct  _Ximp_Status4 {
	Ximp_AreaRec	Area;
	Ximp_SizeRec	AreaNeeded;
	Colormap	Colormap;
	Atom		StdColormap;
	unsigned long   Foreground;
	unsigned long   Background;
	Pixmap		Bg_Pixmap;
	long		LineSpacing;
	Cursor		Cursor;
	Window		window;
} Ximp_StatusPropRec4;

#define XIMP_STATUS_MAX_LONG3 13
#define XIMP_STATUS_MAX_CHAR3 52
 
typedef struct  _Ximp_Status3 {
        Ximp_AreaRec    Area;
        unsigned long   Foreground;
        unsigned long   Background;
        Colormap        Colormap;
        Pixmap          Bg_Pixmap;
        long            LineSpacing;
        Cursor          Cursor;
        Ximp_SizeRec    AreaNeeded;
        Window          window;
} Ximp_StatusPropRec3;

#else /* XIMP_40 */ /* For Backward Compatibility  */

#define XIMP_PREEDIT_MAX_LONG  14
#define XIMP_PREEDIT_MAX_CHAR  56

typedef struct  _Ximp_Preedit {
        Ximp_AreaRec    Area;
        unsigned long   Foreground;
        unsigned long   Background;
        Colormap        Colormap;
        Pixmap          Bg_Pixmap;
        long            LineSpacing;
        Cursor          Cursor;
        Ximp_SizeRec    AreaNeeded;
        Ximp_PointRec   SpotLocation;
} Ximp_PreeditPropRec;

#define XIMP_STATUS_MAX_LONG  13
#define XIMP_STATUS_MAX_CHAR  52
 
typedef struct  _Ximp_Status {
        Ximp_AreaRec    Area;
        unsigned long   Foreground;
        unsigned long   Background;
        Colormap        Colormap;
        Pixmap          Bg_Pixmap;
        long            LineSpacing;
        Cursor          Cursor;
        Ximp_SizeRec    AreaNeeded;
        Window          window;
} Ximp_StatusPropRec;

#endif /* XIMP_40 */

/* for CallBack */
typedef struct _Ximp_PreeditDrawDataPropRec {
	long caret;
	long chg_first;
	long chg_length;
} Ximp_PreeditDrawDataPropRec, *Ximp_PreeditDrawDataProp;

/* for Commit, PreEditDraw, StatusDraw */
typedef struct _Ximp_CommitPropRec {
	unsigned long   icid;
	char            size;
	char            ctext[11];
} Ximp_CommitPropRec, Ximp_PreEditDrawCallbackPropRec,
  Ximp_StatusDrawCallbackPropRec;

/* for PreEditDraw */
typedef struct {
	short           chg_first;
	short           chg_length;
} Ximp_slong;

typedef union {
	Ximp_slong      slong;
	long            l;
} Ximp_uslong;
