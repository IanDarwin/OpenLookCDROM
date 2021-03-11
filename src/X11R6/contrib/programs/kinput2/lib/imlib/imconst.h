/* $Id: imconst.h,v 1.4 1994/06/02 05:01:17 ishisone Exp $ */

#ifndef _imconst_h
#define _imconst_h

/*
 * Constants defined by the prototol spec.
 */

/* Protocol version number */
#define XIM_MAJOR_PROTOCOL_VERSION	1
#define XIM_MINOR_PROTOCOL_VERSION	0

/* Major code for the core requests */
#define XIM_CONNECT			1
#define XIM_CONNECT_REPLY		2
#define XIM_DISCONNECT			3
#define XIM_DISCONNECT_REPLY		4
#define XIM_AUTH_REQUIRED		10
#define XIM_AUTH_REPLY			11
#define XIM_AUTH_NEXT			12
#define XIM_AUTH_SETUP			13
#define XIM_AUTH_NG			14
#define XIM_ERROR			20
#define XIM_OPEN			30
#define XIM_OPEN_REPLY			31
#define XIM_CLOSE			32
#define XIM_CLOSE_REPLY			33
#define XIM_REGISTER_TRIGGERKEYS	34
#define XIM_TRIGGER_NOTIFY		35
#define XIM_TRIGGER_NOTIFY_REPLY	36
#define XIM_SET_EVENT_MASK		37
#define XIM_ENCODING_NEGOTIATION	38
#define XIM_ENCODING_NEGOTIATION_REPLY	39
#define XIM_QUERY_EXTENSION		40
#define XIM_QUERY_EXTENSION_REPLY	41
#define XIM_SET_IM_VALUES		42
#define XIM_SET_IM_VALUES_REPLY		43
#define XIM_GET_IM_VALUES		44
#define XIM_GET_IM_VALUES_REPLY		45
#define XIM_CREATE_IC			50
#define XIM_CREATE_IC_REPLY		51
#define XIM_DESTROY_IC			52
#define XIM_DESTROY_IC_REPLY		53
#define XIM_SET_IC_VALUES		54
#define XIM_SET_IC_VALUES_REPLY		55
#define XIM_GET_IC_VALUES		56
#define XIM_GET_IC_VALUES_REPLY		57
#define XIM_SET_IC_FOCUS		58
#define XIM_UNSET_IC_FOCUS		59
#define XIM_FORWARD_EVENT	       	60
#define XIM_SYNC			61
#define XIM_SYNC_REPLY			62
#define XIM_COMMIT			63
#define XIM_RESET_IC			64
#define XIM_RESET_IC_REPLY		65
#define XIM_GEOMETRY			70
#define XIM_STR_CONVERSION		71
#define XIM_STR_CONVERSION_REPLY	72
#define XIM_PREEDIT_START		73
#define XIM_PREEDIT_START_REPLY		74
#define XIM_PREEDIT_DRAW		75
#define XIM_PREEDIT_CARET		76
#define XIM_PREEDIT_CARET_REPLY		77
#define XIM_PREEDIT_DONE		78
#define XIM_STATUS_START		79
#define XIM_STATUS_DRAW			80
#define XIM_STATUS_DONE			81
#define XIM_PREEDITSTATE		82

/* Data representation type */
#define TYPE_SEPARATOR			0
#define TYPE_CARD8			1
#define TYPE_CARD16			2
#define TYPE_CARD32			3
#define TYPE_CHAR			4
#define TYPE_WINDOW			5
#define TYPE_XIM_STYLES			10
#define TYPE_XRECTANGLE			11
#define TYPE_XPOINT			12
#define TYPE_XFONTSET			13
#define TYPE_XIM_OPTIONS		14
#define TYPE_XIM_HOT_KEY_TRIGGERS	15
#define TYPE_XIM_HOT_KEY_STATE		16
#define TYPE_XIM_STRING_CONVERSION	17
#define TYPE_XIM_PREEDIT_STATE		18
#define TYPE_XIM_RESET_STATE		19
#define TYPE_XIM_RESET_RETURN		20
#define TYPE_XIM_STRING_TEXT		21
#define TYPE_NESTED_LIST		0x7fff

/* Error code */
#define IMBadAlloc			1
#define IMBadStyle			2
#define IMBadClientWindow		3
#define IMBadFocusWindow		4
#define IMBadArea			5
#define IMBadSpotLocation		6
#define IMBadColormap			7
#define IMBadAtom			8
#define IMBadPixel			9
#define IMBadPixmap			10
#define IMBadName			11
#define IMBadCursor			12
#define IMBadProtocol			13
#define IMBadForeground			14
#define IMBadBackground			15
#define IMLocaleNotSupported		16
#define IMBadSomething			999

/* common flag */
#define XIM_FLAG_SYNCHRONOUS		1

/* XIM_FORWARD_EVENT flags */
#define XIM_FLAG_REQUEST_FILTERING	2
#define XIM_FLAG_REQUEST_LOOKUPSTRING	4

/* XIM_COMMIT flags */
#define XIM_FLAG_X_LOOKUP_CHARS		2
#define XIM_FLAG_X_LOOKUP_KEYSYM	4


/*
 * Constants defined by this implementaion
 */

/* Major code for extension requests (> 128) */
#define XIM_EXT_SET_EVENT_MASK		129
#define XIM_EXT_FORWARD_KEYEVENT	130
#define XIM_EXT_MOVE			131

/* Extension mask */
#define XIM_EXT_SET_EVENT_MASK_MASK	1
#define XIM_EXT_FORWARD_KEYEVENT_MASK	2
#define XIM_EXT_MOVE_MASK		4


/* Transport status code */
#define TRANSPORT_OK			0
#define TRANSPORT_EOF			1
#define TRANSPORT_PARTIAL		2
#define TRANSPORT_ERROR			3

#endif /* _imconst_h */
