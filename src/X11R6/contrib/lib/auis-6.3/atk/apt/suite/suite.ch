/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Suite-object

MODULE	suite.ch

VERSION	1.0

AUTHOR	TC Peters & GW Keim
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Suite-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  11/01/88	Created (GW Keim)

END-SPECIFICATION  ************************************************************/



#include <vector.ih>
#define	suite_VERSION			    1

struct color_state {
    char				    *caption_name;
    char				    *foreground_name;
    char				    *background_name;

    unsigned char			     caption_color[3];
    unsigned char			     foreground_color[3];
    unsigned char			     background_color[3];
};


typedef struct suite_item *suite_type_item;
struct suite_item {
	char				    *name;
	struct suite			    *suite;
	char				    *caption;
	struct fontdesc			    *captionfont;
	char				    *captionfontname;
	char				    *title;
	char				    *titlefontname;
	struct fontdesc			    *titlefont;
	char				    *dataobjectname;
	struct dataobject		    *dataobject;
	long				   (*dataobjecthandler)();
	char				    *viewobjectname;
	struct view			    *viewobject;
	struct rectangle		     inset_rect;
	struct rectangle		     title_rect;
	struct rectangle		     caption_rect;
	struct rectangle		     bounds;
	long				   (*viewobjecthandler)();
	long				   (*hithandler)();
	long				     datum;
	struct fontdesc			    *cursorfont;
	char				    *cursorfontname;
	struct cursor			    *cursor;
	short				     captionfontsize;
	short				     titlefontsize;
	unsigned char			     cursorbyte;
	unsigned char			     bordersize;
	unsigned char			     captionalignment;
	unsigned char			     titleplacement;
	unsigned char			     titlecaptionalignment;
	unsigned char			     highlightstyle;
	unsigned char			     passivestyle;
	unsigned char			     titlefonttype;
	unsigned char			     captionfonttype;
	unsigned char			     borderstyle;
	unsigned char			     mode;
	unsigned char			     accesstype;
	boolean				     exposed;
	struct vector			    *breaks;
	boolean				     debug;
	long				     position;
	struct color_state		    *color;
};

class suite : aptv {

    classprocedures:

	InitializeClass() returns boolean;
	InitializeObject() returns boolean;
	FinalizeObject() returns void;
	Create(suite_Specification,unsigned anchor) returns struct suite *;

    overrides:

	FullUpdate(enum view_UpdateType type,long left,long top,long width,long height);
	Update();
	Hit(enum view_MouseAction action,long x,long y,long numberOfClicks) returns struct view *;
	ReceiveInputFocus();
	LoseInputFocus();
	LinkTree( struct view *parent );

    methods:

	CreateItem( char *name, long datum ) returns struct suite_item *;
	DestroyItem(struct suite_item *item) returns void;
	ClearAllItems();
	ExposeItem(struct suite_item *item);
	HideItem(struct suite_item *item);
	Reset(long state) returns long;
	PassivateItem(struct suite_item *item);
	ActivateItem(struct suite_item *item);
	ItemActivated( struct suite_item *item ) returns boolean;
	HighlightItem( struct suite_item *item ) returns long;
	ItemHighlighted( struct suite_item *item ) returns boolean;
	NormalizeItem( struct suite_item *item ) returns long;
	ItemNormalized( struct suite_item *item ) returns boolean;
	Apply((long(*)())proc,unsigned anchor,unsigned datum);
	Sort(unsigned mode,long (*handler)());
	SelectedItems(long *number) returns struct suite_item **;
	SuiteAttribute(long attribute) returns long;
	SetSuiteAttribute(long attribute) returns long;
	ChangeSuiteAttribute(long attribute) returns long;
	ItemAttribute(struct suite_item *item,long attribute) returns long;
	SetItemAttribute(struct suite_item *item,long attribute) returns long;
	ChangeItemAttribute(struct suite_item *item,long attribute) returns long;
	ItemOfDatum(long datum) returns struct suite_item *;
	ItemsOfDatum(long datum) returns struct suite_item **;
	ItemOfName(char *name) returns struct suite_item *;
	ItemsOfName(char *name) returns struct suite_item **;
	ItemAtPosition(long position) returns struct suite_item *;
	SetDebug(boolean value);
	HighlightTitle();
	NormalizeTitle();

	SetSuiteFGColor(char *name, int red, int green, int blue);
	SetSuiteBGColor(char *name, int red, int green, int blue);
	GetSuiteFGColor(unsigned char rgb_vect[]) returns char *;
	GetSuiteBGColor(unsigned char rgb_vect[]) returns char *;

	SetItemCaptionColor(struct suite_item *item, char *name, int red, int green, int blue);
	SetItemFGColor(struct suite_item *item, char *name, int red, int green, int blue);
	SetItemBGColor(struct suite_item *item, char *name, int red, int green, int blue);

	SetActiveItemCaptionColor(char *name, int red, int green, int blue);
	SetActiveItemFGColor(char *name, int red, int green, int blue);
	SetActiveItemBGColor(char *name, int red, int green, int blue);
	GetActiveItemFGColor(unsigned char rgb_vect[]) returns char *;
	GetActiveItemBGColor(unsigned char rgb_vect[]) returns char *;

	SetPassiveItemCaptionColor(char *name, int red, int green, int blue);
	SetPassiveItemFGColor(char *name, int red, int green, int blue);
	SetPassiveItemBGColor(char *name, int red, int green, int blue);
	GetPassiveItemFGColor(unsigned char rgb_vect[]) returns char *;
	GetPassiveItemBGColor(unsigned char rgb_vect[]) returns char *;

	ParseRGB(char *rgb_string, unsigned char rgb_vect[]);

    macromethods:

        CurrentItem()		    (self->current_item)
	ItemCount()		    (self->items ? vector_Count(self->items):0)
	ItemAnchor(item)	    (item->anchor)
	ItemDataObject(item)	    (item->dataobject)
	ItemViewObject(item)	    (item->viewobject)
	ItemExposed(item)	    (item->exposed)
	SetExceptionHandler(handler)(self->exception_handler)
	ExceptionCode()		    (self->exception_status)
	ExceptionItem()		    (self->exception_item)
	FirstVisible()		    (self->firstvisible)
	LastVisible()		    (self->lastvisible)
	SetFirstVisible(item)	    ((self)->newfirstvisible = item)
	SetLastVisible(item)	    ((self)->lastvisible = item)

    data:

	struct suite_item	    *current_item;
	struct apt		    *apt;
	struct vector		    *items;
	struct suiteev		    *setview;
	struct rectangle	     container;
	struct rectangle	     bounds;
	struct scroll		    *scroll;
	struct rectangle	     scrollrect;
	char			    *title_caption;
	struct rectangle	     title_rect;
	unsigned		     titlehighlightstyle;
	unsigned		     titleborderstyle;
	short			     titlebordersize;
	char			    *titledataobjectname;
	struct dataobject	    *titledataobject;
	struct view		    *titleviewobject;
	long			   (*titledataobjecthandler)();
	long			   (*titleviewobjecthandler)();
	unsigned		     titleplacement;
	unsigned		     titlecaptionalignment;
	long			   (*titlehithandler)();
	char			    *titleviewobjectname;
	char			    *titlefontname;
	struct fontdesc		    *titlefont;
	long			     titlefontsize;
	unsigned		     titlefonttype;
	unsigned		     titlehighlighted;
	unsigned		     itemorder;
	unsigned		     arrangement;
	unsigned		     selection_mode;
	unsigned		     borderstyle;
	short			     bordersize;
	unsigned		     scrolltype;
	char			    *captionfontname;
	struct fontdesc		    *captionfont;
	short			     captionfontsize;
	unsigned		     captionfonttype;
	char			    *itemtitlefontname;
	short			     itemtitlefontsize;
	unsigned		     itemtitlefonttype;
	unsigned		     captionalignment;
	unsigned		     itemborderstyle;
	unsigned		     itembordersize;
	unsigned		     itemhighlightstyle;
	unsigned		     itempassivestyle;
	unsigned		     itemtitleplacement;
	unsigned		     itemtitlecaptionalignment;
	unsigned		     accesstype;
	char			    *itemviewobjectname, 
				    *itemdataobjectname;
	long			   (*itemviewobjecthandler)(), 
				   (*itemdataobjecthandler)();
	long			   (*hithandler)();
	long			     anchor, datum;
	struct suite_item	    *firstvisible, *lastvisible, *newfirstvisible;
	long			     visiblerows, visiblecolumns, 
				     rows, columns, numvisible;
	short			     itemwidth, itemheight;
	short			     itemfixedwidth, itemfixedheight;
	struct cursor		    *cursor;
	char			    *cursorfontname;
	struct fontdesc		    *cursorfont;
	char			     cursorbyte;
	struct cursor		    *itemcursor;
	char			    *itemcursorfontname;
	struct fontdesc		    *itemcursorfont;
	char			     itemcursorbyte;
	long			   (*sort_handler)();
	unsigned		     sortorder;
	short			     x_guttersize, y_guttersize;
	struct suite_item	   **itemarray;
	long			   (*exception)();
	long			     exception_status;
	struct suite_item	    *exception_item;
	long			     title_m_width, caption_m_width;
	boolean			     has_focus;
	boolean			     debug;
	short			     wrappingstyle;
	long			     max_item_pos_given;
	struct sbutton_prefs	    *buttonprefs;
	struct color_state	    *suiteColor;
	struct color_state	    *activeItemColor;
	struct color_state	    *passiveItemColor;
	double			     suite_bg_shade[3];
	boolean			     mono;
};

/*** Suite Object-types ***/
#define	suite_NoObject				    0
#define	suite_ItemObject			    1
#define	suite_TitleObject			    2

/***  Suite Attribute Values  ***/

/*** List Wrapping Styles ***/
#define	 suite_LeftIndent		    (1)
#define	 suite_LeftRight		    (1<<1)

/*** Ordering ***/
#define  suite_ColumnMajor		    (1)
#define  suite_RowMajor			    (1<<1)

/***  Arrangements  ***/
#define  suite_Matrix			    (1)
#define  suite_Column			    (1<<1)
#define  suite_Row			    (1<<2)
#define  suite_Balanced			    (1<<3)
#define  suite_Unbalanced		    (1<<4)
#define  suite_List			    (1<<5)
#define  suite_RowLine			    (1<<6)
#define  suite_ColumnLine		    (1<<7)
#define	 suite_Fixed			    (1<<8)

/***  BorderStyles  ***/
#define  suite_Rectangle		    (1)
#define  suite_Invisible		    (1<<5)
#define	 suite_Line			    (1<<6)

/***  Selection Modes   ***/
#define  suite_Exclusive		    (1)
#define  suite_Inclusive		    (1<<1)
#define	 suite_Toggle			    (1<<2)

/***  TitlePlacements  ***/
#define  suite_Left			    (1)
#define  suite_Right			    (1<<1)
#define  suite_Top			    (1<<2)
#define  suite_Bottom			    (1<<3)
#define  suite_Center			    (1<<4)
#define	 suite_Middle			    (1<<5)

/***  Highlight Styles ***/
#define  suite_Invert			    (1)
#define  suite_Border			    (1<<1)
#define  suite_Bold			    (1<<2)
#define  suite_Italic			    (1<<3)
#define  suite_Pale			    (1<<4)
#define  suite_None			    (1<<9)
#define  suite_Removed			    (1<<6)

/*** Item AccessTypes ***/
#define  suite_ReadOnly			    (1)
#define  suite_ReadWrite		    (1<<1)
#define	 suite_Proceed			    (1<<2)

/*** Sort Modes ***/
#define  suite_Alphabetic		    (1)
#define  suite_Numeric			    (1<<1)
#define  suite_Ascend			    (1<<2)
#define  suite_Descend			    (1<<3)

/*** Sort fields ***/
#define suite_ByCaption			    (1<<4)
#define suite_ByTitle			    (1<<5)
#define suite_ByDatum			    (1<<6)

/*** Reset States ***/
#define	suite_Clear			    (1)
#define	suite_ClearItems		    (1<<1)
#define	suite_ClearTitle		    (1<<2)
#define	suite_Normalize			    (1<<3)
#define	suite_Activate			    (1<<4)
#define	suite_Immediate			    (1<<5)
#define	suite_Defer			    (1<<6)
#define	suite_Expose			    (1<<7)

#define	suite_NoCursor			    (0)

/*** Exception Codes ***/
#define suite_NonExistentItem		    (1)
#define suite_InsufficientSpace		    (1<<1)
#define suite_AttributeConflict		    (1<<2)

typedef	struct suite_specification  suite_Specification;
struct  suite_specification {
    char attribute;
    long value;
};

/*** Attribute Macros ***/

#define suite_TitleCaption(x)		    suite_titlecaption,			(long) (x)
#define suite_TitleCaptionAlignment(x)	    suite_titlecaptionalignment,	(long) (x)
#define suite_TitleBorderStyle(x)	    suite_titleborderstyle,		(long) (x)
#define suite_TitleHighlightStyle(x)	    suite_titlehighlightstyle,		(long) (x)
#define suite_TitleBorderSize(x)	    suite_titlebordersize,		(long) (x)
#define suite_ItemOrder(x)		    suite_itemorder,			(long) (x)
#define suite_ItemCaptionList(x)	    suite_itemcaptionlist,		(long) (x)
#define suite_Item(x)			    suite_itemspec,			(long) (x)
#define suite_SelectionMode(x)		    suite_selectionmode,		(long) (x)
#define suite_BorderStyle(x)		    suite_borderstyle,			(long) (x)
#define suite_BorderSize(x)		    suite_bordersize,			(long) (x)
#define suite_HitHandler(x)		    suite_hithandler,			(long) (x)
#define suite_Arrangement(x)		    suite_arrangement,			(long) (x)
#define suite_Scroll(x)			    suite_scroll,			(long) (x)
#define suite_TitlePlacement(x)		    suite_titleplacement,		(long) (x)
#define	suite_FontName(x)		    suite_fontname,			(long) (x)
#define suite_TitleFontName(x)		    suite_titlefontname,		(long) (x)
#define suite_TitleCaptionFontName(x)	    suite_titlefontname,		(long) (x)
#define suite_TitleViewObjectName(x)	    suite_titleviewobjectname,		(long) (x)
#define suite_TitleViewObjectHandler(x)	    suite_titleviewobjecthandler,	(long) (x)
#define suite_TitleDataObjectName(x)	    suite_titledataobjectname,		(long) (x)
#define suite_TitleDataObjectHandler(x)	    suite_titledataobjecthandler,	(long) (x)
#define	suite_TitleDataObject(x)	    suite_titledataobject,		(long) (x)
#define	suite_TitleViewObject(x)	    suite_titleviewobject,		(long) (x)
#define suite_TitleHitHandler(x)	    suite_titlehithandler,		(long) (x)
#define	suite_ItemPosition(x)		    suite_itemposition,			(long) (x)
#define suite_ItemCaptionFontName(x)	    suite_itemcaptionfontname,		(long) (x)
#define suite_ItemBorderStyle(x)	    suite_itemborderstyle,		(long) (x)
#define suite_ItemBorderSize(x)		    suite_itembordersize,		(long) (x)
#define suite_ItemHighlightStyle(x)	    suite_itemhighlightstyle,		(long) (x)
#define suite_ItemPassiveStyle(x)	    suite_itempassivestyle,		(long) (x)
#define suite_ItemCaption(x)		    suite_itemcaption,			(long) (x)
#define suite_ItemCaptionAlignment(x)	    suite_itemcaptionalignment,		(long) (x)
#define suite_ItemTitleCaption(x)	    suite_itemtitlecaption,		(long) (x)
#define suite_ItemTitleFontName(x)	    suite_itemtitlefontname,		(long) (x)
#define suite_ItemTitleCaptionFontName(x)   suite_itemtitlefontname,		(long) (x)
#define suite_ItemTitleCaptionAlignment(x)  suite_itemtitlecaptionalignment,	(long) (x)
#define suite_ItemTitlePlacement(x)	    suite_itemtitleplacement,		(long) (x)
#define suite_ItemAccessMode(x)		    suite_accesstype,			(long) (x)
#define suite_AccessMode(x)		    suite_accesstype,			(long) (x)
#define suite_ItemWidth(x)		    suite_itemwidth,			(long) (x)
#define suite_ItemHeight(x)		    suite_itemheight,			(long) (x)
#define suite_ItemViewObjectName(x)	    suite_itemviewobjectname,		(long) (x)
#define suite_ItemViewObjectHandler(x)	    suite_itemviewobjecthandler,	(long) (x)
#define suite_ItemDataObjectName(x)	    suite_itemdataobjectname,		(long) (x)
#define suite_ItemDataObjectHandler(x)	    suite_itemdataobjecthandler,	(long) (x)
#define suite_ItemHitHandler(x)		    suite_itemhithandler,		(long) (x)
#define suite_Data(x)			    suite_datum,				(long) (x)
#define suite_Datum(x)			    suite_datum,				(long) (x)
#define suite_ItemDatum(x)		    suite_itemdatum,			(long) (x)
#define suite_ItemData(x)		    suite_itemdatum,			(long) (x)
#define suite_HorizontalGuttersize(x)	    suite_horizontalguttersize,		(long) (x)
#define suite_VerticalGuttersize(x)	    suite_verticalguttersize,		(long) (x)
#define	suite_Guttersize(x)		    suite_guttersize,			(long) (x)
#define suite_HorizontalGutterSize(x)	    suite_horizontalguttersize,		(long) (x)
#define suite_VerticalGutterSize(x)	    suite_verticalguttersize,		(long) (x)
#define	suite_GutterSize(x)		    suite_guttersize,			(long) (x)
#define suite_SortMode(x)		    suite_sortmode,			(long) (x)
#define suite_SortHandler(x)		    suite_sorthandler,			(long) (x)
#define suite_CursorFontName(x)		    suite_cursorfontname,		(long) (x)
#define	suite_CursorType(x)		    suite_cursorbyte,			(long) (x)
#define suite_Cursor(x)			    suite_cursorbyte,			(long) (x)
#define suite_ItemCursorFontName(x)	    suite_itemcursorfontname,		(long) (x)
#define suite_ItemCursor(x)		    suite_itemcursorbyte,		(long) (x)
#define suite_ItemCursorType(x)		    suite_itemcursorbyte,		(long) (x)
#define	suite_WrappingStyle(x)		    suite_wrappingstyle,		(long) (x)
#define	suite_ItemName(x)		    suite_itemname,			(long) (x)
#define	suite_Rows(x)			    suite_rows,				(long) (x)
#define	suite_Columns(x)		    suite_columns,			(long) (x)
#define	suite_ForegroundColor(x)	    suite_foregroundcolor,		(long) (x)
#define	suite_BackgroundColor(x)	    suite_backgroundcolor,		(long) (x)
#define	suite_ItemCaptionColor(x)	    suite_itemcaptioncolor,		(long) (x)
#define	suite_ItemForegroundColor(x)	    suite_itemforegroundcolor,		(long) (x)
#define	suite_ItemBackgroundColor(x)	    suite_itembackgroundcolor,		(long) (x)
#define	suite_ActiveItemCaptionColor(x)	    suite_activeitemcaptioncolor,	(long) (x)
#define	suite_ActiveItemForegroundColor(x)  suite_activeitemforegroundcolor,	(long) (x)
#define	suite_ActiveItemBackgroundColor(x)  suite_activeitembackgroundcolor,	(long) (x)
#define	suite_PassiveItemCaptionColor(x)    suite_passiveitemcaptioncolor,	(long) (x)
#define	suite_PassiveItemForegroundColor(x) suite_passiveitemforegroundcolor,	(long) (x)
#define	suite_PassiveItemBackgroundColor(x) suite_passiveitembackgroundcolor,	(long) (x)

/***  Suite Attribute Codes  ***/

#define	suite_titlecaption			    1
#define	suite_titleborderstyle			    2
#define	suite_titlehighlightstyle		    3
#define	suite_titlebordersize			    4
#define	suite_titledataobjectname		    5
#define	suite_titledataobjecthandler		    6
#define	suite_selectionmode			    7
#define	suite_titleplacement			    8
#define	suite_borderstyle			    9
#define	suite_hithandler			    10
#define	suite_arrangement			    11
#define	suite_scroll				    12
#define	suite_titlefontname			    13
#define	suite_titleviewobjectname		    14
#define	suite_titleviewobjecthandler		    15
#define	suite_titlehithandler			    18
#define	suite_itemcaptionfontname		    19

#define	suite_itemborderstyle			    23
#define	suite_itembordersize			    24
#define	suite_itemhighlightstyle		    25
#define	suite_itempassivestyle			    26
#define	suite_itemorder				    27
#define	suite_itemcaptionlist			    28
#define	suite_itemspec				    29
#define	suite_bordersize			    30
#define	suite_datum				    31
#define	suite_itemcaption			    32
#define	suite_itemtitlecaption			    33
#define	suite_itemtitlefontname			    34
#define	suite_itemtitleplacement		    35
#define	suite_accesstype			    36
#define	suite_itemdataobjectname		    37
#define	suite_itemdataobjecthandler		    38
#define	suite_itemviewobjectname		    39
#define	suite_itemviewobjecthandler		    40
#define	suite_itemhithandler			    41
#define	suite_itemwidth				    42
#define	suite_itemheight			    43
#define	suite_horizontalguttersize		    46
#define	suite_verticalguttersize		    47
#define	suite_guttersize			    48
#define	suite_sortmode				    49
#define	suite_sorthandler			    50
#define	suite_cursorfontname			    52
#define	suite_cursorbyte			    53
#define	suite_itemcursorbyte			    55
#define	suite_itemcursorfontname		    56
#define	suite_itemdatum				    57
#define	suite_wrappingstyle			    58
#define	suite_itemposition			    59
#define	suite_fontname				    60
#define	suite_itemname				    61
#define	suite_itemcaptionalignment		    62
#define	suite_itemtitlecaptionalignment		    63
#define	suite_titlecaptionalignment		    64
#define	suite_titleviewobject			    65
#define	suite_titledataobject			    66
#define	suite_rows				    67
#define	suite_columns				    68

#define	suite_foregroundcolor			    69
#define	suite_backgroundcolor			    70
#define	suite_itemcaptioncolor			    71
#define	suite_itemforegroundcolor		    72
#define	suite_itembackgroundcolor		    73
#define	suite_activeitemcaptioncolor		    74
#define	suite_activeitemforegroundcolor		    75
#define	suite_activeitembackgroundcolor		    76
#define	suite_passiveitemcaptioncolor		    77
#define	suite_passiveitemforegroundcolor	    78
#define	suite_passiveitembackgroundcolor 	    79

