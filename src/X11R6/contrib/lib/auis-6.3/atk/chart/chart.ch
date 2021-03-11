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

TITLE	The Chart Data-object

MODULE	chart.ch

VERSION	0.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that suport the Chart Data-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  02/23/89	Created (TCP)
  05/23/89	Add EnclosureModified notification-code (TCP)
  09/01/89	Upgrade to V1.0

END-SPECIFICATION  ************************************************************/


#define  chart_VERSION    1

class chart : apt
  {

overrides:

  Read( FILE *file, long id )				returns long;
  Write( FILE *file, long id, long level )		returns long;
  ViewName()						returns char *;

methods:

  /**  Methods Dealing with Chart as a Whole  **/

  SetChartAttribute( long code_value )			returns long;
  ChartAttribute( long attribute_code )			returns long;

  Apply( (long(*)())proc, long anchor, long datum );
  Reset( long mode );
  Sort( long mode, long (*handler)() );
  Monikers()						returns struct chart_monikers *;
  ModuleName( char *moniker )				returns char *;
  SetDebug( boolean state );

  /**  Methods Dealing with Individual Items  **/

  SetItemAttribute( struct chart_item *item, long code_value )
							returns long;
  ItemAttribute( struct chart_item *item, long attribute_code )
							returns long;
  CreateItem( char *item_name, long datum )		returns struct chart_item *;
  DestroyItem( struct chart_item *item );
  ItemOfName( char *item_name )				returns struct chart_item *;

macromethods:

  ItemAnchor()			      (self->item_anchor)
  ItemCount()			      (self->item_count)

  ItemValueGreatest()		      (self->item_value_greatest)
  ItemValueLeast()		      (self->item_value_least)
  ItemValueRangeHigh()		      (self->item_value_range_high)
  ItemValueRangeInterval()	      (self->item_value_range_interval)
  ItemValueRangeLow()		      (self->item_value_range_low)
  ItemValueSpan()		      (self->item_value_span)
  ItemFontName()		      (self->item_font_name)

  NextItem( item )		      ((item)->next)

classprocedures:

  InitializeObject( struct chart *self )		returns boolean;
  FinalizeObject( struct chart *self );
  Create( chart_Specification, char *anchor )		returns struct chart *;

data:

  char				     *chart_moniker, *chart_title,
				     *chart_title_data_object_name, *chart_title_view_object_name;
  struct dataobject		     *client_anchor;
  long				      client_datum, item_count;
  struct chart_item		     *item_anchor;
  long				      id;
  char				     *chart_file_name, *item_font_name;
  long				      item_value_greatest, item_value_least,
				      item_value_range_low, item_value_range_high,
				      item_value_range_interval, item_value_span;
  struct chart_monikers		     *chart_monikers;
  long				      exception_code;
  struct chart_item		     *exception_item;
  };

/***  Exception Codes  ***/
#define  chart_NonExistentItem			1
#define  chart_UnknownChartAttribute		2
#define  chart_UnknownItemAttribute		3

/***  Notification Codes  ***/
#define  chart_ItemCreated			1
#define  chart_ItemDestroyed			2
#define  chart_ItemValueChanged			3
#define  chart_ItemNameChanged			4
#define  chart_ItemCaptionChanged		5
#define  chart_ItemsSorted			6
#define  chart_EnclosureModified		7


/***  Chart Attribute Values  ***/

/***  Chart Sort Fields  ***/
#define  chart_ByValue			    (1<<0)
#define  chart_ByLabel			    (1<<1)
#define  chart_ByPosition		    (1<<2)

/***  Chart Sort Order	***/
#define  chart_Ascend			    (1<<8)
#define  chart_Descend			    (1<<9)

/***  Attribute Macros  ***/

#define  chart_Datum(x)			    chart_datum,	    (long) x
#define  chart_FileName(x)		    chart_filename,	    (long) x
#define  chart_ItemDatum(x)		    chart_itemdatum,	    (long) x
#define  chart_ItemName(x)		    chart_itemname,	    (long) x
#define  chart_ItemPosition(x)		    chart_itemposition,	    (long) x
#define  chart_ItemValue(x)		    chart_itemvalue,	    (long) x
#define  chart_TitleCaption(x)		    chart_titlecaption,	    (long) x
#define  chart_TitleDataObjectName(x)	    chart_titledataobjectname,	(long) x
#define  chart_TitleViewObjectName(x)	    chart_titleviewobjectname,	(long) x
#define  chart_Type(x)			    chart_type,		    (long) x

/***  Attribute Codes  ***/

#define  chart_datum	    		    5
#define  chart_filename			    6

#define  chart_itemdatum		    12
#define  chart_itemname			    14
#define  chart_itemposition		    15
#define  chart_itemvalue		    16

#define  chart_titlecaption		    33
#define  chart_titledataobjectname	    35
#define  chart_titleviewobjectname	    38

#define  chart_type			    40


typedef struct chart_specification  chart_Specification;
struct  chart_specification
  {
  char	attribute;
  long	value;
  };

typedef struct chart_item	     *chart_type_item;
struct  chart_item
  {
  struct chart_item		     *next;
  char				     *name;
  long				      datum;
  long				      position;
  long				      value;
  char				      shade; /* 0 => None, 1 => White ... 255 = Black */
  };

typedef struct chart_monikers	     *chart_type_monikers;
struct  chart_monikers
  {
  char				     *chart_moniker;
  char				     *chart_module_name;
  };
