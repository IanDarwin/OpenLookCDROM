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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/chart/RCS/chart.c,v 1.21 1994/05/09 21:31:49 rr2b Exp $";
#endif

#ifndef lint
static char *rcsidchart = "$Header $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Chart Data-object

MODULE	chart.c

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
  02/23/88	Created (TCP)
  05/04/89	Add Line chart (TCP)
  05/23/89	Use apt_LeftArea, etc (TCP)
  05/31/89	Add classID parameter in FinalizeObject (TCP)
		Fix DestroyItem (free dumping)
  06/02/89	Set Item Position by default (TCP)
  09/01/89	Upgrade to V1.0

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#if defined(POSIX_ENV) && !defined(bsdi)
#include <values.h>
#endif
#include <rect.h>
#include <dataobj.ih>
#include <apt.h>
#include <apt.ih>
#include <apts.ih>
#include <chart.eh>

#define  ChartTitle		     (self->chart_title)
#define  ChartTitleDataObjectName    (self->chart_title_data_object_name)
#define  ChartTitleViewObjectName    (self->chart_title_view_object_name)
#define  ChartFileName		     (self->chart_file_name)
#define  ChartType		     (self->chart_moniker)
#define  ChartMonikers		     (self->chart_monikers)

#define  ClientAnchor		     (self->client_anchor)
#define  ClientDatum		     (self->client_datum)
#define  ExceptionCode		     (self->exception_code)
#define  ExceptionItem		     (self->exception_item)

#define  ItemAnchor		     (self->item_anchor)
#define  ItemCount		     (self->item_count)
#define  ItemValueRangeLow	     (self->item_value_range_low)
#define  ItemValueRangeInterval	     (self->item_value_range_interval)
#define  ItemValueRangeHigh	     (self->item_value_range_high)
#define  ItemValueGreatest	     (self->item_value_greatest)
#define  ItemValueLeast		     (self->item_value_least)
#define  ItemValueSpan		     (self->item_value_span)
#define	 ItemFontName		     (self->item_font_name)

#define  ItemName(x)		     ((x)->name)
#define  ItemDatum(x)		     ((x)->datum)
#define  ItemValue(x)		     ((x)->value)
#define  ItemPosition(x)	     ((x)->position)
#define  NextItem(x)		     ((x)->next)


int chart_debug = 0;

struct chart *
chart__Create( ClassID, specification, anchor )
  register struct  classheader	     *ClassID;
  chart_Specification		     *specification;
  register struct dataobject	     *anchor;
  {
  register struct chart	     *self;

  IN(chart_Create);
  self = chart_New();
  ClientAnchor = anchor;
  while ( specification  &&  specification->attribute )
    {
    SetChartAttribute( self, specification->attribute, specification->value );
    specification++;
    }

  OUT(chart_Create);
  return self;
  }

boolean
chart__InitializeObject( classID, self )
  register struct classheader	     *classID;
  register struct chart		     *self;
  {

#ifndef MAXINT
#define MAXINT 400000000
#endif

  IN(chart_InitializeObject);
  DEBUGst(RCSID,rcsidchart);
  chart_SetAreaTitleFontName( self, "Andy12b", apt_TopArea );
  chart_SetAreaLegendFontName( self, "Andy8", apt_BottomArea );
  ChartTitle = ChartFileName = NULL;
  ChartTitleDataObjectName = ChartTitleViewObjectName = NULL;
  ChartType = NULL;
  apts_CaptureString( "Histogram", &ChartType );
  ChartMonikers = NULL;
  ClientAnchor = NULL;
  ClientDatum = NULL;
  ItemAnchor = NULL;
  ItemCount = 0;
  ItemValueRangeLow = MAXINT;
  ItemValueRangeHigh = -MAXINT;
  ItemValueSpan = 0;
  ItemValueRangeInterval = 1;
  ItemValueGreatest = ItemValueLeast = 0;
  ItemFontName= NULL;
  OUT(chart_InitializeObject);
  return TRUE;
  }

void
chart__FinalizeObject( classID, self )
  register struct classheader	     *classID;
  register struct chart    	      *self;
  {
  register struct chart_item	      *item = ItemAnchor, *next;

  IN(chart_FinalizeObject);
  if ( ChartTitle )		    free( ChartTitle );
  if ( ChartFileName )		    free( ChartFileName );
  if ( ChartTitleDataObjectName )   free( ChartTitleDataObjectName );
  if ( ChartTitleViewObjectName )   free( ChartTitleViewObjectName );
  while ( item )
    {
    next = item->next;
    chart_DestroyItem( self, item );
    item = next;
    }
  OUT(chart_FinalizeObject);
  }

char *
chart__ViewName( self )
  register struct chart    	      *self;
  {
  IN(chart_ViewName);
  OUT(chart_ViewName);
  return "chartv";
  }

long
chart__SetChartAttribute( self, attribute, value )
  {  return  SetChartAttribute( self, attribute, value );  }

static
SetChartAttribute( self, attribute, value )
  register struct chart		   *self;
  register long			    attribute, value;
  {
  register long			    status = ok;

  IN(SetChartAttribute);
  switch ( attribute )
    {
    case  chart_datum:
      ClientDatum = value;				break;
    case  chart_filename:
      apts_CaptureString( value, &ChartFileName );	break;
    case  chart_titlecaption:
      apts_CaptureString( value, &ChartTitle );		break;
    case  chart_titledataobjectname:
      apts_CaptureString( value, &ChartTitleDataObjectName );break;
    case  chart_titleviewobjectname:
      apts_CaptureString( value, &ChartTitleViewObjectName );break;
    case  chart_type:
      apts_CaptureString( value, &ChartType );		break;
    default:
      fprintf( stderr, "Chart: Unrecognized ChartAttribute (%d) -- Ignored\n", attribute );
    }
  OUT(SetChartAttribute);
  return  status;
  }

long
chart__ChartAttribute( self, attribute )
  register struct chart	     *self;
  register long		      attribute;
  {
  register long		      value = NULL;

  IN(chart_ChartAttribute);
  switch ( attribute )
    {
    case  chart_datum:
      value = (long) ClientDatum;			break;
    case  chart_filename:
      value = (long) ChartFileName;			break;
    case  chart_titlecaption:
      value = (long) ChartTitle;			break;
    case  chart_titledataobjectname:
      value = (long) ChartTitleDataObjectName;		break;
    case  chart_titleviewobjectname:
      value = (long) ChartTitleViewObjectName;		break;
    case  chart_type:
      value = (long) ChartType;				break;
    default:
      ExceptionCode = chart_UnknownChartAttribute;
      fprintf( stderr, "Chart: Unrecognized ChartAttribute (%d) -- Ignored\n", attribute );
    }
  OUT(chart_ChartAttribute);
  return  value;
  }

long
chart__SetItemAttribute( self, item, attribute, value )
  {  return  SetItemAttribute( self, item, attribute, value );  }

static
SetItemAttribute( self, item, attribute, value )
  register struct chart      *self;
  register struct chart_item *item;
  register long		      attribute, value;
  {
  register long		      status = ExceptionCode = ok;

  IN(SetItemAttribute);
  DEBUGdt(Attribute,attribute);
  DEBUGdt(Value,value);
  if ( item )
    switch ( attribute )
      {
      case  chart_itemdatum:
	ItemDatum(item) = value;				    break;
      case  chart_itemname:
	apts_CaptureString( value, &ItemName(item) );		    break;
      case  chart_itemposition:
/*===*/  break;
      case  chart_itemvalue:
	SetItemValue( self, item, value );			    break;
/*===*/
      default:
        status = ExceptionCode = chart_UnknownItemAttribute;
	fprintf( stderr, "Chart: Unknown Item Attribute (%d) -- Ignored\n", attribute );
      }
    else  status = ExceptionCode = chart_NonExistentItem;
  OUT(SetItemAttribute);
  return  status;
  }

long
chart__ItemAttribute( self, item, attribute )
  register struct chart      *self;
  register struct chart_item *item;
  register long		      attribute;
  {
  register long		      value = NULL;

  IN(chart_ItemAttribute);
  if ( item )
    switch ( attribute )
      {
      case  chart_itemdatum:
	value = (long) ItemDatum(item);		    break;
      case  chart_itemname:
	value = (long) ItemName(item);		    break;
      case  chart_itemposition:
	value = (long) ItemPosition(item);	    break;
      case  chart_itemvalue:
	value = (long) ItemValue(item);		    break;
/*===*/
      default:
        ExceptionCode = chart_UnknownItemAttribute;
	fprintf( stderr, "Chart: Unknown Item Attribute (%d) -- Ignored\n", attribute );
      }
  OUT(chart_ItemAttribute);
  return  value;
  }

void
chart__SetDebug( self, state )
  register struct chart	      *self;
  register char		       state;
  {
  IN(chart_SetDebug);
  chart_debug = state;
/*===  super_SetDebug( self, debug );*/
  OUT(chart_SetDebug);
  }

static char *
Extract_Field_Value( self, fields, name )
  register struct chart		     *self;
  register char			    **fields;
  register char			     *name;
  {
  register char			     *field = NULL, *s, *t;
  register long			      length;
  char				      mask[257];

  IN(Extract_Field_Value);
  DEBUGst(Name,name);
  sprintf( mask, "%s(", name );
  length = strlen( mask );
  while ( *fields )
    {
    DEBUGst(Fields,*fields);
    if ( strncmp( *fields, mask, length ) == 0 )
      {
      DEBUG(Matched);
      s = *fields + length;
      t = field = (char *) malloc( 257 );
      while ( *s  &&  *s != ')' )
	*t++ = *s++;
      *t = 0;
      break;
      }
      else fields++;
    }
  DEBUGst(Extracted,field);
  OUT(Extract_Field_Value);
  return  field;
  }

static
Reader( self )
  register struct chart	    	     *self;
  {
  register struct apt_field	     *field;

  IN(Reader);
  while ( field = chart_ReadObjectField( self ) )
    {
    DEBUGst(Field-Name,field->name);
    DEBUGst(Field-Content,field->content);
    if ( strcmp( "Item", field->name ) == 0 )
	Parse_Item_Field( self, field->content );
    else
    if ( strcmp( "ChartType", field->name ) == 0 )
	Parse_Type_Field( self, field->content );
    else
    if ( strcmp( "ChartTitle", field->name ) == 0 )
	Parse_Name_Field( self, field->content );
    }
  OUT(Reader);
  }

long
chart__Read( self, file, id )
  register struct chart	    	     *self;
  register FILE			     *file;
  register long			      id;
  {
  register long			      status; 

  IN(chart_Read);
  ItemCount = 0;
  if ( (status = chart_ReadObject( self, file, id, Reader )) ==
	dataobject_NOREADERROR )
    {
    chart_NotifyObservers( self, 1234 );
    }
  OUT(chart__Read);
  return status;
  }

static
Parse_Name_Field( self, string )
  register struct chart		     *self;
  register char			     *string;
  {
  IN(Parse_Name_Field);
  DEBUGst(Name,string);
  chart_SetChartAttribute( self, chart_TitleCaption(string) );
  OUT(Parse_Name_Field);
  }

static
Parse_Type_Field( self, string )
  register struct chart		     *self;
  register char			     *string;
  {
  IN(Parse_Type_Field);
  DEBUGst(Type,string);
  ChartType = string;
  OUT(Parse_Type_Field);
  }

static
Parse_Item_Field( self, string )
  register struct chart		     *self;
  register char			     *string;
  {
  register char			    **fields,
				     *extract;
  register struct chart_item	     *item;
  long				      value;

  IN(Parse_Item_Field);
  DEBUGst(Item,string);
  if ( fields = chart_ParseFieldContent( self, string ) )
    {
    extract = Extract_Field_Value( self, fields, "Name" );
    if ( item = chart_CreateItem( self, extract, NULL ) )
      {
      if ( extract )  free( extract );
      if ( extract = Extract_Field_Value( self, fields, "Value" ) )
	{
	sscanf( extract, "%d", &value );
	SetItemValue( self, item, value );
        free( extract );
	}
      if ( extract = Extract_Field_Value( self, fields, "Position" ) )
	{
	sscanf( extract, "%d", &value );
	ItemPosition(item) = value;
	free( extract );
	}
      }
    }
  OUT(Parse_Item_Field);
  }

static char *
ValueString( self, item )
  register struct chart		     *self;
  register struct chart_item	     *item;
  {
  static char			      value[257];
  register char			     *ptr = value;

  *value = 0;
  sprintf( value, "%d", ItemValue(item) );
  return  ptr;
  }

static
Writer( self )
  register struct chart		     *self;
  {
  register long			      i;
  register struct chart_item	     *item = ItemAnchor;
  struct apt_field		      field;
  char				      content[100], contents[1000];

  IN(Writer);
  field.name = "ChartType";
  DEBUGst(Chart-type,ChartType);
  field.content = ChartType;
  DEBUGst(Type,field.content);
  chart_WriteObjectField( self, &field );
  field.name = "Item";
  field.content = contents;
  i = ItemCount;
  while ( i-- )
    {
    *contents = 0;
    if ( ItemName(item) )
      {sprintf( content, "Name(%s);",	    ItemName(item) );
       strcat( contents, content );}
    if ( ValueString(self,item) )
      {sprintf( content, "Value(%s);",	    ValueString(self,item) );
       strcat( contents, content );}
    if ( ItemPosition(item) )
      {sprintf( content, "Position(%d);",   ItemPosition(item) );
       strcat( contents, content );}
    chart_WriteObjectField( self, &field );
    item = NextItem(item);
    }
  OUT(Writer);
  }

long
chart__Write( self, file, writeID, level )
  register struct chart		     *self;
  register FILE			     *file;
  register long			      writeID;
  register long			      level;
  {
  IN(chart_Write);
  chart_WriteObject( self, file, writeID, level, Writer );
  OUT(chart_Write);
  return  self->header.dataobject.id;
  }

struct chart_monikers *
chart__Monikers( self )
  register struct chart		     *self;
  {
static struct chart_monikers	monikers[] = /*===MUST BE DYNAMIC*/
{
{"Histogram","charthst"},
{"Pie","chartpie"},
{"Dot","chartdot"},
{"Line","chartlin"},
{"Cartesian","chartcsn"},
{"Map","chartmap"},
{"Stack","chartstk"},
{0,0}
};

  IN(chart_Monikers);
/*===*/
  OUT(chart_Monikers);
return  monikers;
  }

char *
chart__ModuleName( self, moniker )
  register struct chart		     *self;
  register char			     *moniker;
  {
  register char			     *module_name = NULL;
  register struct chart_monikers     *monikers;

  IN(chart_ModuleName);
  DEBUGst(Moniker,moniker);
  if ( monikers = chart_Monikers( self ) )
    {
    while ( monikers->chart_moniker )
      {
      DEBUGst(Candidate-moniker,monikers->chart_moniker);
      if ( strcmp( moniker, monikers->chart_moniker ) == 0 )
	{
	module_name = monikers->chart_module_name;
	break;
	}
      monikers++;
      }
    }
    else
    {
/*===*/
    }
  DEBUGst(Module-name,module_name);
  OUT(chart_ModuleName);
  return  module_name;
  }

struct chart_item *
chart__CreateItem( self, name, datum )
  register struct chart		     *self;
  register char			     *name;
  register long			      datum;
  {
  register struct chart_item	     *item, *next = ItemAnchor;

  IN(chart_CreateItem);
  if ( item = (struct chart_item *) calloc( 1, sizeof(struct chart_item) ) )
    {
    ItemPosition(item) = ++ItemCount;
    apts_CaptureString( name, &ItemName(item) );
    item->datum = datum;
    if ( ItemAnchor )
      {
      while ( next->next )
        next = next->next;
      next->next = item;
      }
      else  ItemAnchor = item;
    }
  OUT(chart_CreateItem);
  return  item;
  }

struct chart_item *
chart__ItemOfName( self, name )
  register struct chart		     *self;
  register char			     *name;
  {
  register struct chart_item	     *item = NULL, *next = ItemAnchor;

  IN(chart_ItemOfName);
  while ( next )
    if ( ItemName(next)  &&  *ItemName(next)  &&
	 strcmp( ItemName(next), name ) == 0 )
      {
      item = next;
      break;
      }
      else  next = next->next;
  OUT(chart_ItemOfName);
  return  item;
  }

void
chart__DestroyItem( self, item )
  register struct chart		     *self;
  register struct chart_item	     *item;
  {
  register struct chart_item	     *prior;

  IN(chart_DestroyItem);
  if ( item )
    {
    ItemCount--;
    if ( ItemName(item) )   free( ItemName(item) );
    if ( item == ItemAnchor )
      ItemAnchor = item->next;
      else
      {
      prior = ItemAnchor;
      while ( prior )
	{
	if ( prior->next == item )
	  {
	  prior->next = item->next;
	  break;
	  }
	prior = NextItem(prior);
	}
      }
    free( item );
    }
  OUT(chart_DestroyItem);
  }

static
SetItemValue( self, item, value )
  register struct chart		     *self;
  register struct chart_item	     *item;
  register long			      value;
  {
  IN(SetItemValue);
  ItemValue(item) = value;
  if ( ItemValue(item) < ItemValueLeast )
    {
    ItemValueLeast = ItemValue(item);
    ItemValueSpan = ItemValueGreatest - ItemValueLeast;
    if ( ItemValueLeast < ItemValueRangeLow )
      ItemValueRangeLow = ItemValueLeast;
    }
    else
    if ( ItemValue(item) > ItemValueGreatest )
      {
      ItemValueGreatest = ItemValue(item);
      ItemValueSpan = ItemValueGreatest - ItemValueLeast;
      if ( ItemValueGreatest > ItemValueRangeHigh )
        ItemValueRangeHigh = ItemValueGreatest;
     }
  OUT(SetItemValue);
  }

void
chart__Reset( self, mode )
  register struct chart		     *self;
  register long			      mode;
  {
  IN(chart_Reset);
/*===*/
  OUT(chart_Reset);
  }

void
chart__Apply( self, proc, anchor, datum )
  register struct chart		     *self;
  register void			   *(*proc)();
  register long			      anchor;
  register char			     *datum;
  {
  register chart_type_item	      item = ItemAnchor;

  IN(chart_Apply);
  while ( item )
    {
    (proc)( anchor, self, item, datum );
    item = NextItem(item);
    }
  OUT(chart_Apply);
  }

static long
Sort_By_Ascending_Value( a, b )
  register struct chart_item	    **a, **b;
  {
  if ( a && b )
    {
    if ( ItemValue(*a) < ItemValue(*b) )  return -1;
    if ( ItemValue(*a) > ItemValue(*b) )  return  1;
    }
  return 0;
  }

static long
Sort_By_Descending_Value( a, b )
  register struct chart_item	    **a, **b;
  {
  if ( a && b )
    {
    if ( ItemValue(*a) < ItemValue(*b) )  return  1;
    if ( ItemValue(*a) > ItemValue(*b) )  return -1;
    }
  return 0;
  }

static long
Sort_By_Ascending_Label( a, b )
  register struct chart_item	    **a, **b;
  {
  if ( a && b )
    return  strcmp( ItemName(*a), ItemName(*b) );
  return 0;
  }

static long
Sort_By_Descending_Label( a, b )
  register struct chart_item	    **a, **b;
  {
  if ( a && b )
    return  strcmp( ItemName(*b), ItemName(*a) );
  return 0;
  }

static long
Sort_By_Ascending_Position( a, b )
  register struct chart_item	    **a, **b;
  {
  if ( a && b )
    {
    if( ItemPosition(*a) < ItemPosition(*b) )  return -1;
    if( ItemPosition(*a) > ItemPosition(*b) )  return  1;
    }
  return 0;
  }

static long
Sort_By_Descending_Position( a, b )
  register struct chart_item	    **a, **b;
  {
  if ( a && b )
    {
    if( ItemPosition(*a) < ItemPosition(*b) )  return  1;
    if( ItemPosition(*a) > ItemPosition(*b) )  return -1;
    }
  return 0;
  }

void
chart__Sort( self, mode, handler )
  register struct chart		     *self;
  register long			      mode;
  register long			    (*handler)();
  {
  register long			    (*sorter)() = NULL, i = 0;
  register chart_type_item	     *vector;
  register chart_type_item	      item = ItemAnchor;

  IN(chart_Sort);
  if ( vector = (chart_type_item *) malloc( ItemCount * sizeof(chart_type_item) ) )
    {
    while ( item )
      {
      vector[i++] = item;
      item = NextItem(item);
      }
    if ( mode & chart_ByValue )
      {
      if ( mode & chart_Descend )
        sorter = Sort_By_Descending_Value;
	else
        sorter = Sort_By_Ascending_Value;
      }
    else
    if ( mode & chart_ByLabel )
      {
      if ( mode & chart_Descend )
        sorter = Sort_By_Descending_Label;
	else
        sorter = Sort_By_Ascending_Label;
      }
    else
    if ( mode & chart_ByPosition )
      {
      if ( mode & chart_Descend )
        sorter = Sort_By_Descending_Position;
	else
        sorter = Sort_By_Ascending_Position;
      }
    if ( sorter )
      { DEBUGdt(Qsort,ItemCount);
      qsort( vector, ItemCount, sizeof(chart_type_item), sorter );
      DEBUG(Qsort Done);
      ItemAnchor = vector[0];
      for ( i = 1; i < ItemCount; i++ )
	{
	DEBUGst(ItemName,ItemName(vector[i-1]));
	NextItem(vector[i-1]) = vector[i];
	}
      NextItem(vector[i-1]) = NULL;
      free ( vector );
      }
    }
    else
    {
/*===*/
    }
  OUT(chart_Sort);
  }

