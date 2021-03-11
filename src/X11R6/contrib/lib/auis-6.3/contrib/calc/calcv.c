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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/calc/RCS/calcv.c,v 1.4 1992/12/15 21:48:53 rr2b R6tape $";
#endif



/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Calc View-object

MODULE	calcv.c

VERSION	1.0

DESCRIPTION
	This is the suite of Methods that support the Calc View-object,
	a trivial example of an ATK Inset.

HISTORY
  02/23/88	Created (TCP)
  08/03/89	Suppress Top-row Icons (TCP)
		Accept Keyboard input.
  08/07/89	Print Outline (TCP)

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include <graphic.ih>
#include <observe.ih>
#include <view.ih>
#include <im.ih>
#include <rect.h>
#include <proctbl.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <apt.h>
#include <aptv.ih>
#include <calc.ih>
#include <calcv.eh>
#include <ctype.h>

#define  circle			      1
#define  box			      2
#define  roundbox		      3

#define  Balanced		     (view_BETWEENLEFTANDRIGHT | view_BETWEENTOPANDBASELINE)
#define  RightMiddle		     (view_ATRIGHT | view_BETWEENTOPANDBASELINE)

#define  Data			    ((struct calc *)self->header.aptv.data_object)
#define  Operand1	    	     (self->operand_1)
#define  Operand2   		     (self->operand_2)
#define  Expression		     (self->expression)
#define  PriorExpression	     (self->prior_expression)
#define  PendingOp		     (self->pending_op)
#define  PendingDisplay		     (self->states.pending_display)
#define  PendingOutline		     (self->states.pending_outline)
#define  PointPresent		     (self->states.point_present)
#define  InputFocus		     (self->states.inputfocus)
#define  Keystate		     (self->keystate)

#define  Bounds			     (&self->bounds)
#define  Left			     (self->bounds.left)
#define  Top			     (self->bounds.top)
#define  Width			     (self->bounds.width)
#define  Height			     (self->bounds.height)

#define  Areas			     (self->areas)
#define  AreaCount		     (self->area_count)
#define	 Area(i)		     (Areas[i])

#define	 AreaBound(i)		     (&Area(i).bounds)
#define	 AreaBounds(i)		     (Area(i).bounds)
#define	 AreaLeft(i)		     (AreaBounds(i).left)
#define	 AreaRight(i)		     (AreaLeft(i) + AreaWidth(i) - 1)
#define	 AreaCenter(i)		     (AreaLeft(i) + (AreaWidth(i) / 2))
#define	 AreaTop(i)		     (AreaBounds(i).top)
#define	 AreaBottom(i)		     (AreaTop(i)  + AreaHeight(i) - 1)
#define	 AreaMiddle(i)		     (AreaTop(i)  + (AreaHeight(i) / 2))
#define	 AreaWidth(i)		     (AreaBounds(i).width)
#define	 AreaHeight(i)		     (AreaBounds(i).height)

#define	 AreaString(i)		      Area(i).string
#define	 AreaFont(i)		      Area(i).font
#define	 AreaAlign(i)		      Area(i).mode
#define	 AreaShape(i)		      Area(i).shape
#define	 AreaHitHandler(i)	      Area(i).hit_handler
#define	 AreaSpec(i)	    	      Area(i).spec
#define	 AreaSpecX(i)    	      Area(i).spec->x_center
#define	 AreaSpecY(i)    	      Area(i).spec->y_center
#define	 AreaSpecW(i)    	      Area(i).spec->width
#define	 AreaSpecH(i)    	      Area(i).spec->height
#define	 AreaHighlighted(i)    	      Area(i).states.highlighted

void				      Digit(), Operator(), Clear(),
				      Display();
static char			      digit_font[] = "andysans10b",
				      oper_font[]  = "andysans16b",
				      expr_font[]  = "andysans12b";

#define  DW	/* Digit Width  */    (20)
#define  DH	/* Digit Height */    (DW)
#define  OW	/* Oper. Width  */    (25)
#define  OH	/* Oper. Height */    (OW/2)

#define  DR1	/* Digit ROW 1 */     (30)
#define  DR2	/* Digit ROW 1 */     (DR1 + DH)
#define  DR3	/* Digit ROW 1 */     (DR2 + DH)
#define  DR4	/* Digit ROW 1 */     (DR3 + DH)
#define  DC1	/* Digit Col 1 */     (14)
#define  DC2	/* Digit Col 2 */     (DC1 + DW )
#define  DC3	/* Digit Col 3 */     (DC2 + DW )
#define  OC1	/* Oper. Col 1 */     (85)
#define  OR1	/* Oper. Row 1 */     (30)
#define  OR2	/* Oper. Row 2 */     (OR1 + OH)
#define  OR3	/* Oper. Row 3 */     (OR2 + OH)
#define  OR4	/* Oper. Row 4 */     (OR3 + OH)
#define  OR5	/* Oper. Row 5 */     (OR4 + OH)
#define  OR6	/* Oper. Row 6 */     (OR5 + OH)

struct calcv_setup
  {
  char				     *string;
  char				     *font_name;
  int				      mode;
  char				      shape;
  void				    (*hit_handler)();
  long				      x_center, y_center, width, height;
  };

static struct calcv_setup setups[] =
   {
 { "7",	  digit_font, Balanced, box,   Digit,	    DC1,DR1, DW,DH },/* Area  0 */
 { "8",   digit_font, Balanced, box,   Digit,	    DC2,DR1, DW,DH },/* Area  1 */
 { "9",   digit_font, Balanced, box,   Digit,	    DC3,DR1, DW,DH },/* Area  2 */
 { "4",   digit_font, Balanced, box,   Digit,	    DC1,DR2, DW,DH },/* Area  3 */
 { "5",   digit_font, Balanced, box,   Digit,	    DC2,DR2, DW,DH },/* Area  4 */
 { "6",   digit_font, Balanced, box,   Digit,	    DC3,DR2, DW,DH },/* Area  5 */
 { "1",   digit_font, Balanced, box,   Digit,	    DC1,DR3, DW,DH },/* Area  6 */
 { "2",   digit_font, Balanced, box,   Digit,	    DC2,DR3, DW,DH },/* Area  7 */
 { "3",   digit_font, Balanced, box,   Digit,	    DC3,DR3, DW,DH },/* Area  8 */
 { "0",   digit_font, Balanced, box,   Digit,	    DC1,DR4, DW,DH },/* Area  9 */
 { ".",   digit_font, Balanced, box,   Digit,	    DC2,DR4, DW,DH },/* Area 10 */
 { "=",   oper_font,  Balanced, box,   Operator,    OC1,OR1, OW,OH },/* Area 11 */
 { "+",   oper_font,  Balanced, box,   Operator,    OC1,OR2, OW,OH },/* Area 12 */
 { "-",   oper_font,  Balanced, box,   Operator,    OC1,OR3, OW,OH },/* Area 13 */
 { "X",   oper_font,  Balanced, box,   Operator,    OC1,OR4, OW,OH },/* Area 14 */
 { "/",   oper_font,  Balanced, box,   Operator,    OC1,OR5, OW,OH },/* Area 15 */
 { "AC",  oper_font,  Balanced, box,   Clear,	    OC1,OR6, OW,OH },/* Area 16 */
 { "0",   expr_font, RightMiddle, roundbox, Display, 50,10, 98,18 },/* Area 17 */
 { NULL }
   };
#define  AssignArea		      11
#define  MultiplyArea		      14
#define  ClearArea		      16
#define  DisplayArea		      17

static struct keymap		     *class_keymap;
static void			      Stroke();

boolean 
calcv__InitializeClass( classID )
  register struct classheader	     *classID;
  {
  IN(calcv_InitializeClass);
  class_keymap = keymap_New();
  OUT(calcv_InitializeClass);
  return TRUE;
  }

boolean 
calcv__InitializeObject( classID, self)
  register struct classheader *classID;
  register struct calcv	      *self;
  {
  register long		       i;
  register struct proctable_Entry *key_proc;

  IN(calcv_InitializeObject);
  calcv_SetOptions( self, aptv_SuppressControl |
			  aptv_SuppressBorder |
			  aptv_SuppressEnclosures );
  calcv_SetDimensions( self, 150, 175 );
  bzero( &self->states, sizeof(struct calcv_states) );
  Keystate = keystate_Create( self, class_keymap );
  key_proc = proctable_DefineProc( "stroke", Stroke,
		&calcv_classinfo, NULL, "Type Digit or Operator" );
  AreaCount = 0;
  for ( i = 0; setups[i].string; i++ )
    {
    AreaCount++;
    bzero( &Area(i), sizeof(struct calcv_area) );
    AreaString(i)	= setups[i].string;
    AreaAlign(i)	= setups[i].mode;
    AreaShape(i)	= setups[i].shape;
    AreaHitHandler(i)	= setups[i].hit_handler;
    AreaSpec(i)		= &setups[i];
    if(i < DisplayArea)
	keymap_BindToKey( class_keymap, AreaString(i), key_proc, i );
    }
  keymap_BindToKey( class_keymap, "x",	  key_proc, MultiplyArea );
  keymap_BindToKey( class_keymap, "*",	  key_proc, MultiplyArea );
  keymap_BindToKey( class_keymap, "\012", key_proc, AssignArea );
  keymap_BindToKey( class_keymap, "\015", key_proc, AssignArea );
  keymap_BindToKey( class_keymap, " ",    key_proc, ClearArea );
  keymap_BindToKey( class_keymap, "ac",   key_proc, ClearArea );
  strcpy( Expression, AreaString(DisplayArea) );
  strcpy( PriorExpression, Expression );
  AreaString(DisplayArea) = Expression;
  *Operand1 = *Operand2 = PendingOp = 0;
  OUT(calcv_InitializeObject);
  return  TRUE;
  }

void 
calcv__FinalizeObject( classID, self )
  register struct classheader *classID;
  register struct calcv	      *self;
  {
  IN(calcv_FinalizeObject);
  if ( Keystate )	keystate_Destroy( Keystate );
  OUT(calcv_FinalizeObject);
  }

void
calcv__SetDataObject( self, data )
  register struct calcv	      *self;
  register struct calc	      *data;
  {
  IN(calcv_SetDataObject);
  super_SetDataObject( self, data );
  DEBUGgt(Value,calc_Value( Data ));
  sprintf( Operand1, "%f", calc_Value( Data ) );
  Shrink( Operand1 );
  strcpy( Expression, Operand1 );
  strcpy( PriorExpression, Expression );
  DEBUGst(Operand1,Operand1);
  OUT(calcv_SetDataObject);
  }

void 
calcv__ReceiveInputFocus( self )
  register struct calcv	      *self;
  {
  IN(calcv_ReceiveInputFocus);
  InputFocus = true;
  Keystate->next = NULL;
  calcv_PostKeyState( self, Keystate );
  PendingOutline = true;
  calcv_WantUpdate( self, self );
  OUT(calcv_ReceiveInputFocus);
  }

void
calcv__LoseInputFocus( self )
  register struct calcv	      *self;
  {
  IN(calcv_LoseInputFocus);
  InputFocus = false;
  PendingOutline = true;
  calcv_WantUpdate( self, self );
  OUT(calcv_LoseInputFocus);
  }

void 
calcv__FullUpdate( self, type, left, top, width, height )
  register struct calcv	     *self;
  register enum view_UpdateType	type;
  register long		      left, top, width, height;
  {
  register long		      i, L, T, W, H;

  IN(calcv_FullUpdate);
  if ( Data  &&  (type == view_FullRedraw || type == view_LastPartialRedraw) )
    {
      calcv_GetLogicalBounds( self, Bounds );
      super_FullUpdate( self, type, 0,0, Width, Height );
    if ( ! calcv_BypassUpdate(self) )
      {
      L = Left + 2;  T = Top + 2;
      for ( i = 0; i < AreaCount; i++ )
        {
        W = ((AreaSpecW(i) * (Width-4)) / 100);
        H = ((AreaSpecH(i) * (Height-4)) / 100);
        if ( AreaShape(i) == circle )
	  if ( W > H )  W = H;
	  else if ( H > W )  H = W;
        AreaWidth(i)  = W - 2;
        AreaHeight(i) = H - 2;
        AreaLeft(i) = L + (((AreaSpecX(i) * (Width-4))  / 100) - W/2);
        AreaTop(i)  = T + (((AreaSpecY(i) * (Height-4)) / 100) - H/2);
	AreaFont(i) = calcv_BuildFont( self, setups[i].font_name, NULL );
        }
      Draw_Calc( self );
      Draw_Outline( self );
      }
    }
  OUT(calcv_FullUpdate);
  }

void 
calcv__Update( self )
  register struct calcv	   *self;
  {
  IN(calcv_Update);
  if ( PendingDisplay )
    {
    PendingDisplay = false;
    Replace_String( self, PriorExpression, Expression, DisplayArea );
    }
  else
  if ( PendingOutline )
    {
    PendingOutline = false;
    Draw_Outline( self );
    }
  OUT(calcv_Update);
  }

static
Replace_String( self, old, new, area )
  register struct calcv	     *self;
  register char		     *old,*new;
  register long		      area;
  {

  calcv_ClearBoundedString( self, old, AreaFont(area), AreaBound(area),
			  AreaRight(area) - 5, AreaMiddle(area), AreaAlign(area) );
  calcv_DrawBoundedString( self, new, AreaFont(area), AreaBound(area),
			  AreaRight(area) - 5, AreaMiddle(area), AreaAlign(area) );
  strcpy( old, new );
  }

void
calcv__ObservedChanged( self, changed, value )
  register struct calcv	     *self;
  register struct observable *changed;
  register long		     value;
  {
  IN(calcv_ObservedChanged);
  switch ( value )
    {
    case calc_value_changed:
	sprintf( Expression, "%f", calc_Value( Data ) );
      Shrink( Expression );
      strcpy( Operand1, Expression );
      *Operand2 = 0;
      PendingDisplay = true;
      calcv_WantUpdate( self, self );
      break;
    }
  OUT(calcv_ObservedChanged);
  }

static long
Which_Area( self, x, y )
  register struct calcv	     *self;
  register long		      x, y;
  {
  register long		      i;

  for ( i = 0; i < AreaCount; i++ )
    if ( calcv_Within( self, x, y, AreaBound(i) ) )
      break;
  return  i;
  }

struct view *
calcv__Hit( self, action, x, y, clicks )
  register struct calcv	     *self;
  register enum view_MouseAction action;
  register long		      x, y, clicks;
  {
  register struct view	     *hit;
  register long		      which;

  IN(calcv_Hit );
  if ( ! InputFocus )
    calcv_WantInputFocus( self, self );
  if ( InputFocus  &&  (hit = super_Hit( self, action, x, y, clicks )) == NULL )
    if ( calcv_Within( self, x, y, Bounds ) )
      {
      hit = (struct view *) self;
      if ( (which = Which_Area( self, x, y )) < AreaCount )
        switch ( action )
          {
          case  view_LeftDown:
	    (AreaHitHandler(which))( self, which );
            break;
          case  view_LeftMovement:
            break;
          case  view_LeftUp:
            break;
        }
      }
  OUT(calcv_Hit );
  return  hit;
  }

static
Printer( self )
  register struct calcv	     *self;
  {
  register long		      i, x, y;

  calcv_PrintRoundBox( self, Left+1, Top+1, Width-3, Height-3, 0 );
  for ( i = 0; i < AreaCount; i++ )
    {
    switch ( AreaShape(i) )
      {
      case  circle:
	break;
      case  box:
	calcv_PrintBox( self, AreaLeft(i), AreaTop(i),
			    AreaWidth(i), AreaHeight(i), 0 );
    	break;
      case  roundbox:
	calcv_PrintRoundBox( self, AreaLeft(i), AreaTop(i),
				  AreaWidth(i), AreaHeight(i), 0 );
	break;
      }
    x = AreaCenter(i);
    y = AreaMiddle(i);
/*===
    switch ( AreaAlign(i) )
      {
      case Balanced:	x = AreaCenter(i);	break;
      case RightMiddle:	x = AreaRight(i) - 5;	break;
      }
===*/
    if ( AreaFont(i) )
      calcv_SetPrintFont( self, AreaSpec(i)->font_name );
    calcv_PrintString( self, x, y, AreaString(i), 0 );
    }
  }

void
calcv__Print( self, file, processor, format, level )
  register struct calcv	     *self;
  register FILE		     *file;
  register char		     *processor;
  register char		     *format;
  register boolean	      level;
  {
  IN(calcv_Print);
  calcv_PrintObject( self, file, processor, format, level, Printer );
  OUT(calcv_Print);
  }

static void
Stroke( self, area )
  register struct calcv	     *self;
  register long		      area;
  {
  IN(Stroke);
  DEBUGdt(Area,area);
  if ( area >= 0  &&  area <= 10 )  Digit( self, area );    
  else
  if ( area >= 11  &&  area <= 15 ) Operator( self, area );
  else
  if ( area == 16 )		    Clear( self, area );
  OUT(Stroke);
  }

static void
Digit( self, area )
  register struct calcv	     *self;
  register long		      area;
  {
  IN(Digit);
  Highlight_Area( self, area );
  if ( Expression[0] == '0'  &&  Expression[1] != '.'  &&
       *AreaString(area) != '.')
    strcpy( Expression, Expression + 1 );
  if ( PendingOp )
    {
    if ( PendingOp == '=' )
      {
      PendingOp = 0;
      *Expression = 0;
      strcpy( Operand1, AreaString(area) );
      }
      else  strcat( Operand2, AreaString(area) );
    }
    else  strcat( Operand1, AreaString(area) );
  sscanf( Operand1, "%F", &calc_Value( Data ) );
  if ( !(*AreaString(area) == '.'  &&  PointPresent) )
    {
    if ( *AreaString(area) == '.' )   PointPresent = true;
    strcat( Expression, AreaString(area) );
    PendingDisplay = true;
    calcv_WantUpdate( self, self );
    }
  OUT(Digit);
  }

static void
Operator( self, area )
  register struct calcv	     *self;
  register long		      area;
  {
  double		      operand_1, operand_2, value;

  IN(Operator);
  Highlight_Area( self, area );
  if ( *Operand1 )
    {
    sscanf( Operand1, "%F", &value );
    DEBUGst(Operand1,Operand1);
    PointPresent = false;
    if ( PendingOp )
      {
      DEBUGct(PendingOp,PendingOp);
      if ( *Operand2 )
        {
	DEBUGst(Operand2,Operand2);
        sscanf( Operand1, "%F", &operand_1 );
        sscanf( Operand2, "%F", &operand_2 );
        switch ( PendingOp )
          {
          case  '+':  value = operand_1 + operand_2;        break;
          case  '-':  value = operand_1 - operand_2;        break;
          case  'X':  value = operand_1 * operand_2;        break;
          case  '/':  value = operand_1 / ((operand_2) ? operand_2 : 1); break;
          }
	sprintf( Expression, "%f", value );
        Shrink( Expression );
        strcpy( Operand1, Expression );
        *Operand2 = 0;
        }
        else  sprintf( Expression, "%s", Operand1 );
      }
    if ( *AreaString(area) != '=' )
      {
      sprintf( Expression, "%s %s ", Expression, AreaString(area) );
      PendingDisplay = true;
      calcv_WantUpdate( self, self );
      }
      else
      {
      calc_SetValue( Data, value );
      calc_NotifyObservers( Data, calc_value_changed );
      }
    PendingOp = *AreaString(area);
    }
  OUT(Operator);
  }

static
Shrink( string )
  register char		     *string;
  {
  register char		     *ptr;

  ptr = string + strlen( string ) - 1;
  if ( index( string, '.' ) )
    while ( *ptr == '0' ) *ptr-- = 0;
  if ( *ptr == '.' )  *ptr = 0;
  }
 
static void
Clear( self, area )
  register struct calcv	     *self;
  register long		      area;
  {
  Highlight_Area( self, area );
  calc_SetValue( Data, 0.0 );
  strcpy( Expression, "0" );
  *Operand1 = *Operand2 = PendingOp = 0;
  PointPresent = false;
  calc_NotifyObservers( Data, calc_value_changed );
  }

static void
Display( self, area )
  register struct calcv	     *self;
  register long		      area;
  {
  }

static
Fill_Area( self, area, op )
  register struct calcv	     *self;
  register long		      area;
  register long		      op;
  {
  calcv_SetTransferMode( self, op );
  switch( AreaShape(area) )
    {
    case  circle:
      calcv_FillOval( self, AreaBound(area), graphic_BLACK );
      break;
    case  box:
      calcv_FillRectSize( self, AreaLeft(area) + 2, AreaTop(area) + 2,
			    AreaWidth(area) - 3, AreaHeight(area) - 3, graphic_BLACK );
      break;
    case  roundbox:
      calcv_FillRRectSize( self, AreaLeft(area), AreaTop(area),
			AreaWidth(area), AreaHeight(area), 6,6, graphic_BLACK );
      break;
    }
  }

static
Highlight_Area( self, area )
  register struct calcv	     *self;
  register long		      area;
  {
  if ( ! AreaHighlighted(area) )
    {
    AreaHighlighted(area) = true;
    Fill_Area( self, area, graphic_INVERT );
    Normalize_Other_Areas( self, area );
    }
  }

static
Normalize_Other_Areas( self, area )
  register struct calcv	     *self;
  register long		      area;
  {
  register long		      i;
  for ( i = 0; i < AreaCount; i++ )
    if ( AreaHighlighted(i)  &&  i != area )
      Normalize_Area( self, i );
  }

static
Normalize_Area( self, area )
  register struct calcv	     *self;
  register long			      area;
  {
  if (  AreaHighlighted(area) )
    {
    AreaHighlighted(area) = false;
    Fill_Area( self, area, graphic_INVERT );
    }
  }

static
Draw_Calc( self )
  register struct calcv	     *self;
  {
  register long		      i, x, y;

  IN(Draw_Calc);
  calcv_SetTransferMode( self, graphic_BLACK );
  for ( i = 0; i < AreaCount; i++ )
    {
    y = AreaMiddle(i);
    switch ( AreaAlign(i) )
      {
      case Balanced:	x = AreaCenter(i);	break;
      case RightMiddle:	x = AreaRight(i) - 5;	break;
      default:		x = AreaCenter(i);
      }
    calcv_DrawBoundedString( self, AreaString(i), AreaFont(i),  AreaBound(i),
				x, y, AreaAlign(i) );
    switch ( AreaShape(i) )
      {
      case  circle:	calcv_DrawOval( self, AreaBound(i) );	break;
      case  box:	calcv_DrawRect( self, AreaBound(i) );	break;
      case  roundbox:	calcv_DrawRRectSize( self, AreaLeft(i), AreaTop(i),
				AreaWidth(i), AreaHeight(i), 8,8 );
	break;
      }
    if ( AreaHighlighted(i) )  Fill_Area( self, i, graphic_INVERT );
    }
  OUT(Draw_Calc);
  }

static
Draw_Outline( self )
  register struct calcv	     *self;
  {
  IN(Draw_Outline);
  calcv_ClearClippingRect( self );
  calcv_SetTransferMode( self, graphic_BLACK );
  calcv_DrawRRectSize( self, Left+1, Top+1, Width-3,Height-3, 10,10 );
  if ( ! InputFocus )
    calcv_SetTransferMode( self, graphic_WHITE );
  calcv_DrawRRectSize( self, Left, Top, Width-1, Height-1, 11,11 );
  OUT(Draw_Outline);
  }
