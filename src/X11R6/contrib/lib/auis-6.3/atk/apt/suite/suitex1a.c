

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/apt/suite/RCS/suitex1a.c,v 1.5 1993/06/03 20:27:31 gk5g Exp $";
#endif
#include <im.ih>
#include <frame.ih>
#include <suite.ih>
#include <message.ih>
#include <suitex1a.eh>

struct view *Flavor_Choice();

static suite_Specification vanilla[] = {
    suite_ItemCaption( "Vanilla" ),
    suite_ItemTitleCaption( "Flavor 1:" ),
    0
};

static suite_Specification strawberry[] = {
    suite_ItemCaption( "Strawberry" ),
    suite_ItemTitleCaption( "Flavor 2:" ),
    0
};

static suite_Specification chocolate[] = {
    suite_ItemCaption( "Chocolate" ),
    suite_ItemTitleCaption( "Flavor 3:" ),
    0
};

static suite_Specification grape[] = {
    suite_ItemCaption( "Grape" ),
    suite_ItemTitleCaption( "Flavor 4:" ),
    0
};

static suite_Specification fudge[] = {
    suite_ItemCaption( "Fudge" ),
    suite_ItemTitleCaption( "Flavor 5:" ),
    0
};

static suite_Specification licorice[] = {
    suite_ItemCaption( "Licorice" ),
    suite_ItemTitleCaption( "Flavor 6:" ),
    0
};

static suite_Specification caramel[] = {
    suite_ItemCaption( "Caramel" ),
    suite_ItemTitleCaption( "Flavor 7:" ),
    0
};

static suite_Specification lemon[] = {
    suite_ItemCaption( "Lemon" ),
    suite_ItemTitleCaption( "Flavor 8:" ),
    0
};

static suite_Specification orange[] = {
    suite_ItemCaption( "Orange" ),
    suite_ItemTitleCaption( "Flavor 9:" ),
    0
};


suite_Specification flavors[] = {
    suite_HitHandler( Flavor_Choice ),
    suite_Item(vanilla),
    suite_Item(strawberry), 
    suite_Item(chocolate),
    suite_Item(grape), 
    suite_Item(fudge), 
    suite_Item(licorice),
    suite_Item(caramel), 
    suite_Item(lemon), 
    suite_Item(orange), 
    suite_ItemTitleCaptionAlignment( suite_Top | suite_Left ),
    suite_ItemCaptionAlignment( suite_Middle | suite_Center ),
    suite_BorderSize( 4 ),
    suite_ItemBorderSize( 4 ),
    suite_ItemCaptionFontName( "andysans12b" ),
    suite_GutterSize ( 5 ),
    suite_Arrangement( suite_Matrix | suite_Fixed ),
    suite_Columns( 3 ), suite_Rows( 3 ),
    suite_ForegroundColor( "red" ),
    suite_BackgroundColor( "antiquewhite" ),
    suite_ActiveItemForegroundColor( "black" ),
    suite_ActiveItemBackgroundColor( "cornsilk" ),
    suite_TitleCaption("9 Wondertastic Flavors!"),
    suite_TitleCaptionFontName( "andy14bi" ),
    NULL
};


boolean
suitex1app__Start( self )
struct suitex1app *self;
{
    struct frame *f = frame_New();
    frame_SetView(f, suite_Create(flavors, self));
    im_SetPreferedDimensions(0, 0, 300, 300);
    im_SetView(im_Create(NULL), f);
    frame_PostDefaultHandler(f, "message", frame_WantHandler(f, "message"));
    return(TRUE);
}

struct view *
Flavor_Choice( self, suite, item, type, action, x, y, clicks )
struct suitex1app *self;
register struct suite *suite;
register struct suite_item *item;
{
    char msg[100];
    if(action == view_LeftUp) {
	sprintf(msg, "Chosen Flavor is %s.", suite_ItemAttribute(suite, item, suite_ItemCaption(0)));
	message_DisplayString(suite, 0, msg);
    }
    return(NULL);
}

