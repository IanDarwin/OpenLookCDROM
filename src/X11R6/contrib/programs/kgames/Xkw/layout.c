#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>

#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Cardinals.h>
#include "Layout.h"
#include "Cards.h"

main (argc, argv)
    char    **argv;
{
    XtAppContext xtcontext;
    Widget toplevel, layout, label, cards;
    Arg arg[2];
    int	i;
    CardsCardRec    card[10];
    
    toplevel = XtAppInitialize(&xtcontext, "Layout", NULL, 0,
			       &argc, argv, NULL, NULL, 0);

    layout = XtCreateManagedWidget ("layout", layoutWidgetClass, toplevel, NULL, 0);
    label = XtCreateManagedWidget ("label1", labelWidgetClass, layout, NULL, 0);
    label = XtCreateManagedWidget ("label2", labelWidgetClass, layout, NULL, 0);
    label = XtCreateManagedWidget ("label3", labelWidgetClass, layout, NULL, 0);
    cards = XtCreateManagedWidget ("cards", cardsWidgetClass, layout, NULL, 0);
    card[0].rank = CardsAce;
    card[0].suit = CardsHeart;
    card[1].rank = CardsKing;
    card[1].suit = CardsClub;
    card[2].rank = CardsAce;
    card[2].suit = CardsBack;
    card[3].rank = CardsAce;
    card[3].suit = CardsEmpty;
    CardsAddCard (cards, &card[0], 0, 0, (XtPointer) 0, Above);
    CardsAddCard (cards, &card[1], 0, 1, (XtPointer) 0, Above);
    CardsAddCard (cards, &card[2], 1, 0, (XtPointer) 0, Above);
    CardsAddCard (cards, &card[3], 1, 1, (XtPointer) 0, Above);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (xtcontext);
}
