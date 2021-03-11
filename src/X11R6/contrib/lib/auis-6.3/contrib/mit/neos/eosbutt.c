/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eosbutt.c,v 1.4 1993/05/17 16:51:08 susan Exp $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eosbutt.c,v $ */
/* $Author: susan $ */

#ifndef lint
static char *eosbut_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eosbutt.c,v 1.4 1993/05/17 16:51:08 susan Exp $";
#endif /* lint */
/*
 * eosbutt.c
 *
 * Package for creating lists of buttons.
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *   For full copyright information see:'mit-copyright.h'     *
 ************************************************************ */

#include <mit-copyright.h>

#include "eosbutt.eh"

struct buttonList *eosbutton__MakeButton(classID, blist, text, function, object) 
struct classheader *classID;
struct buttonList *blist;
char *text;
void (*function)();
struct view *object;
/* Creates a new button and with given attributes
   returns [newbutton::blist]
 */
{
    struct buttonList *button = (struct buttonList *)     malloc(sizeof(struct buttonList));
    int style;

    style = environ_GetProfileInt("buttonstyle", 2);

    button->butt  = pushbutton_New();
    button->buttv = newbuttonview_New();
    pushbutton_SetStyle(button->butt, style);
    pushbutton_SetText(button->butt, text);
    newbuttonview_SetDataObject(button->buttv, button->butt);
    newbuttonview_AddRecipient(button->buttv,   atom_Intern("buttonpushed"), object, function, 0L);
      
    button->next = blist;
    return button;
}

struct lplist *eosbutton__MakeLpair(classID, lpl)
struct classheader *classID;
struct lplist *lpl;
/* Creates a new lpair and returns [newlpair::lplist]
 */
{
    struct lplist *lpair = (struct lplist *) malloc(sizeof(struct lplist));
    
    lpair->lp = lpair_New();
    lpair->next = lpl;
    return lpair;
}

