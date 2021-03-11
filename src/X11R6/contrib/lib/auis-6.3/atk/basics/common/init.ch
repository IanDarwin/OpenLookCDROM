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


 

/* init.H
 * Class header file for initialization file reader.
 *
 */

class init  {
    classprocedures:
        InitializeObject() returns boolean;
	FinalizeObject(struct init *self);
    methods:
        ModifyMenulist(struct menulist *ml) returns struct menulist *;
        ModifyKeystate(struct keystate *ks) returns struct keystate *;
        Duplicate() returns struct init *;
        Load(char *filename, procedure errorProc, long errorRock, boolean executeImmediately) returns int;
        AddKeyBinding(char *class, boolean inherit, struct keymap *keymap);
        DeleteKeyBinding(char *class, boolean inherit, struct keymap *keymap);
        AddMenuBinding(char *class, boolean inherit, struct menulist *menulist);
        DeleteMenuBinding(char *class, boolean inherit, struct menulist *menulist);

    data:
        struct keys *keys;
        struct menus *menus;
        struct keystateList *usedKeystates;
	struct mlList *usedMenus;
	struct children *kids;
	struct init *parent;
	int version;
};
