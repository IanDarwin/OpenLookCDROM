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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/dict.c,v 2.7 1992/12/15 21:41:42 rr2b R6tape $";
#endif


 


#include <class.h>
#include <dict.eh>
#include <view.ih>

#define INITIALSIZE 128
#define dictionary_DELETED NULL
#define EntryDeleted(A) (A->view == (struct view *) dictionary_DELETED &&  A->id == (char *)dictionary_DELETED)
#define DeleteEntry(A)  A->view = (struct view *)dictionary_DELETED ; A->id = (char *) dictionary_DELETED ;
struct dirtable {
    struct view *view;
    char *id;
    char *object;
};
static struct dirtable *table, *last, *end;
boolean dictionary__InitializeClass(classID)
    struct classheader *classID;
{
    table = (struct dirtable *) malloc(INITIALSIZE * sizeof(struct dirtable));
    end = &(table[INITIALSIZE]);
    last = table;

    return TRUE;
}
void dictionary__Insert(classID, view,id,object)
struct classheader *classID;
register struct view *view;
char *id; 
char *object;
{
    register struct dirtable *dd,*freeref;
    freeref = NULL;
    for(dd = table; dd < last; dd++){
        if(dd->view == view && dd->id == id) break;
        if(freeref == NULL && EntryDeleted(dd)) freeref = dd;
    }
    if(dd == last) {
        if(freeref != NULL){
            dd = freeref;
        }
        else {
            if(last == end){
                int newsize = INITIALSIZE + (end - table) ;
		long diff = last - table; 
                table = (struct dirtable *) realloc(table,newsize * sizeof(struct dirtable));
		if(table == NULL){
			fprintf(stderr,"Out Of Memory in dict.c\n");
			return;
			} 
                end  = &(table[newsize]);
		last = table + diff;
		dd = last;
            }
            last++;
        }
    }
    dd->view = view;
    dd->id = id;
    dd->object = object;
}
char *dictionary__LookUp(classID, view,id)
struct classheader *classID;
register struct view *view;
char *id;
{
    register struct dirtable *dd;
    for(dd = table; dd < last; dd++)
        if(dd->view == view && dd->id == id) return(dd->object);
    return(NULL);
}
long dictionary__CountViews(classID, id)
struct classheader *classID;
char *id;
{
    register struct dirtable *dd;
    long i = 0;
    for(dd = table; dd < last; dd++){
        if(dd->id == id && dd->view != NULL){
            i++;
        }
    }
    return i;
}
long dictionary__ListViews(classID, id,viewarray,len)
struct classheader *classID;
char *id;
struct view **viewarray;
long len;
{
    register struct dirtable *dd;
    long i = 0;
    for(dd = table; dd < last; dd++){
        if(dd->id == id && dd->view != NULL ){
            viewarray[i++] = dd->view;
            if(i == len) break;
        }
    }
    return i;
}
long dictionary__CountRefs(classID, view)
struct classheader *classID;
register struct view *view;
{
    register struct dirtable *dd;
    long i = 0;
    for(dd = table; dd < last; dd++){
        if(dd->view == view ){
            i++;
        }
    }
    return i;
}
long dictionary__ListRefs(classID, view,refarray,len)
struct classheader *classID;
register struct view *view;
char **refarray;
long len;
{
    register struct dirtable *dd;
    long i = 0;
    for(dd = table; dd < last; dd++){
        if(dd->view == view){
            refarray[i++] = dd->id;
            if(i == len) break;
        }
    }
    return i;
}

void dictionary__Delete(classID, view,id)
struct classheader *classID;
register struct view *view;
char *id;
{
    register struct dirtable *dd;
    for(dd = table; dd < last; dd++)
        if(dd->view == view && dd->id == id){
            DeleteEntry(dd);
            return;
        }
}
