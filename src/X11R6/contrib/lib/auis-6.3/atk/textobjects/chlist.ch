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

struct listitem {
    char *str;
    int loc;
    int (*proc)();
    long rock;
    char **regionStrings;
};

class chlist:text {
    classprocedures:
      InitializeObject(struct chlist *self) returns boolean;
      FinalizeObject(struct chlist *self);
    overrides:
      Clear();
    methods:
      AddItemAtIndex(char *str, long index, procedure proc, long rock) returns boolean;
      AddItemToEnd(char *str, procedure proc, long rock) returns boolean;
      DeleteItem(char *str) returns boolean;
      DeleteItemByIndex(long index) returns boolean;
      GetIndexByString(char *str) returns long;
      GetIndexByData(long rock) returns long;
      FindItem(char *str) returns struct listitem *;
      FindItemByIndex(unsigned long index) returns struct listitem *;
      SetFreeProcedure(procedure freeProc) returns procedure;
      ChangeItem(char *oldstr, char *newstr) returns boolean;
      ChangeItemByIndex(long index, char *newstr) returns boolean;
      ChangeData(char *oldstr, long rock) returns boolean;
      ChangeDataByIndex(long index, long rock) returns boolean;
      DefineRegion(long regionNum);
      DefineStringRegion(long regionNum);
      SetRegionStringByIndex(long index, long regionNum, char *regionStr);
      SetRegionString(char *str, long regionNum, char *regionStr);
      GetRegionInfoForPosition(long index, long position, long *size, long *offset) returns long;
      GetIndexByPosition(long position, long *regionID, long *size, long *offset) returns long;
      EnumerateItems(long startIndex, long length, procedure proc, long rock);

    macromethods:
      GetItemList() (self->ItemList)
      GetNumItems() (self->numitems)
      GetNumRegions() (self->numRegions)
    data:
      struct listitem *ItemList;
      int numitems, numitemsallocated;
      procedure freeProc;
      int numRegions, numRegionsAllocated;
      int strRegionNum;
};
