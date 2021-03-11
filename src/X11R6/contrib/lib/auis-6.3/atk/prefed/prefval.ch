/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University All rights Reserved. */

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



enum prefval_Type {
    prefval_None,
    prefval_Integer,
    prefval_Real,
    prefval_String,
    prefval_Boolean,
    prefval_Filename,
    prefval_Directory,
    prefval_Font,
    prefval_Color
};

enum prefval_Changes {
    prefval_Generic,
    prefval_ChoicesChanged,
    prefval_ValuesChanged
};

struct prefval_value {
    union prefval_realvalue {
	int ival;
	float rval;
	char *sval;
	boolean bval;
	char *fval;
	char *dval;
	char *fontval;
	char *cval;
    } v;
    boolean set;
};

#define prefval_DS_VERSION 0

class prefval : dataobject [dataobj] {
classprocedures:
    InitializeObject(struct prefval *self) returns boolean;
    FinalizeObject(struct prefval *self);
    StringToType(char *str) returns enum prefval_Type;

methods:
    SetAppName(char *name);
    SetPrefName(char *name);
    SetSeparator(char *sep);
    SetChoices(int nchoices, char **choices, struct prefvalue *cvalues);
    SetValues(int nvalues, struct prefval_value *tvalues);
    SetIndexValue(int which, struct prefval_value *tvalue);
    SetValue(struct prefval_value *tvalue);
    SetChoice(int which, char *choice, struct prefval_value *tvalue);
    StringToValue(char *str) returns struct prefval_value *;
    IndexValueString(int which) returns char *;
    TypeString() returns char *;
    ReadDataPart(FILE *fp, int dsversion) returns long;
    PreferenceString() returns char *;
    SetFromPreferenceString(char *str);
    ClearValues();
    ClearChoices();
    FreeValue(struct prefval_value *v);
    InitValue(struct prefval_value *v);
    CopyValue(strcccct prefval_value *v1, struct  prefval_value *v2);
    SortChoices();
    SetCondition(char *str);
    SetDefault();
    FullPreferenceString() returns char *;
overrides:
    Write(FILE *fp, long writeID, int level) returns long;
    Read(FILE *fp, long id) returns long;
    SetModified();
  
macros:
macromethods:
    GetAppName() (self->appname)
    GetPrefName()(self->prefname)
    GetChoiceNames() (self->choices)
    GetIndexChoiceName(int which) (self->choices?self->choices[which]:NULL)
    GetIndexChoiceValue(int which) (self->cvalues?&self->cvalues[which]:NULL)
    GetIndexValue(int which) (self->values?&self->values[which]:NULL)
    GetListSize() (self->vlistsize)
    GetChoiceListSize() (self->clistsize)
    GetValue() (self->values ? &self->values[0] : NULL)
    SetType(enum prefval_Type t) ((self)->type=(t))
    GetType() (self->type)
    ValueString() (prefval_IndexValueString(self, 0))
    SetListMax(n) (self->listmax=(n))
    GetListMax() (self->listmax)
    GetSeparator() (self->separator)
    SetCurrentItem(n) (self->curitem=(n))
    GetCurrentItem() (self->curitem)
    GetCondition() (self->condition)
    GetIsDefault() (self->isdefault)
    SetRangeLow(int l) (self->low=(l))
    SetRangeHigh(int h) (self->high=(h))
    GetRangeLow() (self->low)
    GetRangeHigh() (self->high)

data:
    char *prefname;
    char *appname;
    enum prefval_Type type;
    boolean valueset;
    int listmax;
    int vlistsize;
    struct prefval_value *values;
    int clistsize;
    char **choices;
    struct prefval_value *cvalues;
    char *separator;
    int curitem;
    char *condition;
    boolean isdefault;
    long low, high;
};
