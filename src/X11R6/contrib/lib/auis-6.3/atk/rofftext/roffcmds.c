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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/rofftext/RCS/roffcmds.c,v 2.15 1994/03/04 20:30:04 wjh Exp $";
#endif


 

/* rofftext
 *
 * Commands
 */

#include <andrewos.h> /* strings.h */
#include <class.h>
#include <hash.ih>
#include <glist.ih>
#include <text.ih>
#include <mmtext.ih>
#include <link.ih>
#include <ctype.h>
#include <roffstyl.h>
#include <rofftext.ih>
#include <rofftext.h>
#include <roffutil.h>

/* define or re-define string
  * must parse its own arguments
 */

ds_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    register int c;
    static BUF b = NULL;
    char name[3];
    boolean tmp = self->v_CopyMode;

    if (b == NULL)
        b = NewBuf();

    getarg(self,t,name,2,0);
    if (*name == '\n')
        return;

    ClearBuf(b);

    self->v_CopyMode = TRUE;
    /* read whitespace */
    while((c = get(self,t)) == ' ' || c == '\t');

    if (c == '\"') /* strip leading quote */
        c = get(self,t);

    /* read string */
    while((c != '\n') && (c != EOF)) {
        Add2Buf(b,c);
        c = get(self,t);
    }
    Add2Buf(b,'\0');
    self->v_CopyMode = tmp;

    DEBUG(1, (stderr,"Defining string (%s) as (%s)\n",name,Buf2Str(b)));
    putstring(self,name,Buf2Str(b));
}

/*  append to string */
as_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    register int c;
    static BUF b = NULL;
    char name[3];
    char *init;
    boolean tmp = self->v_CopyMode;

    b = NewBuf();

    getarg(self,t,name,2,0);
    if (*name == '\n')
        return;

    ClearBuf(b);
    init = getstring(self, name);
    while(*init) Add2Buf(b, *init++);

    self->v_CopyMode = TRUE;
    /* read whitespace */
    while((c = get(self,t)) == ' ' || c == '\t');

    if (c == '\"') /* strip leading quote */
        c = get(self,t);

    /* read string */
    while((c != '\n') && (c != EOF)) {
        Add2Buf(b,c);
        c = get(self,t);
    }
    Add2Buf(b,'\0');
    self->v_CopyMode = tmp;

    DEBUG(1, (stderr,"Appending to string (%s) becoming (%s)\n",name,Buf2Str(b)));
    putstring(self,name,Buf2Str(b));
    FreeBuf(b);
}

/* rename or remove request, string, macro */
rm_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    char *value,*str = "removing",*v;
    boolean rename = FALSE;

    if (argv[0][1] == 'n') { /* this is rn, not rm */
        rename = TRUE;
        str = "renaming";
    }

    value = hash_Lookup(self->Macros,argv[1]);
    if (value) {
        DEBUG(1, (stderr,"%s string (%s)",str,argv[1]));
        if (rename) {
            v=hash_Rename(self->Macros,argv[1],argv[2]);
            v += (int)hash_Rename(self->Commands,argv[1],argv[2]);
        }
        else {
            v = hash_Delete(self->Macros,argv[1]);
            v += (int)hash_Delete(self->Commands,argv[1]);
            free(value);
        }
        DEBUG(1, (stderr,"%s\n",(v?"succeeded":"FAILED ***")));
        return;
    }
    value = hash_Lookup(self->Commands,argv[1]);
    if (value) {
        DEBUG(1, (stderr,"%s string (%s)",str,argv[1]));
        if (rename)
            v=hash_Rename(self->Commands,argv[1],argv[2]);
        else {
            v=hash_Delete(self->Commands,argv[1]);
            free(value);
        }
        DEBUG(1, (stderr,"%s\n",(v?"succeeded":"FAILED ***")));
        return;
    }
    DEBUG(1, (stderr,"RENAME/DELETE: (%s) NOT FOUND\n",argv[1]));
}


/* define register */
nr_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int value,inc;
    boolean relative;

    EvalString(self,argv[2],&value,scale_u,NULL,&relative);
    if (argc >= 4) {
        EvalString(self,argv[3],&inc,scale_u,NULL,NULL);
    }
    else
        inc = 0;

    DEBUG(1, (stderr,"Defining register (%s) as (%d) inc (%d) rel (%d)\n",argv[1],value, inc, relative));
    putregister(self,argv[1],value,0,inc,relative);
}

/* assign format */
af_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    struct reg *r = (Reg)hash_Lookup(self->Registers,argv[1]);
    enum RegFmt fmt;

    switch(*argv[2]) {
        case '0':
            fmt = reg_Triple;
            break;
        case 'i':
            fmt = reg_LCRoman;
            break;
        case 'I':
            fmt = reg_UCRoman;
            break;
        case 'a':
            fmt = reg_LCaz;
            break;
        case 'A':
            fmt = reg_UCaz;
            break;
        default:
            fmt = reg_Single;
    }
    if (r==NULL) {
        r = NewObj(struct reg);
        r->value = 0;
        r->format = fmt;
        r->autoinc = 0;
        hash_Store(self->Registers,argv[1],r);
    }
    else {
        r->format = fmt;
    }

}

/* remove register */
rr_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{    
    hash_Delete(self->Registers, argv[1]);
}


/* read from a file */
so_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    tpush(self,t,argv[1],NULL,NULL,FALSE,NULL,NULL);
}



/* line break */
br_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    DoBreak(self);
}

/* exit be2roff ??? */
ex_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
}


/* Do a macro */
DoMacro(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{

    char *macro = hash_Lookup(self->Macros,argv[0]);
    DEBUG(1, (stderr,"v-----Calling Macro named (%s)-------v\n",argv[0]));
    tpush(self,t,NULL,NULL,macro,TRUE,argc,argv);
}



/* define or re-define macro */
de_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    char *macro,*existing,*name = StrDup(argv[1]);
    char *end = StrDup(argv[2]);
    static BUF b = NULL;
    char *oldValue;
    boolean svCopyMode = self->v_CopyMode;
    BUF svSnarfOutput = self->CurrentDiversion->SnarfOutput;

    b = NewBuf();
    ClearBuf(b);

    self->v_CopyMode = TRUE;
    self->CurrentDiversion->SnarfOutput = b;

    Scan(self,t,((argc<3)?".":end));

    self->CurrentDiversion->SnarfOutput = NULL;
    self->v_CopyMode = FALSE;
    Add2Buf(b,'\0');

    macro = StrDup(Buf2Str(b));

    hash_Delete(self->Commands,name);
    existing = hash_Delete(self->Macros,name);
    if (existing)
        free(existing);

    DEBUG(1, (stderr,"--Defining Macro (%s)--\n",name));

    oldValue = hash_Delete(self->Macros, name);
    if (oldValue != NULL) {
	free(oldValue);
    }
    hash_Store(self->Macros,name,macro);
    hash_Store(self->Commands,name,DoMacro);
    free(name);
    free(end);
    FreeBuf(b);

    self->v_CopyMode = svCopyMode;
    self->CurrentDiversion->SnarfOutput = svSnarfOutput;

/*    DEBUG(1, (stderr,"%s\n<----------------\n",macro));*/
}

/* append to macro */
am_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    char *macro,*existing,*name = StrDup(argv[1]);
    char *end = StrDup(argv[2]);
    char *init;
    BUF b;

    b =	NewBuf(); 

    ClearBuf(b);
    init = getstring(self, name);
    while(*init) Add2Buf(b, *init++);

    self->v_CopyMode = TRUE;
    self->CurrentDiversion->SnarfOutput = b;

    Scan(self,t,((argc<3)?".":end));

    self->CurrentDiversion->SnarfOutput = NULL;
    self->v_CopyMode = FALSE;
    Add2Buf(b,'\0');

    macro = StrDup(Buf2Str(b));

    hash_Delete(self->Commands,name);
    existing = hash_Delete(self->Macros,name);
    if (existing)
        free(existing);

    DEBUG(1, (stderr,"--Appending To Macro (%s)--\n",name));

    hash_Store(self->Macros,name,macro);
    hash_Store(self->Commands,name,DoMacro);
    free(name);
    free(end);
    FreeBuf(b);
/*    DEBUG(1, stderr,"---%s\n<----------------\n",macro);*/
}

/* divert output into a macro */

di_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    char *macro;
    static BUF b = NULL;
    char *existing;

    b = NewBuf();

    if (argc > 1) { 

        ClearBuf(b);

        DEBUG(1, (stderr,"--Diverting to Macro (%s)------>\n",argv[1]));

/*        tmp = self->v_CopyMode;
        self->v_CopyMode = TRUE;*/
        PushDiversion(self);
	self->CurrentDiversion->SnarfOutput = b;
        self->CurrentDiversion->name = StrDup(argv[1]);
    }
    else{
        if (self->v_DiversionLevel > 0) {
	    b =	self->CurrentDiversion->SnarfOutput;
	    self->CurrentDiversion->SnarfOutput = NULL;
            /*        self->v_CopyMode = tmp;*/
            Add2Buf(b,'\0');

            macro = StrDup(Buf2Str(b));

            existing = (char *)hash_Delete(self->Macros,self->CurrentDiversion->name);
            if (existing)
                free(existing);
            hash_Delete(self->Commands,self->CurrentDiversion->name);

            hash_Store(self->Macros,self->CurrentDiversion->name,macro);
            hash_Store(self->Commands,self->CurrentDiversion->name,DoMacro);
            DEBUG(1, (stderr,"\n<----------------divert (%s)\n",self->CurrentDiversion->name));
            PopDiversion(self);
	    FreeBuf(b);
	    putregister(self, "dn", 1, reg_Single, 0, 0);
        }
    }
}

/* divert and append output into a macro */
da_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    char *macro;
    char *existing;
    char *init;
    BUF	b;

    b =	NewBuf();

    if (argc > 1) { 

        ClearBuf(b);

	init = getstring(self, argv[1]);
	while(*init) Add2Buf(b, *init++);
        DEBUG(1, (stderr,"--Diverting and appending to Macro (%s)---(%d)--->\n",argv[1], self->v_DiversionLevel));

/*        tmp = self->v_CopyMode;
        self->v_CopyMode = TRUE;*/
        PushDiversion(self);
	self->CurrentDiversion->SnarfOutput =	b;
        self->CurrentDiversion->name = StrDup(argv[1]);
    }
    else{
        if (self->v_DiversionLevel > 0) {
	    b =	self->CurrentDiversion->SnarfOutput;
	    self->CurrentDiversion->SnarfOutput = NULL; 
            /*        self->v_CopyMode = tmp;*/
            Add2Buf(b,'\0');

            macro = StrDup(Buf2Str(b));

            existing = (char *)hash_Delete(self->Macros,self->CurrentDiversion->name);
            if (existing)
                free(existing);
            hash_Delete(self->Commands,self->CurrentDiversion->name);

            hash_Store(self->Macros,self->CurrentDiversion->name,macro);
            hash_Store(self->Commands,self->CurrentDiversion->name,DoMacro);
            DEBUG(1, (stderr,"\n<----------------divert (%s)\n",self->CurrentDiversion->name));
            PopDiversion(self);
	    FreeBuf(b);
	    putregister(self, "dn", 1, reg_Single, 0, 0);
	}
    }
}


/* debugging */
c0_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    self->v_CopyMode = 0;
}

c1_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    self->v_CopyMode = 1;
}


/* if-else  -- special syntax */

ie_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int c,delim,delimcount;
    char *str,*string2;
    boolean sense = FALSE,result;
    int dresult;
    static BUF b = NULL;

    b = NewBuf();

    /* read leading whitespace */
    do {
        c = get(self,t);
    } while((c==' ')||(c=='\t'));

    /* tell what to do by first char */

    if (c=='!') {
        sense = TRUE;
        do {
            c = get(self,t);
        } while((c==' ')||(c=='\t'));
    }
    if (isdigit(c) || c == '(') { /* numeric compare */
        ClearBuf(b);
        while((c!= ' ') && (c != '\t') && (c != '\n')) {
            Add2Buf(b,c);
            c = get(self,t);
        }
        Add2Buf(b,'\0');
        str = Buf2Str(b);
        DEBUG(1, (stderr,"Comparing numeric value (%s)\n",str));
        EvalString(self,str,&dresult,scale_u,NULL,NULL);
        result = (dresult > 0);
    }
    else {
        switch(c) {
            case 'o': result = TRUE;
                break;
            case 'e': result = FALSE;
                break;
            case 't': result = self->RoffType;
                break;
            case 'n': result = !self->RoffType;
                break;
            default: /* string compare */
                /* read first argument into string */ /* this is inadequate */
                ClearBuf(b);
                delimcount = 0;
                delim = c;
                c = get(self,t);
                if (c == delim)
                    delimcount++;

                while((c != '\n') && (delimcount < 2)) {
                    Add2Buf(b,c);
                    c = get(self,t);
                    if (c == delim)
                        delimcount++;
                }
                Add2Buf(b,'\0');
                str = Buf2Str(b);
                DEBUG(1, (stderr,"====>if: delim '%c', string argument is (%s)\n",delim,str));


                string2 = index(str,delim);
                if (string2 == NULL)
                    string2 = "";
                else {
                    *string2++ = '\0';
                }
                DEBUG(1, (stderr,"Comparing (%s) (%s)\n",str,string2));
                result = (strcmp(str,string2)==0);

                break;
        }
        c = get(self,t); /* get first char afterwards because number does */

    }
    ClearBuf(b);
    DEBUG(1, (stderr,"if: sense %d, result %d\n",sense,result));
    /* if condition is false, munch to end of conditional input */
    if (result == sense) {
	int oldlev = self->v_InCond;
        self->v_RawMode = TRUE;
        DEBUG(1, (stderr,"-->Munch->"));
	while(c	!= EOF && ((c != '\n') || (self->v_InCond > oldlev))) {
            c = get(self,t);
        }
        DEBUG(1, (stderr,"<-Munch<--"));
        self->v_RawMode = FALSE;
    }
    else {
        /* read spaces */
        while((c ==' ') || (c == '\t'))
            c = get(self,t);
        ung(self,c,t);
        Set_BOL(self); /* next char is BOL */
    }

    self->v_LastIfResult = (sense != result);
    FreeBuf(b);
}


/* if command -- special syntax */
if_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    boolean tmp = self->v_LastIfResult;
    ie_cmd(self,t,br,argc,argv);
    self->v_LastIfResult = tmp;
}

/* else command -- special syntax */
el_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int c;

    DEBUG(1, (stderr,"else: previous result was %d\n",self->v_LastIfResult));
    if (self->v_LastIfResult) { /* munch to end of conditional input */
	int oldlev = self->v_InCond;
        self->v_RawMode = TRUE;
        DEBUG(1, (stderr,"-->Munch->"));
        do {
            c = get(self,t);
        } while(c != EOF && ((c != '\n') || (self->v_InCond > oldlev)));
        DEBUG(1, (stderr,"<-Munch<--"));
        self->v_RawMode = FALSE;
    }
    else {
        do {
            c = get(self,t);
        } while((c ==' ')||(c == '\t')); /* munch whitespace */
        ung(self,c,t);
        Set_BOL(self);
    }
    self->v_LastIfResult = TRUE;
}



/* table macros */

/* create a table */
Ct_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int i;
    self->Tbl = table_New();
    table_ChangeSize(self->Tbl, 0, 0);
    self->v_TblMode = 1;
    self->v_TblTab = '\t';
    for (i = 0; i < MAX_COLS; i++) self->colWidth[i] = 1;
}

static char gettblopts(argc, argv)
int argc;
char argv[80][80]; {
    int i;
    for (i = 0; i < argc; i++) {
	if (strncmp(argv[i], "tab(", 4) == 0)
	    return argv[i][4];
    }
    return '\t';
}

static void gettblfmt(argc, argv)
int argc;
char argv[80][80]; {
}

static char *parsetbl(line, sep, multi, out)
char *line;	/* the input */
char *sep;	/* separators */
int multi;	/* if 1, skip multiple separators */
char *out; {	/* the output */
    char *p = line;
    *out = *p;
    if (*p == '\0') return NULL;
    while (*p != '\0' && !strchr(sep, *p)) *out++ = *p++;  /* copy till separator */
    *out = '\0';
    if (!multi) {
	if (*p != '\0')	p++;				    /* skip one separator */
    } else {
	while (*p != '\0' && strchr(sep, *p)) p++;	    /* skip all separators */
    }
    return p;
}


/* insert a row in the table */
InsertTbl(self,t)
struct rofftext *self;
Trickle t;
{
    int row, col, colmax;
    char line[500], tabstr[10];
    char argv[80][80]; static int argc = 0;
    char c, last, *p = line;
    if (self->v_TblMode == 0 || self->Tbl == NULL) return;

    while ((c =	get(self, t)) != '\n') *p++ = self->Berliz[(int)c];    /* copy the line */
    *p-- = '\0';
DEBUG(1, (stderr, "Tbl reading [%s] in state %d\n", line, self->v_TblMode));
    while (p > line && (*p == ' ' || *p == '\t')) p--;
    if (p <= line) last	= '\0';			    /* remember the last char */
    else last = *p;
    p = line;
    if (self->v_TblMode == 1) {	/* look for global options */
	self->v_TblMode = 2;
	if (last == ';') {
	    while ((p = parsetbl(p, " \t,", TRUE, argv[argc])) != NULL) argc++;	/* parse it */
	    self->v_TblTab = gettblopts(argc, argv);
	    argc = 0;
	    return;
	}
    }
    if (self->v_TblMode == 2) {	/* look for formats */
	while ((p = parsetbl(p, " \t", TRUE, argv[argc])) != NULL) {
	    argc++;	/* parse it */
	}
	gettblfmt(argc, argv);
	if (last == '.') {
	    self->v_TblMode = 3;
	}
	argc = 0;
	return;
    }
    if ((strcmp(line, "=") == 0) || (strcmp(line, "_") == 0)) return;	  /* ignore horiz lines */
    tabstr[0] = self->v_TblTab;	/* to parse table */
    tabstr[1] = '\0';
DEBUG(1, (stderr, "about to parse [%s]\n", line));
    while ((p = parsetbl(p, tabstr, FALSE, argv[argc])) != NULL) {
	if(!strcmp(argv[argc], "T{") || !strcmp(argv[argc], "T}"))
	    return;	/* skip it -- string inside a "T{...}T" construct */
	else argc++;	/* parse it */
    }
    row	= table_NumberOfRows(self->Tbl)+1;	/* size the table */
    colmax = table_NumberOfColumns(self->Tbl);
    if (colmax < argc) colmax = argc;
    table_ChangeSize(self->Tbl, row, colmax);
    for (col = 0; col < argc; col++) {
	struct cell *c = table_GetCell(self->Tbl, row-1, col);
	char buf[2000];
	int width = strlen(argv[col]);
	if (width > self->colWidth[col]) self->colWidth[col] = width;
	buf[0]='\"';
	strcpy(buf+1, argv[col]);	/* force to string */
	table_ParseCell(self->Tbl, c, buf);
    }
    argc = 0;	/* reset argc when we drop through because it's static */
}

/* end the table */
Et_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int colmax = table_NumberOfColumns(self->Tbl);
    int col;
    for (col = 0; col < colmax; col++) 
	self->Tbl->col[col].thickness = self->colWidth[col]*9;
 
    text_AddView(self->text, self->pos++, table_ViewName(self->Tbl), self->Tbl);
    text_InsertCharacters(self->text, self->pos++, "\n", 1);
    self->v_TblMode = 0;
}

/* Heading command */
Hd_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int style = 0;
    int lev;
    char *p;
    char *str;
    if (argc < 3) return;
    lev = atoi(argv[1]);
    switch (lev) {
	case 0: 
	case 1:	str = "chapter";
	    break;
	case 2:	str = "section";
	    break;
	case 3:	str = "subsection";
	    break;
	case 4:	
	case 5: str = "paragraph";
	    break;
	case 6:	
	case 7:	
	    break;
	case 8:	str = "majorheading";
	    break;
	case 9:	str = "heading";
	    break;
	default: str = "";
	    break;
    }
    if (str[0] != '\0') style = BeginStyle(self, str);
    p = argv[2];
    while (*p != '\0') put(self, *p++);
    if (str[0] != '\0') EndStyle(self, style);
}

/* Begin Page command */
bp_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    put(self, '\n');	    /* this is necessary; otherwise font changes are one off */
    text_InsertObject(self->text, self->pos++, "bp", "bpv");
    put(self, '\n');
}

/* Footnote command */
Fn_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int sw = 0;
    if (argc > 1) sw = atoi(argv[1]);
    if (sw != 0) {		    /* start a footnote */
	self->Fn = fnote_New();
	self->fnpos = 0;
    } else if (self->Fn	!= NULL) {  /* end a footnote */
	text_AddView(self->text, self->pos++, fnote_ViewName(self->Fn), self->Fn);
	self->Fn = NULL;
    }
}

/*  Turn off escape processing in GC mode */
Gc_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int newval = 1;
    if (argc > 1) newval = atoi(argv[1]);
    self->gc_mode = newval;
}

/*  Pic command */
Ps_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    struct link *l;
    int open = 0;
    if (argc > 1) open = atoi(argv[1]);
    if (!open) {
	/* closing */
	char cmd[400];
	char filename[400];
	if (self->filename != NULL) strcpy(filename, self->filename);
	else if (class_IsTypeByName(class_GetTypeName(self), "mmtext")) {
		    struct mmtext *doc = (struct mmtext *) self;
		    mmtext_GetFilename(doc, filename);
	} else filename[0] = '\0';
	sprintf(cmd, "$tail <%s +%d|head -%d|pic|troff|preview", filename,
		self->picBegin,	t->t->LineNumber - self->picBegin+1);	/* include .PE */
	link_SetLink(self->picButton, cmd);
	return;
    }
    /* opening */
    l = link_New();
    link_SetText(l, "Click here to see PIC figure");
    self->picBegin = t->t->LineNumber -	1;  /* include the .PS */
    self->picButton= l;
    text_AddView(self->text, self->pos++, link_ViewName(l), l);
}

/*  Hypertext link buttons */
Bu_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    struct link * l;
    if (argc < 3) return;
    l = link_New();
    link_SetText(l, argv[1]);
    link_SetLink(l, argv[2]);
    text_AddView(self->text, self->pos++, link_ViewName(l), l);
}

#ifdef TROFF_TAGS_ENV
/*  Tag command -- definition */
Tag_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    char *tag;
    int count;
    /* definition -- make table entry */
    /* fix up references later */
    /* argv[1] is "G", because the command is actually .TAG */
    if (argc < 3) return;
    tag = malloc(strlen(argv[2])+1);
    if (tag == NULL) return;
    strcpy(tag, argv[2]);
    if (self->tag_count >= MAX_TAG) return;
    count = self->tag_count++;
    self->tags[count].tag = tag;
    self->tags[count].def = TRUE;
    self->tags[count].pos = self->pos;
}

/*  Tag reference */
Tag_ref(self,t, sym)
struct rofftext *self;
Trickle t;
char *sym;
{
    struct link * l;
    char *tag;
    int count;
    char filename[200];
    char taglabel[100];
    /* tag being referred to -- put in a button */
    /* need to do SetPos to put in destination */
    tag = malloc(strlen(sym)+1);
    if (tag == NULL) return;
    strcpy(tag, sym);
    if (self->tag_count >= MAX_TAG) return;
    if (self->filename != NULL) strcpy(filename, self->filename);
    else if (class_IsTypeByName(class_GetTypeName(self), "mmtext")) {
	struct mmtext *doc = (struct mmtext *) self;
	mmtext_GetFilename(doc, filename);
    } else strcpy(filename, t->t->filename);
    count = self->tag_count++;
    self->tags[count].tag = tag;
    l = link_New();
    self->tags[count].def = FALSE;
    self->tags[count].l = l;
    sprintf(taglabel, " [-->>%s] ", tag);
    link_SetText(l, taglabel);
    link_SetLink(l, filename);
    text_AddView(self->text, self->pos++, link_ViewName(l), l);
}

/*
 *  Fix up tags
 */
Tag_fixup(self)
struct rofftext *self; {
    int i;
    for (i = 0; i < self->tag_count; i++) {
	if (! self->tags[i].def) {
	    /* search for the definition */
	    int j;
	    for (j = 0; j < self->tag_count; j++) {
		if (self->tags[j].def && strcmp(self->tags[j].tag, self->tags[i].tag) == 0) {
		    /* found definition matching the reference */
		    /* j points to the def, i to the ref */
/*		    link_SetPos(self->tags[i].l, self->tags[j].pos); */
		}
	    }
	}
    }
}
#endif /* TROFF_TAGS_ENV */	

/* print macro -- for debugging */
PM_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    char *m;
    printf("Macro named (%s) ----->\n",argv[1]);
    m = hash_Lookup(self->Macros,argv[1]);
    if (m)
        printf("%s",m);
    else
        printf("(Could not find %s)\n",argv[1]);
    printf("\n--------------End\n");
}


PA_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    printf("::::::::::::::::Printing all macros:::::::::::::::::\n");
    hash_Debug((self->Macros));
    printf("::::::::::::::::DONE::::::::::::::::\n");
}


/* title command -- special syntax */

tl_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    register int c,style;
    char *str,*string1=NULL,*string2=NULL,*string3=NULL,*end=NULL;
    int tmp = self->CurrentDiversion->OutputDone;
    static BUF b = NULL;

    b = NewBuf();
    ClearBuf(b);
    c = get(self,t);
    /* read leading whitespace */
    while((c==' ')||(c=='\t'))
        c = get(self,t);
    /* read stuff into string */
    while((c != EOF) && (c != '\n')) {
        Add2Buf(b,c);
        c = get(self,t);
    }
    Add2Buf(b,'\0');
    str = Buf2Str(b);

    style = BeginStyle(self,"majorheading");
    /* now get three parts */
    string1 = str+1;
    string2 = index(string1,*str);
    if (string2) {
        *string2++ = '\0';
        string3 = index(string2,*str);
        if (string3) {
            *string3++ = '\0';
            end = index(string3,*str);
            if (end)
                *end = '\0';
        }
    }
    if (string2) {
        while(*string1 != '\0') {
            if ((c = *string1++) != '%')
                put(self,c);
        }
        put(self,' '); put(self,' ');
        put(self,' '); put(self,' ');
    }
    if (string3) {
        while(*string2 != '\0') {
            if ((c = *string2++) != '%')
                put(self,c);
        }
        put(self,' '); put(self,' ');
        put(self,' '); put(self,' ');
    }
    if (end) {
        while(*string3 != '\0') {
            if ((c = *string3++) != '%')
                put(self,c);
        }
        put(self,'\n');
    }
    EndStyle(self,style);
    FreeBuf(b);
    self->CurrentDiversion->OutputDone = tmp; /* don't screw up breaks */

}

SortTraps(trap1,trap2)
struct trap *trap1,*trap2;
{
    if (trap1->loc > trap2->loc)
        return 1;
    else if (trap1->loc < trap2->loc)
        return -1;
    else return 0;
}

/* set a trap -- beginning or end of page */
wh_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int result;
    boolean absolute,relative;

    /* check location for trap */
    EvalString(self,argv[1],&result,scale_u,&absolute,&relative);
    if (!relative && !absolute && (result == 0)) {
        DEBUG(1, (stderr,"Setting beginning trap\n"));
        self->v_Trap = StrDup(argv[2]);
    }
    /* foo */
    if (relative && (result < 0)) {
        struct trap *trap = (struct trap *)malloc(sizeof(struct trap));
        trap->loc = result;
        trap->macro = StrDup(argv[2]);
        glist_InsertSorted(self->EndMacros,trap,SortTraps);
    }

}

/* space, the final frontier (ugh) */
sp_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    if (self->CurrentDiversion->NoSpaceMode) {
        DEBUG(1, (stderr,"Space: no space mode is on...\n"));
        return;
    }
    else
        sv_cmd(self,t,br,argc,argv);
}

sv_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int result;
    int lines = 1;

    /* do a break */
    if (br)
        DoBreak(self);

    if (argc > 1) {
        EvalString(self,argv[1],&result,scale_v,NULL,NULL);
        if (result > 0) {
            lines = (int)((result / (self->ScaleFactor[(int)scale_v])) + (0.5));
        }
        else
            lines = 0;
    }
    DEBUG(1, (stderr,"SPACE: spacing %d from a result of %g\n",lines,result));
    if (lines > 0) {
        while(lines-- > 0)
            put(self,'\n');
        self->CurrentDiversion->OutputDone = 0;
    }
}


it_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int result;

    if (argc<3) {
        if (self->CurrentEnviron->InputTrapCmd)
            free(self->CurrentEnviron->InputTrapCmd);
        self->CurrentEnviron->InputTrapCmd = NULL;
        self->CurrentEnviron->NextInputTrap = 0;
    }
    else {
        self->CurrentEnviron->InputTrapCmd = StrDup(argv[2]);
        EvalString(self,argv[1], &result, scale_u,NULL,NULL);
        self->CurrentEnviron->NextInputTrap = result + self->v_InputLineCount;
    }
}

/* set global font */

ft_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    char *font;
    font = "P";
    if (argc>1)font=argv[1];
    if (self->v_DiversionLevel>0) { /* yuck-o, handle fonts in diversions */
        int tmp = self->CurrentDiversion->OutputDone;
        put(self,'\\');
        put(self,'f');
	put(self,*font);
        self->CurrentDiversion->OutputDone = tmp;
        return;
    }
    if (isdigit(font[0])) {
        int digit = font[0]-'1';
        if ((digit >= 0) && (digit <= 5))
            font = self->Fonts[digit];
        else {
            DEBUG(1, (stderr,"********WARNING: bogus font (%s)********\n)",font));
            font = self->Fonts[0];
        }
    }
    else
        switch(font[0]) {
            case 'B': font = "bold";
                break;
            case 'I': font = "italic";
                break;
            case 'S': font = "special";
                break;
            case 'P': font = self->CurrentEnviron->prevFont;
                break;
	    case 'G':
            case 'L': font = "typewriter";
                break;
            default: font = "roman";
                break;

        }
    self->CurrentEnviron->prevFont = self->CurrentEnviron->font;
    self->CurrentEnviron->font = font;
    self->CurrentEnviron->fontStyle = ChangeStyle(self,self->CurrentEnviron->fontStyle,font);

}

/* set global indent */

in_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    /* this is bogus for now */
    int result;
    boolean relative;
    int temp;

    if (br)
	DoBreak(self);

    temp = self->CurrentEnviron->indent;

    if (argc>1) {
        DEBUG(1, (stderr,"Indenting (%s)\n",argv[1]));

        EvalString(self,argv[1],&result,scale_m,NULL,&relative);
        if (relative)
            self->CurrentEnviron->indent += result;
        else
            self->CurrentEnviron->indent = result;
    }
    else {
        self->CurrentEnviron->indent = self->CurrentEnviron->prevIndent;
    }
    self->CurrentEnviron->prevIndent = temp;
    self->CurrentEnviron->tempIndent = self->CurrentEnviron->indent;

    if (self->CurrentEnviron->indent != temp)
        SetIndent(self,self->CurrentEnviron->indent);
}

/* temporary indent */
ti_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    /* this is bogus for now */
    int result;
    boolean absolute,relative;

    if (br)
        DoBreak(self);

    if (argc>1) {
        DEBUG(1, (stderr,"Temp. indenting (%s)\n",argv[1]));

        EvalString(self,argv[1],&result,scale_m,&absolute,&relative);
        if (relative)
            self->CurrentEnviron->tempIndent = self->CurrentEnviron->indent + result;
        else
            self->CurrentEnviron->tempIndent = result;
        if (self->CurrentEnviron->tempIndent < 0)
            self->CurrentEnviron->tempIndent = 0;
        if (self->CurrentEnviron->tempIndent != self->CurrentEnviron->indent)
            SetTempIndent(self,self->CurrentEnviron->tempIndent);
    }

}


/* no-fill mode */
nf_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    if (br)
        DoBreak(self);
    self->CurrentEnviron->fill = FALSE;
}

/* fill mode */
fi_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    if (br)
        DoBreak(self);
    self->CurrentEnviron->fill = TRUE;
}


/* set no-space mode */
ns_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    DEBUG(1, (stderr,"===Turning on no-space mode===\n"));
    self->CurrentDiversion->NoSpaceMode = TRUE;
}


/* restore spacing */
rs_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    DEBUG(1, (stderr,"=Resetting Space Mode=\n"));
    self->CurrentDiversion->NoSpaceMode = FALSE;
}


/* center text */
ce_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    if (br)
        DoBreak(self);
}

/* center text */
Ce_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int result;
    static int styleID = 0;

    if (br)
        DoBreak(self);
    DEBUG(1, (stderr, "Center command argc (%d) \n", argc));
    if (argc > 1) {
	EvalString(self,argv[1],&result,scale_u,NULL,NULL);
	if (result > 0) {
	    styleID = BeginStyle(self, "center");
	    return;
	}
    }
    if (styleID) EndStyle(self, styleID);
    styleID = 0;
}

/* ignore input */
ig_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    char *end = StrDup(argv[1]);
    static BUF b = NULL;

    b = NewBuf();

    ClearBuf(b);

    self->v_CopyMode = TRUE;
    self->CurrentDiversion->SnarfOutput = b;

    Scan(self,t,((argc<2)?".":end));

    self->CurrentDiversion->SnarfOutput = NULL;
    self->v_CopyMode = FALSE;
    /* ignore completely! */
    FreeBuf(b);
    free(end);
}


/* translate characters  - special syntax */
tr_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    register int c;
    unsigned char source,new;
    char temp[3],name[3];

    /* read leading spaces */

    self->v_RawMode = TRUE;
    do {
        c = get(self,t);
    } while ((c==' ')||(c == '\t'));

    /* now translate, checking for special chars */

    while( (c != '\n') && (c != EOF)) {
        if (c == '\\') { /* check for special */
            c = get(self,t);
            if (c == '(') {
                name[0] = get(self,t);
                name[1] = get(self,t);
                name[2] = '\0';
                DEBUG(1, (stderr,"tr: found special char %s\n",name));
                /* change special char */
            }
            else {
                temp[0] = '\\';
                temp[1] = c;
                temp[2] = '\0';
                tpush(self,t,NULL,NULL,temp,FALSE,NULL,NULL);
                self->v_RawMode = FALSE;
                c = get(self,t);
                name[0] = '\0';
                DEBUG(1, (stderr,"tr: found special char %s, which maps to %c\n",temp,c));
            }
        }
        else
            name[0] = '\0';
        source = c;
        self->v_RawMode = FALSE;

        if (c == '\n') /* in case a translated character was null */
            break;

        /* should really handle special chars here, too */
        new = c = get(self,t);
        if (new == '\n')
            new = ' ';
        if (name[0] == '\0') {
            DEBUG(1, (stderr,"tr: translating %c to %c\n",source,new));
            self->Berliz[source] = new;
        }
        else {
            temp[0] = new;
            temp[1] = '\0';
            /* we won't worry about keeping track of mallocs here */
            hash_Store(self->SpecialChars,name,StrDup(temp));
            DEBUG(1, (stderr,"tr: renaming %s to %s\n",name,temp));
        }
        self->v_RawMode = TRUE;
	if (c != '\n') c = get(self,t);
    }
    self->v_RawMode = FALSE;
}

/* switch environment */
ev_cmd(self,t,br,argc,argv)
struct rofftext *self;
Trickle t;
boolean br;
int argc;
char *argv[];
{
    int result;
    int i;

    if (argc > 1) {
        EvalString(self,argv[1],&result,scale_u,NULL,NULL);
        i = result;
        if ((i >= 0) && (i <= 2))
            PushEnviron(self,i);
    }
    else
        PopEnviron(self);
}
