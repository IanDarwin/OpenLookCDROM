\begindata{text,537620288}
\textdsversion{12}
\template{default}
\define{italic
menu:[Font,Italic]
attr:[FontFace Italic Int Set]}
\define{bold
menu:[Font,Bold]
attr:[FontFace Bold Int Set]}
\define{chapter
menu:[Title,Chapter]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]
attr:[FontSize PreviousFontSize Point 4]}
\define{section
menu:[Title,Section]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]
attr:[FontSize PreviousFontSize Point 2]}
\define{subsection
menu:[Title,Subsection]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]}
\define{paragraph
menu:[Title,Paragraph]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Italic Int Set]}
\define{bigger
menu:[Font,Bigger]
attr:[FontSize PreviousFontSize Point 2]}
\define{indent
menu:[Region,Indent]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[RightMargin RightMargin Inch 32768]}
\define{typewriter
menu:[Font,Typewriter]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]}
\define{display
menu:[Region,Display]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[RightMargin RightMargin Inch 32768]
attr:[Justification LeftJustified Point 0]}
\define{example
menu:[Region,Example]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Justification LeftJustified Point 0]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]}
\define{description
menu:[Region,Description]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Indent LeftEdge Inch -32768]}
\define{quotation
menu:[Region,Quotation]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[RightMargin RightMargin Inch 32768]
attr:[FontFace Italic Int Set]}
\define{subscript
menu:[Font~1,Subscript~31]
attr:[Script PreviousScriptMovement Point 2]
attr:[FontSize PreviousFontSize Point -2]}
\define{superscript
menu:[Font~1,Superscript~30]
attr:[Script PreviousScriptMovement Point -6]
attr:[FontSize PreviousFontSize Point -2]}
\define{smaller
menu:[Font,Smaller]
attr:[FontSize PreviousFontSize Point -2]}
\define{heading
menu:[Title,Heading]
attr:[LeftMargin LeftMargin Inch -13107]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]}
\define{majorheading
menu:[Title,MajorHeading]
attr:[Justification Centered Point 0]
attr:[FontSize PreviousFontSize Point 4]}
\define{formatnote
menu:[Region,FormatNote]
attr:[Flags PassThru Int Set]}
\define{subheading
menu:[Title,Subheading]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]}
\define{center
menu:[Justify,Center]
attr:[Justification Centered Point 0]}
\define{flushleft
menu:[Justify,FlushLeft]
attr:[Justification LeftJustified Point 0]}
\define{flushright
menu:[Justify,FlushRight]
attr:[Justification RightJustified Point 0]}
\define{leftindent
menu:[Region,LeftIndent]
attr:[LeftMargin LeftMargin Inch 32768]}
\majorheading{C Panel and Menus Handling Package

}
\center{Author - Larry K. Raper

Menu Code by Mark Chance

Unix Support by Mike Sclafani

Version 2.0

(C) Copyright IBM Corporation 1986, 1987, 1988

Program Property of IBM

\smaller{\smaller{$Disclaimer: 

Permission to use, copy, modify, and distribute this software and its 

documentation for any purpose is hereby granted without fee, 

provided that the above copyright notice appear in all copies and that 

both that copyright notice, this permission notice, and the following 

disclaimer appear in supporting documentation, and that the names of 

IBM, Carnegie Mellon University, and other copyright holders, not be 

used in advertising or publicity pertaining to distribution of the software 

without specific, written prior permission.



IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 

DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 

ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 

SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 

BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 

DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 

WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 

ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 

OF THIS SOFTWARE.

 $}}}

\heading{Abstract

}
This package supports monochrome, color, and B/W displays via the standard PC 
BIOS Int 0x10 Video Interface.  Also supported are terminals with cursor 
movement capability via the curses/termcap interface on Unix systems.


Provides high level definition of input and output screens for easy user 
manipulation.  Also provides programmer-friendly mechanisms for defining 
hierarchic menus in the style of Lotus 1-2-3.


The normal sequence of the panel handler calls is


1.  InitPanels		(once)

2.  SaveVideoEnv		(once)

3.  DrawPanel		(as needed)

4.  GetKey		(once)

5.  RestoreVideoEnv	(once)


The normal sequence of the menu handler calls is


1.  InitMenus		(once)

2.  SetBaseMenu		(once)

3.  MenuInput		(as needed)


The keytab provided to GetKey is presumed to contain an exit key.


\heading{General Panel Data Structures}


There are several useful definitions provided in the panel input/output 
package.  To use these your program must #include "panel.h".


\subheading{PANEL}


This is the fundamental output structure.


struct \{

    char	prow, pcol,	Starting row, column of panel.  (0,0) is upper left.

	pattr,		Panel attribute from list of field attribute values

	plen;		Number of characters for input.  Zero for an output panel.

    char  *pdata;		Panel text data

    \};


A panel array ends in (0,0,0,0,NULL).


Example:

\example{
PANEL login_boilerplate2[] = \{

    19,  3, RVIDEO, 0, "F1",

    19,  6, NORMAL, 0, "Login",

    19, 13, RVIDEO, 0, "F2",

    19, 16, NORMAL, 0, "Logout",

    19, 27, RVIDEO, 0, "PgDn",

    19, 32, NORMAL, 0, "Display other sessions",

    19, 60, RVIDEO, 0, "Esc",

    19, 64, NORMAL, 0, "Return to DOS",

     0, 0, 0, 0, NULL

     \};


PANEL PCSERVER_lines[] = \{

    14,  3, NORMAL, 0, "Root Directory ",

    16,  8, NORMAL, 0, "PC Drive  ",

    0, 0, 0, 0, NULL

    \};


PANEL *PanelArray[] = \{

    login_boilerplate1,

    login_boilerplate2,

    PCSERVER_lines,

    No_PCSERVER_lines,

    login_data,

    NULL\};}


\subheading{FIELD

}
A field is the fundamental input structure.  Each field consists of one or 
more extents for input and one prompt that will be shown when the field has 
the input focus.


struct field \{

    struct field *freturn;		Next field on 'return' key

    struct field *fup;			                    'up'

    struct field *fdown;		                    'down'

    struct field *fleft;		                    'left'

    struct field *fright;		                    'right'

    XTENT *flocation;		Field location ending in a zero-triplet 

    unsigned char fattr;		Video attribute of field

    char   ftype;			Type of data in field

    char  *fdata;			Data to display in field

    PRMPT *fprompt;		Prompt to show when in field

    void (*fmodifyhandler)();		Function to call when field is modified

    void (*fentryhandler)();		Function to call on entry

    void (*fexithandler)();		Function to call on exit

    \};


\subheading{XTENT}


An extent is one segment of a field.  Generally extents do not cross lines.


struct \{

    unsigned char xrow, xcol, xlen;	Row, column, and length vector

    \};


\subheading{PRMPT}


A prompt is the text shown at the bottom of the screen when the user moves the 
cursor into a new field.


typedef struct \{

    char plen;		Prompt length

    char *pdata;		Prompt text

    \} PRMPT;


Example:


\example{PRMPT toprompt = \{0, "Enter the names of people to send the message 
to" \};

XTENT to_xtents[] = \{ 1, 4, TOCC_FIELD_LEN, 2, 4, TOCC_FIELD_LEN, 3, 4, 
TOCC_FIELD_LEN, 0, 0, 0 \};

char to_data[1+TOCC_FIELD_LEN*3];


PRMPT subprompt =\{0, "Enter the subject of your message" \};

XTENT subj_xtents[] = \{ 4, 9, SUBJ_FIELD_LEN, 0, 0, 0 \};

char subj_data[1+SUBJ_FIELD_LEN];


PRMPT ccprompt = \{0, "Enter the names of people to receive copies of your 
message" \};

XTENT cc_xtents[] = \{ 5, 4, TOCC_FIELD_LEN, 6, 4, TOCC_FIELD_LEN, 7, 4, 
TOCC_FIELD_LEN, 0, 0, 0 \};

char cc_data[1+ TOCC_FIELD_LEN*3];


PRMPT boprompt = \{0, "Enter the text of your message" \};


FIELD entry_fields[] = \{

/\italic{*0:To:*/} &entry_fields[1], &entry_fields[3], &entry_fields[1],

    &entry_fields[1], &entry_fields[1], to_xtents, NORMAL,

    FTYPE_ASC, to_data, &toprompt, SetSavethepage, (int (*)()) NULL, (int 
(*)())NULL,

/\italic{*1:Su:*/} &entry_fields[2], &entry_fields[0], &entry_fields[2],

    &entry_fields[2], &entry_fields[2], subj_xtents, NORMAL,

    FTYPE_ASC, subj_data, &subprompt, SetSavethepage, (int (*)()) NULL, (int 
(*)())NULL,

/\italic{*2:CC:*/} &entry_fields[3], &entry_fields[1], &entry_fields[3],

    &entry_fields[3], &entry_fields[3], cc_xtents, NORMAL,

    FTYPE_ASC, cc_data, &ccprompt, SetSavethepage, (int (*)()) NULL, (int 
(*)())NULL,

/\italic{*3:bod*/} &entry_fields[0], &entry_fields[2], &entry_fields[0],

    &entry_fields[0], &entry_fields[0], (XTENT *) NULL, NORMAL,

    FTYPE_SCR, NIL, &boprompt, SetSavethepage, (int (*)()) NULL, 
EntryPageDnKey,

    NULL_FIELD

    \};


FIELD *FieldArray[] = \{

     entry_fields,

     (FIELD *) NULL,

     \};}


\subheading{Field attribute values}


NORMAL	Normal on all displays

HILITE		Hilite on all displays

RVIDEO		Reverse Video

BLINKH		Blink + HILITE

REGDIF		RVIDEO unless B/W adapter

NORMCE	Same as NORMAL, unless color

NORMCI	Same as NORMAL, unless color

HILITC		Same as HILITE, unless color

RVIDHI		Reverse Video + HILITE

WHTBLU	While on blue, or normal

INVISI		Invisible

RINVIS		Reverse Video & invisible data

UNDERN	Normal underline

UNDERH	Hilited underline


\subheading{Field type values}


FTYPE_ASC	character string

FTYPE_INT	integer

FTYPE_YN	yes or no

FTYPE_SEL	cursor selection field

FTYPE_WRD	word (without blanks)

FTYPE_SCR	automatic scroll field


\subheading{KEYTAB}


This structure allows you to build tables which define how to treat special 
keys.  The key codes are defined in keycodes.h which contains definitions for 
both the PC and terminals.


struct \{

    int scan_code;

    FIELD *(*keyhandler)();

    \};


Example

\example{
KEYTAB login_keys[] = \{

    KEYCODE_F1,  NewLogin,			/* F1 	*/

    KEYCODE_F2,  NewLogout,		/* F2 	*/

    KEYCODE_PAGE_DOWN,  NewPriorSession,	/* PgDn */

    KEYCODE_PAGE_UP,  NewNextSession,	/* PgUp */

    KEYCODE_ESCAPE,  EscapeKey,		/* Esc  */

    0, 0\};


FIELD *EscapeKey (curfield)

FIELD *curfield;

    \{

    RestoreVideoEnv (&vp, PANEL_NOCLEAR, 22);

    longjmp (endpanel1, -1);

    \}

}

\subheading{VIDEOPARMS}


This structure holds the state of the system's video parameters.  Generally 
you will not need to change these.


struct \{

    unsigned char palette;

    unsigned char page;

    unsigned char attr;

    unsigned char mode;

    unsigned char columns;

    \};


Example


\example{VIDEOPARMS	vp;

SaveVideoEnv(&vp, 1);

...

RestoreVideoEnv( &vp, PANEL_CLEAR, 0);}



\heading{Menu Creation Data Structures (lmenus.h)}


The package provides data and constant definitions for the 1-2-3 style menus 
enhancements.  Your program must #include "lmenus.h".


\subheading{MKEYTAB

}
This structure is just like KEYTAB except that the handling functions return 
integers instead of field pointers.


struct \{

    int scan_code;		Key code

    int (*keyhandler)();	Handling function

    \} MKEYTAB;


\subheading{MENU_OPTS

}
This structure defines one menu option.  It consists of the one word 
description and a prompt to be displayed when the option is highlighted.


struct \{

    char *Option;		Option word

    PRMPT prompt;	Description of option

    \} MENU_OPTS;


\subheading{MENU_TREE

}
This structure provides the definition of the menu hierarchy.  The first 
element is an index into an array of menu options, the second is a pointer to 
a sub-menu.  (Currently the nesting has only been tested to a level of 3 deep.


struct \{

    int this;

    MENU_TREE *submenu;

    \} ;


Example:


\example{MENU_OPTS vui_menus[] = \{

#define OPT_D_READ 0

"Read", \{0, "Display the list of messages in the highlighted folder"\},

#define OPT_D_ADD 1

"Create", \{0, "Create a new folder"\},

.......

NIL, \{0, NIL\},

\}


MENU_TREE bb_menu[] = \{

OPT_D_READ, (MENU_TREE *)NIL,

OPT_S_POST, (MENU_TREE *)NIL,

OPT_D_INFO, d_info_menu,

OPT_D_SUBSCR, d_subscr_menu,

NULL_MENU

\};


MENU_TREE d_info_menu[] = \{

OPT_D_BRIEF, (MENU_TREE *)NIL,

OPT_D_LONG, (MENU_TREE *)NIL,

NULL_MENU

\};}


The OPT_* constants can be used in the calling program to process the various 
options that are entered by the user.

 	

\heading{Function Calls for Paneling}


To use the functions listed in this section you must compile and link in the 
panelp.c module.


\subheading{InitPanels

}This procedure fills in the length fields in panel and field arrays to agree 
with the corresponding constant strings.  Some minimal consistency checking is 
also performed.

\example{
InitPanels (p, f)

PANEL *p[];

FIELD *f[];}


\subheading{SaveVideoEnv

}This routine should be called early in your program to allow you to exit and 
return the machine to the state is was prior to running your program.

\example{
SaveVideoEnv (vp)

VIDEOPARMS *vp;}


\subheading{RestoreVideoEnv}

This routine should be called just before your program exits.

\example{
RestoreVideoEnv (vp, clear, cursor_row)

VIDEOPARMS *vp;

int clear;

int cursor_row;}


\subheading{ShowString

}This routine allows you to put a text string on the screen.  Generally there 
are higher-level routines that are preferred.  Attr is one of the Field 
Attribute Values.

\example{
ShowString (data, row, col, len, attr)

char *data;

unsigned char row, col, len, attr;}


\subheading{ShowCursor}

This routine redisplays the field currently with input focus.  (At one time 
this field was displayed in reverse video, but it was considered too harsh for 
data entry especially when red is used as the background color, plus it takes 
a while to paint the screen reverse over slow terminals.)

\example{
ShowCursor();}


\subheading{EraseCursor}

This routine removes highlighting from the current field.

\example{
EraseCursor();}


\subheading{ShowPrompt

}This routine displays a prompt.  Again, there are higher-level routines that 
are preferred.


\example{ShowPrompt(msg)

PRMPT *msg;

}
\subheading{DrawPanel}

This routine displays a panel on the screen.

\example{
DrawPanel (panel, inputfield, clear)

PANEL *panel;				An array of panels

FIELD *inputfield;			First field (may be NULL)

int clear;				Flag to indicate clearing the screen}


For the \italic{clear} argument use either of the two constants:

\leftindent{PANEL_CLEAR

PANEL_NOCLEAR}


\subheading{ShowMsg}

\example{
ShowMsg (msg)

PRMPT *msg;}


\subheading{ErrorBeep}

\example{
ErrorBeep();}


\subheading{ShowError}

\example{
ShowError (msg)

PRMPT *msg;}


\subheading{ClearError

}
\example{ClearError();

}
\subheading{ClearLine


}\example{ClearLine(line_no)

int line_no;

}
\subheading{ClearScreen


}\example{ClearScreen();

}
\subheading{RedrawScreen

}This routine is only be needed in a Unix terminal environment.


\example{RedrawScreen();

}
\subheading{UpdateScreen

}This routine is only be needed in a Unix terminal environment.

\subheading{
}\example{UpdateScreen();

}
\subheading{GetCurrentField}

\example{
FIELD *GetCurrentField ();}


\subheading{SetCurrentField}

This routine causes \italic{newfield} to become the current field.  The exit 
handler on the current field is called, its highlighting removed and the entry 
handler of the new field is called and it is highlighted.

\example{
SetCurrentField (newfield, prompt)

FIELD *newfield;

int prompt;}


\subheading{RestoreCurrentField}

This routine changes the current field, but does not update the screen or call 
any of the entry or exit handlers.

\example{
RestoreCurrentField (curfield)

FIELD *curfield;}


\subheading{GetKey}

Wait for the user to press a key, then process the value.

\example{
GetKey (keytable)

KEYTAB *keytable;}


\subheading{GetFromUser

}Use this routine to get an arbitrary string from the user.


\example{GetFromUser(row, col, length, default, field_type, return_str)

int row, col, length;

char *default, field_type, *return_str;

}
\subheading{GetNUser

}Use this routine to get a number within a specified range from the user.


\example{GetNUser(row, col, length, min, max, val_out)

int row, col, length, min, max, *val_out;

}
\subheading{GetSUser

}This routine gets a string from the user using the default locations for 
input and output.


\example{GetSUser(msg, prompt, default, str_out)

char *msg, *prompt, *default, *str_out;

}
\subheading{GetStringFromUser

}Another version with fewer options.


\example{GetStringFromUser(prompt, output, length)

char *prompt, *output;

int length;

}
\subheading{GetBooleanFromUser

}This routine asks the user a yes/no question and returns a boolean value.


\example{GetBooleanFromUser(msg, default)

char *msg, default;

}
\subheading{ConfirmUser

}This routine displays the message and waits for the user to press return.


\example{ConfirmUser(msg)

char *msg;

}
\subheading{KeyHit

}This routine returns a true value if a key has been pressed.


\example{KeyHit();

}
\subheading{KeyIn

}This routine waits for the user to press a key and returns that value.  When 
possible use GetKey.


\example{KeyIn();

}
\subheading{SetCursorPosn

}
\example{SetCursorPosn();

}
\subheading{CursorOff

}This routine turns the flashing cursor off or moves it out of the way on 
terminals that can't turn off the cursor.


\example{CursorOff();

}
\subheading{CursorOn

}This routine turns the cursor on and displays it at the current input 
location.


\example{CursorOn();


}\heading{Function Calls for Menu bars

}
To use the functions listed in this section you must compile and link in the 
lmenus.c module.


\subheading{InitMenus

}Initialize a set of menus by determining the length of each prompt and 
inserting that value into the data structure.  This lets you avoid having to 
count the length of character strings.


\example{InitMenus(menu_opt)

MENU_OPTS *menu_opt;

}
\subheading{SetBaseMenu

}This lets the panelling code know what array you are using.  The menu trees 
contain indices into this array.


\example{SetBaseMenu(menu)

MENU_OPTS *menu;

}
\subheading{ShowMenu

}Display a specified menu.


\example{ShowMenu(menu_tree, current_option)

MENU_TREE menu_tree[];

int current_option;

}
\subheading{EraseMenuCursor

}Turn off the highlighting of a menu option.  Typically you will call this 
routine before beginning to process a menu option.


\example{EraseMenuCursor();

}
\subheading{ShowMenuCursor

}Call this routine to indicate that processing has finished and you are ready 
for input again.


\example{ShowMenuCursor();

}
\subheading{MenuInput

}Display a menu, handle keys according to the table and return a value.  The 
previous option is stored in last_opt_p.


\example{MenuInput( menu_tree, keytable, last_opt_p)

MENU_TREE menu_tree[];

MKEYTAB *keytable;

int *last_opt_p;

}
\subheading{ClearPrompt

}Erase the menu prompt.


\example{ClearPrompt();

}

\heading{Program Sample}


\example{
ShowPanel ()

\{

    FIELD *first_field;


    InitPanels (PanelArray, FieldArray);

    SaveVideoEnv (&vp);


    if (*p_hostname == 0)	first_field = &login_fields[HOSIDX];

    else first_field = &login_fields[USEIDX];


    DrawPanel (login_boilerplate1, NULL, PANEL_CLEAR);

    DrawPanel (login_boilerplate2, NULL, PANEL_NOCLEAR);

    DrawPanel (login_data,  first_field, PANEL_NOCLEAR);

    int i;


    if ((i = setjmp (endpanel1)) != 0) \{  /* Note: because of setjmp	 */

       StripInput ();			/* no register variables may */

       ExitKey ();			    	/* be used in this function. */

    \}


    GetKey (login_keys);

\}


MainMenu ()

\{

    quit = FALSE;


    while (!quit) \{

        if (reshow) \{

            DrawPanel(intro, NIL, PANEL_CLEAR);

            ShowBar();

            reshow=FALSE;

        \}

        opt = MenuInput(main_menu, f1_keys, &last_opt);

        if (opt<0) \{ /* They pressed escape */

            if (!GetBooleanFromUser("Do you really want to quit now?", FALSE))

                return(0);

        \}

        switch (opt) \{

        case OPT_G_MAIL:

            .....

        \}

    \}

\}


}
\heading{Implementer's Notes:

}
Aside from the files mentioned above: (panel.h, panelp.c, lmenus.h, lmenus.c, 
keycodes.h) there is a file that contains screen manipulation routines that 
are machine specific, panelmach.c.

\enddata{text,537620288}
