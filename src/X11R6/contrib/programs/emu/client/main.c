#if !defined(lint) && !defined(__clipper__)
     static char *rcsid = "main.c,v 1.6 1994/06/03 10:58:38 me Exp";
#endif

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * This is the new "xterm" program. It uses the new term widget to do most
 * of its work. One file; not bad, eh? :-)
 *
 * Author: Jordan K. Hubbard
 * Date: July 26th, 1990.
 * Description: The canonical term widget client program. If nothing else,
 *		a good example of how to use the term and termCanvas widgets
 *		in a complete application.
 *
 * Revision History:
 *
 * main.c,v
 * Revision 1.6  1994/06/03  10:58:38  me
 * The scrollbar thumb wasn't updated properly when it wasn't mapped.
 * Added a facility to track the thumb size and position while the
 * bar isn't mapped.
 *
 * Revision 1.5  1994/06/02  20:08:36  me
 * Corrected the commandline args handling
 *
 * Revision 1.4  1994/06/02  13:03:27  me
 * Added version number in help text
 *
 * Revision 1.3  1994/06/02  07:47:11  me
 * Put the toolkit dependend code into separate files,
 * Cleaned up the geometry layout and management routine
 * Added support for dynamically addable and removable decorations
 *
 * Revision 1.2  1994/05/26  18:30:45  me
 * new copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:40  me
 * Initial import into CVS
 *
 * Revision 1.15  92/10/16  16:11:10  me
 * More fixes by Steve Crooks
 * 
 * Revision 1.14  92/05/16  06:24:17  jkh
 * Synchronization checkin.
 * 
 * Revision 1.13  92/02/26  11:34:31  me
 * Steve Crooks clix port and general code cleanup
 * 
 * Revision 1.12  91/08/17  10:17:09  jkh
 * Added a lot more command line flags.
 * 
 * Revision 1.11  91/04/22  11:26:35  me
 * Whatever Jordan did
 * 
 * Revision 1.10  90/11/20  17:36:45  jkh
 * Alpha.
 * 
 * Revision 1.1  90/07/26  02:57:12  jkh
 * Initial revision
 * 
 */

#include "client.h"
#include <signal.h>
#include "patchlevel.h"

/* We want these global */
Export TermWidget Term;
Export Widget SBar, MBar, Canvas;
Export Boolean Smapped = False, Mmapped = False;

/* Import this with second argument opaque */
Import void rparse(TermWidget, void *);
Import void tty_set_size(Widget w, int rows, int cols, int width, int height);


typedef struct _AppResources {
     Boolean menu_bar, scroll_bar, show_usage, is_console;
     String command, title, icon_name, term_type;
} AppResourcesRec, *AppResources;

static AppResourcesRec appResources;

static XrmOptionDescRec optionList[] = {
     {"-mbar",	"*menuBar",		XrmoptionNoArg,  "True"		},
     {"+mbar",	"*menuBar",		XrmoptionNoArg,  "False"	},
     {"-sbar",	"*scrollBar",		XrmoptionNoArg,  "True"		},
     {"+sbar",	"*scrollBar",		XrmoptionNoArg,	 "False"	},
     {"-C",	"*console",		XrmoptionNoArg,  "True"		},
     {"+C",	"*console",		XrmoptionNoArg,  "False"	},
     {"-help",	"*help",		XrmoptionNoArg,  "True"		},
     {"-e",	"*command",		XrmoptionSepArg, NULL		},
     {"-term",	"*termType",		XrmoptionSepArg, NULL		},
     {"-rs",	"*readSize",		XrmoptionSepArg, (caddr_t)0	},
     {"-ut",	"*utmpInhibit",		XrmoptionNoArg,	 "True"		},
     {"+ut",	"*utmpInhibit",		XrmoptionNoArg,	 "False"	},
     {"-ls",	"*loginShell",		XrmoptionNoArg,	 "True"		},
     {"+ls",	"*loginShell",		XrmoptionNoArg,	 "False"	},
     {"-T",	"*title",		XrmoptionSepArg, NULL		},
     {"-n",	"*iconName",		XrmoptionSepArg, NULL		},
     /* Screen specific resources */
     {"-ul",	"*screen.underlineWidth", XrmoptionSepArg, (caddr_t)1	},
     {"-cfg",	"*screen.cursorFg",	XrmoptionSepArg, NULL           },
     {"-cbg",	"*screen.cursorBg",	XrmoptionSepArg, NULL           },
     {"-ch",	"*screen.cursorHeight",	XrmoptionSepArg, (caddr_t)0	},
     {"-cw",	"*screen.cursorWidth",	XrmoptionSepArg, (caddr_t)0	},
     {"-cbl",	"*screen.cursorBlinking", XrmoptionNoArg, "True"	},
     {"+cbl",	"*screen.cursorBlinking", XrmoptionNoArg, "False"	},
     {"-cbival","*screen.blinkInterval", XrmoptionSepArg, (caddr_t)500	},
     {"-mclick","*screen.multiClickTime", XrmoptionSepArg, (caddr_t)300	},
     {"-tbival","*screen.textBlinkInterval", XrmoptionSepArg, (caddr_t)500, },
     {"-bfocus","*screen.blinkWOFocus",	XrmoptionNoArg, "False",	},
     {"+bfocus","*screen.blinkWOFocus",	XrmoptionNoArg, "True",	},
     {"-pcol",	"*screen.pointerColor",	XrmoptionSepArg, XtDefaultForeground },
     {"-pcur",	"*screen.pointerShape",	XrmoptionSepArg, NULL		},
     {"-wrap",	"*screen.wrapAround",	XrmoptionNoArg, "True",	},
     {"+wrap",	"*screen.wrapAround",	XrmoptionNoArg, "False",	},
     {"-insert","*screen.insertMode",	XrmoptionNoArg, "False",	},
     {"+insert","*screen.insertMode",	XrmoptionNoArg, "True",	},
     {"-bell",	"*screen.bellVolume",	XrmoptionSepArg, (caddr_t)100	},
     {"-tab",	"*screen.defTabWidth",	XrmoptionSepArg, (caddr_t)8	},
     {"-sl",	"*screen.saveLines",	XrmoptionSepArg, (caddr_t)0	},
     {"-fn",	"*screen.font",		XrmoptionSepArg,  NULL		},
     {"-fb",	"*screen.boldFont",	XrmoptionSepArg,  NULL		},
     {"-js",	"*screen.jumpScrollLines",XrmoptionSepArg,  (caddr_t)20	},
};

#define offset(field) XtOffset(struct _AppResources *, field)

/* Just the resources we're directly interested in */
static XtResource appResourceList[] = {
     { "menuBar", XtCBoolean, XtRBoolean, sizeof(Boolean),
	    offset(menu_bar), XtRString, "False",
     },
     { "scrollBar", "ScrollBar", XtRBoolean, sizeof(Boolean),
	    offset(scroll_bar), XtRString, "False",
     },
     { "console", XtCBoolean, XtRBoolean, sizeof(Boolean),
	    offset(is_console), XtRString, "False",
     },
     { "title",	"Title", XtRString, sizeof(String),
	    offset(title), XtRImmediate, NULL,
     },
     { "iconName", "IconName", XtRString, sizeof(String),
	    offset(icon_name), XtRImmediate, NULL,
     },
     { XpNcommand, XpCCommand, XtRString, sizeof(String),
	    offset(command), XtRImmediate, NULL,
     },
     { XpNtermType, XpCTermType, XtRString, sizeof(String),
	    offset(term_type), XtRImmediate, NULL,
     },
     { "help", XtCBoolean, XtRBoolean, sizeof(Boolean),
	    offset(show_usage), XtRImmediate, FALSE,
     },
};

/* Our process death handler. Just exits */
/*ARGSUSED*/
void
do_exit(Widget widget, XtPointer closure, XtPointer call_data)
{
     exit(0);
}

Local void
usage(int argc, char **argv)
{
     int i;
     
     static char *syntax[] = {
	  "-display dispname",	"Selects X server to use",
	  "-geometry geom",	"Specify window geometry in characters",
	  "-sl nlines",		"Indicates how many scroll lines to save",
	  "-rs nbytes",		"Sets the term widget's read size.",
	  "-e command",		"Command to execute on startup",
	  "-term termtype",	"Terminal type to emulate",
	  "-fn fontname",	"Normal font",
	  "-fb fontname",	"Bold font",
	  "-T title",		"Set window title string",
	  "-n icon_name",	"Set icon name for window",
	  "-name string",	"Set application name, icon and title strings",
	  "-/+rv",		"Turn on (off) reverse video",
	  "-help",		"Prints this output",
	  "-/+mbar",		"Selects (unselects) menu bar option",
	  "-/+sbar",		"Adds (removes) a vertical scrollbar",
	  "-/+C",		"Turns on (off) console mode",
	  "-/+ut",		"Add (don't add) utmp entry for session",
	  "-/+ls",		"Turn on (off) login shell handling",
	  "-ul ulwidth",	"Width of an underline",
	  "-cfg color",		"Cursor foreground color",
	  "-cbg color",		"Cursor background color",
	  "-cbival interval",	"Set cursor blink interval in milliseconds",
	  "-cw width",		"Set cursor width in pixels",
	  "-ch height",		"Set cursor height in pixels",
	  "-/+cbl",		"Turn off (on) cursor blink",
	  "-/+bfocus",		"Turn off (on) blinking cursor without focus",
	  "-mclick interval",	"Set multi-click interval in milliseconds",
	  "-tbival interval",	"Set text blink interval in milliseconds",
	  "-pcol color",	"Set pointer color",
	  "-pcur cursor",	"Set pointer cursor",
	  "-/+wrap",		"Enable (disable) text wrap",
	  "-/+insert",		"Disable (enable) insert mode",
	  "-bell volume",	"Set bell volume as a percentage",
	  "-tab n",		"Set tabstops to be every <n> characters",
	  NULL,			NULL,
     };
     static char **use = syntax;
     
     if (argc > 1) {
	  printf("%s: unknown arguments: ", argv[0]);
	  for (i = 1; i < argc ; i++)
	       printf("%s ", argv[i]);
     }
     printf("\n%s version %s takes the following arguments:\n",
	    argv[0], EMU_VERSION );
     while (*use != NULL) {
	  printf("%-30s - %s\n", use[0], use[1]);
	  use += 2;
     }
     exit(0);
}

Export int
main(int argc, char **argv)
{
     int i;
     Dimension w_inc, h_inc;
     Arg args[15];
     char *cp;
     Widget top;
     Dimension base_width = 0, base_height = 0, mbar_height = 0;

     top = XtInitialize(argv[0], "Emu", optionList, XtNumber(optionList),
			&argc, argv);
     
     XtGetApplicationResources(top, (XtPointer)&appResources,
			       appResourceList, XtNumber(appResourceList),
			       NULL, (Cardinal)0);
     
     if ((argc != 1 && !appResources.command) || (appResources.show_usage))
	  usage(argc, argv);

     i = 0;
     XtSetArg(args[i], XpNlayoutProc, do_layout);		i++;
     if (!appResources.command) {
	  /* If no command, select shell by default */
	  if ((cp = (char *)getenv("SHELL")) == NULL)
	       cp = XpNdefaultCommand;
	  XtSetArg(args[i], XpNcommand, cp);			i++;
     }
     else if (argc != 1) {
	  /* We have some arguments to the command */
	  XtSetArg(args[i], XpNcommandArgs, argv);		i++;
     }
     if (!appResources.term_type) {
	  /* If no term type, use argv[0] */
	  XtSetArg(args[i], XpNtermType, argv[0]);		i++;
     }
     XtSetArg(args[i], XtNborderWidth, 0);			i++;
     Term = (TermWidget)XtCreateManagedWidget("term", termWidgetClass,
					      top, args, i);
     XtAddCallback((Widget)Term, XpNprocessDeath, do_exit, NULL);

     i = 0;
     XtSetArg(args[i], XtNborderWidth, 0);			i++;
/*      XtSetArg(args[i], XtNborderColor, XtDefaultBackground);	i++; */
     XtSetArg(args[i], XpNoutput, rparse);			i++;
     XtSetArg(args[i], XpNsetSize, tty_set_size);		i++;
     XtSetArg(args[i], XpNcomBlock, XpTermComBlock(Term));	i++;
     XtSetArg(args[i], XpNadjustScrollBar, ScrollbarAdjust); i++;
     Canvas = XtCreateManagedWidget("screen", termCanvasWidgetClass,
				    (Widget)Term, args, i);

     /* If we want a scrollbar, now's the time to whap it up */
     SBar = ScrollbarCreate((Widget)Term);
     if (appResources.scroll_bar) {
	  Smapped = True;
	  XtManageChild(SBar);
     }

     /* Wake up the menus */
     XpEmuInitializeMenus((Widget)Term);

     MBar = XpEmuCreateMbar((Widget)Term);
     /* If necessary manage the mbar */
     if (appResources.menu_bar) {
	  Mmapped = True;
	  XtManageChild(MBar);
     } else {
	  /*
	   * To be sure, we unmanage it right here and now.
	   * The Motif app uses XmCreateMenuBar which is always
	   * managed
	   */
	  XtUnmanageChild(MBar);
     }

     /*
      * Get the cell width and height from the canvas and pass them
      * to the term widget, which will in turn automatically pass them
      * up to the shell.
      */
     i = 0;
     XtSetArg(args[i], XpNcellWidth, &w_inc);			i++;
     XtSetArg(args[i], XpNcellHeight, &h_inc);			i++;
     XtGetValues(Canvas, args, i);

     i = 0;
     XtSetArg(args[i], XtNwidthInc, w_inc);			i++;
     XtSetArg(args[i], XtNheightInc, h_inc);			i++;
     XtSetValues((Widget)Term, args, i);

     /* let the shell resize as needed */
     i = 0;
     XtSetArg(args[i], XtNallowShellResize, TRUE);		i++;
     XtSetArg(args[i], XtNinput, TRUE);				i++;
     if (appResources.title) {
	  XtSetArg(args[i], XtNtitle, appResources.title);	i++;
     }
     if (appResources.icon_name) {
	  XtSetArg(args[i], XtNiconName, appResources.icon_name); i++;
     }
     XtSetValues(top, args, i);

     /* Zark it up there */
     XtRealizeWidget(top);

     XtMainLoop();
     
     /* Keep GCC happy */
     return 0;
}
