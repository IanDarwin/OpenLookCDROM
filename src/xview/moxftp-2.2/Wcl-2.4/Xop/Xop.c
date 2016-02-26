#include <X11/Xop/COPY>
/*
* SCCS_data: @(#) Xop.c    1.2 92/06/10 06:15:18
*
* Xop - Open Look Public Utilities for Wcl - Xop.c
*
* This module contains registration routine, and convenience
* callbacks for the Open Look widget set.
*
* Module_history:
*                                                  
*   mm/dd/yy  initials  function  action
*   --------  --------  --------  ---------------------------------------------
*   06/19/90  D.Smyth   all	  create.
*   12/01/90  K.Gillies all       modified for OLIT support.
*   03/01/92  D.Smyth   all       update for Wcl2
*
* Design_notes:
*    
*******************************************************************************
*/
/*
*******************************************************************************
* Include_files.
*******************************************************************************
*/

#include <stdio.h>

#include <X11/Xop/XopOpenLook.h>
#include <X11/Wc/WcCreateP.h>
#include <X11/Xop/Xop.h>

/* Shorthand for Wcl registrations
*/
#define RCP( name, class ) WcRegisterClassPtr	( app, name, class    );
#define RCR( name, func )  WcRegisterConstructor( app, name, func     );
#define RCB( name, cb   )  WcRegisterCallback	( app, name, cb, NULL );

/* Get rid of those damn Xt version warnings
*/

static XtErrorMsgHandler OriginalHandler;

void XopErrorMsgHandler( name, type, class, defaultMsg, params, numParams )
    String	name, type, class, defaultMsg;
    String*	params;
    Cardinal*	numParams;
{
    if ( (char*)0 == WcStrStr( defaultMsg, "binary compiled for R3" ) )
	OriginalHandler( name, type, class, defaultMsg, params, numParams );
}

/* Replace the original Xt warning message handler with XopErrorMsgHandler
*/
void XopReplaceErrorHandler( app )
    XtAppContext app;
{
    if ( OriginalHandler != (XtErrorMsgHandler)0 )
	OriginalHandler = XtAppSetWarningMsgHandler( app, XopErrorMsgHandler );
}


/* This converter is included for the scrolling list viewHeight
 * resource.
 */
#ifdef NEED_CvtStringToCardinal
void CvtStringToCardinal(args, num_args, fromVal, toVal)
  XrmValuePtr args;
  Cardinal *num_args;
  XrmValuePtr fromVal;
  XrmValuePtr toVal;
{
  static unsigned int l;

  if (*num_args != 0)
    XtWarningMsg("wrongParameters", "cvtStringToCardinal","XtToolkitError",
      "String to Cardinal conversion needs no extra arguments",
      (String *)NULL, (Cardinal *)NULL);
  if (sscanf((char *)fromVal->addr, "%ud", &l) == 1) {
    (*toVal).size = sizeof(unsigned int);
    (*toVal).addr = (caddr_t)&l;
  } else {
    XtStringConversionWarning((char *) fromVal->addr, XtRCardinal);
  }
}
#endif

/* The following is included to support testing of scrolling lists
 * only if INCLUDEDEMOS is defined.
 */
#ifdef INCLUDEDEMOS

static String OlTestItems[] = {
	"test1", "test2", "test3", "test4", "a long test5",
	"test6", "test7", "test8", "a long test10",
};
#define NOLTESTITEMS	(sizeof(OlTestItems)/sizeof(String))

/* Private data for each list */
typedef struct OlSLDemoData {
  OlListToken current;
  Widget this;         /* This sl if needed */
} OlSLDemoData;


/* Constructor */
Widget OlScrollingListDemoCreate ( /* Widget w, 
			  String name, 
			  ArgList args, 
			  Cardinal num_args */ );
/* Demo callbacks */
static void OlScrollingListDemoSelectCB(/* Widget w, caddr_t client_data,
					caddr_t call_data */ );

static void OlScrollingListDemoVerifyCB( /* Widget w,
					    caddr_t client_data,
					    caddr_t call_data */ );
static void OlScrollingListDemoDeleteCB( /* Widget w, caddr_t client_data,
				    caddr_t call_data */ );
static void OlScrollingListDemoDeleteCB(/* Widget w, caddr_t client_data,
				    caddr_t call_data */ );


/* Global function pointers to list routines */
static OlListToken	(*SLAddItem)();
static void		(*SLDeleteItem)();
static void		(*SLEditClose)();
static void		(*SLEditOpen)();
static void		(*SLTouchItem)();
static void		(*SLUpdateView)();
static void		(*SLViewItem)();

static Widget OlScrollingListDataFetch(ref, name)
Widget ref;
char *name;
{
  Widget list;

  if (name == NULL) {
    fprintf(stderr, "List Demo callbacks need a list argument.\n");
    return NULL;
  }
  list =  WcFullNameToWidget(ref, name);
  if (list == NULL) {
    fprintf(stderr, "List Demo Edit could not find widget: `%s'\n", name);
    return NULL;
  }
/*
  fprintf(stderr, "list: %d, ref: %d, %s\n", list, ref, name);
*/
  return list;
}

/* Function OlScrollingListDemoCreate
 * Creates a scrolling list, adds test data.
 */
Widget OlScrollingListDemoCreate(parent, name, inargs, nargs)
  Widget parent;
  String name;
  Arg *inargs;
  Cardinal nargs;
{
  Widget list;
  OlListItem item;
  OlSLDemoData *oldata;
  static int first = 1;          /* first time flag */
  int i;

  oldata = (OlSLDemoData *)XtMalloc(sizeof(OlSLDemoData));
 
  list = XtCreateWidget(name, scrollingListWidgetClass,
	parent, inargs, nargs);
  /* Fetch the list functions for global use. */
  if (first) {              /* Just do this the first time */
    XtVaGetValues(list,
		  XtNapplAddItem, &SLAddItem,
		  XtNapplDeleteItem, &SLDeleteItem,
		  XtNapplEditClose, &SLEditClose,
		  XtNapplEditOpen, &SLEditOpen,
		  XtNapplTouchItem, &SLTouchItem,
		  XtNapplUpdateView, &SLUpdateView,
		  XtNapplViewItem, &SLViewItem, NULL);
  }
  first = 0;

  oldata = (OlSLDemoData *)XtMalloc(sizeof(OlSLDemoData));
  /* Set up the demo list data */
  oldata->this = list;
  oldata->current = (OlListToken)0;
  XtVaSetValues(list, XtNuserData, (XtPointer)oldata, NULL);

  /* Pump in the test data */
  for (i = 0; i < NOLTESTITEMS; i++) {
    item.label_type = OL_STRING;
    item.label = OlTestItems[i];
    item.attr = 0;
    (void)(*SLAddItem)(list, 0, (OlListToken)0, item);
  }
  return list;
}

/* Used with the userMakeCurrent callback, changes the selected item. */
static void OlScrollingListDemoSelectCB(w, client_data, call_data)
  Widget w;
  XtPointer client_data;
  XtPointer call_data;
{
  OlListItem *item;
  OlListToken token;
  OlSLDemoData *oldata;

  token = (OlListToken)call_data;
  item = OlListItemPointer(token);

  /* This widget is an sl so just get the user data */
  XtVaGetValues(w, XtNuserData, (XtPointer *)&oldata, NULL);

  if (token == oldata->current) {
    fprintf(stderr, "selected: %s, again.\n", item->label);
  } else {
    fprintf(stderr, "selected: %s.\n", item->label);

    item->attr |= OL_LIST_ATTR_CURRENT;
    /* tell list of change */ 
    (*SLTouchItem)(w, token);
    /* make it visible  */
    (*SLViewItem)(w, token);
    if (oldata->current != 0) {
      item = OlListItemPointer(oldata->current);
      item->attr &= ~OL_LIST_ATTR_CURRENT;
      (*SLTouchItem)(w, oldata->current);
    }
    oldata->current = token;
  }
}

/* Used to check a new value when return has been hit */
static void OlScrollingListDemoVerifyCB(w, client_data, call_data)
  Widget w;
  XtPointer client_data;
  XtPointer call_data;
{
  OlSLDemoData *oldata;
  OlTextFieldVerify *verifyfield;
  Widget list;

  if((list = OlScrollingListDataFetch(w, (char *)client_data)) == NULL) {
    return;
  }
  XtVaGetValues(list, XtNuserData, (XtPointer *)&oldata, NULL);
    
  if (oldata == NULL) {
    fprintf(stderr, "Verify found no list data.\n");
  } else {
    verifyfield = (OlTextFieldVerify *)call_data;
    fprintf(stderr, "New data is: %s, but it will be ignored.\n", 
        verifyfield->string);
    (*SLTouchItem)(oldata->this, oldata->current);
    (*SLEditClose)(oldata->this);
  }
}

/* Used when an edit starts */
static void OlScrollingListDemoEditCB(w, client_data, call_data)
  Widget w;
  XtPointer client_data;
  XtPointer call_data;
{
  OlSLDemoData *oldata;
  Widget list; 

  if((list = OlScrollingListDataFetch(w, (char *)client_data)) == NULL) {
    return;
  }
  XtVaGetValues(list, XtNuserData, (XtPointer *)&oldata, NULL);

  if (oldata == NULL) {
    fprintf(stderr, "List Demo Edit found no list data.\n");
  } else {
    (*SLEditOpen)(oldata->this, FALSE, oldata->current);
  }
}

/* Delete an item from the list */
static void OlScrollingListDemoDeleteCB(w, client_data, call_data)
  Widget w;
  caddr_t client_data;
  caddr_t call_data;
{
  OlSLDemoData *oldata;
  OlListItem *item;
  OlListDelete *delList;
  Widget list;

 /* Check to see if this was a cut and ignore */
  delList = (OlListDelete *)call_data;
  if (delList != NULL) {
    fprintf(stderr, "The delete demo doesn't cut more than one.\n");
    return;
  }
  if((list = OlScrollingListDataFetch(w, (char *)client_data)) == NULL) {
    return;
  }
  XtVaGetValues(list, XtNuserData, (XtPointer *)&oldata, NULL);

  if (oldata == NULL) {
    fprintf(stderr, "Delete found no list data.\n");
  } else {
    item = OlListItemPointer(oldata->current);
    /* Unselect */
    item->attr &= ~OL_LIST_ATTR_CURRENT;
    (*SLTouchItem)(oldata->this, oldata->current);
    (*SLDeleteItem)(oldata->this, oldata->current);
    oldata->current = 0;
  }
}

static void registerListDemoCBs (app)
  XtAppContext app;
{
  RCB("OlScrollingListDemoSelectCB",	OlScrollingListDemoSelectCB)
  RCB("OlScrollingListDemoSelect",	OlScrollingListDemoSelectCB)
  RCB("OlScrollingListDemoEditCB",	OlScrollingListDemoEditCB)
  RCB("OlScrollingListDemoEdit",	OlScrollingListDemoEditCB)
  RCB("OlScrollingListDemoVerifyCB",	OlScrollingListDemoVerifyCB)
  RCB("OlScrollingListDemoVerify",	OlScrollingListDemoVerifyCB)
  RCB("OlScrollingListDemoDeleteCB",	OlScrollingListDemoDeleteCB)
  RCB("OlScrollingListDemoDelete",	OlScrollingListDemoDeleteCB)
}
#endif /* INCLUDEDEMOS */

void OriRegisterOpenLook ( app )
    XtAppContext app;
{
    XopRegisterOpenLook(app);
}

void XopRegisterAll ( app )
    XtAppContext app;
{
    XopRegisterOpenLook(app);
}

void XopRegisterOpenLook ( app )
    XtAppContext app;
{
    ONCE_PER_XtAppContext( app );

#ifdef NEED_CvtStringToCardinal
    XtAddConverter       (XtRString,
                          XtRCardinal,
                          CvtStringToCardinal,
                          (XtConvertArgList)NULL,
                          (Cardinal)0);
#endif

    XopReplaceErrorHandler( app );

#ifdef INCLUDEDEMOS
    registerListDemoCBs(app);

    /* -- register all OpenLook widget constructors */
    RCR("ScrollingListDemo", OlScrollingListDemoCreate);
#endif /* INCLUDEDEMOS */

    /* -- register Table widget classes */
    RCP("XpTable",		xpTableWidgetClass	)
    RCP("xpTableWidgetClass",	xpTableWidgetClass	)

    /* -- register all OpenLook widget classes */
    RCP("AbbrevMenu",			abbrevMenuButtonWidgetClass	);
    RCP("abbrevMenuButtonWidgetClass",	abbrevMenuButtonWidgetClass	); 
    RCP("BaseWindow",			baseWindowShellWidgetClass	);
    RCP("baseWindowShellWidgetClass",	baseWindowShellWidgetClass	);
    RCP("BulletinBoard",		bulletinBoardWidgetClass	);
    RCP("bulletinBoardWidgetClass",	bulletinBoardWidgetClass       	);
    RCP("Caption",			captionWidgetClass	        );
    RCP("captionWidgetClass",		captionWidgetClass	        );
    RCP("Checkbox",			checkBoxWidgetClass	        );
    RCP("checkBoxWidgetClass",		checkBoxWidgetClass     	);
    RCP("ControlArea",			controlAreaWidgetClass  	);
    RCP("controlAreaWidgetClass",	controlAreaWidgetClass  	);
    RCP("Exclusives",			exclusivesWidgetClass   	);
    RCP("exclusivesWidgetClass",	exclusivesWidgetClass	        );
    RCP("FlatCheckbox",			flatCheckBoxWidgetClass 	);
    RCP("flatCheckWidgetClass",		flatCheckBoxWidgetClass 	); 
    RCP("FlatExclusives",		flatExclusivesWidgetClass	);
    RCP("flatExclusivesWidgetClass",	flatExclusivesWidgetClass	);
    RCP("FlatNonexclusives",		flatNonexclusivesWidgetClass	);
    RCP("flatNonexclusivesWidgetClass",	flatNonexclusivesWidgetClass	);

    RCP("FooterPanel",			footerPanelWidgetClass  	);
    RCP("footerPanelWidgetClass",	footerPanelWidgetClass  	);
    RCP("Form",			        formWidgetClass         	);
    RCP("formWidgetClass",      	formWidgetClass         	);

    /* Menus */
    RCP("Menu",		                menuShellWidgetClass	);
    RCP("menuShellWidgetClass",	        menuShellWidgetClass	); 
    RCP("Menubutton",		        menuButtonWidgetClass	);
    RCP("menuButtonWidgetClass",	menuButtonWidgetClass	); 
    RCP("Menubuttongadget",	        menuButtonGadgetClass	);
    RCP("menuButtonGadgetClass",	menuButtonGadgetClass	); 

    RCP("Nonexclusives",		nonexclusivesWidgetClass	);
    RCP("nonexclusivesWidgetClass",     nonexclusivesWidgetClass	);
    RCP("Notice",		        noticeShellWidgetClass	);
    RCP("noticeShellWidgetClass",      	noticeShellWidgetClass	); 
    RCP("Oblongbutton",		        oblongButtonWidgetClass	);
    RCP("oblongButtonWidgetClass",      oblongButtonWidgetClass	);   
    RCP("Oblongbuttongadget",	        oblongButtonGadgetClass	);
    RCP("oblongButtonGadgetClass",      oblongButtonGadgetClass	);

    RCP("Popupwindow",		        popupWindowShellWidgetClass	);
    RCP("popupWindowShellWidgetClass", 	popupWindowShellWidgetClass	); 
 
    RCP("Rectbutton",			rectButtonWidgetClass	);
    RCP("rectButtonWidgetClass",       	rectButtonWidgetClass	);
    RCP("Scrollbar",			scrollbarWidgetClass	);
    RCP("scrollbarWidgetClass",		scrollbarWidgetClass	); 
    RCP("ScrollingList",                scrollingListWidgetClass );
    RCP("scrollingListWidgetClass",     scrollingListWidgetClass );
    RCP("ScrolledWindow",               scrolledWindowWidgetClass);
    RCP("scrolledWindowWidgetClass",    scrolledWindowWidgetClass);
    RCP("Slider",			sliderWidgetClass	);
    RCP("sliderWidgetClass",		sliderWidgetClass	);
    /* Text */
    RCP("Statictext",			staticTextWidgetClass	);
    RCP("staticTextWidgetClass",	staticTextWidgetClass	);
    RCP("TextEdit",                     textEditWidgetClass     );
    RCP("textEditWidgetClass",          textEditWidgetClass     );
    RCP("Textfield",                    textFieldWidgetClass    );
    RCP("textFieldWidgetClass",         textFieldWidgetClass    );

    RCP("Stub",				stubWidgetClass		);
    RCP("stubWidgetClass",		stubWidgetClass		);
}
