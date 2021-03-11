/* -*-C-*-
********************************************************************************
*
* File:         xldmem.h
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xldmem.h,v 2.6 1994/06/06 15:59:13 npm Exp $
* Description:  dynamic memory definitions
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:37 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Enterprise Integration
* Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
* Betz make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
* COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

/* small fixnum range */
#define SFIXMIN		(-128)
#define SFIXMAX		255
#define SFIXSIZE	384

/* character range */
#define CHARMIN		0
#define CHARMAX		255
#define CHARSIZE	256

/* new node access macros */
#define ntype(x)	((x)->n_type)

/* cons access macros */
#define car(x)		((x)->n_car)
#define cdr(x)		((x)->n_cdr)
#define rplaca(x,y)	((x)->n_car = (y))
#define rplacd(x,y)	((x)->n_cdr = (y))

/* symbol access macros */
#define getvalue(x)	 ((x)->n_vdata[0])
#define setvalue(x,v)	 ((x)->n_vdata[0] = (v))
#define getfunction(x)	 ((x)->n_vdata[1])
#define setfunction(x,v) ((x)->n_vdata[1] = (v))
#define getplist(x)	 ((x)->n_vdata[2])
#define setplist(x,v)	 ((x)->n_vdata[2] = (v))
#define getpname(x)	 ((x)->n_vdata[3])
#define setpname(x,v)	 ((x)->n_vdata[3] = (v))
#define SYMSIZE		4

/* closure access macros */
#define getname(x)	((x)->n_vdata[0])
#define setname(x,v)	((x)->n_vdata[0] = (v))
#define gettype(x)	((x)->n_vdata[1])
#define settype(x,v)	((x)->n_vdata[1] = (v))
#define getargs(x)	((x)->n_vdata[2])
#define setargs(x,v)	((x)->n_vdata[2] = (v))
#define getoargs(x)	((x)->n_vdata[3])
#define setoargs(x,v)	((x)->n_vdata[3] = (v))
#define getrest(x)	((x)->n_vdata[4])
#define setrest(x,v)	((x)->n_vdata[4] = (v))
#define getkargs(x)	((x)->n_vdata[5])
#define setkargs(x,v)	((x)->n_vdata[5] = (v))
#define getaargs(x)	((x)->n_vdata[6])
#define setaargs(x,v)	((x)->n_vdata[6] = (v))
#define getbody(x)	((x)->n_vdata[7])
#define setbody(x,v)	((x)->n_vdata[7] = (v))
#define getenvi(x)	((x)->n_vdata[8])
#define setenvi(x,v)	((x)->n_vdata[8] = (v))
#define getfenv(x)	((x)->n_vdata[9])
#define setfenv(x,v)	((x)->n_vdata[9] = (v))
#define getlambda(x)	((x)->n_vdata[10])
#define setlambda(x,v)	((x)->n_vdata[10] = (v))
#define CLOSIZE		11

/* vector access macros */
#define getsize(x)	((x)->n_vsize)
#define getelement(x,i) ((x)->n_vdata[i])
#define setelement(x,i,v) ((x)->n_vdata[i] = (v))

/* object access macros */
#define getclass(x)	((x)->n_vdata[0])
#define getivar(x,i)	((x)->n_vdata[i+1])
#define setivar(x,i,v)	((x)->n_vdata[i+1] = (v))

/* subr/fsubr access macros */
#define getsubr(x)	((x)->n_subr)
#define getoffset(x)	((x)->n_offset)

/* fixnum/flonum/char access macros */
#define getfixnum(x)	((x)->n_fixnum)
#define getflonum(x)	((x)->n_flonum)
#define getchcode(x)	((x)->n_chcode)

/* string access macros */
#define getstring(x)	((x)->n_string)
#define getslength(x)	((x)->n_strlen)
/* the following functions were TAA modifications */
#define getstringch(x,i) (((unsigned char FAR *)((x)->n_string))[i])
#define setstringch(x,i,v) ((x)->n_string[i] = (char)(v))

/* file stream access macros */
#define getfile(x)	((x)->n_fp)
#define setfile(x,v)	((x)->n_fp = (v))
#define getsavech(x)	((x)->n_savech)
#define setsavech(x,v)	((x)->n_savech = (v))

/* unnamed stream access macros */
#define gethead(x)	((x)->n_car)
#define sethead(x,v)	((x)->n_car = (v))
#define gettail(x)	((x)->n_cdr)
#define settail(x,v)	((x)->n_cdr = (v))

#ifdef WINTERP

/* XLTYPE_XT_RESOURCE access macros */
#define get_xtresource(x)	   ((x)->n_xtresource)

/* XLTYPE_Pixel access macros */
#define get_xpixel(x)		   ((x)->n_pixel)

/* XLTYPE_Pixmap access macros */
typedef struct _WINTERP_GIF_Color_Info_Node *WINTERP_GIF_COLOR_INFO;
#define get_pixmap(x)		   ((x)->n_pixmap)
#define get_pixmap_color_info(x)   ((x)->n_pixmap_color_info)

/* XLTYPE_XImage access macros */
#define get_ximage(x)		   ((x)->n_ximage)
#define set_ximage(x,v)		   ((x)->n_ximage = (v))

/* XLTYPE_XmString access macros */
#define get_xmstring(x)		   ((x)->n_xmstring)

/* XLTYPE_XEvent access macros */
#define get_xevent(x)		   ((x)->n_xevent)

/* XLTYPE_Window access macros */
#define get_window(x)		   ((x)->n_window)

/* XLTYPE_XtAccelerators access macros */
#define get_xtaccelerators(x)	      ((x)->n_xtaccelerators)

/* XLTYPE_XtTranslations access macros */
#define get_xttranslations(x)	      ((x)->n_xttranslations)

/*
 * XLTYPE_PIXMAP_REFOBJ:
 *
 * A "pixmap reference object" is created whenever a Widget is set to a Pixmap
 * created by WINTERP. Reference objects are created by doing
 * (send <widget_class> :new ...) or (send <widget> :set_values) and will be
 * freed via garbage collection when the associated widget is destroyed, or when
 * the associated widget's resource is set to a new Pixmap. Unless otherwise
 * referenced, the XLTYPE_PIXMAP associated with the reference object is freed
 * when no more references to the pixmap exist.
 * 
 * This is a ARRAY based type with the following C-accessible slots
 * 0. LVAL(XLTYPE_Pixmap)	get_pixref_pixmap(x)	-- GC must mark() this LVAL
 * 1. LVAL(XLTYPE_WIDGETOBJ)	get_pixref_widget(x)	-- GC must mark() this LVAL
 * 3. LVAL(SYMBOL)		get_pixref_resname(x)	-- GC must mark() this LVAL
 *
 * In order to satisfy JGC mods, note that XLTYPE_PIXMAP_REFOBJ is a pure ARRAY,
 * not a HYBRID_ARRAY. Therefore we need not define PIXMAP_REFOBJ_IDX_OF_FIRST_LVAL...
 */
#define PIXMAP_REFOBJ_SIZE 3
/* access macros */
#define get_pixref_pixmap(x)	   ((x)->n_vdata[0])
#define set_pixref_pixmap(x,v)	   ((x)->n_vdata[0] = (v))
#define get_pixref_widget(x)	   ((x)->n_vdata[1])
#define set_pixref_widget(x,v)	   ((x)->n_vdata[1] = (v))
#define get_pixref_resname(x)	   ((x)->n_vdata[2])
#define set_pixref_resname(x,v)	   ((x)->n_vdata[2] = (v))


/* 
 * XLTYPE_CALLBACKOBJ:
 *
 * A "callback object" is created everytime a widget callback is created; when the
 * callback is fired, the CALLBACKOBJ is passed on to callback proc as client_data.
 * All CALLBACKOBJs are protected from garbage collection by saving them in
 * v_savedobjs[]; CALLBACKOBJs are removed from v_savedobjs if the widget gets
 * destroyed, XT_REMOVE_CALLBACK is called, or if :SET_CALLBACK overrides the existing
 * callback for a particular callback-list resource name (XmRCallback).
 *
 * This is a HYBRID_ARRAY based type containing both direct pointers and LVALs
 * which must be marked by the garbage collector. It contains the following
 * C-accessible slots:
 * 0. char*			get_callback_name(x)	-- don't mark() this elt.
 * 1. XtCallbackProc		get_callback_proc(x)	-- don't mark() this elt.
 * 2. LVAL(XLTYPE_WIDGETOBJ)	get_callback_widget(x)	-- GC must mark() this LVAL
 * 3. LVAL(CLOSURE)		get_callback_closure(x) -- GC must mark() this LVAL
 *
 * In order to satisfy JGC mods, note that XLTYPE_CALLBACKOBJ is a HYBRID_ARRAY
 * in which the first markable slot is at index 2.
 */
#define CALLBACKOBJ_SIZE 4
#define CALLBACKOBJ_IDX_OF_FIRST_LVAL 2
/* access macros */
#define get_callback_name(x)	   ((char *) (x)->n_vdata[0])
#define set_callback_name(x,v)	   ((x)->n_vdata[0] = (LVAL) (v)) /* 'v' is type char* */
#define get_callback_proc(x)	   ((XtCallbackProc) (x)->n_vdata[1])
#define set_callback_proc(x,v)	   ((x)->n_vdata[1] = (LVAL) (v)) /* 'v' is type XtCallbackProc */
#define get_callback_widget(x)	   ((x)->n_vdata[2])
#define set_callback_widget(x,v)   ((x)->n_vdata[2] = (v)) /* 'v' is type LVAL(XLTYPE_WIDGETOBJ) */
#define get_callback_closure(x)	   ((x)->n_vdata[3])
#define set_callback_closure(x,v)  ((x)->n_vdata[3] = (v)) /* 'v' is type LVAL(CLOSURE) */


/*
 * XLTYPE_TIMEOUTOBJ:
 *
 * A "timeout object" is created everytime a XT_ADD_TIMEOUT is callled; when the
 * timeout fires, the TIMEOUTOBJ is passed on to callback proc as client_data.
 * All TIMEOUTOBJs are protected from garbage collection by saving them in
 * v_savedobjs[]; TIMEOUTOBJs are removed from v_savedobjs when the callback
 * fires, or when XT_REMOVE_TIMEOUT is called.
 *
 * This is a HYBRID_ARRAY based type containing both direct pointers and LVALs
 * which must be marked by the garbage collector. It contains the following
 * C-accessible slots:
 * 0. XtIntervalId	get_timeout_id(x)	-- don't mark() this elt.
 * 1. LVAL(CLOSURE)	get_timeout_closure(x)	-- GC must mark() this LVAL
 */
#define TIMEOUTOBJ_SIZE 2
#define TIMEOUTOBJ_IDX_OF_FIRST_LVAL 1
/* access macros */
#define get_timeout_id(x)	   ((XtIntervalId) (x)->n_vdata[0])
#define set_timeout_id(x,v)	   ((x)->n_vdata[0] = (LVAL) (v)) /* 'v' is type XtIntervalId */
#define get_timeout_closure(x)	   ((x)->n_vdata[1])
#define set_timeout_closure(x,v)   ((x)->n_vdata[1] = (v)) /* 'v' is type LVAL(CLOSURE) */


/*
 * XLTYPE_EVHANDLEROBJ:
 *
 * A "event handler object" is created everytime the :{ADD,SET}_EVENT_HANDLER
 * method are callled to set up an event handler on a widget. The EVHANDLEROBJ
 * is passed to the event handler proc as client_data.
 * All EVHANDLEROBJs are protected from garbage collection by saving them in
 * v_savedobjs[]; EVHANDLEROBJs are removed from v_savedobjs[] when
 * REMOVE_EVENT_HANDLER is called, when the associated widget is destroyed,
 * or if :SET_EVENT_HANDLER overrides the existing eventhandler on the 
 * given <widget>, <event_mask>, and :RAW  & :NONMASKABLE flags.
 *
 * This is a HYBRID_ARRAY based type containing both direct pointers and LVALs
 * which must be marked by the garbage collector. It contains the following
 * C-accessible slots:
 * 0. EventMask			get_evhandler_mask(x)    -- don't mark() this elt.
 * 1. long			get_evhandler_options(x) -- don't mark() this elt.
 * 2. LVAL(XLTYPE_WIDGETOBJ)	get_evhandler_widget(x)  -- GC must mark() this LVAL
 * 3. LVAL(CLOSURE)		get_evhandler_closure(x) -- GC must mark() this LVAL
 */
#define EVHANDLEROBJ_SIZE 4
#define EVHANDLEROBJ_IDX_OF_FIRST_LVAL 2
/* access macros */
#define get_evhandler_mask(x)	   ((EventMask) (x)->n_vdata[0])
#define set_evhandler_mask(x,v)	   ((x)->n_vdata[0] = (LVAL) (v)) /* 'v' is type 'EventMask' == 'unsigned long' */
#define get_evhandler_options(x)   ((long) (x)->n_vdata[1])
#define set_evhandler_options(x,v) ((x)->n_vdata[1] = (LVAL) (v)) /* 'v' is type 'long' */
#define get_evhandler_widget(x)	   ((x)->n_vdata[2])
#define set_evhandler_widget(x,v)  ((x)->n_vdata[2] = (v)) /* 'v' is type LVAL(XLTYPE_WIDGETOBJ) */
#define get_evhandler_closure(x)   ((x)->n_vdata[3])
#define set_evhandler_closure(x,v) ((x)->n_vdata[3] = (v)) /* 'v' is type LVAL(CLOSURE) */


/*
 * XLTYPE_FDINPUTCBOBJ:
 * 
 * A "file-descriptor input callback object" is created everytime a XT_ADD_INPUT
 * is callled. When the callback fires, the FDINPUTCBOBJ is passed on to callback
 * proc as client_data. All FDINPUTCBOBJs are protected from garbage collection
 * by saving them in v_savedobjs[]; FDINPUTCBOBJs are removed from v_savedobjs when
 * when XT_REMOVE_INPUTCB is called.
 *
 * This is a HYBRID_ARRAY based type containing both direct pointers and LVALs
 * which must be marked by the garbage collector. It contains the following
 * C-accessible slots:
 * 0. XtInputId		get_fdinputcb_id(x)      -- don't mark() this elt.
 * 1. int		get_fdinputcb_parencnt(x)/get_fdinputcb_bufendidx(x) -- don't mark() this elt.
 * 2. int		set_fdinputcb_commtcnt(x)-- don't mark() this elt.
 * 3. int		get_fdinputcb_readstate(x)  -- don't mark() this elt.
 * 4. char*		get_fdinputcb_readbuf(x) -- don't mark() this elt.
 * 5. XtPointer		get_fdinputcb_condition(x) -- don't mark() this elt.
 * 6. int		get_fdinputcb_type(x)	 -- don't mark() this elt.
 * 7. LVAL(USTREAM)	get_fdinputcb_ustream(x) -- GC must mark() this LVAL.
 * 8. LVAL(CLOSURE)	get_fdinputcb_closure(x) -- GC must mark() this LVAL.
 * 9. LVAL(STREAM)	get_fdinputcb_file(x)    -- GC must mark() this LVAL.
 */
#define FDINPUTCBOBJ_SIZE 10
#define FDINPUTCBOBJ_IDX_OF_FIRST_LVAL 7
/* access macros */
#define get_fdinputcb_id(x)	     ((XtInputId) (x)->n_vdata[0])
#define set_fdinputcb_id(x,v)	     ((x)->n_vdata[0] = (LVAL) (v)) /* 'v' is type XtInputId */
#define get_fdinputcb_parencnt(x)    ((int) (x)->n_vdata[1])
#define set_fdinputcb_parencnt(x,v)  ((x)->n_vdata[1] = (LVAL) (v)) /* 'v' is type 'int' */
#define get_fdinputcb_commtcnt(x)    ((int) (x)->n_vdata[2])
#define set_fdinputcb_commtcnt(x,v)  ((x)->n_vdata[2] = (LVAL) (v)) /* 'v' is type 'int' */
#define get_fdinputcb_readstate(x)    ((int) (x)->n_vdata[3])
#define set_fdinputcb_readstate(x,v)  ((x)->n_vdata[3] = (LVAL) (v)) /* 'v' is type 'int' */
#define get_fdinputcb_readbuf(x)     ((char*) (x)->n_vdata[4])
#define set_fdinputcb_readbuf(x,v)   ((x)->n_vdata[4] = (LVAL) (v)) /* 'v' is type 'char*' */
#define get_fdinputcb_condition(x)     ((XtPointer) (x)->n_vdata[5])
#define set_fdinputcb_condition(x,v)   ((x)->n_vdata[5] = (LVAL) (v)) /* 'v' is type 'XtPointer' */
#define get_fdinputcb_type(x)     ((int) (x)->n_vdata[6])
#define set_fdinputcb_type(x,v)   ((x)->n_vdata[6] = (LVAL) (v)) /* 'v' is type 'int' */
#define get_fdinputcb_ustream(x)     ((x)->n_vdata[7])
#define set_fdinputcb_ustream(x,v)   ((x)->n_vdata[7] = (v)) /* 'v' is type LVAL(USTREAM) */
#define get_fdinputcb_closure(x)     ((x)->n_vdata[8])
#define set_fdinputcb_closure(x,v)   ((x)->n_vdata[8] = (v)) /* 'v' is type LVAL(CLOSURE) */
#define get_fdinputcb_file(x)	     ((x)->n_vdata[9]) 
#define set_fdinputcb_file(x,v)	     ((x)->n_vdata[9] = (v)) /* 'v' is type LVAL(STREAM) */

/* these are "aliases" for get_fdinputcb_parencnt() for Wicb_READ_LINE_TO_STRING_XtInputCallbackProc() */
#define get_fdinputcb_bufendidx(x)    ((int) (x)->n_vdata[1])
#define set_fdinputcb_bufendidx(x,v)  ((x)->n_vdata[1] = (LVAL) (v)) /* 'v' is type 'int' */


/*
 * XLTYPE_WIDGETOBJ:
 *
 * A "widget object" is created everytime an instance of WIDGET_CLASS
 * (or subclass thereof) is created. All WIDGETOBJs are protected from garbage
 * collection by saving them in v_savedobjs[]; WIDGETOBJs are removed from
 * v_savedobjs if the widget gets destroyed.
 *
 * Note that WIDGETOBJs support all the same operations as OBJECT. The main
 * difference between this type and OBJECT (both of which are implemented as
 * vectors) is that a WIDGETOBJ holds a single instance variable at slot 1
 * which is an immediate pointer to a WidgetID. This slot may not be marked
 * since it is not a LVAL node. All other slots are LVALs and must be marked,
 * this is done by special-casing on XLTYPE_WIDGETOBJ in the garbage collector.
 *
 * Whereas OBJECTS are treated as ARRAYs, WIDGETOBJs are treated as a
 * HYBRID_ARRAY a type containing both direct pointers and LVALs which must
 * be marked by the garbage collector. It contains the following
 * C-accessible slots:
 * 0. LVAL(OBJECT)	getclass(x)			-- GC must mark() this LVAL.
 * 1. Widget		get_widgetobj_widgetID(x)	-- don't mark() this elt.
 * 2. LVAL              getivar(x,WIDGETOBJ_SIZE + (0))	-- GC must mark() this LVAL
 * .   .				...		...
 * X. LVAL              getivar(x,WIDGETOBJ_SIZE + (N-1)) -- GC must mark() this LVAL
 *
 * Note, the getclass() and getivar() macros are valid for both
 * OBJECTs and WIDGETOBJs. getivar(x,i) accesses are only valid
 * for WIDGETOBJs that have been subclassed and contain additional
 * instance variables.
 */
#define WIDGETOBJ_SIZE 1	/* this val represents the number of instance variables in the object. newobject() will actually create a vector of size WIDGETOBJ_SIZE+1 so as to hold the ivar and the class pointer. */
#define WIDGETOBJ_IDX_OF_FIRST_LVAL 2 /* not quite true here since first LVAL at 0, but, see code in mark()... */
/* access macros */
/* #define getclass(x)	  ((x)->n_vdata[0]) --> this macro valid for both this and OBJECT */
#define get_widgetobj_widgetID(x)   ((Widget) ((x)->n_vdata[1]))
#define set_widgetobj_widgetID(x,v) ((x)->n_vdata[1] = (LVAL) (v)) /* 'v' is type 'Widget' */
/* #define getivar(x,i)	((x)->n_vdata[i+1]) --> this macro valid for both this and OBJECT */

#ifdef WINTERP_XTANGO_WIDGET
/*
 * XLTYPE_TANGOIMAGEOBJ:
 *
 * A "TANGOIMAGEOBJ" is created everytime an instance of TANGOIMAGE_CLASS
 * (or subclass thereof) is created. All TANGOIMAGEOBJs are protected from
 * garbage collection by saving them in v_savedobjs[]; TANGOIMAGEOBJs are
 * removed from v_savedobjs if the XTANGO_WIDGET_CLASS WIDGETOBJ gets
 * destroyed, or when the :DELETE message
 * (TANGOtrans_create(TANGO_TRANS_TYPE_DELETE...) is sent to a TANGOIMAGEOBJ
 *
 * Note that TANGOIMAGEOBJs support all the same operations as OBJECT. The main
 * difference between this type and OBJECT (both of which are implemented as
 * vectors) is that a TANGOIMAGEOBJ holds a single instance variable at slot 1
 * which is an immediate pointer to a WidgetID. This slot may not be marked
 * since it is not a LVAL node. All other slots are LVALs and must be marked,
 * this is done by special-casing on XLTYPE_TANGOIMAGEOBJ in the garbage
 * collector.
 *
 * Whereas OBJECTS are treated as ARRAYs, TANGOIMAGEOBJs are treated as a
 * HYBRID_ARRAY a type containing both direct pointers and LVALs which must
 * be marked by the garbage collector. It contains the following
 * C-accessible slots:
 * 0. LVAL(OBJECT)		getclass(x)				-- GC must mark() this LVAL.
 * 1. TANGO_IMAGE		get_tangoimageobj_timageID(x)		-- don't mark() this elt.
 * 2. WINTERP_TANGO_CONTEXT	get_tangoimageobj_context(x)		-- don't mark() this elt.
 * 3. LVAL			getivar(x,TANGOIMAGEOBJ_SIZE + (0))	-- GC must mark() this LVAL
 * .   .				...				...
 * X. LVAL			getivar(x,TANGOIMAGEOBJ_SIZE + (N-1))	-- GC must mark() this LVAL
 *
 * Note, the getclass() and getivar() macros are valid for both
 * OBJECTs, WIDGETOBJs, and TANGOIMAGEOBJs. getivar(x,i) accesses
 * are only valid for WIDGETOBJs that have been subclassed and
 * contain additional instance variables.
 */
#define TANGOIMAGEOBJ_SIZE 2	/* this val represents the number of instance variables in the object. newobject() will actually create a vector of size TANGOIMAGEOBJ_SIZE+1 so as to hold the ivar and the class pointer. */
#define TANGOIMAGEOBJ_IDX_OF_FIRST_LVAL 3 /* not quite true here since first LVAL at 0, but, see code in mark()... */
/* access macros */
/* #define getclass(x)			((x)->n_vdata[0]) --> this macro valid for both this and OBJECT */
#define get_tangoimageobj_timageID(x)	((TANGO_IMAGE) ((x)->n_vdata[1]))
#define set_tangoimageobj_timageID(x,v)	((x)->n_vdata[1] = (LVAL) (v)) /* 'v' is type 'TANGO_IMAGE' */
#define get_tangoimageobj_context(x)	((WINTERP_TANGO_CONTEXT) ((x)->n_vdata[2]))
#define set_tangoimageobj_context(x,v)	((x)->n_vdata[2] = (LVAL) (v)) /* 'v' is type 'WINTERP_TANGO_CONTEXT' */
/* #define getivar(x,i)			((x)->n_vdata[i+1]) --> this macro valid for both this and OBJECT */

/* XLTYPE_TANGO_PATH access macros */
#define get_tangopath(x)	      ((x)->n_xtangopath)
#define set_tangopath(x,v)	      ((x)->n_xtangopath = (v))

/* XLTYPE_TANGO_TRANS access macros */
#define get_tangotrans(x)	      ((x)->n_xtangotrans)
#define set_tangotrans(x,v)	      ((x)->n_xtangotrans = (v))
#define get_tangotrans_context(x)     ((x)->n_xtangotrans_context)
#define set_tangotrans_context(x,v)   ((x)->n_xtangotrans_context = (v))

#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */


/* node types */
#define FREE	0
#define SUBR	1
#define FSUBR	2
#define CONS	3

#ifdef JGC

#define FIXNUM	4
#define FLONUM	5
#define STRING	6
#define STREAM	7
#define CHAR	8
#define USTREAM 9

#if (defined(UNIX) || defined(WINTERP))
#define XLTYPE_PIPE		10
#endif /* (defined(UNIX) || defined(WINTERP)) */

#ifdef WINTERP

#define XLTYPE_XtAccelerators	11
#define XLTYPE_XtTranslations	12
#define XLTYPE_XEvent		13
#define XLTYPE_Window		14
#define XLTYPE_Pixel		15
#define XLTYPE_Pixmap		16
#define XLTYPE_XImage		17
#define XLTYPE_XmString		18
#define XLTYPE_XmFontList	19
#define XLTYPE_XT_RESOURCE	20

#ifdef WINTERP_XTANGO_WIDGET
#define XLTYPE_TANGO_PATH	21
#define XLTYPE_TANGO_TRANS	22
#else /* !defined(WINTERP_XTANGO_WIDGET) */
/* define XLTYPE_SIMPLE_1	21 */
/* define XLTYPE_SIMPLE_2	22 */
#endif /* WINTERP_XTANGO_WIDGET */

/* define XLTYPE_SIMPLE_3	23 */

/*
 * hybrid arrays should be between indexes 24..31. During garbage collection,
 * we test for hybrid array with
 * (this->n_type & (HYBRID_ARRAY|ARRAY|MARK)) == HYBRID_ARRAY
 * On adding new hybrid types, if XLTYPE_HYBRID_1, XLTYPE_HYBRID_2, XLTYPE_HYBRID_3
 * not enough, then repartition such that
 * normal: 0-31
 * hybrid: 32-47 -- test w/ n->n_type & #00110000 == #00010000
 * array:  48-63 -- test w/ n->n_type & #00110000 == #00110000
 */
#define HYBRID_ARRAY		0x18
#define XLTYPE_CALLBACKOBJ	(HYBRID_ARRAY+0)
#define XLTYPE_TIMEOUTOBJ	(HYBRID_ARRAY+1)
#define XLTYPE_WIDGETOBJ	(HYBRID_ARRAY+2)
#define XLTYPE_EVHANDLEROBJ	(HYBRID_ARRAY+3)
#define XLTYPE_FDINPUTCBOBJ	(HYBRID_ARRAY+4)

#ifdef WINTERP_XTANGO_WIDGET
#define XLTYPE_TANGOIMAGEOBJ	(HYBRID_ARRAY+5)
#else
/* #define XLTYPE_HYBRID_1	(HYBRID_ARRAY+5) */
#endif /* WINTERP_XTANGO_WIDGET */

/* #define XLTYPE_HYBRID_2	(HYBRID_ARRAY+6) */
/* #define XLTYPE_HYBRID_3	(HYBRID_ARRAY+7) */
/*
 * arrayed types
 * Test for these with ((type & ARRAY) != 0).
 * allowable values 32...63
 */
#define ARRAY			32
#define SYMBOL			(ARRAY+1)
#define OBJECT			(ARRAY+2)
#define VECTOR			(ARRAY+3)
#define CLOSURE			(ARRAY+4)
#ifdef STRUCTS
#define STRUCT			(ARRAY+5)
#endif /* STRUCTS */
#ifdef COMPLX
#define COMPLEX			(ARRAY+6)
#endif /* COMPLX */
#define XLTYPE_PIXMAP_REFOBJ	(ARRAY+7)
/* & mask for low 6 bits, top two bits are MARK and LEFT (see xldmem.c) */
#define TYPEFIELD 0x3f

#else /* !defined(WINTERP) */

#define ARRAY	16	/* arrayed types */
#define SYMBOL	(ARRAY+1)
#define OBJECT	(ARRAY+2)
#define VECTOR	(ARRAY+3)
#define CLOSURE (ARRAY+4)
#ifdef STRUCTS
#define STRUCT	(ARRAY+5)
#endif /* STRUCTS */
#ifdef COMPLX
#define COMPLEX (ARRAY+6)
#endif /* COMPLX */
#define TYPEFIELD 0x1f

#endif /* WINTERP */

#else /* !defined(JGC) */

#define SYMBOL	4
#define FIXNUM	5
#define FLONUM	6
#define STRING	7
#define OBJECT	8
#define STREAM	9
#define VECTOR	10
#define CLOSURE 11
#define CHAR	12
#define USTREAM 13
#ifdef STRUCTS
#define STRUCT	14
#endif /* STRUCTS */
#ifdef COMPLX
#define COMPLEX 15
#endif /* COMPLEX */
#if (defined(UNIX) || defined(WINTERP))
#define XLTYPE_PIPE			16 /* same node representation as STREAM, different ntype used to distinguish between file and pipe because files get fclose()'d on gc, and pipes get pclose()'d */
#endif /* (defined(UNIX) || defined(WINTERP)) */
#ifdef WINTERP
#define XLTYPE_XtAccelerators		17
#define XLTYPE_XtTranslations		18
#define XLTYPE_XEvent			19
#define XLTYPE_Window			20
#define XLTYPE_Pixel			21
#define XLTYPE_Pixmap			22
#define XLTYPE_XImage			23
#define XLTYPE_XmString			24
#define XLTYPE_XmFontList		25
#define XLTYPE_XT_RESOURCE		26
#define XLTYPE_CALLBACKOBJ		27
#define XLTYPE_TIMEOUTOBJ		28
#define XLTYPE_PIXMAP_REFOBJ		29
#define XLTYPE_WIDGETOBJ		30
#define XLTYPE_EVHANDLEROBJ		31
#define XLTYPE_FDINPUTCBOBJ		32
#ifdef WINTERP_XTANGO_WIDGET
#define XLTYPE_TANGOIMAGEOBJ		33
#define XLTYPE_TANGO_PATH		34
#define XLTYPE_TANGO_TRANS		35
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */

#endif /* JGC */

/* subr/fsubr node */
#define n_subr		n_info.n_xsubr.xs_subr
#define n_offset	n_info.n_xsubr.xs_offset

/* cons node */
#define n_car		n_info.n_xcons.xc_car
#define n_cdr		n_info.n_xcons.xc_cdr

/* fixnum node */
#define n_fixnum	n_info.n_xfixnum.xf_fixnum

/* flonum node */
#define n_flonum	n_info.n_xflonum.xf_flonum
/* character node */
#define n_chcode	n_info.n_xchar.xc_chcode

/* string node */
#define n_string	n_info.n_xstring.xs_string
#define n_strlen	n_info.n_xstring.xs_length

/* stream node */
#define n_fp		n_info.n_xstream.xs_fp
#define n_savech	n_info.n_xstream.xs_savech
#ifdef BETTERIO
#define S_READING	1   /* File is in reading mode */
#define S_WRITING	2   /* file is in writing mode */
#define S_FORREADING	4   /* File open for reading */
#define S_FORWRITING	8   /* file open for writing */
#define S_BINARY	16  /* file is binary file */

#define n_sflags	n_info.n_xstream.xs_flags
#define n_cpos		n_info.n_xstream.xs_cpos
#endif /* BETTERIO */

/* vector/object node */
#define n_vsize		n_info.n_xvector.xv_size
#define n_vdata		n_info.n_xvector.xv_data
#if (!defined(ALIGN32)) & defined(SPECIALS)
#define n_spflags	n_info.n_xvector.xv_flags
#endif

#ifdef WINTERP

/* XLTYPE_XT_RESOURCE node */
#define n_xtresource	       n_info.n_xresource.xr_resource

/* XLTYPE_Pixel node */
#define n_pixel		       n_info.n_xpixel.xp_pixel

/* XLTYPE_Pixmap node */
#define n_pixmap	       n_info.n_xpixmap.xp_pixmap
#define n_pixmap_color_info    n_info.n_xpixmap.xp_pixmap_color_info

/* XLTYPE_XImage node */
#define n_ximage	       n_info.n_xximage.xx_ximage

/* XLTYPE_XmString node */
#define n_xmstring	       n_info.n_xxmstring.xx_xmstring

/* XLTYPE_XEvent node */
#define n_xevent	       n_info.n_xxevent.xx_xevent

/* XLTYPE_Window node */
#define n_window	       n_info.n_xwindow.xw_window

/* XLTYPE_XtAccelerators node */
#define n_xtaccelerators       n_info.n_xxtaccelerators.xx_xtaccelerators

/* XLTYPE_XtTranslations node */
#define n_xttranslations       n_info.n_xxttranslations.xx_xttranslations

#ifdef WINTERP_XTANGO_WIDGET
/* XLTYPE_TANGO_PATH node */
#define n_xtangopath	       n_info.n_xxtangopath.xx_xtangopath

/* XLTYPE_TANGO_TRANS node */
#define n_xtangotrans	       n_info.n_xxtangotrans.xx_xtangotrans
#define n_xtangotrans_context  n_info.n_xxtangotrans.xx_xtangotrans_context
#endif /* WINTERP_XTANGO_WIDGET */

#endif /* WINTERP */

/* node structure */
typedef struct node {
/* 32 bit compilers that pack structures will do better with
   these chars at the end (especially if SPECIALS declared) */
#ifndef ALIGN32
    char n_type;		/* type of node */
#ifndef JGC
    char n_flags;		/* flag bits */
#endif
#endif
    union ninfo {		/* value */
	struct xsubr {		/* subr/fsubr node */
#ifdef ANSI
	    struct node FAR*(*xs_subr)(void);	/* function pointer */
#else
	    struct node FAR*(*xs_subr)();   /* function pointer */
#endif
	    int xs_offset;		/* offset into funtab */
	} n_xsubr;
	struct xcons {		/* cons node */
	    struct node FAR*xc_car;	/* the car pointer */
	    struct node FAR*xc_cdr;	/* the cdr pointer */
	} n_xcons;
	struct xfixnum {	/* fixnum node */
	    FIXTYPE xf_fixnum;		/* fixnum value */
	} n_xfixnum;
	struct xflonum {	/* flonum node */
	    FLOTYPE xf_flonum;		/* flonum value */
	} n_xflonum;
	struct xchar {		/* character node */
	    int xc_chcode;		/* character code */
	} n_xchar;
	struct xstring {	/* string node */
	    unsigned xs_length;		/* string length */
	    char FAR *xs_string;	    /* string pointer */
	} n_xstring;
	struct xstream {	/* stream node */
	    FILEP xs_fp;		/* the file pointer */
#ifdef BETTERIO
	    unsigned char xs_savech;	/* lookahead character */
	    char xs_flags;		/* read/write mode flags */
	    short xs_cpos;		/* character position in line */
#else
	    int xs_savech;		/* lookahead character */
#endif
	} n_xstream;
	struct xvector {	/* vector/object/symbol/structure node */
	    int xv_size;		/* vector size */
	    struct node FAR * FAR *xv_data;	/* vector data */
#ifndef ALIGN32
#ifdef SPECIALS
	    char xv_flags;	/* constant and special symbol flags */
#endif
#endif
	} n_xvector;
#ifdef WINTERP
    struct xresource {		/* XLTYPE_XTRESOURCE node */
      struct _Resource_Instance *xr_resource;
    } n_xresource;
    struct xpixel {		/* XLTYPE_Pixel node */
      Pixel xp_pixel;
    } n_xpixel;
    struct xpixmap {		/* XLTYPE_Pixmap node */
      Pixmap xp_pixmap;
      WINTERP_GIF_COLOR_INFO xp_pixmap_color_info;
    } n_xpixmap;
    struct xximage {		/* XLTYPE_XImage node */
      XImage *xx_ximage;
    } n_xximage;
    struct xxmstring {		/* XLTYPE_XmString node */
      XmString xx_xmstring;
    } n_xxmstring;
    struct xxevent {		/* XLTYPE_XEvent node */
      XEvent *xx_xevent;
    } n_xxevent;
    struct xwindow {		/* XLTYPE_Window node */
      Window xw_window;
    } n_xwindow;
    struct xxtaccelerators {
      XtAccelerators xx_xtaccelerators;	/* XLTYPE_XtAccelerators node */
    } n_xxtaccelerators;
    struct xxttranslations {
      XtTranslations xx_xttranslations;	/* XLTYPE_XtTranslations node */
    } n_xxttranslations;
#ifdef WINTERP_XTANGO_WIDGET
    struct xxtangopath {
      TANGO_PATH xx_xtangopath;         /* XLTYPE_TANGO_PATH */
    } n_xxtangopath;
    struct xxtangotrans {
      TANGO_TRANS xx_xtangotrans;       /* XLTYPE_TANGO_TRANS */
      WINTERP_TANGO_CONTEXT xx_xtangotrans_context; /* see ../tango.h */
    } n_xxtangotrans;
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */
    } n_info;
#ifdef ALIGN32
    char n_type;		/* type of node */
#ifndef JGC
    char n_flags;		/* flag bits */
#endif
#ifdef SPECIALS
    char n_spflags;
#endif
#endif
} FAR *LVAL;

/* memory segment structure definition */
typedef struct segment {
    int sg_size;
    struct segment FAR *sg_next;
    struct node sg_nodes[1];
} SEGMENT;

#if (defined(UNIX) || defined(WINTERP))
#ifdef ANSI
#ifdef BETTERIO
extern LVAL cv_pipe(FILE *fp, int flags); /* create a XLTYPE_PIPE */
#else /* !defined(BETTERIO) */
extern LVAL cv_pipe(FILE *fp);	/* create a XLTYPE_PIPE */
#endif /* BETTERIO */
#else /* !defined(ANSI) */
extern LVAL cv_pipe();		/* create a XLTYPE_PIPE */
#endif /* ANSI */
#endif /* (defined(UNIX) || defined(WINTERP)) */

#ifdef WINTERP
/* WINTERP memory allocation functions */
#ifdef ANSI
extern LVAL cv_xtresource(struct _Resource_Instance *res); /* convert a pointer to struct _Resource_Instance */
extern LVAL cv_pixel(Pixel pixel); /* convert a X11 Pixel to XLTYPE_Pixel */
extern LVAL cv_pixmap(Pixmap pixmap); /* create a XLTYPE_Pixmap */
extern LVAL cv_pixmap_allocd_colors(Pixmap pixmap, WINTERP_GIF_COLOR_INFO color_info); /* create a XLTYPE_Pixmap; allocd_colors will be freed by XFreeColors() when garbage collected... */
extern LVAL cv_ximage(XImage *ximage); /* create a XLTYPE_XImage */
extern LVAL cv_xmstring(XmString xmstr); /* create a XLTYPE_XmString */
extern LVAL cv_xevent(XEvent *xevp); /* create a XLTYPE_XEvent */
extern LVAL cv_window(Window win); /* create a XLTYPE_Window */
extern LVAL cv_xtaccelerators(XtAccelerators axl); /* create a XLTYPE_XtAccelerators */
extern LVAL cv_xttranslations(XtTranslations txl); /* create a XLTYPE_XtTranslations */
/* WARNING: use cvstring() to make a copy of the string; the string passed to cv_string will get freed by GC later */
extern LVAL cv_string(char* str); /* create a STRING, see WARNING above... */
#ifdef WINTERP_XTANGO_WIDGET
extern LVAL cv_tangopath(TANGO_PATH path); /* create a XLTYPE_TANGO_PATH */
extern LVAL cv_tangotrans(TANGO_TRANS trans, WINTERP_TANGO_CONTEXT context); /* create a XLTYPE_TANGO_TRANS */
#endif /* WINTERP_XTANGO_WIDGET */
extern LVAL new_pixrefobj(void); /* create a XLTYPE_PIXMAP_REFOBJ */
extern LVAL new_timeoutobj(void); /* create a XLTYPE_TIMEOUTOBJ */
extern LVAL new_callbackobj(void); /* create a XLTYPE_CALLBACKOBJ */
extern LVAL new_evhandlerobj(void); /* create a XLTYPE_EVHANDLEROBJ */
extern LVAL new_fdinputcbobj(void); /* create a XLTYPE_FDINPUTCBOBJ */
#else /* !defined(ANSI) */
extern LVAL cv_xtresource();	/* convert a pointer to struct _Resource_Instance */
extern LVAL cv_pixel();		/* convert a X11 Pixel to XLTYPE_Pixel */
extern LVAL cv_pixmap();	/* create a XLTYPE_Pixmap -- freed by GC-->Wpm_Decr_Refcount_Or_Free_Pixmap() */
extern LVAL cv_pixmap_allocd_colors(); /* create a XLTYPE_Pixmap; allocd_colors will be freed by XFreeColors() when garbage collected via Wpm_Decr_Refcount_Or_Free_Pixmap() ... */
extern LVAL cv_ximage();	/* create a XLTYPE_XImage */
extern LVAL cv_xmstring();	/* create a XLTYPE_XmString */
extern LVAL cv_xevent();	/* create a XLTYPE_XEvent */
extern LVAL cv_window();	/* create a XLTYPE_Window */
extern LVAL cv_xtaccelerators(); /* create a XLTYPE_XtAccelerators */
extern LVAL cv_xttranslations(); /* create a XLTYPE_XtTranslations */
/* WARNING: use cvstring() to make a copy of the string; the string passed to cv_string will get freed by GC later */
extern LVAL cv_string();	/* create a STRING, see WARNING above... */
#ifdef WINTERP_XTANGO_WIDGET
extern LVAL cv_tangopath();	/* create a XLTYPE_TANGO_PATH */
extern LVAL cv_tangotrans();	/* create a XLTYPE_TANGO_TRANS */
#endif /* WINTERP_XTANGO_WIDGET */
extern LVAL new_pixrefobj();	/* create a XLTYPE_PIXMAP_REFOBJ */
extern LVAL new_timeoutobj();	/* create a XLTYPE_TIMEOUTOBJ */
extern LVAL new_callbackobj();	/* create a XLTYPE_CALLBACKOBJ */
extern LVAL new_evhandlerobj();	/* create a XLTYPE_EVHANDLEROBJ */
extern LVAL new_fdinputcbobj();	/* create a XLTYPE_FDINPUTCBOBJ */
#endif /* ANSI */
#endif /* WINTERP */

/* memory allocation functions */
#ifdef ANSI
extern void gc(void);		    /* do a garbage collect */
extern SEGMENT FAR *newsegment(int n);	/* create a new segment */
extern LVAL cons(LVAL x, LVAL y);   /* (cons x y) */
extern LVAL cvsymbol(char *pname);  /* convert a string to a symbol */
extern LVAL cvstring(char FAR *str);	/* convert a string */
#ifdef BETTERIO
extern LVAL cvfile(FILEP fp, int flags);    /* convert a FILEP to a file */
#else
extern LVAL cvfile(FILEP fp);	    /* convert a FILEP to a file */
#endif
extern LVAL cvsubr(LVAL (*fcn)(void), int type, int offset);
				/* convert a function to a subr/fsubr */
#ifdef JMAC
extern LVAL Cvfixnum(FIXTYPE n);    /* convert a fixnum */
extern LVAL Cvchar(int n);	    /* convert a character */
#else
extern LVAL cvfixnum(FIXTYPE n);    /* convert a fixnum */
extern LVAL cvchar(int n);	    /* convert a character */
#endif
extern LVAL cvflonum(FLOTYPE n);    /* convert a flonum */

extern LVAL newstring(unsigned size);	/* create a new string */
extern LVAL newvector(unsigned size);	/* create a new vector */
extern LVAL newobject(LVAL cls, int size);  /* create a new object */
extern LVAL newclosure(LVAL name, LVAL type, LVAL env, LVAL fenv);
				    /* create a new closure */
extern LVAL newustream(void);	    /* create a new unnamed stream */
#ifdef STRUCTS
extern LVAL newstruct(LVAL type, int size); /* create a new structure */
#endif
#ifdef COMPLX
extern LVAL newcomplex(LVAL r, LVAL i);	    /* create a new complex number */
extern LVAL newicomplex(FIXTYPE r, FIXTYPE i);
extern LVAL newdcomplex(FLOTYPE r, FLOTYPE i);
#endif
#ifdef SPECIALS
extern VOID defconstant(LVAL sym, LVAL val);
#endif
#else	/* not ANSI */
extern VOID gc();		/* do a garbage collect */
extern SEGMENT *newsegment();	/* create a new segment */
extern LVAL cons();		/* (cons x y) */
extern LVAL cvsymbol();		/* convert a string to a symbol */
extern LVAL cvstring();		/* convert a string */
extern LVAL cvfile();		/* convert a FILEP to a file */
extern LVAL cvsubr();		/* convert a function to a subr/fsubr */
#ifdef JMAC
extern LVAL Cvfixnum();		/* convert a fixnum */
extern LVAL Cvchar();		/* convert a character */
#else
extern LVAL cvfixnum();		/* convert a fixnum */
extern LVAL cvchar();		/* convert a character */
#endif
extern LVAL cvflonum();		/* convert a flonum */

extern LVAL newstring();	/* create a new string */
extern LVAL newvector();	/* create a new vector */
extern LVAL newobject();	/* create a new object */
extern LVAL newclosure();	/* create a new closure */
extern LVAL newustream();	/* create a new unnamed stream */
#ifdef STRUCTS
extern LVAL newstruct();	/* create a new structure */
#endif
#ifdef COMPLX
extern LVAL newcomplex();	/* create a new complex number */
extern LVAL newicomplex();
extern LVAL newdcomplex();
#endif
#endif

#ifdef SPECIALS
#define F_SPECIAL   1
#define F_CONSTANT  2
#define F_NORMAL    0

#define setsvalue(s,v)	(setvalue(s,v), setsflags(s, F_SPECIAL))
#define setsflags(x,c)	((x)->n_spflags = (c))
#define constantp(x)  ((x)->n_spflags & F_CONSTANT)
#define specialp(x) ((x)->n_spflags & F_SPECIAL)

#else
/* no special handling of new constants or special variables */
#define setsvalue(sym,val) setvalue(sym, val)
#define defconstant(sym, val) setvalue(sym, val)

#endif

#ifdef JMAC
/* Speed ups, reduce function calls for fixed characters and numbers	   */
/* Speed is exeptionaly noticed on machines with a large instruction cache */
/* No size effects here (JonnyG) */

extern SEGMENT FAR *fixseg, FAR *charseg;
extern FIXTYPE _tfixed;
extern int _tint;

#define cvfixnum(n) ((_tfixed = n), \
		((_tfixed > SFIXMIN && _tfixed < SFIXMAX) ? \
		&fixseg->sg_nodes[(int)_tfixed-SFIXMIN] : \
		Cvfixnum(_tfixed)))

#if (CHARMIN == 0)  /* eliminate a comparison */
#define cvchar(c) ((_tint = c), \
		(((unsigned)_tint) <= CHARMAX ? \
			&charseg->sg_nodes[_tint-CHARMIN] : \
		Cvchar(_tint)))
#else
#define cvchar(c) ((_tint = c), \
		((_tint >= CHARMIN && _tint <= CHARMAX) ? \
			&charseg->sg_nodes[_tint-CHARMIN] : \
		Cvchar(_tint)))
#endif
#endif
