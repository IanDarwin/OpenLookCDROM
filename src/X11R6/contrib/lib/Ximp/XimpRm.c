/* $XimpImplementGroup: XimpRm.c, v 1.1 94/05/31 21:16:07 $ */
/* $XConsortium: XimpRm.c,v 1.6 92/10/19 19:26:16 rws Exp $ */
/******************************************************************

              Copyright 1991, 1992 by FUJITSU LIMITED
	      Copyright 1991, 1992 by Sun Microsystems, Inc.
              Copyright 1991, 1992 by Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of FUJITSU LIMITED,
Sun Microsystems, Inc. and Sony Corporation not be used in advertising 
or publicity pertaining to distribution of the software without specific,
written prior permission.
FUJITSU LIMITED, Sun Microsystems, Inc. and Sony Corporation make no 
representations about the suitability of this software for any purpose.
It is provided "as is" without express or implied warranty.

FUJITSU LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION DISCLAIM 
ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL FUJITSU
LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
          Hiromu Inukai        Sun Microsystems, Inc.
          Hideki Hiura         Sun Microsystems, Inc.
	  Makoto Wakamatsu     Sony Corporation

******************************************************************/
/*

Copyright (c) 1991 - 1994  FUJITSU LIMITED
Copyright (c) 1991 - 1994  Sony Corporation

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE FUJITSU LIMITED AND SONY CORPORATION BE LIABLE FOR
ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the FUJITSU LIMITED and
Sony Corporation shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from the FUJITSU LIMITED and Sony Corporation.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
	  Makoto Wakamatsu     Sony Corporation

*/

#define NEED_EVENTS
#include <X11/keysym.h>
#include "Xlibint.h"

#include "XimpIm.h"

#ifndef	isalnum
#define	isalnum(c)	\
    (('0' <= (c) && (c) <= '9')  || \
     ('A' <= (c) && (c) <= 'Z')  || \
     ('a' <= (c) && (c) <= 'z'))
#endif

Private void
_Ximp_Get_resource_name(im, res_name, res_class)
    Ximp_XIM	 im;
    char	*res_name;
    char	*res_class;
{
    if(im->core.res_name == NULL) {
	strcpy(res_name, "*");
    } else {
	strcpy(res_name, im->core.res_name);
	strcat(res_name, ".");
    }
    if(im->core.res_class == NULL) {
	strcpy(res_class, "*");
    } else {
	strcpy(res_class, im->core.res_class);
	strcat(res_class, ".");
    }
    strcat(res_name, "ximp.");
    strcat(res_class, "Ximp.");
}

Private void
_Ximp_parse( im, event )
    Ximp_XIM		 im;
    char		*event;
{
    Ximp_Key		key;
    char		*modifier, *detail;
    char		*ss;
    int			ii;
    Bool		exclamation, tilde;
    Ximp_KeyList	*keylist;
    static struct {
	char	*name;
	int	len;
	long	mask;
    } mod[] = {
	{ "Ctrl",	4,	ControlMask	},
	{ "Lock",	4,	LockMask	},
	{ "Caps",	4,	LockMask	},
	{ "Shift",	5,	ShiftMask	},
	{ "Alt",	3,	Mod1Mask	},
	{ "Meta",	4,	Mod1Mask	},
	{ NULL,		0,	0		}};
#define	AllMask	(ControlMask | LockMask | ShiftMask | Mod1Mask)

    if( (ss = _Ximp_Strstr( event, "<Key>" )) == NULL )
	return;
    detail = ss + 5;
    *ss = NULL;

    modifier = event;
    key.modifier = 0;
    key.modifier_mask = 0;
    key.keysym = XK_VoidSymbol;
    exclamation = False;
    do {
	while( *modifier == ' '  ||  *modifier == '\t' )
	    modifier++;
	if( *modifier == NULL )
	    break;
	if( strncmp( modifier, "None", 4 ) == 0 ) {
	    if( key.modifier_mask != 0  ||  exclamation )
		return;
	    key.modifier_mask = AllMask;
	    modifier += 4;
	} else {
	    if( *modifier == '!' ) {
		if( key.modifier_mask != 0  ||  exclamation )
		    return;
		key.modifier_mask = AllMask;
		exclamation = True;
		modifier++;
		continue;
	    }
	    if( (tilde = (*modifier == '~')) ) {
		modifier++;
		while( *modifier == ' '  ||  *modifier == '\t' )
		    modifier++;
	    }
	    for( ii = 0; mod[ii].name != NULL; ii++ ) {
		if( strncmp( modifier, mod[ii].name, mod[ii].len ) == 0 ) {
		    key.modifier_mask |= mod[ii].mask;
		    if( !tilde )
			key.modifier |= mod[ii].mask;
		    modifier += mod[ii].len;
		    break;
		}
	    }
	}
	if( mod[ii].name == NULL )
	    return;
    } while( *modifier != NULL );

    while( *detail == ' '  ||  *detail == '\t' )
	detail++;
    for( ss = detail; isalnum(*ss)  ||  *ss == '_'; ss++ );
    *ss = NULL;
    if( (key.keysym = XStringToKeysym( detail )) != NoSymbol ) {
	if( !(keylist = im->ximp_impart->process_start_keys) ) {
	    if( (keylist = (Ximp_KeyList *)Xcalloc(1, sizeof(Ximp_KeyList))) == NULL )
		return;
	    if((keylist->keys_list = (Ximp_Key *)Xmalloc(sizeof(Ximp_Key))) == NULL) {
		Xfree( keylist );
		return;
	    }
	} else {
	    Ximp_Key	*keys_list;
	    if( (keys_list = (Ximp_Key *)Xrealloc(keylist->keys_list, sizeof(Ximp_Key) * (keylist->count_keys + 1))) == NULL )
		return;
	    keylist->keys_list = keys_list;
	}
	keylist->keys_list[keylist->count_keys] = key;
	keylist->count_keys++;
	im->ximp_impart->process_start_keys = keylist;
    }
    return;
}

Private void
_Ximp_InputServerMode(im)
    Ximp_XIM		 im;
{
    char		 res_name[256];
    char		 res_class[256];
    char		*str_type;
    XrmValue		 value;
    KeySym		 keysym = NoSymbol;
    Ximp_KeyList	*keylist;
    XIMStyles		*imstyles;
    XIMStyle		 imstyle1, imstyle2;

    _Ximp_Get_resource_name(im, res_name, res_class);
    strcat(res_name, "inputserver");
    strcat(res_class, "Inputserver");
    if((XrmGetResource(im->core.rdb, res_name, res_class,&str_type, &value) == True)
       && (strcmp(value.addr, "off") == 0) ) {
	MAKE_CONNECTABLE(im) ;
    }

    _Ximp_Get_resource_name(im, res_name, res_class);
    strcat(res_name, "delaybinding");
    strcat(res_class, "Delaybinding");
    if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value) == True) { 
	if(strcmp(value.addr, "ON") == 0 || 
	   strcmp(value.addr, "on") == 0) {
	    MAKE_DELAYBINDABLE(im);
	}
    }

    _Ximp_Get_resource_name(im, res_name, res_class);
    strcat(res_name, "reconnect");
    strcat(res_class, "Reconnect");
    if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value) == True) { 
	if(strcmp(value.addr, "ON") == 0 || 
	   strcmp(value.addr, "on") == 0) {
	    MAKE_RECONNECTABLE(im) ;
	}
    }

    _Ximp_Get_resource_name(im, res_name, res_class);
    strcat(res_name, "restart");
    strcat(res_class, "Restart");
    if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value) == True) { 
	if(strcmp(value.addr, "ON") == 0 || 
	   strcmp(value.addr, "on") == 0) {
	    MAKE_RESTARTABLE(im) ;
	}
    }

    if(IS_UNCONNECTABLE(im))
	return; 

    /* Keysym */
    _Ximp_Get_resource_name(im, res_name, res_class);
    strcat(res_name, "startkeysym");
    strcat(res_class, "Startkeysym");
    if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value) == True) { 
	keysym = XStringToKeysym(value.addr);
    }
    if(keysym != NoSymbol) {
	if((keylist = (Ximp_KeyList *)Xmalloc(sizeof(Ximp_KeyList))) != NULL ) {
	    if((keylist->keys_list = (Ximp_Key *)Xmalloc(sizeof(Ximp_Key)))!= NULL) {
		keylist->count_keys = 1;
		keylist->keys_list[0].modifier = 0;
		keylist->keys_list[0].modifier_mask = 0;
		keylist->keys_list[0].keysym = keysym;
		im->ximp_impart->process_start_keys = keylist;
	    } else
		Xfree(keylist);
	}
    }

    /* ProcessStartKeys */
    _Ximp_Get_resource_name(im, res_name, res_class);
    strcat(res_name, "processStartKeys");
    strcat(res_class, "ProcessStartKeys");
    if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value)  == True) {
	char	*string, *ss, c;
	char	*line;

	if( (line = Xmalloc(value.size)) != NULL ) {
	    string = value.addr;
	    do {
		ss = line;
		while( (c = *string) != NULL ) {
		    string++;
		    if( c == '\n' )
			break;
		    *ss++ = c;
		}
		*ss = NULL;
		_Ximp_parse( im, line );
	    } while( *string != NULL );
	    Xfree( line );
	}
    }

    if(im->ximp_impart->process_start_keys == (Ximp_KeyList *)NULL)
	MAKE_UNCONNECTABLE(im) ;

    if(IS_DELAYBINDABLE(im)) {
	imstyle1 = imstyle2 = 0;
	_Ximp_Get_resource_name(im, res_name, res_class);
	strcat(res_name, "preeditDefaultStyle");
	strcat(res_class, "PreeditDefaultStyle");
	if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value) == True) { 
	    if(strcmp(value.addr, "XIMPreeditArea") == 0)
		imstyle1 = XIMPreeditArea;
	    else if(strcmp(value.addr, "XIMPreeditCallbacks") == 0)
		imstyle1 = XIMPreeditCallbacks;
	    else if(strcmp(value.addr, "XIMPreeditPosition") == 0)
		imstyle1 = XIMPreeditPosition;
	    else if(strcmp(value.addr, "XIMPreeditNothing") == 0)
		imstyle1 = XIMPreeditNothing;
	    else if(strcmp(value.addr, "XIMPreeditNone") == 0)
		imstyle1 = XIMPreeditNone;
	}
	if(imstyle1 == 0)
	    imstyle1 = XIMPreeditNothing;
	_Ximp_Get_resource_name(im, res_name, res_class);
	strcat(res_name, "statusDefaultStyle");
	strcat(res_class, "StatusDefaultStyle");
	if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value) == True) { 
	    if(strcmp(value.addr, "XIMStatusArea") == 0)
		imstyle2 = XIMStatusArea;
	    else if(strcmp(value.addr, "XIMStatusCallbacks") == 0)
		imstyle2 = XIMStatusCallbacks;
	    else if(strcmp(value.addr, "XIMStatusNothing") == 0)
		imstyle2 = XIMStatusNothing;
	    else if(strcmp(value.addr, "XIMStatusNone") == 0)
		imstyle2 = XIMStatusNone;
	}
	if(imstyle2 == 0)
	    imstyle2 = XIMStatusNothing;

	if((imstyles = (XIMStyles *)Xmalloc(sizeof(XIMStyles) + sizeof(XIMStyle))) == NULL)
	    return;
	imstyles->count_styles = 1;
	imstyles->supported_styles = (XIMStyle *)((char *)imstyles + sizeof(XIMStyles));
	imstyles->supported_styles[0] = imstyle1 | imstyle2;
	im->ximp_impart->delaybind_styles = imstyles;
    }
    return;
}

Private void
_Ximp_CallBackWchar(im)
    Ximp_XIM		 im;
{
    char		 res_name[256];
    char		 res_class[256];
    char		*str_type;
    XrmValue		 value;

    _Ximp_Get_resource_name(im, res_name, res_class);
    strcat(res_name, "callbackEncoding");
    strcat(res_class, "CallbackEncoding");
    if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value) == True) {
	if(strcmp(value.addr, "wchar") == 0) {
	    im->ximp_impart->use_wchar = True;
	}
    }
    return;
}

Private void
_Ximp_ForceSelectKeyRelease(im)
    Ximp_XIM		 im;
{
    char		 res_name[256];
    char		 res_class[256];
    char		*str_type;
    XrmValue		 value;

    _Ximp_Get_resource_name(im, res_name, res_class);
    strcat(res_name, "forceSelectKeyRelease");
    strcat(res_class, "ForceSelectKeyRelease");
    if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value) == True) {
	if(strcmp(value.addr, "yes") == 0) {          /* Ximp 4.0 */
	    im->ximp_impart->is_forceselectkeyrelease = True;
	}
    }
    return;
}

Private int
_Ximp_Environ()
{
    char	*env_p;
#ifdef sun
    int		 ret = XIMP_FE_TYPE3;
#else
    int		 ret = XIMP_FE_TYPE1;
#endif    
    if((env_p = (char *)getenv("XIMP_TYPE")) != (char *)NULL) {
	if(strcmp(env_p, "XIMP_FE_TYPE1") == 0)
	    ret = XIMP_FE_TYPE1;
	else if(strcmp(env_p, "XIMP_FE_TYPE2") == 0)
	    ret = XIMP_FE_TYPE2;
	else if(strcmp(env_p, "XIMP_FE_TYPE3") == 0)
	    ret = XIMP_FE_TYPE3;
	else if(strcmp(env_p, "XIMP_BE_TYPE1") == 0)
	    ret = XIMP_BE_TYPE1;
	else if(strcmp(env_p, "XIMP_BE_TYPE2") == 0)
	    ret = XIMP_BE_TYPE2;
	else if(strcmp(env_p, "XIMP_SYNC_BE_TYPE1") == 0)
	    ret = XIMP_SYNC_BE_TYPE1;
	else if(strcmp(env_p, "XIMP_SYNC_BE_TYPE2") == 0)
	    ret = XIMP_SYNC_BE_TYPE2;
    }
    return(ret);
}

Private int
_Ximp_InputTypeResource(im)
    Ximp_XIM		 im;
{
    char		 res_name[256];
    char		 res_class[256];
    char		*str_type;
    XrmValue		 value;
#ifdef sun
    int		 ret = XIMP_FE_TYPE3;
#else
    int		 ret = XIMP_FE_TYPE1;
#endif    

    _Ximp_Get_resource_name(im, res_name, res_class);
    strcat(res_name, "immode");
    strcat(res_class, "Immode");
    if(XrmGetResource(im->core.rdb, res_name, res_class, &str_type, &value) == True) {
	if(strcmp(value.addr, "XIMP_FE_TYPE1") == 0)
	    ret = XIMP_FE_TYPE1;
	else if(strcmp(value.addr, "XIMP_FE_TYPE2") == 0)
	    ret = XIMP_FE_TYPE2;
	else if(strcmp(value.addr, "XIMP_FE_TYPE3") == 0)
	    ret = XIMP_FE_TYPE3;
	else if(strcmp(value.addr, "XIMP_BE_TYPE1") == 0)
	    ret = XIMP_BE_TYPE1;
	else if(strcmp(value.addr, "XIMP_BE_TYPE2") == 0)
	    ret = XIMP_BE_TYPE2;
	else if(strcmp(value.addr, "XIMP_SYNC_BE_TYPE1") == 0)
	    ret = XIMP_SYNC_BE_TYPE1;
	else if(strcmp(value.addr, "XIMP_SYNC_BE_TYPE2") == 0)
	    ret = XIMP_SYNC_BE_TYPE2;
	return(ret);
    }
    return(-1);
}

Public void
_Ximp_OpenIM_Resource(im)
    Ximp_XIM	 im;
{
    int		 mode;

    im->ximp_impart->def_svr_mode = _Ximp_Environ(); /* server input type */ 

    if(im->core.rdb == NULL)
	return;

    /* Inputserver */
    _Ximp_InputServerMode(im);

    /* Input Type */
    if((mode = _Ximp_InputTypeResource(im)) != -1)
	im->ximp_impart->def_svr_mode = mode;

    /* Call Back */
    _Ximp_CallBackWchar(im);

    /* Force Select KeyRelease support */
    _Ximp_ForceSelectKeyRelease(im);

    /* Extension Resource */
    _Ximp_OpenIMResourceExtension(im);
    return;
}

Public void
_Ximp_SetValue_Resource(ic, mask)
    Ximp_XIC		 ic;
    XimpChangeaMask	 mask;
{
    Ximp_XIM	 im;
    char	 res_name[256];
    char	 res_class[256];
    char	*str_type ;
    XrmValue	 value;
    Colormap	 default_colormap;
    XColor	 screen_def, exact_def;
    int		 num, mode;
    
    im = (Ximp_XIM)XIMOfIC((XIC)ic);
    if(im->core.rdb == NULL)
	return;

    if(!(XIMP_CHK_SERVERTYPEMASK(ic))) {
	if((mode = _Ximp_InputTypeResource(im)) != -1) {
	    ic->ximp_icpart->svr_mode = mode;
	    XIMP_SET_SERVERTYPEMASK(ic, mask);
	}
    }

    if(!(   (ic->core.input_style & XIMPreeditCallbacks)
         || (ic->core.input_style & XIMPreeditNone) ) ) {
	if(!(XIMP_CHK_PREBGMASK(ic))) {
	    _Ximp_Get_resource_name(im, res_name, res_class);
	    strcat(res_name, "preedit.background");
	    strcat(res_class, "Preedit.Background");
	    if(XrmGetResource(im->core.rdb, res_name, res_class,
				  &str_type, &value) == True) { 
		default_colormap = DefaultColormap(
					       im->core.display,
					       DefaultScreen(im->core.display) );
		if( XAllocNamedColor(im->core.display, default_colormap,
					 value.addr,
					 &screen_def, &exact_def) ) {
		    ic->core.preedit_attr.background = screen_def.pixel;
		    ic->ximp_icpart->preedit_attr.Background = 
				ic->core.preedit_attr.background;
		    XIMP_SET_PREBGMASK(ic, mask);
		}
	    }
	}
	if(!(XIMP_CHK_PREFGMASK(ic))) {
	    _Ximp_Get_resource_name(im, res_name, res_class);
	    strcat(res_name, "preedit.foreground");
	    strcat(res_class, "Preedit.Foreground");
	    if(XrmGetResource(im->core.rdb, res_name, res_class,
				  &str_type, &value) == True) { 
		default_colormap = DefaultColormap(
					       im->core.display,
					       DefaultScreen(im->core.display) );
		if( XAllocNamedColor(im->core.display, default_colormap,
					 value.addr,
					 &screen_def, &exact_def) ) {
		    ic->core.preedit_attr.foreground = screen_def.pixel;
		    ic->ximp_icpart->preedit_attr.Foreground = 
				ic->core.preedit_attr.foreground;
		    XIMP_SET_PREBGMASK(ic, mask);
		}
	    }
	}
	if(!(XIMP_CHK_PRELINESPMASK(ic))) {
	    _Ximp_Get_resource_name(im, res_name, res_class);
	    strcat(res_name, "preedit.linespacing");
	    strcat(res_class, "Preedit.Linespacing");
	    if(XrmGetResource(im->core.rdb, res_name, res_class,
				  &str_type, &value) == True) { 
		num = atoi(value.addr);
		ic->core.preedit_attr.line_spacing = num;
		ic->ximp_icpart->preedit_attr.LineSpacing = 
			ic->core.preedit_attr.line_spacing;
		XIMP_SET_PRELINESPMASK(ic, mask);
	    }
	}
    }

    if(!(   (ic->core.input_style & XIMStatusCallbacks)
         || (ic->core.input_style & XIMStatusNone) ) ) {
	if(!(XIMP_CHK_STSBGMASK(ic))) {
	    _Ximp_Get_resource_name(im, res_name, res_class);
	    strcat(res_name, "status.background");
	    strcat(res_class, "Status.Background");
	    if(XrmGetResource(im->core.rdb, res_name, res_class,
				  &str_type, &value) == True) { 
	    default_colormap = DefaultColormap(im->core.display,
					       DefaultScreen(im->core.display) );
	    if( XAllocNamedColor(im->core.display, default_colormap,
				 value.addr,
				 &screen_def, &exact_def) ) {
		ic->core.status_attr.background = screen_def.pixel;
		ic->ximp_icpart->status_attr.Background = 
			ic->core.status_attr.background;
		XIMP_SET_STSBGMASK(ic, mask);
		}
	    }
	}
	if(!(XIMP_CHK_STSFGMASK(ic))) {
	    _Ximp_Get_resource_name(im, res_name, res_class);
	    strcat(res_name, "status.foreground");
	    strcat(res_class, "Status.Foreground");
	    if(XrmGetResource(im->core.rdb, res_name, res_class,
				  &str_type, &value) == True) { 
	    default_colormap = DefaultColormap(im->core.display,
					       DefaultScreen(im->core.display) );
	    if( XAllocNamedColor(im->core.display, default_colormap,
				 value.addr,
				 &screen_def, &exact_def) ) {
		ic->core.status_attr.foreground = screen_def.pixel;
		ic->ximp_icpart->status_attr.Foreground = 
			ic->core.status_attr.foreground;
		XIMP_SET_STSFGMASK(ic, mask);
		}
	    }
	}
	if(!(XIMP_CHK_STSLINESPMASK(ic))) {
	    _Ximp_Get_resource_name(im, res_name, res_class);
	    strcat(res_name, "status.linespacing");
	    strcat(res_class, "Status.Linespacing");
	    if(XrmGetResource(im->core.rdb, res_name, res_class,
				  &str_type, &value) == True) { 
		num = atoi(value.addr);
		ic->core.status_attr.line_spacing = num;
		ic->ximp_icpart->status_attr.LineSpacing = 
			ic->core.status_attr.line_spacing;
		XIMP_SET_STSLINESPMASK(ic, mask);
	    }
	}
    }

    if(   (ic->ximp_icpart->value_mask & XIMP_RES_NAME)
       || (ic->ximp_icpart->value_mask & XIMP_RES_CLASS) )
	ic->ximp_icpart->value_mask &= ~(XIMP_RES_NAME | XIMP_RES_CLASS);
    return;
}
