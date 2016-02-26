/* $Header: draw.c,v 1.6 88/12/02 10:43:13 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */

/*
	%
	% This file is a product of Sun Microsystems, Inc. and is provided for
	% unrestricted use provided that this legend is included on all tape
	% media and as a part of the software program in whole or part.
	% Users may copy, modify or distribute this file at will.
	%
	% THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
	% WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
	% PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
	%
	% This file is provided with no support and without any obligation on the
	% part of Sun Microsystems, Inc. to assist in its use, correction,
	% modification or enhancement.
	%
	% SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
	% INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
	% OR ANY PART THEREOF.
	%
	% In no event will Sun Microsystems, Inc. be liable for any lost revenue
	% or profits or other special, indirect and consequential damages, even
	% if Sun has been advised of the possibility of such damages.
	%
	% Sun Microsystems, Inc.
	% 2550 Garcia Avenue
	% Mountain View, California  94043
	%
	% Copyright (C) 1988 by Sun Microsystems. All rights reserved.
*/
#include <stdio.h>
#include "psio.h"
#include "psint.h"
#include "draw.h"

main()
  {
	int mode, newmode;
	int key;
	int x0, y0, x1, y1;
	int dx0, dy0, dx1, dy1;	/* damage events */
	float angle;
	char buf[256];

    if (ps_open_PostScript() == 0 )
	  {
		fprintf(stderr,"Cannot connect to NeWS server\n");
		exit(1);
      }
	getwd(buf);
	ps_init(buf);

	EHInit();
	GOInit();
	FontInit();
	PropInit();

    mode = STRETCH;
	ps_setcanvas();
    while (!psio_error(PostScriptInput))
	  {

		if(0) ;
		else if(ps_getmode(&newmode))
		  {
			if(mode != newmode)
			  {
				ehprocs[mode].handleend();
				mode = newmode;
				ehprocs[newmode].handlebegin();
			  }
		  }
		else if(ps_getselect(&x0, &y0))
			ehprocs[mode].handleselect(x0, y0);
		else if(ps_getadjust(&x0, &y0))
			ehprocs[mode].handleadjust(x0, y0);
		else if(ps_getkey(&key))
			ehprocs[mode].handlekey(key);
		else if(ps_getcreatepoint(&x1, &y1))
		  {
			ehprocs[mode].handlereply(x0, y0, x1, y1);
		  }
		else if(ps_getcreateangle(&angle))
		  {
				ehprocs[mode].handlereply(angle);
		  }
		else if(ps_getdamage(&dx0, &dy0, &dx1, &dy1))
		  {
			ps_setcanvas();
			ehprocs[mode].handledamage(dx0, dy0, dx1, dy1);
		  }
		else if(ps_getprop(&key))
		  {
			ehprocs[mode].handleprop(key);
		  }
		else
		  {
			fprintf(stderr, "End of program or illegal tag!\n");
			break;
		  }
	  }

    ps_close_PostScript();
	exit(0);
  }
