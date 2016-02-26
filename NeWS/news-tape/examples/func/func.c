/*
 * Copyright (C) 1988 by Martha Zimet. All rights reserved.
 * This program is provided for unrestricted use, provided that this 
 * copyright message is preserved. There is no warranty, and no author 
 * or distributer accepts responsibility for any damage caused by this 
 * program. 
 */

#include <stdio.h>
#include <math.h>
#include "func.h"

main() {
	int index;
	float x, y;
	if (ps_open_PostScript() == 0) {
		fprintf(stderr, "Can't contact NeWS server\n");
		exit(1);
	}
	ps_initialize();
	while (1)
		if (ps_menuhit(&index)) {
			ps_begincurve();
			for (x = 0; x<=13; x += .1) {
				switch(index) {
				case 0: y = sin(x); break;
				case 1: y = cos(x); break;
				case 2: y = sin(x)*exp(-x/3)*3; break;
				case 3: y = sin(x) + .1*sin(x*5+1); break;
				default: y = 0;
				}
				ps_lineto(x, y);
			}
			ps_endcurve();
		} else break;
}
