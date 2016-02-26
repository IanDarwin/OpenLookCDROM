/* $Header: gopvt.h,v 1.3 88/12/02 10:43:30 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#ifndef GOPVT_H
#define GOPVT_H

/* graphical objects */
#define ITEMHEADER                                                \
	int	type;		/* type of item        */                     \
	int	flags;		/* bitwise flags       */                     \
	int x0, y0;		/* bottom/left corner  */                     \
	int x1, y1;		/* top/right corner    */                     \
	float rotation;	/* degrees of rotation from canonical form */ \
	int xscale;		/* scaling in x dimension                  */ \
	int yscale;		/* scaling in y dimension                  */ \
	PROP* prop ;	/* properties for item                     */ \
	ITEM* pprev;    \
	ITEM* pnext

struct item {
	ITEMHEADER;
};

#define X0	(pitem->x0)
#define Y0	(pitem->y0)
#define X1	(pitem->x1)
#define Y1	(pitem->y1)

#define X	((pitem->x0 + pitem->x1) / 2)
#define Y	((pitem->y0 + pitem->y1) / 2)
#define W	((pitem->x1 - pitem->x0))
#define H	((pitem->y1 - pitem->y0))
#define W2	((pitem->x1 - pitem->x0) / 2)
#define H2	((pitem->y1 - pitem->y0) / 2)

#endif
