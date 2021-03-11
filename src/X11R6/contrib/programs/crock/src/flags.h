/*
     Copyright (c) 1994    Frank Adelstein

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */

/*
 *  Values for the flags field for the special moves
 */

#define NORMAL 		0x000
#define PROJECTILE	0x001		/* move fires a projectile */
#define PROJECTIMAGE	0x002		/* move IS a projectile */

#define FREEZE		0x004
#define HARPOON		0x008
#define TELEPORT	0x010
#define TELEATTACK	0x020
#define FLYATTACK	0x040
#define SHADOWATTACK	0x080
#define INVISIBLE	0x100
#define NEARFATALITY	0x200
#define FARFATALITY	0x400
#define FLYINGKICK	0x800
#define BIGFATALITY     0x1000
#define HEARTFATALITY   0x2000
#define BLOCKOFF        0x4000
#define SIXTEENTONS     0x8000
#define ONGROUND        0x10000

/* hey...not everything is implemented!!!  Fancy that! */
/* special.c is the place where most of this is used   */
