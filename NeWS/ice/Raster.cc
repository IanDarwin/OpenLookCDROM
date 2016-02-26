/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <rasterfile.h>
#include <pixrect/pixrect_hs.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "Raster.h"

extern "C" {
void		bcopy(char *, char *, int);
int		free(char *);
char *		strcpy(char *, char *);
int		strlen(char *);
}

extern FILE *	doc_open(char *, char *);

static int redct[256], greenct[256], bluect[256];

Raster::Raster()
{
	filenm= (char *) 0;
	width= height= depth= 0;
	pixrep= 1;
	origloc= RASTER_ULORIG;
	drawmode= RASTER_FULL;
	display= (Display *) 0;
	visual= (Visual *) 0;
	fullgc= None;
	outlinegc= None;
	cmap= None;
	image= (XImage *) 0;
	fgpix= bgpix= outline= 0;
	redfg= greenfg= bluefg= 0;
	redbg= greenbg= bluebg= 0;
	redint= greenint= blueint= (intensity *) 0;
	sunras.ras_width= 0;
	sunras.ras_height= 0;
	sunras.ras_depth= 0;
	suncmap.length= 0;
	suncmap.map[0]= (unsigned char *) 0;
	(void) settype(GROBJ_RASTER);
	setsequence(0);
}

Raster::Raster(Dlnk **listhd, char *f, Display *dpy, Visual *v, unsigned long ol, int d, int seq)
{
	FILE *fp;
	Pixmap pm;
	XGCValues gcv;

	filenm= (char *) 0;
	cmap= None;
	image= (XImage *) 0;
	redint= greenint= blueint= (intensity *) 0;
	fullgc= None;
	outlinegc= None;
	suncmap.length= 0;
	suncmap.map[0]= (unsigned char *) 0;
	display= dpy;
	visual= v;
	depth= d;
	fgpix= 1;
	bgpix= 0;
	outline= ol;
	redfg= greenfg= bluefg= 0;
	redbg= greenbg= bluebg= 255;
	pixrep= 1;
	origloc= RASTER_ULORIG;
	drawmode= RASTER_FULL;

	if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);
	if (f == (char *) 0) {
		filenm= (char *) 0;
		return;
	}
	else if (strlen(f) == 0) {
		filenm= (char *) 0;
		return;
	}
	else {
		if ((filenm= new char[strlen(f)+1]) != (char *) 0)
			(void) strcpy(filenm, f);
		if ((fp= doc_open(filenm, "r")) == (FILE *) NULL) {
			delete filenm;
			filenm= (char *) 0;
			return;
		}
	}

	if (pr_load_header(fp, &sunras) != 0) {
		(void) fclose(fp);
		delete filenm;
		filenm= (char *) 0;
		return;
	}
	if (sunras.ras_magic != RAS_MAGIC) {
		(void) fclose(fp);
		delete filenm;
		filenm= (char *) 0;
		return;
	}
	(void) fclose(fp);

	width= sunras.ras_width;
	height= sunras.ras_height;

	/* temporary pixmap is created just so we can create GCs */
	if ((pm= XCreatePixmap(display, DefaultRootWindow(display), (unsigned) 1, (unsigned) 1, (unsigned) depth)) == None) {
		delete filenm;
		filenm= (char *) 0;
		return;
	}

	gcv.function= GXcopy;
	gcv.foreground= fgpix;
	gcv.background= bgpix;
	gcv.plane_mask= AllPlanes;
	if ((fullgc= XCreateGC(display, pm, (GCFunction | GCForeground | GCBackground | GCPlaneMask), &gcv)) == None) {
		delete filenm;
		filenm= (char *) 0;
		XFreePixmap(display, pm);
		return;
	}
	gcv.foreground= outline;
	if ((outlinegc= XCreateGC(display, pm, (GCFunction | GCForeground | GCPlaneMask), &gcv)) == None) {
		delete filenm;
		filenm= (char *) 0;
		XFreePixmap(display, pm);
		XFreeGC(display, fullgc);
		fullgc= None;
		return;
	}
	XFreePixmap(display, pm);

	(void) settype(GROBJ_RASTER);
	setsequence(seq);
	sortsequence((Grobj **) listhd);
}

int
Raster::bldimage()
{
	FILE *fp;
	struct rasterfile junk;
	Pixrect *pr;
	unsigned long srcpix, dstpix;
	int sx, sy, dx, dy;

	if ((sunras.ras_depth >= 8) && (depth < 8)) {
		drawmode= RASTER_OUTLINE;
		return GROBJ_SUCCESS;
	}

	if ((fp= doc_open(filenm, "r")) == (FILE *) NULL)
		return GROBJ_FAILURE;
	if (pr_load_header(fp, &junk) != 0) {
		(void) fclose(fp);
		return GROBJ_FAILURE;
	}
	if ((sunras.ras_maptype != RMT_NONE) && (sunras.ras_maplength > 0)) {
		if (suncmap.map[0] != (unsigned char *) 0)
			free((char *) (suncmap.map[0]));
		suncmap.length= 0;
		suncmap.map[0]= (unsigned char *) 0;
		if (pr_load_colormap(fp, &sunras, &suncmap) != 0) {
			(void) fclose(fp);
			return GROBJ_FAILURE;
		}
	}
	if ((pr= pr_load_image(fp, &sunras, &suncmap)) == (Pixrect *) 0) {
		(void) fclose(fp);
		return GROBJ_FAILURE;
	}
	(void) fclose(fp);

	if (image != (XImage *) 0)
		XDestroyImage(image);
	if ((image= XCreateImage(display, visual, depth, ZPixmap, 0, (char *) 0, width, height, 8, 0)) == (XImage *) 0) {
		pr_destroy(pr);
		return GROBJ_FAILURE;
	}
	if ((image->data= new char[image->bytes_per_line*height]) == (char *) 0) {
		pr_destroy(pr);
		XDestroyImage(image);
		image= (XImage *) 0;
		return GROBJ_FAILURE;
	}

	if ((depth == 8) && (sunras.ras_depth == 8)) {
		if (sunras.ras_maptype == RMT_EQUAL_RGB) {
			int i;
			XColor color;

			cmap= XCreateColormap(display, DefaultRootWindow(display), visual, AllocAll);
			color.flags= DoRed | DoGreen | DoBlue;
			for (i= 0; i < suncmap.length; i++) {
				color.pixel= i;
				color.red= (unsigned short) (256*((int) *(suncmap.map[0]+i)));
				color.green= (unsigned short) (256*((int) *(suncmap.map[1]+i)));
				color.blue= (unsigned short) (256*((int) *(suncmap.map[2]+i)));
				XStoreColor(display, cmap, &color);
			}
		}
	}
	if (sunras.ras_depth >= 24) {
		cmap= XCreateColormap(display, DefaultRootWindow(display), visual, AllocAll);
		if (bldhistcmap(pr) != GROBJ_SUCCESS) {
			pr_destroy(pr);
			XDestroyImage(image);
			image= (XImage *) 0;
			return GROBJ_FAILURE;
		}
	}

	switch (origloc) {
	case RASTER_ULORIG:
		for (sy= 0; sy < sunras.ras_height; sy++) {
			for (sx= 0; sx < sunras.ras_width; sx++) {
				int i, j;

				dx= sx*pixrep;
				dy= sy*pixrep;
				srcpix= (unsigned long) pr_get(pr, sx, sy);
				switch (sunras.ras_depth) {
				case 1:
					dstpix= srcpix ? fgpix : bgpix;
					break;
				case 8:
					dstpix= srcpix;
					break;
				case 24:
				case 32:
					dstpix= pseudopix(srcpix);
					break;
				}
				for (i= 0; i < pixrep; i++)
					for (j= 0; j < pixrep; j++)
						XPutPixel(image, dx+i, dy+j, dstpix);
			}
		}
		break;
	case RASTER_LLORIG:
		for (sy= 0; sy < sunras.ras_height; sy++) {
			for (sx= 0; sx < sunras.ras_width; sx++) {
				int i, j;

				dy= height-((sx+1)*pixrep);
				dx= sy*pixrep;
				srcpix= (unsigned long) pr_get(pr, sx, sy);
				switch (sunras.ras_depth) {
				case 1:
					dstpix= srcpix ? fgpix : bgpix;
					break;
				case 8:
					dstpix= srcpix;
					break;
				case 24:
				case 32:
					dstpix= pseudopix(srcpix);
					break;
				}
				for (i= 0; i < pixrep; i++)
					for (j= 0; j < pixrep; j++)
						XPutPixel(image, dx+i, dy+j, dstpix);
			}
		}
		break;
	case RASTER_LRORIG:
		for (sy= 0; sy < sunras.ras_height; sy++) {
			for (sx= 0; sx < sunras.ras_width; sx++) {
				int i, j;

				dx= width-((sx+1)*pixrep);
				dy= height-((sy+1)*pixrep);
				srcpix= (unsigned long) pr_get(pr, sx, sy);
				switch (sunras.ras_depth) {
				case 1:
					dstpix= srcpix ? fgpix : bgpix;
					break;
				case 8:
					dstpix= srcpix;
					break;
				case 24:
				case 32:
					dstpix= pseudopix(srcpix);
					break;
				}
				for (i= 0; i < pixrep; i++)
					for (j= 0; j < pixrep; j++)
						XPutPixel(image, dx+i, dy+j, dstpix);
			}
		}
		break;
	case RASTER_URORIG:
		for (sy= 0; sy < sunras.ras_height; sy++) {
			for (sx= 0; sx < sunras.ras_width; sx++) {
				int i, j;

				dy= sx*pixrep;
				dx= width-((sy+1)*pixrep);
				srcpix= (unsigned long) pr_get(pr, sx, sy);
				switch (sunras.ras_depth) {
				case 1:
					dstpix= srcpix ? fgpix : bgpix;
					break;
				case 8:
					dstpix= srcpix;
					break;
				case 24:
				case 32:
					dstpix= pseudopix(srcpix);
					break;
				}
				for (i= 0; i < pixrep; i++)
					for (j= 0; j < pixrep; j++)
						XPutPixel(image, dx+i, dy+j, dstpix);
			}
		}
		break;
	}
	pr_destroy(pr);

	return GROBJ_SUCCESS;
}

int
Raster::bldhistcmap(Pixrect *pr)
/*
   Perform a histogram on a 24-bit image and generate an 8-bit
   colormap to display the image on a PseudoColor display.
   The domain of intensity values for each primary (0-255) is
   split into several intervals, with each interval containing
   a roughly equal number of pixels of the image whose primary
   intensity falls within that interval. The average intensity
   value for each interval will be used to display any pixel
   whose intensity is in the interval. The domain of red and
   green intensities is split into eight intervals, and the
   domain of blue intensities is split into four intervals,
   yielding 8*8*4 == 256 possible combinations of the average
   interval intensities of all three primaries. These 256
   combinations are used to fill the 8-bit colormap.
*/
{
	int i, j, lb, alphaskip, npix, x, y;
	struct mpr_data *mpr;
	unsigned char *uc;
	int nred, ngreen, nblue;
	int cell_done;
	int ri, gi, bi;
	int rrem, grem, brem;
	double sum;
	XColor color;

	alphaskip= 1;

	if (redint == (intensity *) 0)
		if ((redint= new intensity[8]) == (intensity *) 0)
			return GROBJ_FAILURE;
	if (greenint == (intensity *) 0)
		if ((greenint= new intensity[8]) == (intensity *) 0)
			return GROBJ_FAILURE;
	if (blueint == (intensity *) 0)
		if ((blueint= new intensity[4]) == (intensity *) 0)
			return GROBJ_FAILURE;

	for (i= 0; i < 256; i++)
		redct[i]= greenct[i]= bluect[i]= 0;

	mpr= mpr_d(pr);
	lb= mpr->md_linebytes;

	/* get a count of the number of pixels with each primary intensity */
	for (y= 0; y < sunras.ras_height; y++) {
		uc= ((unsigned char *) mpr->md_image)+(y*lb)+alphaskip;
		for (x= 0; x < sunras.ras_width; x++, uc+= alphaskip) {
			bluect[(int) *uc++]++;
			greenct[(int) *uc++]++;
			redct[(int) *uc++]++;
		}
	}

	nred= ngreen= nblue= 0;
	for (i= 0; i < 256; i++) {
		if (redct[i] > 0)
			nred++;
		if (greenct[i] > 0)
			ngreen++;
		if (bluect[i] > 0)
			nblue++;
	}

	rrem= grem= brem= sunras.ras_width*sunras.ras_height;
	ri= gi= bi= 0;

	/* less than 8 red primary values in use? */
	if (nred <= 8) {
		j= cell_done= 0;
		redint[j].imin= 0;
		for (i= 0; i < 256; i++) {
			if (redct[i] == 0)
				continue;

			redint[j].imax= i;
			redint[j].ival= i;
			cell_done= 1;
			j++;
			if (j == 8)
				break;
			if (i < 255) {
				redint[j].imin= i+1;
				cell_done= 0;
			}
		}
		if (!cell_done) {
			redint[j].imax= 255;
			redint[j].ival= 0;
			j++;
		}
		for (; j < 8; j++) {
			redint[j].imin= redint[j].imax= 256;
			redint[j].ival= 0;
		}
	}

	else {

		/* split red into eight intervals and determine
		   the average intensity for each interval */
		for (i= 0; i < 8; i++) {
			if ((ri >= 256) || (rrem <= 0))
				break;
			redint[i].imin= ri;
			npix= 0;
			sum= 0.;
			while (npix < rrem/(8-i)) {
				npix+= redct[ri];
				sum+= ri*redct[ri];
				ri++;
			}
			redint[i].imax= ri-1;
			redint[i].ival= (int) ((double) (sum/((double) npix)));
			rrem-= npix;
		}
	}

	/* less than 8 green primary values in use? */
	if (ngreen <= 8) {
		j= cell_done= 0;
		greenint[j].imin= 0;
		for (i= 0; i < 256; i++) {
			if (greenct[i] == 0)
				continue;

			greenint[j].imax= i;
			greenint[j].ival= i;
			cell_done= 1;
			j++;
			if (j == 8)
				break;
			if (i < 255) {
				greenint[j].imin= i+1;
				cell_done= 0;
			}
		}
		if (!cell_done) {
			greenint[j].imax= 255;
			greenint[j].ival= 0;
			j++;
		}
		for (; j < 8; j++) {
			greenint[j].imin= greenint[j].imax= 256;
			greenint[j].ival= 0;
		}
	}

	else {
		/* split green into eight intervals and determine
		   the average intensity for each interval */
		for (i= 0; i < 8; i++) {
			if ((gi >= 256) || (grem <= 0))
				break;
			greenint[i].imin= gi;
			npix= 0;
			sum= 0.;
			while (npix < grem/(8-i)) {
				npix+= greenct[gi];
				sum+= gi*greenct[gi];
				gi++;
			}
			greenint[i].imax= gi-1;
			greenint[i].ival= (int) ((double) (sum/((double) npix)));
			grem-= npix;
		}
	}

	/* less than 4 blue primary values in use? */
	if (nblue <= 4) {
		j= cell_done= 0;
		blueint[j].imin= 0;
		for (i= 0; i < 256; i++) {
			if (bluect[i] == 0)
				continue;

			blueint[j].imax= i;
			blueint[j].ival= i;
			cell_done= 1;
			j++;
			if (j == 4)
				break;
			if (i < 255) {
				blueint[j].imin= i+1;
				cell_done= 0;
			}
		}
		if (!cell_done) {
			blueint[j].imax= 255;
			blueint[j].ival= 0;
			j++;
		}
		for (; j > 4; j++) {
			blueint[j].imin= blueint[j].imax= 256;
			blueint[j].ival= 0;
		}
	}

	else {
		/* split blue into four intervals and determine
		   the average intensity for each interval */
		for (i= 0; i < 4; i++) {
			if ((bi >= 256) || (brem <= 0))
				break;
			blueint[i].imin= bi;
			npix= 0;
			sum= 0.;
			while (npix < brem/(4-i)) {
				npix+= bluect[bi];
				sum+= bi*bluect[bi];
				bi++;
			}
			blueint[i].imax= bi-1;
			blueint[i].ival= (int) ((double) (sum/((double) npix)));
			brem-= npix;
		}
	}

	/* create a colormap containing all possible combinations of the
	   average intensity values for all intervals of all three primaries */
	color.flags= DoRed | DoGreen | DoBlue;
	for (i= 0; i < 256; i++) {
		ri= i/32;
		gi= (i%32)/4;
		bi= i%4;
		color.pixel= i;
		color.red= (unsigned short) (256*redint[ri].ival);
		color.green= (unsigned short) (256*greenint[gi].ival);
		color.blue= (unsigned short) (256*blueint[bi].ival);
		XStoreColor(display, cmap, &color);
	}

	return GROBJ_SUCCESS;
}

unsigned long
Raster::pseudopix(unsigned long src)
{
	unsigned long dst;
	unsigned char *uc;
	int r, g, b, ri, gi, bi;

	/* skip alpha channel */
	uc= (unsigned char *) &src;
	uc++;

	b= (int) *uc++;
	g= (int) *uc++;
	r= (int) *uc;
	for (ri= 0; redint[ri].imax < r; ri++);
	for (gi= 0; greenint[gi].imax < g; gi++);
	for (bi= 0; blueint[bi].imax < b; bi++);
	dst= (ri*32)+(gi*4)+bi;

	return dst;
}

void
Raster::copy(Drawable drw)
{
	float fx, fy;
	int ix, iy;
	Window root;
	unsigned int w, h, bw, d;

	XGetGeometry(display, drw, &root, &ix, &iy, &w, &h, &bw, &d);
	getloc(&fx, &fy, &ix, &iy);
	iy= h-1-iy;
	if ((drawmode == RASTER_FULL) && (image != (XImage *) 0))
		XPutImage(display, drw, fullgc, image, 0, 0, ix, iy, width, height);
	else {
		XDrawLine(display, drw, outlinegc, ix, iy, ix+width-1, iy);
		XDrawLine(display, drw, outlinegc, ix+width-1, iy, ix+width-1, iy+height-1);
		XDrawLine(display, drw, outlinegc, ix, iy+height-1, ix+width-1, iy+height-1);
		XDrawLine(display, drw, outlinegc, ix, iy, ix, iy+height-1);
	}
	return;
}

int
Raster::setpixrep(int r)
{
	if (r <= 0)
		return GROBJ_FAILURE;

	pixrep= r;

	switch (origloc) {
	case RASTER_ULORIG:
	case RASTER_LRORIG:
		width= sunras.ras_width*pixrep;
		height= sunras.ras_height*pixrep;
		break;
	case RASTER_LLORIG:
	case RASTER_URORIG:
		width= sunras.ras_height*pixrep;
		height= sunras.ras_width*pixrep;
		break;
	}
	return GROBJ_SUCCESS;
}

int
Raster::setorigloc(int ol)
{
	switch (ol) {
	case RASTER_ULORIG:
	case RASTER_LRORIG:
		origloc= ol;
		width= sunras.ras_width*pixrep;
		height= sunras.ras_height*pixrep;
		return GROBJ_SUCCESS;
	case RASTER_LLORIG:
	case RASTER_URORIG:
		origloc= ol;
		width= sunras.ras_height*pixrep;
		height= sunras.ras_width*pixrep;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Raster::setdrawmode(int draw)
{
	switch (draw) {
	case RASTER_FULL:
		drawmode= draw;
		return GROBJ_SUCCESS;
	case RASTER_OUTLINE:
		if (drawmode == RASTER_FULL) {
			if (cmap != None) {
				XFreeColormap(display, cmap);
				cmap= None;
			}
			if (image != (XImage *) 0) {
				XDestroyImage(image);
				image= (XImage *) 0;
			}
			delete redint;
			delete greenint;
			delete blueint;
			redint= greenint= blueint= (intensity *) 0;
			if (suncmap.map[0] != (unsigned char *) 0) {
				free((char *) (suncmap.map[0]));
				suncmap.map[0]= (unsigned char *) 0;
			}
		}
		drawmode= draw;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Raster::setfg(int fg, unsigned long fgp, unsigned char r, unsigned char g, unsigned char b)
{
	Pixmap pm;
	XGCValues gcv;

	switch (fg) {
	case RASTER_GLOBALFG:
	case RASTER_BLACKFG:
	case RASTER_WHITEFG:
	case RASTER_OTHERFG:
		break;
	default:
		return GROBJ_FAILURE;
	}

	foreground= fg;
	fgpix= fgp;
	redfg= r;
	greenfg= g;
	bluefg= b;

	if (fullgc != None)
		XFreeGC(display, fullgc);

	/* temporary pixmap is created just so we can create fullgc */
	if ((pm= XCreatePixmap(display, DefaultRootWindow(display), (unsigned) 1, (unsigned) 1, (unsigned) depth)) == None)
		return GROBJ_FAILURE;

	gcv.function= GXcopy;
	gcv.foreground= fgpix;
	gcv.background= bgpix;
	gcv.plane_mask= AllPlanes;
	if ((fullgc= XCreateGC(display, pm, (GCFunction | GCForeground | GCBackground | GCPlaneMask), &gcv)) == None) {
		XFreePixmap(display, pm);
		return GROBJ_FAILURE;
	}
	XFreePixmap(display, pm);
	return GROBJ_SUCCESS;
}

int
Raster::setbg(int bg, unsigned long bgp, unsigned char r, unsigned char g, unsigned char b)
{
	Pixmap pm;
	XGCValues gcv;

	switch (bg) {
	case RASTER_GLOBALBG:
	case RASTER_WHITEBG:
	case RASTER_BLACKBG:
	case RASTER_OTHERBG:
		break;
	default:
		return GROBJ_FAILURE;
	}

	background= bg;
	bgpix= bgp;
	redbg= r;
	greenbg= g;
	bluebg= b;

	if (fullgc != None)
		XFreeGC(display, fullgc);

	/* temporary pixmap is created just so we can create fullgc */
	if ((pm= XCreatePixmap(display, DefaultRootWindow(display), (unsigned) 1, (unsigned) 1, (unsigned) depth)) == None)
		return GROBJ_FAILURE;

	gcv.function= GXcopy;
	gcv.foreground= fgpix;
	gcv.background= bgpix;
	gcv.plane_mask= AllPlanes;
	if ((fullgc= XCreateGC(display, pm, (GCFunction | GCForeground | GCBackground | GCPlaneMask), &gcv)) == None) {
		XFreePixmap(display, pm);
		return GROBJ_FAILURE;
	}
	XFreePixmap(display, pm);
	return GROBJ_SUCCESS;
}

int
Raster::dump(Pixrect *opr, int w, int h)
{
	FILE *fp;
	Pixrect *ipr;
	int ixloc, iyloc;
	float fxloc, fyloc;
	int loadras, mono, pseudocolor;
	struct rasterfile junk;
	unsigned char ucpix[4];
	int fg, bg, srcpix, dstpix;
	int sx, sy, dx, dy;

	switch (sunras.ras_depth) {
	case 1:
		if (image == (XImage *) 0)
			loadras= 1;
		else
			loadras= 0;
		mono= 1;
		pseudocolor= 0;
		ucpix[0]= 0;
		ucpix[1]= bluefg;
		ucpix[2]= greenfg;
		ucpix[3]= redfg;
		bcopy((char *) ucpix, (char *) &fg, sizeof(int));
		ucpix[1]= bluebg;
		ucpix[2]= greenbg;
		ucpix[3]= redbg;
		bcopy((char *) ucpix, (char *) &bg, sizeof(int));
		break;
	case 8:
		if ((depth == 1) || (image == (XImage *) 0))
			loadras= 1;
		else
			loadras= 0;
		mono= 0;
		pseudocolor= 1;
		ucpix[0]= 0;
		break;
	case 24:
	case 32:
		loadras= 1;
		mono= 0;
		pseudocolor= 0;
		break;
	}

	if (loadras) {
		if ((fp= doc_open(filenm, "r")) == (FILE *) NULL)
			return GROBJ_FAILURE;
		if (pr_load_header(fp, &junk) != 0) {
			(void) fclose(fp);
			return GROBJ_FAILURE;
		}
		if ((sunras.ras_maptype != RMT_NONE) && (sunras.ras_maplength > 0)) {
			if (suncmap.map[0] != (unsigned char *) 0)
				free((char *) (suncmap.map[0]));
			suncmap.length= 0;
			suncmap.map[0]= (unsigned char *) 0;
			if (pr_load_colormap(fp, &sunras, &suncmap) != 0) {
				(void) fclose(fp);
				return GROBJ_FAILURE;
			}
		}
		if ((ipr= pr_load_image(fp, &sunras, &suncmap)) == (Pixrect *) 0) {
			(void) fclose(fp);
			return GROBJ_FAILURE;
		}
		(void) fclose(fp);
	}

	getloc(&fxloc, &fyloc, &ixloc, &iyloc);
	switch (origloc) {
	case RASTER_ULORIG:
		for (sy= 0; sy < sunras.ras_height; sy++) {
			for (sx= 0; sx < sunras.ras_width; sx++) {
				int i, j;

				dx= sx*pixrep;
				dy= sy*pixrep;
				if (loadras)
					srcpix= pr_get(ipr, sx, sy);
				else
					srcpix= (int) XGetPixel(image, dx, dy);
				if (mono) {
					if (loadras)
						dstpix= srcpix ? fg : bg;
					else
						dstpix= (srcpix == fgpix) ? fg : bg;
				}
				else if (pseudocolor) {
					ucpix[1]= suncmap.map[2][srcpix];
					ucpix[2]= suncmap.map[1][srcpix];
					ucpix[3]= suncmap.map[0][srcpix];
					bcopy((char *) ucpix, (char *) &dstpix, sizeof(int));
				}
				else
					dstpix= srcpix;
				for (i= 0; i < pixrep; i++) {
					if (ixloc+dx+i >= w)
						break;
					for (j= 0; j < pixrep; j++) {
						if (h-1-iyloc+dy+j >= h)
							break;
						pr_put(opr, ixloc+dx+i, h-1-iyloc+dy+j, dstpix);
					}
				}
			}
		}
		break;
	case RASTER_LLORIG:
		for (sy= 0; sy < sunras.ras_height; sy++) {
			for (sx= 0; sx < sunras.ras_width; sx++) {
				int i, j;

				dy= height-((sx+1)*pixrep);
				dx= sy*pixrep;
				if (loadras)
					srcpix= pr_get(ipr, sx, sy);
				else
					srcpix= (int) XGetPixel(image, dx, dy);
				if (mono) {
					if (loadras)
						dstpix= srcpix ? fg : bg;
					else
						dstpix= (srcpix == fgpix) ? fg : bg;
				}
				else if (pseudocolor) {
					ucpix[1]= suncmap.map[2][srcpix];
					ucpix[2]= suncmap.map[1][srcpix];
					ucpix[3]= suncmap.map[0][srcpix];
					bcopy((char *) ucpix, (char *) &dstpix, sizeof(int));
				}
				else
					dstpix= srcpix;
				for (i= 0; i < pixrep; i++) {
					if (ixloc+dx+i >= w)
						break;
					for (j= 0; j < pixrep; j++) {
						if (h-1-iyloc+dy+j >= h)
							break;
						pr_put(opr, ixloc+dx+i, h-1-iyloc+dy+j, dstpix);
					}
				}
			}
		}
		break;
	case RASTER_LRORIG:
		for (sy= 0; sy < sunras.ras_height; sy++) {
			for (sx= 0; sx < sunras.ras_width; sx++) {
				int i, j;

				dx= width-((sx+1)*pixrep);
				dy= height-((sy+1)*pixrep);
				if (loadras)
					srcpix= pr_get(ipr, sx, sy);
				else
					srcpix= (int) XGetPixel(image, dx, dy);
				if (mono) {
					if (loadras)
						dstpix= srcpix ? fg : bg;
					else
						dstpix= (srcpix == fgpix) ? fg : bg;
				}
				else if (pseudocolor) {
					ucpix[1]= suncmap.map[2][srcpix];
					ucpix[2]= suncmap.map[1][srcpix];
					ucpix[3]= suncmap.map[0][srcpix];
					bcopy((char *) ucpix, (char *) &dstpix, sizeof(int));
				}
				else
					dstpix= srcpix;
				for (i= 0; i < pixrep; i++) {
					if (ixloc+dx+i >= w)
						break;
					for (j= 0; j < pixrep; j++) {
						if (h-1-iyloc+dy+j >= h)
							break;
						pr_put(opr, ixloc+dx+i, h-1-iyloc+dy+j, dstpix);
					}
				}
			}
		}
		break;
	case RASTER_URORIG:
		for (sy= 0; sy < sunras.ras_height; sy++) {
			for (sx= 0; sx < sunras.ras_width; sx++) {
				int i, j;

				dy= sx*pixrep;
				dx= width-((sy+1)*pixrep);
				if (loadras)
					srcpix= pr_get(ipr, sx, sy);
				else
					srcpix= (int) XGetPixel(image, dx, dy);
				if (mono) {
					if (loadras)
						dstpix= srcpix ? fg : bg;
					else
						dstpix= (srcpix == fgpix) ? fg : bg;
				}
				else if (pseudocolor) {
					ucpix[1]= suncmap.map[2][srcpix];
					ucpix[2]= suncmap.map[1][srcpix];
					ucpix[3]= suncmap.map[0][srcpix];
					bcopy((char *) ucpix, (char *) &dstpix, sizeof(int));
				}
				else
					dstpix= srcpix;
				for (i= 0; i < pixrep; i++) {
					if (ixloc+dx+i >= w)
						break;
					for (j= 0; j < pixrep; j++) {
						if (h-1-iyloc+dy+j >= h)
							break;
						pr_put(opr, ixloc+dx+i, h-1-iyloc+dy+j, dstpix);
					}
				}
			}
		}
		break;
	}

	if (loadras)
		pr_destroy(ipr);
	return GROBJ_SUCCESS;
}

int
Raster::icedump(FILE *fp)
{
	float fx, fy;
	int ix, iy;

	if (fp == (FILE *) 0)
		return GROBJ_FAILURE;

	fprintf(fp, "%%%%ICE-Ras: Begin %s\n", filenm);

	getloc(&fx, &fy, &ix, &iy);
	fprintf(fp, "%%%%ICE-Ras: Loc %6.4f %6.4f\n", fx, fy);

	fprintf(fp, "%%%%ICE-Ras: Flags %1d %1d %1d\n", pixrep, origloc, drawmode);

	if (sunras.ras_depth == 1) {
		fprintf(fp, "%%%%ICE-Ras: FG %1d %1d %1d %1d\n", foreground, (int) redfg, (int) greenfg, (int) bluefg);
		fprintf(fp, "%%%%ICE-Ras: BG %1d %1d %1d %1d\n", background, (int) redbg, (int) greenbg, (int) bluebg);
	}

	fprintf(fp, "%%%%ICE-Ras: Seq %1d\n", getsequence());

	fprintf(fp, "%%%%ICE-Ras: IOT %1d\n", getiotag());

	fprintf(fp, "%%%%ICE-Ras: End\n");
	return GROBJ_SUCCESS;
}

Raster::~Raster()
{
	delete filenm;
	if (fullgc != None)
		XFreeGC(display, fullgc);
	if (outlinegc != None)
		XFreeGC(display, outlinegc);
	if (cmap != None)
		XFreeColormap(display, cmap);
	if (image != (XImage *) 0)
		XDestroyImage(image);
	delete redint;
	delete greenint;
	delete blueint;
	if (suncmap.map[0] != (unsigned char *) 0)
		free((char *) (suncmap.map[0]));
}
