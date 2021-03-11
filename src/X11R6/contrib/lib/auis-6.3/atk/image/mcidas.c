/* mcidas.ch - class description for interface from MCIDAS format to image */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

/*
 * McIDAS areafile support.  contributed by Glenn P. Davis
 * (davis@unidata.ucar.edu).
 */

#include <andrewos.h>
#include <image.ih>
#include <mcidas.h>
#include <mcidas.eh>

/*
 * convert from little endian to big endian four byte object
 */
static unsigned long
vhtonl(lend)
unsigned long lend ;
{
	unsigned long bend ;
	unsigned char *lp, *bp ;

	lp = ((unsigned char *)&lend) + 3 ;
	bp = (unsigned char *) &bend ;

	*bp++ = *lp-- ;
	*bp++ = *lp-- ;
	*bp++ = *lp-- ;
	*bp = *lp ;

	return(bend) ;
}


/* ARGSUSED */
int 
mcidas__Ident( classID, fullname )
    struct classheader *classID;
    char *fullname;
{ FILE          *f;
  struct area_dir dir ;
  int             r;

  if(!(f = fopen(fullname,"r"))) {
    perror("mcidasIdent");
    return(0);
  }
  switch (fread((byte *)&dir, sizeof(struct area_dir), 1, f)) {
      case -1:
	  r = 0;
	  break;

      case 1:
	  if (dir.type != 4 && dir.type != 67108864) {
	      r = 0;
	      break;
	  }
	  r = 1;
	  break;

      default:
	  r = 0;
	  break;
  }
  fclose(f);
  return(r);
}


int
mcidas__Load( mcidas, fullname, fp )
    struct mcidas *mcidas;
    char *fullname;
    FILE *fp;
{ FILE          *f;
  struct area_dir  dir;
  struct navigation  nav;
  unsigned int    y;
  int doswap = 0 ;

  if((f = fp) == 0) {
      if (! (f = fopen(fullname, "r"))) {
	  fprintf(stderr, "Couldn't open mcidas file %s.\n", fullname);
	  return(-1);
      }
  }
  switch (fread((byte *)&dir, sizeof(struct area_dir), 1, f)) {
      case -1:
	  perror("mcidasLoad");
	  fclose(f);
	  return(-1);

      case 1:
	  if(dir.type != 4) {
	      if(dir.type != 67108864) {
		  fclose(f);
		  return(-1);
	      } else {
		  doswap = 1;
	      }
	  }
	  break;

      default:
	  fclose(f);
	  return(-1);
  }

  if(doswap) {
    unsigned long *begin; 
    unsigned long *ulp;
    begin = (unsigned long*) &dir;
    for(ulp = begin; ulp < &begin[AREA_COMMENTS]; ulp++)
       *ulp = vhtonl(*ulp);
     for(ulp = &begin[AREA_CALKEY]; ulp < &begin[AREA_STYPE]; ulp++)
        *ulp = vhtonl(*ulp);
   }

  /* skip the nav */
  if (fread((byte*) &nav, sizeof(struct navigation), 1, f) != 1) {
      fclose(f);
      return(-1);
  }

  /* get an image to put the data in
   */

   mcidas_newRGBImage(mcidas,
		      dir.esiz,
		      dir.lsiz,
		      8 * dir.bands);

  /* set up the colormap, linear grey scale
   */

    for (y = 0; y < 255; y++) {
	mcidas_RedPixel(mcidas, y) = 
	  mcidas_RedPixel(mcidas, y) =
	  mcidas_RedPixel(mcidas, y)  = y * 257;
    }
    mcidas_RGBUsed(mcidas) = 255;

  fread(mcidas_Data(mcidas), sizeof(byte), dir.esiz * dir.lsiz, f);

  fclose(f);
  return(0);
}

long
mcidas__Read( self, file, id )
    struct mcidas *self;
    FILE *file;
    long id;
{
    if(mcidas_Load(self, NULL, file) == 0)
	return(dataobject_NOREADERROR);
    else
	return(dataobject_BADFORMAT);
}

long
mcidas__Write( self, file, writeID, level )
    struct mcidas *self;
    FILE *file;
    long writeID;
    int level;
{
    return(super_Write(self, file, writeID, level));
}

long
mcidas__WriteNative( self, file, filename )
    struct mcidas *self;
    FILE *file;
    char *filename;
{
return(0);
}
