
/*  @(#)scan_compress.c 1.2 90/04/02
 *
 *  Routines for uncompressing a scanned file, which has been compressed
 *  using the CCITT Recommendation T.4 (modified Huffman code).
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me, then an attempt will be made to fix them.
 */

#include <stdio.h>
#include "scan.h"
#include "scan_extern.h"


initialise_white(filename)    /* Read white Make Up and Terminating Codes. */
char filename[MAXLINE] ;
{
  char bitstring[MAXLINE] ;     /* This lines huffman encoded bit string. */
  char line[MAXLINE] ;          /* Current line read from file. */
  int bvalue, i, indexval, n, runlength ;
  struct code *ptr ;            /* Current codes structure pointer. */

  for (i = 0; i < 16; i++)
    {
      whites[i] = (struct code *) LINT_CAST(malloc(sizeof(struct code))) ;
      whites[i]->next[0] = NULL ;
      whites[i]->next[1] = NULL ;
    }

  if ((rd = fopen(filename,"r")) == NULL)
    {
      FPRINTF(stderr, "%s: can't open %s\n", progname, filename) ;
      exit(1) ;
    }

  while (fgets(line, MAXLINE, rd) != NULL)
    {
      SSCANF(line, "%d %s", &runlength, bitstring) ;
      n = 0 ;
      indexval = 0 ;
      for (i = 0; i < 4; i++)
        {
          bvalue = bitstring[n++] - '0' ;
          indexval = (indexval << 1) + bvalue ;
        }
      ptr = whites[indexval] ;
      for (i = n ; i < strlen(bitstring); i++)
        {
          if (ptr->next[bvalue] == NULL)
            {
              ptr->next[bvalue] = (struct code *) LINT_CAST(malloc(sizeof(struct code))) ;
              ptr->next[bvalue]->next[0] = NULL ;
              ptr->next[bvalue]->next[1] = NULL ;
            }
          ptr = ptr->next[bvalue] ;
          bvalue = bitstring[i] - '0' ;
        }
      ptr->value[bvalue] = runlength ;
    }
  FCLOSE(rd) ;
}


initialise_black(filename)    /* Read black Make Up and Terminating Codes. */
char filename[MAXLINE] ;
{
  char bitstring[MAXLINE] ;     /* This lines huffman encoded bit string. */
  char line[MAXLINE] ;          /* Current line read from file. */
  int bvalue, i, indexval, n, runlength ;
  struct code *ptr ;            /* Current codes structure pointer. */

  for (i = 0; i < 4; i++)
    {
      blacks[i] = (struct code *) LINT_CAST(malloc(sizeof(struct code))) ;
      blacks[i]->next[0] = NULL ; 
      blacks[i]->next[1] = NULL ; 
    }
 
  if ((rd = fopen(filename,"r")) == NULL)
    {
      FPRINTF(stderr, "%s: can't open %s\n", progname, filename) ;
      exit(1) ;
    } 
 
  while (fgets(line, MAXLINE, rd) != NULL)
    {
      SSCANF(line, "%d %s", &runlength, bitstring) ;
      n = 0 ;
      indexval = 0 ;
      for (i = 0; i < 2; i++)
        {
          bvalue = bitstring[n++] - '0' ;
          indexval = (indexval << 1) + bvalue ;
        }
      ptr = blacks[indexval] ;
      for (i = n ; i < strlen(bitstring); i++)
        {
          if (ptr->next[bvalue] == NULL)
            {
              ptr->next[bvalue] = (struct code *) LINT_CAST(malloc(sizeof(struct code))) ;
              ptr->next[bvalue]->next[0] = NULL ;
              ptr->next[bvalue]->next[1] = NULL ;
            }
          ptr = ptr->next[bvalue] ;
          bvalue = bitstring[i] - '0' ;
        } 
      ptr->value[bvalue] = runlength ;
    }   
  FCLOSE(rd) ;
}


get_bitval()        /* Get next bit value from compressed scanned file. */
{
  if (rcount == 8)
    {
      if ((rc = getc(rd)) == EOF)
        {
          finished = 1 ;
          return(-1) ;
        }
      rcount = 0 ;
    }
  return((rc >> (7-rcount++)) & 1) ;
}


get_code(color)        /* Get next huffman code for this color. */
int color ;
{
  int bvalue, i, indexval ;
  struct code *ptr ;

  indexval = 0 ;
  for (i = 0; i < color; i++)
    {
      if ((bvalue = get_bitval()) == -1) return(-1) ;
      indexval = (indexval << 1) + bvalue ;
    }
  if (color == WHITE) ptr = whites[indexval] ;
  else ptr = blacks[indexval] ;
  for (;;)
    {
      if (ptr->next[bvalue] == NULL) return(ptr->value[bvalue]) ;
      ptr = ptr->next[bvalue] ;
      if ((bvalue = get_bitval()) == -1) return(-1) ;
    }
}


write_code(color, length)   /* Send uncompressed data to temporary file. */
int color, length ;
{
  int i ;

  for (i = 0; i < length; i++)
    {
      if (color == BLACK) wc |= (1 << 7 - wcount) ;
      if (++wcount == 8)
        {
          PUTC(wc, wd) ;
          wc = 0 ;
          wcount = 0 ;
        }
    }
}


uncompress(filename)     /* Uncompress the scanned file using huffman codes. */
char filename[MAXLINE] ;
{
  int rem, runlength ;

  finished = 0 ;
  if ((rd = fopen(filename, "r")) == NULL)
    {
      FPRINTF(stderr, "%s: can't open %s\n", progname, filename) ;
      exit(1) ;
    }
  SPRINTF(finalimage, "/usr/tmp/%s.unc.image", mktemp("XXXXXX")) ;
  if ((wd = fopen(finalimage, "w")) == NULL)
    {
      FPRINTF(stderr, "%s: can't open %s\n", progname, finalimage) ;
      exit(1) ;
    }

  width = 0 ;
  height = 0 ;
  rcount = 8 ;            /* Force a character read. */
  wcount = 0 ;
  do
    {
      runlength = get_code(WHITE) ;
      if (runlength != -1) width += runlength ;
      if (finished) break ;
      if (runlength == -1)
        {
          height++ ;
          rem = width % 16 ;
          if (rem) write_code(WHITE, rem) ;
          fwidth = width + rem ;
          if (!finished) width = 0 ;
          rcount = 8 ;
          continue ;
        }
      write_code(WHITE, runlength) ;
      if (runlength >= 64)
        {
          runlength = get_code(WHITE) ;
          if (runlength != -1) width += runlength ;
          write_code(WHITE, runlength) ;
        }

      runlength = get_code(BLACK) ;
      if (runlength != -1) width += runlength ;
      if (finished) break ;
      if (runlength == -1)
        {
          height++ ;
          rem = width % 16 ;
          if (rem) write_code(WHITE, rem) ;
          fwidth = width + rem ;
          if (!finished) width = 0 ;
          rcount = 8 ;
          continue ;
        }
      write_code(BLACK, runlength) ;
      if (runlength >= 64)
        {
          runlength = get_code(BLACK) ;
          if (runlength != -1) width += runlength ;
          write_code(BLACK, runlength) ;
        }
    }
  while (!finished) ;
  FCLOSE(rd) ;
  FCLOSE(wd) ;
  if (!make_header(fwidth, height))     /* Write out Sun rasterfile header. */
    {
      SPRINTF(line, "cat %s %s > %s", temphead, finalimage, picname) ;
      SYSTEM(line) ;                    /* Create Sun rasterfile. */
      UNLINK(temphead) ;                /* Remove header file. */
    }
  UNLINK(tempimage) ;
  UNLINK(finalimage) ;
}
