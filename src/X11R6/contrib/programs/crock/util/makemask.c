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
 * A try at reading in sun raster files and seeing if we can automatically
 * build up an image mask.
 *
 * Note: this is not part of crock itself, it is a tool used to generate
 *       the bitmap masks when generating characters for crock.
 */

#include <stdio.h>
#include <rasterfile.h>

struct colormap {
  unsigned char red, green, blue;
}

main (argc, argv)
int argc;
char **argv;
{
  int i;
  struct rasterfile header1, header2;
  struct colormap *map1, *map2;
  unsigned char *data1, *data2;

  if (argc != 5) {
    fprintf(stderr, "Usage: %s file reffile outfile epsilon\n", *argv);
    exit (1);
  }

  readrast(argv[1], &header1, &map1, &data1);
  readrast(argv[2], &header2, &map2, &data2);


  /* do the comparison */
  if (header1.ras_maplength != 0) {
    /* for color */
    for (i = 0; i < header1.ras_length; i++) {
      if (isclose(map1, data1, map2, data2, i, atoi(argv[4]), 
                  header1.ras_width)) {
        data1[i] = 0;
      } else {
        data1[i] = 255;
      }
    } 
  } else if (header1.ras_depth == 24 || header1.ras_depth == 32) {
    /* for 24 bit color */
    for (i = 0; i < header1.ras_length; i+=3) {
      if (header1.ras_depth == 32) 
        /* skip first byte if 32 bitplane mode */
        i++;
      if (isclose24(data1, data2, i, atoi(argv[4]))) {
        data1[i] = (unsigned char) 0;
        data1[i+1] = (unsigned char) 0;
        data1[i+2] = (unsigned char) 0;
      } else {
        data1[i] = (unsigned char) 255;
        data1[i+1] = (unsigned char) 255;
        data1[i+2] = (unsigned char) 255;
      }
    }
  } else {
    fprintf(stderr, "monochrome is not supported\n");
    exit(0);
  }
  
  (void) fprintf(stderr, "Writing out raster file..");
  writerast(argv[3], header1, map1, data1);
  (void) fprintf(stderr, "done.\n"); 
}

isclose24(data1, data2, i, epsilon, width)
unsigned char   *data1, *data2;
int i, epsilon, width;
{
  if (check24(data1, data2, i, i, epsilon)) return 1;
  if (check24(data1, data2, i, i-3, epsilon)) return 1;
  if (check24(data1, data2, i, i+3, epsilon)) return 1;
  if (check24(data1, data2, i, i-3*width, epsilon)) return 1;
  if (check24(data1, data2, i, i+3*width, epsilon)) return 1;
  return 0;
}

check24(data1, data2, i, j, epsilon)
unsigned char   *data1, *data2;
int i, j,  epsilon;
{
  if (j<0) return 0;
  if (abs(data1[i] - data2[j]) < epsilon &&
      abs(data1[i+1] - data2[j+1]) < epsilon &&
      abs(data1[i+2] - data2[j+2]) < epsilon) {
    return 1;
  }
  return 0;
}

isclose(map1, data1, map2, data2, i, epsilon)
struct colormap *map1;
unsigned char   *data1;
struct colormap *map2;
unsigned char   *data2;
int i, epsilon;
{
  if (abs(map1[data1[i]].red - map2[data2[i]].red) < epsilon &&
      abs(map1[data1[i]].green - map2[data2[i]].green) < epsilon &&
      abs(map1[data1[i]].blue  - map2[data2[i]].blue) < epsilon) {
    return 1;
  }
  return 0;
}

readrast(filename, header, map, data)
char *filename;
struct rasterfile *header;
struct colormap **map;
unsigned char **data;

{
  int status, count, i, j;
  FILE *fp1;
  struct colormap *map1;
  unsigned char *data1, *rle;

  if ((fp1 = fopen(filename, "r")) == NULL) {
    (void) fprintf (stderr, "Can't read file %s\n", filename);
    exit(1);
  }

  /* read in the header */
  if ((status = fread(header, sizeof(struct rasterfile), 1, fp1)) == 0) {
    (void) fprintf(stderr, "Error reading header\n");
    exit(1);
  }

  if (header->ras_magic != RAS_MAGIC) {
    (void) fprintf(stderr, "Error, file is not a raster file. \n");
    exit (1);
  }

  if (header->ras_maptype == RMT_RAW || header->ras_maplength == 0) {
    (void) fprintf(stderr, "Watch out, no color map.  Maplength = %d type = %d.\n",
		   header->ras_maplength, header->ras_maptype);
  } else {
    /* allocate space for the color map */
    if ((map1 = (struct colormap *) malloc(sizeof(struct colormap) * 
					   header->ras_maplength)) == NULL) {
      perror("Can't malloc map1");
      exit(1);
    }

    /* read in the color map */
    if (header->ras_maptype == RMT_EQUAL_RGB) {
      for (i = 0; i < header->ras_maplength / 3; i++) {
        if ((status = fread(&map1[i], sizeof(struct colormap), 1, fp1)) == 0) {
	  (void) fprintf(stderr, "Error reading in map \n");
	  exit (1);
        }
      } 
    } else {
      (void) fprintf (stderr, "Colormap type is not RMT_EQUAL_RGB!!!\n");
      exit (1);
    }
  }
  /* allocate space for the data (with margin for error */
  if ((data1 = (unsigned char *) malloc(2 * header->ras_length)) == NULL) {
    perror("Can't malloc data");
    exit(1);
  }

  /* read in the data */
  if ((status = fread(data1, sizeof (unsigned char), 
		      header->ras_length, fp1)) != header->ras_length) {
    (void) fprintf(stderr, "Error reading in data.\n");
    exit(1);
  }
  

  if (header->ras_type == RT_BYTE_ENCODED) {
    /* read in the rle encoded data */
    rle = data1;

    /* allocate a buffer of the proper size for the data */
    if ((data1 = (unsigned char *) 
	 malloc(sizeof (unsigned char *) * header->ras_width * 
		header->ras_height)) == NULL) {
      perror("Can't malloc data");
      exit(1);
    }

    /* go through and expand the RLE data */
    i = j = 0;
    while (i < header->ras_length) {
      if (rle[i] != 0x80) {
	/* record is 1 byte long */
	data1[j++] = rle[i++];
      } else {
	if (rle[++i] == 0) {
	  /* record is 2 bytes long, literal 0x80 */
	  data1[j++] = 0x80;
	  i++;
	} else {
	  /* record is 3 bytes long, 2nd byte is count-1, 3rd is value */
	  count = rle[i++];
	  for (; count >= 0; count--) {
	    data1[j++] = rle[i];
	  }
	  i++;
	}	/* end if rle == 0 */
      }		/* end if rle != 0x80 */
    }		/* end while i < ras_length */
    header->ras_type = RT_STANDARD;
    header->ras_length = j;
  }		/* end if ras_type == BYTE ENCODED */

  (void) fclose(fp1);
  *map = map1;
  *data = data1;
}

writerast(filename, header, map1, data1)
char *filename;
struct rasterfile header;
struct colormap *map1;
unsigned char *data1;

{
  int status, i;
  FILE *fp1;

  if ((fp1 = fopen(filename, "w")) == NULL) {
    (void) fprintf (stderr, "Can't write file %s\n", filename);
    exit(1);
  }

  /* write out the header */
  if (fwrite(header, sizeof(struct rasterfile), 1, fp1) == 0) {
    (void) fprintf(stderr, "Error writing header\n");
    exit(1);
  }

  if (header.ras_maplength != 0) {
    /* write out the color map */
    for (i = 0; i < header.ras_maplength / 3; i++) {
      if (fwrite(&map1[i], sizeof(struct colormap), 1, fp1) == 0) {
        (void) fprintf(stderr, "Error writing out map \n");
        exit (1);
      }
    }
  }

  /* write out the data */
  if (fwrite(data1, sizeof (unsigned char), header.ras_length, fp1) != 
      header.ras_length) {
    (void) fprintf(stderr, "Error writing out data.\n");
    exit(1);
  }
  (void) fclose(fp1);
}

  
