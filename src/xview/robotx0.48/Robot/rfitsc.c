#include <stdio.h>
#include <sys/file.h>
#include <unistd.h>

#define BLOCK 2880

static	char	block[BLOCK];
static	char	*imagedata;
static	unsigned short	*shortdata;
static	unsigned int	*intdata;
static	float	*floatdata;
static	double	*doubledata;

static	int	naxis, naxis1, naxis2, naxis3, bitpix;
int
readfits_(filename)
char	*filename;
{
int	read_fits();

	return read_fits(filename);
}


int
read_fits(filename)
char	*filename;
{
    /* char keyword[8]; unused at present */
    int numread, imgsize, file; 
	int get_end();
	int	nheader;	/* No. of BLOCKs of header */

    /* Open the output file */
    if ( (file=open(filename, O_RDONLY)) < 0)
      {
        totext2_("read_fits: unable to access input file");
	totext2_(filename);
        return(0);
      }


     /* initially only want to read in the first 2880 bytes of the header */

		read(file, block, 2880);
	naxis = get_value("NAXIS   ");


    if (naxis > 3)
    {
        (void)fprintf(stderr,"\nread_fits: Sorry, this program \
				is too stupid to cope with \
				%d dimensional images\n", naxis);
        close(file);
        return(0);
    }
	naxis1 = get_value("NAXIS1  ");
	naxis2 = get_value("NAXIS2  ");
	naxis3 = get_value("NAXIS3  ");  
	bitpix = get_value("BITPIX  ");

/* Keep reading header until END is found */
	nheader = 1;
	while (1){
		if(get_end() != 1){
			read(file, block, 2880);
			nheader++;
		}
		else
			break;
	}

/*	printf("nheader = %d\n", nheader); */

/* 	printf("naxis = %d, naxis1 = %d, naxis2 = %d\n", naxis, 
		naxis1, naxis2);  */

    imgsize = naxis1  * (bitpix)/8;
    if(naxis >=2) imgsize = imgsize *  naxis2;  
    if(naxis == 3) imgsize = imgsize *  naxis3; 


       /* Get space for image data */

	imagedata = (char *) calloc(sizeof(char), imgsize);
    if (imagedata == NULL)
    {
        (void)fprintf(stderr,"\nread_fits: Not enough memory for image data.\n");
        (void)fprintf(stderr,"\nread_fits: wanted %d bytes\n", imgsize);

        free((char *) imagedata);
        close(file);
        return(0);
    }

         /* read the image */
	numread = read(file, imagedata, imgsize);
    if (  numread != imgsize)
    {
       (void) fprintf(stderr,"\nread_fits: ");
       (void) fprintf(stderr,"Incorrect byte count reading image data!\n\
				%d bytes read \n \
				%d bytes requested\n", numread, imgsize);
       free(imagedata);
       close(file);
       return(0);
    }

        shortdata = (unsigned short *) imagedata;
        intdata = (unsigned int *) imagedata;
    close(file);
    return(imgsize);
}

/* get_end
 * returns 1 if END is in the block of data
 * else returns 0
 */
int
get_end()
{
	int	i;

	for (i = 0; i < BLOCK; i += 80) {
		if (strncmp(block + i, "END", 3) == 0) {
			return (1);
		}
	}
	/*
	 * end not found
	 */
	return (0);
}


/* get_value
 * returns the value of a specified keyword
 * if the keyword is not found then a value of 0 is returned 
 */
int
get_value(keyword)
	char           *keyword;
{
	int             value;
	int             i;
	/* find keyword */
	for (i = 0; i < BLOCK; i += 80) {
		if (strncmp(block + i, keyword, 8) == 0) {
			sscanf(block + i + 10, "%d", &value);
			return (value);
		}
	}
	/*
	 * well... I guess we didn't find that value - send back a zero (good
	 * choice???)
	 */
	return (0);
}

int
gval_(keyword)
char	*keyword;
{
	if(strcmp(keyword, "NAXIS1") == 0)
		return(naxis1);
	else if(strcmp(keyword, "NAXIS2") == 0)
		return(naxis2);
	else if(strcmp(keyword, "NAXIS") == 0)
		return(naxis);
	else if(strcmp(keyword, "BITPIX") == 0)
		return(bitpix);
	else{
		totext_("Error - can't return that parameter");
		return (-99);
	}
}


/* Get the value of a specified pixel */
double
getpix_(i, j)
int	*i, *j;
{
double	pixel_value;
int	off_set;
unsigned short	*sp;
unsigned int	*inp;
float	*fp;
double	*dp;
unsigned char	*var;

/*	printf(" i = %d, j = %d\n", *i, *j);
	printf("naxis1 = %d, bitpix = %d\n", naxis1, bitpix); */
	off_set = (*i -1) * naxis1 + *j -1;


	if(bitpix == 8){
		var = (unsigned char *) (imagedata + off_set);
		pixel_value = (double) *var;
	}
	else if(bitpix == 16){
		sp =  (shortdata + off_set);
		pixel_value = (double) *sp;
	}
	else if(bitpix == 32){
		inp = (intdata + off_set);
		pixel_value = (double) *inp;

	}
	else if(bitpix == -32){
		fp = (floatdata + off_set);
		pixel_value = (double) *fp;
	}
	else if(bitpix == -64){
		dp = (doubledata + off_set);
		pixel_value = (double) *dp;
	}
	else
		fprintf(stderr, "bitpix of %d can't be dealt with\n",
					bitpix);

	return (pixel_value);
}


/* free memory */

void
freef_()
{
	if(imagedata != NULL) free((char *) imagedata);
}
