/*
*
	grade routine 
*
*	input:	handle[9]   array of pulse heights,
*                           0:center, 1-8:surrounding pixels
*               split       split threshold
*	output:	*sumph      total ph
*               *type       grade
*               *above      numbers of pixels summed in *sumph
*
*		version 3.0	91-04-23	by K.Yoshida
*		version 3.1	91-05-01	by K.Yoshida
*		slight mods 	91-11		Robin Corbet
*		(to simplfy call from FORTRAN)
*/
static lookup[256] = {  0,1,2,4,1,1,4,7,3,4,5,6,3,4,7,7,
			3,3,5,7,4,4,6,7,7,7,7,7,7,7,7,7,
			1,1,2,4,1,1,4,7,4,7,7,7,4,7,7,7,
			3,3,5,7,4,4,6,7,7,7,7,7,7,7,7,7,
			2,2,7,7,2,2,7,7,5,7,7,7,5,7,7,7,
			5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			4,4,7,7,4,4,7,7,6,7,7,7,6,7,7,7,
			7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			1,1,2,4,1,1,4,7,3,4,5,6,3,4,7,7,
			4,4,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			1,1,2,4,1,1,4,7,4,7,7,7,4,7,7,7,
			4,4,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			4,4,7,7,4,4,7,7,7,7,7,7,7,7,7,7,
			6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7  };


/* normal mode */
void classify_(handle, split, sumph, type, above)
int handle[9];
int *split, *sumph, *type, *above;
{
	int i, n;
	n = 0;

        for (i = 1; i < 9; i++)
        {
		 /*printf("handle[%d] = %d \n", i,handle[i] ); */
		if( handle[i] >= *split ){
			n += power(2, i-1);
			/* printf("%d ", power(2, i-1) ); */
		}
	}
	/* printf("n = %d\n", n); */
	/* *type = toGrade(n); */
	*type = lookup[n];

	*sumph = sum(split, above, handle, type);

/*	return *type; */
}

/*	power	*/
power(base, n)
int base, n;
{
	int i, p;

	p = 1;
	for(i = 1; i <= n; ++i){
		p = p*base;
	}
	return p;
}

/*	return grade	*/
/* This function is not in use.
toGrade(n)
int n;
{
	int grade;

	switch (n) {
		case 0:
			grade=0;
			break;
		case 1: case 4: case 5: case 32: case 33: case 36: case 37:
		case 128: case 129: case 132: case 133: case 160: case 161:
		case 164: case 165:
			grade=1;
			break;
		case 2: case 34: case 64: case 65: case 68: case 69: case 130:
		case 162:
			grade=2;
			break;
		case 8: case 12: case 16: case 17: case 48: case 49: case 136:
		case 140:
			grade=3;
			break;
		case 3: case 6: case 9: case 13: case 20: case 21: case 35:
		case 38: case 40: case 44: case 52: case 53: case 96: case 97:
		case 100: case 101: case 131: case 134: case 137: case 141:
		case 144: case 145: case 163: case 166: case 168: case 172:
		case 176: case 177: case 192: case 193: case 196: case 197:
			grade=4;
			break;
		case 10: case 18: case 50: case 72: case 76: case 80: case 81:
		case 138:
			grade=5;
			break;
		case 11: case 22: case 54: case 104: case 108: case 139:
		case 208: case 209:
			grade=6;
			break;
		default:
			grade=7;
			break;
	}

	return grade;
}
*/


/*
*	Sum PHs except for corner pixels
*		In the case of grade6, add a corner pixel
*/
int sum(split, above, handle, type) 
int *split, *above;
int handle[9];
int *type;
{
	int sumph;
	int i;

	*above = 1;
	sumph = handle[0];


	for (i = 2; i < 8; i++)  /* sum ph over split_thres. */
	{
	        if ( handle[i] >= *split ){
			 /* judge pixels over split_threshold */
			switch (i) {
				case 2: case 4: case 5: case 7:
					sumph += handle[i];
					(*above) ++;
					break;
				default:
					break;
			}
		}
	}
	if( 6==*type ){
		if( handle[1] >= *split )
		{
			if( (handle[2]>=*split) && (handle[4]>=*split) )
			{
				(*above)++;
				sumph += handle[1];
			}
		}
		if( handle[3] >= *split )
		{
			if( (handle[2]>=*split) && (handle[5]>=*split) )
			{
				(*above)++;
				sumph += handle[3];
			}
		}
		if( handle[6] >= *split )
		{
			if( (handle[4]>=*split) && (handle[7]>=*split) )
			{
				(*above)++;
				sumph += handle[6];
			}
		}
		if( handle[8] >= *split )
		{
			if( (handle[5]>=*split) && (handle[7]>=*split) )
			{
				(*above)++;
				sumph += handle[8];
			}
		}
	}

	return sumph;
}

/* fast mode */
int fastclassify_event(handle, split, sumph, type, above_split)
short int *handle[3];
int split, *sumph, *type, *above_split;
{
        int i;

        *above_split = 1;
        *sumph = *(handle[0]) ;
        for (i = 1; i < 3; i++)
        {
	    if (*(handle[i]) >= split)  /*judge pixels over split_threshold */
	    {    
	        (*above_split)++;
	        *sumph += *(handle[i]);   /* sum ph over split_threshold */
	    }
        }
	if (*above_split <= 1) 
		*type = 0;
	else if (*above_split == 2) 
		*type = 1;
	else if (*above_split == 3) 
		*type = 2;

	return *type;
}
