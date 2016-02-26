#include <math.h>

/* Math functions that are available under C but not FORTRAN */
void cj0_(x)
float	*x;
{
	*x = (float) j0((double) *x);
}

void cj1_(x)
float	*x;
{
	*x = (float) j1((double) *x);
}

void *cceil_(x)
float	*x;
{

	*x = (float) ceil((double) *x);

}

void *cfloor_(x)
float	*x;
{

	*x = (float) floor((double) *x);

}

void *crint_(x)
float	*x;
{

	*x = (float) rint((double) *x);

}

