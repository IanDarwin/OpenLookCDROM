#include "fractool.h"


extern double step, sines[], sin();
extern double cos();
extern int fastmath;

char *getenv();


init_sines()
{
	int i;
	float theta;
	char *envptr;

	fastmath = 1;
	envptr = getenv("FASTMATH");
	if (envptr != 0 &&
	    (*envptr == 't' || *envptr == 'T'))
		return(0);

	fastmath = 0;
	step = ((double)PI_OVER_2) / ((double)(N_SINES-1));
	theta = 0;
	for (i = 0; i<N_SINES; i++) {
		sines[i] = sin(theta);
		theta += step;
	}
}



double my_sin(theta)
double theta;
{
	double norm_theta, table_value;

	if (fastmath)
		return(sin(theta));

	while (theta < 0)
		theta += TWO_PI;
	while (theta >= TWO_PI)
		theta -= TWO_PI;

	if (theta > THREE_PI_OVER_2)
		norm_theta = TWO_PI- theta;
	else if (theta > PI)
		norm_theta = theta - PI;
	else if (theta > PI_OVER_2)
                norm_theta = PI - theta;
	else norm_theta = theta;

	table_value = sines[(int)(norm_theta/step)];

	if (theta < PI)
		return(table_value);
	else
		return(0 - table_value);
}


double my_cos(theta)
double theta;
{
	double pi_over_2, new_theta, stuff_over_step;
	int index;

        if (fastmath) 
                return(cos(theta));

	pi_over_2 = PI_OVER_2;

        while (theta < 0)
                theta += TWO_PI;
        while (theta >= TWO_PI)
                theta -= TWO_PI;

        if (theta > THREE_PI_OVER_2)
		return(sines[(int)((theta - THREE_PI_OVER_2)/step)]);
	else if (theta > PI)
		return(0 - sines[(int)((THREE_PI_OVER_2 - theta)/step)]);
	else if (theta > PI_OVER_2)
		return(0 - sines[(int)((theta - PI_OVER_2)/step)]);
	else {
		new_theta = pi_over_2 - theta;
		stuff_over_step = new_theta / step;
		index = (int)stuff_over_step;
		return(sines[(int)((PI_OVER_2 - theta)/step)]);
	}
}

