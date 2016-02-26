
void swapint(I1,I2)
int *I1,*I2;
{
   int Temp;
  Temp = *I1;
  *I1 = *I2;
  *I2 = Temp;
} /* procedure Swapint */

void swapreal(R1,R2)
double *R1,*R2;
{ 
  double Temp;

  Temp = *R1;
  *R1 = *R2;
  *R2 = Temp;
} /* procedure Swapreal */
