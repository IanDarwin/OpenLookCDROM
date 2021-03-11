/*
 * Hacked fcvt() that provides just the functionalty required
 * by `table'
 */

char *
fcvt(value, ndigit, decpt, sign)
     double value;  /* value to be converted to a string */
     int ndigit; /* digits of precision */
     int *decpt; /* where is the decimal point, 314 & 1 -> 3.14 */
     int *sign; /* it was netative */
{
  *decpt = 0;
  *sign = 0;
  if (isnan(value))
    return "NaN";
  else if (isinf(value))
    return "Inf";
  else
    return "0";
}
