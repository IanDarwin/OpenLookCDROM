/*
 * fcvt() implemented by calling its replacemnt fconvert()
 */
char *
fcvt(value, ndigit, decpt, sign)
     double value;  /* value to be converted to a string */
     int ndigit; /* digits of precision */
     int *decpt; /* where is the decimal point, 314 & 1 -> 3.14 */
     int *sign; /* it was netative */
{
	static char fcvt_buff[400 /* 310+max(0,ndigit)*/];
	return fconvert(value, ndigit, decpt, sign, fcvt_buff);
}
