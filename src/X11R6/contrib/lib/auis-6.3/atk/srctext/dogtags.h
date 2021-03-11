/* File dogtags.h created by R S Kemmetmueller
   COPYRIGHT IBM CORP 1993
   LICENSED MATERIAL - PROGRAM PROPERTY OF IBM
   REFER TO COPYRIGHT INSTRUCTIONS FORM NO. G120-2083

   dogtags, an automatic 'stamper' for files as they're loaded in. */

/* INSTRUCTIONS:  Just #include this file, and link your .do with dogtags.o.  Then, after a successful call to text_Read(), call dogtags_substitute(struct text *) to roll through your file looking for special dogtags and substituting the appropriate information.  (Dogtags are delimited by <@ and @>, and have no respect for strings or language syntax, by design).  See source code in dogtags.c, and the dogtags.help help file for a list of recognized dogtag names, and available features such as optional right-justification and centering of dogtag contents. */
extern void dogtags_substitute();
extern void dogtags_substituteregion();
