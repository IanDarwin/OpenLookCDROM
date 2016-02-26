
/*
 *
 *    The X Consortium, and any party obtaining a copy of these files from
 *    the X Consortium, directly or indirectly, is granted, free of charge, a
 *    full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *    nonexclusive right and license to deal in this software and
 *    documentation files (the "Software"), including without limitation the
 *    rights to use, copy, modify, merge, publish, distribute, sublicense,
 *    and/or sell copies of the Software, and to permit persons who receive
 *    copies from any such party to do so.  This license includes without
 *    limitation a license to do the foregoing actions under any patents of
 *    the party supplying this software to the X Consortium.
 */

   XStandardColormap \f(CWbest_map_info\fP;
   float red, green, blue;
   unsigned long pixelvalue;
   int status;

.XX "XGetStandardColormap, example using"
   status = XGetStandardColormap(display, RootWindow(display,
      screen), &best_map_info, XA_RGB_BEST_MAP);

   if (!status)
      {
      printf("%s: specified standard colormap not available", argv[0]);
      exit(-1);
      }

   pixelvalue = \f(CWbest_map_info\fP.base_pixel +
      ((unsigned long)(0.5 + (red * \f(CWbest_map_info\fP.red_max)) *
           \f(CWbest_map_info\fP.red_mult) + 
      ((unsigned long)(0.5 + (green * \f(CWbest_map_info\fP.green_max)) * 
           \f(CWbest_map_info\fP.green_mult) +
      ((unsigned long)(0.5 + (blue * \f(CWbest_map_info\fP.blue_max)) *
           \f(CWbest_map_info\fP.blue_mult);
