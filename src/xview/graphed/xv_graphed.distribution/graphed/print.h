/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef PRINT_HEADER
#define PRINT_HEADER

typedef enum {
	OUTPUT_POSTSCRIPT,
	OUTPUT_RASTERFILE,
	OUTPUT_NECP6
}
	Print_device;

typedef enum { gesamt,
	       autoteilen 
	     } Druckmodus;


extern	Pixrect *pixrect_paint_graph_in_rect ();

typedef   struct  WI_OUT_STR  {
                      int        width,length,
                                 count;
                      char       dec1,dec2,dec3;
                      char*      name;
                    } wi_out_str;


#endif
