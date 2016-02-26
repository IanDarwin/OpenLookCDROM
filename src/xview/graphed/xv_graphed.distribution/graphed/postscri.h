/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Uwe Scnieders, Michael Himsolt	*/
typedef enum{landscape,portrait} Orientierung ;

extern int	post_help_compute_string_length_from_pixrect();

extern int	post_dateikopf();

extern int	post_dateiende();

extern int	post_help_open_output();

extern int	post_help_close_output();

extern void	post_dot();

extern void	post_moveto();

extern void	post_lineto();

extern void	post_vector();

extern void	post_font();

extern void	post_show();

extern void	post_read_image();

extern void	post_image();

extern void	post_ellipse();

extern void	post_box();

extern void	post_closepath();

extern void	post_newpath();

extern void	post_stroke();

extern void	post_gsave();

extern void	post_grestore();

extern void	post_scale();

extern void	post_translate();

extern void	post_rotate();

extern void	post_setlinewidth();

extern void	post_showpage();

extern void	post_def();

extern void	post_call_proc();

extern void	post_setdash();

extern void	post_def_string();

extern void	post_write_image();
