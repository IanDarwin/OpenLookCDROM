/* (C) Universitaet Passau 1986-1991 */

typedef	struct {
	int	grid;
	int	grid_defaults;
}
	Bends_settings;

extern	Bends_settings	bends_settings;
extern	void		save_bends_settings ();

extern	void	call_call_bends ();



