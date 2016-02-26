/* (C) Universitaet Passau 1986-1991 */
/************************************************************************/
/*									*/
/*	fileselect.h		headerfile to fileselector		*/
/*				library module				*/
/*									*/
/*		April 1990,	by Lamshoeft Thomas			*/
/*									*/
/************************************************************************/


#ifndef	FILESELECT_HEADER
#define FILESELECT_HEADER

/************************************************/
/*		KONSTANTEN			*/
/************************************************/

#define	WDLEN	1024	/* length of working directory string */
#define	FNLEN	128	/* length of working filename string */
#define	MASKNO	14	/* max. number of mask entries */
#define	SILEN	64	/* length of fileselector info string */
#define	UCLEN	80	/* length of user extension string */

/************************************************/
/*	type declarations			*/
/*						*/
/*	Undocumented parts of the declaration	*/
/*	are not guaranteed to be supported in	*/
/*	further releases! (those 'internal_..')	*/
/*	Documented parts only use with care!	*/
/************************************************/

typedef struct fls_record {

	char			cwd[WDLEN],		/* current working directory */
				cwd_old[WDLEN],		/* backup of cwd; used to hold enter state of fileselector */
				filename[FNLEN],	/* filename */
				fileinput[FNLEN],	/* file-input-string */
				select_info[SILEN],	/* select_frame's label */
				*extension[MASKNO];	/* extension-buttons */
				
	int			*internal_pa_f,
				*internal_fls_f,
				
				*internal_co_p,
				*internal_ex_p,
				*internal_sh_p,
				
				*internal_sc_b,
				
				*internal_dr_i,
				*internal_f_i,
				*internal_fin_i,
				*internal_me_i,
				*internal_ed_i,	 
				*internal_ex_c,	 
				*internal_so_c,	 
				*internal_ls_i; 

	struct {
	   int	internal_se,
		internal_ms;
		}		internal_ti;
	
	int			*internal_qu_pr,	 
				*internal_usp_pr;

	struct {
	   int	*internal_pa_i,
	   	internal_num,
	   	*internal_nam,
	   	*internal_suc;
	   	}		internal_spl_rc;		 
	
	int			*internal_spl, 
				internal_la_ex,
				internal_fi_ex,
				internal_sh_pf,
				internal_dr_c,	 
				ok_implies_close,	/* set FALSE if fileselector should not disappear when
							   pushing 'O.k.'-button */
				window_open,		/* TRUE while fileselector is on screen */
				auto_destroy;	 	/* if TRUE, then 'this_item' is released when leaving
							   the fileselector. default: FALSE */

				
	struct fls_record	*this_item;	 
	
	} *Fs_item;

/************************************************/
/*	exported procedures/functions		*/
/************************************************/

extern	Fs_item	fls_create();
extern	void	fls_close();
extern	void	fls_destroy();

extern	void	fls_set_user_panel_items_create_proc();

extern	void	fls_copy_attributes();
extern	void	fls_set_auto_destroy();
extern	void	fls_set_extension();
extern	void	fls_set_current_extension();
extern	void	fls_set_working_directory();
extern	void	fls_set_default_filename();
extern	void	fls_set_info();

extern	void	fileselect();
extern	int	fls_busy();
extern	void	fls_setup_from_file();
extern	void	fls_write_to_file();


/************************************************************************/
/*									*/
/*	REMARK : As the application of fls_set_..-procedures may	*/
/*		 produce unusual effects while the fileselector is	*/
/*		 active, never use them while fls_busy() is TRUE	*/
/*									*/
/************************************************************************/
/*									*/
/*	Fs_item	fls_create()						*/
/*									*/
/*	Creates a fileselector-record and sets its default values.	*/
/*									*/
/************************************************************************/
/*									*/
/*	fls_close( item )						*/
/*	Fs_item	item;							*/
/*									*/
/*	removes a busy fileselector from screen 			*/
/*									*/
/************************************************************************/
/*									*/
/*	fls_destroy( item )						*/
/*	Fs_item	*item;		CALL BY REFERENCE !!!			*/
/*									*/
/*	Destroys a fileselector-item and releases the corresponding	*/
/*	allocated memory. Sets item to (Fs_item)NULL.			*/
/*	DO NOT USE when AUTO_DESTROY of item is TRUE.			*/
/*	By the use of fls_set_auto_destroy, the programmer is able to	*/
/*	create global fileselectors (-> set_auto_destroy(FALSE) )	*/
/*	or use local fileselectors (-> set_auto_destroy(TRUE) )		*/
/*									*/
/************************************************************************/
/*									*/
/*	void	fls_set_user_panel_items_create_proc( item, user_proc )	*/
/*	Fs_item	item;							*/
/*	void	(*user_proc)();						*/
/*									*/
/*	user's chance to add an own panel to the fileselector.		*/
/*	user_proc is defined as follows :				*/
/*									*/
/*		void	user_proc( user_panel )				*/
/*		Panel	user_panel;					*/
/*									*/
/*	if the programmer has specified a user_proc as above, then the	*/
/*	fileselector system will try to create an additional panel to	*/
/*	the fileselector 'item'. If this creation succeeds then 	*/
/*	user_proc will be called.					*/
/*	It is assumed that user_proc creates all the panel_items to be	*/
/*	placed in this additional panel.				*/
/*									*/
/************************************************************************/
/*									*/
/*	fls_copy_attributes( dest, source )				*/
/*	Fs_item	dest, source;						*/
/*									*/
/*	Copies all user relevant attributes/settings from source 	*/
/*	to dest.							*/
/*									*/
/************************************************************************/
/*									*/
/*	fls_set_extension( item, no, string )				*/
/*	Fs_item	item;							*/
/*	int	no;							*/
/*	char	*string;						*/
/*									*/
/*	Sets extension-button 'no' to 'string'.				*/
/*	'no' must have a value within [0..(MASKNO-3)].			*/
/*									*/
/************************************************************************/
/*									*/
/*	fls_set_current_extension( item, no )				*/
/*	Fs_item	item;							*/
/*	int	no;							*/
/*									*/
/*	Makes the extension associated with 'no' (see fls_set_extension */
/*	above) to the current working selection mask of item.		*/
/*									*/
/************************************************************************/
/*									*/
/*	fls_set_working_directory( item, dir )				*/
/*	Fs_item	item;							*/
/*	char	*dir;							*/
/*									*/
/*	Sets the default value for the working directory.		*/
/*	if 'dir' is equal to empty string (not NULL !), then the 	*/
/*	current working directory is used.				*/
/*	Same effect if 'dir' is not accessable ( for what reasons ever )*/
/*									*/
/************************************************************************/
/*									*/
/*	fls_set_default_filename( item, file )				*/
/*	Fs_item	item;							*/
/*	char	*file;							*/
/*									*/
/*	Sets the default value for the filename.			*/
/*	'file' must not be longer than FNLEN characters.		*/
/*									*/
/************************************************************************/
/*									*/
/*	fls_set_info( item, info )					*/
/*	Fs_item	item;							*/
/*	char	*info;							*/
/*									*/
/*	Sets the fileselector's frame label.				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	fls_busy( item )					*/
/*	Fs_item	item;							*/
/*									*/
/*	returns TRUE if the fileselector is active, otherwise FALSE.	*/
/*									*/
/************************************************************************/
/*									*/
/*	fls_set_auto_destroy( item, bool )				*/
/*	Fs_item	item;							*/
/*	int	bool;							*/
/*									*/
/*	Sets AUTO_DESTROY-flag of fileselector 'item'.			*/
/*	If AUTO_DESTROY is TRUE then 'item' is released when finishing	*/
/*	the fileselector. NO FURTHER ACCESS to 'item' is allowed after 	*/
/*	calling 'fileselect( item )'.					*/
/*	By the use of fls_set_auto_destroy, the programmer is able to	*/
/*	create global fileselectors (-> set_auto_destroy(FALSE) )	*/
/*	or use local fileselectors.					*/
/*									*/
/************************************************************************/
/*									*/
/*	fileselect( item, parent_frame, done_proc )			*/
/*	Fs_item	item;							*/
/*	Frame	parent_frame;						*/
/*	void	(*done_proc)();						*/
/*									*/
/*	Creates a fileselector-box using item.				*/
/*	The window is assigned to parent_frame, so if parent_frame	*/
/*	is destroyed, then it happens to the fileselector, too.		*/
/*									*/
/*	When the user has made his/her choice, then done_proc() is	*/
/*	called. done_proc() is defined as follows :			*/
/*									*/
/*		void	done_proc( dir, file )				*/
/*		char	*dir, *file;					*/
/*									*/
/*	dir contains the path of the chosen file,			*/
/*	file contains the filename.					*/
/*	( usually you have to insert a '/' between the two strings to 	*/
/*	obtain a complete and correct filename )			*/
/*									*/
/*	if dir is equal to "" (empty string) and file is equal to	*/
/*	"NOTHING SELECTED", then the user chose 'Abort' or 'done'	*/
/*	or the system failed to create the fileselector.		*/
/*									*/
/************************************************************************/
/*									*/
/*	void	fls_write_to_file( item, file, identification )		*/
/*	Fs_item	item;							*/
/*	FILE	*file;							*/
/*	char	*identification						*/
/*									*/
/*	Writes the user relevant contents of the fileselector 'item'	*/
/*	to the stream 'file' and gives the data the specified 		*/
/*	'identification'.						*/
/*	User relevant contents are:					*/
/*		- current directory					*/
/*		- status of the '.'-files button			*/
/*		- status of the fit extension button			*/
/*		- all selection masks					*/
/*		- number of the current mask				*/
/*		- displaying mode					*/
/*	The 'identification' is needed for 'fls_setup_from_file'.	*/
/*	The output of this procedure can be edited by an editor.	*/
/*									*/
/************************************************************************/
/*									*/
/*	void	fls_setup_from_file( item, filename, identification )	*/
/*	Fs_item	item;							*/
/*	char	*filename, *identification;				*/
/*									*/
/*	Looks for user relevant data belonging to 'identification' in	*/
/*	file 'filename' and assigns them to 'item'.			*/
/*	By the use of different identifications, you can specify	*/
/*	several fileselectors in one textfile				*/
/*									*/
/************************************************************************/

#endif
