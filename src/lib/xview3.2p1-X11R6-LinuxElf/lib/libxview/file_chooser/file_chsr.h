/*      @(#)file_chsr.h 1.20 93/06/28 SMI      */

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *	file for terms of the license.
 */

#ifndef xview_file_chooser_DEFINED
#define xview_file_chooser_DEFINED


#include <xview/panel.h>
#include <sys/stat.h>


extern Xv_pkg		file_chooser_pkg;
#define FILE_CHOOSER	&file_chooser_pkg

typedef Xv_opaque File_chooser;




/*
 * Macros for xv_create
 */
#define FILE_CHOOSER_OPEN_DIALOG \
		FILE_CHOOSER, FILE_CHOOSER_TYPE, FILE_CHOOSER_OPEN

#define FILE_CHOOSER_SAVE_DIALOG \
		FILE_CHOOSER, FILE_CHOOSER_TYPE, FILE_CHOOSER_SAVE

#define FILE_CHOOSER_SAVEAS_DIALOG \
		FILE_CHOOSER, FILE_CHOOSER_TYPE, FILE_CHOOSER_SAVEAS



#define FCHSR_ATTR(type, ordinal)	ATTR(ATTR_PKG_FILE_CHOOSER, type, ordinal)

typedef enum {
    FILE_CHOOSER_TYPE 		= FCHSR_ATTR(ATTR_ENUM,		 1), /* CG- */
    FILE_CHOOSER_CHILD		= FCHSR_ATTR(ATTR_ENUM,		 2), /* -G- */
    FILE_CHOOSER_HISTORY_LIST	= FCHSR_ATTR(ATTR_OPAQUE,	 3), /* CGS */
    FILE_CHOOSER_DOC_NAME	= FCHSR_ATTR(ATTR_STRING,	 4), /* CGS */
    FILE_CHOOSER_NOTIFY_FUNC	= FCHSR_ATTR(ATTR_FUNCTION_PTR,	 5), /* CGS */
    FILE_CHOOSER_CUSTOMIZE_OPEN	= FCHSR_ATTR(ATTR_OPAQUE_TRIPLE, 6), /* C-- */
    FILE_CHOOSER_UPDATE		= FCHSR_ATTR(ATTR_NO_VALUE,	 7), /* --S */
    FILE_CHOOSER_AUTO_UPDATE	= FCHSR_ATTR(ATTR_BOOLEAN,	 8), /* CGS */
    FILE_CHOOSER_SHOW_DOT_FILES = FCHSR_ATTR(ATTR_BOOLEAN,	 9), /* CGS */
    FILE_CHOOSER_ABBREV_VIEW	= FCHSR_ATTR(ATTR_BOOLEAN,	10), /* CGS */
    FILE_CHOOSER_APP_DIR	= FCHSR_ATTR(ATTR_OPAQUE_PAIR,	11), /* C-S */
    FILE_CHOOSER_DIRECTORY	= FCHSR_ATTR(ATTR_STRING,	12), /* CGS */
    FILE_CHOOSER_FILTER_STRING	= FCHSR_ATTR(ATTR_STRING,	13), /* CGS */
    FILE_CHOOSER_MATCH_GLYPH	= FCHSR_ATTR(ATTR_OPAQUE,	14), /* CGS */
    FILE_CHOOSER_MATCH_GLYPH_MASK = FCHSR_ATTR(ATTR_OPAQUE,	29), /* CGS */
    FILE_CHOOSER_CD_FUNC	= FCHSR_ATTR(ATTR_FUNCTION_PTR,	15), /* CGS */
    FILE_CHOOSER_FILTER_MASK	= FCHSR_ATTR(ATTR_SHORT,	16), /* CGS */
    FILE_CHOOSER_FILTER_FUNC	= FCHSR_ATTR(ATTR_FUNCTION_PTR,	17), /* CGS */
    FILE_CHOOSER_COMPARE_FUNC	= FCHSR_ATTR(ATTR_FUNCTION_PTR,	18), /* CGS */
    FILE_CHOOSER_EXTEN_HEIGHT	= FCHSR_ATTR(ATTR_INT,		19), /* CGS */
    FILE_CHOOSER_EXTEN_FUNC	= FCHSR_ATTR(ATTR_FUNCTION_PTR,	20),  /* CGS */
    FILE_CHOOSER_SAVE_TO_DIR	= FCHSR_ATTR(ATTR_BOOLEAN,	28), /* CGS */

#ifdef OW_I18N
    /*
     * Wide Char Interface
     */
    FILE_CHOOSER_DOC_NAME_WCS		= FCHSR_ATTR(ATTR_WSTRING,	21), /* CGS */
    FILE_CHOOSER_DIRECTORY_WCS		= FCHSR_ATTR(ATTR_WSTRING,	22), /* CGS */
    FILE_CHOOSER_FILTER_STRING_WCS	= FCHSR_ATTR(ATTR_WSTRING,	23), /* CGS */
    FILE_CHOOSER_APP_DIR_WCS		= FCHSR_ATTR(ATTR_OPAQUE_PAIR,	24), /* C-S */
    FILE_CHOOSER_CUSTOMIZE_OPEN_WCS	= FCHSR_ATTR(ATTR_OPAQUE_TRIPLE,25), /* C-- */
    FILE_CHOOSER_NOTIFY_FUNC_WCS	= FCHSR_ATTR(ATTR_FUNCTION_PTR,	26), /* CGS */
    FILE_CHOOSER_WCHAR_NOTIFY		= FCHSR_ATTR(ATTR_BOOLEAN,	27), /* CGS */
#endif /* OW_I18N */


    /*
     * Private Attributes
     */
    FILE_CHOOSER_ASSUME_DEFAULT_SIZE	= FCHSR_ATTR(ATTR_NO_VALUE,	30),
    FILE_CHOOSER_NO_CONFIRM		= FCHSR_ATTR(ATTR_BOOLEAN,	31)
} File_chooser_attr;



typedef enum {
    FILE_CHOOSER_OPEN,
    FILE_CHOOSER_SAVE,
    FILE_CHOOSER_SAVEAS
} File_chooser_type;


typedef enum {
    FILE_CHOOSER_GOTO_MESSAGE_CHILD,
    FILE_CHOOSER_GOTO_BUTTON_CHILD,
    FILE_CHOOSER_GOTO_PATH_CHILD,
    FILE_CHOOSER_HISTORY_MENU_CHILD,
    FILE_CHOOSER_CURRENT_FOLDER_CHILD,
    FILE_CHOOSER_SELECT_MESSAGE_CHILD,
    FILE_CHOOSER_FILE_LIST_CHILD,
    FILE_CHOOSER_DOCUMENT_NAME_CHILD,
    FILE_CHOOSER_OPEN_BUTTON_CHILD,
    FILE_CHOOSER_SAVE_BUTTON_CHILD,
    FILE_CHOOSER_CANCEL_BUTTON_CHILD,
    FILE_CHOOSER_CUSTOM_BUTTON_CHILD
} File_chooser_child;



typedef enum {
    /* for FILE_CHOOSER_FILTER_FUNC */
    FILE_CHOOSER_IGNORE,
    FILE_CHOOSER_ACCEPT,

    /* Ops to FILE_CHOOSER_CD_FUNC */
    FILE_CHOOSER_BEFORE_CD,
    FILE_CHOOSER_AFTER_CD,

    /*
     * values for FILE_CHOOSER_FILTER_FUNC, matched field.
     * specifies if entry matched FILE_CHOOSER_FILTER_STRING
     */
    FILE_CHOOSER_NOT_MATCHED,
    FILE_CHOOSER_MATCHED,

    /* used by FILE_CHOOSER_CUSTOMIZE_OPEN attr */
    FILE_CHOOSER_SELECT_FILES,
    FILE_CHOOSER_SELECT_ALL
} File_chooser_op;




#define FILE_CHOOSER_NULL_FILTER_FUNC	(File_chooser_op (*)())NULL

typedef enum {
    FC_NONE_MASK		= 0,
    FC_MATCHED_FILES_MASK	= (1<<0),
    FC_NOT_MATCHED_FILES_MASK	= (1<<1),
    FC_MATCHED_DIRS_MASK	= (1<<2),
    FC_NOT_MATCHED_DIRS_MASK	= (1<<3),
    FC_DOTDOT_MASK		= (1<<4),
    FC_ALL_MASK			= 0xffff
} File_chooser_filter_mask;



/* used by FILE_CHOOSER_COMPARE_FUNC */
typedef struct {
    char *		file;
    struct stat *	stats;
    File_chooser_op	matched;
    char *		xfrm;		/* returned by strxfrm() */
    Xv_opaque		reserved;	/* reserved for future use */
} File_chooser_row;

#ifdef OW_I18N
typedef struct {
    wchar_t *		file_wcs;
    struct stat *	stats;
    File_chooser_op	matched;
    char *		xfrm;		/* returned by strxfrm() */
    Xv_opaque		reserved;	/* reserved for future use */
} File_chooser_row_wcs;
#endif


/*
 * Built-in comparison functions
 */
#define FILE_CHOOSER_DEFAULT_COMPARE	fchsr_no_case_ascend_compare
#define FILE_CHOOSER_NULL_COMPARE	(int (*)())NULL
EXTERN_FUNCTION(int fchsr_no_case_ascend_compare, (File_chooser_row *row1, File_chooser_row *row2) );
EXTERN_FUNCTION(int fchsr_no_case_descend_compare, (File_chooser_row *row1, File_chooser_row *row2) );
EXTERN_FUNCTION(int fchsr_case_ascend_compare, (File_chooser_row *row1, File_chooser_row *row2) );
EXTERN_FUNCTION(int fchsr_case_descend_compare, (File_chooser_row *row1, File_chooser_row *row2) );


typedef struct {
    Xv_frame_cmd	parent_data;
    Xv_opaque		private_data;
} File_chooser_public;


#endif	/* ~xview_file_chooser_DEFINED */
