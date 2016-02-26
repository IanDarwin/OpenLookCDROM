#include "ftptool.h"

#pragma ident   "@(#)create_main.c 1.6     93/05/26"

#ifdef USE_PROTOTYPES
void create_base_window(void)
#else
void create_base_window()
#endif
{
	Menu file_menu;
	Menu copy_menu;
	Menu delete_menu;
	Menu dir_menu;
	Menu view_menu;
	Menu props_menu;
	Menu remote_list_menu;
	Xv_Screen	screen;
	int		screen_no;
	int	width, height, x, y;

	base_window.frame = xv_create(XV_NULL,
		FRAME,
		XV_X, 350,
		XV_Y, 100,
		XV_LABEL, header_name,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_LEFT_FOOTER, NULL,
		NULL);

	base_window.panel = xv_create(base_window.frame,
		PANEL,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);

	list_label =
	    "                 Date                     Size   Filename";

	list_font = (Xv_font)xv_find(base_window.panel, FONT,
		FONT_FAMILY, FONT_FAMILY_DEFAULT_FIXEDWIDTH,
		FONT_STYLE, FONT_STYLE_NORMAL,
		NULL);

	if (list_font == XV_NULL) {
		fprintf(stderr, "could not find default fixed font.\n");
	} else {
		switch ((int)xv_get(list_font, FONT_SCALE)) {
		case WIN_SCALE_SMALL:
			list_label = "                     Date                        Size    Filename";
			break;
		case WIN_SCALE_MEDIUM:
		default:
			/* this seems to be the default */
			list_label = "                 Date                     Size   Filename";
			break;
		case WIN_SCALE_LARGE:
			list_label = "                     Date                            Size    Filename";
			break;
		case WIN_SCALE_EXTRALARGE:
			list_label = "            Date                    Size   Filename";
			break;
		}
	}

	bold_list_font = (Xv_font)xv_find(base_window.panel, FONT,
		FONT_FAMILY, FONT_FAMILY_DEFAULT_FIXEDWIDTH,
		FONT_STYLE, FONT_STYLE_BOLD,
		NULL);

	if (bold_list_font == XV_NULL)
		fprintf(stderr, "could not find default bold fixed font.\n");

	xv_set(base_window.panel,
		PANEL_LAYOUT,
		PANEL_HORIZONTAL,
		NULL);


	copy_menu = xv_create(XV_NULL,
		MENU,
		MENU_GEN_PROC, file_copy_menu_gen,
		MENU_ITEM,
			MENU_STRING, "Remote to Local",
			MENU_NOTIFY_PROC, get_proc,
			XV_HELP_DATA, "ftptool:FileCopyRemote",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Local to Remote",
			MENU_NOTIFY_PROC, put_proc,
			XV_HELP_DATA, "ftptool:FileCopyLocal",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Remote to Local (Batch)",
			MENU_NOTIFY_PROC, batchget_proc,
			XV_HELP_DATA, "ftptool:BatchReceiveCopy",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Local to Remote (Batch)",
			MENU_NOTIFY_PROC, batchput_proc,
			XV_HELP_DATA, "ftptool:BatchSendCopy",
			NULL,
		NULL);

	delete_menu = xv_create(XV_NULL,
		MENU,
		MENU_GEN_PROC, file_delete_menu_gen,
		MENU_ITEM,
			MENU_STRING, "Remote File",
			MENU_NOTIFY_PROC, remote_delete_proc,
			XV_HELP_DATA, "ftptool:FileDeleteRemote",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Local File",
			MENU_NOTIFY_PROC, local_delete_proc,
			XV_HELP_DATA, "ftptool:FileLocalRemote",
			NULL,
		NULL);

	dir_menu = xv_create(XV_NULL,
		MENU,
		MENU_ITEM,
			MENU_STRING, "DIR",
			MENU_NOTIFY_PROC, dir_list_proc,
			XV_HELP_DATA, "ftptool:FileDir",
			NULL,
		MENU_ITEM,
			MENU_STRING, "LS",
			MENU_NOTIFY_PROC, ls_list_proc,
			XV_HELP_DATA, "ftptool:FileLS",
			NULL,
		NULL);

#ifdef LINT
	file_menu = NULL;
	file_menu = file_menu;
	copy_menu = copy_menu;
	delete_menu = delete_menu;
	dir_menu = dir_menu;
#else
	file_menu = xv_create(XV_NULL,
		MENU,
		MENU_GEN_PROC, file_menu_gen,
		MENU_PULLRIGHT_ITEM, "Copy", copy_menu,
		MENU_PULLRIGHT_ITEM, "Delete", delete_menu,
		MENU_PULLRIGHT_ITEM, "List Directory", dir_menu,
		MENU_ITEM,
			MENU_STRING, "Compress File",
			MENU_NOTIFY_PROC, compress_proc,
			XV_HELP_DATA, "ftptool:FileCompress",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Uncompress File",
			MENU_NOTIFY_PROC, uncompress_proc,
			XV_HELP_DATA, "ftptool:FileUncompress",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Create Tar File",
			MENU_NOTIFY_PROC, tar_proc,
			XV_HELP_DATA, "ftptool:FileCreateTar",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Extract Tar File",
			MENU_NOTIFY_PROC, extract_proc,
			XV_HELP_DATA, "ftptool:FileExtractTar",
			NULL,
		NULL);
#endif

	base_window.file = xv_create(base_window.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "File",
		PANEL_ITEM_MENU, file_menu,
		XV_HELP_DATA, "ftptool:FileButton",
		NULL);

#ifdef LINT
	view_menu = NULL;
	view_menu = view_menu;
#else
	view_menu = xv_create(XV_NULL,
		MENU,
		MENU_GEN_PROC, view_menu_gen,
		MENU_ITEM,
			MENU_STRING, "Remote File",
			MENU_NOTIFY_PROC, remote_view,
			XV_HELP_DATA, "ftptool:ViewRemote",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Local File",
			MENU_NOTIFY_PROC, local_view,
			XV_HELP_DATA, "ftptool:ViewLocal",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Local Directory List...",
			MENU_NOTIFY_PROC, local_dir_view,
			XV_HELP_DATA, "ftptool:ViewLocalDirectory",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Session Log...",
			MENU_NOTIFY_PROC, session_view,
			XV_HELP_DATA, "ftptool:ViewSessionLog",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Host Information...",
			MENU_NOTIFY_PROC, host_view,
			XV_HELP_DATA, "ftptool:ViewCurrentHost",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Transfer Status...",
			MENU_NOTIFY_PROC, status_view,
			XV_HELP_DATA, "ftptool:ViewStatus",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Batch Schedule...",
			MENU_NOTIFY_PROC, schedule_view,
			XV_HELP_DATA, "ftptool:ViewSchedule",
			NULL,
		MENU_ITEM,
			MENU_STRING, "About Ftptool...",
			MENU_NOTIFY_PROC, about_proc,
			XV_HELP_DATA, "ftptool:FileAbout",
			NULL,
		NULL);
#endif

	base_window.view = xv_create(base_window.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "View",
		PANEL_ITEM_MENU, view_menu,
		XV_HELP_DATA, "ftptool:ViewButton",
		NULL);


	props_menu = xv_create(XV_NULL,
		MENU,
		MENU_GEN_PROC, props_menu_gen,
		MENU_ITEM,
			MENU_STRING, "Tool...",
			MENU_NOTIFY_PROC, props_proc,
			XV_HELP_DATA, "ftptool:PropertiesTool",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Local File...",
			MENU_NOTIFY_PROC, local_properties,
			XV_HELP_DATA, "ftptool:PropertiesLocal",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Remote File...",
			MENU_NOTIFY_PROC, remote_properties,
			XV_HELP_DATA, "ftptool:PropertiesRemote",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Save Layout",
			MENU_NOTIFY_PROC, save_layout_func,
			XV_HELP_DATA, "ftptool:PropertiesSaveLayout",
			NULL,
		NULL);

	base_window.props = xv_create(base_window.panel,
		PANEL_BUTTON,
		PANEL_ITEM_X_GAP, 3 * (int)xv_get(base_window.panel,
		    PANEL_ITEM_X_GAP),
		PANEL_LABEL_STRING, "Properties",
		PANEL_ITEM_MENU, props_menu,
		XV_HELP_DATA, "ftptool:PropertiesButton",
		NULL);

	base_window.connect = xv_create(base_window.panel,
		PANEL_BUTTON,
		PANEL_ITEM_X_GAP, 3 * (int)xv_get(base_window.panel,
		    PANEL_ITEM_X_GAP),
		PANEL_LABEL_STRING, " Connect... ",
		PANEL_NOTIFY_PROC, connect_proc,
		XV_HELP_DATA, "ftptool:ConnectButton",
		NULL);

	base_window.abort = xv_create(base_window.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Abort",
		PANEL_NOTIFY_PROC, abort_proc,
		PANEL_INACTIVE, TRUE,
		XV_HELP_DATA, "ftptool:AbortButton",
		NULL);

	base_window.quit = xv_create(base_window.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Quit",
		PANEL_NOTIFY_PROC, quit_proc,
		XV_HELP_DATA, "ftptool:QuitButton",
		XV_SHOW, openlook_mode ? FALSE : TRUE,
		NULL);

	xv_set(base_window.panel,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);

	base_window.directory = xv_create(base_window.panel,
		PANEL_TEXT,
		PANEL_ITEM_X, 0,
		PANEL_VALUE_DISPLAY_LENGTH, 42,
		PANEL_VALUE_STORED_LENGTH, MAXPATHLEN,
		PANEL_LABEL_STRING, "Remote Directory:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_LEVEL, PANEL_SPECIFIED,
		PANEL_NOTIFY_PROC, remote_cd_text,
		PANEL_INACTIVE, TRUE,
		XV_HELP_DATA, "ftptool:RemoteDirectory",
		NULL);

	base_window.list = xv_create(base_window.panel,
		PANEL_LIST,
		PANEL_ITEM_X, 2,
		PANEL_NEXT_ROW, xv_row(base_window.panel, 1),
		PANEL_LIST_DISPLAY_ROWS, 8,
		PANEL_LABEL_STRING, list_label,
		PANEL_FONT, bold_list_font,
		PANEL_LAYOUT, PANEL_VERTICAL,
		PANEL_READ_ONLY, TRUE,
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_NOTIFY_PROC, remote_list_proc,
		XV_HELP_DATA, "ftptool:RemoteList",
		NULL);

	remote_list_menu = xv_get(base_window.list, PANEL_ITEM_MENU);
#ifdef LINT
	remote_list_menu = remote_list_menu;
#else
	xv_set(remote_list_menu,
		MENU_GEN_PIN_WINDOW, base_window.frame, "Remote File List",
		MENU_TITLE_ITEM, "Remote File List",
		MENU_ITEM,
			MENU_STRING, "",
			MENU_FEEDBACK, FALSE,
			NULL,
		MENU_ITEM,
			MENU_STRING, "CD to Selection",
			MENU_NOTIFY_PROC, remote_cd_select,
			XV_HELP_DATA, "ftptool:RemoteMenuCDSelection",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Up One Level",
			MENU_NOTIFY_PROC, remote_cd_dotdot,
			XV_HELP_DATA, "ftptool:RemoteMenuCDUp",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Copy to Local",
			MENU_NOTIFY_PROC, get_proc,
			XV_HELP_DATA, "ftptool:RemoteMenuCopyLocal",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Add to Batch Receive List",
			MENU_NOTIFY_PROC, add_batch_receive_proc,
			XV_HELP_DATA, "ftptool:BatchReceiveAdd",
			NULL,
		MENU_ITEM,
			MENU_STRING, "View File",
			MENU_NOTIFY_PROC, remote_view,
			XV_HELP_DATA, "ftptool:RemoteMenuView",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Delete",
			MENU_NOTIFY_PROC, remote_delete_proc,
			XV_HELP_DATA, "ftptool:FileDeleteRemote",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Properties...",
			MENU_NOTIFY_PROC, remote_properties,
			XV_HELP_DATA, "ftptool:RemoteMenuProperties",
			NULL,
		NULL);
#endif

	change_remote_list_menu();
	xv_set(base_window.panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	window_fit(base_window.panel);
	window_fit(base_window.frame);

	dpy = (Display *)xv_get(base_window.frame, XV_DISPLAY);
	screen = (Xv_Screen)xv_get(base_window.frame, XV_SCREEN);
	screen_no = (int)xv_get(screen, SCREEN_NUMBER);

	display_width = DisplayWidth(dpy, screen_no);
	display_height = DisplayHeight(dpy, screen_no);

	width = xv_get(base_window.frame, XV_WIDTH);
	height = xv_get(base_window.frame, XV_HEIGHT);
	x = xv_get(base_window.frame, XV_X);
	y = xv_get(base_window.frame, XV_Y);

	set_geometry(base_window.geometry, base_window.frame, width,
	    height, x, y);

	XSync(dpy, False);

	xv_set(base_window.frame,
		WIN_EVENT_PROC, base_event_proc,
		NULL);

	xv_set(base_window.panel,
		PANEL_BACKGROUND_PROC, props_event_proc,
		PANEL_ACCEPT_KEYSTROKE, TRUE,
		NULL);

#ifdef XVIEW3
	base_window.drop_site = xv_create(base_window.frame,
		DROP_SITE_ITEM,
		DROP_SITE_ID, 1234,
		DROP_SITE_REGION, xv_get(base_window.list, XV_RECT),
		NULL);
	base_window.dnd = xv_create(base_window.frame,
		DRAGDROP,
		DND_TYPE, DND_COPY,
		NULL);
	base_window.sel = xv_create(base_window.frame,
		SELECTION_REQUESTOR,
		NULL);
	get_remote_list_event_proc();
	xv_set(base_window.list,
		PANEL_EVENT_PROC, remote_list_event,
		NULL);
#endif
}

#ifdef USE_PROTOTYPES
void create_property_window(void)
#else
void create_property_window()
#endif
{
	Rect    *butrect;
	Rect    *rect;
	int		options;
	int		x, y;
	int		base_x;
	int		gap;
	int		height;

	tool_property_window.frame = xv_create(base_window.frame,
		FRAME_PROPS,
		XV_LABEL, "Ftptool: Properties",
		NULL);

	tool_property_window.panel = (Panel)xv_get(tool_property_window.frame,
		FRAME_PROPS_PANEL);

	tool_property_window.category = xv_create(tool_property_window.panel,
		PANEL_CHOICE_STACK,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Category:",
		PANEL_CHOICE_STRINGS,
			"Ftptool",
			"Directory Lists",
			"Viewers",
			NULL,
		PANEL_VALUE, 0,
		PANEL_NOTIFY_PROC, category_proc,
		NULL);

	xv_set(tool_property_window.panel,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);

	window_fit_height(tool_property_window.panel);

	/* ftptool panel */

	tool_property_window.ftptool.panel =
	    xv_create(tool_property_window.frame,
		PANEL,
		PANEL_LAYOUT, PANEL_VERTICAL,
		WIN_BORDER, TRUE,
		NULL);

	x = xv_get(tool_property_window.ftptool.panel, XV_X);
	y = xv_get(tool_property_window.ftptool.panel, XV_Y);

	options = logging | keepalive;
	tool_property_window.ftptool.options =
	    xv_create(tool_property_window.ftptool.panel,
		PANEL_CHOICE,
		PANEL_LABEL_STRING, "FTP options:",
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_CHOICE_STRINGS,
			"Log Session",
			"Keep Connection Alive",
			NULL,
		PANEL_VALUE, options,
		XV_HELP_DATA, "ftptool:PropertyOptions",
		NULL);

	options = unique_local_names | unique_remote_names;
	tool_property_window.ftptool.unique =
	    xv_create(tool_property_window.ftptool.panel, PANEL_CHOICE,
		PANEL_LABEL_STRING, "Generate unique:",
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_CHOICE_STRINGS,
			"Local Filenames",
			"Remote Filenames",
			NULL,
		PANEL_VALUE, options,
		XV_HELP_DATA, "ftptool:PropertyUnique",
		NULL);

	options = auto_connect | show_status | try_proxy;
	tool_property_window.ftptool.automatic =
	    xv_create(tool_property_window.ftptool.panel, PANEL_CHOICE,
		PANEL_LABEL_STRING, "Automatically:",
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_CHOICE_STRINGS,
			"Connect",
			"Show Status",
			"Try Sun Proxy FTP",
			NULL,
		PANEL_VALUE, options,
		XV_HELP_DATA, "ftptool:PropertyAuto",
		NULL);

	tool_property_window.ftptool.openlook =
	    xv_create(tool_property_window.ftptool.panel, PANEL_CHECK_BOX,
		PANEL_LABEL_STRING, "Window manager:",
		PANEL_CHOICE_STRINGS,
			"OPEN LOOK",
			NULL,
		PANEL_VALUE, openlook_mode,
		XV_HELP_DATA, "ftptool:PropertyOpenLook",
		NULL);

	tool_property_window.ftptool.anonftp =
	    xv_create(tool_property_window.ftptool.panel, PANEL_TEXT,
		PANEL_LABEL_STRING, "Initial password:",
		PANEL_VALUE_DISPLAY_LENGTH, 40,
		PANEL_VALUE_STORED_LENGTH, MAXPASSWORDLEN,
		PANEL_VALUE, anonftp_password,
		XV_HELP_DATA, "ftptool:PropertyPassword",
		NULL);

	tool_property_window.ftptool.ignore_case =
	    xv_create(tool_property_window.ftptool.panel, PANEL_CHECK_BOX,
		PANEL_LABEL_STRING, "Case sensitivity:",
		PANEL_CHOICE_STRINGS,
			"Ignore case",
			NULL,
		PANEL_VALUE, ignore_case,
		XV_HELP_DATA, "ftptool:PropertyIgnoreCase",
		NULL);

	options = confirmdeletes | confirmoverwrites;
	tool_property_window.ftptool.confirm =
	    xv_create(tool_property_window.ftptool.panel, PANEL_CHOICE,
		PANEL_LABEL_STRING, "Confirm:",
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_CHOICE_STRINGS,
			"Deletions",
			"Overwrites",
			NULL,
		PANEL_VALUE, options,
		XV_HELP_DATA, "ftptool:PropertyConfirm",
		NULL);

	justify_items(tool_property_window.ftptool.panel, FALSE);

	tool_property_window.ftptool.apply =
	    xv_create(tool_property_window.ftptool.panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Apply",
		PANEL_NOTIFY_PROC, ftptool_props_apply_proc,
		XV_HELP_DATA, "ftptool:PropertyApply",
		NULL);


	window_fit_width(tool_property_window.ftptool.panel);

	xv_set(tool_property_window.ftptool.panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	butrect = (Rect *)xv_get(tool_property_window.ftptool.apply, XV_RECT);
	xv_set(tool_property_window.ftptool.apply,
		XV_X, (int)xv_get(tool_property_window.ftptool.panel,
		    XV_WIDTH)/2 - butrect->r_width,
		NULL);

	xv_create(tool_property_window.ftptool.panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Reset",
		PANEL_NOTIFY_PROC, ftptool_props_reset_proc,
		XV_HELP_DATA, "ftptool:PropertyReset",
		NULL);

	xv_set(tool_property_window.ftptool.panel,
		PANEL_DEFAULT_ITEM, tool_property_window.ftptool.apply,
		NULL);

	window_fit_height(tool_property_window.ftptool.panel);

	/* directory lists panel */

	tool_property_window.directory_lists.panel =
	    xv_create(tool_property_window.frame, PANEL,
		PANEL_LAYOUT, PANEL_VERTICAL,
		WIN_BORDER, TRUE,
		XV_X, x,
		XV_Y, y,
		NULL);

	tool_property_window.directory_lists.cache =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_SLIDER,
		PANEL_LABEL_STRING, "Directory Cache Size:",
		PANEL_MIN_VALUE, 1,
		PANEL_MAX_VALUE, 20,
		PANEL_SLIDER_WIDTH, 200,
		PANEL_TICKS, 5,
		PANEL_VALUE, dircache_size,
		PANEL_INACTIVE, dircache_size == 0,
		XV_HELP_DATA, "ftptool:PropertyCacheSize",
		NULL);

	tool_property_window.directory_lists.cache_inf =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_CHECK_BOX,
		PANEL_CHOICE_STRINGS,
			"Unlimit Cache Size",
			NULL,
		PANEL_VALUE, dircache_size == 0,
		PANEL_NOTIFY_PROC, props_inf_check_box,
		PANEL_ITEM_Y,
		    xv_get(tool_property_window.directory_lists.cache, XV_X)
			+ xv_get(tool_property_window.directory_lists.cache,
			    XV_HEIGHT),
		XV_HELP_DATA, "ftptool:PropertyCacheInf",
		NULL);


	tool_property_window.directory_lists.remote_sort =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_CHOICE,
		PANEL_NEXT_ROW,
		    xv_row(tool_property_window.directory_lists.panel, 1),
		PANEL_LABEL_STRING, "Sort remote files by:",
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_NOTIFY_PROC, remote_sort_choice_proc,
		PANEL_CHOICE_STRINGS,
			"Name",
			"Date",
			"Size",
			NULL,
		PANEL_VALUE, remote_sort_mode,
		XV_HELP_DATA, "ftptool:PropertySortChoice",
		NULL);

	tool_property_window.directory_lists.remote_sortdir =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_CHOICE,
		PANEL_LABEL_STRING, "Sort order:",
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_CHOICE_STRINGS,
			"Least Recently Changed",
			"Most Recently Changed",
			NULL,
		PANEL_VALUE, remote_sort_direction,
		XV_HELP_DATA, "ftptool:PropertySortDirection",
		NULL);

	tool_property_window.directory_lists.remote_dotfiles =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_CHOICE,
		PANEL_LABEL_STRING, "Hidden files:",
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_CHOICE_STRINGS,
			"Hide",
			"Show",
			NULL,
		PANEL_VALUE, remote_showdotfiles,
		XV_HELP_DATA, "ftptool:PropertyHidden",
		NULL);

	tool_property_window.directory_lists.remote_group =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_CHECK_BOX,
		PANEL_CHOICE_STRINGS,
			"Group files by type",
			NULL,
		PANEL_VALUE, group_remote_files,
		XV_HELP_DATA, "ftptool:PropertySortGrouping",
		NULL);

	tool_property_window.directory_lists.local_sort =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_CHOICE,
		PANEL_NEXT_ROW,
		    xv_row(tool_property_window.directory_lists.panel, 1),
		PANEL_LABEL_STRING, "Sort local files by:",
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_NOTIFY_PROC, local_sort_choice_proc,
		PANEL_CHOICE_STRINGS,
			"Name",
			"Date",
			"Size",
			NULL,
		PANEL_VALUE, local_sort_mode,
		XV_HELP_DATA, "ftptool:PropertyLocalSortChoice",
		NULL);

	tool_property_window.directory_lists.local_sortdir =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_CHOICE,
		PANEL_LABEL_STRING, "Sort order:",
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_CHOICE_STRINGS,
			"Least Recently Changed",
			"Most Recently Changed",
			NULL,
		PANEL_VALUE, local_sort_direction,
		XV_HELP_DATA, "ftptool:PropertySortDirection",
		NULL);

	tool_property_window.directory_lists.local_dotfiles =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_CHOICE,
		PANEL_LABEL_STRING, "Hidden files:",
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_CHOICE_STRINGS,
			"Hide",
			"Show",
			NULL,
		PANEL_VALUE, local_showdotfiles,
		XV_HELP_DATA, "ftptool:PropertyHidden",
		NULL);

	tool_property_window.directory_lists.local_group =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_CHECK_BOX,
		PANEL_CHOICE_STRINGS,
			"Group files by type",
			NULL,
		PANEL_VALUE, group_local_files,
		XV_HELP_DATA, "ftptool:PropertyLocalSortGrouping",
		NULL);

	justify_items(tool_property_window.directory_lists.panel, FALSE);

	tool_property_window.directory_lists.apply =
	    xv_create(tool_property_window.directory_lists.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Apply",
		PANEL_NOTIFY_PROC, directory_lists_props_apply_proc,
		XV_HELP_DATA, "ftptool:PropertyApply",
		NULL);


	window_fit_width(tool_property_window.directory_lists.panel);

	xv_set(tool_property_window.directory_lists.panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	butrect = (Rect *)xv_get(tool_property_window.directory_lists.apply,
	    XV_RECT);

	xv_set(tool_property_window.directory_lists.apply,
		XV_X, (int)xv_get(tool_property_window.directory_lists.panel,
		    XV_WIDTH)/2 - butrect->r_width,
		NULL);

	xv_create(tool_property_window.directory_lists.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Reset",
		PANEL_NOTIFY_PROC, directory_lists_props_reset_proc,
		XV_HELP_DATA, "ftptool:PropertyReset",
		NULL);

	xv_set(tool_property_window.directory_lists.panel,
		PANEL_DEFAULT_ITEM,
		    tool_property_window.directory_lists.apply,
		NULL);

	window_fit_height(tool_property_window.directory_lists.panel);

	/* viewers panel */

	tool_property_window.viewers.panel =
	    xv_create(tool_property_window.frame,
		PANEL,
		PANEL_LAYOUT, PANEL_VERTICAL,
		WIN_BORDER, TRUE,
		XV_X, x,
		XV_Y, y,
		NULL);

	tool_property_window.viewers.viewer =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_TEXT,
		PANEL_LABEL_STRING, "Default file viewer:",
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, MAXPATHLEN,
		PANEL_VALUE, default_viewer,
		XV_HELP_DATA, "ftptool:PropertyViewer",
		NULL);

	tool_property_window.viewers.compressor =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_TEXT,
		PANEL_LABEL_STRING, "Default file compressor:",
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, MAXPATHLEN,
		PANEL_VALUE, default_compressor,
		XV_HELP_DATA, "ftptool:PropertyCompressor",
		NULL);

	tool_property_window.viewers.list =
	    xv_create(tool_property_window.viewers.panel, PANEL_LIST,
		PANEL_LIST_DISPLAY_ROWS, 4,
		PANEL_LIST_WIDTH, 100,
		PANEL_LABEL_STRING, "Known extensions:",
		PANEL_READ_ONLY, TRUE,
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_NOTIFY_PROC, extension_list_proc,
		XV_HELP_DATA, "ftptool:PropertyKnownExtensions",
		NULL);

	xv_set(tool_property_window.viewers.panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	tool_property_window.viewers.add =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Add ",
		PANEL_NOTIFY_PROC, add_extension_proc,
		XV_HELP_DATA, "ftptool:PropertyAddExtension",
		NULL);

	tool_property_window.viewers.delete =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Delete ",
		PANEL_NOTIFY_PROC, delete_extension_proc,
		XV_HELP_DATA, "ftptool:PropertyDeleteExtension",
		NULL);

	tool_property_window.viewers.change =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Change ",
		PANEL_NOTIFY_PROC, change_extension_proc,
		XV_HELP_DATA, "ftptool:PropertyChangeExtension",
		NULL);

	xv_set(tool_property_window.viewers.panel,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);

	tool_property_window.viewers.extension =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_TEXT,
		PANEL_LABEL_STRING, "Extension:",
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, MAXPATHLEN,
		XV_HELP_DATA, "ftptool:PropertyExtension",
		NULL);

	tool_property_window.viewers.magic =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_TEXT,
		PANEL_LABEL_STRING, "Contents begin with:",
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, MAXPATHLEN,
		XV_HELP_DATA, "ftptool:PropertyMagic",
		NULL);

	tool_property_window.viewers.program =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_TEXT,
		PANEL_LABEL_STRING, "Program to run:",
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, MAXPATHLEN,
		XV_HELP_DATA, "ftptool:PropertyProgram",
		NULL);

	tool_property_window.viewers.type =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_CHOICE_STACK,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Program is a:",
		PANEL_CHOICE_STRINGS,
			"file viewer",
			"decompressor",
			NULL,
		XV_HELP_DATA, "ftptool:PropertyType",
		NULL);

	justify_items(tool_property_window.viewers.panel, FALSE);

	/* Move add/delete/change to side of list */

	rect = (Rect *)xv_get(tool_property_window.viewers.list, XV_RECT);
	base_x = rect->r_left + rect->r_width;
	height = rect->r_height;

	base_x += 3 * xv_get(tool_property_window.viewers.panel,
	    PANEL_ITEM_X_GAP);

	gap = xv_get(tool_property_window.viewers.panel, PANEL_ITEM_Y_GAP);

	gap /= 2;

	butrect = (Rect *)xv_get(tool_property_window.viewers.delete, XV_RECT);

	/* put delete in middle */
	xv_set(tool_property_window.viewers.delete,
		XV_X, base_x,
		XV_Y, rect->r_top + height / 2 - gap,
		NULL);

	xv_set(tool_property_window.viewers.add,
		XV_X, base_x,
		XV_Y, rect->r_top + height / 2 - butrect->r_height - 2 * gap,
		NULL);

	xv_set(tool_property_window.viewers.change,
		XV_X, base_x,
		XV_Y, rect->r_top + height / 2 + butrect->r_height,
		NULL);

	tool_property_window.viewers.apply =
	    xv_create(tool_property_window.viewers.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Apply",
		PANEL_NOTIFY_PROC, viewers_props_apply_proc,
		XV_HELP_DATA, "ftptool:PropertyApply",
		NULL);


	window_fit_width(tool_property_window.viewers.panel);

	xv_set(tool_property_window.viewers.panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	butrect = (Rect *)xv_get(tool_property_window.viewers.apply, XV_RECT);

	xv_set(tool_property_window.viewers.apply,
		XV_X, (int)xv_get(tool_property_window.viewers.panel,
		    XV_WIDTH)/2 - butrect->r_width,
		NULL);

	xv_create(tool_property_window.viewers.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Reset",
		PANEL_NOTIFY_PROC, viewers_props_reset_proc,
		XV_HELP_DATA, "ftptool:PropertyReset",
		NULL);

	xv_set(tool_property_window.viewers.panel,
		PANEL_DEFAULT_ITEM, tool_property_window.viewers.apply,
		NULL);

	window_fit_height(tool_property_window.viewers.panel);

	/* end panels */
	xv_set(tool_property_window.viewers.panel,
		XV_SHOW, FALSE,
		NULL);
	xv_set(tool_property_window.directory_lists.panel,
		XV_SHOW, FALSE,
		NULL);
	xv_set(tool_property_window.ftptool.panel,
		XV_SHOW, TRUE,
		NULL);

	switch_category(0, TRUE);
	set_remote_sort_order(remote_sort_mode);
	set_local_sort_order(local_sort_mode);
}

#ifdef USE_PROTOTYPES
void create_local_window(void)
#else
void create_local_window()
#endif
{
	Menu local_list_menu;
	Rect	rect;
	int		width, height, x, y;
	Panel_item	message;
#ifdef SYSV
	struct utsname uts;
#endif

#ifdef SYSV
	if (uname(&uts) != -1) {
		strcpy(myhostname, uts.nodename);
	} else {
		fprintf(stderr, "What host is this?\n");
		strcpy(myhostname, "unknown");
	}
#else
	if (gethostname(myhostname, MAXHOSTNAMELEN) == -1) {
		fprintf(stderr, "What host is this?\n");
		strcpy(myhostname, "unknown");
	}
#endif

	sprintf(scratch, "Ftptool: Local Host - %s", myhostname);

	local_window.frame = xv_create(base_window.frame,
		FRAME_CMD,
#ifdef XVIEW3
		FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
#else
		FRAME_CMD_PUSHPIN_IN, TRUE,
#endif
		XV_LABEL, scratch,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_LEFT_FOOTER, NULL,
		NULL);

	local_window.panel = xv_get(local_window.frame, FRAME_CMD_PANEL);

	xv_set(local_window.panel,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);

	local_window.directory = xv_create(local_window.panel,
		PANEL_TEXT,
		PANEL_VALUE_DISPLAY_LENGTH, 44,
		PANEL_VALUE_STORED_LENGTH, MAXPATHLEN,
		PANEL_LABEL_STRING, "Local Directory:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_LEVEL, PANEL_SPECIFIED,
		PANEL_NOTIFY_PROC, local_cd_text,
		XV_HELP_DATA, "ftptool:LocalDirectory",
		NULL);

	xv_set(local_window.panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	xv_set(local_window.panel,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);

	message = xv_create(local_window.panel,
		PANEL_MESSAGE,
		PANEL_LABEL_STRING, "Free Space:",
		PANEL_LABEL_BOLD, TRUE,
		NULL);

	justify_items(local_window.panel, FALSE);

	local_window.list = xv_create(local_window.panel,
		PANEL_LIST,
		PANEL_LIST_DISPLAY_ROWS, 8,
		PANEL_LABEL_STRING, list_label,
		PANEL_FONT, bold_list_font,
		PANEL_LAYOUT, PANEL_VERTICAL,
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_READ_ONLY, TRUE,
		PANEL_NOTIFY_PROC, local_list_proc,
		XV_HELP_DATA, "ftptool:LocalList",
		NULL);

	local_list_menu = xv_get(local_window.list, PANEL_ITEM_MENU);
#ifdef LINT
	local_list_menu = local_list_menu;
#else
	xv_set(local_list_menu,
		MENU_GEN_PIN_WINDOW, base_window.frame, "Local File List",
		MENU_TITLE_ITEM, "Local File List",
		MENU_ITEM,
			MENU_STRING, "",
			MENU_FEEDBACK, FALSE,
			NULL,
		MENU_ITEM,
			MENU_STRING, "CD to Selection",
			MENU_NOTIFY_PROC, local_cd_select,
			XV_HELP_DATA, "ftptool:LocalMenuCDSelection",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Up One Level",
			MENU_NOTIFY_PROC, local_cd_dotdot,
			XV_HELP_DATA, "ftptool:LocalMenuCDUp",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Copy to Remote",
			MENU_NOTIFY_PROC, put_proc,
			XV_HELP_DATA, "ftptool:LocalMenuCopyRemote",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Add to Batch Send List",
			MENU_NOTIFY_PROC, add_batch_send_proc,
			XV_HELP_DATA, "ftptool:BatchSendAdd",
			NULL,
		MENU_ITEM,
			MENU_STRING, "View File",
			MENU_NOTIFY_PROC, local_view,
			XV_HELP_DATA, "ftptool:LocalMenuView",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Delete",
			MENU_NOTIFY_PROC, local_delete_proc,
			XV_HELP_DATA, "ftptool:FileLocalRemote",
			NULL,
		MENU_ITEM,
			MENU_STRING, "Properties...",
			MENU_NOTIFY_PROC, local_properties,
			XV_HELP_DATA, "ftptool:LocalMenuProperties",
			NULL,
		NULL);
#endif

	change_local_list_menu();

	local_window.space = xv_create(local_window.panel, PANEL_MESSAGE,
		PANEL_LABEL_STRING, "",
		PANEL_ITEM_X, xv_get(message, XV_X) + xv_get(message, XV_WIDTH)
				+ xv_col(local_window.panel, 1),
		PANEL_ITEM_Y, xv_get(message, XV_Y),
		NULL);

	local_window.dismiss = xv_create(local_window.panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Dismiss",
		PANEL_NOTIFY_PROC, dismiss_local_window,
		XV_SHOW, openlook_mode ? FALSE : TRUE,
		XV_HELP_DATA, "ftptool:DismissButton",
		NULL);

	window_fit(local_window.panel);
	window_fit(local_window.frame);

	XSync(dpy, False);


	frame_get_rect(base_window.frame, &rect);

	x = rect.r_left;
	y = rect.r_top + rect.r_height;

	width = xv_get(local_window.frame, XV_WIDTH);
	height = xv_get(local_window.frame, XV_HEIGHT);

	rect.r_left = x;
	rect.r_top = y;
	rect.r_height = height;

	frame_set_rect(local_window.frame, &rect);
	frame_set_rect(local_window.panel, &rect);

	set_geometry(local_window.geometry, local_window.frame,
	    width, height, x, y);

	resize_window(local_window.panel, local_window.list,
		local_window.dismiss);

	xv_set(local_window.frame,
		WIN_EVENT_PROC, local_event_proc,
		NULL);
#ifdef XVIEW3
	local_window.drop_site = xv_create(local_window.frame, DROP_SITE_ITEM,
		DROP_SITE_ID, 5678,
		DROP_SITE_REGION, xv_get(local_window.list, XV_RECT),
		NULL);
	local_window.dnd = xv_create(local_window.frame, DRAGDROP,
		DND_TYPE, DND_COPY,
		NULL);
	local_window.sel = xv_create(local_window.frame, SELECTION_REQUESTOR,
		NULL);
	get_local_list_event_proc();
	xv_set(local_window.list,
		PANEL_EVENT_PROC, local_list_event,
		NULL);
#endif
}
