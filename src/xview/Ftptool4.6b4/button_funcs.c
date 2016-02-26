#include "ftptool.h"

#pragma ident   "@(#)button_funcs.c 1.10     93/11/05"

#ifdef USE_PROTOTYPES
void	connect_proc(void)
#else
void	connect_proc()
#endif
{
	if (!strcmp((char *)xv_get(base_window.connect,
		PANEL_LABEL_STRING), " Connect... ")) {
		if (xv_get(host_window.frame, XV_SHOW) == FALSE) {
			xv_set(host_window.frame,
				XV_SHOW, TRUE,
				NULL);
			return;
		}
		dowhat = DOCONNECT;
		notify_stop();
	} else {
		/* disconnect */
		/* used to make sure we are connected */
		/* with PANEL_INACTIVE, don't need to anymore */
		footer_message("Disconnecting...");
		footer_message("");
		disconnect();
	}
}

#ifdef USE_PROTOTYPES
void	local_properties(void)
#else
void	local_properties()
#endif
{
	if (!openlook_mode && xv_get(local_file_properties.frame,
	    XV_SHOW) == TRUE){
		xv_set(local_file_properties.frame,
			XV_SHOW, FALSE,
			NULL);
	} else {
		xv_set(local_file_properties.frame,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	remote_properties(void)
#else
void	remote_properties()
#endif
{
	if (!openlook_mode && xv_get(remote_file_properties.frame,
	    XV_SHOW) == TRUE){
		xv_set(remote_file_properties.frame,
			XV_SHOW, FALSE,
			NULL);
	} else {
		xv_set(remote_file_properties.frame,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	props_proc(void)
#else
void	props_proc()
#endif
{
	if (!openlook_mode && xv_get(tool_property_window.frame,
	    XV_SHOW) == TRUE){
		xv_set(tool_property_window.frame,
			XV_SHOW, FALSE,
			NULL);
	} else {
		xv_set(tool_property_window.frame,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	get_proc(void)
#else
void	get_proc()
#endif
{
	dowhat = DOGET;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	batchget_proc(void)
#else
void	batchget_proc()
#endif
{
	dowhat = DOBATCHGET;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	uncompress_proc(void)
#else
void	uncompress_proc()
#endif
{
	dowhat = DOUNCOMPRESS;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	compress_proc(void)
#else
void	compress_proc()
#endif
{
	dowhat = DOCOMPRESS;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	tar_proc(void)
#else
void	tar_proc()
#endif
{
	dowhat = DOGETTARFILENAME;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	create_tar_proc(void)
#else
void	create_tar_proc()
#endif
{
	dowhat = DOTAR;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	extract_proc(void)
#else
void	extract_proc()
#endif
{
	dowhat = DOEXTRACT;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	put_proc(void)
#else
void	put_proc()
#endif
{
	dowhat = DOPUT;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	batchput_proc(void)
#else
void	batchput_proc()
#endif
{
	dowhat = DOBATCHPUT;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	remote_view(void)
#else
void	remote_view()
#endif
{
	dowhat = DOREMOTEVIEW;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	local_view(void)
#else
void	local_view()
#endif
{
	dowhat = DOLOCALVIEW;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	local_dir_view(void)
#else
void	local_dir_view()
#endif
{
	if (!openlook_mode && xv_get(local_window.frame, XV_SHOW) == TRUE) {
		xv_set(local_window.frame,
			XV_SHOW, FALSE,
			NULL);
	} else {
		xv_set(local_window.frame,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	session_view(void)
#else
void	session_view()
#endif
{
	if (!openlook_mode && xv_get(session_window.frame, XV_SHOW) == TRUE) {
		xv_set(session_window.frame,
			XV_SHOW, FALSE,
			NULL);
	} else {
		xv_set(session_window.frame,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	status_view(void)
#else
void	status_view()
#endif
{
	if (!openlook_mode && xv_get(status_window.frame, XV_SHOW) == TRUE) {
		xv_set(status_window.frame,
			XV_SHOW, FALSE,
			NULL);
	} else {
		xv_set(status_window.frame,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	host_view(void)
#else
void	host_view()
#endif
{
	if (!openlook_mode && xv_get(host_window.frame, XV_SHOW) == TRUE) {
		xv_set(host_window.frame,
			XV_SHOW, FALSE,
			NULL);
	} else {
		xv_set(host_window.frame,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	schedule_view(void)
#else
void	schedule_view()
#endif
{
	if (!openlook_mode && xv_get(schedule_window.frame, XV_SHOW) == TRUE) {
		xv_set(schedule_window.frame,
			XV_SHOW, FALSE,
			NULL);
	} else {
		xv_set(schedule_window.frame,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	abort_proc(void)
#else
void	abort_proc()
#endif
{
	abort_transfer = 1;
	xv_set(base_window.frame,
		FRAME_BUSY, TRUE,
		NULL);
}


#ifdef USE_PROTOTYPES
int local_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event)
#else
int local_list_proc(item, string, client_data, op, event)
Panel_item		item;
char			*string;
Xv_opaque		client_data;
Panel_list_op   op;
Event			*event;
#endif
{
	struct dirlist *tmp = (struct dirlist *)client_data;
	static struct dirlist *lastselected;
	static double lasttime;
	double	newtime;

	local_footer_message("");
	switch (op) {
	case PANEL_LIST_OP_SELECT:
		if (S_ISREG(tmp->mode) || S_ISLNK(tmp->mode)) {
			local_list_nfiles++;
		} else if (S_ISDIR(tmp->mode)) {
			local_list_ndirs++;
		} else
			local_list_nothers++;
		show_stats(&local_file_properties, tmp);
		break;
	case PANEL_LIST_OP_DESELECT:
		if (S_ISREG(tmp->mode) || S_ISLNK(tmp->mode)) {
			local_list_nfiles--;
		} else if (S_ISDIR(tmp->mode)) {
			local_list_ndirs--;
		} else
			local_list_nothers--;
		if ((local_list_nfiles + local_list_ndirs +
		    local_list_nothers) == 0)
			inactivate_props(&local_file_properties);
		break;
	default:
		return (XV_OK);
	}
	local_show_items();
	if (tmp == lastselected) {
		newtime = event->ie_time.tv_sec +
		    event->ie_time.tv_usec/1000000.0;
		if ((newtime - lasttime) <= click_timeout) {
			local_doubleclick(tmp);
			lastselected = NULL;
			goto out;
		}
	}
	lastselected = tmp;
	lasttime = event->ie_time.tv_sec + event->ie_time.tv_usec/1000000.0;
out:
	change_local_list_menu();
	return (XV_OK);
}

#ifdef USE_PROTOTYPES
int remote_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event)
#else
int remote_list_proc(item, string, client_data, op, event)
Panel_item		item;
char			*string;
Xv_opaque 		client_data;
Panel_list_op   op;
Event			*event;
#endif
{
	struct dirlist *tmp = (struct dirlist *)client_data;
	static struct dirlist *lastselected;
	static double lasttime;
	double	newtime;

	footer_message("", (char *)NULL);
	if (!connected)
		return (XV_OK);
	switch (op) {
	case PANEL_LIST_OP_SELECT:
		if (non_unix || S_ISREG(tmp->mode) || S_ISLNK(tmp->mode)) {
			remote_list_nfiles++;
		} else if (S_ISDIR(tmp->mode)) {
			remote_list_ndirs++;
		} else
			remote_list_nothers++;
		show_stats(&remote_file_properties, tmp);
		break;
	case PANEL_LIST_OP_DESELECT:
		if (non_unix || S_ISREG(tmp->mode) || S_ISLNK(tmp->mode)) {
			remote_list_nfiles--;
		} else if (S_ISDIR(tmp->mode)) {
			remote_list_ndirs--;
		} else
			remote_list_nothers--;
		if ((remote_list_nfiles + remote_list_ndirs +
		    remote_list_nothers) == 0)
			inactivate_props(&remote_file_properties);
		break;
	default:
		return (XV_OK);
	}
	remote_show_items();
	if (tmp == lastselected) {
		newtime = event->ie_time.tv_sec +
		    event->ie_time.tv_usec/1000000.0;
		if ((newtime - lasttime) <= click_timeout) {
			if (S_ISLNK(tmp->mode))
				which_remote_file = linkname(tmp->name);
			else
				which_remote_file = strdup(tmp->name);
			if (which_remote_file == NULL) {
				fprintf(stderr, "Out of memory.\n");
			} else {
				which_remote_mode = tmp->mode;
				which_remote_size = tmp->size;
				dowhat = DOREMOTEDOUBLECLICK;
				notify_stop();
			}
			lastselected = NULL;
			goto out;
		}
	}
	lastselected = tmp;
	lasttime = event->ie_time.tv_sec + event->ie_time.tv_usec/1000000.0;
out:
	change_remote_list_menu();
	return (XV_OK);
}

#ifdef USE_PROTOTYPES
int host_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event)
#else
int host_list_proc(item, string, client_data, op, event)
Panel_item		item;
char			*string;
Xv_opaque 		client_data;
Panel_list_op   op;
Event			*event;
#endif
{
	struct hostlist *tmp = (struct hostlist *)client_data;
	static struct hostlist *lastselected;
	static double lasttime;
	double  newtime;

	switch (op) {
	case PANEL_LIST_OP_SELECT:
		host_window_update(tmp);
		break;
	case PANEL_LIST_OP_DESELECT:
		break;
	default:
		return (XV_OK);
	}
	if (connected)
		return (XV_OK);
	if (tmp == lastselected) {
		newtime = event->ie_time.tv_sec +
		    event->ie_time.tv_usec/1000000.0;
	if ((newtime - lasttime) <= click_timeout) {
		dowhat = DOCONNECT; 
		notify_stop();   
		lastselected = NULL;
		goto out;
	}
	}
	lastselected = tmp;
	lasttime = event->ie_time.tv_sec + event->ie_time.tv_usec/1000000.0;
out:                  
	return (XV_OK);
}

#ifdef USE_PROTOTYPES
int send_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event)
#else
int send_list_proc(item, string, client_data, op, event)
Panel_item		item;
char			*string;
Xv_opaque 		client_data;
Panel_list_op   op;
Event			*event;
#endif
{
	switch (op) {
	case PANEL_LIST_OP_SELECT:
		nsenditems++;
		break;
	case PANEL_LIST_OP_DESELECT:
		nsenditems--;
		break;
	default:
		return (XV_OK);
	}
	return (XV_OK);
}

#ifdef USE_PROTOTYPES
int receive_list_proc(Panel_item item, char *string, Xv_opaque client_data,
	Panel_list_op op, Event *event)
#else
int receive_list_proc(item, string, client_data, op, event)
Panel_item		item;
char			*string;
Xv_opaque 		client_data;
Panel_list_op   op;
Event			*event;
#endif
{
	switch (op) {
	case PANEL_LIST_OP_SELECT:
		nreceiveitems++;
		break;
	case PANEL_LIST_OP_DESELECT:
		nreceiveitems--;
		break;
	default:
		return (XV_OK);
	}
	return (XV_OK);
}


#ifdef USE_PROTOTYPES
void apply_changes(void)
#else
void apply_changes()
#endif
{
	int	answer;
#ifdef XVIEW3
	Xv_notice notice;
#endif

	sprintf(scratch, "replace your %s and %s files. ",
		FTPTOOL_DEFAULTS, FTPTOOL_TYPES);
#ifdef XVIEW3

	/* set resources */
	notice = xv_create(tool_property_window.panel, NOTICE,
		NOTICE_MESSAGE_STRINGS,
		    "Applying your changes will only apply to this session, but",
		    "can also be saved for future sessions. Saving changes will",
		    scratch,
		    NULL,
		NOTICE_BUTTON_YES,	"Apply to session only",
		NOTICE_BUTTON,		"Save changes", 2,
		NOTICE_STATUS, &answer,
		XV_SHOW, TRUE,
		NULL);
	xv_destroy_safe(notice);
#else
	answer = notice_prompt(tool_property_window.panel, NULL,
		NOTICE_MESSAGE_STRINGS,
			"Applying your changes will only apply to this session, but",
			"can also be saved for future sessions. Saving changes will",
			scratch,
			NULL,
		NOTICE_BUTTON_YES,	"Apply to session only",
		NOTICE_BUTTON,		"Save changes", 2,
		NULL);
#endif
	if (answer == NOTICE_YES)
		return;


	set_xdefaults();
	save_xdefaults();
	save_extensions();
}

#ifdef USE_PROTOTYPES
void ftptool_props_apply_proc(void)
#else
void ftptool_props_apply_proc()
#endif
{
	int	oldopenlook_mode;
	int	oldignore_case;
	int	changed = 0;
	char	*s;
	int	options;
	int	oldoptions;
	struct dirlist *head;

	/* selections */
	/* logging keepalive */
	oldoptions = logging | keepalive;
	options = xv_get(tool_property_window.ftptool.options, PANEL_VALUE);
	logging = options & 0x1;
	keepalive = options & 0x2;
	if (logging) {
		xv_set(session_window.frame,
			XV_SHOW, TRUE,
			NULL);
	} else {
		xv_set(session_window.frame,
			XV_SHOW, FALSE,
			NULL);
	}

	if (oldoptions != options)
		changed++;

	oldoptions = unique_local_names | unique_remote_names;
	options = xv_get(tool_property_window.ftptool.unique, PANEL_VALUE);
	unique_local_names = options & 0x1;
	unique_remote_names = options & 0x2;

	if (oldoptions != options)
		changed++;

	oldoptions = auto_connect | show_status | try_proxy;
	options = xv_get(tool_property_window.ftptool.automatic, PANEL_VALUE);
	auto_connect = options & 0x1;
	show_status = options & 0x2;
	try_proxy = options & 0x4;

	if (oldoptions != options)
		changed++;

	oldoptions = confirmdeletes | confirmoverwrites;
	options = xv_get(tool_property_window.ftptool.confirm, PANEL_VALUE);
	confirmdeletes = options & 0x1;
	confirmoverwrites = options & 0x2;

	if (oldoptions != options)
		changed++;

	oldignore_case = ignore_case;
	ignore_case = xv_get(tool_property_window.ftptool.ignore_case,
		PANEL_VALUE);
	if (oldignore_case != ignore_case) {
		hostlist_head = sort_hostlist(hostlist_head);
		reload_host_list_menu(hostlist_head);

		if (local_sort_mode == SORTBYNAME) {
			clear_slist(local_window.list);
			head = sort_dirlist(local_dircache.first->dlist,
				local_sort_mode, local_sort_direction);
			if (head != NULL) {
				dirlist_to_slist(local_window.list, head);
			}
		}

		if (remote_sort_mode == SORTBYNAME && connected) {
			clear_slist(base_window.list);
			head = sort_dirlist(remote_dircache.first->dlist,
				remote_sort_mode, remote_sort_direction);
			if (head != NULL) {
				dirlist_to_slist(base_window.list, head);
			}
		}

		changed = 1;
	}

	oldopenlook_mode = openlook_mode;
	openlook_mode = xv_get(tool_property_window.ftptool.openlook,
		PANEL_VALUE);

	if (oldopenlook_mode != openlook_mode)
		changed = 1;

	if (openlook_mode) {
		xv_set(base_window.quit,
			XV_SHOW, FALSE,
			NULL);
		xv_set(local_window.dismiss,
			XV_SHOW, FALSE,
			NULL);
		xv_set(local_file_properties.dismiss,
			XV_SHOW, FALSE,
			NULL);
		xv_set(remote_file_properties.dismiss,
			XV_SHOW, FALSE,
			NULL);
		xv_set(schedule_window.dismiss,
			XV_SHOW, FALSE,
			NULL);
		xv_set(status_window.dismiss,
			XV_SHOW, FALSE,
			NULL);

	} else {
		xv_set(base_window.quit,
			XV_SHOW, TRUE,
			NULL);
		xv_set(local_window.dismiss,
			XV_SHOW, TRUE,
			NULL);
		xv_set(local_file_properties.dismiss,
			XV_SHOW, TRUE,
			NULL);
		xv_set(remote_file_properties.dismiss,
			XV_SHOW, TRUE,
			NULL);
		xv_set(schedule_window.dismiss,
			XV_SHOW, TRUE,
			NULL);
		xv_set(status_window.dismiss,
			XV_SHOW, TRUE,
			NULL);
	}
	resize_window(local_window.panel, local_window.list,
		local_window.dismiss);

	add_dismiss(host_window.basic.panel, host_window.basic.connect,
		host_window.basic.dismiss);
	add_dismiss(about_window.panel, about_window.mail,
		about_window.dismiss);

	s = (char *)xv_get(tool_property_window.ftptool.anonftp, PANEL_VALUE);
	if (*s != '\0' && strcmp(s, anonftp_password) != 0) {
		changed = 1;
		free(anonftp_password);
		anonftp_password = strdup(s);
	}

	if (changed)
		apply_changes();
}

#ifdef USE_PROTOTYPES
void directory_lists_props_apply_proc(void)
#else
void directory_lists_props_apply_proc()
#endif
{
	int	olddircache_size;
	int	oldsort_mode;
	int	oldsort_direction;
	int	olddotfiles;
	int	oldgroup_files;
	struct dirlist *head;
	int	changed = 0;

	olddircache_size = dircache_size;
	if (xv_get(tool_property_window.directory_lists.cache_inf,
		PANEL_VALUE) == 1)
		dircache_size = 0;
	else
		dircache_size = xv_get(
		    tool_property_window.directory_lists.cache,
			PANEL_VALUE);
	if (olddircache_size != dircache_size) {
		changed = 1;
		dircache_shrink(&local_dircache, dircache_size);
		dircache_shrink(&remote_dircache, dircache_size);
	}

	oldsort_mode = remote_sort_mode;
	remote_sort_mode=
		xv_get(tool_property_window.directory_lists.remote_sort,
			PANEL_VALUE);

	oldsort_direction = remote_sort_direction;
	remote_sort_direction=
		xv_get(tool_property_window.directory_lists.remote_sortdir,
			PANEL_VALUE);

	olddotfiles = local_showdotfiles;
	local_showdotfiles =
		xv_get(tool_property_window.directory_lists.local_dotfiles,
			PANEL_VALUE);
	if (olddotfiles != local_showdotfiles) {
		changed = 1;
		change_local_dir(".", 0);
	}

	olddotfiles = remote_showdotfiles;
	remote_showdotfiles =
		xv_get(tool_property_window.directory_lists.remote_dotfiles,
			PANEL_VALUE);
	if (olddotfiles != remote_showdotfiles) {
		changed = 1;
		if (connected) {
			which_remote_file = strdup(".");
			if (which_remote_file == NULL) {
				fprintf(stderr, "Out of memory.\n");
				exit(1);
			} else {
				dowhat = DOREMOTECD;
				notify_stop();
			}
		}
	}

	oldgroup_files = group_remote_files;
	group_remote_files=
		xv_get(tool_property_window.directory_lists.remote_group,
			PANEL_VALUE);

	if ((oldsort_mode != remote_sort_mode) ||
	    (oldsort_direction != remote_sort_direction) ||
	    (oldgroup_files != group_remote_files)) {
		changed = 1;
		if (connected && (remote_showdotfiles == olddotfiles)) {
			clear_slist(base_window.list);
			head = sort_dirlist(remote_dircache.first->dlist,
				remote_sort_mode, remote_sort_direction);
			if (head != NULL) {
				dirlist_to_slist(base_window.list, head);
			}
		}
	}

	oldsort_mode = local_sort_mode;
	local_sort_mode =
		xv_get(tool_property_window.directory_lists.local_sort,
			PANEL_VALUE);

	oldsort_direction = local_sort_direction;
	local_sort_direction =
		xv_get(tool_property_window.directory_lists.local_sortdir,
			PANEL_VALUE);

	oldgroup_files = group_local_files;
	group_local_files =
		xv_get(tool_property_window.directory_lists.local_group,
			PANEL_VALUE);

	if ((oldsort_mode != local_sort_mode) ||
	    (oldsort_direction != local_sort_direction) ||
	    (oldgroup_files != group_local_files)) {
		changed = 1;
		clear_slist(local_window.list);
		head = sort_dirlist(local_dircache.first->dlist,
			local_sort_mode, local_sort_direction);
		if (head != NULL) {
			dirlist_to_slist(local_window.list, head);
		}
	}

	if (changed)
		apply_changes();
}

#ifdef USE_PROTOTYPES
void viewers_props_apply_proc(void)
#else
void viewers_props_apply_proc()
#endif
{
	int	changed = 0;
	char	*s;

	s = (char *)xv_get(tool_property_window.viewers.viewer, PANEL_VALUE);
	if (*s != '\0' && strcmp(s, default_viewer) != 0) {
		changed = 1;
		free(default_viewer);
		default_viewer = strdup(s);
	}

	s = (char *)xv_get(tool_property_window.viewers.compressor,
	    PANEL_VALUE);
	if (*s != '\0' && strcmp(s, default_compressor) != 0) {
		changed = 1;
		free(default_compressor);
		default_compressor = strdup(s);
	}

	if (extensions_changed)
		changed = 1;

	if (changed)
		apply_changes();
}

#ifdef USE_PROTOTYPES
void ftptool_props_reset_proc(void)
#else
void ftptool_props_reset_proc()
#endif
{
	int	options;

	options = logging | keepalive;
	xv_set(tool_property_window.ftptool.options,
		PANEL_VALUE, options,
		NULL);
	options = unique_local_names | unique_remote_names;
	xv_set(tool_property_window.ftptool.unique,
		PANEL_VALUE, options,
		NULL);
	options = auto_connect | show_status | try_proxy;
	xv_set(tool_property_window.ftptool.automatic,
		PANEL_VALUE, options,
		NULL);
	xv_set(tool_property_window.ftptool.openlook,
		PANEL_VALUE, openlook_mode,
		NULL);
	xv_set(tool_property_window.ftptool.confirm,
		PANEL_VALUE, confirmdeletes,
		NULL);
	xv_set(tool_property_window.ftptool.anonftp,
		PANEL_VALUE, anonftp_password,
		NULL);
	xv_set(tool_property_window.ftptool.ignore_case,
		PANEL_VALUE, ignore_case,
		NULL);
}

#ifdef USE_PROTOTYPES
void directory_lists_props_reset_proc(void)
#else
void directory_lists_props_reset_proc()
#endif
{
	xv_set(tool_property_window.directory_lists.cache_inf,
		PANEL_VALUE, dircache_size == 0,
		NULL);
	xv_set(tool_property_window.directory_lists.cache,
		PANEL_VALUE, dircache_size,
		NULL);
	xv_set(tool_property_window.directory_lists.remote_sort,
		PANEL_VALUE, remote_sort_mode,
		NULL);
	xv_set(tool_property_window.directory_lists.remote_sortdir,
		PANEL_VALUE, remote_sort_direction,
		NULL);
	xv_set(tool_property_window.directory_lists.remote_dotfiles,
		PANEL_VALUE, remote_showdotfiles,
		NULL);
	xv_set(tool_property_window.directory_lists.remote_group,
		PANEL_VALUE, group_remote_files,
		NULL);
	set_remote_sort_order(remote_sort_direction);
	xv_set(tool_property_window.directory_lists.local_sort,
		PANEL_VALUE, local_sort_mode,
		NULL);
	xv_set(tool_property_window.directory_lists.local_sortdir,
		PANEL_VALUE, local_sort_direction,
		NULL);
	set_local_sort_order(local_sort_direction);
	xv_set(tool_property_window.directory_lists.local_dotfiles,
		PANEL_VALUE, local_showdotfiles,
		NULL);
	xv_set(tool_property_window.directory_lists.local_group,
		PANEL_VALUE, group_local_files,
		NULL);
}

#ifdef USE_PROTOTYPES
void viewers_props_reset_proc(void)
#else
void viewers_props_reset_proc()
#endif
{
	xv_set(tool_property_window.viewers.viewer,
		PANEL_VALUE, default_viewer,
		NULL);

	xv_set(tool_property_window.viewers.compressor,
		PANEL_VALUE, default_compressor,
		NULL);
}

char	about_message[] =
"Ftptool was written by Mike Sullivan, now of the DDI group:\n\
   1. To make it easier for Sun training centers to retrieve\n\
      course material from our server. \n\
   2. To learn about XView and X (somewhat). \n\
   3. To make FTP easier to use. \n\
\n\
Ftptool is still under development, so suggestions (and bug reports, of course!) are welcome (although bug reports aren't _quite_ so welcome :-). Send mail to me with the \"Send Mail\" button below.\n\
\n\
Retrieve the HISTORY file in the Tools directory on yavin.Eng (via anonymous FTP) to see what's changed, and what's planned.  \n\
\n\
There is now an ftptool-users alias on yavin.Eng.Sun.COM. I'll notify this alias of new releases. \n\
\n\
Credits:\n\
\n\
This about window is based on code from Larry Wake. Well, it is more like completely his code.\n\
\n\
The code dealing with pseudo-terminals (which I now think I understand) is from UNIX Network Programming by Richard Stevens.\n\
\n\
Many of the features in Ftptool were suggested by members of the internal tstech alias/newsgroup, along with the Internet community. Thanks to everyone!\n\
\n\
Ported to 386 SVR4 by Jerry Whelan <guru@stasi.bradley.edu>. \n\
\n\
Ported to Ultrix by Robert Evans <Robert.Evans@cm.cf.ac.uk>. \n\
"
;

#ifdef USE_PROTOTYPES
void about_proc(void)
#else
void about_proc()
#endif
{
#ifdef USE_PROTOTYPES
	void textsw_normalize_view(Textsw textsw, Textsw_index pos);
#else
	void textsw_normalize_view();
#endif

	textsw_reset(about_window.message, 0, 0);
	textsw_insert(about_window.message, about_message,
		strlen(about_message));
	textsw_normalize_view(about_window.message, 0);
	xv_set(about_window.frame, XV_SHOW, TRUE, NULL);
}

char	*sort_dir_strings[] = {
	"Alphabetical", 			"Reverse Alphabetical",
	"Least Recently Changed", 	"Most Recently Changed",
	"Smallest First", 			"Largest First",
};

#ifdef USE_PROTOTYPES
void	remote_sort_choice_proc(void)
#else
void	remote_sort_choice_proc()
#endif
{
	int	val;

	/* get the new value, and change the strings appropriately */

	val = xv_get(tool_property_window.directory_lists.remote_sort,
		PANEL_VALUE);
	set_remote_sort_order(val);

}

#ifdef USE_PROTOTYPES
void set_remote_sort_order(int val)
#else
void set_remote_sort_order(val)
int	val;
#endif
{
	xv_set(tool_property_window.directory_lists.remote_sortdir,
		PANEL_CHOICE_STRINGS,
			sort_dir_strings[2 * val],
			sort_dir_strings[2 * val + 1],
			NULL,
		NULL);
}

#ifdef USE_PROTOTYPES
void	local_sort_choice_proc(void)
#else
void	local_sort_choice_proc()
#endif
{
	int	val;

	/* get the new value, and change the strings appropriately */

	val = xv_get(tool_property_window.directory_lists.local_sort,
		PANEL_VALUE);
	set_local_sort_order(val);

}

#ifdef USE_PROTOTYPES
void set_local_sort_order(int val)
#else
void set_local_sort_order(val)
int	val;
#endif
{
	xv_set(tool_property_window.directory_lists.local_sortdir,
		PANEL_CHOICE_STRINGS,
			sort_dir_strings[2 * val],
			sort_dir_strings[2 * val + 1],
			NULL,
		NULL);
}

#ifdef USE_PROTOTYPES
void	about_send_proc(void)
#else
void	about_send_proc()
#endif
{
	xv_set(feedback_window.frame,
		XV_SHOW, TRUE,
		NULL);
}

#define	OTHER_ADDRESS 2
#define	TO_LEN  64
#define	MAILER "/usr/ucb/mail"

#ifdef USE_PROTOTYPES
void	feedback_address_proc(Panel_item item, unsigned int value,
	Event *event)
#else
void	feedback_address_proc(item, value, event)
Panel_item	item;
unsigned int value;
Event *event;
#endif
{
	if (value == OTHER_ADDRESS) {
		xv_set(feedback_window.other,
			XV_SHOW, TRUE,
			NULL);
	} else {
		xv_set(feedback_window.other,
			XV_SHOW, FALSE,
			NULL);
	}

}


#ifdef USE_PROTOTYPES
void	feedback_send_proc(Panel_item item, Event *event)
#else
void	feedback_send_proc(item, event)
Panel_item	item;
Event	*event;
#endif
{
	FILE *pp;
	char addr[TO_LEN + 1], buf[BUFSIZ+1];
	int addr_sel, last_was_NL = 0;
	Textsw_index cur_pos, next_pos;
	char	*sigfile;
	FILE	*sigfp;
	int		ch;

	static char *fb_cmd = NULL;

	if ((int)xv_get(feedback_window.feedback, TEXTSW_LENGTH) == 0) {
		footer_message("No text in message.");
		xv_set(item,
			PANEL_NOTIFY_STATUS, XV_ERROR,
			NULL);
		return;
	}

	addr_sel = (int)xv_get(feedback_window.which, PANEL_VALUE);
	if (addr_sel == OTHER_ADDRESS)
		strncpy(addr,
			(char *)xv_get(feedback_window.other, PANEL_VALUE),
			TO_LEN);
	else
		strncpy(addr, (char *)xv_get(feedback_window.which,
			PANEL_CHOICE_STRING, addr_sel), TO_LEN);

	if (addr[0] == '\0') {
		footer_message("No address specified.");
		xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, NULL);
		return;
	}

	if (fb_cmd == NULL)
		fb_cmd = (char *)malloc((unsigned int)(strlen(MAILER) +
		    TO_LEN + 1));


	sprintf(fb_cmd, "%s %s", MAILER, addr);
	if ((pp = popen(fb_cmd, "w")) == NULL) {
		footer_message("popen error; couldn't send feedback message!");
		xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, NULL);
		return;
	}

	fprintf(pp, "~s %s Comment\n\n", header_name);

	fprintf(pp, "\n");

	next_pos = 0;
	cur_pos = next_pos - BUFSIZ;
	while (next_pos == cur_pos + BUFSIZ) {
		cur_pos = next_pos;
		next_pos = (Textsw_index)xv_get(feedback_window.feedback,
			TEXTSW_CONTENTS, cur_pos, buf, BUFSIZ);
		if ((next_pos - cur_pos) != 0) {
			buf[next_pos - cur_pos] = '\0';
			fprintf(pp, "%s", buf);
			last_was_NL = (buf[next_pos-cur_pos-1] == '\n');
		}
	}
	/*
	 *  Force last char out to be a newline
	 */
	if (!last_was_NL)
		putc('\n', pp);

	sigfile = find_dotfile(".signature");
	if (sigfile) {
		sigfp = fopen(sigfile, "r");
		if (sigfp != NULL) {
			while ((ch = getc(sigfp)) != EOF)
				putc(ch, pp);
			fclose(sigfp);
		}
		free(sigfile);
	}

	if (pclose(pp) != 0) {
		footer_message("Mail failed -- message not sent!");
		xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, NULL);
		return;
	}
	textsw_reset(feedback_window.feedback, 0, 0);
}


#ifdef USE_PROTOTYPES
void	feedback_cancel_proc(void)
#else
void	feedback_cancel_proc()
#endif
{
	textsw_reset(feedback_window.feedback, 0, 0);
}

#ifdef USE_PROTOTYPES
Panel_setting	reject_spaces(Panel_item item, Event *event)
#else
Panel_setting	reject_spaces(item, event)
Panel_item	item;
Event	*event;
#endif
{
	switch (event_action(event)) {
	case ' ':
	case '\t':
		return (PANEL_NONE);
	default:
		return (panel_text_notify(item, event));
	}
}

#ifdef notdef

#ifdef USE_PROTOTYPES
void	host_window_choice_proc(Panel_item item, unsigned int value,
	Event *event)
#else
void	host_window_choice_proc(item, value, event)
Panel_item	item;
unsigned int value;
Event *event;
#endif
{
	if (value == 1) {
		xv_set(host_window.advanced.proxy,
			XV_SHOW, TRUE,
			NULL);
	} else {
		xv_set(host_window.advanced.proxy,
			XV_SHOW, FALSE,
			NULL);
	}
}

#endif

#ifdef USE_PROTOTYPES
void	remote_delete_proc(void)
#else
void	remote_delete_proc()
#endif
{
	int	nitems, row;
	struct dirlist *tmp;
	int	mode, dirchanged = 0;
	char	*name = NULL;

	xfer_buttons_inactive();
	if (ping_server())
		goto out;
	nitems = xv_get(base_window.list, PANEL_LIST_NROWS);
	/* start at 1 to skip '..' */
	for (row = 1; row < nitems; row++)
		if (xv_get(base_window.list, PANEL_LIST_SELECTED, row)) {
			tmp = (struct dirlist *)xv_get(base_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			mode = tmp->mode & S_IFMT;
			switch (mode) {
			case S_IFDIR:
				if (delete_remote_dir(tmp->name))
					goto out;
				dirchanged++;
				remote_list_ndirs--;
				break;
			case S_IFREG:
				if (delete_remote_file(tmp->name, "DELE"))
					goto out;
				dirchanged++;
				remote_list_nfiles--;
				break;
			case S_IFLNK:
				name = linkname(tmp->name);
				if (name == NULL)
					break;
				if (delete_remote_file(name, "DELE"))
					goto out;
				remote_list_nfiles--;
				dirchanged++;
				free(name);
				name = NULL;
				break;
			default:
				remote_list_nothers--;
				footer_message(
				    "Ignoring non-file/directory %s.",
				    tmp->name);
				log_message("Ignoring non-file.\n");
				break;
			}
			xv_set(base_window.list,
				PANEL_LIST_SELECT, row, FALSE,
				NULL);
			remote_show_items();
		}

out:
	if (name)
		free(name);
	if (dirchanged) {
		which_remote_file = strdup(".");
		if (which_remote_file == NULL) {
			fprintf(stderr, "Out of memory.\n");
		} else {
			dowhat = DOREMOTECDFORCE;
			notify_stop();
		}
	}
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
	return;
}

#ifdef USE_PROTOTYPES
void	local_delete_proc(void)
#else
void	local_delete_proc()
#endif
{
	int	nitems, row;
	struct dirlist *tmp;
	int	mode, dirchanged = 0;
	char	*name = NULL;

	xfer_buttons_inactive();
	nitems = xv_get(local_window.list, PANEL_LIST_NROWS);
	/* start at 1 to skip .. */
	for (row = 1; row < nitems; row++)
		if (xv_get(local_window.list, PANEL_LIST_SELECTED, row)) {
			tmp = (struct dirlist *)xv_get(local_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			mode = tmp->mode & S_IFMT;
			switch (mode) {
			case S_IFDIR:
				if (delete_local_dir(tmp->name))
					goto out;
				local_list_ndirs--;
				dirchanged++;
				break;
			case S_IFREG:
				if (delete_local_file(tmp->name, unlink))
					goto out;
				dirchanged++;
				local_list_nfiles--;
				break;
			case S_IFLNK:
				name = linkname(tmp->name);
				if (delete_local_file(name, unlink))
					goto out;
				dirchanged++;
				local_list_nfiles--;
				free(name);
				name = NULL;
				break;
			default:
				local_list_nothers--;
				local_footer_message(
					"Ignoring non-file/directory %s.",
					tmp->name);
				log_message("Ignoring non-file.\n");
				break;
			}
			xv_set(local_window.list,
				PANEL_LIST_SELECT, row, FALSE,
				NULL);
			local_show_items();
	}

out:
	if (name)
		free(name);
	if (dirchanged)
		change_local_dir(".", 1);
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
	return;
}

#ifdef USE_PROTOTYPES
void	show_load_receive_list_proc(void)
#else
void	show_load_receive_list_proc()
#endif
{
	xv_set(schedule_window.lsbutton,
		PANEL_LABEL_STRING, "Load",
		PANEL_NOTIFY_PROC, load_receive_list_proc,
		NULL);
	xv_set(schedule_window.lsframe,
		XV_LABEL, "Load Receive Batch List",
		XV_SHOW, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void	show_save_receive_list_proc(void)
#else
void	show_save_receive_list_proc()
#endif
{
	xv_set(schedule_window.lsbutton,
		PANEL_LABEL_STRING, "Save",
		PANEL_NOTIFY_PROC, save_receive_list_proc,
		NULL);
	xv_set(schedule_window.lsframe,
		XV_LABEL, "Save Receive Batch List",
		XV_SHOW, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void	show_load_send_list_proc(void)
#else
void	show_load_send_list_proc()
#endif
{
	xv_set(schedule_window.lsbutton,
		PANEL_LABEL_STRING, "Load",
		PANEL_NOTIFY_PROC, load_send_list_proc,
		NULL);
	xv_set(schedule_window.lsframe,
		XV_LABEL, "Load Send Batch List",
		XV_SHOW, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void	show_save_send_list_proc(void)
#else
void	show_save_send_list_proc()
#endif
{
	xv_set(schedule_window.lsbutton,
		PANEL_LABEL_STRING, "Save",
		PANEL_NOTIFY_PROC, save_send_list_proc,
		NULL);
	xv_set(schedule_window.lsframe,
		XV_LABEL, "Save Send Batch List",
		XV_SHOW, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void	load_send_list_proc(void)
#else
void	load_send_list_proc()
#endif
{
	char	*filename;

	filename = expand_dirname((char *)xv_get(schedule_window.filename,
		PANEL_VALUE));
	if (filename == NULL)
		return;

	load_batch_list(schedule_window.send_list, filename);
	free(filename);
}

#ifdef USE_PROTOTYPES
void	save_send_list_proc(void)
#else
void	save_send_list_proc()
#endif
{
	char	*filename;

	filename = expand_dirname((char *)xv_get(schedule_window.filename,
		PANEL_VALUE));
	if (filename == NULL)
		return;

	save_batch_list(schedule_window.send_list, filename);
	free(filename);
}

#ifdef USE_PROTOTYPES
void	load_receive_list_proc(void)
#else
void	load_receive_list_proc()
#endif
{
	char	*filename;

	filename = expand_dirname((char *)xv_get(schedule_window.filename,
		PANEL_VALUE));
	if (filename == NULL)
		return;

	load_batch_list(schedule_window.receive_list, filename);
	free(filename);
}

#ifdef USE_PROTOTYPES
void	save_receive_list_proc(void)
#else
void	save_receive_list_proc()
#endif
{
	char	*filename;

	filename = expand_dirname((char *)xv_get(schedule_window.filename,
		PANEL_VALUE));
	if (filename == NULL)
		return;

	save_batch_list(schedule_window.receive_list, filename);
	free(filename);
}

#ifdef USE_PROTOTYPES
void add_batch_send_proc(void)
#else
void add_batch_send_proc()
#endif
{
	struct dirlist *tmp;
	int	nitems, row;
	int	mode;
	char	*name;

	nitems = xv_get(local_window.list, PANEL_LIST_NROWS);
	/* ignore '..' */
	for (row = 1; row < nitems; row++) {
		if (xv_get(local_window.list, PANEL_LIST_SELECTED,
		    row) == FALSE)
			continue;
		tmp = (struct dirlist *)xv_get(local_window.list,
			PANEL_LIST_CLIENT_DATA, row);
		mode = tmp->mode & S_IFMT;
		if (non_unix)
			mode = S_IFREG;
		switch (mode) {
		case S_IFDIR:
			add_batchname(schedule_window.send_list, tmp->name,
			    tmp->mode, tmp->size, local_dircache.first->name);
			local_list_ndirs--;
			break;
		case S_IFREG:
			add_batchname(schedule_window.send_list, tmp->name,
			    tmp->mode, tmp->size, local_dircache.first->name);
			local_list_nfiles--;
			break;
		case S_IFLNK:
			name = linkname(tmp->name);
			if (name == NULL)
				break;
			add_batchname(schedule_window.send_list, name,
			    tmp->mode, tmp->size, local_dircache.first->name);
			local_list_nfiles--;
			free(name);
			break;
		default:
			footer_message("Ignoring non-directory/file %s",
			    tmp->name);
			local_list_nothers--;
			break;
		}
		xv_set(local_window.list,
			PANEL_LIST_SELECT, row, FALSE,
			NULL);
		local_show_items();
	}
	change_local_list_menu();
}

#ifdef USE_PROTOTYPES
void add_batch_receive_proc(void)
#else
void add_batch_receive_proc()
#endif
{
	struct dirlist *tmp;
	int	nitems, row;
	int	mode;
	char	*name;

	nitems = xv_get(base_window.list, PANEL_LIST_NROWS);
	/* ignore '..' */
	for (row = 1; row < nitems; row++) {
		if (xv_get(base_window.list, PANEL_LIST_SELECTED, row) == FALSE)
			continue;
		tmp = (struct dirlist *)xv_get(base_window.list,
			PANEL_LIST_CLIENT_DATA, row);
		mode = tmp->mode & S_IFMT;
		if (non_unix)
			mode = S_IFREG;
		switch (mode) {
		case S_IFDIR:
			add_batchname(schedule_window.receive_list,
			    tmp->name, tmp->mode, tmp->size,
			    remote_dircache.first->name);
			remote_list_ndirs--;
			break;
		case S_IFREG:
			add_batchname(schedule_window.receive_list,
			    tmp->name, tmp->mode, tmp->size,
			    remote_dircache.first->name);
			remote_list_nfiles--;
			break;
		case S_IFLNK:
			name = linkname(tmp->name);
			if (name == NULL)
				break;
			add_batchname(schedule_window.receive_list,
			    name, tmp->mode, tmp->size,
			    remote_dircache.first->name);
			remote_list_nfiles--;
			free(name);
			break;
		default:
			footer_message("Ignoring non-directory/file %s",
			    tmp->name);
			remote_list_nothers--;
			break;
		}
		xv_set(base_window.list,
			PANEL_LIST_SELECT, row, FALSE,
			NULL);
		remote_show_items();
	}
	change_remote_list_menu();
}

#ifdef USE_PROTOTYPES
void	props_inf_check_box(Panel_item item, int value, Event *event)
#else
void	props_inf_check_box(item, value, event)
Panel_item	item;
int		value;
Event	*event;
#endif
{
	if (value == 1) {
		xv_set(tool_property_window.directory_lists.cache,
			PANEL_INACTIVE, TRUE,
			NULL);
	} else {
		xv_set(tool_property_window.directory_lists.cache,
			PANEL_INACTIVE, FALSE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	quit_proc(void)
#else
void	quit_proc()
#endif
{
	destroy_func((Notify_client)NULL, DESTROY_PROCESS_DEATH);
	/* should not be connected now */
	if (connected)
		return;
	dowhat = DOQUIT;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void switch_category(int value, int show)
#else
void switch_category(value, show)
int	value;
int	show;
#endif
{
	Panel panel;
	Panel_item apply;
	void (*apply_func)();

	switch (value) {
	default:
		fprintf(stderr, "Invalid category in switch_category.\n");
		/* fall through */
	case 0:
		panel = tool_property_window.ftptool.panel;
		apply = tool_property_window.ftptool.apply;
		break;
	case 1:
		panel = tool_property_window.directory_lists.panel;
		apply = tool_property_window.directory_lists.apply;
		break;
	case 2:
		panel = tool_property_window.viewers.panel;
		apply = tool_property_window.viewers.apply;
		break;
	}
	if (show == TRUE) {
		int	width;

		width = xv_get(panel, XV_WIDTH);

		xv_set(tool_property_window.panel,
			XV_WIDTH, width,
			NULL);
		xv_set(panel,
			XV_SHOW, TRUE,
			NULL);
		xv_set(tool_property_window.frame,
			XV_WIDTH, width,
			XV_HEIGHT, xv_get(panel, XV_HEIGHT)
				+ xv_get(tool_property_window.panel, XV_HEIGHT),
			NULL);
		tool_property_window.apply = apply;
	} else {
		apply_func = (void (*)())xv_get(apply, PANEL_NOTIFY_PROC);
		(*apply_func)();
		xv_set(panel,
			XV_SHOW, FALSE,
			NULL);
	}
	xv_set(tool_property_window.frame,
		FRAME_LEFT_FOOTER, "",
		NULL);
}

#ifdef USE_PROTOTYPES
void category_proc(void)
#else
void category_proc()
#endif
{
	int	value;
	static int oldvalue = 0;

	value = xv_get(tool_property_window.category, PANEL_VALUE);

	switch_category(oldvalue, FALSE);
	switch_category(value, TRUE);
	oldvalue = value;
}

#ifdef USE_PROTOTYPES
void	dismiss_local_window(void)
#else
void	dismiss_local_window()
#endif
{
	xv_set(local_window.frame,
		XV_SHOW, FALSE,
#ifdef XVIEW3
		FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_OUT,
#else
		FRAME_CMD_PUSHPIN_IN, FALSE,
#endif
		NULL);
}

#ifdef USE_PROTOTYPES
void	dismiss_host_window(void)
#else
void	dismiss_host_window()
#endif
{
	xv_set(host_window.frame,
		XV_SHOW, FALSE,
#ifdef XVIEW3
		FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_OUT,
#else
		FRAME_CMD_PUSHPIN_IN, FALSE,
#endif
		NULL);
}

#ifdef USE_PROTOTYPES
void	dismiss_about_window(void)
#else
void	dismiss_about_window()
#endif
{
	xv_set(about_window.frame,
		XV_SHOW, FALSE,
#ifdef XVIEW3
		FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_OUT,
#else
		FRAME_CMD_PUSHPIN_IN, FALSE,
#endif
		NULL);
}

#ifdef USE_PROTOTYPES
void	dismiss_file_props_window(Panel_item item, Event *event)
#else
void	dismiss_file_props_window(item, event)
Panel_item	item;
Event	*event;
#endif
{
	Frame	frame;

	frame = xv_get(item, PANEL_CLIENT_DATA);
	xv_set(frame,
		XV_SHOW, FALSE,
#ifdef XVIEW3
		FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_OUT,
#else
		FRAME_CMD_PUSHPIN_IN, FALSE,
#endif
		NULL);
}

#ifdef USE_PROTOTYPES
void	dismiss_schedule_window(void)
#else
void	dismiss_schedule_window()
#endif
{
	xv_set(schedule_window.frame,
		XV_SHOW, FALSE,
#ifdef XVIEW3
		FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_OUT,
#else
		FRAME_CMD_PUSHPIN_IN, FALSE,
#endif
		NULL);
}

#ifdef USE_PROTOTYPES
void	dismiss_status_window(void)
#else
void	dismiss_status_window()
#endif
{
	xv_set(status_window.frame,
		XV_SHOW, FALSE,
#ifdef XVIEW3
		FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_OUT,
#else
		FRAME_CMD_PUSHPIN_IN, FALSE,
#endif
		NULL);
}

#ifdef USE_PROTOTYPES
void dir_list_proc(void)
#else
void dir_list_proc()
#endif
{
	dowhat = DODIR;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void ls_list_proc(void)
#else
void ls_list_proc()
#endif
{
	dowhat = DOLS;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void list_remote_dir(void)
#else
void list_remote_dir()
#endif
{
	FILE	*din = NULL;

	xfer_buttons_inactive();
	if (ping_server())
		goto out;
	if (dowhat == DODIR)
		din = open_remote_ls(0);
	else
		din = open_remote_ls(1);
	if (din == NULL) {
		goto out;
	}
	while (next_remote_line(din) != NULL) {
		log_message(response_line);
		log_char('\n');
	}
out:
	if (din)
		close_remote_ls(din);
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
}

#ifdef USE_PROTOTYPES
void save_layout_func(void)
#else
void save_layout_func()
#endif
{
	char    *filename = NULL;
	FILE	*fp;
	char	*false = "False";
	char	*true = "True";

	host_window.visible = xv_get(host_window.frame, XV_SHOW) == TRUE;

	host_window.advanced.visible =
		xv_get(host_window.advanced.panel, XV_SHOW) == TRUE;

	local_window.visible = xv_get(local_window.frame, XV_SHOW) == TRUE;
	schedule_window.visible =
	    xv_get(schedule_window.frame, XV_SHOW) == TRUE;
	status_window.visible =
	    xv_get(status_window.frame, XV_SHOW) == TRUE;

	defaults_set_boolean("ftptool.HostInfoVisible", host_window.visible);

	defaults_set_boolean("ftptool.HostInfoAdvancedVisible",
		host_window.advanced.visible);

	defaults_set_boolean("ftptool.LocalWindowVisible",
	    local_window.visible);

	defaults_set_boolean("ftptool.BatchWindowVisible",
	    schedule_window.visible);

	defaults_set_boolean("ftptool.StatusWindowVisible",
	    status_window.visible);

	save_geometry(base_window.geometry, base_window.frame);
	save_geometry(local_window.geometry, local_window.frame);
	save_geometry(host_window.geometry, host_window.frame);
	save_geometry(schedule_window.geometry, schedule_window.frame);
	save_geometry(session_window.geometry, session_window.frame);
	save_geometry(status_window.geometry, status_window.frame);

	defaults_set_string("ftptool.RemoteWindowGeometry",
	    base_window.geometry);

	defaults_set_string("ftptool.LocalWindowGeometry",
	    local_window.geometry);

	defaults_set_string("ftptool.HostWindowGeometry",
	    host_window.geometry);

	defaults_set_string("ftptool.BatchWindowGeometry",
	    schedule_window.geometry);

	defaults_set_string("ftptool.SessionWindowGeometry",
		session_window.geometry);

	defaults_set_string("ftptool.StatusWindowGeometry",
	    status_window.geometry);

	filename = find_dotfile(FTPTOOL_LAYOUT);
	if (filename == NULL) {
		if ((filename = create_dotfile(FTPTOOL_LAYOUT, 0644)) == NULL)
			return;
	}
	if ((fp = fopen(filename, "w")) == NULL) {
		footer_message("Could not save layout: %s", sys_errlist[errno]);
		return;
	}
	fprintf(fp, "ftptool.HostInfoVisible:\t%s\n",
		(host_window.visible == 0) ? false : true);
	fprintf(fp, "ftptool.HostInfoAdvancedVisible:\t%s\n",
		(host_window.advanced.visible == 0) ? false : true);
	fprintf(fp, "ftptool.LocalWindowVisible:\t%s\n",
		(local_window.visible == 0) ? false : true);
	fprintf(fp, "ftptool.BatchWindowVisible:\t%s\n",
		(schedule_window.visible == 0) ? false : true);
	fprintf(fp, "ftptool.StatusWindowVisible:\t%s\n",
		(status_window.visible == 0) ? false : true);

	fprintf(fp, "ftptool.RemoteWindowGeometry:\t%s\n",
	    base_window.geometry);
	fprintf(fp, "ftptool.LocalWindowGeometry:\t%s\n",
	    local_window.geometry);
	fprintf(fp, "ftptool.HostWindowGeometry:\t%s\n", host_window.geometry);
	fprintf(fp, "ftptool.BatchWindowGeometry:\t%s\n",
	    schedule_window.geometry);
	fprintf(fp, "ftptool.SessionWindowGeometry:\t%s\n",
	    session_window.geometry);
	fprintf(fp, "ftptool.StatusWindowGeometry:\t%s\n",
	    status_window.geometry);
	fclose(fp);
	footer_message("Layout saved to %s.", filename);
	free(filename);
}

#ifdef USE_PROTOTYPES
void local_doubleclick(struct dirlist *tmp)
#else
void local_doubleclick(tmp)
struct dirlist *tmp;
#endif
{
	xfer_buttons_inactive();
	if (S_ISLNK(tmp->mode)) {
		char	*name;

		name = linkname(tmp->name);
		if (name != NULL) {
			if (change_local_dir(name, 0) == ENOTDIR) {
				/* Try to view a file */
				(void) view_local_file(name,
				    DOLOCALVIEW, (int *)NULL);
			}
			free(name);
		}
	} else if (S_ISDIR(tmp->mode)) {
		change_local_dir(tmp->name, 0);
	} else if (S_ISREG(tmp->mode)) {
		(void) view_local_file(tmp->name, DOLOCALVIEW, (int *)NULL);
	}
	xfer_buttons_active();
}

#ifdef USE_PROTOTYPES
void remote_doubleclick(void)
#else
void remote_doubleclick()
#endif
{
	if (which_remote_file == NULL)
		return;
	xfer_buttons_inactive();
	abort_transfer = 0;
	if (ping_server())
		goto out;
	if (show_status)
		xv_set(status_window.frame,
			XV_SHOW, TRUE,
			NULL);
	init_status(which_remote_size);

	if (S_ISLNK(which_remote_mode) || non_unix) {
		char	*name;

		name = linkname(which_remote_file);
		if (name != NULL) {
			if (change_remote_dir(name, 0) == ENOTDIR) {
				/* Try to view a file */
				update_status_label("Receiving", name,
				    which_remote_size);
				(void) view_remote_file(name,
				    which_remote_size);
			}
			free(name);
		}
	} else if (S_ISDIR(which_remote_mode)) {
		change_remote_dir(which_remote_file, 0);
	} else if (S_ISREG(which_remote_mode)) {
		update_status_label("Receiving", which_remote_file,
		    which_remote_size);
		(void) view_remote_file(which_remote_file, which_remote_size);
	}
out:
	free(which_remote_file);
	which_remote_file = NULL;
	which_remote_mode = 0;
	which_remote_size = 0;
	if (show_status)
		xv_set(status_window.frame,
			XV_SHOW, FALSE,
			NULL);
	update_status_label("Not", "transferring", (size_t)0);
	end_status();
	xfer_buttons_active();
	if (timedout)
		timeout_disconnect();
}

#ifdef USE_PROTOTYPES
void plus_proc(void)
#else
void plus_proc()
#endif
{
	xv_set(host_window.frame,
		XV_WIDTH, (int)xv_get(host_window.basic.panel, XV_WIDTH)
			+ (int)xv_get(host_window.advanced.panel, XV_WIDTH),
		NULL);
	xv_set(host_window.panel,
		XV_WIDTH, (int)xv_get(host_window.frame, XV_WIDTH),
		NULL);
	xv_set(host_window.advanced.panel,
		XV_HEIGHT, xv_get(host_window.basic.panel, XV_HEIGHT),
		WIN_BORDER, TRUE,
		XV_SHOW, TRUE,
		NULL);
	window_fit(host_window.panel);
	window_fit(host_window.frame);
	xv_set(host_window.basic.plus,
		PANEL_INACTIVE, TRUE,
		NULL);
	/* raise window */
	xv_set(host_window.frame,
		XV_SHOW, TRUE,
		NULL);
	xv_set(host_window.basic.plus,
		PANEL_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
void minus_proc(void)
#else
void minus_proc()
#endif
{
	xv_set(host_window.basic.plus,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(host_window.advanced.panel,
		XV_SHOW, FALSE,
		NULL);
	window_fit(host_window.panel);
	xv_set(host_window.panel,
		WIN_BORDER, FALSE,
		NULL);
	window_fit(host_window.frame);
	xv_set(host_window.advanced.minus,
		PANEL_NOTIFY_STATUS, XV_ERROR,
		NULL);
}

#ifdef USE_PROTOTYPES
void remote_os_proc(void)
#else
void remote_os_proc()
#endif
{
	int	value;

	value = xv_get(host_window.advanced.os_type, PANEL_VALUE);

	if (value == REMOTE_OS_OTHER) {
		xv_set(host_window.advanced.dir_parse,
			XV_SHOW, TRUE,
			NULL);
	} else {
		xv_set(host_window.advanced.dir_parse,
			XV_SHOW, FALSE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void	repeat_check_box(Panel_item item, int value, Event *event)
#else
void	repeat_check_box(item, value, event)
Panel_item	item;
int		value;
Event	*event;
#endif
{
	int minutes;

	minutes = xv_get(schedule_window.repeat_minutes, PANEL_VALUE);

	if (value == 1) {
		xv_set(schedule_window.repeat_minutes,
			XV_SHOW, TRUE,
			NULL);
		xv_set(schedule_window.repeat,
			PANEL_CHOICE_STRING, 0, "in ",
			XV_SHOW, TRUE,
			NULL);
		if (minutes > 1) {
			xv_set(schedule_window.repeat_message,
				PANEL_LABEL_STRING, "minutes",
				PANEL_LABEL_BOLD, FALSE,
				XV_SHOW, TRUE,
				NULL);
		} else {
			xv_set(schedule_window.repeat_message,
				PANEL_LABEL_STRING, "minute",
				PANEL_LABEL_BOLD, FALSE,
				XV_SHOW, TRUE,
				NULL);
		}
	} else {
		xv_set(schedule_window.repeat_minutes,
			XV_SHOW, FALSE,
			NULL);
		xv_set(schedule_window.repeat_message,
			XV_SHOW, FALSE,
			NULL);
		xv_set(schedule_window.repeat,
			PANEL_CHOICE_STRING, 0, "",
			XV_SHOW, TRUE,
			NULL);
	}
}


#ifdef USE_PROTOTYPES
Panel_setting	repeat_minute_check(Panel_item item, Event *event)
#else
Panel_setting	repeat_minute_check(item, event)
Panel_item	item;
Event	*event;
#endif
{
	int	value;

	value = xv_get(schedule_window.repeat_minutes, PANEL_VALUE);
	if (value > 1) {
		xv_set(schedule_window.repeat_message,
			PANEL_LABEL_STRING, "minutes",
			PANEL_LABEL_BOLD, FALSE,
			NULL);
	} else {
		xv_set(schedule_window.repeat_message,
			PANEL_LABEL_STRING, "minute",
			PANEL_LABEL_BOLD, FALSE,
			NULL);
	}

	return (panel_text_notify(item, event));
}
