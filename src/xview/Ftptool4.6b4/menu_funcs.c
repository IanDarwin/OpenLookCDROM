#include "ftptool.h"

#pragma ident   "@(#)menu_funcs.c 1.3     93/05/27"

#ifdef USE_PROTOTYPES
Menu file_menu_gen(Menu m, Menu_generate op)
#else
Menu file_menu_gen(m, op)
Menu m;
Menu_generate op;
#endif
{
	static Menu_item copy_item;
	static Menu_item delete_item;
	static Menu_item dir_item;
	static Menu_item compress_file_item;
	static Menu_item uncompress_file_item;
	static Menu_item create_tar_file_item;
	static Menu_item extract_tar_file_item;
	int	ns, nr;

	switch (op) {
	case MENU_DISPLAY:
		/* menu looks like */
		/*
		 * Copy ->
		 * Delete ->
		 * Directory ->
		 * Compress File
		 * Uncompress File
		 * Create Tar File
		 * Extract Tar File
		 */
		if (copy_item == 0) {
			copy_item = xv_get(m, MENU_NTH_ITEM, 1);
			delete_item = xv_get(m, MENU_NTH_ITEM, 2);
			dir_item = xv_get(m, MENU_NTH_ITEM, 3);
			compress_file_item = xv_get(m, MENU_NTH_ITEM, 4);
			uncompress_file_item = xv_get(m, MENU_NTH_ITEM, 5);
			create_tar_file_item = xv_get(m, MENU_NTH_ITEM, 6);
			extract_tar_file_item = xv_get(m, MENU_NTH_ITEM, 7);
		}

		xv_set(copy_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(delete_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(dir_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(uncompress_file_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(extract_tar_file_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(compress_file_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(create_tar_file_item,
			MENU_INACTIVE, FALSE,
			NULL);

		if ((local_list_nfiles + local_list_ndirs) == 0) {
			xv_set(create_tar_file_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		if (!(local_list_nfiles || local_list_ndirs ||
		    local_list_nothers || remote_list_nfiles ||
		    remote_list_ndirs || remote_list_nothers)) {
			xv_set(delete_item,
				MENU_INACTIVE, TRUE,
				NULL);
			ns = (int)xv_get(schedule_window.send_list,
			    PANEL_LIST_NROWS);
			nr = (int)xv_get(schedule_window.receive_list,
			    PANEL_LIST_NROWS);
			if ((ns + nr) == 0)
				xv_set(copy_item,
					MENU_INACTIVE, TRUE,
					NULL);
		}

		if (!connected) {
			xv_set(copy_item,
				MENU_INACTIVE, TRUE,
				NULL);
			xv_set(dir_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		if (local_list_nfiles == 0) {
			xv_set(uncompress_file_item,
				MENU_INACTIVE, TRUE,
				NULL);
			xv_set(extract_tar_file_item,
				MENU_INACTIVE, TRUE,
				NULL);
			xv_set(compress_file_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		break;
	case MENU_DISPLAY_DONE:
		break;
	case MENU_NOTIFY_DONE:
		break;
	case MENU_NOTIFY:
		break;
	}
	return (m);
}

#ifdef USE_PROTOTYPES
Menu file_copy_menu_gen(Menu m, Menu_generate op)
#else
Menu file_copy_menu_gen(m, op)
Menu m;
Menu_generate op;
#endif
{
	static Menu_item remote_to_local_item;
	static Menu_item local_to_remote_item;
	static Menu_item batch_remote_to_local_item;
	static Menu_item batch_local_to_remote_item;
	int	ns, nr;

	switch (op) {
	case MENU_DISPLAY:
		/* pullright menu looks like */
		/*
		 * Remote to Local
		 * Local to Remote
		 * Remote to Local (Batch)
		 * Local to Remote (Batch)
		 */
		if (remote_to_local_item == 0) {
			remote_to_local_item = xv_get(m, MENU_NTH_ITEM, 1);
			local_to_remote_item = xv_get(m, MENU_NTH_ITEM, 2);
			batch_remote_to_local_item = xv_get(m, MENU_NTH_ITEM,
			    3);
			batch_local_to_remote_item = xv_get(m, MENU_NTH_ITEM,
			    4);
		}

		xv_set(remote_to_local_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(local_to_remote_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(batch_remote_to_local_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(batch_local_to_remote_item,
			MENU_INACTIVE, FALSE,
			NULL);

		if ((local_list_nfiles + local_list_ndirs) == 0 || !connected)
			xv_set(local_to_remote_item,
				MENU_INACTIVE, TRUE,
				NULL);

		if ((remote_list_nfiles + remote_list_ndirs) == 0) {
			xv_set(remote_to_local_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		nr = (int)xv_get(schedule_window.receive_list,
		    PANEL_LIST_NROWS);
		if (nr == 0) {
			xv_set(batch_remote_to_local_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		ns = (int)xv_get(schedule_window.send_list, PANEL_LIST_NROWS);
		if (!connected || ns == 0) {
			xv_set(batch_local_to_remote_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}
		break;
	case MENU_DISPLAY_DONE:
		break;
	case MENU_NOTIFY_DONE:
		break;
	case MENU_NOTIFY:
		break;
	}
	return (m);
}

#ifdef USE_PROTOTYPES
Menu file_delete_menu_gen(Menu m, Menu_generate op)
#else
Menu file_delete_menu_gen(m, op)
Menu m;
Menu_generate op;
#endif
{
	static Menu_item delete_remote_file_item;
	static Menu_item delete_local_file_item;

	switch (op) {
	case MENU_DISPLAY:
		/* menu looks like */
		/*
		 * Remote File
		 * Local File
		 */
		if (delete_remote_file_item == 0) {
			delete_remote_file_item = xv_get(m, MENU_NTH_ITEM, 1);
			delete_local_file_item = xv_get(m, MENU_NTH_ITEM, 2);
		}

		xv_set(delete_remote_file_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(delete_local_file_item,
			MENU_INACTIVE, FALSE,
			NULL);

		if ((local_list_nfiles + local_list_ndirs) == 0) {
			xv_set(delete_local_file_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		if ((remote_list_nfiles + remote_list_ndirs) == 0) {
			xv_set(delete_remote_file_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		break;
	case MENU_DISPLAY_DONE:
		break;
	case MENU_NOTIFY_DONE:
		break;
	case MENU_NOTIFY:
		break;
	}
	return (m);
}


#ifdef USE_PROTOTYPES
Menu view_menu_gen(Menu m, Menu_generate op)
#else
Menu view_menu_gen(m, op)
Menu m;
Menu_generate op;
#endif
{
	static Menu_item remote_file_item;
	static Menu_item local_file_item;
	static Menu_item local_dir_item;
	static Menu_item session_log_item;
	static Menu_item host_info_item;

	switch (op) {
	case MENU_DISPLAY:
		/* menu looks like */
		/*
		 * Remote File
		 * Local File
		 * Local Directory
		 * Session Log
		 * Current Host Information
		 * Batch Schedule
		 * About Ftptool...
		 */
		if (remote_file_item == 0) {
			remote_file_item = xv_get(m, MENU_NTH_ITEM, 1);
			local_file_item = xv_get(m, MENU_NTH_ITEM, 2);
			local_dir_item = xv_get(m, MENU_NTH_ITEM, 3);
			session_log_item = xv_get(m, MENU_NTH_ITEM, 4);
			host_info_item = xv_get(m, MENU_NTH_ITEM, 5);
		}

		xv_set(remote_file_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(local_file_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(local_dir_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(session_log_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(host_info_item,
			MENU_INACTIVE, FALSE,
			NULL);

		if (remote_list_nfiles == 0)
			xv_set(remote_file_item,
				MENU_INACTIVE, TRUE,
				NULL);

		if (local_list_nfiles == 0)
			xv_set(local_file_item,
				MENU_INACTIVE, TRUE,
				NULL);

		if (!logging)
			xv_set(session_log_item,
				MENU_INACTIVE, TRUE,
				NULL);

		break;
	case MENU_DISPLAY_DONE:
		break;
	case MENU_NOTIFY_DONE:
		break;
	case MENU_NOTIFY:
		break;
	}
	return (m);
}

#ifdef USE_PROTOTYPES
void change_local_list_menu(void)
#else
void change_local_list_menu()
#endif
{
	Menu	m;
	static Menu_item cd_selection_item;
	static Menu_item local_to_remote_item;
	static Menu_item add_batch_send_item;
	static Menu_item local_file_item;
	static Menu_item delete_local_file_item;
	static Menu_item local_file_props_item;


	/* menu looks like */
	/*
	 * Local Files (Title)
	 * -----------------
	 * Locate Next Choice
	 * Clear All Choices
	 *
	 * CD to Selection
	 * Up One Level
	 * Copy to Remote
	 * Add to Batch Send List
	 * View File
	 * Delete File
	 * File Properties
	 */
	if (cd_selection_item == 0) {
		m = xv_get(local_window.list, PANEL_ITEM_MENU);
		cd_selection_item = xv_get(m, MENU_NTH_ITEM, 5);
		local_to_remote_item = xv_get(m, MENU_NTH_ITEM, 7);
		add_batch_send_item = xv_get(m, MENU_NTH_ITEM, 8);
		local_file_item = xv_get(m, MENU_NTH_ITEM, 9);
		delete_local_file_item = xv_get(m, MENU_NTH_ITEM, 10);
		local_file_props_item = xv_get(m, MENU_NTH_ITEM, 11);
	}
	xv_set(local_to_remote_item,
		MENU_INACTIVE, FALSE,
		NULL);
	xv_set(add_batch_send_item,
		MENU_INACTIVE, FALSE,
		NULL);
	xv_set(local_file_item,
		MENU_INACTIVE, FALSE,
		NULL);
	xv_set(delete_local_file_item,
		MENU_INACTIVE, FALSE,
		NULL);
	xv_set(local_file_props_item,
		MENU_INACTIVE, FALSE,
		NULL);

	if (local_list_ndirs == 1 && local_list_nfiles == 0) {
		xv_set(cd_selection_item,
			MENU_INACTIVE, FALSE,
			NULL);
	} else {
		xv_set(cd_selection_item,
			MENU_INACTIVE, TRUE,
			NULL);
	}

	if ((local_list_nfiles + local_list_ndirs) == 0 || !connected)
		xv_set(local_to_remote_item,
			MENU_INACTIVE, TRUE,
			NULL);

	if ((local_list_nfiles + local_list_ndirs) == 0)
		xv_set(add_batch_send_item,
			MENU_INACTIVE, TRUE,
			NULL);

	if (local_list_nfiles == 0)
		xv_set(local_file_item,
			MENU_INACTIVE, TRUE,
			NULL);
	if ((local_list_nfiles+local_list_ndirs+local_list_nothers) == 0) {
		xv_set(local_file_props_item,
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(delete_local_file_item,
			MENU_INACTIVE, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void change_remote_list_menu(void)
#else
void change_remote_list_menu()
#endif
{
	Menu	m;
	static Menu_item cd_selection_item;
	static Menu_item up_one_level_item;
	static Menu_item remote_to_local_item;
	static Menu_item add_batch_receive_item;
	static Menu_item remote_file_item;
	static Menu_item delete_remote_file_item;
	static Menu_item remote_file_props_item;

	/* menu looks like */
	/*
	 * Remote File (title)
	 * -----------------
	 * Locate Next Choice
	 * Clear All Choices
	 *
	 * CD to Selection
	 * Up One Level
	 * Copy to Local
	 * Add to Batch Receive List
	 * View File
	 * Delete
	 * File Properties
	 */
	if (cd_selection_item == 0) {
		m = xv_get(base_window.list, PANEL_ITEM_MENU),
		cd_selection_item = xv_get(m, MENU_NTH_ITEM, 5);
		up_one_level_item = xv_get(m, MENU_NTH_ITEM, 6);
		remote_to_local_item = xv_get(m, MENU_NTH_ITEM, 7);
		add_batch_receive_item = xv_get(m, MENU_NTH_ITEM, 8);
		remote_file_item = xv_get(m, MENU_NTH_ITEM, 9);
		delete_remote_file_item = xv_get(m, MENU_NTH_ITEM, 10);
		remote_file_props_item = xv_get(m, MENU_NTH_ITEM, 11);
	}
	if (!connected) {
		xv_set(cd_selection_item,
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(remote_to_local_item,
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(up_one_level_item,
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(add_batch_receive_item,
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(remote_file_item,
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(delete_remote_file_item,
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(remote_file_props_item,
			MENU_INACTIVE, TRUE,
			NULL);
		return;
	}

	xv_set(up_one_level_item,
		MENU_INACTIVE, FALSE,
		NULL);
	xv_set(remote_to_local_item,
		MENU_INACTIVE, FALSE,
		NULL);
	xv_set(add_batch_receive_item,
		MENU_INACTIVE, FALSE,
		NULL);
	xv_set(remote_file_item,
		MENU_INACTIVE, FALSE,
		NULL);
	xv_set(delete_remote_file_item,
		MENU_INACTIVE, FALSE,
		NULL);
	xv_set(remote_file_props_item,
		MENU_INACTIVE, FALSE,
		NULL);

	if (remote_list_ndirs == 1 && remote_list_nfiles == 0) {
		xv_set(cd_selection_item,
			MENU_INACTIVE, FALSE,
			NULL);
	} else {
		if (non_unix && remote_list_nfiles == 1) {
			xv_set(cd_selection_item,
				MENU_INACTIVE, FALSE,
				NULL);
		} else {
			xv_set(cd_selection_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}
	}

	if ((remote_list_nfiles + remote_list_ndirs) == 0) {
		xv_set(remote_to_local_item,
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(add_batch_receive_item,
			MENU_INACTIVE, TRUE,
			NULL);
	}

	if (remote_list_nfiles == 0)
		xv_set(remote_file_item,
			MENU_INACTIVE, TRUE,
			NULL);

	if ((remote_list_nfiles+remote_list_ndirs+remote_list_nothers) == 0) {
		xv_set(remote_file_props_item,
			MENU_INACTIVE, TRUE,
			NULL);
		xv_set(delete_remote_file_item,
			MENU_INACTIVE, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
Menu props_menu_gen(Menu m, Menu_generate op)
#else
Menu props_menu_gen(m, op)
Menu m;
Menu_generate op;
#endif
{
	static Menu_item local_file_item;
	static Menu_item remote_file_item;

	switch (op) {
	case MENU_DISPLAY:
		/* menu looks like */
		/*
		 * Tool
		 * Local File
		 * Remote File
		 */
		if (local_file_item == 0) {
			local_file_item = xv_get(m, MENU_NTH_ITEM, 2);
			remote_file_item = xv_get(m, MENU_NTH_ITEM, 3);
		}
		xv_set(local_file_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(remote_file_item,
			MENU_INACTIVE, FALSE,
			NULL);

		if ((local_list_nfiles + local_list_ndirs +
		    local_list_nothers) == 0)
			xv_set(local_file_item,
				MENU_INACTIVE, TRUE,
				NULL);

		if ((remote_list_nfiles + remote_list_ndirs +
		    remote_list_nothers) == 0)
			xv_set(remote_file_item,
				MENU_INACTIVE, TRUE,
				NULL);
		break;
	case MENU_DISPLAY_DONE:
		break;
	case MENU_NOTIFY_DONE:
		break;
	case MENU_NOTIFY:
		break;
	}
	return (m);
}

#ifdef USE_PROTOTYPES
Menu send_list_menu_gen(Menu m, Menu_generate op)
#else
Menu send_list_menu_gen(m, op)
Menu m;
Menu_generate op;
#endif
{
	static Menu_item delete_item;
	static Menu_item copy_remote_item;
	static Menu_item load_item;
	static Menu_item save_item;
	int	ns;

	switch (op) {
	case MENU_DISPLAY:
		/* menu looks like */
		/*
		 * Title
		 * Locate Choice
		 * Clear all choices
		 *
		 * Delete
		 * Copy to Remote
		 * Load
		 * Save
		 */
		if (delete_item == 0) {
			delete_item = xv_get(m, MENU_NTH_ITEM, 5);
			copy_remote_item = xv_get(m, MENU_NTH_ITEM, 6);
			load_item = xv_get(m, MENU_NTH_ITEM, 7);
			save_item = xv_get(m, MENU_NTH_ITEM, 8);
		}
		xv_set(delete_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(copy_remote_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(load_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(save_item,
			MENU_INACTIVE, FALSE,
			NULL);

		if (nsenditems == 0) {
			xv_set(delete_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		ns = (int)xv_get(schedule_window.receive_list,
		    PANEL_LIST_NROWS);
		if (ns == 0) {
			xv_set(save_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		if (!connected || ns == 0)
			xv_set(copy_remote_item,
				MENU_INACTIVE, TRUE,
				NULL);

		break;
	case MENU_DISPLAY_DONE:
		break;
	case MENU_NOTIFY_DONE:
		break;
	case MENU_NOTIFY:
		break;
	}
	return (m);
}

#ifdef USE_PROTOTYPES
Menu receive_list_menu_gen(Menu m, Menu_generate op)
#else
Menu receive_list_menu_gen(m, op)
Menu m;
Menu_generate op;
#endif
{
	static Menu_item delete_item;
	static Menu_item copy_local_item;
	static Menu_item load_item;
	static Menu_item save_item;
	int	nr;

	switch (op) {
	case MENU_DISPLAY:
		/* menu looks like */
		/*
		 * Title
		 * Locate Choice
		 * Clear all choices
		 *
		 * Delete
		 * Copy to Local
		 * Load
		 * Save
		 */
		if (delete_item == 0) {
			delete_item = xv_get(m, MENU_NTH_ITEM, 5);
			copy_local_item = xv_get(m, MENU_NTH_ITEM, 6);
			load_item = xv_get(m, MENU_NTH_ITEM, 7);
			save_item = xv_get(m, MENU_NTH_ITEM, 8);
		}
		xv_set(delete_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(copy_local_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(load_item,
			MENU_INACTIVE, FALSE,
			NULL);
		xv_set(save_item,
			MENU_INACTIVE, FALSE,
			NULL);

		if (nreceiveitems == 0) {
			xv_set(delete_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		nr = (int)xv_get(schedule_window.receive_list,
		    PANEL_LIST_NROWS);
		if (nr == 0) {
			xv_set(save_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		if (nr == 0 || !connected) {
			xv_set(copy_local_item,
				MENU_INACTIVE, TRUE,
				NULL);
		}

		break;
	case MENU_DISPLAY_DONE:
		break;
	case MENU_NOTIFY_DONE:
		break;
	case MENU_NOTIFY:
		break;
	}
	return (m);
}

#ifdef USE_PROTOTYPES
Menu host_menu_gen(Menu m, Menu_generate op)
#else
Menu host_menu_gen(m, op)
Menu m;
Menu_generate op;
#endif
{
	static Menu_item save_item;
	static Menu_item append_netrc_item;

	switch (op) {
	case MENU_DISPLAY:
		/* menu looks like */
		/*
		 * Save
		 * Load
		 * Append .netrc
		 * Add
		 * Change
		 * Delete
		 */
		if (save_item == 0) {
			save_item = xv_get(m, MENU_NTH_ITEM, 1);
			append_netrc_item = xv_get(m, MENU_NTH_ITEM, 3);
		}

		if (netrc_filename == NULL)
			xv_set(append_netrc_item,
				MENU_INACTIVE, TRUE,
				NULL);

		if (timestamped || list_changed) {
			xv_set(save_item,
				MENU_STRING, "Save (needed)",
				NULL);
		} else {
			xv_set(save_item,
				MENU_STRING, "Save",
				NULL);
		}

		break;
	case MENU_DISPLAY_DONE:
		break;
	case MENU_NOTIFY_DONE:
		break;
	case MENU_NOTIFY:
		break;
	}
	return (m);
}
