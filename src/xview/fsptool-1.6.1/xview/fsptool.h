/********************************************************************************/
/* fsptool.h --									*/
/********************************************************************************/

#ifndef _FSPtool_H_
#define _FSPtool_H_ 1

#include "../lib/common.h"
#include "../lib/cache.h"

#include <X11/Xos.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/svrimage.h>
#include <xview/icon.h>
#include <xview/seln.h>
#include <xview/notify.h>
#include <xview/notice.h>
#include <xview/font.h>
#include <xview/sel_attrs.h>
#include <xview/textsw.h>
#include <xview/defaults.h>
#include <xview/dragdrop.h>

/********************************************************************************/

typedef enum { Warning, Error, Severe } ErrorLevel;

/********************************************************************************/

extern void	done_proc(const char*);
extern void	quit_proc();
extern void	fget_files(CacheData*);
extern void	fput_files(CacheData*);
extern void	select_files(Panel_item,char*,caddr_t,Panel_list_op,Event*);
extern void	select_local_files(Panel_item,char*,caddr_t,Panel_list_op,Event*);
extern void	update_local_dir_list();
extern void	update_dir_list();
extern void	abort_proc(Panel_item,int,Event*);
extern void	error_report(int,...);

extern int	get_next_selected(Panel_item);

extern Panel_setting	remote_dir_proc();
extern Panel_setting	local_dir_proc();

extern Notify_value	read_get_stream(Notify_client,int);
extern Notify_value	read_put_stream(Notify_client,int);
extern Notify_value	read_single_stream(Notify_client,int);

/********************************************************************************/
#endif
