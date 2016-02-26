#ifndef OLUTILS_H
#define OLUTILS_H
#include <string.h>
#include <xview/xview.h>
#include <xview/panel.h>

typedef int (*ol_notify_list_proc)(Panel_item,char*,
			       Xv_opaque,Panel_list_op,Event*,int);

class ol_object {
 protected: 
  Xv_opaque the_object;
 public:
  operator Xv_opaque() {
    return the_object;
  }
  void show(int flag=1) {
    xv_set(the_object, XV_SHOW,flag,NULL);
  }

};
class ol_panel : public ol_object {
 public:
  
  void set_label(char* string) {xv_set(the_object,PANEL_LABEL_STRING,string, NULL);}
  char* get_label() {return (char*)xv_get(the_object,PANEL_LABEL_STRING);}  
};

class ol_frame :public ol_object {    
 public:
  ol_frame(Xv_opaque o) {the_object = o;}
  ol_frame() {the_object = 0;}
  void set_left_footer(char* string) {
    xv_set(the_object,FRAME_LEFT_FOOTER,string,NULL);
  }
  void set_right_footer(char* string) {
    xv_set(the_object,FRAME_RIGHT_FOOTER,string,NULL);
  }
  void show_footer(int flag=TRUE) {
    xv_set(the_object,FRAME_SHOW_FOOTER,flag,NULL);
  }
  void show_header(int flag=TRUE) {
    xv_set(the_object,FRAME_SHOW_HEADER,flag,NULL);
  }
  void set_header(char* string) {
    xv_set(the_object,FRAME_LABEL,string,NULL);
  }
  void set_busy(int flag=TRUE) {
    xv_set(the_object,FRAME_BUSY,flag,NULL);
  }
  void set_icon(Server_image im) {
    Icon i = xv_create(the_object,ICON,
		       ICON_IMAGE,im,
		       NULL);
    xv_set(the_object,FRAME_ICON,i,NULL);
  }
};

typedef Menu_item (*ol_menu_callback)(Menu men, Menu_item ot);

class ol_menu : public ol_object {
  int allocated;
 public:
  ol_menu() { allocated = 0; the_object = 0;}
//  ~ol_menu() {if (allocated) {xv_destroy_safe(the_object);}}
  ol_menu(char* string) {
    create(string);
  }
  void add_item(char* title, ol_menu_callback action=0) {
    Menu_item mi;
    mi = xv_create(NULL,MENUITEM,
		   MENU_STRING,title,
		   NULL);
    if(action) {
      xv_set(mi,MENU_NOTIFY_PROC,action,NULL);
    }
    xv_set(the_object,MENU_APPEND_ITEM,mi,NULL);
  }
  
  
  void create(char* title) {
    the_object = xv_create(NULL,MENU,
			   MENU_TITLE_ITEM,title,
			   NULL);
    allocated = 1;
  }
  void make_pinnable(int flag=TRUE) {
    xv_set(the_object,MENU_PIN,flag,NULL);
  }
};

class ol_list : public ol_panel {
  int sel;
 public:
  ol_list(Xv_opaque l) { the_object = l;}
  ol_list() {the_object = 0 ; sel = -1;}
  int num_rows() {return (int)xv_get(the_object,PANEL_LIST_NROWS); }

  int insert(char* string,int n,  void* data=0,Xv_opaque glyph = 0);
  int append(char* string, void*data=0,Xv_opaque glyph =0) {
    return insert(string,num_rows(),data,glyph);
  }
  void delete_all() {
    int n=0;
    n = num_rows();
    xv_set(the_object, PANEL_LIST_DELETE_ROWS, 0,n, NULL);
  }
  void del(int n) {xv_set(the_object,PANEL_LIST_DELETE,n, NULL); }
  void select(int n, int f=FALSE) {
    xv_set(the_object, PANEL_LIST_SELECT,n,f, NULL); }
  int is_selected(int n) {
    return (int)xv_get(the_object,PANEL_LIST_SELECTED,n);
  }
  int get_first() {
    return sel =  (int) xv_get(the_object, PANEL_LIST_FIRST_SELECTED);  }

  int get_next() {
    return sel = (int) xv_get(the_object,PANEL_LIST_NEXT_SELECTED,sel);  }
  
  void deselect_all() {
    int n = 0;
    for(n = get_first() ; n != -1 ; n = get_next()) { select(n,FALSE); } }
  char* get_item(int n, void** data=0, Xv_opaque* glyph=0) ;      
  void set_unique(int flag) {
    xv_set(the_object, PANEL_LIST_INSERT_DUPLICATE,flag, NULL);
  }
  void delete_selected() {
    xv_set(the_object,PANEL_LIST_DELETE_SELECTED_ROWS,NULL); }
  void set_sorted(int flag=PANEL_FORWARD) {
    xv_set(the_object,PANEL_LIST_SORT,flag,NULL);}
  void set_notify_proc(ol_notify_list_proc the_proc) {
    xv_set(the_object,
	   PANEL_NOTIFY_PROC,the_proc,
	   NULL);
  }
  void set_menu(Xv_opaque menu) {
    xv_set(the_object,PANEL_ITEM_MENU,menu,NULL);
  }
};

class ol_button : public ol_panel {
 public:
  ol_button() {    the_object =0;  }
  ol_button(Xv_opaque b) { the_object = b;}


};
class ol_text : public ol_panel {
 public:
  ol_text() {the_object = 0;}
  ol_text(Xv_opaque t) {the_object = t;}
  char* get_text() {
    return (char*)xv_get(the_object,PANEL_VALUE);
  }
  void set_text(char* message) {
    xv_set(the_object,PANEL_VALUE,message,0);
  }
};

  
#endif 
