#include "olutils.h"

int ol_list::insert(char* string,int n,  void* data,Xv_opaque glyph ) {
  int duplicates;
  int t;
  duplicates = (int) xv_get(the_object,PANEL_LIST_INSERT_DUPLICATE);
  if(duplicates == FALSE)  {
    for(t=0;t<num_rows();t++) {
      if(!strcmp((char*)(xv_get(the_object,PANEL_LIST_STRING,t)),string)) {
	printf("rejected duplicate %s\n",string);
	return 1;
      }
    }
  }
  xv_set(the_object,
	 PANEL_LIST_INSERT,n,
	 PANEL_LIST_STRING,n,string,
	 PANEL_LIST_CLIENT_DATA,n,data,
	 PANEL_LIST_GLYPH,n,glyph,NULL);
  return 0;

}
char* ol_list::get_item(int n, void** data, Xv_opaque* glyph) 
{
  char* tmp;
  tmp =(char*)xv_get(the_object,PANEL_LIST_STRING,n);
  
  if(data) {
    *data = (void*)xv_get(the_object,PANEL_LIST_CLIENT_DATA,n);
  }
  if(glyph) {
    *glyph = (Xv_opaque)xv_get(the_object,PANEL_LIST_GLYPH,n);
    }
  return tmp;
}
