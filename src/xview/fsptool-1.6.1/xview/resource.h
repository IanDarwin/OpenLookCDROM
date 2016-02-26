/********************************************************************************/
/* xview/resource.h --								*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#ifndef _FSPtool_RESOURCE_H_
#define _FSPtool_RESOURCE_H_ 1

#define MAX_HOSTS 512

/********************************************************************************/

typedef struct
    {
    char *hostname;		/* -- host name up to 63 chars */
    int   hostport;
    char *alias;		/* -- site name alias - no whitespace < 32 chs */
    char *directory;		/* -- site directory to go to < 64 chs */
    char *description;		/* -- text description < 128 chs */
    }
  HostInfo;

/********************************************************************************/

typedef struct
    {
    int	openlook, cancelclose, menuclose, hostread;
    }
  Tool_Properties;

/********************************************************************************/

extern void get_layout();
extern void set_layout();
extern void save_resources();
extern void load_resources();
extern void set_defaults();

extern void set_string_resource(const char*,const char*);
extern void set_bool_resource(const char*,int);
extern void set_int_resource(const char*,int);

extern char *string_resource(const char*,char*);
extern int   int_resource(const char*,int);
extern int   bool_resource(const char*,int);

extern void return_hosts_list();

extern int  save_hosts();
extern int  load_hosts();

#endif
/********************************************************************************/

