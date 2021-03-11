# Makefile for ctwm
.IFDEF __ALPHA__
CFLAGS = $(CFLAGS)/STAND=VAXC
LIBS = ctwm.axp_opt/opt
.ELSE
LIBS = ctwm.vax_opt/opt
.ENDIF

CFLAGS = $(CFLAGS)/define=(VMS,XPM)


OBJS = gram.obj,lex.obj,deftwmrc.obj,add_window.obj,gc.obj,list.obj, \
       ctwm.obj,parse.obj,menus.obj,events.obj,resize.obj,util.obj, \
       version.obj,iconmgr.obj,cursor.obj,icons.obj,workmgr.obj, \
       vms_cmd_services.obj,lnm.obj

ctwm.exe : $(OBJS)
	$(LINK)$(LINKFLAGS) $(LIBS)

