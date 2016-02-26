
#       @(#) Makefile 1.35 92/10/27 

SSPKGHOME	= ..
LIBDIR		= $(SSPKGHOME)/lib
STATICLIBRARY	= $(LIBDIR)/libsspkg.a
SHAREDLIB	= $(LIBDIR)/libsspkg.so.1.0	# ignored
SHAREDLIBDATA	= $(LIBDIR)/libsspkg.sa.1.0	# ignored
SHAREDLIBLN	= $(LIBDIR)/libsspkg.so
INSTALL_DIR	= ..

.PRECIOUS:	$(STATICLIBRARY) $(LIBDIR)/libsspkg.so.1 $(LIBDIR)/libsspkg.so.1.0 $(LIBDIR)/libsspkg.sa.1.0

.KEEP_STATE:

INCDIR		= $(SSPKGHOME)/include/sspkg
PRIVATEINCDIR	= $(SSPKGHOME)/src
CFLAGS 		= -O
#CFLAGS		= -g
CPPFLAGS	= -I$(SSPKGHOME)/include -I$(PRIVATEINCDIR) -I$(OPENWINHOME)/include


INCLUDES =	$(INCDIR)/array.h	\
		$(INCDIR)/box.h		\
		$(INCDIR)/canshell.h	\
		$(INCDIR)/drawobj.h 	\
		$(INCDIR)/list.h	\
		$(INCDIR)/rectobj.h	\
		$(INCDIR)/tree.h	\
		$(INCDIR)/patchlevel.h


PRIVATE_INCLUDES =	\
		$(PRIVATEINCDIR)/box_impl.h	\
		$(PRIVATEINCDIR)/canshell_impl.h\
		$(PRIVATEINCDIR)/dimage_impl.h	\
		$(PRIVATEINCDIR)/dtext_impl.h	\
		$(PRIVATEINCDIR)/drawobj_impl.h	\
		$(PRIVATEINCDIR)/rectobj_impl.h	\
		$(PRIVATEINCDIR)/shared_info.h	\
		$(PRIVATEINCDIR)/tree_impl.h 	\
		$(PRIVATEINCDIR)/grip_image.xbm


LIBSOURCES =    canshell.c      \
		cbox.c          \
		box.c           \
		rectobj.c       \
		bag.c           \
		drawrect.c      \
		drawline.c      \
		drawimage.c     \
		drawtext.c      \
		drawicon.c      \
		drawarea.c      \
		display_list.c  \
		tacho.c         \
		clock.c         \
		grip.c          \
		tree.c          \
		array.c         \
		event.c         \
		bkg_event.c     \
		dnd.c           \
		selection.c     \
		list.c
 
LIBDATA =	rectobj_data.c  \
		array_data.c 	\
		drawobj_data.c	\
		tree_data.c	\
		box_data.c

CSOURCE_ALL = $(LIBSOURCES) $(LIBDATA)

STATICLIBDIR=Static
SHAREDLIBDIR=Shared

include ../Make.config

all: Static Shared $(LIBDIR) $(CONFIGTARGETS) $(STATICLIBRARY) $(SHAREDLIBLN)

$(STATICLIBRARY):	$(CSOURCE_ALL:%.c=Static/%.o)
	ar rv $@ $?
	-ranlib $@

$(STATICLIBDIR)/%.o: %.c $(PRIVATE_INCLUDES) $(INCLUDES)
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

$(SHAREDLIB):	$(CSOURCE_ALL:%.c=Shared/%.o)
	$(CONFIGSHAREDLIBLD)

$(SHAREDLIBDATA): $(LIBDATA:%.c=Shared/%.o)
	ar ruv $(SHAREDLIBDATA) $?
	-ranlib $(SHAREDLIBDATA)

$(SHAREDLIBDIR)/%.o: %.c $(PRIVATE_INCLUDES) $(INCLUDES)
	$(CC) $(CFLAGS) $(CONFIGPIC) $(CPPFLAGS) -c $< -o $@

$(SHAREDLIBLN):
	rm -f $(SHAREDLIBLN)
	ln -s $(SHAREDLIB) $(SHAREDLIBLN)

Shared:
	-mkdir Shared

Static:
	-mkdir Static

$(LIBDIR):
	-mkdir $(LIBDIR)


install: $(STATICLIBRARY) $(SHAREDLIB) FORCE
	cp $(STATICLIBRARY) $(INSTALL_DIR)/lib
	cp $(SHAREDLIB) $(INSTALL_DIR)/lib
	ln -s $(SHAREDLIB) $(INSTALLDIR)/libsspkg.so


clean:
	-sccs clean
	rm -rf Shared Static
	rm -f $(SHAREDLIB) $(SHAREDLIBDATA) $(STATICLIBRARY) $(SHAREDLIBLN) 
	rm -f tags TAGS .make.state .nse_depinfo

tags:
	ctags $(LIBSOURCES)

FORCE:

