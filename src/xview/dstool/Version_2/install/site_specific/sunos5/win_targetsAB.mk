
# Derived parameters.
SOURCES = $(SOURCES_A) $(SOURCES_B)
OBJECTS_A = $(SOURCES_A:.c=.o)
OBJECTS_B = $(SOURCES_B:.c=.o)
OBJECTS = $(OBJECTS_A) $(OBJECTS_B)


# Standard targets.

$(LIBRARY): $(OBJECTS) $(OBJECT_CODE)
	ar rv $@ $?

$(OBJECTS_A): $(DEPS_A) $(SOURCES_A)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) $(WINCPPFLAGS) $(FLAGS_A) -c $*.c -o $@

$(OBJECTS_B): $(DEPS_B) $(SOURCES_B)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) $(WINCPPFLAGS) $(FLAGS_B) -c $*.c -o $@

install: $(LIBRARY)
	cp $(LIBRARY) $(DSTOOL)/libraries
	make clean

FORCE:


clean:
	$(RM) $(OBJECTS) $(LIBRARY) *~ *%

FILE_LIST = $(SOURCES_A) $(DEPS_A) $(SOURCES_B) $(DEPS_B)

listallfiles:
	@echo $(CWD)/Makefile
	@for i in $(FILE_LIST); do \
		case "$$i" in \
		  FORCE) ;; \
		  /*) echo $$i ;; \
		  *) echo $(CWD)/$$i ;; \
		esac \
	 done
