
# Derived parameters.
SOURCES = $(SOURCES_A)
OBJECTS_A = $(SOURCES_A:.c=.o)
OBJECTS = $(OBJECTS_A)


# Standard targets.

$(LIBRARY): $(OBJECTS) $(OBJECT_CODE)
	ar rv $@ $?

$(OBJECTS_A): $(DEPS_A) $(SOURCES_A)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) $(FLAGS_A) -c $*.c -o $@

install: $(LIBRARY)
	cp $(LIBRARY) $(DSTOOL)/libraries
	make clean

FORCE:


clean:
	$(RM) $(OBJECTS) $(LIBRARY) *~ *%

FILE_LIST = $(SOURCES_A) $(DEPS_A)

listallfiles:
	@echo $(CWD)/Makefile
	@for i in $(FILE_LIST); do \
		case "$$i" in \
		  FORCE) ;; \
		  /*) echo $$i ;; \
		  *) echo $(CWD)/$$i ;; \
		esac \
	 done


