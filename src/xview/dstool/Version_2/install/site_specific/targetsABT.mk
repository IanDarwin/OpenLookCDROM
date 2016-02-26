
# Derived parameters.
SOURCES = $(SOURCES_A) $(SOURCES_B)
OBJECTS_A = $(SOURCES_A:.c=.o)
OBJECTS_B = $(SOURCES_B:.c=.o)
OBJECTS = $(OBJECTS_A) $(OBJECTS_B)
TEST_OBJECTS = $(TEST_SOURCES:.c=.o)


# Standard targets.

$(LIBRARY): $(OBJECTS) $(OBJECT_CODE)
	ar rv $@ $?
	ranlib $@

$(TEST_PROGRAM): $(OBJECTS) $(TEST_OBJECTS)
	$(LINK.c) $(TEST_OBJECTS) $(OBJECTS) -o $(TEST_PROGRAM) $(LDFLAGS)


$(OBJECTS_A): $(DEPS_A) $(SOURCES_A)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) $(FLAGS_A) -c $*.c -o $@

$(OBJECTS_B): $(DEPS_B) $(SOURCES_B)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) $(FLAGS_B) -c $*.c -o $@

$(TEST_OBJECTS): $(TEST_DEPS)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) $(FLAGS_TEST) -c $*.c -o $@

install: $(LIBRARY)
	cp $(LIBRARY) $(DSTOOL)/libraries
	make clean

FORCE:


clean:
	$(RM) $(OBJECTS) $(TEST_OBJECTS) $(LIBRARY) *~ *%

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

