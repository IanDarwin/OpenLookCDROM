PROGRAM = eq_test  	

SOURCES = eq_test.c dslv.c

DEPS_A = 

SOURCES_B =

DEPS_B = 

TEST_PROGRAM = 

TEST_SOURCES = 

TEST_DEPS =

##############################################
# The following lines should not be modified #
# except for working with test code          #
##############################################

# Derived parameters.

OBJECTS = ${SOURCES_A:%.c=%.o}

# Compiler flags.

CFLAGS += -g -DDEBUG
LDFLAGS = -lm

# Standard targets.

${PROGRAM}: ${OBJECTS}
	cc ${CFLAGS} ${SOURCES} -o ${PROGRAM} ${LDFLAGS}


${OBJECTS}: 
	cc ${CFLAGS} ${CPPFLAGS} ${DSFLAGS} -c $*.c -o $@

clean:
	${RM} ${OBJECTS} ${TEST_OBJECTS}

