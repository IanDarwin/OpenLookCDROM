CFLAGS		 = -g

LEX = lex -v

DEST	      = .

EXTHDRS	      =

HDRS	      =

LDFLAGS	      =

LIBS	      =  -ll

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = pspp.o

PRINT	      = pr

PROGRAM	      = pspp

SRCS	      = pspp.l

all:		$(PROGRAM)

${PROGRAM}.shar: $(SRCS) ${MAKEFILE} README test patchlevel.h
	shar -a $(SRCS) ${MAKEFILE} README test  patchlevel.h> ${PROGRAM}.shar

$(PROGRAM):     $(OBJS)
		@echo -n "Loading $(PROGRAM) ... "
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS) $(PROGRAM) core *.BAK *~ 

depend:;	@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@install -s $(PROGRAM) $(DEST)

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

###
