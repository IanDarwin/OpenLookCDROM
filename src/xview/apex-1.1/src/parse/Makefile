CFLAGS	= -g
LFLAGS	= 

SRC	= scan.l new.c
OBJ	= new.o scan.o
BIN	= parse

$(BIN)	:	$(OBJ)
	cc $(CFLAGS) $(OBJ) -o $(BIN) -lm

scan.o	: y.tab.h

