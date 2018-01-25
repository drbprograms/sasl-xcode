OBJ= \
common.o \
sasl.o \
parse.o \
lex.o \
reduce.o \
abstract.o \
make.o \
store.o \
zone.o

common.c: common.h
sasl.c: sasl.h
parse.c: parse.h
lex.c: lex.h
reduce.c: reduce.h
abstract.c: abstract.h
make.c: make.h
store.c: store.h
zone.c: zone.h

%.o: %.c $(DEFS)
	$(CC) $(CFLAGS) -c -o $@ $<

sasl: $(OBJ)

test: sasl test/*.sasl
	(cd test; make all)

clean:
	-rm $(OBJ)

#depend: $(OBJ)
#	for f in $* ;do echo here is: $f ;done
 
