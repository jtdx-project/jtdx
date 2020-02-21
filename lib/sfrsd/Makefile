srcdir = .
prefix = /usr/local
exec_prefix=${prefix}
CC=gcc

CFLAGS=-I/usr/local/include  -Wall -O3

all: sfrsd

encode_rs_int.o: encode_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^

decode_rs_int.o: decode_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^

init_rs_int.o: init_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^

sfrsd: sfrsd.o encode_rs_int.o decode_rs_int.o init_rs_int.o
	gcc -g -o $@ $^

clean:
	rm -f *.o *.a sfrsd

