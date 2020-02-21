# Makefile for Windows in JTSDK-PY environment

# Re-direct stdout and stderr:     cmd.exe              bash
#                              make > junk 2>&1      make &> junk

CC = gcc
FC = gfortran

FFLAGS = -O2 -DWIN32 -fbounds-check -fno-second-underscore -Wall \
       -Wno-conversion -Wno-character-truncation
CFLAGS = -I. -DWIN32 -DWin32 -DBIGSYM -DHAVE_STRUCT_TIMESPEC

# Default rules
%.o: %.c
	${CC} ${CFLAGS} -c $<
%.o: %.f
	${FC} ${FFLAGS} -c $<
%.o: %.F
	${FC} ${FFLAGS} -c $<
%.o: %.f90
	${FC} ${FFLAGS} -c $<
%.o: %.F90
	${FC} ${FFLAGS} -c $<

all:    libftrsd.a

OBJS1 = extract2.o ftrsd2.o init_rs_int.o encode_rs_int.o decode_rs_int.o
libftrsd.a: $(OBJS1)
	ar cr libftrsd.a $(OBJS1)
	ranlib libftrsd.a
	cp libftrsd.a ..

# Build rsdtest
OBJS2 = rsdtest.o 
rsdtest: $(OBJS2) ../libjt.a
	$(FC) -o rsdtest $(OBJS2) libftrsd.a ../libjt.a ../libpthreadGC2.a

ftrsd: ftrsd.o encode_rs_int.o decode_rs_int.o init_rs_int.o
	gcc -g -o $@ $^

encode_rs_int.o: encode_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^

decode_rs_int.o: decode_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^

init_rs_int.o: init_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^

.PHONY : clean

clean:
	rm -rf *.o libjt.a rsdtest ftrsd
