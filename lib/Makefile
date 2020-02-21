CC = gcc
CXX = g++
FC = gfortran
AR = ar cr
MKDIR = mkdir -p
CP = cp
RANLIB = ranlib
RM = rm -f

FFLAGS = -O3 -funroll-loops -Wall -Wno-conversion -fno-second-underscore -DUNIX
CFLAGS = -I. -fPIE

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
%.mod: %.f90
	${FC} ${FFLAGS} -c $<

#all:    jt4.mod testjt4 testfast9
all:	testjt65

OBJS1 = testjt4.o jt4.o sync4.o ps4.o four2a.o flat1a.o smo.o xcor4.o \
	slope.o peakup.o db.o pctile.o sort.o timer.o image.o zplt.o

testjt4: $(OBJS1) 
	$(FC) -o testjt4 $(OBJS1) -L. -lfftw3f_threads -lfftw3f 

OBJS2 = t2.o image.o
t2:     $(OBJS2)
	$(FC) -o t2 $(OBJS2)

OBJS3 = testfast9.o four2a.o pctile.o db.o interleave9.o jt9fano.o \
	sort.o fano232.o packjt.o deg2grid.o grid2deg.o fmtmsg.o \

testfast9: $(OBJS3)
	$(FC) -o testfast9 $(OBJS3) C:\JTSDK\fftw3f\libfftw3f-3.dll

OBJS4 = testjt65.o symspec65.o four2a.o db.o flat65.o pctile.o shell.o \
	xcor.o setup65.o slope.o peakup.o sync65.o
testjt65: $(OBJS4)
	$(FC) -o testjt65 $(OBJS4) C:\JTSDK\fftw3f\libfftw3f-3.dll

.PHONY : clean

clean:
	$(RM) *.o libjt9.a testjt4
