gcc -c gran.c
gfortran -c four2a.f90
gfortran -c f77_wisdom.f90
gfortran -o chkfft chkfft.f90 four2a.o f77_wisdom.o gran.o /JTSDK-QT/appsupport/runtime/libfftw3f-3.dll
