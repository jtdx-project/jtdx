gcc -c gran.c
gfortran -c fftw3mod.f90
gfortran -c f77_wisdom.f90
gfortran -o chkfft2 chkfft2.f90 f77_wisdom.o gran.o /JTSDK-QT/appsupport/runtime/libfftw3f-3.dll
