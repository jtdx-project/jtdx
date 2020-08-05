! FT4 
! LDPC(174,91) code, four 4x4 Costas arrays for sync, ramp-up and ramp-down symbols

parameter (KK=91)                     !Information bits (77 + CRC14)
parameter (ND=87)                     !Data symbols
parameter (NS=16)                     !Sync symbols 
parameter (NN=103)                    !Sync and data symbols NN=NS+ND
parameter (NN2=105)                   !Total channel symbols NN2=NS+ND+2
parameter (NSPS=576)                  !Samples per symbol at 12000 S/s
parameter (NZ=59328)                  !Sync and Data samples NZ=NSPS*NN
parameter (NZ2=60480)                 !Total samples in shaped waveform NZ2=NSPS*NN2
!parameter (NMAX=21*3456)              !Samples in iwave (72576)
parameter (NMAX=73728)                !Samples in iwave
!parameter (NFFT1=2304, NH1=NFFT1/2)   !Length of FFTs for symbol spectra
parameter (NFFT1=2560, NH1=1280)      !Length of FFTs for symbol spectra NH1=NFFT1/2
parameter (NSTEP=NSPS)                !Coarse time-sync step size
parameter (NHSYM=123)  !Number of symbol spectra (1/4-sym steps) NHSYM=(NMAX-NFFT1)/NSTEP)
parameter (NDOWN=18)                  !Downsample factor
