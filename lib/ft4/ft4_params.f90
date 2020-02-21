! FT4 
! LDPC(174,91) code, four 4x4 Costas arrays for sync, ramp-up and ramp-down symbols

parameter (KK=91)                     !Information bits (77 + CRC14)
parameter (ND=87)                     !Data symbols
parameter (NS=16)                     !Sync symbols 
parameter (NN=NS+ND)                  !Sync and data symbols (103)
parameter (NN2=NS+ND+2)               !Total channel symbols (105)
parameter (NSPS=576)                  !Samples per symbol at 12000 S/s
parameter (NZ=NSPS*NN)                !Sync and Data samples (59328)
parameter (NZ2=NSPS*NN2)              !Total samples in shaped waveform (60480)
!parameter (NMAX=21*3456)              !Samples in iwave (72576)
parameter (NMAX=73728)                !Samples in iwave
!parameter (NFFT1=2304, NH1=NFFT1/2)   !Length of FFTs for symbol spectra
parameter (NFFT1=2560, NH1=NFFT1/2)   !Length of FFTs for symbol spectra
parameter (NSTEP=NSPS)                !Coarse time-sync step size
parameter (NHSYM=(NMAX-NFFT1)/NSTEP)  !Number of symbol spectra (1/4-sym steps)
parameter (NDOWN=18)                  !Downsample factor
