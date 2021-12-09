! LDPC (174,91) code
parameter (KK=91)                     !Information bits (77 + CRC14)
parameter (ND=58)                     !Data symbols
parameter (NS=21)                     !Sync symbols (3 @ Costas 7x7)
parameter (NN=79)                     !Total channel symbols NS+ND (79)
parameter (NSPS=1920)                 !Samples per symbol at 12000 S/s
parameter (NZ=151680)                 !Samples in full 15 s waveform NSPS*NN (151,680)
parameter (NMAX=180000)               !Samples in iwave 15s*12000 (180,000)
parameter (NFFT1=3840, NH1=1920)      !Length of FFTs for symbol spectra NFFT1=2*NSPS, NH1=NFFT1/2
parameter (NSTEP=480)                 !Rough time-sync step size NSTEP=NSPS/4
parameter (NHSYM=372)                 !Number of symbol spectra (1/4-sym steps) NHSYM=NMAX/NSTEP-3
parameter (NDOWN=60)                  !Downsample factor
