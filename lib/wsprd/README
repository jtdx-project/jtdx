wsprd is a decoder for K1JT's Weak Signal Propagation Reporter (WSPR) mode.

The program is written in C and is a command-line program that reads from a
.c2 file or .wav file and writes output to the console. It is used by WSJT-X
for wspr-mode decoding. 

USAGE: 
       wsprd [options...] infile

OPTIONS:
       -a <path> path to writeable data files, default="."
       -B disable block demodulation - use single-symbol noncoherent demod
       -c write .c2 file at the end of the first pass
       -C maximum number of decoder cycles per bit, default 10000
       -d deeper search. Slower, a few more decodes
       -e x (x is transceiver dial frequency error in Hz)
       -f x (x is transceiver dial frequency in MHz)
       -H do not use (or update) the hash table
       -J use the stack decoder instead of Fano decoder
       -m decode wspr-15 .wav file
       -o n (0<=n<=5), decoding depth for OSD, default is disabled
       -q quick mode - doesn't dig deep for weak signals
       -s single pass mode, no subtraction (same as original wsprd)
       -v verbose mode (shows dupes)
       -w wideband mode - decode signals within +/- 150 Hz of center
       -z x (x is fano metric table bias, default is 0.45)

infile can be either .wav or .c2

e.g. 
./wsprd -wf 14.0956 140709_2258.wav

Note that for .c2 files, the frequency within the file overrides the command
line value.

FEATURES:
By default, wsprd reports signals that are within +/- 110 Hz of the
subband center frequency. The wideband option (-w) extends this to +/- 150 Hz.

wsprd maintains a hashtable and will decode all three types of wspr
messages. An option (-H) is available to turn off use of the hashtable.

Decoding is attempted using soft symbols derived using single-symbol 
noncoherent detection. If decoding fails, then soft symbols derived using
block demodulation over 2- and 3-symbol blocks are used in subsequent
decoding attempts. For slower computers, block demodulation can be 
disabled with the -B command line option. 

The symbols are decoded using Phil Karn's sequential decoder routine,
fano.c. The -J option replaces the Fano algorithm with the Jelinek 
"stack-bucket" algorithm. If the -o command line option is enabled, then
an ordered-statistics decoder (OSD) is invoked if the Fano (or Jelinek) algorithm
fails. The OSD is a complete decoder, meaning that it always returns a codeword. 
A returned codeword is considered valid only if the unpacked decode contains
a callsign that is already in the hashtable. 

NOTES:
This program attempts to maximize the number of successful decodes per transmit
interval by trying to decode virtually every peak in the averaged spectrum. 
The program also implements two-pass decoding, whereby signals that are successfully
decoded are subtracted one-by-one during the first decoding pass. Then, the 
decoder is run again. In many cases the subtraction process will uncover signals
that can then be successfully decoded on the second pass.

There will be occasional duplicate decodes when two closely spaced 
peaks come from the same signal. The program removes dupes based on callsign 
and frequency. Two decodes that have the same callsign and estimated frequencies
that are within 1 Hz will be treated as decodes of the same signal. This
dupechecking is turned off with the -v flag.

