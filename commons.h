#ifndef COMMONS_H
#define COMMONS_H

#define NSMAX 6827
#define NTMAX 120
#define RX_SAMPLE_RATE 12000

#ifdef __cplusplus
#include <cstdbool>
extern "C" {
#else
#include <stdbool.h>
#endif

  /*
   * This structure is shared with Fortran code, it MUST be kept in
   * sync with lib/jt9com.f90
   * int nutc;                   //UTC as integer, HHMM
   * int ntrperiod;              //TR period (seconds)
   * int nfqso;                  //User-selected QSO freq (kHz)
   * int npts8;                  //npts for c0() array
   * int nfa;                    //Low decode limit (Hz)
   * int nfSplit;                //JT65 | JT9 split frequency
   * int nfb;                    //High decode limit (Hz)
   * int ntol;                   //+/- decoding range around fQSO (Hz)
   * bool ndiskdat;              //true ==> data read from *.wav file
   * bool newdat;                //true ==> new data, must do long FFT
   */
extern struct dec_data {
  float ss[184*NSMAX];
  float savg[NSMAX];
  short int d2[NTMAX*RX_SAMPLE_RATE];
  float dd2[NTMAX*RX_SAMPLE_RATE];
  struct
  {
//    char datetime[20]; 
    char mycall[12];
    char mybcall[12];
    char hiscall[12];
    char hisbcall[12];
//    char mygrid[6];
    char hisgrid[6];
    int listutc[10];
    int napwid;
    int nQSOProgress;
    int nftx;
    int nutc;
    int ntrperiod;
    int nfqso;
    int npts8;
    int nfa;
    int nfSplit;
    int nfb;
    int ntol;
    int kin;
    int nzhsym;
    int ndepth;
    int nft8depth;
    int nft8filtdepth;
    int nft8cycles;
    int nft8swlcycles;
    int ntxmode;
    int nmode;
    int nlist;
    int nranera;
    int ntrials10;
    int ntrialsrxf10;
    int naggressive;
    int nharmonicsdepth;
    int ntopfreq65;
    int nprepass;
    int nsdecatt;
    int nlasttx;
    int ndelay;
    int nmt;
    int nft8rxfsens;
    int nft4depth;
    int nsecbandchanged;
    bool ndiskdat;
    bool newdat;
    bool nagain;
    bool nagainfil; 
    bool nswl;
    bool nfilter;
    bool nstophint;
    bool nagcc;
    bool nhint;
    bool fmaskact;
    bool showharmonics;
    bool lft8lowth;
    bool lft8subpass;
    bool lft8latestart;
    bool lhidetest;
    bool lhidetelemetry;
    bool lhideft8dupes;
    bool lhound;
    bool lhidehash;
    bool lcommonft8b;
    bool lmycallstd;
    bool lhiscallstd;
    bool lapmyc;
    bool lmodechanged;
    bool lbandchanged;
    bool lenabledxcsearch;
    bool lwidedxcsearch;
    bool lmultinst;
    bool lskiptx1;
    } params;
} dec_data;

// for unknown reason values of the variables at beginning of dec_data list are being
// not updated while Decode button is pushed manually, for decoding again keep variables at end
// of the list

extern struct {
  float wave[606720];
} foxcom_;

#ifdef __cplusplus
}
#endif

#endif // COMMONS_H
