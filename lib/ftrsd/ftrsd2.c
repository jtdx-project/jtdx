// This source code file was last time modified by Igor UA3DJY on August 20th, 2017
// All changes are shown in the patch file coming together with the full JTDX source code.

/*
 ftrsd2.c
 
 A soft-decision decoder for the JT65 (63,12) Reed-Solomon code.
 
 This decoding scheme is built around Phil Karn's Berlekamp-Massey
 errors and erasures decoder. The approach is inspired by a number of
 publications, including the stochastic Chase decoder described
 in "Stochastic Chase Decoding of Reed-Solomon Codes", by Leroux et al.,
 IEEE Communications Letters, Vol. 14, No. 9, September 2010 and
 "Soft-Decision Decoding of Reed-Solomon Codes Using Successive Error-
 and-Erasure Decoding," by Soo-Woong Lee and B. V. K. Vijaya Kumar.
 
 Steve Franke K9AN and Joe Taylor K1JT
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
//#include <time.h>
#include <string.h>
#include "rs2.h"

static void *rs;
void getpp_(int workdat[], float *pp);

void ftrsd2_(int mrsym[], int mrprob[], int mr2sym[], int mr2prob[], 
	     int* nvect, int* ipass, int correct[], int param[])
{
  int rxdat[63], rxprob[63], rxdat2[63], rxprob2[63];
  int workdat[63];
  int indexes[63];
  int era_pos[51];
  int i, j, numera, nerr, nn=63;
  int ntrials0 = *nvect;
  int ipassc = *ipass;
  int nhard=0,nhard_min=32768,nsoft=0,nsoft_min=32768;
  int ntotal=0,ntotal_min=32768,ncandidates;
  int nera_best=0;
  float pp,pp1,pp2,param4;
  static unsigned int nseed;

// Power-percentage symbol metrics - composite gnnf/hf 
  int perr[8][8] = {
    { 4,      9,     11,     13,     14,     14,     15,     15},
    { 2,     20,     20,     30,     40,     50,     50,     50},
    { 7,     24,     27,     40,     50,     50,     50,     50},
    {13,     25,     35,     46,     52,     70,     50,     50},
    {17,     30,     42,     54,     55,     64,     71,     70},
    {25,     39,     48,     57,     64,     66,     77,     77},
    {32,     45,     54,     63,     66,     75,     78,     83},
    {51,     58,     57,     66,     72,     77,     82,     86}};

    
// Initialize the KA9Q Reed-Solomon encoder/decoder
  unsigned int symsize=6, gfpoly=0x43, fcr=3, prim=1, nroots=51;
// printf("correct %d param %d\n",sizeof(correct),sizeof(param));
  rs=init_rs_int(symsize, gfpoly, fcr, prim, nroots, 0);

// Reverse the received symbol vectors for BM decoder
  for (i=0; i<63; i++) {
    rxdat[i]=mrsym[62-i];
    rxprob[i]=mrprob[62-i];
    rxdat2[i]=mr2sym[62-i];
    rxprob2[i]=mr2prob[62-i];
  }
    
// Sort rxprob to find indexes of the least reliable symbols
  int k, pass, tmp, nsym=63;
  int probs[63];
  for (i=0; i<63; i++) {
    indexes[i]=i;
    probs[i]=rxprob[i];
  }
  for (pass = 1; pass <= nsym-1; pass++) {
    for (k = 0; k < nsym - pass; k++) {
      if( probs[k] < probs[k+1] ) {
        tmp = probs[k];
        probs[k] = probs[k+1];
        probs[k+1] = tmp;
        tmp = indexes[k];
        indexes[k] = indexes[k+1];
        indexes[k+1] = tmp;
      }
    }
  }
  
// See if we can decode using BM HDD, and calculate the syndrome vector.
  memset(era_pos,0,51*sizeof(int));
  numera=0;
  memcpy(workdat,rxdat,sizeof(rxdat));
  nerr=decode_rs_int(rs,workdat,era_pos,numera,1);
  if( nerr >= 0 ) {
    // Hard-decision decoding succeeded.  Save codeword and some parameters.
    nhard=0;
    for (i=0; i<63; i++) {
      if( workdat[i] != rxdat[i] ) nhard=nhard+1;
    }
    memcpy(correct,workdat,252); // 252=63*sizeof(int)
    param[0]=0;
    param[1]=nhard;
    param[2]=0;
    param[3]=0;
    param[4]=0;
    param[5]=0;
    return;
  }

/*
Hard-decision decoding failed.  Try the FT soft-decision method.
Generate random erasure-locator vectors and see if any of them
decode. This will generate a list of "candidate" codewords.  The
soft distance between each candidate codeword and the received 
word is estimated by finding the largest (pp1) and second-largest 
(pp2) outputs from a synchronized filter-bank operating on the 
symbol spectra, and using these to decide which candidate 
codeword is "best".
*/

  nseed=ipassc;                                 //Seed for random numbers

  float ratio;
  int thresh, nsum;
  int thresh0[63];
  ncandidates=0;
  nsum=0;
  int ii,jj;
  for (i=0; i<nn; i++) {
    nsum=nsum+rxprob[i];
    j = indexes[62-i];
    ratio = (float)rxprob2[j]/((float)rxprob[j]+0.01);
    ii = 7.999*ratio;
    jj = (62-i)/8;
    thresh0[i] = 1.3*perr[ii][jj];
  }
  if(nsum<=0) return;

  pp1=0.0;
  pp2=0.0;
  for (k=1; k<=ntrials0; k++) {
    memset(era_pos,0,51*sizeof(int));
    memcpy(workdat,rxdat,sizeof(rxdat));

 
// Mark a subset of the symbols as erasures.
// Run through the ranked symbols, starting with the worst, i=0.
// NB: j is the symbol-vector index of the symbol with rank i.

    numera=0;
    for (i=0; i<nn; i++) {
      j = indexes[62-i];
      thresh=thresh0[i];
      long int ir;

// Generate a random number ir, 0 <= ir < 100 (see POSIX.1-2001 example).
      nseed = nseed * 1103515245 + 12345;
      ir = (unsigned)(nseed/65536) % 32768;
      ir = (100*ir)/32768;

      if((ir < thresh ) && numera < 51) {
        era_pos[numera]=j;
        numera=numera+1;
      }
    }

    nerr=decode_rs_int(rs,workdat,era_pos,numera,0);        
    if( nerr >= 0 ) {
      /* We have a candidate codeword.  Find its hard and soft distance from
       the received word.  Also find pp1 and pp2 from the full array 
      s3(64,63) of synchronized symbol spectra.*/
      ncandidates=ncandidates+1;
      nhard=0;
      nsoft=0;
      for (i=0; i<63; i++) {
        if(workdat[i] != rxdat[i]) {
          nhard=nhard+1;
          if(workdat[i] != rxdat2[i]) {
            nsoft=nsoft+rxprob[i];
          }
        }
      }
      nsoft=63*nsoft/nsum;
      ntotal=nsoft+nhard;

      getpp_(workdat,&pp);
      if(pp>pp1) {
        pp2=pp1;
        pp1=pp;
        nsoft_min=nsoft;
        nhard_min=nhard;
        ntotal_min=ntotal;
        memcpy(correct,workdat,252); // 252=63*sizeof(int)
        nera_best=numera;
      } else {
        if(pp>pp2 && pp!=pp1) pp2=pp;
      }
      if(nhard_min <= 41 && ntotal_min <= 71) break;
    }
  }
  param4=1000.0*pp2/pp1;
  param[0]=ncandidates;
  param[1]=nhard_min;
  param[2]=nsoft_min;
  param[3]=nera_best;
  param[4]=(int)param4;
  param[5]=ntotal_min;
  if(param[0]==0) param[2]=-1;

  return;
}
