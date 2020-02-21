/*
 sfrsd2.c
 
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
#include <time.h>
#include <string.h>
#include "rs2.h"

static void *rs;

void sfrsd2_(int mrsym[], int mrprob[], int mr2sym[], int mr2prob[], 
	     int* ntrials0, int* verbose0, int correct[], int param[],
	     int indexes[], double tt[], int ntry[])
{        
  int rxdat[63], rxprob[63], rxdat2[63], rxprob2[63];
  int workdat[63],workdat2[63];
  int era_pos[51];
  int c, i, j, numera, nmr2, nerr, nn=63, kk=12;
  FILE *datfile, *logfile;
  int ntrials = *ntrials0;
  int verbose = *verbose0;
  int nhard=0,nhard_min=32768,nsoft=0,nsoft_min=32768, ncandidates;
  int ngmd,nera_best;
  clock_t t0=0,t1=0;
  int perr[8][8] = {
     12,     31,     44,     52,     60,     57,     50,     50,
     28,     38,     49,     58,     65,     69,     64,     80,
     40,     41,     53,     62,     66,     73,     76,     81,
     50,     53,     53,     64,     70,     76,     77,     81,
     50,     50,     52,     60,     71,     72,     77,     84,
     50,     50,     56,     62,     67,     73,     81,     85,
     50,     50,     71,     62,     70,     77,     80,     85,
     50,     50,     62,     64,     71,     75,     82,     87};

  int pmr2[8][8] = { 
      4,      8,      9,      7,      6,      0,      0,      0,
     13,     18,     15,     11,      9,      7,      5,      0,
      0,     23,     21,     15,     12,     10,      7,      4,
      0,     34,     28,     20,     16,     14,     11,      7,
      0,     20,     26,     25,     19,     14,     12,      9,
      0,      0,     28,     27,     22,     19,     14,     11,
      0,      0,     40,     29,     29,     23,     18,     12,
      0,      0,     40,     35,     31,     21,     20,     13};

  if(verbose) {
    logfile=fopen("sfrsd.log","a");
    if( !logfile ) {
      printf("Unable to open sfrsd.log\n");
      exit(1);
    }
  } 
    
// Initialize the KA9Q Reed-Solomon encoder/decoder
  unsigned int symsize=6, gfpoly=0x43, fcr=3, prim=1, nroots=51;
  rs=init_rs_int(symsize, gfpoly, fcr, prim, nroots, 0);

// Reverse the received symbol vector for BM decoder
  for (i=0; i<63; i++) {
    rxdat[i]=mrsym[62-i];
    rxprob[i]=mrprob[62-i];
    rxdat2[i]=mr2sym[62-i];
    rxprob2[i]=mr2prob[62-i];
  }
    
// Sort the mrsym probabilities to find the least reliable symbols
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
    if(verbose) fprintf(logfile,"   BM decode nerrors= %3d : ",nerr);
    memcpy(correct,workdat,63*sizeof(int));
    ngmd=-1;
    param[0]=0;
    param[1]=0;
    param[2]=0;
    param[3]=0;
    param[4]=0;
    return;
  }

/*
Generate random erasure-locator vectors and see if any of them
decode. This will generate a list of potential codewords. The
"soft" distance between each codeword and the received word is
used to decide which codeword is "best".
*/

#ifdef WIN32
  srand(0xdeadbeef);
#else
  srandom(0xdeadbeef);
#endif

  float ratio, ratio0[63];
  int threshe, thresh2, nsum;
  int thresh0[63],thresh1[63], mr2flag;
  ncandidates=0;
  nsum=0;
  int ii,jj;
  for (i=0; i<nn; i++) {
    nsum=nsum+rxprob[i];
    j = indexes[62-i];
    ratio = (float)rxprob2[j]/(float)rxprob[j];
    ratio0[i]=ratio;
    ii = 7.999*ratio;
    jj = (62-i)/8;
    thresh0[i] = 1.3*perr[ii][jj];
    thresh1[i] = 0.4*pmr2[ii][jj];
  }
  if(nsum==0) return;
    
  for( k=0; k<ntrials; k++) {
    memset(era_pos,0,51*sizeof(int));
    memcpy(workdat,rxdat,sizeof(rxdat));

/* 
Mark a subset of the symbols as erasures.
Run through the ranked symbols, starting with the worst, i=0.
NB: j is the symbol-vector index of the symbol with rank i.
*/
    numera=0;
    nmr2=0;
    for (i=0; i<nn; i++) {
      j = indexes[62-i];
      threshe=thresh0[i];
      thresh2=thresh1[i];
      long int ir, ir2;
#ifdef WIN32
      ir=rand();
      ir2=rand();
#else
      ir=random();
      ir2=random();
#endif
      if( ((ir % 100) < threshe ) && (numera+2*nmr2) < 51 ) {
          era_pos[numera]=j;
	  numera=numera+1;
      }
      if( ((ir2 % 100) < thresh2) && (numera+2*nmr2)<51) {
          workdat[j]=rxdat2[j];
          nmr2=nmr2+1;
      }
    }
    t0=clock();
//    rs=init_rs_int(symsize, gfpoly, fcr, prim, nroots, 1);
    nerr=decode_rs_int(rs,workdat,era_pos,numera,1);
    t1=clock();
    tt[0]+=(double)(t1-t0)/CLOCKS_PER_SEC;
        
    if( nerr >= 0 ) {
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
      if((nsoft < 33) && (nhard < 43) && (nhard+nsoft) < 74) {  //???
	if( (nsoft < nsoft_min) ) {
	  nsoft_min=nsoft;
	  nhard_min=nhard;
	  memcpy(correct,workdat,63*sizeof(int));
	  ngmd=0;
	  nera_best=numera;
	  ntry[0]=k;
	}
      }
      if(nsoft_min < 27) break;
      if((nsoft_min < 32) && (nhard_min < 43) && 
	 (nhard_min+nsoft_min) < 74) break;
    }
    if(k == ntrials-1) ntry[0]=k+1;
  }
  
  if(verbose) fprintf(logfile,
     "%d trials and %d candidates after stochastic loop\n",k,ncandidates);

  if( (ncandidates >= 0) && (nsoft_min < 36) && (nhard_min < 44) ) {
    if(verbose) {
      for (i=0; i<63; i++) {
	fprintf(logfile,"%3d %3d %3d %3d %3d %3d\n",i,correct[i],
		rxdat[i],rxprob[i],rxdat2[i],rxprob2[i]);
      }
      fprintf(logfile,"**** ncandidates %d nhard %d nsoft %d nsum %d\n",
	      ncandidates,nhard_min,nsoft_min,nsum);
    }
  } else {
    nhard_min=-1;
  }
  
  if(verbose) {
    fprintf(logfile,"exiting sfrsd\n");
    fclose(logfile);
  }
  param[0]=ncandidates;
  param[1]=nhard_min;
  param[2]=nsoft_min;
  param[3]=nera_best;
  param[4]=ngmd;
  if(param[0]==0) param[2]=-1;
  return;
}
