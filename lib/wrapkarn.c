#include <math.h>
#include <stdio.h>
#include <float.h>
#include <limits.h>
#include <stdlib.h>
#include "rs.h"

static void *rs;
static int first=1;

void rs_encode_(int *dgen, int *sent)
// Encode JT65 data dgen[12], producing sent[63].
{
  int dat1[12];
  int b[51];
  int i;

  if(first) {
    // Initialize the JT65 codec
    rs=init_rs_int(6,0x43,3,1,51,0);
    first=0;
  }

  // Reverse data order for the Karn codec.
  for(i=0; i<12; i++) {
    dat1[i]=dgen[11-i];
  }
  // Compute the parity symbols
  encode_rs_int(rs,dat1,b);

  // Move parity symbols and data into sent[] array, in reverse order.
  for (i = 0; i < 51; i++) sent[50-i] = b[i];
  for (i = 0; i < 12; i++) sent[i+51] = dat1[11-i];
}

void rs_decode_(int *recd0, int *era0, int *numera0, int *decoded, int *nerr)
// Decode JT65 received data recd0[63], producing decoded[12].
// Erasures are indicated in era0[numera].  The number of corrected
// errors is *nerr.  If the data are uncorrectable, *nerr=-1 is returned.
{
  int numera;
  int i;
  int era_pos[50];
  int recd[63];

  if(first) {
    rs=init_rs_int(6,0x43,3,1,51,0);
    first=0;
  }

  numera=*numera0;
  for(i=0; i<12; i++) recd[i]=recd0[62-i];
  for(i=0; i<51; i++) recd[12+i]=recd0[50-i];
  if(numera) 
    for(i=0; i<numera; i++) era_pos[i]=era0[i];
  *nerr=decode_rs_int(rs,recd,era_pos,numera);
  for(i=0; i<12; i++) decoded[i]=recd[11-i];
}


void rs_encode__(int *dgen, int *sent)
{
  rs_encode_(dgen, sent);
}

void rs_decode__(int *recd0, int *era0, int *numera0, int *decoded, int *nerr)
{
  rs_decode_(recd0, era0, numera0, decoded, nerr);
}

