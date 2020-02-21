/*
./jt65code "Hi there"
     Message                 Decoded                Err? Type
--------------------------------------------------------------------------
 1.  HI THERE                HI THERE                    6:    Free text 

Packed message, 6-bit symbols  25 57  1  8 29 22 61 14 46 15 56 28

Information-carrying channel symbols
   34 27 12 48 28 59 12 38 25 47 21 40 46  9 12 24 36  7  4 15 49
   50  6 49 56  2 19 15  7 59 22  7  5 14 20  3 29 56  2  9 17 14
   45 26 43 31 17 10 50 31  2 25 57  1  8 29 22 61 14 46 15 56 28
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include "rs.h"
#include "init_random_seed.h"

static void *rs;

int main(){
  int hi_there[]={25,57,1,8,29,22,61,14,46,15,56,28};
  int data[12], revdat[12];
  int parity[51];
  int rxdat[63], errlocs[63];
  int era_pos[51];
  int i, numera, nerr, nn=63;

  FILE *datfile;
  //nsec,xlambda,maxe,nads,mrsym,mrprob,mr2sym,mr2prob
  int nsec, maxe, nads;
  float xlambda;
  int mrsym[63],mrprob[63],mr2sym[63],mr2prob[63];
  int nsec2,ncount,dat4[12];

  init_random_seed();

  datfile=fopen("kvasd.dat","rb");
  if( !datfile ) {
    printf("Unable to open kvasd.dat\n");
    return 1;
  } else {
    fread(&nsec,sizeof(int),1,datfile);
    fread(&xlambda,sizeof(float),1,datfile);
    fread(&maxe,sizeof(int),1,datfile);
    fread(&nads,sizeof(int),1,datfile);
    fread(&mrsym,sizeof(int),63,datfile);
    fread(&mrprob,sizeof(int),63,datfile);
    fread(&mr2sym,sizeof(int),63,datfile);
    fread(&mr2prob,sizeof(int),63,datfile);
    fread(&nsec2,sizeof(int),1,datfile);
    fread(&ncount,sizeof(int),1,datfile);
    fread(&dat4,sizeof(int),12,datfile);
    fclose(datfile);
    printf("%d %f %d %d \n",nsec,xlambda,maxe,nads);
    for (i=0; i<63; i++) printf("%d ",mrsym[i]);
    printf("\n");
//    for (i=0; i<63; i++) printf("%d ",mrprob[i]);
//    printf("\n");
//    for (i=0; i<63; i++) printf("%d ",mr2sym[i]);
//    printf("\n");
//    for (i=0; i<63; i++) printf("%d ",mr2prob[i]);
//    printf("\n");
//    printf("%d %d \n",nsec2,ncount);
    printf("kv decode: ");
    for (i=0; i<12; i++) printf("%d ",dat4[i]);
    printf("\n");
  }

  // initialize the ka9q reed solomon encoder/decoder
  unsigned int symsize=6, gfpoly=0x43, fcr=3, prim=1, nroots=51;
  rs=init_rs_int(symsize, gfpoly, fcr, prim, nroots, 0);

  // copy the 'hi there' message to the data vector
//  memcpy(data,hi_there,sizeof(hi_there));
//  memcpy(data,dat4,sizeof(dat4));

//  printf("data symbols\n");
//  for( i=0; i<12; i++) {
//    revdat[i]=data[11-i];
//    printf("%d ",data[i]);
//  }
//  printf("\n");

//  encode_rs_int(rs,revdat,parity);

//set up the received symbol vector
//  for( i=0; i<63; i++ ) {
//    if( i < 12 ) rxdat[i]=revdat[i];
//    if( i >=12 ) rxdat[i]=parity[i-12];
//  }

/*
  int errval, errloc;
  int num_errors=0;
  printf("num_errors = %d\n",num_errors);
  for( i=0; i<num_errors; i++) {
    do {
      errval = rand() & nn;
    } while(errval == 0);  //generate random 

    do {
      errloc = rand() % nn;
    } while(errlocs[errloc]!=0); 

    errlocs[errloc] = errval;
    rxdat[errloc] ^= errval;
  } 
*/

  numera=0;
  printf("channel symbols\n");
  for( i=0; i<63; i++ ) {
    rxdat[i]=mrsym[i];
    printf("%d ",rxdat[i]);
  }
  printf("\n");

  nerr=decode_rs_int(rs,rxdat,era_pos,numera);

  printf("nerr %d\n",nerr);

  printf("decoded data\n");
  for(i=0; i<63; i++) printf("%d ",rxdat[i]);
  printf("\n");

  exit(0);
}


