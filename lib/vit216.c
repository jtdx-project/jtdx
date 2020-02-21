/* Viterbi decoder for arbitrary convolutional code
 * viterbi27 and viterbi37 for the r=1/2 and r=1/3 K=7 codes are faster
 * Copyright 1999 Phil Karn, KA9Q
 * May be used under the terms of the GNU Public License
 */

/* Select code here */

#define V216


#ifdef V216
#define	K 16			/* Constraint length */
#define N 2			/* Number of symbols per data bit */
#define Polys	Poly216		/* Select polynomials here */
#endif

/* Rate 1/2 codes */
unsigned int Poly216[] = {0126723, 0152711};	/* k = 16  */

#include <memory.h>
#define NULL ((void *)0)

#define LONGBITS 32
#define LOGLONGBITS 5

#undef max
#define max(x,y) ((x) > (y) ? (x) : (y))
#define D       (1 << max(0,K-LOGLONGBITS-1))
#define MAXNBITS 200            /* Maximum frame size (user bits) */

extern unsigned char Partab[];	/* Parity lookup table */

int Syms[1 << K];
int VDInit = 0;

int parity(int x)
{
  x ^= (x >> 16);
  x ^= (x >> 8);
  return Partab[x & 0xff];
}

// Wrapper for calling "encode" from Fortran:
//void __stdcall ENCODE(
void enc216_(
unsigned char data[],           // User data, 8 bits per byte
int *nbits,                     // Number of user bits
unsigned char symbols[],        // Encoded one-bit symbols, 8 per byte
int *nsymbols,                  // Number of symbols
int *kk,                        // K
int *nn)                        // N
{
  int nbytes;
  nbytes=(*nbits+7)/8;          // Always encode multiple of 8 information bits
  enc216(symbols,data,nbytes,0,0); // Do the encoding
  *nsymbols=(*nbits+K-1)*N;        // Return number of encoded symbols
  *kk=K;
  *nn=N;
}

/* Convolutionally encode data into binary symbols */
  enc216(unsigned char symbols[], unsigned char data[],
       unsigned int nbytes, unsigned int startstate,
       unsigned int endstate)
{
  int i,j,k,n=-1;
  unsigned int encstate = startstate;

  for(k=0; k<nbytes; k++) {
    for(i=7;i>=0;i--){
      encstate = (encstate + encstate) + ((data[k] >> i) & 1);
      for(j=0;j<N;j++) {
	n=n+1;
	symbols[n] = parity(encstate & Polys[j]);
      }
    }
  }
  // Flush out with zero tail.  (No need, if tail-biting code.)
  for(i=0; i<K-1;i++){
    encstate = (encstate << 1) | ((endstate >> i) & 1);
    for(j=0;j<N;j++) {
      n=n+1;
      symbols[n] = parity(encstate & Polys[j]);
    }
  }
  return 0;
}

// Wrapper for calling "viterbi" from Fortran:
//void __stdcall VITERBI(
void vit216_(
unsigned char symbols[],  /* Raw deinterleaved input symbols */
unsigned int *Nbits,	  /* Number of decoded information bits */
int mettab[2][256],	  /* Metric table, [sent sym][rx symbol] */
unsigned char ddec[],	  /* Decoded output data */
long *Metric              /* Final path metric (bigger is better) */
){
  long metric;
  vit216(&metric,ddec,symbols,*Nbits,mettab,0,0);
  *Metric=metric;
}

/* Viterbi decoder */
int vit216(
long *metric,           /* Final path metric (returned value) */
unsigned char *data,	/* Decoded output data */
unsigned char *symbols,	/* Raw deinterleaved input symbols */
unsigned int nbits,	/* Number of output bits */
int mettab[2][256],	/* Metric table, [sent sym][rx symbol] */
unsigned int startstate,         /* Encoder starting state */
unsigned int endstate            /* Encoder ending state */
){
  int bitcnt = -(K-1);
  long m0,m1;
  int i,j,sym,ipp;
  int mets[1 << N];
  unsigned long paths[(MAXNBITS+K-1)*D];
  unsigned long *pp,mask;
  long cmetric[1 << (K-1)],nmetric[1 << (K-1)];
  
  memset(paths,0,sizeof(paths));

  // Initialize on first time through:
  if(!VDInit){
    for(i=0;i<(1<<K);i++){
      sym = 0;
      for(j=0;j<N;j++)
	sym = (sym << 1) + parity(i & Polys[j]);
      Syms[i] = sym;
    }
    VDInit++;
  }

  // Keep only lower K-1 bits of specified startstate and endstate
  startstate &= ~((1<<(K-1)) - 1);
  endstate &=   ~((1<<(K-1)) - 1);

  /* Initialize starting metrics */
  for(i=0;i< 1<<(K-1);i++)
    cmetric[i] = -999999;
  cmetric[startstate] = 0;

  pp = paths;
  ipp=0;
  for(;;){ /* For each data bit */
    /* Read input symbols and compute branch metrics */
    for(i=0;i< 1<<N;i++){
      mets[i] = 0;
      for(j=0;j<N;j++){
	mets[i] += mettab[(i >> (N-j-1)) & 1][symbols[j]];
      }
    }
    symbols += N;
    /* Run the add-compare-select operations */
    mask = 1;
    for(i=0;i< 1 << (K-1);i+=2){
      int b1,b2;
      
      b1 = mets[Syms[i]];
      nmetric[i] = m0 = cmetric[i/2] + b1; 
      b2 = mets[Syms[i+1]];
      b1 -= b2;
      m1 = cmetric[(i/2) + (1<<(K-2))] + b2;

      if(m1 > m0){
	nmetric[i] = m1;
	*pp |= mask;
      }

      m0 -= b1;
      nmetric[i+1] = m0;
      m1 += b1;

      if(m1 > m0){
	nmetric[i+1] = m1;
	*pp |= mask << 1;
      }

      mask <<= 2;
      if(mask == 0){
	mask = 1;
	pp++;
	ipp++;
      }
    }
    if(mask != 1){
      pp++;
      ipp++;
    }
    if(++bitcnt == nbits){
      *metric = nmetric[endstate];
      break;
    }
    memcpy(cmetric,nmetric,sizeof(cmetric));
  }

  /* Chain back from terminal state to produce decoded data */
  if(data == NULL)
    return 0;/* Discard output */
  memset(data,0,(nbits+7)/8); /* round up in case nbits % 8 != 0 */

  for(i=nbits-1;i >= 0;i--){
    //    int a0,a1;
    pp -= D;
    ipp -= D;
    m0=endstate >> LOGLONGBITS;
    m1=1L << (endstate & (LONGBITS-1));
    if(pp[m0] & m1) {
      //      a0=nmetric[endstate];
      endstate |= (1 << (K-1));
      //      a1=nmetric[endstate];
      data[i>>3] |= 0x80 >> (i&7);
      //      printf("B  %d  %d  %d  %d\n",*metric,i,a0,a1);
    }
    endstate >>= 1;
  }
  return 0;
}
