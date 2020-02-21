/* Include file to configure the RS codec for integer symbols
 *
 * Copyright 2002, Phil Karn, KA9Q
 * May be used under the terms of the GNU General Public License (GPL)
 */
#define DTYPE int

/* Reed-Solomon codec control block */
struct rs {
  int mm;              /* Bits per symbol */
  int nn;              /* Symbols per block (= (1<<mm)-1) */
  DTYPE *alpha_to;     /* log lookup table */
  DTYPE *index_of;     /* Antilog lookup table */
  DTYPE *genpoly;      /* Generator polynomial */
  int nroots;     /* Number of generator roots = number of parity symbols */
  int fcr;        /* First consecutive root, index form */
  int prim;       /* Primitive element, index form */
  int iprim;      /* prim-th root of 1, index form */
  int pad;        /* Padding bytes in shortened block */
};

static int modnn(struct rs *rs,int x){
  while (x >= rs->nn) {
    x -= rs->nn;
    x = (x >> rs->mm) + (x & rs->nn);
  }
  return x;
}
#define MODNN(x) modnn(rs,x)

#define MM (rs->mm)
#define NN (rs->nn)
#define ALPHA_TO (rs->alpha_to) 
#define INDEX_OF (rs->index_of)
#define GENPOLY (rs->genpoly)
//#define NROOTS (rs->nroots)
#define NROOTS (51)
#define FCR (rs->fcr)
#define PRIM (rs->prim)
#define IPRIM (rs->iprim)
#define PAD (rs->pad)
#define A0 (NN)

#define ENCODE_RS encode_rs_int
#define DECODE_RS decode_rs_int
#define INIT_RS init_rs_int
#define FREE_RS free_rs_int

void ENCODE_RS(void *p,DTYPE *data,DTYPE *parity);
int DECODE_RS(void *p,DTYPE *data,int *eras_pos,int no_eras);
void *INIT_RS(int symsize,int gfpoly,int fcr,
		   int prim,int nroots,int pad);
void FREE_RS(void *p);




