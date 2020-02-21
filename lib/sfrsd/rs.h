/* User include file for the Reed-Solomon codec
 * Copyright 2002, Phil Karn KA9Q
 * May be used under the terms of the GNU General Public License (GPL)
 */

/* General purpose RS codec, integer symbols */
void encode_rs_int(void *rs,int *data,int *parity);
int decode_rs_int(void *rs,int *data,int *eras_pos,int no_eras, int calc_syn);
void *init_rs_int(int symsize,int gfpoly,int fcr,
		  int prim,int nroots,int pad);
void free_rs_int(void *rs);

/* Tables to map from conventional->dual (Taltab) and
 * dual->conventional (Tal1tab) bases
 */
extern unsigned char Taltab[],Tal1tab[];
