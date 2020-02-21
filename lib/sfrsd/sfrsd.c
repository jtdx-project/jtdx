/*
 sfrsd.c
 
 A soft-decision decoder for the JT65 (63,12) Reed-Solomon code.
 
 This decoding scheme is built around Phil Karn's Berlekamp-Massey
 errors and erasures decoder. The approach is inspired by a number of
 publications, including the stochastic Chase decoder described
 in "Stochastic Chase Decoding of Reed-Solomon Codes", by Leroux et al.,
 IEEE Communications Letters, Vol. 14, No. 9, September 2010 and
 "Soft-Decision Decoding of Reed-Solomon Codes Using Successive Error-
 and-Erasure Decoding," by Soo-Woong Lee and B. V. K. Vijaya Kumar.
 
 Steve Franke K9AN, Urbana IL, September 2015
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include "rs.h"
#include "init_random_seed.h"

static void *rs;

//***************************************************************************
void usage(void)
{
    printf("Usage: sfrsd [options...] <path to kvasd.dat>\n");
    printf("       input file should be in kvasd format\n");
    printf("\n");
    printf("Options:\n");
    printf("       -n number of random erasure vectors to try\n");
    printf("       -v verbose\n");
}

int main(int argc, char *argv[]){
    
    extern char *optarg;
    extern int optind;
    
    int rxdat[63], rxprob[63], rxdat2[63], rxprob2[63];
    int workdat[63], correct[63];
    int era_pos[51];
    int c, i, numera, nerr, nn=63, kk=12;
    char *infile;
    
    FILE *datfile, *logfile;
    int nsec, maxe, nads;
    float xlambda;
    int mrsym[63],mrprob[63],mr2sym[63],mr2prob[63];
    int nsec2,ncount,dat4[12],bestdat[12];
    int ntrials=10000;
    int verbose=0;
    int nhard=0,nhard_min=32768,nsoft=0,nsoft_min=32768, ncandidates;
    
    while ( (c = getopt(argc, argv, "n:qv")) !=-1 ) {
        switch (c) {
                case 'n':
                ntrials=(int)strtof(optarg,NULL);
                printf("ntrials set to %d\n",ntrials);
                break;
                case 'v':
                verbose=1;
                break;
                case 'q': //accept (and ignore) -q option for WSJT10 compatibility
                break;
                case '?':
                usage();
                exit(1);
        }
    }
    
    if( optind+1 > argc) {
        //        usage();
        //        exit(1);
        infile="kvasd.dat";
    } else {
        infile=argv[optind];
    }
    
    logfile=fopen("/tmp/sfrsd.log","a");
    if( !logfile ) {
        printf("Unable to open sfrsd.log\n");
        exit(1);
    }
    
    datfile=fopen(infile,"rb");
    if( !datfile ) {
        printf("Unable to open kvasd.dat\n");
        exit(1);
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
        //        printf("ncount %d\n",ncount);
        fread(&dat4,sizeof(int),12,datfile);
        fclose(datfile);
    }
    
    // initialize the ka9q reed solomon encoder/decoder
    unsigned int symsize=6, gfpoly=0x43, fcr=3, prim=1, nroots=51;
    rs=init_rs_int(symsize, gfpoly, fcr, prim, nroots, 0);
    
    /*    // debug
     int revdat[12], parity[51], correct[63];
     for (i=0; i<12; i++) {
     revdat[i]=dat4[11-i];
     printf("%d ",revdat[i]);
     }
     printf("\n");
     encode_rs_int(rs,revdat,parity);
     for (i=0; i<63; i++) {
     if( i<12 ) {
     correct[i]=revdat[i];
     printf("%d ",parity[i]);
     } else {
     correct[i]=parity[i-12];
     }
     }
     printf("\n");
     */
    
    // reverse the received symbol vector for bm decoder
    for (i=0; i<63; i++) {
        rxdat[i]=mrsym[62-i];
        rxprob[i]=mrprob[62-i];
        rxdat2[i]=mr2sym[62-i];
        rxprob2[i]=mr2prob[62-i];
    }
    
    // sort the mrsym probabilities to find the least reliable symbols
    int k, pass, tmp, nsym=63;
    int probs[63], indexes[63];
    for (i=0; i<63; i++) {
        indexes[i]=i;
        probs[i]=rxprob[i]; // must un-comment sfrsd metrics in demod64a
        
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
    
    // see if we can decode using BM HDD (and calculate the syndrome vector)
    memset(era_pos,0,51*sizeof(int));
    numera=0;
    memcpy(workdat,rxdat,sizeof(rxdat));
    nerr=decode_rs_int(rs,workdat,era_pos,numera,1);
    if( nerr >= 0 ) {
        fprintf(logfile,"   BM decode nerrors= %3d : ",nerr);
        for(i=0; i<12; i++) printf("%2d ",workdat[11-i]);
        fprintf(logfile,"\n");
        fclose(logfile);
        exit(0);
    }
    
    // generate random erasure-locator vectors and see if any of them
    // decode. This will generate a list of potential codewords. The
    // "soft" distance between each codeword and the received word is
    // used to decide which codeword is "best".
    //

    init_random_seed();

    float p_erase;
    int thresh, nsum;
    ncandidates=0;
    
    
    for( k=0; k<ntrials; k++) {
        memset(era_pos,0,51*sizeof(int));
        memcpy(workdat,rxdat,sizeof(rxdat));
        
        // mark a subset of the symbols as erasures
        numera=0;
        for (i=0; i<nn; i++) {
            p_erase=0.0;
            if( probs[62-i] >= 255 ) {
                p_erase = 0.5;
            } else if ( probs[62-i] >= 196 ) {
                p_erase = 0.6;
            } else if ( probs[62-i] >= 128 ) {
                p_erase = 0.6;
            } else if ( probs[62-i] >= 32 ) {
                p_erase = 0.6;
            } else {
                p_erase = 0.8;
            }
            thresh = p_erase*100;
            long int ir;
            ir=rand();
            if( ((ir % 100) < thresh ) && numera < 51 ) {
                era_pos[numera]=indexes[62-i];
                numera=numera+1;
            }
        }
        
        nerr=decode_rs_int(rs,workdat,era_pos,numera,0);
        
        if( nerr >= 0 ) {
            ncandidates=ncandidates+1;
            for(i=0; i<12; i++) dat4[i]=workdat[11-i];
            //            fprintf(logfile,"loop1 decode nerr= %3d : ",nerr);
            //            for(i=0; i<12; i++) fprintf(logfile, "%2d ",dat4[i]);
            //            fprintf(logfile,"\n");
            nhard=0;
            nsoft=0;
            nsum=0;
            for (i=0; i<63; i++) {
                nsum=nsum+rxprob[i];
                if( workdat[i] != rxdat[i] ) {
                    nhard=nhard+1;
                    nsoft=nsoft+rxprob[i];
                }
            }
            if( nsum != 0 ) {
                nsoft=63*nsoft/nsum;
                if( (nsoft < nsoft_min) ) {
                    nsoft_min=nsoft;
                    nhard_min=nhard;
                    memcpy(bestdat,dat4,12*sizeof(int));
                    memcpy(correct,workdat,63*sizeof(int));
                }
                
            } else {
                fprintf(logfile,"error - nsum %d nsoft %d nhard %d\n",nsum,nsoft,nhard);
            }
	    //            if( ncandidates >= 5000 ) {
            if( ncandidates >= ntrials/2 ) {
                break;
            }
        }
    }
    
    fprintf(logfile,"%d candidates after stochastic loop\n",ncandidates);
    
    // do Forney Generalized Minimum Distance pattern
    for (k=0; k<25; k++) {
        memset(era_pos,0,51*sizeof(int));
        numera=2*k;
        for (i=0; i<numera; i++) {
            era_pos[i]=indexes[62-i];
        }
        
        memcpy(workdat,rxdat,sizeof(rxdat));
        nerr=decode_rs_int(rs,workdat,era_pos,numera,0);
        
        if( nerr >= 0 ) {
            ncandidates=ncandidates+1;
            for(i=0; i<12; i++) dat4[i]=workdat[11-i];
            //            fprintf(logfile,"GMD decode nerr= %3d : ",nerr);
            //            for(i=0; i<12; i++) fprintf(logfile, "%2d ",dat4[i]);
            //            fprintf(logfile,"\n");
            nhard=0;
            nsoft=0;
            nsum=0;
            for (i=0; i<63; i++) {
                nsum=nsum+rxprob[i];
                if( workdat[i] != rxdat[i] ) {
                    nhard=nhard+1;
                    nsoft=nsoft+rxprob[i];
                }
            }
            if( nsum != 0 ) {
                nsoft=63*nsoft/nsum;
                if( (nsoft < nsoft_min) ) {
                    nsoft_min=nsoft;
                    nhard_min=nhard;
                    memcpy(bestdat,dat4,12*sizeof(int));
                    memcpy(correct,workdat,63*sizeof(int));
                }
                
            } else {
                fprintf(logfile,"error - nsum %d nsoft %d nhard %d\n",nsum,nsoft,nhard);
            }
	    //            if( ncandidates >=5000 ) {
            if( ncandidates >= ntrials/2 ) {
                break;
            }
        }
    }
    
    fprintf(logfile,"%d candidates after GMD\n",ncandidates);
    
    if( (ncandidates >= 0) && (nsoft_min < 36) && (nhard_min < 44) ) {
        for (i=0; i<63; i++) {
            fprintf(logfile,"%3d %3d %3d %3d %3d %3d\n",i,correct[i],rxdat[i],rxprob[i],rxdat2[i],rxprob2[i]);
            //            fprintf(logfile,"%3d %3d %3d %3d %3d\n",i,workdat[i],rxdat[i],rxprob[i],rxdat2[i],rxprob2[i]);
        }
        
        fprintf(logfile,"**** ncandidates %d nhard %d nsoft %d nsum %d\n",ncandidates,nhard_min,nsoft_min,nsum);
    } else {
        nhard_min=-1;
        memset(bestdat,0,12*sizeof(int));
    }
    datfile=fopen(infile,"wb");
    if( !datfile ) {
        printf("Unable to open kvasd.dat\n");
        return 1;
    } else {
        fwrite(&nsec,sizeof(int),1,datfile);
        fwrite(&xlambda,sizeof(float),1,datfile);
        fwrite(&maxe,sizeof(int),1,datfile);
        fwrite(&nads,sizeof(int),1,datfile);
        fwrite(&mrsym,sizeof(int),63,datfile);
        fwrite(&mrprob,sizeof(int),63,datfile);
        fwrite(&mr2sym,sizeof(int),63,datfile);
        fwrite(&mr2prob,sizeof(int),63,datfile);
        fwrite(&nsec2,sizeof(int),1,datfile);
        fwrite(&nhard_min,sizeof(int),1,datfile);
        fwrite(&bestdat,sizeof(int),12,datfile);
        fclose(datfile);
    }
    
    fprintf(logfile,"exiting sfrsd\n");
    fflush(logfile);
    fclose(logfile);
    exit(0);
}


