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
#include "sfrsd2.h"

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
    
    int correct[63], indx[63], param[8];
    int c,i;
    char *infile;
    
    FILE *datfile, *logfile;
    int nsec, maxe, nads;
    float xlambda;
    int mrsym[63],mrprob[63],mr2sym[63],mr2prob[63];
    int nsec2,ncount,dat4[12];
    int ntrials, nverbose, ntry;
    int nhard;
    double tt;

    ntrials=10000;
    nverbose=1;
    
    while ( (c = getopt(argc, argv, "n:qv")) !=-1 ) {
        switch (c) {
                case 'n':
                ntrials=(int)strtof(optarg,NULL);
                printf("ntrials set to %d\n",ntrials);
                break;
                case 'v':
                nverbose=1;
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
        fread(&dat4,sizeof(int),12,datfile);
        fclose(datfile);
    }
    
    sfrsd2_(mrsym,mrprob,mr2sym,mr2prob,&ntrials,&nverbose,correct,param,indx,&tt,&ntry);
    nhard=param[1];
    if( nhard>=0 ) {
        for (i=0; i<12; i++) {
            dat4[i]=correct[11-i];
        }
    } else {
        nhard=-1;
        memset(dat4,0,12*sizeof(int));
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
        fwrite(&nhard,sizeof(int),1,datfile);
        fwrite(&dat4,sizeof(int),12,datfile);
        fclose(datfile);
    }
    exit(0);
}


