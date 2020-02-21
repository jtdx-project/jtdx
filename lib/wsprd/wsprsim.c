/*
 File name: wsprsim.c (first committed to wsjtx June 13, 2015)
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "wsprsim_utils.h"
#include "wsprd_utils.h"
#include "fano.h"

int printdata=0;

void usage() {
    printf("Usage: wsprsim [options] message\n");
    printf("       message format:   \"K1ABC FN42 33\"\n");
    printf("                         \"PJ4/K1ABC 33\"\n");
    printf("                         \"<PJ4/K1ABC> FK52UD 33\"\n");
    printf("Options:\n");
    printf("       -c   (print channel symbols)\n");
    printf("       -d   (print packed data with zero tail - 11 bytes)\n");
    printf("       -f x (-100 Hz < f < 100 Hz)\n");
    printf("       -o filename (write a c2 file with this name)\n");
    printf("       -s x (x is snr of signal that is written to .c2 file)\n");
    printf("\n");
    printf(" e.g. ./wsprsim -cds -28 -o 150613_1920.c2 \"K1ABC FN42 33\"\n");
    printf(" then ./wsprd 150613_1920.c2\n");
}

int add_signal_vector(float f0, float t0, float amp, unsigned char* symbols
                      , double* isig, double* qsig)
{
    int i, j, ii, idelay;
    double phi=0.0, twopidt, df, dt, dphi;
    twopidt=8.0*atan(1.0)/375.0;
    df=375.0/256.0;
    dt=1/375.0;
    idelay=t0/dt;
    
    for (i=0; i<162; i++) {
        dphi=twopidt*(f0 + ( (double)symbols[i]-1.5)*df );
        for ( j=0; j<256; j++ ) {
            ii=idelay+256*i+j;
            isig[ii]=isig[ii]+amp*cos(phi);
            qsig[ii]=qsig[ii]+amp*sin(phi);
            phi=phi+dphi;
        }
    }
    return 1;
}

char* tobinary(int x)
{
    static char b[33];
    b[0] = '\0';
    
    long unsigned int z;
    for (z = 0x80000000; z > 0; z >>= 1)
    {
        strcat(b, ((x & z) == z) ? "1" : "0");
    }
    
    return b;
}

double gaussrand()
{
    static double V1, V2, S;
    static int phase = 0;
    double X;
    
    if(phase == 0) {
        do {
            double U1 = (double)rand() / RAND_MAX;
            double U2 = (double)rand() / RAND_MAX;
            
            V1 = 2 * U1 - 1;
            V2 = 2 * U2 - 1;
            S = V1 * V1 + V2 * V2;
        } while(S >= 1 || S == 0);
        
        X = V1 * sqrt(-2 * log(S) / S);
    } else
        X = V2 * sqrt(-2 * log(S) / S);
    
    phase = 1 - phase;
    
    return X;
}

unsigned long writec2file(char *c2filename, int trmin, double freq
                          , double *idat, double *qdat)
{
    int i;
    float buffer[2*45000];
    memset(buffer,0,sizeof(float)*2*45000);
    FILE *fp;
    
    fp = fopen(c2filename,"wb");
    if( fp == NULL ) {
        fprintf(stderr, "Could not open c2 file '%s'\n", c2filename);
        return 0;
    }
    unsigned long nwrite = fwrite(c2filename,sizeof(char),14,fp);
    nwrite = fwrite(&trmin, sizeof(int), 1, fp);
    nwrite = fwrite(&freq, sizeof(double), 1, fp);
    
    for(i=0; i<45000; i++) {
        buffer[2*i]=idat[i];
        buffer[2*i+1]=-qdat[i];
    }
    
    nwrite = fwrite(buffer, sizeof(float), 2*45000, fp);
    if( nwrite == 2*45000 ) {
        return nwrite;
    } else {
        return 0;
    }
}


//********************************************************************
int main(int argc, char *argv[])
{
    extern char *optarg;
    extern int optind;
    int i, c, printchannel=0, writec2=0;
    float snr=50.0;
    float f0=0.0, t0=1.0;
    char *message, *c2filename, *hashtab;
    c2filename=malloc(sizeof(char)*15);
    hashtab=malloc(sizeof(char)*32768*13);
    memset(hashtab,0,sizeof(char)*32768*13);

    // message length is 22 characters
    message=malloc(sizeof(char)*23);
    
    strcpy(c2filename,"000000_0001.c2");

    srand(getpid());

    while ( (c = getopt(argc, argv, "cdf:o:s:")) !=-1 ) {
        switch (c) {
            case 'c':
                printchannel=1;
                break;
            case 'd':
                printdata=1;
                break;
            case 'f':
                f0 = atof(optarg);
            case 'o':
                c2filename = optarg;
                writec2=1;
                break;
            case 's':
//                snr = (float)atoi(optarg);
                snr = atof(optarg);
                break;
        }
    }
    
    if( optind+1 > argc ) {
        usage();
        return 0;
    } else {
        message=argv[optind];
    }
    
    unsigned char channel_symbols[162];
    get_wspr_channel_symbols(message, hashtab, channel_symbols);
    
    if( printchannel ) {
        printf("Channel symbols:\n");
        for (i=0; i<162; i++) {
            printf("%d ",channel_symbols[i]);
        }
        printf("\n");
    }
    
    // add noise, then signal
    double isig[45000], qsig[45000];
    memset(isig,0,sizeof(double)*45000);
    memset(qsig,0,sizeof(double)*45000);
    
    if( snr < 40 ) {
        // snr in 375Hz is 8.2 dB higher than in 2500 Hz.
        snr=snr+8.2;
        snr=pow(10,snr/20.0)*pow(2,0.5);

        for (i = 0; i<45000; i++) {
            isig[i]=isig[i]+gaussrand();
            qsig[i]=qsig[i]+gaussrand();
        }
    } else {
        snr=1.0;
    }
    
    add_signal_vector(f0, t0, snr, channel_symbols, isig, qsig);
    if( writec2) {
        // write a .c2 file
        double carrierfreq=10.1387;
        int wsprtype=2;
        printf("Writing %s\n",c2filename);
        writec2file(c2filename, wsprtype, carrierfreq, isig, qsig);
    }
    return 1;
}
