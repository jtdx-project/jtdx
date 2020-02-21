#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tstrig.h"

int set_conf(RIG *my_rig, char *conf_parms);

int rig_control(rig_model_t my_model, int verbose);

int main (int argc, char *argv[])
{
  rig_model_t my_model = RIG_MODEL_DUMMY;
  int verbose=0;

  my_model=214;
  rig_control(my_model,verbose);
  return 0;
}

/*
gcc -c -Wall -I../include rig_control.c
gcc -c -Wall -I../include tstrig.c
gcc -o tstrig.exe -Wl,--enable-auto-import tstrig.o rig_control.o libhamlib.dll.a
strip tstrig.exe
*/
