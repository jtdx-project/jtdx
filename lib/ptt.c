#include <windows.h>
#include <stdio.h>

int ptt_(int *nport, int *ntx, int *iptt)
{
  static HANDLE hFile;
  static int open=0;
  char s[10];
  int i3=1,i4=1,i5=1,i6=1,i9=1,i00=1;

  if(*nport==0) {
    *iptt=*ntx;
    return(0);
  }

  if(*ntx && (!open)) {
    sprintf(s,"\\\\.\\COM%d",*nport);
    hFile=CreateFile(TEXT(s),GENERIC_WRITE,0,NULL,OPEN_EXISTING,
		     FILE_ATTRIBUTE_NORMAL,NULL);
    if(hFile==INVALID_HANDLE_VALUE) {
      //      printf("PTT: Cannot open COM port %d.\n",*nport);
      return 1;
    }
    open=1;
  }

  if(*ntx && open) {
    i3=EscapeCommFunction(hFile,3);
    i5=EscapeCommFunction(hFile,5);
    *iptt=1;
  }

  else {
    i4=EscapeCommFunction(hFile,4);
    i6=EscapeCommFunction(hFile,6);
    i9=EscapeCommFunction(hFile,9);
    i00=CloseHandle(hFile);
    *iptt=0;
    open=0;
  }
  /*
  if(i3==0) return 3;
  if(i4==0) return 4;
  if(i5==0) return 5;
  if(i6==0) return 6;
  if(i9==0) return 9;
  if(i00==0) return 10;
  */
  return 0;
}
