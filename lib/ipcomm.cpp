// This source code file was last time modified by Igor UA3DJY on September 2nd, 2017
// All changes are shown in the patch file coming together with the full JTDX source code.

#include <QDebug>
#include <QString>
#include <QSharedMemory>
#include <QSystemSemaphore>

#include "../commons.h"

// Multiple instances: KK1D, 17 Jul 2013
QSharedMemory mem_jtdxjt9;

// Semaphore not changed, as the acquire/release calls do not 
// appear to be used anywhere.  
QSystemSemaphore sem_jtdxjt9("sem_jtdxjt9", 1, QSystemSemaphore::Open);

extern "C" {
  bool attach_jtdxjt9_();
  bool create_jtdxjt9_(int nsize);
  bool detach_jtdxjt9_();
  bool lock_jtdxjt9_();
  bool unlock_jtdxjt9_();
  struct jt9com * address_jtdxjt9_();
  int size_jtdxjt9_();
// Multiple instances:  wrapper for QSharedMemory::setKey()
  bool setkey_jtdxjt9_(char* mykey, int mykey_len);

  bool acquire_jtdxjt9_();
  bool release_jtdxjt9_();
}

bool attach_jtdxjt9_() {return mem_jtdxjt9.attach();}
bool create_jtdxjt9_(int nsize) {return mem_jtdxjt9.create(nsize);}
bool detach_jtdxjt9_() {return mem_jtdxjt9.detach();}
bool lock_jtdxjt9_() {return mem_jtdxjt9.lock();}
bool unlock_jtdxjt9_() {return mem_jtdxjt9.unlock();}
struct jt9com * address_jtdxjt9_() {return reinterpret_cast<struct jt9com *>(mem_jtdxjt9.data());}
int size_jtdxjt9_() {return (int)mem_jtdxjt9.size();}

// Multiple instances:
bool setkey_jtdxjt9_(char* mykey, int mykey_len) {
   char *tempstr = (char *)calloc(mykey_len+1,1);
   memset(tempstr, 0, mykey_len+1);
   strncpy(tempstr, mykey, mykey_len);
   QString s1 = QString(QLatin1String(tempstr));
   mem_jtdxjt9.setKey(s1);
   return true;}

bool acquire_jtdxjt9_() {return sem_jtdxjt9.acquire();}
bool release_jtdxjt9_() {return sem_jtdxjt9.release();}
