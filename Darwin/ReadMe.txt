                    Notes on JTDX Installation for Mac OS X
                    -----------------------------------------

                               Updated 11 October 2018
                               -------------------- 

If you have already installed a previous version of JTDX then I suggest 
you change the name in the Applications folder from JTDX to JTDX_previous 
before proceeding.  

BEGIN:

There are some system matters you must deal with first.  Open a Terminal window
by going to Applications->Utilities and clicking on Terminal.

Along with this ReadMe file there is a file:  com.jtdx.sysctl.plist  which must be copied to a
system area by typing this line in the Terminal window and then pressing the Return key.

      sudo  cp  /Volumes/JTDX/com.jtdx.sysctl.plist  /Library/LaunchDaemons

you will be asked for your normal password because authorisation is needed to copy this file.
(Your password will not be echoed but press the Return key when completed.)
Now re-boot your Mac. This is necessary to install the changes.  After the
reboot you should re-open the Terminal window as before and you can check that the
change has been made by typing:

      sysctl -a | grep sysv.shm

If shmmax is not shown as 14680064 then contact me since JTDX will fail to load with
an error message: "Unable to create shared memory segment".

You are now finished with system changes.  You should make certain that NO error messages
have been produced during these steps.   You can now close the Terminal window.  It will
not be necessary to repeat this procedure again, even when you download an updated
version of JTDX.  It might be necessary if you upgrade macOS.


NEXT:

Drag the JTDX app to your preferred location, such as Applications.

You need to configure your sound card.   Visit Applications > Utilities > Audio MIDI 
Setup and select your sound card and then set Format to be "48000Hz 2ch-16bit" for 
input and output.

Now double-click on the JTDX app and two windows will appear.
It is mandatory to Accept JTDX Audio Capture usage if first launch ask this.
Select Preferences under the JTDX Menu and fill in various station details on the General panel.   
I recommend checking the 4 boxes under the Display heading and the first 4 boxes under 
the Behaviour heading.

Next visit the Audio panel and select the Audio Codec you use to communicate between 
JTDX and your rig.   There are so many audio interfaces available that it is not 
possible to give detailed advice on selection.  If you have difficulties contact me.   
Note the location of the Save Directory.  Decoded wave forms are located here.

Look at the Reporting panel.  If you check the "Prompt me" box, a logging panel will appear 
at the end of the QSO.  Two log files are provided in Library/Application Support/JTDX.
These are a simple wsjtx.log file and wsjtx_log.adi which is formatted for use with 
logging databases.    The "File" menu bar items include a button "Open log directory" 
to open the log directory in Finder for you, ready for processing by any logging 
application you use.

Finally, visit the Radio panel.  JTDX is most effective when operated with CAT 
control.  You will need to install the relevant Mac driver for your rig.   This must 
be located in the device driver directory  /dev. You should install your driver 
and then re-launch JTDX. Return to the the Radio panel in Preferences and in 
the "Serial port" panel select your driver from the list that is presented.   If 
for some reason your driver is not shown, then insert the full name 
of your driver in the Serial Port panel.   Such as:  /dev/tty.PL2303-00002226 or 
whatever driver you have.  The /dev/ prefix is mandatory.  Set the relevant 
communication parameters as required by your transceiver and click "Test CAT" to
check.

JTDX needs the Mac clock to be accurate.  Visit System Preferences > Date & Time 
and make sure that date and time are set automatically.  The drop-down menu will 
normally offer you several time servers to choose from.

On the Help menu, have a look at the new Online User's Guide for operational hints 
and tips.

Please email me if you have problems.

--- Arvo ES1JA     (es1ja@es1ja@homeunix.com)

Addendum:  Information about com.jtdx.sysctl.plist and multiple instances of JTDX.

JTDX makes use of a block of memory which is shared between different parts of
the code.  The normal allocation of shared memory on a Mac is insufficient and this 
has to be increased.  The com.wsjtx.sysctl.plist file is used for this purpose.  You can 
use a Mac editor to examine the file.  (Do not use another editor - the file 
would probably be corrupted.)

It is possible to run multiple instances of WSJT-X simultaneously.  If you wish to run more
instances simultaneously, the shmall parameter in the com.jtdx.sysctl.plist file needs to be
modified as follows.

The shmall parameter determines the amount of shared memory which is allocated in 4096 byte pages
with 14MB (14680064) required for each instance.   The shmall parameter is calculated as: 
(n * 14680064)/4096  where 'n' is the number of instances required to run simultaneously.
Remember to reboot your Mac afterwards.

Note that the shmmax parameter remains unchanged.  This is the maximum amount of shared memory that
any one instance is allowed to request from the total shared memory allocation and should not
be changed.

If two instances of WSJT-X are running, it is likely that you might need additional
audio devices, from two rigs for example.  Visit Audio MIDI Setup and create an Aggregate Device
which will allow you to specify more than one interface.  I recommend you consult Apple's guide
on combining multiple audio interfaces which is at https://support.apple.com/en-us/HT202000.  
