[Setup]
AppName=wsjtx
AppVerName=wsjtx Version 1.2 r3537
AppCopyright=Copyright (C) 2001-2013 by Joe Taylor, K1JT
DefaultDirName=c:\wsjtx1.2
DefaultGroupName=wsjtx1.2

[Files]
Source: "c:\Users\joe\wsjt\wsjtx_install\wsjtx.exe";                     DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx_install\jt9.exe";                       DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx\shortcuts.txt";                         DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx\mouse_commands.txt";                    DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx\WSJT-X_Users_Guide_v1.2.pdf";           DestDir: "{app}"

[Icons]
Name: "{group}\wsjtx1.2";        Filename: "{app}\wsjtx.exe";   WorkingDir: {app}; IconFilename: {app}\wsjt.ico
Name: "{userdesktop}\wsjtx1.2";  Filename: "{app}\wsjtx.exe";   WorkingDir: {app}; IconFilename: {app}\wsjt.ico

