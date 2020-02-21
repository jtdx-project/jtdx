[Setup]
AppName=wsjtx
AppVerName=wsjtx Version 1.1.0 r3478
AppCopyright=Copyright (C) 2001-2013 by Joe Taylor, K1JT
DefaultDirName=c:\wsjtx2
DefaultGroupName=wsjtx2

[Files]
Source: "c:\Users\joe\wsjt\wsjtx_install\*.exe";                         DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx_install\*.dll";                         DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjtx_install\*.dat";                         DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjtx_install\wsjt.ico";                      DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjtx_install\CALL3.TXT";                     DestDir: "{app}";  Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\wsjtx\shortcuts.txt";                         DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx\mouse_commands.txt";                    DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx\WSJT-X_Users_Guide_v1.1.pdf";           DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx_install\save\Samples\130418_1742.wav";  DestDir: "{app}\save\Samples";
Source: "c:\Users\joe\wsjt\wsjtx_install\save\Samples\130610_2343.wav";  DestDir: "{app}\save\Samples";

[Icons]
Name: "{group}\wsjtx2";        Filename: "{app}\wsjtx.exe";   WorkingDir: {app}; IconFilename: {app}\wsjt.ico
Name: "{userdesktop}\wsjtx2";  Filename: "{app}\wsjtx.exe";   WorkingDir: {app}; IconFilename: {app}\wsjt.ico

