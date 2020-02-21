[Setup]
AppName=wsjtx
AppVerName=wsjtx Version 1.3 r3673
AppCopyright=Copyright (C) 2001-2014 by Joe Taylor, K1JT
DefaultDirName=c:\wsjtx_w
DefaultGroupName=wsjtx_w

[Files]
Source: "c:\Users\joe\wsjt\wsjtx_install\wsjtx.exe";                     DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx_install\jt9.exe";                       DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx_install\jt9code.exe";                   DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx_install\kvasd.exe";                     DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx_install\*.dll";                         DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjtx\cty.dat";                       DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjtx\kvasd.dat";                     DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjtx\wsjt.ico";                      DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjtx\qt.conf";                       DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjtx\CALL3.TXT";                             DestDir: "{app}";  Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\wsjtx\shortcuts.txt";                         DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx\mouse_commands.txt";                    DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx\prefixes.txt";                          DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjtx_install\save\Samples\130418_1742.wav";  DestDir: "{app}\save\Samples";
Source: "c:\Users\joe\wsjt\wsjtx_install\save\Samples\130610_2343.wav";  DestDir: "{app}\save\Samples";
Source: "c:\Users\joe\wsjt\wsjtx_install\platforms\qwindows.dll";        DestDir: "{app}\platforms";
Source: "c:\Users\joe\wsjt\wsjtx_install\Palettes\*.pal";                DestDir: "{app}\Palettes";

[Icons]
Name: "{group}\wsjtx_w";        Filename: "{app}\wsjtx.exe";   WorkingDir: {app}; IconFilename: {app}\wsjt.ico
Name: "{userdesktop}\wsjtx_w";  Filename: "{app}\wsjtx.exe";   WorkingDir: {app}; IconFilename: {app}\wsjt.ico

