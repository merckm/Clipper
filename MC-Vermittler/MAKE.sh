REM
REM Create Memeory defaults
REM
../../harbour-3.0.0/bin/win/mingw64/hbmk2 savemem
./savemem.exe
REM
REM Create empty Database
REM
../../harbour-3.0.0/bin/win/mingw64/hbmk2 MD-VERMI
./md-vermi.exe
REM
REM Duild main program
REM
../../harbour-3.0.0/bin/win/mingw64/hbmk2 MC0600 MC0640
