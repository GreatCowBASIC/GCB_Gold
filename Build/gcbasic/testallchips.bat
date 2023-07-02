echo off
cls
REM updated for /CP 22.10.2019
set FileType=dat
Echo Start>TestAllChips.log
Echo Start>TestAllChipsCompiler.log

Echo 'Default Chip Parameters %date% >chipdata\chipdata.out


rem 12Cxxxx series of chips
for /R chipdata\ %%F in (12*c*.%FileType%) do (

  echo Processing %%~nF
  echo #chip %%~nF > %temp%\chiptest.gcb
  if exist chipdata\%%~nF.dat (
  echo %%~nF >> TestAllChipsCompiler.log
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP /CP >> TestAllChipsCompiler.log

  copy /B  chipdata\chipdata.out+%temp%\"chiptest.config" chipdata\chipdata.out > nul
  
    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)



rem 12HVxxxx series of chips
for /R chipdata\ %%F in (12*hv*.%FileType%) do (

  echo Processing %%~nF
  echo #chip %%~nF > %temp%\chiptest.gcb
  if exist chipdata\%%~nF.dat (
  echo %%~nF >> TestAllChipsCompiler.log	
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP /CP >> TestAllChipsCompiler.log

  copy /B  chipdata\chipdata.out+%temp%\"chiptest.config" chipdata\chipdata.out > nul
  
    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)





for /R chipdata\ %%F in (16c*.%FileType%) do (

  echo Processing %%~nF
  echo #chip %%~nF > %temp%\chiptest.gcb
  if exist chipdata\%%~nF.dat (
  echo %%~nF >> TestAllChipsCompiler.log
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP /CP >> TestAllChipsCompiler.log

  copy /B  chipdata\chipdata.out+%temp%\"chiptest.config" chipdata\chipdata.out > nul
  
    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)



rem 12HVxxxx series of chips
for /R chipdata\ %%F in (16*hv*.%FileType%) do (

  echo Processing %%~nF
  echo #chip %%~nF > %temp%\chiptest.gcb
  if exist chipdata\%%~nF.dat (
  echo %%~nF >> TestAllChipsCompiler.log
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP /CP >> TestAllChipsCompiler.log

  copy /B  chipdata\chipdata.out+%temp%\"chiptest.config" chipdata\chipdata.out > nul
  
    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)




for /R chipdata\ %%F in (18*c*.%FileType%) do (

  echo Processing %%~nF
  echo #chip %%~nF > %temp%\chiptest.gcb
  if exist chipdata\%%~nF.dat (
  echo %%~nF >> TestAllChipsCompiler.log
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP /CP >> TestAllChipsCompiler.log

  copy /B  chipdata\chipdata.out+%temp%\"chiptest.config" chipdata\chipdata.out > nul
  
    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)






for /R chipdata\ %%F in (1*f*.%FileType%) do (

  echo Processing %%~nF	
  echo #chip %%~nF > %temp%\chiptest.gcb

  if exist chipdata\%%~nF.dat (
     echo %%~nF >> TestAllChipsCompiler.log
     gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP /CP >> TestAllChipsCompiler.log

     copy /B  chipdata\chipdata.out+%temp%\"chiptest.config" chipdata\chipdata.out > nul		     

    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)


type chipdata\chipdata.out > chipdata\chipdata.def
del chipdata\chipdata.out

for /R chipdata\ %%F in (mega*.%FileType%) do (

  echo Processing %%~nF
  echo #chip %%~nF > %temp%\chiptest.gcb
  if exist chipdata\%%~nF.dat (
  echo %%~nF >> TestAllChipsCompiler.log
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP >> TestAllChipsCompiler.log
  
    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)


for /R chipdata\ %%F in (tiny*.%FileType%) do (

  echo Processing %%~nF
  echo #chip %%~nF > %temp%\chiptest.gcb
  if exist chipdata\%%~nF.dat (
  echo %%~nF >> TestAllChipsCompiler.log
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP  >> TestAllChipsCompiler.log
  
    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)


echo Completed>>TestAllChips.log