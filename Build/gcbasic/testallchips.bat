echo oN
cls
REM updated for /CP 22.10.2019
REM updated for /CP on AVR 14.10.2024 
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

:AVR

for /R chipdata\ %%F in (mega*.%FileType%) do (

  echo Processing %%~nF
  echo #chip %%~nF > %temp%\chiptest.gcb
  if exist chipdata\%%~nF.dat (
  echo %%~nF >> TestAllChipsCompiler.log
  del %temp%\"chiptest.config" > nul
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP /CP >> TestAllChipsCompiler.log
  dir chipdata\chipdata.out > nul
  copy /B  chipdata\chipdata.out+%temp%\"chiptest.config" chipdata\chipdata.out > nul		     

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
  del %temp%\"chiptest.config" > nul
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP /CP  >> TestAllChipsCompiler.log
  dir chipdata\chipdata.out > nul
  copy /B  chipdata\chipdata.out+%temp%\"chiptest.config" chipdata\chipdata.out > nul		     


    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)

for /R chipdata\ %%F in (AVR*.%FileType%) do (

  echo Processing %%~nF
  echo #chip %%~nF > %temp%\chiptest.gcb
  if exist chipdata\%%~nF.dat (
  echo %%~nF >> TestAllChipsCompiler.log
  del %temp%\"chiptest.config" > nul
  gcbasic.exe "%temp%\chiptest.gcb" /A:GCASM /NP /CP  >> TestAllChipsCompiler.log
  dir chipdata\chipdata.out > nul
  copy /B  chipdata\chipdata.out+%temp%\"chiptest.config" chipdata\chipdata.out > nul		     


    if exist errors.txt (
      echo Processing %%~nF>>TestAllChips.log
      copy /A TestAllChips.log + Errors.txt TestAllChips.log > nul
    )
  )
)

type chipdata\chipdata.out > chipdata\chipdata.def
del chipdata\chipdata.out


echo Completed>>TestAllChips.log