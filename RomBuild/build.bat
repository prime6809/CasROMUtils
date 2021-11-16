@echo off
if [%1]==[] goto usage

SET INFILE="%1.txt"
if not exist "%INFILE%" goto nofile

copy "Z:\Retro\Dragon\Firmware\MultiCartBoot\Ver 0.99\mcbd.rom" .
..\RomLink\romlink %INFILE% mcbd.rom %1.rom
copy %1*.rom g:\emulate\software\dragon\cart\
dir %1*.rom

goto end

:nofile
echo Error! %INFILE% does not exist!
echo

:usage
echo Usage : %0 RomName
echo RomName, is the name of the control file (will have .txt appended), and the produced roms.

:end
