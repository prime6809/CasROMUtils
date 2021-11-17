@echo off
lazbuild -B -q CasUtils\CasBas.lpi
lazbuild -B -q CasUtils\CasBin.lpi
lazbuild -B -q CasUtils\CasCat.lpi
lazbuild -B -q CasUtils\CasFix.lpi
lazbuild -B -q RomLink\RomLink.lpi

dir bin