@echo off
lazbuild -B -q --bm=Release CasUtils\CasBas.lpi
lazbuild -B -q --bm=Release CasUtils\CasBin.lpi
lazbuild -B -q --bm=Release CasUtils\CasCat.lpi
lazbuild -B -q --bm=Release CasUtils\CasFix.lpi
lazbuild -B -q --bm=Release RomLink\RomLink.lpi

dir bin