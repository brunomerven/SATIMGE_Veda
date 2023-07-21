@echo off
rem
rem   VT_GAMS.CMD %1 %2 %3 %4 %5
rem
rem   %1 = CaseName
rem   %2 = directory where TIMES GAMS source code resides
rem   %3 = GAMS save file name (less .gdx)
rem   %4 = STOCH for stochastic run, '' for non-stochastic run
rem   %5 = alternate location of DD files; VEDA will pass '..\' for parametric scenario runs
rem
echo *
echo *********************************************************************
echo *                                                                   *
echo *                            VEDA-TIMES                             *
echo *                     ========================                      *
echo *                                                                   *
echo *********************************************************************
echo *
if exist %1.LST del %1.LST
if exist %1.VD del %1.VD
if exist %1.VDE del %1.VDE
if exist %1.VDS del %1.VDS
if exist %1.ERR del %1.ERR
if exist ..\%2\_times.g00 Call GAMS %1.RUN IDIR=..\%2;%5 GDX=%3 PS=99999 r=..\%2\_times
if not exist ..\%2\_times.g00 Call GAMS %1.RUN IDIR=..\%2;%5 GDX=%3 PS=99999
rem if not '%4' == 'STOCH' GDX2VEDA GAMSSAVE\%1 ..\%2\times2veda.vdd %1
rem if     '%4' == 'STOCH' GDX2VEDA GAMSSAVE\%1 ..\%2\times2veda_stc.vdd %1
echo GAMS run ended >> END_GAMS.STA
