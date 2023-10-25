@echo off
Title ref [REF]
CALL ..\..\GAMS_SrcTIMES.v4.7.0\VT_GAMS ref ..\GAMS_SrcTIMES.v4.7.0 GAMSSAVE\ref ''  ..\ lo=1 2>&1 | tee "ref_run_log.txt"
GDX2VEDA GAMSSAVE\ref ..\..\GAMS_SrcTIMES.v4.7.0\times2veda.vdd ref_2510
@echo Closed >RunTerminated
