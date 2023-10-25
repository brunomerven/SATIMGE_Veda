@echo off
Title nz_ndcup [NZ_NDCUP]
CALL ..\..\GAMS_SrcTIMES.v4.7.0\VT_GAMS nz_ndcup ..\GAMS_SrcTIMES.v4.7.0 GAMSSAVE\nz_ndcup ''  ..\ lo=1 2>&1 | tee "nz_ndcup_run_log.txt"
GDX2VEDA GAMSSAVE\nz_ndcup ..\..\GAMS_SrcTIMES.v4.7.0\times2veda.vdd nz_ndcup_2510
@echo Closed >RunTerminated
