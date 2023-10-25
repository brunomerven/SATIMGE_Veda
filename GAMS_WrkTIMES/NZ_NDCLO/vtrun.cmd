@echo off
Title nz_ndclo [NZ_NDCLO]
CALL ..\..\GAMS_SrcTIMES.v4.7.0\VT_GAMS nz_ndclo ..\GAMS_SrcTIMES.v4.7.0 GAMSSAVE\nz_ndclo ''  ..\ lo=1 2>&1 | tee "nz_ndclo_run_log.txt"
GDX2VEDA GAMSSAVE\nz_ndclo ..\..\GAMS_SrcTIMES.v4.7.0\times2veda.vdd nz_ndclo_2510
@echo Closed >RunTerminated
