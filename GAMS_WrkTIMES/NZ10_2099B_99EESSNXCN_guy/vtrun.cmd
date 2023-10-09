@echo off
SET "$case_name=nz10_2099b_99eessnxcn_guy"
SET "$vd_file_name=nz10_2099b_99eessnxcn_guy_0910"
Title nz10_2099b_99eessnxcn_guy [NZ10_2099B_99EESSNXCN_GUY] 
CALL ..\..\GAMS_SrcTIMES.v4.7.0\VT_GAMS %$case_name% ..\GAMS_SrcTIMES.v4.7.0 GAMSSAVE\%$case_name% '' ..\ lo=1 2>&1 | tee "%$case_name%_run_log.txt"
GDX2VEDA GAMSSAVE\%$case_name% ..\..\GAMS_SrcTIMES.v4.7.0\times2veda.vdd %$vd_file_name%
@echo Closed >RunTerminated
