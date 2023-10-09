@echo off
SET "$case_name=nz10_2099_99eessnxc"
SET "$vd_file_name=nz10_2099_99eessnxc_0910"
Title nz10_2099_99eessnxc [ guy trial run unconstrained ] [NZ10_2099_99EESSNXC] 
CALL ..\..\GAMS_SrcTIMES.v4.7.0\VT_GAMS %$case_name% ..\GAMS_SrcTIMES.v4.7.0 GAMSSAVE\%$case_name% '' ..\ lo=1 2>&1 | tee "%$case_name%_run_log.txt"
GDX2VEDA GAMSSAVE\%$case_name% ..\..\GAMS_SrcTIMES.v4.7.0\times2veda.vdd %$vd_file_name%
@echo Closed >RunTerminated
