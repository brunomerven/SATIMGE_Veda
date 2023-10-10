*--------------------------------------------------------------
*SETUP AND CALL TIMES MODEL IN GAMS -------------------------------------
*--------------------------------------------------------------

* Write executable for running SATIM with specified path and run-name
  PUT  RUNTIMES;
  RUNTIMES.pc = 2;
  RUNTIMES.nd = 5;
  RUNTIMES.ap = 0;

  loop(MRUNCASE(RUN,SATIMCASES),

  put "copy %GDXfolder%\dmd_prj.dd %GDXfolder%\" SATIMCASES.TL:0 "\dmd_prj.dd"
  put "%GDXfolder%\" SATIMCASES.TL:0 "\vtrun.cmd");

  PUTCLOSE "";

* Run TIMES model
 execute "%GDXfolder%\RUNTIMES.CMD"
 execute "echo --- Return to CGE model"

