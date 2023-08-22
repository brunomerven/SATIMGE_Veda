
*Macro closures ---------------------------------

   mps01(INSDNG)  = MPS01SIM(XC,INSDNG);
   tins01(INSDNG) = TINS01SIM(XC,INSDNG);

*Numeraire
  IF(NUMERAIRE(XC) EQ 1,
   CPI.FX      = CPI0;
   DPI.LO     = -INF;
   DPI.UP     = +INF;
   );
  IF(NUMERAIRE(XC) EQ 2,
   DPI.FX     = DPI0;
   CPI.LO     = -INF;
   CPI.UP     = +INF;
   );

*Government
  IF(GOVCLOS(XC) EQ 1,
   TRNADJ.FX  = TRNADJ0;
   TAADJ.FX   = TAADJ0;
   TAPS.FX    = TAPS0;
   TINSADJ.FX = TINSADJ.L + DTAXGR(XC,TC);
   TQADJ.FX   = TQADJ0;
   TQPS.FX    = TQPS0;
   TVAADJ.FX  = TVAADJ0;
   TVAPS.FX   = TVAPS0;
   DTINS.FX   = DTINS0;
   GSAV.LO    = -INF;
   GSAV.UP    = +INF;
   );
  IF(GOVCLOS(XC) EQ 2,
   TRNADJ.FX  = TRNADJ0;
   TAADJ.FX   = TAADJ0;
   TAPS.FX    = TAPS0;
*   TAPS.FX    = TAPSX('BASE',TC,TT);
   TINSADJ.FX = TINSADJ.L + DTAXGR(XC,TC);
   TQADJ.FX   = TQADJ0;
   TQPS.FX    = TQPS0;
   TVAADJ.FX  = TVAADJ0;
   TVAPS.FX   = TVAPS0;
   DTINS.LO   = -INF;
   DTINS.UP   = +INF;
   GSAV.FX    = GSAV.L * (1+GSAVGR(XC,TC));
*   GSAV.FX    = GSAVX('BASE',TC,TT);
   );
  IF(GOVCLOS(XC) EQ 3,
   TRNADJ.FX  = TRNADJ0;
   TAADJ.FX   = TAADJ0;
   TAPS.FX    = TAPS0;
   TINSADJ.LO = -INF;
   TINSADJ.UP = +INF;
   TQADJ.FX   = TQADJ0;
   TQPS.FX    = TQPS0;
   TVAADJ.FX  = TVAADJ0;
   TVAPS.FX   = TVAPS0;
   DTINS.FX   = DTINS.L + DTAXGR(XC,TC);
   GSAV.FX    = GSAV.L * (1+GSAVGR(XC,TC));
*   GSAV.FX    = GSAVX('BASE',TC,TT);
   );
  IF(GOVCLOS(XC) EQ 4,
   TRNADJ.FX  = TRNADJ0;
   TAADJ.FX   = TAADJ0;
   TAPS.LO = -INF;
   TAPS.UP = +INF;
   TINSADJ.FX = TINSADJ.L + DTAXGR(XC,TC);
   TQADJ.FX   = TQADJ0;
   TQPS.FX    = TQPS0;
   TVAADJ.FX  = TVAADJ0;
   TVAPS.FX   = TVAPS0;
   DTINS.FX   = DTINS0;
*   GSAV.FX    = GSAV.L * (1+GSAVGR(XC,TC));
   GSAV.FX    = GSAVX('BASE',TC,TT);
   );
  IF(GOVCLOS(XC) EQ 5,
   TRNADJ.FX  = TRNADJ0;
   TAADJ.LO = -INF;
   TAADJ.UP = +INF;
   TAPS.FX    = TAPS0;
   TINSADJ.FX = TINSADJ.L + DTAXGR(XC,TC);
   TQADJ.FX   = TQADJ0;
   TQPS.FX    = TQPS0;
   TVAADJ.FX  = TVAADJ0;
   TVAPS.FX   = TVAPS0;
   DTINS.FX   = DTINS0;
*   GSAV.FX    = GSAV.L * (1+GSAVGR(XC,TC));
   GSAV.FX    = GSAVX('BASE',TC,TT);
   );

 IF(GOVCLOS(XC) EQ 6,
   TRNADJ.FX  = TRNADJ0;
   TAPS.FX    = TAPS0;
   TAADJ.FX   = TAADJ0;
   TINSADJ.FX = TINSADJ.L + DTAXGR(XC,TC);
   TQADJ.FX   = TQADJ0;
   TQPS.LO    = -INF;
   TQPS.UP    = +INF;
   TVAADJ.FX  = TVAADJ0;
   TVAPS.FX   = TVAPS0;
   DTINS.FX   = DTINS0;
   GSAV.FX    = GSAV.L * (1+GSAVGR(XC,TC));
*   GSAV.FX    = GSAVX('BASE',TC,TT);
   );
  IF(GOVCLOS(XC) EQ 7,
   TRNADJ.FX  = TRNADJ0;
   TAPS.FX    = TAPS0;
   TAADJ.FX   = TAADJ0;
   TINSADJ.FX = TINSADJ.L + DTAXGR(XC,TC);
   TQADJ.LO   = -INF;
   TQADJ.UP   = +INF;
   TQPS.FX    = TQPS0;
   TVAADJ.FX  = TVAADJ0;
   TVAPS.FX   = TVAPS0;
   DTINS.FX   = DTINS0;
   GSAV.FX    = GSAV.L * (1+GSAVGR(XC,TC));
*   GSAV.FX    = GSAVX('BASE',TC,TT);
   );
 IF(GOVCLOS(XC) EQ 8,
   TRNADJ.FX  = TRNADJ0;
   TAPS.FX    = TAPS0;
   TAADJ.FX   = TAADJ0;
   TINSADJ.FX = TINSADJ.L + DTAXGR(XC,TC);
   TQADJ.FX   = TQADJ0;
   TQADJ.FX   = TQADJ0;
   TQPS.FX    = TQPS0;
   TVAPS.LO   = -INF;
   TVAPS.UP   = +INF;
   DTINS.FX   = DTINS0;
*   GSAV.FX    = GSAV.L * (1+GSAVGR(XC,TC));
   GSAV.FX    = GSAVX('BASE',TC,TT);
   );
  IF(GOVCLOS(XC) EQ 9,
   TRNADJ.FX  = TRNADJ0;
   TAPS.FX    = TAPS0;
   TAADJ.FX   = TAADJ0;
   TINSADJ.FX = TINSADJ.L + DTAXGR(XC,TC);
   TQADJ.FX  = TQADJ0;
   TQPS.FX    = TQPS0;
   TVAADJ.LO   = -INF;
   TVAADJ.UP   = +INF;
   TVAPS.FX   = TVAPS0;
   DTINS.FX   = DTINS0;
*   GSAV.FX    = GSAV.L * (1+GSAVGR(XC,TC));
   GSAV.FX    = GSAVX('BASE',TC,TT);
   );

  IF(GOVCLOS(XC) EQ 10,
   TRNADJ.LO  = -INF;
   TRNADJ.UP  = +INF;
   TAPS.FX    = TAPS0;
   TAADJ.FX   = TAADJ0;
   TINSADJ.FX = TINSADJ.L + DTAXGR(XC,TC);
   TQADJ.FX  = TQADJ0;
   TQPS.FX    = TQPS0;
   TVAADJ.FX  = TVAADJ0;
   TVAPS.FX   = TVAPS0;
   DTINS.FX   = DTINS0;
*   GSAV.FX    = GSAV.L * (1+GSAVGR(XC,TC));
   GSAV.FX    = GSAVX('BASE',TC,TT);
   );


*Current account
  IF(ROWCLOS(XC) EQ 1,
   FBOR.FX   = FBOR.L;
   FSAV.FX   = FSAV.L * (1+FSAVGR(XC,TC));
   EXR.LO    = -INF;
   EXR.UP    = +INF;
   );
  IF(ROWCLOS(XC) EQ 2,
   FBOR.FX   = FBOR.L;
   EXR.FX    = EXR0;
   FSAV.LO   = -INF;
   FSAV.UP   = +INF;
   );

*SAVINGS AND INVESTMENT and Govt expenditure closure
  IF(SICLOS(XC) EQ 1,
   MPSADJ.FX = MPSADJ.L * (1+MPSGR(XC,TC));
   DMPS.LO   = -INF;
   DMPS.UP   = +INF;
   IADJ.FX(IT) = IADJ.L(IT);
   INVSHR.LO = -INF;
   INVSHR.UP = +INF;
   GADJ.FX   = GADJ.L * (1+GOVGR(XC,TC));
   GOVSHR.LO = -INF;
   GOVSHR.UP = +INF;
   );
  IF(SICLOS(XC) EQ 2,
   MPSADJ.LO = -INF;
   MPSADJ.UP = +INF;
   DMPS.FX   = DMPS.L + MPSGR(XC,TC);
   IADJ.FX(IT) = IADJ.L(IT);
   INVSHR.LO = -INF;
   INVSHR.UP = +INF;
   GADJ.FX   = GADJ.L * (1+GOVGR(XC,TC));
   GOVSHR.LO = -INF;
   GOVSHR.UP = +INF;
   );
  IF(SICLOS(XC) EQ 3,
   MPSADJ.FX = (1+MPSADJ.L) * (1+MPSGR(XC,TC)) - 1;
   DMPS.FX   = DMPS0;
   IADJ.FX('S-E') = IADJSESIM(XC,TC);
   IADJ.LO('S-I') = -INF;
   IADJ.UP('S-I') = +INF;
   INVSHR.LO = -INF;
   INVSHR.UP = +INF;
   GADJ.FX   = GADJ.L * (1+GOVGR(XC,TC));
   GOVSHR.LO = -INF;
   GOVSHR.UP = +INF;
   );
  IF(SICLOS(XC) EQ 4,
   MPSADJ.FX = MPSADJ.L * (1+MPSGR(XC,TC));
   DMPS.LO   = -INF;
   DMPS.UP   = +INF;
   IADJ.FX('S-E') = IADJSESIM(XC,TC);
   IADJ.LO('S-I') = -INF;
   IADJ.UP('S-I') = +INF;
   INVSHR.FX = INVSHR0;
   GADJ.LO   = -INF;
   GADJ.UP   = +INF;
   GOVSHR.FX = GOVSHR0;
   );
  IF(SICLOS(XC) EQ 5,
   MPSADJ.LO = -INF;
   MPSADJ.UP = +INF;
   DMPS.FX   = DMPS.L + MPSGR(XC,TC);
   IADJ.FX('S-E') = IADJSESIM(XC,TC);
   IADJ.LO('S-I') = -INF;
   IADJ.UP('S-I') = +INF;
   INVSHR.FX = INVSHR0;
   GADJ.LO   = -INF;
   GADJ.UP   = +INF;
   GOVSHR.FX = GOVSHR0;
   );

*Factor closures ------------------

  LOOP(F$FMOBFE(XC,F),
   WFDIST.FX(F,A,RD)$ANFX(A,RD) = WFDIST0(F,A,RD);
   QFS.FX(F)$(NOT FLS(F)) = QFS.L(F);
   WF.LO(F) = -INF;
   WF.UP(F) = +INF;
   QF.LO(F,A,RD)$(ANFX(A,RD) AND QF0(F,A,RD)) = -INF;
   QF.UP(F,A,RD)$(ANFX(A,RD) AND QF0(F,A,RD)) = +INF;
*  Upward-sloping curves (shift QFS0 and leave QFS flexible)
   QFS0(F)$FLS(F) = SUM((RD,A), QF.L(F,A,RD));
   QFS.LO(F)$FLS(F) = -INF;
   QFS.UP(F)$FLS(F) = +INF;

  );

  LOOP(F$FACTFE(XC,F),
   WF.FX(F)   = WF.L(F);
   QF.FX(F,A,RD) = QF.L(F,A,RD);
   WFDIST.LO(F,A,RD)$QF0(F,A,RD) = -INF;
   WFDIST.UP(F,A,RD)$QF0(F,A,RD) = +INF;
   QFS.L(F)  = SUM((A,RD),QF.L(F,A,RD));
   QFS.LO(F) = -INF;
   QFS.UP(F) = +INF;
  );

  LOOP(F$FMOBUE(XC,F),
   WFDIST.FX(F,A,RD) = WFDIST0(F,A,RD);
   WF.FX(F) = WF.L(F) * (1+WGR(F,XC,TC));
   QF.LO(F,A,RD)$QF0(F,A,RD) = -INF;
   QF.UP(F,A,RD)$QF0(F,A,RD) = +INF;
   QFS.LO(F) = -INF;
   QFS.UP(F) = +INF;
  );

 LOOP(A$FL(A),
   QF.FX(FLND,A,RD) = QF.L(FLND,A,RD);
   WFDIST.LO(FLND,A,RD)$QF0(FLND,A,RD) = -INF;
   WFDIST.UP(FLND,A,RD)$QF0(FLND,A,RD) = +INF;
 );

*For nested factors, no changes are to be made to the closure options of the
*aggregate factors essentially these factors do not exist and are entirely flexible
*in every variable thta typically appears in the factor closure (wf, wfdist, qf, qfs).
*these aggregate factors are also not allowed to have upward sloping labor supply
*curves - all such closures apply only to the lowest nesting (or actual factors)
  LOOP(F$FA(F),
   QFS.LO(F)  = -INF ;
   QFS.UP(F)  = +INF ;
   WF.LO(F)   = -INF;
   WF.UP(F)   = +INF;
   QF.LO(F,A,RD)$QF0(F,A,RD) = -INF;
   QF.UP(F,A,RD)$QF0(F,A,RD) = +INF;
   WFDIST.LO(F,A,RD) = -INF;
   WFDIST.UP(F,A,RD) = +INF;
  );




