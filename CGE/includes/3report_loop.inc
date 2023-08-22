*utax code
* GDPTAB1('NOMINAL','PRVCON',XC,TC,TT)   = SUM((C,H), PQX(C,XC,TC,TT)*QHX(C,H,XC,TC,TT));
* GDPTAB1('REAL','PRVCON',XC,TC,TT)      = SUM((C,H), PQ0(C)*QHX(C,H,XC,TC,TT));
 GDPTAB1('NOMINAL','PRVCON',XC,TC,TT)   = SUM((C,H), PQHX(C,H,XC,TC,TT)*QHX(C,H,XC,TC,TT));
 GDPTAB1('REAL','PRVCON',XC,TC,TT)      = SUM((C,H), PQH0(C,H)*QHX(C,H,XC,TC,TT));

 GDPTAB1('NOMINAL','FIXINV',XC,TC,TT)   = SUM((C,IT), PQX(C,XC,TC,TT)*QINVX(C,IT,XC,TC,TT));
 GDPTAB1('REAL','FIXINV',XC,TC,TT)      = SUM((C,IT), PQ0(C)*QINVX(C,IT,XC,TC,TT));
 GDPTAB1('NOMINAL','DSTOCK',XC,TC,TT)   = SUM(C, PQX(C,XC,TC,TT)*qdst(C));
 GDPTAB1('REAL','DSTOCK',XC,TC,TT)      = SUM(C, PQ0(C)*qdst(C));
 GDPTAB1('NOMINAL','GOVCON',XC,TC,TT)   = SUM(C, PQX(C,XC,TC,TT)*QGX(C,XC,TC,TT));
 GDPTAB1('REAL','GOVCON',XC,TC,TT)      = SUM(C, PQ0(C)*QGX(C,XC,TC,TT));
 GDPTAB1('NOMINAL','EXPORTS',XC,TC,TT)  = SUM((RW,CE), PWEX(CE,RW,XC,TC,TT)*EXRX(XC,TC,TT)*QEX(CE,RW,XC,TC,TT));
 GDPTAB1('REAL','EXPORTS',XC,TC,TT)     = SUM((RW,CE), PWE0(CE,RW)*EXR0*QEX(CE,RW,XC,TC,TT));
 GDPTAB1('NOMINAL','IMPORTS',XC,TC,TT)  = -SUM((RW,CM), PWMX(CM,RW,XC,TC,TT)*EXRX(XC,TC,TT)*QMX(CM,RW,XC,TC,TT));
 GDPTAB1('REAL','IMPORTS',XC,TC,TT)     = -SUM((RW,CM), pwm0(CM,RW)*EXR0*QMX(CM,RW,XC,TC,TT));

 GDPTAB1('NOMINAL','GDPMP',XC,TC,TT)    = SUM(IGDP2, GDPTAB1('NOMINAL',IGDP2,XC,TC,TT));
 GDPTAB1('REAL','GDPMP',XC,TC,TT)       = SUM(IGDP2, GDPTAB1('REAL',IGDP2,XC,TC,TT));
 GDPTAB1('NOMINAL','ABSORP',XC,TC,TT)   = GDPTAB1('NOMINAL','GDPMP',XC,TC,TT) - GDPTAB1('NOMINAL','IMPORTS',XC,TC,TT) - GDPTAB1('NOMINAL','EXPORTS',XC,TC,TT);
 GDPTAB1('REAL','ABSORP',XC,TC,TT)      = GDPTAB1('REAL','GDPMP',XC,TC,TT) - GDPTAB1('REAL','IMPORTS',XC,TC,TT) - GDPTAB1('REAL','EXPORTS',XC,TC,TT);

 GDPTAB1('NOMINAL','GDPFC2',XC,TC,TT)   = SUM((RD,A), PVAX(A,RD,XC,TC,TT)*(1-TVAX(A,RD,XC,TC,TT))*QVAX(A,RD,XC,TC,TT));
 GDPTAB1('REAL','GDPFC2',XC,TC,TT)      = SUM((RD,A), PVA0(A,RD)*(1-TVA0(A,RD))*QVAX(A,RD,XC,TC,TT));

 GDPTAB1('NOMINAL','NETITAX',XC,TC,TT)
         = SUM((RD,A), TAX(A,RD,XC,TC,TT)*PARX(A,RD,XC,TC,TT)*QARX(A,RD,XC,TC,TT))
         + SUM((A,RD), TVAX(A,RD,XC,TC,TT)*PVAX(A,RD,XC,TC,TT)*QVAX(A,RD,XC,TC,TT))
         + SUM((RW,CM), TMX(CM,RW,XC,TC,TT)*QMX(CM,RW,XC,TC,TT)*PWMX(CM,RW,XC,TC,TT))*EXRX(XC,TC,TT)
         + SUM((RW,CE), te(CE,RW)*QEX(CE,RW,XC,TC,TT)*PWEX(CE,RW,XC,TC,TT))*EXRX(XC,TC,TT)
         + SUM(C, TQX(C,XC,TC,TT)*PQX(C,XC,TC,TT)*QQX(C,XC,TC,TT))
         + SUM(C, tco2dx(XC,TC,TT)*co2cx(C,XC,TC,TT)*QQX(C,XC,TC,TT))
         + SUM((C,RW), tco2mx(XC,TC,TT)*co2mx(C,RW,XC,TC,TT)*QMX(C,RW,XC,TC,TT))
         - SUM((C,RW), tco2ex(XC,TC,TT)*co2ex(C,RW,XC,TC,TT)*QEX(C,RW,XC,TC,TT))
*utax code
         + SUM((C,A,RD), tui(C,A,RD)*PQX(C,XC,TC,TT)*QINTAX(A,RD,XC,TC,TT)*icax(C,A,RD,XC,TC,TT))
         + SUM((C,H), tuh(C,H)*PQX(C,XC,TC,TT)*QHX(C,H,XC,TC,TT))
         ;

 GDPTAB1('REAL','NETITAX',XC,TC,TT)
         = SUM((RD,A), ta0(A,RD)*PAR0(A,RD)*QARX(A,RD,XC,TC,TT))
         + SUM((A,RD), TVA0(A,RD)*PVA0(A,RD)*QVAX(A,RD,XC,TC,TT))
         + SUM((RW,CM), tm0(CM,RW)*QMX(CM,RW,XC,TC,TT)*PWM0(CM,RW))*EXR0
         + SUM((RW,CE), te0(CE,RW)*QEX(CE,RW,XC,TC,TT)*PWE0(CE,RW))*EXR0
         + SUM(C, tq0(C)*PQ0(C)*QQX(C,XC,TC,TT))
         + SUM(C, tco2d0*co2c0(C)*QQX(C,XC,TC,TT))
         + SUM((C,RW), co2m0(C,RW)*tco2m0*QMX(C,RW,XC,TC,TT))
         - SUM((C,RW), co2e0(C,RW)*tco2e0*QEX(C,RW,XC,TC,TT))
*utax code
         + SUM((C,A,RD), tui0(C,A,RD)*PQ0(C)*QINTAX(A,RD,XC,TC,TT)*icax(C,A,RD,XC,TC,TT))
         + SUM((C,H), tuh0(C,H)*PQ0(C)*QHX(C,H,XC,TC,TT))
         ;

 GDPTAB1('NOMINAL','GDPMP2',XC,TC,TT)            = GDPTAB1('NOMINAL','GDPFC2',XC,TC,TT) + GDPTAB1('NOMINAL','NETITAX',XC,TC,TT);
 GDPTAB1('REAL','GDPMP2',XC,TC,TT)               = GDPTAB1('REAL','GDPFC2',XC,TC,TT) + GDPTAB1('REAL','NETITAX',XC,TC,TT);
 GDPTAB1('NOMSHARE',IGDP,XC,TC,TT)$GDPTAB1('NOMINAL','GDPMP',XC,TC,TT) = 100*GDPTAB1('NOMINAL',IGDP,XC,TC,TT)/GDPTAB1('NOMINAL','GDPMP',XC,TC,TT);
 GDPTAB1('REALSHARE',IGDP,XC,TC,TT)$GDPTAB1('REAL','GDPMP',XC,TC,TT)  = 100*GDPTAB1('REAL',IGDP,XC,TC,TT)/GDPTAB1('REAL','GDPMP',XC,TC,TT);
 GDPTAB1('DEFLATOR',IGDP,XC,TC,TT)$GDPTAB1('REAL',IGDP,XC,TC,TT) = 100*GDPTAB1('NOMINAL',IGDP,XC,TC,TT)/GDPTAB1('REAL',IGDP,XC,TC,TT);

 GDPTAB2('NOMINAL',A,XC,TC,TT)                   = SUM(RD, PVAX(A,RD,XC,TC,TT)*(1-TVAX(A,RD,XC,TC,TT))*QVAX(A,RD,XC,TC,TT));
 GDPTAB2('REAL',A,XC,TC,TT)                      = SUM(RD, PVA0(A,RD)*(1-TVA0(A,RD))*QVAX(A,RD,XC,TC,TT));

 GDPTAB2('NOMINAL','TOTAL',XC,TC,TT)             = SUM(A, GDPTAB2('NOMINAL',A,XC,TC,TT));
 GDPTAB2('REAL','TOTAL',XC,TC,TT)                = SUM(A, GDPTAB2('REAL',A,XC,TC,TT));
 GDPTAB2('NOMSHARE',A,XC,TC,TT)$GDPTAB2('NOMINAL','TOTAL',XC,TC,TT) = 100*GDPTAB2('NOMINAL',A,XC,TC,TT)/GDPTAB2('NOMINAL','TOTAL',XC,TC,TT);
 GDPTAB2('REALSHARE',A,XC,TC,TT)$GDPTAB2('REAL','TOTAL',XC,TC,TT)   = 100*GDPTAB2('REAL',A,XC,TC,TT)/GDPTAB2('REAL','TOTAL',XC,TC,TT);
 GDPTAB2('NOMSHARE','TOTAL',XC,TC,TT)            = SUM(A, GDPTAB2('NOMSHARE',A,XC,TC,TT));
 GDPTAB2('REALSHARE','TOTAL',XC,TC,TT)           = SUM(A, GDPTAB2('REALSHARE',A,XC,TC,TT));
 GDPTAB2('DEFLATOR',A,XC,TC,TT)$GDPTAB2('REAL',A,XC,TC,TT) = 100*GDPTAB2('NOMINAL',A,XC,TC,TT)/GDPTAB2('REAL',A,XC,TC,TT);
 GDPTAB2('DEFLATOR','TOTAL',XC,TC,TT)$GDPTAB2('REAL','TOTAL',XC,TC,TT) = 100*GDPTAB2('NOMINAL','TOTAL',XC,TC,TT)/GDPTAB2('REAL','TOTAL',XC,TC,TT);

*Don't calculate GDPTAB for first year of TIMES
*JT: AE paper (removed this code)
* GDPTAB1(KGDP,IGDP,X,T,TT)$TT1(TT) = 0;
* GDPTAB2(KGDP,AC,X,T,TT)$TT1(TT) = 0;

 GDPGAP(XC,TC,TT) = ABS(GDPTAB1('NOMINAL','GDPMP',XC,TC,TT) - GDPTAB1('NOMINAL','GDPMP2',XC,TC,TT));
 GDPGAP(XC,TC,TT)$(ABS(GDPGAP(XC,TC,TT)) lt 1e-3)  = 0 ;

*Year-on-year percentage changes ------------------------------

 ALPHAVAXY(A,RD,XC,TC,TT)$((NOT T1(TC))   AND ALPHAVAX(A,RD,XC,TC-1,TT))    = (ALPHAVAX(A,RD,XC,TC,TT)   / ALPHAVAX(A,RD,XC,TC-1,TT)-1)*100;
 CPIXY(XC,TC,TT)$((NOT T1(TC))            AND CPIX(XC,TC-1,TT))             = (CPIX(XC,TC,TT)            / CPIX(XC,TC-1,TT)-1)*100;
 DPIXY(XC,TC,TT)$((NOT T1(TC))            AND DPIX(XC,TC-1,TT))             = (DPIX(XC,TC,TT)            / DPIX(XC,TC-1,TT)-1)*100;
 DMPSXY(XC,TC,TT)$((NOT T1(TC))           AND DMPSX(XC,TC-1,TT))            = (DMPSX(XC,TC,TT)           / DMPSX(XC,TC-1,TT)-1)*100;
 DTINSXY(XC,TC,TT)$((NOT T1(TC))          AND DTINSX(XC,TC-1,TT))           = (DTINSX(XC,TC,TT)          / DTINSX(XC,TC-1,TT)-1)*100;
 EGXY(XC,TC,TT)$((NOT T1(TC))             AND EGX(XC,TC-1,TT))              = (EGX(XC,TC,TT)             / EGX(XC,TC-1,TT)-1)*100;
 EHXY(H,XC,TC,TT)$((NOT T1(TC))           AND EHX(H,XC,TC-1,TT))            = (EHX(H,XC,TC,TT)           / EHX(H,XC,TC-1,TT)-1)*100;
 EXRXY(XC,TC,TT)$((NOT T1(TC))            AND EXRX(XC,TC-1,TT))             = (EXRX(XC,TC,TT)            / EXRX(XC,TC-1,TT)-1)*100;
 FSAVXY(XC,TC,TT)$((NOT T1(TC))           AND FSAVX(XC,TC-1,TT))            = (FSAVX(XC,TC,TT)           / FSAVX(XC,TC-1,TT)-1)*100;
 GADJXY(XC,TC,TT)$((NOT T1(TC))           AND GADJX(XC,TC-1,TT))            = (GADJX(XC,TC,TT)           / GADJX(XC,TC-1,TT)-1)*100;
 GOVSHRXY(XC,TC,TT)$((NOT T1(TC))         AND GOVSHRX(XC,TC-1,TT))          = (GOVSHRX(XC,TC,TT)         / GOVSHRX(XC,TC-1,TT)-1)*100;
 GSAVXY(XC,TC,TT)$((NOT T1(TC))           AND GSAVX(XC,TC-1,TT))            = (GSAVX(XC,TC,TT)           / GSAVX(XC,TC-1,TT)-1)*100;
 HPOPXY(H,XC,TC,TT)$((NOT T1(TC))         AND HPOPX(H,XC,TC-1,TT))          = (HPOPX(H,XC,TC,TT)         / HPOPX(H,XC,TC-1,TT)-1)*100;
 IADJXY(IT,XC,TC,TT)$((NOT T1(TC))        AND IADJX(IT,XC,TC-1,TT))         = (IADJX(IT,XC,TC,TT)        / IADJX(IT,XC,TC-1,TT)-1)*100;
 INVSHRXY(XC,TC,TT)$((NOT T1(TC))         AND INVSHRX(XC,TC-1,TT))          = (INVSHRX(XC,TC,TT)         / INVSHRX(XC,TC-1,TT)-1)*100;
* MPSXY(INS,XC,TC)$((NOT T1(TC))        AND MPSX(INS,XC,TC-1,TT))         = (MPSX(INS,XC,TC)        / MPSX(INS,XC,TC-1,TT)-1)*100;
 MPSADJXY(XC,TC,TT)$((NOT T1(TC))         AND MPSADJX(XC,TC-1,TT))          = (MPSADJX(XC,TC,TT)         / MPSADJX(XC,TC-1,TT)-1)*100;
 PAXY(A,XC,TC,TT)$((NOT T1(TC))           AND PAX(A,XC,TC-1,TT))            = (PAX(A,XC,TC,TT)           / PAX(A,XC,TC-1,TT)-1)*100;
 PARXY(A,RD,XC,TC,TT)$((NOT T1(TC))       AND PARX(A,RD,XC,TC-1,TT))        = (PARX(A,RD,XC,TC,TT)       / PARX(A,RD,XC,TC-1,TT)-1)*100;
 PDDXY(C,XC,TC,TT)$((NOT T1(TC))          AND PDDX(C,XC,TC-1,TT))           = (PDDX(C,XC,TC,TT)          / PDDX(C,XC,TC-1,TT)-1)*100;
 PDSXY(C,XC,TC,TT)$((NOT T1(TC))          AND PDSX(C,XC,TC-1,TT))           = (PDSX(C,XC,TC,TT)          / PDSX(C,XC,TC-1,TT)-1)*100;
 PEXY(C,RW,XC,TC,TT)$((NOT T1(TC))        AND PEX(C,RW,XC,TC-1,TT))         = (PEX(C,RW,XC,TC,TT)        / PEX(C,RW,XC,TC-1,TT)-1)*100;
 PINTAXY(A,RD,XC,TC,TT)$((NOT T1(TC))     AND PINTAX(A,RD,XC,TC-1,TT))      = (PINTAX(A,RD,XC,TC,TT)     / PINTAX(A,RD,XC,TC-1,TT)-1)*100;
 PMXY(C,RW,XC,TC,TT)$((NOT T1(TC))        AND PMX(C,RW,XC,TC-1,TT))         = (PMX(C,RW,XC,TC,TT)        / PMX(C,RW,XC,TC-1,TT)-1)*100;
 PQXY(C,XC,TC,TT)$((NOT T1(TC))           AND PQX(C,XC,TC-1,TT))            = (PQX(C,XC,TC,TT)           / PQX(C,XC,TC-1,TT)-1)*100;
 PVAXY(A,RD,XC,TC,TT)$((NOT T1(TC))       AND PVAX(A,RD,XC,TC-1,TT))        = (PVAX(A,RD,XC,TC,TT)       / PVAX(A,RD,XC,TC-1,TT)-1)*100;
 PWEXY(C,RW,XC,TC,TT)$((NOT T1(TC))       AND PWEX(C,RW,XC,TC-1,TT))        = (PWEX(C,RW,XC,TC,TT)       / PWEX(C,RW,XC,TC-1,TT)-1)*100;
 PWMXY(C,RW,XC,TC,TT)$((NOT T1(TC))       AND PWMX(C,RW,XC,TC-1,TT))        = (PWMX(C,RW,XC,TC,TT)       / PWMX(C,RW,XC,TC-1,TT)-1)*100;
 PXXY(C,XC,TC,TT)$((NOT T1(TC))           AND PXX(C,XC,TC-1,TT))            = (PXX(C,XC,TC,TT)           / PXX(C,XC,TC-1,TT)-1)*100;
 PXACXY(A,C,XC,TC,TT)$((NOT T1(TC))       AND PXACX(A,C,XC,TC-1,TT))        = (PXACX(A,C,XC,TC,TT)       / PXACX(A,C,XC,TC-1,TT)-1)*100;
 QAXY(A,XC,TC,TT)$((NOT T1(TC))           AND QAX(A,XC,TC-1,TT))            = (QAX(A,XC,TC,TT)           / QAX(A,XC,TC-1,TT)-1)*100;
 QARXY(A,RD,XC,TC,TT)$((NOT T1(TC))       AND QARX(A,RD,XC,TC-1,TT))        = (QARX(A,RD,XC,TC,TT)       / QARX(A,RD,XC,TC-1,TT)-1)*100;
 QDXY(C,XC,TC,TT)$((NOT T1(TC))           AND QDX(C,XC,TC-1,TT))            = (QDX(C,XC,TC,TT)           / QDX(C,XC,TC-1,TT)-1)*100;
 QEXY(C,RW,XC,TC,TT)$((NOT T1(TC))        AND QEX(C,RW,XC,TC-1,TT))         = (QEX(C,RW,XC,TC,TT)        / QEX(C,RW,XC,TC-1,TT)-1)*100;
 QFXY(F,A,RD,XC,TC,TT)$((NOT T1(TC))      AND QFX(F,A,RD,XC,TC-1,TT))       = (QFX(F,A,RD,XC,TC,TT)      / QFX(F,A,RD,XC,TC-1,TT)-1)*100;
 QFSXY(F,XC,TC,TT)$((NOT T1(TC))          AND QFSX(F,XC,TC-1,TT))           = (QFSX(F,XC,TC,TT)          / QFSX(F,XC,TC-1,TT)-1)*100;
 QGXY(C,XC,TC,TT)$((NOT T1(TC))           AND QGX(C,XC,TC-1,TT))            = (QGX(C,XC,TC,TT)           / QGX(C,XC,TC-1,TT)-1)*100;
 QHXY(C,H,XC,TC,TT)$((NOT T1(TC))         AND QHX(C,H,XC,TC-1,TT))          = (QHX(C,H,XC,TC,TT)         / QHX(C,H,XC,TC-1,TT)-1)*100;
 QHAXY(A,H,XC,TC,TT)$((NOT T1(TC))        AND QHAX(A,H,XC,TC-1,TT))         = (QHAX(A,H,XC,TC,TT)        / QHAX(A,H,XC,TC-1,TT)-1)*100;
 QINTXY(C,A,XC,TC,TT)$((NOT T1(TC))       AND QINTX(C,A,XC,TC-1,TT))        = (QINTX(C,A,XC,TC,TT)       / QINTX(C,A,XC,TC-1,TT)-1)*100;
 QINTAXY(A,RD,XC,TC,TT)$((NOT T1(TC))     AND QINTAX(A,RD,XC,TC-1,TT))      = (QINTAX(A,RD,XC,TC,TT)     / QINTAX(A,RD,XC,TC-1,TT)-1)*100;
 QINVXY(C,IT,XC,TC,TT)$((NOT T1(TC))      AND QINVX(C,IT,XC,TC-1,TT))       = (QINVX(C,IT,XC,TC,TT)      / QINVX(C,IT,XC,TC-1,TT)-1)*100;
 QMXY(C,RW,XC,TC,TT)$((NOT T1(TC))        AND QMX(C,RW,XC,TC-1,TT))         = (QMX(C,RW,XC,TC,TT)        / QMX(C,RW,XC,TC-1,TT)-1)*100;
 QQXY(C,XC,TC,TT)$((NOT T1(TC))           AND QQX(C,XC,TC-1,TT))            = (QQX(C,XC,TC,TT)           / QQX(C,XC,TC-1,TT)-1)*100;
 QTXY(C,XC,TC,TT)$((NOT T1(TC))           AND QTX(C,XC,TC-1,TT))            = (QTX(C,XC,TC,TT)           / QTX(C,XC,TC-1,TT)-1)*100;
 QVAXY(A,RD,XC,TC,TT)$((NOT T1(TC))       AND QVAX(A,RD,XC,TC-1,TT))        = (QVAX(A,RD,XC,TC,TT)       / QVAX(A,RD,XC,TC-1,TT)-1)*100;
 QXXY(C,XC,TC,TT)$((NOT T1(TC))           AND QXX(C,XC,TC-1,TT))            = (QXX(C,XC,TC,TT)           / QXX(C,XC,TC-1,TT)-1)*100;
 QXACXY(A,C,XC,TC,TT)$((NOT T1(TC))       AND QXACX(A,C,XC,TC-1,TT))        = (QXACX(A,C,XC,TC,TT)       / QXACX(A,C,XC,TC-1,TT)-1)*100;
 TABSXY(XC,TC,TT)$((NOT T1(TC))           AND TABSX(XC,TC-1,TT))            = (TABSX(XC,TC,TT)           / TABSX(XC,TC-1,TT)-1)*100;
 TINSXY(INS,XC,TC,TT)$((NOT T1(TC))       AND TINSX(INS,XC,TC-1,TT))        = (TINSX(INS,XC,TC,TT)       / TINSX(INS,XC,TC-1,TT)-1)*100;
*! TINSADJXY(XC,TC,TT)$((NOT T1(TC))        AND TINSADJX(XC,TC-1,TT))         = (TINSADJX(XC,TC,TT)        / TINSADJX(XC,TC-1,TT)-1)*100;
 TAXY(A,RD,XC,TC,TT)$((NOT T1(TC))        AND TAX(A,RD,XC,TC-1,TT))         = (TAX(A,RD,XC,TC,TT)        / TAX(A,RD,XC,TC-1,TT)-1)*100;
 TMXY(C,RW,XC,TC,TT)$((NOT T1(TC))        AND TMX(C,RW,XC,TC-1,TT))         = (TMX(C,RW,XC,TC,TT)        / TMX(C,RW,XC,TC-1,TT)-1)*100;
 TQXY(C,XC,TC,TT)$((NOT T1(TC))           AND TQX(C,XC,TC-1,TT))            = (TQX(C,XC,TC,TT)           / TQX(C,XC,TC-1,TT)-1)*100;
 TRIIXY(INS,INSP,XC,TC,TT)$((NOT T1(TC))  AND TRIIX(INS,INSP,XC,TC-1,TT))   = (TRIIX(INS,INSP,XC,TC,TT)  / TRIIX(INS,INSP,XC,TC-1,TT)-1)*100;
 WFXY(F,XC,TC,TT)$((NOT T1(TC))           AND WFX(F,XC,TC-1,TT))            = (WFX(F,XC,TC,TT)           / WFX(F,XC,TC-1,TT)-1)*100;
 WFDISTXY(F,A,RD,XC,TC,TT)$((NOT T1(TC))  AND WFDISTX(F,A,RD,XC,TC-1,TT))   = (WFDISTX(F,A,RD,XC,TC,TT)  / WFDISTX(F,A,RD,XC,TC-1,TT)-1)*100;
 YFXY(F,XC,TC,TT)$((NOT T1(TC))           AND YFX(F,XC,TC-1,TT))            = (YFX(F,XC,TC,TT)           / YFX(F,XC,TC-1,TT)-1)*100;
 YGXY(XC,TC,TT)$((NOT T1(TC))             AND YGX(XC,TC-1,TT))              = (YGX(XC,TC,TT)             / YGX(XC,TC-1,TT)-1)*100;
 YIFXY(INS,F,XC,TC,TT)$((NOT T1(TC))      AND YIFX(INS,F,XC,TC-1,TT))       = (YIFX(INS,F,XC,TC,TT)      / YIFX(INS,F,XC,TC-1,TT)-1)*100;
 YIXY(INS,XC,TC,TT)$((NOT T1(TC))         AND YIX(INS,XC,TC-1,TT))          = (YIX(INS,XC,TC,TT)         / YIX(INS,XC,TC-1,TT)-1)*100;
 trnsfrxy(INS,ACNT,XC,TC,TT)$((NOT T1(TC)) AND trnsfrx(INS,ACNT,XC,TC-1,TT)) = (trnsfrx(INS,ACNT,XC,TC,TT) / trnsfrx(INS,ACNT,XC,TC-1,TT)-1)*100;

*Average annual compound growth rates -------------------------

 ALPHAVAXA(A,RD,XC,TT)     $SUM(T1, ALPHAVAX(A,RD,XC,T1,TT))   = ((SUM(TRN, ALPHAVAX(A,RD,XC,TRN,TT))  / SUM(T1, ALPHAVAX(A,RD,XC,T1,TT))  )**(1/TRNUM)-1)*100;
 CPIXA(XC,TT)              $SUM(T1, CPIX(XC,T1,TT))            = ((SUM(TRN, CPIX(XC,TRN,TT))           / SUM(T1, CPIX(XC,T1,TT))           )**(1/TRNUM)-1)*100;
 DPIXA(XC,TT)              $SUM(T1, DPIX(XC,T1,TT))            = ((SUM(TRN, DPIX(XC,TRN,TT))           / SUM(T1, DPIX(XC,T1,TT))           )**(1/TRNUM)-1)*100;
 DMPSXA(XC,TT)             $(abs(SUM(T1, DMPSX(XC,T1,TT))) gt .001)           = ((SUM(TRN, DMPSX(XC,TRN,TT))          / SUM(T1, DMPSX(XC,T1,TT))          )**(1/TRNUM)-1)*100;
 DTINSXA(XC,TT)            $SUM(T1, DTINSX(XC,T1,TT))          = ((SUM(TRN, DTINSX(XC,TRN,TT))         / SUM(T1, DTINSX(XC,T1,TT))         )**(1/TRNUM)-1)*100;
 EGXA(XC,TT)               $SUM(T1, EGX(XC,T1,TT))             = ((SUM(TRN, EGX(XC,TRN,TT))            / SUM(T1, EGX(XC,T1,TT))            )**(1/TRNUM)-1)*100;
 EHXA(H,XC,TT)             $SUM(T1, EHX(H,XC,T1,TT))           = ((SUM(TRN, EHX(H,XC,TRN,TT))          / SUM(T1, EHX(H,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 EXRXA(XC,TT)              $SUM(T1, EXRX(XC,T1,TT))            = ((SUM(TRN, EXRX(XC,TRN,TT))           / SUM(T1, EXRX(XC,T1,TT))           )**(1/TRNUM)-1)*100;
 FSAVXA(XC,TT)             $SUM(T1, FSAVX(XC,T1,TT))           = ((SUM(TRN, FSAVX(XC,TRN,TT))          / SUM(T1, FSAVX(XC,T1,TT))          )**(1/TRNUM)-1)*100;
 GADJXA(XC,TT)             $SUM(T1, GADJX(XC,T1,TT))           = ((SUM(TRN, GADJX(XC,TRN,TT))          / SUM(T1, GADJX(XC,T1,TT))          )**(1/TRNUM)-1)*100;
 GOVSHRXA(XC,TT)           $SUM(T1, GOVSHRX(XC,T1,TT))         = ((SUM(TRN, GOVSHRX(XC,TRN,TT))        / SUM(T1, GOVSHRX(XC,T1,TT))        )**(1/TRNUM)-1)*100;
*GSAVXA(XC,TT)             $SUM(T1, GSAVX(XC,T1,TT))           = ((SUM(TRN, GSAVX(XC,TRN,TT))          / SUM(T1, GSAVX(XC,T1,TT))          )**(1/TRNUM)-1)*100;
 HPOPXA(H,XC,TT)           $SUM(T1, HPOPX(H,XC,T1,TT))         = ((SUM(TRN, HPOPX(H,XC,TRN,TT))        / SUM(T1, HPOPX(H,XC,T1,TT))        )**(1/TRNUM)-1)*100;
 IADJXA(IT,XC,TT)          $SUM(T1, IADJX(IT,XC,T1,TT))        = ((SUM(TRN, IADJX(IT,XC,TRN,TT))       / SUM(T1, IADJX(IT,XC,T1,TT))       )**(1/TRNUM)-1)*100;
 INVSHRXA(XC,TT)           $SUM(T1, INVSHRX(XC,T1,TT))         = ((SUM(TRN, INVSHRX(XC,TRN,TT))        / SUM(T1, INVSHRX(XC,T1,TT))        )**(1/TRNUM)-1)*100;
* MPSXA(INS,XC,TT)          $SUM(T1, MPSX(INS,XC,T1,TT))        = ((SUM(TRN, MPSX(INS,XC,TRN,TT))       / SUM(T1, MPSX(INS,XC,T1,TT))       )**(1/TRNUM)-1)*100;
*MPSADJXA(XC,TT)           $SUM(T1, MPSADJX(XC,T1,TT))         = ((SUM(TRN, MPSADJX(XC,TRN,TT))        / SUM(T1, MPSADJX(XC,T1,TT))        )**(1/TRNUM)-1)*100;
 PAXA(A,XC,TT)             $SUM(T1, PAX(A,XC,T1,TT))           = ((SUM(TRN, PAX(A,XC,TRN,TT))          / SUM(T1, PAX(A,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 PARXA(A,RD,XC,TT)         $SUM(T1, PARX(A,RD,XC,T1,TT))       = ((SUM(TRN, PARX(A,RD,XC,TRN,TT))      / SUM(T1, PARX(A,RD,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 PDDXA(C,XC,TT)            $SUM(T1, PDDX(C,XC,T1,TT))          = ((SUM(TRN, PDDX(C,XC,TRN,TT))         / SUM(T1, PDDX(C,XC,T1,TT))         )**(1/TRNUM)-1)*100;
 PDSXA(C,XC,TT)            $SUM(T1, PDSX(C,XC,T1,TT))          = ((SUM(TRN, PDSX(C,XC,TRN,TT))         / SUM(T1, PDSX(C,XC,T1,TT))         )**(1/TRNUM)-1)*100;
 PEXA(C,RW,XC,TT)          $SUM(T1, PEX(C,RW,XC,T1,TT))        = ((SUM(TRN, PEX(C,RW,XC,TRN,TT))       / SUM(T1, PEX(C,RW,XC,T1,TT))       )**(1/TRNUM)-1)*100;
 PINTAXA(A,RD,XC,TT)       $SUM(T1, PINTAX(A,RD,XC,T1,TT))     = ((SUM(TRN, PINTAX(A,RD,XC,TRN,TT))    / SUM(T1, PINTAX(A,RD,XC,T1,TT))    )**(1/TRNUM)-1)*100;
 PMXA(C,RW,XC,TT)          $SUM(T1, PMX(C,RW,XC,T1,TT))        = ((SUM(TRN, PMX(C,RW,XC,TRN,TT))       / SUM(T1, PMX(C,RW,XC,T1,TT))       )**(1/TRNUM)-1)*100;
 PQXA(C,XC,TT)             $SUM(T1, PQX(C,XC,T1,TT))           = ((SUM(TRN, PQX(C,XC,TRN,TT))          / SUM(T1, PQX(C,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 PVAXA(A,RD,XC,TT)         $SUM(T1, PVAX(A,RD,XC,T1,TT))       = ((SUM(TRN, PVAX(A,RD,XC,TRN,TT))      / SUM(T1, PVAX(A,RD,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 PWEXA(C,RW,XC,TT)         $SUM(T1, PWEX(C,RW,XC,T1,TT))       = ((SUM(TRN, PWEX(C,RW,XC,TRN,TT))      / SUM(T1, PWEX(C,RW,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 PWMXA(C,RW,XC,TT)         $SUM(T1, PWMX(C,RW,XC,T1,TT))       = ((SUM(TRN, PWMX(C,RW,XC,TRN,TT))      / SUM(T1, PWMX(C,RW,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 PXXA(C,XC,TT)             $SUM(T1, PXX(C,XC,T1,TT))           = ((SUM(TRN, PXX(C,XC,TRN,TT))          / SUM(T1, PXX(C,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 PXACXA(A,C,XC,TT)         $SUM(T1, PXACX(A,C,XC,T1,TT))       = ((SUM(TRN, PXACX(A,C,XC,TRN,TT))      / SUM(T1, PXACX(A,C,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 QAXA(A,XC,TT)             $SUM(T1, QAX(A,XC,T1,TT))           = ((SUM(TRN, QAX(A,XC,TRN,TT))          / SUM(T1, QAX(A,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 QARXA(A,RD,XC,TT)         $SUM(T1, QARX(A,RD,XC,T1,TT))       = ((SUM(TRN, QARX(A,RD,XC,TRN,TT))      / SUM(T1, QARX(A,RD,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 QDXA(C,XC,TT)             $SUM(T1, QDX(C,XC,T1,TT))           = ((SUM(TRN, QDX(C,XC,TRN,TT))          / SUM(T1, QDX(C,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 QEXA(C,RW,XC,TT)          $SUM(T1, QEX(C,RW,XC,T1,TT))        = ((SUM(TRN, QEX(C,RW,XC,TRN,TT))       / SUM(T1, QEX(C,RW,XC,T1,TT))       )**(1/TRNUM)-1)*100;
 QFXA(F,A,RD,XC,TT)        $SUM(T1, QFX(F,A,RD,XC,T1,TT))      = ((SUM(TRN, QFX(F,A,RD,XC,TRN,TT))     / SUM(T1, QFX(F,A,RD,XC,T1,TT))     )**(1/TRNUM)-1)*100;
 QFSXA(F,XC,TT)            $SUM(T1, QFSX(F,XC,T1,TT))          = ((SUM(TRN, QFSX(F,XC,TRN,TT))         / SUM(T1, QFSX(F,XC,T1,TT))         )**(1/TRNUM)-1)*100;
 QGXA(C,XC,TT)             $SUM(T1, QGX(C,XC,T1,TT))           = ((SUM(TRN, QGX(C,XC,TRN,TT))          / SUM(T1, QGX(C,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 QHXA(C,H,XC,TT)           $SUM(T1, QHX(C,H,XC,T1,TT))         = ((SUM(TRN, QHX(C,H,XC,TRN,TT))        / SUM(T1, QHX(C,H,XC,T1,TT))        )**(1/TRNUM)-1)*100;
 QHAXA(A,H,XC,TT)          $SUM(T1, QHAX(A,H,XC,T1,TT))        = ((SUM(TRN, QHAX(A,H,XC,TRN,TT))       / SUM(T1, QHAX(A,H,XC,T1,TT))       )**(1/TRNUM)-1)*100;
 QINTXA(C,A,XC,TT)         $SUM(T1, QINTX(C,A,XC,T1,TT))       = ((SUM(TRN, QINTX(C,A,XC,TRN,TT))      / SUM(T1, QINTX(C,A,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 QINTAXA(A,RD,XC,TT)       $SUM(T1, QINTAX(A,RD,XC,T1,TT))     = ((SUM(TRN, QINTAX(A,RD,XC,TRN,TT))    / SUM(T1, QINTAX(A,RD,XC,T1,TT))    )**(1/TRNUM)-1)*100;
 QINVXA(C,IT,XC,TT)        $SUM(T1, QINVX(C,IT,XC,T1,TT))      = ((SUM(TRN, QINVX(C,IT,XC,TRN,TT))     / SUM(T1, QINVX(C,IT,XC,T1,TT))     )**(1/TRNUM)-1)*100;
 QMXA(C,RW,XC,TT)          $SUM(T1, QMX(C,RW,XC,T1,TT))        = ((SUM(TRN, QMX(C,RW,XC,TRN,TT))       / SUM(T1, QMX(C,RW,XC,T1,TT))       )**(1/TRNUM)-1)*100;
 QQXA(C,XC,TT)             $SUM(T1, QQX(C,XC,T1,TT))           = ((SUM(TRN, QQX(C,XC,TRN,TT))          / SUM(T1, QQX(C,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 QTXA(C,XC,TT)             $SUM(T1, QTX(C,XC,T1,TT))           = ((SUM(TRN, QTX(C,XC,TRN,TT))          / SUM(T1, QTX(C,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 QVAXA(A,RD,XC,TT)         $SUM(T1, QVAX(A,RD,XC,T1,TT))       = ((SUM(TRN, QVAX(A,RD,XC,TRN,TT))      / SUM(T1, QVAX(A,RD,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 QXXA(C,XC,TT)             $SUM(T1, QXX(C,XC,T1,TT))           = ((SUM(TRN, QXX(C,XC,TRN,TT))          / SUM(T1, QXX(C,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 QXACXA(A,C,XC,TT)         $SUM(T1, QXACX(A,C,XC,T1,TT))       = ((SUM(TRN, QXACX(A,C,XC,TRN,TT))      / SUM(T1, QXACX(A,C,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 TABSXA(XC,TT)             $SUM(T1, TABSX(XC,T1,TT))           = ((SUM(TRN, TABSX(XC,TRN,TT))          / SUM(T1, TABSX(XC,T1,TT))          )**(1/TRNUM)-1)*100;
 TINSXA(INS,XC,TT)         $SUM(T1, TINSX(INS,XC,T1,TT))       = ((SUM(TRN, TINSX(INS,XC,TRN,TT))      / SUM(T1, TINSX(INS,XC,T1,TT))      )**(1/TRNUM)-1)*100;
 TINSADJXA(XC,TT)          $(abs(SUM(T1, TINSADJX(XC,T1,TT))) gt .001)        = ((SUM(TRN, TINSADJX(XC,TRN,TT))       / SUM(T1, TINSADJX(XC,T1,TT))       )**(1/TRNUM)-1)*100;
*! TAXA(A,RD,XC,TT)          $SUM(T1, TAX(A,RD,XC,T1,TT))        = ((SUM(TRN, TAX(A,RD,XC,TRN,TT))       / SUM(T1, TAX(A,RD,XC,T1,TT))       )**(1/TRNUM)-1)*100;
 TMXA(C,RW,XC,TT)          $SUM(T1, TMX(C,RW,XC,T1,TT))        = ((SUM(TRN, TMX(C,RW,XC,TRN,TT))       / SUM(T1, TMX(C,RW,XC,T1,TT))       )**(1/TRNUM)-1)*100;
*! TQXA(C,XC,TT)             $SUM(T1, TQX(C,XC,T1,TT))           = ((SUM(TRN, TQX(C,XC,TRN,TT))          / SUM(T1, TQX(C,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 TRIIXA(INS,INSP,XC,TT)    $SUM(T1, TRIIX(INS,INSP,XC,T1,TT))  = ((SUM(TRN, TRIIX(INS,INSP,XC,TRN,TT)) / SUM(T1, TRIIX(INS,INSP,XC,T1,TT)) )**(1/TRNUM)-1)*100;
 WFXA(F,XC,TT)             $SUM(T1, WFX(F,XC,T1,TT))           = ((SUM(TRN, WFX(F,XC,TRN,TT))          / SUM(T1, WFX(F,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 WFDISTXA(F,A,RD,XC,TT)    $SUM(T1, WFDISTX(F,A,RD,XC,T1,TT))  = ((SUM(TRN, WFDISTX(F,A,RD,XC,TRN,TT)) / SUM(T1, WFDISTX(F,A,RD,XC,T1,TT)) )**(1/TRNUM)-1)*100;
 YFXA(F,XC,TT)             $SUM(T1, YFX(F,XC,T1,TT))           = ((SUM(TRN, YFX(F,XC,TRN,TT))          / SUM(T1, YFX(F,XC,T1,TT))          )**(1/TRNUM)-1)*100;
 YGXA(XC,TT)               $SUM(T1, YGX(XC,T1,TT))             = ((SUM(TRN, YGX(XC,TRN,TT))            / SUM(T1, YGX(XC,T1,TT))            )**(1/TRNUM)-1)*100;
 YIFXA(INS,F,XC,TT)        $SUM(T1, YIFX(INS,F,XC,T1,TT))      = ((SUM(TRN, YIFX(INS,F,XC,TRN,TT))     / SUM(T1, YIFX(INS,F,XC,T1,TT))     )**(1/TRNUM)-1)*100;
 YIXA(INS,XC,TT)           $SUM(T1, YIX(INS,XC,T1,TT))         = ((SUM(TRN, YIX(INS,XC,TRN,TT))        / SUM(T1, YIX(INS,XC,T1,TT))        )**(1/TRNUM)-1)*100;
 trnsfrxa(INS,ACNT,XC,TT)  $SUM(T1, trnsfrx(INS,ACNT,XC,T1,TT)) = ((SUM(TRN, trnsfrx(INS,ACNT,XC,TRN,TT)) / SUM(T1, trnsfrx(INS,ACNT,XC,T1,TT)))**(1/TRNUM)-1)*100;


*Price indices ------------------------------------------------

 pdwt(C)  = PDD0(C)*QD0(C)/SUM(CP, PDD0(CP)*QD0(CP));
 pwewt(C,RW) = pwe0(C,RW)*QE0(C,RW)/SUM((CP,RWP), pwe0(CP,RWP)*QE0(CP,RWP));
 pwmwt(C,RW) = pwm0(C,RW)*QM0(C,RW)/SUM((CP,RWP), pwm0(CP,RWP)*QM0(CP,RWP));
 pwwt('EXP') = (SUM((CP,RW), pwe0(CP,RW)*QE0(CP,RW))) / (SUM((CP,RW), pwe0(CP,RW)*QE0(CP,RW)) + SUM((CP,RW), pwm0(CP,RW)*QM0(CP,RW)));
 pwwt('IMP') = (SUM((CP,RW), pwm0(CP,RW)*QM0(CP,RW))) / (SUM((CP,RW), pwe0(CP,RW)*QE0(CP,RW)) + SUM((CP,RW), pwm0(CP,RW)*QM0(CP,RW)));

*Macrotab -----------------------------------------------------

*Data from all periods
 MACROTABX('QINVTOT',XC,TC,TT)       = SUM((C,IT), PQ0(C)*QINVX(C,IT,XC,TC,TT));
 MACROTABX('QGTOT',XC,TC,TT)         = SUM(C, PQ0(C)*QGX(C,XC,TC,TT));
 MACROTABX('QHTOT',XC,TC,TT)         = SUM((C,H), PQ0(C)*QHX(C,H,XC,TC,TT));
 MACROTABX('QABSTOT',X,TC,TT)        = MACROTABX('QINVTOT',X,TC,TT) + MACROTABX('QGTOT',X,TC,TT) + MACROTABX('QHTOT',X,TC,TT);
 MACROTABX('QETOT',XC,TC,TT)         = SUM((C,RW), PE0(C,RW)*QEX(C,RW,XC,TC,TT));
 MACROTABX('QMTOT',XC,TC,TT)         = SUM((C,RW), PM0(C,RW)*QMX(C,RW,XC,TC,TT));
 MACROTABX('CPI',XC,TC,TT)           = 100*CPIX(XC,TC,TT);
 MACROTABX('NEXR',XC,TC,TT)          = 100*EXRX(XC,TC,TT);
 MACROTABX('PDIND',XC,TC,TT)         = 100*SUM(C, pdwt(C)*PDDX(C,XC,TC,TT));
 MACROTABX('PWEIND',XC,TC,TT)        = 100*SUM((C,RW), pwex(C,RW,XC,TC,TT)*pwewt(C,RW))/SUM((C,RW), pwe0(C,RW)*pwewt(C,RW));
 MACROTABX('PWMIND',XC,TC,TT)        = 100*SUM((C,RW), pwmx(C,RW,XC,TC,TT)*pwmwt(C,RW))/SUM((C,RW), pwm0(C,RW)*pwmwt(C,RW));
 MACROTABX('TOFT',XC,TC,TT)$MACROTABX('PWMIND',XC,TC,TT) = 100*MACROTABX('PWEIND',XC,TC,TT)/MACROTABX('PWMIND',XC,TC,TT);
 MACROTABX('PWIND',XC,TC,TT)         = pwwt('EXP')*MACROTABX('PWEIND',XC,TC,TT) +  pwwt('IMP')*MACROTABX('PWMIND',XC,TC,TT);
 MACROTABX('REXR',XC,TC,TT)$MACROTABX('PDIND',XC,TC,TT) = MACROTABX('NEXR',XC,TC,TT)*MACROTABX('PWIND',XC,TC,TT)/MACROTABX('PDIND',XC,TC,TT);
 MACROTABX('INVGDP',XC,TC,TT)$GDPTAB1('NOMINAL','GDPMP',XC,TC,TT)    = 100*(SUM((C,IT), PQX(C,XC,TC,TT)*QINVX(C,IT,XC,TC,TT))+SUM(C, PQX(C,XC,TC,TT)*qdst(C)))/GDPTAB1('NOMINAL','GDPMP',XC,TC,TT);
 MACROTABX('PRVSAVGDP',XC,TC,TT)$GDPTAB1('NOMINAL','GDPMP',XC,TC,TT) = 100*SUM(INSDNG, MPSX(INSDNG,XC,TC,TT)*((1-TINSX(INSDNG,XC,TC,TT))*YIX(INSDNG,XC,TC,TT)))/GDPTAB1('NOMINAL','GDPMP',XC,TC,TT);
 MACROTABX('FORSAVGDP',XC,TC,TT)$GDPTAB1('NOMINAL','GDPMP',XC,TC,TT) = 100*EXRX(XC,TC,TT)*FSAVX(XC,TC,TT)/GDPTAB1('NOMINAL','GDPMP',XC,TC,TT);
 MACROTABX('TRDDEFGDP',XC,TC,TT)$GDPTAB1('NOMINAL','GDPMP',XC,TC,TT) = 100*SUM((C,RW), PMX(C,RW,XC,TC,TT)*QMX(C,RW,XC,TC,TT) - PEX(C,RW,XC,TC,TT)*QEX(C,RW,XC,TC,TT))/GDPTAB1('NOMINAL','GDPMP',XC,TC,TT);
 MACROTABX('GOVSAVGDP',XC,TC,TT)$GDPTAB1('NOMINAL','GDPMP',XC,TC,TT) = 100*GSAVX(XC,TC,TT)/GDPTAB1('NOMINAL','GDPMP',XC,TC,TT);
 MACROTABX('IMPTAXGDP',XC,TC,TT)$GDPTAB1('NOMINAL','GDPMP',XC,TC,TT) = 100*SUM((CM,RW), TMX(CM,RW,XC,TC,TT)*PWMX(CM,RW,XC,TC,TT)*QMX(CM,RW,XC,TC,TT)*EXRX(XC,TC,TT))/GDPTAB1('NOMINAL','GDPMP',XC,TC,TT);
 MACROTABX('DIRTAXGDP',XC,TC,TT)$GDPTAB1('NOMINAL','GDPMP',XC,TC,TT) = 100*(SUM(INSDNG, TINSX(INSDNG,XC,TC,TT)*YIX(INSDNG,XC,TC,TT)) + SUM(F, tf(F)*YFX(F,XC,TC,TT)))/GDPTAB1('NOMINAL','GDPMP',XC,TC,TT);

 MACROTAB(SIND,XC,TT)       = SUM(TRN, MACROTABX(SIND,XC,TRN,TT));
 MACROTAB(SIND,'INIT',TT)   = SUM(T1, MACROTABX(SIND,'BASE',T1,TT));

 MACROTABXA(SINDP,XC,TT)$MACROTAB(SINDP,'INIT',TT) = ((MACROTAB(SINDP,XC,TT)/MACROTAB(SINDP,'INIT',TT))**(1/TRNUM)-1)*100;
 MACROTABXA(SINDDEV,XC,TT)   =  (MACROTAB(SINDDEV,XC,TT) - MACROTAB(SINDDEV,'INIT',TT))/TRNUM;

 MACROTABXY(SINDP,XC,TC,TT)$MACROTABX(SINDP,XC,TC-1,TT)    = (MACROTABX(SINDP,XC,TC,TT)/MACROTABX(SINDP,XC,TC-1,TT)-1)*100;
 MACROTABXY(SINDDEV,XC,TC,TT)$MACROTABX(SINDDEV,XC,TC-1,TT) = MACROTABX(SINDDEV,XC,TC,TT) - MACROTABX(SINDDEV,XC,TC-1,TT);

 MACROTAB(SINDGR,XC,TT)$MACROTAB(SINDGR,'INIT',TT)  = ((MACROTAB(SINDGR,XC,TT)/MACROTAB(SINDGR,'INIT',TT))**(1/TRNUM)-1)*100;
 MACROTAB(SIND,XC,TT)$(SINDP(SIND) AND NOT SINDGR(SIND) AND MACROTAB(SIND,'INIT',TT)) = (MACROTAB(SIND,XC,TT)/MACROTAB(SIND,'INIT',TT)-1)*100;
 MACROTAB(SINDDEV,XC,TT)   =  MACROTAB(SINDDEV,XC,TT) - MACROTAB(SINDDEV,'INIT',TT);

*Tables other than MACROTAB ------------------------------

 QHTAB(H,XC,TT)                    = SUM((TRN,C), PQ0(C)*QHX(C,H,XC,TRN,TT));
 QHTAB(H,'INIT',TT)                = SUM(C, PQ0(C)*QH0(C,H));
 QHTAB('TOTAL',X,TT)               = SUM((TRN,C,H), PQ0(C)*QHX(C,H,X,TRN,TT));
 QHTAB(H,XC,TT)$QHTAB(H,'INIT',TT) = ((QHTAB(H,XC,TT)/QHTAB(H,'INIT',TT))**(1/TRNUM)-1)*100;

 YFTAB(F,XC,TT)                    = SUM((TRN,A,RD), WFX(F,XC,TRN,TT)*WFDISTX(F,A,RD,XC,TRN,TT)*QFX(F,A,RD,XC,TRN,TT));
 YFTAB(F,'INIT',TT)                = SUM((A,RD), WF0(F)*WFDIST0(F,A,RD)*QF0(F,A,RD));
 YFTAB('TOTAL',X,TT)               = SUM(F, YFTAB(F,X,TT));
 YFTAB(F,XC,TT)$YFTAB(F,'INIT',TT) = ((YFTAB(F,XC,TT)/YFTAB(F,'INIT',TT))**(1/TRNUM)-1)*100;

 QATAB(A,XC,TT)                    = SUM(TRN, PA0(A)*QAX(A,XC,TRN,TT));
 QATAB(A,'INIT',TT)                = PA0(A)*QA0(A);
 QATAB('TOTAL',X,TT)               = SUM((A,TRN), PA0(A)*QAX(A,X,TRN,TT));
 QATAB(A,XC,TT)$QATAB(A,'INIT',TT) = ((QATAB(A,XC,TT)/QATAB(A,'INIT',TT))**(1/TRNUM)-1)*100;

 PATAB(A,XC,TT)                    = SUM(TRN, PAX(A,XC,TRN,TT));
 PATAB(A,'INIT',TT)                = PA0(A);
 PATAB(A,XC,TT)$PATAB(A,'INIT',TT) = ((PATAB(A,XC,TT)/PATAB(A,'INIT',TT))**(1/TRNUM)-1)*100;


*CA add government revenue tablulations
 GOVTAB('DIRTAX',XC,TC,TT) = SUM(INSDNG, TINSX(INSDNG,XC,TC,TT)*YIX(INSDNG,XC,TC,TT));
 GOVTAB('FACTAX',XC,TC,TT) = SUM(F, tfx(F,XC,TC,TT)*YFX(F,XC,TC,TT)) ;
 GOVTAB('FACINC',XC,TC,TT) = SUM(F, YIFX('GOV',F,XC,TC,TT));
 GOVTAB('TRNSFR',XC,TC,TT) =  trnsfrx('GOV','ROW',XC,TC,TT)*EXRX(XC,TC,TT);
 GOVTAB('ACTTAX',XC,TC,TT) = SUM((RD,A), TAX(A,RD,XC,TC,TT)*PARX(A,RD,XC,TC,TT)*QARX(A,RD,XC,TC,TT))  ;
 GOVTAB('VATAX',XC,TC,TT)  = SUM((A,RD), TVAX(A,RD,XC,TC,TT)*PVAX(A,RD,XC,TC,TT)*QVAX(A,RD,XC,TC,TT)) ;
 GOVTAB('IMPTAX',XC,TC,TT) = SUM((RW,CM), TMX(CM,RW,XC,TC,TT)*QMX(CM,RW,XC,TC,TT)*PWMX(CM,RW,XC,TC,TT))*EXRX(XC,TC,TT);
 GOVTAB('EXPTAX',XC,TC,TT) = SUM((RW,CE), te(CE,RW)*QEX(CE,RW,XC,TC,TT)*PWEX(CE,RW,XC,TC,TT))*EXRX(XC,TC,TT);
 GOVTAB('COMTAX',XC,TC,TT) = SUM(C, TQX(C,XC,TC,TT)*PQX(C,XC,TC,TT)*QQX(C,XC,TC,TT))  ;
 GOVTAB('CO2D',XC,TC,TT)   = SUM(C, tco2dx(XC,TC,TT)*co2cx(C,XC,TC,TT)*QQX(C,XC,TC,TT)) ;
 GOVTAB('CO2M',XC,TC,TT)   = SUM((C,RW), tco2mx(XC,TC,TT)*co2mx(C,RW,XC,TC,TT)*QMX(C,RW,XC,TC,TT)) ;
 GOVTAB('CO2E',XC,TC,TT)   =-SUM((C,RW), tco2ex(XC,TC,TT)*co2ex(C,RW,XC,TC,TT)*QEX(C,RW,XC,TC,TT))  ;
 GOVTAB('UTAX',XC,TC,TT)   = SUM((C,A,RD), tui(C,A,RD)*PQX(C,XC,TC,TT)*QINTAX(A,RD,XC,TC,TT)*icax(C,A,RD,XC,TC,TT))
                              + SUM((C,H), tuh(C,H)*PQX(C,XC,TC,TT)*QHX(C,H,XC,TC,TT)) ;

  GOVTAB('TOTAL',XC,TC,TT) =   GOVTAB('DIRTAX',XC,TC,TT)
    +  GOVTAB('FACTAX',XC,TC,TT)
    +  GOVTAB('FACINC',XC,TC,TT)
    +  GOVTAB('TRNSFR',XC,TC,TT)
    +  GOVTAB('ACTTAX',XC,TC,TT)
    +  GOVTAB('VATAX',XC,TC,TT)
    +  GOVTAB('IMPTAX',XC,TC,TT)
    +  GOVTAB('EXPTAX',XC,TC,TT)
    +  GOVTAB('COMTAX',XC,TC,TT)
    +  GOVTAB('CO2D',XC,TC,TT)
    +  GOVTAB('CO2M',XC,TC,TT)
    +  GOVTAB('CO2E',XC,TC,TT)
    +  GOVTAB('UTAX',XC,TC,TT) ;

*Checking model solutions for errors ---------------------
$ontext
*If error in the negative price/quantity segment, one or more solution prices
*or quantities are negative.
 NEGPWARN(C,XC,TC,TT)$(
  ((PDDX(C,XC,TC,TT) LT 0) OR (PDSX(C,XC,TC,TT) LT 0) OR
  (SUM(RW, PEX(C,RW,XC,TC,TT)) LT 0) OR (SUM(RW, PMX(C,RW,XC,TC,TT)) LT 0)  OR
  (PQX(C,XC,TC,TT) LT 0)  OR (SUM(RW, PWEX(C,RW,XC,TC,TT)) LT 0) OR
  (SUM(RW, PWMX(C,RW,XC,TC,TT)) LT 0) OR (PXX(C,XC,TC,TT) LT 0)  OR
  (SMIN(A, PXACX(A,C,XC,TC,TT)) LT 0) )) = 1/0;

 NEGQWARN(C,XC,TC,TT)$(
  ((QDX(C,XC,TC,TT) LT 0) OR
  (SUM(RW, QMX(C,RW,XC,TC,TT)) LT 0) OR (QQX(C,XC,TC,TT) LT 0) OR
  (SUM(RW, QEX(C,RW,XC,TC,TT)) LT 0) OR (QXX(C,XC,TC,TT) LT 0) OR
  (SMIN(A, QXACX(A,C,XC,TC,TT)) LT 0) )) = 1/0;

 NEGWFWARN(F,XC,TC,TT)$(((WFX(F,XC,TC,TT) LT 0) OR (SMIN((RD,A), WFDISTX(F,A,RD,XC,TC,TT)) LT 0))) = 1/0;

 NEGQFWARN(F,XC,TC,TT)$(((QFSX(F,XC,TC,TT) LT 0) OR (SMIN((RD,A), QFX(F,A,RD,XC,TC,TT)) LT 0))) = 1/0;

*If error here, check displays of GDPGAP to find it.
 GAPWARN(XC,TC,TT)$((GDPGAP(XC,TC,TT) GT 1E-3)) = 1/0;

*If error here, check displays of SAMBUDGAP for hints about the source
 WALRASWARN(XC,TC,TT)$((ABS(WALRASX(XC,TC,TT)) GT 1E-3)) = 1/0;

*If error here, check displays of SAMBUDGAP and GDPGAP to find it. Also check WALRASX.
*Conditional printing of error check
 DISPLAY$(SMAX((XC,TC,TT), ABS(GDPGAP(XC,TC,TT))) GT 1E-3)       GDPGAP ;

*Fatal error check here. Not really necessary to be fatal.
 GAPWARNING(XC,TC,TT)  = ABS(GDPGAP(XC,TC,TT))  ;
 DISPLAY$(SMAX((XC,TC,TT), GAPWARNING(XC,TC,TT)) GT 1E-3) GAPWARNING ;
 GAPWARNING(XC,TC,TT)$((GAPWARNING(XC,TC,TT) GT 1E-3)) = 1/0 ;

*Error checking on Walras variable and conditional printing
 DISPLAY$(SMAX((XC,TC,TT), ABS(WALRASX(XC,TC,TT))) GT 1E-4) WALRASX ;

$offtext
*-----------------------------------------------------------------------------
*Activities based tables (GDP at factor cost)
*-----------------------------------------------------------------------------

*GDP at factor cost
 GDPX(AR,XC,TC,TT)  = SUM((A,RD)$ACTAGG(A,RD,AR), PVA0(A,RD)*QVAX(A,RD,XC,TC,TT));

*Average annual growth in GDP at factor cost
 GDPXA(AR,XC,TT)$SUM(TR1, GDPX(AR,XC,TR1,TT)) = ((SUM(TRN, GDPX(AR,XC,TRN,TT))/SUM(TR1, GDPX(AR,XC,TR1,TT)))**(1/TRNUM)-1)*100;

*Year-on-year growth in GDP at factor cost
 GDPXY(AR,XC,TC,TT)$GDPX(AR,XC,TC-1,TT) = (GDPX(AR,XC,TC,TT)/GDPX(AR,XC,TC-1,TT)-1)*100;

*Share of GDP at factor cost
 GDPXS(AR,XC,TC,TT)$SUM(ARP$(NOT ARA(ARP)), GDPX(ARP,XC,TC,TT)) = GDPX(AR,XC,TC,TT)/SUM(ARP$(NOT ARA(ARP)), GDPX(ARP,XC,TC,TT))*100;


*-----------------------------------------------------------------------------
*GDP at market prices (demand-side or expenditures)
*-----------------------------------------------------------------------------

*Levels
 GDPDX('ABSORP',XC,TC,TT)   = GDPTAB1('REAL','ABSORP',XC,TC,TT);
 GDPDX('PRVCON',XC,TC,TT)   = GDPTAB1('REAL','PRVCON',XC,TC,TT);
 GDPDX('FIXINV',XC,TC,TT)   = GDPTAB1('REAL','FIXINV',XC,TC,TT);
 GDPDX('DSTOCK',XC,TC,TT)   = GDPTAB1('REAL','DSTOCK',XC,TC,TT);
 GDPDX('GOVCON',XC,TC,TT)   = GDPTAB1('REAL','GOVCON',XC,TC,TT);
 GDPDX('EXPORTS',XC,TC,TT)  = GDPTAB1('REAL','EXPORTS',XC,TC,TT);
 GDPDX('IMPORTS',XC,TC,TT)  = GDPTAB1('REAL','IMPORTS',XC,TC,TT);
 GDPDX('GDPMP',XC,TC,TT)    = GDPTAB1('REAL','GDPMP',XC,TC,TT);
 GDPDX('NETITAX',XC,TC,TT)  = GDPTAB1('REAL','NETITAX',XC,TC,TT);
 GDPDX('GDPFC2',XC,TC,TT)   = GDPTAB1('REAL','GDPFC2',XC,TC,TT);

*Shares
 GDPDXS(IGDP,XC,TC,TT)$GDPDX('GDPMP',XC,TC,TT) =  GDPDX(IGDP,XC,TC,TT)/GDPDX('GDPMP',XC,TC,TT)*100;

*Year-on-year growth rates
 GDPDXY(IGDP,XC,TC,TT)$GDPDX(IGDP,XC,TC-1,TT) = (GDPDX(IGDP,XC,TC,TT)/GDPDX(IGDP,XC,TC-1,TT)-1)*100;

*Average annual growth rates
 GDPDXA(IGDP,XC,TT)$SUM(TR1, GDPDX(IGDP,XC,TR1,TT)) = ((SUM(TRN, GDPDX(IGDP,XC,TRN,TT))/SUM(TR1, GDPDX(IGDP,XC,TR1,TT)))**(1/TRNUM)-1)*100;
 GDPDXA(IGDP,'INIT',TT) = SUM(TR1, GDPDX(IGDP,'BASE',TR1,TT));

*-----------------------------------------------------------------------------
*Factors based tables
*-----------------------------------------------------------------------------

*Factor supplies
 FACX(FR,XC,TC,TT)  = SUM((F,A,RD)$FACAGG(F,FR), QFX(F,A,RD,XC,TC,TT));
*Average annual growth in factor supplies
 FACXA(FR,XC,TT)$SUM(TR1, FACX(FR,XC,TR1,TT)) = ((SUM(TRN, FACX(FR,XC,TRN,TT))/SUM(TR1, FACX(FR,XC,TR1,TT)))**(1/TRNUM)-1)*100;
 FACXA(FR,'INIT',TT) = SUM(TR1, FACX(FR,'BASE',TR1,TT));
*Year-on-year growth in factor supplies
 FACXY(FR,XC,TC,TT)$FACX(FR,XC,TC-1,TT) = (FACX(FR,XC,TC,TT)/FACX(FR,XC,TC-1,TT)-1)*100;
 FACXY(FR,XC,T1,TT) = FACX(FR,'BASE',T1,TT);

*Factor returns
 FACRX(FR,XC,TC,TT)$SUM(F$FACAGG(F,FR), QFSX(F,XC,TC,TT)) = SUM((A,RD,F)$FACAGG(F,FR), QFX(F,A,RD,XC,TC,TT)*WFDISTX(F,A,RD,XC,TC,TT)*WFX(F,XC,TC,TT)) / SUM(F$FACAGG(F,FR), QFSX(F,XC,TC,TT)) / (CPIX(XC,TC,TT)/CPI0);
*Average annual growth in factor returns
 FACRXA(FR,XC,TT)$SUM(TR1, FACRX(FR,XC,TR1,TT)) = ((SUM(TRN, FACRX(FR,XC,TRN,TT))/SUM(TR1, FACRX(FR,XC,TR1,TT)))**(1/TRNUM)-1)*100;
 FACRXA(FR,'INIT',TT) = SUM(TR1, FACRX(FR,'BASE',TR1,TT));
*year-on-year growth in factor returns
 FACRXY(FR,XC,TC,TT)$FACRX(FR,XC,TC-1,TT) = (FACRX(FR,XC,TC,TT)/FACRX(FR,XC,TC-1,TT)-1)*100;
 FACRXY(FR,XC,TR1,TT) = FACRX(FR,'BASE',TR1,TT);


*-----------------------------------------------------------------------------
*Household based tables
*-----------------------------------------------------------------------------

*Household population
 POPX(HR,XC,TC,TT) = SUM(H$HHDAGG(H,HR), HPOPX(H,XC,TC,TT));
*Average annual household population growth
 POPXA(HR,XC,TT)$SUM(TR1, POPX(HR,XC,TR1,TT)) = ((SUM(TRN, POPX(HR,XC,TRN,TT))/SUM(TR1, POPX(HR,XC,TR1,TT)))**(1/TRNUM)-1)*100;
*Year-on-year household population growth
 POPXY(HR,XC,TC,TT)$POPX(HR,XC,TC-1,TT) = (POPX(HR,XC,TC,TT)/POPX(HR,XC,TC-1,TT)-1)*100;

*Household real consumption
 HHDCX(HR,XC,TC,TT) = SUM((H,C)$HHDAGG(H,HR), PQ0(C)*QHX(C,H,XC,TC,TT)) + SUM((H,A)$HHDAGG(H,HR), PA0(A)*QHAX(A,H,XC,TC,TT));
*Average annual growth in household real consumption
 HHDCXA(HR,XC,TT)$SUM(TR1, HHDCX(HR,XC,TR1,TT)) = ((SUM(TRN, HHDCX(HR,XC,TRN,TT))/SUM(TR1, HHDCX(HR,XC,TR1,TT)))**(1/TRNUM)-1)*100;
 HHDCXA(HR,'INIT',TT) = SUM(TR1, HHDCX(HR,'BASE',TR1,TT));
*Year-on-year growth in household real consumption
 HHDCXY(HR,XC,TC,TT)$HHDCX(HR,XC,TC-1,TT) = (HHDCX(HR,XC,TC,TT)/HHDCX(HR,XC,TC-1,TT)-1)*100;
 HHDCXY(HR,XC,T1,TT) = HHDCX(HR,'BASE',T1,TT);

*Household real per capita consumption
 HHDPCX(HR,XC,TC,TT)$POPX(HR,XC,TC,TT) = HHDCX(HR,XC,TC,TT) / POPX(HR,XC,TC,TT) * 1000;
*Average annual growth in per capita household real consumption
 HHDPCXA(HR,XC,TT)$SUM(TR1, HHDPCX(HR,XC,TR1,TT)) = ((SUM(TRN, HHDPCX(HR,XC,TRN,TT))/SUM(TR1, HHDPCX(HR,XC,TR1,TT)))**(1/TRNUM)-1)*100;
 HHDPCXA(HR,'INIT',TT) = SUM(TR1, HHDPCX(HR,'BASE',TR1,TT));
*Year-on-year growth in per capita household real consumption
 HHDPCXY(HR,XC,TC,TT)$HHDPCX(HR,XC,TC-1,TT) = (HHDPCX(HR,XC,TC,TT)/HHDPCX(HR,XC,TC-1,TT)-1)*100;
 HHDPCXY(HR,XC,T1,TT) = HHDPCX(HR,'BASE',T1,TT);


*Base year household consumption patterns
 HHDBC(AR,HR,TT)$SUM((H,C)$HHDAGG(H,HR), SAM(C,H)) = SUM((H,C)$(COMAGG(C,AR) AND HHDAGG(H,HR)), SAM(C,H)) / SUM((H,C)$HHDAGG(H,HR), SAM(C,H))*100;

*Base year household income patterns
 HHDBY(HR,FR,TT) = SUM((H,F)$(FACAGG(F,FR) AND HHDAGG(H,HR)), SAM(H,F)) / SUM((ACNT,H)$HHDAGG(H,HR), SAM(H,ACNT))*100;
 HHDBY(HR,'hENT',TT) = SUM((EN,H)$HHDAGG(H,HR), SAM(H,EN)) / SUM((ACNT,H)$HHDAGG(H,HR), SAM(H,ACNT))*100;
 HHDBY(HR,'hGOV',TT) = SUM(H$HHDAGG(H,HR), SAM(H,'GOV')) / SUM((ACNT,H)$HHDAGG(H,HR), SAM(H,ACNT))*100;
 HHDBY(HR,'hROW',TT) = SUM(H$HHDAGG(H,HR), SAM(H,'ROW')) / SUM((ACNT,H)$HHDAGG(H,HR), SAM(H,ACNT))*100;
 HHDBY(HR,'hTOT',TT) = HHDBY(HR,'xLABOR',TT) + HHDBY(HR,'xCAPITAL',TT) + HHDBY(HR,'xLAND',TT) + HHDBY(HR,'hENT',TT) + HHDBY(HR,'hGOV',TT) + HHDBY(HR,'hROW',TT);

*-----------------------------------------------------------------------------
*Foreign trade tables
*-----------------------------------------------------------------------------

*Real imports and exports
 TREX(AR,XC,TC,TT) = SUM((C,RW)$COMAGG(C,AR), pwe0(C,RW)*EXR0*QEX(C,RW,XC,TC,TT));
 TRMX(AR,XC,TC,TT) = SUM((C,RW)$COMAGG(C,AR), pwm0(C,RW)*EXR0*QMX(C,RW,XC,TC,TT));
 NETX(AR,XC,TC,TT) = TREX(AR,XC,TC,TT) - TRMX(AR,XC,TC,TT);

*Average annual growth in real imports and exports
 TREXA(AR,XC,TT)$SUM(TR1, TREX(AR,XC,TR1,TT)) = ((SUM(TRN, TREX(AR,XC,TRN,TT))/SUM(TR1, TREX(AR,XC,TR1,TT)))**(1/TRNUM)-1)*100;
 TRMXA(AR,XC,TT)$SUM(TR1, TRMX(AR,XC,TR1,TT)) = ((SUM(TRN, TRMX(AR,XC,TRN,TT))/SUM(TR1, TRMX(AR,XC,TR1,TT)))**(1/TRNUM)-1)*100;
 TREXA(AR,'INIT',TT)$SUM(TR1, TREX('xGDP','BASE',TR1,TT)) = SUM(TR1, TREX(AR,'BASE',TR1,TT))/SUM(TR1, TREX('xGDP','BASE',TR1,TT))*100;
 TRMXA(AR,'INIT',TT)$SUM(TR1, TRMX('xGDP','BASE',TR1,TT)) = SUM(TR1, TRMX(AR,'BASE',TR1,TT))/SUM(TR1, TRMX('xGDP','BASE',TR1,TT))*100;

*Year-on-year growth in real imports and exports
 TREXY(AR,XC,TC,TT)$TREX(AR,XC,TC-1,TT) = (TREX(AR,XC,TC,TT)/TREX(AR,XC,TC-1,TT)-1)*100;
 TRMXY(AR,XC,TC,TT)$TRMX(AR,XC,TC-1,TT) = (TRMX(AR,XC,TC,TT)/TRMX(AR,XC,TC-1,TT)-1)*100;

*-----------------------------------------------------------------------------
*Structural base table with aggregates
*-----------------------------------------------------------------------------

 STRUCBASE2(AR,'VAshr')  = SUM((TT1,TR1), GDPXS(AR,'BASE',TR1,TT1));
 STRUCBASE2(AR,'PRDshr') = SUM((RD,A)$ACTAGG(A,RD,AR), PAR0(A,RD)*(1-tva0(A,RD))*QAR0(A,RD)) / SUM((RD,A), PAR0(A,RD)*(1-tva0(A,RD))*QAR0(A,RD)) * 100;
 STRUCBASE2(AR,'EMPshr') = SUM((FLAB,A,RD)$ACTAGG(A,RD,AR), QF0(FLAB,A,RD)) / SUM((FLAB,A,RD), QF0(FLAB,A,RD)) * 100;
 STRUCBASE2(AR,'EXPshr') = SUM((RW,C)$COMAGG(C,AR), pwe0(C,RW)*EXR0*QE0(C,RW)) / SUM((RW,C), pwe0(C,RW)*EXR0*QE0(C,RW)) * 100;
 STRUCBASE2(AR,'IMPshr') = SUM((RW,C)$COMAGG(C,AR), pwm0(C,RW)*EXR0*QM0(C,RW)) / SUM((RW,C), pwm0(C,RW)*EXR0*QM0(C,RW)) * 100;

 STRUCBASE2(AR,'EXP-OUTshr')$SUM(C$COMAGG(C,AR), PX0(C)*QX0(C)) = SUM((RW,C)$COMAGG(C,AR), PE0(C,RW)*EXR0*QE0(C,RW)) / SUM(C$COMAGG(C,AR), PX0(C)*QX0(C)) * 100;
 STRUCBASE2(AR,'IMP-DEMshr')$SUM(C$COMAGG(C,AR), PQ0(C)*(1-tq0(C))*QQ0(C)) = SUM((RW,C)$COMAGG(C,AR), PM0(C,RW)*EXR0*QM0(C,RW)) / SUM(C$COMAGG(C,AR), PQ0(C)*(1-tq0(C))*QQ0(C)) * 100;

 STRUCBASE2('xGDP','VAshr')  = SUM(AR$(NOT ARA(AR)), STRUCBASE2(AR,'VAshr') );
 STRUCBASE2('xGDP','PRDshr') = SUM(AR$(NOT ARA(AR)), STRUCBASE2(AR,'PRDshr'));
 STRUCBASE2('xGDP','EMPshr') = SUM(AR$(NOT ARA(AR)), STRUCBASE2(AR,'EMPshr'));
 STRUCBASE2('xGDP','EXPshr') = SUM(AR$(NOT ARA(AR)), STRUCBASE2(AR,'EXPshr'));
 STRUCBASE2('xGDP','IMPshr') = SUM(AR$(NOT ARA(AR)), STRUCBASE2(AR,'IMPshr'));

 STRUCBASE2('xGDP','EXP-OUTshr')$SUM(C, PX0(C)*QX0(C)) = SUM((RW,C), PE0(C,RW)*EXR0*QE0(C,RW)) / SUM(C, PX0(C)*QX0(C)) * 100;
 STRUCBASE2('xGDP','IMP-DEMshr')$SUM(C, PQ0(C)*(1-tq0(C))*QQ0(C)) = SUM((RW,C), PM0(C,RW)*EXR0*QM0(C,RW)) / SUM(C, PQ0(C)*(1-tq0(C))*QQ0(C)) * 100;
$ontext
*-----------------------------------------------------------------------------
*National emissions decomposition table
*-----------------------------------------------------------------------------

*Primary fuels (coal, crude oil and natural gas)
* CO2TAB('P-QQ','eCOAL',XC,TC) = SUM(TTRN, QQX('CCOAL-DIS',XC,TC,TTRN)+QQX('CCOAL-LOW',XC,TC,TTRN)+QQX('CCOAL-HGH',XC,TC,TTRN));
 CO2TAB('P-QQ','eCOAL',XC,TC) = SUM(TTRN, QQX('CCOAL-LOW',XC,TC,TTRN)+QQX('CCOAL-HGH',XC,TC,TTRN));
 CO2TAB('P-QQ','eCOIL',XC,TC) = SUM(TTRN, QQX('CCOIL',XC,TC,TTRN));
 CO2TAB('P-QQ','eNGAS',XC,TC) = SUM(TTRN, QQX('CNGAS',XC,TC,TTRN));
*CO2 emissions coefficients
*(mt CO2eq per mt coal, mt CO2eq per mt oil, mt CO2eq per TJ natural gas)
 CO2TAB('P-EC','eCOAL',XC,TC) =
*         SUM(TTRN, QQX('CCOAL-DIS',XC,TC,TTRN)/CO2TAB('P-QQ','eCOAL',XC,TC)*CO2CSIM('CCOAL-DIS',XC,TC)
         SUM(TTRN, QQX('CCOAL-LOW',XC,TC,TTRN)/CO2TAB('P-QQ','eCOAL',XC,TC)*CO2CSIM('CCOAL-LOW',XC,TC)
         + QQX('CCOAL-HGH',XC,TC,TTRN)/CO2TAB('P-QQ','eCOAL',XC,TC)*CO2CSIM('CCOAL-HGH',XC,TC));
 CO2TAB('P-EC','eCOIL',XC,TC) = CO2CSIM('CCOIL',XC,TC);
 CO2TAB('P-EC','eNGAS',XC,TC) = CO2CSIM('CNGAS',XC,TC);
*CO2 emissions (reference approach)
 CO2TAB('P-CO2',CO2AC,XC,TC) = CO2TAB('P-QQ',CO2AC,XC,TC) * CO2TAB('P-EC',CO2AC,XC,TC);
 CO2TAB('P-CO2','eTOTAL',XC,TC) = SUM(CO2AC, CO2TAB('P-CO2',CO2AC,XC,TC));

*Direct use of primary fuels
* CO2TAB('D-COAL-QQ',CO2AC,XC,TC) = SUM((TTRN,A,C)$(MCO2(CO2AC,C) AND MAC(A,C)), QINTX('CCOAL-DIS',A,XC,TC,TTRN)+QINTX('CCOAL-LOW',A,XC,TC,TTRN)+QINTX('CCOAL-HGH',A,XC,TC,TTRN));
 CO2TAB('D-COAL-QQ',CO2AC,XC,TC) = SUM((TTRN,A,C)$(MCO2(CO2AC,C) AND MAC(A,C)), QINTX('CCOAL-LOW',A,XC,TC,TTRN)+QINTX('CCOAL-HGH',A,XC,TC,TTRN));
 CO2TAB('D-COIL-QQ',CO2AC,XC,TC) = SUM((TTRN,A,C)$(MCO2(CO2AC,C) AND MAC(A,C)), QINTX('CCOIL',A,XC,TC,TTRN));
 CO2TAB('D-NGAS-QQ',CO2AC,XC,TC) = SUM((TTRN,A,C)$(MCO2(CO2AC,C) AND MAC(A,C)), QINTX('CNGAS',A,XC,TC,TTRN));
* CO2TAB('D-COAL-QQ','eHHD',XC,TC) = SUM((TTRN,H), QHX('CCOAL-DIS',H,XC,TC,TTRN)+QHX('CCOAL-LOW',H,XC,TC,TTRN)+QHX('CCOAL-HGH',H,XC,TC,TTRN));
 CO2TAB('D-COAL-QQ','eHHD',XC,TC) = SUM((TTRN,H), QHX('CCOAL-LOW',H,XC,TC,TTRN)+QHX('CCOAL-HGH',H,XC,TC,TTRN));
 CO2TAB('D-COIL-QQ','eHHD',XC,TC) = SUM((TTRN,H), QHX('CCOIL',H,XC,TC,TTRN));
 CO2TAB('D-NGAS-QQ','eHHD',XC,TC) = SUM((TTRN,H), QHX('CNGAS',H,XC,TC,TTRN));
* CO2TAB('D-COAL-QQ','eS-I',XC,TC) = SUM((TTRN,IT), QINVX('CCOAL-DIS',IT,XC,TC,TTRN)+QINVX('CCOAL-LOW',IT,XC,TC,TTRN)+QINVX('CCOAL-HGH',IT,XC,TC,TTRN));
 CO2TAB('D-COAL-QQ','eS-I',XC,TC) = SUM((TTRN,IT), QINVX('CCOAL-LOW',IT,XC,TC,TTRN)+QINVX('CCOAL-HGH',IT,XC,TC,TTRN));
 CO2TAB('D-COIL-QQ','eS-I',XC,TC) = SUM((TTRN,IT), QINVX('CCOIL',IT,XC,TC,TTRN));
 CO2TAB('D-NGAS-QQ','eS-I',XC,TC) = SUM((TTRN,IT), QINVX('CNGAS',IT,XC,TC,TTRN));
* CO2TAB('D-COAL-QQ','eGOV',XC,TC) = SUM(TTRN, QGX('CCOAL-DIS',XC,TC,TTRN)+QGX('CCOAL-LOW',XC,TC,TTRN)+QGX('CCOAL-HGH',XC,TC,TTRN));
 CO2TAB('D-COAL-QQ','eGOV',XC,TC) = SUM(TTRN, QGX('CCOAL-LOW',XC,TC,TTRN)+QGX('CCOAL-HGH',XC,TC,TTRN));
 CO2TAB('D-COIL-QQ','eGOV',XC,TC) = SUM(TTRN, QGX('CCOIL',XC,TC,TTRN));
 CO2TAB('D-NGAS-QQ','eGOV',XC,TC) = SUM(TTRN, QGX('CNGAS',XC,TC,TTRN));
* CO2TAB('D-COAL-QQ','eDSTK',XC,TC) = qdst('CCOAL-DIS')+qdst('CCOAL-LOW')+qdst('CCOAL-HGH');
 CO2TAB('D-COAL-QQ','eDSTK',XC,TC) = qdst('CCOAL-LOW')+qdst('CCOAL-HGH');
 CO2TAB('D-COIL-QQ','eDSTK',XC,TC) = qdst('CCOIL');
 CO2TAB('D-NGAS-QQ','eDSTK',XC,TC) = qdst('CNGAS');
*Remove transformation sectors from industry
 CO2TAB('D-COAL-QQ','eIND',XC,TC) = CO2TAB('D-COAL-QQ','eIND',XC,TC) - CO2TAB('D-COAL-QQ','eELEC',XC,TC) - CO2TAB('D-COAL-QQ','ePETR',XC,TC);
 CO2TAB('D-COIL-QQ','eIND',XC,TC) = CO2TAB('D-COIL-QQ','eIND',XC,TC) - CO2TAB('D-COIL-QQ','eELEC',XC,TC) - CO2TAB('D-COIL-QQ','ePETR',XC,TC);
 CO2TAB('D-NGAS-QQ','eIND',XC,TC) = CO2TAB('D-NGAS-QQ','eIND',XC,TC) - CO2TAB('D-NGAS-QQ','eELEC',XC,TC) - CO2TAB('D-NGAS-QQ','ePETR',XC,TC);
*Totals
 CO2TAB('D-COAL-QQ','eTOTAL',XC,TC) = SUM(CO2AC, CO2TAB('D-COAL-QQ',CO2AC,XC,TC));
 CO2TAB('D-COIL-QQ','eTOTAL',XC,TC) = SUM(CO2AC, CO2TAB('D-COIL-QQ',CO2AC,XC,TC));
 CO2TAB('D-NGAS-QQ','eTOTAL',XC,TC) = SUM(CO2AC, CO2TAB('D-NGAS-QQ',CO2AC,XC,TC));
*CO2 emissions (direct primary fuel use)
 CO2TAB('D-COAL-CO2',CO2AC,XC,TC) = CO2TAB('D-COAL-QQ',CO2AC,XC,TC) * CO2TAB('P-EC','eCOAL',XC,TC);
 CO2TAB('D-COIL-CO2',CO2AC,XC,TC) = CO2TAB('D-COIL-QQ',CO2AC,XC,TC) * CO2TAB('P-EC','eCOIL',XC,TC);
 CO2TAB('D-NGAS-CO2',CO2AC,XC,TC) = CO2TAB('D-NGAS-QQ',CO2AC,XC,TC) * CO2TAB('P-EC','eNGAS',XC,TC);
 CO2TAB('D-TOTL-CO2',CO2AC,XC,TC) = CO2TAB('D-COAL-CO2',CO2AC,XC,TC) + CO2TAB('D-COIL-CO2',CO2AC,XC,TC) + CO2TAB('D-NGAS-CO2',CO2AC,XC,TC);

*Calculate composite emission coefficient per unit of elec or petr (endogenous energy source shares)
 CO2TAB('I-EC','eELEC',XC,TC) = SUM((TTRN,CP), QINTX(CP,'AELEC',XC,TC,TTRN) * CO2CSIM(CP,XC,TC)) / SUM(TTRN, QAX('AELEC',XC,TC,TTRN));
 CO2TAB('I-EC','ePETR',XC,TC) = SUM((TTRN,CP), QINTX(CP,'APETR',XC,TC,TTRN) * CO2CSIM(CP,XC,TC)) / SUM(TTRN, QAX('APETR',XC,TC,TTRN));
*Direct use of secondary fuels
 CO2TAB('I-ELEC-QQ',CO2AC,XC,TC) = SUM((TTRN,A,C)$(MCO2(CO2AC,C) AND MAC(A,C)), QINTX('CELEC',A,XC,TC,TTRN));
 CO2TAB('I-PETR-QQ',CO2AC,XC,TC) = SUM((TTRN,A,C)$(MCO2(CO2AC,C) AND MAC(A,C)), QINTX('CPETR',A,XC,TC,TTRN));
 CO2TAB('I-ELEC-QQ','eHHD',XC,TC) = SUM((TTRN,H), QHX('CELEC',H,XC,TC,TTRN));
 CO2TAB('I-PETR-QQ','eHHD',XC,TC) = SUM((TTRN,H), QHX('CPETR',H,XC,TC,TTRN));
 CO2TAB('I-ELEC-QQ','eS-I',XC,TC) = SUM((TTRN,IT), QINVX('CELEC',IT,XC,TC,TTRN));
 CO2TAB('I-PETR-QQ','eS-I',XC,TC) = SUM((TTRN,IT), QINVX('CPETR',IT,XC,TC,TTRN));
 CO2TAB('I-ELEC-QQ','eGOV',XC,TC) = SUM(TTRN, QGX('CELEC',XC,TC,TTRN));
 CO2TAB('I-PETR-QQ','eGOV',XC,TC) = SUM(TTRN, QGX('CPETR',XC,TC,TTRN));
 CO2TAB('I-ELEC-QQ','eDSTK',XC,TC) = qdst('CELEC');
 CO2TAB('I-PETR-QQ','eDSTK',XC,TC) = qdst('CPETR');
*Remove transformation sectors from industry
 CO2TAB('I-ELEC-QQ','eIND',XC,TC) = CO2TAB('I-ELEC-QQ','eIND',XC,TC) - CO2TAB('I-ELEC-QQ','eELEC',XC,TC) - CO2TAB('I-ELEC-QQ','ePETR',XC,TC);
 CO2TAB('I-PETR-QQ','eIND',XC,TC) = CO2TAB('I-PETR-QQ','eIND',XC,TC) - CO2TAB('I-PETR-QQ','eELEC',XC,TC) - CO2TAB('I-PETR-QQ','ePETR',XC,TC);
*Totals
 CO2TAB('I-ELEC-QQ','eTOTAL',XC,TC) = SUM(CO2AC, CO2TAB('I-ELEC-QQ',CO2AC,XC,TC));
 CO2TAB('I-PETR-QQ','eTOTAL',XC,TC) = SUM(CO2AC, CO2TAB('I-PETR-QQ',CO2AC,XC,TC));
*CO2 emissions (direct primary fuel use)
 CO2TAB('I-ELEC-CO2',CO2AC,XC,TC) = CO2TAB('I-ELEC-QQ',CO2AC,XC,TC) * CO2TAB('I-EC','eELEC',XC,TC);
 CO2TAB('I-PETR-CO2',CO2AC,XC,TC) = CO2TAB('I-PETR-QQ',CO2AC,XC,TC) * CO2TAB('I-EC','ePETR',XC,TC);
 CO2TAB('I-TOTL-CO2',CO2AC,XC,TC) = CO2TAB('I-ELEC-CO2',CO2AC,XC,TC) + CO2TAB('I-PETR-CO2',CO2AC,XC,TC);

*Total direct use of primary fuels plus first round indirect use via secondary fuels
 CO2TAB('D+I-TOTL-CO2',CO2AC,XC,TC) = CO2TAB('I-TOTL-CO2',CO2AC,XC,TC)+CO2TAB('D-TOTL-CO2',CO2AC,XC,TC);
 CO2TAB('D+I-TOTL-CO2','eELEC',XC,TC) = CO2TAB('D+I-TOTL-CO2','eELEC',XC,TC) - CO2TAB('D-TOTL-CO2','eELEC',XC,TC);
 CO2TAB('D+I-TOTL-CO2','ePETR',XC,TC) = CO2TAB('D+I-TOTL-CO2','ePETR',XC,TC) - CO2TAB('D-TOTL-CO2','ePETR',XC,TC);
 CO2TAB('D+I-TOTL-CO2','eTOTAL',XC,TC) = 0;
 CO2TAB('D+I-TOTL-CO2','eTOTAL',XC,TC) = SUM(CO2AC, CO2TAB('D+I-TOTL-CO2',CO2AC,XC,TC));

*Reference to sectoral ratio
 CO2TAB('REF-SEC-RAT','eTOTAL',XC,TC) =  CO2TAB('P-CO2','eTOTAL',XC,TC) /  CO2TAB('D+I-TOTL-CO2','eTOTAL',XC,TC);
$offtext

*fh:020218 - Starred out
$ontext
*-----------------------------------------------------------------------------
* TIMES-SAGE REPORTING
*-----------------------------------------------------------------------------


*Growth sheet -----

*g1 g2: average annual GDP growth rate over reporting period
 tsGDPXA(AR,XC) = SUM(TTRN, GDPXA(AR,XC,TTRN));
 tsGDPXA(AR,'INIT') = SUM((T1,TTRN), GDPXS(AR,'BASE',T1,TTRN));
 tsGDPXA(AR,X)$(NOT tsGDPXA(AR,X)) = 1E-6;

*g3: sectoral source of additional GDP growth
 tsGDPSRC(AR,XC) = (SUM((TRN,TTRN), GDPX(AR,XC,TRN,TTRN))-SUM((TR1,TTRN), GDPX(AR,XC,TR1,TTRN)))
         / (SUM((TRN,TTRN), GDPX('XGDP',XC,TRN,TTRN))-SUM((TR1,TTRN), GDPX('XGDP',XC,TR1,TTRN)))*100;
 tsGDPSRC(AR,X)$(NOT tsGDPSRC(AR,X)) = 1E-6;
 tsGDPSRC(AR,'INIT') = 0;

*g4: total GDP per capita over reporting period
 tsGDPX(TC,XC) = SUM(TTRN, GDPX('XGDP',XC,TC,TTRN));
 tsGDPX(T,X)$(NOT tsGDPX(T,X)) = 1E-6;
 tsGDPX(T,'INIT') = 0;
 tsGDPX(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*Jobs sheet -----

*j1: Total factor supply growth rates over reporting period
 tsFACXA(FR,XC) = SUM(TTRN, FACXA(FR,XC,TTRN));
 tsFACXA(FR,X)$(NOT tsFACXA(FR,X)) = 1E-6;
 tsFACXA(FR,'INIT') = SUM((TR1,TTRN), FACX(FR,'BASE',TR1,TTRN));

*j2: New jobs created over reporting period
 tsNEWJOB(FR,XC) = SUM((TRN,TTRN), FACX(FR,XC,TRN,TTRN))-SUM((TR1,TTRN), FACX(FR,XC,TR1,TTRN));
 tsNEWJOB(FR,X)$(NOT tsNEWJOB(FR,X)) = 1E-6;
 tsNEWJOB(FR,'INIT') = SUM((TR1,TTRN), FACX(FR,'BASE',TR1,TTRN));
 tsNEWJOB(FR,X)$(NOT RFRAC(FR)) = 0;
 tsNEWJOB('ELSECT',XC) = SUM((FLAB,TRN,TTRN), QFX(FLAB,'AELEC','NAT',XC,TRN,TTRN)) - SUM((FLAB,TR1,TTRN), QFX(FLAB,'AELEC','NAT',XC,TR1,TTRN));
 tsNEWJOB('ELSECT',X)$(NOT tsNEWJOB('ELSECT',X)) = 1E-6;
 tsNEWJOB('ELSECT','INIT') = SUM((FLAB,TR1,TTRN), QFX(FLAB,'AELEC','NAT','BASE',TR1,TTRN));

*j3: Average annual growth in factor returns over report period
 tsRTNXA(FR,XC) = SUM(TTRN, FACRXA(FR,XC,TTRN));
 tsRTNXA(FR,X)$(NOT tsRTNXA(FR,X)) = 1E-6;
 tsRTNXA(FR,'INIT') = SUM((TR1,TTRN), FACRX(FR,'BASE',TR1,TTRN))*1000000;

*j4: total labor supply over reporting period
 tsLABX(TC,XC) = SUM(TTRN, FACX('xLABOR',XC,TC,TTRN))/1000;
 tsLABX(T,X)$(NOT tsLABX(T,X)) = 1E-6;
 tsLABX(T,'INIT') = 0;
 tsLABX(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*j5: average low skilled labor wage over reporting period
 tsWLOWX(TC,XC) = SUM(TTRN, FACRX('xLABUNSK',XC,TC,TTRN))*1000;
 tsWLOWX(T,X)$(NOT tsWLOWX(T,X)) = 1E-6;
 tsWLOWX(T,'INIT') = 0;
 tsWLOWX(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*j5: average high skilled labor wage over reporting period
 tsWHGHX(TC,XC) = SUM(TTRN, FACRX('xLABSK',XC,TC,TTRN))*1000;
 tsWHGHX(T,X)$(NOT tsWHGHX(T,X)) = 1E-6;
 tsWHGHX(T,'INIT') = 0;
 tsWHGHX(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*j6: average annual labor employment growth rate over reporting period
 tsJOBSXA(AR,XC)$SUM((FLAB,A,RD,TR1,TTRN)$ACTAGG(A,RD,AR), QFX(FLAB,A,RD,XC,TR1,TTRN))
         = ((SUM((FLAB,A,RD,TRN,TTRN)$ACTAGG(A,RD,AR), QFX(FLAB,A,RD,XC,TRN,TTRN))
         /SUM((FLAB,A,RD,TR1,TTRN)$ACTAGG(A,RD,AR), QFX(FLAB,A,RD,XC,TR1,TTRN)))**(1/TRNUM)-1)*100;
 tsJOBSXA(AR,'INIT') = SUM((FLAB,A,RD,TR1,TTRN)$ACTAGG(A,RD,AR), QFX(FLAB,A,RD,'BASE',TR1,TTRN));
 tsJOBSXA(AR,X)$(NOT tsJOBSXA(AR,X)) = 1E-6;

*Welfare sheet -----

*w1: total population
 tsPOPX(TC,XC) = SUM(TTRN, POPX('xALLHHD',XC,TC,TTRN))/1000;
 tsPOPX(T,X)$(NOT tsPOPX(T,X)) = 1E-6;
 tsPOPX(T,'INIT') = 0;
 tsPOPX(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*w2: Base year household consumption patterns
 tsHHDBC(AR,HR) = SUM(TTRN, HHDBC(AR,HR,TTRN));
 tsHHDBC(AR,HR)$(NOT tsHHDBC(AR,HR)) = 1E-6;

*w3: Base year household income patterns
 tsHHDBY(FR,HR) = SUM(TTRN, HHDBY(HR,FR,TTRN));
 tsHHDBY('hENT',HR) = SUM(TTRN, HHDBY(HR,'hENT',TTRN));
 tsHHDBY('hGOV',HR) = SUM(TTRN, HHDBY(HR,'hGOV',TTRN));
 tsHHDBY('hROW',HR) = SUM(TTRN, HHDBY(HR,'hROW',TTRN));
 tsHHDBY('hTOT',HR) = SUM(TTRN, HHDBY(HR,'hTOT',TTRN));
 tsHHDBY(FR,HR)$(NOT tsHHDBY(FR,HR)) = 1E-6;
 tsHHDBY('hENT',HR)$(NOT tsHHDBY('hENT',HR)) = 1E-6;
 tsHHDBY('hGOV',HR)$(NOT tsHHDBY('hGOV',HR)) = 1E-6;
 tsHHDBY('hROW',HR)$(NOT tsHHDBY('hROW',HR)) = 1E-6;
 tsHHDBY('hTOT',HR)$(NOT tsHHDBY('hTOT',HR)) = 1E-6;

*w4: Average annual growth in per capita household real consumption
 tsHHDPCXA(HR,XC) = SUM(TTRN, HHDPCXA(HR,XC,TTRN));
 tsHHDPCXA(HR,X)$(NOT tsHHDPCXA(HR,X)) = 1E-6;
 tsHHDPCXA(HR,'INIT') = SUM((TR1,TTRN),  HHDPCX(HR,'BASE',TR1,TTRN))*1000;

*w5: Per capita consumption for percentiles 0-50
 tsPC050X(TC,XC) = SUM(TTRN, HHDPCX('xPOOR',XC,TC,TTRN));
 tsPC050X(T,X)$(NOT tsPC050X(T,X)) = 1E-6;
 tsPC050X(T,'INIT') = 0;
 tsPC050X(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*w5: Per capita consumption for percentiles 50-90
 tsPC5090X(TC,XC) = SUM(TTRN, HHDPCX('xNP40',XC,TC,TTRN));
 tsPC5090X(T,X)$(NOT tsPC5090X(T,X)) = 1E-6;
 tsPC5090X(T,'INIT') = 0;
 tsPC5090X(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*w5: Per capita consumption for percentiles 90-100
 tsPC90100X(TC,XC) = SUM(TTRN, HHDPCX('xxHHD-9',XC,TC,TTRN));
 tsPC90100X(T,X)$(NOT tsPC90100X(T,X)) = 1E-6;
 tsPC90100X(T,'INIT') = 0;
 tsPC90100X(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*Electricity sheet -----

*e1: total electricity demand projections (GWh)
 tsELCDEM(TC,XC) = SUM(TTRN, EPROJ('AELEC',XC,TC,TTRN))/3.6;
 tsELCDEM(T,X)$(NOT tsELCDEM(T,X)) = 1E-6;
 tsELCDEM(T,'INIT') = 0;
 tsELCDEM(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*e1: electricity demand at selected reporting periods
 tsELCDTAB(RTAC,XC) = SUM(TTRN, EPROJ('AELEC',XC,RTAC,TTRN))/3.6;
 tsELCDTAB(T,X)$(NOT tsELCDTAB(T,X)) = 1E-6;
 tsELCDTAB(T,X)$(NOT RTAC(T)) = 0;
 tsELCDTAB(T,'INIT') = 0;
 tsELCDTAB(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*e2: average electricity price projections (cents per KWh)
 tsELCPRC(TC,XC) = SUM(TTRN, PQX('CELEC',XC,TC,TTRN))*100;
 tsELCPRC(T,X)$(NOT tsELCPRC(T,X)) = 1E-6;
 tsELCPRC(T,'INIT') = 0;
 tsELCPRC(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*e2: average electricity price at selected reporting periods
 tsELCPTAB(RTAC,XC) = SUM(TTRN, PQX('CELEC',XC,RTAC,TTRN))*100;
 tsELCPTAB(T,X)$(NOT tsELCPTAB(T,X)) = 1E-6;
 tsELCPTAB(T,X)$(NOT RTAC(T)) = 0;
 tsELCPTAB(T,'INIT') = 0;
 tsELCPTAB(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*e3b: electricity supply composition for base
 tsELCSUP(XC,RD,TC) = SUM(TTRN, EFLOW(RD,XC,TC,TTRN));
 tsELCSUP(X,RD,T)$(NOT tsELCSUP(X,RD,T)) = 1E-6;
 tsELCSUP(X,'NAT',T) = 0;
 tsELCSUP(X,'OIL',T) = 0;
 tsELCSUP(X,'CTL',T) = 0;
 tsELCSUP(X,'GTL',T) = 0;
 tsELCSUP(X,'BIO',T) = 0;
 tsELCSUP('INIT',RD,T) = 0;
 tsELCSUP(X,RD,T)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*e4: electricity demand growth to GDP growth elasticity
 tsELCGDP('GDPXA',XC)  = tsGDPXA('XGDP',XC);
 tsELCGDP('ELCDEM',XC) = ((SUM((TRN,TTRN), EPROJ('AELEC',XC,TRN,TTRN))/SUM((TR1,TTRN), EPROJ('AELEC',XC,TR1,TTRN)))**(1/TRNUM)-1)*100;
 tsELCGDP('EGELAS',XC)$tsELCGDP('GDPXA',XC) = tsELCGDP('ELCDEM',XC)/tsELCGDP('GDPXA',XC);
 tsELCGDP(EGEAC,X)$(NOT tsELCGDP(EGEAC,X)) = 1E-6;
 tsELCGDP(EGEAC,'INIT') = 0;

*e5: electricity build plan annual investment cost
 tsINVCST(T,XC) = SUM(TTRN, EINVCOST(XC,T,TTRN));
 tsINVCST(T,X)$(NOT tsINVCST(T,X)) = 1E-6;
 tsINVCST(T,'INIT') = 0;
 tsINVCST(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*e6: build plan debt (accumulated investment cost less interest payments)
 tsEDEBT(T,XC) = SUM(TTRN, DEBTX(XC,T,TTRN));
 tsEDEBT(T,X)$(NOT tsEDEBT(T,X)) = 1E-6;
 tsEDEBT(T,'INIT') = 0;
 tsEDEBT(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*e7: electricity investment interest payments to GDP ratio
 tsINTGDP(T,XC)$SUM(TTRN, GDPDX('GDPMP',XC,T,TTRN)) = SUM(TTRN, irate*DEBTX(XC,T,TTRN))/SUM(TTRN, GDPDX('GDPMP',XC,T,TTRN));
 tsINTGDP(T,X)$(NOT tsINTGDP(T,X)) = 1E-6;
 tsINTGDP(T,'INIT') = 0;
 tsINTGDP(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*Emissions sheet -----

*m1: total emissions reference approach
 tsCO2REF(T,XC) = CO2TAB('P-CO2','eTOTAL',XC,T);
 tsCO2REF(T,X)$(NOT tsCO2REF(T,X)) = 1E-6;
 tsCO2REF(T,'INIT') = 0;
 tsCO2REF(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*m2: total emissions sectoral approach
 tsCO2SEC(T,XC) = CO2TAB('D+I-TOTL-CO2','eTOTAL',XC,T);
 tsCO2SEC(T,X)$(NOT tsCO2SEC(T,X)) = 1E-6;
 tsCO2SEC(T,'INIT') = 0;
 tsCO2SEC(T,X)$(ORD(T) LT SUM(TCP$T1(TCP), ORD(TCP))) = 0;

*m3: beginning and final year emissions sources during reporting period
 tsEMSRC(CO2AC,XC) = SUM(TRN, CO2TAB('D-TOTL-CO2',CO2AC,XC,TRN));
 tsEMSRC(CO2AC,'INIT') = SUM(TR1, CO2TAB('D-TOTL-CO2',CO2AC,'BASE',TR1));
 tsEMSRC(CO2AC,X)$(NOT tsEMSRC(CO2AC,X)) = 1E-6;

*m4: total emissions for selected time periods
 tsEMTOT(RTAC,XC) = CO2TAB('D+I-TOTL-CO2','eTOTAL',XC,RTAC);
 tsEMTOT(RTAC,X)$(NOT tsEMTOT(RTAC,X)) = 1E-6;
 tsEMTOT(RTAC,'INIT') = 0;

*m5: electricity sector emissions for selected time periods
 tsEMELC(RTAC,XC) = CO2TAB('D-TOTL-CO2','eELEC',XC,RTAC);
 tsEMELC(RTAC,X)$(NOT tsEMELC(RTAC,X)) = 1E-6;
 tsEMELC(RTAC,'INIT') = 0;
$offtext

*Including the gini coefficient*
 income('xxhhd-0',XC,TC,TT)=YIX('hhd-0',XC,TC,TT);
 income('xxhhd-1',XC,TC,TT)=YIX('hhd-1',XC,TC,TT);
 income('xxhhd-2',XC,TC,TT)=YIX('hhd-2',XC,TC,TT);
 income('xxhhd-3',XC,TC,TT)=YIX('hhd-3',XC,TC,TT);
 income('xxhhd-4',XC,TC,TT)=YIX('hhd-4',XC,TC,TT);
 income('xxhhd-5',XC,TC,TT)=YIX('hhd-5',XC,TC,TT);
 income('xxhhd-6',XC,TC,TT)=YIX('hhd-6',XC,TC,TT);
 income('xxhhd-7',XC,TC,TT)=YIX('hhd-7',XC,TC,TT);
 income('xxhhd-8',XC,TC,TT)=YIX('hhd-8',XC,TC,TT);
 income('xxhhd-9',XC,TC,TT)=YIX('hhd-9',XC,TC,TT);

 rank(hr)=d('rank',hr);
 mean(XC,TC,TT)=sum(hr,income(hr,XC,TC,TT))/10;
 gini(XC,TC,TT)=((10+1)/(10-1))-((2/(10*(10-1)*mean(XC,TC,TT)))*(sum(hr,(rank(hr)*income(hr,XC,TC,TT)))));
