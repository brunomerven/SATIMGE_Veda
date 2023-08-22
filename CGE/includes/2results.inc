 ALPHAVAX(A,RD,XC,TC,TT)  = alphava(A,RD);
 LEOVAX(A,RD,XC,TC,TT)    = leova(A,RD);
 FPRDX(F,A,RD,XC,TC,TT)   = fprd(F,A,RD);
 CPIX(XC,TC,TT)           = CPI.L;
 DEBTX(XC,TC,TT)          = debt;
 DPIX(XC,TC,TT)           = DPI.L;
 DMPSX(XC,TC,TT)          = DMPS.L;
 DTINSX(XC,TC,TT)         = DTINS.L;
 EGX(XC,TC,TT)            = EG.L;
 EHX(H,XC,TC,TT)          = EH.L(H);
 EXRX(XC,TC,TT)           = EXR.L;
 FBORX(XC,TC,TT)          = FBOR.L;
 FSAVX(XC,TC,TT)          = FSAV.L;
 GADJX(XC,TC,TT)          = GADJ.L;
 GOVSHRX(XC,TC,TT)        = GOVSHR.L;
 GSAVX(XC,TC,TT)          = GSAV.L;
 HPOPX(H,XC,TC,TT)        = hpop(H);
 IADJX(IT,XC,TC,TT)       = IADJ.L(IT);
 INVSHRX(XC,TC,TT)        = INVSHR.L;
 MPSX(INS,XC,TC,TT)       = MPS.L(INS);
 MPSADJX(XC,TC,TT)        = MPSADJ.L;
 PAX(A,XC,TC,TT)          = PA.L(A);
 PARX(A,RD,XC,TC,TT)      = PAR.L(A,RD);
 PDDX(C,XC,TC,TT)         = PDD.L(C);
 PDSX(C,XC,TC,TT)         = PDS.L(C);
 PEX(C,RW,XC,TC,TT)       = PE.L(C,RW);
 PINTAX(A,RD,XC,TC,TT)    = PINTA.L(A,RD);
 PMX(C,RW,XC,TC,TT)       = PM.L(C,RW);
 PQX(C,XC,TC,TT)          = PQ.L(C);
 PQHX(C,H,XC,TC,TT)       = PQH.L(C,H);
 PQIX(C,A,RD,XC,TC,TT)    = PQI.L(C,A,RD);
 PVAX(A,RD,XC,TC,TT)      = PVA.L(A,RD);
 PWEX(C,RW,XC,TC,TT)      = PWE.L(C,RW);
 PWMX(C,RW,XC,TC,TT)      = PWM.L(C,RW);
 PXX(C,XC,TC,TT)          = PX.L(C);
 PXACX(A,C,XC,TC,TT)      = PXAC.L(A,C);
 QAX(A,XC,TC,TT)          = QA.L(A);
 QARX(A,RD,XC,TC,TT)      = QAR.L(A,RD);
 QDX(C,XC,TC,TT)          = QD.L(C);
 QEX(C,RW,XC,TC,TT)       = QE.L(C,RW);
 QFX(F,A,RD,XC,TC,TT)     = QF.L(F,A,RD);
 QFSX(F,XC,TC,TT)         = QFS.L(F);
 QFS_FORX(F,XC,TC,TT)     = QFS_FOR.L(F);
 QGX(C,XC,TC,TT)          = QG.L(C);
 QHX(C,H,XC,TC,TT)        = QH.L(C,H);
 QHAX(A,H,XC,TC,TT)       = QHA.L(A,H);
 QINTX(C,A,XC,TC,TT)      = QINT.L(C,A);
 QINTAX(A,RD,XC,TC,TT)    = QINTA.L(A,RD);
 QINVX(C,IT,XC,TC,TT)     = QINV.L(C,IT);
 QMX(C,RW,XC,TC,TT)       = QM.L(C,RW);
 QQX(C,XC,TC,TT)          = QQ.L(C);
 QTX(C,XC,TC,TT)          = QT.L(C);
 QVAX(A,RD,XC,TC,TT)      = QVA.L(A,RD);
 QXX(C,XC,TC,TT)          = QX.L(C);
 QXACX(A,C,XC,TC,TT)      = QXAC.L(A,C);
 TABSX(XC,TC,TT)          = TABS.L;
 THETAX(A,C,RUN,TC)       = theta(A,C);
 TINSX(INS,XC,TC,TT)      = TINS.L(INS);
 TINSADJX(XC,TC,TT)       = TINSADJ.L;
 TAX(A,RD,XC,TC,TT)       = TA.L(A,RD);
 TAADJX(XC,TC,TT)         = TAADJ.L;
 TAPSX(XC,TC,TT)          = TAPS.L;
 TMX(C,RW,XC,TC,TT)       = tm(C,RW);
 TFX(F,XC,TC,TT)          = tf(F);
 TQADJX(XC,TC,TT)         = TQADJ.L;
 TRNADJX(XC,TC,TT)         = TRNADJ.L;
 TQPSX(XC,TC,TT)          = TQPS.L;
 TQX(C,XC,TC,TT)          = TQ.L(C);
 TVAADJX(XC,TC,TT)        = TVAADJ.L;
 TVAPSX(XC,TC,TT)         = TVAPS.L;
 TVAX(A,RD,XC,TC,TT)      = TVA.L(A,RD);
 TRIIX(INS,INSP,XC,TC,TT) = TRII.L(INS,INSP);
 WALRASX(XC,TC,TT)        = WALRAS.L;
* WFX(F,XC,TC,TT)$QFS0(F)  = SUM((A,RD), WF.L(F)*WFDIST.L(F,A,RD)*QF.L(F,A,RD))/SUM((A,RD), QF.L(F,A,RD));
 WFX(F,XC,TC,TT)          = WF.L(F);
 WFDISTX(F,A,RD,XC,TC,TT) = WFDIST.L(F,A,RD);
 YFX(F,XC,TC,TT)          = YF.L(F);
 YGX(XC,TC,TT)            = YG.L;
*fh 01072019
 YG_DTAXX(XC,TC,TT)       = YG_DTAX.L;
 YG_ITAXX(XC,TC,TT)       = YG_ITAX.L;
 YG_NTAXX(XC,TC,TT)       = YG_NTAX.L;

 YIFX(INS,F,XC,TC,TT)     = YIF.L(INS,F);
 YIX(INS,XC,TC,TT)        = YI.L(INS);
 trnsfrx(INS,ACNT,XC,TC,TT) = trnsfr(INS,ACNT);
 QINVKX(XC,TC,TT)         = QINVK;
 NGFCFX(XC,TC,TT)         = NGFCF;

 tco2dx(XC,TC,TT)   = tco2d;
 tco2ex(XC,TC,TT)   = tco2e;
 tco2mx(XC,TC,TT)   = tco2m;
 rtco2ex(XC,TC,TT)  = rtco2e;
 co2cx(C,XC,TC,TT)    = co2c(C);
 co2ex(C,RW,XC,TC,TT) = co2e(C,RW);
 co2mx(C,RW,XC,TC,TT) = co2m(C,RW);

 intax(A,RD,XC,TC,TT)   = inta(A,RD);
 ivax(A,RD,XC,TC,TT)    = iva(A,RD);
 ifax(F,A,RD,XC,TC,TT)  = ifa(F,A,RD);
 icax(C,A,RD,XC,TC,TT)  = ica(C,A,RD);
 betamx(CEGY,H,XC,TC,TT)  = betam(CEGY,H);

 DTUX(C,XC,TC,TT) =  DTU.L(C);

 QFSUPX(FCAP,XC,TC,TT)$(QFS.L(FCAP) LT QFS.UP(FCAP) AND QFS.UP(FCAP)) = QFS.L(FCAP)/QFS.UP(FCAP);
 ALPHAQFX(XC,TC,TT) = ALPHAQF.L;

 SHREINVQINV(XC,TC,TT)$QINVK = EINVQINV(XC,TC,TT)/QINVK;


*bm&tc
  DKAPX(FCAP,A,RD,XC,TC,TT) = DKAP(FCAP,A,RD);
  WFDIST2X(FCAP,A,RD,XC,TC,TT) = WFDIST2(FCAP,A,RD);
  INVSHR2X(FCAP,A,RD,XC,TC,TT) = INVSHR2(FCAP,A,RD);
*fh
  CTA_EMX(A,RUN,XC,TC,TT) = CTA_EM(A);
  CTA_EMRX(A,RUN,XC,TC,TT)= CTA_EMR(A);
  CTA_EMRRX(A,RUN,XC,TC,TT)=CTA_EMRR(A);
*  CTA_EMRRX(A,RUN,XC,TC,'2050')=0;

 deltaqx(c,rw,run,xc,tc,tt)=deltaq(c,rw);
 deltatx(c,rw,run,xc,tc,tt)=deltat(c,rw);
