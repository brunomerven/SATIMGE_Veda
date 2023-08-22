

* EBCHECK - to compare the energy use by SATIM sectors

EBCHECK('SATIM',FS,'IN',C,TC,TT,RUN) = FLOW_IN_FSFuels(FS,C,TC)*(-1);
EBCHECK('SATIM',FS,'OUT',C,TC,TT,RUN) = FLOW_OUT_FSFuels(FS,C,TC);
EBCHECK('SATIM','elec','OUT','celec',TC,TT,RUN) = ELC_FLOW(RUN,TC);

EBCHECK('SATIM','imp','OUT',C,TC,TT,RUN) = EBCHECK('SATIM','imp','OUT',C,TC,TT,RUN)+EBCHECK('SATIM','exp','IN',C,TC,TT,RUN);
EBCHECK('SATIM','exp','IN',C,TC,TT,RUN) = 0;

EBCHECK('eSAGE',FS,'IN',C,TC,TT,RUN)$energy(C) = SUM(A$MFSA(FS,A),QINTX(C,A,XC,TC,TT))*(-1);
*EBCHECK('eSAGE','petr','IN',C,TC,TT,RUN)$UCONV_C_T2C(C) = QINTX(C,'apetr',XC,TC,TT)/UCONV_C_T2C(C)*-1;
*EBCHECK('eSAGE','exp','IN',C,TC,TT,RUN) = QEX(C,'rest',XC,TC,TT)*(-1);

EBCHECK('eSAGE',FS,'OUT',C,TC,TT,RUN)$energy(C) = SUM(A$MFSA(FS,A),QXACX(A,C,XC,TC,TT));
*EBCHECK('eSAGE','petr','IN',C,TC,TT,RUN)$UCONV_C_T2C(C) = QINTX(C,'apetr',XC,TC,TT)/UCONV_C_T2C(C)*-1;
EBCHECK('eSAGE','imp','OUT',C,TC,TT,RUN) = QMX(C,'rest',XC,TC,TT)-QEX(C,'rest',XC,TC,TT);

EBCHECK('SATIM',FH,'IN',C,TC,TT,RUN) = FLOW_IN_FSFuels(FH,C,TC)*(-1);
EBCHECK('eSAGE',FH,'IN',C,TC,TT,RUN)$energy(C) = SUM(H$MFHHT(FH,H,TC),QHX(C,H,XC,TC,TT))*(-1);




EBCHECK(MOD,FSATIM,'CO2',C,TC,TT,RUN) = EBCHECK(MOD,FSATIM,'IN',C,TC,TT,RUN)*CO2OUT(C);

*EIntensity('SATIM',FS,C,TC,TT,RUN)$SFORE_X(FS,TC) = FinalDemand('SATIM',FS,C,TC,TT,RUN)/SFORE_X(FS,TC);
*EIntensity('eSAGE',FS,C,TC,TT,RUN)$SFORE_X(FS,TC) = FinalDemand('eSAGE',FS,C,TC,TT,RUN)/SFORE_X(FS,TC);
*EIntensity('SATIM',FH,C,TC,XC)$TFHPOP_X(FH,TC) = FinalDemand('SATIM',FH,C,TC,XC)/TFHPOP_X(FH,TC);
*EIntensity('eSAGE',FH,C,TC,XC)$TFHPOP_X(FH,TC) = FinalDemand('eSAGE',FH,C,TC,XC)/TFHPOP_X(FH,TC);

*CGE_OUTPUT(A,TC,'GVA',RUN) = QVAX(A,'nat','base',TC,TT);
*CGE_OUTPUT(A,TC,'Employment',RUN) = sum(flab,QFX(flab,A,'nat','base',TC,TT));


*   QType quantitity types /qq, qd, qint, qe, qm, qh, qx, qxac/

BalanceCheck('qq',C,TC,RUN) = QQX(C,XC,TC,TT);
BalanceCheck('qd',C,TC,RUN) = QDX(C,XC,TC,TT);
BalanceCheck('qint',C,TC,RUN) = sum(A,QINTX(C,A,XC,TC,TT));
BalanceCheck('qh',C,TC,RUN) = sum(H,QHX(C,H,XC,TC,TT));
BalanceCheck('qe',C,TC,RUN) = QEX(C,'rest',XC,TC,TT);
BalanceCheck('qm',C,TC,RUN) = QMX(C,'rest',XC,TC,TT);
BalanceCheck('qx',C,TC,RUN) = QXX(C,XC,TC,TT);
BalanceCheck('qxac',C,TC,RUN) = sum(A,QXACX(A,C,XC,TC,TT));


* Checks and results for private transport modification
HH_Energy('SATIM','TRA',C,TC,RUN) = Flow_IN_FSFuels('tppr',C,TC)*0.8;
* 0.8 is the assumption that 20% of fuel is not consumed/paid for from households budgets: rental cars, govm, companies, etc.
HH_Energy('SATIM','OTH',C,TC,RUN) = sum(FH,Flow_IN_FSFuels(FH,C,TC));
HH_Energy('eSAGE','TRA',C,TC,RUN) = QINTX(C,'aprtr',XC,TC,TT);
HH_Energy('eSAGE','OTH',CHEnergy,TC,RUN) = sum(H,QHX(CHEnergy,H,XC,TC,TT));
