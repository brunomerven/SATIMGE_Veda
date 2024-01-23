 CPI.L                  = CPI0;
 DMPS.L                 = DMPS0;
 DPI.L                  = DPI0;
 DTINS.L                = DTINS0;
 EG.L                   = EG0;
 EH.L(H)                = EH0(H);
 EXR.L                  = EXR0;
 FSAV.L                 = FSAV0;
 GADJ.L                 = GADJ0;
 GOVSHR.L               = GOVSHR0;
 GSAV.L                 = GSAV0;
 IADJ.L(IT)             = IADJ0(IT);
 INVSHR.L               = INVSHR0;
 MPS.L(INSDNG)          = MPS0(INSDNG);
 MPSADJ.L               = MPSADJ0;
 PA.L(A)                = PA0(A);
 PAR.L(A,RD)            = PAR0(A,RD);
 PDD.L(C)               = PDD0(C);
 PDS.L(C)               = PDS0(C);
 PINTA.L(A,RD)          = PINTA0(A,RD) ;
 PE.L(C,RW)             = PE0(C,RW);
 PM.L(C,RW)             = PM0(C,RW);
 PQ.L(C)                = PQ0(C);
 PQH.L(C,H)             = PQH0(C,H);
 PQI.L(C,A,RD)          = PQI0(C,A,RD);
 PVA.L(A,RD)            = PVA0(A,RD);
 PX.L(C)                = PX0(C);
 PXAC.L(A,C)            = PXAC0(A,C);
 QA.L(A)                = QA0(A);
 QAR.L(A,RD)            = QAR0(A,RD);
 QD.L(C)                = QD0(C);
 QE.L(C,RW)             = QE0(C,RW);
 QF.L(F,A,RD)           = QF0(F,A,RD);
 QFS0(F)                = QFS00(F);
 QFS.L(F)               = QFS0(F);
 QG.L(C)                = QG0(C);
 QH.L(C,H)              = QH0(C,H);
 QHA.L(A,H)             = QHA0(A,H);
 QINT.L(C,A)            = QINT0(C,A);
 QINTA.L(A,RD)          = QINTA0(A,RD) ;
 QINV.L(C,IT)           = QINV0(C,IT);
 QM.L(C,RW)             = QM0(C,RW);
 QQ.L(C)                = QQ0(C);
 QT.L(C)                = QT0(C);
 QVA.L(A,RD)            = QVA0(A,RD);
 QX.L(C)                = QX0(C);
 QXAC.L(A,C)            = QXAC0(A,C);
 TA.L(A,RD)             = TA0(A,RD);
 TAADJ.L                = TAADJ0;
 TABS.L                 = TABS0;
 TAPS.L                 = TAPS0;
 theta(A,C)             = theta0(A,C);
 TQADJ.L                = TQADJ0;
 TQ.L(C)                = TQ0(C);
 TQPS.L                 = TQPS0;
 TRII.L(INSDNG,INSDNGP) = TRII0(INSDNG,INSDNGP);
 TINS.L(INSDNG)         = TINS0(INSDNG);
 TINSADJ.L              = TINSADJ0;
 WALRAS.L               = WALRAS0;
 WALRASSQR.L            = 0 ;
 WF.L(F)                = WF0(F);
 WFDIST.L(F,A,RD)       = WFDIST0(F,A,RD);
 YF.L(F)                = YF0(f);
 YG.L                   = YG0;
 YI.L(INS)              = YI0(INS);
 YIF.L(INS,F)           = YIF0(INS,F);

 gammam(C,H)            = gammam0(C,H);
 gammah(A,H)            = gammah0(A,H);
 PWE.l(C,RW)            = pwe0(C,RW);
 PWM.l(C,RW)            = pwm0(C,RW);
 trnsfr(INS,AC)         = trnsfr0(INS,AC);
 fprd(F,A,RD)$QF0(F,A,RD) = 1;

 alphava(A,RD)          = alphava0(A,RD);
 leova(A,RD)            = leova0(A,RD);
 hpop(H)                = hpop0(H);

 betam(C,H) = betam0(C,H);
 debt = debt0;

*Rest carbon tax and content coefficients
 tco2d      = tco2d0;
 tco2e      = tco2e0;
 rtco2e     = rtco2e0;
 tco2m      = tco2m0;
 co2c(C)    = co2c0(C);
 co2e(C,RW) = co2e0(C,RW);
 co2m(C,RW) = co2m0(C,RW);

 RTC(CE) = yes;
 RTC('cptrp-l') = no;
 RTC('cptrp-o') = no;
 RTC('cftrp-l') = no;
 RTC('cftrp-o') = no;
 RTC('ctrps') = no;




*CA rest of world price equations and trade quantities
*Fix electricity imports and exports

 CEFIX(C)=NO;
 CEFIX('CELEC')=YES;
 CERES('CELEC')=YES;
 QE.FX('CELEC','REST') = QE0('CELEC','REST') ;


* CMFIX(C)=NO;
* CMFIX('CELEC')=YES;
* CMRES('CELEC')= YES;
* QM.FX('CELEC','REST') = QM0('CELEC','REST') ;

*$ontext

*bm+fh_20190207
* CMFIX('CPETR_T')=YES;
* CMRES('CPETR_T')= YES;
* QM.FX('CPETR_T','REST') = QM0('CPETR_T','REST') ;
* CEFIX('ccoal-hgh')=YES;
* CERES('ccoal-hgh')=YES;
* CERES('ccoal-low')=YES;

* QE.FX('ccoal-hgh','REST') = QE0('ccoal-hgh','REST') ;


*@ CEFIX('cftrp-o')=YES;
*@ CERES('cftrp-o')=YES;
*@ QE.FX('cftrp-o','REST') = QE0('cftrp-o','REST') ;
*@
*@ CEFIX('cptrp-o')=YES;
*@ CERES('cptrp-o')=YES;
*@ QE.FX('cptrp-o','REST') = QE0('cptrp-o','REST') ;
*@
*@
 CERES('ccoal-low')=YES;
 CERES('ccoal-hgh')=YES;
*@
 CERES('ccoil') = YES;
*@
 CERES('cngas') = YES;
*@* QM.FX('cngas','REST') = QM0('CNGAS','REST');
*@
 CERES('chydr')=YES;
 CERES('cfcel')=YES;
 CERES('celct')=YES;
*@* QE.FX('celct','REST') = QE0('celct','REST') ;
*@
* CERES('cbchm') = YES;
* CERES('cochm') = YES;
*@
*@
 CERES('ciron') = NO;
 CEFIX('ciron') = no;
 QE.up('ciron','REST') = inf;
 QE.lo('ciron','REST') = -inf;
* CEFIX('ciron') = YES;
* QE.FX('ciron','REST') = QE0('ciron','REST') ;
*@*bm Dec2022: reason for fixing imports: without fixing it we seem to see an explosion in imports caused by an increase in PQ?
*bm Jan2024: will now make exports dynamic based on whether green exports scenario is active so these exports are responsive to BTA
*bm BTA is overwritten in big exports scenario

 CMRES('ciron') = NO;
 QM.up('ciron','REST') = inf ;
 QM.lo('ciron','REST') = -inf ;

* CMRES('ciron')= YES;
* QM.FX('ciron','REST') = QM0('ciron','REST') ;
*@
*@
 CERES('cammo') = YES;
 CEFIX('cammo') = YES;
 QE.FX('cammo','REST') = QE0('cammo','REST') ;
 CMRES('cammo')= YES;
 QM.FX('cammo','REST') = QM0('cammo','REST') ;
*@
*@
 CERES('cpgm') = YES;
* CEFIX('cpgm') = YES;
* QE.FX('cpgm','REST') = QE0('cpgm','REST') ;
*@
*@

* CERES('cmore') = YES;
* CMRES('cmore')= YES;
* QM.FX('cmore','REST') = QM0('cmore','REST') ;

*!! CERES('cvehi') = YES;
*!! QE.FX('cvehi','REST') = QE0('cvehi','REST');

 CMRES('cmach_e') = No;
 CMRES('cemch_e') = No;
 CMRES('cmetp') = No;

 QM.LO('cmach_e','REST') = -inf;
 QM.UP('cmach_e','REST') = +inf;

 QM.LO('cemch_e','REST') = -inf;
 QM.UP('cemch_e','REST') = +inf;

 QM.LO('cmetp','REST') = -inf;
 QM.UP('cmetp','REST') = +inf;

*@
*@
*@
*@
*@* CMRES('cpetr_p')= YES;
*@* CMRES('cpetr_d')= YES;
*@* CMRES('cpetr_o')= YES;
*@
*@* CEFIX('cpetr_p')=YES;
*@* QE.FX('cpetr_p','REST') = QE0('cpetr_p','REST') ;
*@
*@* CEFIX('cpetr_d')=YES;

*@* QE.FX('cpetr_d','REST') = QE0('cpetr_d','REST') ;
*@
*@
*@
*@* CEFIX('cpetr_o')=YES;
 CERES('cpetr-d')=YES;
 CERES('cpetr-p')=YES;
 CERES('cpetr-h')=YES;
 CERES('cpetr-k')=YES;
 CERES('cpetr-l')=YES;
*@* QE.FX('cpetr_o','REST') = QE0('cpetr_o','REST') ;
*@
*@*$offtext

*bm resetting ice for cpetr-k. Need more elgant solution here!!!
 ice('ctrad','cpetr-k') = 0.110830250152141;
 ice('cftrp-l','cpetr-k') = 0.0111801722874545;
 ice('cftrp-o','cpetr-k') = 0.000434565319761202;


*bm setting acoal coal aggregate price
 pacoalx(T1,RUN) =  sum(CCOAL,theta0('acoal',CCOAL)*pq0(CCOAL))/sum(CCOAL,theta0('acoal',CCOAL));


 pwebar(C,RW) = PWE0(C,RW);
 pwMbar(C,RW) = PWM0(C,RW);

*Reset IO coefficients to base values
 inta(A,RD) = inta0(A,RD);
 iva(A,RD) = iva0(A,RD);
 ica(C,A,RD)=ica0(C,A,RD);
 ifa(F,A,RD)=ifa0(F,A,RD);

 tui(C,A,RD) = tui0(C,A,RD);
 tuH(C,H) = tuh0(C,H);

 tm(C,RW)=tm0(C,RW);

 QINVK=0;
 NGFCF=0;

 AFX(A,RD) = NO;
 ANFX(A,RD)$(NOT AFX(A,RD)) = YES;

 ALPHAQF.L=1;

 QFBAR(FCAP,A,RD)$QF0(FCAP,A,RD)=QF0(FCAP,A,RD);

 WFK2AV(FCAP) = WFK2AV0(FCAP);

*CA reset variables to format from model.gms
 PQ.LO('CELEC') = -inf;
 PQ.UP('CELEC') = +inf;
*fh 71218
*fh: TEMP STAR OUT TO CHECK WHAT HAPPENS WITH NEW SAM
$ONTEXT
 PQ.LO('CPETR_P') = -inf;
 PQ.UP('CPETR_P') = +inf;
 PQ.LO('CPETR_D') = -inf;
 PQ.UP('CPETR_D') = +inf;
 PQ.LO('CPETR_O') = -inf;
 PQ.UP('CPETR_O') = +inf;
$OFFTEXT



 tq01(C) = 1;
 TQPS.FX = tqps0;
 TQELEC.FX(C) = TQELEC0(C);

*Reset share parameter in low versus high coal production function
 deltaca('acoal','ccoal-low')= deltaca0('acoal','ccoal-low');

*Reset labor supply curve calibration parameters
 WF_BAR(F)  = WF0(F);
 QFS_BAR(F) = QFS0(F);

*Reset indirect tax share variables
 YG_DTAX.L =    YG_DTAX0 ;
 YG_ITAX.L =    YG_ITAX0 ;
 ITAXSHR.L =    ITAXSHR0 ;
 TAXADJ.L  =    TAXADJ0  ;
 GDPMP.L   =    GDPMP0   ;


betaca(A,C)=1;

* Energy aspects
*Initialize values of linking variables
 PAFXCAP(XC,TC,TT) = EPS;
* EFLOW('IMP',XC,TC,TT) = QM0('CELEC','REST');
* PFLOW('IMP',XC,TC,TT) = QM0('CPETR','REST');

 ETPRICE(TC) = PQ0('CELEC');
* EFLOW('IMP',XC,TC,TT) = QM0('CELEC','REST');
*fh: TEMP STAR OUT TO CHECK WHAT HAPPENS WITH NEW SAM
* PFLOW('IMP',XC,TC,TT) = QM0('CPETR','REST');
* AFLOW_INFuels('APETR','CCOIL',TC,TT) = QINT0('CCOIL','APETR');
 EPRICE(TC,TT)=PQ0('CELEC');
 EINVCOST(RUN,TC) = 0;
 INVCOSTSubSector(RUN,TC,Sector,SubSector) = 0;
 NCAPSubSector(RUN,TC,Sector,SubSector) = 0;

 INVCOST(RUN,TC,PRC) = 0;
 BPINV(RUN,TC,TCP,PRC) = 0;

 IWTSX(C,IT,XC,TC,TT) = 0;
* QINV.LO(C,'s-e') =-INF;
* QINV.UP(C,'s-e') =+INF;
 QINV.FX(C,'s-e') = QINV0(C,'s-e');


 HCAP(X,TC,TT) = 0;
 ELCTCAP(TC,RUN) = 0;
 ELCTCAPF(TC,RUN) = 0;



 ICAGR(C,A,TC) = 1;
 ICAFSGR(C,FS,TC) = 1;
 IFAGR(F,A,TC) = 1;
 IFAFSGR(F,FS,TC) = 1;

 gammamgr(C,H,TC) = 1;

 deltaq(cm,rw) = deltaq0(cm,rw);
 deltat(cm,rw) = deltat0(cm,rw);


* budget share aspects
* initialise budgetshare template
DistanceMatrix(H,HP,TC) = 0;
DistUpper(H,TC) = 0;
DistLower(H,TC) = 0;
TmpMin(H,TC) = 0;
CUN(H,HP,TC) = No;
CLN(H,HP,TC) = No;

BudShare0(C,H,TC) = BUDSHR(C,H);

BudShareNonEnergy(H,T1) =  1-sum(CHEnergy,BudShare0(CHEnergy,H,T1));



* Other parameters needed for exogenous sectors
QINTCBCHM(T1,RUN) = SUM(a,QINT0('CBCHM',a));
QINTCOCHM(T1,RUN) = SUM(a,QINT0('COCHM',a));
QINTCIRON(T1,RUN) = SUM(a,QINT0('CIRON',a));


 IADJSESIM(XC,TC) = 1;
