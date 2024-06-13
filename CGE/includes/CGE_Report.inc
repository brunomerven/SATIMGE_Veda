
*** Aggregate Indicators -> Report
* REPORT(PRC,COM,AY,RUN,Indicators), where Aggregate Indicator -> PRC, 'ACTGRP' -> COM, 'EconIndicator' -> Indicators.
* Indicator -> PRC needs to be added to setsandmaps.xlsx!mapPRC column A

* GENERAL ECONOMY
Report('Econ_Population','ACTGRP',TC,RUN,'EconIndicator')            =POP(TC);
Report('Econ_PopulationActive','ACTGRP',TC,RUN,'EconIndicator')            =POPActive(TC);

Report('Econ_GDPFactorCosts_NOM','ACTGRP',TC,RUN,'EconIndicator')               = SUM(A,PVAX(A,'nat',XC,TC,TT)*(1-TVAX(A,'nat',XC,TC,TT))*QVAX(A,'nat',XC,TC,TT));

* GDP at factor costs is sum of GVA
Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator')                  = SUM(A,PVA0(A,'nat')*(1-TVA0(A,'nat'))*QVAX(A,'nat',XC,TC,TT));

* Net Tax/subsidy income
Report('Econ_NetTaxes_NOM','ACTGRP',TC,RUN,'EconIndicator')
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

Report('Econ_NetTaxes','ACTGRP',TC,RUN,'EconIndicator')
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


Report('Econ_GDPMarketPrices_NOM','ACTGRP',TC,RUN,'EconIndicator') = Report('Econ_GDPFactorCosts_NOM','ACTGRP',TC,RUN,'EconIndicator') + Report('Econ_NetTaxes_NOM','ACTGRP',TC,RUN,'EconIndicator');
Report('Econ_GDPMarketPrices','ACTGRP',TC,RUN,'EconIndicator') = Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator') + Report('Econ_NetTaxes','ACTGRP',TC,RUN,'EconIndicator');


Report('Econ_GDPperCapita','ACTGRP',TC,RUN,'EconIndicator')          = Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator')/POP(TC);

*INVESTMENT
Report('Econ_TotalInvestEconomy','ACTGRP',TC,RUN,'EconIndicator') = sum((IT,C),QINVX(C,IT,XC,TC,TT)*PQ0(C));
Report('Econ_TotalInvestEconomy_NOM','ACTGRP',TC,RUN,'EconIndicator') = sum((IT,C),QINVX(C,IT,XC,TC,TT)*PQX(C,XC,TC,TT));


Report('Econ_EmploymentRate','ACTGRP',TC,RUN,'EconIndicator') = sum(FLAB,QFSX(FLAB,XC,TC,TT))/POPActive(TC);
Report('Econ_UnemploymentRate','ACTGRP',TC,RUN,'EconIndicator') = 1-Report('Econ_EmploymentRate','ACTGRP',TC,RUN,'EconIndicator');

* Econ indicators for SATIM sectors
REPORT(PRC,'ACTGRP',TC,RUN,'EmploymentSATIM') = sum(FLAB,sum(FS$MPRCFS2(PRC,FS),sum(A$MFSA(FS,A),QFX(FLAB,A,'nat',XC,TC,TT))));

Report('Econ_ExchangeRate','ACTGRP',TC,RUN,'EconIndicator')          = EXRX(XC,TC,TT);

Report('hhd-le','ACTGRP',TC,RUN,'PopulationSATIM') = TFHPOP('hhd-le',TC)*POP(TC);
Report('hhd-me','ACTGRP',TC,RUN,'PopulationSATIM') = TFHPOP('hhd-me',TC)*POP(TC);
Report('hhd-he','ACTGRP',TC,RUN,'PopulationSATIM') = TFHPOP('hhd-he',TC)*POP(TC);

* GOVERNMENT
Report('Econ_GovRevenue','ACTGRP',TC,RUN,'EconIndicator')            = YGX(XC,TC,TT) ;
Report('Econ_GovExpenditure','ACTGRP',TC,RUN,'EconIndicator')        = EGX(XC,TC,TT);
Report('Econ_GovSaving','ACTGRP',TC,RUN,'EconIndicator')             = GSAVX(XC,TC,TT);




************** Julia to Work Here

Report('Econ_GovExpendtoGDP','ACTGRP',TC,RUN,'EconIndicator')            = EGX(XC,TC,TT)/Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator');


Report('Econ_GovRevtoGDP','ACTGRP',TC,RUN,'EconIndicator')            = YGX(XC,TC,TT)/Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator');

Report('Econ_InvestmenttoGDP','ACTGRP',TC,RUN,'EconIndicator') = sum((IT,C),QINVX(C,IT,XC,TC,TT)*PQ0(C))/Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator');

* HOUSEHOLDS
Report('Econ_PrivateConsumption','ACTGRP',TC,RUN,'EconIndicator')   = sum((C,H),QHX(C,H,'BASE',TC,TT)*PQHX(C,H,'BASE',TC,TT));

Report('Econ_PrivateConstoGDP','ACTGRP',TC,RUN,'EconIndicator')      = Report('Econ_PrivateConsumption','ACTGRP',TC,RUN,'EconIndicator')
                                                                       /Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator');

* TRADE

Report('Econ_NetExports','ACTGRP',TC,RUN,'EconIndicator')         =  Sum(C,QEX(C,'rest','BASE',TC,TT)*PE0(C,'rest')-QMX(C,'rest','BASE',TC,TT)*PM0(C,'rest')) ;

Report('Econ_ExchangeRate','ACTGRP',TC,RUN,'EconIndicator')       = EXRX('base',TC,TT);

Report('Econ_LabourIncomeShare','ACTGRP',TC,RUN,'EconIndicator')  = sum((INS,FLAB),YIFX(INS,FLAB,'base',TC,TT))/sum((INS,F),YIFX(INS,F,'base',TC,TT));

Report('Econ_CapitalIncomeShare','ACTGRP',TC,RUN,'EconIndicator') = sum((INS,FCAP),YIFX(INS,FCAP,'base',TC,TT))/sum((INS,F),YIFX(INS,F,'base',TC,TT));

* Report('Econ_PrimaryIndShare','ACTGRP',TC,RUN,'EconIndicator')    --> will generate just dont have a map of industry breakdown. Should I include a map to generate these sub parameters?
* Report('Econ_SecondaryIndShare','ACTGRP',TC,RUN,'EconIndicator')
* Report('Econ_TertiaryIndShare','ACTGRP',TC,RUN,'EconIndicator')

Report('Econ_PalmaRatio','ACTGRP',TC,RUN,'EconIndicator')         = (YIX('hhd-9',XC,TC,TT)+YIX('hhd-8',XC,TC,TT))
                                                                          /(YIX('hhd-0',XC,TC,TT)+YIX('hhd-1',XC,TC,TT));
Report('Econ_20-20Ratio','ACTGRP',TC,RUN,'EconIndicator')         = (YIX('hhd-9',XC,TC,TT))/(YIX('hhd-0',XC,TC,TT)+YIX('hhd-1',XC,TC,TT)
                                                                     +YIX('hhd-2',XC,TC,TT)+YIX('hhd-3',XC,TC,TT));


*************** End of Julia Section


*** Energy Specific Indicators Aggregates
*PrimaryEnergy/GDP        Report
*FinalEnergy/GDP        Report
*PrimaryEnergy/Capita        Report
*FinalEnergy/Capita        Report
*CO2eq/GDP        Report
*CO2eq/capita        Report
*ElcPrice        Report
*EnergyBillhouseholds        Report
*EnergyBillFirms        Report
*EnergyBillGovernment        Report
*TotalInvestmentEnergy        Report
*InvestmentEnergy/GDP        Report
*EmploymentEnergy        Report
*CO2eq/Electricity        Report
*CarbonTaxLevel        Report
*CarbonTaxRevenue        Report


*** Flows and Prices (Q, P, P*Q)
*GVA
Report2('GVA',A,'total',TC,RUN)                  = PVA0(A,'nat')*(1-TVA0(A,'nat'))*QVAX(A,'nat',XC,TC,TT);

*Output
Report2('Output',A,C,TC,RUN) = QXACX(A,C,XC,TC,TT);

* LABOUR
Report2('EmploymentbyActivitybyLabourType',A,FLAB,TC,RUN)               = QFX(FLAB,A,'NAT',XC,TC,TT)   ;
Report2('WageRatebyLabourType',A,FLAB,TC,RUN)              = WFX(FLAB,XC,TC,TT)*WFDISTX(FLAB,A,'nat',XC,TC,TT)  ;
Report2('WageBill',A,FLAB,TC,RUN)                 = WFX(FLAB,XC,TC,TT)*WFDISTX(FLAB,A,'nat',XC,TC,TT)*QFX(FLAB,A,'nat',XC,TC,TT)  ;

*FACTOR INCOME
Report2('FactorIncome',INS,F,TC,RUN)                    = YIFX(INS,F,XC,TC,TT)    ;


* TRADE
* CERES commodities can have negative exports (imports at the pex price)
QExptP(C,TC)$(QEX(C,'rest',XC,TC,TT) >=0) = QEX(C,'rest',XC,TC,TT);
QExptN(C,TC)$(QEX(C,'rest',XC,TC,TT) <0) = QEX(C,'rest',XC,TC,TT)*(-1);

QImpt(C,TC) = QMX(C,'rest',XC,TC,TT);

Report2('ImportsValueRands',C,'total',TC,RUN) = QImpt(C,TC)*PMX(C,'rest',XC,TC,TT) + QExptN(C,TC)*PEX(C,'rest',XC,TC,TT);
Report2('ImportsQuantities',C,'total',TC,RUN) = QImpt(C,TC) + QExptN(C,TC);

Report2('ExportsValueRands',C,'total',TC,RUN) = QExptP(C,TC)*PEX(C,'rest',XC,TC,TT);
Report2('ExportsQuantities',C,'total',TC,RUN) = QExptP(C,TC);


Report2('ExchangeRate','total','total',TC,RUN)          =EXRX(XC,TC,TT);

*Final Consumption by institution by commodity

*Intermediate Consumption by activity

Report2('CommodityPrices',C,'total',TC,RUN)           = PQX(C,XC,TC,TT) ;

* Investment
Report2('InvestmentDemandbyCommodity',C,'total',TC,RUN)               = sum(IT,QINVX(C,IT,XC,TC,TT)*PQ0(C));


* Capital formation or savings. INSDNG are households and enterprises.
* Total CapitalFormation is the equal to TotalInvestment
Report2('CapitalFormation','total',INSDNG,TC,RUN) = MPSX(INSDNG,XC,TC,TT) * (1 - TINSX(INSDNG,XC,TC,TT)) * YIX(INSDNG,XC,TC,TT);
Report2('CapitalFormation','total','gov',TC,RUN) = GSAVX(XC,TC,TT);
Report2('CapitalFormation','total','row',TC,RUN) = (FSAVX(XC,TC,TT)-irate*DEBTX(XC,TC,TT))*EXRX(XC,TC,TT);


Report2('CapitalAlloc',A,'total',TC,RUN)$AFX(A,'nat') = QFX('fcap',A,'nat',XC,TC,TT)*(AFXGR(A,'nat',XC,TC) + natdrate)*SUM(C$PQ0(C), iwts(C,'S-I')*PQX(C,XC,TC,TT)/PQ0(C));
TotalInvestmentFixed(TC,RUN) = sum(A,Report2('CapitalAlloc',A,'total',TC,RUN));

Report2('CapitalAlloc','aelec','total',TC,RUN) = EINVQINV(XC,TC,TT);
* aelec is part of AFX but not included in QFX('fcap'). INVSHR2 is share of ANFX sectors
Report2('CapitalAlloc',A,'total',TC,RUN)$ANFX(A,'nat') = INVSHR2X('fcap',A,'nat',XC,TC,TT)*(Report('Econ_TotalInvestEconomy','ACTGRP',TC,RUN,'EconIndicator')-TotalInvestmentFixed(TC,RUN));

* Carbon Content and BTA indicators
* PX0 is used because CCON is calculated using ASAM, which gets constructed using PQ0
Report2('CarbonContentCommodities',C,'total',TC,RUN) = CCONX(C,TC)*PX0(C);





$ontext
* will do this under agent income/expenses
Report2('GovDirTax',H,'total',TC,RUN)                   =  SUM(INSDNG, TINSX(INSDNG,XC,TC,TT)*YIX(INSDNG,XC,TC,TT)) ;
Report2('GovFacTax','total',F,TC,RUN)                = tfx(F,XC,TC,TT)*YFX(F,XC,TC,TT) ;
Report2('GovFacInc',H,F,TC,RUN)                   =  SUM(INS, YIFX(INS,F,XC,TC,TT)) ;
Report2('GovTrnsfr',AC,INS,TC,RUN)                =  trnsfrx(INS,AC,XC,TC,TT)*EXRX(XC,TC,TT);
Report2('GovActTax',A,'total',TC,RUN)                = TAX(A,'nat',XC,TC,TT)*PAX(A,XC,TC,TT)*QAX(A,XC,TC,TT)  ;
Report2('GovVatTax',A,'total',TC,RUN)                = TVAX(A,'nat',XC,TC,TT)*PVAX(A,'nat',XC,TC,TT)*QVAX(A,'nat',XC,TC,TT) ;
Report2('GovImpTax',C,'total',TC,RUN)                = TMX(C,'rest',XC,TC,TT)*QMX(C,'rest',XC,TC,TT)*PMX(C,'rest',XC,TC,TT)*EXRX(XC,TC,TT);
Report2('GovExpTax',C,'total',TC,RUN)                = te(C,'rest')*QEX(C,'rest',XC,TC,TT)*PEX(C,'rest',XC,TC,TT)*EXRX(XC,TC,TT);
Report2('GovComTax',C,'total',TC,RUN)                = TQX(C,XC,TC,TT)*PQX(C,XC,TC,TT)*QQX(C,XC,TC,TT)   ;
$offtext
