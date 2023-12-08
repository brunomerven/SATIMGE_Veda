
*** Aggregate Indicators -> Report
* REPORT(PRC,COM,AY,RUN,Indicators), where Aggregate Indicator -> PRC, 'ACTGRP' -> COM, 'EconIndicator' -> Indicators.
* Indicator -> PRC needs to be added to setsandmaps.xlsx!mapPRC column A

* GENERAL ECONOMY
Report('Econ_Population','ACTGRP',TC,RUN,'EconIndicator')            =POP(TC);
Report('Econ_PopulationActive','ACTGRP',TC,RUN,'EconIndicator')            =POPActive(TC);

Report('Econ_GVA_NOM','ACTGRP',TC,RUN,'EconIndicator')               = SUM(A,PVAX(A,'nat',XC,TC,TT)*(1-TVAX(A,'nat',XC,TC,TT))*QVAX(A,'nat',XC,TC,TT));

* GDP at factor costs is sum of GVA
Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator')                  = SUM(A,PVA0(A,'nat')*(1-TVA0(A,'nat'))*QVAX(A,'nat',XC,TC,TT));
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

Report('Econ_GovExpendtoGDP',TC,RUN,'EconIndicator')            = EGX(XC,TC,TT)/Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator');
Report('Econ_GovRevtoGDP',TC,RUN,'EconIndicator')            = YGX(XC,TC,TT)/Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator');
Report('Econ_InvestmenttoGDP','ACTGRP',TC,RUN,'EconIndicator') = sum((IT,C),QINVX(C,IT,XC,TC,TT)*PQ0(C))/Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator');

* HOUSEHOLDS
Report('Econ_PrivateConsumption','ACTGRP',TC,RUN,'EconIndicator')   = QHX(C,H,'BASE',T,'2055')*PQHX(C,H,'BASE',T,TT);
Report('Econ_PrivateConstoGDP','ACTGRP',TC,RUN,'EconIndicator')      = Report('Econ_PrivateConsumption','ACTGRP',TC,RUN,'EconIndicator')
                                                                       /Report('Econ_GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator');

* TRADE
Report('Econ_NetExports','ACTGRP',TC,RUN,'EconIndicator')         = Report2('Exports',C,'NA',T,RUN)-Report2('Imports',C,'NA',T,RUN);
Report('Econ_ExchangeRate','ACTGRP',TC,RUN,'EconIndicator')       = EXRX('base',T,TT);

Report('Econ_LabourIncomeShare','ACTGRP',TC,RUN,'EconIndicator')  = sum((INS,FLAB),YIFX(INS,FLAB,'base',T,TT))/sum((INS,F),YIFX(INS,F,'base',T,TT));
Report('Econ_CapitalIncomeShare','ACTGRP',TC,RUN,'EconIndicator') = sum((INS,FCAP),YIFX(INS,FCAP,'base',T,TT))/sum((INS,F),YIFX(INS,F,'base',T,TT));

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
Report2('GVA',A,'NA',TC,RUN)                  = PVA0(A,'nat')*(1-TVA0(A,'nat'))*QVAX(A,'nat',XC,TC,TT);

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

Report2('ImportsValueRands',C,'NA',TC,RUN) = QImpt(C,TC)*PMX(C,'rest',XC,TC,TT) + QExptN(C,TC)*PEX(C,'rest',XC,TC,TT);
Report2('ImportsQuantities',C,'NA',TC,RUN) = QImpt(C,TC) + QExptN(C,TC);

Report2('ExportsValueRands',C,'NA',TC,RUN) = QExptP(C,TC)*PEX(C,'rest',XC,TC,TT);
Report2('ExportsQuantities',C,'NA',TC,RUN) = QExptP(C,TC);


Report2('ExchangeRate','NA','NA',TC,RUN)          =EXRX(XC,TC,TT);

*Final Consumption by institution by commodity

*Intermediate Consumption by activity

Report2('CommodityPrices',C,'NA',TC,RUN)           = PQX(C,XC,TC,TT) ;

* Investment
Report2('InvestmentDemandbyCommodity',C,'NA',TC,RUN)               = sum(IT,QINVX(C,IT,XC,TC,TT)*PQ0(C));


* Capital formation or savings. INSDNG are households and enterprises.
* Total CapitalFormation is the equal to TotalInvestment
Report2('CapitalFormation','NA',INSDNG,TC,RUN) = MPSX(INSDNG,XC,TC,TT) * (1 - TINSX(INSDNG,XC,TC,TT)) * YIX(INSDNG,XC,TC,TT);
Report2('CapitalFormation','NA','gov',TC,RUN) = GSAVX(XC,TC,TT);
Report2('CapitalFormation','NA','row',TC,RUN) = (FSAVX(XC,TC,TT)-irate*DEBTX(XC,TC,TT))*EXRX(XC,TC,TT);


Report2('CapitalAlloc',A,'NA',TC,RUN)$AFX(A,'nat') = QFX('fcap',A,'nat',XC,TC,TT)*(AFXGR(A,'nat',XC,TC) + natdrate)*SUM(C$PQ0(C), iwts(C,'S-I')*PQX(C,XC,TC,TT)/PQ0(C));
TotalInvestmentFixed(TC,RUN) = sum(A,Report2('CapitalAlloc',A,'NA',TC,RUN));

Report2('CapitalAlloc','aelec','NA',TC,RUN) = EINVQINV(XC,TC,TT);
* aelec is part of AFX but not included in QFX('fcap'). INVSHR2 is share of ANFX sectors
Report2('CapitalAlloc',A,'NA',TC,RUN)$ANFX(A,'nat') = INVSHR2X('fcap',A,'nat',XC,TC,TT)*(Report('TotalInvestEconomy','ACTGRP',TC,RUN,'EconIndicator')-TotalInvestmentFixed(TC,RUN));

* Carbon Content and BTA indicators
Report2('CarbonContentCommodities',C,'NA',TC,RUN) = CCONX(C,TC)*PXX(C,XC,TC,TT);


execute_unload "Tableau.gdx" Report2;
execute 'gdxdump Tableau.gdx output=Report_00.csv symb=Report2 format=csv header="Indicator,AC,AC,Year,Scenario,SATIMGE"';



$ontext
* will do this under agent income/expenses
Report2('GovDirTax',H,'NA',TC,RUN)                   =  SUM(INSDNG, TINSX(INSDNG,XC,TC,TT)*YIX(INSDNG,XC,TC,TT)) ;
Report2('GovFacTax','NA',F,TC,RUN)                = tfx(F,XC,TC,TT)*YFX(F,XC,TC,TT) ;
Report2('GovFacInc',H,F,TC,RUN)                   =  SUM(INS, YIFX(INS,F,XC,TC,TT)) ;
Report2('GovTrnsfr',AC,INS,TC,RUN)                =  trnsfrx(INS,AC,XC,TC,TT)*EXRX(XC,TC,TT);
Report2('GovActTax',A,'NA',TC,RUN)                = TAX(A,'nat',XC,TC,TT)*PAX(A,XC,TC,TT)*QAX(A,XC,TC,TT)  ;
Report2('GovVatTax',A,'NA',TC,RUN)                = TVAX(A,'nat',XC,TC,TT)*PVAX(A,'nat',XC,TC,TT)*QVAX(A,'nat',XC,TC,TT) ;
Report2('GovImpTax',C,'NA',TC,RUN)                = TMX(C,'rest',XC,TC,TT)*QMX(C,'rest',XC,TC,TT)*PMX(C,'rest',XC,TC,TT)*EXRX(XC,TC,TT);
Report2('GovExpTax',C,'NA',TC,RUN)                = te(C,'rest')*QEX(C,'rest',XC,TC,TT)*PEX(C,'rest',XC,TC,TT)*EXRX(XC,TC,TT);
Report2('GovComTax',C,'NA',TC,RUN)                = TQX(C,XC,TC,TT)*PQX(C,XC,TC,TT)*QQX(C,XC,TC,TT)   ;
$offtext
