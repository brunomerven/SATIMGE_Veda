
*** Aggregate Indicators -> Report
* REPORT(PRC,COM,AY,RUN,Indicators), where Aggregate Indicator -> PRC, 'ACTGRP' -> COM, 'EconIndicator' -> Indicators.
* Indicator -> PRC needs to be added to setsandmaps.xlsx!mapPRC column A

* GENERAL ECONOMY
Report('Population','ACTGRP',TC,RUN,'EconIndicator')            =POP(TC);
Report('PopulationActive','ACTGRP',TC,RUN,'EconIndicator')            =POPActive(TC);

Report('GVA_NOM','ACTGRP',TC,RUN,'EconIndicator')               = SUM(A,PVAX(A,'nat',XC,TC,TT)*(1-TVAX(A,'nat',XC,TC,TT))*QVAX(A,'nat',XC,TC,TT));

* GDP at factor costs is sum of GVA
Report('GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator')                  = SUM(A,PVA0(A,'nat')*(1-TVA0(A,'nat'))*QVAX(A,'nat',XC,TC,TT));
Report('GDPperCapita','ACTGRP',TC,RUN,'EconIndicator')          = Report('GDPFactorCosts','ACTGRP',TC,RUN,'EconIndicator')/POP(TC);

*INVESTMENT
Report('TotalInvestEconomy','ACTGRP',TC,RUN,'EconIndicator') = sum((IT,C),QINVX(C,IT,XC,TC,TT)*PQ0(C));
Report('TotalInvestEconomy_NOM','ACTGRP',TC,RUN,'EconIndicator') = sum((IT,C),QINVX(C,IT,XC,TC,TT)*PQX(C,XC,TC,TT));

Report('EmploymentRate','ACTGRP',TC,RUN,'EconIndicator') = sum(FLAB,QFSX(FLAB,XC,TC,TT))/POPActive(TC);
Report('UnemploymentRate','ACTGRP',TC,RUN,'EconIndicator') = 1-Report('EmploymentRate','ACTGRP',TC,RUN,'EconIndicator');

* Econ indicators for SATIM sectors
REPORT(PRC,'ACTGRP',TC,RUN,'EmploymentSATIM') = sum(FLAB,sum(FS$MPRCFS2(PRC,FS),sum(A$MFSA(FS,A),QFX(FLAB,A,'nat',XC,TC,TT))));

Report('ExchangeRate','ACTGRP',TC,RUN,'EconIndicator')          = EXRX(XC,TC,TT);

Report('hhd-le','ACTGRP',TC,RUN,'PopulationSATIM') = TFHPOP('hhd-le',TC)*POP(TC);
Report('hhd-me','ACTGRP',TC,RUN,'PopulationSATIM') = TFHPOP('hhd-me',TC)*POP(TC);
Report('hhd-he','ACTGRP',TC,RUN,'PopulationSATIM') = TFHPOP('hhd-he',TC)*POP(TC);

* GOVERNMENT
Report('GovRevenue','ACTGRP',TC,RUN,'EconIndicator')            = YGX(XC,TC,TT) ;
Report('GovExpenditure','ACTGRP',TC,RUN,'EconIndicator')                = EGX(XC,TC,TT);
Report('GovSaving','ACTGRP',TC,RUN,'EconIndicator')             = GSAVX(XC,TC,TT);

************** Julia to Work Here
$ontext
PrivateConsumption/GDP
GovConsumption/GDP
Investment/GDP
NetExports
ExchangeRate
FiscalRevenue/GDP
FiscalExpenditure/GDP
LabourIncomeShare
CapitalIncomeShare
PrimaryIndShare
SecondaryIndShare
TertiaryIndShare
PalmaRatio
20-20Ratio
$offtext


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
* PX0 is used because CCON is calculated using ASAM, which gets constructed using PQ0
Report2('CarbonContentCommodities',C,'NA',TC,RUN) = CCONX(C,TC)*PX0(C);


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
