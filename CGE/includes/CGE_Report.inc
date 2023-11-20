Sets
* from model

* for microsim
Years(T)
l                        labour classes /f-labun, f-labsk/
mFl(F,l)                   Mapping F to l
;

Parameters
GDPperCapita(T)                    GDP per capita
* for microsim
CPI_SA(T)                           CPI
empl_SA(A,T)                      labour demand by category and sector
hcons_SA(T)                         Total household consumption
hconssf_SA(T)                       Total household food consumption
hconsnf_SA(T)                       Total household non-food consumption
swage_SA(A,T)                       Sector wage by skill
unemp_SA(T)                         Unemployed population
wage_SA(T)                        Aggregate wage

TotalFactorInc(T)                  Total Factor Income
LabourInc(T)                       Labour income
CapitalInc(T)
LandInc(T)
GOV(T)
IndirectTaxes(T)
* for CCDR template
*Report(CCDRIndicators,T,RUN)             Indicators for Template
Report(*,T,RUN)                   Indicators for Template
HHCons(C,T)
GovCons(C,T)
Inv(C,T)
Expt(C,T)
Imp(C,T)
GDP(C,T)
HPOPX(H,X,T,TT)



EXR                              2019 Exchange Rate 2019 ZAR to 2015 USD
CO2eq(T,RUN)                         CO2 eq from SATIM
Report2(*,*,*,T,RUN)
Report3(*,*,*,T,RUN)

;



EXR = 15.674;

mFl('flab-p','f-labun') = Yes;
mFl('flab-m','f-labun') = Yes;
mFl('flab-s','f-labsk') = Yes;
mFl('flab-t','f-labsk') = Yes;


SET
L2SECT
L3SECT
ML2SECT(L2SECT,L3SECT,AC)
EngSector
EngSubSector
ENGSECT(PRC,EngSector,EngSubSector)
;

* Importing Aggregation Maps
$SETGLOBAL map "MapsSets\Maps.xlsx"
$call "gdxxrw i=Maps\MapsSets.xlsx o=Map.gdx index=index!a6"
$gdxin Map.gdx
$load L2SECT L3SECT ML2SECT EngSector EngSubSector ENGSECT
;



alias(*,u);
file ordered /ordered.txt/;
loop(SortedUels(u,L3SECT), put ordered L3SECT.tl:0 '  ');
putclose ordered;


alias(L3SECT,SECT),(AC,AACC) ;

File Scen;

LOOP(RUN,

put_utilities Scen 'gdxin' / "gdxs\",RUN.TL:50;


GDPperCapita(T)$POP(T)   = GDP_RUN(T)/POP(T);
Expt(C,T)                = QEX(C,'rest','BASE',T,TT)*PE0(C,'rest');
Imp(C,T)                 = QMX(C,'rest','BASE',T,TT)*PM0(C,'rest');


;



* GENERAL ECONOMY
Report2('Population','NA','NA',T,RUN)            =POP(T)      ;
Report2('GVA_NOM',A,'NA',T,RUN)               = PVAX(A,'nat','base',T,TT)*(1-TVAX(A,'nat','base',T,TT))*QVAX(A,'nat','base',T,TT);
Report2('GVA',A,'NA',T,RUN)                  = PVA0(A,'nat')*(1-TVA0(A,'nat'))*QVAX(A,'nat','base',T,TT);

* GDP at factor costs is sum of GVA
Report2('GDPFC','Total','NA',T,RUN)          = sum(A,Report2('GVA',A,'NA',T,RUN);

Report2('GDPperCapita','NA','NA',T,RUN)          = Report2('GDPFC','Total','NA',T,RUN)/POP(T);

* HOUSEHOLDS
Report2('Consumption',C,H,T,RUN)               = QHX(C,H,'BASE',T,TT)*PQHX(C,H,'BASE',T,TT);
Report2('FactorIncome',H,F,T,RUN)                    = YIFX(H,F,'base',T,TT)              ;
Report2('Savings',INSDNG,F,T,RUN)                    = MPSX(INSDNG,XC,TT,T) * (1 - TINSX(INSDNG,XC,TC,TT)) * YIX(INSDNG,XC,TC,TT))              ;
Report2('Transfers',INS,AC,T,RUN) = TRNSFRX(INS,AC,XC,TC,TT);
*INVESTMENT
Report2('Investment',C,'NA',T,RUN)               = sum(IT,QINVX(C,IT,'base',T,TT)*PQ0(C));
Report2('TotalInvestment','Total','NA',T,RUN) = SUM(C,Report2('Investment',C,'NA',T,RUN));

* Capital formation or savings. INSDNG are households and enterprises.
* Total CapitalFormation is the equal to TotalInvestment
Report2('CapitalFormation','NA',INSDNG,T,RUN) = SUM(INSDNG, MPSX(INSDNG,XC,TT,T) * (1 - TINSX(INSDNG,XC,TC,TT)) * YIX(INSDNG,XC,TC,TT)));
Report2('CapitalFormation','NA','gov',T,RUN) = GSAVX('BASE',T,TT);
Report2('CapitalFormation','NA','row',T,RUN) = (FSAVX('BASE',T,TT)-irate*DEBTX(XC,T,TT)*EXRX(XC,T,TT);


Report2('CapitalAlloc',AFX,'NA',T,RUN) = QFX('fcap',AFX,'nat',XC,TC,TT)*(AFXGR(AFX,'nat',XC,TC) + natdrate))*(SUM(C$PQ0(C), iwts(C,'S-I')*PQX(C,XC,T,TT)/PQ0(C)));
Report2('CapitalAlloc','aelec','NA',T,RUN) = EINVQINV(XC,TC,TT);
* aelec is part of AFX but not included in QFX('fcap'). INVSHR2 is share of ANFX sectors
Report2('CapitalAlloc',ANFX,'NA',T,RUN) = INVSHR2X('fcap',ANFX,'nat',XC,TC,TT)*(Report2('TotalInvestment','Total','NA',T,RUN)-sum(AFX,Report2('CapitalAlloc',AFX,'NA',T,RUN)));

* GOVERNMENT
Report2('GovRevenue','NA','NA',T,RUN)            = YGX('BASE',T,TT) ;
Report2('GovExp','NA','NA',T,RUN)                = EGX('base',T,TT);
Report2('GovSaving','NA','NA',T,RUN)             = GSAVX('BASE',T,TT);

Report2('GovDirTax',A,H,T,RUN)                   =  SUM(INSDNG, TINSX(INSDNG,'base',T,TT)*YIX(INSDNG,'base',T,TT)) ;
Report2('GovFacTax','NA',F,T,RUN)                = tfx(F,'base',T,TT)*YFX(F,'base',T,TT) ;
Report2('GovFacInc',H,F,T,RUN)                   =  SUM(INS, YIFX(INS,F,'Base',T,TT)) ;
Report2('GovTrnsfr',AC,INS,T,RUN)                =  trnsfrx(INS,AC,'base',T,TT)*EXRX('base',T,TT);
Report2('GovActTax',A,'NA',T,RUN)                = TAX(A,'nat','base',T,TT)*PAX(A,'base',T,TT)*QAX(A,'base',T,TT)  ;
Report2('GovVatTax',A,'NA',T,RUN)                = TVAX(A,'nat','base',T,TT)*PVAX(A,'nat','base',T,TT)*QVAX(A,'nat','base',T,TT) ;
Report2('GovImpTax',C,'NA',T,RUN)                = TMX(C,'rest','base',T,TT)*QMX(C,'rest','base',T,TT)*PMX(C,'rest','base',T,TT)*EXRX('base',T,TT);
Report2('GovExpTax',C,'NA',T,RUN)                = te(C,'rest')*QEX(C,'rest','base',T,TT)*PEX(C,'rest','base',T,TT)*EXRX('base',T,TT);
Report2('GovComTax',C,'NA',T,RUN)                = TQX(C,'base',T,TT)*PQX(C,'base',T,TT)*QQX(C,'base',T,TT)   ;


* TRADE
Report2('Imports',C,'NA',T,RUN)$(EXPT(C,T)>=0)   = IMP(C,T)*(-1);
Report2('Imports',C,'NA',T,RUN)$(EXPT(C,T)<0)    = (IMP(C,T)-EXPT(C,T))*(-1);
Report2('Exports',C,'NA',T,RUN)$(EXPT(C,T)>=0)   = EXPT(C,T);
Report2('Exports',C,'NA',T,RUN)$(EXPT(C,T)<0)    = 0;
Report2('NetTrade',C,'NA',T,RUN)                 = Report2('Exports',C,'NA',T,RUN)-Report2('Imports',C,'NA',T,RUN);
Report2('ExchangeRate','NA','NA',T,RUN)          =EXRX('base',T,TT);

* LABOUR
Report2('Employment',A,FLAB,T,RUN)               = QFX(FLAB,A,'NAT','BASE',T,TT)   ;
Report2('AverageWage',A,FLAB,T,RUN)              = WFX(FLAB,'base',T,TT)*WFDISTX(FLAB,A,'nat','base',T,TT)  ;
Report2('WageBill',A,FLAB,T,RUN)                 = WFX(FLAB,'base',T,TT)*WFDISTX(FLAB,A,'nat','base',T,TT)*QFX(FLAB,A,'nat','base',T,TT)  ;

*FACTOR INCOME
Report2('FactorIncome',INS,F,T,RUN)                    = YIFX(INS,F,'base',T,TT)    ;

* PRICE, QUANTITY, OUTPUT
Report3('ComodityPrices',C,'NA',T,RUN)           = PQX(C,'BASE',T,TT) ;
Report3('ActivityPrices',A,'NA',T,RUN)           = PAX(A,'BASE',T,TT);

Report3('ComodityQuantity',C,'NA',T,RUN)         = PQX(C,'BASE',T,TT) ;
Report3('ActivityQuantitiy',A,'NA',T,RUN)        = PAX(A,'BASE',T,TT);
Report3('IntDemand',A,C,T,RUN)                   = QINTX(C,A,'base',T,TT)*PQI0(C,A,'nat') ;

);

execute_unload "Tableau.gdx" Report2;
execute 'gdxdump Tableau.gdx output=Report_00.csv symb=Report2 format=csv header="Indicator,AC,AC,Year,Scenario,SATIMGE"';

execute_unload "Tableau.gdx" Report3;
execute 'gdxdump Tableau.gdx output=Report_01.csv symb=Report3 format=csv header="Indicator,AC,AC,Year,Scenario,SATIMGE"';




$EXIT
