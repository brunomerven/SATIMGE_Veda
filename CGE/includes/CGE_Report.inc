Sets
* from model
 AC                      global set for model accounts - aggregated microsam accounts
 A(AC)                   activities
 C(AC)                   commodities
 F(AC)                   factors
 FLAB(F)                 labor
 FCAP(F)                 capital
 FLND(F)                 land
 INS(AC)                 institutions
 INSD(INS)               domestic institutions
 INSDNG(INSD)            domestic non-government institutions
 H(INSDNG)               households
 RD(AC)                  domestic or internal regions
 RW(AC)                  world trading regions
 EN(INSDNG)              Enterprises
 TX(AC)                  tax
 IT(AC)
 RUN                     simulations
 FS                      TIMES economic sectors
 PRC
 REG
 X                       simulations in eSAGE (CGE)
 XC(X)                   active simulations
 REG
 AY                      All Years /1850*2200, 0/
 T(AY)                   AY /2019*2055/
 TT(T)                   SATIM-eSAGE Iterations
 T1(AY)             base year /2019/

ST(AC)
YT(AC)
I(AC)
total(AC)
MX(AC)
other(AC)
MFSA(FS,A)
MPRCFS(PRC,FS)
EnergySupply /Hydrogen, Power, Refineries, Supply /

RUN /Reference,8GT,9GT/


* for microsim
Years(T)
l                        labour classes /f-labun, f-labsk/
mFl(F,l)                   Mapping F to l
;

Parameters
* from model
PQHX(C,H,X,T,TT)                 subsidized or taxed price of composite good c
PQX(C,X,T,TT)                    pre-subsidized or taxed price of composite good c
QQX(C,X,T,TT)
PAX(A,X,T,TT)                    output price of activities
QAX(A,X,T,TT)                    level of domestic activity
QINTX(C,A,X,T,TT)                Quantity of intermediate demand for c from activty a
PQI0(C,A,RD)                     PQ plus subsidy paid on c
PQ0(C)                           Price of composite goods
QH0(C,H)                         the quanity consumed of market commodity c by household h
EG0
QXX
QF0(F,A,RD)                      quantity demanded of factor f from activity a
SAM(AC,AC)
STAX(AC)
Mtax(AC)
QHX(C,H,X,T,TT)                  qnty consumed of market commodity c by household h
WFX(F,X,T,TT)                    economy-wide wage (rent) for factor f
WF0(F)
WFDISTX(F,A,RD,X,T,TT)           factor wage distortion variable
WFDIST0(F,A,RD)
QFSX(F,X,T,TT)                   qnty of factor supply
QFX(F,A,RD,X,T,TT)               qnty demanded of factor f from activity a
QEX(C,RW,X,T,TT)                 qnty of exports
QMX(C,RW,X,T,TT)                 qnty of imports
QE0(C,RW)
PEX(C,RW,X,T,TT)                 price of exports
QM0(C,RW)                        qnty of imports base year
PE0(C,RW)
PMX(C,RW,X,T,TT)                 price of imports
PM0(C,RW)
EXRX(X,T,TT)                     exchange rate
EGX(X,T,TT)                      total current government expenditure

GVA_FS(FS,T)                     SATIM Sector GVA
GDP_RUN(T)                          GDP Projection for RUN
POP(T)                              Population Projection
QGX(C,X,T,TT)                       qnty of government consumption
GSAVX(X,T,TT)                       government savings
QVAX(A,RD,X,T,TT)                   qnty of aggregate value added
PVA0(A,RD)
YGX(X,T,TT)                         total current government income
MPSX(INS,X,T,TT)                    marginal propensity to save for dom non-gov inst
TINSX(INS,X,T,TT)                   rate of direct tax on domestic institutions
YIX(INS,X,T,TT)                     income of domestic non-governmental institutions
YIFX(INS,F,X,T,TT)                  income of institutions ins from factor f
YG_ITAXX(X,T,TT)                    indirect current government income
TRNSFRX(INS,AC,X,T,TT)              transfers from institutions ins or factors ac to institutions
TAX(A,RD,X,T,TT)                    activity tax rate
TVAX(A,RD,X,T,TT)                   indirect value added tax rate
PVAX(A,RD,X,T,TT)                   value added price
PARX(A,RD,X,T,TT)                   output price of activity
QARX(A,RD,X,T,TT)                   level of domestic activity
TMX(C,RW,X,T,TT)                    import tax
TE(C,RW)
TQX(C,X,T,TT)
TFX(F,X,T,TT)                    factor tax rate
YFX(F,X,T,TT)                    factor income
PQGX
TVA0(A,RD)
TCST_INVC(REG,AY,PRC)
INVCOST(RUN,AY,PRC)
* tmp params
wagebill(F,A,T)                    wage bill by factor and activity
wagebill2(A,T)                   wage bill by aggregate factors
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
QINVX(C,IT,X,T,TT)
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

$gdxin  gdxs\Reference
$load AC,A,C,F,FLAB,FCAP,INS,INSD,INSDNG,H,RD,X,XC,TT,RW,FS,SAM,TX,MFSA,IT,FLND,PRC,REG, MPRCFS



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

* IMPORT  GDXS
* $gdxin  gdxs\Reference
* $gdxin  gdxs\8GT
* $gdxin gdxs\9GT



* $gdxin  gdxs\Reference
* $gdxin  gdxs\8GT
* $gdxin gdxs\9GT

execute_load GDP_RUN,POP,QGX,EGX,GSAVX,QVAX,PVA0,YGX,MPSX,TINSX,YIX,PQHX,PQX,QHX,WFX,WFDISTX,WFDIST0
execute_load QFSX,QFX,QEX,PEX,QMX,PMX,GVA_FS,EXRX,PM0,PE0,QE0,PAX,QAX,YIFX,trnsfrx,PARX,QARX,TAX,TVAX
execute_load PVAX,TMX,te,TQX,QQX,TFX,YFX,TVA0,QINTX,PQI0,QH0,PQ0,QM0,WF0,QF0,HPOPX,YG_ITAXX,TCST_INVC,INVCOST,QINVX    ;



GDPperCapita(T)$POP(T)   = GDP_RUN(T)/POP(T);
Expt(C,T)                = QEX(C,'rest','BASE',T,'2055')*PE0(C,'rest');
Imp(C,T)                 = QMX(C,'rest','BASE',T,'2055')*PM0(C,'rest');


;



* GENERAL ECONOMY
Report2('Population','NA','NA',T,RUN)            =POP(T)      ;
Report2('GVA',A,'NA',T,RUN)                      = QVAX(A,'NAT','BASE',T,'2055')*PVA0(A,'NAT');
Report2('NominalGDP',A,'NA',T,RUN)               = PVAX(A,'nat','base',T,'2055')*(1-TVAX(A,'nat','base',T,'2055'))*QVAX(A,'nat','base',T,'2055');
Report2('RealGDP',A,'NA',T,RUN)                  = PVA0(A,'nat')*(1-TVA0(A,'nat'))*QVAX(A,'nat','base',T,'2055');
Report2('GDPperCapita','NA','NA',T,RUN)          = GDPperCapita(T);

* HOUSEHOLDS
Report2('HHConsumption',C,H,T,RUN)               = QHX(C,H,'BASE',T,'2054')*PQHX(C,H,'BASE',T,'2054');
Report2('HHIncome',H,F,T,RUN)                    = YIFX(H,F,'base',T,'2054')              ;

*INVESTMENT
Report2('Investment',C,'NA',T,RUN)               = sum(IT,QINVX(C,IT,'base',T,'2054')*PQ0(C));
Report2('EngInv',EngSector,EngSubSector,T,RUN)   = SUM(PRC$(ENGSECT(PRC,EngSector,EngSubSector)),TCST_INVC('REGION1',T,PRC))    ;

* GOVERNMENT
Report2('GovRevenue','NA','NA',T,RUN)            = YGX('BASE',T,'2054') ;
Report2('GovExp','NA','NA',T,RUN)                = EGX('base',T,'2054');
Report2('GOVSaving','NA','NA',T,RUN)             = GSAVX('BASE',T,'2054');
Report2('GovDirTax',A,H,T,RUN)                   =  SUM(INSDNG, TINSX(INSDNG,'base',T,'2054')*YIX(INSDNG,'base',T,'2054')) ;
Report2('GovFacTax','NA',F,T,RUN)                = tfx(F,'base',T,'2054')*YFX(F,'base',T,'2054') ;
Report2('GovFacInc',H,F,T,RUN)                   =  SUM(INS, YIFX(INS,F,'Base',T,'2054')) ;
Report2('GovTrnsfr',AC,INS,T,RUN)                =  trnsfrx(INS,AC,'base',T,'2054')*EXRX('base',T,'2054');
Report2('GovActTax',A,'NA',T,RUN)                = TAX(A,'nat','base',T,'2054')*PAX(A,'base',T,'2054')*QAX(A,'base',T,'2054')  ;
Report2('GovVatTax',A,'NA',T,RUN)                = TVAX(A,'nat','base',T,'2054')*PVAX(A,'nat','base',T,'2054')*QVAX(A,'nat','base',T,'2054') ;
Report2('GovImpTax',C,'NA',T,RUN)                = TMX(C,'rest','base',T,'2054')*QMX(C,'rest','base',T,'2054')*PMX(C,'rest','base',T,'2054')*EXRX('base',T,'2054');
Report2('GovExpTax',C,'NA',T,RUN)                = te(C,'rest')*QEX(C,'rest','base',T,'2054')*PEX(C,'rest','base',T,'2054')*EXRX('base',T,'2054');
Report2('GovComTax',C,'NA',T,RUN)                = TQX(C,'base',T,'2054')*PQX(C,'base',T,'2054')*QQX(C,'base',T,'2054')   ;


* TRADE
Report2('Imports',C,'NA',T,RUN)$(EXPT(C,T)>=0)   = IMP(C,T)*(-1);
Report2('Imports',C,'NA',T,RUN)$(EXPT(C,T)<0)    = (IMP(C,T)-EXPT(C,T))*(-1);
Report2('Exports',C,'NA',T,RUN)$(EXPT(C,T)>=0)   = EXPT(C,T);
Report2('Exports',C,'NA',T,RUN)$(EXPT(C,T)<0)    = 0;
Report2('NetTrade',C,'NA',T,RUN)                 = Report2('Exports',C,'NA',T,RUN)-Report2('Imports',C,'NA',T,RUN);
Report2('ExchangeRate','NA','NA',T,RUN)          =EXRX('base',T,'2054');

* LABOUR
Report2('Employment',A,FLAB,T,RUN)               = QFX(FLAB,A,'NAT','BASE',T,'2054')   ;
Report2('AverageWage',A,FLAB,T,RUN)              = WFX(FLAB,'base',T,'2054')*WFDISTX(FLAB,A,'nat','base',T,'2054')  ;
Report2('WageBill',A,FLAB,T,RUN)                 = WFX(FLAB,'base',T,'2054')*WFDISTX(FLAB,A,'nat','base',T,'2054')*QFX(FLAB,A,'nat','base',T,'2054')  ;

*FACTOR INCOME
Report2('FactorIncome',INS,F,T,RUN)                    = YIFX(INS,F,'base',T,'2054')    ;

* PRICE, QUANTITY, OUTPUT
Report3('ComodityPrices',C,'NA',T,RUN)           = PQX(C,'BASE',T,'2054') ;
Report3('ActivityPrices',A,'NA',T,RUN)           = PAX(A,'BASE',T,'2054');

Report3('ComodityQuantity',C,'NA',T,RUN)         = PQX(C,'BASE',T,'2054') ;
Report3('ActivityQuantitiy',A,'NA',T,RUN)        = PAX(A,'BASE',T,'2054');
Report3('IntDemand',A,C,T,RUN)                   = QINTX(C,A,'base',T,'2054')*PQI0(C,A,'nat') ;

);

execute_unload "Tableau.gdx" Report2;
execute 'gdxdump Tableau.gdx output=Report_00.csv symb=Report2 format=csv header="Indicator,AC,AC,Year,Scenario,SATIMGE"';

execute_unload "Tableau.gdx" Report3;
execute 'gdxdump Tableau.gdx output=Report_01.csv symb=Report3 format=csv header="Indicator,AC,AC,Year,Scenario,SATIMGE"';




$EXIT
