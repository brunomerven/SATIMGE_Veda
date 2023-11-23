
* GENERAL ECONOMY
Report2('Population','NA','NA',T,RUN)            =POP(T)      ;
Report2('GVA_NOM',A,'NA',T,RUN)               = PVAX(A,'nat',XC,T,TT)*(1-TVAX(A,'nat',XC,T,TT))*QVAX(A,'nat',XC,T,TT);
Report2('GVA',A,'NA',T,RUN)                  = PVA0(A,'nat')*(1-TVA0(A,'nat'))*QVAX(A,'nat',XC,T,TT);

* GDP at factor costs is sum of GVA
Report2('GDPFC','Total','NA',T,RUN)          = sum(A,Report2('GVA',A,'NA',T,RUN);

Report2('GDPperCapita','NA','NA',T,RUN)          = Report2('GDPFC','Total','NA',T,RUN)/POP(T);

* HOUSEHOLDS
Report2('Consumption',C,H,T,RUN)               = QHX(C,H,XC,T,TT)*PQHX(C,H,XC,T,TT);
Report2('FactorIncome',H,F,T,RUN)                    = YIFX(H,F,XC,T,TT)              ;
Report2('Savings',INSDNG,F,T,RUN)                    = MPSX(INSDNG,XC,TT,T) * (1 - TINSX(INSDNG,XC,TC,TT)) * YIX(INSDNG,XC,TC,TT))              ;
Report2('Transfers',INS,AC,T,RUN) = TRNSFRX(INS,AC,XC,TC,TT);
*INVESTMENT
Report2('Investment',C,'NA',T,RUN)               = sum(IT,QINVX(C,IT,XC,T,TT)*PQ0(C));
Report2('TotalInvestment','Total','NA',T,RUN) = SUM(C,Report2('Investment',C,'NA',T,RUN));

* Capital formation or savings. INSDNG are households and enterprises.
* Total CapitalFormation is the equal to TotalInvestment
Report2('CapitalFormation','NA',INSDNG,T,RUN) = SUM(INSDNG, MPSX(INSDNG,XC,TT,T) * (1 - TINSX(INSDNG,XC,TC,TT)) * YIX(INSDNG,XC,TC,TT)));
Report2('CapitalFormation','NA','gov',T,RUN) = GSAVX(XC,T,TT);
Report2('CapitalFormation','NA','row',T,RUN) = (FSAVX(XC,T,TT)-irate*DEBTX(XC,T,TT)*EXRX(XC,T,TT);


Report2('CapitalAlloc',AFX,'NA',T,RUN) = QFX('fcap',AFX,'nat',XC,TC,TT)*(AFXGR(AFX,'nat',XC,TC) + natdrate))*(SUM(C$PQ0(C), iwts(C,'S-I')*PQX(C,XC,T,TT)/PQ0(C)));
Report2('CapitalAlloc','aelec','NA',T,RUN) = EINVQINV(XC,TC,TT);
* aelec is part of AFX but not included in QFX('fcap'). INVSHR2 is share of ANFX sectors
Report2('CapitalAlloc',ANFX,'NA',T,RUN) = INVSHR2X('fcap',ANFX,'nat',XC,TC,TT)*(Report2('TotalInvestment','Total','NA',T,RUN)-sum(AFX,Report2('CapitalAlloc',AFX,'NA',T,RUN)));

* GOVERNMENT
Report2('GovRevenue','NA','NA',T,RUN)            = YGX(XC,T,TT) ;
Report2('GovExp','NA','NA',T,RUN)                = EGX(XC,T,TT);
Report2('GovSaving','NA','NA',T,RUN)             = GSAVX(XC,T,TT);

Report2('GovDirTax',A,H,T,RUN)                   =  SUM(INSDNG, TINSX(INSDNG,XC,T,TT)*YIX(INSDNG,XC,T,TT)) ;
Report2('GovFacTax','NA',F,T,RUN)                = tfx(F,XC,T,TT)*YFX(F,XC,T,TT) ;
Report2('GovFacInc',H,F,T,RUN)                   =  SUM(INS, YIFX(INS,F,XC,T,TT)) ;
Report2('GovTrnsfr',AC,INS,T,RUN)                =  trnsfrx(INS,AC,XC,T,TT)*EXRX(XC,T,TT);
Report2('GovActTax',A,'NA',T,RUN)                = TAX(A,'nat',XC,T,TT)*PAX(A,XC,T,TT)*QAX(A,XC,T,TT)  ;
Report2('GovVatTax',A,'NA',T,RUN)                = TVAX(A,'nat',XC,T,TT)*PVAX(A,'nat',XC,T,TT)*QVAX(A,'nat',XC,T,TT) ;
Report2('GovImpTax',C,'NA',T,RUN)                = TMX(C,'rest',XC,T,TT)*QMX(C,'rest',XC,T,TT)*PMX(C,'rest',XC,T,TT)*EXRX(XC,T,TT);
Report2('GovExpTax',C,'NA',T,RUN)                = te(C,'rest')*QEX(C,'rest',XC,T,TT)*PEX(C,'rest',XC,T,TT)*EXRX(XC,T,TT);
Report2('GovComTax',C,'NA',T,RUN)                = TQX(C,XC,T,TT)*PQX(C,XC,T,TT)*QQX(C,XC,T,TT)   ;


* TRADE
Report2('Imports',C,'NA',T,RUN)$(EXPT(C,T)>=0)   = IMP(C,T)*(-1);
Report2('Imports',C,'NA',T,RUN)$(EXPT(C,T)<0)    = (IMP(C,T)-EXPT(C,T))*(-1);
Report2('Exports',C,'NA',T,RUN)$(EXPT(C,T)>=0)   = EXPT(C,T);
Report2('Exports',C,'NA',T,RUN)$(EXPT(C,T)<0)    = 0;
Report2('NetTrade',C,'NA',T,RUN)                 = Report2('Exports',C,'NA',T,RUN)-Report2('Imports',C,'NA',T,RUN);
Report2('ExchangeRate','NA','NA',T,RUN)          =EXRX(XC,T,TT);

* LABOUR
Report2('Employment',A,FLAB,T,RUN)               = QFX(FLAB,A,'NAT',XC,T,TT)   ;
Report2('AverageWage',A,FLAB,T,RUN)              = WFX(FLAB,XC,T,TT)*WFDISTX(FLAB,A,'nat',XC,T,TT)  ;
Report2('WageBill',A,FLAB,T,RUN)                 = WFX(FLAB,XC,T,TT)*WFDISTX(FLAB,A,'nat',XC,T,TT)*QFX(FLAB,A,'nat',XC,T,TT)  ;

*FACTOR INCOME
Report2('FactorIncome',INS,F,T,RUN)                    = YIFX(INS,F,XC,T,TT)    ;

Expt(C,T)                = QEX(C,'rest',XC,T,TT)*PE0(C,'rest');
Imp(C,T)                 = QMX(C,'rest',XC,T,TT)*PM0(C,'rest');


* PRICE, QUANTITY, OUTPUT
Report3('ComodityPrices',C,'NA',T,RUN)           = PQX(C,XC,T,TT) ;
Report3('ActivityPrices',A,'NA',T,RUN)           = PAX(A,XC,T,TT);

Report3('ComodityQuantity',C,'NA',T,RUN)         = PQX(C,XC,T,TT) ;
Report3('ActivityQuantitiy',A,'NA',T,RUN)        = PAX(A,XC,T,TT);
Report3('IntDemand',A,C,T,RUN)                   = QINTX(C,A,XC,T,TT)*PQI0(C,A,'nat') ;

);

execute_unload "Tableau.gdx" Report2;
execute 'gdxdump Tableau.gdx output=Report_00.csv symb=Report2 format=csv header="Indicator,AC,AC,Year,Scenario,SATIMGE"';

execute_unload "Tableau.gdx" Report3;
execute 'gdxdump Tableau.gdx output=Report_01.csv symb=Report3 format=csv header="Indicator,AC,AC,Year,Scenario,SATIMGE"';




$EXIT
