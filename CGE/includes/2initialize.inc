PARAMETER
*Variables to be captured
 CPIX(X,T,TT)                     consumer price index (PQ-based)
 DEBTX(X,T,TT)                    accumulated foreign debt
 DPIX(X,T,TT)                     index for domestic producer prices (PDS-based)
 DMPSX(X,T,TT)                    change in marginal propensity to save for selected inst
 DTINSX(X,T,TT)                   change in domestic institution tax share
 EGX(X,T,TT)                      total current government expenditure
 EHX(H,X,T,TT)                    household consumption expenditure
 EXRX(X,T,TT)                     exchange rate
 FBORX(X,T,TT)                    annual foreign borrowing
 FSAVX(X,T,TT)                    foreign savings
 GADJX(X,T,TT)                    government demand scaling factor
 GOVSHRX(X,T,TT)                  govt consumption share of absorption
 GSAVX(X,T,TT)                    government savings
 HPOPX(H,X,AY,TT)                  household populations
 IADJX(IT,X,T,TT)                 investment scaling factor (for fixed capital formation)
 INVSHRX(X,T,TT)                  investment share of absorption
 MPSX(INS,X,T,TT)                 marginal propensity to save for dom non-gov inst ins
 MPSADJX(X,T,TT)                  savings rate scaling factor
 PAX(A,X,T,TT)                    output price of activity a
 PARX(A,RD,X,T,TT)                output price of activity a
 PDDX(C,X,T,TT)                   demand price for com'y c produced & sold domestically
 PDSX(C,X,T,TT)                   supply price for com'y c produced & sold domestically
 PEX(C,RW,X,T,TT)                 price of exports
 PINTAX(A,RD,X,T,TT)              price of intermediate aggregate
 PMX(C,RW,X,T,TT)                 price of imports
 PQX(C,X,T,TT)                    pre subsidized or taxed price of composite good c
 PQHX(C,H,X,T,TT)                 subsidized or taxed price of composite good c
 PQIX(C,A,RD,X,T,TT)              subsidized or taxed price of composite good c
 PVAX(A,RD,X,T,TT)                value added price
 PWEX(C,RW,X,T,TT)                world price of exports
 PWMX(C,RW,X,T,TT)                world price of imports
 PXX(C,X,T,TT)                    average output price
 PXACX(A,C,X,T,TT)                price of commodity c from activity a
 QAX(A,X,T,TT)                    level of domestic activity
 QARX(A,RD,X,T,TT)                level of domestic activity
 QDX(C,X,T,TT)                    qnty of domestic sales
 QEX(C,RW,X,T,TT)                 qnty of exports
 QFX(F,A,RD,X,T,TT)               qnty demanded of factor f from activity a
 QFSX(F,X,T,TT)                   qnty of factor supply
 QFS_FORX(F,X,T,TT)               qnty of emergency factor purchase
 QGX(C,X,T,TT)                    qnty of government consumption
 QHX(C,H,X,T,TT)                  qnty consumed of market commodity c by household h
 QHAX(A,H,X,T,TT)                 qnty consumed of home commodity c fr act a by hhd h
 QINTX(C,A,X,T,TT)                qnty of intermediate demand for c from activity a
 QINTAX(A,RD,X,T,TT)              qnty of aggregate intermediate input
 QINVX(C,IT,X,T,TT)               qnty of fixed investment demand
 QMX(C,RW,X,T,TT)                 qnty of imports
 QQX(C,X,T,TT)                    qnty of composite goods supply
 QTX(C,X,T,TT)                    qnty of trade and transport demand for commodity c
 QVAX(A,RD,X,T,TT)                qnty of aggregate value added
 QXX(C,X,T,TT)                    qnty of aggregate marketed commodity output
 QXACX(A,C,X,T,TT)                qnty of ouput of commodity c from activity a
 TABSX(X,T,TT)                    total absorption
 TINSX(INS,X,T,TT)                rate of direct tax on domestic institutions ins
 TINSADJX(X,T,TT)                 direct tax scaling factor
 TAX(A,RD,X,T,TT)                 activity tax rate
 TAADJX(X,T,TT)                   activity tax scaling factor
 TAPSX(X,T,TT)                    point change in activity tax rate
 TMX(C,RW,X,T,TT)                 import tariff rate
 TFX(F,X,T,TT)                    factor tax rate
 TRNADJX(X,T,TT)                  scaling factor for gov transfers to institutions
 TQADJX(X,T,TT)                   sales tax scaling factor
 TQX(C,X,T,TT)                    indirect sales tax rate
 TQPSX(X,T,TT)                    point change in sales tax rate
 TVAADJX(X,T,TT)                  value-added tax scaling factor
 TVAX(A,RD,X,T,TT)                indirect value-added tax rate
 TVAPSX(X,T,TT)                   point change in value-added tax rate
 TRIIX(INS,INSP,X,T,TT)           transfers to dom. inst. insdng from insdngp
 WALRASX(X,T,TT)                  savings-investment imbalance (should be zero)
 WFX(F,X,T,TT)                    economy-wide wage (rent) for factor f
 WFDISTX(F,A,RD,X,T,TT)           factor wage distortion variable
 YFX(F,X,T,TT)                    factor income
 YGX(X,T,TT)                      total current government income
 YG_DTAXX(X,T,TT)               direct current government income
 YG_ITAXX(X,T,TT)               indirect current government income
 YG_NTAXX(X,T,TT)               other current government income - should be 0
 YIFX(INS,F,X,T,TT)               income of institution ins from factor f
 YIX(INS,X,T,TT)                  income of (domestic non-governmental) institution ins
 WALRASX(X,T,TT)                  walras law value
*Parameters to be captured
 alphavax(A,RD,X,T,TT)
 leovax(A,RD,X,T,TT)
 fprdx(F,A,RD,X,T,TT)
 trnsfrx(INS,AC,X,T,TT)
 BETAMX(C,H,X,T,TT)               household marketed consumption shares
*Parameters to be saved
 QFS00(F)                      base year factor supply numbers (used for upward-sloping labor supply curves)

 tco2dx(X,T,TT)
 tco2ex(X,T,TT)
 tco2mx(X,T,TT)
 rtco2ex(X,T,TT)
 co2cx(C,X,T,TT)
 co2ex(C,RW,X,T,TT)
 co2mx(C,RW,X,T,TT)

 intax(A,RD,X,T,TT)
 ivax(A,RD,X,T,TT)
 ifax(F,A,RD,X,T,TT)

 DTUX(C,X,T,TT)

 QFSUPX(F,X,T,TT)           share of installed capital used (if less than 1)
 ALPHAQFX(X,T,TT)           proportional change in regular capital stock
 QINVKX(X,T,TT)             total units of capital available for allocation to sectors
 NGFCFX(X,T,TT)             net units of capital after deduction for fixed growth sectors

*CA more tracking of times
 EINVQINV(X,T,TT)           electricity investment cost in capital units
 SHREINVQINV(X,T,TT)        Times driven share of electricity investment in total

  DKAPX(FCAP,A,RD,X,T,TT)
  WFDIST2X(FCAP,A,RD,X,T,TT)
  INVSHR2X(FCAP,A,RD,X,T,TT)

  CTA_EMX(A,RUN,X,T,TT)
  CTA_EMRX(A,RUN,X,T,TT)
  CTA_EMRRX(A,RUN,X,T,TT)

*fh: EV shock
 deltaqx(C,RW,RUN,X,T,TT)
 deltatx(C,RW,RUN,X,T,TT)

 thetax(A,C,RUN,T) yield of commodity C per unit of activity A
 pacoalx(T,RUN) output price of acoal just for coal
;

 QFS00(F) = QFS0(F);
 TAPSX(X,T,TT) = 0;
 GSAVX(X,T,TT) = 0;

*CA set  CPIX(X,T,TT) to base value
  CPIX(XC,TC,TT) =cpi0;

*CA initialize TABSX
  TABSX(XC,T1,TT)=TABS0;

*CA initialize qfsupx
  QFSUPX(F,XC,TC,TT)=1;

*CA initialize wfx
  WFX(F,XC,TC,TT)$WF0(F)=1;
  QFSX(F,XC,TC,TT)$QFS0(F)=QFS0(F);

