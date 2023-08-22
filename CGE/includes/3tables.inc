SET
 SAC             SAM account categories  /
  ACT            activities
  COM            commodities
  FAC            factors
  INSDNG         domestic non-government institutions
  GOV            government
  SAVINV         savings-investments
  INSTAX         direct taxes on institutions
  FACTAX         direct taxes on factors
  VATAX          value added tax
  ACTTAX         tax on activity gross revenue
  COMTAX         tax on commodity sales in the domestic market
  IMPTAX         import tariff
  EXPTAX         export tax
  ROW            rest of world (FCU)
 /
 KGDP            first index in GDP tables  /
  NOMINAL, REAL, NOMSHARE, REALSHARE, DEFLATOR
 /
 KGDP2(KGDP)     Non shares for growth rates /
  NOMINAL, REAL, DEFLATOR
 /
 IGDP            Items for GDP and national accounts  /
  ABSORP         absorption
  PRVCON         private consumption
  FIXINV         fixed investment
  DSTOCK         stock change
  GOVCON         government consumption
  EXPORTS        exports
  IMPORTS        imports
  GDPMP          GDP at market prices (alt. 1: spending)
  GDPMP2         GDP at market prices (alt. 2: value-added)
  NETITAX        net indirect taxes
  GDPFC2         GDP at factor cost
 /
 IGDP2(IGDP)     Items for GDPMP summation  /
  PRVCON         private consumption
  FIXINV         fixed investment
  DSTOCK         stock change
  GOVCON         government consumption
  EXPORTS        exports
  IMPORTS        imports
 /
 MODTYPE         model types  /
  MCP            mixed-complementarity programming
  NLP            non-linear programming
 /
 SIND            Macro summary indicators /
  QABSTOT        real absorption (LCU at base prices)
  QHTOT          real household consumption (LCU at base prices)
  QINVTOT        real investment (LCU at base prices)
  QGTOT          real government consumption (LCU at base prices)
  QETOT          total real exports (LCU at base prices)
  QMTOT          total real imports (LCU at base prices)
  REXR           PPP real exchange rate (LCUs per FCU)
  NEXR           nominal exchange rate (LCUs per FCU)
  PWEIND         exports price index (FCU -- 100 for base)
  PWMIND         imports price index (FCU -- 100 for base)
  PWIND          world (tradables) price index (FCU -- 100 for base)
  PDIND          domestic (non-tradables) price index (100 for base)
  CPI            consumer price index (100 for base)
  TOFT           terms of trade (ratio pwe index & pwm index) (100 for base)
  INVGDP         investment (% of nominal GDP)
  PRVSAVGDP      private (household + enterprise) savings (% of nominal GDP)
  FORSAVGDP      foreign savings (% of nominal GDP)
  TRDDEFGDP      trade deficit (% of nominal GDP)
  GOVSAVGDP      government savings (% of nominal GDP)
  IMPTAXGDP      tariff revenue (% of nominal GDP)
  DIRTAXGDP      direct tax revenue (% of nominal GDP)
 /
 SINDDEV(SIND)   summary indicators showing deviation from BASE /
  INVGDP         investment (% of nominal GDP)
  PRVSAVGDP      private (household + enterprise) savings (% of nominal GDP)
  FORSAVGDP      foreign savings (% of nominal GDP)
  TRDDEFGDP      trade deficit  (% of nominal GDP)
  GOVSAVGDP      government savings (% of nominal GDP)
  IMPTAXGDP      import tax revenue (% of nominal GDP)
  DIRTAXGDP      direct tax revenue (% of nominal GDP)
 /
 SINDGR(SIND) /
  QABSTOT        real absorption (LCU at base prices)
  QHTOT          real household consumption (LCU at base prices)
  QINVTOT        real investment (LCU at base prices)
  QGTOT          real government consumption (LCU at base prices)
  QETOT          total real exports (LCU at base prices)
  QMTOT          total real imports (LCU at base prices)
 /
 SINDP(SIND)     summary indicators showing percent change
;

 SINDP(SIND)$(NOT SINDDEV(SIND)) = YES;

PARAMETERS
 pdwt(C)                       weight of commodity c in PD index
 pwewt(C,RW)                   weight of commodity c in pwe index
 pwmwt(C,RW)                   weight of commodity c in pwm index
 pwwt(*)                       weight of aggregate exports-imports in pw (tradables) index
 IC(H,X)                       indirect compensation fn by hhd h and Xulation X
 EV(H,X)                       equivalent variation by hhd h and Xulation X
 MACROTAB(SIND,X,TT)           macro table (whole period changes)
 MACROTABX(SIND,X,T,TT)        macro table data
 MACROTABXA(SIND,X,TT)         macro table (average annual changes)
 MACROTABXY(SIND,X,T,TT)       macro table (year-on-year changes)
 QHTAB(AC,X,TT)                disaggregated real household consumption
 EVTAB(AC,X,TT)                disaggregated EV
 YFTAB(AC,X,TT)                disaggregated factor income distribution
 QATAB(AC,X,TT)                disaggregated activity production levels
 PATAB(AC,X,TT)                disaggregated activity prices
 QHTABA(AC,X,TT)               disaggregated real household consumption
 EVTABA(AC,X,TT)               disaggregated EV
 YFTABA(AC,X,TT)               factor distribution
 QATABA(AC,X,TT)               disaggregated activity production levels
 PATABA(AC,X,TT)               disaggregated activity prices

 ALPHAVAXY(A,RD,X,T,TT)        productivity parameter in production function
 CPIXY(X,T,TT)                 consumer price index (PQ-based)
 DPIXY(X,T,TT)                 index for domestic producer prices (PDS-based)
 DMPSXY(X,T,TT)                change in maRDinal propensity to save for selected inst
 DTINSXY(X,T,TT)               change in domestic institution tax share
 EGXY(X,T,TT)                  total current government expenditure
 EHXY(H,X,T,TT)                household consumption expenditure
 EXRXY(X,T,TT)                 exchange rate
 FSAVXY(X,T,TT)                foreign savings
 GADJXY(X,T,TT)                government demand scaling factor
 GOVSHRXY(X,T,TT)              govt consumption share of absorption
 GSAVXY(X,T,TT)                government savings
 HPOPXY(H,X,T,TT)              household population growth
 IADJXY(IT,X,T,TT)             investment scaling factor (for fixed capital formation)
 INVSHRXY(X,T,TT)              investment share of absorption
 MPSXY(INS,X,T,TT)             maRDinal propensity to save for dom non-gov inst ins
 MPSADJXY(X,T,TT)              savings rate scaling factor
 PAXY(A,X,T,TT)                output price of activity a
 PARXY(A,RD,X,T,TT)            output price of activity a
 PDDXY(C,X,T,TT)               demand price for com'y c produced & sold domestically
 PDSXY(C,X,T,TT)               supply price for com'y c produced & sold domestically
 PEXY(C,RW,X,T,TT)             price of exports
 PINTAXY(A,RD,X,T,TT)          price of intermediate aggregate
 PMXY(C,RW,X,T,TT)             price of imports
 PQXY(C,X,T,TT)                price of composite good c
 PVAXY(A,RD,X,T,TT)            value added price
 PWEXY(C,RW,X,T,TT)            world price of exports
 PWMXY(C,RW,X,T,TT)            world price of imports
 PXXY(C,X,T,TT)                average output price
 PXACXY(A,C,X,T,TT)            price of commodity c from activity a
 QAXY(A,X,T,TT)                level of domestic activity
 QARXY(A,RD,X,T,TT)            level of domestic activity
 QDXY(C,X,T,TT)                qnty of domestic sales
 QEXY(C,RW,X,T,TT)             qnty of exports
 QFXY(F,A,RD,X,T,TT)           qnty demanded of factor f from activity a
 QFSXY(F,X,T,TT)               qnty of factor supply
 QGXY(C,X,T,TT)                qnty of government consumption
 QHXY(C,H,X,T,TT)              qnty consumed of market commodity c by household h
 QHAXY(A,H,X,T,TT)             qnty consumed of home commodity c fr act a by hhd h
 QINTXY(C,A,X,T,TT)            qnty of intermediate demand for c from activity a
 QINTAXY(A,RD,X,T,TT)          qnty of aggregate intermediate input
 QINVXY(C,IT,X,T,TT)           qnty of fixed investment demand
 QMXY(C,RW,X,T,TT)             qnty of imports
 QQXY(C,X,T,TT)                qnty of composite goods supply
 QTXY(C,X,T,TT)                qnty of trade and transport demand for commodity c
 QVAXY(A,RD,X,T,TT)            qnty of aggregate value added
 QXXY(C,X,T,TT)                qnty of aggregate marketed commodity output
 QXACXY(A,C,X,T,TT)            qnty of ouput of commodity c from activity a
 TABSXY(X,T,TT)                total absorption
 TINSXY(INS,X,T,TT)            rate of direct tax on domestic institutions ins
 TINSADJXY(X,T,TT)             direct tax scaling factor
 TAXY(A,RD,X,T,TT)             activity tax rate
 TMXY(C,RW,X,T,TT)             import tariff rate
 TQXY(C,X,T,TT)                indirect sales tax rate
 TRIIXY(INS,INSP,X,T,TT)       transfers to dom. inst. insdng from insdngp
 WFXY(F,X,T,TT)                economy-wide wage (rent) for factor f
 WFDISTXY(F,A,RD,X,T,TT)       factor wage distortion variable
 YFXY(F,X,T,TT)                factor income
 YGXY(X,T,TT)                  total current government income
 YIFXY(INS,F,X,T,TT)           income of institution ins from factor f
 YIXY(INS,X,T,TT)              income of (domestic non-governmental) institution ins

 ALPHAVAXA(A,RD,X,TT)          productivity parameter in production function
 CPIXA(X,TT)                   consumer price index (PQ-based)
 DPIXA(X,TT)                   index for domestic producer prices (PDS-based)
 DMPSXA(X,TT)                  change in maRDinal propensity to save for selected inst
 DTINSXA(X,TT)                 change in domestic institution tax share
 EGXA(X,TT)                    total current government expenditure
 EHXA(H,X,TT)                  household consumption expenditure
 EXRXA(X,TT)                   exchange rate
 FSAVXA(X,TT)                  foreign savings
 GADJXA(X,TT)                  government demand scaling factor
 GOVSHRXA(X,TT)                govt consumption share of absorption
 GSAVXA(X,TT)                  government savings
 HPOPXA(H,X,TT)                household population growth
 IADJXA(IT,X,TT)               investment scaling factor (for fixed capital formation)
 INVSHRXA(X,TT)                investment share of absorption
 MPSXA(INS,X,TT)               maRDinal propensity to save for dom non-gov inst ins
 MPSADJXA(X,TT)                savings rate scaling factor
 PAXA(A,X,TT)                  output price of activity a
 PARXA(A,RD,X,TT)              output price of activity a
 PDDXA(C,X,TT)                 demand price for com'y c produced & sold domestically
 PDSXA(C,X,TT)                 supply price for com'y c produced & sold domestically
 PEXA(C,RW,X,TT)               price of exports
 PINTAXA(A,RD,X,TT)            price of intermediate aggregate
 PMXA(C,RW,X,TT)               price of imports
 PQXA(C,X,TT)                  price of composite good c
 PVAXA(A,RD,X,TT)              value added price
 PWEXA(C,RW,X,TT)              world price of exports
 PWMXA(C,RW,X,TT)              world price of imports
 PXXA(C,X,TT)                  average output price
 PXACXA(A,C,X,TT)              price of commodity c from activity a
 QAXA(A,X,TT)                  level of domestic activity
 QARXA(A,RD,X,TT)              level of domestic activity
 QDXA(C,X,TT)                  qnty of domestic sales
 QEXA(C,RW,X,TT)               qnty of exports
 QFXA(F,A,RD,X,TT)             qnty demanded of factor f from activity a
 QFSXA(F,X,TT)                 qnty of factor supply
 QGXA(C,X,TT)                  qnty of government consumption
 QHXA(C,H,X,TT)                qnty consumed of market commodity c by household h
 QHAXA(A,H,X,TT)               qnty consumed of home commodity c fr act a by hhd h
 QINTXA(C,A,X,TT)              qnty of intermediate demand for c from activity a
 QINTAXA(A,RD,X,TT)            qnty of aggregate intermediate input
 QINVXA(C,IT,X,TT)             qnty of fixed investment demand
 QMXA(C,RW,X,TT)               qnty of imports
 QQXA(C,X,TT)                  qnty of composite goods supply
 QTXA(C,X,TT)                  qnty of trade and transport demand for commodity c
 QVAXA(A,RD,X,TT)              qnty of aggregate value added
 QXXA(C,X,TT)                  qnty of aggregate marketed commodity output
 QXACXA(A,C,X,TT)              qnty of ouput of commodity c from activity a
 TABSXA(X,TT)                  total absorption
 TINSXA(INS,X,TT)              rate of direct tax on domestic institutions ins
 TINSADJXA(X,TT)               direct tax scaling factor
 TAXA(A,RD,X,TT)               activity tax rate
 TMXA(C,RW,X,TT)               import tariff rate
 TQXA(C,X,TT)                  indirect sales tax rate
 TRIIXA(INS,INSP,X,TT)         transfers to dom. inst. insdng from insdngp
 WFXA(F,X,TT)                  economy-wide wage (rent) for factor f
 WFDISTXA(F,A,RD,X,TT)         factor wage distortion variable
 YFXA(F,X,TT)                  factor income
 YGXA(X,TT)                    total current government income
 YIFXA(INS,F,X,TT)             income of institution ins from factor f
 YIXA(INS,X,TT)                income of (domestic non-governmental) institution ins
 trnsfrxy(INS,AC,X,T,TT)
 trnsfrxa(INS,AC,X,TT)

 GDPTAB1(KGDP,IGDP,X,T,TT)     GDP and national accounts
 GDPTAB2(KGDP,AC,X,T,TT)       GDP at factor cost by activity
 GDPGAP(X,T,TT)                gaps in GDP reports
 GOVTAB(*,X,T,TT)              detailed reporting of government revenue

 NEGPWARN(C,X,T,TT)            comm c has one or more neg'e prices for sim'on sim
 NEGQWARN(C,X,T,TT)            comm c has one or more neg'e qnties for sim'on sim
 NEGWFWARN(F,X,T,TT)           factor f has one or more neg'e wages for sim'on sim
 NEGQFWARN(F,X,T,TT)           factor f has one or more neg'e qnties for sim'on sim
 GAPWARN(X,T,TT)               error if abs gap above cutoff
 GAPWARNING(X,T,TT)            error message if abs gap above limit
 WALRASWARN(X,T,TT)            error if abs value of WALRAS above cutoff
;



