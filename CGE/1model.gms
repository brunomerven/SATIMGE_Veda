$onempty

*gdx=model s=model
*--------------------------------------------------------------------------------------------
*1. SETS
*--------------------------------------------------------------------------------------------

*Section 1: Set names for input data include file and output spreadsheets
$SETGLOBAL countrydata "1model.dat"

*Section 1: Excel data file in XXX.dat include file
$SETGLOBAL modeldata    "1model.xlsx"

*Section 2: Base dynamic calibration file
$SETGLOBAL basedata  "2simulation.xlsx"

*Section 3: Reporting file defining table dimenions
$SETGLOBAL reportdata  "3report.xlsx"

SETS
*a. model sets
 AC                      global set for model accounts - aggregated microsam accounts
 ACNT(AC)                all elements in AC except TOTAL
 RD(AC)                  domestic or internal regions
 RW(AC)                  world trading regions
 A(AC)                   activities
 ARD(AC)                 input in the form A*_RD* (activity*region)
 C(AC)                   commodities
 CA(C)                   aggregate commodities
 CD(C)                   commodities with domestic sales of output
 CDN(C)                  commodities without domestic sales of output
 CE(C)                   exported commodities
 CERW(C,RW)              exported commodities
 CEN(C)                  non-export commodities
 CM(C)                   imported commodities
 CMRW(C,RW)              imported commodities
 CMN(C)                  non-imported commodities
 CX(C)                   commodities with output
 F(AC)                   factors
 FLS(F)                  factors with upward-sloping labor supply curves
 FA(F)                   aggregate factors
 FD(F)                   disaggregate factors
 FLAB(F)                 labor
 FCAP(F)                 capital
 FLND(F)                 land
 INS(AC)                 institutions
 INSD(INS)               domestic institutions
 INSDNG(INSD)            domestic non-government institutions
 H(INSDNG)               households
 EN(INSDNG)              enterprises
 IT(AC)                  investment demand categories
*b. calibration sets
 CT(C)                   transaction service commodities
 CTD(AC)                 domestic transactions cost account
 CTE(AC)                 export transactions cost account
 CTM(AC)                 import transactions cost account
 ATAX(AC)                activity taxes
 DTAX(AC)                direct taxes
 ETAX(AC)                export taxes
 FTAX(AC)                factor taxes
 MTAX(AC)                import taxes
 STAX(AC)                sales taxes
 VTAX(AC)                value added taxes
 ACES2(A)                activities with CES activity output aggregation function
 ACET2(A)                activities with CET activity output disaggregation function
 AFLEO(A)                activities with leontief factor demand
 ACO2(A)                 source carbon emissions activities
 CCO2(C)                 source carbon emissions commodities
*c. mappings
 MAC(A,C)                mapping between activities and commodities
 MARD(ARD,A,RD)          mapping between SAM regions
;

*--------------------------------------------------------------------------------------------
*2. DATABASE
*--------------------------------------------------------------------------------------------

PARAMETER
 SAM(AC,AC)              standard SAM
 SAMBALCHK(AC)           column minus row total for SAM
;

$INCLUDE %countrydata%

*SAM adjustments ----------------------------------------------

*Adjustment for sectors with only exports and no domestic sales.
*If there is a very small value for domestic sales, add the discrepancy
*to exports.
 SAM(C,'ROW')$(ABS(SUM(ARD, SAM(ARD,C)) - (SAM(C,'ROW') - TAXPAR('EXPTAX',C) - SUM(CTE, SAM(CTE,C))) ) LT 1.E-6)
                 = SUM(ARD, SAM(ARD,C)) +  TAXPAR('EXPTAX',C) + SUM(CTE, SAM(CTE,C)) ;

*Netting transfers between domestic institutions and RoW.
 SAM(INSD,'ROW')   = SAM(INSD,'ROW') - SAM('ROW',INSD);
 SAM('ROW',INSD)   = 0;

*Netting transfers between factors and RoW.
 SAM('ROW',F)  = SAM('ROW',F) - SAM(F,'ROW');
 SAM(F,'ROW')  = 0;

*Netting transfers between government and domestic non-
*government institutions.
 SAM(INSDNG,'GOV') = SAM(INSDNG,'GOV') - SAM('GOV',INSDNG);
 SAM('GOV',INSDNG) = 0;

*Eliminating payments of any account to itself.
 SAM(ACNT,ACNT) = 0;

*Checking SAM balance -----------------------------------------

*Account totals are recomputed. Check for SAM balance.
 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));
 SAMBALCHK(AC)   = SAM('TOTAL',AC) - SAM(AC,'TOTAL');
 SAMBALCHK(AC)$(abs( SAMBALCHK(AC)) lt 1e-6) = 0;

*Additional set definitions based on country SAM======================

 CD(C)  = YES$(SUM(ARD, SAM(ARD,C)) GT (SAM(C,'ROW') - TAXPAR('EXPTAX',C) - SUM(CTE, SAM(CTE,C))) );
 CDN(C) = NOT CD(C);

 CE(C)  = YES$(SAM(C,'ROW'));
 CEN(C) = NOT CE(C);
 CERW(C,RW)$(CE(C) AND REGEXP(C,RW)) = YES;

 CM(C)  = YES$(SAM('ROW',C));
 CMN(C) = NOT CM(C);
 CMRW(C,RW)$(CM(C) AND REGIMP(C,RW)) = YES;

 CX(C) = YES$SUM(ARD, SAM(ARD,C));

 CT(C)$(SUM(CTD, SAM(C,CTD)) + SUM(CTE, SAM(C,CTE)) + SUM(CTM, SAM(C,CTM))) = YES;

*Fine-tuning non-SAM data -------------------------------------

*Eliminating superfluous elasticity data

 AELASTAB(C,'SIGMAT')$(CEN(C) OR (CE(C) AND CDN(C))) = 0;
 AELASTAB(C,'SIGMAQ')$(CMN(C) OR (CM(C) AND CDN(C))) = 0;

* DELASTAB(ARD,'PRODE')$(NOT SAM('TOTAL',ARD)) = 0;

 AELASTAB(C,'OUTAGG')$(NOT SUM(ARD, SAM(ARD,C)))     = 0;

 YELASTAB(C,H)$(NOT SAM(C,H)) = 0;
 HELAS(A,H)$(NOT SUM((ARD,RD)$MARD(ARD,A,RD), SAM(ARD,H)))  = 0;

*Diagnostics --------------------------------------------------

*Include file that displays and generates information that may be
*useful when debugging data set.

$INCLUDE includes\1diagnostics.inc

*Physical factor quantities -----------------------------------

*If there is a SAM payment from A to F and supply (but not
*demand) quantities have been defined in the country data file,
*then the supply values are used to compute demand quantities.
 QF2BASE(F,ARD)$(SAM(F,ARD)$((NOT QFBASE(F,ARD)) AND QFSBASE(F)))
   = QFSBASE(F)*SAM(F,ARD)/SUM(ARDP, SAM(F,ARDP));

*If there is a SAM payment from A to F and neither supply nor
*demand quantities have been defined in the country data file,
*then SAM values are used as quantities
 QF2BASE(F,ARD)$(SAM(F,ARD)$((QFBASE(F,ARD) EQ 0) AND (QFSBASE(F) EQ 0))) = SAM(F,ARD);

*If there is a SAM payment from A to F and demand quantities have
*been defined in the country data file, then this information is used.
 QF2BASE(F,ARD)$QFBASE(F,ARD) = QFBASE(F,ARD);


*--------------------------------------------------------------------------------------------
*3. PARAMETER DECLARATIONS
*--------------------------------------------------------------------------------------------

PARAMETERS
*a. Parameters appearing in model equations
*Parameters other than tax rates
 alphaac(C)              shift parameter for domestic commodity aggregation fn
 alphaq(C)               shift parameter for Armington function
 alphat(C)               shift parameter for CET function
 alphava(A,RD)           shift parameter for CES activity production function
 leova(A,RD)             shift parameter for Leontief production function
 alphava2(F,A,RD)        lower level factor nesting parameter
 alphaa2(A)              shift parameter on ces activity output aggregation function
 alphaca(A)              shift parameter on ces activity output disaggregation function
 beta1                   capital mobility parameter by type                      / 0.75 /
 beta2(F)                capital mobility by sector
 betah(A,H)              marg shr of hhd cons on home com c from act a
 betam(C,H)              marg share of hhd cons on marketed commodity c
 betam0(C,H)             marg share of hhd cons on marketed commodity c
 cwts(C)                 consumer price index weights
 cwtsh(AC,H)             consumer price index weight for com'y c or act a for hhd h
 debt                    accumulated foreign debt
 debt0                   accumulated foreign debt
 deltaac(A,C)            share parameter for domestic commodity aggregation fn
 deltaq(C,RW)            share parameter for Armington function
 deltat(C,RW)            share parameter for CET function
 deltava(F,A,RD)         share parameter for CES activity production function
 deltava2(F,FP,A,RD)     lower level factor nesting parameter
 deltaa2(A,RD)           share parameters on the activity output aggregation function
 deltaca0(A,C)           base value for share parameter for CET output disaggregation
 deltaca(A,C)            share parameter for CET output disaggregation
 dwts(C)                 domestic sales price weights
 fprd(F,A,RD)            factor-specific productivity
 gammah(A,H)             per-cap subsist cons for hhd h on home com c fr act a
 gammam(C,H)             per-cap subsist cons of marketed com c for hhd h
 hpop(H)                 household populations
 ica(C,A,RD)             intermediate input c per unit of aggregate intermediate
 ica0(C,A,RD)            base intermediate input c per unit of aggregate intermediate
 inta(A,RD)              aggregate intermediate input coefficient
 inta0(A,RD)             aggregate intermediate input coefficient
 iva(A,RD)               aggregate value added coefficient
 iva0(A,RD)               aggregate value added coefficient
 icd(C,CP)               trade input of c per unit of comm'y cp produced & sold dom'ly
 ice(C,CP)               trade input of c per unit of comm'y cp exported
 icm(C,CP)               trade input of c per unit of comm'y cp imported
 ifa(F,A,RD)             leontief factor demand shares
 ifa0(F,A,RD)            leontief factor demand shares
 irate                   interest rate on foreign rate
 iwts(C,IT)              investment commodity demand weight
 mps01(INS)              0-1 par for potential flexing of savings rates
 mpsbar(INS)             marg prop to save for dom non-gov inst ins (exog part)
 qdst(C)                 inventory investment by sector of origin
 qbarg(C)                exogenous (unscaled) government demand
 qbarinv(C,IT)           exogenous (unscaled) investment demand
 rf(F)                   factor foreign remittances
 rhoac(C)                domestic commodity aggregation function exponent
 rhof(F)                 labor supply elasticity (upward-sloping supply curves)
 rhoq(C)                 Armington function exponent
 rhot(C)                 CET function exponent
 rhova(A,RD)             CES activity production function exponent
 rhova2(F,A,RD)          lower level factor nesting parameter
 rhoa2(A)                CES activity output aggregation function exponent
 rhoca(A)                CET activity disaggregation function exponent
 elasva(A,RD)            CES elasticity of substitution in production
 pwebar(C,RW)            world price of exports
 pwmbar(C,RW)            world price of imports
 shif(INS,F)             share of dom. inst'on i in income of factor f
 shii(INS,INSP)          share of inst'on i in post-tax post-sav income of inst ip
 supernum(H)             LES supernumerary income
 theta(A,C)              yield of commodity C per unit of activity A
 theta0(A,C)             yield of commodity C per unit of activity A
 ta01(A,RD)              0-1 par for potential flexing of activity tax rates
 tins01(INS)             0-1 par for potential flexing of dir tax rates
 trnsfr(INS,AC)          transfers fr. inst. or factor ac to institution ins
 trn01(INS)
 tq01(C)                 0-1 par for potential flexing of sales tax rates
 tva01(A,RD)             0-1 par for potential flexing of value-added tax rates
*Tax rates
 tabar(A,RD)             rate of tax on activity
 tqbar(C)                rate of tax on sale of commodity
 te(C,RW)                rate of tax on exports
 tf(F)                   rate of direct tax on factors (soc sec tax)
 tinsbar(INS)            rate of (exog part of) direct tax on dom inst ins
 tm(C,RW)                rate of import tariff
 tvabar(A,RD)            rate of value-added tax
 tco2d0                  rate of carbon tax (domestic demand)
 rtco2e0                 retaliatory rate of carbon tax (export)
 tco2e0                  rate of carbon tax (export)
 tco2m0                  rate of carbon tax (import)
 co2c0(C)                domestic carbon content coefficient
 co2e0(C,RW)             export carbon content coefficient
 co2m0(C,RW)             import carbon content coefficient
 tco2d                   rate of carbon tax (domestic demand)
 tco2e                   rate of carbon tax (export)
 rtco2e                  retaliatory rate of carbon tax (export)
 tco2m                   rate of carbon tax (import)
 co2c(C)                 domestic carbon content coefficient
 co2e(C,RW)              export carbon content coefficient
 co2m(C,RW)              import carbon content coefficient
*b. Parameters used for model calibration
*Parameters for definition of model parameters
 alphainv                investment shift parameter
 alphaq0(C)              shift parameter for Armington function
 alphat0(C)              shift parameter for CET function
 alphava0(A,RD)          shift parameter for CES activity production function
 leova0(A,RD)            shift parameter for Leontief production function
 deltaq0(C,RW)           share parameter for Armington function
 deltat0(C,RW)           share parameter for CET function
 qdst0(C)                stock change
 qbarg0(C)               exogenous (unscaled) government demand
 gammah0(A,H)            per-cap subsist cons for hhd h on home com c fr act a
 gammam0(C,H)            per-cap subsist cons of marketed com c for hhd h
 te0(C,RW)               rate of tax on exports
 tf0(F)                  rate of direct tax on factors -- soc sec tax
 tins0(INS)              rate of direct tax on domestic institutions ins
 tm0(C,RW)               rate of import tariff
 trnsfr0(INS,AC)         transfers fr. inst. or factor ac to institution ins
 tvabar0(A,RD)           rate of value-added tax
*Check parameters
 cwtschk                 check that CPI weights sum to unity
 cwtshchk(h)             check that CPI weights sum to unity for hhd h
 dwtschk                 check that PDIND weights sum to unity
 shifchk(F)              check that factor payment shares sum to unity
*Parameters for variable initialization
 CPI0                    consumer price index -- PQ-based
 DPI0                    index for domestic producer prices --PDS-based
 DMPS0                   change in marginal propensity to save for selected inst
 DTINS0                  change in domestic institution tax share
 EG0                     total current government expenditure
 EH0(H)                  household consumption expenditure
 EXR0                    exchange rate
 FBOR0                   foreign borrowing
 FSAV0                   foreign savings
 GADJ0                   government demand scaling factor
 GOVSHR0                 govt consumption share of absorption
 GSAV0                   government savings
 IADJ0(IT)               investment scaling factor for fixed capital formation
 INVSHR0                 investment share of absorption
 MPS0(INS)               marginal propensity to save for dom non-gov inst ins
 MPSADJ0                 savings rate scaling factor
 PA0(A)                  output price of aggregate national a
 PAR0(A,RD)              output price of region specific activity
 PDD0(C)                 demand price for com'y c produced & sold domestically
 PDS0(C)                 supply price for com'y c produced & sold domestically
 PE0(C,RW)               price of exports
 PWE0(C,RW)              world price of exports
 PWM0(C,RW)              world price of imports
 PINTA0(A,RD)            price of intermediate aggregate
 PM0(C,RW)               price of imports
 PQ0(C)                  price of composite good c
 PVA0(A,RD)              value added price
 PX0(C)                  average output price
 PXAC0(A,C)              price of commodity c from activity a
 QA0(A)                  level of domestic activity nationally
 QAR0(A,RD)              level of domestic activity regionally
 QANET0(A)               QA net of home consumption
 QD0(C)                  quantity of domestic sales
 QE0(C,RW)               quantity of exports
 QF0(F,A,RD)             quantity demanded of factor f from activity a
 QFBAR(F,A,RD)           factor demand base to adjust from
 QFS0(F)                 quantity of factor supply
 QFS_BAR(F)              quantity of factor supply (drawn from base run)
 QG0(C)                  quantity of government consumption
 QH0(C,H)                quantity consumed of marketed commodity c by hhd h
 QHA0(A,H)               quantity consumed of home commodity c fr act a by hhd h
 QINT0(C,A)              quantity of intermediate demand for c from activity a
 QINTA0(A,RD)            quantity of aggregate intermediate input
 QINV0(C,IT)             quantity of fixed investment demand
 QM0(C,RW)               quantity of imports
 QQ0(C)                  quantity of composite goods supply
 QT0(C)                  quantity of trade and transport demand for commodity c
 QVA0(A,RD)              quantity of aggregate value added
 QX0(C)                  quantity of aggregate marketed commodity output
 QXAC0(A,C)              quantity of ouput of commodity c from activity a
 TA0(A,RD)               rate of tax on producer gross output value
 TAADJ0                  activity tax scaling factor
 TABS0                   total absorption
 TAPS0                   point change in activity tax rate
 TINS0(INS)              rate of direct tax on domestic institutions ins
 TINSADJ0                direct tax scaling factor
 TRNADJ0                 scaling factor for government transfers to domestic non-gov institutions
 TQ0(C)                  rate of sales tax
 TQADJ0                  sales tax scaling factor
 TQPS0                   point change in sales tax rate
 TQELEC0(C)              point change in sales tax rate that avoids tq01(C)
 TRII0(INS,INSP)         transfers to dom. inst. insdng from insdngp
 TVAADJ0                 value-added tax scaling factor
 TVAPS0                  point change in value-added tax rate
 TVA0(A,RD)              value-added tax rate
 WALRAS0                 savings-investment imbalance (should be zero)
 WF0(F)                  economy-wide wage (rent) for factor f
 WF_BAR(F)               economy-wide wage (rent) for factor f (tracked through base run)
 WFDIST0(F,A,RD)         factor wage distortion variable
 YF0(F)                  factor income
 YG0                     total current government income
 YIF0(INS,F)             income of institution ins from factor f
 YI0(INS)                income of (domestic non-governmental) institution ins
*Capital stock updating parameters (only used in the simulation file)
 CAPSHR1(F)              shares of aggregate capital by type (sums to one)
 CAPSHR2(F,A,RD)         sectoral shares of capital by type (rows sum to one)
 CAPSHR1TOT              used to speed up capital accumulation calculations
 CAPSHR2TOT(F)           used to speed up capital accumulation calculations
 DKAP(F,A,RD)            change in sectoral real capital stock
 DKAPS(F)                change in aggregate real capital stock
 INVSHR1(F)              investment shares by type of capital
 INVSHR2(F,A,RD)         investment shares by sector for each capital type
 NGFCF                   GFCF net of exogenous capital adjustments in fixed sectors
 QINVK                   quantity of new capital stock
 RKAP(F,A,RD)            annual rate of growth of sectoral capital stock by type
 RKAPS(F)                annual rate of growth of aggregate capital stock by type
 WFADJ(F)                WF adjusted to exclude fixed sectors
 WFK1AV                  average rental on all capital (economywide)
 WFK2AV(F)               average rental on capital by type (across all activities)
 WFK2AV0(F)              initial average rental on capital by type (across all activities)
 WFDIST2(F,A,RD)         ratio of sectoral to average rental by capital type
 WFDISTADJ(F,A,RD)       WFDIST adjusted to exclude fixed sectors
*Calibration parameters
 PSUP(C)                 initial supply-side market price for commodity c
 SHCTD(C)                share of comm'y ct in trans services for domestic sales
 SHCTM(C)                share of comm'y ct in trans services for imports
 SHCTE(C)                share of comm'y ct in trans services for exports
 WFA(F,A,RD)             wage for factor f in activity a (used for calibration)
 predeltaa(A)            dummy used to define deltaa
 predelta(C)             dummy used to define deltaq
 BUDSHR(C,H)             budget share for marketed commodity c and household h
 BUDSHR2(A,H)            budget share for home commodity c - act a - hhd h
 BUDSHRCHK(H)            check that budget shares some to unity
 ELASCHK(H)              check that expenditure elasticities satisfy Engel aggr
 SUBSIST(H)              subsistence spending
 FRISCH2(H)              alt. defn of Frisch -- ratio of cons to supernumerary cons
 LESCHK(H)               check on LES parameter definitions (error mssg if error)
 PRODSHR(A,RD)
 YG_DTAX0                direct tax take plus transfers from abroad
 YG_ITAX0                indirect tax take
*FH 25062019
 YG_NTAX0                price differentials (utax code)
 ITAXSHR0                share of indirect taxes in GDPMP
 TAXADJ0                 tax adjustment which maintains indirect tax share
 GDPMP0                  nominal GDP at market prices
*FH
 PWEADJ(C,RW)
;

*FH
 PWEADJ(C,RW)=0;
*CA set within category capital mobility
beta2(fcap)=2;
*beta2('fegy')=0.2;

SET
CFUEL(C) /CPETR-P, CPETR-D, CPETR-H, CPETR-K, CPETR-L/;

*Price block --------------------------------------------------
IF(AGRIPROD EQ 0,
 PSUP(C)              = 1;
 PE0(C,RW)$CERW(C,RW) = PSUP(C);
 PX0(C)$CX(C)         = PSUP(C);
 PDS0(C)$CD(C)        = PSUP(C);
 PXAC0(A,C)$SUM((ARD,RD)$MARD(ARD,A,RD),SAM(ARD,C)) = PSUP(C);
 PA0(A)               = 1;
 PAR0(A,RD)           = 1;
 EXR0                 = 1;
ELSE
 PA0(A)               = 1;
 PAR0(A,RD)           = 1;
 PAR0(A,RD)$QPROD(A,RD)    = SUM(ARD$MARD(ARD,A,RD), SAM('TOTAL',ARD)) / QPROD(A,RD);
* PRODSHR(A,RD)$QPROD(A,RD) = QPROD(A,RD) / QPROD(A,'NAT');
* PA0(A)$SUM(RD, PRODSHR(A,RD)*PAR0(A,RD)) = SUM(RD, PRODSHR(A,RD)*PAR0(A,RD));
 PA0(A)               = SUM(RD,PAR0(A,RD));
*fh 26062019 replace calculation of psup with a weighted average
* PSUP(C)              = SUM(A$MAC(A,C), PA0(A));
 PSUP(C)$sum(AP,SAM(AP,C)) = SUM(A$MAC(A,C),(PA0(A)*SAM(A,C)/sum(AP,SAM(AP,C))));
 PE0(C,RW)$CERW(C,RW) = PSUP(C);
 PX0(C)$CX(C)         = PSUP(C);
 PDS0(C)$CD(C)        = PSUP(C);
 PXAC0(A,C)$SUM((RD,ARD)$MARD(ARD,A,RD), SAM(ARD,C)) = PSUP(C);
 EXR0                 = 1;
);

*Activity quantity = payment to activity divided by activity price
*QA covers both on-farm consumption and marketed output output GROSS of tax
 QAR0(A,RD)$PAR0(A,RD) =  SUM(ARD$MARD(ARD,A,RD), SAM('TOTAL',ARD))/PAR0(A,RD) ;
 QA0(A)                =  SUM(RD, QAR0(A,RD));

*SR If an activity is produced in only one region, set ACES2 to "no"
 ACES2(A)$(SUM(RD$QAR0(A,RD), 1) eq 1) = no ;

*Household populations
 hpop(H) =  hpop0(H);

*Unit value-added price = total value-added / activity quantity define pva gross of tax
 QVA0(A,RD)            = SUM(ARD$MARD(ARD,A,RD), SUM(F, SAM(F,ARD)) + TAXPAR('VATAX',ARD)) ;
 PVA0(A,RD)$QVA0(A,RD) = SUM(ARD$MARD(ARD,A,RD), SUM(F, SAM(F,ARD)) + TAXPAR('VATAX',ARD))/QVA0(A,RD);
 iva0(A,RD)$QAR0(A,RD)  = QVA0(A,RD)/QAR0(A,RD) ;
 iva(A,RD) = iva0(A,RD);
 QXAC0(A,C)$PXAC0(A,C) =  SUM((ARD,RD)$(MARD(ARD,A,RD) AND QVA0(A,RD)), SAM(ARD,C)) / PXAC0(A,C);

*Energy: Special treatment for coal disaggregation
 QXAC0('ACOAL','CCOAL-LOW')        = CALIB('CCOAL-LOW','QA');
 QXAC0('ACOAL','CCOAL-HGH')        = CALIB('CCOAL-HGH','QA');
 PXAC0('ACOAL','CCOAL-LOW')        = SAM('ACOAL','CCOAL-LOW')/QXAC0('ACOAL','CCOAL-LOW');
 PXAC0('ACOAL','CCOAL-HGH')        = SAM('ACOAL','CCOAL-HGH')/QXAC0('ACOAL','CCOAL-HGH');
 PX0('CCOAL-LOW')                  = PXAC0('ACOAL','CCOAL-LOW');
 PX0('CCOAL-HGH')                  = PXAC0('ACOAL','CCOAL-HGH');
 PDS0('CCOAL-LOW')                 = PXAC0('ACOAL','CCOAL-LOW');
 PDS0('CCOAL-HGH')                 = PXAC0('ACOAL','CCOAL-HGH');
 PE0('CCOAL-LOW','REST')           = PXAC0('ACOAL','CCOAL-LOW');
*fh 25062019
 PE0('CCOAL-HGH','REST')           = PXAC0('ACOAL','CCOAL-HGH');
* QE0('CCOAL-HGH','REST')           = CALIB('CCOAL-HGH','QE');
* PE0('CCOAL-HGH','REST')           = (SAM('CCOAL-HGH','ROW'))/QE0('CCOAL-HGH','REST');

*FH: Petroleum
 QXAC0('APETR',CFUEL) = CALIB(CFUEL,'QA');
 PXAC0('APETR',CFUEL) = SAM('APETR',CFUEL)/QXAC0('APETR',CFUEL);
 PX0(CFUEL)           = PXAC0('APETR',CFUEL);
 PDS0(CFUEL)          = PXAC0('APETR',CFUEL);
 PE0(CFUEL,'REST')    = PXAC0('APETR',CFUEL);

 QHA0(A,H)         = SUM((ARD,RD)$MARD(ARD,A,RD),SAM(ARD,H))/PA0(A);
 QANET0(A)         = QA0(A) - SUM(H, QHA0(A,H));

*Output quantity = value received by producers divided by producer price
*QX covers only marketed output
 QX0(C)$PX0(C) = SUM(ARD, SAM(ARD,C)) / PX0(C);

*Export quantity = export revenue received by producers
*(ie. minus tax and transactions cost) divided by export price.
PARAMETER TRESHR(C,RW);
 TRESHR(C,RW)$SUM(RWP, SAM(C,'ROW')*REGEXP(C,RWP)+TAXPAR('EXPTAX',C)*REGETX(C,RWP))
         = (SAM(C,'ROW')*REGEXP(C,RW)+TAXPAR('EXPTAX',C)*REGETX(C,RW)) / SUM(RWP, SAM(C,'ROW')*REGEXP(C,RWP)+TAXPAR('EXPTAX',C)*REGETX(C,RWP));
* QE0(C)$SAM(C,'ROW') = (SAM(C,'ROW') - TAXPAR('EXPTAX',C) - SUM(CTE, SAM(CTE,C)))/PE0(C);
 QE0(C,RW)$(CERW(C,RW) AND PE0(C,RW)) = (SAM(C,'ROW')*REGEXP(C,RW) - TAXPAR('EXPTAX',C)*REGETX(C,RW) - SUM(CTE, SAM(CTE,C))*TRESHR(C,RW) )/PE0(C,RW);

* QE0('CCOAL-HGH','REST')           = CALIB('CCOAL-HGH','QE');
* PE0('CCOAL-HGH','REST')           = SAM('CCOAL-HGH','ROW')/QE0('CCOAL-HGH','REST');

*RoW export price = RoW export payment (in for curr) / export qnty
 PWE0(C,RW)$(CERW(C,RW) AND QE0(C,RW))  = (SAM(C,'ROW')*REGEXP(C,RW)/EXR0) / QE0(C,RW);
 pwebar(C,RW) = PWE0(C,RW);
 te0(C,RW)$(SAM(C,'ROW')*REGEXP(C,RW)) = (TAXPAR('EXPTAX',C)*REGETX(C,RW))/(SAM(C,'ROW')*REGEXP(C,RW));
 te(C,RW)               =  te0(C,RW);

*Quantity of output sold domestically = output quantity less quantity
*exported = value of domestic sales divided by domestic supply price
*QD0 covers only marketed output
*JTSACT QD0(C)$CD(C) =  QX0(C) - QE0(C);
 QD0(C)$CD(C) =  QX0(C) - SUM(RW, QE0(C,RW));

*Domestic demander price = demander payment divided by quantity bought
 PDD0(C)$QD0(C)= (PDS0(C)*QD0(C) + SUM(CTD, SAM(CTD,C)))/QD0(C);

*Define import price to equal domestic price so that import and domestic
*units are the same to the purchaser. If no domestic good, set PM to 1.
 PM0(C,RW)$CMRW(C,RW) = PDD0(C) ;
 PM0(C,RW)$(QD0(C) EQ 0)  = 1 ;

*Import quantity = demander payment for imports (including tariffs
*and marketing cost) divided by demander price.
PARAMETER TRMSHR(C,RW);
 TRMSHR(C,RW)$SUM(RWP, SAM('ROW',C)*REGIMP(C,RWP)+TAXPAR('IMPTAX',C)*REGTAR(C,RWP))
         = (SAM('ROW',C)*REGIMP(C,RW)+TAXPAR('IMPTAX',C)*REGTAR(C,RW)) / SUM(RWP, SAM('ROW',C)*REGIMP(C,RWP)+TAXPAR('IMPTAX',C)*REGTAR(C,RWP));
 QM0(C,RW)$CMRW(C,RW) = (SAM('ROW',C)*REGIMP(C,RW) + TAXPAR('IMPTAX',C)*REGTAR(C,RW) + SUM(CTM, SAM(CTM,C))*TRMSHR(C,RW) )/PM0(C,RW);

*RSA energy CGE model
 QM0('CCOIL','REST') = CALIB('CCOIL','QM');
 PM0('CCOIL','REST') = (SAM('ROW','CCOIL')+SAM('MTAX','CCOIL')+SAM('TRM','CCOIL')) / QM0('CCOIL','REST');

 QM0('CELEC','REST') = CALIB('AELEC','QM');
 PM0('CELEC','REST')$QM0('CELEC','REST') = (SAM('ROW','CELEC')+SAM('MTAX','CELEC')+SAM('TRM','CELEC')) / QM0('CELEC','REST');

 QM0('CHYDR','REST') = CALIB('CHYDR','QM');
 PM0('CHYDR','REST')$QM0('CHYDR','REST') = (SAM('ROW','CHYDR')+SAM('MTAX','CHYDR')+SAM('TRM','CHYDR')) / QM0('CHYDR','REST');

 QM0('CCOAL-LOW','REST') = CALIB('CCOAL-LOW','QM');
 PM0('CCOAL-LOW','REST')$QM0('CCOAL-LOW','REST') = (SAM('ROW','CCOAL-LOW')+SAM('MTAX','CCOAL-LOW')+SAM('TRM','CCOAL-LOW')) / QM0('CCOAL-LOW','REST');

*fh-----------------------------------------------------------------------------
 QM0(CFUEL,'REST') = CALIB(CFUEL,'QM');
* QM0('CPETR_O','REST') = CALIB('CPETR_O','QM');
* QM0('CPETR_D','REST') = CALIB('CPETR_D','QM');

*fh27062019
 QM0('CCOAL-HGH','REST') = CALIB('CCOAL-HGH','QM');
* PM0('CCOAL-HGH','REST') = (SAM('ROW','CCOAL-HGH')+SAM('MTAX','CCOAL-HGH')+SAM('TRM','CCOAL-HGH')) / QM0('CCOAL-HGH','REST');

*World price = import value (in foreign currency / import quantity
 PWM0(C,RW)$CMRW(C,RW) = (SAM('ROW',C)*REGIMP(C,RW)/EXR0) / QM0(C,RW);
 pwMbar(C,RW) = PWm0(C,RW);
 tm0(C,RW)$(SAM('ROW',C)*REGIMP(C,RW))  = (TAXPAR('IMPTAX',C)*REGTAR(C,RW)) / (SAM('ROW',C)*REGIMP(C,RW));
 tm(C,RW) = tm0(C,RW);

 PM0(CFUEL,'REST')$ QM0(CFUEL,'REST') = (SAM('ROW',CFUEL) + TAXPAR('IMPTAX',CFUEL)*REGTAR(CFUEL,'REST') + SUM(CTM, SAM(CTM,CFUEL))*TRMSHR(CFUEL,'REST'))/QM0(CFUEL,'REST');
* PM0('CPETR_O','REST')$ QM0('CPETR_O','REST') = (SAM('ROW','CPETR_O') + TAXPAR('IMPTAX','CPETR_O')*REGTAR('CPETR_O','REST') + SUM(CTM, SAM(CTM,'CPETR_O'))*TRMSHR('CPETR_O','REST'))/QM0('CPETR_O','REST');
* PM0('CPETR_D','REST')$ QM0('CPETR_D','REST') = (SAM('ROW','CPETR_D') + TAXPAR('IMPTAX','CPETR_D')*REGTAR('CPETR_D','REST') + SUM(CTM, SAM(CTM,'CPETR_D'))*TRMSHR('CPETR_D','REST'))/QM0('CPETR_D','REST');
*fffh
 PM0('CCOAL-HGH','REST')$ QM0('CCOAL-HGH','REST') = (SAM('ROW','CCOAL-HGH') + TAXPAR('IMPTAX','CCOAL-HGH')*REGTAR('CCOAL-HGH','REST') + SUM(CTM, SAM(CTM,'CCOAL-HGH'))*TRMSHR('CCOAL-HGH','REST'))/QM0('CCOAL-HGH','REST');

*Composite supply is the sum of domestic market sales and imports
*(since they are initialized at the same price).
 QQ0(C)$(CD(C) OR CM(C)) = QD0(C) + SUM(RW, QM0(C,RW)) ;
 PQ0(C)$QQ0(C) = (SAM(C,'TOTAL') - SAM(C,'ROW'))/QQ0(C);
 TQ0(C)$QQ0(C) = TAXPAR('COMTAX',C)/(PQ0(C)*QQ0(C)) ;
 TQADJ0 = 0;
 TQPS0  = 0;
 TQELEC0(c)=0;

 tq01(C) = 1;
 tqbar(C) = TQ0(C);

 TRNADJ0 = 0;
 trn01(INS) = 1;

 SHCTD(CT)$SUM(CTD, SAM('TOTAL',CTD)) = SUM(CTD, SAM(CT,CTD)/SAM('TOTAL',CTD)) ;
 SHCTM(CT)$SUM(CTM, SAM('TOTAL',CTM)) = SUM(CTM, SAM(CT,CTM)/SAM('TOTAL',CTM)) ;
 SHCTE(CT)$SUM(CTE, SAM('TOTAL',CTE)) = SUM(CTE, SAM(CT,CTE)/SAM('TOTAL',CTE)) ;

*Transactions input coefficients
 icd(CT,C)$QD0(c)
   = (shctd(CT)*SUM(CTD, SAM(CTD,C))/PQ0(ct)) / QD0(C);
 icm(CT,C)$SUM(RW, QM0(C,RW))
   = (shctm(CT)*SUM(CTM, SAM(CTM,C))/PQ0(ct)) / SUM(RW, QM0(C,RW));
 ice(CT,C)$SUM(RW, QE0(C,RW))
   = (shcte(CT)*SUM(CTE, SAM(CTE,C))/PQ0(ct)) / SUM(RW, QE0(C,RW));

*Indirect activity tax rate = tax payment / output value
*Tax is here applied to total output value (incl. on-farm cons.)
 tvabar0(A,RD)$QVA0(A,RD) = SUM(ARD$MARD(ARD,A,RD),TAXPAR('VATAX',ARD)) /
                         (PVA0(A,RD)*QVA0(A,RD));
 tvabar(A,RD) = tvabar0(A,RD);
 TVA0(A,RD) = tvabar0(A,RD);
 TVAADJ0 = 1;
 TVAPS0  = 0;
 tva01(A,RD)$tvabar0(A,RD) = 1;

*QA is GROSS of tax, so base for ta is as well
 TA0(A,RD)$QAR0(A,RD) = SUM(ARD$MARD(ARD,A,RD), TAXPAR('ACTTAX',ARD) /
                         (SAM(ARD,'TOTAL')));
 TAADJ0 = 0;
 TAPS0  = 0;
 ta01(A,RD) = 1;
 tabar(A,RD) = TA0(A,RD);

*Yield coefficient = quantity produced and delivered to market.
*Home consumption is assumed to come from activities
 theta(A,C)$PXAC0(A,C)
  =  QXAC0(A,C)/(QA0(A)- SUM(H,QHA0(A,H))) ;

 theta0(A,C) = theta(A,C);
*Intermediate input coefficient = input use / output quantity
*bm QINTA0(A,RD) = SUM(C$PQ0(C), SUM(ARD$MARD(ARD,A,RD), SAM(C,ARD))  / PQ0(C)) ;
*bm this is commented out to keep units of input in their raw format, given that energy is now in physical units
*bm we cannot add them or scale them relative to other inputs - it also makes linking to energy model more intuitive
 QINTA0(A,RD) = QAR0(A,RD);

*utax code
*CA  and mcp distance paramter
PARAMETER
 tui0(C,A,RD)
 tuh0(C,H)
*fh11062019
 tue0(C,RW)
 tui(C,A,RD)
 tuh(C,H)
*fh11062019
 tue(C,RW)
 PQI0(C,A,RD)
 PQH0(C,H)
*fh11062019
 PQE0(C,RW)
;

SCALAR
 alphawfdist range for value of capital parameter /0/;
*bm /.2/
;

*utax code
*CA specify base level of intermediate input use
 ica0(C,A,RD)$(QINTA0(A,RD) AND PQ0(C)) = SUM(ARD$MARD(ARD,A,RD), SAM(C,ARD))/PQ0(C) / QINTA0(A,RD) ;
 ica(C,A,RD)=ica0(C,A,RD);

 inta0(A,RD)$QAR0(A,RD) = QINTA0(A,RD) / QAR0(A,RD) ;
 inta(A,RD) = inta0(A,RD);

*utax code
 PQI0(C,A,RD) = PQ0(C);
 PQI0('CELEC',A,RD)$(QINTA0(A,RD)*ica('CELEC',A,RD))
*         = SUM(ARD$MARD(ARD,A,RD), SAM('CELEC',ARD) + SAM('UTAX',ARD)) / (QINTA0(A,RD)*ica('CELEC',A,RD));
         = SUM(ARD$MARD(ARD,A,RD), SAM('CELEC',ARD) + SAM('UTAX',ARD)) / (SUM(ARD$MARD(ARD,A,RD), SAM('CELEC',ARD))/PQ0('CELEC'));
*FH 15052019 Include similar for coal high and low
 PQI0('CCOAL-HGH',A,RD)$(QINTA0(A,RD)*ica('CCOAL-HGH',A,RD))
         = SUM(ARD$MARD(ARD,A,RD), SAM('CCOAL-HGH',ARD) + SAM('UTAX-CH',ARD)) / (SUM(ARD$MARD(ARD,A,RD), SAM('CCOAL-HGH',ARD))/PQ0('CCOAL-HGH'));
 PQI0('CCOAL-LOW',A,RD)$(QINTA0(A,RD)*ica('CCOAL-LOW',A,RD))
         = SUM(ARD$MARD(ARD,A,RD), SAM('CCOAL-LOW',ARD) + SAM('UTAX-CL',ARD)) / (SUM(ARD$MARD(ARD,A,RD), SAM('CCOAL-LOW',ARD))/PQ0('CCOAL-LOW'));

*fh11062019
 PQE0(C,RW)$PE0(C,RW)=PE0(C,RW);
 PQE0('CCOAL-HGH',RW)$PE0('CCOAL-HGH',RW)= (SAM('CCOAL-HGH','ROW')+SAM('UTAX-CH','ROW'))/(SAM('CCOAL-HGH','ROW')/PE0('CCOAL-HGH','REST'));

*utax code
 tui0(C,A,RD) = 0;
 tui0(C,A,RD)$PQ0(C) = (PQI0(C,A,RD) - PQ0(C))/PQ0(C);
 tui(C,A,RD) = tui0(C,A,RD);

*fh11062019
 tue0(C,RW)=0;
 tue0(C,RW)$PE0(C,RW)=(PQE0(C,RW) - PE0(C,RW))/PE0(C,RW);
 tue(C,RW) = tue0(C,RW);

*utax code
* pinta0(A,RD)      = SUM(C, ica(C,A,RD)*PQ0(C)) ;
 pinta0(A,RD)      = SUM(C, ica(C,A,RD)*PQI0(C,A,RD)) ;
* pinta0(A,RD)$QINTA0(A,RD) = SUM(ARD$MARD(ARD,A,RD), SUM(C, SAM(C,ARD))+SAM('UTAX',ARD)) / QINTA0(A,RD);


*CPI weight by comm'y = hhd cons value for comm'y / total hhd cons value
*CPI does not consider on-farm consumption.
 cwts(C) = SUM(H, SAM(C,H)) / SUM((CP,H), SAM(CP,H));

*CPI weights by household
 cwtsh(c,h)$(SUM(cp, sam(cp,h)) + SUM(a, QHA0(A,H)*PA0(A)))
         = sam(c,h)/(SUM(cp, sam(cp,h)) + SUM(a, QHA0(A,H)*PA0(A)));
 cwtsh(a,h)$(SUM(cp, sam(cp,h)) + SUM(ap, QHA0(ap,H)*PA0(ap)))
         = QHA0(A,H)*PA0(A)/(SUM(cp, sam(cp,h)) + SUM(ap, QHA0(ap,H)*PA0(ap)));
 cwtshchk(h)   = SUM(acnt, cwtsh(acnt,h)) - 1 ;

*Domestic sales price index weight = dom sales value for comm'y
*/ total domestic salues value
*Domestic sales price index does not consider on-farm consumption.
 dwts(C)       = (SUM(ARD, SAM(ARD,C)) - (SAM(C,'ROW') -
                  SUM(cte, SAM(cte,C))))/
                  SUM(CP, SUM(ARD, SAM(ARD,CP)) - (SAM(CP,'ROW') -
                  SUM(cte, SAM(cte,CP))));

 cwtsh(c,h) = SAM(C,H)/SUM((CP,HP), SAM(CP,HP));

 CWTSCHK       = SUM(C, cwts(C)) - 1;
 DWTSCHK       = SUM(C, dwts(C)) - 1;

*utax code
 CPI0          = SUM(C, cwts(C)*PQ0(C)) ;
* CPI0          = SUM((C,H), cwtsh(C,H)*PQH0(C,H)) ;

*SR Exclude perfect substitute exports (CERES) from domestic price index
*DPI0          = SUM(CD, dwts(CD)*PDS0(CD)) ;
 DPI0          = SUM(CD$(NOT CERES(CD)), dwts(CD)*PDS0(CD)) ;

*Production and trade block ---------------------------------
*Compute exponents from elasticites
 rhof(F) = FELASTAB(F,'FLS');
 rhoq(C)$(CM(C) AND CD(C)) = (1/AELASTAB(C,'SIGMAQ')) - 1;
 rhot(C)$(CE(C) AND CD(C)) = (1/AELASTAB(C,'SIGMAT')) + 1;
 rhova(A,RD)$SUM(ARD$MARD(ARD,A,RD),DELASTAB(ARD,'PRODE')) = SUM(ARD$MARD(ARD,A,RD),(1/DELASTAB(ARD,'PRODE')) - 1);
 rhova2(F,A,RD)$SUM(FP, MFA2(F,FP,A,RD)) = (1/FNESTELAS(F,A,RD)) - 1;

*Add elasticity of substitution in the CES for the cost function
 elasva(A,RD)  = 1/(1 + rhova(A,RD)) ;

*Aggregation of domestic output from different activities

 RHOAC(C)$AELASTAB(C,'OUTAGG') = 1/AELASTAB(C,'OUTAGG') - 1;

 deltaac(A,C)$ (QXAC0(A,C)$AELASTAB(C,'OUTAGG'))
               = (PXAC0(A,C)*QXAC0(A,C)**(1/AELASTAB(C,'OUTAGG')))/
                 SUM(AP, PXAC0(AP,C)*QXAC0(AP,C)**(1/AELASTAB(C,'OUTAGG')));

 alphaac(C)$SUM(A,deltaac(A,C))
               = QX0(C)/(SUM(A$deltaac(A,C), deltaac(A,C) * QXAC0(A,C)**(-RHOAC(C))) )**(-1/RHOAC(C));

*CET disaggregation function (multiple outputs from same activity)
 ELASCA(A)$SUM(C$MAC(A,C), 1) = SUM(C$MAC(A,C), AELASTAB(C,'OUTDIS'))/SUM(C$MAC(A,C), 1);
 rhoca(A)$ELASCA(A) = (1/ELASCA(A)) + 1  ;

 deltaca0(A,C)$QXAC0(A,C) = (PXAC0(A,C)*(QXAC0(A,C))**(1-rhoca(A)))/ SUM(CP$QXAC0(A,CP), PXAC0(A,CP)*(QXAC0(A,CP))**(1-rhoca(A))) ;
 deltaca(A,C)$QXAC0(A,C)= deltaca0(A,C);
 alphaca(A)$QA0(A) = (QA0(A) - SUM(H, QHA0(A,H))) /(SUM(C$deltaca(A,C),deltaca(A,C)*QXAC0(A,C)**rhoca(A)))**(1/rhoca(A));


*Demand computations ----

SET
 MFAGG(F,FP,A,RD) directly or indirectly factor F is an agg of FP
;

 MFAGG(F,FP,A,RD)$(SUM(FPP, FTREE(F,FPP,FP)) AND SUM(ARD$MARD(ARD,A,RD), SAM(FP,ARD)) AND FD(FP)) = YES;

*Defining factor employment and supply.
 QF0(F,A,RD)  = SUM(ARD$MARD(ARD,A,RD),QF2BASE(F,ARD));
 QF0(FA,A,RD) = SUM(FD$MFAGG(FA,FD,A,RD), QF0(FD,A,RD));

*Defining employment for aggregate factors in factor nesting
 QFS0(F)      = SUM((A,RD), QF0(F,A,RD));

 QFS_BAR(F)   = QFS0(F);

*Activity-specific wage is activity labor payment over employment
 WFA(F,A,RD)$QF0(F,A,RD) = SUM(ARD$MARD(ARD,A,RD),SAM(F,ARD))/QF0(F,A,RD);
*Activity-specific wages for aggregate factors in factor nesting
 WFA(FA,A,RD)$QF0(FA,A,RD) = SUM((FD,ARD)$(MFAGG(FA,FD,A,RD) AND MARD(ARD,A,RD)), SAM(FD,ARD))/QF0(FA,A,RD);

*Economy-wide wage average is total factor income over employment
* WF0(F)$SUM((A,RD), QF0(F,A,RD))   = SUM(ARD, SAM(F,ARD))/SUM((A,RD), QF0(F,A,RD));
 WF0(F)$SUM((A,RD), QF0(F,A,RD))   = SUM(ARD, SAM(F,ARD))/SUM((A,RD), QF0(F,A,RD));
 WF0(FA)$SUM((A,RD), QF0(FA,A,RD)) = SUM((FD,A,ARD,RD)$(MFAGG(FA,FD,A,RD) AND MARD(ARD,A,RD)), SAM(FD,ARD)) /SUM((A,RD), QF0(FA,A,RD));

 WF_BAR(F) = WF0(F) ;

*Wage distortion factor
 wfdist0(F,A,RD)$WF0(F) = WFA(F,A,RD)/WF0(F);

*Leontief factor demand shares
 ifa0(F,A,RD)$QVA0(A,RD) = QF0(F,A,RD) / QVA0(A,RD);
 ifa(F,A,RD) = ifa0(F,A,RD);

*CES activity production function
 deltava(F,A,RD)$MFA1(F,A,RD) = (wfdist0(F,A,RD) * WF0(F) * (QF0(F,A,RD))**(1+rhova(A,RD)) )
              / SUM(FP$MFA1(FP,A,RD), wfdist0(FP,A,RD) * WF0(FP)*(QF0(FP,A,RD))**(1+rhova(A,RD)));

 alphava0(A,RD)$QVA0(A,RD) = QVA0(A,RD)/( SUM(F$MFA1(F,A,RD), deltava(F,A,RD)*QF0(F,A,RD)
               **(-rhova(A,RD))) )**(-1/rhova(A,RD));

 alphava(A,RD) = alphava0(A,RD);

 leova0(A,RD) = 1;
 leova(A,RD) = leova0(A,RD);

 fprd(F,A,RD)$QF0(F,A,RD)  = 1;

*Lower layer nested factor substitution parameters
 deltava2(F,FP,A,RD)$MFA2(F,FP,A,RD)
   = (WFDIST0(FP,A,RD) * WF0(FP) * (QF0(FP,A,RD))**(1+rhova2(F,A,RD)) )
     / SUM(FPP$MFA2(F,FPP,A,RD), WFDIST0(FPP,A,RD) * WF0(FPP)*(QF0(FPP,A,RD))**(1+rhova2(F,A,RD)));

  alphava2(F,A,RD)$SUM(FP, MFA2(F,FP,A,RD))
   = QF0(F,A,RD)/( SUM(FP$MFA2(F,FP,A,RD), deltava2(F,FP,A,RD)*QF0(FP,A,RD)**(-rhova2(F,A,RD))) )**(-1/rhova2(F,A,RD));

 rhoa2(A)$SUM(C$MAC(A,C), AELASTAB(C,'REGACT')) = (1/SUM(C$MAC(A,C), AELASTAB(C,'REGACT')))-1;

 deltaa2(A,RD)$QAR0(A,RD)
    = (PAR0(A,RD)*(QAR0(A,RD))**(1+rhoa2(A)))/SUM(RDP, PAR0(A,RDP)*(QAR0(A,RDP))**(1+rhoa2(A)));

 alphaa2(A)$QA0(A)
     = QA0(A)/(SUM(RD$QAR0(A,RD), deltaa2(A,RD)*QAR0(A,RD)**(-rhoa2(A))))**(-1/rhoa2(A));

*Intermediate demand
*utax code
 QINT0(C,A)$PQ0(C) = SUM((ARD,RD)$MARD(ARD,A,RD),SAM(C,ARD)) / PQ0(C);

*Transactions demand
 QT0(CT) = ( SUM(CTD, SAM(CT,CTD)) + SUM(CTE, SAM(CT,CTE))
           + SUM(CTM, SAM(CT,CTM)) ) / PQ0(CT) ;

*CET transformation
* deltat0(C,RW)$(CERW(C,RW))
*   = (PE0(C,RW)*(QE0(C,RW))**(1-rhot(C)))/( SUM(RWP$CERW(C,RWP), PE0(C,RWP)*QE0(C,RWP)**(1-rhot(C))) + (PDS0(C)*QD0(C)**(1-rhot(C))) );
*UTAX
*fh 25062019 account for price differential on coal-hgh exports
 deltat0(C,RW)$(CERW(C,RW))
   = (PQE0(C,RW)*(QE0(C,RW))**(1-rhot(C)))/( SUM(RWP$CERW(C,RWP), PQE0(C,RWP)*QE0(C,RWP)**(1-rhot(C))) + (PDS0(C)*QD0(C)**(1-rhot(C))) );
 deltat(C,RW) =  deltat0(C,RW);

 alphat0(C)$(CE(C) AND CD(C))
   = QX0(C)/(SUM(RW$CERW(C,RW), deltat(C,RW)*QE0(C,RW)**rhot(C))  + ((1-SUM(RW$CERW(C,RW), deltat(C,RW)))*QD0(C)**( rhot(C))) )**(1/rhot(C));
 alphat(C) =  alphat0(C);

*Armington aggregation
 deltaq0(C,RW)$CMRW(C,RW) = (PM0(C,RW)*(QM0(C,RW))**(1+rhoq(C)))/( SUM(RWP$CMRW(C,RWP), PM0(C,RWP)*QM0(C,RWP)**(1+rhoq(C))) + (PDD0(C)*QD0(C)**(1+rhoq(C))) );
 deltaq(C,RW)        = deltaq0(C,RW);

 alphaq0(C)$(CM(C) AND CD(C))
   = QQ0(C)/(SUM(RW$CMRW(C,RW), deltaq(C,RW)*QM0(C,RW)**(-rhoq(C))) + ((1-SUM(RW$CMRW(C,RW), deltaq(C,RW)))*QD0(C)**(-rhoq(C))) )**(-1/rhoq(C));
 alphaq(C)          = alphaq0(C);

*Institution block --------------------------------------------

*Institutional income
 YI0(INSDNG) = SAM('TOTAL',INSDNG);

*Factor income by factor category
 YF0(F)      = SUM(ARD, SAM(F,ARD));

*Institution income from factors
 YIF0(INSD,F) = SAM(INSD,F);

*Transfers to RoW from factors
 trnsfr('ROW',F)    = SAM('ROW',F)/EXR0;

*Remit abroad returns in electricity sector
 rf(F) = 0;

*Transfers from RoW to institutions
 trnsfr(INSD,'ROW') = SAM(INSD,'ROW')/EXR0;

*Government transfers
 trnsfr(INSD,'GOV') = SAM(INSD,'GOV')/CPI0;

*Factor taxes
 tf0(F)$SAM('TOTAL',F) = TAXPAR('FACTAX',F)/SAM('TOTAL',F);
 tf(F)                 = tf0(F);

*Shares of domestic institutions in factor income (net of factor taxes and transfers to RoW).
 shif(INSD,F)$(SAM(F,'TOTAL') - TAXPAR('FACTAX',F) - SAM('ROW',F))
         = SAM(INSD,F)/(SAM(F,'TOTAL') - TAXPAR('FACTAX',F) - SAM('ROW',F));

 SHIFCHK(F)    = SUM(INSD, shif(INSD,F)) - 1 ;

*Inter-institution transfers
 TRII0(INSDNG,INSDNGP) = SAM(INSDNG,INSDNGP);

*Share of dom non-gov institution in income of other dom non-gov institutions (net of direct taxes and savings).
 shii(INSDNG,INSDNGP)$((SAM('TOTAL',INSDNGP) - TAXPAR('INSTAX',INSDNGP) - SUM(IT, SAM(IT,INSDNGP))))
         = SAM(INSDNG,INSDNGP)
   /(SAM('TOTAL',INSDNGP) - TAXPAR('INSTAX',INSDNGP) - SUM(IT, SAM(IT,INSDNGP)));

*Scaling factors for savings and direct tax shares
 MPSADJ0      = 0;
 TINSADJ0     = 0;

*Savings rates
 MPS0(INSDNG)$(SAM('TOTAL',INSDNG) - TAXPAR('INSTAX',INSDNG)) = SUM(IT, SAM(IT,INSDNG))/(SAM('TOTAL',INSDNG) - TAXPAR('INSTAX',INSDNG));
 mpsbar(INSDNG) = MPS0(INSDNG);

*Direct tax rates
 TINS0(INSDNG)$SAM('TOTAL',INSDNG)   = TAXPAR('INSTAX',INSDNG) / SAM('TOTAL',INSDNG);
 tinsbar(INSDNG) = TINS0(INSDNG);

*"Point" change in savings and direct tax shares
 DMPS0  = 0;
 DTINS0 = 0;

*Selecting institutions for potential "point" change in savings and tax rates

*If DMPS or MPSADJ is flexible, institutions with a value of 1 for mps01
*change their savings rates.
 mps01(INSDNG)  = 1;

*If DTIMS is flexible, institutions with a value of 1 for tins01 change
*their savings rates.
 tins01(INSDNG) = 1;

*Household consumption spending and consumption quantities.
*utax code
* EH0(H)         = SUM(C, SAM(C,H)) + SUM(A, QHA0(A,H)*PA0(A));
*FH 20052019
* EH0(H)         = SUM(C, SAM(C,H)) + SAM('UTAX',H) + SUM(A, QHA0(A,H)*PA0(A));
 EH0(H)         = SUM(C, SAM(C,H)) + SAM('UTAX',H) + SAM('UTAX-CH',H) + SUM(A, QHA0(A,H)*PA0(A));

* utax code
 QH0(C,H)$PQ0(C) = SAM(C,H)/PQ0(C);
** QH0(C,H)$PQH0(C,H) = SAM(C,H)/PQH0(C,H);

* utax code
 PQH0(C,H) = PQ0(C);
*FH 20052019
* PQH0('CELEC',H) = (SAM('CELEC',H)+SAM('UTAX',H))/QH0('CELEC',H);
 PQH0('CELEC',H) = (SAM('CELEC',H)+SAM('UTAX',H))/QH0('CELEC',H);

*FH 15052019 Include same for coal high
 PQH0('CCOAL-HGH',H)$QH0('CCOAL-HGH',H) = (SAM('CCOAL-HGH',H)+SAM('UTAX-CH',H))/QH0('CCOAL-HGH',H);

* utax code
 tuh0(C,H) = 0;
 tuh0(C,H)$PQ0(C) = (PQH0(C,H) - PQ0(C))/PQ0(C);
 tuh(C,H)  = tuh0(C,H);

*Government indicators
 YG0           = SAM('TOTAL','GOV');
 EG0           = SAM('TOTAL','GOV') - SUM(IT, SAM(IT,'GOV'));
 QG0(C)$PQ0(C) = SAM(C,'GOV')/PQ0(C);

 qbarg0(C)     = QG0(C);
 qbarg(C)      = qbarg0(C);
 GADJ0         = 1;
 GSAV0         = SUM(IT, SAM(IT,'GOV'));

*LES calibration ----------------------------------------------
*utax code
* BUDSHR(C,H)$(SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP)))
*         = SAM(C,H)/(SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP)));
*FH 20052019
$ONTEXT
 BUDSHR(C,H)$(SUM(CP, SAM(CP,H)) + SAM('UTAX',H)+SUM(AP, QHA0(AP,H)*PA0(AP)))
         = SAM(C,H)/(SUM(CP, SAM(CP,H)) +SAM('UTAX',H) + SUM(AP, QHA0(AP,H)*PA0(AP)));

 BUDSHR('CELEC',H)$(SUM(CP, SAM(CP,H)) + SAM('UTAX',H) + SUM(AP, QHA0(AP,H)*PA0(AP)))
         = (SAM('CELEC',H)+SAM('UTAX',H))/(SUM(CP, SAM(CP,H)) +SAM('UTAX',H) + SUM(AP, QHA0(AP,H)*PA0(AP)));

 BUDSHR2(A,H)$(SUM(CP, SAM(CP,H)) +SAM('UTAX',H) + SUM(AP, QHA0(AP,H)*PA0(AP)))
         = QHA0(A,H)*PA0(A)
                  /(SUM(CP, SAM(CP,H)) +SAM('UTAX',H) + SUM(AP, QHA0(AP,H)*PA0(AP)));
$OFFTEXT

 BUDSHR(C,H)$(SUM(CP, SAM(CP,H)) + SAM('UTAX',H)+ SAM('UTAX-CH',H)+SUM(AP, QHA0(AP,H)*PA0(AP)))
         = SAM(C,H)/(SUM(CP, SAM(CP,H)) +SAM('UTAX',H)+ SAM('UTAX-CH',H) + SUM(AP, QHA0(AP,H)*PA0(AP)));

 BUDSHR('CELEC',H)$(SUM(CP, SAM(CP,H)) + SAM('UTAX',H) + SUM(AP, QHA0(AP,H)*PA0(AP)))
         = (SAM('CELEC',H)+SAM('UTAX',H))/(SUM(CP, SAM(CP,H)) +SAM('UTAX',H)+ SAM('UTAX-CH',H) + SUM(AP, QHA0(AP,H)*PA0(AP)));

 BUDSHR('CCOAL-HGH',H)$(SUM(CP, SAM(CP,H)) + SAM('UTAX-CH',H) + SUM(AP, QHA0(AP,H)*PA0(AP)))
         = (SAM('CCOAL-HGH',H)+SAM('UTAX-CH',H))/(SUM(CP, SAM(CP,H))+SAM('UTAX',H)+SAM('UTAX-CH',H) + SUM(AP, QHA0(AP,H)*PA0(AP)));

 BUDSHR2(A,H)$(SUM(CP, SAM(CP,H)) +SAM('UTAX',H)+ SAM('UTAX-CH',H) + SUM(AP, QHA0(AP,H)*PA0(AP)))
         = QHA0(A,H)*PA0(A)
                  /(SUM(CP, SAM(CP,H)) +SAM('UTAX',H)+ SAM('UTAX-CH',H) + SUM(AP, QHA0(AP,H)*PA0(AP)));

 BUDSHRCHK(H)   = SUM(C, BUDSHR(C,H)) + SUM(A, BUDSHR2(A,H)) - 1 ;

 ELASCHK(H)     = SUM(C, BUDSHR(C,H)*YELASTAB(C,H))
                  + SUM(A, BUDSHR2(A,H)*HELAS(A,H)) - 1 ;

*fh 12/11/19:
*Correct elasticities to make them satisfy Engle aggregation exactly
 YELASTAB(C,H)$(ELASCHK(H) + 1)  = YELASTAB(C,H)/(ELASCHK(H) + 1);
 HELAS(A,H)$(ELASCHK(H) + 1)     = HELAS(A,H)/(ELASCHK(H) + 1);

*Check Engle aggregation again
 ELASCHK(H)      = SUM(C, BUDSHR(C,H)*YELASTAB(C,H)) +
                   SUM(A, BUDSHR2(A,H)*HELAS(A,H)) - 1;

 betam(C,H)   = BUDSHR(C,H)*YELASTAB(C,H);
 betam0(C,H)  = betam(C,H);
 betah(A,H)   = BUDSHR2(A,H)*HELAS(A,H);

*fh 12/11/19: code removed for transforming LES to CD
$ontext
*utax code
* gammam0(C,H)$BUDSHR(C,H)
*     =  ( (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PQ0(C) ) * ( BUDSHR(C,H) + betam(C,H)/FRISCH(H));
 gammam0(C,H)$BUDSHR(C,H)
*FH 20052019
*     =  ( (SUM(CP, SAM(CP,H)) + SAM('UTAX',H) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PQH0(C,H) ) * ( BUDSHR(C,H) + betam(C,H)/FRISCH(H));
     =  ( (SUM(CP, SAM(CP,H)) + SAM('UTAX',H)+ SAM('UTAX-CH',H) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PQH0(C,H) ) * ( BUDSHR(C,H) + betam(C,H)/FRISCH(H));

 gammah0(A,H)$BUDSHR2(A,H)
     =  ( (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PA0(A) )
                      * ( BUDSHR2(A,H) + betah(A,H)/FRISCH(H));

 gammam(C,H)   =  gammam0(C,H);

 gammah(A,H)   =  gammah0(A,H);
$offtext

 gammam0(C,H)  =  eps;
 gammah0(A,H)  =  eps;
 gammam(C,H)   =  gammam0(C,H);
 gammah(A,H)   =  gammah0(A,H);



*Checking LES parameters --------------------------
PARAMETERS
 SUBSIST(H)  subsistence spending
 FRISCH2(H)  alt. defn of Frisch -- ratio of cons to supernumerary cons
 LESCHK(H)   check on LES parameter definitions (error mssg if error)
 LESELASP1(C,H) Own price elasticity for marketed commodities
 LESELASP2(A,H) Own price elasticity for home consumed activities
 ;

*utax code
* SUPERNUM(H)  = SUM(A, gammah(A,H)*PA0(A)) + SUM(C, gammam(C,H)*PQ0(C)) ;
 SUPERNUM(H)  = SUM(A, gammah(A,H)*PA0(A)) + SUM(C, gammam(C,H)*PQH0(C,H)) ;

 FRISCH2(H)$(EH0(H) - SUPERNUM(H))   = -EH0(H)/(EH0(H) - SUPERNUM(H));
 LESCHK(H)$(SAM('TOTAL',H) AND (ABS(FRISCH(H) - FRISCH2(H)) GT 0.00000001)) = 1/0;

*Cross-price elasticities
$ontext
 LESELASP1(C,H)
   = -LESELAS1(C,H)
     *(PQ0(C)*gammam(C,H) / (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP)))
                                                       - 1/FRISCH(H));

 LESELASP2(A,H)
   = -LESELAS2(A,H)
     *(PA0(A)*gammah(A,H) / (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP)))
                                                       - 1/FRISCH(H));
$offtext

*System-constraint block -----------------------

*Fixed investment
 qbarinv(C,IT)$PQ0(C) = SAM(C,IT)/PQ0(C);
 QINV0(C,IT)       = qbarinv(C,IT);
 IADJ0(IT)         = 1;

*Capital stock calibration for recursive investment-based updating
 iwts(C,IT)    = qbarinv(C,IT) / SUM(CP, qbarinv(CP,IT));
 QINVK         = (natdrate + accrate) * SUM(FCAP, QFSBASE(FCAP));
 alphainv      = (SUM(C, SAM(C,'S-I')) / QINVK) / SUM(C, iwts(C,'S-I')*PQ0(C));

*Stock changes
 qdst0(C)$PQ0(C) = SAM(C,'DSTK')/PQ0(C);
 qdst(C)         = qdst0(C);

 FSAV0         = SUM(IT, SAM(IT,'ROW'))/EXR0;
 FBOR0         = 0;
 debt0         = 0;
 debt          = debt0;
 irate         = 0.05;

*utax code
* TABS0         = SUM((C,H), SAM(C,H)) + SUM((A,H), QHA0(A,H)*PA0(A)) + SUM(C, SAM(C,'GOV')) + SUM((IT,C), SAM(C,IT)) + SUM(C, SAM(C,'DSTK'));
*FH 20052019
* TABS0         = SUM((C,H), SAM(C,H)) + SUM(H, SAM('UTAX',H)) + SUM((A,H), QHA0(A,H)*PA0(A)) + SUM(C, SAM(C,'GOV')) + SUM((IT,C), SAM(C,IT)) + SUM(C, SAM(C,'DSTK'));
 TABS0         = SUM((C,H), SAM(C,H)) + SUM(H, SAM('UTAX',H))+ sum(H,SAM('UTAX-CH',H)) + SUM((A,H), QHA0(A,H)*PA0(A)) + SUM(C, SAM(C,'GOV')) + SUM((IT,C), SAM(C,IT)) + SUM(C, SAM(C,'DSTK'));

 INVSHR0       = SUM(IT, SAM('TOTAL',IT))/TABS0;

 GOVSHR0       = SUM(C, SAM(C,'GOV'))/TABS0;

 WALRAS0       = 0;

 trnsfr0(INS,AC) = trnsfr(INS,AC);


*------------------------------------------------------------------
*JAMES 28-5-2013: aggregate energy subsectors
*------------------------------------------------------------------
SET
 APETR(AC) / apetr/
 AELEC(AC) / aelec/
 RDNT(RD)  RD set without aggregate sector
 AAG(A)    sectors to aggregate / /
;

ALIAS (RDNT,RDNTP);

*RD set without national or aggregate
 RDNT(RD) = YES;
*FH 30/01/2019: ONLY HAVE NAT THEREFORE CANNOT SET=0
* RDNT('NAT') = NO;
* RDNT('imp') = NO;

*Only aggregate leontief activities
*  (otherwise have to recalibrate CES production function)
* AAG(A)$(NOT AFLEO(A)) = NO;

*Quantities
 QAR0(AAG,'NAT')    = SUM(RDNT, QAR0(AAG,RDNT));
 QF0(F,AAG,'NAT')   = SUM(RDNT, QF0(F,AAG,RDNT));
 QINTA0(AAG,'NAT')  = SUM(RDNT, QINTA0(AAG,RDNT));
 QVA0(AAG,'NAT')    = SUM(RDNT, QVA0(AAG,RDNT));

*Quantity based coefficients
 iva0(AAG,'NAT')$QAR0(AAG,'NAT')  = QVA0(AAG,'NAT')/QAR0(AAG,'NAT') ;
 ifa0(F,AAG,'NAT')$QVA0(AAG,'NAT') = QF0(F,AAG,'NAT')/QVA0(AAG,'NAT');
*bm inta0(AAG,'NAT')$QAR0(AAG,'NAT') = QINTA0(AAG,'NAT')/QAR0(AAG,'NAT');
 inta0(AAG,'NAT')$QAR0(AAG,'NAT') = 1;
 ica0(C,AAG,'NAT')$SUM(RDNT, QINTA0(AAG,RDNT)) = SUM(RDNT, ica0(C,AAG,RDNT)*QINTA0(AAG,RDNT))/SUM(RDNT, QINTA0(AAG,RDNT));
 iva(AAG,'NAT') = iva0(AAG,'NAT');
 ifa(F,AAG,'NAT') = ifa0(F,AAG,'NAT');
 inta(AAG,'NAT') = inta0(AAG,'NAT');
 ica(C,AAG,'NAT') = ica0(C,AAG,'NAT');
 fprd(F,AAG,'NAT')$QF0(F,AAG,'NAT') = 1;

*Prices
 PAR0(AAG,'NAT')    = SUM(RDNT, PAR0(AAG,RDNT)*QAR0(AAG,RDNT))/SUM(RDNT, QAR0(AAG,RDNT));
 PVA0(AAG,'NAT')    = SUM(RDNT, PVA0(AAG,RDNT)*QVA0(AAG,RDNT))/SUM(RDNT, QVA0(AAG,RDNT));

*utax code
*Electricity taxes and subsidies
*FH 15052019 now also includes coal
 tui0(C,AAG,'NAT')$SUM(RDNT, PQI0(C,AAG,RDNT)*QINTA0(AAG,RDNT)*ica0(C,AAG,RDNT))
     = SUM(RDNT, tui0(C,AAG,RDNT)*PQI0(C,AAG,RDNT)*QINTA0(AAG,RDNT)*ica0(C,AAG,RDNT))
         / SUM(RDNT, PQI0(C,AAG,RDNT)*QINTA0(AAG,RDNT)*ica0(C,AAG,RDNT));
 tui(C,AAG,'NAT') = tui0(C,AAG,'NAT');
 PQI0(C,AAG,'NAT') = PQ0(C)*(1+tui0(C,AAG,'NAT'));

*utax code
* PINTA0(AAG,'NAT')  = SUM(C, PQ0(C)*ica(C,AAG,'NAT'));
 PINTA0(AAG,'NAT')  = SUM(C, PQI0(C,AAG,'NAT')*ica(C,AAG,'NAT'));
 WFDIST0(F,AAG,'NAT')$QF0(F,AAG,'NAT') = SUM(RDNT, QF0(F,AAG,RDNT)*WF0(F)*WFDIST0(F,AAG,RDNT))/SUM(RDNT, QF0(F,AAG,RDNT)*WF0(F));

*Value-added taxes
 tvabar0(AAG,'NAT') = SUM(RDNT, tvabar0(AAG,RDNT)*PVA0(AAG,RDNT)*QVA0(AAG,RDNT)) / SUM(RDNT, PVA0(AAG,RDNT)*QVA0(AAG,RDNT));
 tvabar(AAG,'NAT')  = tvabar0(AAG,'NAT');
 TVA0(AAG,'NAT')    = tvabar0(AAG,'NAT');
 tva01(AAG,'NAT')$tvabar0(AAG,'NAT') = 1;

*Activity taxes
 tabar(AAG,'NAT')   = SUM(RDNT, tabar(AAG,RDNT)*PAR0(AAG,RDNT)*QAR0(AAG,RDNT)) / SUM(RDNT, PAR0(AAG,RDNT)*QAR0(AAG,RDNT));
 TA0(AAG,'NAT')     = tabar(AAG,'NAT');
 ta01(AAG,'NAT')$tabar(AAG,'NAT') = 1;
*fh:01022018

*Production function parameters

 MFA1(F,AAG,'NAT')$SUM(RDNT, MFA1(F,AAG,RDNT)) = YES;
 MFA2(F,FP,AAG,RD)$SUM(RDNT, MFA2(F,FP,AAG,RDNT)) = YES;

 deltava(F,AAG,'NAT')$MFA1(F,AAG,'NAT') = (WFDIST0(F,AAG,'NAT') * WF0(F) * (QF0(F,AAG,'NAT'))**(1+rhova(AAG,'NAT')) )/ SUM(FP$MFA1(FP,AAG,'NAT'), wfdist0(FP,AAG,'NAT') * WF0(FP)*(QF0(FP,AAG,'NAT'))**(1+rhova(AAG,'NAT')));
 alphava0(AAG,'NAT')$QVA0(AAG,'NAT') = QVA0(AAG,'NAT')/( SUM(F$MFA1(F,AAG,'NAT'), deltava(F,AAG,'NAT')*QF0(F,AAG,'NAT')**(-rhova(AAG,'NAT'))) )**(-1/rhova(AAG,'NAT'));
 alphava(AAG,'NAT') = alphava0(AAG,'NAT');

 deltava2(F,FP,AAG,'NAT')$MFA2(F,FP,AAG,'NAT') = (WFDIST0(FP,AAG,'NAT') * WF0(FP) * (QF0(FP,AAG,'NAT'))**(1+rhova2(F,AAG,'NAT')) ) / SUM(FPP$MFA2(F,FPP,AAG,'NAT'), WFDIST0(FPP,AAG,'NAT') * WF0(FPP)*(QF0(FPP,AAG,'NAT'))**(1+rhova2(F,AAG,'NAT')));
 alphava2(F,AAG,'NAT')$SUM(FP, MFA2(F,FP,AAG,'NAT')) = QF0(F,AAG,'NAT')/( SUM(FP$MFA2(F,FP,AAG,'NAT'), deltava2(F,FP,AAG,'NAT')*QF0(FP,AAG,'NAT')**(-rhova2(F,AAG,'NAT'))) )**(-1/rhova2(F,AAG,'NAT'));

 deltaa2(AAG,'NAT')$QAR0(AAG,'NAT') = (PAR0(AAG,'NAT')*(QAR0(AAG,'NAT'))**(1+rhoa2(AAG)))/SUM(RDNT, PAR0(AAG,RDNT)*(QAR0(AAG,RDNT))**(1+rhoa2(AAG)));
 alphaa2(AAG)$QA0(AAG) = QA0(AAG)/(SUM(RDNT$QAR0(AAG,RDNT), deltaa2(AAG,RDNT)*QAR0(AAG,RDNT)**(-rhoa2(AAG))))**(-1/rhoa2(AAG));

*Remove sub-national variables (leave coefficients)
 QAR0(AAG,RDNT)   = 0;
 QINTA0(AAG,RDNT) = 0;
 QVA0(AAG,RDNT)   = 0;
 QF0(F,AAG,RDNT)  = 0;
 PAR0(AAG,RDNT)   = 0;
 PVA0(AAG,RDNT)   = 0;
*CA modified with exception
 PQI0(C,AAG,RDNT)$(NOT ica0(C,AAG,RDNT))  = 0;

 ACES2(AAG) = no ;

*CA Set qfbar
 QFBAR(F,A,RD) = QF0(F,A,RD);

*CA calibrate for complementarity condition on returns to capital
*Use code from 2capital.inc with the following modifications:
*a).L changed to 0 for all variables (convert to baseline parameter values)
*b) Drop exception handling on fixed and not fixed capital

 CAPSHR2TOT(FCAP)        = SUM((RD,A), QF0(FCAP,A,RD));
 CAPSHR2(FCAP,A,RD)$CAPSHR2TOT(FCAP) = QF0(FCAP,A,RD)/CAPSHR2TOT(FCAP);

 WFADJ(FCAP)$CAPSHR2TOT(FCAP) = SUM((RD,A), WF0(FCAP)*WFDIST0(FCAP,A,RD)* CAPSHR2(FCAP,A,RD));
 WFDISTADJ(FCAP,A,RD)$WFADJ(FCAP) = WF0(FCAP)*WFDIST0(FCAP,A,RD)/WFADJ(FCAP);
 WFK2AV0(FCAP)            = SUM((RD,A), WFADJ(FCAP)*WFDISTADJ(FCAP,A,RD)*CAPSHR2(FCAP,A,RD));
 WFK2AV(FCAP) = WFK2AV0(FCAP);


*--------------------------------------------------------------------------------------------
*4. VARIABLE DECLARATIONS
*--------------------------------------------------------------------------------------------

VARIABLES
 CPI                     consumer price index (PQ-based)
 DPI                     index for domestic producer prices (PDS-based)
 DMPS                    change in marginal propensity to save for selected inst
 DTINS                   change in domestic institution tax share
 EG                      total current government expenditure
 EH(H)                   household consumption expenditure
 EXR                     exchange rate
 FBOR                    foreign borrowing
 FSAV                    foreign savings
 GADJ                    government demand scaling factor
 GOVSHR                  govt consumption share of absorption
 GSAV                    government savings
 IADJ(IT)                investment scaling factor (for fixed capital formation)
 INVSHR                  investment share of absorption
 MPS(INS)                marginal propensity to save for dom non-gov inst ins
 MPSADJ                  savings rate scaling factor
 PA(A)                   output price of national aggregate activity a
 PAR(A,RD)               output price of regional activity a
 PDD(C)                  demand price for com'y c produced & sold domestically
 PDS(C)                  supply price for com'y c produced & sold domestically
 PE(C,RW)                price of exports
 PINTA(A,RD)             price of intermediate aggregate
 PM(C,RW)                price of imports
 PQ(C)                   price of composite good c
 PVA(A,RD)               value added price
 PWE(C,RW)               world price of exports
 PWM(C,RW)               world price of imports
 PX(C)                   average output price
 PXAC(A,C)               price of commodity c from activity a
 QA(A)                   level of domestic aggregate activity
 QAR(A,RD)               level of domestic regional activity
 QD(C)                   quantity of domestic sales
 QE(C,RW)                quantity of exports
 QF(F,A,RD)              quantity demanded of factor f from activity a
 QFS(F)                  quantity of factor supply
 QFS_FOR(F)              transfer of factor supply to meet capital shortfalls in energy (designed for energy capital)
 QG(C)                   quantity of government consumption
 QH(C,H)                 quantity consumed of marketed commodity c by household h
 QHA(A,H)                quantity consumed of home act a by hhd h
 QINT(C,A)               quantity of intermediate demand for c from activity a
 QINTA(A,RD)             quantity of aggregate intermediate input
 QINV(C,IT)              quantity of fixed investment demand
 QM(C,RW)                quantity of imports
 QQ(C)                   quantity of composite goods supply
 QT(C)                   quantity of trade and transport demand for commodity c
 QVA(A,RD)               quantity of aggregate value added
 QX(C)                   quantity of aggregate marketed commodity output
 QXAC(A,C)               quantity of ouput of commodity c from activity a
 TA(A,RD)                rate of tax on producer gross output value
 TAADJ                   activity tax scaling factor
 TABS                    total absorption
 TAPS                    point change in activity tax rate
 TINS(INS)               rate of direct tax on domestic institutions ins
 TINSADJ                 direct tax scaling factor
 TRII(INS,INSP)          transfers to dom. inst. insdng from insdngp
 TRNADJ                  scaling factor for gov transfers to domestic non-gov institutions
 TQ(C)                   rate of tax on sale of commodity
 TQADJ                   sales tax scaling factor
 TQPS                    point change in sales tax rate
 TQELEC(C)               point change in sales tax rate that avoids tq01(c)
 TVA(A,RD)               rate of tax on activity value-added
 TVAADJ                  value-added tax scaling factor
 TVAPS                   point change in value-added tax rate
 WALRAS                  savings-investment imbalance (should be zero)
 WALRASSQR               Walras squared
 WF(F)                   economy-wide wage (rent) for factor f
 WFDIST(F,A,RD)          factor wage distortion variable
 YF(F)                   factor income
 YG                      total current government income
 YIF(INS,F)              income of institution ins from factor f
 YI(INS)                 income of (domestic non-governmental) institution ins

 PQI(C,A,RD)
 PQH(C,H)
 PQE(C,RW)
 DTU(C)
 ALPHAQF                 proportionately adjust capital stocks
 YG_DTAX                 direct tax take plus transfers from abroad
 YG_ITAX                 indirect tax take
*fh 25062019
 YG_NTAX                 price differentials (utax code)
 ITAXSHR                 share of indirect taxes in total revenue (may be better if it were GDPMP)
 TAXADJ                  tax adjustment which maintains indirect tax share
 GDPMP                   nominal GDP at market prices
;

 CPI.L                  = CPI0;
 DMPS.L                 = DMPS0;
 DPI.L                  = DPI0;
 DTINS.L                = DTINS0;
 EG.L                   = EG0;
 EH.L(H)                = EH0(H);
 EXR.L                  = EXR0;
 FBOR.L                 = FBOR0;
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
 PVA.L(A,RD)            = PVA0(A,RD);
 PWE.L(C,RW)            = PWE0(C,RW);
 PWM.L(C,RW)            = PWM0(C,RW);
 PX.L(C)                = PX0(C);
 PXAC.L(A,C)            = PXAC0(A,C);
 QA.L(A)                = QA0(A);
 QAR.L(A,RD)            = QAR0(A,RD);
 QD.L(C)                = QD0(C);
 QE.L(C,RW)             = QE0(C,RW);
 QF.L(F,A,RD)           = QF0(F,A,RD);
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
 TRII.L(INSDNG,INSDNGP) = TRII0(INSDNG,INSDNGP);
 TINS.L(INSDNG)         = TINS0(INSDNG);
 TINSADJ.L              = TINSADJ0;
 TRNADJ.L               = TRNADJ0;
 TQ.L(C)                = TQ0(C);
 TQADJ.L                = TQADJ0;
 TQPS.L                 = TQPS0;
 TQELEC.L(C)            = TQELEC0(C);
 TVA.L(A,RD)            = TVA0(A,RD);
 TVAADJ.L               = 1;
 TVAPS.L                = 0;
 WALRAS.L               = WALRAS0;
 WALRASSQR.L            = 0 ;
 WF.L(F)                = WF0(F);
 WFDIST.L(F,A,RD)       = WFDIST0(F,A,RD);
 YF.L(F)                = YF0(f);
 YG.L                   = YG0;
 YI.L(INS)              = YI0(INS);
 YIF.L(INS,F)           = YIF0(INS,F);

 PQI.L(C,A,RD)          =  PQI0(C,A,RD);
 PQH.L(C,H)             =  PQH0(C,H);
*FH 25062019
 PQE.L(C,RW)            =  PQE0(C,RW);
 DTU.L(C) = 0;
 ALPHAQF.L=1;

*--------------------------------------------------------------------------------------------
*5. EQUATIONS
*--------------------------------------------------------------------------------------------

EQUATIONS
*Price block
 PWMDEF(C,RW)            world import price (adjusted by retaliatory tax)
 PMDEF(C,RW)             domestic import price
 PWEDEF(C,RW)            world export price (adjusted by retaliatory tax)
 PEDEF(C,RW)             domestic export price
 PDDDEF(C)               dem price for com'y c produced and sold domestically
 PQDEF(C)                value of sales in domestic market
 PXDEF(C)                value of marketed domestic output
 PADEF(A)                output price for national aggregate activity a
 PADEF2(A,RD)            output price for regional activity a
 PADEF3(A)               Definition of PA for CET aggregator
 PINTADEF(A,RD)          price of aggregate intermediate input
 PVADEF(A,RD)            value-added price
 CPIDEF                  consumer price index
 DPIDEF                  domestic producer price index
*Production and trade block
 QADEF(A)                define national aggregate as sum of regional production
 LEOAGGINT(A,RD)         Leontief aggreg intermed dem (if Leontief top nest)
 LEOAGGVA(A,RD)          Leontief aggreg value-added dem (if Leontief top nest)
 CESVAPRD(A,RD)          CES value-added production function
 CESVAFOC(F,A,RD)        CES value-added first-order condition
 LEOVAPRD(A,RD)          Leontief value-added production function
 LEOVAFOC(F,A,RD)        Leontief value-added first-order condition
 QACES(A)                CES activity output aggregation function
 QACESFOC(A,RD)          CES activity output aggregation function first-order condition
 INTDEM(C,A)             intermediate demand for commodity c from activity a
 COMPRDFN1(A,C)          production function for commodity c and activity a (LEO)
 COMPRDFN2(A,C)          production function for commodity c and activity a (CET)
 OUTAGGFN(C)             output aggregation function
 OUTAGGFOC(A,C)          first-order condition for output aggregation function
 CET(C)                  CET function
 CET2(C)                 domestic sales and exports for outputs without both
 ESUPPLY(C,RW)           export supply
 ARMINGTON(C)            composite commodity aggregation function
 COSTMIN(C,RW)           first-order condition for composite commodity cost min
 ARMINGTON2(C)           comp supply for com's without both dom. sales and imports
 QTDEM(C)                demand for transactions (trade and transport) services
*Institution block
 YFDEF(F)                factor incomes
 YIFDEF(INS,F)           factor incomes to domestic institutions
 YIDEF(INS)              total incomes of domest non-gov't institutions
 EHDEF(H)                household consumption expenditures
 TRIIDEF(INS,INSP)       transfers to inst'on ins from inst'on insp
 HMDEM(C,H)              LES cons demand by hhd h for marketed commodity c
 HADEM(A,H)              LES cons demand by hhd h for home commodity c fr act a
 INVDEM(C,IT)            fixed investment demand
 GOVDEM(C)               government consumption demand
 EGDEF                   total government expenditures
 YGDEF                   total government income
*System constraint block
 COMEQUIL(C)             composite commodity market equilibrium
 FACEQUIL(F)             factor market equilibrium
 LABSUP(F)               upward-sloping labor supply curve (on real wage)
 CURACCBAL               current account balance (of RoW)
 GOVBAL                  government balance
 TINSDEF(INS)            direct tax rate for inst ins
 MPSDEF(INS)             marg prop to save for inst ins
 TADEF(A,RD)             indirect tax rate for activity a rd
 TQDEF(C)                sales tax rate for commodity c
 TVADEF(A,RD)            value-added tax rate for activity a
 SAVINVBAL               savings-investment balance
 TABSEQ                  total absorption
 INVABEQ                 investment share in absorption
 GDABEQ                  government consumption share in absorption
 CESVAPRD2(F,A,RD)
 CESVAFOC2(F,FP,A,RD)
 WFDEF(F)
 EXPRESID1(C,RW)
 EXPRESID2(C,RW)
*utax code
 PQIDEF(C,A,RD)
 PQHDEF(C,H)
*FH 25062019
 PQEDEF(C,RW)
 QFS_FORDEF              transfer capital between types
 QFDEF(F,A,RD)           accomodate factor transfer by proportionately changing factor demands
 QFMAX(F,A,RD)

*CA added equations
 YG_DTAXDEF  define direct taxes plus tranfers to government from abroad
 YG_ITAXDEF  define indirect taxes
*fh 25062019
 YG_NTAXDEF  price differentials (utax code)
 ITAXEQ      share of indirect taxes in total revenues (may be better if this were GDPMP)
 MINQFEQ(F)  lower limit on returns to energy capital (complementarity reduces capital usage)
 MAXQFEQ(F)  upper limit on returns to energy capital (spurs purchase of capital to produce energy- incurs debt)
 GDPMPDEF    define GDP at market prices
;

*Carbon tax rates
 tco2d0  = 0;
 tco2e0  = 0;
 rtco2e0 = 0;
 tco2m0  = 0;
*CO2 emissions factors
 co2c0(C) = 0;
*FH 30/01/2019 co2c0('CCOAL')     = CALIB('CCOAL','EMc');
*FH 30/01/2019 co2c0('CCOAL-DIS') = CALIB('CCOAL-DIS','EMc');
 co2c0('CCOAL-LOW') = CALIB('CCOAL-LOW','EMc');
 co2c0('CCOAL-HGH') = CALIB('CCOAL-HGH','EMc');
 co2c0('CCOIL')     = CALIB('CCOIL','EMc');
 co2c0('CNGAS')     = CALIB('CNGAS','EMc');

 co2c0(C) = CALIB(C,'EMc');



*Initialize
 tco2d   = tco2d0;
 tco2e   = tco2e0;
 rtco2e  = rtco2e0;
 tco2m   = tco2m0;
 co2c(C) = co2c0(C);


$include includes\1carbon.inc
*Calibrate Tracking indirect taxation as a share of GDP
 YG_DTAX0 = SUM(INSDNG, TINS0(INSDNG)*YI0(INSDNG))
           + SUM(F, tf0(F)*YF0(F))
           + SUM(F, YIF0('GOV',F))
           + trnsfr0('GOV','ROW')*EXR0 ;

 YG_ITAX0 =
           SUM((A,RD), TVA0(A,RD)*PVA0(A,RD)*QVA0(A,RD))
           + SUM((A,RD), TA0(A,RD)*PAR0(A,RD)*QAR0(A,RD))
           + SUM((CM,RW), tm0(CM,RW)*PWM0(CM,RW)*QM0(CM,RW))*EXR0
           + SUM((CE,RW), te0(CE,RW)*PWE0(CE,RW)*QE0(CE,RW))*EXR0
           + SUM(C, TQ0(C)*PQ0(C)*QQ0(C))
           + SUM(C, co2c(C)*tco2d*QQ0(C))
           + SUM((CM,RW), co2m(CM,RW)*tco2m*QM0(CM,RW))
           - SUM((CE,RW), co2e(CE,RW)*tco2e*QE0(CE,RW))
;

*fh 25062019
 YG_NTAX0 =
*utax code
           SUM((C,A,RD), tui(C,A,RD)*PQ0(C)*QINTA0(A,RD)*ica0(C,A,RD))
           + SUM((C,H), tuh(C,H)*PQ0(C)*QH0(C,H))
           + SUM((C,RW), tue(C,RW)*PE0(C,RW)*QE0(C,RW))
         ;

  Abort$(ABS(YG0 - YG_DTAX0 - YG_ITAX0 - YG_NTAX0) GT .00000001) "tax calculations incorrect", YG0, YG_DTAX0, YG_ITAX0, YG_NTAX0;

  GDPMP0 = SUM((A,RD), PVA0(A,RD)*(1-TVA0(A,RD))*QVA0(A,RD)) + YG_ITAX0 + YG_NTAX0;

  ITAXSHR0 = (YG_ITAX0+YG_NTAX0)/GDPMP0;
  TAXADJ0      = 1;

 YG_DTAX.L =    YG_DTAX0 ;
 YG_ITAX.L =    YG_ITAX0 ;
 YG_NTAX.L =    YG_NTAX0 ;
 ITAXSHR.L =    ITAXSHR0 ;
 TAXADJ.L  =    TAXADJ0  ;
 GDPMP.L   =    GDPMP0   ;

*Price block

 PWMDEF(C,RW)$CMRW(C,RW)..       PWM(C,RW) =E= pwmbar(C,RW);

 PMDEF(C,RW)$CMRW(C,RW)..        PM(C,RW) =E= PWM(C,RW)*(1 + tm(C,RW))*EXR + SUM(CT, PQ(CT)*icm(CT,C)) + tco2m*co2m(C,RW);

*bm PWEDEF(C,RW)$CERW(C,RW)..       PWE(C,RW) =E= pwebar(C,RW) - rtco2e*(co2e(C,RW)+co2c(C));
 PWEDEF(C,RW)$CERW(C,RW)..       PWE(C,RW) =E= pwebar(C,RW) - rtco2e*(co2e(C,RW));

 PEDEF(C,RW)$CERW(C,RW)..        PE(C,RW) =E= (PWE(C,RW)*(1 - te(C,RW)))*EXR - SUM(CT, PQ(CT)*ice(CT,C)) + tco2e*co2e(C,RW);

 PDDDEF(C)$CD(C)..               PDD(C) =E= PDS(C) + SUM(CT, PQ(CT)*icd(CT,C));

*utax code
 PQIDEF(C,A,RD)$ica0(C,A,RD)..   PQI(C,A,RD)  =E= PQ(C)*(1+tui(C,A,RD));

*utax code
 PQHDEF(C,H)$QH0(C,H)..          PQH(C,H)     =E= PQ(C)*(1+tuh(C,H));

*utax code CA removed from model
* PQEDEF..                        PQ0('CELEC') =L= PQ('CELEC');

 PQDEF(C)$((CD(C) OR CM(C)))..   PQ(C)*(1 - TQ(C))*QQ(C) - tco2d*co2c(C)*QQ(C) =E= PDD(C)*QD(C) + SUM(RW, PM(C,RW)*QM(C,RW));

 PXDEF(C)$CX(C)..                PX(C)*QX(C) =E= PDS(C)*QD(C) + SUM(RW, PE(C,RW)*QE(C,RW));

 PADEF(A)$(QA0(A) AND (NOT ACET2(A))).. PA(A) =E= SUM(C, PXAC(A,C)*theta(A,C));

 PADEF3(A)$(QA0(A) AND ACET2(A))..
         PA(A)*(QA(A) - SUM(H, QHA(A,H))) =E= SUM(C, PXAC(A,C)*QXAC(A,C));

*utax code
* PINTADEF(A,RD)$QVA0(A,RD)..     PINTA(A,RD) =E= SUM(C, PQ(C)*ica(C,A,RD)) ;
 PINTADEF(A,RD)$QVA0(A,RD)..     PINTA(A,RD) =E= SUM(C, PQI(C,A,RD)*ica(C,A,RD)) ;

 PVADEF(A,RD)$QVA0(A,RD)..       PAR(A,RD)*(1-TA(A,RD))*QAR(A,RD) =E= PVA(A,RD)*QVA(A,RD) + PINTA(A,RD)*QINTA(A,RD) ;

*utax code
 CPIDEF..                        CPI =E= SUM(C, cwts(C)*PQ(C)) ;
* CPIDEF..                        CPI =E= SUM((C,H), cwtsh(C,H)*PQH(C,H)) ;

 DPIDEF..                        DPI =E= SUM(CD$(NOT CERES(CD)), dwts(CD)*PDS(CD)) ;

*Production and trade block

 LEOAGGINT(A,RD)$QVA0(A,RD)..    QINTA(A,RD) =E= inta(A,RD)*QAR(A,RD);

 LEOAGGVA(A,RD)$QVA0(A,RD)..     QVA(A,RD) =E= iva(A,RD)*QAR(A,RD);

 QFMAX(F,A,RD)..                   QF(F,A,RD) =G= 0;

 CESVAPRD(A,RD)$(QVA0(A,RD) AND NOT AFLEO(A))..   QVA(A,RD) =E= alphava(A,RD)*
         (SUM(F$MFA1(F,A,RD), deltava(F,A,RD)*(fprd(F,A,RD)*QF(F,A,RD))**(-rhova(A,RD))) )**(-1/rhova(A,RD)) ;

 CESVAFOC(F,A,RD)$(MFA1(F,A,RD) AND QF0(F,A,RD) AND QVA0(A,RD)  AND NOT AFLEO(A))..
   WF(F)*WFDIST(F,A,RD) =E= PVA(A,RD)*(1-TVA(A,RD)) * QVA(A,RD) *
         SUM(FP, deltava(FP,A,RD)*(fprd(FP,A,RD)*QF(FP,A,RD))**(-rhova(A,RD)))**(-1)
         *deltava(F,A,RD)*fprd(F,A,RD)**(-rhova(A,RD))*QF(F,A,RD)**(-rhova(A,RD)-1);

 CESVAPRD2(F,A,RD)$(SUM(FP, MFA2(F,FP,A,RD)) AND NOT AFLEO(A))..
   QF(F,A,RD) =E= alphava2(F,A,RD)*
         (SUM(FP$MFA2(F,FP,A,RD), deltava2(F,FP,A,RD)*(fprd(FP,A,RD)*QF(FP,A,RD))**(-rhova2(F,A,RD))))**(-1/rhova2(F,A,RD)) ;

 CESVAFOC2(F,FP,A,RD)$(MFA2(F,FP,A,RD) AND NOT AFLEO(A))..
   WF(FP)*wfdist(FP,A,RD) =E= WF(F)*WFDIST(F,A,RD) * QF(F,A,RD) *
         SUM(FPP$MFA2(F,FPP,A,RD), deltava2(F,FPP,A,RD)*(fprd(FPP,A,RD)*QF(FPP,A,RD))**(-rhova2(F,A,RD)))**(-1)
         *deltava2(F,FP,A,RD)*fprd(FP,A,RD)**(-rhova2(F,A,RD))*QF(FP,A,RD)**(-rhova2(F,A,RD)-1);

 WFDEF(F)$SUM((FP,A,RD), MFA2(F,FP,A,RD))..
         WF(F) =E= SUM((FP,A,RD)$MFA2(F,FP,A,RD), WFDIST(FP,A,RD)*WF(FP)*QF(FP,A,RD) ) / SUM((FP,A,RD)$MFA2(F,FP,A,RD), QF(FP,A,RD) );

 LEOVAPRD(A,RD)$(QVA0(A,RD) AND AFLEO(A)).. PVA(A,RD)*QVA(A,RD) =E= SUM(F, QF(F,A,RD)*WF(F)*WFDIST(F,A,RD));

*SG
* LEOVAFOC(F,A,RD)$(QF0(F,A,RD) AND AFLEO(A)).. QF(F,A,RD) =E= ifa(F,A,RD)*QVA(A,RD);
 LEOVAFOC(F,A,RD)$(QF0(F,A,RD) AND AFLEO(A)).. QF(F,A,RD)*leova(A,RD) =E= ifa(F,A,RD)*QVA(A,RD);

 QADEF(A)$(NOT ACES2(A) AND QA0(A)).. QA(A) =E=  SUM(RD, QAR(A,RD));

 PADEF2(A,RD)$(NOT ACES2(A) AND QAR0(A,RD))..
   PAR(A,RD)*QAR(A,RD) =E= PA(A)*QA(A) - SUM(RDP$(NOT RD(RDP)), PAR(A,RDP)*QAR(A,RDP));

 QACES(A)$(ACES2(A) AND QA0(A))..
   QA(A) =E= alphaa2(A)*(SUM(RD$deltaa2(A,RD), deltaa2(A,RD)*QAR(A,RD)**(-rhoa2(A))))**(-1/rhoa2(A));

* QACESFOC(A,RD)$(ACES2(A) AND QA0(A) AND deltaa2(A,RD))..
 QACESFOC(A,RD)$(ACES2(A) AND QA0(A) AND QAR0(A,RD))..
   PAR(A,RD) =E= PA(A)*deltaa2(A,RD)*(SUM(RDP$deltaa2(A,RDP), deltaa2(A,RDP)*QAR(A,RDP)**(-rhoa2(A))))**(-1)*QA(A)*QAR(A,RD)**(-rhoa2(A)-1);

 INTDEM(C,A)$SUM(RD,ica(C,A,RD))..  QINT(C,A) =E= SUM(RD,ica(C,A,RD)*QINTA(A,RD));

parameter betaca(A,C);
betaca(A,C)=1;
*betaca('acoal','ccoal-low')=1;

 COMPRDFN2(A,C)$(ACET2(A) AND deltaca(A,C))..
  QXAC(A,C)*betaca(A,C) =E= (QA(A) - SUM(H, QHA(A,H)))*(PXAC(A,C)/ (PA(A)*deltaca(A,C)*alphaca(A)**rhoca(A)))**(1/(rhoca(A)-1)) ;

* COMPRDFN2(A,C)$(ACET2(A) AND deltaca(A,C))..
*  QXAC(A,C) =E= (QA(A) - SUM(H, QHA(A,H)))*(PXAC(A,C)/ (PA(A)*deltaca(A,C)*alphaca(A)**rhoca(A)))**(1/(rhoca(A)-1)) ;

 COMPRDFN1(A,C)$(NOT ACET2(A) AND theta(A,C))..
         QXAC(A,C) =E= theta(A,C)*(QA(A) - SUM(H, QHA(A,H))) ;

 OUTAGGFN(C)$CX(C)..     QX(C) =E= alphaac(C)*SUM(A, deltaac(A,C)*QXAC(A,C)**(-rhoac(C)))**(-1/rhoac(C));

 OUTAGGFOC(A,C)$deltaac(A,C)..
   PXAC(A,C) =E= PX(C)*QX(C) * SUM(AP, deltaac(AP,C)*QXAC(AP,C)**(-rhoac(C)) )**(-1)*deltaac(A,C)*QXAC(A,C)**(-rhoac(C)-1);

 CET(C)$(CE(C) AND CD(C) AND (NOT CERES(C)))..
   QX(C) =E= alphat(C)*(SUM(RW, deltat(C,RW)*QE(C,RW)**rhot(C)) + (1 - SUM(RW, deltat(C,RW)))*QD(C)**rhot(C))**(1/rhot(C)) ;

 ESUPPLY(C,RW)$(CERW(C,RW) AND CD(C) AND (NOT CERES(C)))..
   QE(C,RW) =E=  QD(C)*((PE(C,RW)/PDS(C))*((1 - SUM(RWP, deltat(C,RWP)))/deltat(C,RW)))**(1/(rhot(C)-1)) ;

*SR Perfect substitutes for CERES exports
 EXPRESID1(C,RW)$(CX(C) AND CERES(C) AND QE0(C,RW)).. QX(C)  =E= QE(C,RW) + QD(C);
 EXPRESID2(C,RW)$(CX(C) AND CERES(C) AND QE0(C,RW) AND NOT CEFIX(C)).. PDS(C) =G= PE(C,RW) ;
 QE.LO(C,RW)$(QE0(C,RW) AND CERES(C)) = 0 ;
*CA Note that there is no parallel structure for imports, which is OK if imports are fixed


 CET2(C)$((CD(C) AND CEN(C)) OR (CE(C) AND CDN(C)) OR CERES(C))..
   QX(C) =E= QD(C) + SUM(RW, QE(C,RW));

 ARMINGTON(C)$(CM(C) AND CD(C) AND (NOT CMRES(C)))..
   QQ(C) =E= alphaq(C)*(SUM(RW, deltaq(C,RW)*QM(C,RW)**(-rhoq(C))) + (1-SUM(RW, deltaq(C,RW)))*QD(C)**(-rhoq(C)))**(-1/rhoq(C));

 COSTMIN(C,RW)$(CMRW(C,RW) AND CD(C) AND (NOT CMRES(C)))..
   QM(C,RW)/QD(C) =E= (PDD(C)/PM(C,RW)*deltaq(C,RW)/(1-SUM(RWP, deltaq(C,RWP))))**(1/(1+rhoq(C)));

 ARMINGTON2(C)$( ((CD(C) AND CMN(C)) OR (CM(C) AND CDN(C))) OR CMRES(C))..
   QQ(C) =E= QD(C) + SUM(RW, QM(C,RW));

 QTDEM(C)$CT(C)..
  QT(C) =E= SUM(CP, icd(C,CP)*QD(CP)) + SUM((CP,RW), icm(C,CP)*QM(CP,RW)) + SUM((CP,RW), ice(C,CP)*QE(CP,RW));

*Institution block

 YFDEF(F)$FD(F)..   YF(F) =E= WF(F)*SUM((A,RD), WFDIST(F,A,RD)*QF(F,A,RD));

 YIFDEF(INSD,F)$shif(INSD,F)..
   YIF(INSD,F) =E= shif(INSD,F)*((1-tf(f))*(1-rf(F))*YF(F) - trnsfr('ROW',F)*EXR);

 YIDEF(INSDNG).. YI(INSDNG) =E= SUM(F, YIF(INSDNG,F))  + SUM(INSDNGP, TRII(INSDNG,INSDNGP))
             + trnsfr(INSDNG,'GOV')*CPI*(1+TRNADJ*trn01(INSDNG)) + trnsfr(INSDNG,'ROW')*EXR;

 TRIIDEF(INSDNG,INSDNGP)$( shii(INSDNG,INSDNGP))..
   TRII(INSDNG,INSDNGP) =E= shii(INSDNG,INSDNGP) * (1 - MPS(INSDNGP)) * (1 - TINS(INSDNGP))* YI(INSDNGP);

 EHDEF(H).. EH(H) =E= (1 - SUM(INSDNG, shii(INSDNG,H))) * (1 - MPS(H)) * (1 - TINS(H)) * YI(H);

*utax code
* HMDEM(C,H)$( betam(C,H))..
*  PQ(C)*QH(C,H) =E= PQ(C)*gammam(C,H) + betam(C,H)*(EH(H) - SUM(CP, PQ(CP)*gammam(CP,H)) - SUM(A, PA(A)*gammah(A,H))) ;
 HMDEM(C,H)$( betam(C,H))..
  PQH(C,H)*QH(C,H) =E= PQH(C,H)*gammam(C,H) + betam(C,H)*(EH(H) - SUM(CP, PQH(CP,H)*gammam(CP,H)) - SUM(A, PA(A)*gammah(A,H))) ;

*utax code
* HADEM(A,H)$( betah(A,H))..
*  PA(A)*QHA(A,H) =E= PA(A)*gammah(A,H) + betah(A,H)*(EH(H) - SUM(CP, PQ(CP)*gammam(CP,H)) - SUM(AP, PA(AP)*gammah(AP,H))) ;
 HADEM(A,H)$( betah(A,H))..
  PA(A)*QHA(A,H) =E= PA(A)*gammah(A,H) + betah(A,H)*(EH(H) - SUM(CP, PQH(CP,H)*gammam(CP,H)) - SUM(AP, PA(AP)*gammah(AP,H))) ;

*bm INVDEM(C,IT)$(qbarinv(C,IT))..  QINV(C,IT) =E= IADJ(IT)*qbarinv(C,IT);
  INVDEM(C,'s-i')$(qbarinv(C,'s-i'))..  QINV(C,'s-i') =E= IADJ('s-i')*qbarinv(C,'s-i');

 GOVDEM(C)$(qbarg(C))..  QG(C) =E= GADJ*qbarg(C);

 YG_DTAXDEF .. YG_DTAX =E= SUM(INSDNG, TINS(INSDNG)*YI(INSDNG))
           + SUM(F, tf(F)*YF(F))
           + SUM(F, YIF('GOV',F))
           + trnsfr('GOV','ROW')*EXR ;

 YG_ITAXDEF .. YG_ITAX =E=
           + SUM((A,RD), TVA(A,RD)*PVA(A,RD)*QVA(A,RD))
           + SUM((A,RD), TA(A,RD)*PAR(A,RD)*QAR(A,RD))
           + SUM((CM,RW), tm(CM,RW)*PWM(CM,RW)*QM(CM,RW))*EXR
           + SUM((CE,RW), te(CE,RW)*PWE(CE,RW)*QE(CE,RW))*EXR
           + SUM(C, TQ(C)*PQ(C)*QQ(C))
           + SUM(C, co2c(C)*tco2d*QQ(C))
           + SUM((CM,RW), co2m(CM,RW)*tco2m*QM(CM,RW))
           - SUM((CE,RW), co2e(CE,RW)*tco2e*QE(CE,RW));

 YG_NTAXDEF .. YG_NTAX =E=
*utax code
           + SUM((C,A,RD), tui(C,A,RD)*PQ(C)*QINTA(A,RD)*ica(C,A,RD))
           + SUM((C,H), tuh(C,H)*PQ(C)*QH(C,H))
           + SUM((C,RW), tue(C,RW)*PE(C,RW)*QE(C,RW))
         ;

 YGDEF .. YG =E= YG_DTAX + YG_ITAX + YG_NTAX;

 EGDEF.. EG =E= SUM(C, PQ(C)*QG(C)) + SUM(INSDNG, trnsfr(INSDNG,'GOV')*(1+TRNADJ*trn01(INSDNG)))*CPI;

*System constraint block

 FACEQUIL(F)$QFS0(F).. SUM((A,RD), QF(F,A,RD)) =E= QFS(F)+QFS_FOR(F);

 QFDEF('fcap',A,RD)$QF0('fcap',A,RD).. QF('fcap',A,RD) =E= ALPHAQF*qfbar('fcap',A,RD);

 QFS_FORDEF .. SUM(FCAP,QFS_FOR(FCAP))=E=0;

*JT: AE paper (removed this code)
* LABSUP(F)$(QFS0(F) AND FLS(F)).. QFS(F) =E= QFS_BAR(F) * ( WF(F)/WF_BAR(F))**rhof(F);
 LABSUP(F)$(QFS0(F) AND FLS(F)).. QFS(F) =E= QFS0(F) * ( (SUM((A,RD), QF(F,A,RD)*WFDIST(F,A,RD)*WF(F))/QFS(F)/CPI) / (WF0(F)/CPI0) )**rhof(F);

 COMEQUIL(C)$(CD(C) OR CM(C))..
  QQ(C) =E= SUM(A, QINT(C,A)) + SUM(H, QH(C,H)) + QG(C) + SUM(IT, QINV(C,IT)) + qdst(C) + QT(C);

 CURACCBAL..
   SUM((CM,RW), PWM(CM,RW)*QM(CM,RW)) + SUM(F, trnsfr('ROW',F)) + SUM(F, (1-tf(f))*rf(F)*YF(F))/EXR
         =E= SUM((CE,RW), PWE(CE,RW)*QE(CE,RW)) + SUM(INSD, trnsfr(INSD,'ROW')) + (FSAV - irate*debt);

 GOVBAL.. YG =E= EG + GSAV;

 TINSDEF(INSDNG).. TINS(INSDNG) =E= TAXADJ*(tinsbar(INSDNG)*(1+TINSADJ*tins01(INSDNG)) + DTINS*tins01(INSDNG));

 TADEF(A,RD)..     TA(A,RD)*TAXADJ =E= tabar(A,RD)*(1+TAADJ*ta01(A,RD)) + TAPS*ta01(A,RD);

 TQDEF(C)..        TQ(C)*TAXADJ =E= tqbar(C) * (1+TQADJ*tq01(C)) + TQPS*tq01(C) + TQELEC(C);

 TVADEF(A,RD)..    TVA(A,RD)*TAXADJ =E= tvabar(A,RD) * (1+TVAADJ*tva01(A,RD)) + TVAPS*tva01(A,RD);

 MPSDEF(INSDNG)..  MPS(INSDNG)  =E= mpsbar(INSDNG)*(1+MPSADJ*mps01(INSDNG)) + DMPS*mps01(INSDNG);

 SAVINVBAL..
   SUM(INSDNG, MPS(INSDNG) * (1 - TINS(INSDNG)) * YI(INSDNG)) + GSAV + (FSAV - irate*debt)*EXR =E=
   SUM((IT,C), PQ(C)*QINV(C,IT)) + SUM(C, PQ(C)*qdst(C)) + WALRAS;

*utax code
 TABSEQ..
*   TABS =E= SUM((C,H), PQ(C)*QH(C,H)) + SUM((A,H), PA(A)*QHA(A,H)) +
*   SUM(C, PQ(C)*QG(C)) + SUM((IT,C), PQ(C)*QINV(C,IT)) + SUM(C, PQ(C)*qdst(C));
   TABS =E= SUM((C,H), PQH(C,H)*QH(C,H)) + SUM((A,H), PA(A)*QHA(A,H)) +
   SUM(C, PQ(C)*QG(C)) + SUM((IT,C), PQ(C)*QINV(C,IT)) + SUM(C, PQ(C)*qdst(C));

 INVABEQ.. INVSHR*TABS =E= SUM((IT,C), PQ(C)*QINV(C,IT)) + SUM(C, PQ(C)*qdst(C));

 GDABEQ..  GOVSHR*TABS =E= SUM(C, PQ(C)*QG(C));

*Equations added for this analysis
 GDPMPDEF ..   GDPMP =E= SUM((A,RD), PVA(A,RD)*QVA(A,RD)) + YG_ITAX + YG_NTAX;

 ITAXEQ .. ITAXSHR*GDPMP  =E= YG_ITAX + YG_NTAX ;

 MINQFEQ('FEGY')$QF0('FEGY','AELEC','nat') ..  (1-alphawfdist)*WFK2AV('FCAP') =L= WF('FEGY')*WFDIST('FEGY','AELEC','nat') ;
 MAXQFEQ('FEGY')$QF0('FEGY','AELEC','nat') ..  (1+alphawfdist)*WFK2AV('FCAP') =G= WF('FEGY')*WFDIST('FEGY','AELEC','nat') ;

*$offtext
*--------------------------------------------------------------------------------------------
*6. MODEL
*--------------------------------------------------------------------------------------------

MODEL STANDCGE  standard CGE model
 /
 MINQFEQ.QFS
 MAXQFEQ.QFS_FOR
*Price block
 PWMDEF
 PMDEF
 PWEDEF.PWE
 PEDEF.PE
 PQDEF
 PXDEF
*PXDEF2
 PDDDEF.PDD
 PADEF
 PADEF3
 PINTADEF.PINTA
 PVADEF.PVA
 CPIDEF
 DPIDEF
*Production and trade block
 QADEF.QA
 PADEF2.QAR
 QACES
 QACESFOC
 LEOAGGINT.QINTA
 LEOAGGVA
 CESVAPRD.QVA
 CESVAFOC
 CESVAPRD2
 CESVAFOC2
 WFDEF
 LEOVAPRD
 LEOVAFOC
 INTDEM.QINT
 COMPRDFN1
 COMPRDFN2
 OUTAGGFN.QX
 OUTAGGFOC.QXAC
 CET
 CET2
 ESUPPLY
 ARMINGTON
 COSTMIN
 ARMINGTON2
 QTDEM.QT
*Institution block
 YFDEF.YF
 YIFDEF.YIF
 YIDEF.YI
 EHDEF.EH
 TRIIDEF.TRII
 HMDEM.QH
* HADEM.QHA
 EGDEF.EG
 YGDEF.YG
 GOVDEM.QG
 GOVBAL
 INVDEM.QINV
*System-constraint block
 FACEQUIL
 LABSUP
 COMEQUIL
 CURACCBAL
 SAVINVBAL.WALRAS
 TINSDEF.TINS
 MPSDEF.MPS
 TADEF.TA
 TQDEF.TQ
 TVADEF.TVA
 TABSEQ.TABS
 INVABEQ
 GDABEQ
*utax code
 PQIDEF.PQI
 PQHDEF.PQH
* PQEDEF.TQPS
* EXPRESID1
 EXPRESID2.QE
 QFDEF
 QFS_FORDEF
 YG_DTAXDEF
 YG_ITAXDEF
 YG_NTAXDEF
 ITAXEQ
 GDPMPDEF
* QFMAX
/;

*--------------------------------------------------------------------------------------------
*7. FIXING VARIABLES NOT IN MODEL AT ZERO
*--------------------------------------------------------------------------------------------

 QA.FX(A)$(NOT QA0(A)) = 0;
 PA.FX(A)$(NOT QA0(A)) = 0;
 QAR.FX(A,RD)$(NOT QAR0(A,RD)) = 0;
 PAR.FX(A,RD)$(NOT QAR0(A,RD)) = 0;
 QVA.FX(A,RD)$(NOT QVA0(A,RD)) = 0;
 PVA.FX(A,RD)$(NOT QVA0(A,RD)) = 0;
 PDD.FX(C)$(NOT CD(C)) = 0;
 PDS.FX(C)$(NOT CD(C)) = 0;
 PE.FX(C,RW)$(NOT CERW(C,RW)) = 0;
 PM.FX(C,RW)$(NOT CMRW(C,RW)) = 0;
 PWE.FX(C,RW)$(NOT CERW(C,RW)) = 0;
 PWM.FX(C,RW)$(NOT CMRW(C,RW)) = 0;
 PX.FX(C)$(NOT CX(C)) = 0;
 PXAC.FX(A,C)$(NOT PXAC0(A,C)) = 0;
 QD.FX(C)$(NOT CD(C)) = 0;
 QE.FX(C,RW)$(NOT CERW(C,RW)) = 0;
*JT: 28-05-2013
 QF.FX(F,A,RD)$(NOT QF0(F,A,RD)) = 0;
* QF.FX(F,A,RD)$(NOT (MFA1(F,A,RD) + SUM(FP, MFA2(FP,F,A,RD)))) = 0;
 QG.FX(C)$(NOT SAM(C,'GOV')) = 0;
 QH.FX(C,H)$(NOT QH0(C,H)) = 0;
 QHA.FX(A,H)$(NOT BETAH(A,H)) = 0;
 QINT.FX(C,A)$(NOT QINT0(C,A)) = 0;
*JT: 28-05-2013
 QINTA.FX(A,RD)$(NOT QINTA0(A,RD)) = 0;
 QINV.FX(C,IT)$(NOT QINV0(C,IT)) = 0;
 QINV.FX(C,'s-e') = qbarinv(C,'s-e');
 QM.FX(C,RW)$(NOT CMRW(C,RW)) = 0;
 QQ.FX(C)$(NOT (CD(C) OR CM(C))) = 0;
 QT.FX(C)$(NOT CT(C)) = 0;
 QX.FX(C)$(NOT CX(C)) = 0;
 QXAC.FX(A,C)$(NOT QXAC0(A,C)) = 0;
 TRII.FX(INSDNG,INSDNGP)$(NOT SAM(INSDNG,INSDNGP)) = 0;
 YI.FX(INS)$(NOT INSD(INS)) = 0;
 YIF.FX(INS,F)$((NOT INSD(INS)) OR (NOT SAM(INS,F))) = 0;
 YF.FX(F)$(NOT FD(F) AND NOT YF0(F)) = 0;
*utax code
 PQH.FX(C,H)$(NOT QH0(C,H)) = 0;
 PQI.FX(C,A,RD)$(NOT ica0(C,A,RD)) = 0;
 PQE.FX(C,RW)$(NOT QE0(C,RW)) = 0;

*--------------------------------------------------------------------------------------------
*8. MODEL CLOSURE
*--------------------------------------------------------------------------------------------

*Factor markets
*Disaggregate factors:
 QFS.FX(F)$(NOT FLS(F)) = QFS0(F);
 WF.LO(F)           = -INF;
 WF.UP(F)           = +INF;
 WFDIST.FX(F,A,RD)  = WFDIST0(F,A,RD);

*Aggregate factors:
 WF.LO(F)$SUM((FP,A,RD), MFA2(F,FP,A,RD)) = -INF;
 WF.UP(F)$SUM((FP,A,RD), MFA2(F,FP,A,RD)) = +INF;
 QFS.LO(F)$SUM((FP,A,RD), MFA2(F,FP,A,RD)) = -INF;
 QFS.UP(F)$SUM((FP,A,RD), MFA2(F,FP,A,RD)) = +INF;
 WFDIST.LO(F,A,RD)$SUM(FP, MFA2(F,FP,A,RD))  = -INF;
 WFDIST.UP(F,A,RD)$SUM(FP, MFA2(F,FP,A,RD))  = +INF;
* QF.LO(F,A,RD)$SUM(FP, MFA2(F,FP,A,RD))  = -INF;
 QF.LO(F,A,RD)$SUM(FP, MFA2(F,FP,A,RD))  = eps;
 QF.UP(F,A,RD)$SUM(FP, MFA2(F,FP,A,RD))  = +INF;

*Current account of RoW
*EXR.FX       = EXR0;
 FBOR.FX      = FBOR0;
 FSAV.FX      = FSAV0;
* PWM.FX(C,RW) = PWM0(C,RW);

*Current government balance
*GSAV.FX     = GSAV0 ;
 TAADJ.FX    = TAADJ0;
 TAPS.FX     = TAPS0;
 TRNADJ.FX   = TRNADJ0;
 TQADJ.FX    = TQADJ0;
 TQPS.FX     = TQPS0;
 TQELEC.FX(C)= TQELEC0(C);
 TVAADJ.FX   = TVAADJ0;
 TVAPS.FX    = TVAPS0;
 TINSADJ.FX  = TINSADJ0;
 DTINS.FX    = DTINS0;
 GADJ.FX     = GADJ0;
*GOVSHR.FX   = GOVSHR0 ;

*Savings-investment balance
 MPSADJ.FX = MPSADJ0;
 DMPS.FX   = DMPS0;
 IADJ.FX(IT) = IADJ0(IT);
 IADJ.LO('S-I') = -INF;
 IADJ.UP('S-I') = +INF;

*INVSHR.FX = INVSHR0 ;

*Numeraire price index
 CPI.FX        = CPI0;
* DPI.FX        = DPI0;

*CA ELECTRICITY SECTOR MCP CLOSURE
*First set without capital transfer
WFDIST.UP('FCAP',A,RD)$QF0('FCAP',A,RD)=INF;
WFDIST.LO('FCAP',A,RD)$QF0('FCAP',A,RD)=-INF;
*QF.FX('FCAP',A,RD)$QF0('FCAP',A,RD)=QF0('FCAP',A,RD);
ALPHAQF.FX=1;
WF.FX('FCAP')=WF0('FCAP');
QFS.UP('FCAP')=INF;
QFS.LO('FCAP')=-INF;

WF.FX('FEGY')=WF0('FEGY');
WFDIST.UP('FEGY','AELEC','NAT')=INF;
WFDIST.LO('FEGY','AELEC','NAT')=-INF;

QFS.UP('FEGY')= QFS0('FEGY');
QFS.LO('FEGY')=-INF;

QFS_FOR.FX(F) = 0;
QFS_FOR.UP('FEGY')=+INF;

*Add capital transfer equation
QFS_FOR.UP(FCAP)=+INF;
QFS_FOR.lo(FCAP)=-INF;
QFS_FOR.LO('FEGY')=0;
ALPHAQF.LO= -INF;
ALPHAQF.UP= +INF;
QFS.FX('FCAP')=QFS0('FCAP');

*Hold indirect tax share of GDP constant
* TAXADJ.FX=TAXADJ0;
 ITAXSHR.FX=ITAXSHR0;

*--------------------------------------------------------------------------------------------
*9. SOLUTION STATEMENT
*--------------------------------------------------------------------------------------------

OPTIONS ITERLIM = 1000, LIMROW = 3000, LIMCOL = 0, SOLPRINT=ON, MCP=PATH, NLP=CONOPT;
 STANDCGE.HOLDFIXED   = 1 ;
 STANDCGE.TOLINFREP   = .0001 ;

 SOLVE STANDCGE USING MCP ;


$INCLUDE includes\1repbase.inc

DISPLAY STRUCBASE, SAMBALCHK;


PARAMETER
 CALIB2(AC,*)    Check energy calibration quantities
;

*FH 30/01/2019
* CALIB2('CCOAL-DIS','QA') = QX0('CCOAL-DIS');
 CALIB2('CCOAL-LOW','QA') = QX0('CCOAL-LOW');
 CALIB2('CCOAL-HGH','QA') = QX0('CCOAL-HGH');
* CALIB2('CCOAL','QA')     = CALIB2('CCOAL-DIS','QA') + CALIB2('CCOAL-LOW','QA') + CALIB2('CCOAL-HGH','QA');

* CALIB2('CCOAL-DIS','QM') = SUM(RW, QM0('CCOAL-DIS',RW));
 CALIB2('CCOAL-LOW','QM') = SUM(RW, QM0('CCOAL-LOW',RW));
 CALIB2('CCOAL-HGH','QM') = SUM(RW, QM0('CCOAL-HGH',RW));
* CALIB2('CCOAL','QM')     = CALIB2('CCOAL-DIS','QM') + CALIB2('CCOAL-LOW','QM') + CALIB2('CCOAL-HGH','QM');

* CALIB2('CCOAL-DIS','QE') = SUM(RW, QE0('CCOAL-DIS',RW));
 CALIB2('CCOAL-LOW','QE') = SUM(RW, QE0('CCOAL-LOW',RW));
 CALIB2('CCOAL-HGH','QE') = SUM(RW, QE0('CCOAL-HGH',RW));
* CALIB2('CCOAL','QE')     = CALIB2('CCOAL-DIS','QE') + CALIB2('CCOAL-LOW','QE') + CALIB2('CCOAL-HGH','QE');

* CALIB2('CCOAL-DIS','QQ') = QQ0('CCOAL-DIS');
 CALIB2('CCOAL-LOW','QQ') = QQ0('CCOAL-LOW');
 CALIB2('CCOAL-HGH','QQ') = QQ0('CCOAL-HGH');
* CALIB2('CCOAL','QQ')     = CALIB2('CCOAL-DIS','QQ') + CALIB2('CCOAL-LOW','QQ') + CALIB2('CCOAL-HGH','QQ');

 CALIB2('CCOIL','QA') = QX0('CCOIL');
 CALIB2('CCOIL','QM') = SUM(RW, QM0('CCOIL',RW));
 CALIB2('CCOIL','QE') = SUM(RW, QE0('CCOIL',RW));
 CALIB2('CCOIL','QQ') = QQ0('CCOIL');

 CALIB2('CNGAS','QA') = QX0('CNGAS');
 CALIB2('CNGAS','QM') = SUM(RW, QM0('CNGAS',RW));
 CALIB2('CNGAS','QE') = SUM(RW, QE0('CNGAS',RW));
 CALIB2('CNGAS','QQ') = QQ0('CNGAS');

 CALIB2('AELEC','QA') = QA0('AELEC');
 CALIB2('APETR','QA') = QA0('APETR');

parameter sica0(c,a,rd) scaled ica0 such that we get units of input per unit of output ;

sica0(c,a,rd)=ica0(c,a,rd)*inta(a,rd);


display sica0      ;

$ONTEXT
parameter gdpfc0
;
    GDPfc0 = SUM((A,RD), PVA0(A,RD)*(1-TVA0(A,RD))*QVA0(A,RD));

*CHECK CET CHANGE
 DELTACA('ACOAL','CCOAL-LOW')=.3;
 tm(c,rw)=0;
 SOLVE STANDCGE USING MCP;
 DISPLAY PQ.L, PQ0, walras.l, taxadj.l;
$OFFTEXT

* leova(A,RD) = 1.2;
 SOLVE STANDCGE USING MCP;

$INCLUDE FUELVOL.INC
$INCLUDE ICA.INC
