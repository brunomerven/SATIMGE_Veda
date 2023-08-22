SET
 AR                      all activity reporting accounts
 ARA(AR)                 aggregate activity reporting accounts
 FR      x                all factor reporting accounts
 FRA(FR)                 aggregate factor reporting accounts
 HR                      all household reporting accounts
 HRA(HR)                 aggregate household reporting accounts
 TR1(T)                  first year for reporting   / 2019 /
 TRN(T)                  final year for reporting   / 2050 /
 TTRN(TT)                final TIMES solve sequence for reporting
;

ALIAS (AR,ARP), (ARA,ARAP), (FR,FRP), (FRA,FRAP), (HR,HRP), (HRA,HRAP);

PARAMETER
 TRNUM                   number of years in CGE reporting
;

 TRNUM = SUM(T$TRN(T), ORD(T)) - SUM(T$TR1(T), ORD(T));
 TTRN(TT)$(ORD(TT) EQ SMAX(TTP, ORD(TTP))) = YES;

$include cge\includes\3tables.inc

PARAMETER
*Mappings
 ACTAGG(A,RD,AR)         model activities to reporting activities
 COMAGG(C,AR)            model activities to reporting activities
 FACAGG(F,FR)            model factors to reporting factors
 HHDAGG(H,HR)            model households to reporting households
*Activities based tables
 GDPX(AR,X,T,T)          Real GDP at factor cost
 GDPXA(AR,X,T)           average annual growth in real GDP at factor cost
 GDPXY(AR,X,T,T)         year-on-year growth in real GDP at factor cost
 GDPXS(AR,X,T,T)         shares of real GDP at factor cost
*GDP at market prices
 GDPDX(IGDP,X,T,T)       Real GDP at market prices
 GDPDXA(IGDP,X,T)        average annual growth in real GDP at market prices
 GDPDXY(IGDP,X,T,T)      year-on-year growth in real GDP at market prices
 GDPDXS(IGDP,X,T,T)      shares of real GDP at market prices
*Factors based tables
 FACX(FR,X,T,T)          factor supplies
 FACXA(FR,X,T)           average annual growth in factor supplies
 FACXY(FR,X,T,T)         year-on-year growth in factor supplies
 FACRX(FR,X,T,T)         factor returns
 FACRXA(FR,X,T)          average annual growth in factor returns
 FACRXY(FR,X,T,T)        year-on-year growth in factor returns
*Households based tables
 POPX(HR,X,T,T)          household population
 POPXA(HR,X,T)           average annual household population growth
 POPXY(HR,X,T,T)         year-on-year household population growth
 HHDCX(HR,X,T,T)         household real consumption
 HHDCXA(HR,X,T)          average annual growth in household real consumption
 HHDCXY(HR,X,T,T)        year-on-year growth in household real consumption
 HHDPCX(HR,X,T,T)        household real per capita consumption
 HHDPCXA(HR,X,T)         average annual growth in per capita household real consumption
 HHDPCXY(HR,X,T,T)       year-on-year growth in per capita household real consumption
 HHDBC(AR,HR,T)          base year household consumption patterns
 HHDBY(HR,*,T)           base year household income patterns
*Foreign trade tables
 TREX(AR,X,T,T)          real export values
 TRMX(AR,X,T,T)          real import values
 NETX(AR,X,T,T)          net real trade values (+ is exported)
 TREXA(AR,X,T)           average annual growth in real export values
 TRMXA(AR,X,T)           average annual growth in real import values
 TREXY(AR,X,T,T)         year-on-year growth in real export values
 TRMXY(AR,X,T,T)         year-on-year growth in real import values
*Structural base tables (with aggregates)
 STRUCBASE2(AR,STRCOL)   economic structure in the base
;

$call "gdxxrw i=cge\3report.xlsx o=reportdata index=index!a6 checkdate"
$gdxin reportdata.gdx

$load   AR FR HR
$loaddc ARA FRA HRA ACTAGG FACAGG HHDAGG

*Ad-hoc adjustments for South Africa Energy Model
 MAC('ACOIL','CCOIL') = YES;
 COMAGG(C,AR) = SUM((A,RD)$MAC(A,C), ACTAGG(A,RD,AR));


SET
 CO2AC / eCOAL, eCOIL, eNGAS, eELEC, ePETR, eAGR, eIND, eSRV, eHHD, eS-I, eGOV, eDSTK, eTOTAL  /
 CO2CTX /P-QQ, P-EC, P-CO2, D-COAL-QQ, D-COIL-QQ, D-NGAS-QQ, D-COAL-CO2, D-COIL-CO2, D-NGAS-CO2, D-TOTL-CO2, I-EC, I-ELEC-QQ, I-PETR-QQ, I-ELEC-CO2, I-PETR-CO2, I-TOTL-CO2, D+I-TOTL-CO2, REF-SEC-RAT /
$ontext
 MCO2(CO2AC,AC) /
  eELEC.(CELEC)
  ePETR.(CPETR)
  eAGR .(CAGRI,CFORE,CFISH)
  eIND .(CCOAL,CCOIL,CNGAS,CMORE,CMINE,CHYDR,CFOOD,CBEVT,CTEXT,CCLTH,CLEAT,CFOOT,CWOOD,CPAPR,CPRNT,CPETR,CBCHM,COCHM,CRUBB,CPLAS,CGLAS,CNMET,CIRON,CNFRM,CMETP,CMACH,CEMCH,CSEQU,CVEHI,CTEQU,CFURN,COMAN,CELEC,CWATR,CCONS)
  eSRV .(CTRAD,CHOTL,CPTRP-L,CFTRP-L,CTRPS-L,CPTRP-O,CFTRP-O,CTRPS-O,CCOMM,CFSRV,CBSRV,CGSRV,COSRV)
  eHHD .(HHD-0,HHD-1,HHD-2,HHD-3,HHD-4,HHD-5,HHD-6,HHD-7,HHD-8,HHD-91,HHD-92,HHD-93,HHD-94,HHD-95)
  eS-I .(S-I)
  eGOV .(GOV)
  eDSTK.(DSTK)
 /
$offtext
;

PARAMETER
 CO2TAB(CO2CTX,CO2AC,X,T)        detailed CO2 accounts for final times run
*Baseline tables (to check calibration)
 CO2TABBASE(*,CO2AC)              nearest base year for comparison with external accounts (2006)
;


*-----------------------------------------------------------------------------
* TIMES-SAGE REPORTING
*-----------------------------------------------------------------------------

SET
 RTAC(T) / 2010, 2015, 2020, 2025, 2030, 2035, 2040 /
 EGEAC   / GDPXA, ELCDEM, EGELAS /
 RFRAC(FR) / xLABOR, xxflab-p, xxflab-m, xxflab-s, xxflab-t, xLABUNSK, xLABSK /
;

 RTAC(T)$T1(T) = YES;
 RTAC(T)$TR1(T) = YES;
 RTAC(T)$TRN(T) = YES;

PARAMETER
*General
 REPYRS(T)               reporting period years
*Growth sheet
 tsGDPXA(AR,X)           g1 g2: average annual GDP growth rate over reporting period
 tsGDPSRC(AR,X)          g3: sectoral source of additional GDP growth
 tsGDPX(T,X)             g4: total GDP per capita over reporting period
*Jobs sheet
 tsFACXA(FR,X)           j1: total factor supply growth rates over reporting period
 tsNEWJOB(*,X)           j2: new jobs created over reporting period
 tsRTNXA(FR,X)           j3: average annual growth in factor returns over report period
 tsRTNXA(FR,X)           j3: average annual growth in factor returns over report period
 tsLABX(T,X)             j4: total labor supply over reporting period
 tsWLOWX(T,X)            j5: average low skilled labor wage over reporting period
 tsWHGHX(T,X)            j5: average high skilled labor wage over reporting period
 tsJOBSXA(AR,X)          j6: average annual labor employment growth rate over reporting period
*Welfare sheet
 tsPOPX(T,X)             w1: total population
 tsHHDBC(AR,HR)          w2: Base year household consumption patterns
 tsHHDBY(*,HR)           w3: Base year household income patterns
 tsHHDPCXA(HR,X)         w4: Average annual growth in per capita household real consumption
 tsPC050X(T,X)           w5: Per capita consumption for percentiles 0-50
 tsPC5090X(T,X)          w5: Per capita consumption for percentiles 50-90
 tsPC90100X(T,X)         w5: Per capita consumption for percentiles 90-100
*Electricity sheet
 tsELCDEM(T,X)           e1: total electricity demand projections (GWh)
 tsELCDTAB(T,X)          e1: electricity demand at selected reporting periods
 tsELCPRC(T,X)           e2: average electricity price projections (cents per KWh)
 tsELCPTAB(T,X)          e2: average electricity price at selected reporting periods
 tsELCSUP(X,RD,T)        e3: electricity supply composition
 tsELCGDP(EGEAC,X)       e4: electricity demand growth to GDP growth elasticity
 tsINVCST(T,X)           e5: electricity build plan annual investment cost
 tsEDEBT(T,X)            e6: build plan debt (accumulated investment cost less interest payments)
 tsINTGDP(T,X)           e7: electricity investment interest payments to GDP ratio
*Emissions sheet
 tsCO2REF(T,X)           m1: total emissions reference approach
 tsCO2SEC(T,X)           m2: total emissions sectoral approach
 tsEMSRC(CO2AC,X)        m3: beginning and final year emissions sources during reporting period
 tsEMTOT(T,X)            m4: total emissions for selected time periods
 tsEMELC(T,X)            m5: electricity sector emissions for selected time periods
;

 REPYRS(TR1) = 1;
 REPYRS(TRN) = 1;

SETS
 energy(c)       energy commodities     / ccoal-low, ccoal-hgh, ccoil, cngas, cpetr_p, cpetr_d, cpetr_o, celec/
 nonenergy(c)    non-energy commodities
 food(c)         food commodities        /cagri, cfish, cfood, cbevt/
 transport(c)    transport commodities   /cptrp-l ,cftrp-l ,cptrp-o ,cftrp-o ,ctrps/
 other(c)        all excluding food energy transport
 hh(hr)
;

 nonenergy(c)=YES;
 nonenergy(energy)=NO;
 other(c)=YES;
 other(energy)=NO;
 other(food)=NO;
 other(transport)=NO;

 hh(hr)=yes;
 hh('xALLHHD')=no;
 hh('xPOOR')=no;
 hh('xNPOOR')=no;
 hh('xNP40')=no;

SET
 CASE(RUN)
 Sub_case(X)
 YEAR(T)        Reporting time periods /2019, 2020, 2025, 2030, 2035, 2040, 2045, 2050/
;

 CASE(RUN)$INCLRUN(RUN)=YES;
 Sub_case(X)$XC(X)=YES;

PARAMETERS
 Calc(*,*,*,*,*,*,*)    GDP by expenditure group
 income(hr,X,T,TT)     Income of household deciles
 rank(hr)              Weight given to each household
 gini(X,T,TT)          Gini Coefficient
 mean(X,T,TT)          Average income of deciles
;

Table d(*,hr)

         xxhhd-0   xxhhd-1   xxhhd-2   xxhhd-3   xxhhd-4   xxhhd-5   xxhhd-6   xxhhd-7   xxhhd-8  xxhhd-9
  rank    10       9             8         7       6             5       4      3           2        1
;
