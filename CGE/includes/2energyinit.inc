*---------------------------------------------------------------
*INITIALIZATION FILE FOR LINKING TIMES AND CGE MODEL -----------
*---------------------------------------------------------------

SETS
*Forecasting sets
 FSXEP(FS)               sector groupings in SATIM to be used in ica growth mapping controlled below
 CXEP(C)                 commodities that get included in the ica adjustment
 EU                      end uses
*Mappings

* bm sets for adjusting budget share weightings
 CLN(H,HP,T)         Closest Lower Neighbour
 CUN(H,HP,T)         Closets Upper Neighbour

* bm sets for adjusting budget share weightings of energy goods
 CHouseholds(C) Commodities consumed by households
 CHEnergy(C) Energy Commodities consumed by households / ccoal-hgh, celec, cpetr-l, cpetr-k, cptrp-l , cprtr /

* Transport Demands
 TPPR(COM) Private Passenger Demand Commodities /TPPRCAR, TPPRMOT, TPPRSUV/



* leaving cptrp-o out for now, as it doesn't directly link to mode switching questions, this could be treated separately when looking at air travel questions.

 CHNonEnergy(C) Non-Energy Commodities consumed by households
;


;

Parameters
* parameters used for reverse mapping of households
*FH*----------------------------------------------------------------------------
 popval(H,X,AY)     bounds for reverse mapping
 popvalfact(H,X,AY) parameter required for top 10% split


* bm parameters used for updating ica for household consumption of energy services e.g. private transport
 EHIntensity(C,A,AY) energy intensity for household consumed energy services

* bm parameters used for adjusting budget share weightings
 BudShare(C,H,T)  budget share of marketed commodity c by household h

 BudShare0(C,H,T)  budget share of marketed commodity c by household h used as template


 DistanceMatrix(H,HP,T) distance to all

 TmpMin(H,T) minimum of row
 TmpLowerNeighbour(H,HP,T) lower neighbour
 TmpUpperNeighbour(H,HP,T) upper neighbour

 TotalDistance(H,T) sum of distances
 DistLower(H,T) distance to lower
 DistUpper(H,T) distance to upper
 WeightLower(H,T) weighting of lower
 WeightUpper(H,T) weighting of upper

* bm parameters to do budget reallocations of energy goods
 PQest(C,T)         price estimate for commodity based on ica change
 Rebound(T) rebound factor
 BudShareNoRebound(C,H,T)  assuming no Rebound
 BudShareFullRebound(C,H,T) assuming full Rebound
 BudShareNonEnergy(H,T) buget share of non-energy good for re-allocation calculations

;

* Identifying Energy goods consumed by households
CHouseholds(C)$sum(H,BUDSHR(C,H)) = YES;
CHNonEnergy(CHouseholds) = YES;
CHNonEnergy(CHEnergy) = NO;

*initialise rebound parameter (should be made a scenario parameter later)
Rebound(T) = 0;
PQX('celec',XC,'2019',TT) = PQ0('celec');
YIX(H,XC,'2019',TT) = YI0(H);
QHX('cprtr',H,XC,'2019',TT) = QH0('cprtr',H);
* mapping of households
*FH*----------------------------------------------------------------------------
 popvalfact(H,X,TC)=1;






ALIAS (EU,EUP,EUPP), (FH,FHP);





* set sectors that are linked with the main ica calc approach
FSXEP(FS) = Yes;
*FSXEP('COM') = No;
*FSXEP('AGR') = No;
*FSXEP('mi') = No;
*FSXEP('fb') = No;
*FSXEP('ot') = No;
*FSXEP('pp') = No;
*FSXEP('cp') = No;
*FSXEP('nm') = No;
*FSXEP('is') = No;
*FSXEP('nf') = No;
*FSXEP('elec') = No;
*FSXEP('tran') = No;
*FSXEP('petr') = No;
FSXEP('fa') = No;
FSXEP('al') = No;


* set commodities that get set using the main ica calc approach
CXEP(C) = Yes;
*CXEP('celec') = No;
*CXEP('cngas') = No;

* the fuels to be done in a different way
*b SETS
*b  MFUELP(FUELP,AC,*)      mapping fuel price fuels

*b ;



PARAMETER
*Forecasts based on CGE results
 PKMFH(FT,FH,AY)             Passenger km demand share by income group
 HFORE(FH,X,AY)           loaded: household income forecast for SATIM
 YHE(AY)                  income of higher income group for SATIM
 PFORE(FH,X,AY)           loaded: household population forecast for SATIM
 IFORE(AY,AC)             loaded: initial electricity demand forecast
 IEPGFORE(AY,AC)          loaded: initial electricity price growth forecast
*Residential
 FHMMB(FH,*,AY)

 FHMM(FH,*)              loaded: base year min max and mean for each cluster
 FHMEAN(FH,AY)            result: mean household income value in TIMES units
 FHFACT(FH,AY)            result: adjustment factor
 FHMOVE(FH,FHP,AY)        result: population movement between clusters
 FHPOP(FH,AY)             result: final cluster populations after reallocations
 ELECRATE(FH,AY)          loaded: household electriification rates
 TFHPOP(FH,AY)            result: population kept before elc split for transport
 TFHPOP2(FH,AY)           temp calc for TFHPOP
 TFHPOPMY(FH,AY)           result: population for TIMES milestone years
*Exporting from CGE to TIMES






 EFVAL                   output: temporary storage for putting values
 COUNT              counting parameter

 TCST_INVC(REG,AY,PRC) TIMES calculated annual investment costs
 TCST_ACTC(REG,AY,PRC) TIMES calculated annual activity costs
 TCST_FIXC(REG,AY,PRC) TIMES calculated annual fixed costs
 TCST_ELE_I(RUN,AY) ELC total inv costs (Gx)
 TCST_ELE_A(RUN,AY) ELC total act costs (Gx)
 TCST_ELE_F(RUN,AY) ELC total F costs   (Gx)
 TCST_ELE_X(RUN,AY) ELC total TDX costs (Tx + Dx)
 TCST_ELE_INV(RUN,AY) Elc total inv cost (Gx + Tx + Dx)

 TCST_COALSUP(RUN,AY) Total costs from detailed coal supply model

 ELC_FLOW(RUN,AY) Electricity flows used as denominator in elc price calculation
 ELC_FLOW2(RUN,AY) Electricity flows used as denominator in elc price calculation

 OIL_FLOW(RUN,AY) Oil product flows used as denominator in ica calc for petroleum
 COAL_FLOW(RUN,AY,C) Coal flows used for theta calcs

 ELC_IMP(RUN,AY) Electricity Imports
 OIL_IMP(RUN,AY) Oil product imports

* CO2 in power sector
 TPWRCLE(AY) TIMES calculated annual coal use in power sector (PJ)
 TPWRCLETAX(AY) TIMES calculated CO2 tax

 COM_CSTCO2(REG,TC,COM) CO2 tax for elc price calc

*Exporting from TIMES to CGE
 TVNCAP(AY,PRC)   New Capacity
 TVCAP(AY,PRC)    Total Capacity
 TVACT(AY,PRC)    Activity (output)
 TVFLOWIN(COM,AY,PRC) flow in (input)
 TVFLOWOUT(COM,AY,PRC) flow out (output)

 UCONV_C_T2C(C)            Unit conversion from TIMES-SATIM (PJ) to ESAGE (e.g. TWh)
 UCONV_A_T2C(A)            Unit conversion from TIMES-SATIM (PG) to ESAGE (e.g. tons)

 BPINV(RUN,AY,TCP,PRC)       Annual expenditure on construction by PRC for each new Cap year
 EINVCOST(RUN,AY)        Total annual expenditure on construction

 NCAPSubSector(RUN,AY,Sector,SubSector)        New Capacity by subsector

 INVCOST(RUN,AY,PRC)     Annual expenditure on construction by PRC
 INVCOSTSubSector(RUN,AY,Sector,SubSector)        Total annual expenditure on construction by TIMES subsector
*Electricity price from TIMES
 TVCOMBAL(AY,COM)         Fuel Marginals from TIMES
 ETPRICE(AY)              Regulated Elc Price Calculated in SATIM
 ETPRICEGR(AY)            SATIM Elc Price growth

 EPRICE(AY,TT)            Elc Price in CGE units

*Exogenous world commodity prices from times
 PWEPROJ(C,RW,AY)     export price projections from TIMES (supersedes CGE specified growth in pwegr)
 PWMPROJ(C,RW,AY)     import price projections from TIMES (supersedes CGE specified growth in pwmgr)

 MAYR            Number of years for moving average  / 3 /
*Sectoral, household and population forecast data (used in forecast code)
 SBASE(FS,X,AY,TT)   Moving average growth rate used for future sectoral growth projections
 HBASE(FH,X,AY,TT)   Moving average growth rate used for future household 'income' growth projections
 PBASE(FH,X,AY,TT)   Moving average growth rate used for future population growth projections
 SGROW(FS,X,AY,TT)   Moving average growth rate used for future sectoral growth projections
 HGROW(FH,X,AY,TT)   Moving average growth rate used for future household 'income' growth projections
 PGROW(FH,X,AY,TT)   Moving average growth rate used for future population growth projections

 EELAS           / 0.6 /
*Supply and investment data (results from TIMES)

*BM+FH 201218: SETTING STARTING POINT FOR HYDROGEN
 HCAP(X,AY,TT)       Hydrogen (fuels) supply vector for capacity (results from TIMES)
 CCAP(AY,TT)       Coal supply vector for capacity (results from TIMES)
 COALMINCAP(TC) coal mining capacity (sum of consumption and exports-imports)

 FLOW_IN_FS(FSATIM,COM,AY) Consumption of SATIM commodity COM for SATIM sector FS
 FLOW_IN_FSFuels(FSATIM,C,AY) Consumption of CGE commodity C for SATIM sector FS

 FLOW_IN_FSFactors(FSATIM,F,AY) Consumption of CGE Factor F for SATIM sector FS
 FLOW_IN_FSInvestment(FSATIM,C,AY) Consumption of CGE Investment Commodities for SATIM sector FS
 IWTST(AY) Total investment used to update IWTS('s-e')


 FLOW_OUT_FS(FSATIM,COM,AY) Production of SATIM commodity COM for SATIM sector FS
 FLOW_OUT_FSFuels(FSATIM,C,AY) Production of CGE commodity C for SATIM sector FS


 FLOW_IN_FHFuelsGR(FSATIM,C,AY) Change-Growth in Consumption of CGE commodity C for household sector FH

 FLOW_IN_FT(FSATIM,COM,AY) Consumption of SATIM commodity COM for transport activity FT
 FLOW_IN_FTFuels(FSATIM,C,AY) Consumption of CGE commodity C for transport activity FT

 FLOW_IN_FTFHFuels(FSATIM,C,AY) Consumption of transport commodity by SATIM households

 FLOW_IN_FHAllFuels(FSATIM,C,AY) consumption of all energy commodities by household FH

 EIntensity_FH(FH,C,AY)    Energy Intensity of SATIM households
 EIntensity_FS(FS,C,AY)    Energy Intensity of SATIM sectors
 EIntensityQA_FS(FS,C,AY)    Energy Intensity of SATIM sectors linked to QA rather than GVA

 FIntensity_FS(FS,F,AY)    Factor Intensity of SATIM sectors

* ICA change/growth parameters
 ICAFSGR(C,FS,AY) change in ICA for SATIM sectors
 IFAFSGR(F,FS,AY) change in IFA for SATIM sectors

 ICAGR(C,A,AY) change in ICA
 IFAGR(F,A,AY) change in IFA

 QFEGY_GR(RUN,AY) Growth in capital supply (Electricity)

*FH gammam growth parameters
 gammamgr(C,H,AY)    growth in electricity consumption (from TIMES)

* Transport Mode Scenario
 TRAMOD        Transport mode scenario

;
* set Transport Mode scenario to default
TRAMOD = 0;

*b Initialise electricity price growth to historical growth rates
 ETPRICE(TC) = PQ0('CELEC');
