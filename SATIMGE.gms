*This batch file runs the SATMGE model. The file can be used to run the SATIM
*only, CGE only and linked model. The model run is controlled through the MCSIM file.
*The file includes the TIMES model, the simulation files of the CGE model and the CGE
*model results files. To run the batch file for the CGE only or linked model the
*restart file should be set as follows: r=cge\model.

*1.Set directories*-------------------------------------------------------------
$SETGLOBAL workingfolder C:\Models\SATIMGE_Veda\
* TIMES GDX output folder
$SETGLOBAL TIMESfolder Gams_WrkTIMES
$SETGLOBAL gdxfolder %workingfolder%\%TIMESfolder%
* Subset of TIMES GDX output folder
$SETGLOBAL GDXoutfolder %workingfolder%GDXout\


*-------------------------------------------------------------------------------

*2.Defining sets and parameters used in Batch file------------------------------
SETS
* Overall sets
  RUN                            simulations
  SATIMCASES                     simulations in SATIM(TIMES)
  X                              simulations in eSAGE (CGE)
  XC(X)                          active simulations
  XNB(X)                  nonbase simulations
  MRUNCASE(RUN,SATIMCASES)       Mapping RUN to TIMES CASE
  MRUNX(RUN,X)                   Mappinf RUN to CGE sim
  PamsSector                     Pams Sectors for NDC / RES, COM, TRA, WASTE, AFOLU/
* SATIM sets
  REG                            TIMES regions    /REGION1/
  ALLYEAR                        All Years /1850*2200, 0/
  T(ALLYEAR)                     Time periods /2005*2070/
  DATAYEAR(ALLYEAR)              Years for which data is provided /2017*2055,2058,2063,2065,2068/
  ENDYEARS(ALLYEAR)              Years after 2050 for Waste and Agri Model /2051*2055,2058,2063,2068/

  MILESTONYR(ALLYEAR)            TIMES Milestone years
  V(ALLYEAR)                     Vintage years

  NMY1(ALLYEAR)                  All milestone years except for the first year (demand)
  S                              TIMES timeslices
  TS_MAP(REG,S,S)                Timeslice hierarchy tree: node+below
  TSLVL                          Timeslice levels /ANNUAL, SEASON, WEEKLY, DAYNITE/
  TS_GROUP(REG,TSLVL,S)          Timeslice Level Assignment
  TS_SEASON(S)                   seasonal timeslices
  TS_DAYNITE(S)                  day-night(block) timeslices
  TS_WEEKLY(S)                   day-type timeslices
  TS_HOURLY                      Hours in a day /1*24/
*  TS_HOURLY                     Hours in a week /1*168/
  TS_MAP2(TS_WEEKLY,TS_DAYNITE,TS_HOURLY)  maps day-types and blocks to 24 hour sequence
  PRC                            TIMES Processes
  PRC_TSL(REG,PRC,TSLVL)         Timeslice level for a process

  PRC_ANNUAL(REG,PRC)            Annual Processes
  PRC_SEASON(REG,PRC)            Seasonal Processes
  PRC_WEEKLY(REG,PRC)            Weekly Processes
  PRC_DAYNIT(REG,PRC)           Daynite Processes
  PRC_RCAP(REG,PRC)             Process with early retirement

  PRCH(PRC)                      Households groupings in eSAGE

  COM                            TIMES Commodities
  NRGELC(REG,COM)            Elc Commodities
  COM_NRGELCR(COM)               Elc Commodities
  TCG(COM)                       Trade commodity groups
  DEM(REG,COM)                   TIMES Demand Commodities
  DEM1(COM)                      TIMES Demand Commodities for REGION1

  CUR    Currencies /MZAR2022/
*  RPM    plus minus /-, +/
  SYSUC  COST ACT etc /INV/

  UC_N                           List of names of all manual constraints
  SIDE                           LHS and RHS of an equation /LHS, RHS/

* eSAGE sets
  TC(T)                   eSAGE active time periods
  TT(T)                   SATIM-eSAGE Iterations
  TTIMES(T)               SATIM years (for demand)
  T1(T)                   first year of simulation
;

ALIAS (ALLYEAR,AY), (ALLYEAR,AYP), (DATAYEAR,DM_YEAR);
Alias (PRC,P);

PARAMETERS
* Scenario Dash Parameters
  INCLRUN(RUN)                   whether to include or not RUN in batch run
  SIM_SATIM(RUN)                 whether to rerun SATIM or not
  SIM_ESAGE(RUN)                   whether to run linked model or not
  SIM_WASTE(RUN)                   whether to run waste model or not
  SIM_AFOLU(RUN)                   whether to run AFOLU models or not
  SIM_CO2CUMUL(RUN)              Cumulative CO2 Constraint


*  SIM_CO2PRICE(RUN,AY)           CO2 PRICE
  PAMS(RUN,PamsSector)       activated pams for each run
  PAMS_RUN(PamsSector)       activated pams for run in loop

*  GDP_SIMPLE(FS,AY,RUN)          GDP for shorter runs

*  CCONX(AC,T)                  carbon content of final demand historical

;


* Import sets and parameters and data from control spreadsheet-------------------------------
$call   "gdxxrw i=SATIMGE.xlsm o=SATIMGE index=index!a6 checkdate"
$gdxin  SATIMGE.gdx
$load RUN SATIMCASES X XC INCLRUN SIM_SATIM SIM_ESAGE SIM_WASTE SIM_AFOLU MRUNCASE MRUNX TC TT TTIMES PAMS SIM_CO2CUMUL


XNB(XC) = YES;
XNB('BASE') = NO;



SETS
* Sector groupings in SATIM
  FSATIM                  sector groupings in SATIM model
  FS(FSATIM)              TIMES economic sectors
  FSATIMNOELEC(FSATIM)     sector groupings in SATIM excluding electricity
  FSGDP(FS)           sectors without fa and al
  FH(FSATIM)              household groupings in SATIM model
  FT(FSATIM)              passenger transport groupings in SATIM

  FS_L(FS)                Linked subsectors ie excl com agr ele

  Sector                 SATIM sectors
  SubSector              SATIM subsectors
  SubSubSector           SATIM subsubsectors
  MPRCSector(PRC,Sector) MAP for PRC to sectors
  MPRCSubSector(PRC,Sector,SubSector)          Map for PRC to subsectors
  MPRCSubsubSector(PRC,Sector,SubSector,SubSubSector)  Map for PRC to subsubsectors
  PRCPower(PRC)                  Technology set used to compute elc price
  PRCBulkPower(PRC)              Technology set used to compute bulk elc price (up to ETRANS)
* Emissions sets
  CO2SET(COM)                    Sectoral emissions

* Externality Commodities
  COMEXT(COM)                    Externality Commodities

* Results
  mFuels                         Map for Fuels set
  mFuels2                        Map for Fuels set more details
  mExt                           Map for Externalities set
  mRoadFreight                   Map for demands and techs for road freight
  mRoadPassenger                 Map for demands and techs for road passenger

* Groups
* PRC groups

* COM Groups
  Fuels                          Fuels
  Fuels2                         Fuels more details
  Externalities                  Externalities
  FuelPrices(COM)                Fuels whose prices are kept
  PassengerRoad(COM)             Passenger Road for tracking occupency and pkm from vehkm data
  PassengerModes(COM)            Passenger transport modes
  FreightRoad(COM)               Freight Road for tracking load and pkm from vehkm data
  FreightModes(COM)              Freight transport modes


  PamsSector                     PAMS sectors

* mapping sets
  MFHH(FH,H)              mapping SAGE and SATIM households
  MFSA(FS,A)              mapping SAGE and SATIM sectors
  MPRCFS(PRC,FSATIM)          mapping SATIM PRCs to SATIM sectors (temporarily until sectors are reaggregated in the same way in both models)
  MPRCFS2(PRC,FSATIM)          mapping SATIM PRCs to SATIM sectors (used to pass CGE aggregates to SATIM sectors)
  mCOMC(COM,C)            mapping SAGE and SATIM fuels
  mCOMF(COM,F)            mapping SAGE factors and SATIM factor commodities

  MHPRCH(PRC,H)           mapping SAGE households to reporting households
  MCTCG(C,TCG)            mapping SAGE trade commodities to aggregate trade commodity groups


  MFSP(FSATIM,PRC)           mapping of technologies used for each sector to track process emissions


* Other SATIM Sets
 ELE(REG,PRC) power sectir technologies in TIMES
 SUPELE(PRC)  power sector technologies in SATIM
 COALSUP(PRC) detailed coal supply techs for power sector

 CCOAL(C)              coal commodities in eSAGE / ccoal-low, ccoal-hgh /
 RTC(C)                commodities attracting retaliatory taxes

* sets used for reverse mapping of households
*FH*----------------------------------------------------------------------------
 MFHHT(FH,H,AY) reverse mapping (TIMES to CGE) for households

  Indicators SATIM indicators /Activity, Capacity, NewCapacity, CapFac, FlowIn, FlowOut, AnnInvCost, FOM, VOM, FuelCosts, Marginals, Levies, ExternalityCosts, CO2Tax, CO2, CO2CAPT, CH4, N2O, CF4, C2F6, CO2eq, FlowInMt, Investment,Price, GVA, Population, Consumption, Employment-p, Employment-m,Employment-s,Employment-t,PalmaRatio,20-20Ratio,TradeDeficit,Imports,Exports,pkm, tkm/
  Emiss(Indicators) / CO2, CH4, N2O, CF4, C2F6, CO2eq/
  IndicatorsH SATIM Sub-annual indicators /FlowIn, FlowOut, Marginal, Price, Demand/


* Other
  XXX                            Needed for obj    / CUR, LEVCOST, INV,'-' /
;



Sets


* sets used in interpolation function (for linked model)
  RTP(REG,AY,PRC)                Technology valid years
  UNCD7(*,*,*,*,*,*,*)           Non-domain-controlled set of 7-tuples

;
VARIABLES
         VAR_ACT(REG,AY,AY,PRC,S) Overall activity of a process
         VAR_CAP(REG,AY,PRC)     Variable of total capacity including residual
         VAR_RCAP(REG,AY,AY,PRC) Variable of early retirement of processes
;
PARAMETERS
  REPORT(PRC,COM,AY,RUN,Indicators) REPORT of indicators by run and process and commodity
  REPORT_RUN(PRC,COM,AY,Indicators) REPORT of indicators by run and process and commodity for each run

  REPORTH(PRC,COM,AY,TS_WEEKLY,TS_HOURLY,RUN,IndicatorsH) REPORT of indicators by daytype and each hour- run and process and commodity
  REPORTH_RUN(PRC,COM,AY,TS_WEEKLY,TS_HOURLY,IndicatorsH) REPORT of indicators by daytype and each hour- run and process and commodity

* From TIMES
  F_IN(REG,AY,AY,PRC,COM,S)      Flow parameter (level of flow variable) [PJ]
  F_OUT(REG,AY,AY,PRC,COM,S)     Flow parameter (level of flow variable)[PJ]
*  VAR_ACT(REG,AY,AY,PRC,S)     Flow parameter (level of flow variable)[PJ]

  VARACT(REG,AY,PRC)             Activity level [PJ except for demand techs where unit will be aligned to particular demand e.g. VKM for road vehicles]
  PRC_CAPACT(REG,PRC)            Factor going from capacity to activity
  PRC_RESID(REG,AY,PRC)          Existing Capacity
  RESID(REG,AY,PRC)              Existing Capacity milestone years

  PAR_CAPL(REG,AY,PRC)           Capacity excluding existing capacity
  PAR_VCAPL(REG,AY,PRC)          Variable VAR_CAPL results as parameter
  PAR_RCAPL(REG,AY,PRC)          Early retirement of capacity results
  PAR_NCAPL(REG,AY,PRC)          New Capacity
  PAR_COMBALEM(REG,AY,COM,S)     Commodity marginal
  NCAP_ILED(REG,AY,PRC)          TIMES lead time
  CST_INVC(REG,AY,AY,PRC,SYSUC)     TIMES calculated annual investment costs
  CST_ACTC(REG,AY,AY,PRC,*)     TIMES calculated annual activity costs
  CST_FIXC(REG,AY,AY,PRC)         TIMES calculated annual fixed costs
  COM_CSTNET(REG,AY,COM,S,CUR)  TIMES specified commodity taxes
  COM_PROJ(REG,AY,COM)           TIMES Demand baseline projection

  UC_CAP(UC_N,SIDE,REG,AY,PRC)   TIMES multiplier of capacity variables

  G_YRFR(REG,S)                  TIMES seasonal fraction of the year

  OB_ICOST(REG,PRC,CUR,AY)       Interpolated investment cost from TIMES run
  OBICOST(REG,AY,PRC)            TIMES investment cost restructured for interpolation

  OB_ACT(REG,PRC,CUR,AY)       Interpolated variable unit cost from TIMES run R per kW or GJ-a
  OBACT(REG,AY,PRC)            TIMES variable unit cost restructured for interpolation R per GJ


* Data from Demand Model (spreadsheet-based at this stage)
  SIM_DEMX(COM,AY)               Demand extracted from excel

* Intermediate parameters

  GVA_FS(FS,AY)                  SATIM Sector GVA
  GVA_FS_Start(FS,AY)            SATIM Sector GVA used to first iteration of linked model
  QA_FS(FS,AY)                   SATIM Sector QA (used for setting absolute levels for ica for new sectors)
  POP(AY)                        Population Projection to be read from drivers workbook
  STFHPOP(AY)                    sum of population CGE
  POP_GR(AY)                     Population growth to be read from drivers workbook
  GDP_RUN(AY)                    GDP projection for RUN

  EmisFactor(COM,Emiss)          Combustion Emission Factor
  EmisFactorCor(COM,Emiss,TC,RUN)       Corrected Combustion Emission Factor for exceptions

  LandEmissions(PRC,Emiss,AY) Land Emissions
  AgriEmissions(PRC,Emiss,AY) Agriculture Emissions
  WasteEmissions(PRC,Emiss,AY) Waste Emissions
  CoalAshCoal(AY)              Coal used to account for coal ash in waste model


  CoalCV(COM)                    Coal CV in GJ

* TIMES Results Initial Aggregation
  CAPACT(PRC)                    Capacity to activity

  FLO_IN(ALLYEAR,PRC,COM)        Aggregated level of process commodity flow in
  FLO_OUT(ALLYEAR,PRC,COM)       Aggregated level of process commodity flow out

  FLO_IN_S(ALLYEAR,PRC,COM,S,RUN) aggregated level of process commodity flow in with timeslice detail
  FLO_OUT_S(ALLYEAR,PRC,COM,S,RUN) aggregated level of process commodity flow out with timeslice detail

  CAP(ALLYEAR,PRC)               Process capacity
  NCAP(ALLYEAR,PRC)              New Process capacity
  SCAP(ALLYEAR,PRC)              Scaled Capacity by CAPACT

  COMBALEM(ALLYEAR,FuelPrices,RUN) aggregate marginals [2015 R per GJ]
  T_COMBALEM(REG,AY,COM)         Commodity marginals from TIMES-SATIM

  CST_INV(AY,PRC)           Annual investment costs
  CST_ACT(AY,PRC)           Annual activity costs
  CST_FIX(AY,PRC)           Annual fixed costs

* Commodity Levels
  COMBAL(ALLYEAR,C,RUN)          Commodity level
  COMBALExt(ALLYEAR,Externalities,RUN) Emissions level
  FuelCOMBAL(ALLYEAR,C,RUN)      Fuels marginals

* Coal specific activities
  VARACTC(ALLYEAR,PRC,RUN)       Coal production from all coal mines
  VARACTUXLE_AB(ALLYEAR,RUN)     Tranfer of coal from WB to CB

* Flow_out Externalities
  COUTPXSums(ALLYEAR,COM,RUN)    Externalities sums [kton]
  COUTPExtSums(ALLYEAR,Externalities,RUN) Check sum [kton]
  COUTPExtCumulSums(Externalities,RUN) Cumulative sum [kton]

*  Costs
* Regulated Commodity price calc
* Elc price calc parameters
  TCST_ELE(RUN,AY)               Power plant costs excluding fuel
  TCST_PWRCLT(RUN,AY)            Total coal costs to power sector
  TCST_PWROTH(RUN,AY)            Other fuel costs to power sector

* Other useful results
  PKDEM(ALLYEAR,RUN)             Peak demand on electricity grid
  PassengerOccupancy(PRC)        Occupancy of passenger road transport
  FreightLoad(PRC)               Average load per mode

* Other Parameters
* Interpolation parameters - using function developed for TIMES
  DFUNC                          ???
  MY_FIL2(ALLYEAR)               ???
  MY_ARRAY(ALLYEAR)              ???
  LAST_VAL                       ???
  FIRST_VAL
  FINT                           first value?
  Z                              last value?
  MY_F                           ???
  YEARVALT(ALLYEAR)              value of each year for CGE years
  MY_FYEAR                       last year of TIMES model
  B(ALLYEAR)                     begining of period
  E(ALLYEAR)                     end of period
  TGAP(ALLYEAR)                  period lengths
  MYGAP(ALLYEAR)                 period lengths
  POBjz(RUN)                     Objective function
  REG_OBJ(REG)                   Objective function from TIMES

* SATIM Aggregates
  AvgCoalPrice(ALLYEAR,RUN)      Average Marginal for power plant coal
  TotalFinal(ALLYEAR,RUN)
  CoalFinal(ALLYEAR,RUN)
  CoalShareFinal(ALLYEAR,RUN)
  GasFinal(ALLYEAR,RUN)
  GasShareFinal(ALLYEAR,RUN)
  FossilShareFinal(ALLYEAR,RUN)
  ERPRICE(AY,RUN)                regulated electricity price

* CO2 Price
  SIM_CO2PRICE(AY,RUN)           CO2 Price either specified manually or from marginals

* CTL links
  CTL_Weight(AY,RUN)             Weighting of CTL changes in chemicals AFXGR calc
* tracking trends in intermediate use of goods produced by exogenous sectors
  QINTCBCHM(AY,RUN)             stored value for sum(a.QINT(CBCHM.a)
  QINTCOCHM(AY,RUN)             stored value for sum(a.QINT(COCHM.a)
  QINTCIRON(AY,RUN)             stored value for sum(a.QINT(CIRON.a)

* domestic demand to drive energy model (i.e. excluding exports)
  QD_FS(FS,AY)            domestic demand to drive energy model

* Electrolysers and Fuel Cells
  ELCTCAP(AY,RUN)            Electrolyser capacity in SATIM
  FCELLCAP(AY,RUN)            Fuelcell capacity in SATIM

* SubAnnual Analysis parameters
  TS_Duration(TS_DAYNITE)                  duration in hours of each daynite timeslice
  P_IN_H(PRC,COM,AY,TS_WEEKLY,TS_HOURLY)       power flow-ins (hourly)
  P_OUT_H(PRC,COM,AY,TS_WEEKLY,TS_HOURLY)      power outflows (hourly)
  P_DEM_H(PRC,COM,AY,TS_WEEKLY,TS_HOURLY)      Demand profiles (hourly)

  P_IN(PRC,COM,AY,TS_DAYNITE)              power flow-ins
  P_OUT(PRC,COM,AY,TS_DAYNITE)             power outflows
  P_DEM(PRC,COM,AY,TS_DAYNITE)             Demand profiles

  E_IN(PRC,COM,AY,TS_DAYNITE)              energy flow-ins
  E_OUT(PRC,COM,AY,TS_DAYNITE)             energy outflows
  E_DEM(PRC,COM,AY,TS_DAYNITE)             energy Demand profiles

  Marginal(COM,AY,TS_DAYNITE)          Marginals
  Marginal_H(COM,AY,TS_WEEKLY,TS_HOURLY) Marginals (hourly)

  VarCosts_TS(PRC,COM,AY,TS_DAYNITE)               variable costs per timeslice
  Marginal_H(COM,AY,TS_WEEKLY,TS_HOURLY)           Marginal hourly
  AggFixPrice(COM,AY,RUN)                      Aggregate Fixed Cost mR per GW
  AggVarCost(COM,AY,RUN)           Aggregate Cost mR
  AggVarCost_TS(COM,AY,TS_DAYNITE)           Aggregate Variable Cost in each timeslice
  AggVarPrice(COM,AY,RUN)           Aggregate Variable Price R per GJ
  AggVarPrice_TS(COM,AY,TS_DAYNITE)           Aggregate Variable Cost in each timeslice
  AggVarPrice_H(COM,AY,TS_WEEKLY,TS_HOURLY)  Aggregate Variable Cost (hourly)

  VALHOURLY(TS_HOURLY)                    value of TS_HOURLY
  TS_REF(TS_DAYNITE)                      Hourly Reference point


;
LOOP(TS_HOURLY,
   VALHOURLY(TS_HOURLY) = ORD(TS_HOURLY);
);


*-------------------------------------------------------------------------------



* File declarations------------------------------------------------------------
*INCLUDE NOTES!!!What are these files for?
 FILE SIM_DEM_FILE /".\satim\%TIMESfolder%\DMD_ALL+REGION1.dds"/;
 FILE SIM_OTHPAR_FILE /".\satim\%TIMESfolder%\sim_othpar+REGION1.dds"/;
 FILE RUNTIMES2 /".\satim\%TIMESfolder%\RUNTIMES2.CMD"/;
 FILE ShowRunNumber /".\satim\%TIMESfolder%\ShowRunNumber.CMD"/;
 FILE SATIM_Scen;
 FILE CGE_Scen;
 FILE Scen;
*-------------------------------------------------------------------------------



* Import sets and parameters from SetsAndMaps -------------------------------
$call   "gdxxrw i=SetsAndMaps\SetsAndMaps.xlsm o=SetsAndMaps\SetsMaps index=index!a6 checkdate"
$gdxin  SetsAndMaps\SetsMaps.gdx
$loaddc PRC COM S TS_DAYNITE TS_WEEKLY TS_SEASON DEM1 UC_N FSATIM COALSUP PRCH TCG FS FH
$load  MPRCFS MPRCFS2 Sector SubSector SubSubSector MPRCSector MPRCSubSector MPRCSubSubSector COMEXT MHPRCH MCTCG mCOMC MFSA MFHH mCOMF
$load PassengerOccupancy  FreightLoad CoalCV


* some subset definitions
FSATIMNOELEC(FSATIM) = yes;
FSATIMNOELEC('elec') = no;

PRCPower(PRC)$MPRCSector(PRC,'Power') = yes;
PRCPower(PRC)$MPRCSubsector(PRC,'CO2Capture&Storage','DAC') = yes;
PRCPower(PRC)$MPRCSubsector(PRC,'CO2Capture&Storage','CO2Capture&Storage-Power') = yes;

PRCBulkPower(PRC)$PRCPower(PRC) = yes;
PRCBulkPower(PRC)$MPRCSubSector(PRC,'Power','EDist') = no;
PRCBulkPower(PRC)$MPRCSubSector(PRC,'Power','ETrans') = no;
PRCBulkPower(PRC)$MPRCSubSector(PRC,'Power','AutoGen-Chemical') = no;
PRCBulkPower(PRC)$MPRCSubSector(PRC,'Power','AutoGen-EnergySupply') = no;
PRCBulkPower(PRC)$MPRCSubSubSector(PRC,'Power','EBattery','Ebattery_Dist') = no;
PRCBulkPower(PRC)$MPRCSubSubSector(PRC,'Power','EPV','EPV_Dist') = no;

FSGDP(FS) = yes;
FSGDP('fa') = no;
FSGDP('al') = no;

execute 'gdxxrw.exe i=Drivers.xlsm o=driverspop.gdx index=index_E2G!a6';
execute_load "driverspop.gdx" POP_GR;

alias (TC,TCP);
T1(TC)$(ORD(TC) EQ SMIN(TCP, ORD(TCP))) = YES;


* CGE: Parameter and set declaration-------------------------------------------
$batinclude cge\includes\2simulation.inc

$gdxin  SetsAndMaps\SetsMaps.gdx
$load FHMM

MY_FIL2('2050') = 0.1;
MY_FYEAR = 2012;
YEARVALT(ALLYEAR) = 1849+ORD(ALLYEAR);
*setup emissions
$call   "gdxxrw i=EmissionFactors.xlsx o=EmisFac index=Index!a6 checkdate"
$gdxin  EmisFac.gdx
$load EmisFactor

* The CO2eq calc is to be done in Tableau, as coeffcients may change, and we need to be consistent with inventory and across IPCC categories
EmisFactor(COM,'CO2eq') = 0;


$include cge\includes\2energychecksinit.inc


*$include KLEM\KLEM_init.inc


Alias (MILESTONYR,MY), (P,PP);


*$batinclude cge\includes\3report_init.inc
*-------------------------------------------------------------------------------


*-------------------------------------------------------------------------------

 Scalars
 EFVAL                 temporary values stored here
 TRAMOD                transport mode parameter
 TS_TMP1               temporary values in sub-annual results calc
 TS_TMP2               temporary values in sub-annual results calc


;
*-------------------------------------------------------------------------------

* Loop: Model solve-----------------------------------------------------------
LOOP(RUN$INCLRUN(RUN),
   PAMS_RUN(PamsSector) = PAMS(RUN,PamsSector);

$ontext


 PUT 'PARAMETER ACOMCSTNET /' /;
Loop(MY,
  EFVAL = SIM_CO2PRICE(RUN,MY)/1000;
  if(EFVAL,
    PUT "REGION1.CO2S.'ANNUAL'.", MY.TL, EFVAL /;
  );
);


PUTCLOSE "/;";
$offtext

if(SIM_ESAGE(RUN) eq 1,

*$batinclude cge\includes\2simulation_loop.inc

ELSE



* Read in GDP and Population from Drivers Workbook
  execute 'gdxxrw.exe i=Drivers.xlsm o=drivers.gdx index=index_E2G!a6';
  execute_load "drivers.gdx" GVA_FS POP YHE TFHPOP MFHHT QD_FS;


  if(SIM_SATIM(RUN) eq 1,
* Write Drivers to DMD_PROJ workbook
*         execute_unload "drivers.gdx" GVA_FS POP YHE TFHPOP MFHHT QD_FS PAMS_RUN;
*         execute 'gdxxrw.exe i=drivers.gdx o=.\SATIM\DataSpreadsheets\DMD_PRJ.xlsx index=index_G2E!a6';

* Read resulting Demand from DMD_PROJ workbook
*         execute 'gdxxrw.exe i=.\SATIM\DataSpreadsheets\DMD_PRJ.xlsx o=EnergyDemand.gdx index=index_E2G!a6';
*         execute_load "EnergyDemand.gdx" SIM_DEMX;


* Write Demand DDS File
$ontext
         PUT  SIM_DEM_FILE;
         SIM_DEM_FILE.pc = 2;
         SIM_DEM_FILE.nd = 13;
         SIM_DEM_FILE.ap = 0;

         PUT 'PARAMETER ACOM_PROJ /' /;

         LOOP((DEM1,TTIMES),
                 EFVAL = SIM_DEMX(DEM1,TTIMES);
                 if(EFVAL,
                         PUT "REGION1.", DEM1.TL, ".", TTIMES.TL, EFVAL /;
                 else
                         PUT "REGION1.", DEM1.TL, ".", TTIMES.TL, "eps" /;
                 );
         );

         PUT "/;"/;

*Write Cumulative CO2 CAP bound
         PUT 'PARAMETER ACOMCUMNET /' /;
         EFVAL = SIM_CO2CUMUL(RUN)*1000000;
         if(EFVAL,
                 PUT "REGION1.CO2EQSB.2021.2050.'UP'  ", EFVAL /;
         );

         PUTCLOSE "/;";

         PUT  ShowRunNumber;
         RUNTIMES2.pc = 2;
         RUNTIMES2.nd = 5;
         RUNTIMES2.ap = 0;
         PUTCLOSE "";

$offtext
*$include SATIM\includes\2runTIMES.inc

  );
* if(SIM_SATIM(RUN) eq 1


* Get Energy Model Results
$include includes\2TIMESReport.inc
REPORT(PRC,'ACTGRP',TC,RUN,'GVA') = SUM(FS$MPRCFS2(PRC,FS),GVA_FS(FS,TC));


);
*if(SIM_ESAGE(RUN) eq 1,

* Sub-annual results
$include includes\2TIMESSubAnnualReport.inc


GDP_RUN(TC) = SUM(FSGDP,GVA_FS(FSGDP,TC));

*$ontext
$include includes\GHGEnergyReport.inc

*Get Process Emissions
$include includes\GHGProcessReport.inc

if(SIM_WASTE(RUN) eq 1,
* Run Waste Model
$include Waste\includes\GHGWasteReport.inc
);

if(SIM_AFOLU(RUN) eq 1,
* Run AFOLU Model
$include AFOLU\includes\GHGAfoluReport.inc
*$offtext

);

*$include KLEM\KLEM_Report.inc

* generate report for run, which can then be combined later
REPORT_RUN(PRC,COM,TC,Indicators) = REPORT(PRC,COM,TC,RUN,Indicators);
put_utilities Scen 'gdxout' / RUN.TL:30;
execute_unload REPORT_RUN

);
*end RUN loop
*-------------------------------------------------------------------------------

execute_unload "REPORT.gdx" REPORT
execute 'gdxdump REPORT.gdx output=REPORT_00.csv symb=REPORT format=csv header="Process,Commodity,Year,Scenario,Indicator,SATIMGE"';



*execute 'gdxdump REPORT.gdx output=REPORT.csv symb=REPORT format=csv header="Process,Commodity,Year,Scenario,Activity,Capacity,NewCapacity,FlowIn,FlowOut,CO2,CH4,N2O,HFC,PFC,CO2eq,Investment,GVA,Employment" cDim=y';
*execute 'gdxxrw.exe i=REPORT.gdx o=.\Results\REPORT.xlsx index=index!a6';



