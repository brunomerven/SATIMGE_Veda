* This code is used to stitch results from multiple gdx files into 1
SETS
* Overall sets
  RUN                            simulations
  AY                        All Years /1850*2200, 0/
  T(AY)                     Time periods /2005*2070/
  TC(T)                   eSAGE active time periods
  PRC                            TIMES Processes
  COM                            TIMES Commodities
  Indicators SATIM indicators /Activity, Capacity, NewCapacity, CapFac, FlowIn, FlowOut, AnnInvCost, FOM, VOM, OM, FuelCosts, Marginals, Levies, ExternalityCosts, CO2Tax, CO2, CO2CAPT, CH4, N2O, CF4, C2F6, CO2eq, FlowInMt, Investment,Price, pkm, tkm, GVA, EconIndicator, PopulationSATIM, GVASATIM, EmploymentSATIM/
  Sector
  SubSector

  MPRCSubSector(PRC,Sector,SubSector)          Map for PRC to subsectors
;
*$onExternalOutput
Parameters
  REPORT_RUN(PRC,COM,AY,Indicators) REPORT of indicators by run and process and commodity for each run
  REPORTM(PRC,COM,AY,RUN,Indicators) Merged Report
  INCLRUN(RUN)                   whether to include or not RUN in batch run
  CoalCV(COM)
;

FILE Scen;

$call   "gdxxrw i=SATIMGE.xlsm o=SATIMGE index=index!a6 checkdate"
$gdxin  SATIMGE.gdx
$load RUN INCLRUN TC

$call   "gdxxrw i=SetsAndMaps\SetsAndMaps.xlsm o=SetsAndMaps\SetsMaps index=index!a6 checkdate"
$gdxin SetsAndMaps\SetsMaps.gdx
$load PRC COM Sector SubSector MPRCSubSector
$load CoalCV

LOOP(RUN$INCLRUN(RUN),

*put_utilities Scen 'gdxin' / "SARB_prelimruns\",RUN.TL:50;
put_utilities Scen 'gdxin' / RUN.TL:50;
execute_load REPORT_RUN;

REPORTM(PRC,COM,TC,RUN,Indicators) = REPORT_RUN(PRC,COM,TC,Indicators);



);

*tmp fixes until model is rerun These fixes have been made in the model.
*REPORTM(PRC,'IISCOA',TC,RUN,'CO2')$MPRCSubSector(PRC,'Industry','Iron_Steel351') = 0;
*REPORTM(PRC,'IISCKC',TC,RUN,'CO2')$MPRCSubSector(PRC,'Industry','Iron_Steel351') = 0;
*REPORTM('PEXCOA','COA',TC,RUN,'FlowInMt') = 75;

*$offExternalOutput

*end loop

execute_unload "REPORTM.gdx" REPORTM
execute 'gdxdump REPORTM.gdx output=REPORT_00.csv symb=REPORTM format=csv header="Process,Commodity,Year,Scenario,Indicator,SATIMGE"';
