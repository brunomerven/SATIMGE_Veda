* This code is used to stitch results from multiple gdx files into 1
SETS
* Overall sets
  RUN                            simulations
  AY                        All Years /1850*2200, 0/
  S                         Timeslices
  T(AY)                     Time periods /2005*2070/
  TC(T)                   eSAGE active time periods
  PRC                            TIMES Processes
  COM                            TIMES Commodities
  IndicatorsH SATIM Sub-annual indicators /FlowIn, FlowOut, Marginal, Price, Demand/
  TS_WEEKLY(S) day-type timeslices
  TS_HOURLY Hours in a day /1*24/
;

Parameters
  REPORTH_RUN(PRC,COM,AY,TS_WEEKLY,TS_HOURLY,IndicatorsH) REPORT of indicators by daytype and each hour- run and process and commodity
  REPORTMH(PRC,COM,AY,TS_WEEKLY,TS_HOURLY,RUN,IndicatorsH) REPORT of indicators by daytype and each hour- run and process and commodity
  INCLRUN(RUN)                   whether to include or not RUN in batch run
;

FILE Scen;

$call   "gdxxrw i=SATIMGE.xlsm o=SATIMGE index=index!a6 checkdate"
$gdxin  SATIMGE.gdx
$load RUN INCLRUN TC

$call   "gdxxrw i=SetsAndMaps\SetsAndMaps.xlsm o=SetsAndMaps\SetsMaps index=index!a6 checkdate"
$gdxin SetsAndMaps\SetsMaps.gdx
$loaddc PRC COM S TS_WEEKLY

LOOP(RUN$INCLRUN(RUN),

put_utilities Scen 'gdxin' / "H_",RUN.TL:30;
execute_load REPORTH_RUN;

REPORTMH(PRC,COM,TC,TS_WEEKLY,TS_HOURLY,RUN,IndicatorsH) = REPORTH_RUN(PRC,COM,TC,TS_WEEKLY,TS_HOURLY,IndicatorsH);



);
*end loop

execute_unload "REPORTMH.gdx" REPORTMH
execute 'gdxdump REPORTMH.gdx output=REPORTH_00.csv symb=REPORTMH format=csv header="Process,Commodity,Year,Day-Type,Hour,Scenario,Indicator,SATIMGE"';
