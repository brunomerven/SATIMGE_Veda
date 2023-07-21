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
  IndicatorsH SATIM Sub-annual indicators /FlowIn, FlowOut, Marginal, Price, Demand, Capacity/
  TS_WEEKLY(S) day-type timeslices
  TS_HOURLY Hours in a day /1*168/
  TS_DAYNITE day-night(block) timeslices
  TS_MAP2(TS_WEEKLY,TS_DAYNITE,TS_HOURLY)  maps day-types and blocks to 24 hour sequence

  HistoricalDataColumnHeading Historical Hourly Data from Eskom dashboard Column Heading
  mPRCColumnHeading(HistoricalDataColumnHeading,PRC) Map of PRC to Column Headings
  HourCount hour counter in raw historical data /1*43824/
;

Parameters
  REPORTH_RUN(PRC,COM,AY,TS_WEEKLY,TS_HOURLY,IndicatorsH) REPORT of indicators by daytype and each hour- run and process and commodity
  REPORTMH(PRC,COM,AY,TS_WEEKLY,TS_HOURLY,RUN,IndicatorsH) REPORT of indicators by daytype and each hour- run and process and commodity
  INCLRUN(RUN)                   whether to include or not RUN in batch run

  HistoricalData(AY,HourCount,TS_DAYNITE,HistoricalDataColumnHeading)
  REPORTMHourly(PRC,AY,HourCount,TS_DAYNITE)
  HourlyMarks(PRC,AY,HourCount,TS_DAYNITE)
  CountHourly(PRC,AY,TS_DAYNITE)
  REPORTMTSHourly(PRC,AY,TS_DAYNITE)
;

FILE Scen;

$call   "gdxxrw i=SATIMGE.xlsm o=SATIMGE index=index!a6 checkdate"
$gdxin  SATIMGE.gdx
$load RUN INCLRUN TC

$call   "gdxxrw i=SetsAndMaps\SetsAndMaps.xlsm o=SetsAndMaps\SetsMaps index=index!a6 checkdate"
$gdxin SetsAndMaps\SetsMaps.gdx
$loaddc PRC COM S TS_WEEKLY TS_DAYNITE

LOOP(RUN$INCLRUN(RUN),

put_utilities Scen 'gdxin' / "H_",RUN.TL:30;
execute_load REPORTH_RUN;

REPORTMH(PRC,COM,TC,TS_WEEKLY,TS_HOURLY,RUN,IndicatorsH) = REPORTH_RUN(PRC,COM,TC,TS_WEEKLY,TS_HOURLY,IndicatorsH);



);
*end loop

$call   "gdxxrw i=HistoricalData\HistoricalDispatchData.xlsx o=HistoricalData\HistoricalData index=index!a6 checkdate"
$gdxin HistoricalData\HistoricalData.gdx
$load HistoricalDataColumnHeading
$load mPRCColumnHeading
$load HistoricalData

*$exit

$gdxin TS_MAP2_Historical.gdx
$load TS_MAP2

REPORTMHourly(PRC,AY,HourCount,TS_DAYNITE) = sum(HistoricalDataColumnHeading$mPRCColumnHeading(HistoricalDataColumnHeading,PRC),HistoricalData(AY,HourCount,TS_DAYNITE,HistoricalDataColumnHeading));
HourlyMarks(PRC,AY,HourCount,TS_DAYNITE)$REPORTMHourly(PRC,AY,HourCount,TS_DAYNITE) = 1;
CountHourly(PRC,AY,TS_DAYNITE) = sum(HourCount,HourlyMarks(PRC,AY,HourCount,TS_DAYNITE));

REPORTMTSHourly(PRC,AY,TS_DAYNITE)$CountHourly(PRC,AY,TS_DAYNITE) = sum(HourCount,REPORTMHourly(PRC,AY,HourCount,TS_DAYNITE))/CountHourly(PRC,AY,TS_DAYNITE);
REPORTMH(PRC,'ELCC',AY,TS_WEEKLY,TS_HOURLY,'Historical','FlowOut') = SUM(TS_DAYNITE$TS_MAP2(TS_WEEKLY,TS_DAYNITE,TS_HOURLY),REPORTMTSHourly(PRC,AY,TS_DAYNITE))/1000;

execute_unload "REPORTMH.gdx" REPORTMH
execute 'gdxdump REPORTMH.gdx output=REPORTH_00.csv symb=REPORTMH format=csv header="Process,Commodity,Year,Day-Type,Hour,Scenario,Indicator,SATIMGE"';
