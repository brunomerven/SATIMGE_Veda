* Code to calculate CO2 for SATIM (and eSAGE) given SATIM_OUTPUT and INTX and QHX
$SETGLOBAL referencerun REF_EV-IN_RB-ZERO

Sets
  RUN                            simulations
  INCLRUN(RUN)                        whether to include or not run in batch
*  GDXFile                        GDX file per simulation /NoLink_EV-IN, REF_EV-IN_RB-ZERO,REF_EV-IN_RB-0-25,REF_EV-IN_RB-0-5,REF_EV-OUT_RB-ZERO,REF_EV-OUT_RB-0-25,REF_EV-OUT_RB-0-5 /
  GDXFile                        GDX file per simulation /CalibRefSIC/
*, CalibRefSATIM/
  MGDXRUN(GDXFile,RUN)           Map of RUN and GDX file
* SATIM sets
  REG                            TIMES regions    /REGION1/
  ALLYEAR                        All Years
  T(ALLYEAR)                     Time periods
  V(ALLYEAR)                     Vintage
  S                              TIMES timeslices
  PRC                            TIMES Processes
  P(PRC)                         Processes
  COM                            TIMES Commodities
  X                       simulations
  XC(X)                   active simulations
  TC(T)                   active time periods
*Times model sets
  TT(T)                   TIMES model years
  IN_OUT flow direction /IN, OUT/
  SATIM_IND /Activity, Capacity, NewCapacity, FlowIn, FlowOut, CO2, CH4, N2O, CO2eq, Investment, GVA, Employment/
  HH_EI Household Energy Indicators
  C                   commodities
  H               households
  MOD models /SATIM, eSAGE/
  Emiss(SATIM_IND) / CO2, CH4, N2O, CO2eq/
  FS                      SATIM sectors
  MPRCFS(PRC,FS)            mapping SATIM PRCs to SATIM sectors
  MFSP(FS,PRC)           mapping of technologies used for each sector to track process emissions
;

Parameters
  SATIM_OUTPUT(PRC,COM,T,SATIM_IND,RUN) SATIM indicators by run and activity
  SATIM_OUTPUT2(PRC,COM,T,SATIM_IND,GDXFile) SATIM indicators by run and activity
  HH_Energy(MOD,HH_EI,C,T,RUN) Household energy consumption
  HH_Energy2(MOD,HH_EI,C,T,GDXFile) Household energy consumption
  QHX(C,H,X,T,TT)                  qnty consumed of market commodity c by household h

  EmisFactor(COM,Emiss) Combustion Emission Factor

  ProcessEmissions(FS,T,Emiss,GDXFILE) Process emissions
;

$gdxin  %referencerun%.gdx
$load PRC P COM ALLYEAR S V T X XC TC TT RUN HH_EI H C FS MPRCFS

$call   "gdxxrw i=EmissionFactors.xlsx o=EmisFac index=Index!a6 checkdate"
$gdxin  EmisFac.gdx
$load EmisFactor

MGDXRUN('CalibRefSIC','REF2019-UCE-L') = YES;
*MGDXRUN('CalibRefSIC','REF2019-UCE-L') = YES;

*MGDXRUN('REF_EV-IN_RB-ZERO','REF2019-UCE-L') = YES;
*MGDXRUN('REF_EV-IN_RB-0-25','REF2019-UCE-L') = YES;
*MGDXRUN('REF_EV-IN_RB-0-5','REF2019-UCE-L') = YES;
*MGDXRUN('REF_EV-OUT_RB-ZERO','TRAPES-UCE-L') = YES;
*MGDXRUN('REF_EV-OUT_RB-0-25','TRAPES-UCE-L') = YES;
*MGDXRUN('REF_EV-OUT_RB-0-5','TRAPES-UCE-L') = YES;

* chemicals sector
MFSP('cp','XICPGAS') = yes;
* non metallic minerals (cement, lime and glass)
MFSP('nm','XINMGAS') = yes;
* non ferous (aluminium, lead and zinc)
MFSP('nf','XINFGAS') = yes;
* iron and steel
MFSP('is','XIISGAS') = yes;
* ferro-alloys
* 'fa' not defined yet need to do so.
*MFSP('fa','XIFAGAS') = yes;
* non-energy 2D, and product use 2F assigned to "industry other", although the emission should take place
* in the commercial sector in SATIM, as the dominant emission out of this lot is 2F1-refrigerant-air-conditioning.
MFSP('COM','XIOTGAS') = yes;



FILE SATIM_Scen;

LOOP(GDXFile,

put_utilities SATIM_Scen 'gdxin' / GDXFile.TL:20;

execute_load SATIM_OUTPUT HH_Energy;
Loop(RUN,
SATIM_OUTPUT2(PRC,COM,TC,SATIM_IND,GDXFile)$MGDXRUN(GDXFile,RUN) = SATIM_OUTPUT(PRC,COM,TC,SATIM_IND,RUN);
HH_Energy2(MOD,HH_EI,C,TC,GDXFile)$MGDXRUN(GDXFile,RUN) = HH_Energy(MOD,HH_EI,C,TC,RUN);
);


SATIM_OUTPUT2(PRC,COM,TC,Emiss,GDXFile) = SATIM_OUTPUT2(PRC,COM,TC,'FlowIn',GDXFile)*EmisFactor(Com,Emiss);

* convert energy flows to TJ to match inventory and DOE energy balances
SATIM_OUTPUT2(PRC,COM,TC,'Flowin',GDXFile) = SATIM_OUTPUT2(PRC,COM,TC,'FlowIn',GDXFile)*1000;

* Refinery sector Combustion emissions 1A
SATIM_OUTPUT2(PRC,'ACTGRP',TC,'CO2',GDXFile) = SATIM_OUTPUT2(PRC,'UPSCO2S',TC,'FlowOut',GDXFile);
SATIM_OUTPUT2(PRC,'ACTGRP',TC,'CH4',GDXFile) = SATIM_OUTPUT2(PRC,'UPSCH4S',TC,'FlowOut',GDXFile);

* Mining 1B1 Fugitive Emissions
*SATIM_OUTPUT2('XUPSCLE','ACTGRP',TC,'CO2',GDXFile) = sum(PRC$MPRCFS(PRC,'coal'),SATIM_OUTPUT2(PRC,'CO2SF',TC,'FlowOut',GDXFile));
*SATIM_OUTPUT2('XUPSCLE','ACTGRP',TC,'CH4',GDXFile) = sum(PRC$MPRCFS(PRC,'coal'),SATIM_OUTPUT2(PRC,'CH4SF',TC,'FlowOut',GDXFile));
SATIM_OUTPUT2(PRC,'ACTGRP',TC,'CO2',GDXFile)$MPRCFS(PRC,'coal') = SATIM_OUTPUT2(PRC,'CO2SF',TC,'FlowOut',GDXFile));
SATIM_OUTPUT2(PRC,'ACTGRP',TC,'CH4',GDXFile)$MPRCFS(PRC,'coal') = SATIM_OUTPUT2(PRC,'CH4SF',TC,'FlowOut',GDXFile));




* CTL 1B3 Fugitive emissions
SATIM_OUTPUT2('UTRNGIMGAS','ACTGRP',TC,'CO2',GDXFile) = SATIM_OUTPUT2('UCTLCLEIN-E','CO2SF',TC,'FlowOut',GDXFile);
SATIM_OUTPUT2('UTRNGIMGAS','ACTGRP',TC,'CH4',GDXFile) = SATIM_OUTPUT2('UCTLCLEIN-E','CH4SF',TC,'FlowOut',GDXFile);

* GTL 1B3 emissions

* Gas 1B2 Fugitive Emissions = the rest of fugitive emissions happening all over the place (gas network)
SATIM_OUTPUT2('XUPSGAS','ACTGRP',TC,'CH4',GDXFile) = sum(PRC,SATIM_OUTPUT2(PRC,'CH4SF',TC,'FlowOut',GDXFile))
                                                     -SATIM_OUTPUT2('XUPSCLE','ACTGRP',TC,'CH4',GDXFile)
                                                     -SATIM_OUTPUT2('UTRNGIMGAS','ACTGRP',TC,'CH4',GDXFile);

* Process emissions
ProcessEmissions(FS,TC,'CO2',GDXFILE) = sum(PRC$MPRCFS(PRC,FS),SATIM_OUTPUT2(PRC,'CO2SP',TC,'FlowOut',GDXFile));
*ProcessEmissions(FS,TC,'CH4',GDXFILE) = sum(PRC$MPRCFS(PRC,FS),SATIM_OUTPUT2(PRC,'CH4SP',TC,'FlowOut',GDXFile));
*ProcessEmissions(FS,TC,'N2O',GDXFILE) = sum(PRC$MPRCFS(PRC,FS),SATIM_OUTPUT2(PRC,'N2OSP',TC,'FlowOut',GDXFile));

loop(FS,
*SATIM_OUTPUT2(PRC,COM,TC,Emiss,GDXFile)$MFSP(FS,PRC) = 0;
SATIM_OUTPUT2(PRC,'ACTGRP',TC,Emiss,GDXFile)$MFSP(FS,PRC) = ProcessEmissions(FS,TC,Emiss,GDXFile);
);

);

execute_unload "SATIM_OUTPUT.gdx" SATIM_OUTPUT2
execute 'gdxxrw.exe i=SATIM_OUTPUT.gdx o=.\Emissions_Calibration\SATIM_OUTPUT.xlsx index=index!a6';

*execute_unload "HH_En.gdx" HH_Energy2
*execute 'gdxxrw.exe i=HH_En.gdx o=C:\SATIMGE_03\Emissions_Calibration\Private_Transport_Results_v00.xlsx index=index!a6';

