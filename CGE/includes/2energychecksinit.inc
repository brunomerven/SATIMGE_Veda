sets
  MOD models /SATIM, eSAGE/
  HH_EI Household Energy Indicators /TRA, OTH/
;

parameters

 ModelCompa(MOD,FSATIM,Indicators,C,TC,TT,RUN) Model Comparison of common indicators
 TmpQINT(A) Temporary hold for total QINT*PQ
 EIntensity(MOD,FSATIM,C,TC,TT,RUN) Energy Intensity in SATIM and ESAGE models

 CO2OUT(C) CO2 emission factors for linked model

 HH_Energy(MOD,HH_EI,C,TC,RUN) Household energy consumption
;

 CO2OUT('ccoal-low') = 96.25;
 CO2OUT('ccoal-hgh') = 96.25;
 CO2OUT('cngas') = 56.1;
 CO2OUT('cpetr-p') = 69.3;
 CO2OUT('cpetr-d') = 74.0667;
* bm average for HFO, LPG and Kerosene
 CO2OUT('cpetr-h') = 77.4;
 CO2OUT('cpetr-k') = 72.9;
 CO2OUT('cpetr-l') = 63.1;

