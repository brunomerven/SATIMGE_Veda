sets
  MOD models /SATIM, eSAGE/
  QType quantitity types /qq, qd, qint, qe, qm, qh, qx, qxac/
  IN_OUT flow direction /IN, OUT, CO2/
  SATIM_IND SATIM indicators /Activity, Capacity, NewCapacity, FlowIn, FlowOut, CO2, Investment, GVA, Employment/
  CGE_IND /GVA, Employment/

  SATIM_LAB(UC_N) SATIM employment constraint names / UCPWR_FLAB-M, UCPWR_FLAB-P, UCPWR_FLAB-S, UCPWR_FLAB-T/

  HH_EI Household Energy Indicators /TRA, OTH/

;

parameters
 BalanceCheck(QType,C,TC,RUN) balance check

 EBCHECK(MOD,FSATIM,IN_OUT,C,TC,TT,RUN) Final Energy Demand for SATIM sectors in SATIM and eSAGE models

 EIntensity(MOD,FSATIM,C,TC,TT,RUN) Energy Intensity in SATIM and ESAGE models

 CO2OUT(C) CO2 emission factors for linked model

 CGE_OUTPUT(A,TC,CGE_IND,RUN) CGE indicators by run and activity

 SATIM_OUTPUT(PRC,COM,TC,SATIM_IND,RUN) SATIM indicators by run and activity

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

