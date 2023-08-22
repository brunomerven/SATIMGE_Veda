* Initalization of sets and parameters used for KLEM linked model
* Bruno Merven November 2020

SETS
   K_Energy(FSATIM)      KLEM Energy Sectors
   K_EnergyNoElc(FSATIM) KLEM Energy Sectors except Electricity
   K_NonEnergy(FSATIM)   KLEM Non-Energy Sectors
   K_Households(FSATIM)  KLEM households
   K_Indicators KLEM indicators / E_Bill, E_Price, E_Input, E_Output /
   K_Sectors KLEM sectors / E-Firms, NonE-Firms, Households, Trade/
;

Parameters
* intermediate calc parameters
   K_EnergyBill(FSATIM,COM,AY)  KLEM Energy Bill by Sector based on Marginals
   K_EnergyPrice(FSATIM,COM,AY) KLEM Sector Prices for all TIMES commodities

   K_EnergyBillFuels(FSATIM,C,AY) KLEM Energy Bill by sector for aggregate commodities
   K_EnergyPriceFuels(FSATIM,C,AY) KLEM Sector Prices for aggregate commodities

   K_Output(FSATIM,C,AY) KLEM output of energy sector
   K_Input(FSATIM,C,AY) KLEM input of energy sector

* final indicators
   K_AggInd(K_Sectors,K_Indicators,AY,RUN)       KLEM aggregate indicator values
;


* define KLEM Sectors

* Households
K_Households(FH) = Yes;
K_Households('tppr') = Yes;

* NonEnergy Sectors
K_NonEnergy(FSATIM) = Yes;
K_NonEnergy('EXP') = No;
K_NonEnergy('IMP') = No;
K_NonEnergy(K_Households) = No;
K_NonEnergy('hydr') = No;
K_NonEnergy('elec') = No;
K_NonEnergy('petr') = No;
K_NonEnergy('GAS') = No;
K_NonEnergy('coal') = No;

* Energy Sectors
K_Energy(FSATIM) = Yes;
K_Energy(K_NonEnergy) = No;
K_Energy('EXP') = No;
K_Energy('IMP') = No;
* Gas part of imports
K_Energy('GAS') = No;

* Coal in or out?
K_Energy('coal') = No;

K_NonEnergy(K_Households) = No;

*Energy Except Electricity
K_EnergyNoElc(K_Energy) = Yes;
K_EnergyNoElc('elec') = No;
