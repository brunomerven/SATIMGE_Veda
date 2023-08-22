* Initalization of sets and parameters used for KLEM linked model
* Bruno Merven November 2020


Parameters
   K_SAGE(K_Sectors,*,RUN)       KLEM aggregate indicator values
;

* Define KLEM Sectors

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

