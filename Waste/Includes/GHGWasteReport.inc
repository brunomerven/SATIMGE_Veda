* this module is run at the end to compile energy sector emissions
* July 2020
* By Bruno Merven which draws from spreadsheet Models put together by Julia McGregor and Harro von Blottnitz
*

* Write Drivers to Spreadsheet
CoalAshCoal(TC) = sum(fs,FLOW_IN_FSFuels(fs,'ccoal-low',TC));
execute_unload "CoalAshCoal.gdx" CoalAshCoal GDP_RUN POP PAMS_RUN
execute 'gdxxrw.exe i=CoalAshCoal.gdx o=Waste\Waste_model_Linked.xlsx index=indexG2E!a6';

* Read Results from Spreadsheet
execute 'gdxxrw.exe i=Waste\Waste_model_Linked.xlsx o=Waste.gdx index=IndexE2G!a6';
execute_load "Waste.gdx" WasteEmissions;


REPORT(PRC,'ACTGRP',TC,RUN,Emiss)$WasteEmissions(PRC,Emiss,TC) = WasteEmissions(PRC,Emiss,TC);

REPORT(PRC,'ACTGRP',ENDYEARS,RUN,Emiss)$WasteEmissions(PRC,Emiss,'2050') = WasteEmissions(PRC,Emiss,'2050');