

* ModelCompa - to compare the energy use by SATIM sectors

* Energy Flows
ModelCompa('SATIM',FS,'FlowIn',C,TC,TT,RUN) = FLOW_IN_FSFuels(FS,C,TC)*(-1);
ModelCompa('SATIM',FS,'FlowOut',C,TC,TT,RUN) = FLOW_OUT_FSFuels(FS,C,TC);
ModelCompa('SATIM','elec','FlowOut','celec',TC,TT,RUN) = ELC_FLOW(RUN,TC);

ModelCompa('SATIM','imp','FlowOut',C,TC,TT,RUN) = ModelCompa('SATIM','imp','FlowOut',C,TC,TT,RUN)+ModelCompa('SATIM','exp','FlowIn',C,TC,TT,RUN);
ModelCompa('SATIM','exp','FlowIn',C,TC,TT,RUN) = 0;

ModelCompa('eSAGE',FS,'FlowIn',C,TC,TT,RUN)$energy(C) = SUM(A$MFSA(FS,A),QINTX(C,A,XC,TC,TT))*(-1);

ModelCompa('eSAGE',FS,'FlowOut',C,TC,TT,RUN)$energy(C) = SUM(A$MFSA(FS,A),QXACX(A,C,XC,TC,TT));
ModelCompa('eSAGE','imp','FlowOut',C,TC,TT,RUN) = QMX(C,'rest',XC,TC,TT)-QEX(C,'rest',XC,TC,TT);

ModelCompa('SATIM',FH,'FlowIn',C,TC,TT,RUN) = FLOW_IN_FSFuels(FH,C,TC)*(-1);
ModelCompa('eSAGE',FH,'FlowIn',C,TC,TT,RUN)$energy(C) = SUM(H$MFHHT(FH,H,TC),QHX(C,H,XC,TC,TT))*(-1);

* CO2
ModelCompa(MOD,FSATIM,'CO2',C,TC,TT,RUN) = ModelCompa(MOD,FSATIM,'FlowIn',C,TC,TT,RUN)*CO2OUT(C);

* Costs
* using 'cimpt' for non commodity specific indicators
ModelCompa('SATIM',FS,'AnnInvCost','cimpt',TC,TT,RUN) = sum(PRC$MPRCFS(PRC,FS),REPORT(PRC,'ACTGRP',TC,RUN,'AnnInvCost'))*0.868/1000;
ModelCompa('SATIM',FS,'OM','cimpt',TC,TT,RUN) = sum(PRC$MPRCFS(PRC,FS),REPORT(PRC,'ACTGRP',TC,RUN,'FOM'))/1000+sum(PRC$MPRCFS(PRC,FS),REPORT(PRC,'ACTGRP',TC,RUN,'VOM'))*0.868/1000;
ModelCompa('SATIM',FS,'FuelCosts',C,TC,TT,RUN) = SUM(COM$MCOMC(COM,C),sum(PRC$MPRCFS(PRC,FS),REPORT(PRC,COM,TC,RUN,'FuelCosts')))*0.868/1000;

ModelCompa('eSAGE',FS,'AnnInvCost','cimpt',TC,TT,RUN) = SUM(A$MFSA(FS,A),SUM(FCAP,QFX(FCAP,A,'nat',XC,TC,TT)*WFX(FCAP,XC,TC,TT)*WFDISTX(FCAP,A,'nat',XC,TC,TT)));

ModelCompa('eSAGE',FS,'OM','cimpt',TC,TT,RUN) = SUM(A$MFSA(FS,A),SUM(FLAB,QFX(FLAB,A,'nat',XC,TC,TT)*WFX(FLAB,XC,TC,TT)*WFDISTX(FLAB,A,'nat',XC,TC,TT)));
*                                                + SUM(A$MFSA(FS,A),sum(nonenergy,QINTX(nonenergy,A,XC,TC,TT)*PQX(nonenergy,XC,TC,TT)));

ModelCompa('eSAGE',FS,'FuelCosts',energy,TC,TT,RUN) = SUM(A$MFSA(FS,A),QINTX(energy,A,XC,TC,TT)*PQX(energy,XC,TC,TT));


* Checks and results for private transport modification
HH_Energy('SATIM','TRA',C,TC,RUN) = Flow_IN_FSFuels('tppr',C,TC)*0.8;
* 0.8 is the assumption that 20% of fuel is not consumed/paid for from households budgets: rental cars, govm, companies, etc.
HH_Energy('SATIM','OTH',C,TC,RUN) = sum(FH,Flow_IN_FSFuels(FH,C,TC));
HH_Energy('eSAGE','TRA',C,TC,RUN) = QINTX(C,'aprtr',XC,TC,TT);
HH_Energy('eSAGE','OTH',CHEnergy,TC,RUN) = sum(H,QHX(CHEnergy,H,XC,TC,TT));
