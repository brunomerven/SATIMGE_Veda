* this module is run at the end to compile energy sector emissions

REPORT(PRC,COM,TC,RUN,Emiss) = REPORT(PRC,COM,TC,RUN,'FlowIn')*EmisFactor(Com,Emiss);

* convert energy flows to TJ to match inventory and DOE energy balances
*REPORT(PRC,COM,TC,RUN,'Flowin') = REPORT(PRC,COM,TC,RUN,'FlowIn')*1000;

* Refinery sector Combustion emissions 1A
REPORT(PRC,'ACTGRP',TC,RUN,'CO2') = REPORT(PRC,'UPSCO2S',TC,RUN,'FlowOut');
REPORT(PRC,'ACTGRP',TC,RUN,'CH4') = REPORT(PRC,'UPSCH4S',TC,RUN,'FlowOut');
*Sasol Boilers for CTL plant
REPORT('UHETCLE-E','UPSCLE',TC,RUN,'CO2') = REPORT('UHETCLE-E','UPSCO2S',TC,RUN,'FlowOut');
REPORT('UHETCLE-E','UPSCLE',TC,RUN,'CH4') = REPORT('UHETCLE-E','UPSCH4S',TC,RUN,'FlowOut');
REPORT('UHETCLE-E','ACTGRP',TC,RUN,'CO2') = 0;
REPORT('UHETCLE-E','ACTGRP',TC,RUN,'CH4') = 0;

*Sasol Boilers used for Electricity Generation
REPORT('ETCLEPFSS-E','UPSCLE',TC,RUN,'CO2') = REPORT('ETCLEPFSS-E','UPSCLE',TC,RUN,'FlowIn')*96.25;
REPORT('ETCLEPFSS-E','UPSCLE',TC,RUN,'CH4') = REPORT('ETCLEPFSS-E','UPSCLE',TC,RUN,'FlowIn')*0.001;
REPORT('ETCLEPFSS-E','UPSCLE',TC,RUN,'N2O') = REPORT('ETCLEPFSS-E','UPSCLE',TC,RUN,'FlowIn')*0.0014;



* Mining 1B1 Fugitive Emissions
*SATIM_OUTPUT2('XUPSCLE','ACTGRP',TC,'CO2',GDXFile) = sum(PRC$MPRCFS(PRC,'coal'),SATIM_OUTPUT2(PRC,'CO2SF',TC,'FlowOut',GDXFile));
*SATIM_OUTPUT2('XUPSCLE','ACTGRP',TC,'CH4',GDXFile) = sum(PRC$MPRCFS(PRC,'coal'),SATIM_OUTPUT2(PRC,'CH4SF',TC,'FlowOut',GDXFile));
REPORT(PRC,'ACTGRP',TC,RUN,'CO2')$MPRCFS(PRC,'coal') = REPORT(PRC,'CO2SF',TC,RUN,'FlowOut');
REPORT(PRC,'ACTGRP',TC,RUN,'CH4')$MPRCFS(PRC,'coal') = REPORT(PRC,'CH4SF',TC,RUN,'FlowOut');

* CTL 1B3 Fugitive emissions
REPORT('UCTLCLEIN-E','UPSCLE',TC,RUN,'CO2') = REPORT('UCTLCLEIN-E','CO2SF',TC,RUN,'FlowOut');
REPORT('UCTLCLEIN-E','UPSCLE',TC,RUN,'CH4') = REPORT('UCTLCLEIN-E','CH4SF',TC,RUN,'FlowOut');

* GTL 1B3 emissions

* Gas 1B2 Fugitive Emissions = the rest of fugitive emissions happening all over the place (gas network)
REPORT('XUPSGAS','ACTGRP',TC,RUN,'CH4') = sum(PRC,REPORT(PRC,'CH4SF',TC,RUN,'FlowOut'))
                                                     -REPORT('XUPSCLE','ACTGRP',TC,RUN,'CH4')
                                                     -REPORT('UTRNGIMGAS','ACTGRP',TC,RUN,'CH4');

* Process emissions
ProcessEmissions(FS,TC,RUN,'CO2') = sum(PRC$MPRCFS(PRC,FS),REPORT(PRC,'CO2SP',TC,RUN,'FlowOut'));
ProcessEmissions('fa',TC,RUN,'CO2') = REPORT('IFAEAF','CO2SP',TC,RUN,'FlowOut');
ProcessEmissions('fa',TC,RUN,'CH4') = REPORT('IFAEAF','CH4S',TC,RUN,'FlowOut');
*Ferro-alloys currently falling under is (iron and steel) because it is not in the CGE
ProcessEmissions('is',TC,RUN,Emiss) = ProcessEmissions('is',TC,RUN,Emiss)-ProcessEmissions('fa',TC,RUN,Emiss);


*ProcessEmissions(FS,TC,RUN,'CH4') = sum(PRC$MPRCFS(PRC,FS),REPORT(PRC,'CH4SP',TC,RUN,'FlowOut'));
ProcessEmissions(FS,TC,RUN,'CF4') = sum(PRC$MPRCFS(PRC,FS),REPORT(PRC,'CF4',TC,RUN,'FlowOut'));
ProcessEmissions(FS,TC,RUN,'C2F6') = sum(PRC$MPRCFS(PRC,FS),REPORT(PRC,'CF6',TC,RUN,'FlowOut'));

* Default emission for HFC's using CO2 eq as per inventory
ProcessEmissions('COM',TC,RUN,'CO2eq') = 4035;
ProcessEmissions('cp',TC,RUN,'CO2') = 523.9;
ProcessEmissions('cp',TC,RUN,'CH4') = 8;
ProcessEmissions('cp',TC,RUN,'N2O') = 0.9;

* Below stopped working not sure why, using manual process below in the meantime
*loop(FS,
*SATIM_OUTPUT2(PRC,COM,TC,Emiss,GDXFile)$MFSP(FS,PRC) = 0;
*REPORT(PRC,'ACTGRP',TC,RUN,Emiss)$MFSP(FS,PRC) = ProcessEmissions(FS,TC,RUN,Emiss);
*);
Loop(Emiss,
REPORT('I2A','ACTGRP',TC,RUN,Emiss) = ProcessEmissions('nm',TC,RUN,Emiss);
REPORT('I2B','ACTGRP',TC,RUN,Emiss) = ProcessEmissions('cp',TC,RUN,Emiss);
REPORT('I2C1','ACTGRP',TC,RUN,Emiss) = ProcessEmissions('is',TC,RUN,Emiss);
REPORT('I2C2','ACTGRP',TC,RUN,Emiss) = ProcessEmissions('fa',TC,RUN,Emiss);
REPORT('I2C3','ACTGRP',TC,RUN,Emiss) = ProcessEmissions('nf',TC,RUN,Emiss);
REPORT('I2F','ACTGRP',TC,RUN,Emiss) = ProcessEmissions('COM',TC,RUN,Emiss);
);

