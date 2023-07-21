* Process emissions

*Mineral Industry
REPORT('I2A1','ACTGRP',TC,RUN,'CO2') = sum(PRC$MPRCSubSubSector(PRC,'Industry','NMMProducts34','Cement'),REPORT(PRC,'CO2SPCEM',TC,RUN,'FlowOut'));
REPORT('I2A2','ACTGRP',TC,RUN,'CO2') = sum(PRC$MPRCSubSubSector(PRC,'Industry','NMMProducts34','Lime'),REPORT(PRC,'CO2SPLIM',TC,RUN,'FlowOut'));
REPORT('I2A3','ACTGRP',TC,RUN,'CO2') = sum(PRC$MPRCSubSubSector(PRC,'Industry','NMMProducts34','Glass'),REPORT(PRC,'CO2SP',TC,RUN,'FlowOut'));

* Chemical Industry (currently not in SATIM),
* Ammonia now in SATIM
*REPORT('I2B1','ACTGRP',TC,RUN,'CH4') = 7.962*GVA_FS('cp',TC)/GVA_FS('cp','2019');
*REPORT('I2B1','ACTGRP',TC,RUN,'CO2') = 241.415*GVA_FS('cp',TC)/GVA_FS('cp','2019');


REPORT('I2B2','ACTGRP',TC,RUN,'N2O') = 0.944*GVA_FS('cp',TC)/GVA_FS('cp','2019');
REPORT('I2B5','ACTGRP',TC,RUN,'CO2') = 2.81*GVA_FS('cp',TC)/GVA_FS('cp','2019');
REPORT('I2B6','ACTGRP',TC,RUN,'CO2') = 152.39*GVA_FS('cp',TC)/GVA_FS('cp','2019');
REPORT('I2B8','ACTGRP',TC,RUN,'CO2') = 127.24*GVA_FS('cp',TC)/GVA_FS('cp','2019');
REPORT('I2B8','ACTGRP',TC,RUN,'CH4') = 0.0029*GVA_FS('cp',TC)/GVA_FS('cp','2019');

* Metals Industry (lead 2C5 and zinc 2C6 currently not in SATIM)
REPORT('I2C1','ACTGRP',TC,RUN,'CO2') = sum(PRC$MPRCSubSector(PRC,'Industry','Iron_Steel351'),REPORT(PRC,'CO2SP',TC,RUN,'FlowOut'));
REPORT('I2C2','ACTGRP',TC,RUN,'CO2') = sum(PRC$MPRCSubSector(PRC,'Industry','FerroAlloys'),REPORT(PRC,'CO2SPIFC',TC,RUN,'FlowOut'))+sum(PRC$MPRCSubSector(PRC,'Industry','FerroAlloys'),REPORT(PRC,'CO2SPIFM',TC,RUN,'FlowOut'));
REPORT('I2C2','ACTGRP',TC,RUN,'CH4') = sum(PRC$MPRCSubSector(PRC,'Industry','FerroAlloys'),REPORT(PRC,'CH4S',TC,RUN,'FlowOut'));
REPORT('I2C3','ACTGRP',TC,RUN,'CO2') = sum(PRC$MPRCSubSector(PRC,'Industry','Aluminium35220'),REPORT(PRC,'CO2SP',TC,RUN,'FlowOut'));
REPORT('I2C3','ACTGRP',TC,RUN,'CF4') = sum(PRC$MPRCSubSector(PRC,'Industry','Aluminium35220'),REPORT(PRC,'CF4',TC,RUN,'FlowOut'));
REPORT('I2C3','ACTGRP',TC,RUN,'C2F6') = sum(PRC$MPRCSubSector(PRC,'Industry','Aluminium35220'),REPORT(PRC,'CF6',TC,RUN,'FlowOut'));
REPORT('I2C5','ACTGRP',TC,RUN,'CO2') = 21.74*GVA_FS('nf',TC)/GVA_FS('nf','2019');
REPORT('I2C6','ACTGRP',TC,RUN,'CO2') = 46.32*GVA_FS('nf',TC)/GVA_FS('nf','2019');

* CO2 capture accounted for under CO2C
REPORT('I2A1','ACTGRP',TC,RUN,'CO2C') = sum(PRC$MPRCSector(PRC,'CO2Capture&Storage'),REPORT(PRC,'CO2SPCEM',TC,RUN,'FlowOut'));
REPORT('I2A2','ACTGRP',TC,RUN,'CO2C') = sum(PRC$MPRCSector(PRC,'CO2Capture&Storage'),REPORT(PRC,'CO2SPLIM',TC,RUN,'FlowOut'));

REPORT('I2C2','ACTGRP',TC,RUN,'CO2C') = sum(PRC$MPRCSector(PRC,'CO2Capture&Storage'),REPORT(PRC,'CO2SPIFC',TC,RUN,'FlowOut'))+sum(PRC$MPRCSector(PRC,'CO2Capture&Storage'),REPORT(PRC,'CO2SPIFM',TC,RUN,'FlowOut'));

* still need to add others if needed


* Other  (currently not in SATIM)
REPORT('I2D1','ACTGRP',TC,RUN,'CO2') = 272.94*sum(fs,GVA_FS(fs,TC))/sum(fs,GVA_FS(fs,'2019'));
REPORT('I2D2','ACTGRP',TC,RUN,'CO2') = 2.7*sum(fs,GVA_FS(fs,TC))/sum(fs,GVA_FS(fs,'2019'));

*REPORT('I2F1','ACTGRP',TC,RUN,'CO2eq') = 3963.46*GVA_FS('COM',TC)/GVA_FS('COM','2019');

REPORT('I2F1','ACTGRP',TC,RUN,'CO2eq') = 3963.46;
Loop(TC,
  if(ORD(TC) gt 14,
      REPORT('I2F1','ACTGRP',TC,RUN,'CO2eq') = REPORT('I2F1','ACTGRP',TC-1,RUN,'CO2eq') - 3963.46/25;
  )
);


REPORT('I2F2','ACTGRP',TC,RUN,'CO2eq') = 2.104*sum(fs,GVA_FS(fs,TC))/sum(fs,GVA_FS(fs,'2019'));
REPORT('I2F3','ACTGRP',TC,RUN,'CO2eq') = 51.09*sum(fs,GVA_FS(fs,TC))/sum(fs,GVA_FS(fs,'2019'));
REPORT('I2F4','ACTGRP',TC,RUN,'CO2eq') = 18.2*sum(fs,GVA_FS(fs,TC))/sum(fs,GVA_FS(fs,'2019'));

