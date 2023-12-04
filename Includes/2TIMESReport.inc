*--------------------------------------------------------------
*Process TIMES results -------------------------------------
*--------------------------------------------------------------

loop(MRUNCASE(RUN,SATIMCASES),

* put_utilities SATIM_Scen 'gdxin' / "%GDXfolder%" "\Gamssave\" SATIMCASES.TL:100;

 put_utilities SATIM_Scen 'gdxin' / "%GDXfolder%\" SATIMCASES.TL:0 "\Gamssave\" SATIMCASES.TL:100;
* put_utilities SATIM_Scen 'gdxin' / "%GDXfolder%\TestBY\Gamssave\",SATIMCASES.TL:50;
 execute_load MILESTONYR ELE NRGELC RTP B E V TS_MAP PRC_RCAP PRC_TSL TS_GROUP
* NRGELC
*COM_NRGELC PRC_ANNUAL PRC_WEEKLY PRC_SEASON PRC_DAYNIT
 execute_load COM_PROJ REG_OBJ PAR_NCAPL PRC_RESID PRC_CAPACT PAR_CAPL VAR_ACT F_IN F_OUT COEF_ILED OB_ICOST OB_ACT OB_FOM PAR_COMBALEM CST_FIXC CST_ACTC CST_INVC VAR_CAP UC_CAP G_YRFR COM_CSTNET;
* VAR_RCAP;
);

VARACT('REGION1',TTIMES,PRC) = SUM((V,S), VAR_ACT.L('REGION1',V,TTIMES,PRC,S));

TGAP(MILESTONYR) = E(MILESTONYR) - B(MILESTONYR)+1;

SUPELE(PRC)$ELE('REGION1',PRC) = YES;

* interpolate all TIMES results for CGE link and report tables

RESID(REG,MILESTONYR,PRC) = PRC_RESID(REG,MILESTONYR,PRC);
* perform interpolation
*$BATINCLUDE interpolate PRC_RESID 'REG' 'PRC' ",'0','0','0','0','0'" TC 'RTP(REG,TC,PRC)'

*$ontext
$BATINCLUDE Includes\interpolate.inc RESID 'REG' 'PRC' ",'0','0','0','0','0'" TC 'RTP(REG,TC,PRC)'

$BATINCLUDE Includes\interpolate.inc PAR_CAPL 'REG' 'PRC' ",'0','0','0','0','0'" TC 'PAR_CAPL(REG,TC,PRC)'

*$BATINCLUDE interpolate PAR_RCAPL 'REG' 'PRC' ",'0','0','0','0','0'" TC 'PAR_RCAPL(REG,TC,PRC)'

$BATINCLUDE Includes\interpolate.inc VARACT 'REG' 'PRC' ",'0','0','0','0','0'" TC 'VARACT(REG,TC,PRC)'

 OBICOST(REG,TC,PRC) = OB_ICOST(REG,PRC,'MZAR2022',TC);
$BATINCLUDE Includes\interpolate.inc OBICOST 'REG' 'PRC' ",'0','0','0','0','0'" TC 'RTP(REG,TC,PRC)'

 OBACT(REG,TC,PRC) = OB_ACT(REG,PRC,'MZAR2022',TC);
$BATINCLUDE Includes\interpolate.inc OBACT 'REG' 'PRC' ",'0','0','0','0','0'" TC 'RTP(REG,TC,PRC)'

 TVFLOWIN(COM,TC,PRC) = SUM((V,S), F_IN('REGION1',V,TC,PRC,COM,S));
$BATINCLUDE Includes\interpolate.inc TVFLOWIN 'COM' 'PRC' ",'0','0','0','0','0'" TC 'TVFLOWIN(COM,TC,PRC)'

 TVFLOWOUT(COM,TC,PRC) = SUM((V,S), F_OUT('REGION1',V,TC,PRC,COM,S));
$BATINCLUDE Includes\interpolate.inc TVFLOWOUT 'COM' 'PRC' ",'0','0','0','0','0'" TC 'TVFLOWOUT(COM,TC,PRC)'

 TCST_INVC(REG,TC,PRC)= sum(V,CST_INVC(REG,V,TC,PRC,'INV'));
$BATINCLUDE Includes\interpolate.inc TCST_INVC 'REG' 'PRC' ",'0','0','0','0','0'" TC 'TCST_INVC(REG,TC,PRC)'

 TCST_ACTC(REG,TC,PRC)= sum(V,CST_ACTC(REG,V,TC,PRC,'-'));
$BATINCLUDE Includes\interpolate.inc TCST_ACTC 'REG' 'PRC' ",'0','0','0','0','0'" TC 'TCST_ACTC(REG,TC,PRC)'

 TCST_FIXC(REG,TC,PRC)= sum(V,CST_FIXC(REG,V,TC,PRC));
$BATINCLUDE Includes\interpolate.inc TCST_FIXC 'REG' 'PRC' ",'0','0','0','0','0'" TC 'TCST_FIXC(REG,TC,PRC)'

 COM_CSTCO2('REGION1',TC,'CO2EQST')= COM_CSTNET('REGION1',TC,'CO2S','ANNUAL','MZAR2022');
*$BATINCLUDE Includes\interpolate.inc COM_CSTCO2 'REG' 'COM' ",'0','0','0','0','0'" TC 'COM_CSTCO2(REG,TC,COM)'



 T_COMBALEM(REG,TC,COM) = PAR_COMBALEM(REG,TC,COM,'ANNUAL');
*Special calc for hydrogen for power
 T_COMBALEM('REGION1',TC,'PWRHGN')$SUM((V,S),F_OUT('REGION1',V,TC,'XPWRHGN','PWRHGN',S)) = SUM(S,PAR_COMBALEM('REGION1',TC,'PWRHGN',S)*SUM(V,F_OUT('REGION1',V,TC,'XPWRHGN','PWRHGN',S)))/SUM((V,S),F_OUT('REGION1',V,TC,'XPWRHGN','PWRHGN',S));
$BATINCLUDE Includes\interpolate.inc T_COMBALEM 'REG' 'COM' ",'0','0','0','0','0'" TC 'T_COMBALEM(REG,TC,COM)'

$BATINCLUDE Includes\interpolate.inc COM_PROJ 'REG' 'COM' ",'0','0','0','0','0'" TC 'COM_PROJ(REG,TC,COM)'


* Attempt at putting parameter for results equal to variable:
 PAR_VCAPL(REG,TC,PRC) = VAR_CAP.L(REG,TC,PRC);
$BATINCLUDE Includes\interpolate.inc PAR_VCAPL 'REG' 'PRC' ",'0','0','0','0','0'" TC 'PAR_VCAPL(REG,TC,PRC)'

*$offtext

PAR_VCAPL(REG,TC,PRC) = VAR_CAP.L(REG,TC,PRC);
OBICOST(REG,TC,PRC) = OB_ICOST(REG,PRC,'MZAR2022',TC);
TVFLOWIN(COM,TC,PRC) = SUM((V,S), F_IN('REGION1',V,TC,PRC,COM,S));
TVFLOWOUT(COM,TC,PRC) = SUM((V,S), F_OUT('REGION1',V,TC,PRC,COM,S));
TCST_INVC(REG,TC,PRC)= sum(V,CST_INVC(REG,V,TC,PRC,'INV'));
TCST_ACTC(REG,TC,PRC)= sum(V,CST_ACTC(REG,V,TC,PRC,'-'));
TCST_FIXC(REG,TC,PRC)= sum(V,CST_FIXC(REG,V,TC,PRC));
COM_CSTCO2('REGION1',TC,'CO2EQST')= COM_CSTNET('REGION1',TC,'CO2EQST','ANNUAL','MZAR2022');
T_COMBALEM('REGION1',TC,'PWRHGN')$SUM((V,S),F_OUT('REGION1',V,TC,'XPWRHGN','PWRHGN',S)) = SUM(S,PAR_COMBALEM('REGION1',TC,'PWRHGN',S)*SUM(V,F_OUT('REGION1',V,TC,'XPWRHGN','PWRHGN',S)))/SUM((V,S),F_OUT('REGION1',V,TC,'XPWRHGN','PWRHGN',S));

*excluding TVCOMBAL because of CO2 marginals TVCOMBAL(T,COM)  = PAR_COMBALEM('REGION1',T,COM,'ANNUAL');


* Calculate New Capacity from milestone year results
* for TVNCAP we assume uniform spread of the investment (although TIMES uses the discount rate to spread it - we could do this later)
 TVNCAP(TC,PRC)    = PAR_NCAPL('REGION1',TC,PRC);
 TVNCAP(MILESTONYR,PRC) = TVNCAP(MILESTONYR,PRC)/TGAP(MILESTONYR);
 LOOP(MILESTONYR,
   LOOP(TC,
     if(YEARVALT(TC) GE B(MILESTONYR) and YEARVALT(TC) LE E(MILESTONYR),
       TVNCAP(TC,PRC) = TVNCAP(MILESTONYR,PRC);
     );
   );
  );


 TVCAP(TC,PRC)     = PAR_VCAPL('REGION1',TC,PRC);

 TVCAP(TC,PRC)$(MPRCSubsubSector(PRC,'Power','ECoal','ECoal_Existing-Retrofit') and OB_FOM('REGION1',PRC,'MZAR2022',TC))= TCST_FIXC('REGION1',TC,PRC)/OB_FOM('REGION1',PRC,'MZAR2022',TC);


 TVACT(TC,PRC)     = VARACT('REGION1',TC,PRC);


* Calculate final consumption for SATIM industry sectors and freight
 FLOW_IN_FS(FSATIM,COM,TC) = SUM(PRC$MPRCFS(PRC,FSATIM), TVFLOWIN(COM,TC,PRC));
 FLOW_OUT_FS(FSATIM,COM,TC) = SUM(PRC$MPRCFS(PRC,FSATIM), TVFLOWOUT(COM,TC,PRC));

* calculate final consumption
 FLOW_IN_FSFuels(FSATIM,C,TC) = SUM(COM$MCOMC(COM,C),FLOW_IN_FS(FSATIM,COM,TC));
 FLOW_OUT_FSFuels(FSATIM,C,TC) = SUM(COM$MCOMC(COM,C),FLOW_OUT_FS(FSATIM,COM,TC));

* Factor inputs
 FLOW_IN_FSFactors(FS,F,TC) = SUM(COM$MCOMF(COM,F),FLOW_IN_FS(FS,COM,TC));

* Investment inputs
 FLOW_IN_FSInvestment(FS,C,TC) = SUM(COM$MCOMI(COM,C),FLOW_IN_FS(FS,COM,TC));


*bm methane rich gas is currently a product of sasol which is accounted for in SATIM, but not in eSAGE so making this zero
 FLOW_OUT_FSFuels('petr','cngas',TC) = 0;
*putting a small minimum to avoid division by zero when all refineries are shut
 FLOW_OUT_FSFuels('petr', 'cpetr-p',TC) = max(0.0001,FLOW_OUT_FSFuels('petr', 'cpetr-p',TC));
 FLOW_OUT_FSFuels('petr', 'cpetr-d',TC) = max(0.0001,FLOW_OUT_FSFuels('petr', 'cpetr-d',TC));


*bm correcting for elc autogeneration which should be excluded from elc demand
 FLOW_IN_FSFuels('petr','celec',TC) = max(0,FLOW_IN_FSFuels('petr','celec',TC)-FLOW_OUT_FSFuels('petr','celec',TC));
 FLOW_OUT_FSFuels('petr','celec',TC) = 0;


*Correction made for Pulp and paper own gen, this may have to be done to more sectors as own gen becomes more prevalent
 FLOW_IN_FSFuels('pp','celec',TC) = FLOW_IN_FSFuels('pp','celec',TC) - FLOW_OUT_FSFuels('pp','celec',TC);

*Correction made for power sector gas. New energy balance implies all gas consumed in chemical sector. Old EB had some gas going to power.
* Sasol autoproducers are no longer included in power.
 FLOW_IN_FSFuels('elec','cngas',TC) = max(FLOW_IN_FSFuels('elec','cngas',TC),3);


*UCONV_C_T2C(C);

*bm calc oil flow   !!! note that TVACT includes some non-energy commodities that are currently not part of the output of apetr in the CGE. These should ideally also be included there.
* OIL_FLOW(RUN,TC) = SUM(PRC$MPRCFS(PRC,'petr'), TVACT(TC,PRC));
 OIL_FLOW(RUN,TC) = sum(C,FLOW_OUT_FSFuels('petr',C,TC));

*bm calc coal flow - putting in a minimum value to avoid problems with theta calcs in CGE
 COAL_FLOW(RUN,TC,'ccoal-low') = max(30,sum(FSATIM,FLOW_IN_FSFuels(FSATIM,'ccoal-low',TC)));
 COAL_FLOW(RUN,TC,'ccoal-hgh') = max(30,sum(FSATIM,FLOW_IN_FSFuels(FSATIM,'ccoal-hgh',TC)));

* EIntensityQA_FS(FS,C,TC)$(QA_FS(FS,TC) GT 0.1) = FLOW_IN_FSFuels(FS,C,TC)/QA_FS(FS,TC);

 EIntensity_FS(FS,C,TC)$(GVA_FS(FS,TC) GT 0.1) = FLOW_IN_FSFuels(FS,C,TC)/GVA_FS(FS,TC);
* EIntensity_FS(FS,C,TC)$(QA_FS(FS,TC) GT 0.1) = FLOW_IN_FSFuels(FS,C,TC)/QA_FS(FS,TC);

*bm calculating coal mining capacity based on consumption+exports)
 COALMINCAP(TC) = sum(FSATIM,FLOW_IN_FSFuels(FSATIM,'ccoal-hgh',TC))+sum(FSATIM,FLOW_IN_FSFuels(FSATIM,'ccoal-low',TC));

* For households on transport
* multiply by share of pkm by household to allocate consumption to different hh
* FLOW_IN_FTFHFuels(FH,C,TC) = FLOW_IN_FTFuels('TPPR',C,TC)*PKMFH('TPPR',FH,TC);
* add transport fuels to other fuels
* FLOW_IN_FHAllFuels(FH,C,TC) = FLOW_IN_FHFuels(FH,C,TC)+FLOW_IN_FTFHFuels(FH,C,TC);

* EIntensity_FH(FH,C,TC)$TFHPOP_X(FH,TC) = FLOW_IN_FHAllFuels(FH,C,TC);
*/TFHPOP_X(FH,TC);

*IF(TT1(TT),

* FLOW_IN_FHFuelsGR(FH,C,TC)$EIntensity_FH(FH,C,TC-1) = EIntensity_FH(FH,C,TC)/EIntensity_FH(FH,C,TC-1);
*);
* FLOW_IN_FHFuelsGR('hhd-he','cpetr_o','2013') = 0;
* FLOW_IN_FHFuelsGR('hhd-he','cpetr_o','2014') = 0;
* FLOW_IN_FHFuelsGR('hhd-he','cpetr_o','2015') = 0;


*Investment costs from TIMES ----------------------------------

$ontext
 Investment costs are based on year of build decision (TVNCAP) and the number
 of years needed to build the plant (lead time NCAP_ILED).
 Annual cost = build capacity (TVNCAP) x unit cost (OBJ_ICOST) / years to build (NCAP_ILED)
 Calculation: Keep track of vintages (TP)
$offtext

COEF_ILED('REGION1',TC,PRC)$(COEF_ILED('REGION1',TC,PRC) EQ eps)= 1;

$ontext

LOOP((PRC,TC)$(TVNCAP(TC,PRC) AND NCAP_ILED('REGION1','2017',PRC) GT 0),
 LOOP(TCP$((ORD(TCP) GE ORD(TC)) AND (ORD(TCP) LT (ORD(TC)+NCAP_ILED('REGION1','2017',PRC)))),
  BPINV(RUN,TC,TCP,PRC)$NCAP_ILED('REGION1','2017',PRC) = TVNCAP(TC,PRC)/NCAP_ILED('REGION1','2017',PRC) * OBICOST('REGION1',TC,PRC);
 );
);

$offtext
LOOP((PRC,TC)$TVNCAP(TC,PRC),
 LOOP(TCP$((ORD(TCP) LT ORD(TC)) AND (ORD(TCP) GE (ORD(TC)-COEF_ILED('REGION1',TC,PRC)))),
  BPINV(RUN,TC,TCP,PRC)$COEF_ILED('REGION1',TC,PRC) = TVNCAP(TC,PRC)/(COEF_ILED('REGION1',TC,PRC)) * OBICOST('REGION1',TC,PRC);
 );
);

* expenditure schedule in billions
 INVCOST(RUN,TC,PRC) = SUM(TCP, BPINV(RUN,TCP,TC,PRC))/1000;
 Loop(Sector,
         INVCOSTSubSector(RUN,TC,Sector,SubSector) = SUM(PRC$MPRCSubSector(PRC,Sector,SubSector),INVCOST(RUN,TC,PRC));
 );
*Sum across vintages to get total annual cost (billions of rands) and convert from 2022 to 2019 Rands for CGE
* EINVCOST(XC,T,TT) = SUM((SUPELE AND SUPOIL), INVCOST(XC,T,TT,ELE));
 EINVCOST(RUN,TC) = SUM(PRCPower, INVCOST(RUN,TC,PRCPower))*0.868;

* fixing end effect until we run past 2055
 EINVCOST(RUN,'2055') = EINVCOST(RUN,'2054');

* setting starting value higher otherwise too much of a shock
 EINVCOST(RUN,'2019') = EINVCOST(RUN,'2020');
 EINVQINV(XC,T1,TT) = EINVCOST(RUN,'2020');



* EFLOW('NAT',XC,TC,TT) = SUM(SUPELE, TVACT(TC,SUPELE))/3.6;
* !!EFLOW now calculated from flow through transmission. When we include distributed generation we also need to include them in the calc
* ELC_FLOW2(RUN,TC) = (TVACT(TC,'XAGRELC') + TVACT(TC,'XCOMELC') +TVACT(TC,'XINDELC') + TVACT(TC,'XRESELC') + TVACT(TC,'XTRAELC') + TVACT(TC,'XUPSELC') + TVACT(TC,'PEXELC'));
 ELC_FLOW(RUN,TC) = sum(FSATIMNOELEC,FLOW_IN_FSFuels(FSATIMNOELEC,'celec',TC));

* ELC_FLOW(RUN,TC) = TVACT(TC,'ELCDEM');

* recalculate intensity for energy sectors, where GVA not suitable
 EIntensity_FS('elec',C,TC) = FLOW_IN_FSFuels('elec',C,TC)/ELC_FLOW(RUN,TC);
 EIntensity_FS('elec','ccoal-low',TC) = max(0.01,EIntensity_FS('elec','ccoal-low',TC));

 EIntensity_FS(FS,'ccoal-hgh',TC) = max(0.0001,EIntensity_FS(FS,'ccoal-hgh',TC));

 EIntensity_FS('hydr',C,TC)$(FLOW_OUT_FSFuels('hydr','chydr',TC) GT .001) = FLOW_IN_FSFuels('hydr',C,TC)/FLOW_OUT_FSFuels('hydr','chydr',TC);
 EIntensity_FS('petr',C,TC)$OIL_FLOW(RUN,TC) = FLOW_IN_FSFuels('petr',C,TC)/OIL_FLOW(RUN,TC);

 EIntensity_FS('petr','ccoal-low',TC) = max(0.01,EIntensity_FS('petr','ccoal-low',TC));

 EIntensity_FS('am',C,TC)$FLOW_OUT_FSFuels('am','cammo',TC) = FLOW_IN_FSFuels('am',C,TC)/FLOW_OUT_FSFuels('am','cammo',TC);
 EIntensity_FS('is',C,TC) = FLOW_IN_FSFuels('is',C,TC)/(FLOW_OUT_FS('is','IISCRS',TC)+VARACT('REGION1',TC,'PEXGDRI'));

* BM!!! need to create QA_FSStart
* EIntensityQA_FS(FS,'chydr',TC)$FLOW_IN_FSFuels(FS,'chydr',TC) = FLOW_IN_FSFuels(FS,'chydr',TC)/QA_FS(FS,TC);


*BM+FH 201218: SETTING STARTING POINT FOR HYDROGEN
 ICAFSGR(C,FS,T)$(EIntensity_FS(FS,C,T-1) GT 0.00001) = EIntensity_FS(FS,C,T)/EIntensity_FS(FS,C,T-1);


 ICAFSGR(C,'nf',T) = min(1,ICAFSGR(C,'nf',T));

* ICAFSGR('cpetr_d','tranlp',T,TT) = 0.8;

*Also need to divide by sum of final demand rather than sum of generation to better accommodate storage and losses

* carbon tax resulting from cap (marginal)
*bm TCARBONTAX(XC,TC,TT) = T_COMBALEM('REGION1',TC,'CO2EQS')*(-0.791);

*Regulated Commodity price from TIMES ----------------------


* Electricity Price = (power plant costs + fuel costs)/total elc consumed excl. for storage
*Power Plant Costs (excluding fuel)


 REPORT(PRC,'ACTGRP',TC,RUN,'AnnInvCost') = TCST_INVC('REGION1',TC,PRC);
 REPORT(PRC,'ACTGRP',TC,RUN,'FOM') = TCST_FIXC('REGION1',TC,PRC);
 REPORT(PRC,'ACTGRP',TC,RUN,'VOM') = TCST_ACTC('REGION1',TC,PRC);
 REPORT(PRC,COM,TC,RUN,'FuelCosts') = TVFLOWIN(COM,TC,PRC)*T_COMBALEM('REGION1',TC,COM);
* bm!!! If there are fixed costs, they might not be taken into account in the marginal and so would have to be added to this calc

 REPORT(PRC,'ACTGRP',TC,RUN,'CO2Tax') = TVFLOWOUT('PWRCO2S',TC,PRC)*COM_CSTCO2('REGION1',TC,'CO2EQST');

 REPORT(PRC,'ACTGRP',TC,RUN,'Levies') = TVFLOWOUT('PWRENV',TC,PRC)*T_COMBALEM('REGION1',TC,'PWRENV')*(-1);

 REPORT(PRC,'ACTGRP',TC,RUN,'ExternalityCosts') = SUM(COMEXT,TVFLOWOUT(COMEXT,TC,PRC)*T_COMBALEM('REGION1',TC,COMEXT)*(-1));


* bm!!! would need to expand fuel cost calc if fixed costs or lower limits on coal production for power plants are set

*sum the components of elec costs calcs:
 TCST_ELE(RUN,TC) = sum(PRCPower,REPORT(PRCPower,'ACTGRP',TC,RUN,'AnnInvCost')+REPORT(PRCPower,'ACTGRP',TC,RUN,'FOM')+REPORT(PRCPower,'ACTGRP',TC,RUN,'VOM')+REPORT(PRCPower,'ACTGRP',TC,RUN,'FuelCosts')+REPORT(PRCPower,'ACTGRP',TC,RUN,'CO2Tax')+REPORT(PRCPower,'ACTGRP',TC,RUN,'Levies'));


* Elc Price
 ETPRICE(TC) = TCST_ELE(RUN,TC)/ELC_FLOW(RUN,TC);

 AggFixPrice('ELC',TC,RUN) = Sum(PRCBulkPower,(REPORT(PRCBulkPower,'ACTGRP',TC,RUN,'AnnInvCost')+REPORT(PRCBulkPower,'ACTGRP',TC,RUN,'FOM')))/TVCAP(TC,'ETRANS');
 AggVarCost('ELC',TC,RUN) = Sum(PRCBulkPower, (REPORT(PRCBulkPower,'ACTGRP',TC,RUN,'VOM')+REPORT(PRCBulkPower,'ACTGRP',TC,RUN,'FuelCosts')+REPORT(PRCBulkPower,'ACTGRP',TC,RUN,'CO2Tax')+REPORT(PRCBulkPower,'ACTGRP',TC,RUN,'Levies')));
 AggVarPrice('ELC',TC,RUN)$REPORT('ETRANS','ELC',TC,RUN,'FlowOut') = AggVarCost('ELC',TC,RUN)/REPORT('ETRANS','ELC',TC,RUN,'FlowOut');



*b add growth rates from TIMES from 2014 onward
 ETPRICEGR(TC)$(not T1(TC)) = ETPRICE(TC)/ETPRICE(TC-1)-1;


* Labour and capital intensity
 TCST_ELE_INV(RUN,TC) = Sum(PRCPower,REPORT(PRCPower,'ACTGRP',TC,RUN,'AnnInvCost'));
 QFEGY_GR(RUN,TC)$TCST_ELE_INV(RUN,TC-1) = TCST_ELE_INV(RUN,TC)/TCST_ELE_INV(RUN,TC-1);

 FIntensity_FS('elec','fegy',TC) = TCST_ELE_INV(RUN,TC)/ELC_FLOW(RUN,TC);
 FIntensity_FS('elec',FLAB,TC) = FLOW_IN_FSFactors('elec',FLAB,TC)/ELC_FLOW(RUN,TC);

 IFAFSGR(F,FS,TC)$FIntensity_FS(FS,F,TC-1) = FIntensity_FS(FS,F,TC)/FIntensity_FS(FS,F,TC-1);


* Electrolyser sector - supply of electrolysers is linked to electrolyser exports and PEM electrolyser capacity in SATIM
 ELCTCAP(TC,RUN) = sum(SubSector,INVCOSTSubSector(RUN,TC,'Hydrogen',SubSector))+TVACT(TC,'PEXHETP')*0.4;
* filtering new capacity to smooth things a bit
 ELCTCAPF(TC,RUN) = 0.3*ELCTCAP(TC,RUN)+(1-0.3)*ELCTCAPF(TC-1,RUN);

* fuelcell sector - supply of fuel cells is linked to fuel cell export and fuel cell capacity in trucks - !!! still need to add fcell to mining 5%?
* it works out in the CGE that with 0.24, 3 times value of trucks goes to export, which is what is intented in SATIM
 FCELLCAP(TC,RUN) = TVCAP(TC,'HFCP-TRAHCV-N')+TVACT(TC,'PEXHFCP')*0.2;
*0.24;


* Energy Model Indicators

 REPORT('SATIMMarginals',COM,TC,RUN,'Marginals') = T_COMBALEM('REGION1',TC,COM);

 REPORT(PRC,'ACTGRP',TC,RUN,'Activity') = TVACT(TC,PRC);
 REPORT(PRC,'ACTGRP',TC,RUN,'Capacity') = TVCAP(TC,PRC);
 REPORT(PRC,'ACTGRP',TC,RUN,'NewCapacity') = TVNCAP(TC,PRC);

 REPORT(PRC,'ACTGRP',TC,RUN,'CapFac')$(TVCAP(TC,PRC)*PRC_CAPACT('REGION1',PRC) gt 0.00001) = TVACT(TC,PRC)/(TVCAP(TC,PRC)*PRC_CAPACT('REGION1',PRC));

 REPORT(PRC,COM,TC,RUN,'FlowIn') = TVFLOWIN(COM,TC,PRC);
 REPORT(PRC,COM,TC,RUN,'FlowOut') = TVFLOWOUT(COM,TC,PRC);

 REPORT(PRC,'ACTGRP',TC,RUN,'Investment') = INVCOST(RUN,TC,PRC);

 REPORT('ETRANSDUM','ELC',TC,RUN,'Price') = ETPRICE(TC);


* Other indicators - this could be done more elegantly
* REPORT('hhd-le','ACTGRP',TC,RUN,'Population') = TFHPOP('hhd-le',TC)*POP(TC);
* REPORT('hhd-me','ACTGRP',TC,RUN,'Population') = TFHPOP('hhd-me',TC)*POP(TC);
* REPORT('hhd-he','ACTGRP',TC,RUN,'Population') = TFHPOP('hhd-he',TC)*POP(TC);

* pkm and tkm
 REPORT(PRC,'ACTGRP',TC,RUN,'pkm')$PassengerOccupancy(PRC) = TVACT(TC,PRC)*PassengerOccupancy(PRC);
 REPORT(PRC,'ACTGRP',TC,RUN,'tkm')$FreightLoad(PRC) = TVACT(TC,PRC)*FreightLoad(PRC);

* mtons of coal
 REPORT(PRC,COM,TC,RUN,'FlowInMt')$CoalCV(COM) = TVFLOWIN(COM,TC,PRC)/CoalCV(COM);


* Exception for biofuels in transport
* Note that this is not perfect in that it doesn't take into account possible upstream emissions from production
* it would be better in future iterations to have a sink linked to the supply of bioenergy
EmisFactorCor('TRAODS','CO2',TC,RUN)$(REPORT('XTRAODS','TRAODS',TC,RUN,'FlowOut')+REPORT('XTRABID','TRAODS',TC,RUN,'FlowOut')) =
         EmisFactor('TRAODS','CO2')*REPORT('XTRAODS','TRAODS',TC,RUN,'FlowOut')/(REPORT('XTRAODS','TRAODS',TC,RUN,'FlowOut')+REPORT('XTRABID','TRAODS',TC,RUN,'FlowOut'))+eps;

EmisFactorCor('TRAOGS','CO2',TC,RUN)$(REPORT('XTRAOGS','TRAOGS',TC,RUN,'FlowOut')+REPORT('XTRABIE','TRAOGS',TC,RUN,'FlowOut')) =
         EmisFactor('TRAOGS','CO2')*REPORT('XTRAOGS','TRAOGS',TC,RUN,'FlowOut')/(REPORT('XTRAOGS','TRAOGS',TC,RUN,'FlowOut')+REPORT('XTRABIE','TRAOGS',TC,RUN,'FlowOut'))+eps;





$ontext
* adding small Entry for coal IPP because of issues in Tableau
 REPORT('ETCLDFB-N','ACTGRP',TC,RUN,Indicators) = 0.00001;
 REPORT('ETCLDFB-N','ELCC',TC,RUN,'FlowOut') = 0.00001;
 REPORT('ETCLDFB-N','PWRCMOX',TC,RUN,'FlowOut') = 0.00001;
 REPORT('ETCLDFB-N','PWRCO2S',TC,RUN,'FlowOut') = 0.00001;
 REPORT('ETCLDFB-N','PWRENV',TC,RUN,'FlowOut') = 0.00001;
 REPORT('ETCLDFB-N','PWRN2OS',TC,RUN,'FlowOut') = 0.00001;
 REPORT('ETCLDFB-N','PWRNMVS',TC,RUN,'FlowOut') = 0.00001;
 REPORT('ETCLDFB-N','PWRNOXS',TC,RUN,'FlowOut') = 0.00001;
 REPORT('ETCLDFB-N','PWRSOXS',TC,RUN,'FlowOut') = 0.00001;
 REPORT('ETCLDFB-N','PWRWAT',TC,RUN,'FlowIn') = 0.00001;
 REPORT('ETCLDFB-N','PWRCLD',TC,RUN,'FlowIn') = 0.00001;
 REPORT('ETCLDFB-N','PWRCLD',TC,RUN,'FlowInMt') = 0.00001;
 REPORT('ETCLDFB-N','PWRCLD',TC,RUN,'CO2') = 0.00001;
 REPORT('ETCLDFB-N','PWRCLD',TC,RUN,'N2O') = 0.00001;
$offtext
