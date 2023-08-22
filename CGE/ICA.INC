*This include file creates the starting variables and parameters needed for updating the
*electricity and petroleum tech vectors to account for changes in the technology mix
*-------------------------------------------------------------------------------
*1. DEFINING SETS, PARAMATERS AND DATA
*-------------------------------------------------------------------------------
Set
 TECH            ELECTRICITY AND REFINERY TECHNOLOGIES
 /aelc_coal, aelc_gas, aelc_hyd, aelc_nuc, aelc_oil, aelc_pst,
         aelc_win, aelc_bio, aelc_pv, aelc_csp, aelc_geo, aref_oil, aref_ctl, aref_gtl/
 NTECH(TECH)     ELECTRICITY AND REFINERY TECHNOLOGIES
 /aelc_win, aelc_pv, aelc_csp, aelc_geo/
 CTECH(C)        COMMODITIES SPECIFIC TO TECHNOLOGY TYPES
 /cawhe,camai,caoce,caveg,caofr,cagra,caoil,catub,capul,casug,caoth,clani,
 ccoal-low, cbchm, cngas, cpetr-d/
;

ALIAS (TECH,TECH0);

Parameter
 WAGEVAL(FLAB,A)
 ICA_0(C,TECH)
 ICA_0_CHECK(AC)
 P_IFA_0(FLAB,TECH)
 P1_IFA_0(TECH)
 IFA_0(FLAB,TECH)
 IFA_0_CHECK(FLAB)
;

Table
 QETECHTAB(TECH,*)          OUTPUT IN PJ
               VAL
 aelc_coal     707.49
*708.94
 aelc_gas        1.18
 aelc_hyd        9.29
 aelc_nuc       41.89
 aelc_oil        3.42
*BRUNO TO CHECK! NB!!!
*INCLUDING A SMALL NUMBER FOR BIOMASS FOR CALCULATION PURPOSES - REMOVING FROM CCOAL
*NUMBERS TAKEN FROM EXCEL EB PRODUCTION. NOTE SATIM VIZ NUMBERS DIFFER FROM THIS.
 aelc_win           0
*.21
 aelc_bio        1.24
*imaginary number given
 aelc_pv            0
 aelc_csp           0
 aelc_geo           0
;

Table
 LABTECHTAB(TECH,*)        EMPLOYMENT PER GWH (EXCLUDES TRANSMISSION AND DISTRIBUTION)
               VAL
 aelc_coal     0.11
 aelc_gas      0.11
 aelc_hyd      0.01
 aelc_nuc      0.14
 aelc_oil      0.11
 aelc_win      0.17
 aelc_bio      0.21
 aelc_pv       0.23
 aelc_csp      0.23
 aelc_geo      0.17
;

Table
 QPTECHTAB(TECH,*)          OUTPUT IN PJ
               VAL
 aref_oil      855.50
 aref_ctl      198.47
 aref_gtl       29.70
;

Table
 FLABSKTAB(TECH,FLAB)     SPLIT OF GVA BY TECHNOLOGY (TAKEN FROM KALIE PAUW)
                 flab-p          flab-m          flab-s          flab-t
aelc_coal          0.0203          0.0310          0.1277          0.8210
aelc_gas           0.0140          0.0165          0.0948          0.8747
aelc_hyd           0.0132          0.0267          0.1533          0.8068
aelc_nuc           0.0247          0.0198          0.0884          0.8671
aelc_oil           0.0140          0.0165          0.0948          0.8747
aelc_win           0.0171          0.0184          0.0945          0.8700
aelc_bio           0.0176          0.0198          0.1022          0.8604
aelc_pv            0.0140          0.0165          0.0948          0.8747
aelc_csp           0.0140          0.0165          0.0948          0.8747
aelc_geo           0.0171          0.0184          0.0945          0.8700
;

*-------------------------------------------------------------------------------
*2. ELECTRICITY
*-------------------------------------------------------------------------------
*2.A. CALCULATING ICA0 BY TECHNOLOGY
*-------------------------------------------------------------------------------
 ICA_0(C,TECH) $ (QETECHTAB(TECH,'VAL') AND NOT CTECH(C)) = ((QETECHTAB(TECH,'VAL')/SUM(TECH0,QETECHTAB(TECH0,'VAL')))*SAM(C,'AELEC'))
                                                                 /QETECHTAB(TECH,'VAL');

 ICA_0(C,TECH)$NTECH(TECH)=ICA_0(C,'AELC_WIN');

 ICA_0('CCOAL-LOW','AELC_COAL') = SAM('CCOAL-LOW','AELEC')/QETECHTAB('AELC_COAL','VAL');
 ICA_0('CNGAS','AELC_GAS')      = SAM('CNGAS','AELEC')    /QETECHTAB('AELC_GAS' ,'VAL');
 ICA_0('CBCHM','AELC_NUC')      = SAM('CBCHM','AELEC')    /QETECHTAB('AELC_NUC' ,'VAL');
 ICA_0('CPETR-D','AELC_OIL')    = SAM('CPETR-D','AELEC')  /QETECHTAB('AELC_OIL' ,'VAL');
 ICA_0('CASUG','AELC_BIO')      = SAM('CASUG','AELEC')    /QETECHTAB('AELC_BIO' ,'VAL');

*check
 ICA_0_CHECK(C)=SUM(TECH $(NOT NTECH(TECH)),ICA_0(C,TECH)*QETECHTAB(TECH,'VAL'))-SAM(C,'AELEC');

*-------------------------------------------------------------------------------
*2.B. CALCULATING IFA0 BY TECHNOLOGY
*-------------------------------------------------------------------------------
PARAMETER
 FACADJEMP
 EMP(TECH)
 LABTECHTAB2(TECH,*)
 WAGEBILL(TECH)
 WAGEBILL2(FLAB,TECH)

 EMP_CHECK
 WAGEBILL2_CHECK(FLAB)

 AWAGE(A)
 EMP2(FLAB,TECH)
;

 LABTECHTAB(TECH,'VAL')=LABTECHTAB(TECH,'VAL')*277.7778;
 EMP(TECH)=(LABTECHTAB(TECH,'VAL')*QETECHTAB(TECH,'VAL'))/1000;
 FACADJEMP=((SUM(FLAB,QF.L(FLAB,'AELEC','NAT'))-SUM(TECH,EMP(TECH)))*1000)/SUM(TECH,QETECHTAB(TECH,'VAL'));
 LABTECHTAB2(TECH,'VAL')=LABTECHTAB(TECH,'VAL')+FACADJEMP;
 EMP(TECH)$(NOT NTECH(TECH))=(LABTECHTAB2(TECH,'VAL')*QETECHTAB(TECH,'VAL'))/1000;
 EMP(TECH)$(NTECH(TECH))=(LABTECHTAB2(TECH,'VAL')*1)/1000;

 EMP_CHECK=SUM(TECH$(NOT NTECH(TECH)),EMP(TECH))-SUM(FLAB,QF.L(FLAB,'AELEC','NAT'));

 AWAGE('AELEC')=SUM(FLAB,SAM(FLAB,'AELEC'))/SUM(FLAB,QF.L(FLAB,'AELEC','NAT'));
 WAGEBILL(TECH)=EMP(TECH)*AWAGE('AELEC');
 WAGEBILL2(FLAB,TECH)= WAGEBILL(TECH)* FLABSKTAB(TECH,FLAB);

 WAGEBILL2_CHECK(FLAB) = SUM(TECH$(NOT NTECH(TECH)),WAGEBILL2(FLAB,TECH))-SAM(FLAB,'AELEC');

 EMP2(FLAB,TECH)=WAGEBILL2(FLAB,TECH)/(WF.L(FLAB)*WFDIST.L(FLAB,'AELEC','NAT'));

 IFA_0(FLAB,TECH)$(NOT NTECH(TECH) AND QETECHTAB(TECH,'VAL'))=EMP2(FLAB,TECH)/QVA.L('AELEC','NAT');
 IFA_0(FLAB,TECH)$(NTECH(TECH))=EMP2(FLAB,TECH);

 IFA_0_CHECK(FLAB) = SUM(TECH$(NOT NTECH(TECH)),IFA_0(FLAB,TECH)*QVA.L('AELEC','NAT'))
                 -QF.L(FLAB,'AELEC','NAT');

*-------------------------------------------------------------------------------
*2.C. CALCULATING IVA0 BY TECHNOLOGY
*-------------------------------------------------------------------------------
