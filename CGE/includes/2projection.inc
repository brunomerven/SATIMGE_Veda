PARAMETER
*Excel-inputted growth rates
 TFPPROJ(A,RD,X,T)       sectoral TFP growth projections
 FPRDPROJ(F,X,T)         factor-specific productivity growth projections
 FACPROJ(F,X,T)          factor supply growth projections
 WPROJ(F,X,T)            factor return growth projections
 AFXPROJ(A,RD,X,T)       growth rate for sectors with fixed resources
 MISCPROJ(MISCAC,X,T)    miscellaneous growth projections
 POPPROJ(H,X,T)          household population growth projections
*Excel-inputted acceleration or decelaration rates
 STFPAD(A,RD,X)          sectoral TFP acceleration or decelaration rate
 SFACAD(F,X)             factor supply acceleration or decelaration rate
*fh
 SFACAD2(F,X)          additional factor supply acceleration or decelaration rate
 SWAD(F,X)               factor return acceleration or decelaration rate
 SFPRDAD(F,X)            factor-specific productivity acceleration or decelaration rate
 SAFXAD(A,RD,X)          acceleration or decelaration rate for sectors with fixed resources
 SMISCAD(MISCAC,X)       miscellaneous acceleration or decelaration rate
*fh
 SMISCAD2(MISCAC,X)      additional miscellaneous acceleration or decelaration rate
 SPOPAD(H,X)             household population acceleration or decelaration rate
 TFPAD(A,RD,X,T)         sectoral TFP acceleration or decelaration rates
 FACAD(F,X,T)            factor supply acceleration or decelaration rates
*fh
 FACAD2(F,X,T)           additional factor supply acceleration or decelaration rates
 WAD(F,X,T)              factor return acceleration or decelaration rates
 FPRDAD(F,X,T)           factor-specific productivity acceleration or decelaration rates
 AFXAD(A,RD,X,T)         acceleration or decelaration rates for sectors with fixed resources
 MISCAD(MISCAC,X,T)      miscellaneous acceleration or decelaration rates
*fh
 MISCAD2(MISCAC,X,T)     additional miscellaneous acceleration or decelaration rates
 POPAD(H,X,T)            household population acceleration or decelaration rates
*Final simulation growth rates
 AFXGR(A,RD,X,T)         growth rate for sectors with fixed resources
 FACGR(F,X,T)            factor supply growth rate
 WGR(F,X,T)              factor return growth rate
 TFPGR(A,RD,X,T)         total factor productivity growth rate
 FPRDGR(F,X,T)           factor-specific productivity growth rate
 GOVGR(X,T)              government consumption expenditure growth
 FSAVGR(X,T)             foreign savings growth rate (positive if increasing)
 GSAVGR(X,T)             government savings growth rate (positive if increasing)
* PWEGR(C,X,T)            world export price changes
* PWMGR(C,X,T)            world import price changes
 PWEGR(C,RUN,T)            world export price changes
 PWMGR(C,RUN,T)            world import price changes
 TRNSFRGR(INS,AC,X,T)    exogenous institutional transfer changes
 POPGR(H,X,T)            household population growth projections
 MPSGR(X,T)              private savings growth rate
 DTAXGR(X,T)             direct tax rate on enterprises and households
;
$load TFPPROJ STFPAD
$loaddc FPRDPROJ FACPROJ MISCPROJ SFACAD SFPRDAD SMISCAD SPOPAD WPROJ SWAD SMISCAD2 SFACAD2
*AFXPROJ SAFXAD
$gdxin

SET
 SAGR(X,T)               fixed capital demand growth rates
 SFAC(X,T)               sector factor demand growth projections
 SW(X,T)                 sector factor supply growth projections
 STFP(X,T)               sectoral TFP growth projections
 SFPRD(X,T)              factor-specific productivity growth projections
 SMISC(X,T)              miscellaneous growth projections
 SPOP(X,T)               household population growth projections

 TPAST(T)                historical years
;

 POPPROJ(H,X,TC) = POP_GR(TC);

*JT: AE paper (changed this code from no's to yes's)
 SAGR(X,T)   = yes;
 SFAC(X,T)   = yes;
 SW(X,T)     = yes;
 STFP(X,T)   = yes;
 SFPRD(X,T)  = yes;
 SMISC(X,T)  = yes;
 SPOP(X,T)   = yes;

 TPAST('2012') = yes;
 TPAST('2013') = yes;
 TPAST('2014') = yes;
 TPAST('2015') = yes;
 TPAST('2016') = yes;
 TPAST('2017') = yes;

LOOP(TC$(NOT T1(TC)),
* SAGR(X,TC)$(NOT SAGR(X,TC-1) AND SUM((A,RD), AFXPROJ(A,RD,X,TC))) = YES;
* SAGR(X,TC)$SAGR(X,TC-1) = YES;
 SFAC(X,TC)$(NOT SFAC(X,TC-1) AND SUM(F, FACPROJ(F,X,TC))) = YES;
 SFAC(X,TC)$SFAC(X,TC-1) = YES;
 SW(X,TC)$(NOT SW(X,TC-1) AND SUM(F, WPROJ(F,X,TC))) = YES;
 SW(X,TC)$SW(X,TC-1) = YES;
 STFP(X,TC)$(NOT STFP(X,TC-1) AND SUM((A,RD), TFPPROJ(A,RD,X,TC))) = YES;
 STFP(X,TC)$STFP(X,TC-1) = YES;
 SFPRD(X,TC)$(NOT SFPRD(X,TC-1) AND SUM(F, FPRDPROJ(F,X,TC))) = YES;
 SFPRD(X,TC)$SFPRD(X,TC-1) = YES;
 SMISC(X,TC)$(NOT SMISC(X,TC-1) AND SUM(MISCAC, MISCPROJ(MISCAC,X,TC))) = YES;
 SMISC(X,TC)$SMISC(X,TC-1) = YES;
 SPOP(X,TC)$(NOT SPOP(X,TC-1) AND SUM(H, POPPROJ(H,X,TC))) = YES;
 SPOP(X,TC)$SPOP(X,TC-1) = YES;
);

LOOP(TC$(NOT T1(TC)),

*Sector factor demand growth projections
*bm done by a time series in simulationy.xlsx
* AFXPROJ(A,RD,'BASE',TC)$(AFXPROJ(A,RD,'BASE',TC)) = AFXPROJ(A,RD,'BASE',TC);
* AFXPROJ(A,RD,'BASE',TC)$(NOT AFXPROJ(A,RD,'BASE',TC)) = AFXPROJ(A,RD,'BASE',TC-1);
* AFXPROJ(A,RD,XNB,TC)$(AFXPROJ(A,RD,XNB,TC)) = AFXPROJ(A,RD,XNB,TC);
* AFXPROJ(A,RD,XNB,TC)$(NOT AFXPROJ(A,RD,XNB,TC) AND NOT SFAC(XNB,TC)) = AFXPROJ(A,RD,'BASE',TC);
* AFXPROJ(A,RD,XNB,TC)$(NOT AFXPROJ(A,RD,XNB,TC) AND SFAC(XNB,TC)) = AFXPROJ(A,RD,XNB,TC-1);

*Factor supply growth projections
 FACPROJ(F,'BASE',TC)$FACPROJ(F,'BASE',TC) = FACPROJ(F,'BASE',TC);
 FACPROJ(F,'BASE',TC)$(NOT FACPROJ(F,'BASE',TC)) = FACPROJ(F,'BASE',TC-1);
 FACPROJ(F,XNB,TC)$FACPROJ(F,XNB,TC) = FACPROJ(F,XNB,TC);
 FACPROJ(F,XNB,TC)$(NOT FACPROJ(F,XNB,TC) AND NOT SFAC(XNB,TC)) = FACPROJ(F,'BASE',TC);
 FACPROJ(F,XNB,TC)$(NOT FACPROJ(F,XNB,TC) AND SFAC(XNB,TC)) = FACPROJ(F,XNB,TC-1);

*Factor return growth projections
 WPROJ(F,'BASE',TC)$WPROJ(F,'BASE',TC) = WPROJ(F,'BASE',TC);
 WPROJ(F,'BASE',TC)$(NOT WPROJ(F,'BASE',TC)) = WPROJ(F,'BASE',TC-1);
 WPROJ(F,XNB,TC)$WPROJ(F,XNB,TC) = WPROJ(F,XNB,TC);
 WPROJ(F,XNB,TC)$(NOT WPROJ(F,XNB,TC) AND NOT SW(XNB,TC)) = WPROJ(F,'BASE',TC);
 WPROJ(F,XNB,TC)$(NOT WPROJ(F,XNB,TC) AND SW(XNB,TC)) = WPROJ(F,XNB,TC-1);

*Sectoral TFP growth projections
 TFPPROJ(A,RD,'BASE',TC)$TFPPROJ(A,RD,'BASE',TC) = TFPPROJ(A,RD,'BASE',TC);
 TFPPROJ(A,RD,'BASE',TC)$(NOT TFPPROJ(A,RD,'BASE',TC)) = TFPPROJ(A,RD,'BASE',TC-1);
 TFPPROJ(A,RD,XNB,TC)$TFPPROJ(A,RD,XNB,TC) = TFPPROJ(A,RD,XNB,TC);
 TFPPROJ(A,RD,XNB,TC)$(NOT TFPPROJ(A,RD,XNB,TC) AND NOT STFP(XNB,TC)) = TFPPROJ(A,RD,'BASE',TC);
 TFPPROJ(A,RD,XNB,TC)$(NOT TFPPROJ(A,RD,XNB,TC) AND STFP(XNB,TC)) = TFPPROJ(A,RD,XNB,TC-1);

*Factor-specific productivity growth projections
 FPRDPROJ(F,'BASE',TC)$FPRDPROJ(F,'BASE',TC) = FPRDPROJ(F,'BASE',TC);
 FPRDPROJ(F,'BASE',TC)$(NOT FPRDPROJ(F,'BASE',TC)) = FPRDPROJ(F,'BASE',TC-1);
 FPRDPROJ(F,XNB,TC)$FPRDPROJ(F,XNB,TC) = FPRDPROJ(F,XNB,TC);
 FPRDPROJ(F,XNB,TC)$(NOT FPRDPROJ(F,XNB,TC) AND NOT SFPRD(XNB,TC)) = FPRDPROJ(F,'BASE',TC);
 FPRDPROJ(F,XNB,TC)$(NOT FPRDPROJ(F,XNB,TC) AND SFPRD(XNB,TC)) = FPRDPROJ(F,XNB,TC-1);

*Miscellaneous growth projections
 MISCPROJ(MISCAC,'BASE',TC)$MISCPROJ(MISCAC,'BASE',TC) = MISCPROJ(MISCAC,'BASE',TC);
 MISCPROJ(MISCAC,'BASE',TC)$(NOT MISCPROJ(MISCAC,'BASE',TC)) = MISCPROJ(MISCAC,'BASE',TC-1);
 MISCPROJ(MISCAC,XNB,TC)$MISCPROJ(MISCAC,XNB,TC) = MISCPROJ(MISCAC,XNB,TC);
 MISCPROJ(MISCAC,XNB,TC)$(NOT MISCPROJ(MISCAC,XNB,TC) AND NOT SMISC(XNB,TC)) = MISCPROJ(MISCAC,'BASE',TC);
 MISCPROJ(MISCAC,XNB,TC)$(NOT MISCPROJ(MISCAC,XNB,TC) AND SMISC(XNB,TC)) = MISCPROJ(MISCAC,XNB,TC-1);

*Population growth projections
 POPPROJ(H,'BASE',TC)$POPPROJ(H,'BASE',TC) = POPPROJ(H,'BASE',TC);
 POPPROJ(H,'BASE',TC)$(NOT POPPROJ(H,'BASE',TC)) = POPPROJ(H,'BASE',TC-1);
 POPPROJ(H,XNB,TC)$POPPROJ(H,XNB,TC) = POPPROJ(H,XNB,TC);
 POPPROJ(H,XNB,TC)$(NOT POPPROJ(H,XNB,TC) AND NOT SMISC(XNB,TC)) = POPPROJ(H,'BASE',TC);
 POPPROJ(H,XNB,TC)$(NOT POPPROJ(H,XNB,TC) AND SMISC(XNB,TC)) = POPPROJ(H,XNB,TC-1);

);

*Initialize acceleration or deceleration rates
 TFPAD(A,RD,XC,TC)    = 0;
 FACAD(F,XC,TC)       = 0;
*
 FACAD2(F,XC,TC)      = 0;
 WAD(F,XC,TC)         = 0;
 FPRDAD(F,XC,TC)      = 0;
 MISCAD(MISCAC,XC,TC) = 0;
*fh
 MISCAD2(MISCAC,XC,TC) = 0;
 POPAD(H,XC,TC)       = 0;




*Accumulate annual acceleration or deceleration rates over simulation period
*LOOP(TC$(NOT T1(TC)),
LOOP(TC$(NOT TPAST(TC)),
* TFPAD(A,RD,XC,TC)$STFP(XC,TC-1)     = TFPAD(A,RD,XC,TC-1) + STFPAD(A,RD,XC);
*FH
 TFPAD(A,RD,XC,TC)$STFP(XC,TC-1)     = TFPAD(A,RD,XC,TC-1) + STFPAD(A,RD,XC);
* FACAD(F,XC,TC)$SFAC(XC,TC-1)        = FACAD(F,XC,TC-1) + SFACAD(F,XC);
 FACAD(F,XC,TC)$SFAC(XC,TC-1)        = FACAD(F,XC,TC-1) + SFACAD(F,XC);
*fh
 FACAD2(F,XC,TCF)$SFAC(XC,TCF-1)     = FACAD2(F,XC,TCF-1) + SFACAD2(F,XC);
 WAD(F,XC,TC)$SW(XC,TC-1)            = WAD(F,XC,TC-1) + SWAD(F,XC);
 FPRDAD(F,XC,TC)$SFPRD(XC,TC-1)      = FPRDAD(F,XC,TC-1) + SFPRDAD(F,XC);
 MISCAD(MISCAC,XC,TC)$SMISC(XC,TC-1) = MISCAD(MISCAC,XC,TC-1) + SMISCAD(MISCAC,XC);
 MISCAD2(MISCAC,XC,TCF)$SMISC(XC,TCF-1)= MISCAD2(MISCAC,XC,TCF-1) + SMISCAD2(MISCAC,XC);
 POPAD(H,XC,TC)$SPOP(XC,TC-1)        = POPAD(H,XC,TC-1) + SPOPAD(H,XC);
);

*FH
 Parameter
 DUMTIME(T);

* DUMTIME(T) = 0;
 DUMTIME(T) = 1;
 DUMTIME('2013') = 0;
 DUMTIME('2014') = 0;
 DUMTIME('2015') = 0;
 DUMTIME('2016') = 0;
 DUMTIME('2017') = 0;
 DUMTIME('2018') = 0;
 DUMTIME('2019') = 0;
 DUMTIME('2020') = 0;
 DUMTIME('2021') = 0;
 DUMTIME('2022') = 0;
 DUMTIME('2023') = 0;
 DUMTIME('2024') = 0;
 DUMTIME('2025') = 0;
 DUMTIME('2026') = 0;
 DUMTIME('2027') = 0;
 DUMTIME('2028') = 0;
 DUMTIME('2029') = 0;
 DUMTIME('2030') = 0;


*Add cumulative acceleration or deceleration rates to annual long-term growth rates
*FH: Change deceleration for IFPRI and COBEN
* TFPPROJ(A,RD,XC,TC)    = TFPPROJ(A,RD,XC,TC)    + TFPAD(A,RD,XC,TC);
 TFPPROJ(A,RD,XC,TC)    = TFPPROJ(A,RD,XC,TC)    + TFPAD(A,RD,XC,TC)*DUMTIME(TC);
 FPRDPROJ(F,XC,TC)      = FPRDPROJ(F,XC,TC)      + FPRDAD(F,XC,TC);
 FACPROJ(F,XC,TC)       = FACPROJ(F,XC,TC)       + FACAD(F,XC,TC)+ FACAD2(F,XC,TC);
 WPROJ(F,XC,TC)         = WPROJ(F,XC,TC)         + WAD(F,XC,TC);
 MISCPROJ(MISCAC,XC,TC) = MISCPROJ(MISCAC,XC,TC) + MISCAD(MISCAC,XC,TC)+ MISCAD2(MISCAC,XC,TC);
 POPPROJ(H,XC,TC)       = POPPROJ(H,XC,TC)       + POPAD(H,XC,TC);

* Final simulation growth rates


*Total factor productivity changes
 TFPGR(A,RD,XC,TC)       = TFPPROJ(A,RD,XC,TC)/100;

*Embodied factor productivity growth rate
 FPRDGR(F,XC,TC)         = FPRDPROJ(F,XC,TC)/100;

*Exogenous factor supply growth rate
 FACGR(F,XC,TC)          = FACPROJ(F,XC,TC)/100;

*Exogenous factor return growth rate
 WGR(F,XC,TC)            = WPROJ(F,XC,TC)/100;

*Institutional transfers (recipient comes first)
 TRNSFRGR(H,'GOV',XC,TC) = MISCPROJ('GTRH',XC,TC)/100;
 TRNSFRGR(H,'ROW',XC,TC) = MISCPROJ('HREM',XC,TC)/100;

*Government expenditure items
 GOVGR(XC,TC)            = MISCPROJ('QG',XC,TC)/100;

*Foreign capital inflow changes (current account balance)
 FSAVGR(XC,TC)           = MISCPROJ('FSAV',XC,TC)/100;
 GSAVGR(XC,TC)           = MISCPROJ('GSAV',XC,TC)/100;

*World price changes
*FH: REMOVED CODE AND RESET TO ORIGINAL CODE
* PWMGR(C,XC,TC)          = PWGR(C,XC,TC);
* PWEGR(C,XC,TC)          = PWGR(C,XC,TC);
 PWMGR(C,RUN,TC)          = PWGR(C,RUN,TC);
 PWEGR(C,RUN,TC)          = PWGR(C,RUN,TC);
* PWMGR(C,XC,TC)          = MISCPROJ('PWM',XC,TC)/100;
* PWEGR(C,XC,TC)          = MISCPROJ('PWE',XC,TC)/100;
*FH: including coal
* PWEGR('ccoal-hgh',XC,TC) = PWGR('ccoal-hgh',XC,TC);

*Growth of fixed factor sectors. Convert to fraction from percent.
* AFXGR(A,RD,XC,TC)$AFX(A,RD) = AFXPROJ(A,RD,XC,TC)/100;

*Population growth
 POPGR(H,XC,TC)          = POPPROJ(H,XC,TC);

*Private savings growth
 MPSGR(XC,TC)            = MISCPROJ('MPS',XC,TC)/100;

*Direct tax rate
 DTAXGR(XC,TC)           = MISCPROJ('DTAX',XC,TC)/100;

*All base year growth rates must be zero ----------------------
 TFPGR(A,RD,X,T1)      = 0;
 FPRDGR(F,X,T1)        = 0;
 FACGR(F,X,T1)         = 0;
 WGR(F,X,T1)           = 0;
 GOVGR(X,T1)           = 0;
 FSAVGR(X,T1)          = 0;
 GSAVGR(X,T1)          = 0;
* PWEGR(C,X,T1)         = 0;
* PWMGR(C,X,T1)         = 0;
 PWGR(C,RUN,T1)            =0;
 TRNSFRGR(INS,AC,X,T1) = 0;
 AFXGR(A,RD,X,T1)      = 0;
 MPSGR(X,T1)           = 0;
 DTAXGR(X,T1)          = 0;

