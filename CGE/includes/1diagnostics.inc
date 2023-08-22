SET
 SIREC(AC)               set for elements permitted to receive payment from S-I
 NEGCELL(AC,ACP)         the SAM payment from ACP (column) to AC (row) is negative
 LARGECELL(AC,ACP)       cells with absolute values larger than cellcutoffup
 SMALLCELL(AC,ACP)       cells with absolute values smaller than cellcutofflo
 SHRITEM                 items for which shares and cutoffs are defined /
  FAC                    factor in activity
  VA                     value-added
  EXP                    exports
  IMP                    imports
 /
;

PARAMETER
 ERRSAMFLOW(AC,ACP)      if UNDF -- illegal SAM payment from 2nd index to 1st
 ERREXPOUT(C)            if UNDF -- exports exceed domestic output for comm c
 SAMGAPCUTOFF            max acceptable abs gap bt model SAM row and col totals
 ERRSAMBAL(AC)           if UNDF -- the absolute imbalance for AC exceeds cutoff
 MAXCELL                 maximum absolute cell value in SAM (excluding totals)
 MINCELL                 minimum absolute non-zero cell value in SAM
 cellcutoffup            absolute cell values above cutoff are displayed as large
 cellcutofflo            absolute cell values below cutoff are displayed as small
 ACTFACSHR(F,ARD)        small share of factor f in total value-added of activity a
 ACTVASHR(AC)            small share of activity a in total economywide value-added
 EXPSHR(C)               small share of exports in total output of commodity c
 IMPSHR(C)               small share of imports in total domestic demand for commodity c
 SHRCUTOFF(SHRITEM)      upper limit for shares values that are displayed
 ERRHOME(A,H)            act with home cons comm shares not summing to one
 ERRQFBASE1(F,ARD)       demand specified for factor but no SAM payment
 ERRQFBASE2(F,ARD)       demands specified but missing for f-a combination in SAM
 ERRTRADE(C,AELAS)       commodities with missing trade elas
 ERRPROD1(ARD)           activities with missing factor substitut elas
 ERRPROD2(ARD)           CES activities with missing agg va - intermed elas
 ERRAC(C)                commodities with missing domestic aggregation elas
 ERRLES1(C,H)            marketed commodities with missing hhd cons elas
 ERRLES2(A,C,H)          home comm's with shrhome but missing hhd cons elas
;

 SIREC(C)      = YES;
 SIREC('DSTK') = YES;

*ILLEGAL SAM PAYMENTS OR VALUES -------------------------------

*Savings-investment (S-I) account is only permitted to pay to accounts for commodities (C) and stock change (DSTK)
 ERRSAMFLOW(ACNT,'S-I')$(SAM(ACNT,'S-I') AND (NOT SIREC(ACNT))) = 1/0;
*Savings-investment (S-I) account is only permitted to receive payments from institutions (INS)
 ERRSAMFLOW('S-I',ACNT)$(SAM('S-I',ACNT) AND (NOT INS(ACNT))) = 1/0;
*Stock change account (DSTK) is only permitted to receive payments from savings-investment (S-I) and make payments to commodities (C).
 ERRSAMFLOW('DSTK',ACNT)$(SAM('DSTK',ACNT) AND (NOT ACNT('S-I'))) = 1/0;
 ERRSAMFLOW(ACNT,'DSTK')$(SAM(ACNT,'DSTK') AND (NOT C(ACNT))) = 1/0;
*It is illegal to have import tariffs for commodities without imports
 ERRSAMFLOW('IMPTAX',C)$(TAXPAR('IMPTAX',C) AND (NOT SAM('ROW',C))) = 1/0;
*It is illegal to have export taxes for commodities without exports
 ERRSAMFLOW('EXPTAX',C)$(TAXPAR('EXPTAX',C) AND (NOT SAM(C,'ROW'))) = 1/0;
*Value of exports is greater than the value of domestic production.
*Note that exports can equal to value of domestic production, so test includes 1E-6.
 ERREXPOUT(C)$(SAM(C,'ROW') GT (SUM(ARD, SAM(ARD,C)) + TAXPAR('EXPTAX',C) + SUM(CTE, SAM(CTE,C)) + 1.E-6)) = 1/0 ;

*Activities (A) are not permitted to pay to institutions (INS) or other activities (AP).
 ERRSAMFLOW(INS,A)$SAM(INS,A) = 1/0;
 ERRSAMFLOW(AP,A)$SAM(AP,A) = 1/0;
*Commodities (C) are not permitted to pay to factors (F) or domestic institutions (INSD).
 ERRSAMFLOW(F,C)$SAM(F,C) = 1/0;
 ERRSAMFLOW(INSD,C)$SAM(INSD,C) = 1/0;
*Factors (F) are not permitted to pay to activities (A), commodities (C) or other factors (FP).
 ERRSAMFLOW(A,F)$SAM(A,F) = 1/0;
 ERRSAMFLOW(C,F)$SAM(C,F) = 1/0;
 ERRSAMFLOW(FP,F)$SAM(FP,F) = 1/0;
*Domestic institutions (INSD) are not permitted to pay to factors (F).
 ERRSAMFLOW(F,INSD)$SAM(F,INSD) = 1/0;
*Government (GOV) and the rest of the world (ROW) are not permitted to pay to activities (A).
 ERRSAMFLOW(A,'GOV')$SAM(A,'GOV') = 1/0;
 ERRSAMFLOW(A,'ROW')$SAM(A,'ROW') = 1/0;
*Enterprises (EN) are not permitted to pay to activities (A) or commodities (C).
 ERRSAMFLOW(A,EN)$SAM(A,EN) = 1/0;
 ERRSAMFLOW(C,EN)$SAM(C,EN) = 1/0;

*SAMBALCHK ----------------------------------------------------
 SAMGAPCUTOFF = 1.0e-5;
* ERRSAMBAL(AC)$(ABS(SAMBALCHK(AC)) GT SAMGAPCUTOFF) = 1/0;
 ERRSAMBAL(AC)$(ABS(SAMBALCHK(AC)) GT SAMGAPCUTOFF) = SAMBALCHK(AC);
 display$sum(ac,ERRSAMBAL(AC)) '*#* error in diagnostics', ERRSAMBAL;

*NEGCELL ------------------------------------------------------

 NEGCELL(AC,ACP)$(SAM(AC,ACP) LT 0) = YES;

*MAXCELL, MINCELL, LARGECELL AND SMALLCELL --------------------

 MAXCELL = SMAX((ACNT,ACNTP), ABS(SAM(ACNT,ACNTP)));
 MINCELL = SMIN((ACNT,ACNTP)$SAM(ACNT,ACNTP), ABS(SAM(ACNT,ACNTP)));

*No cells are large if cellcutoffup is set at +INF.
 cellcutoffup = 1000;
*No cells are small if cellcutofflo is set at zero.
 cellcutofflo = 0.01;

 LARGECELL(AC,ACP)$(ABS(SAM(AC,ACP)) GT cellcutoffup) = YES;
 SMALLCELL(AC,ACP)$(SAM(AC,ACP) AND (ABS(SAM(AC,ACP)) LT cellcutofflo)) = YES;

*SMALL TRADE AND VALUE-ADDED SHARES ---------------------------

 ACTFACSHR(F,ARD)$SUM(FP, SAM(FP,ARD)) = SAM(F,ARD) / SUM(FP, SAM(FP,ARD));
 ACTVASHR(ARD) = SUM(F, SAM(F,ARD)) / SUM((F,ARDP), SAM(F,ARDP));

 EXPSHR(C)$SUM(A, SAM(A,C))
         = (SAM(C,'ROW') - TAXPAR('EXPTAX',C) - SUM(CTE, SAM(CTE,C))) / SUM(A, SAM(A,C));

 IMPSHR(C)$(SAM(C,'TOTAL') - SAM(C,'ROW') - TAXPAR('COMTAX',C))
         = (SAM('ROW',C) + TAXPAR('IMPTAX',C) + SUM(CTM, SAM(CTM,C))) / (SAM(C,'TOTAL') - SAM(C,'ROW') - TAXPAR('COMTAX',C));

 SHRCUTOFF('FAC') = 0.05;
 SHRCUTOFF('VA')  = 0.05;
 SHRCUTOFF('EXP') = 0.05;
 SHRCUTOFF('IMP') = 0.05;

*If SHRCUTOFF is set at 1 or larger, no shares are eliminated from display
*SHRCUTOFF(SHRITEM) = 1;

*Eliminating shares that are above the cutoff from display.
 ACTFACSHR(F,ARD)$(NOT (ACTFACSHR(F,ARD) LT SHRCUTOFF('FAC'))) = 0;
 ACTVASHR(ARD)$(NOT (ACTVASHR(ARD) LT SHRCUTOFF('VA'))) = 0;
 EXPSHR(C)$(NOT (EXPSHR(C) LT SHRCUTOFF('EXP'))) = 0;
 IMPSHR(C)$(NOT (IMPSHR(C) LT SHRCUTOFF('IMP'))) = 0;

*MISSING OR INCORRECT NON-SAM DATA ----------------------------

*Error messages will be generated if data are missing or incorrect.
*Parameter with error in data provided: SHRHOME(A,C,H)
* ERRHOME(A,H)$(SAM(A,H)$(SUM(C, SHRHOME(A,C,H))$((SUM(C, SHRHOME(A,C,H)) NE 1)))) = 1/0;
*Parameter with missing data: SAM (or error in QFBASE)
 ERRQFBASE1(F,ARD)$(QFBASE(F,ARD) AND NOT SAM(F,ARD)) = 1/0;
*Parameter with missing data: QFBASE
 ERRQFBASE2(F,ARD)$(SAM(F,ARD) AND  NOT QFBASE(F,ARD) AND SUM(ARDP, QFBASE(F,ARDP))) = 1/0;
*Parameter with missing data: TRDELAS
 ERRTRADE(C,'SIGMAQ')$((CM(C)AND CD(C))$(AELASTAB(C,'SIGMAQ')EQ 0)) = 1/0;
 ERRTRADE(C,'SIGMAT')$((CE(C)AND CD(C))$(AELASTAB(C,'SIGMAT')EQ 0)) = 1/0;
*Parameters with missing data: PRODELAS and-or PRODELAS2
 ERRPROD1(ARD)$(SAM('TOTAL',ARD) AND (DELASTAB(ARD,'PRODE') EQ 0)) = 1/0;
 ERRPROD2(ARD)$(DELASTAB(ARD,'PRODE2') EQ 0) = 1/0;
*Parameter with missing data: ELASAC
 ERRAC(C)$(SUM(A, SAM(A,C))$(AELASTAB(C,'OUTAGG') EQ 0)) = 1/0;
*Parameter with missing data: LESELAS1
*FH: Below text turned off to allow SATIM to take control of household consumption commodities
* ERRLES1(C,H)$(SAM(C,H)$(YELASTAB(C,H) EQ 0)) = 1/0;
*Parameter with missing data: LESELAS2
* ERRLES2(A,C,H)$(SAM(A,H) AND (HELAS(A,H) EQ 0)) = 1/0;
