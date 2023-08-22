*1. Economic structure table (STRUCBASE) ---------------------------------------

SETS
 STRCOL          columns /
  VAshr          value-added share (%)
  PRDshr         production share (%)
  EMPshr         share in total employment (%)
  EXPshr         sector share in total exports (%)
  EXP-OUTshr     exports as share in sector output (%)
  IMPshr         sector share in total imports (%)
  IMP-DEMshr     imports as share of domestic demand (%)
 /
 STRCOL2(STRCOL) columns for summation operation /
  VAshr          value-added share (%)
  PRDshr         production share (%)
  EMPshr         share in total employment (%)
  EXPshr         sector share in total exports (%)
  IMPshr         sector share in total imports (%)
 /
 MAPAC(ARD,C)    mapping from activities to commodities
;

 MAPAC(ARD,C)$SAM(ARD,C) = YES;

PARAMETER
 STRUCBASE(*,STRCOL) economic structure in the base
;

*Note: For VAshr, PRDshr, and EMPshr, data scaled by share of activity
*in output of commodity to avoid double-counting when there is not a
*one-to-one mapping between activities and commodities.

 STRUCBASE(C,'VAshr')
   = 100 * SUM(A$MAC(A,C), (PXAC0(A,C)*QXAC0(A,C)/SUM(CP, PXAC0(A,CP)*QXAC0(A,CP)))
         * SUM(RD, PVA0(A,RD)*(1-TVA0(A,RD))*QVA0(A,RD))) / SUM((AP,RD), PVA0(AP,RD)*(1-TVA0(AP,RD))*QVA0(AP,RD));

 STRUCBASE(C,'PRDshr')
   = 100 * SUM(A$MAC(A,C), (PXAC0(A,C)*QXAC0(A,C)/SUM(CP, PXAC0(A,CP)*QXAC0(A,CP)))
         * SUM(RD, PAR0(A,RD)*(1-TVA0(A,RD))*QAR0(A,RD))) / SUM((AP,RD), PAR0(AP,RD)*(1-TVA0(AP,RD))*QAR0(AP,RD));

 STRUCBASE(C,'EMPshr')$SUM((CP,A)$MAC(A,C), PXAC0(A,CP)*QXAC0(A,CP))
   = 100 * SUM(A$MAC(A,C), (PXAC0(A,C)*QXAC0(A,C)/SUM(CP, PXAC0(A,CP)*QXAC0(A,CP)))
         * SUM((FLAB,RD), QF0(FLAB,A,RD))) / SUM((FLAB,AP,RD), QF0(FLAB,AP,RD));

 STRUCBASE(C,'EXPshr') = 100*SUM(RW, PWE.L(C,RW)*QE.L(C,RW))/SUM((RW,CP), PWE.L(CP,RW)*QE.L(CP,RW));

 STRUCBASE(C,'IMPshr') = 100*SUM(RW, PWM.L(C,RW)*QM.L(C,RW))/SUM((RW,CP), PWM.L(CP,RW)*QM.L(CP,RW));

 STRUCBASE(C,'EXP-OUTshr')$(PX.L(C)*QX.L(C)) = 100*SUM(RW, PE.L(C,RW)*QE.L(C,RW))/(PX.L(C)*QX.L(C));
 STRUCBASE('TOTAL','EXP-OUTshr') = 100*SUM((RW,C), PE.L(C,RW)*QE.L(C,RW))/SUM(CP, PX.L(CP)*QX.L(CP)) ;

 STRUCBASE(C,'IMP-DEMshr')$(PQ.L(C)*(1 - TQ.L(C))*QQ.L(C)) = 100*SUM(RW, PM.L(C,RW)*QM.L(C,RW))/(PQ.L(C)*(1 - TQ.L(C))*QQ.L(C));

 STRUCBASE('TOTAL','IMP-DEMshr') = 100*SUM((RW,C), PM.L(C,RW)*QM.L(C,RW)) / SUM(CP, PQ.L(CP)*(1 - TQ.L(CP))*QQ.L(CP));

 STRUCBASE('TOTAL',STRCOL2) = SUM(C, STRUCBASE(C,STRCOL2)) ;


*2. GDP table (GDPBASE) --------------------------------------------------------

SETS
 IGDPX           Items for GDP and national accounts /
  ABSORP         absorption
  PRVCON         private consumption
  FIXINV         fixed investment
  DSTOCK         stock change
  GOVCON         government consumption
  EXPORTS        exports
  IMPORTS        imports
  GDPMP          GDP at market prices (alt. 1: spending)
  GDPMP2         GDP at market prices (alt. 2: value-added)
  NETITAX        net indirect taxes
  GDPFC2         GDP at factor cost
  /
 IGDPXX(IGDPX)   Items for GDPMP summation  /
  PRVCON         private consumption
  FIXINV         fixed investment
  DSTOCK         stock change
  GOVCON         government consumption
  EXPORTS        exports
  IMPORTS        imports
  /
 KGDPX           second index in GDP tables /
  VALUE, PERC-GDP, PERC-TOT
 /
;

PARAMETERS
 GDPBASE(IGDPX,KGDPX)  aggregate national accounts summary
 GDPERR                error if alt GDP definitions are not identical
;

*utax code
* GDPBASE('PRVCON','VALUE')  = SUM((C,H), PQ.L(C)*QH.L(C,H)) + SUM((A,H), PA.L(A)*QHA.L(A,H));
 GDPBASE('PRVCON','VALUE')  = SUM((C,H), PQH.L(C,H)*QH.L(C,H)) + SUM((A,H), PA.L(A)*QHA.L(A,H));
 GDPBASE('FIXINV','VALUE')  = SUM((C,IT), PQ.L(C)*QINV.L(C,IT));
 GDPBASE('DSTOCK','VALUE')  = SUM(C, PQ.L(C)*QDST(C));
 GDPBASE('GOVCON','VALUE')  = SUM(C, PQ.L(C)*QG.L(C));
 GDPBASE('EXPORTS','VALUE') = SUM((RW,CE), PWE.L(CE,RW)*EXR.L*QE.L(CE,RW));
 GDPBASE('IMPORTS','VALUE') = -SUM((RW,CM), PWM.L(CM,RW)*EXR.L*QM.L(CM,RW));
 GDPBASE('GDPMP','VALUE')   = SUM(IGDPXX, GDPBASE(IGDPXX,'VALUE'));

 GDPBASE('ABSORP','VALUE')  = GDPBASE('GDPMP','VALUE') - GDPBASE('IMPORTS','VALUE') - GDPBASE('EXPORTS','VALUE');
 GDPBASE('GDPFC2','VALUE')  = SUM((A,RD), PVA.L(A,RD)*(1-TVA.L(A,RD))*QVA.L(A,RD));

 GDPBASE('NETITAX','VALUE') =
            SUM((A,RD), TA.L(A,RD)*PAR.L(A,RD)*QAR.L(A,RD))
          + SUM((A,RD), TVA.L(A,RD)*PVA.L(A,RD)*QVA.L(A,RD))
          + SUM((RW,CM), tm(CM,RW)*QM.L(CM,RW)*PWM.L(CM,RW))*EXR.L
          + SUM((RW,CE), te(CE,RW)*QE.L(CE,RW)*PWE.L(CE,RW))*EXR.L
          + SUM(C, TQ.L(C)*PQ.L(C)*QQ.L(C))
  ;

 GDPBASE('GDPMP2','VALUE')  = GDPBASE('GDPFC2','VALUE') + GDPBASE('NETITAX','VALUE');
 GDPERR$(ABS(GDPBASE('GDPMP2','VALUE') - GDPBASE('GDPMP','VALUE')) GT 0.00001) = 1/0;
 GDPBASE(IGDPX,'PERC-GDP')$GDPBASE('GDPMP','VALUE') =  100*GDPBASE(IGDPX,'VALUE')/GDPBASE('GDPMP','VALUE');


*3. Macro SAM (MACSAM) ---------------------------------------------------------

SET
 ACMAC           macrosam accounts /
  ACT2           Activities
  COM2           Commodities
  FAC2           Factors
  HOU2           Domestic non-gov institution (households + enterprises)
  GOV2           Current government
  ROW2           Rest of World
  S-I2           Savings investment account
  INSTAX2        direct taxes on domestic institutions
  FACTAX2        direct factor taxes
  IMPTAX2        import taxes
  EXPTAX2        export taxes
  VATAX2         value-added taxes
  ACTTAX2        indirect taxes on activity revenue
  COMTAX2        indirect taxes on commodity sales in domestic market
  CARTAX2        indirect taxes on domestic carbon content
  ECBTAX2        indirect rebates on exported carbon content
  MCBTAX2        indirect taxes on imported carbon content
  ELCTAX2        taxes and subsidies on electricity prices
  TOTAL2         totals
 /
 ACMACNT(ACMAC)  all macrosam accounts except TOTAL2
 ACMACTAX(ACMAC) tax accounts /
  INSTAX2        direct taxes on domestic institutions
  FACTAX2        direct factor taxes
  IMPTAX2        import taxes
  EXPTAX2        export taxes
  VATAX2         value-added taxes
  ACTTAX2        indirect taxes on activity revenue
  COMTAX2        indirect taxes on commodity sales in domestic market
  CARTAX2        indirect taxes on domestic carbon content
  ECBTAX2        indirect rebates on exported carbon content
  MCBTAX2        indirect taxes on imported carbon content
  ELCTAX2        taxes and subsidies on electricity prices
 /
;

 ACMACNT(ACMAC)    = YES;
 ACMACNT('TOTAL2') = NO;

ALIAS(ACMAC,ACMACP),(ACMACNT,ACMACNTP);

PARAMETER
 MACROSAM(ACMAC,ACMACP)  macro SAM
 BALCHK2(ACMAC)          total balance check
 MACCHK                  error message if Macro SAM does not balance
;

*Defining SAM cells
 MACROSAM('COM2','ACT2')    = SUM((C,A), PQ.L(C)*QINT.L(C,A));
 MACROSAM('FAC2','ACT2')    = SUM((A,RD), PVA.L(A,RD)*(1-TVA.L(a,RD))*QVA.L(A,RD));
 MACROSAM('VATAX2','ACT2')  = SUM((A,RD), TVA.L(A,RD)*PVA.L(A,RD)*QVA.L(A,RD));
 MACROSAM('ACTTAX2','ACT2') = SUM((A,RD), TA.L(A,RD)*PAR.L(A,RD)*QAR.L(A,RD));
 MACROSAM('ELCTAX2','ACT2') = SUM((C,A,RD), tui(C,A,RD)*PQ.L(C)*QINTA.L(A,RD)*ica(C,A,RD));

 MACROSAM('COM2','COM2')    = SUM(C, PQ.L(C)*QT.L(C));
 MACROSAM('ACT2','COM2')    = SUM((C,A), QXAC.L(A,C)*PXAC.L(A,C));
 MACROSAM('ROW2','COM2')    = SUM((RW,C), PWM.L(C,RW)*QM.L(C,RW))*EXR.L;
 MACROSAM('COMTAX2','COM2') = SUM(C, TQ.L(C)*PQ.L(C)*QQ.L(C));
 MACROSAM('CARTAX2','COM2') = SUM(C, co2c(C)*tco2d*QQ.L(C));
 MACROSAM('MCBTAX2','COM2') = SUM((C,RW), co2m(C,RW)*tco2m*QM.L(C,RW));
 MACROSAM('ECBTAX2','COM2') = SUM((C,RW), co2e(C,RW)*tco2e*QE.L(C,RW));
 MACROSAM('IMPTAX2','COM2') = SUM((RW,C), tm(C,RW)*PWM.L(C,RW)*QM.L(C,RW))*EXR.L;
 MACROSAM('EXPTAX2','COM2') = SUM((RW,C), te(C,RW)*PWE.L(C,RW)*QE.L(C,RW))*EXR.L;

 MACROSAM('HOU2','FAC2')    = SUM((INSDNG,F), YIF.L(INSDNG,F));
 MACROSAM('GOV2','FAC2')    = SUM(F, YIF.L('GOV',F)) ;
 MACROSAM('ROW2','FAC2')    = SUM(F, trnsfr('ROW',F))*EXR.L + sum(f, (1-tf(f))*RF(F)*YF.l(F));
 MACROSAM('FACTAX2','FAC2') = SUM(f, tf(f)*YF.L(F));

 MACROSAM('COM2','HOU2')    = SUM((C,H), PQ.L(C)*QH.L(C,H));
 MACROSAM('ACT2','HOU2')    = SUM((A,H), PA.L(A)*QHA.L(A,H));
 MACROSAM('INSTAX2','HOU2') = SUM(INSDNG, TINS.L(INSDNG)*YI.L(INSDNG));
 MACROSAM('HOU2','HOU2')    = SUM((INSDNG,INSDNGP), TRII.L(INSDNG,INSDNGP));
 MACROSAM('S-I2','HOU2')    = SUM(INSDNG, MPS.L(INSDNG) * (1 - TINS.L(INSDNG)) * YI.L(INSDNG));
 MACROSAM('ELCTAX2','HOU2') = SUM((C,H), tuh(C,H)*PQ.L(C)*QH.L(C,H));

 MACROSAM('COM2','GOV2')    = SUM(C, PQ.L(C)*QG.L(C));
 MACROSAM('HOU2','GOV2')    = SUM(INSD, trnsfr(INSD,'GOV'))*CPI.L;
 MACROSAM('S-I2','GOV2')    = GSAV.L ;

 MACROSAM('COM2','ROW2')    = SUM((RW,C), PWE.L(C,RW)*QE.L(C,RW))*EXR.L;
 MACROSAM('HOU2','ROW2')    = SUM(INSDNG, trnsfr(INSDNG,'ROW'))*EXR.L;
 MACROSAM('GOV2','ROW2')    = trnsfr('GOV','ROW')*EXR.L;
 MACROSAM('S-I2','ROW2')    = FSAV.L*EXR.L ;

 MACROSAM('COM2','S-I2')    = SUM((C,IT), PQ.L(C)*QINV.L(C,IT)) + SUM(C, PQ.L(C)*qdst(C));

 MACROSAM('GOV2',ACMACTAX)  = SUM(ACMAC, MACROSAM(ACMACTAX,ACMAC));

*Computing account totals

 MACROSAM('TOTAL2', ACMAC)    = 0;
 MACROSAM(ACMAC,'TOTAL2')     = 0;

 MACROSAM('TOTAL2', ACMACNTP) = SUM(ACMACNT,  MACROSAM(ACMACNT,ACMACNTP));
 MACROSAM(ACMACNT,'TOTAL2')   = SUM(ACMACNTP, MACROSAM(ACMACNT,ACMACNTP));

*Checking account balances

 BALCHK2(ACMACNT) = MACROSAM('TOTAL2',ACMACNT) - MACROSAM(ACMACNT,'TOTAL2');

 MACCHK(ACMACNT)$(ABS(BALCHK2(ACMACNT)) GT 0.00001) = 1/0;
*If error, either correct the SAM or exclude the include file.


*4. Input shares, factor wages, and elasticities (FACTAB1 - 4) -----------------

SETS
 FF(AC)          all factor accounts and total
 A2(AC)          all activity accounts and total
 RD2(AC)         all regional accounts and total
;

 FF(F)        = YES;
 FF('TOTAL')  = YES;
 A2(A)        = YES;
 A2('TOTAL')  = YES;
 RD2(RD)      = YES;
 RD2('TOTAL') = YES;

PARAMETERS
 FACTAB1(AC,ACP) factor payment shares within sector
 FACTAB2(AC,ACP) factor payment shares across sectors
 FACTAB3(AC,*)   agg factor & intd. input shares & their sub. elasticity
 FACTAB4(AC,ACP) factor wages by sector
 FACTAB5(AC,AC)  factor payment shares across regions
 FACTAB6(AC,AC)  factor payment shares within regions
;

 FACTAB1(A,F)       = SUM(RD, WF.L(F)*WFDIST.L(F,A,RD)*QF.L(F,A,RD)) ;
 FACTAB1('TOTAL',F) = SUM(A, FACTAB1(A,F)) ;
 FACTAB1(A,'TOTAL') = SUM(F, FACTAB1(A,F)) ;
 FACTAB1('TOTAL','TOTAL') = SUM(F, FACTAB1('TOTAL',F)) ;
 FACTAB2(A2,FF)    = FACTAB1(A2,FF) ;
 FACTAB4(A2,FF)    = FACTAB1(A2,FF) ;

 FACTAB2(A2,FF)$FACTAB2('TOTAL',FF) = 100*FACTAB2(A2,FF)/FACTAB2('TOTAL',FF) ;
 FACTAB1(A2,FF)$FACTAB1(A2,'TOTAL') = 100*FACTAB1(A2,FF)/FACTAB1(A2,'TOTAL') ;

 FACTAB3(A,'QVASHR')$QA0(A)  = 100*SUM(RD, PVA.L(A,RD)*QVA.L(A,RD)) / SUM(RD, PAR.L(A,RD)*(1-TA.L(A,RD))*QAR.L(A,RD)) ;
 FACTAB3(A,'QINTSHR')$QA0(A) = 100*SUM(RD, PINTA.L(A,RD)*QINTA.L(A,RD))/ SUM(RD, PAR.L(A,RD)*(1-TA.L(A,RD))*QAR.L(A,RD)) ;
 FACTAB3('TOTAL','QVASHR')   = 100*SUM((A,RD), PVA.L(A,RD)*QVA.L(A,RD)) / SUM((A,RD), PAR.L(A,RD)*(1-TA.L(A,RD))*QAR.L(A,RD)) ;
 FACTAB3('TOTAL','QINTSHR')  = 100*SUM((A,RD), PINTA.L(A,RD)*QINTA.L(A,RD)) / SUM((A,RD), PAR.L(A,RD)*(1-TA.L(A,RD))*QAR.L(A,RD)) ;

 FACTAB4(A2,'TOTAL')         = 0 ;
 FACTAB4(A,F)$SUM(RD, QF.L(F,A,RD)) = FACTAB4(A,F)/SUM(RD, QF.L(F,A,RD)) ;
 FACTAB4('TOTAL',F)$SUM((A,RD), QF.L(F,A,RD)) = FACTAB4('TOTAL',F)/SUM((A,RD), QF.L(F,A,RD)) ;

 FACTAB5(F,RD)             = SUM(A, WF.L(F)*wfdist.L(F,A,RD)*QF.L(F,A,RD)) ;
 FACTAB5('TOTAL',RD)       = SUM(F, FACTAB5(F,RD)) ;
 FACTAB5(FF,'TOTAL')       = SUM(RD, FACTAB5(FF,RD)) ;
 FACTAB6(FF,RD)            = FACTAB5(FF,RD) ;
 FACTAB6(FF,'TOTAL')       = SUM(RD, FACTAB5(FF,RD)) ;

 FACTAB5(FF,RD2)$FACTAB5('TOTAL',RD2) = 100*FACTAB5(FF,RD2)/FACTAB5('TOTAL',RD2) ;
 FACTAB6(FF,RD2)$FACTAB6(FF,'TOTAL')  = 100*FACTAB6(FF,RD2)/FACTAB6(FF,'TOTAL')


*5. Household incomes (HHDTAB1 - 2) --------------------------------------------

PARAMETERS
 HHDTAB1(AC,ACP) share of item ACP in the total income of hhd H (%)
 HHDTAB2(AC,ACP) share of hhd AC in total hhd income from item ACP (%)
;

 HHDTAB1(H,ACNT)$YI0(H) = 100*SAM(H,ACNT)/SUM(ACNTP, SAM(H,ACNTP));
 HHDTAB1('TOTAL',ACNT)  = 100*SUM(H, SAM(H,ACNT))/SUM((H,ACNTP), SAM(H,ACNTP));
 HHDTAB1(AC,'TOTAL')    = SUM(ACNT, HHDTAB1(AC,ACNT));

 HHDTAB2(H,ACNT)$SUM(HP, SAM(HP,ACNT)) = 100*SAM(H,ACNT)/SUM(HP, SAM(HP,ACNT));
 HHDTAB2(H,'TOTAL')     = 100*SUM(ACNT, SAM(H,ACNT))/SUM((HP,ACNT), SAM(HP,ACNT));

 HHDTAB2('TOTAL',AC)    = SUM(H, HHDTAB2(H,AC));


*6. New SAM budget accounts (SAMBUD) -------------------------------------------

*SR For debugging
SET
 INDB2           second index for SAM budgets /INC, SPND, GAP /
;

PARAMETER
 SAMBUDB(AC,INDB2) sam budget accounts
 SAMBUDGAPB(AC)    gap in sam budget accounts
;


 SAMBUDB(A,'INC')   = SUM(C, PXAC.L(A,C)*QXAC.L(A,C)) + SUM(H, PA.L(A)*QHA.L(A,H)) ;

 SAMBUDB(A,'SPND')  =
   SUM((F,RD), WF.L(F)*WFDIST.L(F,A,RD)*QF.L(F,A,RD)) + SUM(C, PQ.L(C)*QINT.L(C,A)) +
   SUM(RD, TVA.L(A,RD)*PVA.L(A,RD)*QVA.L(A,RD) + TA.L(A,RD)*PAR.L(A,RD)*QAR.L(A,RD)) ;

 SAMBUDB(C,'SPND')  = PDD.L(C)*QD.L(C) + SUM(RW, PM.L(C,RW)*QM.L(C,RW)) + PQ.L(C)*TQ.L(c)*QQ.L(C) ;

 SAMBUDB(C,'INC')   = PQ.L(C)*SUM(A, QINT.L(C,A) ) + PQ.L(C)*SUM(H, QH.L(C,H)) +
                      PQ.L(C)*(QG.L(C) + SUM(IT, QINV.L(C,IT)) + qdst(C) + QT.L(C)) ;

 SAMBUDB('ROW','INC') = EXR.L*(SUM((RW,C), PWM.L(C,RW)*QM.L(C,RW)) + SUM(F, TRNSFR('ROW',F))) ;
 SAMBUDB('ROW','SPND') = EXR.L*(SUM((RW,C), PWE.L(C,RW)*QE.L(C,RW)) + SUM(INSD, TRNSFR(INSD,'ROW')) + FSAV.L) ;

 SAMBUDB('S-I','SPND') = SUM(C, PQ.L(C)*(SUM(IT, QINV.L(C,IT)) + qdst(C))) ;
 SAMBUDB('S-I','INC')  = SUM(INSDNG, MPS.L(INSDNG)*(1 - TINS.L(INSDNG))*YI.L(INSDNG)) + GSAV.L + FSAV.L*EXR.L ;

 SAMBUDB('GOV','INC') =
   SUM(INSDNG, TINS.L(INSDNG)*YI.L(INSDNG))
   + SUM(f, tf(F)*YF.L(F))
   + SUM((A,RD), TVA.L(A,RD)*PVA.L(A,RD)*QVA.L(A,RD))
   + SUM((A,RD), TA.L(A,RD)*PAR.L(A,RD)*QAR.L(A,RD))
   + SUM((C,RW), tm(C,RW)*PWM.L(C,RW)*QM.L(C,RW))*EXR.L
   + SUM((C,RW), te(C,RW)*PWE.L(C,RW)*QE.L(C,RW))*EXR.L
   + SUM(C, TQ.L(C)*PQ.L(C)*QQ.L(C))
   + SUM(F, YIF.L('GOV',F))
   + trnsfr('GOV','ROW')*EXR.L ;

 SAMBUDB('GOV','SPND') = SUM(C, PQ.L(C)*QG.L(C)) + SUM(INSDNG, trnsfr(INSDNG,'GOV'))*CPI.L + GSAV.L ;

 SAMBUDB(INSDNG,'INC') = SUM(F, YIF.L(INSDNG,F)) + SUM(INSDNGP, TRII.L(INSDNG,INSDNGP)) +
   trnsfr(INSDNG,'GOV')*CPI.L + trnsfr(INSDNG,'ROW')*EXR.L + SUM(F, trnsfr(INSDNG,F)) ;

 SAMBUDB(INSDNG,'SPND') =
   SUM(INSDNGP, TRII.L(INSDNGP,INSDNG)) +
   trnsfr('ROW',INSDNG)*EXR.L +
   TINS.L(INSDNG)*YI.L(INSDNG) +
   MPS.L(INSDNG)*(1 - TINS.L(INSDNG))*YI.L(INSDNG) +
   SUM((C,H)$SAMEAS(INSDNG,H), PQ.L(C)*QH.L(C,H)) +
   SUM((A,H)$SAMEAS(INSDNG,H), PA.L(A)*QHA.L(A,H));

 SAMBUDB(AC,'GAP')  = SAMBUDB(AC,'INC') - SAMBUDB(AC,'SPND') ;
 SAMBUDGAPB(AC)$(ABS(SAMBUDB(AC,'GAP')) gt 1e-6) = SAMBUDB(AC,'GAP');
