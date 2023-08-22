
 ASAM(AC,ACP)    = 0;
 ASAM(C,A)       = PQ0(C)*QINT.L(C,A);
 ASAM(F,A)       = SUM(RD, WF0(F)*WFDIST0(F,A,RD)*QF.L(F,A,RD));
 ASAM('ATAX',A)  = SUM(RD, TA0(A,RD)*PAR0(A,RD)*QAR.L(A,RD));

 ASAM(A,C)$MAC(A,C) = SUM(ACNT, ASAM(ACNT,A));
 ASAM('TRC',C)   = SUM(CP, PQ0(CP)*icd(CP,C)*QD.L(C) + SUM(RW, PQ0(CP)*icm(CP,C)*QM.L(C,RW)) + SUM(RW, PQ0(CP)*ice(CP,C)*QE.L(C,RW)));
 ASAM('CTAX',C)  = co2c(C)*tco2d*QQ.L(C) + SUM(RW, co2m(C,RW)*tco2m*QM.L(C,RW)) + SUM(RW, co2e(C,RW)*tco2e*QE.L(C,RW));
 ASAM('STAX',C)  = TQ0(C)*PQ0(C)*QQ.L(C);
 ASAM('MTAX',C)  = SUM(RW, tm0(C,RW)*pwm0(C,RW)*QM.L(C,RW))*EXR0;
 ASAM('ROW',C)   = SUM(RW, pwm0(C,RW)*QM.L(C,RW))*EXR0;

 ASAM(C,H)       = PQ0(C)*QH.L(C,H);
 ASAM(C,'GOV')   = PQ0(C)*QG.L(C);
 ASAM(C,'ROW')   = SUM(RW, pwe0(C,RW)*QE.L(C,RW))*EXR0;
 ASAM(C,'S-I')   = SUM(IT, PQ0(C)*QINV.L(C,IT));
 ASAM(C,'DSTK')  = PQ0(C)*qdst(C);
 ASAM(C,'TRC')   = SUM(CP, icd(C,CP)*PQ0(C)*QD.L(CP)) + SUM((CP,RW), icm(C,CP)*PQ0(C)*QM.L(CP,RW)) + SUM((CP,RW), ice(C,CP)*PQ0(C)*QE.L(CP,RW));

 ASAM('TOTAL',ACNT) = SUM(ACNTP, ASAM(ACNTP,ACNT));
 ASAM(ACNT,'TOTAL') = SUM(ACNTP, ASAM(ACNT,ACNTP));
 ADIFF(A) = ASAM('TOTAL',A) - ASAM(A,'TOTAL');
 ADIFF(C) = ASAM('TOTAL',C) - ASAM(C,'TOTAL');
 ADIFF(ACNT)$(ABS(ADIFF(ACNT)) LT 1E-4) = 0;

 CTOT(C) = co2c(C)*(QX.L(C)+SUM(RW, QM.L(C,RW)));

 AMAT(ACM,ACMP) = 0;
 AMAT(ACM,ACMP) = ASAM(ACM,ACMP);
 AMAT(ACM,ACMP)$SUM(ACNT, ASAM(ACNT,ACMP)) = AMAT(ACM,ACMP) / SUM(ACNT, ASAM(ACNT,ACMP));

 AMAT(AC,ACP) = IMAT(AC,ACP) - AMAT(AC,ACP);

 CPRC(C)$SUM(ACNT, ASAM(C,ACNT)) = CTOT(C) / SUM(ACNT, ASAM(C,ACNT));

execute_unload 'gdxforinverse.gdx' ACM, AMAT;
execute 'invert gdxforinverse.gdx ACM AMAT gdxfrominverse.gdx LMAT';
execute_load 'gdxfrominverse.gdx' , LMAT;

*Carbon content
 CCON(ACM) = SUM(ACMP, CPRC(ACMP) * LMAT(ACMP,ACM));
 CCONX(ACM,TC) = CCON(ACM);

*Map activities to commodities and multiply by activity price for exports

* only keep commodities that will attract retaliatory tax
* co2e(C,RW) = CCON(C)*PE0(C,RW);
 co2e(RTC,RW) = CCON(RTC)*PE0(RTC,RW);

* co2e(C,RW)$QE0(C,RW) = CCON(C);
*Find appropriate import coefficient
 co2m(C,RW) = CCON(C)*PWM0(C,RW)*EXR0;
* co2m(C,RW)$QM0(C,RW) = CCON(C);
*Redo for electricity assuming that imports are hydro   ;
 co2m('CELEC',RW)= 0;

 co2e(C,RW)$co2c(C) = 0;
 co2m(C,RW)$co2c(C) = 0;




