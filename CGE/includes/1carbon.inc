SET
 ACM(AC)
;

 ACM(A)   = YES;
 ACM(C)   = YES;
 ACM(CTD) = YES;
 ACM(CTE) = YES;
 ACM(CTM) = YES;

ALIAS (ACM,ACMP);

PARAMETER
 AMAT(AC,ACP)    coefficient matrix
 IMAT(AC,ACP)    identity matrix
 LMAT(AC,ACP)    leontief inverse
 CTOT(AC)        total carbon (domestic and imported)
 CPRC(AC)        carbon per unit of total supply
 CCON(AC)        carbon content per unit of final demand
 ASAM(AC,ACP)    aggregated activity SAM
 ADIFF(AC)
;

 ASAM(AC,ACP) = SAM(AC,ACP);
 ASAM(ARD,AC) = 0;
 ASAM(AC,ARD) = 0;
 ASAM(ACNT,A) = SUM((RD,ARD)$MARD(ARD,A,RD), SAM(ACNT,ARD));
 ASAM(A,ACNT) = SUM((RD,ARD)$MARD(ARD,A,RD), SAM(ARD,ACNT));
 ASAM('TOTAL',ACNT) = SUM(ACNTP, ASAM(ACNTP,ACNT));
 ASAM(ACNT,'TOTAL') = SUM(ACNTP, ASAM(ACNT,ACNTP));
 ADIFF(ACNT) = ASAM('TOTAL',ACNT) - ASAM(ACNT,'TOTAL');
 ADIFF(ACNT)$(ABS(ADIFF(ACNT)) LT 1E-4) = 0;

 CTOT(C) = co2c(C)*(QX0(C)+SUM(RW, QM0(C,RW)));

 AMAT(ACM,ACMP) = ASAM(ACM,ACMP);
 AMAT(ACM,ACMP)$SUM(ACNT, ASAM(ACNT,ACMP)) = AMAT(ACM,ACMP) / SUM(ACNT, ASAM(ACNT,ACMP));

 IMAT(ACM,ACM) = 1;

 AMAT(AC,ACP) = IMAT(AC,ACP) - AMAT(AC,ACP);

 CPRC(C)$SUM(ACNT, ASAM(C,ACNT)) = CTOT(C) / SUM(ACNT, ASAM(C,ACNT));

execute_unload 'gdxforinverse.gdx' ACM, AMAT;
execute 'invert gdxforinverse.gdx ACM AMAT gdxfrominverse.gdx LMAT';
execute_load 'gdxfrominverse.gdx' , LMAT;

*Carbon content
 CCON(ACM) = SUM(ACMP, CPRC(ACMP) * LMAT(ACMP,ACM));


*Map activities to commodities and multiply by activity price for exports
 co2e(C,RW) = CCON(C)*PE0(C,RW);
*Find appropriate import coefficient
 co2m(C,RW) = CCON(C)*PWM0(C,RW)*EXR0;
*Redo for electricity assuming that imports are hydro   ;
 co2m('CELEC',RW)= 0;

 co2e(C,RW)$co2c(C) = 0;
 co2m(C,RW)$co2c(C) = 0;

 co2e0(C,RW) = co2e(C,RW);
 co2m0(C,RW) = co2m(C,RW);