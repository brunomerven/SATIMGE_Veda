*---------------------------------------------------------------
*ESTIMATE DEMAND FORECAST
*---------------------------------------------------------------

* Previous: SFORE(FS,XC,T) = SUM((A,RD)$MFSA(FS,A), PVA0(A,RD)*QVAX(A,RD,XC,T,TT));
*
 GVA_FS(FS,TC) = SUM(A$MFSA(FS,A), QVAX(A,'nat',XC,TC,TT));
 REPORT(PRC,'ACTGRP',TC,RUN,'GVA') = SUM(FS$MPRCFS2(PRC,FS),GVA_FS(FS,TC));

* Another possible approach is to link to QA but QA has different units in different activities and doesn't add up to GDP
 QA_FS(FS,T) = SUM(A$MFSA(FS,A), QAX(A,XC,T,TT));

* Domestic demand for energy model
 QD_FS('is',T) = QA_FS('is',T)-QEX('ciron','rest',XC,T,TT);
*# QD_FS('am',T) = QA_FS('am',T)-QEX('cammo','rest',XC,T,TT);


 HFORE(FH,XC,TC)$SUM(H$MFHH(FH,H), HPOPX(H,XC,TC,TT)) = SUM(H$MFHH(FH,H), YIX(H,XC,TC,TT)/HPOPX(H,XC,TC,TT))/CPIX(XC,TC,TT);
 PFORE(FH,XC,TC) = SUM(H$MFHH(FH,H), HPOPX(H,XC,TC,TT));

*INCOME CALCULATIONS

*Calculate mean (assume uniform population distribution within each group)
 FHMM(FH,'MEAN')$FHMM(FH,'MAX') = (FHMM(FH,'MAX') + FHMM(FH,'MIN'))/2;

*Forecast mean incomes based on TIMES data
 FHMEAN(FH,TC) = FHMM(FH,'MEAN') * HFORE(FH,XC,TC);

*POPULATION CALCULATIONS

 FHMMB(FH,'MIN',TC)$SUM(T1, HFORE(FH,XC,T1)) = FHMM(FH,'MIN') * HFORE(FH,XC,TC)/SUM(T1, HFORE(FH,XC,T1));
 FHMMB(FH,'MAX',TC)$SUM(T1, HFORE(FH,XC,T1)) = FHMM(FH,'MAX') * HFORE(FH,XC,TC)/SUM(T1, HFORE(FH,XC,T1));

*Lowest group to highest group
 FHMOVE('HHD-LE','HHD-HE',TC)
         = (FHMMB('HHD-LE','MAX',TC) - FHMM('HHD-HE','MIN'))
         / (FHMMB('HHD-LE','MAX',TC) - FHMMB('HHD-LE','MIN',TC));
 FHMOVE('HHD-LE','HHD-HE',TC)$(FHMOVE('HHD-LE','HHD-HE',TC) LT 0) = 0;

*Lowest group to middle group
 FHMOVE('HHD-LE','HHD-ME',TC)
         = (FHMMB('HHD-LE','MAX',TC) - FHMM('HHD-ME','MIN'))
         / (FHMMB('HHD-LE','MAX',TC) - FHMMB('HHD-LE','MIN',TC))
         - FHMOVE('HHD-LE','HHD-HE',TC);

*Middle group to highest group
 FHMOVE('HHD-ME','HHD-HE',TC)$(FHMMB('HHD-ME','MIN',TC) LT FHMM('HHD-HE','MIN'))
         = (FHMMB('HHD-ME','MAX',TC) - FHMM('HHD-HE','MIN'))
         / (FHMMB('HHD-ME','MAX',TC) - FHMMB('HHD-ME','MIN',TC));
 FHMOVE('HHD-ME','HHD-HE',TC)$(FHMMB('HHD-ME','MIN',TC) GT FHMM('HHD-HE','MIN')) = 1;
 FHMOVE('HHD-ME','HHD-HE',TC)$(FHMOVE('HHD-ME','HHD-HE',TC) LT 0) = 0;

*Final population numbers
 TFHPOP('HHD-LE',TC) = PFORE('HHD-LE',XC,TC) *
         (1 - FHMOVE('HHD-LE','HHD-ME',TC) - FHMOVE('HHD-LE','HHD-HE',TC));

 TFHPOP('HHD-ME',TC) = PFORE('HHD-ME',XC,TC) *
         (1 - FHMOVE('HHD-ME','HHD-HE',TC))
         + FHMOVE('HHD-LE','HHD-ME',TC)*PFORE('HHD-LE',XC,TC);

 TFHPOP('HHD-HE',TC) = PFORE('HHD-HE',XC,TC)
         + FHMOVE('HHD-LE','HHD-HE',TC)*PFORE('HHD-LE',XC,TC)
         + FHMOVE('HHD-ME','HHD-HE',TC)*PFORE('HHD-ME',XC,TC);

*INCOME CALCULATIONS (CONT)

loop (H,
 popval(H,XC,TC) =  (sum(FH,TFHPOP(FH,TC))/10)*ORD(H)*popvalfact(H,XC,TC);
 );

*Reverse mapping
 MFHHT(FH,H,TC)=no;
 MFHHT('hhd-le',H,TC)$((popval(H,XC,TC) LE TFHPOP('hhd-le',TC)))=YES;
 MFHHT('hhd-me',H,TC)$(((popval(H,XC,TC) GT TFHPOP('hhd-le',TC)) and (popval(H,XC,TC) LE (TFHPOP('hhd-le',TC)+TFHPOP('hhd-me',TC)))))=YES;
 MFHHT('hhd-he',H,TC)$((popval(H,XC,TC) GT (TFHPOP('hhd-me',TC)+TFHPOP('hhd-le',TC))))=YES;

 HFORE(FH,XC,TC)$SUM(H$MFHHT(FH,H,TC), HPOPX(H,XC,TC,TT)) = SUM(H$MFHHT(FH,H,TC), YIX(H,XC,TC,TT)/HPOPX(H,XC,TC,TT))/CPIX(XC,TC,TT);

 STFHPOP(TC) = sum(FH,TFHPOP(FH,TC));

 TFHPOP(FH,TC) = TFHPOP(FH,TC)/STFHPOP(TC);

* remember to update HFORE with new mappings
*    HFORE(FH,XC,MYPTC) = HFORE(FH,XC,'2040');

    YHE(TC) = HFORE('hhd-he',XC,TC);

* write new demand driver values to drivers gdx if CGE gets to 2050
*  if(QVAX('aagri','nat',XC,'2050',TT),
    execute_unload "drivers.gdx" GVA_FS POP YHE TFHPOP MFHHT PAMS_RUN QA_FS QD_FS;
*  );


