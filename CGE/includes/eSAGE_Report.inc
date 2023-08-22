*--------------------------------------------------------------
*Process eSAGE results (Socio-economic) -----------------------
*--------------------------------------------------------------


* Calculate Employment per SATIM sector
REPORT(PRC,'ACTGRP',TC,RUN,'Employment-p') = sum(FS$MPRCFS2(PRC,FS),sum(A$MFSA(FS,A),QFX('flab-p',A,'nat',XC,TC,'2050')));
REPORT(PRC,'ACTGRP',TC,RUN,'Employment-m') = sum(FS$MPRCFS2(PRC,FS),sum(A$MFSA(FS,A),QFX('flab-m',A,'nat',XC,TC,'2050')));
REPORT(PRC,'ACTGRP',TC,RUN,'Employment-s') = sum(FS$MPRCFS2(PRC,FS),sum(A$MFSA(FS,A),QFX('flab-s',A,'nat',XC,TC,'2050')));
REPORT(PRC,'ACTGRP',TC,RUN,'Employment-t') = sum(FS$MPRCFS2(PRC,FS),sum(A$MFSA(FS,A),QFX('flab-t',A,'nat',XC,TC,'2050')));

* Overwrite Elc Sector with SATIM coeffs
 REPORT(PRC,'ACTGRP',TC,RUN,'Employment-p')$UC_CAP('UCPWR_FLAB-P','LHS','REGION1','2012',PRC) = UC_CAP('UCPWR_FLAB-P','LHS','REGION1',TC,PRC)*(-1)*TVCAP(TC,PRC);
 REPORT(PRC,'ACTGRP',TC,RUN,'Employment-m')$UC_CAP('UCPWR_FLAB-M','LHS','REGION1','2012',PRC) = UC_CAP('UCPWR_FLAB-M','LHS','REGION1',TC,PRC)*(-1)*TVCAP(TC,PRC);
 REPORT(PRC,'ACTGRP',TC,RUN,'Employment-s')$UC_CAP('UCPWR_FLAB-S','LHS','REGION1','2012',PRC) = UC_CAP('UCPWR_FLAB-S','LHS','REGION1',TC,PRC)*(-1)*TVCAP(TC,PRC);
 REPORT(PRC,'ACTGRP',TC,RUN,'Employment-t')$UC_CAP('UCPWR_FLAB-T','LHS','REGION1','2012',PRC) = UC_CAP('UCPWR_FLAB-T','LHS','REGION1',TC,PRC)*(-1)*TVCAP(TC,PRC);






*Calculate Consumption per CGE income decile (this could be done more elegantly)
REPORT(PRCH,'ACTGRP',TC,RUN,'Consumption') = sum(H$MHPRCH(PRCH,H),EHX(H,XC,TC,'2050'));
REPORT(PRCH,'ACTGRP',TC,RUN,'Population') = sum(H$MHPRCH(PRCH,H),HPOPX(H,XC,TC,'2050'));


*Calculate Consumption per SATIM income group


*Calculate Real Trade Balance
*REPORT('OtherEconIndicators',TCG,TC,RUN,'Imports') = sum(C$MCTCG(C,TCG),PM0(C,'rest')*QMX(C,'rest',XC,TC,'2050'));
REPORT('OtherEconIndicators',TCG,TC,RUN,'Imports') = sum(C$MCTCG(C,TCG),PWM0(C,'rest')*QMX(C,'rest',XC,TC,'2050'));

*REPORT('OtherEconIndicators',TCG,TC,RUN,'Exports') = sum(C$MCTCG(C,TCG),PE0(C,'rest')*QEX(C,'rest',XC,TC,'2050'));
REPORT('OtherEconIndicators',TCG,TC,RUN,'Exports') = sum(C$MCTCG(C,TCG),PWE0(C,'rest')*QEX(C,'rest',XC,TC,'2050'));

*REPORT('OtherEconIndicators','ACTGRP',TC,RUN,'TradeDeficit') = sum(C,PM0(C,'rest')*QMX(C,'rest',XC,TC,'2050')-PE0(C,'rest')*QEX(C,'rest',XC,TC,'2050'));
REPORT('OtherEconIndicators','ACTGRP',TC,RUN,'TradeDeficit') = sum(C,PWM0(C,'rest')*QMX(C,'rest',XC,TC,'2050')-PWE0(C,'rest')*QEX(C,'rest',XC,TC,'2050'));

*The Palma ratio is defined as the ratio of the richest 10% of the population's
*share of gross national income divided by the poorest 40%'s share. It is
*based on the work of Chilean economist Gabriel Palma who found that middle
*class incomes almost always represent about half of gross national income
*while the other half is split between the richest 10% and poorest 40%, but the
*share of those two groups varies considerably across countries. The Palma ratio
*addresses the Gini index's over-sensitivity to changes in the middle of the
*distribution and insensitivity to changes at the top and bottom and therefore
*more accurately reflects income inequality's economic impacts on society as a
*whole. Palma has suggested that distributional politics pertains mainly to the
*struggle between the rich and poor, and who the middle classes side with.

 Report('OtherEconIndicators','ACTGRP',TC,RUN,'PalmaRatio')
         = (YIX('hhd-9',XC,TC,'2050'))
          /(YIX('hhd-0',XC,TC,'2050')+YIX('hhd-1',XC,TC,'2050')
           +YIX('hhd-2',XC,TC,'2050')+YIX('hhd-3',XC,TC,'2050'));

*The 20:20 or 20/20 ratio compares how much richer the top 20% of populations
*are to the bottom 20% of a given population. This can be more revealing of the
*actual impact of inequality in a population, as it reduces the effect on the
*statistics of outliers at the top and bottom and prevents the middle 60% from
*statistically obscuring inequality that is otherwise obvious in the field. The
*measure is used for the United Nations Development Programme Human Development
*Indicators. Some believe the 20:20 ratio is a more useful measure as it
*correlates well with measures of human development and social stability
*including the index of child well-being,[5] index of health and social problems,
*population in prison, physical health, mental health and many others.

 Report('OtherEconIndicators','ACTGRP',TC,RUN,'20-20Ratio')
         = (YIX('hhd-9',XC,TC,'2050')+YIX('hhd-8',XC,TC,'2050'))
          /(YIX('hhd-0',XC,TC,'2050')+YIX('hhd-1',XC,TC,'2050'));



 GOVTAB('DIRTAX',XC,TC,TT) = SUM(INSDNG, TINSX(INSDNG,XC,TC,TT)*YIX(INSDNG,XC,TC,TT));
 GOVTAB('FACTAX',XC,TC,TT) = SUM(F, tfx(F,XC,TC,TT)*YFX(F,XC,TC,TT)) ;
 GOVTAB('FACINC',XC,TC,TT) = SUM(F, YIFX('GOV',F,XC,TC,TT));
 GOVTAB('TRNSFR',XC,TC,TT) =  trnsfrx('GOV','ROW',XC,TC,TT)*EXRX(XC,TC,TT);
 GOVTAB('ACTTAX',XC,TC,TT) = SUM((RD,A), TAX(A,RD,XC,TC,TT)*PARX(A,RD,XC,TC,TT)*QARX(A,RD,XC,TC,TT))  ;
 GOVTAB('VATAX',XC,TC,TT)  = SUM((A,RD), TVAX(A,RD,XC,TC,TT)*PVAX(A,RD,XC,TC,TT)*QVAX(A,RD,XC,TC,TT)) ;
 GOVTAB('IMPTAX',XC,TC,TT) = SUM((RW,CM), TMX(CM,RW,XC,TC,TT)*QMX(CM,RW,XC,TC,TT)*PWMX(CM,RW,XC,TC,TT))*EXRX(XC,TC,TT);
 GOVTAB('EXPTAX',XC,TC,TT) = SUM((RW,CE), te(CE,RW)*QEX(CE,RW,XC,TC,TT)*PWEX(CE,RW,XC,TC,TT))*EXRX(XC,TC,TT);
 GOVTAB('COMTAX',XC,TC,TT) = SUM(C, TQX(C,XC,TC,TT)*PQX(C,XC,TC,TT)*QQX(C,XC,TC,TT))  ;
 GOVTAB('CO2D',XC,TC,TT)   = SUM(C, tco2dx(XC,TC,TT)*co2cx(C,XC,TC,TT)*QQX(C,XC,TC,TT)) ;
 GOVTAB('CO2M',XC,TC,TT)   = SUM((C,RW), tco2mx(XC,TC,TT)*co2mx(C,RW,XC,TC,TT)*QMX(C,RW,XC,TC,TT)) ;
 GOVTAB('CO2E',XC,TC,TT)   =-SUM((C,RW), tco2ex(XC,TC,TT)*co2ex(C,RW,XC,TC,TT)*QEX(C,RW,XC,TC,TT))  ;
 GOVTAB('UTAX',XC,TC,TT)   = SUM((C,A,RD), tui(C,A,RD)*PQX(C,XC,TC,TT)*QINTAX(A,RD,XC,TC,TT)*icax(C,A,RD,XC,TC,TT))
                              + SUM((C,H), tuh(C,H)*PQX(C,XC,TC,TT)*QHX(C,H,XC,TC,TT)) ;