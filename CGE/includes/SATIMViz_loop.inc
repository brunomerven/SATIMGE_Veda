 EReport('GVA',A,T,'N')
         = QVAX(A,'NAT','BASE',T,'2050')*PVA0(A,'NAT');

 EReport('Employment',A,T,FLAB)
         = QFX(FLAB,A,'NAT','BASE',T,'2050');

 EReport('Exports','N',T,C)
         = QEX(C,'REST','BASE',T,'2050')*PE0(C,'REST');

 EReport('Consumption','N',T,H)
         = EHX(H,'BASE',T,'2050');

 EReport('Income','N',T,H)
         = YIX(H,'BASE',T,'2050');

*-------------------------------------------------------------------------------
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

 EReport('20-20 Ratio','N',T,'N')
         = (YIX('hhd-9','BASE',T,'2050')+YIX('hhd-8','BASE',T,'2050'))
          /(YIX('hhd-0','BASE',T,'2050')+YIX('hhd-1','BASE',T,'2050'));
*-------------------------------------------------------------------------------
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

 EReport('Palma Ratio','N',T,'N')
         = (YIX('hhd-9','BASE',T,'2050'))
          /(YIX('hhd-0','BASE',T,'2050')+YIX('hhd-1','BASE',T,'2050')
           +YIX('hhd-2','BASE',T,'2050')+YIX('hhd-3','BASE',T,'2050'));
*-------------------------------------------------------------------------------
 EReport('Population','N',T,'hhd-0')
         = POPX('xxhhd-0','BASE',T,'2050');
 EReport('Population','N',T,'hhd-1')
         = POPX('xxhhd-1','BASE',T,'2050');
 EReport('Population','N',T,'hhd-2')
         = POPX('xxhhd-2','BASE',T,'2050');
 EReport('Population','N',T,'hhd-3')
         = POPX('xxhhd-3','BASE',T,'2050');
 EReport('Population','N',T,'hhd-4')
         = POPX('xxhhd-4','BASE',T,'2050');
 EReport('Population','N',T,'hhd-5')
         = POPX('xxhhd-5','BASE',T,'2050');
 EReport('Population','N',T,'hhd-6')
         = POPX('xxhhd-6','BASE',T,'2050');
 EReport('Population','N',T,'hhd-7')
         = POPX('xxhhd-7','BASE',T,'2050');
 EReport('Population','N',T,'hhd-8')
         = POPX('xxhhd-8','BASE',T,'2050');
 EReport('Population','N',T,'hhd-9')
         = POPX('xxhhd-9','BASE',T,'2050');

 EReport('GDP per capita','N',T,'N')
         =  (sum(A,EReport('GVA',A,T,'N'))/(sum(H,EReport('Population','N',T,H))/1000))*1000;
*-------------------------------------------------------------------------------

 execute_unload "EREPORT.gdx" EReport
 execute 'gdxdump EREPORT.gdx output=EREPORT_00.csv symb=EREPORT format=csv header=Indicator,Activity,Year,Other,Value';

 execute_unload "RMFSA.gdx" MFSA
 execute 'gdxdump RMFSA.gdx output=RMFSA.csv symb=MFSA format=csv header="FS,Activity"';

 execute_unload "RMFHH.gdx" MFHH
 execute 'gdxdump RMFHH.gdx output=RMFHH.csv symb=MFHH format=csv header="FH,H"';

