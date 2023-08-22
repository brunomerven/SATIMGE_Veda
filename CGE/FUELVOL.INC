Parameter
 ENETABLE(*,AC)
 ENEPTABLE(*,AC)
;

Set
 anfrm(a)       /anfrm/
 aaff(a)        /aawhe, aamai, aaoce, aaveg, aaofr, aagra, aaoil, aatub,
                 aapul, aasug, aaoth, alani, afore, afish/
 acom(a)        /awatr,atrad,ahotl,aprtr,atrps,acomm,afsrv,absrv,agsrv,aosrv/
 achm(a)        /abchm, aochm, arubb, aplas/
 afab(a)        /ameat,apfis,avege,afats,adair,agrai,astar,afeed,abake,
                 asuga,aconf,apast,aofoo,abevt/
 aoin(a)        /atext,aclth,aleat,afoot,awood,aprnt,ametp,amach,aemch,
                 asequ,avehi,atequ,afurn,aoman,acons,apapr/
 airo(a)        /airon/
 amin(a)        /acoal, agold, amore, amine, angas/
 anmm(a)        /anmet/
 apap(a)        /apapr/
 aelc(a)        /aelec/
 aref(a)        /apetr, ahydr/
 atrl_f(a)      /altrp-f/
 atrl_p(a)      /altrp-p/
 atra(a)        /aotrp-p, aotrp-f/
 h1(h)          /hhd-0, hhd-1, hhd-2, hhd-3, hhd-4/
 h2(h)          /hhd-5, hhd-6, hhd-7/
 h3(h)          /hhd-8, hhd-9/

 fuel(c)       /ccoal-low, ccoal-hgh, ccoil, cngas, chydr, cpetr-p, cpetr-d, cpetr-h, cpetr-k, cpetr-l,celec/
;

 ENETABLE('EXPORTS',FUEL)=QE.L(FUEL,'REST');
 ENETABLE('IMPORTS',FUEL)=QM.L(FUEL,'REST');
 ENETABLE('PRODUCTION',FUEL)=SUM(A,QXAC.L(A,FUEL));

 ENETABLE('anfrm',fuel)           =sum(anfrm,QINT.L(fuel,anfrm));
 ENETABLE('aaff',fuel)            =sum(aaff,QINT.L(fuel,aaff));
 ENETABLE('acom',fuel)            =sum(acom,QINT.L(fuel,acom));
 ENETABLE('achm',fuel)            =sum(achm,QINT.L(fuel,achm));
 ENETABLE('afab',fuel)            =sum(afab,QINT.L(fuel,afab));
 ENETABLE('aoin',fuel)            =sum(aoin,QINT.L(fuel,aoin));
 ENETABLE('airo',fuel)            =sum(airo,QINT.L(fuel,airo));
 ENETABLE('amin',fuel)            =sum(amin,QINT.L(fuel,amin));
 ENETABLE('anmm',fuel)            =sum(anmm,QINT.L(fuel,anmm));
 ENETABLE('apap',fuel)            =sum(apap,QINT.L(fuel,apap));
 ENETABLE('aelc',fuel)            =sum(aelc,QINT.L(fuel,aelc));
 ENETABLE('aref',fuel)            =sum(aref,QINT.L(fuel,aref));
 ENETABLE('atrl_f',fuel)          =sum(atrl_f,QINT.L(fuel,atrl_f));
 ENETABLE('atrl_p',fuel)          =sum(atrl_p,QINT.L(fuel,atrl_p));
 ENETABLE('atra',fuel)            =sum(atra,QINT.L(fuel,atra));

 ENETABLE('h1',fuel)              =sum(h1,QH.L(fuel,h1));
 ENETABLE('h2',fuel)              =sum(h2,QH.L(fuel,h2));
 ENETABLE('h3',fuel)              =sum(h3,QH.L(fuel,h3));

 ENEPTABLE('EXPORTS',fuel)        =PE.L(fuel,'rest')*1000;
 ENEPTABLE('IMPORTS',fuel)        =PM.L(fuel,'rest')*1000;
 ENEPTABLE('DEMAND',fuel)         =PQ.L(fuel)*1000;
 ENEPTABLE('PRODUCTION',fuel)     =SUM(A$MAC(A,FUEL),PXAC.L(A,FUEL))*1000;
