




*For capital that is not entirely fixed
 CAPSHR1TOT              = SUM((FCAP,A,RD)$ANFX(A,RD), QF.L(FCAP,A,RD));
 CAPSHR1(FCAP)           = SUM((RD,A)$ANFX(A,RD), QF.L(FCAP,A,RD))/CAPSHR1TOT;
 CAPSHR2TOT(FCAP)        = SUM((RD,A)$ANFX(A,RD), QF.L(FCAP,A,RD));
 CAPSHR2(FCAP,A,RD)$(ANFX(A,RD) AND CAPSHR2TOT(FCAP)) = QF.L(FCAP,A,RD)/CAPSHR2TOT(FCAP);

IF (KOPTION EQ 1,
*Define capital composition BMAT and price of capital PKAP.
*Assume BMAT composition is the same for all kinds of capital
 Abort "must use koption2 for linked energy model CGE sims";
 BMAT(C,FCAP)            = QINV.L(C,'S-I')/SUM(CP, QINV.L(CP,'S-I')) ;
 PKAP(FCAP)              = SUM(C, PQ.L(C)*BMAT(C,FCAP)) ;
 GFCF                    = SUM(C, PQ.L(C)*QINV.L(C,'S-I')) ;
 QINVK                   = GFCF/(SUM(FCAP, PKAP(FCAP))/CARD(FCAP)) ;
 NGFCF                   = QINVK - SUM((RD,A,FCAPP)$AFX(A,RD), QF.L(FCAPP,A,RD)*(AFXGR(A,RD,XC,TC) + natdrate));
);

IF (KOPTION EQ 2,
*Targeted capital stock growth rate
*CA deflates GFCF by an investment price index
         GFCF     = SUM(C, PQ.L(C)*QINV.L(C,'S-I'))/SUM(C$PQ0(C), iwts(C,'S-I')*PQ.L(C)/PQ0(C)) ;
*        GFCF     = SUM(C, PQ.L(C)*QINV.L(C,'S-I')) ;
         QINVK    = GFCF / (alphainv*SUM(C, iwts(C,'S-I')*PQ0(C)));
*        NGFCF    = QINVK - SUM((RD,A,FCAPP)$AFX(A,RD), QF.L(FCAPP,A,RD)*(AFXGR(A,RD,XC,TC) + natdrate));
*CA uses invesment costs from times for energy capital
         IF(EFX(TT,TC),
*Remove fixed growth for non-energy sectors
                 NGFCF = QINVK - SUM((RD,A)$AFX(A,RD), QF.L('fcap',A,RD)*(AFXGR(A,RD,XC,TC) + natdrate));
* Remove TIMES imposed real energy investment cost (deflator is first term in denominator and price of capital is second)
* Note that depreciation is implicityly contained in EINVCOST from TIMES
                 EINVQINV(XC,TC,TT) =  EINVCOST(XC,TC-1,TT)/(SUM(C$PQ0(C), iwts(C,'S-I')*PQ.L(C)/PQ0(C))*(alphainv*SUM(C, iwts(C,'S-I')*PQ0(C)))) ;
                 NGFCF = NGFCF -  EINVQINV(XC,TC,TT);
         ELSE
                 NGFCF = QINVK - SUM((RD,A,FCAPP)$AFX(A,RD), QF.L(FCAPP,A,RD)*(AFXGR(A,RD,XC,TC) + natdrate));
         );
);

 WFADJ(FCAP)$CAPSHR2TOT(FCAP) = SUM((RD,A)$ANFX(A,RD), WF.L(FCAP)*WFDIST.L(FCAP,A,RD)*QF.L(FCAP,A,RD))/SUM((RD,A)$ANFX(A,RD), QF.L(FCAP,A,RD));
 WFDISTADJ(FCAP,A,RD)$(ANFX(A,RD) AND WFADJ(FCAP)) = WF.L(FCAP)*WFDIST.L(FCAP,A,RD)/WFADJ(FCAP);
 WFK2AV(FCAP)            = SUM((RD,A)$ANFX(A,RD), WFADJ(FCAP)*WFDISTADJ(FCAP,A,RD)*CAPSHR2(FCAP,A,RD));
 WFDIST2(FCAP,A,RD)$(ANFX(A,RD) AND WFK2AV(FCAP))   = WFADJ(FCAP)*WFDISTADJ(FCAP,A,RD)/WFK2AV(FCAP);
 WFK1AV                  = SUM(FCAPP, WFK2AV(FCAPP)*CAPSHR1(FCAPP));
 INVSHR1(FCAP)           = CAPSHR1(FCAP) * (1 + beta1 * ( (WFK2AV(FCAP)-WFK1AV) / WFK1AV ) ) ;
 INVSHR2(FCAP,A,RD)$ANFX(A,RD) = CAPSHR2(FCAP,A,RD)*(1 + beta2(FCAP)*(WFDIST2(FCAP,A,RD) - 1)) ;
 DKAPS(FCAP)             = INVSHR1(FCAP)*NGFCF;
 DKAP(FCAP,A,RD)$ANFX(A,RD)      = INVSHR2(FCAP,A,RD)*DKAPS(FCAP) ;
 RKAPS(FCAP)             = DKAPS(FCAP)/(QFS.L(FCAP)+QFS_FOR.L(FCAP)) - SUM((RD,A), natdrate*QF.L(FCAP,A,RD)/SUM((RDP,AP), QF.L(FCAP,AP,RDP))) ;

*CA Note that rkaps(fcap) is not used in our approach

**##############################################################################
*Some special handling for case of lower limits on wfdist for capital
*Above relies on capital employed whereas below switches to capital installed
**#############################################################################

*CA Alter to handle complementarity condition on capital (minimum wfdist)
 QF.L('FEGY','AELEC','NAT')$(QF.L('FEGY','AELEC','NAT') LT QFS.UP('FEGY')) = QFS.UP('FEGY');
* QF.L('FSAS','APETR','NAT')$(QF.L('FSAS','APETR','NAT') LT QFS.UP('FSAS')) = QFS.UP('FSAS');

 RKAP(FCAP,A,RD)$(ANFX(A,RD) AND QF0(FCAP,A,RD)) = DKAP(FCAP,A,RD)/QF.L(FCAP,A,RD) - natdrate;

*For capital that is entirely fixed
 RKAP(FCAP,A,RD)$(AFX(A,RD) AND QF0(FCAP,A,RD))   = AFXGR(A,RD,XC,TC);

*Apply annual growth rates over gaps
 QF.L(FCAP,A,RD)$QF0(FCAP,A,RD) = QF.L(FCAP,A,RD)*(1 + RKAP(FCAP,A,RD));

*bm turning off domestic crude when existing GTL plant shuts down
* QF.L(F,'ACOIL','NAT')$(NOT TVACT(TC,'UGTLGICCO-E')) = QF.L(F,'ACOIL','NAT')*0.5;
* QA.L('ACOIL')$(NOT TVACT(TC,'UGTLGICCO-E')) = QA.L('ACOIL')*0.5;
* QINT.L(C,'ACOIL')$(NOT TVACT(TC,'UGTLGICCO-E')) = QINT.L(C,'ACOIL')*0.5;

 QFBAR('FCAP',A,RD)$QF0('FCAP',A,RD) = QF.L('FCAP',A,RD);

 ALPHAQF.L=1;
