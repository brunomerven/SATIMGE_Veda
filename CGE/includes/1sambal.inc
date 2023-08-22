
$ontext
1SAMBAL.INC from CGE model code

SR: Revised October 2016

This SAM-balancing program is intended for the removal of rounding
errors, not for estimating SAMs with significant imbalances. For an
approach to SAM estimation that is appropriate in such contexts, see:

Robinson, Sherman and Moataz El-Said. "GAMS Code for Estimating a Social
Accounting Matrix (SAM) Using Cross Entropy Methods (CE)" Discussion
Paper 64, Trade and Macroeconomics Division, IFPRI, December 2000.

Robinson, Sherman, Andrea Cattaneo, and Moataz El-Said. "Updating and
Estimating a Social Account Matrix Using Cross Entropy Methods."
Economic Systems Research, Vol. 13, No. 1, 2001, pp. 47-64.
Also available as Discussion Paper 58, Trade and Macroeconomics Division,
IFPRI, August 2000.

In this file:

--the user has the option of removing small cells (with absolute values
below a cutoff point).
--the row/column balance of the SAM is checked.
--a SAM-balancing program is activated if the account imbalances in the SAM
exceed a cut-off point.
--each non-zero cell in the SAM is assumed to be measured with error. The
error is expressed as a probability-weighted average of a support set, with
a prior mean (zero) and standard deviation (sigmac).
--The column sums are also assumed to be measured with error.
--the minimand is the cross-entropy (CE) distance between the estimated
probability weights and the prior.

$offtext

*CHECKING SAM BALANCE AND REMOVING SMALL CELLS=========================

 SET
  SMLCELL(AC,ACP)       SAM cells with abs value < cutoff are removed
 ;
 PARAMETER SAMBALCHK(ac) ;

 SCALAR
  CUTOFF   lower bound on absolute cell values           /0.0/
  delta     epsilon value for cross-entropy minimand     /.00001 /
 ;
*If the user does not want to remove small absolute values, then CUTOFF
*should be set at zero.
*If the user wants to remove small absolute values then CUTOFF should
*be set at a low positive value (e.g. 0.005).

 SAM('TOTAL',ACP) = 0;
 SAM(AC,'TOTAL')  = 0;

*ACNT-ACNTP is an element in the set SMLCELL if
*(1) they have a non-zero SAM cell; and
*(2) the absolute value of this cell is less than cutoff
 SMLCELL(ACNT,ACNTP)$(SAM(ACNT,ACNTP)$(ABS(SAM(ACNT,ACNTP)) LT cutoff))=YES;

*The cells corresponding to elements in SMLCELL are set at zero.
 SAM(ACNT,ACNTP)$SMLCELL(ACNT,ACNTP) = 0;

 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));
 SAMBALCHK(ACNT)   = SAM('TOTAL',ACNT) - SAM(ACNT,'TOTAL');

*DISPLAY "After excluding small cells", SAMBALCHK, SMLCELL;
*DISPLAY "After excluding small cells", SAM ;

SETS
 ICOEFF(AC,ACP)   Cells to include in adjustment process
 ;

*initialize as yes, then exclude cells
 ICOEFF(ACNT,ACNTP)     = YES ;

 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));
 SAMBALCHK(ACNT)   = SAM('TOTAL',ACNT) - SAM(ACNT,'TOTAL');
 SAMBALCHK(acnt)$(ABS(SAMBALCHK(acnt)) lt 1e-6) = 0 ;

 DISPLAY "SAMBALCHK in 1SamBal.inc before adjustment", SAMBALCHK ;
*DISPLAY "After excluding zero sum rows", SAM, IROW2 ;


*SAM BALANCING PROGRAM==========================================
*This program assures that the SAM is perfectly balanced. Put an
*asterisk before the solve statement if you do not want to run it.
*Currently it runs if the sum of the absolute values of the account
*imbalances exceed an upper limit close to zero.

PARAMETER
 SUMABSDEV  sum of absolute values of column-row imbalances;
 SUMABSDEV = SUM(ACNT, ABS(SAMBALCHK(ACNT)));

PARAMETER
 COLSUM0(AC)      Initial column sums
 TSAM0(AC,ACP)    Initial SAM
 COLTARG(AC)      Target column sum
 ;

 COLSUM0(ACNT)        = SUM(ACNTP, SAM(ACNTP,ACNT)) ;
 TSAM0(acnt,acntp)    = SAM(acnt,acntp) ;
 COLTARG(acnt)        = 0.5*(COLSUM0(acnt) + SUM(acntp, TSAM0(acnt,acntp))) ;

*Exclude all cells which are zero.
 ICOEFF(ACNT,ACNTP)$(SAM(ACNT,ACNTP) EQ 0) = NO ;

*Establish prior errors on all cells, positive or negative
 set jwt   number of elements in error support set /1*3 / ;
 Parameter sigmac(ac,acp)      standard deviation of cell values
           vbarc(ac,acp,jwt)   support set for cell errors
           wbarc(ac,acp,jwt)   prior on cell error probabilities
           sigma2(ac)          standard deviation of column sum
           vbar2(ac,jwt)       support set for column errors
           wbar2(ac,jwt)       prior on column error probabilities
 ;
* Set constants for 3-weight error distribution.
* Set prior error standard deviation for cell values
* The prior error distribution is assumed Normal(0,sigmac)
 parameter stderr  standard error of measurement /.5/ ;

 sigmac(acnt,acntp)$icoeff(acnt,acntp)     = stderr*abs(TSAM0(acnt,acntp)) ;
 sigma2(acnt)                              = stderr*abs(coltarg(acnt)) ;

 vbarc(acnt,acntp,"1")$icoeff(acnt,acntp)  = -3*sigmac(acnt,acntp);
 vbarc(acnt,acntp,"2")$icoeff(acnt,acntp)  =  0  ;
 vbarc(acnt,acntp,"3")$icoeff(acnt,acntp)  = +3*sigmac(acnt,acntp);

 wbarc(acnt,acntp,"1")$icoeff(acnt,acntp)  =  1/18  ;
 wbarc(acnt,acntp,"2")$icoeff(acnt,acntp)  = 16/18  ;
 wbarc(acnt,acntp,"3")$icoeff(acnt,acntp)  =  1/18  ;

 vbar2(acnt,"1")  = -3*sigma2(acnt);
 vbar2(acnt,"2")  =  0  ;
 vbar2(acnt,"3")  = +3*sigma2(acnt);

 wbar2(acnt,"1")  =  1/18  ;
 wbar2(acnt,"2")  = 16/18  ;
 wbar2(acnt,"3")  =  1/18  ;

VARIABLES
 CNTRPY         Cross entropy measure
 TSAM(AC,ACP)   scaled SAM coefficients
 WC(AC,ACP,jwt) Probability weights on error terms
 ERRC(AC,ACP)   error in cell
 W2(AC,jwt)     probability weights for column sums
 ERR2(AC)       error in column sums
 ;

*Initialize variables
 TSAM.L(acnt,acntp)   = SAM(acnt,acntp) ;
 CNTRPY.L             = 0 ;
 WC.L(ACNT,ACNTP,jwt) = wbarc(acnt,acntp,jwt) ;
 ERRC.L(ACNT,ACNTP)   = 0 ;
 W2.L(acnt,jwt)       = wbar2(acnt,jwt) ;
 ERR2.L(acnt)         = 0 ;

EQUATIONS
 ENTROPYEQ           Cross entropy objective function
 WSUMEQ(AC,ACP)      Sum of error weights
 ERROREQ(AC,ACP)     cell value error equation
 WSUMEQ2(AC)         Sum of error weights
 ERROREQ2(AC)        column sum error equation
 SAMEQ(ac,acp)       SAM cells
 COLROWBAL           column total equals row total
 COLSUMEQ            Column sums
 ;

 ENTROPYEQ..
$ONTEXT
  CNTRPY   =E= SUM((ACNT,ACNTP,jwt)$ICOEFF(acnt,acntp),
                   WC(acnt,acntp,jwt)*(log(WC(acnt,acntp,jwt) + delta)
                   - log(wbarc(acnt,acntp,jwt) + delta))) +
               SUM((acnt,jwt),
                   W2(acnt,jwt)*(log(W2(acnt,jwt) + delta)
                   - log(wbar2(acnt,jwt) + delta))) ;
$OFFTEXT

  CNTRPY   =E= SUM[(ACNT,ACNTP,jwt)$ICOEFF(acnt,acntp),
                   CENTROPY(WC(acnt,acntp,jwt),wbarc(acnt,acntp,jwt))] +
               SUM[(acnt,jwt),
                   CENTROPY(W2(acnt,jwt),wbar2(acnt,jwt))] ;

 WSUMEQ(ACNT,ACNTP)$ICOEFF(ACNT,ACNTP)..
  SUM(jwt, WC(acnt,acntp,jwt)) =E= 1 ;

 WSUMEQ2(ACNT)..
  SUM(jwt, W2(acnt,jwt)) =E= 1 ;

 ERROREQ(ACNT,ACNTP)$ICOEFF(ACNT,ACNTP)..
  ERRC(ACNT,ACNTP) =E= SUM(jwt, WC(acnt,acntp,jwt)*vbarc(acnt,acntp,jwt)) ;

 ERROREQ2(ACNT)..
  ERR2(ACNT) =E= SUM(jwt, W2(acnt,jwt)*vbar2(acnt,jwt)) ;

 SAMEQ(acnt,acntp)$ICOEFF(acnt,acntp)..
  TSAM(acnt,acntp) =E= TSAM0(acnt,acntp) + ERRC(acnt,acntp) ;

 COLROWBAL(ACNT)..
  SUM(ACNTP, TSAM(ACNTP,ACNT)) =E=
  SUM(ACNTP, TSAM(ACNT,ACNTP)) ;

 COLSUMEQ(ACNT)..
  SUM(ACNTP, TSAM(ACNTP,ACNT)) =E= COLTARG(ACNT) + ERR2(ACNT) ;

 MODEL
  SAMBAL SAM balancing program for removing rounding errors
  /
   ENTROPYEQ
   WSUMEQ
   ERROREQ
   WSUMEQ2
   ERROREQ2
   SAMEQ
   COLROWBAL
   COLSUMEQ
  /;


*Fix cells not to be changed.
*After balancing, no unfixed positive cell will have value less than cutoff.
 TSAM.FX(acnt,acntp)$(NOT ICOEFF(acnt,acntp)) = TSAM0(acnt,acntp) ;

*Set lower bound of cutoff on positive cells
 TSAM.LO(ACNT,ACNTP)$(TSAM0(acnt,acntp) gt 0) = cutoff ;

*Set lower bound on weights. The constraint is, in fact, redundant.
 WC.LO(ACNT,ACNTP,jwt)$ICOEFF(ACNT,ACNTP)       = 0 ;
 WC.UP(acnt,acntp,jwt)$ICOEFF(ACNT,ACNTP)       = 1 ;
 W2.LO(acnt,jwt)                                = 0 ;
 W2.UP(acnt,jwt)                                = 1 ;


 OPTION LIMROW=100, LIMCOL=0;
 SAMBAL.HOLDFIXED = 1;
 SAMBAL.TOLINFREP = .0001 ;

*OPTION NLP=PATHNLP;
*OPTION NLP=CONOPT;
 OPTION NLP=IPOPT;

 IF(SUMABSDEV GT 1E-6,
  SOLVE SAMBAL MINIMIZING CNTRPY USING NLP;
 );

*Reload new SAM

 SAM(ACNT,ACNTP)   = TSAM.L(ACNT,ACNTP) ;

 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));
 SAMBALCHK(ACNT)   = SAM('TOTAL',ACNT) - SAM(ACNT,'TOTAL');
 SAMBALCHK(acnt)$(ABS(SAMBALCHK(acnt)) lt 1e-6) = 0 ;

*Compute percent changes in cells
$ONTEXT
 Parameter DEVP(ac,acp) Percent deviation ;

 DEVP(acnt,acntp)$tsam0(acnt,acntp) =
             100*((TSAM.L(acnt,acntp)/tsam0(acnt,acntp)) - 1) ;
 DEVP("total",acnt)$COLSUM0(ACNT) =
             100*(SUM(ACNTP, TSAM.L(ACNTP,ACNT))/COLSUM0(ACNT) - 1) ;

 option devp:6, errc:6 ;
 display devp ;
$OFFTEXT

 OPTION SAM:3 ;
 DISPLAY "Balanced SAM after SAMBAL", SAMBALCHK, SAM;
*DISPLAY "Balanced SAM after SAMBAL", SAM;


*#*#*#*#*# THE END OF 1SAMBAL.INC #*#*#*#*
