*******************************************************************************
* FILLPARM : Interpolation/extrapolation of user data
* Description: Default interpolation/extrapolation if no control option given
*              Non-default interpolation/extrapolation according to user option
*              DFUNC = -1-> no interpolation, 0-> default action,
*              DFUNC = +1-> interp., 2->interp.+EPS, 3->interp.+extrap.
*              DFUNC >999 -> exponential interpolation beyond year DFUNC
* Parameters:
*      %1 - table name
*      %2 - control set 1
*      %3 - control set 2
*      %4 - UNCD7 residual dimension
*      %5 - MODLYEAR or MILESTONYR depending on parameter
*      %6 - RTP controlling the assignment to the MODLYEARs
*      %7 - Selective test for control option (normally GE 0)
*      %8 - Optional name for temporary write cache
*******************************************************************************
*$ONLISTING
$EOLCOM !
$SETLOCAL DATA '%1(%2,%5,%3)' SETLOCAL ITEM '%1(%2,%5,%3)' SETLOCAL ADD ''
*DFUNC=3;
OPTION CLEAR = UNCD7;
LOOP(DATAYEAR,
  UNCD7(%2,%3%4)$%1(%2,DATAYEAR,%3) = YES;

);

LOOP(UNCD7(%2,%3%4),
  OPTION CLEAR=MY_FIL2;
*     CNT = (DFUNC LE 999);
  MY_ARRAY(DM_YEAR) = %1(%2,DM_YEAR,%3);

*bm added this line here so that sudden steps are smoothed out
  MY_ARRAY(MILESTONYR)$(MY_ARRAY(MILESTONYR)=0) = eps;

  LAST_VAL=0; FINT = 0; Z = 0;
* do interpolate
  LOOP(DM_YEAR$MY_ARRAY(DM_YEAR),                    ! check for nonzero (including EPS)
    MY_F = MY_ARRAY(DM_YEAR);
    Z = YEARVALT(DM_YEAR);
    IF(LAST_VAL,
      %ITEM%$((Z GT YEARVALT(%5))$(YEARVALT(%5) GT MY_FYEAR))
             = LAST_VAL + (MY_F-LAST_VAL)/(Z-MY_FYEAR)*(YEARVALT(%5)-MY_FYEAR); ! not the first one
    ELSE
      FINT = Z;
      FIRST_VAL = MY_F;
    );
    LAST_VAL = MY_F;
    MY_FYEAR=Z;
  );        ! remember the value and year

*  %DATA%$%6 $= FIRST_VAL$(YEARVALT(%5) LT FINT) %ADD% + LAST_VAL$(YEARVALT(%5) GT Z);

);

*bm added this line here to take out the eps that we put in above
%ITEM%$(%ITEM%=eps) = 0;

$OFFLISTING
