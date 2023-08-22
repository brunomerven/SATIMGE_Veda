* code to recalculate budget share based on income
* Bruno Merven with help from Faaiqa Hartley and Sherman Robinson, 2019
LOOP(HP,
    DistanceMatrix(H,HP,TC) = round(YI.L(H) - YI0(HP),2)+0.00001;
);

TmpLowerNeighbour(H,HP,TC)$(DistanceMatrix(H,HP,TC) GE 0) = DistanceMatrix(H,HP,TC);
TmpLowerNeighbour(H,HP,TC)$(DistanceMatrix(H,HP,TC) LT 0) = 9999;

TmpUpperNeighbour(H,HP,TC)$(DistanceMatrix(H,HP,TC) LE 0) = -1 * DistanceMatrix(H,HP,TC);
TmpUpperNeighbour(H,HP,TC)$(DistanceMatrix(H,HP,TC) GT 0) = 9999;


LOOP(H,
    TmpMin(H,TC) = smin(HP, TmpLowerNeighbour(H,HP,TC));
    CLN(H,HP,TC)$(TmpMin(H,TC) eq TmpLowerNeighbour(H,HP,TC)) = yes;
    DistLower(H,TC) = sum(HP$CLN(H,HP,TC), TmpLowerNeighbour(H,HP,TC));

    TmpMin(H,TC) = smin(HP, TmpUpperNeighbour(H,HP,TC));
    TmpMin(H,TC)$(TmpMin(H,TC) eq 9999) = 0;
    CUN(H,'hhd-9',TC)$(TmpMin(H,TC) eq 0) = yes;
    CUN(H,HP,TC)$(TmpMin(H,TC) eq TmpUpperNeighbour(H,HP,TC)) = yes;
    DistUpper(H,TC) = sum(HP$CUN(H,HP,TC), TmpUpperNeighbour(H,HP,TC));

    TotalDistance(H,TC) = DistLower(H,TC) + DistUpper(H,TC);

*    WeightLower(H,TC)$(NOT DistUpper(H,TC)) = 0;
*    WeightUpper(H,TC)$(NOT DistUpper(H,TC)) = 1;

*    WeightLower(H,TC)$DistUpper(H,TC) = 1 - DistLower(H,TC)/TotalDistance(H,TC);
*    WeightUpper(H,TC)$DistUpper(H,TC) = 1 - DistUpper(H,TC)/TotalDistance(H,TC);

    WeightLower(H,TC)$DistUpper(H,TC) = DistUpper(H,TC)/TotalDistance(H,TC);
    WeightUpper(H,TC) = 1 - WeightLower(H,TC);



    BudShare(C,H,TC) = WeightLower(H,TC) * sum(HP$CLN(H,HP,TC), BudShare0(C,HP,TC)) + WeightUpper(H,TC) * sum(HP$CUN(H,HP,TC), BudShare0(C,HP,TC));
*    BudShare(C,H,TC) = WeightLower(H,TC) * sum(HP$CLN(H,HP,TC), BUDSHR(C,HP)) + WeightUpper(H,TC) * sum(HP$CUN(H,HP,TC), BUDSHR(C,HP));


*uncomment / comment below to enforce / not enforce budget share calc in model
    betam(C,H) = BudShare(C,H,TC);

);


