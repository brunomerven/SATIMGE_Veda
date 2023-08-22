* code to translate budget share algorithm from spreadsheet: PrivateTransport_SATIMGE_Prototype.xlsx

* Calculate estimated price change from ica change
* this is done after ica is updated

PQest('cprtr',TC) = sum(C,ica(C,'aprtr','nat')*PQ.L(C));
BudShareFullRebound('cprtr',H,TC) = BudShare0('cprtr',H,TC-1);
BudShareNoRebound('cprtr',H,TC) = BudShare0('cprtr',H,TC-1)*PQest('cprtr',TC)/PQ.L('cprtr');



BudShare0('cprtr',H,TC) = BudShareFullRebound('cprtr',H,TC)*Rebound(TC)+BudShareNoRebound('cprtr',H,TC)*(1-Rebound(TC));


* Temporary fix for electricity by households, which is adjusted to track SATIM, until a full end-use model in eSAGE is implemented
BudShare0('celec',H,TC)$PQX('celec',XC,TC-2,TT) = BudShare0('celec',H,TC-1)*sum(FH,FLOW_IN_FSFuels(FH,'celec',TC-1))/sum(FH,FLOW_IN_FSFuels(FH,'celec',TC))*PQX('celec','base',TC-1,TT)/PQX('celec',XC,TC-2,TT);

BudShareNonEnergy(H,TC) =  1-sum(CHEnergy,BudShare0(CHEnergy,H,TC));

BudShare0(CHNonEnergy,H,TC)$BudShareNonEnergy(H,TC-1) = BudShare0(CHNonEnergy,H,TC-1)/BudShareNonEnergy(H,TC-1)*BudShareNonEnergy(H,TC);

