* This code is used to refresh the demstart.gdx file with values sitting in the Drivers.xlsm workbook
* This file is kept to ensure that all runs are initialised with the same demand.
* dem_start should be refreshed everytime there is a large departure in growth assumptions
* to speed up convergence

sets
AY /2019*2070/
FS
FH
H /hhd-0*hhd-9/

MFHHT(FH,H,AY) reverse mapping (TIMES to CGE) for households
;

parameters
GVA_FS(FS,AY)           GDP by SATIM sector for sim XC
QA_FS(FS,AY)            SATIM Sector QA (used for setting absolute levels for ica for new sectors)
QD_FS(FS,AY)            domestic demand to drive energy model
TFHPOP(FH,AY)          Population by SATIM income group for sim XC
POP(AY)                Total Population
YHE(AY) income of higher income group for SATIM
;
$call   "gdxxrw i=SetsAndMaps\SetsAndMaps.xlsm o=SetsAndMaps\SetsMaps index=index!a6 checkdate"
$gdxin  SetsAndMaps\SetsMaps.gdx
$load FS FH

$call "gdxxrw i=Drivers.xlsm o=drivers.gdx index=index_E2G!a6";
$gdxin drivers.gdx
$load GVA_FS POP YHE TFHPOP MFHHT QA_FS QD_FS


$GDXout dem_start.gdx
$unload GVA_FS POP YHE TFHPOP MFHHT QA_FS QD_FS
