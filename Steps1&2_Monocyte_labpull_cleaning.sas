/* Project: Monocyte

/* Description: pull Monocyte labs from CDW using SAS Pass Thru and will save the Monocyte lab pull table as a SAS dataset for further cleaning.
/* This code also creates a final lab value for each hospitalization*/

/* Date Created: 1/26/24
/* Author: Jennifer Cano (modified Shirley Wang's code)
*/

%let DATASRCs=database;

libname sasoutm 'filepath';

/*Step 1 - pull data from CDW using SAS Pass Thru*/

*A. Monocyte Count;

PROC SQL  ;
CONNECT TO SQLSVR AS TUNNEL (DATASRC=&DATASRCs. &SQL_OPTIMAL. )  ;

/*pull in labchemtestnames to send to PIs for review*/
EXECUTE (
  select LOINC, Component, Sta3n, LOINCSID 
  into #tempLOIN  
  from [CDWWork].[Dim].[loinc]
  where loinc in ('26484-6','743-5','742-7','30440-2', '29260-7', '34924-1', '33841-8')

)  BY TUNNEL  ;

CREATE TABLE labchemtestsmc AS
SELECT  *
  FROM CONNECTION TO TUNNEL ( 
/*pull in labchemtestnames to send to PIs for review*/
SELECT a.labchemtestsid, a.PatientSID, a.sta3n, a.LabChemSpecimenDateTime, a.LOINCSID,  c.labchemtestname 
FROM  [database].[Src].[Chem_PatientLabChem] AS A
INNER join #tempLOIN as b 
                on a.Loincsid=b.Loincsid 
LEFT JOIN [CDWWork].[Dim].[labchemtest] as c on a.labchemtestsid=c.labchemtestsid
WHERE (a.LabChemSpecimenDateTime >= '20130101' AND a.LabChemSpecimenDateTime < '20190101')
       and (a.LabChemResultNumericValue is NOT NULL)

);

DISCONNECT FROM TUNNEL ;
QUIT ;

*B. MDW -  NO RESULTS;

PROC SQL  ;
CONNECT TO SQLSVR AS TUNNEL (DATASRC=&DATASRCs. &SQL_OPTIMAL. )  ;

/*pull in labchemtestnames to send to PIs for review*/
EXECUTE (
  select LOINC, Component, Sta3n, LOINCSID 
  into #tempLOIN  
  from [CDWWork].[Dim].[loinc]
  where loinc in ('88880-0')

)  BY TUNNEL  ;

CREATE TABLE labchemtestsmdw AS
SELECT  *
  FROM CONNECTION TO TUNNEL ( 
/*pull in labchemtestnames to send to PIs for review*/
SELECT a.labchemtestsid, a.PatientSID, a.sta3n, a.LabChemSpecimenDateTime, a.LOINCSID,  c.labchemtestname 
FROM  [database].[Src].[Chem_PatientLabChem] AS A
INNER join #tempLOIN as b 
                on a.Loincsid=b.Loincsid 
LEFT JOIN [CDWWork].[Dim].[labchemtest] as c on a.labchemtestsid=c.labchemtestsid
WHERE (a.LabChemSpecimenDateTime >= '20130101' AND a.LabChemSpecimenDateTime < '20190101') 
       and (a.LabChemResultNumericValue is NOT NULL)

);

DISCONNECT FROM TUNNEL ;
QUIT ;

/*construct frequency table of labchemtestnames to send to PIs for review,*/

/*MC*/
proc sort data=labchemtestsmc nodupkey;
	by PatientSID labchemtestname LabChemSpecimenDateTime sta3n LOINCSID; 
	run;

proc freq data=labchemtestsmc order=freq; 
	table labchemtestname ;
run;


/*copy and paste frequency table into excel*/

*import LabChemTestNames PI's decided to keep;
proc import datafile="filepath"
out= Mon_LabNames_csv dbms=csv replace;
getnames=yes;
guessingrows=1234567;
run;

*extract all lab names to create macro list to copy into following code;
proc sql;
	select catt("'", labchemtestname, "'")
	into :Mon_LabNames_list separated by ','
	from Mon_LabNames_csv;
	quit;

%put &Mon_LabNames_list;


*Pull Labs;
PROC SQL  ;
CONNECT TO SQLSVR AS TUNNEL (DATASRC=&DATASRCs. &SQL_OPTIMAL. )  ;

/*pull in labchemtestnames associated with LOINC codes*/
EXECUTE (
  select LOINC, Component, Sta3n, LOINCSID 
  into #loinc  
  from [CDWWork].[Dim].[loinc]
  where loinc in ('26484-6','743-5','742-7','30440-2', '29260-7', '34924-1', '33841-8')

)  BY TUNNEL  ;

*pull in labchemtestnames following review from PIs;
EXECUTE (
/*pull in Labchemtest*/
SELECT labchemtestsid, LabChemTestName, LabChemPrintTestName, Sta3n, 1 as PIselectLabName
into #labtestnames
FROM  [CDWWork].[Dim].[LabChemTest]
WHERE labchemtestname in ('MONO #','MONOCYTES, ABSOLUTE','MO#','MONO#','MO #','ABSOLUTE MONOCYTE COUNT (AUTO)','MONOS ABSOLUTE#','MONOCYTE, ALTERNATE ABS','ABSOLUTE MONOCYTE COUNT','A MO#','MONOCYTES # (AUTO)','MONOS 
#','MONOCYTES (#)','MONOCYTE #','MONOS#','MONO ABSOLUTE CT.','MO#3','ABS MONOCYTE','MONOCYTE, ABSOLUTE (5/2/2019)','MONO, ABS','MONOCYTE(AUTO)','Monocytes (AUTO)','ABSOLUTE MONOCYTES','MONOCYTE 
NUMBER','MONO# (AUTO)','MONOCYTE # (AUTO)','Mo#','MONONUCL, ABSOLUTE AUTOMATED','A MONO #','ABSOLUTE MONO COUNT','MONO # (SS)','MONOS, ABSOLUTE','Mono(10e3)','MONOCYTE, ABS','ABS MONO','MONO 
ABS','ABS. MONOCYTE COUNT','TOTAL MONOCYTE','MO# (FV)','ABS MONO AUTOMATED','MONO #, ABSOLUTE','MONO #-AUTO---------O','MONOCYTE#','MONO ABS.','MONOS # (dc 10/11/22)','ABSOLUTE MONO#','ABS 
MONOS','MONO,ABS','MONO ABSOLUTE','A-MONO #','MONO-A','MONOCYTES #','MONOS # -','MONO, ABSOLUTE CT','ABSOLUTE MONOCYTE#','MONO#(CAL)','ABSOLUTE MONOCYTES # (MANUAL)','MONOS-ABSOLUTE','MONOCYTE, 
ABSOLUTE','MONOCYTES ABSOLUTE','MONOS #(D/C 5/25/17)','Absolute Monocytes','ABSOLUTE MONOCYTES(DCed 9-7-21)','MONOCYTE ABSOLUTE','MONOS ABSOLUTE #','Monocytes (Manual)','M MONO #','MONO MAN #','MO# 
AUTO','Mono, Abs','M MONO#','MO# (FS)*INACT(10/1/2020)','MONO,ABS(TOPC)AUTO','MONOCYTES, ABSOLUTE(M)','MONO # (AUTO)','MONO ABSOLUTE CALCULATED(WAM)','MONOCYTE, ALTENATE ABS','MONO# (MAN 
DIFF)','MO#4','MONOS','MONOCYTES #(M)','MONOCYTE %','MO# (MV)*INACT(1-1-15)','MONO #(s)','MONO ABS (DCED 072313)','PB MONO#','MN MID#','P-ABSOLUTE MONOS (DC''ed 4720)','MONOS (V2)','abs monocytes, 
lca','MO# (HR)*INACT(10-2-18)','REF-Monocytes Abs.','MO # (CD4/CD8)','BR-MO# AUTO','W-MONOCYTE#','MONOCYTES %(M)','MID#-PR','ZMH ABSOLUTE MONO BEFORE 1/28/14','MONOS#(BMT)','LRL MONOCYTE 
ABSOLUTE','MONOCYTES (ABS) (LABCORP)','ZZMONOS ABS. (Ref.Lab)','MONOCYTES % AUTOMATED','(IDM) MONO ABSOL','MONOS (FLUID)','ELD MO#','MONO ABS (LC) -','POC MO #','ZZZMONOCYTES ABSOLUTE 
(LC)','ZZZMONOCYTES % (LC)','M-MONO#-4','MONOCYTES ABS (LABCORP)','MONOCYTES # (MANUAL)','Monocytes Absolute (CDH)','ABS MONO MANUAL','LEG MONO#','MONOCYTES%(SPL)','MONO% (LC) 
-','MONOS#(LABCORP)','ZZ-MONOCYTES# QUEST(CBC)','EOSINO #','MONOCYTES %','#MONO (PB)','(FFTH) ABS,MONO','(STRONG) MONO ABSOL','ATYPICAL MONOCYTES','MV-MONO #','.MONOCYTES (ABSOLUTE)','COOK 
MONO#','MONOCYTES','Monos Abs. (LC)','ZZ-MONOCYTES(ABSOLUTE)LCA','.MONOS #-TMCB','TAMC MONO#','MONO,ABS d/c','.MONO ABS (OTHER VA)','MONOS-ABS(CD PANEL)')
) BY TUNNEL;

EXECUTE (
/*pull loincsids and labchemtestsids from CDW for 2013-2018*/
SELECT distinct a.LabChemSID, a.LabSubjectSID,  a.Sta3n, a.LabPanelIEN, a.LabPanelSID, a.LongAccessionNumberUID, a.ShortAccessionNumber,
       a.labchemtestsid, a.PatientSID, a.LabChemSpecimenDateTime, a.LabChemSpecimenDateSID, a.LabChemCompleteDateTime, a.LabChemCompleteDateSID,
       a.LabChemResultValue, a.LabChemResultNumericValue, a.TopographySID, a.LOINCSID, a.Units, a.RefHigh, a.RefLow, d.Topography
into #monocyte2013_2018
FROM  src.Chem_PatientLabChem AS A
INNER JOIN #loinc b on  a.Loincsid=b.Loincsid 
LEFT JOIN [CDWWork].[Dim].[topography] AS d ON A.TopographySID =D.TopographySID
	WHERE a.LabChemSpecimenDateTime >= '2013-01-01' and a.LabChemSpecimenDateTime < '2019-01-01'   

UNION

SELECT distinct a.LabChemSID, a.LabSubjectSID,  a.Sta3n, a.LabPanelIEN, a.LabPanelSID, a.LongAccessionNumberUID, a.ShortAccessionNumber,
       a.labchemtestsid, a.PatientSID, a.LabChemSpecimenDateTime, a.LabChemSpecimenDateSID, a.LabChemCompleteDateTime, a.LabChemCompleteDateSID,
       a.LabChemResultValue, a.LabChemResultNumericValue, a.TopographySID, a.LOINCSID, a.Units, a.RefHigh, a.RefLow, d.Topography
FROM src.Chem_PatientLabChem a         
INNER JOIN #labtestnames b ON a.labchemtestsid=b.labchemtestsid 
LEFT JOIN [CDWWork].[Dim].[topography] AS d ON A.TopographySID =D.TopographySID
     WHERE   
      a.LabChemSpecimenDateTime >= '2013-01-01' and a.LabChemSpecimenDateTime < '2019-01-01'  
) BY TUNNEL;

/*get unique PatientICN and save table as SAS data set*/
CREATE TABLE monocyte_2013_2018 AS 
SELECT  *
	FROM CONNECTION TO TUNNEL ( 
select distinct a.*, b.PatientICN
from #monocyte2013_2018 a
left join Src.SPatient_SPatient b on a.patientsid=b.PatientSID
);

DISCONNECT FROM TUNNEL ;
QUIT ;


/*Step 2 - cleaning */

/*remove duplicate labs by patient, facility, time of specimen and result*/
PROC SORT DATA=monocyte_2013_2018 nodupkey; 
BY PatientSID  Sta3n LabChemSpecimenDateTime LabChemResultNumericValue;
RUN;

/*create new year, datevalue, and time variable*/
data monocyte_2013_2018;
set monocyte_2013_2018;
LabSpecimenDate=datepart(LabChemSpecimenDateTime);
LabSpecimenTime=timepart(LabChemSpecimenDateTime);
year=year(LabSpecimenDate);
format LabSpecimenDate mmddyy10.;
format LabSpecimenTime time8.;
keep Sta3n year labchemtestsid PatientSID LabChemResultValue LabChemResultNumericValue TopographySID LOINCSID Units RefHigh RefLow Topography LabSpecimenDate LabSpecimenTime patienticn;
run;

data monocyte_2013_2018;
set monocyte_2013_2018;
Units2=upcase(units); /*turn all units into uppercase*/
units3=compress(Units2,'.'); /*removes '.' in units*/
clean_unit = compress(units3); /*removes all blanks (by default - specify options to remove other chars)*/
drop units2 units3 units ;
run;

/*change patienticn into numeric*/  
DATA monocyte_2013_2018 (rename=patienticn2=patienticn);
SET monocyte_2013_2018;
patienticn2 = input(patienticn, 10.);
year=year(LabSpecimenDate);
drop patienticn;
RUN;

/*check lab value missingness*/
proc sql;
select count(*)
from monocyte_2013_2018;
quit;
proc means data=monocyte_2013_2018 nmiss;
var LabChemResultNumericValue;
run;

*check which LabChemResultValues not missing when LabChemResultNumericValue is missing;
proc freq data=monocyte_2013_2018 order=freq;
tables LabChemResultValue;
where LabChemResultValue is not missing and LabChemResultNumericValue is missing;
run;
*majority of missing is 'comment' (137807);*257406 missing out of 17790667 obs (1.4%);

*don't need to recode any LabChemResultNumericValue's per PIs 1/30/24;

*Clean RefHigh and RefLow variables;
proc freq data=monocyte_2013_2018 order= freq;
tables RefHigh RefLow;

DATA monocyte_2013_2018;
SET monocyte_2013_2018;
RefHigh2=compress(RefHigh,'"'); /*removes '"' */
RefHigh_clean_cat = compress(RefHigh2); /*removes all blanks (by default - specify options to remove other chars)*/

RefLow2=compress(RefLow,'"'); /*removes '"' */
RefLow_clean_cat = compress(RefLow2); /*removes all blanks (by default - specify options to remove other chars)*/
drop RefHigh2 RefLow2 ;
run;

proc freq data=monocyte_2013_2018 order= freq;
tables RefHigh_clean_cat RefLow_clean_cat;
run; 

*don't need to recode any Ref values per PIs 1/30/24;

data sasoutm.mon;
set monocyte_2013_2018;
run;
proc contents data=sasoutm.mon;
run;

*check width and decimal places of numeric values for formatting in next step;
proc freq data=sasoutm.mon order= freq;
tables RefLow_clean_cat RefHigh_clean_cat;
run; 

*remove any characters from Refs and convert to numeric;
data monocyte_2013_2018;
set sasoutm.mon ;
RefLow_clean2 = compress(RefLow_clean_cat,'.' , 'kd'); *keep digits and decimal points;
RefLow_clean = input(RefLow_clean2, 4.);
RefHigh_clean2 = compress(RefHigh_clean_cat,'.' , 'kd');
RefHigh_clean = input(RefHigh_clean_cat, 5.);
drop RefLow_clean2 RefHigh_clean2;
run;

*make sure numeric formats/decimal places look correct;
proc univariate data=monocyte_2013_2018 ;
var RefLow_clean RefHigh_clean; 
run;

*Examine Topography and Units and create summary statistics;
proc sort data=monocyte_2013_2018 ;
by Topography clean_unit;
run;
*create table of summary stats of labs by topography and units;
proc means data=monocyte_2013_2018 n mean min p10 median p90 max;
class Topography clean_unit;
var LabChemResultNumericValue ;
where LabChemResultNumericValue ne .;
ods output summary=Labm ;
run;
*create table of summary stats of Refs by topography and units;
proc means data=monocyte_2013_2018  mean median;
class Topography clean_unit;
var RefLow_clean RefHigh_clean ;
ods output summary=Refsm;
run;
*create merged table to send to PIs;
proc sql;
create table top_unit_labs_refs_mon as
select *
from Labm a
left join Refsm b on a.Topography=b.Topography and
a.clean_unit = b.clean_unit;
quit;

proc export data=top_unit_labs_refs_mon (drop= NObs VName_RefLow_clean VName_RefHigh_clean)
outfile = "filepath"
dbms = xlsx replace;
sheet = 'Monocyte';
run;

data sasoutm.mon;
set monocyte_2013_2018;
run;


*Check with PIs what to do with lab values that have topography but no units;
*create table of summary stats of labs by topography and units;
proc means data=sasoutm.mon n mean min p10 median p90 max missing;
class Topography clean_unit;
var LabChemResultNumericValue ;
where LabChemResultNumericValue ne . /*and clean_unit = ""*/;
ods output summary=Labm_miss ;
run;
*create table of summary stats of Refs by topography and units;
proc means data=sasoutm.mon  mean median missing;
class Topography clean_unit;
var RefLow_clean RefHigh_clean ;
where LabChemResultNumericValue ne . and clean_unit = "";
ods output summary=Refsm_miss;
run;

proc sql;
create table top_unit_labs_refs_miss_m as
select *
from Labm_miss a
left join Refsm_miss b on a.Topography=b.Topography and
a.clean_unit = b.clean_unit;
quit;

proc export data=top_unit_labs_refs_miss_m (drop= NObs VName_RefLow_clean VName_RefHigh_clean)
outfile = "filepath"
dbms = xlsx replace;
sheet = 'Monocyte';
run;

*After PI review of labs by topography and units, several unit conversions needed - 2/1/24;

*check how many obs before deleting combos of topography and units to make sure correct number of obs after deleting;
proc means data=sasoutm.mon n mean min p10 median p90 max missing;
class Topography clean_unit;
var LabChemResultNumericValue ;
where LabChemResultNumericValue ne .;
ods output summary=test;
run;
/*NOTE: The data set WORK.LAB has 253 observations and 10 variables.*/

*theres a unit called %MANUAL that is not allowing macro to run;
data sasoutm.mon;
set sasoutm.mon;
if clean_unit  = "%MANUAL" then clean_unit = "MANUAL";
run;

*create macro lists of topography and units to be excluded;
options source source2 mprint symbolgen mlogic macrogen;

proc import datafile="filepath"
out= names_m dbms=csv replace;
getnames=yes;
guessingrows=1234567;
run;
*extract all values to create macro list to exclude combos of topography and units;
proc sql;
	select catt('"', Topography, '"')
	into :top_n separated by ' '
	from names_m;
	select catt('"', clean_unit, '"')
	into :units_n separated by ' '
	from names_m;
	quit;
%put &top_n;
%put &units_n;

*exclude combinations of topography and clean_unit;
%macro exclude_mon;
data monocyte_2013_2018_v2;
set sasoutm.mon;

%do i=1 %to %sysfunc(countw(&top_n,' ',q));
	%let next1 = %scan(&top_n,&i,' ',q);
	%let next2 = %scan(&units_n,&i,' ',q);

		if topography = &next1 and clean_unit = &next2 then delete;
%end;
run;
%mend exclude_mon;

%exclude_mon

*confirm they are excluded;
proc means data=monocyte_2013_2018_v2 n mean min p10 median p90 max missing;
class Topography clean_unit;
var LabChemResultNumericValue ;
where LabChemResultNumericValue ne .;
ods output summary=test2 ;
run;
/*NOTE: The data set WORK.LAB has 57 observations and 10 variables.*/
/*original table had 253 obs and 196 were excluded = 57 */

*convert units;
data monocyte_2013_2018_v2;
set monocyte_2013_2018_v2;
*convert units;
if topography = 'BLOOD' and clean_unit in ('/UL','CELLS/UL') 
then LabChemResultNumericValue = LabChemResultNumericValue/1000;
*standardize units;
if LabChemResultNumericValue ne . then clean_unit = 'K/UL';
run;


/*Create data set with final lab value per date per patient*/

/*create count var of labs per date per pt in order of latest labs first and 
create final_mon_daily var*/
data finaldate_mon;
set monocyte_2013_2018_v2;
run;

proc sort data = finaldate_mon;
by patienticn labspecimendate descending labspecimentime;
run;

data finaldate_mon;
set finaldate_mon;
by patienticn labspecimendate;
 retain n;
 if first.labspecimendate then n=1;
 else n = n+1;

if n = 1 then final_mon_daily = LabChemResultNumericValue;
 run;

/*create data set only including final monocyte labs*/
data final_mon_daily_2013_2018;
set finaldate_mon;
where n=1;
run;

*compare means of LabChemResultNumericValue and final_mon_daily - results are identical;
proc means data=final_mon_daily_2013_2018  n min median max mean;
var LabChemResultNumericValue final_mon_daily ;
run;

*confirm only one observation per date per patient - yes;
proc sort data=final_mon_daily_2013_2018  ;
by patienticn LabSpecimenDate;
run;
data conf;
set final_mon_daily_2013_2018 ;
by patienticn LabSpecimenDate;
if first.LabSpecimenDate = 0 or last.LabSpecimenDate=0;
run;


/*/*/*/*/*/*/*/*/* Apply physiologic cutoffs */*/*/*/*/*/*/*/*/;
data final_mon_daily_2013_2018;
set final_mon_daily_2013_2018;
if LabChemResultNumericValue < 0 or LabChemResultNumericValue > 300 then delete;
run;


/*/*/*/*/*/*/*/*/*/* Identify last lab per hosp in cohort */*/*/*/*/*/*/*/*//*/*/*/*/;

libname coh 'filepath';

*confirm date var is same format as datevalue in HAPPI data set;
proc contents data =final_mon_daily_2013_2018;
run;
proc contents data =coh.happi_sepsis_alive_20240131;
run;

*LEFT JOIN - merge HAPPI with final_hgb data set;
proc sql;
create table happi_final_monocyte as
select a.*, b.final_mon_daily, b.LabSpecimenDate, b.LabSpecimenTime, b.RefHigh_clean, b.RefLow_clean,
		b.Topography, b.clean_unit
from coh.happi_20132018_20211029 as a
left join final_mon_daily_2013_2018 as b on a.patienticn = b.patienticn
and a.datevalue = b.LabSpecimenDate;
quit;
* Table HAPPI.HAPPI_FINAL_monocyte created, with 8095111 rows;

proc sql;
select count (distinct unique_hosp_count_id)
from happi_final_monocyte;
quit;*1100996;

proc print data = happi_final_monocyte (obs = 100);
var patienticn unique_hosp_count_id hospital_day datevalue LabSpecimenDate LabSpecimenTime final_mon_daily ;
run;


*create final lab of hospitalization variable ;

*sort data for next data step;
proc sort data = happi_final_monocyte;
by patienticn unique_hosp_count_id descending datevalue descending hospital_day;
run;

*1)create reverse hosp_count variable
*2)create two variables with 1.labs from final hosp_day and 2.labs from 2nd to last hosp_day if final hosp_day labs are missing;
data happi_final_monocyte_v2;
set happi_final_monocyte;
by patienticn unique_hosp_count_id descending datevalue descending hospital_day;
	retain reverse_hosp_day final_mon_hosp_1;
	if first.unique_hosp_count_id then reverse_hosp_day = 1;
	else reverse_hosp_day = reverse_hosp_day +1;

if reverse_hosp_day = 1 then final_mon_hosp_1 = final_mon_daily;
if reverse_hosp_day = 2 then final_mon_hosp_2 = final_mon_daily;
if reverse_hosp_day = 2 and final_mon_hosp_1 ne . then final_mon_hosp_2 = .;
run;

*copy reverse_hosp_day day 2 labs that are missing in day 1 when there are no day 1 labs;
proc sort data = happi_final_monocyte_v2;
by patienticn unique_hosp_count_id reverse_hosp_day;
run;

data happi_final_monocyte_v3;
array firstExist{2};
array lastExist{2};
array final_mon_hosp_{2};
do until (last.unique_hosp_count_id);
    set happi_final_monocyte_v2; by patienticn unique_hosp_count_id reverse_hosp_day;
    do i = 1 to dim(final_mon_hosp_);
        if missing(firstExist{i}) then firstExist{i} = final_mon_hosp_{i};
        end;
    end;
do until (last.unique_hosp_count_id);
    set happi_final_monocyte_v2; by patienticn unique_hosp_count_id reverse_hosp_day;
    do i = 1 to dim(final_mon_hosp_);
        if missing(final_mon_hosp_{i}) then final_mon_hosp_{i} = coalesce(lastExist{i}, firstExist{i}, 0);
        else lastExist{i} = final_mon_hosp_{i};
        end;
    output;
    end;
run;

*create final_mon_hosp variable my merging reverse_hosp_day 1 and day 2 labs;
data sasoutm.happi_final_monocyte_v3;
set happi_final_monocyte_v3;
if final_mon_hosp_1 = 0.0 then final_mon_hosp_1 = .;
if final_mon_hosp_2 = 0.0 then final_mon_hosp_2 = .;
final_mon_hosp = coalesce(of final_mon_hosp_1-final_mon_hosp_2);
run;
*The data set HAPPI.HAPPI_FINAL_monocyte_V3 has 8095111 observations ;

*create indicator for labs on day of discharge only;
data sasoutm.happi_final_monocyte_v3 ;
set sasoutm.happi_final_monocyte_v3 ;
if final_mon_hosp_1 ne . then disch_lab = 1;
	else disch_lab = 0;
run;
