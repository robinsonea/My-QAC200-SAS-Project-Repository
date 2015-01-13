/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 2:11:33 PM
PROJECT: RobinsonE_SAS_project_0113
PROJECT PATH: P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp
---------------------------------------- */

/* Library assignment for Local.MYWORK */
Libname MYWORK V9 'P:\QAC\qac200\students\earobinson\Assignments' ;
/* Library assignment for Local.MYWORK */
Libname MYWORK V9 'P:\QAC\qac200\students\earobinson\Assignments' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MYWORK)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYWORK)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYWORK  "P:\QAC\qac200\students\earobinson\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Main Set Atr   */
%LET _CLIENTTASKLABEL='Main Set Atr';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:32 PM
   By task: Main Set Atr

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat [Local]
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Filter and Sort-age 18Up   */
%LET _CLIENTTASKLABEL='Filter and Sort-age 18Up';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYWORK._18UP_MEPS_FULLYR_2012);

PROC SQL;
   CREATE TABLE MYWORK._18UP_MEPS_FULLYR_2012(label="_17UP_MEPS_FULLYR_2012") AS 
   SELECT t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EDUYRDEG, 
          t1.FTSTU12X, 
          t1.CANCERDX, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCAID12X, 
          t1.MCARE12X, 
          t1.TOTEXP12, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.OBDEXP12, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X > 17
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:33 PM
   By task: Data Set Attributes

   Input Data: Local:MYWORK._18UP_MEPS_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFor_18UP_MEPS_FULLYR);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYWORK._18UP_MEPS_FULLYR_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFor_18UP_MEPS_FULLYR(LABEL="Contents Details for _18UP_MEPS_FULLYR_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFor_18UP_MEPS_FULLYR
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='_18UP_MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFor_18UP_MEPS_FULLYR OUT=WORK.CONTContentsFor_18UP_MEPS_FULLYR;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFor_18UP_MEPS_FULLYR
		WHERE memname='_18UP_MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For Data Set Attributes   */
%LET _CLIENTTASKLABEL='Code For Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 10:18:16 AM
   By task: Data Set Attributes

   Input Data: Local:MYWORK._18UP_MEPS_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFor_18UP_MEPS_FULLYR);
TITLE "Added Title: QAC200 18UP Dataset"
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";


PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYWORK._18UP_MEPS_FULLYR_2012;

RUN;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies-All Variables   */
%LET _CLIENTTASKLABEL='One-Way Frequencies-All Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:33 PM
   By task: One-Way Frequencies-All Variables

   Input Data: Local:MYWORK._18UP_MEPS_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYWORK._18UP_MEPS_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.DUPERSID, T.SEX, T.REGION12, T.RACETHX, T.MARRY12X, T.EDUYRDEG, T.FTSTU12X, T.CANCERDX, T.HIBPDX, T.INS12X, T.MCAID12X, T.MCARE12X, T.TOTEXP12, T.MIDX, T.STRKDX, T.OBDEXP12, T.ADPRX42, T.ADILCR42, T.ADILWW42, T.ADRTCR42
		     , T.ADRTWW42, T.ADAPPT42, T.ADNDCR42, T.ADEGMC42, T.ADLIST42, T.ADEXPL42, T.ADRESP42, T.ADPRTM42, T.ADINST42, T.ADEZUN42, T.ADTLHW42, T.ADFFRM42, T.ADFHLP42, T.ADHECR42, T.ADSMOK42, T.ADNSMK42, T.ADDRBP42, T.ADSPEC42
		     , T.ADSPRF42, T.ADGENH42, T.ADDAYA42, T.ADCLIM42, T.ADPALS42, T.ADPWLM42, T.ADMALS42, T.ADMWLM42, T.ADPAIN42, T.ADCAPE42, T.ADNRGY42, T.ADDOWN42, T.ADSOCA42, T.PCS42, T.MCS42, T.SFFLAG42, T.ADNERV42, T.ADHOPE42, T.ADREST42
		     , T.ADSAD42, T.ADEFRT42, T.ADWRTH42, T.K6SUM42, T.ADINTR42, T.ADDPRS42, T.PHQ242, T.ADINSA42, T.ADINSB42, T.ADRISK42, T.ADOVER42, T.ADCMPM42, T.ADCMPD42, T.ADCMPY42, T.ADLANG42
	FROM MYWORK._18UP_MEPS_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
TITLE3 "Assignment 4";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
FOOTNOTE2 "by Eriq Robinson";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X /  SCORES=TABLE;
	TABLES DUPERSID /  SCORES=TABLE;
	TABLES SEX /  SCORES=TABLE;
	TABLES REGION12 /  SCORES=TABLE;
	TABLES RACETHX /  SCORES=TABLE;
	TABLES MARRY12X /  SCORES=TABLE;
	TABLES EDUYRDEG /  SCORES=TABLE;
	TABLES FTSTU12X /  SCORES=TABLE;
	TABLES CANCERDX /  SCORES=TABLE;
	TABLES HIBPDX /  SCORES=TABLE;
	TABLES INS12X /  SCORES=TABLE;
	TABLES MCAID12X /  SCORES=TABLE;
	TABLES MCARE12X /  SCORES=TABLE;
	TABLES TOTEXP12 /  SCORES=TABLE;
	TABLES MIDX /  SCORES=TABLE;
	TABLES STRKDX /  SCORES=TABLE;
	TABLES OBDEXP12 /  SCORES=TABLE;
	TABLES ADPRX42 /  SCORES=TABLE;
	TABLES ADILCR42 /  SCORES=TABLE;
	TABLES ADILWW42 /  SCORES=TABLE;
	TABLES ADRTCR42 /  SCORES=TABLE;
	TABLES ADRTWW42 /  SCORES=TABLE;
	TABLES ADAPPT42 /  SCORES=TABLE;
	TABLES ADNDCR42 /  SCORES=TABLE;
	TABLES ADEGMC42 /  SCORES=TABLE;
	TABLES ADLIST42 /  SCORES=TABLE;
	TABLES ADEXPL42 /  SCORES=TABLE;
	TABLES ADRESP42 /  SCORES=TABLE;
	TABLES ADPRTM42 /  SCORES=TABLE;
	TABLES ADINST42 /  SCORES=TABLE;
	TABLES ADEZUN42 /  SCORES=TABLE;
	TABLES ADTLHW42 /  SCORES=TABLE;
	TABLES ADFFRM42 /  SCORES=TABLE;
	TABLES ADFHLP42 /  SCORES=TABLE;
	TABLES ADHECR42 /  SCORES=TABLE;
	TABLES ADSMOK42 /  SCORES=TABLE;
	TABLES ADNSMK42 /  SCORES=TABLE;
	TABLES ADDRBP42 /  SCORES=TABLE;
	TABLES ADSPEC42 /  SCORES=TABLE;
	TABLES ADSPRF42 /  SCORES=TABLE;
	TABLES ADGENH42 /  SCORES=TABLE;
	TABLES ADDAYA42 /  SCORES=TABLE;
	TABLES ADCLIM42 /  SCORES=TABLE;
	TABLES ADPALS42 /  SCORES=TABLE;
	TABLES ADPWLM42 /  SCORES=TABLE;
	TABLES ADMALS42 /  SCORES=TABLE;
	TABLES ADMWLM42 /  SCORES=TABLE;
	TABLES ADPAIN42 /  SCORES=TABLE;
	TABLES ADCAPE42 /  SCORES=TABLE;
	TABLES ADNRGY42 /  SCORES=TABLE;
	TABLES ADDOWN42 /  SCORES=TABLE;
	TABLES ADSOCA42 /  SCORES=TABLE;
	TABLES PCS42 /  SCORES=TABLE;
	TABLES MCS42 /  SCORES=TABLE;
	TABLES SFFLAG42 /  SCORES=TABLE;
	TABLES ADNERV42 /  SCORES=TABLE;
	TABLES ADHOPE42 /  SCORES=TABLE;
	TABLES ADREST42 /  SCORES=TABLE;
	TABLES ADSAD42 /  SCORES=TABLE;
	TABLES ADEFRT42 /  SCORES=TABLE;
	TABLES ADWRTH42 /  SCORES=TABLE;
	TABLES K6SUM42 /  SCORES=TABLE;
	TABLES ADINTR42 /  SCORES=TABLE;
	TABLES ADDPRS42 /  SCORES=TABLE;
	TABLES PHQ242 /  SCORES=TABLE;
	TABLES ADINSA42 /  SCORES=TABLE;
	TABLES ADINSB42 /  SCORES=TABLE;
	TABLES ADRISK42 /  SCORES=TABLE;
	TABLES ADOVER42 /  SCORES=TABLE;
	TABLES ADCMPM42 /  SCORES=TABLE;
	TABLES ADCMPD42 /  SCORES=TABLE;
	TABLES ADCMPY42 /  SCORES=TABLE;
	TABLES ADLANG42 /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR__18UP_MEPS_FULLYR);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR__18UP_MEPS_FULLYR AS 
   SELECT t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EDUYRDEG, 
          t1.FTSTU12X, 
          t1.CANCERDX, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCAID12X, 
          t1.MCARE12X, 
          t1.TOTEXP12, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.OBDEXP12, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          /* ILL_Injury_Imm_Care */
            (CASE 
               WHEN -1 = t1.ADILCR42 THEN .
               WHEN -9 = t1.ADILCR42 THEN .
               ELSE t1.ADILCR42
            END) LABEL="Ill/Injury needing immediate care (missing)" AS ILL_Injury_Imm_Care, 
          /* GOT_CARE_NEEDED */
            (CASE 
               WHEN -1 = t1.ADILWW42 THEN .
               WHEN -9 = t1.ADILWW42 THEN .
               ELSE t1.ADILWW42
            END) LABEL="Got Care When Needed ILL/INJ (missing)" AS GOT_CARE_NEEDED, 
          /* ROUTINE_MED */
            (CASE 
               WHEN -1 = t1.ADRTCR42 THEN .
               WHEN -8 = t1.ADRTCR42 THEN .
               WHEN -9 = t1.ADRTCR42 THEN .
               ELSE t1.ADRTCR42
            END) LABEL="Made Appointment for Routine Med Care (missing)" AS ROUTINE_MED, 
          /* NUM_MED_VISITS */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="# of Visits to the Med Office for Care (missing)" AS NUM_MED_VISITS, 
          /* MED_EASE */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="Easy Getting Needed Med Care (missing)" AS MED_EASE, 
          /* DOC_LIST */
            (CASE 
               WHEN -1 = t1.ADLIST42 THEN .
               WHEN -7 = t1.ADLIST42 THEN .
               WHEN -9 = t1.ADLIST42 THEN .
               ELSE t1.ADLIST42
            END) LABEL="Doctor Listened to You (missing)" AS DOC_LIST, 
          /* DOC_EXPL */
            (CASE 
               WHEN -1 = t1.ADEXPL42 THEN .
               WHEN -9 = t1.ADEXPL42 THEN .
               ELSE t1.ADEXPL42
            END) LABEL="Doctor Explained so the Patient Understood (missing)" AS DOC_EXPL, 
          /* DOC_TIME_ENUF */
            (CASE 
               WHEN -1 = t1.ADPRTM42 THEN .
               WHEN -9 = t1.ADPRTM42 THEN .
               ELSE t1.ADPRTM42
            END) LABEL="Doctor Spent Enough Time with Patient (missing)" AS DOC_TIME_ENUF, 
          /* DOC_SPEC_INSTR */
            (CASE 
               WHEN -1 = t1.ADINST42 THEN .
               WHEN -8 = t1.ADINST42 THEN .
               WHEN -9 = t1.ADINST42 THEN .
               ELSE t1.ADINST42
            END) LABEL="Doctor Gave Specific Instructions (missing)" AS DOC_SPEC_INSTR, 
          /* CURR_SMOK */
            (CASE 
               WHEN -1 = t1.ADSMOK42 THEN .
               WHEN -9 = t1.ADSMOK42 THEN .
               ELSE t1.ADSMOK42
            END) LABEL="Current Smoker" AS CURR_SMOK, 
          /* HEALTH_GEN */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health In General (missing)" AS HEALTH_GEN, 
          /* HEALTH_MOD_ACT */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="Health Litmits Mod Activities (missing)" AS HEALTH_MOD_ACT, 
          /* HLTH_CLIM_STRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Health Limits Climbing Stairs (missing)" AS HLTH_CLIM_STRS, 
          /* ACCMP_LESS_BC_PHYS_PROBS */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="Accomplish Less Because Physical Problems (missing)" AS ACCMP_LESS_BC_PHYS_PROBS, 
          /* WORK_LIMIT_PHYS_PROB */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="Work Limit Because Physical Problems (missing)" AS WORK_LIMIT_PHYS_PROB, 
          /* ACCMP_LESS_MNT_PROB */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="Accomplish Less Because of Mental Problems" AS ACCMP_LESS_MNT_PROB, 
          /* WRK_LMT_MNT_PROB */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="Work Limit Because Mental Problems (missing)" AS WRK_LMT_MNT_PROB, 
          /* PAIN_LMT_NORM_WORK */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="Pain Limits for Normal Work" AS PAIN_LMT_NORM_WORK, 
          /* CAPE */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="Felt Calm/Peaceful" AS CAPE, 
          /* ALOT_NRG */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="Had A Lot of Energy (missing)" AS ALOT_NRG, 
          /* DWN_HRTED */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Felt Downhearted/Depressed (missing)" AS DWN_HRTED, 
          /* SOC_ACT */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="Health Stopped Social Activity (missing)" AS SOC_ACT, 
          /* MRY */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Married (missing)" AS MRY, 
          /* YR_ED */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="Year of Education or Highest Degree (missing)" AS YR_ED, 
          /* CNCR */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="Cancer Diagnosis (missing)" AS CNCR, 
          /* HIBPD */
            (CASE 
               WHEN -7 = t1.HIBPDX THEN .
               WHEN -8 = t1.HIBPDX THEN .
               WHEN -9 = t1.HIBPDX THEN .
               ELSE t1.HIBPDX
            END) LABEL="High Blood Pressure Diagnosis (missing)" AS HIBPD, 
          /* HRT_ATK */
            (CASE 
               WHEN -7 = t1.MIDX THEN .
               WHEN -8 = t1.MIDX THEN .
               WHEN -9 = t1.MIDX THEN .
            END) LABEL="Heart Attack Diagnosis (missing)" AS HRT_ATK, 
          /* STRK_DIAG */
            (CASE 
               WHEN -7 = t1.STRKDX THEN .
               WHEN -8 = t1.STRKDX THEN .
               WHEN -9 = t1.STRKDX THEN .
               ELSE t1.STRKDX
            END) LABEL="Stroke Diagnosis (missing)" AS STRK_DIAG, 
          /* MARRYRC */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marry Recoded (missing)" AS MARRYRC, 
          /* EDUCODERC */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="The new edu one with the recode" AS EDUCODERC
      FROM MYWORK._18UP_MEPS_FULLYR_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis-SF12 for Missing   */
%LET _CLIENTTASKLABEL='Table Analysis-SF12 for Missing';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:54 PM
   By task: Table Analysis-SF12 for Missing

   Input Data: Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_GEN, T.ADGENH42, T.ADCLIM42, T.ADSOCA42, T.ADPAIN42, T.ADDOWN42, T.SOC_ACT, T.HLTH_CLIM_STRS, T.PAIN_LMT_NORM_WORK, T.DWN_HRTED, T.ILL_Injury_Imm_Care, T.GOT_CARE_NEEDED, T.ROUTINE_MED, T.NUM_MED_VISITS, T.MED_EASE
		     , T.DOC_LIST, T.DOC_EXPL, T.DOC_TIME_ENUF, T.DOC_SPEC_INSTR, T.CURR_SMOK, T.HEALTH_MOD_ACT, T.ACCMP_LESS_BC_PHYS_PROBS, T.WORK_LIMIT_PHYS_PROB, T.ACCMP_LESS_MNT_PROB, T.WRK_LMT_MNT_PROB, T.CAPE, T.ALOT_NRG, T.MRY, T.YR_ED, T.CNCR
		     , T.HIBPD, T.HRT_ATK, T.STRK_DIAG, T.MARRY12X, T.EDUYRDEG, T.FTSTU12X, T.CANCERDX, T.HIBPDX, T.INS12X, T.MCAID12X, T.MCARE12X, T.TOTEXP12, T.MIDX, T.STRKDX, T.OBDEXP12, T.ADPRX42, T.ADILCR42, T.ADILWW42, T.ADRTCR42
		     , T.ADRTWW42, T.ADAPPT42, T.ADNDCR42, T.ADEGMC42, T.ADLIST42, T.ADEXPL42, T.ADRESP42, T.ADPRTM42, T.ADINST42, T.ADEZUN42, T.ADTLHW42, T.ADFFRM42, T.ADFHLP42, T.ADHECR42, T.ADSMOK42, T.ADNSMK42, T.ADDRBP42, T.ADSPEC42, T.ADSPRF42
		     , T.ADDAYA42, T.ADPALS42, T.ADPWLM42, T.ADMALS42, T.ADMWLM42, T.ADCAPE42, T.ADNRGY42, T.PCS42, T.MCS42, T.SFFLAG42, T.ADNERV42, T.ADHOPE42, T.ADREST42, T.ADSAD42, T.ADEFRT42, T.ADWRTH42, T.K6SUM42, T.ADINTR42, T.ADDPRS42
		     , T.PHQ242, T.ADINSA42, T.ADINSB42, T.ADRISK42, T.ADOVER42, T.ADCMPM42, T.ADCMPD42, T.ADCMPY42, T.ADLANG42
	FROM WORK.QUERY_FOR__18UP_MEPS_FULLYR as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGENH42 * HEALTH_GEN /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCLIM42 * HLTH_CLIM_STRS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDOWN42 * DWN_HRTED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSOCA42 * SOC_ACT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPAIN42 * PAIN_LMT_NORM_WORK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPALS42 * ACCMP_LESS_BC_PHYS_PROBS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMALS42 * ACCMP_LESS_MNT_PROB /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNRGY42 * ALOT_NRG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCAPE42 * CAPE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES CANCERDX * CNCR /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNSMK42 * CURR_SMOK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADEXPL42 * DOC_EXPL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADLIST42 * DOC_LIST /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADINST42 * DOC_SPEC_INSTR /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPRTM42 * DOC_TIME_ENUF /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADILWW42 * GOT_CARE_NEEDED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES HIBPDX * HIBPD /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MIDX * HRT_ATK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADILCR42 * ILL_Injury_Imm_Care /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADEGMC42 * MED_EASE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MARRY12X * MRY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADAPPT42 * NUM_MED_VISITS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADRTCR42 * ROUTINE_MED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSOCA42 * SOC_ACT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES STRKDX * STRK_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPWLM42 * WORK_LIMIT_PHYS_PROB /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMWLM42 * WRK_LMT_MNT_PROB /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDUYRDEG * YR_ED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder-Agg SF12   */
%LET _CLIENTTASKLABEL='Query Builder-Agg SF12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000 AS 
   SELECT t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EDUYRDEG, 
          t1.FTSTU12X, 
          t1.CANCERDX, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCAID12X, 
          t1.MCARE12X, 
          t1.TOTEXP12, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.OBDEXP12, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.ILL_Injury_Imm_Care, 
          t1.GOT_CARE_NEEDED, 
          t1.ROUTINE_MED, 
          t1.NUM_MED_VISITS, 
          t1.MED_EASE, 
          t1.DOC_LIST, 
          t1.DOC_EXPL, 
          t1.DOC_TIME_ENUF, 
          t1.DOC_SPEC_INSTR, 
          t1.CURR_SMOK, 
          t1.HEALTH_GEN, 
          t1.HEALTH_MOD_ACT, 
          t1.HLTH_CLIM_STRS, 
          t1.ACCMP_LESS_BC_PHYS_PROBS, 
          t1.WORK_LIMIT_PHYS_PROB, 
          t1.ACCMP_LESS_MNT_PROB, 
          t1.WRK_LMT_MNT_PROB, 
          t1.PAIN_LMT_NORM_WORK, 
          t1.CAPE, 
          t1.ALOT_NRG, 
          t1.DWN_HRTED, 
          t1.SOC_ACT, 
          t1.MRY, 
          t1.YR_ED, 
          t1.CNCR, 
          t1.HIBPD, 
          t1.HRT_ATK, 
          t1.STRK_DIAG, 
          /* AGG_SF12 */
            
            (SUM(t1.HEALTH_GEN,t1.HEALTH_MOD_ACT,t1.HLTH_CLIM_STRS,t1.ACCMP_LESS_BC_PHYS_PROBS,t1.WORK_LIMIT_PHYS_PROB,t1.ACCMP_LESS_MNT_PROB,t1.WRK_LMT_MNT_PROB,t1.PAIN_LMT_NORM_WORK,t1.CAPE,t1.ALOT_NRG,t1.DWN_HRTED,t1.SOC_ACT)) 
            LABEL="Aggragate SF-12 Variables" AS AGG_SF12
      FROM WORK.QUERY_FOR__18UP_MEPS_FULLYR t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Check Agg List First 50   */
%LET _CLIENTTASKLABEL='Check Agg List First 50';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:55 PM
   By task: Check Agg List First 50

   Input Data: Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_GEN, T.HEALTH_MOD_ACT, T.HLTH_CLIM_STRS, T.ACCMP_LESS_BC_PHYS_PROBS, T.WORK_LIMIT_PHYS_PROB, T.ACCMP_LESS_MNT_PROB, T.WRK_LMT_MNT_PROB, T.PAIN_LMT_NORM_WORK, T.CAPE, T.ALOT_NRG, T.DWN_HRTED, T.SOC_ACT, T.AGG_SF12
	FROM WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000 as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregate Variable Coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR HEALTH_GEN HEALTH_MOD_ACT HLTH_CLIM_STRS ACCMP_LESS_BC_PHYS_PROBS WORK_LIMIT_PHYS_PROB ACCMP_LESS_MNT_PROB WRK_LMT_MNT_PROB PAIN_LMT_NORM_WORK CAPE ALOT_NRG DWN_HRTED SOC_ACT AGG_SF12;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics of OVRALL HLTH  VAR   */
%LET _CLIENTTASKLABEL='Summary Statistics of OVRALL HLTH  VAR';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:55 PM
   By task: Summary Statistics of OVRALL HLTH  VAR

   Input Data: Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGG_SF12
	FROM WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000 as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		STDERR 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGG_SF12;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Hist of OVAL HVAR   */
%LET _CLIENTTASKLABEL='Hist of OVAL HVAR';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:56 PM
   By task: Hist of OVAL HVAR

   Input Data: Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGG_SF12
	FROM WORK.QUERY_FOR__18UP_MEPS_FULLYR_0000 as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: AGG_SF12";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
		CIPCTLNORMAL(TYPE=TWOSIDED ALPHA=0.05)
;
	VAR AGG_SF12;
	HISTOGRAM / 	CFRAME=LTGRAY CAXES=CXFF6600 WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:56 PM
   By task: Distribution Analysis

   Input Data: Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUCODERC
	FROM WORK.QUERY_FOR__18UP_MEPS_FULLYR as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: EDUCODERC";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR EDUCODERC;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: QB - Categorical EDU_MAR   */
%LET _CLIENTTASKLABEL='QB - Categorical EDU_MAR';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR__18UP_MEPS_FULLYR_0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR__18UP_MEPS_FULLYR_0001 AS 
   SELECT /* MRY_YN */
            (CASE  
               WHEN t1.MARRYRC=1
               THEN 1
               ELSE 2
            END) LABEL="Married Yes/No" AS MRY_YN, 
          /* EDU_LMH */
            (CASE  
               WHEN t1.EDUCODERC < 12 and t1.EDUCODERC  >0
               THEN 1
               WHEN t1.EDUCODERC >= 12 and  t1.EDUCODERC < 14
               THEN 2
               ELSE 3
            END) LABEL="Education Categorical: Low/Medium/High" AS EDU_LMH
      FROM WORK.QUERY_FOR__18UP_MEPS_FULLYR t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis11   */
%LET _CLIENTTASKLABEL='Distribution Analysis11';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:57 PM
   By task: Distribution Analysis11

   Input Data: Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR__18UP_MEPS_FULLYR_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDU_LMH, T.MRY_YN
	FROM WORK.QUERY_FOR__18UP_MEPS_FULLYR_0001 as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: EDU_LMH, MRY_YN";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS QUANTILES;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR EDU_LMH MRY_YN;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_RECODE_TEMPV1);

PROC SQL;
   CREATE TABLE WORK."QUERY_RECODE_TEMPV1"n AS 
   SELECT t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EDUYRDEG, 
          t1.FTSTU12X, 
          t1.CANCERDX, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCAID12X, 
          t1.MCARE12X, 
          t1.TOTEXP12, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.OBDEXP12, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.ILL_Injury_Imm_Care, 
          t1.GOT_CARE_NEEDED, 
          t1.ROUTINE_MED, 
          t1.NUM_MED_VISITS, 
          t1.MED_EASE, 
          t1.DOC_LIST, 
          t1.DOC_EXPL, 
          t1.DOC_TIME_ENUF, 
          t1.DOC_SPEC_INSTR, 
          t1.CURR_SMOK, 
          t1.HEALTH_GEN, 
          t1.HEALTH_MOD_ACT, 
          t1.HLTH_CLIM_STRS, 
          t1.ACCMP_LESS_BC_PHYS_PROBS, 
          t1.WORK_LIMIT_PHYS_PROB, 
          t1.ACCMP_LESS_MNT_PROB, 
          t1.WRK_LMT_MNT_PROB, 
          t1.PAIN_LMT_NORM_WORK, 
          t1.CAPE, 
          t1.ALOT_NRG, 
          t1.DWN_HRTED, 
          t1.SOC_ACT, 
          t1.MRY, 
          t1.YR_ED, 
          t1.CNCR, 
          t1.HIBPD, 
          t1.HRT_ATK, 
          t1.STRK_DIAG, 
          t1.MARRYRC, 
          t1.EDUCODERC, 
          /* IN_FULLYR */
            (1) AS IN_FULLYR
      FROM WORK.QUERY_FOR__18UP_MEPS_FULLYR t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder1   */
LIBNAME EC100012 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* IN_ER */
            (1) AS IN_ER
      FROM EC100012.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder2   */
%LET _CLIENTTASKLABEL='Query Builder2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MERGER_ER_FULLYR);

PROC SQL;
   CREATE TABLE WORK."MERGER_ER_FULLYR"n AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t2.AGE12X, 
          t2.DUPERSID AS DUPERSID1, 
          t2.SEX, 
          t2.REGION12, 
          t2.RACETHX, 
          t2.MARRY12X, 
          t2.EDRECODE, 
          t2.EDUYRDEG, 
          t2.FTSTU12X, 
          t2.CANCERDX, 
          t2.HIBPDX, 
          t2.INS12X, 
          t2.MCAID12X, 
          t2.MCARE12X, 
          t2.TOTEXP12, 
          t2.MIDX, 
          t2.STRKDX, 
          t2.OBDEXP12, 
          t2.ADPRX42, 
          t2.ADILCR42, 
          t2.ADILWW42, 
          t2.ADRTCR42, 
          t2.ADRTWW42, 
          t2.ADAPPT42, 
          t2.ADNDCR42, 
          t2.ADEGMC42, 
          t2.ADLIST42, 
          t2.ADEXPL42, 
          t2.ADRESP42, 
          t2.ADPRTM42, 
          t2.ADINST42, 
          t2.ADEZUN42, 
          t2.ADTLHW42, 
          t2.ADFFRM42, 
          t2.ADFHLP42, 
          t2.ADHECR42, 
          t2.ADSMOK42, 
          t2.ADNSMK42, 
          t2.ADDRBP42, 
          t2.ADSPEC42, 
          t2.ADSPRF42, 
          t2.ADGENH42, 
          t2.ADDAYA42, 
          t2.ADCLIM42, 
          t2.ADPALS42, 
          t2.ADPWLM42, 
          t2.ADMALS42, 
          t2.ADMWLM42, 
          t2.ADPAIN42, 
          t2.ADCAPE42, 
          t2.ADNRGY42, 
          t2.ADDOWN42, 
          t2.ADSOCA42, 
          t2.PCS42, 
          t2.MCS42, 
          t2.SFFLAG42, 
          t2.ADNERV42, 
          t2.ADHOPE42, 
          t2.ADREST42, 
          t2.ADSAD42, 
          t2.ADEFRT42, 
          t2.ADWRTH42, 
          t2.K6SUM42, 
          t2.ADINTR42, 
          t2.ADDPRS42, 
          t2.PHQ242, 
          t2.ADINSA42, 
          t2.ADINSB42, 
          t2.ADRISK42, 
          t2.ADOVER42, 
          t2.ADCMPM42, 
          t2.ADCMPD42, 
          t2.ADCMPY42, 
          t2.ADLANG42, 
          t2.ILL_Injury_Imm_Care, 
          t2.GOT_CARE_NEEDED, 
          t2.ROUTINE_MED, 
          t2.NUM_MED_VISITS, 
          t2.MED_EASE, 
          t2.DOC_LIST, 
          t2.DOC_EXPL, 
          t2.DOC_TIME_ENUF, 
          t2.DOC_SPEC_INSTR, 
          t2.CURR_SMOK, 
          t2.HEALTH_GEN, 
          t2.HEALTH_MOD_ACT, 
          t2.HLTH_CLIM_STRS, 
          t2.ACCMP_LESS_BC_PHYS_PROBS, 
          t2.WORK_LIMIT_PHYS_PROB, 
          t2.ACCMP_LESS_MNT_PROB, 
          t2.WRK_LMT_MNT_PROB, 
          t2.PAIN_LMT_NORM_WORK, 
          t2.CAPE, 
          t2.ALOT_NRG, 
          t2.DWN_HRTED, 
          t2.SOC_ACT, 
          t2.MRY, 
          t2.YR_ED, 
          t2.CNCR, 
          t2.HIBPD, 
          t2.HRT_ATK, 
          t2.STRK_DIAG, 
          t2.MARRYRC, 
          t2.EDUCODERC, 
          t2.IN_FULLYR, 
          t1.IN_ER
      FROM WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT t1
           FULL JOIN WORK.QUERY_RECODE_TEMPV1 t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder3   */
%LET _CLIENTTASKLABEL='Query Builder3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MERGER_ER_FULLYR);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MERGER_ER_FULLYR AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.AGE12X, 
          t1.DUPERSID1, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EDUYRDEG, 
          t1.FTSTU12X, 
          t1.CANCERDX, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCAID12X, 
          t1.MCARE12X, 
          t1.TOTEXP12, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.OBDEXP12, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.ILL_Injury_Imm_Care, 
          t1.GOT_CARE_NEEDED, 
          t1.ROUTINE_MED, 
          t1.NUM_MED_VISITS, 
          t1.MED_EASE, 
          t1.DOC_LIST, 
          t1.DOC_EXPL, 
          t1.DOC_TIME_ENUF, 
          t1.DOC_SPEC_INSTR, 
          t1.CURR_SMOK, 
          t1.HEALTH_GEN, 
          t1.HEALTH_MOD_ACT, 
          t1.HLTH_CLIM_STRS, 
          t1.ACCMP_LESS_BC_PHYS_PROBS, 
          t1.WORK_LIMIT_PHYS_PROB, 
          t1.ACCMP_LESS_MNT_PROB, 
          t1.WRK_LMT_MNT_PROB, 
          t1.PAIN_LMT_NORM_WORK, 
          t1.CAPE, 
          t1.ALOT_NRG, 
          t1.DWN_HRTED, 
          t1.SOC_ACT, 
          t1.MRY, 
          t1.YR_ED, 
          t1.CNCR, 
          t1.HIBPD, 
          t1.HRT_ATK, 
          t1.STRK_DIAG, 
          t1.MARRYRC, 
          t1.EDUCODERC, 
          t1.IN_FULLYR, 
          t1.IN_ER
      FROM WORK.MERGER_ER_FULLYR t1
      WHERE t1.IN_ER = 1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:58 PM
   By task: List Data

   Input Data: Local:WORK.QUERY_FOR_MERGER_ER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MERGER_ER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID1, T.DUPERSID, T.IN_ER
	FROM WORK.QUERY_FOR_MERGER_ER_FULLYR as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	OBS="Row number"
	LABEL
	;
	VAR DUPERSID1 DUPERSID IN_ER;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes1   */
%LET _CLIENTTASKLABEL='Data Set Attributes1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:09:59 PM
   By task: Data Set Attributes1

   Input Data: Local:WORK.QUERY_FOR_MERGER_ER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForQUERY_FOR_MERGER_);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=WORK.QUERY_FOR_MERGER_ER_FULLYR OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForQUERY_FOR_MERGER_(LABEL="Contents Details for QUERY_FOR_MERGER_ER_FULLYR");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForQUERY_FOR_MERGER_
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='QUERY_FOR_MERGER_ER_FULLYR';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForQUERY_FOR_MERGER_ OUT=WORK.CONTContentsForQUERY_FOR_MERGER_;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForQUERY_FOR_MERGER_
		WHERE memname='QUERY_FOR_MERGER_ER_FULLYR';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: QB Recoded MRI-XRAY   */
%LET _CLIENTTASKLABEL='QB Recoded MRI-XRAY';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MERGER_ER_FULLYR_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MERGER_ER_FULLYR_0000 AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.AGE12X, 
          t1.DUPERSID1, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EDUYRDEG, 
          t1.FTSTU12X, 
          t1.CANCERDX, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCAID12X, 
          t1.MCARE12X, 
          t1.TOTEXP12, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.OBDEXP12, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.ILL_Injury_Imm_Care, 
          t1.GOT_CARE_NEEDED, 
          t1.ROUTINE_MED, 
          t1.NUM_MED_VISITS, 
          t1.MED_EASE, 
          t1.DOC_LIST, 
          t1.DOC_EXPL, 
          t1.DOC_TIME_ENUF, 
          t1.DOC_SPEC_INSTR, 
          t1.CURR_SMOK, 
          t1.HEALTH_GEN, 
          t1.HEALTH_MOD_ACT, 
          t1.HLTH_CLIM_STRS, 
          t1.ACCMP_LESS_BC_PHYS_PROBS, 
          t1.WORK_LIMIT_PHYS_PROB, 
          t1.ACCMP_LESS_MNT_PROB, 
          t1.WRK_LMT_MNT_PROB, 
          t1.PAIN_LMT_NORM_WORK, 
          t1.CAPE, 
          t1.ALOT_NRG, 
          t1.DWN_HRTED, 
          t1.SOC_ACT, 
          t1.MRY, 
          t1.YR_ED, 
          t1.CNCR, 
          t1.HIBPD, 
          t1.HRT_ATK, 
          t1.STRK_DIAG, 
          t1.MARRYRC, 
          t1.EDUCODERC, 
          t1.IN_FULLYR, 
          t1.IN_ER, 
          /* MRI_RECODE */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               ELSE t1.MRI
            END) AS MRI_RECODE, 
          /* XRAY_RECODE */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               ELSE t1.XRAYS
            END) LABEL="Xray Recoded (missing)" AS XRAY_RECODE
      FROM WORK.QUERY_FOR_MERGER_ER_FULLYR t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:10:18 PM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_MERGER_ER_FULLYR_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MERGER_ER_FULLYR_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MRI, T.XRAYS, T.MRI_RECODE, T.XRAY_RECODE
	FROM WORK.QUERY_FOR_MERGER_ER_FULLYR_0000 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MRI * MRI_RECODE /
		NOROW
		NOCOL
		MISSPRINT
		OUTPCT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES XRAYS * XRAY_RECODE /
		NOROW
		NOCOL
		MISSPRINT
		OUTPCT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder5   */
%LET _CLIENTTASKLABEL='Query Builder5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.RECODE_MRI_XRAY_COUNT);

PROC SQL;
   CREATE TABLE WORK."RECODE_MRI_XRAY_COUNT"n AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM WORK.QUERY_FOR_MERGER_ER_FULLYR_0000 t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder6   */
%LET _CLIENTTASKLABEL='Query Builder6';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.XRAY_DUPER_COMBINE);

PROC SQL;
   CREATE TABLE WORK."XRAY_DUPER_COMBINE"n AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.AGE12X, 
          t1.DUPERSID1, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EDUYRDEG, 
          t1.FTSTU12X, 
          t1.CANCERDX, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCAID12X, 
          t1.MCARE12X, 
          t1.TOTEXP12, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.OBDEXP12, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.ILL_Injury_Imm_Care, 
          t1.GOT_CARE_NEEDED, 
          t1.ROUTINE_MED, 
          t1.NUM_MED_VISITS, 
          t1.MED_EASE, 
          t1.DOC_LIST, 
          t1.DOC_EXPL, 
          t1.DOC_TIME_ENUF, 
          t1.DOC_SPEC_INSTR, 
          t1.CURR_SMOK, 
          t1.HEALTH_GEN, 
          t1.HEALTH_MOD_ACT, 
          t1.HLTH_CLIM_STRS, 
          t1.ACCMP_LESS_BC_PHYS_PROBS, 
          t1.WORK_LIMIT_PHYS_PROB, 
          t1.ACCMP_LESS_MNT_PROB, 
          t1.WRK_LMT_MNT_PROB, 
          t1.PAIN_LMT_NORM_WORK, 
          t1.CAPE, 
          t1.ALOT_NRG, 
          t1.DWN_HRTED, 
          t1.SOC_ACT, 
          t1.MRY, 
          t1.YR_ED, 
          t1.CNCR, 
          t1.HIBPD, 
          t1.HRT_ATK, 
          t1.STRK_DIAG, 
          t1.MARRYRC, 
          t1.EDUCODERC, 
          t1.IN_FULLYR, 
          t1.IN_ER, 
          t1.MRI_RECODE, 
          t1.XRAY_RECODE, 
          t2.DUPERSID AS DUPERSID2, 
          t2.COUNT_of_DUPERSID
      FROM WORK.QUERY_FOR_MERGER_ER_FULLYR_0000 t1
           INNER JOIN WORK.RECODE_MRI_XRAY_COUNT t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:10:18 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.XRAY_DUPER_COMBINE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT,
		WORK.OneWayFreqOfCOUNT_of_DUPERSIDInX);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.XRAY_DUPER_COMBINE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID
	FROM WORK.XRAY_DUPER_COMBINE as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
ODS GRAPHICS ON;
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID / 	OUT=WORK.OneWayFreqOfCOUNT_of_DUPERSIDInX(LABEL="Cell statistics for COUNT_of_DUPERSID analysis of WORK.XRAY_DUPER_COMBINE")
 SCORES=TABLE plots(only)=freq;
RUN;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Dist Analysis Count DUPERSID   */
%LET _CLIENTTASKLABEL='Dist Analysis Count DUPERSID';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:10:19 PM
   By task: Dist Analysis Count DUPERSID

   Input Data: Local:WORK.XRAY_DUPER_COMBINE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.XRAY_DUPER_COMBINE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM WORK.XRAY_DUPER_COMBINE as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: COUNT_of_DUPERSID";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM / NOPLOT ;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder7   */
%LET _CLIENTTASKLABEL='Query Builder7';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_XRAY_DUPER_COMBINE);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_XRAY_DUPER_COMBINE AS 
   SELECT /* CAT_VAR_ERVISIT */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID=1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID >=2 AND t1.COUNT_of_DUPERSID <5
               THEN 2
               WHEN t1.COUNT_of_DUPERSID >=5 and t1.COUNT_of_DUPERSID <8
               THEN 3
               WHEN t1.COUNT_of_DUPERSID >=8 
               THEN 4 
            END) LABEL="Categorical var for er visit numbers 1-ave 2-above ave, 3-95% and ^, 4-super extreme." AS 
            CAT_VAR_ERVISIT, 
          t1.COUNT_of_DUPERSID
      FROM WORK.XRAY_DUPER_COMBINE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:10:37 PM
   By task: Table Analysis1

   Input Data: Local:WORK.QUERY_FOR_XRAY_DUPER_COMBINE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_XRAY_DUPER_COMBINE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID, T.CAT_VAR_ERVISIT
	FROM WORK.QUERY_FOR_XRAY_DUPER_COMBINE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID * CAT_VAR_ERVISIT /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:10:38 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.QUERY_FOR_XRAY_DUPER_COMBINE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT,
		WORK.OneWayFreqOfCAT_VAR_ERVISITInQUE);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_XRAY_DUPER_COMBINE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.CAT_VAR_ERVISIT
	FROM WORK.QUERY_FOR_XRAY_DUPER_COMBINE as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
ODS GRAPHICS ON;
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES CAT_VAR_ERVISIT / OUTCUM 	OUT=WORK.OneWayFreqOfCAT_VAR_ERVISITInQUE(LABEL="Cell statistics for CAT_VAR_ERVISIT analysis of WORK.QUERY_FOR_XRAY_DUPER_COMBINE")
 SCORES=TABLE plots(only)=freq;
RUN;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis1   */
%LET _CLIENTTASKLABEL='Distribution Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\earobinson\Assignments\RobinsonE_SAS_project_0113.egp';
%LET _CLIENTPROJECTNAME='RobinsonE_SAS_project_0113.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:10:38 PM
   By task: Distribution Analysis1

   Input Data: Local:WORK.QUERY_FOR_XRAY_DUPER_COMBINE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_XRAY_DUPER_COMBINE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM WORK.QUERY_FOR_XRAY_DUPER_COMBINE as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: COUNT_of_DUPERSID";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
	FREQ
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
