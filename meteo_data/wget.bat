REM Example of CAMS Radiation request using wget (Windows).
REM 
REM NB1: please note that you CANNOT directly execute this script right away
REM as you need to replace "myemailaddress%%2540mycompany.com" with your own email address used to register on the SoDa website. 
REM The reason is that we need to identify you when launching the requests for statistical constraints imposed by the European Union.
REM NB2: replace the "@" by a single %2540 on a Windows command terminal, an internet browser or on Unix, but use a double %%2540 in a Windows .bat file.
REM
REM Please change the location and other parameters according to your needs.
REM See http://www.soda-pro.com/help/cams-services/cams-radiation-service/automatic-access

REM output file
set output_file=cams_radiation_Wuestenrot_2017-01-01_2017-01-05.csv

REM Try main SoDa server www.soda-is.com
wget -O %output_file% "http://www.soda-is.com/service/wps?Service=WPS&Request=Execute&Identifier=get_cams_radiation&version=1.0.0&DataInputs=latitude=49.080;longitude=9.460;altitude=-999;date_begin=2017-01-01;date_end=2017-01-05;time_ref=UT;summarization=PT01H;username=sally.koehler%%2540hft-suttgart.de&RawDataOutput=irradiation"

REM On error, try SoDa server mirror pro.soda-is.com
IF ERRORLEVEL 1 (
    wget -O %output_file% "http://www.soda-is.com/service/wps?Service=WPS&Request=Execute&Identifier=get_cams_radiation&version=1.0.0&DataInputs=latitude=49.080;longitude=9.460;altitude=-999;date_begin=2017-01-01;date_end=2017-01-05;time_ref=UT;summarization=PT01H;username=sally.koehler%%2540hft-suttgart.de&RawDataOutput=irradiation"
)

@echo off

REM pause if script launched by Windows Explorer
IF %0 == "%~0"  pause


