//jobname  JOB  (start of JOB statement parameters)
//stepname EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=* 
//SYSIN    DD DUMMY 
//SYSUT1   DD DSN=NB329.TEST.SORT1,DISP=SHR
//SYSUT2   DD DSN=NB329.TEST.BKUP.V1,DISP=OLD,
//     UNIT=unit,
//     VOL=SER=volser
