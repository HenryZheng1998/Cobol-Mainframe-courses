//KC03JB8A JOB MSGCLASS=Q,MSGLEVEL=(1,1)
//*********************************************************************
//*   THIS IS THE BMS MAP DEFINITION
//*   WHICH IS USED TO ASSEMBLE PHYSICAL AND SYMBOLIC MAPS
//*********************************************************************
//*
//*   TO CUSTOMIZE THIS SCRIPT YOU WILL NEED TO CHANGE THE FOLLOWING:
//*
//*
//*       DSCTLIB - SET THIS TO THE NAME OF THE DATASET WHICH CONTAINS
//*                 YOUR CICS/COBOL APPLICATION CODE.
//*
//*       MAPNAME - SET THIS TO THE MAP SET NAME PROVIDED BY YOUR
//*                 INSTRUCTOR (SHOULD BE DCXXXMM WHERE XXX ARE THE LAST
//*                 THREE CHARACTERS IN YOUR USER ID AND MM MEANS THAT
//*                 THIS IS THE MENU MAP)
//*
//*       BMSLIB  - SET THIS TO THE NAME OF THE DATASET WHICH CONTAINS
//*                 YOUR BMS CODE TO BE INTERPRETED
//*
//*********************************************************************
//*
//*
//CICSPROC JCLLIB ORDER=(TSOECCC.CICSTS12.PROCLIB)
//*
//MAPSET EXEC DFHMAPS,OUTC='*',RMODE=24,
//*
//* DATASET THAT CONTIAINS BMS SOURCE CODE
//  BMSLIB='KC03JB8.DEMO.CICS.BMS',
//*
//* NAME OF MEMBER THAT CONTAINS BMS SOURCE
//  MAPNAME='DCJB8MR',
//*
//* DATASET THAT WILL CONTAIN COBOL SOURCE CODE
//  DSCTLIB='KC03JB8.DEMO.CICS.COBOL',
//*
//* DO NOT CHANGE THE PARAMETERS BELOW
//  MAPLIB='TSOECCC.CICSTS12.STUDENT.LOADLIB',
//  INDEX='DFH310.CICS'
//COPY.SYSUT1 DD DSN=&BMSLIB(&MAPNAME),DISP=SHR
//ASMDSECT.SYSPUNCH DD DSN=&DSCTLIB(&MAPNAME),DISP=SHR