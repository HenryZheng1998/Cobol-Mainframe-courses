       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCJB8PGC.
       AUTHOR. HENRY ZHENG.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-COMMAREA PIC X(20) VALUE SPACES.
       01 WS-COMMAREA-LEN PIC S9(4) COMP VALUE 1.
      *01 WS-COMMAREA           PIC X(220).

      *01 WS-COMMAREA-LENGTH    PIC S9(4) COMP VALUE 400.

       01 WS-ERRORMSG           pic x(40).

       01 WS-CONSTANTS.
           05 WS-STU-LENGTH-CONST PIC 9 VALUE 4.

       01 WS-COURSE-CODE-1.
           05 WS-COURSE-CODE-1A  PIC X(4).
           05 WS-COURSE-CODE-1B  PIC X(4).

       01 WS-COURSE-CODE-2.
           05 WS-COURSE-CODE-2A  PIC X(4).
           05 WS-COURSE-CODE-2B  PIC X(4).

       01 WS-COURSE-CODE-3.
           05 WS-COURSE-CODE-3A  PIC X(4).
           05 WS-COURSE-CODE-3B  PIC X(4).

       01 WS-COURSE-CODE-4.
           05 WS-COURSE-CODE-4A  PIC X(4).
           05 WS-COURSE-CODE-4B  PIC X(4).

       01 WS-COURSE-CODE-5.
           05 WS-COURSE-CODE-5A  PIC X(4).
           05 WS-COURSE-CODE-5B  PIC X(4).

       01 WS-PHONE-LINE.
           05 FILLER        PIC XX VALUE "( ".
           05 WS-STU-PHONE1 PIC XXX.
           05 FILLER        PIC XXX VALUE " ) ".
           05 WS-STU-PHONE2 PIC XXX.
           05 FILLER        PIC XXX VALUE " - ".
           05 WS-STU-PHONE3 PIC X(4).

       01 WS-COURSE-COUNTER PIC 9 VALUE 0.

       01 WS-NOERROR-CONST PIC 99 VALUE 0.
      * COPY INQUIRY MAP LAYOUT
       COPY 'DCJB8MC'.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'STUREC'.

       COPY 'STUDATA'.

       01 WS-MSG           PIC X(40)
           VALUE SPACE.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X.

       PROCEDURE DIVISION.

       000-START-LOGIC.

      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
      * TODO: ADD CONDITION FOR RECORD NOT FOUND IN STUFILE
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
                DUPREC(300-DUPREC)
           END-EXEC.

           EXEC CICS HANDLE AID
      *         RESETS THE PROGRAM,
                PF4(100-FIRST-TIME)
      *         EXITS THE PROGRAM
                PF9(999-EXIT)
           END-EXEC.

           IF (EIBCALEN = 1) THEN
                GO TO 100-FIRST-TIME
           END-IF.
      * ATTEMPT TO RECEIVE MAP FROM TERMINAL
           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCJB8MC')
           END-EXEC.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCJB8MC')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('JB82')
           END-EXEC.

       200-MAIN-LOGIC.

      * TODO: IMPLEMENT VALIDATION LOGIC
           IF(STUNUMI = 'XXXXXXX')
               GO TO 999-EXIT
           END-IF.

      * Moving the variables to STUDATA
           MOVE STUNUML to STU-NUMBERL-T.
           MOVE STUNUMI TO STU-NUMBERI-T.


           MOVE STUCR1AL TO STU-CR1AL-T.
           MOVE STUCR1BL TO STU-CR1BL-T.
           MOVE STUCR1AI TO STU-CR1AI-T.
           MOVE STUCR1BI TO STU-CR1BI-T.

           MOVE STUCR2AL TO STU-CR2AL-T.
           MOVE STUCR2BL TO STU-CR2BL-T.
           MOVE STUCR2AI TO STU-CR2AI-T.
           MOVE STUCR2BI TO STU-CR2BI-T.

           MOVE STUCR3AL TO STU-CR3AL-T.
           MOVE STUCR3BL TO STU-CR3BL-T.
           MOVE STUCR3AI TO STU-CR3AI-T.
           MOVE STUCR3BI TO STU-CR3BI-T.


           MOVE STUCR4AL TO STU-CR4AL-T.
           MOVE STUCR4BL TO STU-CR4BL-T.
           MOVE STUCR4AI TO STU-CR4AI-T.
           MOVE STUCR4BI TO STU-CR4BI-T.


           MOVE STUCR5AL TO STU-CR5AL-T.
           MOVE STUCR5BL TO STU-CR5BL-T.
           MOVE STUCR5AI TO STU-CR5AI-T.
           MOVE STUCR5BI TO STU-CR5BI-T.

           MOVE STUNAMEL TO STU-NAMEL-T.
           MOVE STUNAMEI TO STU-NAMEI-T.

           MOVE STUADD1L TO STU-ADD1L-T.


           MOVE STUADD2L TO STU-ADD2L-T.


           MOVE STUPOS1L TO STU-POST1L-T.
           MOVE STUPOS1I TO STU-POST1I-T.

           MOVE STUPOS2L TO STU-POST2L-T.
           MOVE STUPOS2I TO STU-POST2I-T.

           MOVE STUPHN1L TO STU-PHN1L-T.
           MOVE STUPHN1I TO STU-PHN1I-T.

           MOVE STUPHN2L TO STU-PHN2L-T.
           MOVE STUPHN2I TO STU-PHN2I-T.

           MOVE STUPHN3L TO STU-PHN3L-T.
           MOVE STUPHN3I TO STU-PHN3I-T.


           MOVE 00 TO STU-ERRORCODE.
      *    MOVE STUDATA-RECORD TO WS-COMMAREA.

           EXEC CICS LINK
                PROGRAM('DCJB8PGE')
                COMMAREA(STUDATA-RECORD)
                LENGTH(STUDATA-LENGTH)
           END-EXEC.


      *    MOVE WS-COMMAREA TO STUDATA-RECORD.
      *    0 = NO ERRORS
      *    ANYTHING ELSE = ERRORS
      *    If error code is not 0 - error has occured
           If STU-ERRORCODE OF STUDATA-RECORD IS NOT EQUAL TO
           WS-NOERROR-CONST
                MOVE STU-ERRORMSG TO WS-MSG
                EVALUATE STU-ERRORCODE
                    WHEN 1
                        MOVE -1 TO STUNUML
                    WHEN 2
                        MOVE -1 TO STUCR1AL
                    WHEN 3
                        MOVE -1 TO STUCR2AL
                    WHEN 4
                        MOVE -1 TO STUCR3AL
                    WHEN 5
                        MOVE -1 TO STUCR4AL
                    WHEN 6
                        MOVE -1 TO STUCR5AL
                    WHEN 7
                        MOVE -1 TO STUCR1BL
                    WHEN 8
                        MOVE -1 TO STUCR2BL
                    WHEN 9
                        MOVE -1 TO STUCR3BL
                    WHEN 10
                        MOVE -1 TO STUCR4BL
                    WHEN 11
                        MOVE -1 TO STUCR5BL
                    WHEN 12
                        MOVE -1 TO STUNAMEL
                    WHEN 13
                        MOVE -1 TO STUADD1L
                    WHEN 14
                        MOVE -1 TO STUADD2L
                    WHEN 15
                        MOVE -1 TO STUPOS1L
                    WHEN 16
                        MOVE -1 TO STUPOS2L
                    WHEN 17
                        MOVE -1 TO STUPHN1L
                    WHEN 18
                        MOVE -1 TO STUPHN2L
                    WHEN 19
                        MOVE -1 TO STUPHN3L
                END-EVALUATE
                GO TO 400-ERROR-RETURN
           END-IF.

      * TODO: ATTEMPT TO FIND STUDENT RECORD IN STUFILE
      *       FROM USER INPUT STUDENT NUMBER
           MOVE STUNUMI TO STU-NUMBER.

           MOVE STUCR1AI TO WS-COURSE-CODE-1A.
           MOVE STUCR1BI TO WS-COURSE-CODE-1B.
           MOVE STUCR2AI TO WS-COURSE-CODE-2A.
           MOVE STUCR2BI TO WS-COURSE-CODE-2B.
           MOVE STUCR3AI TO WS-COURSE-CODE-3A.
           MOVE STUCR3BI TO WS-COURSE-CODE-3B.
           MOVE STUCR4AI TO WS-COURSE-CODE-4A.
           MOVE STUCR4BI TO WS-COURSE-CODE-4B.
           MOVE STUCR5AI TO WS-COURSE-CODE-5A.
           MOVE STUCR5BI TO WS-COURSE-CODE-5B.

           MOVE WS-COURSE-CODE-1 TO STU-COURSE-1.
           MOVE WS-COURSE-CODE-2 TO STU-COURSE-2.
           MOVE WS-COURSE-CODE-3 TO STU-COURSE-3.
           MOVE WS-COURSE-CODE-4 TO STU-COURSE-4.
           MOVE WS-COURSE-CODE-5 TO STU-COURSE-5.
           MOVE STUNAMEI TO STU-NAME.
           MOVE STUADD1I TO STU-ADDRESS-1.
           MOVE STUADD2I TO STU-ADDRESS-2.
           MOVE STUADD3I TO STU-ADDRESS-3.
           MOVE STUPOS1I TO STU-POSTAL-1.
           MOVE STUPOS2I TO STU-POSTAL-2.
           MOVE STUPHN1I TO STU-PHONE-1.
           MOVE STUPHN2I TO STU-PHONE-2.
           MOVE STUPHN3I TO STU-PHONE-3.

           EXEC CICS WRITE
                FILE('STUFILE')
                FROM(STUFILE-RECORD)
                RIDFLD(STU-KEY)
           END-EXEC.

      * RECORD FOUND, MOVE VALUES TO MAP OUTPUTS
           MOVE LOW-VALUES TO MAP1O.
           MOVE "RECORD SUCCESSFULLY WRITTEN!" TO MSGO.

      * TODO: MOVE VALUES FROM STUREC TO O FIELDS

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MC')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB82')
           END-EXEC.

       300-DUPREC.

           MOVE LOW-VALUES TO MAP1O.
           MOVE 'STUDENT ALREADY EXISTS' TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MC')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB82')
           END-EXEC.

       400-ERROR-RETURN.

           MOVE WS-MSG TO MSGO.

           EXEC CICS
                SEND MAP('MAP1')
                MAPSET('DCJB8MC')
                CURSOR
           END-EXEC.
           EXEC CICS RETURN TRANSID('JB82') END-EXEC.


       999-EXIT.

      *Lab 13: Instead of exiting program we have to go to menu now
           EXEC CICS XCTL
                PROGRAM('DCJB8PGM')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LEN)
           END-EXEC.
      *    MOVE LOW-VALUES TO MAP1O.
      *    MOVE 'PROGRAM ENDING' TO MSGO.
      *    EXEC CICS SEND MAP('MAP1') MAPSET('DCJB8MC') END-EXEC.
      *    EXEC CICS RETURN END-EXEC.

       END PROGRAM DCJB8PGC.
