       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCJB8PGU.
       AUTHOR. HENRY ZHENG.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-COMMAREA PIC X(20) VALUE SPACES.
       01 WS-COMMAREA-LEN PIC S9(4) COMP VALUE 1.

      * COPY INQUIRY MAP LAYOUT
       COPY 'DCJB8MC'.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'STUREC'.
       COPY 'STUDATA'.

       01 WS-MSG           PIC X(40)
           VALUE SPACE.

      * VARIABLES FOR PASSING DATA BETWEEN RUNS OF THIS PROGRAM
       01 WS-SAVE-DATA.
           05 WS-MODE        PIC X
                VALUE 'I'.
           05 WS-STU-NUMBER  PIC X(7).
           05 WS-STU-NAME    PIC X(20).
           05 WS-STU-COURSE-1A PIC X(4).
           05 WS-STU-COURSE-1B PIC X(4).
           05 WS-STU-COURSE-2A PIC X(4).
           05 WS-STU-COURSE-2B PIC X(4).
           05 WS-STU-COURSE-3A PIC X(4).
           05 WS-STU-COURSE-3B PIC X(4).
           05 WS-STU-COURSE-4A PIC X(4).
           05 WS-STU-COURSE-4B PIC X(4).
           05 WS-STU-COURSE-5A PIC X(4).
           05 WS-STU-COURSE-5B PIC X(4).
           05 WS-STU-ADD1      PIC X(20).
           05 WS-STU-ADD2      PIC X(20).
           05 WS-STU-ADD3      PIC X(20).
           05 WS-STU-POS1      PIC X(3).
           05 WS-STU-POS2      PIC X(3).
           05 WS-STUPHONE1    PIC X(3).
           05 WS-STUPHONE2    PIC X(3).
           05 WS-STUPHONE3    PIC X(4).

      * VARIABLES FOR PASSING DATA BETWEEN PGB AND PGU
       01 WS-PGB-DATA.
           05 WS-STU-NUMT        PIC X(7).

       01 WS-SAVE-DATA-LEN   PIC S9(4) COMP
           VALUE 150.

       01 WS-ERRORNUM           PIC 9 VALUE 1.
      *01 WS-COMMAREA           PIC X(400).

      *01 WS-COMMAREA-LENGTH    PIC S9(4) COMP VALUE 144.

       01 WS-ERRORMSG           PIC x(40).

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

      * THE BELOW FILLER VARIABLE MUST BE PRESENT BETWEEN DFHBMSCA
      * AND ALL OTHER VARIABLES
       01 FILLER             PIC X(1024)
           VALUE SPACES.

       COPY DFHBMSCA.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X(144).

       PROCEDURE DIVISION.

       000-START-LOGIC.

      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
      * TODO: ADD CONDITION FOR RECORD NOT FOUND IN STUFILE
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
                NOTFND(300-NOTFND)
           END-EXEC.

           IF (EIBCALEN = 7) THEN
                GO TO 210-TRANSFER
           END-IF.

           IF (EIBCALEN = 1) THEN
                GO TO 100-FIRST-TIME
           END-IF.

      *    WILL ALWAYS EXIT THE MAIN LOGIC
      * TODO: NOTIFY USERS ABOUT RESET AND EXIT KEYS
           EXEC CICS HANDLE AID
      *         RESETS THE PROGRAM,
                PF4(100-FIRST-TIME)
      *         EXITS THE PROGRAM
                PF9(999-EXIT)
           END-EXEC.

      * ATTEMPT TO RECEIVE MAP FROM TERMINAL
           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCJB8MC')
           END-EXEC.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           PERFORM 400-PREP-INQUIRY.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCJB8MC')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('JB83')
               COMMAREA(WS-SAVE-DATA)
               LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       200-MAIN-LOGIC.

           MOVE DFHCOMMAREA TO WS-SAVE-DATA.

           IF(STUNUMI = 'XXXXXXX')
               GO TO 999-EXIT
           END-IF.

           IF (WS-MODE = 'I') THEN
      * GO TO INQUIRY MODE
                GO TO 225-INQUIRY
           ELSE IF (WS-MODE = 'U')
                GO TO 250-UPDATE
      * GO TO UPDATE MODE
           ELSE
      * PROVIDE ERROR MESSAGE
                MOVE LOW-VALUES TO MAP1O
                MOVE "INVALID MODE" TO MSGO

      * MOVE VALUES FROM STUREC TO O FIELDS

                EXEC CICS SEND
                     MAP('MAP1')
                     MAPSET('DCJB8MC')
                     ERASE
                END-EXEC

                EXEC CICS RETURN
                     TRANSID('JB83')
                     COMMAREA(WS-SAVE-DATA)
                     LENGTH(WS-SAVE-DATA-LEN)
                END-EXEC
           END-IF.


           MOVE LOW-VALUES TO MAP1O.
           MOVE "NOTHING HAS HAPPENED YET..." TO MSGO.


      * MOVE VALUES FROM STUREC TO O FIELDS

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MC')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB83')
                COMMAREA(WS-SAVE-DATA)
                LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       210-TRANSFER.
           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MC')
                CURSOR
                ERASE
           END-EXEC.
           PERFORM 400-PREP-INQUIRY.

      *    eibcalen = 7
           MOVE DFHCOMMAREA(1:7) TO WS-PGB-DATA.
           MOVE WS-STU-NUMT TO STUNUMO STUNUMI.
           MOVE EIBCALEN TO STUNUML.

           GO TO 225-INQUIRY.

       225-INQUIRY.
      * CHECK IF INPUT IS EMPTY
           IF STUNUML = 0
              MOVE 'PLEASE ENTER STUDENT NUMBER' TO WS-MSG
              GO TO 500-INQUIRY-ERROR
           END-IF.
      * PERFORM INPUT VALIDATION
           IF STUNUML < 7
              MOVE 'STUDENT NUMBER MUST BE 7 CHARACTERS' TO WS-MSG
              GO TO 500-INQUIRY-ERROR
           ELSE IF STUNUMI IS NOT NUMERIC
              MOVE 'STUDENT NUMBER MUST BE NUMBERIC' TO WS-MSG
              GO TO 500-INQUIRY-ERROR
           END-IF
           END-IF.
      * ASSUME THAT VALIDATION WAS PASSED
           MOVE STUNUMI TO STU-NUMBER.
      *
           EXEC CICS READ
                FILE('STUFILE')
                INTO(STUFILE-RECORD)
                RIDFLD(STU-KEY)
           END-EXEC.


           PERFORM 450-PREPARE-UPDATE.
           MOVE "STUDENT FOUND!" TO MSGO.

      *    MOVE RECORD TO OUTPUT
      *    ALL THE THINGS YOU WILL NEED TO UPDATE
      *    Passes the variables to the output and checker respectively
           MOVE STU-NUMBER TO STUNUMO WS-STU-NUMBER.
           MOVE STU-COURSE-1(1:4) TO STUCR1AO WS-STU-COURSE-1A.
           MOVE STU-COURSE-1(5:4) TO STUCR1BO WS-STU-COURSE-1B.
           MOVE STU-COURSE-2(1:4) TO STUCR2AO WS-STU-COURSE-2A.
           MOVE STU-COURSE-2(5:4) TO STUCR2BO WS-STU-COURSE-2B.
           MOVE STU-COURSE-3(1:4) TO STUCR3AO WS-STU-COURSE-3A.
           MOVE STU-COURSE-3(5:4) TO STUCR3BO WS-STU-COURSE-3B.
           MOVE STU-COURSE-4(1:4) TO STUCR4AO WS-STU-COURSE-4A.
           MOVE STU-COURSE-4(5:4) TO STUCR4BO WS-STU-COURSE-4B.
           MOVE STU-COURSE-5(1:4) TO STUCR5AO WS-STU-COURSE-5A.
           MOVE STU-COURSE-5(5:4) TO STUCR5BO WS-STU-COURSE-5B.
           MOVE STU-NAME      TO STUNAMEO WS-STU-NAME.
           MOVE STU-ADDRESS-1 TO STUADD1O WS-STU-ADD1.
           MOVE STU-ADDRESS-2 TO STUADD2O WS-STU-ADD2.
           MOVE STU-ADDRESS-3 TO STUADD3O WS-STU-ADD3.
           MOVE STU-POSTAL-1  TO STUPOS1O WS-STU-POS1.
           MOVE STU-POSTAL-2  TO STUPOS2O WS-STU-POS2.
           MOVE STU-PHONE-1   TO STUPHN1O WS-STUPHONE1.
           MOVE STU-PHONE-2   TO STUPHN2O WS-STUPHONE2.
           MOVE STU-PHONE-3   TO STUPHN3O WS-STUPHONE3.

           MOVE -1 TO STUCR1AL.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MC')
                CURSOR
                ERASE
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB83')
                COMMAREA(WS-SAVE-DATA)
                LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.


       250-UPDATE.
      * CHECK IF ANY CHANGES HAS BEEN MADE
           IF ( STUCR1AI IS EQUAL TO WS-STU-COURSE-1A AND
                STUCR1BI IS EQUAL TO WS-STU-COURSE-1B AND
                STUCR2AI IS EQUAL TO WS-STU-COURSE-2A AND
                STUCR2BI IS EQUAL TO WS-STU-COURSE-2B AND
                STUCR3AI IS EQUAL TO WS-STU-COURSE-3A AND
                STUCR3BI IS EQUAL TO WS-STU-COURSE-3B AND
                STUCR4AI IS EQUAL TO WS-STU-COURSE-4A AND
                STUCR4BI IS EQUAL TO WS-STU-COURSE-4B AND
                STUCR5AI IS EQUAL TO WS-STU-COURSE-5A AND
                STUCR5BI IS EQUAL TO WS-STU-COURSE-5B AND
                STUNAMEI IS EQUAL TO WS-STU-NAME AND
                STUADD1I IS EQUAL TO WS-STU-ADD1 AND
                STUADD2I IS EQUAL TO WS-STU-ADD2 AND
                STUADD3I IS EQUAL TO WS-STU-ADD3 AND
                STUPHN1I IS EQUAL TO WS-STUPHONE1 AND
                STUPHN2I IS EQUAL TO WS-STUPHONE2 AND
                STUPHN3I EQUAL TO WS-STUPHONE3) THEN
      *         IF NO CHANGES HAS BEEN MADE
                PERFORM 400-PREP-INQUIRY

                MOVE "NOTHING HAS CHANGED, PLEASE ENTER STUDENT NUMBER"
                     TO MSGO

                EXEC CICS SEND
                     MAP('MAP1')
                     MAPSET('DCJB8MC')
                     ERASE
                END-EXEC

                EXEC CICS RETURN
                     TRANSID('JB83')
                     COMMAREA(WS-SAVE-DATA)
                     LENGTH(WS-SAVE-DATA-LEN)
                END-EXEC
           ELSE
      * IF CHANGES ARE DETECTED, VALIDATE
      * Moving the variables to STUDATA
           MOVE STUNUML to STU-NUMBERL-T
           MOVE STUNUMI TO STU-NUMBERI-T
           MOVE STUCR1AL TO STU-CR1AL-T
           MOVE STUCR1BL TO STU-CR1BL-T
           MOVE STUCR1AI TO STU-CR1AI-T
           MOVE STUCR1BI TO STU-CR1BI-T
           MOVE STUCR2AL TO STU-CR2AL-T
           MOVE STUCR2BL TO STU-CR2BL-T
           MOVE STUCR2AI TO STU-CR2AI-T
           MOVE STUCR2BI TO STU-CR2BI-T
           MOVE STUCR3AL TO STU-CR3AL-T
           MOVE STUCR3BL TO STU-CR3BL-T
           MOVE STUCR3AI TO STU-CR3AI-T
           MOVE STUCR3BI TO STU-CR3BI-T
           MOVE STUCR4AL TO STU-CR4AL-T
           MOVE STUCR4BL TO STU-CR4BL-T
           MOVE STUCR4AI TO STU-CR4AI-T
           MOVE STUCR4BI TO STU-CR4BI-T
           MOVE STUCR5AL TO STU-CR5AL-T
           MOVE STUCR5BL TO STU-CR5BL-T
           MOVE STUCR5AI TO STU-CR5AI-T
           MOVE STUCR5BI TO STU-CR5BI-T
           MOVE STUNAMEL TO STU-NAMEL-T
           MOVE STUNAMEI TO STU-NAMEI-T
           MOVE STUADD1L TO STU-ADD1L-T
           MOVE STUADD2L TO STU-ADD2L-T
           MOVE STUPOS1L TO STU-POST1L-T
           MOVE STUPOS1I TO STU-POST1I-T
           MOVE STUPOS2L TO STU-POST2L-T
           MOVE STUPOS2I TO STU-POST2I-T
           MOVE STUPHN1L TO STU-PHN1L-T
           MOVE STUPHN1I TO STU-PHN1I-T
           MOVE STUPHN2L TO STU-PHN2L-T
           MOVE STUPHN2I TO STU-PHN2I-T
           MOVE STUPHN3L TO STU-PHN3L-T
           MOVE STUPHN3I TO STU-PHN3I-T
           MOVE 00 TO STU-ERRORCODE
      *    MOVE STUDATA-RECORD TO WS-COMMAREA.

           EXEC CICS LINK
                PROGRAM('DCJB8PGE')
                COMMAREA(STUDATA-RECORD)
                LENGTH(STUDATA-LENGTH)
           END-EXEC

      *    If error code is not 0 - error has occured
           If STU-ERRORCODE OF STUDATA-RECORD IS NOT EQUAL TO
           WS-NOERROR-CONST
                MOVE STU-ERRORMSG TO WS-MSG
                GO TO 600-ERROR-RETURN
           END-IF
      *    If validation is good updates record

                 MOVE STUNUMI TO STU-NUMBER

                 EXEC CICS READ
                      FILE('STUFILE')
                      RIDFLD(STU-KEY)
                      INTO(STUFILE-RECORD)
                      UPDATE
                 END-EXEC

      *          MOVE THE REST OF THE INPUTS TO STUREC
                 MOVE STUNAMEI TO STU-NAME

                 MOVE STUCR1AI TO WS-COURSE-CODE-1A
                 MOVE STUCR1BI TO WS-COURSE-CODE-1B
                 MOVE STUCR2AI TO WS-COURSE-CODE-2A
                 MOVE STUCR2BI TO WS-COURSE-CODE-2B
                 MOVE STUCR3AI TO WS-COURSE-CODE-3A
                 MOVE STUCR3BI TO WS-COURSE-CODE-3B
                 MOVE STUCR4AI TO WS-COURSE-CODE-4A
                 MOVE STUCR4BI TO WS-COURSE-CODE-4B
                 MOVE STUCR5AI TO WS-COURSE-CODE-5A
                 MOVE STUCR5BI TO WS-COURSE-CODE-5B

                 MOVE WS-COURSE-CODE-1 TO STU-COURSE-1
                 MOVE WS-COURSE-CODE-2 TO STU-COURSE-2
                 MOVE WS-COURSE-CODE-3 TO STU-COURSE-3
                 MOVE WS-COURSE-CODE-4 TO STU-COURSE-4
                 MOVE WS-COURSE-CODE-5 TO STU-COURSE-5

                 MOVE STUADD1I TO STU-ADDRESS-1
                 MOVE STUADD2I TO STU-ADDRESS-2
                 MOVE STUADD3I TO STU-ADDRESS-3

                 MOVE STUPOS1I TO STU-POSTAL-1
                 MOVE STUPOS2I TO STU-POSTAL-2

                 MOVE STUPHN1I TO WS-STU-PHONE1
                 MOVE STUPHN2I TO WS-STU-PHONE2
                 MOVE STUPHN3I TO WS-STU-PHONE3
                 EXEC CICS REWRITE
                      FILE('STUFILE')
                      FROM(STUFILE-RECORD)
                      LENGTH(STUFILE-LENGTH)
                 END-EXEC

                PERFORM 400-PREP-INQUIRY
                MOVE "RECORD UPDATED. PLEASE ENTER STUDENT NUMBER"
                     TO MSGO

                EXEC CICS SEND
                     MAP('MAP1')
                     MAPSET('DCJB8MC')
                     ERASE
                END-EXEC

                EXEC CICS RETURN
                     TRANSID('JB83')
                     COMMAREA(WS-SAVE-DATA)
                     LENGTH(WS-SAVE-DATA-LEN)
                END-EXEC
           END-IF.

           PERFORM 400-PREP-INQUIRY.
                MOVE "RECORD UPDATED" TO MSGO.

                EXEC CICS SEND
                     MAP('MAP1')
                     MAPSET('DCJB8MC')
                END-EXEC.

                EXEC CICS RETURN
                     TRANSID('JB83')
                     COMMAREA(WS-SAVE-DATA)
                     LENGTH(WS-SAVE-DATA-LEN)
                END-EXEC.



       300-NOTFND.

           PERFORM 400-PREP-INQUIRY.
           MOVE 'STUDENT NOT FOUND. PLEASE ENTER STUDENT NUMBER'
           TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MC')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB83')
                COMMAREA(WS-SAVE-DATA)
                LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       400-PREP-INQUIRY.

           MOVE LOW-VALUES TO MAP1O.

           MOVE "U P D A T E     S C R E E N" TO TITLEO.

      * Disables these fields
           MOVE DFHBMASF TO STUCR1AA STUCR1BA STUCR2AA STUCR2BA
           STUCR3AA STUCR3BA STUCR4AA STUCR4BA STUCR5AA STUCR5BA
           STUNAMEA STUADD1A STUADD2A STUADD3A STUPOS1A STUPOS2A
           STUPHN1A STUPHN2A STUPHN3A.

           MOVE "I" TO WS-MODE.

       500-INQUIRY-ERROR.

           PERFORM 400-PREP-INQUIRY.

           MOVE WS-MSG TO MSGO.

           MOVE -1 TO STUNUML.

           EXEC CICS
                SEND MAP('MAP1')
                MAPSET('DCJB8MC')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB83')
                COMMAREA(WS-SAVE-DATA)
                LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       600-ERROR-RETURN.

           PERFORM 450-PREPARE-UPDATE.

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
           END-EVALUATE.

           MOVE WS-MSG TO MSGO.

           EXEC CICS
                SEND MAP('MAP1')
                MAPSET('DCJB8MC')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB83')
                COMMAREA(WS-SAVE-DATA)
                LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       450-PREPARE-UPDATE.

           MOVE LOW-VALUES TO MAP1O.

           MOVE "U P D A T E     S C R E E N" TO TITLEO.
      * Disables these fields
           MOVE DFHBMASF TO STUNUMA.

           MOVE "U" TO WS-MODE.

       999-EXIT.
      *Lab 13: Instead of exiting program we have to go to menu now
           EXEC CICS XCTL
                PROGRAM('DCJB8PGM')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LEN)
           END-EXEC.

      *    MOVE LOW-VALUES TO MAP1O.
      *    MOVE "U P D A T E     S C R E E N" TO TITLEO.
      *    MOVE 'PROGRAM ENDING' TO MSGO.
      *    EXEC CICS SEND MAP('MAP1') MAPSET('DCJB8MC') END-EXEC.
      *    EXEC CICS RETURN END-EXEC.

       END PROGRAM DCJB8PGU.