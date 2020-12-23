       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCJB8PGR.
       AUTHOR. HENRY ZHENG.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-COMMAREA PIC X(20) VALUE SPACES.
       01 WS-COMMAREA-LEN PIC S9(4) COMP VALUE 1.

       01 WS-PHONE-LINE.
           05 FILLER        PIC XX VALUE "( ".
           05 WS-STU-PHONE1 PIC XXX.
           05 FILLER        PIC XXX VALUE " ) ".
           05 WS-STU-PHONE2 PIC XXX.
           05 FILLER        PIC XXX VALUE " - ".
           05 WS-STU-PHONE3 PIC X(4).

       01 WS-MSG            PIC X(40).
      * COPY INQUIRY MAP LAYOUT
       COPY 'DCJB8MR'.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'STUREC'.


       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X.

       PROCEDURE DIVISION.

       000-START-LOGIC.

      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
      * TODO: ADD CONDITION FOR RECORD NOT FOUND IN STUFILE
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
                NOTFND(300-NOTFND)
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
                MAPSET('DCJB8MR')
           END-EXEC.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCJB8MR')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('JB81')
           END-EXEC.

       200-MAIN-LOGIC.
      * TODO: IMPLEMENT VALIDATION LOGIC
           IF(STUNUMI = 'XXXXXXX')
               GO TO 999-EXIT
           END-IF.

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
      * TODO: ATTEMPT TO FIND STUDENT RECORD IN STUFILE
      *       FROM USER INPUT STUDENT NUMBER
           MOVE STUNUMI TO STU-NUMBER.

           EXEC CICS READ
                FILE('STUFILE')
                INTO(STUFILE-RECORD)
                RIDFLD(STU-KEY)
           END-EXEC.

      * RECORD FOUND, MOVE VALUES TO MAP OUTPUTS
           MOVE LOW-VALUES TO MAP1O.
           MOVE "RECORD FOUND!" TO MSGO.

           MOVE STU-NUMBER    TO STUNUMO.
           MOVE STU-NAME      TO STUNAMEO.
           MOVE STU-COURSE-1(1:4) TO STUCR1AO.
           MOVE STU-COURSE-1(5:4) TO STUCR1BO.
           MOVE STU-COURSE-2(1:4) TO STUCR2AO.
           MOVE STU-COURSE-2(5:4) TO STUCR2BO.
           MOVE STU-COURSE-3(1:4) TO STUCR3AO.
           MOVE STU-COURSE-3(5:4) TO STUCR3BO.
           MOVE STU-COURSE-4(1:4) TO STUCR4AO.
           MOVE STU-COURSE-4(5:4) TO STUCR4BO.
           MOVE STU-COURSE-5(1:4) TO STUCR5AO.
           MOVE STU-COURSE-5(5:4) TO STUCR5BO.
           MOVE STU-ADDRESS-1 TO STUADD1O.
           MOVE STU-ADDRESS-2 TO STUADD2O.
           MOVE STU-ADDRESS-3 TO STUADD3O.
           MOVE STU-POSTAL-1  TO STUPOS1O.
           MOVE STU-POSTAL-2  TO STUPOS2O.
           MOVE STU-PHONE-1   TO WS-STU-PHONE1.
           MOVE STU-PHONE-2   TO WS-STU-PHONE2.
           MOVE STU-PHONE-3   TO WS-STU-PHONE3.
           MOVE WS-PHONE-LINE TO STUPHNLO.
      * TODO: MOVE VALUES FROM STUREC TO O FIELDS

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MR')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB81')
           END-EXEC.

       300-NOTFND.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MR')
                ERASE
           END-EXEC.

           MOVE LOW-VALUES TO MAP1O.
           MOVE 'STUDENT NOT FOUND' TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MR')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB81')
           END-EXEC.

       500-INQUIRY-ERROR.

           MOVE WS-MSG TO MSGO.

           MOVE -1 TO STUNUML.

           EXEC CICS
                SEND MAP('MAP1')
                MAPSET('DCJB8MR')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB81')
           END-EXEC.

       999-EXIT.
      *Lab 13: Instead of exiting program we have to go to menu now
           EXEC CICS XCTL
                PROGRAM('DCJB8PGM')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LEN)
           END-EXEC.

      *    MOVE LOW-VALUES TO MAP1O.
      *    MOVE 'PROGRAM ENDING' TO MSGO.
      *    EXEC CICS SEND MAP('MAP1') MAPSET('DCJB8MR') END-EXEC.
      *    EXEC CICS RETURN END-EXEC.

       END PROGRAM DCJB8PGR.
