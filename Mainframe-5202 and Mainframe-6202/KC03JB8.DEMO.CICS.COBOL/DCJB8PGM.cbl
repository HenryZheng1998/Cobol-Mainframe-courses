       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCJB8PGM.
       AUTHOR. HENRY ZHENG.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY 'DCJB8MM'.

       01 WS-COMMAREA PIC X(20) VALUE SPACES.
       01 WS-COMMAREA-LEN PIC S9(4) COMP VALUE 1.
       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X.

       PROCEDURE DIVISION.

           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
           END-EXEC.

           EXEC CICS HANDLE AID
                PF1 (300-CHOICE-ONE)
                PF2 (400-CHOICE-TWO)
                PF3 (500-CHOICE-THREE)
                PF4 (600-CHOICE-FOUR)
                PF12 (100-FIRST-TIME)
                PF9 (999-EXIT)
           END-EXEC.

           IF (EIBCALEN = 1) THEN
                GO TO 100-FIRST-TIME
           END-IF.

           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCJB8MM')
           END-EXEC.

           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.
      * SEND MAP AND RETURN CONTROL TO USER
           MOVE LOW-VALUES TO MAP1O.
           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCJB8MM')
               ERASE
           END-EXEC.

           EXEC CICS
                RETURN
                TRANSID('JB80')
           END-EXEC.

       200-MAIN-LOGIC.

           IF CHOICEL < 1 THEN
                GO TO 800-NO-INPUT
           END-IF.
           IF CHOICEI = '1'
      * EXECUTE PARAGRAPH FOR OPTION 1
                GO TO 300-CHOICE-ONE
           ELSE IF CHOICEI = '2'
      * EXECUTE PARAGRAPH FOR OPTION 2
                GO TO 400-CHOICE-TWO
           ELSE IF CHOICEI = '3'
      * EXECUTE PARAGRAPH FOR OPTION 3
                GO TO 500-CHOICE-THREE
           ELSE IF CHOICEI = '4'
      * EXECUTE PARAGRAPH FOR OPTION 4
                GO TO 600-CHOICE-FOUR
      * EXECUTE PARAGRAPH FOR OPTION 9
           ELSE IF CHOICEI = '9'
      * EXIT APPLICATION
                GO TO 999-EXIT
           ELSE IF CHOICEI IS ALPHABETIC
                GO TO 950-CHOICE-NONNUMERIC
           ELSE
      * INVALID CHOICE
                GO TO 900-CHOICE-INVALID

           END-IF
           END-IF.

       300-CHOICE-ONE.

      *    MOVE LOW-VALUES TO MAP1O.
      *
      *    MOVE 'CHOICE 1 - ENTER NEW STUDENT IS NOT AVAILABLE'
      *        TO MSGO.
      *
      *    EXEC CICS SEND
      *         MAP('MAP1')
      *         MAPSET('DCJB8MM')
      *    END-EXEC.
      *
      *    EXEC CICS RETURN
      *         TRANSID('JB80')
      *    END-EXEC.

           EXEC CICS XCTL
                PROGRAM('DCJB8PGC')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LEN)
           END-EXEC.

       400-CHOICE-TWO.

      *    MOVE LOW-VALUES TO MAP1O.
      *
      *    MOVE 'CHOICE 2 - INQUIRE BY STUDENT NUMBER IS NOT AVAILABLE'
      *        TO MSGO.
      *
      *    EXEC CICS SEND
      *         MAP('MAP1')
      *         MAPSET('DCJB8MM')
      *    END-EXEC.
      *
      *    EXEC CICS RETURN
      *         TRANSID('JB80')
      *    END-EXEC.

           EXEC CICS XCTL
                PROGRAM('DCJB8PGR')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LEN)
           END-EXEC.

       500-CHOICE-THREE.

           EXEC CICS XCTL
                PROGRAM('DCJB8PGU')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LEN)
           END-EXEC.

       600-CHOICE-FOUR.
      *    MOVE "A" TO WS-COMMAREA.
      *    EXEC CICS LINK
      *         PROGRAM('DCJB8PGE')
      *         COMMAREA(WS-COMMAREA)
      *         LENGTH(WS-COMMAREA-LEN)
      *    END-EXEC.
      *
      *    MOVE LOW-VALUES TO MAP1O
      *    MOVE WS-COMMAREA TO MSGO.

      *    MOVE LOW-VALUES TO MAP1O.
      *
      *    MOVE 'CHOICE 4 - SEARCH BY STUDENT NAME IS NOT AVAILABLE'
      *        TO MSGO.
      *
      *    EXEC CICS SEND
      *         MAP('MAP1')
      *         MAPSET('DCJB8MM')
      *    END-EXEC.
      *
      *    EXEC CICS RETURN
      *         TRANSID('JB80')
      *    END-EXEC.

           EXEC CICS XCTL
                PROGRAM('DCJB8PGB')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LEN)
           END-EXEC.


       800-NO-INPUT.
           MOVE LOW-VALUES TO MAP1O.
           MOVE 'PLEASE INPUT A NUMBER' TO MSGO.
           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MM')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB80')
           END-EXEC.

       900-CHOICE-INVALID.
           MOVE LOW-VALUES TO MAP1O.

           MOVE 'INVALID CHOICE, PLEASE SELECT A NUMBER FROM THE MENU'
               TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MM')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB80')
           END-EXEC.

       950-CHOICE-NONNUMERIC.
           MOVE LOW-VALUES TO MAP1O.

           MOVE 'INPUT IS NOT NUMERIC, PLEASE ENTER A NUMBER' TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MM')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB80')
           END-EXEC.

       999-EXIT.
           MOVE LOW-VALUES TO MAP1O.

           MOVE 'APPLICATION ENDING' TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MM')
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
       END PROGRAM DCJB8PGM.
