       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCJB8PGB.
       AUTHOR. HENRY ZHENG.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * COPY BROWSE MAP LAYOUT
       COPY 'DCJB8MB'.

       01 WS-COMMAREA PIC X(20) VALUE SPACES.
       01 WS-COMMAREA-LEN PIC S9(4) COMP VALUE 1.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'STUREC'.

       01 WS-MESSAGE             PIC X(60).
       01 WS-LINE-COUNT          PIC 99 VALUE 0.
       01 WS-STATUS              PIC S9(4) COMP.

      *Line output should be a certain length
       01 WS-STU-DETAIL.
           05 WS-NUMBER          PIC XX VALUE "00".
           05 FILLER             PIC X(4) VALUE SPACES.
           05 WS-STUNUM-OUT      PIC X(7).
           05 FILLER             PIC X(4) VALUE SPACES.
           05 WS-STUNAME-OUT     PIC X(20) VALUE SPACES.
           05 FILLER             PIC X(4) VALUE SPACES.
           05 WS-STUPHN1-OUT     PIC XXX.
           05 WS-DASH            PIC X VALUE "-".
           05 WS-STUPHN2-OUT     PIC XXX.
           05 WS-DASH2           PIC X VALUE "-".
           05 WS-STUPHN3-OUT     PIC X(4).

      * VARIABLES FOR PASSING DATA BETWEEN RUNS OF THIS PROGRAM
       01 WS-SAVE-DATA.
           05 FILLER             PIC X(3) VALUE SPACES.
           05 WS-INPUT-FLAG      PIC X    VALUE "X".
           05 WS-FORWARD-FLAG    PIC X    VALUE "F".
           05 WS-BACKWARD-FLAG   PIC X    VALUE "B".

       01 WS-TRANSFER-DATA.
           05 WS-STU-NUMT        PIC X(7).
       01 TRANSFER-LENGTH        PIC S9(4) COMP VALUE 7.

      * Use this to supress leading 0's when putting it into LINESO
       01 WS-XFER-INPUT          PIC Z9.
       01 WS-XFER-REVERSE        PIC XX.
       01 WS-XFER-REMOVESPACE    PIC XX.
       01 WS-TALLY               PIC 9.
       01 WS-SAVE-DATA-LEN       PIC S9(4) COMP VALUE 7.
       01 WS-DATA-LENGTH         PIC Z9.
       01 WS-XFER-NUMONLY        PIC 99.

       COPY DFHBMSCA.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X(144).

       PROCEDURE DIVISION.

       000-START-LOGIC.

           IF (EIBCALEN = 1) THEN
                GO TO 999-SEARCH-RETURN
           END-IF.

           EXEC CICS HANDLE CONDITION
      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
                MAPFAIL(999-SEARCH-RETURN)
                NOTFND(400-NOTFND)
           END-EXEC.

           EXEC CICS HANDLE AID
                PF2(700-TRANSFER-LOGIC)
                PF4(999-SEARCH-RETURN)
                PF7(300-BROWSE-BACK)
                PF8(205-BROWSE-FORWARD)
                PF9(999-EXIT-RETURN)
           END-EXEC.

      * TODO: HANDLE dupkey

           EXEC CICS IGNORE CONDITION DUPKEY END-EXEC.

      * ATTEMPT TO RECEIVE MAP FROM TERMINAL
           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCJB8MB')
           END-EXEC.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO MAP1O.
           PERFORM 600-BLOCK-INSERT-XFER.
           MOVE -1 TO STUNAMEL.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MB')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('JB84')
               COMMAREA(WS-SAVE-DATA)
               LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       200-MAIN-LOGIC.
           MOVE DFHCOMMAREA TO WS-SAVE-DATA.
      * validation for student name length is being weird
      * it gets filled with spaces to 20 and then bypassing it
      * TODO: implement browse logic
           IF (STUNAMEL = 0)
                MOVE LOW-VALUES TO MAP1O
                PERFORM 500-CLEAR-MAP
                     VARYING WS-LINE-COUNT
                     FROM 1 BY 1
                     UNTIL WS-LINE-COUNT > 10
                MOVE "X" TO WS-INPUT-FLAG
                MOVE "YOU MUST INPUT STUDENT NAME" TO MSGO
                PERFORM 600-BLOCK-INSERT-XFER
                GO TO 400-ERROR-RETURN

           END-IF.

           IF (STUNAMEI IS NOT ALPHABETIC)
                MOVE LOW-VALUES TO MAP1O
                PERFORM 500-CLEAR-MAP
                     VARYING WS-LINE-COUNT
                     FROM 1 BY 1
                     UNTIL WS-LINE-COUNT > 10
                MOVE "X" TO WS-INPUT-FLAG
                MOVE "STUDENT NAME MUST BE ALPHABETIC" TO MSGO
                PERFORM 600-BLOCK-INSERT-XFER
                GO TO 400-ERROR-RETURN
           END-IF.

           IF (STUNAMEL < 3)
      *    Need the low values as stuname gets filled with spaces
                Move LOW-VALUES TO MAP1O
                PERFORM 500-CLEAR-MAP
                     VARYING WS-LINE-COUNT
                     FROM 1 BY 1
                     UNTIL WS-LINE-COUNT > 10
                MOVE "X" TO WS-INPUT-FLAG
                MOVE "INPUT MUST BE GREATER THAN 3 LETTERS" TO MSGO
                PERFORM 600-BLOCK-INSERT-XFER
                GO TO 400-ERROR-RETURN
           END-IF.

      * PASSES VALIDATION
           MOVE STUNAMEI TO STU-NAME.
           MOVE LOW-VALUES TO MAP1O.
           MOVE "P" TO WS-INPUT-FLAG.

      *    Clears map in case of previous input
           MOVE 00 TO WS-LINE-COUNT.
           MOVE LOW-VALUES TO MAP1O.
           PERFORM 500-CLEAR-MAP
                VARYING WS-LINE-COUNT
                FROM 1 BY 1
                UNTIL WS-LINE-COUNT > 10.

           EXEC CICS STARTBR
                FILE('STUNAME')
                RIDFLD(STU-NAME)
           END-EXEC.

           PERFORM 210-FORWARD
                VARYING WS-LINE-COUNT
                FROM 1 BY 1
                UNTIL WS-LINE-COUNT > 10.

           EXEC CICS ENDBR
                FILE('STUNAME')
           END-EXEC.

           GO TO 999-BROWSE-RETURN.

       205-BROWSE-FORWARD.
           MOVE DFHCOMMAREA TO WS-SAVE-DATA.

           IF (WS-INPUT-FLAG = "X")
                MOVE LOW-VALUES TO MAP1O
                MOVE 'INPUT STUDENT NAME TO USE BROWSE FUNCTION KEYS'
                    TO MSGO
                GO TO 400-ERROR-RETURN
           END-IF.

           IF (WS-FORWARD-FLAG = "X")
                MOVE LOW-VALUES TO MAP1O
                MOVE
       'END OF FILE - ENTER NEW NAME, BROWSE BACK OR DO XFER UPDATE'
                    TO MSGO
                GO TO 400-ERROR-RETURN
           END-IF.

      *    RESET BACKWARD FLAG IN CASE THEY HIT BACK OF EOF
           MOVE "B" TO WS-BACKWARD-FLAG

           MOVE LINESI(10) TO WS-STU-DETAIL
           MOVE WS-STUNAME-OUT TO STU-NAME
      *    This is better because it's no longer based on the position
      *    MOVE LINESI(10)(18:20) TO STU-NAME.

           MOVE LOW-VALUES TO MAP1O.

           PERFORM 500-CLEAR-MAP
                VARYING WS-LINE-COUNT
                FROM 1 BY 1
                UNTIL WS-LINE-COUNT > 10.

           EXEC CICS STARTBR
                FILE('STUNAME')
                RIDFLD(STU-NAME)
           END-EXEC.

           PERFORM 210-FORWARD
                VARYING WS-LINE-COUNT
                FROM 1 BY 1
                UNTIL WS-LINE-COUNT > 10.

           EXEC CICS ENDBR
                FILE('STUNAME')
           END-EXEC.

           GO TO 999-BROWSE-RETURN.

       210-FORWARD.

           EXEC CICS READNEXT
                FILE('STUNAME')
                INTO(STUFILE-RECORD)
                LENGTH(STUFILE-LENGTH)
                RIDFLD(STU-NAME)
                RESP(WS-STATUS)
           END-EXEC.

           IF (WS-STATUS = DFHRESP(ENDFILE))
                MOVE "*** END OF FILE ***" TO LINESO(WS-LINE-COUNT)
                MOVE
       'END OF FILE - ENTER NEW NAME, BROWSE BACK OR DO XFER UPDATE'
                TO MSGO
                MOVE "X" TO WS-FORWARD-FLAG
                GO TO 400-ERROR-RETURN
           ELSE
                MOVE WS-LINE-COUNT TO WS-NUMBER
                MOVE STU-NAME TO WS-STUNAME-OUT
                MOVE STU-NUMBER TO WS-STUNUM-OUT
                MOVE STU-PHONE-1 TO WS-STUPHN1-OUT
                MOVE STU-PHONE-2 TO WS-STUPHN2-OUT
                MOVE STU-PHONE-3 TO WS-STUPHN3-OUT
                MOVE "-" TO WS-DASH
                MOVE "-" TO WS-DASH2
                MOVE WS-STU-DETAIL TO LINESO(WS-LINE-COUNT)
           END-IF.

       300-BROWSE-BACK.
           MOVE DFHCOMMAREA TO WS-SAVE-DATA.
           IF (WS-INPUT-FLAG = "X")
                MOVE LOW-VALUES TO MAP1O
                MOVE 'INPUT STUDENT NAME TO USE BROWSE FUNCTION KEYS'
                    TO MSGO
                PERFORM 600-BLOCK-INSERT-XFER
                GO TO 400-ERROR-RETURN
           END-IF.

           IF (WS-BACKWARD-FLAG = "X")
                MOVE LOW-VALUES TO MAP1O
                MOVE
       'END OF FILE - ENTER NEW NAME, BROWSE FRWRD OR DO XFER UPDATE'
                    TO MSGO
                GO TO 400-ERROR-RETURN
           END-IF.

      *    RESET FORWARD FLAG IN CASE THEY HIT EOF
           MOVE "F" TO WS-FORWARD-FLAG

           MOVE LINESI(1) TO WS-STU-DETAIL
           MOVE WS-STUNAME-OUT TO STU-NAME
      *    This is better because it's no longer based on the position
      *    MOVE LINESI(1)(18:20) TO STU-NAME.

           MOVE LOW-VALUES TO MAP1O.

           PERFORM 500-CLEAR-MAP
                VARYING WS-LINE-COUNT
                FROM 1 BY 1
                UNTIL WS-LINE-COUNT > 10.

           EXEC CICS STARTBR
                FILE('STUNAME')
                RIDFLD(STU-NAME)
           END-EXEC.

           PERFORM 310-BACKWARD
                VARYING WS-LINE-COUNT
                FROM 10 BY -1
                UNTIL WS-LINE-COUNT < 1.

           EXEC CICS ENDBR
                FILE('STUNAME')
           END-EXEC.

           GO TO 999-BROWSE-RETURN.

       310-BACKWARD.

           EXEC CICS READPREV
                FILE('STUNAME')
                INTO(STUFILE-RECORD)
                LENGTH(STUFILE-LENGTH)
                RIDFLD(STU-NAME)
                RESP(WS-STATUS)
           END-EXEC.

           IF (WS-STATUS = DFHRESP(ENDFILE))
                MOVE "*** END OF FILE ***" TO LINESO(WS-LINE-COUNT)
                MOVE
       'END OF FILE - ENTER NEW NAME, BROWSE FRWRD OR DO XFER UPDATE'
                TO MSGO
                MOVE "X" TO WS-BACKWARD-FLAG
                GO TO 400-ERROR-RETURN
           ELSE
                MOVE WS-LINE-COUNT TO WS-NUMBER
                MOVE STU-NAME TO WS-STUNAME-OUT
                MOVE STU-NUMBER TO WS-STUNUM-OUT
                MOVE STU-PHONE-1 TO WS-STUPHN1-OUT
                MOVE STU-PHONE-2 TO WS-STUPHN2-OUT
                MOVE STU-PHONE-3 TO WS-STUPHN3-OUT
                MOVE "-" TO WS-DASH
                MOVE "-" TO WS-DASH2
                MOVE WS-STU-DETAIL TO LINESO(WS-LINE-COUNT)
           END-IF.

       400-NOTFND.
           MOVE LOW-VALUES TO MAP1O.
           MOVE -1 TO STUNAMEL.
           PERFORM 500-CLEAR-MAP
               VARYING WS-LINE-COUNT
               FROM 1 BY 1
               UNTIL WS-LINE-COUNT > 10.

           MOVE 'STUDENT NOT FOUND, PLEASE ENTER STUDENT NAME' TO MSGO.
           MOVE 'X' TO WS-INPUT-FLAG.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MB')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('JB84')
                COMMAREA(WS-SAVE-DATA)
                LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       999-SEARCH-RETURN.

           MOVE LOW-VALUES TO MAP1O.
           PERFORM 600-BLOCK-INSERT-XFER.
           MOVE "PLEASE ENTER A STUDENT NAME" TO MSGO.
           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MB')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('JB84')
               COMMAREA(WS-SAVE-DATA)
               LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       999-BROWSE-RETURN.
      *    MOVE WS-XFER-REVERSE(1:WS-DATA-LENGTH) TO MSGO.
           MOVE "BROWSE STUDENT NAME WITH PF KEYS BELOW"
                TO MSGO.

           MOVE -1 TO XFERL.
      *    Below will clear values, need to implement logic from update
      *    mode and send values to itself for validation
           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MB')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('JB84')
               COMMAREA(WS-SAVE-DATA)
               LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.


       400-ERROR-RETURN.
           MOVE -1 TO STUNAMEL.
           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MB')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('JB84')
               COMMAREA(WS-SAVE-DATA)
               LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       400-XFER-ERROR-RETURN.
           MOVE -1 TO XFERL.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MB')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('JB84')
               COMMAREA(WS-SAVE-DATA)
               LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       500-CLEAR-MAP.

           MOVE SPACES TO WS-STU-DETAIL.
           MOVE WS-STU-DETAIL TO LINESO(WS-LINE-COUNT).

       600-BLOCK-INSERT-NAME.
           MOVE DFHBMASF TO STUNAMEA.

       600-BLOCK-INSERT-XFER.
           MOVE DFHBMASF TO XFERA.
           MOVE SPACES TO XFERO.

       700-TRANSFER-LOGIC.
           MOVE DFHCOMMAREA TO WS-SAVE-DATA.

      *    Do Validation
           IF (XFERL = 0)
                MOVE LOW-VALUES TO MAP1O
                MOVE
           "BROWSE STUDENTS WITH PF KEYS - PLEASE ENTER LINE# TO UPDATE"
                TO MSGO
                GO TO 400-XFER-ERROR-RETURN
           END-IF.

           IF (XFERI IS ALPHABETIC)
                MOVE LOW-VALUES TO MAP1O
                MOVE
           "BROWSE STUDENTS WITH PF KEYS - XFER INPUT MUST BE NUMERIC"
                TO MSGO
                GO TO 400-XFER-ERROR-RETURN
           END-IF.

      * Deals with input of #_ <- Blank space
      * If I don't have it, it will trip the > 10 validation
      * Does not like #_ without the reverse function below
           MOVE XFERI TO WS-XFER-REVERSE.
           MOVE 0 TO WS-TALLY.
           MOVE 0 TO WS-DATA-LENGTH.
           INSPECT FUNCTION REVERSE (WS-XFER-REVERSE) TALLYING WS-TALLY
                FOR LEADING SPACE, LEADING LOW-VALUES.
                COMPUTE WS-DATA-LENGTH
                    = LENGTH OF WS-XFER-REVERSE - WS-TALLY.
                MOVE WS-XFER-REVERSE(1:WS-DATA-LENGTH) TO WS-XFER-INPUT.

           IF (WS-XFER-INPUT > 10)
                MOVE LOW-VALUES TO MAP1O
                MOVE
           "BROWSE STUDENTS WITH PF KEYS - INPUT MUST BE LESS THAN 10"
                TO MSGO
                GO TO 400-XFER-ERROR-RETURN
           END-IF.

      *Catches 0_
           IF (XFERI <= 0 or XFERI = 00)
                MOVE LOW-VALUES TO MAP1O
      *         MOVE WS-XFER-INPUT TO MSGO
                MOVE
       "BROWSE STUDENTS WITH PF KEYS - INPUT MUST BE GREATER THAN 0"
                TO MSGO
                GO TO 400-XFER-ERROR-RETURN
           END-IF.

      * Need to check if number line actually exists
           MOVE WS-XFER-INPUT TO WS-XFER-NUMONLY.
           MOVE LINESI(WS-XFER-NUMONLY) TO WS-STU-DETAIL.
           IF (WS-NUMBER IS NOT NUMERIC)
                MOVE
       "BROWSE STUDENTS WITH PF KEYS - LINE# DOES NOT EXIST"
                TO MSGO
      *         MOVE WS-NUMBER TO MSGO
                GO TO 400-XFER-ERROR-RETURN
           END-IF.

      *    Final Validation done
           MOVE WS-STUNUM-OUT TO WS-STU-NUMT.

      *    MOVE WS-TRANSFER-DATA TO DFHCOMMAREA.
      *    Do the XTCL

      *    Go to 999-xfer-return.
           EXEC CICS XCTL
                PROGRAM('DCJB8PGU')
                COMMAREA(WS-TRANSFER-DATA)
                LENGTH(TRANSFER-LENGTH)
           END-EXEC.

      * PF2 transfers only
       999-XFER-RETURN.
           MOVE -1 TO XFERL.
           MOVE "TRANSFER VALIDATION GOOD" TO MSGO.
           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCJB8MB')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('JB84')
               COMMAREA(WS-SAVE-DATA)
               LENGTH(WS-SAVE-DATA-LEN)
           END-EXEC.

       999-EXIT-RETURN.
      *Lab 13: Instead of exiting program we have to go to menu now
           EXEC CICS XCTL
                PROGRAM('DCJB8PGM')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LEN)
           END-EXEC.
      *    MOVE LOW-VALUES TO MAP1O.
      *    MOVE 'PROGRAM ENDING' TO MSGO.
      *    EXEC CICS SEND MAP('MAP1') MAPSET('DCJB8MB') END-EXEC.
      *    EXEC CICS RETURN END-EXEC.

       END PROGRAM DCJB8PGB.
