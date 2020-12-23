       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCJB8WSB.
       AUTHOR. HENRY ZHENG.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-FULLPHONES.
           05 WS-PHONE1             PIC X(3).
           05 WS-PHONE2             PIC X(3).
           05 WS-PHONE3             PIC X(4).

       01 WS-LINE-COUNT          PIC 99 VALUE 0.
       01 WS-STATUS              PIC S9(4) COMP.
       01 STUDENTS2-NUM          PIC S9(9) COMP-5 SYNC.
      * 09 and 12 DOESN'T MEAN ANYTHING SAME AS 05
      * comp-5 sync: It's weird because it doesn't the picture doesn't
      * give the whole picture or the entire information
      * https://tinyurl.com/y3qzqdah

       01 WS-STU-COUNT              PIC 99 VALUE 0.
       01 WS-COUNT-SPACES           PIC 9999.

       Copy 'WSBREQ'.
       COPY 'WSBRESP'.
       COPY 'STUREC'.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X(441).

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA TO WSBSTU-REQ.
           MOVE LOW-VALUES TO DFHCOMMAREA.

       000-START-LOGIC.

      * Handles can't find student name
           EXEC CICS HANDLE CONDITION
                NOTFND(300-NOTFND)
                ENDFILE(400-EOF)
           END-EXEC.

           EXEC CICS IGNORE CONDITION DUPKEY END-EXEC.

           GO TO 200-MAIN-LOGIC.

       200-MAIN-LOGIC.
      * VALIDATION LOGIC

           MOVE 0 TO WS-COUNT-SPACES.

           INSPECT FUNCTION REVERSE (STUNAME-REQ OF WSBSTU-REQ)
                TALLYING WS-COUNT-SPACES
                FOR LEADING SPACE.

           COMPUTE WS-COUNT-SPACES =
                LENGTH OF STUNAME-REQ OF WSBSTU-REQ - WS-COUNT-SPACES.

           IF (WS-COUNT-SPACES = 0)
                MOVE LOW-VALUES TO STU-RESP
                MOVE 'Please enter a student Name' TO MSG
                    OF student-resp
                MOVE "1" TO STATUS-CODE OF student-resp
                MOVE STU-RESP TO DFHCOMMAREA
                EXEC CICS RETURN END-EXEC
           END-IF.

           IF (STUNAME-REQ OF WSBSTU-REQ IS NOT ALPHABETIC)
                MOVE LOW-VALUES TO STU-RESP
                MOVE 'STUDENT NAME MUST BE ALPHABETIC'
                    TO MSG OF student-resp
                MOVE "1" TO STATUS-CODE OF student-resp
                MOVE STU-RESP TO DFHCOMMAREA
                EXEC CICS RETURN END-EXEC
           END-IF.

           IF (WS-COUNT-SPACES < 3)
                MOVE LOW-VALUES TO STU-RESP
                MOVE 'STUDENT NAME MUST BE AT LEAST 3 CHARACTERS LONG'
                    TO MSG OF student-resp
                MOVE "1" TO STATUS-CODE OF student-resp
                MOVE STU-RESP TO DFHCOMMAREA
                EXEC CICS RETURN END-EXEC
           END-IF.

      * PASSES VALIDATION

      * After it passes validation
      * Student name is used in Start BR

           MOVE "0" TO STATUS-CODE OF student-resp.
           MOVE "Success" TO MSG OF student-resp.
      *    Expected 10 students in list
           MOVE 10 TO STUDENTS2-NUM.
           MOVE STUNAME-REQ OF WSBSTU-REQ TO STU-NAME OF STUFILE-RECORD

           EXEC CICS STARTBR
                FILE('STUNAME')
                RIDFLD(STU-NAME OF STUFILE-RECORD)
           END-EXEC.

           PERFORM 210-FORWARD
                VARYING WS-LINE-COUNT
                FROM 1 BY 1
                UNTIL WS-LINE-COUNT > 10.

           EXEC CICS ENDBR
                FILE('STUNAME')
           END-EXEC.

           SUBTRACT 1 FROM WS-LINE-COUNT GIVING STUDENTS2-NUM.
           MOVE STUDENTS2-NUM TO stuArray2-num.
           MOVE STU-RESP TO DFHCOMMAREA.
           EXEC CICS RETURN END-EXEC.

       210-FORWARD.
           EXEC CICS READNEXT
                FILE('STUNAME')
                INTO(STUFILE-RECORD)
                LENGTH(STUFILE-LENGTH)
                RIDFLD(STU-NAME OF STUFILE-RECORD)
                RESP(WS-STATUS)
           END-EXEC.

           IF (WS-STATUS = DFHRESP(ENDFILE))
                SUBTRACT 1 FROM WS-LINE-COUNT GIVING STUDENTS2-NUM
                MOVE STUDENTS2-NUM TO stuArray2-num
                MOVE '*** SUCCESS: END OF FILE ***'
                    TO MSG OF student-resp
                MOVE STU-RESP TO DFHCOMMAREA
                EXEC CICS RETURN END-EXEC
           ELSE
                MOVE STU-NAME OF STUFILE-RECORD
                    TO STUNAME(WS-LINE-COUNT)
                MOVE STU-NUMBER OF STUFILE-RECORD
                    TO STUNUM(WS-LINE-COUNT)
                MOVE STU-PHONE-1 TO WS-PHONE1
                MOVE STU-PHONE-2 TO WS-PHONE2
                MOVE STU-PHONE-3 TO WS-PHONE3
                MOVE WS-FULLPHONES
                    TO PHONENUM(WS-LINE-COUNT)
           END-IF.

       300-NOTFND.
           MOVE LOW-VALUES TO STU-RESP.
           MOVE 'STUDENT NOT FOUND' TO MSG OF student-resp.
           MOVE "2" TO STATUS-CODE OF student-resp.
           MOVE STU-RESP TO DFHCOMMAREA.
           EXEC CICS RETURN END-EXEC.

       400-EOF.
           MOVE LOW-VALUES TO STU-RESP.
           MOVE '*** END OF FILE ***' TO MSG OF student-resp.
           MOVE "2" TO STATUS-CODE OF student-resp.
           MOVE STU-RESP TO DFHCOMMAREA.
           EXEC CICS RETURN END-EXEC.
       END PROGRAM DCJB8WSB.
