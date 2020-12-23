      * VALIDREC RECORD LAYOUT TO BE COPIED

       01 STUDATA-LENGTH         PIC S9(4) COMP VALUE 220.
      *Above was copied from sturec format and the value spaces below
       01 STUDATA-RECORD.
          05 STU-NUMBERL-T           PIC S9(4).
          05 STU-NUMBERI-T           PIC X(7).
          05 STU-NAMEL-T             PIC S9(4).
          05 STU-NAMEI-T             PIC X(20).
          05 STU-ADD1L-T             PIC S9(4).
          05 STU-ADD2L-T             PIC S9(4).
          05 STU-POST1L-T            PIC S9(4).
          05 STU-POST2L-T            PIC S9(4).
          05 STU-POST1I-T            PIC X(3).
          05 STU-POST2I-T            PIC X(3).
      *   05 FILLER                  PIC X(4) VALUE SPACES.
          05 STU-CR1AL-T             PIC s9(4).
          05 STU-CR1BL-T             PIC s9(4).
          05 STU-CR1AI-T             PIC X(4).
          05 STU-CR1BI-T             PIC X(4).
          05 STU-CR2AL-T             PIC s9(4).
          05 STU-CR2BL-T             PIC s9(4).
          05 STU-CR2AI-T             PIC X(4).
          05 STU-CR2BI-T             PIC X(4).
          05 STU-CR3AL-T             PIC s9(4).
          05 STU-CR3BL-T             PIC s9(4).
          05 STU-CR3AI-T             PIC X(4).
          05 STU-CR3BI-T             PIC X(4).
          05 STU-CR4AL-T             PIC s9(4).
          05 STU-CR4BL-T             PIC s9(4).
          05 STU-CR4AI-T             PIC X(4).
          05 STU-CR4BI-T             PIC X(4).
          05 STU-CR5AL-T             PIC s9(4).
          05 STU-CR5BL-T             PIC s9(4).
          05 STU-CR5AI-T             PIC X(4).
          05 STU-CR5BI-T             PIC X(4).
          05 STU-PHN1L-T             PIC s9(4).
          05 STU-PHN1I-T             PIC X(3).
          05 STU-PHN2L-T             PIC s9(4).
          05 STU-PHN2I-T             PIC X(3).
          05 STU-PHN3L-T             PIC s9(4).
          05 STU-PHN3I-T             PIC X(4).
          05 STU-ERRORMSG            PIC X(40).
          05 STU-ERRORCODE           PIC 99.
