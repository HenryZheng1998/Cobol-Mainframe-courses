       01 STU-RESP.
           05 student-resp.
            10 STATUS-CODE      PIC X.
            10 MSG              PIC X(60).
            10 stuArray2-num    PIC S9(9) COMP-5 SYNC.
            10 stuArray OCCURS 10.
                15 stuData.
                    20 STUNUM       PIC X(7).
                    20 STUNAME      PIC X(20).
                    20 PHONENUM     PIC X(10).