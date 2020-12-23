       01  MAP1I.
           02  FILLER PIC X(12).
           02  TITLEL    COMP  PIC  S9(4).
           02  TITLEF    PICTURE X.
           02  FILLER REDEFINES TITLEF.
             03 TITLEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TITLEI  PIC X(20).
           02  CHOICEL    COMP  PIC  S9(4).
           02  CHOICEF    PICTURE X.
           02  FILLER REDEFINES CHOICEF.
             03 CHOICEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CHOICEI  PIC X(1).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGI  PIC X(60).
           02  CTRLL    COMP  PIC  S9(4).
           02  CTRLF    PICTURE X.
           02  FILLER REDEFINES CTRLF.
             03 CTRLA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CTRLI  PIC X(27).
       01  MAP1O REDEFINES MAP1I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TITLEC    PICTURE X.
           02  TITLEH    PICTURE X.
           02  TITLEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  CHOICEC    PICTURE X.
           02  CHOICEH    PICTURE X.
           02  CHOICEO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(60).
           02  FILLER PICTURE X(3).
           02  CTRLC    PICTURE X.
           02  CTRLH    PICTURE X.
           02  CTRLO  PIC X(27).