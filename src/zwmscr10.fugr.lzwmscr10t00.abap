*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM042..........................................*
DATA:  BEGIN OF STATUS_ZWM042                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM042                        .
CONTROLS: TCTRL_ZWM042
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM042                        .
TABLES: ZWM042                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
