*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM073..........................................*
DATA:  BEGIN OF STATUS_ZWM073                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM073                        .
CONTROLS: TCTRL_ZWM073
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM073                        .
TABLES: ZWM073                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
