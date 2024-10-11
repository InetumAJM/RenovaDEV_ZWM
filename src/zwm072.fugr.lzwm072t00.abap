*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM072..........................................*
DATA:  BEGIN OF STATUS_ZWM072                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM072                        .
CONTROLS: TCTRL_ZWM072
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM072                        .
TABLES: ZWM072                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
