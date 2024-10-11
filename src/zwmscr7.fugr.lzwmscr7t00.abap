*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM028..........................................*
DATA:  BEGIN OF STATUS_ZWM028                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM028                        .
CONTROLS: TCTRL_ZWM028
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM028                        .
TABLES: ZWM028                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
