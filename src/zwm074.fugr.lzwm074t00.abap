*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM074..........................................*
DATA:  BEGIN OF STATUS_ZWM074                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM074                        .
CONTROLS: TCTRL_ZWM074
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM074                        .
TABLES: ZWM074                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
