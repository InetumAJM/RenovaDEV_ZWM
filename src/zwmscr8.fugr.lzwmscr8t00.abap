*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM013..........................................*
DATA:  BEGIN OF STATUS_ZWM013                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM013                        .
CONTROLS: TCTRL_ZWM013
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM013                        .
TABLES: ZWM013                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
