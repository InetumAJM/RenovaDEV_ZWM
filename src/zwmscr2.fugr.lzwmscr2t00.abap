*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM036..........................................*
DATA:  BEGIN OF STATUS_ZWM036                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM036                        .
CONTROLS: TCTRL_ZWM036
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM036                        .
TABLES: ZWM036                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
