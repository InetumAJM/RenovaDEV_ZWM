*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM065..........................................*
DATA:  BEGIN OF STATUS_ZWM065                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM065                        .
CONTROLS: TCTRL_ZWM065
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM065                        .
TABLES: ZWM065                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
