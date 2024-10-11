*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM071..........................................*
DATA:  BEGIN OF STATUS_ZWM071                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM071                        .
CONTROLS: TCTRL_ZWM071
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM071                        .
TABLES: ZWM071                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
