*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM016..........................................*
DATA:  BEGIN OF STATUS_ZWM016                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM016                        .
CONTROLS: TCTRL_ZWM016
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM016                        .
TABLES: ZWM016                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
