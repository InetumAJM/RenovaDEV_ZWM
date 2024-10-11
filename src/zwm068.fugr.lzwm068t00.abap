*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM068..........................................*
DATA:  BEGIN OF STATUS_ZWM068                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM068                        .
CONTROLS: TCTRL_ZWM068
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM068                        .
TABLES: ZWM068                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
