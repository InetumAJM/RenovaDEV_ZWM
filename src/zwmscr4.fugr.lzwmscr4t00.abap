*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM008..........................................*
DATA:  BEGIN OF STATUS_ZWM008                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM008                        .
CONTROLS: TCTRL_ZWM008
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM008                        .
TABLES: ZWM008                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
