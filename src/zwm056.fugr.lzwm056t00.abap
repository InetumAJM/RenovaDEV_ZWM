*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM056..........................................*
DATA:  BEGIN OF STATUS_ZWM056                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM056                        .
CONTROLS: TCTRL_ZWM056
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM056                        .
TABLES: ZWM056                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
