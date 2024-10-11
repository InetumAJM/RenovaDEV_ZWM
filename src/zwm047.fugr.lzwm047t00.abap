*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM050..........................................*
DATA:  BEGIN OF STATUS_ZWM050                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM050                        .
CONTROLS: TCTRL_ZWM050
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM050                        .
TABLES: ZWM050                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
