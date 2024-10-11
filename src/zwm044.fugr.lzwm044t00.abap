*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM044..........................................*
DATA:  BEGIN OF STATUS_ZWM044                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM044                        .
CONTROLS: TCTRL_ZWM044
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM044                        .
TABLES: ZWM044                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
