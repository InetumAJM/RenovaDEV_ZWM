*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM082..........................................*
DATA:  BEGIN OF STATUS_ZWM082                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM082                        .
CONTROLS: TCTRL_ZWM082
            TYPE TABLEVIEW USING SCREEN '9500'.
*.........table declarations:.................................*
TABLES: *ZWM082                        .
TABLES: ZWM082                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
