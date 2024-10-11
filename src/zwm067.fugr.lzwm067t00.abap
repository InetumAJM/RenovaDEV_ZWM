*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM067..........................................*
DATA:  BEGIN OF STATUS_ZWM067                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM067                        .
CONTROLS: TCTRL_ZWM067
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM067                        .
TABLES: ZWM067                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
