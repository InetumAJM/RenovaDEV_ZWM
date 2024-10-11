*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM046..........................................*
DATA:  BEGIN OF STATUS_ZWM046                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM046                        .
CONTROLS: TCTRL_ZWM046
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM046                        .
TABLES: ZWM046                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
