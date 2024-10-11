*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM060..........................................*
DATA:  BEGIN OF STATUS_ZWM060                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM060                        .
CONTROLS: TCTRL_ZWM060
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM060                        .
TABLES: ZWM060                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
