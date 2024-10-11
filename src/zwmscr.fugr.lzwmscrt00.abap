*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM030..........................................*
DATA:  BEGIN OF STATUS_ZWM030                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM030                        .
CONTROLS: TCTRL_ZWM030
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM030                        .
TABLES: ZWM030                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
