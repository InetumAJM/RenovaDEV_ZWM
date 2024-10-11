*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM014..........................................*
DATA:  BEGIN OF STATUS_ZWM014                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM014                        .
CONTROLS: TCTRL_ZWM014
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM014                        .
TABLES: ZWM014                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
