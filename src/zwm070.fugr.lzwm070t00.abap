*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM070..........................................*
DATA:  BEGIN OF STATUS_ZWM070                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM070                        .
CONTROLS: TCTRL_ZWM070
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM070                        .
TABLES: ZWM070                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
