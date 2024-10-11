*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM059..........................................*
DATA:  BEGIN OF STATUS_ZWM059                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM059                        .
CONTROLS: TCTRL_ZWM059
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM059                        .
TABLES: ZWM059                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
