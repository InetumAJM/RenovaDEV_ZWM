*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM045..........................................*
DATA:  BEGIN OF STATUS_ZWM045                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM045                        .
CONTROLS: TCTRL_ZWM045
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM045                        .
TABLES: ZWM045                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
