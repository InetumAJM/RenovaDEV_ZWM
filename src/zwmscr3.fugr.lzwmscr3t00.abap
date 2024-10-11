*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM037..........................................*
DATA:  BEGIN OF STATUS_ZWM037                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM037                        .
CONTROLS: TCTRL_ZWM037
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM037                        .
TABLES: ZWM037                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
