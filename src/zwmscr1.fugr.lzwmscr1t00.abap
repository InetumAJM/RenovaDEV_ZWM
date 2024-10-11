*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM029..........................................*
DATA:  BEGIN OF STATUS_ZWM029                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM029                        .
CONTROLS: TCTRL_ZWM029
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZWM031..........................................*
DATA:  BEGIN OF STATUS_ZWM031                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM031                        .
CONTROLS: TCTRL_ZWM031
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM029                        .
TABLES: *ZWM031                        .
TABLES: ZWM029                         .
TABLES: ZWM031                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
