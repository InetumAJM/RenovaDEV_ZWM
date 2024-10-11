*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM064..........................................*
DATA:  BEGIN OF STATUS_ZWM064                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM064                        .
CONTROLS: TCTRL_ZWM064
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZWMFRT006.......................................*
DATA:  BEGIN OF STATUS_ZWMFRT006                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWMFRT006                     .
CONTROLS: TCTRL_ZWMFRT006
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZWM064                        .
TABLES: *ZWMFRT006                     .
TABLES: ZWM064                         .
TABLES: ZWMFRT006                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
