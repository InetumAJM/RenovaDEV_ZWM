*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWMFRT003.......................................*
DATA:  BEGIN OF STATUS_ZWMFRT003                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWMFRT003                     .
CONTROLS: TCTRL_ZWMFRT003
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWMFRT003                     .
TABLES: ZWMFRT003                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
