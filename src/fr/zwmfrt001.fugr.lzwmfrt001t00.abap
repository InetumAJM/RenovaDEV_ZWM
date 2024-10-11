*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWMFRT001.......................................*
DATA:  BEGIN OF STATUS_ZWMFRT001                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWMFRT001                     .
CONTROLS: TCTRL_ZWMFRT001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWMFRT001                     .
TABLES: ZWMFRT001                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
