*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWMMPT037.......................................*
DATA:  BEGIN OF STATUS_ZWMMPT037                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWMMPT037                     .
CONTROLS: TCTRL_ZWMMPT037
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWMMPT037                     .
TABLES: ZWMMPT037                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
