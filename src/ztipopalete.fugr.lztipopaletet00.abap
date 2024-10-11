*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTIPOPALETE.....................................*
DATA:  BEGIN OF STATUS_ZTIPOPALETE                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTIPOPALETE                   .
CONTROLS: TCTRL_ZTIPOPALETE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTIPOPALETE                   .
TABLES: ZTIPOPALETE                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
