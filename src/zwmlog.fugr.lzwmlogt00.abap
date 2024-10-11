*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWMLOG..........................................*
DATA:  BEGIN OF STATUS_ZWMLOG                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWMLOG                        .
CONTROLS: TCTRL_ZWMLOG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWMLOG                        .
TABLES: ZWMLOG                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
