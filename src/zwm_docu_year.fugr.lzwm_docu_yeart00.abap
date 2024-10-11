*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM_DOCU_YEAR...................................*
DATA:  BEGIN OF STATUS_ZWM_DOCU_YEAR                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM_DOCU_YEAR                 .
CONTROLS: TCTRL_ZWM_DOCU_YEAR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWM_DOCU_YEAR                 .
TABLES: ZWM_DOCU_YEAR                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
