*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM057..........................................*
DATA:  BEGIN OF STATUS_ZWM057                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM057                        .
*.........table declarations:.................................*
TABLES: *ZWM057                        .
TABLES: ZWM057                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
