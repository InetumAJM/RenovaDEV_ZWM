*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM039..........................................*
DATA:  BEGIN OF STATUS_ZWM039                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM039                        .
CONTROLS: TCTRL_ZWM039
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZWM_V039........................................*
TABLES: ZWM_V039, *ZWM_V039. "view work areas
CONTROLS: TCTRL_ZWM_V039
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZWM_V039. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZWM_V039.
* Table for entries selected to show on screen
DATA: BEGIN OF ZWM_V039_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZWM_V039.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWM_V039_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZWM_V039_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZWM_V039.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWM_V039_TOTAL.

*...processing: ZWM_V054........................................*
TABLES: ZWM_V054, *ZWM_V054. "view work areas
CONTROLS: TCTRL_ZWM_V054
TYPE TABLEVIEW USING SCREEN '0003'.
DATA: BEGIN OF STATUS_ZWM_V054. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZWM_V054.
* Table for entries selected to show on screen
DATA: BEGIN OF ZWM_V054_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZWM_V054.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWM_V054_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZWM_V054_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZWM_V054.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWM_V054_TOTAL.

*.........table declarations:.................................*
TABLES: *ZWM039                        .
TABLES: KNA1                           .
TABLES: MAKT                           .
TABLES: MARA                           .
TABLES: ZWM039                         .
TABLES: ZWM054                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
