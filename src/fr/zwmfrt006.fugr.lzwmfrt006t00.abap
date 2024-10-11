*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWMFRT006_V.....................................*
TABLES: ZWMFRT006_V, *ZWMFRT006_V. "view work areas
CONTROLS: TCTRL_ZWMFRT006_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZWMFRT006_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZWMFRT006_V.
* Table for entries selected to show on screen
DATA: BEGIN OF ZWMFRT006_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZWMFRT006_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWMFRT006_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZWMFRT006_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZWMFRT006_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWMFRT006_V_TOTAL.

*.........table declarations:.................................*
TABLES: ZWMFRT006                      .
