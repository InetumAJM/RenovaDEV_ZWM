REPORT  ZGW.


*REPORT ZEXAMPLE.
DATA: GW_HOST LIKE GWY_STRUCT-GWHOST,
GW_SERV LIKE GWY_STRUCT-GWSERV,
SAPSYS(2),
CONVERSATION_ID(8).
DATA: BEGIN OF GWCONN OCCURS 0.
        INCLUDE STRUCTURE GWY_CONN.
DATA: END OF GWCONN.
* 1 GET HOST AND SERVICE
CALL 'C_SAPGPARAM' ID 'NAME' FIELD 'SAPLOCALHOST'
ID 'VALUE' FIELD GW_HOST.
CALL 'C_SAPGPARAM' ID 'NAME' FIELD 'SAPSYSTEM'
ID 'VALUE' FIELD SAPSYS.
GW_SERV = 'SAPGW'.
GW_SERV+5 = SAPSYS.
*Chapter 1 # System 17
*18 Common SAP R/3 Functions Manual
CALL FUNCTION 'GWY_READ_CONNECTIONS'
  EXPORTING
    GWHOST                    = GW_HOST
    GWSERV                    = GW_SERV
  TABLES
    CONNECTIONS               = GWCONN
  EXCEPTIONS
    GWY_UNKNOWN_OPCODE        = 01
    GWY_COMMUNICATION_FAILURE = 02
    GWY_GET_TAB_FAILED        = 03
    GWY_NEWLINE_FAILED        = 04
    GWY_TABLEN_TOO_SHORT      = 05
    GWY_GET_OPCODE_FAILED     = 06
    GWY_GET_GWHOST_FAILED     = 07
    GWY_GET_GWSERV_FAILED     = 08.
IF SY-SUBRC NE 0.
  WRITE:/ 'ERROR IN FUNCTION'.
ELSE.
  WRITE:/2 'UNIT', 11 'PROGRAM', 20 'USER', 33 'SYSTEM', 50 'LAST REQUEST'.
  ULINE AT /1(70).
  LOOP AT GWCONN.
    WRITE: /2 GWCONN-LU,
    11 GWCONN-TP,
    20 GWCONN-GWUSER,
    33 GWCONN-SYMDEST,
    50 GWCONN-LAST_REQ.
  ENDLOOP.
ENDIF.
