*&---------------------------------------------------------------------*
*&  Include           ZWMREP0134_O02
*&---------------------------------------------------------------------*

*&SPWIZARD: OUTPUT MODULE FOR TC 'ZTC0001'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE ztc0001_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_data LINES ztc0001-lines.
ENDMODULE.                    "ZTC0001_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'ZTC0001'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE ztc0001_get_lines OUTPUT.
  g_ztc0001_lines = sy-loopc.
  PERFORM status_ztc0001.
ENDMODULE.                    "ZTC0001_GET_LINES OUTPUT
