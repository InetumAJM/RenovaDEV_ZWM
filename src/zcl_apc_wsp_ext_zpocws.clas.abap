class ZCL_APC_WSP_EXT_ZPOCWS definition
  public
  inheriting from CL_APC_WSP_EXT_STATELESS_BASE
  final
  create public .

public section.

  methods IF_APC_WSP_EXTENSION~ON_ACCEPT
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_CLOSE
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_MESSAGE
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_START
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_ERROR
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_ZPOCWS IMPLEMENTATION.


  method IF_APC_WSP_EXTENSION~ON_ACCEPT.
*CALL METHOD SUPER->IF_APC_WSP_EXTENSION~ON_ACCEPT
*  EXPORTING
*    I_CONTEXT_BASE =
**  IMPORTING
**    e_connect_mode =
*    .
    BREAK-POINT.
  endmethod.


  method IF_APC_WSP_EXTENSION~ON_CLOSE.
*CALL METHOD SUPER->IF_APC_WSP_EXTENSION~ON_CLOSE
*  EXPORTING
*    I_REASON       =
*    I_CODE         =
*    I_CONTEXT_BASE =
*    .
    BREAK-POINT.
  endmethod.


  method IF_APC_WSP_EXTENSION~ON_ERROR.
*CALL METHOD SUPER->IF_APC_WSP_EXTENSION~ON_ERROR
*  EXPORTING
*    I_REASON       =
*    I_CODE         =
*    I_CONTEXT_BASE =
*    .
    BREAK-POINT.
  endmethod.


  method IF_APC_WSP_EXTENSION~ON_MESSAGE.
*CALL METHOD SUPER->IF_APC_WSP_EXTENSION~ON_MESSAGE
*  EXPORTING
*    I_MESSAGE         =
*    I_MESSAGE_MANAGER =
*    I_CONTEXT         =
*    .

    BREAK-POINT.
  endmethod.


  METHOD if_apc_wsp_extension~on_start.
*CALL METHOD SUPER->IF_APC_WSP_EXTENSION~ON_START
*  EXPORTING
*    I_CONTEXT         =
*    I_MESSAGE_MANAGER =
*    .

    BREAK-POINT.

  ENDMETHOD.
ENDCLASS.
