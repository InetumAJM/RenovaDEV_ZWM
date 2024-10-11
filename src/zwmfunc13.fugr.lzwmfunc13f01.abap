*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC13F01.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  f_data_init_get_user_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--CS_XUSER   text
*      <--C_RETURN   text
*----------------------------------------------------------------------*
FORM f_data_init_get_user_data CHANGING cs_xuser  TYPE lrf_wkqu
                                        c_return  TYPE zwm_aux-retorno.

  DATA lt_xuser TYPE STANDARD TABLE OF lrf_wkqu WITH DEFAULT KEY.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = lt_xuser[]
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc NE 0.
    c_return = 10.
    RETURN.
  ENDIF.

  SORT lt_xuser[] BY statu.
  DELETE lt_xuser[] WHERE statu NE abap_true.

  IF lt_xuser[] IS INITIAL.
    c_return = 10.
    RETURN.
  ENDIF.

  READ TABLE lt_xuser[] INTO cs_xuser INDEX 1.  " only on entry with active status is allowed
ENDFORM.                    " F_DATA_INIT_GET_USER_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_HARDCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_LGNUM    text
*      -->IO_BALLOG  text
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_get_hardcodes USING i_lgnum   TYPE lgnum
                           io_ballog TYPE REF TO zcl_ca_bal_log
                     CHANGING c_subrc TYPE sysubrc.
  DATA lv_message TYPE bapiret2-message.                    "#EC NEEDED

  FREE gt_zwmt001[].

  IF c_subrc NE 0.
    RETURN.
  ENDIF.

  SELECT processo parametro item valor
    FROM zwm001 INTO TABLE gt_zwmt001[]
    WHERE armazem EQ i_lgnum.
  IF sy-subrc NE 0.
    " Não foram encontrados valores de parametrização para o armazém &.
    MESSAGE e124 WITH i_lgnum INTO lv_message.
    io_ballog->log_add_message( ).

    c_subrc = 20.
  ENDIF.
ENDFORM.                    " F_GET_HARDCODES
