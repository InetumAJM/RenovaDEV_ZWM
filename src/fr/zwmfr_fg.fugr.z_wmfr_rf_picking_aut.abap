FUNCTION z_wmfr_rf_picking_aut.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_XUSER) TYPE  LRF_WKQU OPTIONAL
*"     REFERENCE(I_EQUIPMENT) TYPE  ZWM010-EQUIPAMENTO
*"----------------------------------------------------------------------
  DATA lt_zwm001 TYPE TABLE OF zwm001.

  DATA ls_bdcmsgcoll TYPE bdcmsgcoll.

  DATA lv_msg     TYPE string.
  DATA lv_retorno TYPE zwm_aux-retorno.
  DATA lv_subrc   TYPE sysubrc.

  FIELD-SYMBOLS <zwm001> TYPE zwm001.

  IF is_xuser IS NOT SUPPLIED OR is_xuser IS INITIAL.
    PERFORM f_data_init_get_user_data CHANGING gs_xuser lv_retorno. " get user RF data for WM
    IF lv_retorno IS NOT INITIAL.
      MESSAGE e035(zwmfr001) WITH sy-uname.

      ls_bdcmsgcoll-msgid = sy-msgid.
      ls_bdcmsgcoll-msgtyp = sy-msgty.
      ls_bdcmsgcoll-msgnr = sy-msgno.

      PERFORM f_show_message USING ls_bdcmsgcoll.
    ENDIF.
  ELSE.
    gs_xuser  = is_xuser.
  ENDIF.

  FREE gt_zwm001.
  FREE gs_scr001.

  PERFORM f_data_init_get_hardcodes
    TABLES lt_zwm001
    USING gs_xuser-lgnum.
  LOOP AT lt_zwm001 ASSIGNING <zwm001>.
    INSERT <zwm001> INTO TABLE gt_zwm001.
  ENDLOOP.

  IF lv_retorno EQ 0.
    gv_equipment = i_equipment.

    PERFORM f_get_aut_to
      USING gs_xuser
            gv_equipment
      CHANGING lv_subrc.

    IF lv_subrc EQ 0.
      CASE gs_xuser-devty.
        WHEN c_devty_16x20 OR c_devty_16x20its.
          gv_dynnr  = '0010'.
        WHEN c_devty_8x40.
          gv_dynnr  = '0011'.
        WHEN OTHERS.
          RETURN.
      ENDCASE.

      CALL SCREEN gv_dynnr.
    ENDIF.
  ENDIF.
ENDFUNCTION.
