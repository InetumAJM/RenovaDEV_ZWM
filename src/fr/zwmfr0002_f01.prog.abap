*----------------------------------------------------------------------*
* Include: ZWMFR0002_F01
*----------------------------------------------------------------------*
* Description: RF - Entrada de Produto Acabado/Bobines PT
* RICEFW: WM.02/WM.03
*----------------------------------------------------------------------*
* Author........: [Pedro Silva] [ROFFD] [ROFF(SDF)]
*                 [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date:  2015-10-26
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_DATA_FREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_free.
  FREE gt_scr002[].
  FREE gt_scr002_check[].
  FREE gt_zwmfrt001[].

  FREE gt_mlgn[].
  FREE gt_ltap[].
  FREE gt_makt[].
  FREE gt_hardcodes[].
  FREE gt_zwmmpt001[].
  FREE gt_ekpo[].

  CLEAR gs_xuser.
  CLEAR gs_texts.
  CLEAR gs_scr001.
  CLEAR gs_scr002.

  CLEAR gv_okcode.
  CLEAR gv_dynnr_1.
  CLEAR gv_dynnr_2.
  CLEAR gv_haserror.
  CLEAR gv_cursor.
ENDFORM.                    " F_DATA_FREE
*&---------------------------------------------------------------------*
*&      Form  F_DATA_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_init.
  PERFORM f_data_init_get_user_data. " get user RF data for WM
  PERFORM f_data_init_get_hardcodes.    " get hardcodes for program

  CASE gs_xuser-devty.
    WHEN c_devty_16x20 OR c_devty_16x20its.
      gv_dynnr_1  = '0010'.

      IF sy-tcode EQ c_tcode_zwmfr0002c.
        gv_dynnr_2  = '0030'.
      ELSE.
        gv_dynnr_2  = '0020'.
      ENDIF.
    WHEN c_devty_8x40.
      gv_dynnr_1  = '0011'.
      IF sy-tcode EQ c_tcode_zwmfr0002c.
        gv_dynnr_2  = '0031'.
      ELSE.
        gv_dynnr_2  = '0021'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  CASE sy-tcode.
    WHEN c_tcode_zwmfr0002a.
      gs_texts-scr001_title  = text-006.
      gs_texts-scr002_ud     = text-009.
      gs_texts-scr001_tpall  = text-007.
    WHEN c_tcode_zwmfr0002b.
      gs_texts-scr001_title  = text-011.
      gs_texts-scr002_ud     = text-008.
      gs_texts-scr001_tpall  = text-010.
    WHEN c_tcode_zwmfr0002c.
      gs_texts-scr001_title  = text-005.
      gs_texts-scr002_ud     = text-008.
      gs_texts-scr001_tpall  = text-010.
    WHEN c_tcode_zwmfr0002d.
      gs_texts-scr001_title  = text-006.
      gs_texts-scr002_ud     = text-009.
      gs_texts-scr001_tpall  = text-007.
    WHEN OTHERS.
      gs_texts-scr001_title  = text-006.
      gs_texts-scr002_ud     = text-009.
      gs_texts-scr001_tpall  = text-007.
  ENDCASE.

  gs_scr001-npall = 1.
  gs_scr001-cpall = 1.

  gv_cursor = c_cursor_ebeln.
ENDFORM.                    " F_DATA_INIT
*&---------------------------------------------------------------------*
*&      Form  F_DATA_INIT_GET_USER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_init_get_user_data.
  DATA lv_msgv1 TYPE bdcmsgcoll-msgv1.

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
    lv_msgv1  = sy-uname.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '035'
        message_var1   = lv_msgv1.

    gv_haserror = abap_true.
    RETURN.
  ENDIF.

  SORT lt_xuser[] BY statu.
  DELETE lt_xuser[] WHERE statu NE abap_true.

  IF lt_xuser[] IS INITIAL.
    lv_msgv1  = sy-uname.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '035'
        message_var1   = lv_msgv1.

    gv_haserror = abap_true.
    RETURN.
  ENDIF.

  READ TABLE lt_xuser[] INTO gs_xuser INDEX 1.  " only on entry with active status is allowed
ENDFORM.                    " F_DATA_INIT_GET_USER_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DATA_INIT_GET_HARDCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_init_get_hardcodes.
  DATA lt_hardcodes TYPE STANDARD TABLE OF zhardcode_table.

  FIELD-SYMBOLS <fs_hardc> LIKE LINE OF gt_hardcodes[].

  IF gv_haserror EQ abap_true.
    RETURN.
  ENDIF.

  SELECT *
    FROM zhardcode_table INTO TABLE lt_hardcodes[]
    WHERE inc_meth EQ c_syrepid.

  SORT lt_hardcodes[] BY inc_meth occurrence counter.
  DELETE ADJACENT DUPLICATES FROM lt_hardcodes[] COMPARING inc_meth occurrence counter.

  LOOP AT lt_hardcodes[] ASSIGNING <fs_hardc>.
    INSERT <fs_hardc> INTO TABLE gt_hardcodes[].
  ENDLOOP.

  SELECT processo parametro item valor
    FROM zwmmpt001 INTO TABLE gt_zwmmpt001[]
    WHERE armazem EQ gs_xuser-lgnum.
ENDFORM.                    " F_DATA_INIT_GET_HARDCODES
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_CHECK_EBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_OKCODE   text
*      <--C_CURSOR   text
*----------------------------------------------------------------------*
FORM f_data_scr0010_check_ebeln CHANGING c_okcode TYPE syucomm
                                         c_cursor TYPE fieldname.
  DATA lv_msgv1 TYPE bdcmsgcoll-msgv1.
  DATA lv_msgv2 TYPE bdcmsgcoll-msgv1.
  DATA lv_lgnum TYPE t320-lgnum.

  DATA lt_ekpo TYPE ty_t_ekpo.

  FIELD-SYMBOLS <fs_ekpo> LIKE LINE OF lt_ekpo[].

  IF gs_scr001-ebeln IS INITIAL.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  SELECT SINGLE ebeln
    FROM ekko INTO gs_scr001-ebeln
    WHERE ebeln EQ gs_scr001-ebeln.
  IF sy-subrc NE 0.
    lv_msgv1  = gs_scr001-ebeln.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '011'
        message_var1   = lv_msgv1.

    CLEAR gs_scr001-ebeln.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  SELECT ebeln ebelp loekz matnr werks lgort
         meins elikz
    FROM ekpo INTO TABLE lt_ekpo[]
    WHERE ebeln EQ gs_scr001-ebeln.
  IF sy-subrc NE 0.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '012'.

    CLEAR gs_scr001-ebeln.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  SORT lt_ekpo[] BY loekz.
  DELETE lt_ekpo[] WHERE loekz IS NOT INITIAL.  " discard items marked for elimination
  SORT lt_ekpo[] BY elikz.
  DELETE lt_ekpo[] WHERE elikz IS NOT INITIAL. " discard items fully delivered

  IF lt_ekpo[] IS INITIAL.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '012'.

    CLEAR gs_scr001-ebeln.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  " Check if PO materials are defined in user's warehouse
  SELECT matnr lgnum lvorm
    FROM mlgn INTO TABLE gt_mlgn[]
    FOR ALL ENTRIES IN lt_ekpo[]
    WHERE matnr EQ lt_ekpo-matnr
      AND lgnum EQ gs_xuser-lgnum.
  IF gt_mlgn[] IS INITIAL.
    lv_msgv1  = sy-uname.
    lv_msgv2  = gs_xuser-lgnum.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '008'
        message_var1   = lv_msgv1
        message_var2   = lv_msgv2.

    CLEAR gs_scr001-ebeln.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  " Check if STO destiny plant and deposit are defined for user's warehouse
  READ TABLE lt_ekpo[] ASSIGNING <fs_ekpo> INDEX 1.
  IF sy-subrc EQ 0.
    SELECT SINGLE lgnum
      FROM t320 INTO lv_lgnum
      WHERE werks EQ <fs_ekpo>-werks
        AND lgort EQ <fs_ekpo>-lgort.
    IF sy-subrc EQ 0.
      IF lv_lgnum NE gs_xuser-lgnum.  " user's WM not allowed for this plant/deposit
        lv_msgv1  = sy-uname.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = c_msgid_zwmfr001
            message_lang   = sy-langu
            message_type   = c_msgty_e
            message_number = '036'
            message_var1   = lv_msgv1.

        FREE gt_mlgn[].
        CLEAR gs_scr001-ebeln.
        CLEAR c_okcode.
        RETURN.
      ENDIF.
    ELSE. " plant/deposit not defined in T320, no active WM
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = c_msgid_zwmfr001
*          message_lang   = sy-langu
*          message_type   = c_msgty_e
*          message_number = '002'.
*
*      FREE gt_mlgn[].
*      CLEAR gs_scr001-ebeln.
*      CLEAR c_okcode.
*      RETURN.
    ENDIF.
  ENDIF.

  LOOP AT lt_ekpo[] ASSIGNING <fs_ekpo>.
    IF lv_lgnum IS NOT INITIAL.
      READ TABLE gt_mlgn[] TRANSPORTING NO FIELDS
        WITH TABLE KEY matnr = <fs_ekpo>-matnr
                       lgnum = gs_xuser-lgnum.  " only load valid materials for this warehouse
      CHECK sy-subrc EQ 0.
    ENDIF.
    INSERT <fs_ekpo> INTO TABLE gt_ekpo[].
  ENDLOOP.

  SELECT matnr maktx
    FROM makt INTO TABLE gt_makt[]
    FOR ALL ENTRIES IN gt_ekpo[]
    WHERE matnr EQ gt_ekpo-matnr
      AND spras EQ sy-langu.

  c_cursor = c_cursor_xblnr.
ENDFORM.                    " F_DATA_SCR0010_CHECK_EBELN
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0010_check_xblnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_OKCODE   text
*      <--C_CURSOR   text
*----------------------------------------------------------------------*
FORM f_data_scr0010_check_xblnr CHANGING c_okcode TYPE syucomm
                                         c_cursor TYPE fieldname.
  DATA lv_xblnr   TYPE zwmfrt001-xblnr.

  IF gs_scr001-xblnr IS INITIAL.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  IF sy-tcode NE c_tcode_zwmfr0002c.
    c_cursor = c_cursor_npall.
  ELSE.
    lv_xblnr = gs_scr001-xblnr.

    SELECT licha UP TO 1 ROWS
      FROM zwmfrt001 INTO gs_scr001-licha
      WHERE lgnum EQ gs_xuser-lgnum
        AND xblnr EQ lv_xblnr
        AND ebeln EQ gs_scr001-ebeln.
    ENDSELECT.

    c_cursor = c_cursor_licha.
  ENDIF.
ENDFORM.                    "f_data_scr0010_check_xblnr
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0010_check_licha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_OKCODE   text
*      <--C_CURSOR   text
*----------------------------------------------------------------------*
FORM f_data_scr0010_check_licha CHANGING c_okcode TYPE syucomm
                                         c_cursor TYPE fieldname.
  IF gs_scr001-licha IS INITIAL AND sy-tcode EQ c_tcode_zwmfr0002c.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  c_cursor = c_cursor_npall.
ENDFORM.                    "f_data_scr0010_check_licha
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0010_check_npall
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_OKCODE   text
*      <--C_CURSOR   text
*----------------------------------------------------------------------*
FORM f_data_scr0010_check_npall CHANGING c_okcode TYPE syucomm
                                         c_cursor TYPE fieldname.
  IF gs_scr001-npall LT 1 OR gs_scr001-npall GT 99.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '013'.

    CLEAR c_okcode.
    RETURN.
  ENDIF.

  c_cursor = c_cursor_pnext.
ENDFORM.                    "f_data_scr0010_check_npall
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_UCOMM_NEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_next.
  DATA lv_xblnr   TYPE zwmfrt001-xblnr.
  DATA lv_msgv1   TYPE bdcmsgcoll-msgv1.
  DATA lv_msgv2   TYPE bdcmsgcoll-msgv2.
  DATA lv_pending TYPE abap_bool.
  DATA lv_retcode TYPE flag.

  FIELD-SYMBOLS <fs_zwmfrt001> LIKE LINE OF gt_zwmfrt001[].

  lv_xblnr = gs_scr001-xblnr.

  " Check if STO/XBLNR is already being processed
  CALL FUNCTION 'ENQUEUE_EZ_ZWMFRT001'
    EXPORTING
      lgnum          = gs_xuser-lgnum
      xblnr          = lv_xblnr
      ebeln          = gs_scr001-ebeln
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc NE 0.
    lv_msgv1  = gs_scr001-ebeln.
    lv_msgv2  = gs_scr001-xblnr.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '038'
        message_var1   = lv_msgv1
        message_var2   = lv_msgv2.

    PERFORM f_data_scr0010_ucomm_clear.
    RETURN.
  ENDIF.

  " Check if STO/XBLNR has a pending status or if it has been already fully processed
  SELECT *
    FROM zwmfrt001 INTO TABLE gt_zwmfrt001[]
    WHERE lgnum EQ gs_xuser-lgnum
      AND xblnr EQ lv_xblnr
      AND ebeln EQ gs_scr001-ebeln.
  IF sy-subrc NE 0. " new entry, continue.
    FREE gt_ltap[].
    FREE gt_scr002[].
    FREE gt_scr002_check[].

    CLEAR gs_scr002.

    gs_scr001-versi = 0.

    IF sy-tcode NE c_tcode_zwmfr0002c.
      gv_cursor = c_cursor_lenum.
    ELSE.
      gv_cursor = c_cursor_zeugn.
    ENDIF.

    SET SCREEN gv_dynnr_2.
    RETURN.
  ENDIF.

  lv_pending  = abap_false. " fully processed
  LOOP AT gt_zwmfrt001[] ASSIGNING <fs_zwmfrt001>.
    IF <fs_zwmfrt001>-mblnr IS INITIAL AND <fs_zwmfrt001>-mjahr IS INITIAL.
      lv_pending  = abap_true.
    ELSE.
      INSERT <fs_zwmfrt001> INTO TABLE gt_zwmfrt001_conf[].

      DELETE gt_zwmfrt001
        WHERE lgnum = <fs_zwmfrt001>-lgnum AND
*                       xblnr = <fs_zwmfrt001>-xblnr
*                       ebeln = <fs_zwmfrt001>-ebeln
*                       versi = <fs_zwmfrt001>-versi
*                       ebelp = <fs_zwmfrt001>-ebelp
                       lenum = <fs_zwmfrt001>-lenum.
*                       vepos = <fs_zwmfrt001>-vepos
*                       zeugn = <fs_zwmfrt001>-zeugn.
    ENDIF.
  ENDLOOP.

  IF lv_pending EQ abap_false.  " fully processed
    lv_msgv1  = gs_scr001-xblnr.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_w
        message_number = '052'
        message_var1   = lv_msgv1
      IMPORTING
        ret_code       = lv_retcode.

    CASE lv_retcode.
      WHEN c_ucomm_retcode_yes.
        LOOP AT gt_zwmfrt001_conf[] ASSIGNING <fs_zwmfrt001>.
          IF <fs_zwmfrt001>-versi GT gs_scr001-versi.
            gs_scr001-versi = <fs_zwmfrt001>-versi.
          ENDIF.
        ENDLOOP.

        gs_scr001-versi = gs_scr001-versi + 1.
      WHEN c_ucomm_retcode_no.
        CALL FUNCTION 'DEQUEUE_EZ_ZWMFRT001'
          EXPORTING
            lgnum = gs_xuser-lgnum
            xblnr = lv_xblnr
            ebeln = gs_scr001-ebeln.

        FREE gt_zwmfrt001[].
        PERFORM f_data_scr0010_ucomm_clear.
        RETURN.
    ENDCASE.
  ELSE.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_w
        message_number = '039'
      IMPORTING
        ret_code       = lv_retcode.

    CASE lv_retcode.
      WHEN c_ucomm_retcode_yes.
        PERFORM f_data_scr0010_ucomm_load.
      WHEN c_ucomm_retcode_no.
        PERFORM f_data_scr0010_ucomm_unload.
    ENDCASE.
  ENDIF.

  IF sy-tcode NE c_tcode_zwmfr0002c.
    gv_cursor = c_cursor_lenum.
  ELSE.
    gv_cursor = c_cursor_zeugn.
  ENDIF.

  SET SCREEN gv_dynnr_2.
ENDFORM.                    " F_DATA_SCR0010_UCOMM_NEXT
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0010_ucomm_load
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_load.
  DATA lv_lines TYPE i.
  DATA lv_param TYPE zwmmpt001-parametro.
  DATA lv_lgnum TYPE ltap-lgnum.

  FIELD-SYMBOLS <fs_zwmfrt001> LIKE LINE OF gt_zwmfrt001[].
  FIELD-SYMBOLS <fs_scr002>    LIKE LINE OF gt_scr002[].
  FIELD-SYMBOLS <fs_makt>      LIKE LINE OF gt_makt[].
  FIELD-SYMBOLS <fs_zwmmpt001> LIKE LINE OF gt_zwmmpt001[].

  FREE gt_ltap[].
  FREE gt_scr002[].
  FREE gt_scr002_check[].

  CLEAR gs_scr002.

  gs_scr001-cpall = 0.

  IF sy-tcode NE c_tcode_zwmfr0002c.
    CASE sy-tcode.
      WHEN c_tcode_zwmfr0002a.
        lv_param  = c_param_lgnum_pa_pt.
      WHEN c_tcode_zwmfr0002b.
        lv_param  = c_param_lgnum_pt.
      WHEN c_tcode_zwmfr0002d.
        lv_param  = c_param_lgnum_pa_pt.
      WHEN OTHERS.
        lv_param  = c_param_lgnum_pa_pt.
    ENDCASE.

    READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
      WITH TABLE KEY processo  = c_process_general
                     parametro = lv_param
                     item      = 0.
    IF sy-subrc EQ 0.
      lv_lgnum  = <fs_zwmmpt001>-valor.
    ENDIF.

    SELECT lgnum tanum tapos matnr charg bestq
           meins letyp pquit vsolm nlpla vlenr
      FROM ltap INTO TABLE gt_ltap[]
      FOR ALL ENTRIES IN gt_zwmfrt001[]
      WHERE lgnum EQ lv_lgnum
        AND tanum EQ gt_zwmfrt001-tanum
        AND tapos EQ gt_zwmfrt001-tapos.
  ENDIF.

  LOOP AT gt_zwmfrt001[] ASSIGNING <fs_zwmfrt001>.
    READ TABLE gt_makt[] ASSIGNING <fs_makt>
      WITH TABLE KEY matnr = <fs_zwmfrt001>-matnr.
    CHECK sy-subrc EQ 0.

    APPEND INITIAL LINE TO gt_scr002[] ASSIGNING <fs_scr002>.
    <fs_scr002>-lenum   = <fs_zwmfrt001>-lenum.
    <fs_scr002>-zeugn   = <fs_zwmfrt001>-zeugn.
    <fs_scr002>-matnr   = <fs_zwmfrt001>-matnr.
    <fs_scr002>-maktx   = <fs_makt>-maktx.
    <fs_scr002>-maktx_1 = <fs_makt>-maktx(20).
    <fs_scr002>-maktx_2 = <fs_makt>-maktx+20(20).
    <fs_scr002>-charg   = <fs_zwmfrt001>-charg.
    <fs_scr002>-vsolm   = <fs_zwmfrt001>-vsolm.
    <fs_scr002>-meins   = <fs_zwmfrt001>-meins.
    <fs_scr002>-letyp   = <fs_zwmfrt001>-letyp.
    <fs_scr002>-lgnum   = <fs_zwmfrt001>-lgnum.
    <fs_scr002>-tanum   = <fs_zwmfrt001>-tanum.
    <fs_scr002>-tapos   = <fs_zwmfrt001>-tapos.
    <fs_scr002>-nlpla   = <fs_zwmfrt001>-nlpla.
    <fs_scr002>-werks   = <fs_zwmfrt001>-werks.
    <fs_scr002>-lgort   = <fs_zwmfrt001>-lgort.
    <fs_scr002>-ebelp   = <fs_zwmfrt001>-ebelp.

    INSERT <fs_scr002> INTO TABLE gt_scr002_check[].

    gs_scr001-npall = <fs_zwmfrt001>-numud.
    gs_scr001-versi = <fs_zwmfrt001>-versi.
  ENDLOOP.

  lv_lines  = lines( gt_scr002[] ).

  IF lv_lines GE gs_scr001-npall.  " no more entries allowed
    READ TABLE gt_scr002[] INTO gs_scr002 INDEX lv_lines.

    gs_scr001-cpall = lv_lines.
  ELSE. " prepare for new entry
    gs_scr001-cpall = lv_lines + 1.

    CLEAR gs_scr002.
  ENDIF.
ENDFORM.                    "f_data_scr0010_ucomm_load
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0010_ucomm_unload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_unload.
  FIELD-SYMBOLS <fs_zwmfrt001> LIKE LINE OF gt_zwmfrt001_conf[].

  FREE gt_ltap[].
  FREE gt_scr002[].
  FREE gt_scr002_check[].

  CLEAR gs_scr002.

  gs_scr001-versi = 0.
  LOOP AT gt_zwmfrt001_conf[] ASSIGNING <fs_zwmfrt001>.
    IF <fs_zwmfrt001>-versi GT gs_scr001-versi.
      gs_scr001-versi = <fs_zwmfrt001>-versi.
    ENDIF.
  ENDLOOP.

  IF sy-subrc EQ 0.
    gs_scr001-versi = gs_scr001-versi + 1.
  ENDIF.

  DELETE zwmfrt001 FROM TABLE gt_zwmfrt001[].
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
ENDFORM.                    "f_data_scr0010_ucomm_unload
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_UCOMM_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_clear.
  CLEAR gs_scr001.

  gs_scr001-npall = 1.
  gs_scr001-cpall = 1.

  FREE gt_ekpo[].
  FREE gt_mlgn[].
  FREE gt_makt[].

  CLEAR: gt_zwm020.

  gv_cursor = c_cursor_ebeln.
ENDFORM.                    " F_DATA_SCR0010_UCOMM_CLEAR
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0020_check_lenum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_OKCODE   text
*----------------------------------------------------------------------*
FORM f_data_scr0020_check_lenum USING    u_remont TYPE flag
                                CHANGING c_okcode TYPE syucomm.
  DATA lv_param TYPE zwmmpt001-parametro.
  DATA lv_index TYPE zwmmpt001-item.
  DATA lv_lgnum TYPE ltap-lgnum.
  DATA lv_lenum TYPE lenum.
  DATA lv_lqnum TYPE lqua-lqnum.                            "#EC NEEDED
  DATA lv_msgv1 TYPE bdcmsgcoll-msgv1.
  DATA lv_venum TYPE venum.
*  DATA lv_lenum_conv TYPE lenum.

  DATA ls_ltap TYPE ty_ltap.
  DATA ls_makt TYPE ty_makt.
  DATA ls_vepo TYPE vepo.
  DATA ls_zwm020 TYPE zwm020.

  DATA lt_ltap    TYPE STANDARD TABLE OF ty_ltap.
  DATA lt_zwm026  TYPE STANDARD TABLE OF zwm026.
  DATA lt_vepo    TYPE STANDARD TABLE OF vepo.

  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].
  FIELD-SYMBOLS <fs_scr002>     LIKE LINE OF gt_scr002[].
  FIELD-SYMBOLS <fs_ekpo>       LIKE LINE OF gt_ekpo[].

  IF gs_scr002-lenum IS INITIAL.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_scr002-lenum
    IMPORTING
      output = gs_scr002-lenum.

*  CALL FUNCTION 'CONVERSION_EXIT_LENUM_INPUT'
*    EXPORTING
*      input           = gs_scr002-lenum
*    IMPORTING
*      output          = lv_lenum_conv
*    EXCEPTIONS
*      check_failed    = 1
*      not_numeric     = 2
*      t344_get_failed = 3
*      wrong_length    = 4
*      OTHERS          = 5.
*  IF sy-subrc <> 0.
*    CLEAR gs_scr002-lenum.
*    CLEAR c_okcode.
*    RETURN.
*  ENDIF.

*  gs_scr002-lenum = lv_lenum_conv.


  READ TABLE gt_scr002_check[] TRANSPORTING NO FIELDS
    WITH TABLE KEY lenum = gs_scr002-lenum
                   zeugn = space.
  IF sy-subrc EQ 0. " UD already entered
    CLEAR gs_scr002-lenum.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '040'.

    CLEAR c_okcode.
    RETURN.
  ENDIF.

  READ TABLE gt_zwmfrt001_conf[] TRANSPORTING NO FIELDS
    WITH KEY lgnum = gs_xuser-lgnum
*                   xblnr = gs_scr001-xblnr
*                   ebeln = gs_scr001-ebeln
*                   versi = gs_scr001-versi
*                   ebelp = gs_scr002-ebelp
                    lenum = gs_scr002-lenum.
*                   vepos = gs_scr002-vepos
*                   zeugn = gs_scr002-zeugn.
  IF sy-subrc EQ 0. " UD already entered
    CLEAR gs_scr002-lenum.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '040'.

    CLEAR c_okcode.
    RETURN.
  ENDIF.

  CASE sy-tcode.
    WHEN c_tcode_zwmfr0002a.
      lv_param  = c_param_lgnum_pa_pt.
      lv_index  = 0.
    WHEN c_tcode_zwmfr0002b.
      lv_param  = c_param_lgnum_pt.
      lv_index  = 1.
    WHEN c_tcode_zwmfr0002d.
      lv_param  = c_param_lgnum_pa_pt.
      lv_index  = 0.
    WHEN OTHERS.
      lv_param  = c_param_lgnum_pa_pt.
      lv_index  = 0.
  ENDCASE.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo  = c_process_general
                   parametro = lv_param
                   item      = lv_index.
  IF sy-subrc EQ 0.
    lv_lgnum  = <fs_zwmmpt001>-valor.
  ENDIF.

  " Check if UD already exists in user's warehouse deposit
  SELECT lqnum UP TO 1 ROWS
    FROM lqua INTO lv_lqnum
    WHERE lgnum EQ gs_xuser-lgnum
      AND lenum EQ gs_scr002-lenum. " Index LQUA~ZSU
  ENDSELECT.
  IF sy-subrc EQ 0. " UD already exists in user's warehouse
    lv_msgv1  = gs_scr002-lenum.

    CLEAR gs_scr002-lenum.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '021'
        message_var1   = lv_msgv1.

    CLEAR c_okcode.
    RETURN.
  ENDIF.

  IF sy-tcode EQ c_tcode_zwmfr0002a OR sy-tcode EQ c_tcode_zwmfr0002d.
    "Se for recepção de Prod Acadado pode ser de palete de Picking
    SELECT * FROM zwm026
             INTO TABLE lt_zwm026
             WHERE armazem = lv_lgnum AND
                   sscc    = gs_scr002-lenum.
  ENDIF.

  IF NOT lt_zwm026 IS INITIAL.
**  Picking
    DO 1 TIMES.
      SELECT SINGLE venum FROM vekp
                          INTO lv_venum
                          WHERE exidv = gs_scr002-lenum AND
                                status <> '0060'.
      CHECK sy-subrc EQ 0.

      SELECT * FROM vepo
               INTO TABLE lt_vepo
               WHERE venum = lv_venum.
    ENDDO.

    LOOP AT lt_vepo INTO ls_vepo.
      CLEAR: ls_ltap.

      READ TABLE gt_ekpo[] ASSIGNING <fs_ekpo>
        WITH TABLE KEY matnr = ls_vepo-matnr.
      IF sy-subrc NE 0.
        CLEAR gs_scr002-lenum.

**      ERRO: A palete & não é válida para este processo
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = c_msgid_zwmfr001
            message_lang   = sy-langu
            message_type   = c_msgty_e
            message_number = '021'.

        CLEAR c_okcode.
        RETURN.
      ENDIF.

      CLEAR: ls_makt.
      READ TABLE gt_makt[] INTO ls_makt
        WITH TABLE KEY matnr = ls_vepo-matnr.

      APPEND INITIAL LINE TO gt_scr002[] ASSIGNING <fs_scr002>.
      <fs_scr002>-lenum   = gs_scr002-lenum.
      <fs_scr002>-vepos   = gs_scr002-vepos.
      <fs_scr002>-zeugn   = space.
      <fs_scr002>-matnr   = ls_vepo-matnr.
      <fs_scr002>-maktx   = ls_makt-maktx.
      <fs_scr002>-maktx_1 = ls_makt-maktx(20).
      <fs_scr002>-maktx_2 = ls_makt-maktx+20(20).
      <fs_scr002>-charg   = ls_vepo-charg.
      <fs_scr002>-vsolm   = ls_vepo-vemng.
      <fs_scr002>-meins   = ls_vepo-vemeh.
      <fs_scr002>-letyp   = ls_ltap-letyp.
      <fs_scr002>-lgnum   = ls_ltap-lgnum.
      <fs_scr002>-tanum   = ls_ltap-tanum.
      <fs_scr002>-tapos   = ls_ltap-tapos.
      <fs_scr002>-nlpla   = ls_ltap-nlpla.
      <fs_scr002>-werks   = <fs_ekpo>-werks.
      <fs_scr002>-lgort   = <fs_ekpo>-lgort.
      <fs_scr002>-ebelp   = <fs_ekpo>-ebelp.
      <fs_scr002>-type    = 'PK'.

      gs_scr001-npall = gs_scr001-npall + 1.

      INSERT <fs_scr002> INTO TABLE gt_scr002_check[].

      gs_scr002 = <fs_scr002>.

      PERFORM f_data_scr0020_add_zwmfrt100 USING <fs_scr002>.
    ENDLOOP.

    gs_scr001-npall = gs_scr001-npall - 1.
  ELSE.
**  Paletes Completas

    " Check if UD exists in origin warehouse deposit
    SELECT lgnum tanum tapos matnr charg bestq meins
           letyp pquit vsolm nlpla vlenr
      FROM ltap INTO TABLE lt_ltap[]
      WHERE lgnum EQ lv_lgnum
        AND vlenr EQ gs_scr002-lenum. " Partial index LTAP~ZWM

    " Delete all entries where stock status is not initial
    SORT lt_ltap[] BY bestq.
    DELETE lt_ltap[] WHERE bestq IS NOT INITIAL.
    " Delete all entries where SU is not confirmed
    SORT lt_ltap[] BY pquit.
    DELETE lt_ltap[] WHERE pquit IS INITIAL.

    SORT lt_ltap[] BY lgnum tanum DESCENDING tapos DESCENDING.

    READ TABLE lt_ltap[] INTO ls_ltap INDEX 1.
    IF sy-subrc NE 0.
      lv_msgv1  = gs_scr002-lenum.

      CLEAR gs_scr002-lenum.

**  ERRO: A palete & não é válida para este processo
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = c_msgid_zwmfr001
          message_lang   = sy-langu
          message_type   = c_msgty_e
          message_number = '021'
          message_var1   = lv_msgv1.

      CLEAR c_okcode.
      RETURN.
    ENDIF.

    READ TABLE gt_ekpo[] ASSIGNING <fs_ekpo>
      WITH TABLE KEY matnr = ls_ltap-matnr.
    IF sy-subrc NE 0.
      CLEAR gs_scr002-lenum.

**  ERRO: A palete & não é válida para este processo
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = c_msgid_zwmfr001
          message_lang   = sy-langu
          message_type   = c_msgty_e
          message_number = '021'.

      CLEAR c_okcode.
      RETURN.
    ENDIF.

    IF sy-tcode EQ c_tcode_zwmfr0002b.
      READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
        WITH TABLE KEY processo  = c_process_general
                       parametro = c_param_letyp
                       item      = 1.
      IF sy-subrc EQ 0.
        ls_ltap-letyp  = <fs_zwmmpt001>-valor.
      ENDIF.
    ENDIF.

    INSERT ls_ltap INTO TABLE gt_ltap[].

    READ TABLE gt_makt[] INTO ls_makt
      WITH TABLE KEY matnr = ls_ltap-matnr.

    APPEND INITIAL LINE TO gt_scr002[] ASSIGNING <fs_scr002>.
    <fs_scr002>-lenum   = gs_scr002-lenum.
    <fs_scr002>-zeugn   = space.
    <fs_scr002>-matnr   = ls_ltap-matnr.
    <fs_scr002>-maktx   = ls_makt-maktx.
    <fs_scr002>-maktx_1 = ls_makt-maktx(20).
    <fs_scr002>-maktx_2 = ls_makt-maktx+20(20).
    <fs_scr002>-charg   = ls_ltap-charg.
    <fs_scr002>-vsolm   = ls_ltap-vsolm.
    <fs_scr002>-meins   = ls_ltap-meins.
    <fs_scr002>-letyp   = ls_ltap-letyp.
    <fs_scr002>-lgnum   = ls_ltap-lgnum.
    <fs_scr002>-tanum   = ls_ltap-tanum.
    <fs_scr002>-tapos   = ls_ltap-tapos.
    <fs_scr002>-nlpla   = ls_ltap-nlpla.
    <fs_scr002>-werks   = <fs_ekpo>-werks.
    <fs_scr002>-lgort   = <fs_ekpo>-lgort.
    <fs_scr002>-ebelp   = <fs_ekpo>-ebelp.
    <fs_scr002>-type    = 'PC'.

    INSERT <fs_scr002> INTO TABLE gt_scr002_check[].

    gs_scr002 = <fs_scr002>.

    PERFORM f_data_scr0020_add_zwmfrt100 USING <fs_scr002>.
  ENDIF.

** Valida Remontada
***********************************************************************
  CLEAR: lv_lenum.

  DO 1 TIMES.
    CHECK u_remont EQ abap_false.

    SELECT SINGLE * FROM zwm020
                    INTO ls_zwm020
                    WHERE armazem = lv_lgnum AND
                          ( p1 = gs_scr002-lenum OR p2 = gs_scr002-lenum ).

    CHECK sy-subrc EQ 0.

    IF ls_zwm020-p1 EQ gs_scr002-lenum.
      lv_lenum = ls_zwm020-p2.
    ELSE.
      lv_lenum = ls_zwm020-p1.
    ENDIF.

    CHECK NOT lv_lenum IS INITIAL.

    gs_scr002-lenum = lv_lenum.
    PERFORM f_data_scr0020_check_lenum USING abap_true CHANGING c_okcode.
*    DESCRIBE TABLE gt_scr002 LINES gs_scr001-npall.
    APPEND ls_zwm020 TO gt_zwm020.
  ENDDO.

  gv_cursor = c_cursor_pnext.
ENDFORM.                    " F_DATA_SCR0020_CHECK_LENUM
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0020_add_zwmfrt100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_SCR002  text
*----------------------------------------------------------------------*
FORM f_data_scr0020_add_zwmfrt100 USING is_scr002 TYPE ty_scr002.
  DATA ls_zwmfrt001 TYPE zwmfrt001.

  GET TIME.
  ls_zwmfrt001-mandt  = sy-mandt.
  ls_zwmfrt001-lgnum  = gs_xuser-lgnum.
  ls_zwmfrt001-xblnr  = gs_scr001-xblnr.
  ls_zwmfrt001-ebeln  = gs_scr001-ebeln.
  ls_zwmfrt001-versi  = gs_scr001-versi.
  ls_zwmfrt001-ebelp  = is_scr002-ebelp.
  ls_zwmfrt001-lenum  = is_scr002-lenum.
  ls_zwmfrt001-vepos  = is_scr002-vepos.
  ls_zwmfrt001-zeugn  = is_scr002-zeugn.
  ls_zwmfrt001-matnr  = is_scr002-matnr.
  ls_zwmfrt001-vsolm  = is_scr002-vsolm.
  ls_zwmfrt001-meins  = is_scr002-meins.
  ls_zwmfrt001-charg  = is_scr002-charg.
  ls_zwmfrt001-licha  = gs_scr001-licha.
  ls_zwmfrt001-letyp  = is_scr002-letyp.
  ls_zwmfrt001-werks  = is_scr002-werks.
  ls_zwmfrt001-lgort  = is_scr002-lgort.
  ls_zwmfrt001-tanum  = is_scr002-tanum.
  ls_zwmfrt001-tapos  = is_scr002-tapos.
  ls_zwmfrt001-nlpla  = is_scr002-nlpla.
  ls_zwmfrt001-numud  = gs_scr001-npall.
  ls_zwmfrt001-uname  = sy-uname.
  ls_zwmfrt001-datlo  = sy-datum.
  ls_zwmfrt001-uzeit  = sy-uzeit.

  INSERT ls_zwmfrt001 INTO TABLE gt_zwmfrt001[].

  MODIFY zwmfrt001 FROM ls_zwmfrt001.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
ENDFORM.                    "f_data_scr0020_add_zwmfrt100
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0020_UCOMM_NEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_scr0020_ucomm_next.
  gs_scr001-cpall = lines( gt_scr002[] ).


  IF gs_scr001-cpall LT gs_scr001-npall.
    gs_scr001-cpall = gs_scr001-cpall + 1.
    CLEAR gs_scr002.

    IF sy-tcode NE c_tcode_zwmfr0002c.
      gv_cursor = c_cursor_lenum.
    ELSE.
      gv_cursor = c_cursor_zeugn.
    ENDIF.
  ELSE.
    gv_cursor = c_cursor_psave.
  ENDIF.
ENDFORM.                    " F_DATA_SCR0020_UCOMM_NEXT
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0020_ucomm_clear
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->C_CURSOR   text
*----------------------------------------------------------------------*
FORM f_data_scr0020_ucomm_clear USING i_cursor TYPE fieldname.
  DATA ls_zwmfrt001 TYPE zwmfrt001.

  IF gs_scr002-lenum IS INITIAL AND gs_scr002-zeugn IS INITIAL.
    RETURN.
  ENDIF.

  READ TABLE gt_zwmfrt001[] INTO ls_zwmfrt001
    WITH KEY lgnum = gs_xuser-lgnum
*                   xblnr = gs_scr001-xblnr
*                   ebeln = gs_scr001-ebeln
*                   versi = gs_scr001-versi
*                   ebelp = gs_scr002-ebelp
                   lenum = gs_scr002-lenum.
*                   vepos = gs_scr002-vepos
*                   zeugn = gs_scr002-zeugn.

  DELETE zwmfrt001 FROM ls_zwmfrt001.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    DELETE gt_scr002[] INDEX gs_scr001-cpall.

    DELETE TABLE gt_scr002_check[]
      WITH TABLE KEY lenum = gs_scr002-lenum
                     zeugn = gs_scr002-zeugn.

    DELETE gt_zwmfrt001
      WHERE lgnum = gs_xuser-lgnum AND
*                     xblnr = gs_scr001-xblnr
*                     ebeln = gs_scr001-ebeln
*                     versi = gs_scr001-versi
*                     ebelp = gs_scr002-ebelp
                     lenum = gs_scr002-lenum.
*                     vepos = gs_scr002-vepos
*                     zeugn = gs_scr002-zeugn.

    CLEAR gs_scr002.

    READ TABLE gt_scr002[] INTO gs_scr002 INDEX gs_scr001-cpall.
    IF sy-subrc NE 0.
      gs_scr001-cpall = lines( gt_scr002[] ) + 1.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  gv_cursor = i_cursor.
ENDFORM.                    " F_DATA_SCR0020_UCOMM_CLEAR
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0020_UCOMM_PDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_scr0020_ucomm_pdown.
  DATA lv_cpall TYPE int1.

  IF gs_scr001-cpall EQ gs_scr001-npall.
    RETURN.
  ENDIF.

  IF gs_scr002 IS INITIAL.
    RETURN.
  ENDIF.

  lv_cpall  = gs_scr001-cpall + 1.

  READ TABLE gt_scr002[] TRANSPORTING NO FIELDS INDEX lv_cpall.
  IF sy-subrc NE 0.
    CLEAR gs_scr002.
    gs_scr001-cpall = lv_cpall.
    RETURN.
  ENDIF.

  gs_scr001-cpall = lv_cpall.

  READ TABLE gt_scr002[] INTO gs_scr002 INDEX lv_cpall.
ENDFORM.                    " F_DATA_SCR0020_UCOMM_PDOWN
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0020_UCOMM_PUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_scr0020_ucomm_pup.
  IF gs_scr001-cpall EQ 1.
    RETURN.
  ENDIF.

  gs_scr001-cpall = gs_scr001-cpall - 1.

  READ TABLE gt_scr002[] TRANSPORTING NO FIELDS INDEX gs_scr001-cpall.
  IF sy-subrc NE 0.
    gs_scr001-cpall = gs_scr001-cpall + 1.
    RETURN.
  ENDIF.

  READ TABLE gt_scr002[] INTO gs_scr002 INDEX gs_scr001-cpall.
ENDFORM.                    " F_DATA_SCR0020_UCOMM_PUP
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0020_UCOMM_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_scr0020_ucomm_save.
  DATA lv_xblnr TYPE zwmfrt001-xblnr.
  DATA lv_subrc TYPE sysubrc.
  DATA lv_msgid TYPE bdcmsgcoll-msgid.
  DATA lv_msgno TYPE bdcmsgcoll-msgnr.
  DATA lv_msgty TYPE bdcmsgcoll-msgtyp.
  DATA lv_msgv1 TYPE bdcmsgcoll-msgv1.
  DATA lv_msgv2 TYPE bdcmsgcoll-msgv2.
  DATA lv_msgv3 TYPE bdcmsgcoll-msgv3.
  DATA lv_msgv4 TYPE bdcmsgcoll-msgv4.

  IF gs_scr001-cpall > gs_scr001-npall OR gs_scr002 IS INITIAL.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '041'.

    RETURN.
  ENDIF.

  PERFORM f_data_scr0020_ucomm_savemblnr CHANGING lv_subrc lv_msgv1.
  IF lv_subrc NE 0.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '042'.
  ELSE.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_s
        message_number = '026'
        message_var1   = lv_msgv1.


    PERFORM f_data_scr0020_ucomm_saveto CHANGING lv_subrc.
    IF lv_subrc NE 0.
      lv_msgid  = sy-msgid.
      lv_msgno  = sy-msgno.
      lv_msgty  = sy-msgty.
      lv_msgv1  = sy-msgv1.
      lv_msgv2  = sy-msgv2.
      lv_msgv3  = sy-msgv3.
      lv_msgv4  = sy-msgv4.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = lv_msgid
          message_lang   = sy-langu
          message_type   = lv_msgty
          message_number = lv_msgno
          message_var1   = lv_msgv1
          message_var2   = lv_msgv2
          message_var3   = lv_msgv3
          message_var4   = lv_msgv4.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = c_msgid_zwmfr001
          message_lang   = sy-langu
          message_type   = c_msgty_e
          message_number = '028'.
    ELSE.
      PERFORM f_data_scr0020_ucomm_remontada CHANGING lv_subrc.

      PERFORM f_data_scr0020_ucomm_savepal CHANGING lv_subrc.
      IF lv_subrc NE 0.

      ELSE.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = c_msgid_zwmfr001
            message_lang   = sy-langu
            message_type   = c_msgty_s
            message_number = '027'.
      ENDIF.
    ENDIF.
  ENDIF.

  lv_xblnr  = gs_scr001-xblnr.
  CALL FUNCTION 'DEQUEUE_EZ_ZWMFRT001'
    EXPORTING
      lgnum = gs_xuser-lgnum
      xblnr = lv_xblnr
      ebeln = gs_scr001-ebeln.
ENDFORM.                    " F_DATA_SCR0020_UCOMM_SAVE
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0020_ucomm_savemblnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_SUBRC    text
*      <--C_MBLNR    text
*----------------------------------------------------------------------*
FORM f_data_scr0020_ucomm_savemblnr CHANGING c_subrc TYPE sysubrc
                                             c_mblnr TYPE bdcmsgcoll-msgv1.
  DATA lv_gmcode    TYPE gm_code.
  DATA lv_bwart     TYPE bapi2017_gm_item_create-move_type.
  DATA lv_mblnr     TYPE mkpf-mblnr.
  DATA lv_mjahr     TYPE mkpf-mjahr.
  DATA lv_param     TYPE zwmmpt001-parametro.
  DATA lv_index     TYPE zwmmpt001-item.
  DATA lv_benum     TYPE ltak-benum.
  DATA lv_spec_mvmt TYPE lvs_bsskz.
  DATA lv_vbeln     TYPE vbeln.
  DATA lv_vbeln2    TYPE vbeln.

  DATA ls_gmcode  TYPE bapi2017_gm_code.
  DATA ls_header  TYPE bapi2017_gm_head_01.
  DATA ls_gitem   TYPE ty_goodsmvt_itm.
  DATA ls_attr    TYPE bapibatchatt.
  DATA ls_lips    TYPE lips.

  DATA lt_items   TYPE STANDARD TABLE OF bapi2017_gm_item_create.
  DATA lt_return  TYPE bapiret2_t.
  DATA lt_gitems  TYPE ty_st_goodsmvt_itm.
  DATA lt_mcha    TYPE ty_ht_mcha.
  DATA lt_ekpo    TYPE ty_ht_ekpo.
  DATA lt_vbfa    TYPE TABLE OF vbfa.
  DATA lt_vbeln   TYPE TABLE OF vbeln.
  DATA lt_lips    TYPE TABLE OF lips WITH HEADER LINE.

  FIELD-SYMBOLS <fs_scr002>     LIKE LINE OF gt_scr002[].
  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].
  FIELD-SYMBOLS <fs_zwmfrt001>  LIKE LINE OF gt_zwmfrt001[].
  FIELD-SYMBOLS <fs_item>       LIKE LINE OF lt_items[].
  FIELD-SYMBOLS <fs_gitem>      LIKE LINE OF lt_gitems[].
  FIELD-SYMBOLS <fs_ekpo>       LIKE LINE OF lt_ekpo[].

*  IF sy-tcode EQ c_tcode_zwmfr0002d OR
*     sy-tcode EQ c_tcode_zwmfr0002b.
*

**    CALL FUNCTION 'Z_WM_GOODS_INBOUND'
**      EXPORTING
**        i_lgnum = gs_xuser-lgnum
**        i_ebeln = gs_scr001-ebeln
**      IMPORTING
**        e_mblnr = lv_mblnr
**      EXCEPTIONS
**        error   = 1
**        OTHERS  = 2.
**
**    IF sy-subrc <> 0.
**      c_subrc = 4.
**      RETURN.
**    ENDIF.
**    c_mblnr = lv_mblnr.
**    c_subrc = 0.
**    RETURN.
*
*  ENDIF.


  CASE sy-tcode.
    WHEN c_tcode_zwmfr0002a.
      lv_param  = c_param_bwart.
      lv_index  = 0.
    WHEN c_tcode_zwmfr0002b OR c_tcode_zwmfr0002c.
      lv_param  = c_param_bwart_mm.
      lv_index  = 1.
    WHEN c_tcode_zwmfr0002d.
      lv_param  = c_param_bwart.
      lv_index  = 0.
    WHEN OTHERS.
      lv_param  = c_param_bwart.
      lv_index  = 0.
  ENDCASE.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo  = c_process_tp_entries
                   parametro = lv_param
                   item      = lv_index.
  IF sy-subrc EQ 0.
    lv_bwart  = <fs_zwmmpt001>-valor.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo  = c_process_tp_entries
                   parametro = c_param_gmcode
                   item      = 0.
  IF sy-subrc EQ 0.
    lv_gmcode  = <fs_zwmmpt001>-valor.
  ENDIF.

  LOOP AT gt_ekpo[] ASSIGNING <fs_ekpo>.
    INSERT <fs_ekpo> INTO TABLE lt_ekpo[].
  ENDLOOP.

  LOOP AT gt_scr002[] ASSIGNING <fs_scr002>.
    CLEAR ls_gitem.
    ls_gitem-ebeln  = gs_scr001-ebeln.
    ls_gitem-ebelp  = <fs_scr002>-ebelp.
    ls_gitem-matnr  = <fs_scr002>-matnr.
    ls_gitem-werks  = <fs_scr002>-werks.
    ls_gitem-charg  = <fs_scr002>-charg.
    ls_gitem-meins  = <fs_scr002>-meins.
    ls_gitem-vsolm  = <fs_scr002>-vsolm.
    ls_gitem-type   = <fs_scr002>-type.
    COLLECT ls_gitem INTO lt_gitems[].
  ENDLOOP.

  IF lt_gitems[] IS NOT INITIAL.
    SELECT matnr werks charg
      FROM mcha INTO TABLE lt_mcha[]
      FOR ALL ENTRIES IN lt_gitems[]
      WHERE matnr EQ lt_gitems-matnr
        AND werks EQ lt_gitems-werks
        AND charg EQ lt_gitems-charg.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_scr001-ebeln
    IMPORTING
      output = lv_benum.

  LOOP AT lt_gitems[] ASSIGNING <fs_gitem>.
    FREE lt_return[].

    READ TABLE lt_mcha[] TRANSPORTING NO FIELDS
      WITH TABLE KEY matnr = <fs_gitem>-matnr
                     werks = <fs_gitem>-werks
                     charg = <fs_gitem>-charg.
    IF sy-subrc NE 0. " if batch doesn't exist, create a new one for this plant
      ls_attr-vendrbatch  = gs_scr001-licha.
* INETUM - NR - 13.01.2022 - RENPRJ00030 - Inicio
      ls_attr-val_type = <fs_gitem>-charg.
* INETUM - NR - 13.01.2022 - RENPRJ00030 - Fim

      CALL FUNCTION 'BAPI_BATCH_CREATE'
        EXPORTING
          batchattributes = ls_attr
          material        = <fs_gitem>-matnr
          batch           = <fs_gitem>-charg
          plant           = <fs_gitem>-werks
        TABLES
          return          = lt_return[].
      SORT lt_return[] BY type.
      READ TABLE lt_return[] TRANSPORTING NO FIELDS
        WITH KEY type = c_msgty_e
        BINARY SEARCH.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE lt_ekpo[] ASSIGNING <fs_ekpo>
      WITH TABLE KEY ebeln = <fs_gitem>-ebeln
                     ebelp = <fs_gitem>-ebelp.
    CHECK sy-subrc EQ 0.

    APPEND INITIAL LINE TO lt_items[] ASSIGNING <fs_item>.
    <fs_item>-material  = <fs_gitem>-matnr.
    <fs_item>-plant     = <fs_gitem>-werks.
    <fs_item>-stge_loc  = <fs_ekpo>-lgort.
    <fs_item>-batch     = <fs_gitem>-charg.
    <fs_item>-move_type = lv_bwart.
    <fs_item>-entry_qnt = <fs_gitem>-vsolm.
    <fs_item>-entry_uom = <fs_gitem>-meins.
    <fs_item>-po_number = <fs_gitem>-ebeln.
    <fs_item>-po_item   = <fs_gitem>-ebelp.
    <fs_item>-mvt_ind   = 'B'.
    <fs_item>-unload_pt = lv_benum.

    IF sy-tcode EQ c_tcode_zwmfr0002d OR
       sy-tcode EQ c_tcode_zwmfr0002b.

      SELECT *
        FROM lips INTO TABLE lt_lips
* INETUM - NR - 13.01.2022 - RENPRJ00030 - Inicio
***        WHERE vgbel = <fs_gitem>-ebeln
***        AND   vgpos = <fs_gitem>-ebelp.
        WHERE vbeln = gs_scr001-xblnr
          AND charg = <fs_gitem>-charg
          AND vgbel = <fs_gitem>-ebeln
          AND vgpos = <fs_gitem>-ebelp.
* INETUM - NR - 13.01.2022 - RENPRJ00030 - Fim

* INETUM - NR - 18.01.2022 - RENPRJ00030 - Inicio
      "Comentado
***      DELETE lt_lips WHERE uecha IS NOT INITIAL.
* INETUM - NR - 18.01.2022 - RENPRJ00030 - Fim

      READ TABLE lt_lips INDEX 1.
      IF sy-subrc = 0.
        <fs_item>-deliv_numb = lt_lips-vbeln.
        <fs_item>-deliv_item = lt_lips-posnr.
        <fs_item>-base_uom   = <fs_gitem>-meins.
        <fs_item>-quantity   = lt_lips-lfimg.
        <fs_item>-val_type   = lt_lips-bwtar.
      ENDIF.
    ENDIF.

  ENDLOOP.

  FREE lt_return[].

  ls_gmcode-gm_code = lv_gmcode.

  ls_header-pstng_date  = sy-datlo.
  ls_header-doc_date    = sy-datlo.
  ls_header-ref_doc_no  = gs_scr001-xblnr.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_header
      goodsmvt_code    = ls_gmcode
    IMPORTING
      materialdocument = lv_mblnr
      matdocumentyear  = lv_mjahr
    TABLES
      goodsmvt_item    = lt_items[]
      return           = lt_return[].
  SORT lt_return[] BY type.
  READ TABLE lt_return[] TRANSPORTING NO FIELDS
    WITH KEY type = c_msgty_e
    BINARY SEARCH.
  IF sy-subrc EQ 0.
    c_subrc = 4.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    c_subrc = 0.
    c_mblnr = lv_mblnr.

    LOOP AT gt_zwmfrt001[] ASSIGNING <fs_zwmfrt001> WHERE lgnum EQ gs_xuser-lgnum
                                                      AND xblnr EQ gs_scr001-xblnr
                                                      AND ebeln EQ gs_scr001-ebeln
                                                      AND versi EQ gs_scr001-versi.
      <fs_zwmfrt001>-mblnr = lv_mblnr.
      <fs_zwmfrt001>-mjahr = lv_mjahr.
    ENDLOOP.

    MODIFY zwmfrt001 FROM TABLE gt_zwmfrt001[].

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDIF.
ENDFORM.                    "f_data_scr0020_ucomm_savemblnr
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0020_ucomm_saveto
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_data_scr0020_ucomm_saveto CHANGING c_subrc TYPE sysubrc.
  TYPES: BEGIN OF lty_sel_mseg,
           mblnr TYPE mblnr,
           mjahr TYPE mjahr,
         END OF lty_sel_mseg.

  TYPES: BEGIN OF lty_tranford,
           matnr TYPE matnr,
           charg TYPE charg_d,
           tbnum TYPE tbnum,
           tbpos TYPE tbpos,
         END OF lty_tranford.

  DATA: lt_tranford TYPE TABLE OF lty_tranford.
  DATA: ls_tranford TYPE lty_tranford.

  DATA: lt_sel_mseg TYPE TABLE OF lty_sel_mseg.
  DATA: ls_sel_mseg TYPE lty_sel_mseg.

  DATA lv_bwart TYPE ltak-bwlvs.
  DATA lv_tanum TYPE ltak-tanum.
  DATA lv_index TYPE zwmmpt001-item.
  DATA lv_benum TYPE ltak-benum.
  DATA lv_mblnr TYPE mseg-mblnr.
  DATA lv_mjahr TYPE mseg-mjahr.
  DATA lv_tbnum TYPE mseg-tbnum.
  DATA lv_tbpos TYPE mseg-tbpos.
  DATA lv_betyp TYPE lvs_betyp.

  DATA ls_ltap  TYPE ltap.
  DATA ls_t320  TYPE t320.

  DATA lr_lenum TYPE RANGE OF ltap-vlenr.
  DATA lr_zeugn TYPE RANGE OF ltap-zeugn.

  DATA lt_ltap  TYPE STANDARD TABLE OF ltap_vb.
  DATA lt_trite TYPE l03b_trite_t.

  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].
  FIELD-SYMBOLS <fs_lenum>      LIKE LINE OF lr_lenum[].
  FIELD-SYMBOLS <fs_zeugn>      LIKE LINE OF lr_zeugn[].
  FIELD-SYMBOLS <fs_scr002>     LIKE LINE OF gt_scr002[].
  FIELD-SYMBOLS <fs_zwmfrt001>  LIKE LINE OF gt_zwmfrt001[].
  FIELD-SYMBOLS <fs_ltap>       LIKE LINE OF lt_ltap[].
  FIELD-SYMBOLS <fs_trite>      LIKE LINE OF lt_trite[].

  READ TABLE gt_zwmfrt001[] ASSIGNING <fs_zwmfrt001> INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE * INTO ls_t320
        FROM t320
            WHERE werks = <fs_zwmfrt001>-werks
              AND lgort = <fs_zwmfrt001>-lgort.
    IF sy-subrc <> 0.
      c_subrc = 0.
      RETURN.
    ENDIF.
  ENDIF.

  CASE sy-tcode.
    WHEN c_tcode_zwmfr0002a OR c_tcode_zwmfr0002d.
      lv_index  = 0.
    WHEN c_tcode_zwmfr0002b OR c_tcode_zwmfr0002c.
      lv_index  = 1.

      LOOP AT gt_zwmfrt001[] ASSIGNING <fs_zwmfrt001> WHERE lgnum EQ gs_xuser-lgnum
                                                        AND xblnr EQ gs_scr001-xblnr
                                                        AND ebeln EQ gs_scr001-ebeln
                                                        AND versi EQ gs_scr001-versi.
        ls_sel_mseg-mblnr = lv_mblnr  = <fs_zwmfrt001>-mblnr.
        ls_sel_mseg-mjahr = <fs_zwmfrt001>-mjahr.
        APPEND ls_sel_mseg TO lt_sel_mseg.
      ENDLOOP.

      IF NOT lt_sel_mseg IS INITIAL.
        SORT lt_sel_mseg.
        DELETE ADJACENT DUPLICATES FROM lt_sel_mseg.
        SELECT matnr charg tbnum
               tbpos             FROM mseg
                                 INTO TABLE lt_tranford
                                 FOR ALL ENTRIES IN lt_sel_mseg
                                 WHERE mblnr EQ lt_sel_mseg-mblnr AND
                                       mjahr EQ lt_sel_mseg-mjahr.

        SORT lt_tranford.
        DELETE ADJACENT DUPLICATES FROM lt_tranford.
      ENDIF.

**      SELECT tbnum tbpos UP TO 1 ROWS
**        FROM mseg INTO (lv_tbnum, lv_tbpos)
**        WHERE mblnr EQ lv_mblnr
**          AND mjahr EQ lv_mjahr.
**      ENDSELECT.
    WHEN OTHERS.
      lv_index  = 0.
  ENDCASE.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_scr001-ebeln
    IMPORTING
      output = lv_benum.

  LOOP AT gt_scr002[] ASSIGNING <fs_scr002>.
    CLEAR ls_ltap.

    CLEAR lv_tanum.

    IF <fs_scr002>-type EQ 'PK'.
      READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
        WITH TABLE KEY processo  = c_process_tp_entries
                       parametro = c_param_bwart_wm_pk
                       item      = lv_index.
    ELSE.
      READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
        WITH TABLE KEY processo  = c_process_tp_entries
                       parametro = c_param_bwart_wm
                       item      = lv_index.
    ENDIF.

    IF sy-subrc EQ 0.
      lv_bwart  = <fs_zwmmpt001>-valor.
    ELSE.
      c_subrc = 4.
      RETURN.
    ENDIF.



    IF lr_lenum[] IS NOT INITIAL AND
       <fs_scr002>-type <> 'PK'.
      CHECK <fs_scr002>-lenum NOT IN lr_lenum[].
    ENDIF.

    IF lr_zeugn[] IS NOT INITIAL.
      CHECK <fs_scr002>-zeugn NOT IN lr_zeugn[].
    ENDIF.

    SET UPDATE TASK LOCAL.


    IF sy-tcode EQ c_tcode_zwmfr0002d.
      CALL FUNCTION 'Z_WMFR_CHANGE_HU'
        EXPORTING
          i_lenum    = <fs_scr002>-lenum
          i_werks    = <fs_scr002>-werks
          i_lgort    = <fs_scr002>-lgort
          i_status   = c_status_0020
          i_vpobj    = c_vpobj_12
          i_lgort_hu = ''.
    ENDIF.


    IF sy-tcode EQ c_tcode_zwmfr0002b OR sy-tcode EQ c_tcode_zwmfr0002c.
      FREE lt_ltap[].
      FREE lt_trite[].

      APPEND INITIAL LINE TO lt_trite[] ASSIGNING <fs_trite>.

      CLEAR: ls_tranford.
      READ TABLE lt_tranford
            INTO ls_tranford
            WITH KEY matnr = <fs_scr002>-matnr
                     charg = <fs_scr002>-charg.

      <fs_trite>-tbpos    = ls_tranford-tbpos.
      <fs_trite>-anfme    = <fs_scr002>-vsolm.
      <fs_trite>-altme    = <fs_scr002>-meins.
      <fs_trite>-charg    = <fs_scr002>-charg.
      <fs_trite>-nlenr    = <fs_scr002>-lenum.
      <fs_trite>-letyp    = <fs_scr002>-letyp.
      <fs_trite>-zzzeugn  = <fs_scr002>-zeugn.

      CALL FUNCTION 'L_TO_CREATE_TR'
        EXPORTING
          i_lgnum                        = gs_xuser-lgnum
          i_tbnum                        = ls_tranford-tbnum
          i_update_task                  = abap_true
          i_commit_work                  = abap_true
          it_trite                       = lt_trite[]
        IMPORTING
          e_tanum                        = lv_tanum
        TABLES
          t_ltap_vb                      = lt_ltap[]
        EXCEPTIONS
          foreign_lock                   = 1
          qm_relevant                    = 2
          tr_completed                   = 3
          xfeld_wrong                    = 4
          ldest_wrong                    = 5
          drukz_wrong                    = 6
          tr_wrong                       = 7
          squit_forbidden                = 8
          no_to_created                  = 9
          update_without_commit          = 10
          no_authority                   = 11
          preallocated_stock             = 12
          partial_transfer_req_forbidden = 13
          input_error                    = 14
          error_message                  = 99
          OTHERS                         = 15.
    ELSE.
      lv_betyp = 'B'.
      CALL FUNCTION 'L_TO_CREATE_SINGLE'
        EXPORTING
          i_lgnum               = gs_xuser-lgnum
          i_bwlvs               = lv_bwart
          i_betyp               = lv_betyp
          i_benum               = lv_benum
          i_charg               = <fs_scr002>-charg
          i_matnr               = <fs_scr002>-matnr
          i_werks               = <fs_scr002>-werks
          i_lgort               = <fs_scr002>-lgort
          i_letyp               = <fs_scr002>-letyp
          i_anfme               = <fs_scr002>-vsolm
          i_altme               = <fs_scr002>-meins
          i_nlenr               = <fs_scr002>-lenum
          i_zeugn               = <fs_scr002>-zeugn
          i_update_task         = abap_true
          i_commit_work         = abap_true
        IMPORTING
          e_tanum               = lv_tanum
          e_ltap                = ls_ltap
        EXCEPTIONS
          no_to_created         = 1
          bwlvs_wrong           = 2
          betyp_wrong           = 3
          benum_missing         = 4
          betyp_missing         = 5
          foreign_lock          = 6
          vltyp_wrong           = 7
          vlpla_wrong           = 8
          vltyp_missing         = 9
          nltyp_wrong           = 10
          nlpla_wrong           = 11
          nltyp_missing         = 12
          rltyp_wrong           = 13
          rlpla_wrong           = 14
          rltyp_missing         = 15
          squit_forbidden       = 16
          manual_to_forbidden   = 17
          letyp_wrong           = 18
          vlpla_missing         = 19
          nlpla_missing         = 20
          sobkz_wrong           = 21
          sobkz_missing         = 22
          sonum_missing         = 23
          bestq_wrong           = 24
          lgber_wrong           = 25
          xfeld_wrong           = 26
          date_wrong            = 27
          drukz_wrong           = 28
          ldest_wrong           = 29
          update_without_commit = 30
          no_authority          = 31
          material_not_found    = 32
          lenum_wrong           = 33
          error_message         = 99
          OTHERS                = 34.
    ENDIF.
    IF sy-subrc NE 0.
      c_subrc = 4.
      EXIT.
    ELSE.
      READ TABLE gt_zwmfrt001[] ASSIGNING <fs_zwmfrt001>
        WITH KEY lgnum  = gs_xuser-lgnum
*                       xblnr  = gs_scr001-xblnr
*                       ebeln  = gs_scr001-ebeln
*                       versi  = gs_scr001-versi
*                       ebelp  = <fs_scr002>-ebelp
                       lenum  = <fs_scr002>-lenum.
*                       vepos  = <fs_scr002>-vepos
*                       zeugn  = <fs_scr002>-zeugn.
      IF sy-subrc EQ 0.
        <fs_zwmfrt001>-ntanum = lv_tanum.

        IF sy-tcode EQ c_tcode_zwmfr0002b.
          READ TABLE lt_ltap[] ASSIGNING <fs_ltap> INDEX 1.
          IF sy-subrc EQ 0.
            <fs_zwmfrt001>-ntapos = <fs_ltap>-tapos.
          ENDIF.
        ELSE.
          <fs_zwmfrt001>-ntapos = ls_ltap-tapos.
        ENDIF.

        MODIFY zwmfrt001 FROM <fs_zwmfrt001>.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        WAIT UP TO 2 SECONDS.

        IF ( sy-tcode EQ c_tcode_zwmfr0002a OR sy-tcode EQ c_tcode_zwmfr0002d ) AND
           <fs_scr002>-type <> 'PK'.
          CALL FUNCTION 'Z_WMFR_CHANGE_HU'
            EXPORTING
              i_lenum  = <fs_scr002>-lenum
              i_werks  = <fs_scr002>-werks
              i_lgort  = <fs_scr002>-lgort
              i_status = c_status_0020
              i_vpobj  = c_vpobj_12.
        ENDIF.

        WAIT UP TO 2 SECONDS.
      ENDIF.
    ENDIF.

    IF <fs_scr002>-lenum IS NOT INITIAL.
      APPEND INITIAL LINE TO lr_lenum[] ASSIGNING <fs_lenum>.
      <fs_lenum>-sign   = c_rsig_i.
      <fs_lenum>-option = c_ropt_eq.
      <fs_lenum>-low    = <fs_scr002>-lenum.
    ELSEIF <fs_scr002>-zeugn IS NOT INITIAL.
      APPEND INITIAL LINE TO lr_zeugn[] ASSIGNING <fs_zeugn>.
      <fs_zeugn>-sign   = c_rsig_i.
      <fs_zeugn>-option = c_ropt_eq.
      <fs_zeugn>-low    = <fs_scr002>-zeugn.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "f_data_scr0020_ucomm_saveto
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0030_check_zeugn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_OKCODE   text
*      <--C_CURSOR   text
*----------------------------------------------------------------------*
FORM f_data_scr0030_check_zeugn  CHANGING c_okcode TYPE syucomm
                                          c_cursor TYPE fieldname.
  FIELD-SYMBOLS <fs_zwmmpt001> LIKE LINE OF gt_zwmmpt001[].

  IF gs_scr002-zeugn IS INITIAL.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  READ TABLE gt_scr002_check[] TRANSPORTING NO FIELDS
    WITH TABLE KEY lenum = space
                   zeugn = gs_scr002-zeugn.
  IF sy-subrc EQ 0. " UD already entered
    CLEAR gs_scr002-zeugn.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '049'.

    CLEAR c_okcode.
    RETURN.
  ENDIF.

  READ TABLE gt_zwmfrt001_conf[] TRANSPORTING NO FIELDS
    WITH TABLE KEY lgnum = gs_xuser-lgnum
                   xblnr = gs_scr001-xblnr
                   ebeln = gs_scr001-ebeln
                   versi = gs_scr001-versi
                   ebelp = gs_scr002-ebelp
                   lenum = gs_scr002-lenum
                   vepos = gs_scr002-vepos
                   zeugn = gs_scr002-zeugn.
  IF sy-subrc EQ 0. " UD already entered
    CLEAR gs_scr002-zeugn.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '049'.

    CLEAR c_okcode.
    RETURN.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo  = c_process_general
                   parametro = c_param_letyp
                   item      = 1.
  IF sy-subrc EQ 0.
    gs_scr002-letyp  = <fs_zwmmpt001>-valor.
  ENDIF.

  gs_scr002-charg = gs_scr001-ebeln.

  c_cursor = c_cursor_matnr.
ENDFORM.                    "f_data_scr0030_check_zeugn
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0030_check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_OKCODE   text
*      <--C_CURSOR   text
*----------------------------------------------------------------------*
FORM f_data_scr0030_check_matnr  CHANGING c_okcode TYPE syucomm
                                          c_cursor TYPE fieldname.
  DATA ls_makt    TYPE ty_makt.

  FIELD-SYMBOLS <fs_ekpo> LIKE LINE OF gt_ekpo[].

  IF gs_scr002-matnr IS INITIAL.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  READ TABLE gt_ekpo[] ASSIGNING <fs_ekpo>
    WITH TABLE KEY matnr = gs_scr002-matnr.
  IF sy-subrc NE 0.
    CLEAR gs_scr002-matnr.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '021'.

    CLEAR c_okcode.
    RETURN.
  ENDIF.

  READ TABLE gt_makt[] INTO ls_makt
    WITH TABLE KEY matnr = gs_scr002-matnr.

  gs_scr002-maktx   = ls_makt-maktx.
  gs_scr002-maktx_1 = ls_makt-maktx(20).
  gs_scr002-maktx_2 = ls_makt-maktx+20(20).
  gs_scr002-meins   = <fs_ekpo>-meins.
  gs_scr002-werks   = <fs_ekpo>-werks.
  gs_scr002-lgort   = <fs_ekpo>-lgort.
  gs_scr002-ebelp   = <fs_ekpo>-ebelp.

  c_cursor = c_cursor_charg.
ENDFORM.                    "f_data_scr0030_check_matnr
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0030_CHECK_CHARG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_OKCODE   text
*      <--C_CURSOR   text
*----------------------------------------------------------------------*
FORM f_data_scr0030_check_charg  CHANGING c_okcode TYPE syucomm
                                          c_cursor TYPE fieldname.
  IF gs_scr002-charg IS INITIAL.
    CLEAR c_okcode.
    RETURN.
  ENDIF.

  c_cursor = c_cursor_vsolm.
ENDFORM.                    " F_DATA_SCR0030_CHECK_CHARG
*&---------------------------------------------------------------------*
*&      Form  f_data_scr0030_check_vsolm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_OKCODE   text
*      <--C_CURSOR   text
*----------------------------------------------------------------------*
FORM f_data_scr0030_check_vsolm  CHANGING c_okcode TYPE syucomm
                                          c_cursor TYPE fieldname.
  FIELD-SYMBOLS <fs_scr002>     LIKE LINE OF gt_scr002[].

  IF gs_scr002-vsolm GT 9999.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = c_msgid_zwmfr001
        message_lang   = sy-langu
        message_type   = c_msgty_e
        message_number = '048'.

    CLEAR c_okcode.
    RETURN.
  ENDIF.

  APPEND INITIAL LINE TO gt_scr002[] ASSIGNING <fs_scr002>.
  <fs_scr002>-lenum   = space.
  <fs_scr002>-zeugn   = gs_scr002-zeugn.
  <fs_scr002>-matnr   = gs_scr002-matnr.
  <fs_scr002>-maktx   = gs_scr002-maktx.
  <fs_scr002>-maktx_1 = gs_scr002-maktx(20).
  <fs_scr002>-maktx_2 = gs_scr002-maktx+20(20).
  <fs_scr002>-charg   = gs_scr002-charg.
  <fs_scr002>-vsolm   = gs_scr002-vsolm.
  <fs_scr002>-meins   = gs_scr002-meins.
  <fs_scr002>-letyp   = gs_scr002-letyp.
  <fs_scr002>-lgnum   = gs_scr002-lgnum.
  <fs_scr002>-tanum   = gs_scr002-tanum.
  <fs_scr002>-tapos   = gs_scr002-tapos.
  <fs_scr002>-nlpla   = gs_scr002-nlpla.
  <fs_scr002>-werks   = gs_scr002-werks.
  <fs_scr002>-lgort   = gs_scr002-lgort.
  <fs_scr002>-ebelp   = gs_scr002-ebelp.

  INSERT <fs_scr002> INTO TABLE gt_scr002_check[].

  gs_scr002 = <fs_scr002>.

  PERFORM f_data_scr0020_add_zwmfrt100 USING <fs_scr002>.

  c_cursor = c_cursor_pnext.
ENDFORM.                    "f_data_scr0030_check_vsolm
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0020_UCOMM_SAVEPAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM f_data_scr0020_ucomm_savepal  CHANGING c_subrc TYPE sysubrc.
  DATA: lt_vekp      TYPE TABLE OF vekp,
        lt_zwmmpt020 TYPE SORTED TABLE OF zwmmpt020 WITH UNIQUE KEY vhilm,
        lt_zwmmpt001 TYPE TABLE OF zwmmpt001,
        lt_items     TYPE tab_bapi_goodsmvt_item,
        lt_messages  TYPE	tab_bdcmsgcoll,
        lt_zwmmpt021 TYPE SORTED TABLE OF zwmmpt021 WITH UNIQUE KEY vhilm.

  DATA: ls_ekpo       TYPE ty_ekpo,
        ls_zwmmpt001  TYPE zwmmpt001,
        ls_vekp       TYPE vekp,
        ls_zwmmpt020  TYPE zwmmpt020,
        ls_item       TYPE bapi2017_gm_item_create,
        lt_ekpo_vhilm TYPE TABLE OF ekpo,
        ls_ekpo_vhilm TYPE ekpo,
        ls_ekko       TYPE ekko.

  DATA: lv_gmcode	TYPE gm_code,
        lv_bwart  TYPE bwart.

  CLEAR: c_subrc.

  CHECK sy-tcode EQ c_tcode_zwmfr0002b OR
        sy-tcode EQ c_tcode_zwmfr0002a OR
        sy-tcode EQ c_tcode_zwmfr0002d.

** Paramtros
***********************************************************************
  SELECT * FROM zwmmpt001
     INTO TABLE lt_zwmmpt001
     WHERE armazem  = gs_xuser-lgnum AND
           processo = 'ENTRADA_TERCEIROS'.

  LOOP AT lt_zwmmpt001 INTO ls_zwmmpt001.
    CASE ls_zwmmpt001-parametro.
      WHEN 'GM_CODE'.
        lv_gmcode = ls_zwmmpt001-valor.
      WHEN 'MOV'.
        lv_bwart = ls_zwmmpt001-valor.
    ENDCASE.
  ENDLOOP.




** Retorna Todas as HU's
**********************************************************************
  CHECK NOT gt_scr002[] IS INITIAL.

  SELECT * FROM vekp
           INTO TABLE lt_vekp
           FOR ALL ENTRIES IN gt_scr002[]
           WHERE exidv = gt_scr002-lenum.

  CHECK sy-subrc EQ 0.

  READ TABLE gt_ekpo
        INTO ls_ekpo
        INDEX 1.

** retorna Pedido
***********************************************************************
  SELECT SINGLE * FROM ekko
                  INTO ls_ekko
                  WHERE ebeln = ls_ekpo-ebeln.

  CHECK sy-subrc EQ 0.

** Retorna Entrada de Paletes Valorizadas
***********************************************************************
  SELECT * FROM zwmmpt021
           INTO TABLE lt_zwmmpt021
           WHERE lifnr = ls_ekko-lifnr.

  CHECK sy-subrc EQ 0.

** Retorna Pedidos de Paletes
***********************************************************************
  SELECT * FROM zwmmpt020
           INTO TABLE lt_zwmmpt020
           FOR ALL ENTRIES IN lt_vekp
           WHERE werks = ls_ekpo-werks AND
                 vhilm = lt_vekp-vhilm AND
                 val   = abap_true.

  CHECK sy-subrc EQ 0.

  SELECT * FROM ekpo
           INTO TABLE lt_ekpo_vhilm
           FOR ALL ENTRIES IN lt_zwmmpt020
           WHERE ebeln = lt_zwmmpt020-ebeln.

  DELETE lt_ekpo_vhilm WHERE loekz IS NOT INITIAL.

  LOOP AT lt_vekp INTO ls_vekp.
    READ TABLE lt_zwmmpt021
         WITH TABLE KEY vhilm = ls_vekp-vhilm
         TRANSPORTING NO FIELDS.

    CHECK sy-subrc EQ 0.

    CLEAR: ls_zwmmpt020.
    READ TABLE lt_zwmmpt020
          INTO ls_zwmmpt020
          WITH TABLE KEY vhilm = ls_vekp-vhilm.
    CHECK sy-subrc EQ 0.

    CLEAR: ls_ekpo_vhilm.
    READ TABLE lt_ekpo_vhilm
          INTO ls_ekpo_vhilm
          WITH KEY ebeln = ls_zwmmpt020-ebeln
                   matnr = ls_zwmmpt020-vhilm.

    CHECK sy-subrc EQ 0.

    IF sy-tcode EQ c_tcode_zwmfr0002b.
      ls_item-stge_loc = 'MP'.
    ELSEIF sy-tcode EQ c_tcode_zwmfr0002a OR sy-tcode EQ c_tcode_zwmfr0002d.
      ls_item-stge_loc = 'CD'.
    ENDIF.

    ls_item-material  = ls_zwmmpt020-vhilm.
    ls_item-plant     = ls_ekpo-werks.
*    ls_item-stge_loc  = ls_ekpo-lgort.
    ls_item-po_number = ls_ekpo_vhilm-ebeln.
    ls_item-po_item   = ls_ekpo_vhilm-ebelp.
    ls_item-entry_qnt = 1.
    COLLECT ls_item INTO lt_items.
  ENDLOOP.

  CHECK NOT lt_items IS INITIAL.

  CALL FUNCTION 'ZWM_GOODSMVT_CREATE'
    EXPORTING
      i_code      = lv_gmcode
      i_bwart     = lv_bwart
      it_items    = lt_items
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
  ENDIF.
ENDFORM.                    " F_DATA_SCR0020_UCOMM_SAVEPAL
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0020_UCOMM_REMONTADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM f_data_scr0020_ucomm_remontada  CHANGING cv_subrc.
  DATA: lt_zwm020 TYPE TABLE OF zwm020.

  DATA: ls_zwm020 TYPE zwm020,
        ls_scr002 TYPE ty_scr002.

  CLEAR: cv_subrc.

  CHECK NOT gt_scr002[] IS INITIAL.

  lt_zwm020 = gt_zwm020.

  LOOP AT gt_scr002 INTO ls_scr002.
    READ TABLE lt_zwm020
          INTO ls_zwm020
          WITH KEY p1 = ls_scr002-lenum.

    CHECK sy-subrc EQ 0.
    DELETE lt_zwm020 INDEX sy-tabix.

    ls_zwm020-armazem = gs_xuser-lgnum.
    MODIFY zwm020 FROM ls_zwm020.
  ENDLOOP.

  COMMIT WORK.
ENDFORM.                    " F_DATA_SCR0020_UCOMM_REMONTADA
