*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0137_F01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FIND_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs.

  CLEAR whs.
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    LEAVE TO SCREEN 0.
  ELSE.
    READ TABLE l_user WITH KEY statu = gc_true.  " con_x.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
        IMPORTING
          ret_code       = resposta.

      IF resposta = 'O'.
        LEAVE TO SCREEN 0.
      ENDIF.
    ELSE.
      whs = l_user-lgnum.
      setscreen1 = '0001'.
      setscreen2 = '0002'.
    ENDIF.
  ENDIF.

ENDFORM.                    " FIND_WHS
*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMIZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_customizing .

*  CLEAR: plant, lgort, code, mov.
*
*** Centro
*  PERFORM get_parameter USING whs
*                             'GERAL'
*                             'PLANT'
*                              valor.
*
*  WRITE valor TO plant LEFT-JUSTIFIED.
*
*** Depósito
*  PERFORM get_parameter USING whs
*                             'GERAL'
*                             'LGORT'
*                              valor.
*
*  WRITE valor TO lgort LEFT-JUSTIFIED.
*
*** Código BAPI
*  PERFORM get_parameter USING whs
*                           'MAN_CONSUMOS'
*                           'COD'
*                            valor.
*
*  WRITE valor TO code LEFT-JUSTIFIED.
*
*** Tipo movimento BAPI
*  PERFORM get_parameter USING whs
*                       'MAN_CONSUMOS'
*                       'MOV'
*                        valor.
*
*  WRITE valor TO mov LEFT-JUSTIFIED.
ENDFORM.                    " GET_CUSTOMIZING

*&---------------------------------------------------------------------*
*&      Form  get_parameter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WHS        text
*      -->MODULE     text
*      -->PARAM      text
*      -->VALOR      text
*----------------------------------------------------------------------*
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

*  IF ti_zwm001[] IS INITIAL.
*    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
*      EXPORTING
*        whs       = whs
*      TABLES
*        ti_zwm001 = ti_zwm001.
*  ENDIF.
*
*  CLEAR zwm001.
*  READ TABLE ti_zwm001 WITH KEY      armazem   = whs
*                                     processo  = module
*                                     parametro = param
*                                     BINARY SEARCH.
*  IF sy-subrc = 0.
*    MOVE ti_zwm001 TO zwm001.
*  ENDIF.
*  MOVE zwm001-valor TO valor.

ENDFORM.                    " GET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  CHECK_REFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_refnr.

  DATA: lv_lines TYPE i.
  DATA: lv_subrc TYPE sy-subrc.
  DATA: lv_dif   TYPE menge_d.

  DATA: ls_vbsk  TYPE vbsk.

  DATA: lt_vbuk  TYPE vbuk OCCURS 0 WITH HEADER LINE.

** Validar Grupo
**********************************************************************
  CHECK scr1-refnr IS NOT INITIAL.

  REFRESH gt_zwm069.

  CLEAR: scr1-idx, scr1-tot.

  SELECT SINGLE *
    FROM vbsk INTO ls_vbsk
    WHERE sammg = scr1-refnr.

  IF sy-subrc <> 0.
    text1 = scr1-refnr.

    " Grupo & inválido!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '308'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

    CLEAR scr1.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Obter remessas
**********************************************************************
  SELECT *
    FROM zwm069 INTO TABLE gt_zwm069
    WHERE refnr = scr1-refnr.

  IF sy-subrc <> 0.
    text1 = scr1-refnr.

    " Grupo & sem remessas com paletes para embalamento e saida mercadoria!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '322'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

    CLEAR scr1.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

  IF gt_zwm069[] IS NOT INITIAL.
    SELECT *
      FROM vbuk INTO TABLE lt_vbuk
      FOR ALL ENTRIES IN gt_zwm069
      WHERE vbeln = gt_zwm069-vbeln.
  ENDIF.

  LOOP AT lt_vbuk WHERE kostk <> 'C'.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    text1 = scr1-refnr.
    text2 = lt_vbuk-vbeln.

    " Grupo & - Remessa & sem picking completo
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '327'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    CLEAR scr1.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Bloqueio do Grupo
**********************************************************************
  CONCATENATE 'ZWMREP0137' scr1-refnr INTO gv_key SEPARATED BY '_'.

  PERFORM enqueue USING gv_key CHANGING lv_subrc.
  IF lv_subrc <> 0.

    text1 = sy-msgv1.
    text2 = scr1-refnr.

    " Utilizador & está a processar grupo &.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '323'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    CLEAR scr1-refnr.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

  scr1-idx = 1.

ENDFORM.                    " CHECK_REFNR
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_SCREEN_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_screen_0001.

  DATA: lv_lines  TYPE i.
  DATA: lv_npal   TYPE i.
  DATA: lv_idx    TYPE i.
  DATA: lv_index  TYPE i.
  DATA: lv_dif    TYPE menge_d.

  DATA: lt_zwm069 TYPE zwm069 OCCURS 0 WITH HEADER LINE.

** Obter dados
**********************************************************************
  CLEAR: scr1-vbeln_1, scr1-npal_1, scr1-pk_1, scr1-pa_1, scr1-sm_1,
         scr1-vbeln_2, scr1-npal_2, scr1-pk_2, scr1-pa_2, scr1-sm_2,
         scr1-vbeln_3, scr1-npal_3, scr1-pk_3, scr1-pa_3, scr1-sm_3.

  REFRESH gt_zwm069.

  SELECT *
    FROM zwm069 INTO TABLE gt_zwm069
    WHERE refnr = scr1-refnr.

  lt_zwm069[] = gt_zwm069[].

  SORT lt_zwm069 BY vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_zwm069 COMPARING vbeln.

  DESCRIBE TABLE lt_zwm069 LINES lv_lines.

  lv_dif   = lv_lines / 3.
  lv_dif   = CEIL( lv_dif ).
  scr1-tot = lv_dif.


  lv_idx = scr1-idx - 1.

  lv_idx = lv_idx * 3.

  DO 3 TIMES.

    lv_index = sy-index + lv_idx.

    READ TABLE lt_zwm069 INDEX lv_index.
    CHECK sy-subrc = 0.

    CLEAR lv_npal.

    LOOP AT gt_zwm069 WHERE vbeln = lt_zwm069-vbeln.
      lv_npal = lv_npal + 1.
    ENDLOOP.

    CASE sy-index.
      WHEN 1.
        scr1-vbeln_1 = lt_zwm069-vbeln.
        scr1-npal_1  = lv_npal.
        scr1-pk_1    = lt_zwm069-picking.
        scr1-pa_1    = lt_zwm069-packing.
        scr1-sm_1    = lt_zwm069-goods_movment.

      WHEN 2.
        scr1-vbeln_2 = lt_zwm069-vbeln.
        scr1-npal_2  = lv_npal.
        scr1-pk_2    = lt_zwm069-picking.
        scr1-pa_2    = lt_zwm069-packing.
        scr1-sm_2    = lt_zwm069-goods_movment.

      WHEN 3.
        scr1-vbeln_3 = lt_zwm069-vbeln.
        scr1-npal_3  = lv_npal.
        scr1-pk_3    = lt_zwm069-picking.
        scr1-pa_3    = lt_zwm069-packing.
        scr1-sm_3    = lt_zwm069-goods_movment.
    ENDCASE.

  ENDDO.

ENDFORM.                    " GET_DATA_SCREEN_0001
*&---------------------------------------------------------------------*
*&      Form  EXIT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0001.

  DATA: lv_ret_code TYPE char1.

  CASE sy-ucomm.
    WHEN 'BACK'.

*      IF scr1-vbeln IS NOT INITIAL.
*
*        WRITE scr1-vbeln TO text1 LEFT-JUSTIFIED.
*
*        " Deseja sair da remessa & ?
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = 'ZWMMSG001'
*            message_lang   = sy-langu
*            message_type   = 'W'
*            message_number = '315'
*            message_var1   = text1
*          IMPORTING
*            ret_code       = lv_ret_code.
*
*        CHECK lv_ret_code = 'O'.
*
*
*      ENDIF.

      PERFORM dequeue USING gv_key.

      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.                    " EXIT_0001
*&---------------------------------------------------------------------*
*&      Form  ENQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_KEY  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM enqueue USING    pv_key TYPE keywords
             CHANGING pv_subrc.

  DATA: lv_rc       TYPE c.
  DATA: lv_ok       TYPE c.
  DATA: lv_uname    TYPE uname.

** Criar bloqueio
**********************************************************************
  pv_subrc = 4.

  DO 2 TIMES.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        keyword_       = pv_key
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      CLEAR pv_subrc.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

ENDFORM.                    "enqueue

*&---------------------------------------------------------------------*
*&      Form  DEQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_KEY  text
*----------------------------------------------------------------------*
FORM dequeue USING pv_key TYPE keywords.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      keyword_ = pv_key.

ENDFORM.                    "dequeue
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0001.

  CASE sy-ucomm.
    WHEN 'CLR'.
      PERFORM clear_0001.
    WHEN 'SAVE'.
      PERFORM save.
    WHEN 'PGDN'.
      PERFORM page_down.
    WHEN 'PGUP'.
      PERFORM page_up.
    WHEN OTHERS.
  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.                    " USER_COMMAND_0001
*&---------------------------------------------------------------------*
*&      Form  CLEAR_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_0001.

  DATA: lv_ret_code TYPE char1.
  DATA: lv_refnr    TYPE t311-refnr.

** Clear
**********************************************************************
  CLEAR scr1.


ENDFORM.                    " CLEAR_0001
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save.

  DATA: lv_tabix      LIKE sy-tabix.
  DATA: lv_pal        TYPE i.
  DATA: lv_mode       TYPE char1 VALUE 'N'.
  DATA: lv_data       TYPE char10.

  DATA: lt_zwm069      TYPE zwm069        OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm069_pack TYPE zwm069        OCCURS 0 WITH HEADER LINE.
  DATA: lt_msg         TYPE bdcmsgcoll    OCCURS 0 WITH HEADER LINE.
  DATA: lt_sscc        TYPE zwm_ltap      OCCURS 0 WITH HEADER LINE.

  DATA: lt_messages    TYPE tab_bdcmsgcoll.

** Validar Remessa
**********************************************************************
  CHECK gt_zwm069[] IS NOT INITIAL.

  lt_zwm069[] = gt_zwm069[].

** Validar Picking
  LOOP AT lt_zwm069 WHERE picking IS INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    text1 = lt_zwm069-vbeln.

    " Remessa & sem o picking efetuado!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '324'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Validar embalamento e Saida de Mercadoria
  DELETE lt_zwm069 WHERE packing       IS NOT INITIAL AND
                         goods_movment IS NOT INITIAL.

  IF lt_zwm069[] IS INITIAL.
    text1 = scr1-refnr.

    " Grupo & sem remessas para embalamento e saida de mercadoria
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '325'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Embalamento
**********************************************************************
  lt_zwm069[] = gt_zwm069[].

  SORT lt_zwm069 BY vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_zwm069 COMPARING vbeln.

  LOOP AT lt_zwm069.

    LOOP AT gt_zwm069 WHERE vbeln = lt_zwm069-vbeln.

      lv_tabix = sy-tabix.

      CHECK gt_zwm069-picking IS NOT INITIAL AND
            gt_zwm069-packing IS INITIAL.

      REFRESH: lt_sscc, lt_messages.

      CLEAR lt_sscc.
      lt_sscc-sscc = gt_zwm069-sscc.
      APPEND lt_sscc.

      lt_zwm069_pack = gt_zwm069.

      CALL FUNCTION 'ZWM_DELIVERY_UPDATE'
        EXPORTING
          lgnum      = whs
          remessa    = lt_zwm069-vbeln
        TABLES
          it_sscc    = lt_sscc
          return_msg = lt_messages
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      IF sy-subrc <> 0.
        CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
          EXPORTING
            it_messages = lt_messages.

        LEAVE TO SCREEN setscreen1.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

      GET TIME.
      lt_zwm069_pack-packing = 'X'.
      lt_zwm069_pack-pa_user = sy-uname.
      lt_zwm069_pack-pa_time = sy-uzeit.
      lt_zwm069_pack-pa_date = sy-datum.

      MODIFY zwm069 FROM lt_zwm069_pack.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.

      gt_zwm069 = lt_zwm069_pack.

      MODIFY gt_zwm069 INDEX lv_tabix.
    ENDLOOP.

  ENDLOOP.

** Saida de mercadoria
**********************************************************************
  LOOP AT lt_zwm069.

    CHECK lt_zwm069-goods_movment IS INITIAL.

    REFRESH gt_bdcdata.

    PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
    PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
    PERFORM bdc_field    USING 'LIKP-VBELN'          lt_zwm069-vbeln.

    WRITE sy-datum TO lv_data.

    PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '=WABU_T'.
    PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-WADAT_IST'.
    PERFORM bdc_field    USING 'LIKP-WADAT_IST'       lv_data.

    CLEAR: lt_msg, lt_msg[].

    REFRESH: lt_messages.

    CALL TRANSACTION 'VL02N' USING gt_bdcdata
                     MODE   lv_mode
                     UPDATE 'S'
                     MESSAGES INTO lt_msg.

    READ TABLE lt_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      APPEND lt_msg TO lt_messages.

      CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
        EXPORTING
          it_messages = lt_messages.

      LEAVE TO SCREEN setscreen1.

    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.

    LOOP AT gt_zwm069 WHERE vbeln = lt_zwm069-vbeln.

      GET TIME.
      gt_zwm069-goods_movment = 'X'.
      gt_zwm069-gm_user       = sy-uname.
      gt_zwm069-gm_time       = sy-uzeit.
      gt_zwm069-gm_date       = sy-datum.

      MODIFY zwm069 FROM gt_zwm069.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

** Mensagem de sucesso
**********************************************************************

  " Embalamento e saida de mercadoria efetuados com sucesso!
  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '326'
    IMPORTING
      ret_code       = resposta.

  PERFORM dequeue USING gv_key.

  CLEAR scr1.

ENDFORM.                    " SAVE
*&---------------------------------------------------------------------*
*&      Form  PAGE_DOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM page_down.

  CHECK scr1-idx > 1.

  scr1-idx = scr1-idx - 1.

ENDFORM.                    " PAGE_DOWN
*&---------------------------------------------------------------------*
*&      Form  PAGE_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM page_up.

  CHECK scr1-idx < scr1-tot.

  scr1-idx = scr1-idx + 1.

ENDFORM.                    " PAGE_UP
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR gt_bdcdata.
  gt_bdcdata-program  = program.
  gt_bdcdata-dynpro   = dynpro.
  gt_bdcdata-dynbegin = 'X'.
  APPEND gt_bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR gt_bdcdata.
  gt_bdcdata-fnam = fnam.
  gt_bdcdata-fval = fval.
  APPEND gt_bdcdata.
ENDFORM.                    "BDC_FIELD
