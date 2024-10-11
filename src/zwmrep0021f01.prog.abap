*&---------------------------------------------------------------------*
*&  Include           ZWMREP0021F01                                    *
*&---------------------------------------------------------------------*

*{   INSERT         DEVK957193                                        1
*&---------------------------------------------------------------------*
*&      Form  CREATE_OT_EXP_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_ot_exp_picking.

  DATA i_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.
  DATA l_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.

  DATA: lv_lznum     TYPE lvs_lznum.
  DATA: lv_tanum     TYPE tanum.
  DATA: lv_remessa   TYPE vbeln.
  DATA: lv_type_d    TYPE lgtyp.
  DATA: lv_etiq      TYPE flag.
  DATA: lv_enfa      TYPE flag.
  DATA: lv_ret_code  TYPE flag.
  DATA: ls_ltak      TYPE ltak.
  DATA: ls_lqua      TYPE lqua.
  DATA: ls_ltap_pck  TYPE ltap.
  DATA: ls_msg       TYPE bdcmsgcoll.
  DATA: ls_return    TYPE bdcmsgcoll.

  DATA: lt_ltap      TYPE TABLE OF ltap      WITH HEADER LINE.
  DATA: lt_ltap_conf TYPE TABLE OF ltap_conf WITH HEADER LINE.

** Validar SSCC
**********************************************************************
  CHECK NOT sscc IS INITIAL.

** Armazém Automático (WCS)
  IF gv_aut_wcs IS NOT INITIAL.
    CHECK NOT lgpla_in IS INITIAL.
  ENDIF.

  CLEAR i_sscc.
  REFRESH i_sscc.

  LOOP AT i_zwm026.
    CLEAR: ls_ltap_pck.
    SELECT SINGLE * FROM ltap
                    INTO ls_ltap_pck
                    WHERE lgnum = lgnum AND
                          tanum = i_zwm026-to_number.


    i_sscc-material      = i_zwm026-material.
    i_sscc-quantidade    = i_zwm026-quantidade.
    i_sscc-uni           = i_zwm026-unidade.
    i_sscc-lote_producao = i_zwm026-lote.
    i_sscc-werks         = ls_ltap_pck-werks.
    i_sscc-lgort         = ls_ltap_pck-lgort.

    APPEND i_sscc.
    CLEAR i_sscc.
  ENDLOOP.

  CLEAR valor.
  CLEAR: mov, plant, s_loc, lagp.

  CLEAR l_lagp.
  REFRESH l_lagp.

  CLEAR zwm026.
  SELECT SINGLE *
      FROM zwm026
          WHERE armazem = lgnum AND
                sscc = sscc.

** Posição Pulmão Pré Picking
**********************************************************************
  IF gv_aut_wcs IS INITIAL.

    CLEAR zwm028.

    SELECT SINGLE *
        FROM zwm028
            WHERE lgnum = lgnum AND
                  refnr = zwm026-grupo AND
                  remessa = '          '.

    IF NOT zwm028-st_ppk IS INITIAL.

      SELECT * INTO TABLE l_lagp
                FROM lagp
                    WHERE lgnum = lgnum AND
                          lgtyp = 'PPK' AND
                          skzue <> 'X' AND
                          skzsi <> 'X' AND
                          anzqu = '0'.
      IF l_lagp[] IS INITIAL.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '207'
            message_var1   = text1.
        CLEAR: ok_code_0001,  ok_code_0002, sscc, lgpla_in, cursorfield.
        EXIT.
      ELSE.
        SORT l_lagp BY sorlp.
        READ TABLE l_lagp INDEX 1.
      ENDIF.

    ELSE.
      CLEAR l_lagp-lgpla.
    ENDIF.

** Posição WCS Automático
**********************************************************************
  ELSE.
    CLEAR lv_remessa.

    SELECT SINGLE id_servisan FROM zwm040
      INTO lv_remessa
      WHERE lgnum   = zwm026-armazem
        AND refnr   = zwm026-grupo
        AND remessa = zwm026-remessa.

    IF lv_remessa IS INITIAL.
      lv_remessa = zwm026-remessa.
    ENDIF.

    CLEAR: ls_zwm028.

    SELECT SINGLE * FROM zwm028
      INTO ls_zwm028
      WHERE lgnum   = zwm026-armazem
        AND refnr   = zwm026-grupo
        AND remessa = lv_remessa.

** Mesa Manual - Chamada de Empilhador
    IF lgpla_in = lgpla_man.

      lv_type_d = 'BPK'. " Buffer Picking

      IF ls_zwm028-st_pul <> 'BPK'.
        IF ls_zwm028-zlock <> '1' AND
         ( ( ls_zwm028-st_pul IS NOT INITIAL AND ls_zwm028-st_pul <> 'AUT' )
          OR ls_zwm028-st_dck IS NOT INITIAL ).

          lv_type_d    = '933'.
          l_lagp-lgpla = 'DESTINO'.
        ENDIF.
      ENDIF.

      IF lv_type_d = 'BPK'.
        LOOP AT i_sscc.
          i_sscc-sscc = sscc.

          IF zwm026-pallet_type = 'M'.
            i_sscc-tipo_su = 'P21'.
          ELSE.
            i_sscc-tipo_su = 'P20'.
          ENDIF.

          MODIFY i_sscc INDEX sy-tabix.
        ENDLOOP.
      ENDIF.

** Mesa Automático - Entrada WCS Automático
    ELSEIF lgpla_in = lgpla_aut.

      LOOP AT i_sscc.
        i_sscc-sscc = sscc.

        IF zwm026-pallet_type = 'M'.
          i_sscc-tipo_su = 'P21'.
        ELSE.
          i_sscc-tipo_su = 'P20'.
        ENDIF.

        MODIFY i_sscc INDEX sy-tabix.
      ENDLOOP.
    ENDIF.
  ENDIF.

** Criar OT
**********************************************************************
  IF gv_aut_wcs IS NOT INITIAL.

    " Mesa do Automático
    IF lgpla_in = lgpla_aut.
      SELECT SINGLE valor INTO valor
        FROM zwm001
            WHERE armazem   = lgnum AND
                  processo  = 'SAIDA_ARMAZEM_PPK' AND
                  parametro = 'MOV_WM_AUT_WCS'.

      " Mesa manual
    ELSEIF lgpla_in = lgpla_man.
      SELECT SINGLE valor INTO valor
       FROM zwm001
           WHERE armazem   = lgnum AND
                 processo  = 'SAIDA_ARMAZEM_PPK' AND
                 parametro = 'MOV_WM_MAN'.
    ENDIF.

  ELSE.
    SELECT SINGLE valor INTO valor
   FROM zwm001
       WHERE armazem   = lgnum AND
             processo  = 'SAIDA_ARMAZEM_PPK' AND
             parametro = 'MOV_WM'.
  ENDIF.

  MOVE valor TO mov.

** Centro
  CLEAR valor.
  SELECT SINGLE valor INTO valor
     FROM zwm001
         WHERE armazem = lgnum AND
               processo = 'GERAL' AND
               parametro = 'PLANT'.
  MOVE valor TO plant.

** Depósito
  CLEAR valor.
  SELECT SINGLE valor INTO valor
   FROM zwm001
       WHERE armazem = lgnum AND
             processo = 'GERAL' AND
             parametro = 'LGORT'.
  MOVE valor TO s_loc.

  CLEAR valor.

** Validar se Palete foi rejeitada
  lv_etiq = 'X'.
  lv_enfa = 'X'.

  CLEAR lv_tanum.

  SELECT SINGLE *
     FROM lqua INTO ls_lqua
      WHERE lgnum = lgnum
      AND   lgtyp = 'REJ'
      AND   lgpla = 'EXP2_REJ'
      AND   lenum = sscc.

  IF sy-subrc = 0.

    CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
      EXPORTING
        i_lenum       = sscc
        i_bwlvs       = '999'
*       i_lznum       = ls_ltak-lznum
        i_letyp       = ls_lqua-letyp
        i_nltyp       = '932'
        i_nlpla       = 'ORIGEM'
        i_squit       = 'X'
        i_commit_work = 'X'
      IMPORTING
        e_tanum       = lv_tanum
      EXCEPTIONS
        error_message = 99.

    IF sy-subrc <> 0 OR lv_tanum IS INITIAL.

      ls_return-msgid  = sy-msgid.
      ls_return-msgtyp = sy-msgty.
      ls_return-msgnr  = sy-msgno.
      ls_return-msgv1  = sy-msgv1.
      ls_return-msgv2  = sy-msgv2.
      ls_return-msgv3  = sy-msgv3.
      ls_return-msgv4  = sy-msgv4.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_return-msgid
          message_lang   = sy-langu
          message_type   = ls_return-msgtyp
          message_number = ls_return-msgnr
          message_var1   = ls_return-msgv1
          message_var2   = ls_return-msgv2
          message_var3   = ls_return-msgv3
          message_var4   = ls_return-msgv4.

      CLEAR: ok_code_0001, ok_code_0002, sscc, lgpla_in, cursorfield.
      EXIT.
    ENDIF.

    DO 10 TIMES.
      SELECT SINGLE *
        FROM ltak INTO ls_ltak
        WHERE lgnum = lgnum
        AND   tanum = lv_tanum.

      IF ls_ltak-kquit IS NOT INITIAL.
        EXIT.
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.

** Mesa Automático - Entrada WCS Automático
    IF lgpla_in = lgpla_aut.

      CLEAR: lv_etiq, lv_enfa.

      CLEAR lv_ret_code.

      " Deseja ETIQUETAR a palete no WCS ?
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'W'
          message_number = '374'
          message_lang   = sy-langu
        IMPORTING
          ret_code       = lv_ret_code.

      IF lv_ret_code = 'O'.
        lv_etiq = 'X'.
      ENDIF.

      CLEAR lv_ret_code.

      " Deseja ENVOLVER a palete no WCS ?
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'W'
          message_number = '375'
          message_lang   = sy-langu
        IMPORTING
          ret_code       = lv_ret_code.

      IF lv_ret_code = 'O'.
        lv_enfa = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  " Entrada Mesa WCS - Automático
  IF gv_aut_wcs IS NOT INITIAL AND lgpla_in = lgpla_aut.

    lv_lznum(1) = '@'.

    " Etiqueta
    IF lv_etiq = 'X'.
      lv_lznum+1(1) = '1'.
    ELSE.
      lv_lznum+1(1) = '0'.
    ENDIF.

    " Envolver
    IF lv_enfa = 'X'.
      lv_lznum+2(1) = '1'.
    ELSE.
      lv_lznum+2(1) = '0'.
    ENDIF.

    " Processo Manual- Palete já foi etiquetada e envolvida
    IF sscc_ant IS NOT INITIAL.
      lv_lznum = '@00'.
    ENDIF.

    CONCATENATE zwm026-grupo lv_lznum INTO lv_lznum SEPARATED BY '#'.

  ELSE.
    lv_lznum = sscc.
  ENDIF.

  CLEAR lv_tanum.

  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse      = lgnum
      mov_type       = mov
      plant          = plant
      s_loc          = s_loc
      st_type_d      = lv_type_d
      bin_destino    = l_lagp-lgpla
      certificado    = 'PKF 00-001'
      sscc_adicional = lv_lznum
    IMPORTING
      to             = lv_tanum
    TABLES
      return_msg     = return_msg
      sscc           = i_sscc
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.

  IF sy-subrc <> 0 OR lv_tanum IS INITIAL.
    READ TABLE return_msg INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1.

      CLEAR: ok_code_0001, ok_code_0002, sscc, lgpla_in, cursorfield.
    ENDIF.

    EXIT.
  ENDIF.

** Confirmar - Entrada na mesa WCS do Automático
**********************************************************************
  CLEAR ls_ltak.

  IF gv_aut_wcs IS NOT INITIAL AND lgpla_in = lgpla_aut.

    DO 10 TIMES.
      SELECT SINGLE *
        FROM ltak INTO ls_ltak
        WHERE lgnum = lgnum
        AND   tanum = lv_tanum.

      IF sy-subrc = 0.
        EXIT.
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.

    " Registar ponto de entrega da OT
    CALL FUNCTION 'ZWM_UPDATE_POS_ENT_WCS'
      EXPORTING
        i_lgnum = lgnum
        i_tanum = lv_tanum
        i_su    = sscc
        i_lgpla = lgpla_in(10)
        i_etiq  = lv_etiq
        i_enfa  = lv_enfa.

    REFRESH: lt_ltap, lt_ltap_conf.

    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = lgnum
      AND   tanum = lv_tanum.

    LOOP AT lt_ltap.
      CLEAR lt_ltap_conf.
      lt_ltap_conf-tanum = lt_ltap-tanum.
      lt_ltap_conf-tapos = lt_ltap-tapos.
      lt_ltap_conf-altme = lt_ltap-meins.
      lt_ltap_conf-nista = lt_ltap-vsolm.
      APPEND lt_ltap_conf.
    ENDLOOP.

    CALL FUNCTION 'L_TO_CONFIRM'
      EXPORTING
        i_lgnum       = lgnum
        i_tanum       = lv_tanum
      TABLES
        t_ltap_conf   = lt_ltap_conf
      EXCEPTIONS
        error_message = 99.

    IF sy-subrc <> 0.
      ls_msg-msgid  = sy-msgid.
      ls_msg-msgtyp = sy-msgty.
      ls_msg-msgnr  = sy-msgno.
      ls_msg-msgv1  = sy-msgv1.
      ls_msg-msgv2  = sy-msgv2.
      ls_msg-msgv3  = sy-msgv3.
      ls_msg-msgv4  = sy-msgv4.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_msg-msgid
          message_lang   = sy-langu
          message_type   = ls_msg-msgtyp
          message_number = ls_msg-msgnr
          message_var1   = ls_msg-msgv1
          message_var2   = ls_msg-msgv2
          message_var3   = ls_msg-msgv3
          message_var4   = ls_msg-msgv4.
    ENDIF.

  ENDIF.

  PERFORM clear_field1.
*  CLEAR: cursorfield, sscc, lgpla_in, lgpla_man, lgpla_aut.
  SET SCREEN '0001'.LEAVE SCREEN.

ENDFORM.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  CLEAR_FIELD1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_field1.

  PERFORM dequeue.

  CLEAR: lgpla_in, lgpla_man, lgpla_aut, carga_txt1, carga_txt2.
  CLEAR: ok_code_0001, sscc, sscc_ant, cursorfield, gv_sscc.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ENQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue CHANGING pv_subrc.

  DATA: lv_key      TYPE keyword-keyword.
  DATA: lv_uname    TYPE uname.

** Criar bloqueio
**********************************************************************
  CLEAR pv_subrc.

  CHECK gv_aut_wcs = 'X'.

  pv_subrc = 4.

  gv_sscc = sscc.

  CONCATENATE sy-tcode lgnum gv_sscc INTO lv_key SEPARATED BY '_'.

  DO 2 TIMES.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        keyword_       = lv_key
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      pv_subrc = 0.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dequeue.

  DATA: lv_key      TYPE keyword-keyword.
  DATA: lv_uname    TYPE uname.

** Retirar bloqueio
**********************************************************************
  CHECK gv_aut_wcs = 'X' AND gv_sscc IS NOT INITIAL.

  CONCATENATE sy-tcode lgnum gv_sscc INTO lv_key SEPARATED BY '_'.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      keyword_ = lv_key.


ENDFORM.
