FUNCTION zwm_confirm_idoc_to_wcs .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LTAK-LGNUM
*"     VALUE(I_TANUM) TYPE  LTAK-TANUM
*"     VALUE(I_FLG_CONF_SU) TYPE  FLAG OPTIONAL
*"  CHANGING
*"     VALUE(C_QUKNZ) TYPE  RL03TQUKNZ
*"     VALUE(CT_LTAP_CONF) TYPE  T_LTAP_CONF
*"     VALUE(CT_IDOC_DATA) TYPE  EDIDD_TT
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  CONSTANTS: lc_object     TYPE balhdr-object    VALUE 'ZWCS'.
  CONSTANTS: lc_subobject2 TYPE balhdr-subobject VALUE 'ZWCS_02'.
  CONSTANTS: lc_subobject3 TYPE balhdr-subobject VALUE 'ZWCS_03'.
  CONSTANTS: lc_subobject4 TYPE balhdr-subobject VALUE 'ZWCS_04'.
  CONSTANTS: lc_subobject5 TYPE balhdr-subobject VALUE 'ZWCS_05'.
  CONSTANTS: lc_subobject6 TYPE balhdr-subobject VALUE 'ZWCS_08'.
  CONSTANTS: lc_subobject7 TYPE balhdr-subobject VALUE 'ZWCS_12'.
  CONSTANTS: lc_subobject8 TYPE balhdr-subobject VALUE 'ZWCS_14'.

  DATA: lv_retorno       TYPE numc2,
        lv_to            TYPE tanum,
        lv_msg           TYPE zwm_msg,
        lv_mesa          TYPE char14,
        lv_linha         TYPE fevor,
        lv_lznum         TYPE lznum,
        lv_lgpla         TYPE lgpla,
        lv_exidv         TYPE exidv,
        lv_lgtyp         TYPE lgtyp,
        lv_tabix         TYPE sy-tabix,
        ls_lqua          TYPE lqua,
        lv_quknz         TYPE rl03t-quknz,
        lv_bwlvs         TYPE bwlvs,
        lv_bwlvs_bpe     TYPE bwlvs,
        lv_bwlvs_pick    TYPE bwlvs,
        lv_bwlvs_dita2   TYPE bwlvs,
        lv_bwlvs_man_bpe TYPE bwlvs,
        lv_subobject     TYPE balhdr-subobject,
        lv_extnumber     TYPE balnrext,
        lv_lgpla_emerg   TYPE lgpla,
        lv_bin_output    TYPE char14,
        lv_tipo_palete   TYPE lvs_letyp,
        lv_su            TYPE lenum,
        lv_su2           TYPE lenum,
        lv_add_su2       TYPE flag,
        lv_queue_tri_g   TYPE lrf_queue,
        lv_lgtyp_epe     TYPE lgtyp.

  DATA: ls_ltak      TYPE ltak,
        ls_ltap_conf TYPE ltap_conf,
        ls_idoc_data TYPE edidd,
        ls_e1ltcoi   TYPE e1ltcoi,
        ls_zwm076    TYPE zwm076,
        ls_zwm077    TYPE zwm077,
        ls_zwm028    TYPE zwm028,
        ls_zwm020    TYPE zwm020,
        ls_zwm026    TYPE zwm026,
        ls_lein      TYPE lein.

  DATA: lt_ltap       TYPE TABLE OF ltap       WITH HEADER LINE,
        lt_ltap_epe   TYPE TABLE OF ltap       WITH HEADER LINE,
        lt_lqua       TYPE TABLE OF lqua       WITH HEADER LINE,
        lt_ltap_conf  TYPE TABLE OF ltap_conf  WITH HEADER LINE,
        lt_return     TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
        lt_lein       TYPE TABLE OF lein       WITH HEADER LINE,
        lt_zwm077     TYPE TABLE OF zwm077     WITH HEADER LINE,
        lt_ltap_creat TYPE TABLE OF ltap_creat WITH HEADER LINE,
        lt_ltap_rem   TYPE TABLE OF ltap       WITH HEADER LINE.

** Validar OT
**********************************************************************
*  WHILE 1 > 0.
*
*  ENDWHILE.

  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum = i_lgnum
    AND   tanum = i_tanum.

  IF ct_ltap_conf[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN ct_ltap_conf
      WHERE lgnum = i_lgnum
      AND   tanum = ct_ltap_conf-tanum
      AND   tapos = ct_ltap_conf-tapos.

*    SELECT *
*      FROM lqua INTO TABLE lt_lqua
*      FOR ALL ENTRIES IN ct_ltap_conf
*      WHERE lgnum = i_lgnum
*      AND   lenum = ct_ltap_conf-lenum.
  ENDIF.

  SORT lt_ltap BY tanum tapos.

  READ TABLE lt_ltap INDEX 1.
  CHECK sy-subrc = 0.

** Parametros
**********************************************************************
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'WCS'
      i_parametro = 'SAIDA_EMERG'
    IMPORTING
      e_valor     = lv_lgpla_emerg
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'OT_DUMMY'
      i_parametro = 'MOV_WM_WCS'
    IMPORTING
      e_valor     = lv_bwlvs
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_ABAST_BPE'
    IMPORTING
      e_valor     = lv_bwlvs_bpe
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_MAN_PICK'
    IMPORTING
      e_valor     = lv_bwlvs_pick
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_AUT_DITA2'
    IMPORTING
      e_valor     = lv_bwlvs_dita2
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_MAN_BPE'
    IMPORTING
      e_valor     = lv_bwlvs_man_bpe
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'TIPO_DEP_ESTACAO'
    IMPORTING
      e_valor     = lv_lgtyp_epe
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

*  CALL FUNCTION 'ZWM_GET_PARAMETER'
*    EXPORTING
*      i_lgnum     = i_lgnum
*      i_processo  = 'WCS'
*      i_parametro = 'MOV_WM_SAI_PICK'
*    IMPORTING
*      e_valor     = lv_bwlvs_pick
*    EXCEPTIONS
*      error       = 1
*      OTHERS      = 2.

** Validar Palete Picking
  READ TABLE lt_ltap INDEX 1.
  IF sy-subrc = 0 AND lt_ltap-vlenr IS NOT INITIAL.
    SELECT SINGLE *
      FROM zwm026 INTO ls_zwm026
      WHERE armazem = lt_ltap-lgnum
      AND   sscc    = lt_ltap-vlenr.

    IF sy-subrc = 0.
      ls_ltak-benum = ls_zwm026-remessa.
    ENDIF.
  ENDIF.

** Validar OT Expedição
  CALL FUNCTION 'ZWM_CHECK_TO_EXP'
    EXPORTING
      i_lgnum   = ls_ltak-lgnum
      i_refnr   = ls_ltak-refnr
      i_vbeln   = ls_ltak-benum
    IMPORTING
      cs_zwm028 = ls_zwm028.

** Log
**********************************************************************
  CLEAR lt_return.
  lt_return-msgtyp = 'S'.
  lt_return-msgid  = 'ZWMMSG001'.
  lt_return-msgnr  = '000'.
  lt_return-msgv1  = 'OT'.
  lt_return-msgv2  = lt_ltap-tanum.
  APPEND lt_return.

  IF c_quknz = '1'.
    CLEAR lt_return.
    lt_return-msgtyp = 'S'.
    lt_return-msgid  = 'ZWMMSG001'.
    lt_return-msgnr  = '000'.
    lt_return-msgv1  = 'PICK'.
    APPEND lt_return.

  ELSEIF c_quknz = '2'.
    CLEAR lt_return.
    lt_return-msgtyp = 'S'.
    lt_return-msgid  = 'ZWMMSG001'.
    lt_return-msgnr  = '000'.
    lt_return-msgv1  = 'TRANSFER'.
    APPEND lt_return.
  ENDIF.

** Obter SSCC
  lv_exidv = lt_ltap-ablad.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_exidv
    IMPORTING
      output = lv_exidv.

  IF lv_exidv IS NOT INITIAL.
    CLEAR lt_return.
    lt_return-msgtyp = 'S'.
    lt_return-msgid  = 'ZWMMSG001'.
    lt_return-msgnr  = '000'.
    lt_return-msgv1  = 'SSCC'.
    lt_return-msgv2  = lv_exidv.
    APPEND lt_return.
  ENDIF.

  LOOP AT ct_ltap_conf INTO ls_ltap_conf WHERE lenum IS NOT INITIAL.
    CLEAR lt_return.
    lt_return-msgtyp = 'S'.
    lt_return-msgid  = 'ZWMMSG001'.
    lt_return-msgnr  = '000'.
    lt_return-msgv1  = 'SSCC'.
    lt_return-msgv2  = ls_ltap_conf-lenum.
    APPEND lt_return.
  ENDLOOP.

  LOOP AT lt_ltap WHERE vlenr IS NOT INITIAL.
    CLEAR lt_return.
    lt_return-msgtyp = 'S'.
    lt_return-msgid  = 'ZWMMSG001'.
    lt_return-msgnr  = '000'.
    lt_return-msgv1  = 'SSCC'.
    lt_return-msgv2  = lt_ltap-vlenr.
    APPEND lt_return.
  ENDLOOP.

** Confirmação de Entradas em armazém automático (AUT)
**********************************************************************
  IF lt_ltap-nltyp = 'AUT' AND lt_ltap-nlpla = 'AUT'.

    lv_subobject = lc_subobject5.

    " Limpar confirmação separada
    CLEAR c_quknz.

    " Limpar SU
    LOOP AT ct_ltap_conf INTO ls_ltap_conf.
      CLEAR: ls_ltap_conf-lenum, ls_ltap_conf-nlpla.

      MODIFY ct_ltap_conf FROM ls_ltap_conf INDEX sy-tabix.
    ENDLOOP.

** Confirmação de Entrada de pilha de paletes vazias no BPE
**********************************************************************
  ELSEIF lt_ltap-nltyp = 'BPE' AND lt_ltap-nlpla = 'MAN_BUF21'.

    lv_subobject = lc_subobject7.

    IF c_quknz = '2'.

      " Limpar SU
      LOOP AT ct_ltap_conf INTO ls_ltap_conf.
        CLEAR: ls_ltap_conf-lenum, ls_ltap_conf-nlpla.

        MODIFY ct_ltap_conf FROM ls_ltap_conf INDEX sy-tabix.
      ENDLOOP.

** Fazer Pick
      CALL FUNCTION 'L_TO_CONFIRM'
        EXPORTING
          i_lgnum       = ls_ltak-lgnum
          i_tanum       = ls_ltak-tanum
          i_quknz       = '1'
          i_subst       = 'X'
          i_commit_work = 'X'
        TABLES
          t_ltap_conf   = ct_ltap_conf
        EXCEPTIONS
          error_message = 99.

      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

** Confirmação de Transferências BPE para estações EPE
**********************************************************************
  ELSEIF lt_ltap-vltyp = 'BPE' AND lt_ltap-nltyp = 'EPE'.

    lv_subobject = lc_subobject5.

    " Limpar confirmação separada
    CLEAR c_quknz.

** Confirmação SU de bloco
    LOOP AT ct_ltap_conf INTO ls_ltap_conf WHERE lenum IS NOT INITIAL.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      SELECT *
        FROM lqua INTO TABLE lt_lqua
        FOR ALL ENTRIES IN ct_ltap_conf
        WHERE lgnum = i_lgnum
        AND   lenum = ct_ltap_conf-lenum.
    ENDIF.

    LOOP AT ct_ltap_conf INTO ls_ltap_conf WHERE lenum IS NOT INITIAL.

      lv_tabix = sy-tabix.

      READ TABLE lt_ltap WITH KEY tanum = ls_ltap_conf-tanum
                                  tapos = ls_ltap_conf-tapos.
      CHECK sy-subrc = 0.

      IF lt_ltap-charg IS NOT INITIAL.
        READ TABLE lt_lqua WITH KEY lenum = ls_ltap_conf-lenum
                                    matnr = lt_ltap-matnr
                                    charg = lt_ltap-charg.
      ELSE.
        READ TABLE lt_lqua WITH KEY lenum = ls_ltap_conf-lenum
                                    matnr = lt_ltap-matnr.
      ENDIF.

      CHECK sy-subrc = 0.

*        CLEAR ls_ltap_conf-squit.

      IF lt_lqua-meins <> lt_ltap-altme.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = lt_ltap-matnr
            i_in_me              = lt_lqua-meins
            i_out_me             = lt_ltap-altme
            i_menge              = lt_lqua-verme
          IMPORTING
            e_menge              = lt_lqua-verme
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
      ENDIF.

      ls_ltap_conf-pickm = lt_lqua-verme.
      ls_ltap_conf-altme = lt_ltap-altme.

      CLEAR: ls_ltap_conf-nlpla, ls_ltap_conf-squit.

      MODIFY ct_ltap_conf FROM ls_ltap_conf INDEX lv_tabix.
    ENDLOOP.

** Confirmação de OT de Aviso de Entrada de Produção
**********************************************************************
  ELSEIF ls_ltak-bwlvs = lv_bwlvs AND lt_ltap-vltyp = 'CHD'.

    lv_subobject = lc_subobject2.

** Transfer
    IF c_quknz = '2'.

      " Dados IDOC
      LOOP AT ct_idoc_data INTO ls_idoc_data WHERE segnam = 'E1LTCOI'.
        MOVE ls_idoc_data-sdata TO ls_e1ltcoi.

        " Confirmação em posição de destino diferente
        IF ls_e1ltcoi-nlpla <> lt_ltap-nlpla.
          lv_lgpla         = ls_e1ltcoi-nlpla.
        ELSE.
          lv_lgpla = lt_ltap-nlpla.
        ENDIF.

        CLEAR ls_e1ltcoi-nlpla.

        MOVE ls_e1ltcoi TO ls_idoc_data-sdata.

        MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
      ENDLOOP.

      LOOP AT ct_ltap_conf INTO ls_ltap_conf.

        CLEAR ls_ltap_conf-nlpla.

        MODIFY ct_ltap_conf FROM ls_ltap_conf INDEX sy-tabix.
      ENDLOOP.

      " Mesa
      CONCATENATE 'PRO' lv_lgpla INTO lv_mesa SEPARATED BY space.

      lv_linha = ls_ltak-lznum.

** Saída de emergência - Rejeição de palete
      IF lv_lgpla = lv_lgpla_emerg.
        CLEAR lt_return.
        lt_return-msgtyp = 'S'.
        lt_return-msgid  = 'ZWMMSG001'.
        lt_return-msgnr  = '000'.
        lt_return-msgv1  = 'Saída de emergência'.
        lt_return-msgv2  = lv_lgpla.
        APPEND lt_return.

        lv_lznum = lv_exidv.

        " Criar nova OT de chamada de entrada de produção
        CALL FUNCTION 'ZWM_CREATE_TO_CHD_WCS_IN'
          EXPORTING
            i_lgnum  = lt_ltap-lgnum
            i_lznum  = lv_lznum
            i_linha  = lv_linha
          TABLES
            t_return = lt_return
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.

        IF sy-subrc <> 0.
          CLEAR lt_return.
          lt_return-msgtyp = 'E'.
          lt_return-msgid  = 'ZWMMSG001'.
          lt_return-msgnr  = '000'.
          lt_return-msgv1  = 'Erro criação OT chamada'.
          lt_return-msgv2  =  lv_exidv.
          APPEND lt_return.
        ENDIF.

** Ponto de entrega
      ELSE.
        CLEAR lt_return.
        lt_return-msgtyp = 'S'.
        lt_return-msgid  = 'ZWMMSG001'.
        lt_return-msgnr  = '000'.
        lt_return-msgv1  = 'Ponto Entrega'.
        lt_return-msgv2  = lv_lgpla.
        APPEND lt_return.

        CALL FUNCTION 'ZWM_RFC_GET_MESA'
          EXPORTING
            i_sscc  = lv_exidv
            i_mesa  = lv_mesa
          IMPORTING
            retorno = lv_retorno.

        " Dar entrada de Palete
*        lv_retorno = '01'.

        IF lv_retorno = '00'.
          CALL FUNCTION 'ZWM_RFC_GET_TO'
            EXPORTING
              i_sscc  = lv_exidv
              mesa    = lv_mesa
              i_linha = lv_linha
            IMPORTING
              e_to    = lv_to
              retorno = lv_retorno
              msg     = lv_msg.

          IF lv_retorno <> '00'.
            CLEAR lt_return.
            lt_return-msgtyp = 'E'.
            lt_return-msgid  = 'ZWMMSG001'.
            lt_return-msgnr  = '000'.
            lt_return-msgv1  = lv_msg(100).
            APPEND lt_return.
          ENDIF.

        ELSE.
          CLEAR lt_return.
          lt_return-msgtyp = 'E'.
          lt_return-msgid  = 'ZWMMSG001'.
          lt_return-msgnr  = '000'.
          lt_return-msgv1  = 'Erro a validar mesa entrada'.
          APPEND lt_return.
        ENDIF.

** OT arrumação
        IF lv_to IS NOT INITIAL.

          CLEAR lt_return.
          lt_return-msgtyp = 'S'.
          lt_return-msgid  = 'ZWMMSG001'.
          lt_return-msgnr  = '000'.
          lt_return-msgv1  = 'OT Arrumação'.
          lt_return-msgv2  = lv_to.
          APPEND lt_return.

          " Mesa entrega de Saida convencional
          IF lv_lgpla <> 'AUT'.

            GET TIME.

            CLEAR ls_zwm077.
            ls_zwm077-lgnum = i_lgnum.
            ls_zwm077-tanum = lv_to.

            READ TABLE ct_idoc_data INDEX 1 INTO ls_idoc_data.
            IF sy-subrc = 0.
              ls_zwm077-docnum = ls_idoc_data-docnum.
            ENDIF.
            ls_zwm077-lgpla = lv_lgpla.
            ls_zwm077-rdatu = sy-datum.
            ls_zwm077-rzeit = sy-uzeit.
            ls_zwm077-rname = sy-uname.
            ls_zwm077-exidv = lv_exidv.

            APPEND ls_zwm077 TO lt_zwm077.

            IF lt_zwm077[] IS NOT INITIAL.
              MODIFY zwm077 FROM TABLE lt_zwm077.
              IF sy-subrc = 0.
                COMMIT WORK AND WAIT.
              ENDIF.
            ENDIF.

            " Entrada no Automático
          ELSE.

            DO 5 TIMES.

              " Validar se criou e confirmou OT do EAU -> AUT
              " No Exit da confirmação da OT
              SELECT SINGLE *
                FROM lqua INTO ls_lqua
                WHERE lgnum = lt_ltap-lgnum
                AND   lgtyp = 'AUT'
                AND   lgpla = 'AUT'
                AND   lenum = lv_exidv.
              IF sy-subrc = 0.
                EXIT.
              ENDIF.
              WAIT UP TO 1 SECONDS.
            ENDDO.

            IF ls_lqua IS INITIAL.

              CLEAR lt_return.
              lt_return-msgtyp = 'E'.
              lt_return-msgid  = 'ZWMMSG001'.
              lt_return-msgnr  = '000'.
              lt_return-msgv1  = 'OT EAU -> AUT'.
              lt_return-msgv2  = 'Não foi criada e confirmada'.
              APPEND lt_return.

              " Registar erro para reprocessamento
              CLEAR ls_zwm076.
              ls_zwm076-lgnum    = lt_ltap-lgnum.
              ls_zwm076-exidv    = lv_exidv.
              ls_zwm076-tanum    = lt_ltap-tanum.
              ls_zwm076-datum    = sy-datum.
              ls_zwm076-uzeit    = sy-uzeit.
              ls_zwm076-lgpla    = lv_lgpla.
              ls_zwm076-tanum_a  = lv_to.
              ls_zwm076-errormsg = 'Não foi criada OT de entrada de EAU -> AUT'.

              MODIFY zwm076 FROM ls_zwm076.
              IF sy-subrc = 0.
                COMMIT WORK .
              ENDIF.
            ENDIF.
          ENDIF.

        ELSE.

          " Erro criação OT arrumação
          SELECT SINGLE *
            FROM lein INTO ls_lein
            WHERE lenum = lv_exidv
            AND   lgnum = lt_ltap-lgnum.

          IF sy-subrc <> 0. "AND lv_lgpla = 'AUT'.

            " Registar erro para reprocessamento
            CLEAR ls_zwm076.
            ls_zwm076-lgnum    = lt_ltap-lgnum.
            ls_zwm076-exidv    = lv_exidv.
            ls_zwm076-tanum    = lt_ltap-tanum.
            ls_zwm076-datum    = sy-datum.
            ls_zwm076-uzeit    = sy-uzeit.
            ls_zwm076-errormsg = lv_msg.
            ls_zwm076-lgpla    = lv_lgpla.

            READ TABLE ct_idoc_data INDEX 1 INTO ls_idoc_data.
            IF sy-subrc = 0.
              ls_zwm076-docnum = ls_idoc_data-docnum.
            ENDIF.

            MODIFY zwm076 FROM ls_zwm076.
            IF sy-subrc = 0.
              COMMIT WORK .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

** Confirmação de OT de saída do AUT -> Paletização Especial
** Confirmação de OT de Saída para DITA2
**********************************************************************
  ELSEIF ( lt_ltap-vltyp = 'AUT' AND ls_ltak-bwlvs = lv_bwlvs_bpe ) OR
           ls_ltak-bwlvs = lv_bwlvs_man_bpe OR
           ls_ltak-bwlvs = lv_bwlvs_dita2.

*    IF c_quknz = '2'.
    IF ls_ltak-bwlvs = lv_bwlvs_dita2.
      lv_subobject = lc_subobject8.
    ELSE.
      lv_subobject = lc_subobject3.
    ENDIF.

** Stock SU de bloco
    LOOP AT ct_ltap_conf INTO ls_ltap_conf WHERE lenum IS NOT INITIAL.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      SELECT *
        FROM lqua INTO TABLE lt_lqua
        FOR ALL ENTRIES IN ct_ltap_conf
        WHERE lgnum = i_lgnum
        AND   lenum = ct_ltap_conf-lenum.
    ENDIF.

** Ponto de Entrega
    LOOP AT ct_idoc_data INTO ls_idoc_data WHERE segnam = 'E1LTCOI'.
      MOVE ls_idoc_data-sdata TO ls_e1ltcoi.

      " Confirmação em posição de destino diferente
      IF ls_e1ltcoi-nlpla <> lt_ltap-nlpla.
        lv_lgpla = ls_e1ltcoi-nlpla.
      ELSE.
        lv_lgpla = lt_ltap-nlpla.
      ENDIF.

      CLEAR ls_e1ltcoi-nlpla.

      MOVE ls_e1ltcoi TO ls_idoc_data-sdata.

      MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
    ENDLOOP.

    CLEAR lt_return.
    lt_return-msgtyp = 'S'.
    lt_return-msgid  = 'ZWMMSG001'.
    lt_return-msgnr  = '000'.
    lt_return-msgv1  = 'Ponto Entrega'.
    lt_return-msgv2  = lv_lgpla.
    APPEND lt_return.

** Validar Confirmação
    LOOP AT ct_ltap_conf INTO ls_ltap_conf.
      lv_tabix = sy-tabix.

      CLEAR: ls_ltap_conf-nlpla, ls_ltap_conf-squit.

      READ TABLE lt_ltap WITH KEY tanum = ls_ltap_conf-tanum
                                  tapos = ls_ltap_conf-tapos.
      CHECK sy-subrc = 0.

      " Confirmação SU
      IF lt_ltap-vlenr IS NOT INITIAL.

        CLEAR ls_ltap_conf-lenum.
        ls_ltap_conf-nista = lt_ltap-vsola.
        ls_ltap_conf-altme = lt_ltap-altme.

        " Confirmação Bloco
      ELSE.

        IF lt_ltap-charg IS NOT INITIAL.
          READ TABLE lt_lqua WITH KEY lenum = ls_ltap_conf-lenum
                                      matnr = lt_ltap-matnr
                                      charg = lt_ltap-charg.
        ELSE.
          READ TABLE lt_lqua WITH KEY lenum = ls_ltap_conf-lenum
                                      matnr = lt_ltap-matnr.
        ENDIF.

        CHECK sy-subrc = 0.

*        CLEAR ls_ltap_conf-squit.

        IF lt_lqua-meins <> lt_ltap-altme.
          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              i_matnr              = lt_ltap-matnr
              i_in_me              = lt_lqua-meins
              i_out_me             = lt_ltap-altme
              i_menge              = lt_lqua-verme
            IMPORTING
              e_menge              = lt_lqua-verme
            EXCEPTIONS
              error_in_application = 1
              error                = 2
              OTHERS               = 3.
        ENDIF.

        ls_ltap_conf-pickm = lt_lqua-verme.
        ls_ltap_conf-altme = lt_ltap-altme.
      ENDIF.

      MODIFY ct_ltap_conf FROM ls_ltap_conf INDEX lv_tabix.
    ENDLOOP.

** Fazer Pick
    CALL FUNCTION 'L_TO_CONFIRM'
      EXPORTING
        i_lgnum       = ls_ltak-lgnum
        i_tanum       = ls_ltak-tanum
        i_quknz       = '1'
        i_subst       = 'X'
        i_commit_work = 'X'
      TABLES
        t_ltap_conf   = ct_ltap_conf
      EXCEPTIONS
        error_message = 99.

    IF sy-subrc = 0.
      COMMIT WORK.

      " Preparar estrutura da OT do IDOC para o Transfer
      REFRESH: lt_ltap.

      SELECT *
        FROM ltap INTO TABLE lt_ltap
        WHERE lgnum = ls_ltak-lgnum
        AND   tanum = ls_ltak-tanum.

*        lt_ltap_conf[] = t_ltap_conf[].

      REFRESH: ct_ltap_conf.

      LOOP AT lt_ltap WHERE vlenr IS NOT INITIAL.
        CLEAR ls_ltap_conf.
        ls_ltap_conf-tanum = lt_ltap-tanum.
        ls_ltap_conf-tapos = lt_ltap-tapos.
*          t_ltap_conf-lenum = lt_ltap-vlenr.
        ls_ltap_conf-nista = lt_ltap-vsola.
        ls_ltap_conf-altme = lt_ltap-altme.
        APPEND ls_ltap_conf TO ct_ltap_conf.

        " OT & confirmada com sscc & material & lote &!
        CLEAR lt_return.
        lt_return-msgtyp = 'S'.
        lt_return-msgid  = 'ZWMMSG001'.
        lt_return-msgnr  = '355'.
        lt_return-msgv1  = lt_ltap-tanum.
        lt_return-msgv2  = lt_ltap-vlenr.
        lt_return-msgv3  = lt_ltap-matnr.
        lt_return-msgv4  = lt_ltap-charg.
        APPEND lt_return.
      ENDLOOP.
*      ENDIF.

    ELSE.
      LOOP AT ct_ltap_conf INTO ls_ltap_conf.

        READ TABLE lt_ltap WITH KEY tanum = ls_ltap_conf-tanum
                                    tapos = ls_ltap_conf-tapos.

        IF lt_ltap-vlenr IS INITIAL.
          lt_ltap-vlenr = ls_ltap_conf-lenum.
        ENDIF.

        " Erro no pick da OT & com sscc & material & lote &!
        CLEAR lt_return.
        lt_return-msgtyp = 'E'.
        lt_return-msgid  = 'ZWMMSG001'.
        lt_return-msgnr  = '356'.
        lt_return-msgv1  = lt_ltap-tanum.
        lt_return-msgv2  = lt_ltap-vlenr.
        lt_return-msgv3  = lt_ltap-matnr.
        lt_return-msgv4  = lt_ltap-charg.
        APPEND lt_return.
      ENDLOOP.
    ENDIF.

*    ENDIF.

** Confirmação de OT de expedição na gravitica de automático
**********************************************************************
  ELSEIF ls_ltak-refnr IS NOT INITIAL AND ls_zwm028-st_pul = 'PUA'.

    IF c_quknz = '2'.

      lv_subobject = lc_subobject6.

** Stock SU de bloco
      LOOP AT ct_ltap_conf INTO ls_ltap_conf WHERE lenum IS NOT INITIAL.
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0.
        SELECT *
          FROM lqua INTO TABLE lt_lqua
          FOR ALL ENTRIES IN ct_ltap_conf
          WHERE lgnum = i_lgnum
          AND   lenum = ct_ltap_conf-lenum.
      ENDIF.

** Ponto de Entrega
      LOOP AT ct_idoc_data INTO ls_idoc_data WHERE segnam = 'E1LTCOI'.
        MOVE ls_idoc_data-sdata TO ls_e1ltcoi.

        " Confirmação em posição de destino diferente
        IF ls_e1ltcoi-nlpla <> lt_ltap-nlpla.
          lv_lgpla = ls_e1ltcoi-nlpla.
        ELSE.
          lv_lgpla = lt_ltap-nlpla.
        ENDIF.

        CLEAR ls_e1ltcoi-nlpla.

        MOVE ls_e1ltcoi TO ls_idoc_data-sdata.

        MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
      ENDLOOP.

      CLEAR lt_return.
      lt_return-msgtyp = 'S'.
      lt_return-msgid  = 'ZWMMSG001'.
      lt_return-msgnr  = '000'.
      lt_return-msgv1  = 'Ponto Entrega'.
      lt_return-msgv2  = lv_lgpla.
      APPEND lt_return.

** Validar Confirmação
      LOOP AT ct_ltap_conf INTO ls_ltap_conf.
        lv_tabix = sy-tabix.

        IF sy-tabix = 1.
          lv_su = ls_ltap_conf-lenum.
        ELSEIF sy-tabix = 2.
          lv_su2 = ls_ltap_conf-lenum.
        ENDIF.

        CLEAR: ls_ltap_conf-nlpla, ls_ltap_conf-squit.

        READ TABLE lt_ltap WITH KEY tanum = ls_ltap_conf-tanum
                                    tapos = ls_ltap_conf-tapos.
        CHECK sy-subrc = 0.

        " Confirmação SU
        IF lt_ltap-vlenr IS NOT INITIAL.

          CHECK ls_ltap_conf-lenum = lt_ltap-vlenr.

          CLEAR ls_ltap_conf-lenum.
          ls_ltap_conf-nista = lt_ltap-vsola.
          ls_ltap_conf-altme = lt_ltap-altme.

          " Confirmação Bloco
        ELSE.

          IF lt_ltap-charg IS NOT INITIAL.
            READ TABLE lt_lqua WITH KEY lenum = ls_ltap_conf-lenum
                                        matnr = lt_ltap-matnr
                                        charg = lt_ltap-charg.
          ELSE.
            READ TABLE lt_lqua WITH KEY lenum = ls_ltap_conf-lenum
                                        matnr = lt_ltap-matnr.
          ENDIF.

          CHECK sy-subrc = 0.

          IF lt_lqua-meins <> lt_ltap-altme.
            CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
              EXPORTING
                i_matnr              = lt_ltap-matnr
                i_in_me              = lt_lqua-meins
                i_out_me             = lt_ltap-altme
                i_menge              = lt_lqua-verme
              IMPORTING
                e_menge              = lt_lqua-verme
              EXCEPTIONS
                error_in_application = 1
                error                = 2
                OTHERS               = 3.
          ENDIF.

          ls_ltap_conf-pickm = lt_lqua-verme.
          ls_ltap_conf-altme = lt_ltap-altme.
        ENDIF.

        MODIFY ct_ltap_conf FROM ls_ltap_conf INDEX lv_tabix.
      ENDLOOP.

      " Não faz pick da OT -> (já foi efetuado no system-guided)
      IF lt_ltap-vltyp <> 'AUT'.
        CLEAR ct_ltap_conf.
        REFRESH ct_ltap_conf.
      ELSE.
        c_quknz = '1'.
      ENDIF.

** Validar se é Paletes remontadas
      SELECT SINGLE *
        FROM zwm020 INTO ls_zwm020
          WHERE armazem = i_lgnum
          AND    p1 = lv_su
          AND    p2 = lv_su2.

      IF sy-subrc <> 0.
        SELECT SINGLE *
          FROM zwm020 INTO ls_zwm020
          WHERE armazem = i_lgnum
          AND    p1 = lv_su2
          AND    p2 = lv_su.
      ENDIF.

      IF sy-subrc = 0.
        lv_su      = ls_zwm020-p1.
        lv_su2     = ls_zwm020-p2.
        lv_add_su2 = 'X'.
      ENDIF.

*      " Validar se é OT do Trilateral
*      IF ls_zwm020 IS NOT INITIAL.
*
*        SELECT SINGLE valor
*          FROM zwm001 INTO lv_queue_tri_g
*          WHERE armazem   = i_lgnum
*          AND   processo  = 'GESTAO_FILAS'
*          AND   parametro = 'FILA_SAIDA_TRI_GRA'.
*
*        " Confirmar a OT da palete remontada (2 OTs independentes)
*        IF ls_ltak-queue = lv_queue_tri_g.
*
*          SELECT *
*            FROM ltap INTO TABLE lt_ltap_rem
*            WHERE lgnum = i_lgnum
*            AND   vlenr = lv_su2.
*
*          DELETE lt_ltap_rem WHERE pquit = 'X'.
*
*          LOOP AT lt_ltap_rem.
*            CLEAR lt_ltap_conf.
*            lt_ltap_conf-tanum = lt_ltap_rem-tanum.
*            lt_ltap_conf-tapos = lt_ltap_rem-tapos.
*            lt_ltap_conf-nista = lt_ltap_rem-vsola.
*            lt_ltap_conf-altme = lt_ltap_rem-altme.
*            APPEND lt_ltap_conf.
*          ENDLOOP.
*
*          IF lt_ltap_conf[] IS NOT INITIAL.
*            CALL FUNCTION 'L_TO_CONFIRM'
*              EXPORTING
*                i_lgnum       = lt_ltap_rem-lgnum
*                i_tanum       = lt_ltap_rem-tanum
*                i_quknz       = '1'
*                i_subst       = ''
*                i_commit_work = 'X'
*              TABLES
*                t_ltap_conf   = lt_ltap_conf
*              EXCEPTIONS
*                error_message = 99.
*
*            IF sy-subrc <> 0.
*
*              " Erro no pick da OT & da palete remontada &!
*              CLEAR lt_return.
*              lt_return-msgtyp = 'E'.
*              lt_return-msgid  = 'ZWMMSG001'.
*              lt_return-msgnr  = '358'.
*              lt_return-msgv1  = lt_ltap_rem-tanum.
*              lt_return-msgv2  = lt_ltap_rem-vlenr.
*              APPEND lt_return.
*            ENDIF.
*          ENDIF.
*
*          DELETE ct_ltap_conf WHERE lenum = lv_su2.
*        ENDIF.
*      ENDIF.

** Atualizar paletes na gravitica
      CONCATENATE ls_zwm028-st_pul lv_lgpla INTO lv_bin_output SEPARATED BY space.

      CALL FUNCTION 'ZWM_CHANGE_PAL_PUL'
        EXPORTING
          i_lgnum        = i_lgnum
          i_grupo        = ls_zwm028-refnr
          i_remessa      = ls_zwm028-remessa
          i_bin_output_d = lv_bin_output
          i_tipo_palete  = lv_tipo_palete
          i_su           = lv_su
          i_add_su2      = lv_add_su2
          i_su2          = lv_su2.

      " Atualizar tabela de controlo de expedição na gravítica
      GET TIME.

      UPDATE zwm078 SET pulmao = abap_true
                        pdatu  = sy-datum
                        pzeit  = sy-uzeit
                        pname  = sy-uname
                        nlpla  = lv_lgpla
                        lenum  = lv_su
                       WHERE  lgnum = ls_ltak-lgnum
                       AND    tanum = ls_ltak-tanum.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

** Confirmação de OT de saída do Automático para mesas do Convencional
**********************************************************************
  ELSEIF ( ( lt_ltap-vltyp = 'AUT' AND lt_ltap-vlpla = 'AUT' ) OR
           ( lt_ltap-vltyp = 'EAU' and ls_ltak-bwlvs = '850' ) or " Palete Especial - Expedição Pulmão Manual
             ls_ltak-bwlvs = lv_bwlvs_pick ).                     " Palete picking  - Expedição Pulmão Manual

    lv_subobject = lc_subobject4.

** Não confirmar OT
    lt_ltap_conf[] = ct_ltap_conf[].

    CLEAR ct_ltap_conf.
    REFRESH ct_ltap_conf.

** Mesa Saída Convencional
    LOOP AT ct_idoc_data INTO ls_idoc_data WHERE segnam = 'E1LTCOI'.
      MOVE ls_idoc_data-sdata TO ls_e1ltcoi.

      " Confirmação em posição de destino diferente
      IF ls_e1ltcoi-nlpla <> lt_ltap-nlpla.
        lv_lgpla = ls_e1ltcoi-nlpla.
      ELSE.
        lv_lgpla = lt_ltap-nlpla.
      ENDIF.

      CLEAR ls_e1ltcoi-nlpla.

      MOVE ls_e1ltcoi TO ls_idoc_data-sdata.

      MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
    ENDLOOP.

    CLEAR lt_return.
    lt_return-msgtyp = 'S'.
    lt_return-msgid  = 'ZWMMSG001'.
    lt_return-msgnr  = '000'.
    lt_return-msgv1  = 'Ponto Entrega'.
    lt_return-msgv2  = lv_lgpla.
    APPEND lt_return.

** Guardar Paletes
    LOOP AT lt_ltap_conf.

      " Confirmação de Bloco
      CHECK lt_ltap_conf-lenum IS NOT INITIAL.

      lt_lein-lenum =  lt_ltap_conf-lenum.
      COLLECT lt_lein.
    ENDLOOP.

    " Confirmação de SU
    LOOP AT lt_ltap.
      CHECK lt_ltap-vlenr IS NOT INITIAL.

      lt_lein-lenum =  lt_ltap-vlenr.
      COLLECT lt_lein.
    ENDLOOP.

    " Guardar dados
    CLEAR ls_zwm077.
    ls_zwm077-lgnum = ls_ltak-lgnum.
    ls_zwm077-tanum = ls_ltak-tanum.

    READ TABLE ct_idoc_data INDEX 1 INTO ls_idoc_data.
    IF sy-subrc = 0.
      ls_zwm077-docnum = ls_idoc_data-docnum.
    ENDIF.

    GET TIME.

    ls_zwm077-lgpla = lv_lgpla.
    ls_zwm077-rdatu = sy-datum.
    ls_zwm077-rzeit = sy-uzeit.
    ls_zwm077-rname = sy-uname.

    LOOP AT lt_lein.

      SELECT SINGLE *
        FROM zwm020 INTO ls_zwm020
        WHERE armazem = ls_ltak-lgnum
        AND   p2      = lt_lein-lenum.

      CHECK sy-subrc <> 0.

      ls_zwm077-exidv  = lt_lein-lenum.

      APPEND ls_zwm077 TO lt_zwm077.
    ENDLOOP.

    IF lt_zwm077[] IS NOT INITIAL.
      MODIFY zwm077 FROM TABLE lt_zwm077.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

** Outras confirmaçoes
**********************************************************************
  ELSE.


  ENDIF.

** Log
**********************************************************************
  lv_extnumber = ls_ltak-refnr.

  CALL FUNCTION 'ZWM_MSG_LOG_WCS'
    EXPORTING
      i_object    = lc_object
      i_subobject = lv_subobject
      i_state     = 'A'
      i_extnumber = lv_extnumber
    TABLES
      t_log2      = lt_return
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.


ENDFUNCTION.
