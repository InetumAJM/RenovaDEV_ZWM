FUNCTION zwm_check_to_confirm_wcs .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LTAK_VB) TYPE  LTAK_VB
*"  EXPORTING
*"     VALUE(E_TANUM) TYPE  TANUM
*"  TABLES
*"      T_LTAP_VB STRUCTURE  LTAP_VB
*"----------------------------------------------------------------------
  CONSTANTS: lc_object     TYPE balhdr-object    VALUE 'ZWCS'.
  CONSTANTS: lc_subobject2 TYPE balhdr-subobject VALUE 'ZWCS_06'.
  CONSTANTS: lc_subobject3 TYPE balhdr-subobject VALUE 'ZWCS_07'.
  CONSTANTS: lc_subobject4 TYPE balhdr-subobject VALUE 'ZWCS_12'.
  CONSTANTS: lc_subobject5 TYPE balhdr-subobject VALUE 'ZWCS_14'.

  DATA: lv_bwlvs      TYPE bwlvs,
        lv_tanum      TYPE tanum,
        lv_lznum      TYPE lvs_lznum,
        lv_code       LIKE bapi2017_gm_code,
        lv_bwlvs_pick TYPE bwlvs,
        lv_subobject  TYPE balhdr-subobject,
        lv_extnumber  TYPE balnrext,
        lv_lgtyp_epe  TYPE lgtyp,
        lv_spgru      TYPE lvs_spgru,
        lv_remessa    TYPE vbeln,
        lv_nltyp      TYPE lgtyp,
        lv_nlpla      TYPE lgpla,
        lv_zsyst      TYPE recvsystem,
        lv_epe_pul    TYPE flag,
        lv_pal_pick   TYPE flag,
        lv_lines      TYPE i,
        lv_refnr      TYPE lvs_refnr,
        lv_mblnr      TYPE mblnr,
        lv_mjahr      TYPE mjahr,
        lv_errormsg   TYPE char100.

  DATA: ls_ltak       TYPE ltak.
  DATA: ls_ltak_n     TYPE ltak.
  DATA: ls_ltakm      TYPE ltak.
  DATA: ls_lein       TYPE lein.
  DATA: ls_leinv      TYPE leinv.
  DATA: ls_zwm077     TYPE zwm077.
  DATA: ls_zwm028     TYPE zwm028.
  DATA: ls_zwm084     TYPE zwm084.
  DATA: ls_gm_header  LIKE bapi2017_gm_head_01.

  DATA: lt_ltap_creat   TYPE TABLE OF ltap_creat   WITH HEADER LINE.
  DATA: lt_ltap_conf    TYPE TABLE OF ltap_conf    WITH HEADER LINE.
  DATA: lt_ltak_rej     TYPE TABLE OF ltak         WITH HEADER LINE.
  DATA: lt_ltap_rej     TYPE TABLE OF ltap         WITH HEADER LINE.
  DATA: lt_ltap         TYPE TABLE OF ltap         WITH HEADER LINE.
  DATA: lt_ltap_n       TYPE TABLE OF ltap         WITH HEADER LINE.
  DATA: lt_ltap_mult    TYPE TABLE OF ltap         WITH HEADER LINE.
  DATA: lt_ltapm        TYPE TABLE OF ltap         WITH HEADER LINE.
  DATA: lt_ltap_epe     TYPE TABLE OF ltap         WITH HEADER LINE.
  DATA: lt_ltap_aux     TYPE TABLE OF ltap         WITH HEADER LINE.
  DATA: lt_zwm026       TYPE TABLE OF zwm026       WITH HEADER LINE.
  DATA: lt_return       TYPE TABLE OF bdcmsgcoll   WITH HEADER LINE.
  DATA: lt_gm_return    TYPE TABLE OF bapiret2     WITH HEADER LINE.
  DATA: lt_ltap_move_su TYPE TABLE OF ltap_move_su WITH HEADER LINE.
  DATA: lt_ltap_vb      TYPE TABLE OF ltap_vb      WITH HEADER LINE.
  DATA: lt_items        TYPE TABLE OF zwm018       WITH HEADER LINE.
  DATA: lt_gm_item      TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE.

** Validar OT de WCS
**********************************************************************
  lv_zsyst = 'WMSRPTA100'.

* WHILE 1 > 0.
*
*  ENDWHILE.

  LOOP AT t_ltap_vb WHERE qdatu IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  CHECK t_ltap_vb-nltyp = 'EAU' OR
        t_ltap_vb-nltyp = 'BPK' OR
      ( t_ltap_vb-nltyp = 'AUT' AND t_ltap_vb-nlpla = 'AUT' ) OR
        i_ltak_vb-bwlvs = '938' OR
        i_ltak_vb-bwlvs = '959'.

** Validar se OT está confirmada
  DO 10 TIMES.
    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = i_ltak_vb-lgnum
      AND   tanum = i_ltak_vb-tanum.

    IF ls_ltak-kquit = 'X'.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = ls_ltak-lgnum
    AND   tanum = ls_ltak-tanum.

** Ponto de Entrada no WCS
  SELECT SINGLE *
    FROM zwm077 INTO ls_zwm077
    WHERE lgnum = i_ltak_vb-lgnum
    AND   tanum = i_ltak_vb-tanum.

  lv_lznum = ls_ltak-lznum.
  lv_refnr = ls_ltak-refnr.

** OT de Entrada no Automático
**********************************************************************
  IF t_ltap_vb-nltyp = 'EAU'.

** Reprocessamento de paletes Rejeitadas
    IF t_ltap_vb-vltyp = 'REJ'.

      " Validar Movimento de ultima entrada no EAU
      REFRESH: lt_ltak_rej, lt_ltap_rej.

      SELECT *
        FROM ltap INTO TABLE lt_ltap_rej
        WHERE lgnum = t_ltap_vb-lgnum
        AND   vlenr = t_ltap_vb-nlenr.

      DELETE lt_ltap_rej WHERE pquit IS INITIAL.
      DELETE lt_ltap_rej WHERE vltyp <> 'EAU'.
      DELETE lt_ltap_rej WHERE nltyp = 'REJ'.
      DELETE lt_ltap_rej WHERE vltyp = 'REJ'.

      IF lt_ltap_rej[] IS NOT INITIAL.
        SELECT *
          FROM ltak INTO TABLE lt_ltak_rej
          FOR ALL ENTRIES IN lt_ltap_rej
          WHERE lgnum = lt_ltap_rej-lgnum
          AND   tanum = lt_ltap_rej-tanum.
      ENDIF.

      SORT lt_ltak_rej BY bdatu DESCENDING bzeit DESCENDING.

      " Obter último Movimento
      READ TABLE lt_ltak_rej INDEX 1.
      IF sy-subrc = 0.
        lv_bwlvs = lt_ltak_rej-bwlvs.
        lv_lznum = lt_ltak_rej-lznum.
        lv_refnr = lt_ltak_rej-refnr.

        READ TABLE lt_ltap_rej WITH KEY tanum = lt_ltak_rej-tanum.
      ENDIF.

** Entrada de Produção
    ELSEIF t_ltap_vb-vltyp = 'PRO' AND t_ltap_vb-vlpla = 'AUT'.

      " Movimento - Não gera IDOC - "943"
      CALL FUNCTION 'ZWM_GET_PARAMETER'
        EXPORTING
          i_lgnum     = i_ltak_vb-lgnum
          i_processo  = 'WCS'
          i_parametro = 'MOV_WM_ENT_PROD'
        IMPORTING
          e_valor     = lv_bwlvs
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

** Abastecimento de Paletes Vazias no BPE
    ELSEIF ls_ltak-bwlvs = '935'.

      " Movimento - Gera IDOC - "938"
      CALL FUNCTION 'ZWM_GET_PARAMETER'
        EXPORTING
          i_lgnum     = i_ltak_vb-lgnum
          i_processo  = 'WCS'
          i_parametro = 'MOV_WM_PILHA_BPE'
        IMPORTING
          e_valor     = lv_bwlvs
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

** Abastecimento Paletização Especial
    ELSEIF ls_ltak-bwlvs = '947'.

      " Origem Automático
      IF t_ltap_vb-vlpla = 'AUT'.

        " Movimento - Não Gera IDOC - "949"
        CALL FUNCTION 'ZWM_GET_PARAMETER'
          EXPORTING
            i_lgnum     = i_ltak_vb-lgnum
            i_processo  = 'WCS'
            i_parametro = 'MOV_WM_AUT_BPE'
          IMPORTING
            e_valor     = lv_bwlvs
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.

        " Origem Convencional
      ELSE.

        " Movimento - Gera IDOC - "948"
        CALL FUNCTION 'ZWM_GET_PARAMETER'
          EXPORTING
            i_lgnum     = i_ltak_vb-lgnum
            i_processo  = 'WCS'
            i_parametro = 'MOV_WM_MAN_BPE'
          IMPORTING
            e_valor     = lv_bwlvs
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.

      ENDIF.

** Transferencia para DITA 2
    ELSEIF ls_ltak-bwlvs = '958'.

      " Movimento - Gera IDOC - "959"
      CALL FUNCTION 'ZWM_GET_PARAMETER'
        EXPORTING
          i_lgnum     = i_ltak_vb-lgnum
          i_processo  = 'WCS'
          i_parametro = 'MOV_WM_AUT_DITA2'
        IMPORTING
          e_valor     = lv_bwlvs
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

** Outras Entradas
    ELSE.

      " Movimento - Gera IDOC - "942"
      CALL FUNCTION 'ZWM_GET_PARAMETER'
        EXPORTING
          i_lgnum     = i_ltak_vb-lgnum
          i_processo  = 'WCS'
          i_parametro = 'MOV_WM_ENT'
        IMPORTING
          e_valor     = lv_bwlvs
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.
    ENDIF.

** Validar se é palete de picking
    lt_ltap_aux[] = lt_ltap[].

    DELETE lt_ltap_aux WHERE nlenr IS INITIAL.

    IF lt_ltap_aux[] IS NOT INITIAL.
      SELECT *
        FROM zwm026 INTO TABLE lt_zwm026
        FOR ALL ENTRIES IN lt_ltap_aux
        WHERE armazem = lt_ltap_aux-lgnum
        AND   sscc    = lt_ltap_aux-nlenr.
    ENDIF.

    READ TABLE lt_zwm026 INDEX 1.
    IF sy-subrc = 0.
      lv_pal_pick = 'X'.
    ENDIF.

** Validar se é Palete do EPE
    IF t_ltap_vb-vltyp = 'EPE' OR lt_ltap_rej-vltyp = 'EPE'.
      SELECT SINGLE *
        FROM zwm028 INTO ls_zwm028
        WHERE lgnum = ls_ltak-lgnum
        AND   refnr = ls_ltak-refnr.

      " Envio de Palete Especial para pulmão
      IF ( ls_zwm028-st_pul IS NOT INITIAL AND
           ls_zwm028-st_pul <> 'AUT'       AND
           ls_zwm028-st_pul <> 'PUA' ).

        lv_epe_pul = 'X'.

      ELSEIF ls_zwm028-st_dck IS NOT INITIAL.

        lv_epe_pul = 'X'.
      ENDIF.
    ENDIF.

** Palete EPE para pulmão
**********************************************************************
    IF lv_epe_pul = 'X'.

      CLEAR lv_tanum.
      CALL FUNCTION 'ZWM_CREATE_TO_PAL_ESP'
        EXPORTING
          i_lgnum = ls_ltak-lgnum
          i_tanum = ls_ltak-tanum
        IMPORTING
          e_tanum = lv_tanum
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.

      IF lv_tanum IS NOT INITIAL.

        " Validar se OT está confirmada
        DO 10 TIMES.
          SELECT SINGLE *
            FROM ltak INTO ls_ltakm
            WHERE lgnum = i_ltak_vb-lgnum
            AND   tanum = lv_tanum.

          IF sy-subrc = 0.
            SELECT *
              FROM ltap INTO TABLE lt_ltapm
              WHERE lgnum = i_ltak_vb-lgnum
              AND   tanum = lv_tanum.

            READ TABLE lt_ltap INDEX 1.

            LOOP AT lt_ltapm.
              MOVE-CORRESPONDING lt_ltapm TO lt_ltap_vb.

              IF ls_zwm077-lgpla IS NOT INITIAL.
                lt_ltap_vb-ablad = ls_zwm077-lgpla.

              ELSE.
                lt_ltap_vb-ablad = lt_ltap-ablad.
              ENDIF.

              IF lt_ltap_rej-ablad IS NOT INITIAL.
                lt_ltap_vb-ablad = lt_ltap_rej-ablad.
              ENDIF.

              lt_ltap_vb-zeugn = lt_ltap-zeugn.
              lt_ltap_vb-nlpla = 'MAN_SAI'.

              APPEND lt_ltap_vb.
            ENDLOOP.

            EXIT.
          ENDIF.

          WAIT UP TO 1 SECONDS.
        ENDDO.

        CALL FUNCTION 'L_IDOC_CREATE_WMTOID02'
          EXPORTING
            i_zsyst = lv_zsyst
            i_ltak  = ls_ltakm
            i_varia = ''
          TABLES
            t_ltap  = lt_ltap_vb.

        COMMIT WORK AND WAIT.

        LOOP AT lt_ltap_vb.

          UPDATE ltap SET kzsub = abap_true
                      WHERE lgnum = lt_ltap_vb-lgnum AND
                            tanum = lt_ltap_vb-tanum AND
                            tapos = lt_ltap_vb-tapos.
        ENDLOOP.

        COMMIT WORK AND WAIT.

      ELSE.
        MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno INTO ls_zwm084-errormsg
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

** Palete Picking
**********************************************************************
    ELSEIF lv_pal_pick = 'X'.

      SELECT SINGLE id_servisan
        FROM zwm040 INTO lv_remessa
        WHERE lgnum   = lt_zwm026-armazem
        AND refnr   = lt_zwm026-grupo
        AND remessa = lt_zwm026-remessa.

      IF lv_remessa IS INITIAL.
        lv_remessa = lt_zwm026-remessa.
      ENDIF.

      CLEAR: ls_zwm028.

      SELECT SINGLE *
        FROM zwm028 INTO ls_zwm028
        WHERE lgnum   = lt_zwm026-armazem
          AND refnr   = lt_zwm026-grupo
          AND remessa = lv_remessa.

      " Palete de Picking para Expedição no Pulmão Manual
      IF ls_zwm028-st_pul IS NOT INITIAL AND
         ls_zwm028-st_pul <> 'AUT'       AND
         ls_zwm028-st_pul <> 'PUA'.

        IF ls_zwm028-zlock = '1' OR ls_zwm028-st_pul = 'BPK'.
          lv_nltyp = 'BPK'.

        ELSE.
          lv_nltyp = '933'.     "ls_zwm028-st_pul.
          lv_nlpla = 'DESTINO'. "ls_zwm028-pulmao1.
        ENDIF.

        lv_bwlvs = '962'.

      ELSEIF ls_zwm028-st_dck IS NOT INITIAL.

        IF ls_zwm028-zlock = '1'.
          lv_nltyp = 'BPK'.

        ELSE.
          lv_nltyp = '933'.    "ls_zwm028-st_dck.
          lv_nlpla = 'DESTINO'."ls_zwm028-porta.
        ENDIF.

        lv_bwlvs = '962'.
      ENDIF.

      " Meias Paletes (não pode entrar no armazém automático)
      IF t_ltap_vb-letyp = 'P21' AND lv_nltyp IS INITIAL.
        lv_nltyp = 'BPK'.
        lv_bwlvs = '962'.
      ENDIF.

      LOOP AT lt_ltap.
        CLEAR lt_ltap_move_su.
        lt_ltap_move_su-matnr = lt_ltap-matnr.
        lt_ltap_move_su-werks = lt_ltap-werks.
        lt_ltap_move_su-lgort = lt_ltap-lgort.
        lt_ltap_move_su-charg = lt_ltap-charg.
        lt_ltap_move_su-bestq = lt_ltap-bestq.
        lt_ltap_move_su-vfdat = lt_ltap-vfdat.

        IF ls_zwm077-lgpla IS NOT INITIAL.
          lt_ltap_move_su-zeugn = ls_zwm077-lgpla.
        ELSE.
          lt_ltap_move_su-zeugn = lt_ltap-ablad.
        ENDIF.

        APPEND lt_ltap_move_su.
      ENDLOOP.

      CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
        EXPORTING
          i_lenum        = lt_zwm026-sscc
          i_bwlvs        = lv_bwlvs
          i_lznum        = lv_lznum
          i_nltyp        = lv_nltyp
          i_nlpla        = lv_nlpla
          i_refnr        = lt_zwm026-grupo
        IMPORTING
          e_tanum        = lv_tanum
        TABLES
          t_ltap_move_su = lt_ltap_move_su
*         t_ltap_vb      = lt_ltap_vb
        EXCEPTIONS
          error_message  = 1
          OTHERS         = 2.

      IF lv_tanum IS NOT INITIAL.

        " Gerar IDOC para Palete de picking para saida EXP2_SAI
        IF lv_bwlvs = '962'.

          " Validar se OT está confirmada
          DO 10 TIMES.
            SELECT SINGLE *
              FROM ltak INTO ls_ltakm
              WHERE lgnum = i_ltak_vb-lgnum
              AND   tanum = lv_tanum.

            IF sy-subrc = 0.
              SELECT *
                FROM ltap INTO TABLE lt_ltapm
                WHERE lgnum = i_ltak_vb-lgnum
                AND   tanum = lv_tanum.

              LOOP AT lt_ltapm.
                MOVE-CORRESPONDING lt_ltapm TO lt_ltap_vb.

                lt_ltap_vb-nlpla = 'EXP2_SAI'.
                APPEND lt_ltap_vb.
              ENDLOOP.

              EXIT.
            ENDIF.

            WAIT UP TO 1 SECONDS.
          ENDDO.

          CALL FUNCTION 'L_IDOC_CREATE_WMTOID02'
            EXPORTING
              i_zsyst = lv_zsyst
              i_ltak  = ls_ltakm
              i_varia = ''
            TABLES
              t_ltap  = lt_ltap_vb.

          COMMIT WORK AND WAIT.

          LOOP AT lt_ltap_vb.

            UPDATE ltap SET kzsub = abap_true
                        WHERE lgnum = lt_ltap_vb-lgnum AND
                              tanum = lt_ltap_vb-tanum AND
                              tapos = lt_ltap_vb-tapos.
          ENDLOOP.

          COMMIT WORK AND WAIT.
        ENDIF.

      ELSE.
        MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno INTO ls_zwm084-errormsg
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

** Outras Paletes
**********************************************************************
    ELSE.
      lt_ltap_mult[] = lt_ltap[].

      SORT lt_ltap_mult BY matnr charg.
      DELETE ADJACENT DUPLICATES FROM lt_ltap_mult COMPARING matnr charg.
      DESCRIBE TABLE lt_ltap_mult LINES lv_lines.

      IF lv_lines > 1.

        LOOP AT lt_ltap.
          CLEAR lt_ltap_move_su.
          lt_ltap_move_su-matnr = lt_ltap-matnr.
          lt_ltap_move_su-werks = lt_ltap-werks.
          lt_ltap_move_su-lgort = lt_ltap-lgort.
          lt_ltap_move_su-charg = lt_ltap-charg.
          lt_ltap_move_su-bestq = lt_ltap-bestq.
          lt_ltap_move_su-vfdat = lt_ltap-vfdat.

          IF ls_zwm077-lgpla IS NOT INITIAL.
            lt_ltap_move_su-zeugn = ls_zwm077-lgpla.
          ELSE.
            lt_ltap_move_su-zeugn = lt_ltap-ablad.
          ENDIF.

          APPEND lt_ltap_move_su.
        ENDLOOP.

        CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
          EXPORTING
            i_lenum        = lt_ltap-nlenr
            i_bwlvs        = lv_bwlvs
            i_lznum        = lv_lznum
*           i_nltyp        = lv_nltyp
*           i_nlpla        = lv_nlpla
            i_refnr        = lv_refnr
          IMPORTING
            e_tanum        = lv_tanum
          TABLES
            t_ltap_move_su = lt_ltap_move_su
          EXCEPTIONS
            error_message  = 1
            OTHERS         = 2.

      ELSE.
        LOOP AT lt_ltap.
          CLEAR lt_ltap_creat.
          lt_ltap_creat-werks = lt_ltap-werks.
          lt_ltap_creat-lgort = lt_ltap-lgort.
          lt_ltap_creat-matnr = lt_ltap-matnr.
          lt_ltap_creat-charg = lt_ltap-charg.
          lt_ltap_creat-anfme = lt_ltap-nsola.
          lt_ltap_creat-altme = lt_ltap-altme.
          lt_ltap_creat-vltyp = lt_ltap-nltyp.
          lt_ltap_creat-vlpla = lt_ltap-nlpla.
          lt_ltap_creat-vlenr = lt_ltap-nlenr.
          lt_ltap_creat-zeugn = lt_ltap-zeugn.
*      lt_ltap_creat-nlenr = lt_ltap-nlenr.
*      lt_ltap_creat-nltyp = 'AUT'.
*      lt_ltap_creat-nlpla = 'AUT'.

          IF ls_zwm077-lgpla IS NOT INITIAL.
            lt_ltap_creat-ablad = ls_zwm077-lgpla.
          ELSE.
            lt_ltap_creat-ablad = lt_ltap-ablad.
          ENDIF.

          lt_ltap_creat-letyp = lt_ltap-letyp.
          APPEND lt_ltap_creat.
        ENDLOOP.

        CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
          EXPORTING
            i_lgnum       = i_ltak_vb-lgnum
            i_bwlvs       = lv_bwlvs
            i_lznum       = lv_lznum
            i_refnr       = lv_refnr
          IMPORTING
            e_tanum       = lv_tanum
          TABLES
            t_ltap_creat  = lt_ltap_creat
          EXCEPTIONS
            error_message = 99.
      ENDIF.

      IF lv_tanum IS INITIAL.
        MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno INTO ls_zwm084-errormsg
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    lv_subobject = lc_subobject3.

** OT de Arrumação no Automático
**********************************************************************
  ELSEIF t_ltap_vb-nltyp = 'AUT' AND t_ltap_vb-nlpla = 'AUT'.

    IF t_ltap_vb-vorga <> 'ST' AND t_ltap_vb-vorga <> 'SL'.

** Validar Palete de Paletização Especial
      CALL FUNCTION 'ZWM_GET_PARAMETER'
        EXPORTING
          i_lgnum     = i_ltak_vb-lgnum
          i_processo  = 'PALETIZACAO_ESPECIAL'
          i_parametro = 'TIPO_DEP_ESTACAO'
        IMPORTING
          e_valor     = lv_lgtyp_epe
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      SELECT *
        FROM ltap INTO TABLE lt_ltap_epe
        WHERE lgnum = t_ltap_vb-lgnum
        AND   nlenr = t_ltap_vb-nlenr.

      " Bloquear Palete - Desbloqueado no cockpit de expedição
      READ TABLE lt_ltap_epe WITH KEY vltyp = lv_lgtyp_epe.
      IF sy-subrc = 0.

        " Bloquear UD
        SELECT SINGLE *
          FROM lein INTO ls_lein
          WHERE lenum = t_ltap_vb-nlenr.

        IF sy-subrc = 0.
          lv_spgru = '6'.

          ls_lein-skzua = 'X'.
          ls_lein-skzue = 'X'.
          ls_lein-spgru = lv_spgru.

          MOVE-CORRESPONDING ls_lein TO ls_leinv.

          CALL FUNCTION 'L_LEIN_VERAENDERN'
            EXPORTING
              xleinv = ls_leinv.

          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

** Validar Palete de picking
      CALL FUNCTION 'ZWM_GET_PARAMETER'
        EXPORTING
          i_lgnum     = i_ltak_vb-lgnum
          i_processo  = 'WCS'
          i_parametro = 'MOV_WM_SAI_PICK'
        IMPORTING
          e_valor     = lv_bwlvs_pick
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      lt_ltap_aux[] = lt_ltap[].

      DELETE lt_ltap_aux WHERE vlenr IS INITIAL.

      IF lt_ltap_aux[] IS NOT INITIAL.
        SELECT *
          FROM zwm026 INTO TABLE lt_zwm026
          FOR ALL ENTRIES IN lt_ltap_aux
          WHERE armazem = lt_ltap_aux-lgnum
          AND   sscc    = lt_ltap_aux-vlenr.
      ENDIF.

      " Criar OT de saída de automático
      READ TABLE lt_zwm026 INDEX 1.
      IF sy-subrc = 0.
        LOOP AT lt_ltap.
          CLEAR lt_ltap_creat.
          lt_ltap_creat-werks = lt_ltap-werks.
          lt_ltap_creat-lgort = lt_ltap-lgort.
          lt_ltap_creat-matnr = lt_ltap-matnr.
          lt_ltap_creat-charg = lt_ltap-charg.
          lt_ltap_creat-anfme = lt_ltap-nsola.
          lt_ltap_creat-altme = lt_ltap-altme.
          lt_ltap_creat-vltyp = lt_ltap-nltyp.
          lt_ltap_creat-vlpla = lt_ltap-nlpla.
          lt_ltap_creat-vlenr = lt_ltap-nlenr.
          lt_ltap_creat-nltyp = lt_ltap-nlenr.
          lt_ltap_creat-vlenr = lt_ltap-nlenr.
          lt_ltap_creat-letyp = lt_ltap-letyp.
          APPEND lt_ltap_creat.
        ENDLOOP.

        lv_bwlvs = lv_bwlvs_pick.

        CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
          EXPORTING
            i_lgnum       = i_ltak_vb-lgnum
            i_bwlvs       = lv_bwlvs
            i_lznum       = lv_lznum
            i_refnr       = lt_zwm026-grupo
          IMPORTING
            e_tanum       = lv_tanum
          TABLES
            t_ltap_creat  = lt_ltap_creat
          EXCEPTIONS
            error_message = 99.

        lv_subobject = lc_subobject2.
        lv_extnumber = lt_zwm026-grupo.

        " Atualizar tabela picking com entrada de palete no automático
        LOOP AT lt_zwm026.
          UPDATE zwm026 SET wcs_ent_aut = 'X'
          WHERE armazem       = lt_zwm026-armazem
          AND   n_pal_picking = lt_zwm026-n_pal_picking
          AND   i_pal_picking = lt_zwm026-i_pal_picking
          AND   grupo         = lt_zwm026-grupo
          AND   remessa       = lt_zwm026-remessa
          AND   posnr         = lt_zwm026-posnr
          AND   sub_item      = lt_zwm026-sub_item.
        ENDLOOP.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.

        IF lv_tanum IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno INTO ls_zwm084-errormsg
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDIF.
    ENDIF.

    lv_subobject = lc_subobject2.

** Entrada de Pilha de paletes vazias no MAN_BUF21 do BPE
**********************************************************************
  ELSEIF i_ltak_vb-bwlvs = '938'.

    " Items
    LOOP AT lt_ltap.
      CLEAR lt_ltap_creat.
      lt_ltap_creat-werks = lt_ltap-werks.
      lt_ltap_creat-lgort = lt_ltap-lgort.
      lt_ltap_creat-matnr = lt_ltap-matnr.
      lt_ltap_creat-charg = lt_ltap-charg.
      lt_ltap_creat-anfme = lt_ltap-nsola.
      lt_ltap_creat-altme = lt_ltap-altme.
      lt_ltap_creat-vltyp = lt_ltap-nltyp.
      lt_ltap_creat-vlpla = lt_ltap-nlpla.
      lt_ltap_creat-vlenr = lt_ltap-nlenr.
      lt_ltap_creat-nltyp = 'PAL'.
      lt_ltap_creat-letyp = lt_ltap-letyp.

      " Posição - Material da Palete
      lt_ltap_creat-nlpla = lt_ltap-matnr.

      APPEND lt_ltap_creat.
    ENDLOOP.

    CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
      EXPORTING
        i_lgnum       = i_ltak_vb-lgnum
        i_bwlvs       = '999'
        i_lznum       = lv_lznum
        i_refnr       = ls_ltak-refnr
      IMPORTING
        e_tanum       = lv_tanum
      TABLES
        t_ltap_creat  = lt_ltap_creat
      EXCEPTIONS
        error_message = 99.

    IF lv_tanum IS NOT INITIAL.

      DO 10 TIMES.
        SELECT SINGLE *
          FROM ltak INTO ls_ltak_n
          WHERE lgnum = i_ltak_vb-lgnum
          AND   tanum = lv_tanum.

        IF sy-subrc = 0.
          EXIT.
        ENDIF.

        WAIT UP TO 1 SECONDS.
      ENDDO.

      SELECT *
        FROM ltap INTO TABLE lt_ltap_n
        WHERE lgnum = ls_ltak_n-lgnum
        AND   tanum = ls_ltak_n-tanum.

      LOOP AT lt_ltap_n.
        CLEAR lt_ltap_conf.
        lt_ltap_conf-tanum = lt_ltap_n-tanum.
        lt_ltap_conf-tapos = lt_ltap_n-tapos.
        lt_ltap_conf-altme = lt_ltap_n-meins.
        lt_ltap_conf-nista = lt_ltap_n-vsolm.
        APPEND lt_ltap_conf.
      ENDLOOP.

      CALL FUNCTION 'L_TO_CONFIRM'
        EXPORTING
          i_lgnum       = ls_ltak_n-lgnum
          i_tanum       = ls_ltak_n-tanum
          i_quknz       = '4' "lv_quknz
          i_commit_work = 'X'
        TABLES
          t_ltap_conf   = lt_ltap_conf
        EXCEPTIONS
          error_message = 99.

      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno INTO ls_zwm084-errormsg
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_subobject = lc_subobject4.

** OT Transferência de Retorno para DITA 2
**********************************************************************
  ELSEIF i_ltak_vb-bwlvs = '959'.

    " Dados Cabeçalho
    lv_code                 = '04'.
    ls_gm_header-pstng_date = sy-datum.

    "  Dados Items
    LOOP AT lt_ltap.
      CLEAR lt_gm_item.
      lt_gm_item-move_type     = '311'.

      lt_gm_item-plant         = lt_ltap-werks.
      lt_gm_item-stge_loc      = lt_ltap-lgort.
      lt_gm_item-move_plant    = lt_ltap-werks.
      lt_gm_item-move_stloc    = 'QUAL'.

      lt_gm_item-material      = lt_ltap-matnr.
      lt_gm_item-batch         = lt_ltap-charg.
      lt_gm_item-entry_qnt     = lt_ltap-nsola.
      lt_gm_item-entry_uom     = lt_ltap-meins.
*      lt_gm_item-stge_type_st  = 'PRO'.
*      lt_gm_item-stge_bin_st   = 'DITA2_SAI'.
      lt_gm_item-spec_mvmt     = 'D'.


      lt_gm_item-entry_uom_iso = lt_gm_item-entry_uom.
      APPEND lt_gm_item.
    ENDLOOP.

**  Trf de Stock para deposito de Qualidade
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ls_gm_header
        goodsmvt_code    = lv_code
      IMPORTING
        materialdocument = lv_mblnr
        matdocumentyear  = lv_mjahr
      TABLES
        goodsmvt_item    = lt_gm_item
        return           = lt_gm_return.

    IF lv_mblnr IS INITIAL.
      READ TABLE lt_gm_return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        MESSAGE ID lt_gm_return-id TYPE 'W' NUMBER lt_gm_return-number INTO ls_zwm084-errormsg
        WITH lt_gm_return-message_v1 lt_gm_return-message_v2 lt_gm_return-message_v3 lt_gm_return-message_v4.
      ENDIF.
    ENDIF.

    lv_subobject = lc_subobject5.

*** OT para Zona de Rejeição
***********************************************************************
*  ELSEIF t_ltap_vb-nltyp = 'REJ'.


** OT para Buffer de Picking
**********************************************************************
  ELSEIF t_ltap_vb-nltyp = 'BPK'.

    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = t_ltap_vb-lgnum
      AND   tanum = t_ltap_vb-tanum.

    " Palete de Picking
    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM zwm026 INTO TABLE lt_zwm026
        FOR ALL ENTRIES IN lt_ltap
        WHERE armazem = lt_ltap-lgnum
        AND   sscc    = lt_ltap-nlenr.
    ENDIF.

    " Items
    LOOP AT lt_ltap.
      CLEAR lt_ltap_creat.
      lt_ltap_creat-werks = lt_ltap-werks.
      lt_ltap_creat-lgort = lt_ltap-lgort.
      lt_ltap_creat-matnr = lt_ltap-matnr.
      lt_ltap_creat-charg = lt_ltap-charg.
      lt_ltap_creat-anfme = lt_ltap-nsola.
      lt_ltap_creat-altme = lt_ltap-altme.
      lt_ltap_creat-vltyp = lt_ltap-nltyp.
      lt_ltap_creat-vlpla = lt_ltap-nlpla.
      lt_ltap_creat-vlenr = lt_ltap-nlenr.
      lt_ltap_creat-nltyp = '932'.
      lt_ltap_creat-nlpla = 'ORIGEM'.
      lt_ltap_creat-letyp = lt_ltap-letyp.
      APPEND lt_ltap_creat.
    ENDLOOP.

    lv_lznum = lt_ltap-nlenr.

    READ TABLE lt_zwm026 INDEX 1.
    IF sy-subrc = 0.

      CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
        EXPORTING
          i_lgnum       = t_ltap_vb-lgnum
          i_bwlvs       = '965' "lv_bwlvs
          i_lznum       = lv_lznum
          i_refnr       = lt_zwm026-grupo
        IMPORTING
          e_tanum       = lv_tanum
        TABLES
          t_ltap_creat  = lt_ltap_creat
        EXCEPTIONS
          error_message = 99.

      IF lv_tanum IS NOT INITIAL.

        " Atualizar Tabela de picking de Palete Tranferida para BPK
        LOOP AT lt_zwm026.
          UPDATE zwm026 SET trf_bpk = 'X'
          WHERE armazem       = lt_zwm026-armazem
          AND   n_pal_picking = lt_zwm026-n_pal_picking
          AND   i_pal_picking = lt_zwm026-i_pal_picking
          AND   grupo         = lt_zwm026-grupo
          AND   remessa       = lt_zwm026-remessa
          AND   posnr         = lt_zwm026-posnr
          AND   sub_item      = lt_zwm026-sub_item.
        ENDLOOP.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.

      ELSE.
        MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno INTO ls_zwm084-errormsg
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDIF.

** Log
**********************************************************************
  IF lv_subobject IS NOT INITIAL.

    CLEAR lt_return.
    lt_return-msgtyp = 'S'.
    lt_return-msgid  = 'ZWMMSG001'.
    lt_return-msgnr  = '000'.
    lt_return-msgv1  = 'IN - OT'.
    lt_return-msgv2  = ls_ltak-tanum.
    APPEND lt_return.

    READ TABLE lt_ltap INDEX 1.
    IF sy-subrc = 0.
      CLEAR lt_return.
      lt_return-msgtyp = 'S'.
      lt_return-msgid  = 'ZWMMSG001'.
      lt_return-msgnr  = '000'.
      lt_return-msgv1  = 'IN - SSCC'.
      lt_return-msgv2  = lt_ltap-nlenr.
      APPEND lt_return.
    ENDIF.

    IF ls_zwm084-errormsg IS NOT INITIAL.
      CLEAR lt_return.
      lt_return-msgtyp = 'E'.
      lt_return-msgid  = sy-msgid.
      lt_return-msgnr  = sy-msgno.
      lt_return-msgv1  = sy-msgv1.
      lt_return-msgv2  = sy-msgv2.
      lt_return-msgv3  = sy-msgv3.
      lt_return-msgv4  = sy-msgv4.
      APPEND lt_return.
    ELSE.

      IF lv_tanum IS NOT INITIAL.
        CLEAR lt_return.
        lt_return-msgtyp = 'S'.
        lt_return-msgid  = 'ZWMMSG001'.
        lt_return-msgnr  = '000'.
        lt_return-msgv1  = 'OUT - OT'.
        lt_return-msgv2  = lv_tanum.
        APPEND lt_return.

        LOOP AT lt_ltap_creat.
          CLEAR lt_return.
          lt_return-msgtyp = 'S'.
          lt_return-msgid  = 'ZWMMSG001'.
          lt_return-msgnr  = '000'.
          lt_return-msgv1  = 'OUT - SSCC'.
          lt_return-msgv2  = lt_ltap_creat-vlenr.
          APPEND lt_return.
        ENDLOOP.

      ELSEIF lv_mblnr IS NOT INITIAL.
        CLEAR lt_return.
        lt_return-msgtyp = 'S'.
        lt_return-msgid  = 'ZWMMSG001'.
        lt_return-msgnr  = '000'.
        lt_return-msgv1  = 'OUT - Documento Trf.'.
        lt_return-msgv2  = lv_mblnr.
        APPEND lt_return.
      ENDIF.
    ENDIF.

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
  ENDIF.

** Registar Entrada da Palete
**********************************************************************
  IF lv_tanum IS NOT INITIAL.

    IF ls_zwm077 IS NOT INITIAL.
      ls_zwm077-pvqui = 'X'.
      ls_zwm077-edatu = sy-datum.
      ls_zwm077-ezeit = sy-uzeit.
      ls_zwm077-ename = sy-uname.

      MODIFY zwm077 FROM ls_zwm077.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

    e_tanum = lv_tanum.
  ENDIF.

** Erros para Reprocessamento
  lv_errormsg = ls_zwm084-errormsg.

  IF lv_errormsg IS NOT INITIAL.

    SELECT SINGLE *
      FROM zwm084 INTO ls_zwm084
      WHERE lgnum = ls_ltak-lgnum
      AND   tanum = ls_ltak-tanum.

    IF sy-subrc <> 0.
      ls_zwm084-lgnum = ls_ltak-lgnum.
      ls_zwm084-tanum = ls_ltak-tanum.

      ls_zwm084-datum = sy-datum.
      ls_zwm084-uzeit = sy-uzeit.

      READ TABLE lt_ltap INDEX 1.
      IF sy-subrc = 0.
        ls_zwm084-exidv = lt_ltap-nlenr.
        ls_zwm084-lgpla = lt_ltap-vlpla.
      ENDIF.

      MODIFY zwm084 FROM ls_zwm084.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.

    ELSE.
      UPDATE zwm084 SET errormsg = lv_errormsg
      WHERE lgnum = ls_ltak-lgnum
      AND   tanum = ls_ltak-tanum.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

  ELSE.

    DELETE FROM zwm084 WHERE lgnum = ls_ltak-lgnum
                       AND   tanum = ls_ltak-tanum.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.

ENDFUNCTION.
