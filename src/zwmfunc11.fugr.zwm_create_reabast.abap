FUNCTION zwm_create_reabast.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LGTYP) TYPE  LGTYP
*"     REFERENCE(I_LGPLA) TYPE  LGPLA
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_mlgt       TYPE TABLE OF mlgt,
        lt_ltap       TYPE TABLE OF ltap,
        lt_lqua       TYPE TABLE OF lqua,
        lt_sscc       TYPE TABLE OF zwm_sscc,
        lt_ltap_cancl TYPE TABLE OF ltap_cancl,
        lt_lgtyp      TYPE mawm_int_tt_lgtyp. " << INS ROFF(SDF):TMGP:30.12.2015 13:47:52

  DATA lr_lgtyp TYPE RANGE OF lgtyp. " << INS ROFF(SDF):TMGP:30.12.2015 13:47:52

  DATA t_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.
  DATA c_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

  DATA: ls_mara       TYPE mara,
        ls_mard       TYPE mard,
        ls_ltak       TYPE ltak,
        ls_ltap       TYPE ltap,
        ls_mlgn       TYPE mlgn,
        ls_mlgt       TYPE mlgt,
        ls_zwm001     TYPE zwm001,
        ls_zwm020     TYPE zwm020,
        ls_sscc       TYPE zwm_sscc,
        ls_message    TYPE bdcmsgcoll,
        ls_ltap_cancl TYPE ltap_cancl.

  DATA: lv_posi           TYPE sytabix,
        lv_bin            TYPE char14,
        lv_matnr          TYPE matnr,
        lv_tanum          TYPE tanum,
        lv_num_quantos    TYPE i,
        lv_werks          TYPE werks_d,
        lv_lgort          TYPE lgort_d,
        lv_lgtyp1         TYPE lgtyp,
        lv_lgtyp2         TYPE lgtyp,
        lv_bwlvs          TYPE bwlvs,
        lv_num_pal_reab_v TYPE i,
        lv_num_pal_reab_f TYPE i,
        lv_num            TYPE sytabix,
        lv_lines          TYPE sytabix,
        lv_n_pal          TYPE i,
        lv_to             TYPE tanum.

  FIELD-SYMBOLS <fs_lgtyp> LIKE LINE OF lt_lgtyp[]. " << INS ROFF(SDF):TMGP:30.12.2015 13:47:52
  FIELD-SYMBOLS <fs_rlgty> LIKE LINE OF lr_lgtyp[]. " << INS ROFF(SDF):TMGP:30.12.2015 13:47:52

** Parametros Z
***********************************************************************
  SELECT SINGLE * FROM zwm001
                  INTO ls_zwm001
                  WHERE armazem   = i_lgnum AND
                        processo  = 'GERAL' AND
                        parametro = 'PLANT'.
  IF sy-subrc = 0.
    lv_werks = ls_zwm001-valor.
  ENDIF.

  SELECT SINGLE * FROM zwm001
                  INTO ls_zwm001
                  WHERE armazem   = i_lgnum AND
                        processo  = 'GERAL' AND
                        parametro = 'LGORT'.
  IF sy-subrc = 0.
    lv_lgort = ls_zwm001-valor.
  ENDIF.

  SELECT SINGLE * FROM zwm001
                  INTO ls_zwm001
                  WHERE armazem   = i_lgnum AND
                        processo  = 'REABASTECIMENTO' AND
                        parametro = 'MOV'.
  IF sy-subrc = 0.
    lv_bwlvs = ls_zwm001-valor.
  ENDIF.

  SELECT SINGLE * FROM zwm001
                  INTO ls_zwm001
                  WHERE armazem   = i_lgnum AND
                        processo  = 'REABASTECIMENTO' AND
                        parametro = 'ST_PCK'.
  IF sy-subrc = 0.
    lv_lgtyp1 = ls_zwm001-valor.
  ENDIF.

  SELECT SINGLE * FROM zwm001
                  INTO ls_zwm001
                  WHERE armazem   = i_lgnum AND
                        processo  = 'REABASTECIMENTO' AND
                        parametro = 'ST_PKR'.
  IF sy-subrc = 0.
    lv_lgtyp2 = ls_zwm001-valor.
  ENDIF.

  SELECT SINGLE * FROM zwm001
                  INTO ls_zwm001
                  WHERE armazem   = i_lgnum AND
                        processo  = 'REABASTECIMENTO' AND
                        parametro = 'PKB'.
  IF sy-subrc = 0.
    lv_num_pal_reab_v = ls_zwm001-valor.
  ENDIF.

  SELECT SINGLE * FROM zwm001
                  INTO ls_zwm001
                  WHERE armazem   = i_lgnum AND
                        processo  = 'REABASTECIMENTO' AND
                        parametro = 'PKR'.
  IF sy-subrc = 0.
    lv_num_pal_reab_f = ls_zwm001-valor.
  ENDIF.

** Bin Externo
***********************************************************************
  CALL FUNCTION 'ZWM_CONCATENATE_BIN'
    EXPORTING
      lgtyp = i_lgtyp
      lgpla = i_lgpla
    IMPORTING
      bin   = lv_bin.


** Retorna Material
***********************************************************************
  SELECT * INTO TABLE lt_mlgt
      FROM mlgt
          WHERE lgnum = i_lgnum
            AND lgtyp = i_lgtyp
            AND lgpla = i_lgpla.

  IF sy-subrc <> 0.
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWMMSG001'.
    ls_message-msgnr = '223'.
    ls_message-msgv1 = lv_bin.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  DESCRIBE TABLE lt_mlgt LINES lv_posi.

  IF lv_posi > 1.
*   Posição & tem mais que um material associado!
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWMMSG001'.
    ls_message-msgnr = '238'.
    ls_message-msgv1 = lv_bin.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  READ TABLE lt_mlgt INTO ls_mlgt INDEX 1.
  lv_matnr = ls_mlgt-matnr.

  SELECT SINGLE * FROM mara
                  INTO ls_mara
                  WHERE matnr = lv_matnr.


  SELECT SINGLE * FROM mlgn
                  INTO ls_mlgn
                  WHERE lgnum = i_lgnum AND
                        matnr = lv_matnr.

** Reabastecimento
***********************************************************************
  SELECT COUNT(*) FROM lagp INTO lv_posi
      WHERE lgnum = i_lgnum AND
            lgtyp = 'REP' AND
            kzler = 'X' AND
            kzvol = ' ' AND
            anzqu = 0   AND
            skzue = ' ' AND
            skzsi = ' ' AND
            skzse = ' '.

  IF lv_posi <= 6.
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWMMSG001'.
    ls_message-msgnr = '206'.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  SELECT * FROM lqua INTO TABLE lt_lqua
          WHERE lgnum = i_lgnum
            AND lgtyp = lv_lgtyp2
            AND matnr = lv_matnr.

  DESCRIBE TABLE lt_lqua LINES lv_lines.


  SELECT SINGLE * FROM mlgn
                  INTO ls_mlgn
                  WHERE matnr = lv_matnr AND
                        lgnum = i_lgnum.

  IF z_wm_cl_management=>is_remontada( is_data = ls_mlgn ) EQ abap_true.
    lv_num_pal_reab_f = lv_num_pal_reab_f * 2.
  ENDIF.

*      CHECK lv_lines < num_pal_reab_f.

  IF lv_lines >= lv_num_pal_reab_f.
*  Quantidade de reabastecimento do material & atingida!
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWMMSG001'.
    ls_message-msgnr = '228'.
    ls_message-msgv1 = lv_matnr.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  SELECT * FROM ltap
     INTO TABLE lt_ltap
     WHERE lgnum = i_lgnum AND
            matnr = lv_matnr AND
            pquit = ' '.

  IF NOT lt_ltap IS INITIAL.
    LOOP AT lt_ltap INTO ls_ltap.
      CLEAR ls_ltak.
      SELECT SINGLE * FROM ltak
                      INTO ls_ltak
                      WHERE lgnum = ls_ltap-lgnum AND
                            tanum = ls_ltap-tanum.

      IF sy-subrc = 0 AND ls_ltak-bwlvs = lv_bwlvs
                      AND ls_ltak-kquit = ' '.
        lv_lines = lv_lines + 1.
      ENDIF.
    ENDLOOP.
  ENDIF.

*      CHECK lv_lines < num_pal_reab_f.
  IF lv_lines >= lv_num_pal_reab_f.
*   Quantidade de reabastecimento do material & atingida!
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWMMSG001'.
    ls_message-msgnr = '228'.
    ls_message-msgv1 = lv_matnr.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  lv_num = lv_num_pal_reab_f - lv_lines.

  SELECT SINGLE * FROM mard
                  INTO ls_mard
                  WHERE matnr = lv_matnr AND
                       werks = lv_werks AND
                       lgort = lv_lgort.

  IF ls_mard-labst IS INITIAL.
*  Não existe stock em MM para o material & !

    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWMMSG001'.
    ls_message-msgnr = '231'.
    ls_message-msgv1 = lv_matnr.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

*& Begin of Modification by Tiago Pateiro - ROFF @ 30.12.2015 13:39:18
*/ Adaptar logica para contemplar Renova França
  CALL FUNCTION 'Z_WM_T334T_GET_LGTYP'
    EXPORTING
      i_lgnum  = i_lgnum
      i_bwref  = lv_bwlvs
      i_matnr  = lv_matnr
    IMPORTING
      et_lgtyp = lt_lgtyp[].
  IF lt_lgtyp[] IS NOT INITIAL.
    CLEAR lv_num_quantos.

    LOOP AT lt_lgtyp[] ASSIGNING <fs_lgtyp>.
      APPEND INITIAL LINE TO lr_lgtyp[] ASSIGNING <fs_rlgty>.
      <fs_rlgty>-sign   = 'I'.
      <fs_rlgty>-option = 'EQ'.
      <fs_rlgty>-low    = <fs_lgtyp>.
    ENDLOOP.

    SELECT COUNT(*)
      FROM lqua INTO lv_num_quantos
      WHERE matnr EQ lv_matnr
        AND lgnum EQ i_lgnum
        AND lgtyp IN lr_lgtyp[] " partial index
        AND verme GT 0.
  ELSE.
*& End of Modification by Tiago Pateiro - ROFF @ 30.12.2015 13:39:29
    CLEAR lv_num_quantos.
    SELECT COUNT(*) FROM lqua INTO lv_num_quantos
            WHERE lgnum = i_lgnum AND
                  matnr = lv_matnr AND
                  ( lgtyp = 'TRI' OR
                    lgtyp = 'DRI' OR
                    lgtyp = 'BLK' ) AND
                    verme > 0.

*      CHECK lv_num <= num_quantos.
*& Begin of Modification by Tiago Pateiro - ROFF @ 30.12.2015 13:48:56
*/ Adaptar logica para contemplar Renova França
  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 30.12.2015 13:48:59

  IF lv_num > lv_num_quantos.
*  Não existe stock em WM para o material & !
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWMMSG001'.
    ls_message-msgnr = '232'.
    ls_message-msgv1 = lv_matnr.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.


  DO lv_num TIMES.
    CLEAR: lt_sscc, ls_sscc, lv_tanum.

    ls_sscc-material   = lv_matnr.
    ls_sscc-quantidade = 1.


    ls_sscc-tipo_su = ls_mlgn-lety1.

    IF z_wm_cl_management=>is_remontada( is_data = ls_mlgn ) eq abap_true.
      ls_sscc-quantidade = 1.
      ls_sscc-uni        = 'PAL'.
      APPEND ls_sscc TO lt_sscc.

      ls_sscc-quantidade = 1.
      ls_sscc-uni        = 'PAL'.
    ELSE.
      ls_sscc-uni        = ls_mara-meins.
    ENDIF.
    APPEND ls_sscc TO lt_sscc.

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse  = i_lgnum
        mov_type   = lv_bwlvs                  "827
        plant      = lv_werks                  "RENV
        s_loc      = lv_lgort                  "CD
      IMPORTING
        to         = lv_tanum
      TABLES
        return_msg = et_messages
        sscc       = lt_sscc
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    IF sy-subrc <> 0.
      DELETE et_messages WHERE msgtyp <> 'E' AND  msgtyp <> 'A'.
      RAISE error.
    ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 25.06.2012 12:32:09
*  Motivo: Prioridade de Reabastecimento
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_TO_SET_LET_DOWN_PRIORITY'
      EXPORTING
        i_lgnum  = i_lgnum
        i_tanum  = lv_tanum
        i_commit = abap_true
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


*** Verificar se é remontada
    CLEAR: t_ltap, c_ltap.
    REFRESH: t_ltap, c_ltap.

    SELECT * APPENDING TABLE t_ltap
        FROM ltap
            WHERE lgnum = i_lgnum AND
                  tanum = lv_tanum.

    LOOP AT t_ltap
        WHERE vltyp = 'TRI' AND
              ( letyp in z_wm_cl_management=>r_letyp_remontada( i_lgnum ) ) AND
              vbeln = ' '.

      CLEAR ls_zwm020.
      SELECT SINGLE * FROM zwm020
                      INTO ls_zwm020
                      WHERE armazem = t_ltap-lgnum AND
                            ( p1 = t_ltap-vlenr OR p2 = t_ltap-vlenr ).

      IF sy-subrc = 0.
        CLEAR c_ltap.
        REFRESH c_ltap.
        MOVE-CORRESPONDING t_ltap TO c_ltap.
        APPEND c_ltap.
        CLEAR c_ltap.

        IF ls_zwm020-p1 = t_ltap-vlenr.
** Verificar se existem duas to´s para as duas paletes remontadas

** Se tiverem as duas paletes tem de se estornar as duas tos e voltar a
** criar as duas to's sendo a primeira a da to de baixo

          READ TABLE t_ltap WITH KEY vlenr = ls_zwm020-p2.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING t_ltap TO c_ltap.
            APPEND c_ltap.
            CLEAR c_ltap.
          ENDIF.

        ELSEIF ls_zwm020-p2 = t_ltap-vlenr.

          READ TABLE t_ltap WITH KEY vlenr = ls_zwm020-p1.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING t_ltap TO c_ltap.
            APPEND c_ltap.
            CLEAR c_ltap.
          ENDIF.
        ENDIF.
      ENDIF.

** Verificar se estao as duas paletes remontadas para a saida.
** Se for as duas paletes para saida verificar se ja estao criadas pela
** ordem correcta
      CLEAR lv_n_pal.
      DESCRIBE TABLE c_ltap LINES lv_n_pal.

      IF lv_n_pal = 2.
        SORT c_ltap BY lgnum tanum tapos.
        READ TABLE c_ltap INDEX 1.
        IF c_ltap-vlenr = ls_zwm020-p1.
          CONTINUE.
        ENDIF.
      ENDIF.

      CLEAR: lt_sscc, ls_sscc.
      LOOP AT c_ltap.

**************************
**   Cancelar as OT
**************************

        CLEAR: lt_ltap_cancl, ls_ltap_cancl.

        ls_ltap_cancl-tanum = c_ltap-tanum.
        ls_ltap_cancl-tapos = c_ltap-tapos.
        APPEND ls_ltap_cancl TO lt_ltap_cancl.
        CLEAR ls_ltap_cancl.

        CALL FUNCTION 'ZWM_CANCEL_TO'
          EXPORTING
            armazem      = c_ltap-lgnum
          TABLES
            t_ltap_cancl = lt_ltap_cancl
          EXCEPTIONS
            error        = 1
            OTHERS       = 2.

        IF sy-subrc <> 0.
          ls_message-msgtyp = 'E'.
          ls_message-msgspra = sy-langu.
          ls_message-msgid = sy-msgid.
          ls_message-msgnr = sy-msgno.
          ls_message-msgv1 = sy-msgv1.
          ls_message-msgv2 = sy-msgv2.
          ls_message-msgv3 = sy-msgv3.
          ls_message-msgv4 = sy-msgv4.
          APPEND ls_message TO et_messages.
          RAISE error.
        ENDIF.
      ENDLOOP.

**************************
**   Tabela para Criar Novas OT´s
**************************
      READ TABLE c_ltap WITH KEY vlenr = ls_zwm020-p1.
      IF sy-subrc = 0.
        ls_sscc-sscc = c_ltap-vlenr.
        ls_sscc-tipo_su = c_ltap-letyp.
        ls_sscc-material = c_ltap-matnr.
        ls_sscc-quantidade = c_ltap-vsola.
        ls_sscc-uni = c_ltap-altme.
        ls_sscc-lote_producao = c_ltap-charg.
        APPEND ls_sscc TO lt_sscc.
        CLEAR: ls_sscc.
      ENDIF.

      CLEAR c_ltap.
      READ TABLE c_ltap WITH KEY vlenr = ls_zwm020-p2.
      IF sy-subrc = 0.
        ls_sscc-sscc = c_ltap-vlenr.
        ls_sscc-tipo_su = c_ltap-letyp.
        ls_sscc-material = c_ltap-matnr.
        ls_sscc-quantidade = c_ltap-vsola.
        ls_sscc-uni = c_ltap-altme.
        ls_sscc-lote_producao = c_ltap-charg.
        APPEND ls_sscc TO lt_sscc.
        CLEAR ls_sscc.
      ENDIF.

      CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse   = i_lgnum
          mov_type    = lv_bwlvs         "827
          st_type_o   = c_ltap-vltyp
          bin_origem  = c_ltap-vlpla
          st_type_d   = c_ltap-nltyp
          bin_destino = c_ltap-nlpla
          plant       = c_ltap-werks
          s_loc       = c_ltap-lgort
          origem      = 'X'
        IMPORTING
          to          = lv_to
        TABLES
          return_msg  = et_messages
          sscc        = lt_sscc
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.
        DELETE et_messages WHERE msgtyp <> 'E' AND  msgtyp <> 'A'.
        RAISE error.
      ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 25.06.2012 12:32:09
*  Motivo: Prioridade de Reabastecimento
*--------------------------------------------------------------------*
      CALL FUNCTION 'ZWM_TO_SET_LET_DOWN_PRIORITY'
        EXPORTING
          i_lgnum  = i_lgnum
          i_tanum  = lv_to
          i_commit = 'X'
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

      CLEAR: lt_sscc, ls_sscc.

      DELETE t_ltap WHERE vlenr = ls_zwm020-p1 OR vlenr = ls_zwm020-p2.
      CLEAR c_ltap.
      REFRESH c_ltap.
    ENDLOOP.
  ENDDO.
ENDFUNCTION.
