*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1F03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CRIA_TO_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_to_picking .

  DATA : valor LIKE zwm001-valor.
  DATA adicional LIKE ltak-lznum.

  WAIT UP TO 1 SECONDS.

  CLEAR : st_type_o,
          st_type_d,
          bin_o,
          plant,
          lgort,
          sscc,
          st_type,
          movimento,
          valor,
          certificado,
          adicional.

  REFRESH : sscc.


** Storage type associado ao pulmão de reposição
  PERFORM get_parameter USING xuser-lgnum
                           'ENTRADA_ARMAZEM'
                           'ST_REP'
                           valor.
  WRITE valor TO st_type LEFT-JUSTIFIED.


  CLEAR valor.
** Movimento para TO de picking
  PERFORM get_parameter USING xuser-lgnum
                           'PICKING'
                           'MOV_WM'
                           valor.
  WRITE valor TO movimento LEFT-JUSTIFIED.

** dados de item
  CLEAR ltap.
  SELECT * FROM ltap
           WHERE lgnum = xuser-lgnum AND
                 tanum = to_ret.
    IF sy-subrc = 0.

      IF ltap-nltyp = st_type.

        st_type_o = ltap-nltyp.
        bin_o = ltap-nlpla.
        plant = ltap-werks.
        lgort = ltap-lgort.

        CLEAR lagp.
        SELECT SINGLE lzone INTO certificado
          FROM lagp WHERE lgnum = xuser-lgnum AND
                          lgtyp = ltap-nltyp AND
                          lgpla = ltap-nlpla.



        sscc-sscc = ltap-nlenr.
        sscc-tipo_su = ltap-letyp.
        sscc-material = ltap-matnr.
        CLEAR sscc-variante.
        sscc-quantidade = ltap-vsola.
        sscc-uni = ltap-altme.
        CLEAR sscc-altura.
        sscc-lote_producao = ltap-charg.
        APPEND sscc.

      ELSE.
        EXIT.
      ENDIF.

    ENDIF.

  ENDSELECT.

  CLEAR return_msg.
  REFRESH return_msg.

  CLEAR st_type_d.
  IF NOT sscc[] IS INITIAL.
    CLEAR ltak.
    SELECT SINGLE * FROM ltak
          WHERE lgnum = xuser-lgnum AND
                tanum = ltap-tanum.

    IF ltak-betyp <> 'Z'. " Sem referencia a um Grupo
      CLEAR valor.
** Movimento para TO de picking
      PERFORM get_parameter USING xuser-lgnum
                               'PICKING'
                               'MOV_WM'
                               valor.
      WRITE valor TO movimento LEFT-JUSTIFIED. "919
** Verificar se tem quantidade no PCK
      CLEAR mlgt.
      SELECT SINGLE *
          FROM mlgt
              WHERE matnr = ltap-matnr AND
                    lgnum = ltak-lgnum AND
                    lgtyp = 'PCK'.

      IF NOT mlgt-lgpla IS INITIAL.
        CLEAR lqua.
        SELECT SINGLE *
            FROM lqua
                WHERE lgnum = mlgt-lgnum AND
                      matnr = mlgt-matnr AND
                      lgtyp = mlgt-lgtyp AND
                      lgpla = mlgt-lgpla.
        IF sy-subrc = 0.
          st_type_d = 'PKR'.
        ELSE.
          st_type_d = mlgt-lgtyp.
        ENDIF.
      ELSE.
        CLEAR mlgn.
        SELECT SINGLE *
            FROM mlgn
                WHERE matnr = ltap-matnr AND
                      lgnum = ltak-lgnum.
        IF mlgn-plkpt = 'PKB'.
          CLEAR valor.
** Movimento para TO de reabastecimento picking variavel
          PERFORM get_parameter USING xuser-lgnum
                                          'PICKING_REAB_VA'
                                          'MOV_WM1'
                                          valor.
          WRITE valor TO movimento LEFT-JUSTIFIED. "828

        ENDIF.
      ENDIF.
    ELSE.   " com referencia a um grupo
      CLEAR adicional.
      adicional = ltak-lznum.

      CLEAR mlgn.
      SELECT SINGLE *
          FROM mlgn
              WHERE matnr = ltap-matnr AND
                    lgnum = ltak-lgnum.
      IF mlgn-plkpt = 'PKB'.
        CLEAR: valor, movimento.
** Movimento para TO de reabastecimento picking variavel
        PERFORM get_parameter USING xuser-lgnum
                                        'PICKING_REAB_VA'
                                        'MOV_WM'
                                        valor.
        WRITE valor TO movimento LEFT-JUSTIFIED. "977

      ELSE.
        CLEAR: valor, movimento.
** Movimento para TO de reabastecimento picking fixo
        PERFORM get_parameter USING xuser-lgnum
                                 'PICKING_REAB'
                                 'MOV_WM'
                                 valor.
        WRITE valor TO movimento LEFT-JUSTIFIED. "973
      ENDIF.
    ENDIF.

    DO 10 TIMES.
      IF sy-index > 1.
        WAIT UP TO 1 SECONDS.
      ENDIF.

      CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse      = xuser-lgnum
          mov_type       = movimento
          st_type_o      = st_type_o
          bin_origem     = bin_o
          st_type_d      = st_type_d
          plant          = plant
          s_loc          = lgort
          req_number     = ltak-benum
          req_type       = ltak-betyp
          certificado    = certificado
          sscc_adicional = adicional
        IMPORTING
          to             = to
        TABLES
          return_msg     = return_msg
          sscc           = sscc
        EXCEPTIONS
          error          = 1
          OTHERS         = 2.

      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.

      CLEAR: lt_messages.
      LOOP AT return_msg.
        CLEAR: ls_message.
        ls_message-msgtyp = 'E'.
        ls_message-msgid  = return_msg-msgid.
        ls_message-msgnr  = return_msg-msgnr.
        ls_message-msgv1  = return_msg-msgv1.
        ls_message-msgv2  = return_msg-msgv2.
        ls_message-msgv3  = return_msg-msgv3.
        ls_message-msgv4  = return_msg-msgv4.
        APPEND ls_message TO lt_messages.
      ENDLOOP.

      CALL FUNCTION 'ZWM_LOG_MESSAGE'
        EXPORTING
          i_master    = xuser-lgnum
          i_object    = 'ZWM001'
          i_subobject = 'ZWM007'
          i_extnumber = ltak-benum
          i_commit    = 'X'
          it_messages = lt_messages.

    ENDDO.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 25.06.2012 14:45:06
*  Motivo: Passagem de Prioridade
*--------------------------------------------------------------------*
    IF NOT ltak-tapri IS INITIAL.
      CALL FUNCTION 'ZWM_TAPRI_CHANGE'
        EXPORTING
          i_lgnum  = xuser-lgnum
          i_tanum  = to
          i_tapri  = ltak-tapri
          i_commit = abap_true
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  ENDIF.

  CLEAR : st_type_o, st_type_d, bin_o, plant, lgort,
          sscc, return_msg.
  REFRESH : sscc, return_msg.


ENDFORM.                    " CRIA_TO_PICKING
*&---------------------------------------------------------------------*
*&      Form  cria_to_picking_variavel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_to_picking_variavel USING p_lgpla p_lgtyp .

  DATA lv_werks TYPE werks_d. " << INS ROFF(SDF):TMGP:13.01.2016 14:41:32
  DATA lv_lgort   TYPE lgort_d.
  DATA quantidade LIKE ltap-vsola.
  DATA t_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

  CLEAR: sscc, quantidade, t_ltap.
  REFRESH: sscc, l_ltap.

*& Begin of Modification by Tiago Pateiro - ROFF @ 13.01.2016 14:41:37
**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO lv_werks
**    WHERE lgort EQ 'CD'
**      AND lgnum EQ xuser-lgnum.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    lv_werks = 'RENV'.
**  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 13.01.2016 14:41:39

  WAIT UP TO 1 SECONDS.
  SELECT * INTO TABLE t_ltap
      FROM ltap
          WHERE lgnum = xuser-lgnum AND
                tanum = to_ret.

  READ TABLE t_ltap INDEX 1.

  lv_werks = t_ltap-werks.
  lv_lgort = t_ltap-lgort.


  CLEAR zwm020.
  SELECT SINGLE *
      FROM zwm020
          WHERE armazem = xuser-lgnum AND
                ( p1 = t_ltap-vlenr OR
                  p2 = t_ltap-vlenr ).
  IF sy-subrc = 0.
    DELETE t_ltap WHERE vlenr <> zwm020-p2.
  ENDIF.

  LOOP AT t_ltap.
    SELECT SINGLE *
        FROM ltak
            WHERE lgnum = xuser-lgnum AND
                  tanum = t_ltap-tanum.

    quantidade = trunc( ltak-lznum ).

    sscc-sscc = t_ltap-vlenr.
    sscc-tipo_su = t_ltap-letyp.
    sscc-material = t_ltap-matnr.
    CLEAR sscc-variante.
    sscc-quantidade = quantidade.
    sscc-uni = t_ltap-meins.
    CLEAR sscc-altura.
    sscc-lote_producao = t_ltap-charg.
    APPEND sscc.
    CLEAR sscc.
  ENDLOOP.


  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse  = xuser-lgnum
      mov_type   = '976'
      st_type_o  = t_ltap-nltyp
      bin_origem = t_ltap-nlpla
*     plant      = 'RENV' " << DEL ROFF(SDF):TMGP:13.01.2016 14:42:36
      plant      = lv_werks " << INS ROFF(SDF):TMGP:13.01.2016 14:42:37
      s_loc      = lv_lgort
      origem     = 'X'
      req_number = ltak-benum
      req_type   = ltak-betyp
    IMPORTING
      to         = to
    TABLES
      return_msg = return_msg
      sscc       = sscc
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
*    EXIT.
  ENDIF.

  DATA uni_incom_p TYPE zpalete_picking-uni_incom_p.
  quantidade = ltak-lznum.
  uni_incom_p = quantidade MOD 1.

  IF uni_incom_p IS NOT INITIAL.

    DATA: lt_ltak LIKE ltak OCCURS 0 WITH HEADER LINE,
          lt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

    CLEAR:   sscc, quantidade.
    REFRESH: sscc.

    LOOP AT t_ltap.
      SELECT SINGLE *
          FROM ltak
              WHERE lgnum = xuser-lgnum AND
                    tanum = t_ltap-tanum.

      sscc-sscc = t_ltap-vlenr.
      sscc-tipo_su = t_ltap-letyp.
      sscc-material = t_ltap-matnr.
      CLEAR sscc-variante.
      sscc-quantidade = 1.
      sscc-uni = t_ltap-meins.
      CLEAR sscc-altura.
      sscc-lote_producao = t_ltap-charg.
      APPEND sscc.
      CLEAR sscc.
    ENDLOOP.

    SELECT * INTO TABLE lt_ltak
        FROM ltak
            WHERE lgnum = xuser-lgnum
              AND refnr = ltak-benum.

    DELETE lt_ltak WHERE kquit IS NOT INITIAL AND queue <> 'QUEUE2'.

    IF lt_ltak[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_ltap
          FROM ltap
              FOR ALL ENTRIES IN lt_ltak
              WHERE lgnum = xuser-lgnum
                AND tanum = lt_ltak-tanum.

      DELETE lt_ltap WHERE vltyp <> 'PKL'
                       OR  matnr <> t_ltap-matnr
                       OR  charg <> t_ltap-charg.

      READ TABLE lt_ltap INDEX 1.
    ENDIF.

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse   = xuser-lgnum
        mov_type    = '979'
        st_type_o   = t_ltap-nltyp
        bin_origem  = t_ltap-nlpla
        st_type_d   = lt_ltap-vltyp
        bin_destino = lt_ltap-vlpla
*       plant       = 'RENV' " << DEL ROFF(SDF):TMGP:13.01.2016 14:42:47
        plant       = lv_werks " << INS ROFF(SDF):TMGP:13.01.2016 14:42:49
        s_loc       = lv_lgort
        origem      = 'X'
        req_number  = ltak-benum
        req_type    = ltak-betyp
      IMPORTING
        to          = to
      TABLES
        return_msg  = return_msg
        sscc        = sscc
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 25.06.2012 14:45:06
*  Motivo: Passagem de Prioridade
*--------------------------------------------------------------------*
  IF NOT ltak-tapri IS INITIAL.
    CALL FUNCTION 'ZWM_TAPRI_CHANGE'
      EXPORTING
        i_lgnum  = xuser-lgnum
        i_tanum  = to
        i_tapri  = ltak-tapri
        i_commit = abap_true
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

ENDFORM.                    " cria_to_picking_variavel

*&---------------------------------------------------------------------*
*&      Form  acerto_int
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM acerto_int.

  DATA qtd_int LIKE lqua-verme.
  CLEAR: qtd_int, ltak, ltap, lqua.

  SELECT SINGLE *
      FROM ltak
          WHERE lgnum = xuser-lgnum AND
                tanum = to.

  IF ltak-bwlvs = '971' OR
     ltak-bwlvs = '973' OR
     ltak-bwlvs = '976'.

    SELECT SINGLE *
        FROM ltap
            WHERE lgnum = xuser-lgnum AND
                  tanum = to.

    SELECT SINGLE *
        FROM lqua
            WHERE lgnum = ltap-lgnum AND
                  lgtyp = 'INT' AND
                  lgpla = '000-000-01' AND
                  matnr = ltap-matnr AND
                  charg = ltap-charg.

    qtd_int = lqua-verme + lqua-einme.

    IF qtd_int > 0.

      CLEAR: sscc, mov.
      REFRESH sscc.

      sscc-material      = lqua-matnr.
      sscc-quantidade    = qtd_int.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = lqua-meins
          language       = sy-langu
        IMPORTING
          output         = sscc-uni
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

*      sscc-uni           = lqua-meins.
      sscc-lote_producao = lqua-charg.
      APPEND sscc.

      CLEAR: mlgn.
      SELECT SINGLE * FROM mlgn
              WHERE matnr = lqua-matnr
                AND lgnum = lqua-lgnum.
      IF mlgn-plkpt = 'PKB'.
        mov = '975'.
      ELSE.
        mov = '970'.
      ENDIF.

      CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse  = xuser-lgnum
          mov_type   = mov                                  "970 ou 975
          plant      = 'RENV'
          s_loc      = 'CD'
          req_number = ltak-benum
          req_type   = ltak-betyp
        IMPORTING
          to         = to
        TABLES
          return_msg = return_msg
          sscc       = sscc
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

    ENDIF.
  ENDIF.
ENDFORM.                    " acerto_int
