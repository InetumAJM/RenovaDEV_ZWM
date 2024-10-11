*----------------------------------------------------------------------*
*   INCLUDE Z02RP_RP_REPDITA_F01                                       *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CARREGA_SESSOES_DITA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_sessoes_dita.
  DATA: lv_tabix          LIKE sy-tabix.
  DATA: lt_z02rpconslt    LIKE z02rpconslt OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_mara OCCURS 0,
          matnr TYPE matnr,
          extwg TYPE mara-extwg,
        END OF lt_mara.

  DATA:
    ls_excpt TYPE ty_excpt.

  DATA: l_dummy LIKE z02rpsessaodita-divisao.

  REFRESH:
    gt_excpt[].

* INETUM - NR - 03.10.2023 - RENPRJ00041 - Inicio
  SELECT * FROM z02rpsessaodita INTO TABLE xsessaodita
***  SELECT * FROM z02rpsessaodita INTO CORRESPONDING FIELDS OF TABLE xsessaodita
* INETUM - NR - 03.10.2023 - RENPRJ00041 - Fim
                WHERE werks IN so_werks
                AND   area IN so_area
                AND   linha IN so_linha
                AND   turno IN so_turno
                AND   data IN so_data.

* INETUM - NR - 03.10.2023 - RENPRJ00041 - Inicio
  IF sy-subrc NE 0.
    MESSAGE i151(z02rp).
    LEAVE LIST-PROCESSING.
  ENDIF.

***  PERFORM obter_sessoes_sem_producao.

***  IF xsessaodita[] IS INITIAL AND xsessaodita_sem_prod_aux[] IS INITIAL.
***    MESSAGE i151(z02rp).
***    LEAVE LIST-PROCESSING.
***  ENDIF.
* INETUM - NR - 03.10.2023 - RENPRJ00041 - Fim

  SORT xsessaodita BY data turno area linha.

  "INS ROFF JCO - 11.04.2019 - RENPRM00002.
* Obter  registos da tabela de hardcodes para a validação do fluxo a escolher
  DATA: lt_hardcode_results TYPE zdev_hardcode_t.
  DATA: ls_results TYPE zhardcode_table.
  DATA: lv_meth        TYPE zdev_inc_meth,
        lv_valdt_charg TYPE flag,
        lv_low         TYPE rsdsselop_.

  CONSTANTS: lc_fname_excpt TYPE name_feld VALUE 'EXCPT'.

  lv_meth = sy-repid.
  CALL METHOD zhard_code=>get_data
    EXPORTING
      inc_meth      = lv_meth
      fieldname     = lc_fname_excpt
    IMPORTING
      zdev_hardcode = lt_hardcode_results.
  "END ROFF JCO - 11.04.2019 - RENPRM00002.

  LOOP AT xsessaodita.

    "INS ROFF JCO - 11.04.2019 - RENPRM00002.
    CLEAR ls_excpt.
    ls_excpt-divisao = xsessaodita-divisao.
    ls_excpt-werks   = xsessaodita-werks.
    ls_excpt-area    = xsessaodita-area.
    ls_excpt-linha   = xsessaodita-linha.

    CLEAR lv_low.
    CONCATENATE
      ls_excpt-divisao
      ls_excpt-werks
      ls_excpt-area
      ls_excpt-linha
      INTO lv_low
      SEPARATED BY '/'.

    CLEAR ls_results.
    READ TABLE lt_hardcode_results INTO ls_results
      WITH KEY ue_low = lv_low.
    IF sy-subrc EQ 0.
* fluxo alternativo
      ls_excpt-consprd = 'X'.
      IF ls_results-ue_high EQ lv_low.
* fluxo alternativo com requisição de papel
        ls_excpt-reqs = 'X'.
      ENDIF.
    ENDIF.
    APPEND ls_excpt TO gt_excpt.
    "END ROFF JCO - 11.04.2019 - RENPRM00002.

    IF ls_excpt-consprd EQ 'X'.
** verifica produções
      IF xsessaodita-producao NE '0'.
        SELECT divisao INTO l_dummy FROM z02rpconsprod
                            WHERE divisao = xsessaodita-divisao
                            AND   consprod = xsessaodita-numoper1
                            AND   anulado = space
                            AND   mblnr = space.
* existem registo de produção com erro (sem ducumento 101)
          xsessaodita-producao = '0'.
          EXIT.
        ENDSELECT.
      ENDIF.
    ELSE.
      IF xsessaodita-producao NE '0'.
        SELECT divisao INTO l_dummy FROM z02rpconsprodl
                            WHERE divisao = xsessaodita-divisao
                            AND   consprod = xsessaodita-numoper1
                            AND   anulado = space
                         AND   ( mblnr = space OR tanum = space ).
* existem registos de produção com erro (sem documento 261 ou OT)
          xsessaodita-producao = '0'.
          EXIT.
        ENDSELECT.
      ENDIF.
    ENDIF.

* verifica consumos lista técnica
    IF xsessaodita-aufnr1 = xsessaodita-ltecnica01 AND NOT
       xsessaodita-aufnr1 IS INITIAL.

** RSOUSA @ 18-04-2011 --> BEGIN
      REFRESH: lt_z02rpconslt, lt_mara.
      SELECT *
        FROM z02rpconslt INTO TABLE lt_z02rpconslt
        WHERE divisao  = xsessaodita-divisao  AND
              consprod = xsessaodita-numoper1 AND
              aufnr    = xsessaodita-aufnr1.

      DELETE lt_z02rpconslt WHERE anulado IS NOT INITIAL.
      DELETE lt_z02rpconslt WHERE mblnr   IS NOT INITIAL.


      IF lt_z02rpconslt[] IS NOT INITIAL.
        SELECT matnr extwg
          FROM mara INTO TABLE lt_mara
          FOR ALL ENTRIES IN lt_z02rpconslt
          WHERE matnr = lt_z02rpconslt-matnr.
*          Begin GN(ROFF) 15.05.2012
*        BREAK roff-sdf.
*        IF divisao <> 'DIPE' or area = 'F03'.
*        Begin GN(ROFF) 11.06.2012
        IF area IS INITIAL.
          area = xsessaodita-area.
        ENDIF.

        IF divisao IS INITIAL.
          divisao = xsessaodita-divisao.
        ENDIF.
*        End   GN(ROFF) 11.06.2012
*         IF divisao <> 'DIPE' or area <> 'F03'.
        IF divisao <> 'DIPE' AND area <> 'F03'.
*          End   GN(ROFF) 15.05.2012
          DELETE lt_mara WHERE extwg <> 'HIBRIDO'.
        ENDIF.
        SORT lt_mara BY matnr.
      ENDIF.

      LOOP AT lt_z02rpconslt.
        lv_tabix = sy-tabix.
        READ TABLE lt_mara WITH KEY matnr = lt_z02rpconslt-matnr.
        IF sy-subrc <> 0.
          DELETE lt_z02rpconslt INDEX lv_tabix.
        ENDIF.
      ENDLOOP.

      READ TABLE lt_z02rpconslt INDEX 1.
      IF sy-subrc = 0.
        xsessaodita-ltecnica01 = '0'.
      ENDIF.
** RSOUSA @ 18-04-2011 <-- END

*    SELECT divisao INTO l_dummy FROM z02rpconslt
*                        WHERE divisao = xsessaodita-divisao
*                        AND   consprod = xsessaodita-numoper1
*                        AND   aufnr = xsessaodita-aufnr1
*                        AND   anulado = space
*                        AND   mblnr = space.
**       Existem registo de consumos de lista técnica com erro
*      xsessaodita-ltecnica01 = '0'.
*      EXIT.
*    ENDSELECT.
    ENDIF.

    IF xsessaodita-aufnr2 = xsessaodita-ltecnica02 AND NOT
       xsessaodita-aufnr2 IS INITIAL.

** RSOUSA @ 18-04-2011 --> BEGIN
      REFRESH: lt_z02rpconslt, lt_mara.
      SELECT *
        FROM z02rpconslt INTO TABLE lt_z02rpconslt
        WHERE divisao  = xsessaodita-divisao  AND
              consprod = xsessaodita-numoper1 AND
              aufnr    = xsessaodita-aufnr2.

      DELETE lt_z02rpconslt WHERE anulado IS NOT INITIAL.
      DELETE lt_z02rpconslt WHERE mblnr   IS NOT INITIAL.

      IF lt_z02rpconslt[] IS NOT INITIAL.
        SELECT matnr extwg
          FROM mara INTO TABLE lt_mara
          FOR ALL ENTRIES IN lt_z02rpconslt
          WHERE matnr = lt_z02rpconslt-matnr.
*Begin GN(ROFF) 15.05.2012
*        IF divisao <> 'DIPE' or area = 'F03'.
        IF divisao <> 'DIPE' OR area <> 'F03'.
*End   GN(ROFF) 15.05.2012
          DELETE lt_mara WHERE extwg <> 'HIBRIDO'.
        ENDIF.
        SORT lt_mara BY matnr.
      ENDIF.

      LOOP AT lt_z02rpconslt.
        lv_tabix = sy-tabix.
        READ TABLE lt_mara WITH KEY matnr = lt_z02rpconslt-matnr.
        IF sy-subrc <> 0.
          DELETE lt_z02rpconslt INDEX lv_tabix.
        ENDIF.
      ENDLOOP.

      READ TABLE lt_z02rpconslt INDEX 1.
      IF sy-subrc = 0.
        xsessaodita-ltecnica02 = '0'.
      ENDIF.
** RSOUSA @ 18-04-2011 <-- END

*      SELECT divisao INTO l_dummy FROM z02rpconslt
*                          WHERE divisao = xsessaodita-divisao
*                          AND   consprod = xsessaodita-numoper1
*                          AND   aufnr = xsessaodita-aufnr2
*                          AND   anulado = space
*                          AND   mblnr = space.
**       Existem registo de consumos de lista técnica com erro
*        xsessaodita-ltecnica02 = '0'.
*        EXIT.
*      ENDSELECT.
    ENDIF.

    IF xsessaodita-tempos = 'X'.
      SELECT divisao INTO l_dummy FROM z02rptmarchale
                          WHERE divisao = xsessaodita-divisao
                          AND   consprod = xsessaodita-numoper1
                          AND   anulado = space
                          AND   confirmacao = space.
* existem registo de confirmação de tempos com erro
        xsessaodita-tempos = '0'.
        EXIT.
      ENDSELECT.
    ENDIF.

    MODIFY xsessaodita.

  ENDLOOP.

* INETUM - NR - 04.10.2023 - RENPRJ00041 - Inicio
***  IF 1 = 2.
***    PERFORM carrega_sessoes_dita_sem_prod.
***  ENDIF.
* INETUM - NR - 04.10.2023 - RENPRJ00041 - Fim

ENDFORM.                    " CARREGA_SESSOES_DITA
*&---------------------------------------------------------------------*
*&      Form  ESCREVE_SESSOESDITA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM escreve_sessoesdita.

** RL -> INS 24.08.2006
  DATA: l_rep(3) TYPE c.
** RL <- INS 24.08.2006



  FORMAT RESET. FORMAT COLOR COL_NORMAL.
  IF sy-tcode = 'Z02RP36'.
    SET TITLEBAR 'DIPE'.
  ENDIF.
  LOOP AT xsessaodita.

** RL -> INS 24.08.2006
    l_rep = xsessaodita-repeticao.
** RL <- INS 24.08.2006

*   JCaetano - 5/7/2005
    IF p_erro EQ 'X'.
      CHECK xsessaodita-producao EQ '0' OR
            xsessaodita-ltecnica01 EQ '0' OR
            xsessaodita-ltecnica02 EQ '0' OR
            xsessaodita-tempos EQ '0'.
    ELSEIF p_ok EQ 'X'.
      CHECK xsessaodita-producao NE '0' AND
            xsessaodita-ltecnica01 NE '0' AND
            xsessaodita-ltecnica02 NE '0' AND
            xsessaodita-tempos NE '0'.
    ENDIF.
*

    WRITE: / sy-vline,
             xsessaodita-werks,
             sy-vline,
             xsessaodita-area,
             sy-vline,
             xsessaodita-linha,
             sy-vline,
             xsessaodita-data,
             sy-vline,
** RL -> INS 24.08.2006
             l_rep,
             sy-vline,
** RL <- INS 24.08.2006
             xsessaodita-turno,
             xsessaodita-subsessao,
             sy-vline,
             xsessaodita-aufnr1,
             xsessaodita-aufnr2,
             sy-vline.

*    IF xsessaodita-producao   IS INITIAL OR
*       xsessaodita-ltecnica01 IS INITIAL.
*      icon_ok_ko1 ' '.
*    ELSE.
*      icon_ok_ko1 'X'.
*    ENDIF.

    icon_ok_ko1 xsessaodita-producao.
    WRITE sy-vline.
    IF NOT xsessaodita-aufnr1 IS INITIAL.
      icon_ok_ko1 xsessaodita-ltecnica01.
    ELSE.
      WRITE: '        '.
    ENDIF.
    WRITE sy-vline.
    IF NOT xsessaodita-aufnr2 IS INITIAL. "JCaetano - 5/7/2005
      icon_ok_ko1 xsessaodita-ltecnica02.
    ELSE.
      WRITE: '        '.
    ENDIF.
    WRITE sy-vline.
    icon_ok_ko1 xsessaodita-tempos.
    WRITE sy-vline.
    HIDE: xsessaodita.
    WRITE: / sy-uline(col).
  ENDLOOP.
  CLEAR xsessaodita.
ENDFORM.                    " ESCREVE_SESSOESDITA
*&---------------------------------------------------------------------*
*&      Form  ESCREVE_CABECALHO_SESSAODITA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM escreve_cabecalho_sessaodita.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_HEADING.
  WRITE: / sy-vline,
           text-002,
           sy-vline,
           text-003,
           sy-vline,
           text-004,
           sy-vline,
           text-005,
           sy-vline,
** RL -> INS 28.04.2006
           text-c01,
           sy-vline,
** RL <- INS 28.04.2006
           text-006,
           sy-vline,
           text-007,
           text-011,
           sy-vline,
           text-008,
           sy-vline,
           text-009,
           text-012,
           ' ',
           sy-vline,
           text-010,
           sy-vline.
  WRITE: / sy-uline(col).
ENDFORM.                    " ESCREVE_CABECALHO_SESSAODITA
*&---------------------------------------------------------------------*
*&      Form  LOG_SESSAO_DITA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM log_sessao_dita.
  DATA:
    ls_excpt TYPE ty_excpt.
  FORMAT RESET.
  CLEAR ls_excpt.

* INETUM - NR - 04.10.2023 - RENPRJ00041 - Inicio
***  IF sessao_sem_prod IS INITIAL.
* INETUM - NR - 04.10.2023 - RENPRJ00041 - Fim

  IF xsessaodita IS INITIAL.
    READ TABLE gt_excpt INTO ls_excpt
      WITH KEY divisao = divisao
               area    = area
               linha   = linha.
  ELSE.
    READ TABLE gt_excpt INTO ls_excpt
      WITH KEY divisao = xsessaodita-divisao
               werks   = xsessaodita-werks
               area    = xsessaodita-area
               linha   = xsessaodita-linha.
  ENDIF.
  IF ls_excpt-consprd EQ 'X'.
    IF ls_excpt-reqs EQ 'X'.
      PERFORM carrega_reqpapel.
      IF NOT xsessaodita IS INITIAL.
        PERFORM carrega_posicao_inicial_bob.
      ENDIF.
      PERFORM escreve_reqpapel.
      PERFORM carrega_consprod_alt_req.
      PERFORM escreve_consprod_alt_req.
    ELSE.
      PERFORM carrega_consprod_alt.
      PERFORM escreve_consprod_alt.
    ENDIF.
  ELSE.
    PERFORM carrega_reqpapel.
    IF NOT xsessaodita IS INITIAL.
      PERFORM carrega_posicao_inicial_bob.
    ENDIF.
    PERFORM escreve_reqpapel.
    PERFORM carrega_consprod.
    PERFORM escreve_consprod.
  ENDIF.
  PERFORM carrega_conslt.
  PERFORM escreve_conslt.
  PERFORM carrega_tempos.
  PERFORM escreve_tempos.
  CLEAR operacao.
  sy-lsind = sy-lsind - 1.

* INETUM - NR - 04.10.2023 - RENPRJ00041 - Inicio
***  ELSE.
***    PERFORM log_sessao_dita_sem_prod.
***  ENDIF.
* INETUM - NR - 04.10.2023 - RENPRJ00041 - Fim

ENDFORM.                    " LOG_SESSAO_DITA
*&---------------------------------------------------------------------*
*&      Form  CARREGA_REQPAPEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_reqpapel.
* ROFF SDF - JMB - 11.06.2019 ->
  DATA:
    lt_hardcode_results TYPE zdev_hardcode_t.
  DATA:
    ls_results TYPE zhardcode_table.
  DATA:
    lv_meth TYPE zdev_inc_meth,
    lv_low  TYPE rsdsselop_.
* ROFF SDF - JMB - 11.06.2019 <-

  CONSTANTS: lc_fname_bobdev TYPE name_feld VALUE 'BOBDEV'.

  REFRESH xreqpapel.
  SELECT z02rpreqpapel~divisao z02rpreqpapel~rqpapel
         z02rpreqpapel~lenum z02rpreqpapel~tanum
         z02rpreqpapel~lgnum z02rpreqpapel~devolv
         z02rpreqpapel~termprod
        INTO CORRESPONDING FIELDS OF xreqpapel
                 FROM z02rpreqpapel INNER JOIN z02rpconsprodh
            ON z02rpreqpapel~divisao = z02rpconsprodh~divisao
           AND z02rpreqpapel~rqpapel = z02rpconsprodh~rqpapel
            WHERE z02rpconsprodh~divisao =  divisao
            AND   z02rpconsprodh~consprod = consprod
            AND   z02rpreqpapel~anulado = space.
    APPEND xreqpapel.
  ENDSELECT.
* carrega requisição de papel para a área, linha sem terminar
* produção e não devolvida
  DATA: lv_linha LIKE z02rpsessao-linha,
        aux_area TYPE co_dispo.
  MOVE linha TO lv_linha.

  aux_area = area.


* ROFF SDF - JMB - 11.06.2019 ->
* a obtenção das restantes bobines não devolvidas é apenas relevante para
* a DIPE/RENV/LPE/LP1
  lv_meth = sy-repid.
  CALL METHOD zhard_code=>get_data
    EXPORTING
      inc_meth      = lv_meth
      fieldname     = lc_fname_bobdev
    IMPORTING
      zdev_hardcode = lt_hardcode_results.

  CLEAR lv_low.
  CONCATENATE
    xsessaodita-divisao
    xsessaodita-werks
    xsessaodita-area
    xsessaodita-linha
    INTO lv_low
    SEPARATED BY '/'.

  READ TABLE lt_hardcode_results TRANSPORTING NO FIELDS
    WITH KEY ue_low = lv_low.
  IF sy-subrc EQ 0.
* ROFF SDF - JMB - 11.06.2019 <-
    SELECT z02rpreqpapel~divisao z02rpreqpapel~rqpapel
           z02rpreqpapel~lenum z02rpreqpapel~tanum
           z02rpreqpapel~lgnum z02rpreqpapel~devolv
           z02rpreqpapel~termprod
          INTO CORRESPONDING FIELDS OF xreqpapel
                   FROM z02rpreqpapel INNER JOIN z02rpsessao
              ON z02rpreqpapel~divisao = z02rpsessao~divisao
             AND z02rpreqpapel~sessao = z02rpsessao~sessao

              WHERE z02rpsessao~linha = lv_linha
*            AND   z02rpsessao~linha = linha

              AND z02rpsessao~area = aux_area

              AND   z02rpreqpapel~anulado = space
              AND   z02rpreqpapel~tanum = space.
      SELECT SINGLE lenum INTO lqua-lenum
                           FROM lqua WHERE lenum = xreqpapel-lenum.
      IF sy-subrc = 0.
        APPEND xreqpapel.
      ENDIF.
    ENDSELECT.
* ROFF SDF - JMB - 11.06.2019 ->
  ENDIF.
* ROFF SDF - JMB - 11.06.2019 <-
  SORT xreqpapel BY lenum.
ENDFORM.                    " CARREGA_REQPAPEL
*&---------------------------------------------------------------------*
*&      Form  ESCREVE_REQPAPEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM escreve_reqpapel.
  CONSTANTS: col TYPE i VALUE 65.
  FORMAT RESET.
  WRITE: / sy-uline(col).
  WRITE: / sy-vline,
           text-013 COLOR COL_GROUP CENTERED,
           AT col sy-vline.
  operacao = 'RP'.
  HIDE operacao.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_HEADING.
  WRITE: / sy-vline,
           text-017,
           text-018,
           sy-vline,
           text-019,
           sy-vline,
           text-034,
           sy-vline,
           text-035,
           sy-vline.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_NORMAL.
  LOOP AT xreqpapel.
    CLEAR lqua.
    SELECT SINGLE lgpla INTO lqua-lgpla
                        FROM lqua WHERE lenum = xreqpapel-lenum.
    CLEAR xbobinas_i.
    READ TABLE xbobinas_i WITH KEY lenum = xreqpapel-lenum.
    WRITE: / sy-vline,
             xreqpapel-lenum USING EDIT MASK '==ALPHA',
             xreqpapel-devolv,
             xreqpapel-termprod,
             sy-vline.
* Roff - NR - 01.07.2019 - Inicio
***    icon_ok_ko2 xreqpapel-tanum.
    WRITE: '        '.
* Roff - NR - 01.07.2019 - Fim
    WRITE: sy-vline,
           xbobinas_i-lgpla,
           sy-vline,
           lqua-lgpla,
           sy-vline.
  ENDLOOP.
  IF sy-subrc = 0.
    WRITE: / sy-uline(col).
  ENDIF.
  CLEAR xreqpapel.
ENDFORM.                    " ESCREVE_REQPAPEL
*&---------------------------------------------------------------------*
*&      Form  CARREGA_CONSPROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_consprod.
  SELECT * FROM z02rpconsprodl
            INTO CORRESPONDING FIELDS OF TABLE xconsprod
            WHERE divisao =  divisao
            AND   consprod = consprod
            AND   anulado = space.
  SORT xconsprod BY aufnr linha.
  LOOP AT xconsprod.
    AT NEW linha.
      SELECT SINGLE * FROM z02rpconsprod
                       WHERE divisao = xconsprod-divisao
                       AND   consprod = xconsprod-consprod
                       AND   aufnr = xconsprod-aufnr
                       AND   linha = xconsprod-linha.
    ENDAT.
    xconsprod-mblnr_p = z02rpconsprod-mblnr.
    xconsprod-mjahr_p = z02rpconsprod-mjahr.
    xconsprod-prodp = z02rpconsprod-prodp.
    xconsprod-meins_p = z02rpconsprod-meins_p.
    xconsprod-codeb = z02rpconsprod-codeb.
    MODIFY xconsprod.
  ENDLOOP.
  SELECT * FROM z02rpconsprodh
            INTO CORRESPONDING FIELDS OF TABLE xconsprodh
            WHERE divisao =  divisao
            AND   consprod = consprod.
ENDFORM.                    " CARREGA_CONSPROD
*&---------------------------------------------------------------------*
*&      Form  ESCREVE_CONSPROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM escreve_consprod.
*  CONSTANTS: col TYPE i VALUE 117.
  CONSTANTS: col TYPE i VALUE 108.
  FORMAT RESET.
  WRITE: / sy-uline(col).
*  FORMAT COLOR COL_GROUP.
  WRITE: / sy-vline,
           text-014 COLOR COL_GROUP CENTERED,
           AT col sy-vline.
  operacao = 'PP'.
  HIDE operacao.
  WRITE: AT 3 icon_generate AS ICON HOTSPOT,
         text-037 COLOR COL_GROUP CENTERED.
*  WRITE: AT 110 text-036 COLOR COL_GROUP CENTERED,
  WRITE: AT 101 text-036 COLOR COL_GROUP, "CENTERED,
         icon_display_text AS ICON HOTSPOT.
**  FORMAT RESET.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_HEADING.
  WRITE: / sy-vline,
           text-017,
           text-020,
           text-021,
           text-022,
           text-023,
           sy-vline,
**           text-024,
           text-025,
           text-019,
*           text-a01.
           sy-vline.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_NORMAL.
  DATA l_linha_ant LIKE z02rpconsprod-linha.
  LOOP AT xconsprod.

* Roff - NR - 28.08.2019 - Inicio
    PERFORM converte_um USING xconsprod-meins_p
                     CHANGING xconsprod-meins_p.

    PERFORM converte_um USING xconsprod-meins_c
                     CHANGING xconsprod-meins_c.
* Roff - NR - 28.08.2019 - Fim

    WRITE: / sy-vline,
             xconsprod-lenum USING EDIT MASK '==ALPHA',
             xconsprod-aufnr,
             xconsprod-prodp, xconsprod-meins_p,
             xconsprod-codeb,
             xconsprod-cons, xconsprod-meins_c,
             sy-vline.
    IF l_linha_ant NE xconsprod-linha.
*      icon_ok_ko2 xconsprod-mblnr_p.
      icon_ok_ko2 xconsprod-mblnr.
      icon_ok_ko2 xconsprod-tanum.
    ENDIF.
    WRITE AT col sy-vline.
    l_linha_ant = xconsprod-linha.
  ENDLOOP.
  IF sy-subrc = 0.
    WRITE: / sy-uline(col).
  ENDIF.
  CLEAR: xconsprod.
ENDFORM.                    " ESCREVE_CONSPROD
*&---------------------------------------------------------------------*
*&      Form  CARREGA_CONSLT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_conslt.
  SELECT * FROM z02rpconslt INTO TABLE xconslt
            WHERE divisao =  divisao
            AND   consprod = consprod
            AND   anulado = space.
  SORT xconslt BY aufnr linha matnr.
ENDFORM.                    " CARREGA_CONSLT

*&---------------------------------------------------------------------*
*&      Form  ESCREVE_CONSLT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM escreve_conslt.
  DATA: lv_flag TYPE flag."GN(ROFF) 03.08.2012
  CONSTANTS: col TYPE i VALUE 79.

  DATA: BEGIN OF lt_mara OCCURS 0,
          matnr TYPE matnr,
          extwg TYPE mara-extwg,
        END OF lt_mara.

  DATA: l_linha_ant LIKE z02rpconslt-linha.
  DATA: lv_tabix    LIKE sy-tabix.

  "INS ROFF JCO - 11.04.2019 - RENPRM00002.
* Obter  registos da tabela de hardcodes para a validação da divisão, para indicar
* erro caso o lote não esteja preenchido.
  DATA: lt_hardcode_results TYPE zdev_hardcode_t.
  DATA: lv_meth        TYPE zdev_inc_meth,
        lv_valdt_charg TYPE flag.

  CONSTANTS: lc_fname_vald_charg TYPE name_feld VALUE 'CHARG_VALD_DIVISAO'.

  lv_meth = sy-repid.
  CALL METHOD zhard_code=>get_data
    EXPORTING
      inc_meth      = lv_meth
      fieldname     = lc_fname_vald_charg
    IMPORTING
      zdev_hardcode = lt_hardcode_results.
  READ TABLE lt_hardcode_results TRANSPORTING NO FIELDS
    WITH KEY ue_low = divisao.
  IF sy-subrc EQ 0.
    lv_valdt_charg = 'X'.
  ENDIF.
  "END ROFF JCO - 11.04.2019 - RENPRM00002.

  FORMAT RESET.
  WRITE: / sy-uline(col).
  WRITE: / sy-vline,
           text-015 COLOR COL_GROUP CENTERED,
           sy-vline.
  operacao = 'CLT'.
  HIDE operacao.
  WRITE: AT 3 icon_generate AS ICON HOTSPOT,
        text-037 COLOR COL_GROUP CENTERED.
  WRITE: AT 72 text-036 COLOR COL_GROUP CENTERED,
              icon_display_text AS ICON HOTSPOT.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_HEADING.
  WRITE: / sy-vline,
           text-020,
           text-026,
           text-027,
           text-023,
           sy-vline,
           text-025,
           sy-vline.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_NORMAL.

** RSOUSA @ 18-04-2011 --> BEGIN
** Filtrar materiais que não são Hibridos
  IF xconslt[] IS NOT INITIAL.
    SELECT matnr extwg
      FROM mara INTO TABLE lt_mara
      FOR ALL ENTRIES IN xconslt
      WHERE matnr = xconslt-matnr.
*      Begin GN(ROFF) 11.06.2012
*    IF divisao <> 'DIPE' or area = 'F03'.
    IF divisao <> 'DIPE' AND area <> 'F03'.
*      End   GN(ROFF) 11.06.2012
      DELETE lt_mara WHERE extwg <> 'HIBRIDO'.
    ENDIF.
    SORT lt_mara BY matnr.
  ENDIF.

  LOOP AT xconslt.
*    Begin GN(ROFF) 23.01.2013
**    Begin GN(ROFF) 03.08.2012
*    if xconslt-mblnr IS INITIAL AND lv_flag IS INITIAL.
*      lv_flag = 'X'.
*    ENDIF.
**    End   GN(ROFF) 03.08.2012
*    End   GN(ROFF) 23.01.2013
    lv_tabix = sy-tabix.
    READ TABLE lt_mara WITH KEY matnr = xconslt-matnr.
    IF sy-subrc <> 0.
      DELETE xconslt INDEX lv_tabix.
*      Begin GN(ROFF) 23.01.2013
    ELSE.
      IF xconslt-mblnr IS INITIAL
          AND lv_flag IS INITIAL.
        lv_flag = 'X'.
      ENDIF.
*    End   GN(ROFF) 23.01.2013
    ENDIF.
  ENDLOOP.
** RSOUSA @ 18-04-2011 <-- END

  LOOP AT xconslt.

* Roff - NR - 28.08.2019 - Inicio
    PERFORM converte_um USING xconslt-meins
                     CHANGING xconslt-meins.
* Roff - NR - 28.08.2019 - Fim

    WRITE: / sy-vline,
             xconslt-aufnr,
             xconslt-matnr,
             xconslt-charg,
             xconslt-menge,
             xconslt-meins,
             sy-vline.
    IF l_linha_ant NE xconslt-linha.
      IF lv_flag IS INITIAL.
        icon_ok_ko2 xconslt-mblnr.
      ELSE.
        icon_ok_ko2 space.
      ENDIF.
    ENDIF.
* Roff - NR - 15.07.2019 - Inicio
    "Comentado: apenas é prentendido um simbolo por linha
***    "INS ROFF JCO - 11.04.2019 - RENPRM00002.
***    IF lv_valdt_charg EQ 'X' AND xconslt-charg IS INITIAL.
***      icon_ok_ko2 xconslt-charg.
***    ENDIF.
***    "END ROFF JCO - 11.04.2019 - RENPRM00002.
* Roff - NR - 15.07.2019 - Fim
    WRITE AT col sy-vline.
    l_linha_ant = xconslt-linha.
  ENDLOOP.
  IF sy-subrc = 0.
    WRITE: / sy-uline(col).
  ENDIF.
  CLEAR xconslt.
ENDFORM.                    " ESCREVE_CONSLT
*&---------------------------------------------------------------------*
*&      Form  CARREGA_TEMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_tempos.
  SELECT * FROM z02rptmarchale INTO TABLE xtempos
            WHERE divisao =  divisao
            AND   consprod = consprod
            AND   anulado = space.
  SORT xtempos BY aufnr linha vornr.
ENDFORM.                    " CARREGA_TEMPOS

*&---------------------------------------------------------------------*
*&      Form  ESCREVE_TEMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM escreve_tempos.
  CONSTANTS: col TYPE i VALUE 78.
  FORMAT RESET.
  WRITE: / sy-uline(col).
  WRITE: / sy-vline,
           text-016 COLOR COL_GROUP CENTERED,
           AT col sy-vline.
  operacao = 'TLE'.
  HIDE operacao.
  WRITE: AT 3 icon_generate AS ICON HOTSPOT,
        text-037 COLOR COL_GROUP CENTERED.
  WRITE: AT 71 text-036 COLOR COL_GROUP CENTERED,
              icon_display_text AS ICON HOTSPOT.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_HEADING.
  WRITE: / sy-vline,
           text-020,
           text-028,
           text-029,
           text-030,
           text-031,
           text-032,
           sy-vline,
           text-033,
           sy-vline.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_NORMAL.
  LOOP AT xtempos.

* Roff - NR - 28.08.2019 - Inicio
    PERFORM converte_um USING xtempos-meins
                     CHANGING xtempos-meins.
* Roff - NR - 28.08.2019 - Fim

    WRITE: / sy-vline,
             xtempos-aufnr,
             xtempos-vornr,
             xtempos-maufn,
             xtempos-meins,
             xtempos-tplinha,
             xtempos-inicio,
             xtempos-fim,
             sy-vline.
    icon_ok_ko2 xtempos-confirmacao.
    WRITE sy-vline.
  ENDLOOP.
  IF sy-subrc = 0.
    WRITE: / sy-uline(col).
  ENDIF.
  CLEAR xtempos.
ENDFORM.                    " ESCREVE_TEMPOS
*&---------------------------------------------------------------------*
*&      Form  LOG_OPERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM log_operacao.
  DATA:
    ls_sessaodita TYPE z02rpsessaodita.

  DATA campo(20).

  CLEAR ls_sessaodita.
  ls_sessaodita = xsessaodita.

  GET CURSOR FIELD campo.
  CASE operacao.
    WHEN 'PP' OR 'PPP' OR 'PPR'.
      READ TABLE xconsprod INDEX 1.
      CHECK sy-subrc = 0.
      IF campo = 'ICON_DISPLAY_TEXT'.
        SUBMIT z02rp_rp_regprod_novo WITH oper  = operacao
                                     WITH consprod = xconsprod-consprod
                                     WITH p_div = divisao
                              AND RETURN.
      ELSEIF campo = 'ICON_GENERATE'.
        PERFORM lancamentos_pp.
      ENDIF.
      xsessaodita = ls_sessaodita.
      PERFORM log_sessao_dita.
    WHEN 'CLT'.
      READ TABLE xconslt INDEX 1.
      CHECK sy-subrc = 0.
      IF campo = 'ICON_DISPLAY_TEXT'.
        SUBMIT z02rp_rp_regprod_novo WITH oper = operacao
                                WITH consprod = xconslt-consprod
                                WITH p_div = divisao
* Roff - NR - 17.09.2019 - Inicio
                                WITH so_area  IN so_area
                                WITH so_linha IN so_linha
                                WITH so_data  IN so_data
                                WITH so_turno IN so_turno
* Roff - NR - 17.09.2019 - Fim
                              AND RETURN.
      ELSEIF campo = 'ICON_GENERATE'.
        PERFORM lancamentos_clt.
      ENDIF.
      xsessaodita = ls_sessaodita.
      PERFORM log_sessao_dita.
    WHEN 'TLE'.
      READ TABLE xtempos INDEX 1.
      CHECK sy-subrc = 0.
      IF campo = 'ICON_DISPLAY_TEXT'.
        SUBMIT z02rp_rp_regprod_novo WITH oper = operacao
                                WITH consprod = xtempos-consprod
                                WITH p_div = divisao
                              AND RETURN.
      ELSEIF campo = 'ICON_GENERATE'.
        PERFORM lancamentos_tle.
      ENDIF.
      xsessaodita = ls_sessaodita.
      PERFORM log_sessao_dita.
  ENDCASE.
ENDFORM.                    " LOG_OPERACAO
*&---------------------------------------------------------------------*
*&      Form  ALTERA_QUANTIDADE_PRODUZIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM altera_quantidade_produzida.
  DATA: l_quant_prod     LIKE z02rpconsprod-prodp,
        l_quant_prod_new LIKE z02rpconsprod-prodp,
        l_prodp1_old     LIKE z02rpconsprod-prodp,
        l_prodp2_old     LIKE z02rpconsprod-prodp.
* verifica se é possível alterar a quantidade
  LOOP AT xconsprod WHERE mblnr NE space
                    OR    tanum NE space
                    OR    mblnr_p NE space.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    MESSAGE i152(z02rp).
    EXIT.
  ENDIF.
  LOOP AT xconslt WHERE mblnr NE space.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    MESSAGE i152(z02rp).
    EXIT.
  ENDIF.
  LOOP AT xtempos WHERE confirmacao NE space.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    MESSAGE i152(z02rp).
    EXIT.
  ENDIF.
  READ TABLE xconsprod WITH KEY linha = '01'.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING xconsprod TO z02rpconsprod.
  ELSE.
    CLEAR z02rpconsprod.
  ENDIF.
  READ TABLE xconsprod WITH KEY linha = '02'.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING xconsprod TO *z02rpconsprod.
  ELSE.
    CLEAR *z02rpconsprod.
  ENDIF.
  l_quant_prod = z02rpconsprod-prodp + *z02rpconsprod-prodp.
  l_prodp1_old = z02rpconsprod-prodp.
  l_prodp2_old = *z02rpconsprod-prodp.
  CHECK l_quant_prod NE 0.
  CALL SCREEN 100 STARTING AT 10 5 ENDING AT 45 8.
  IF okcode = 'ENTER'.
    l_quant_prod_new = z02rpconsprod-prodp + *z02rpconsprod-prodp.
    DATA l_refresh.
    IF NOT z02rpconsprod-aufnr IS INITIAL.
      READ TABLE xconsprod WITH KEY linha = z02rpconsprod-linha.
      IF xconsprod-prodp NE z02rpconsprod-prodp
         OR l_quant_prod NE l_quant_prod_new.
* quantidade alterada
        l_refresh = 'X'.
        PERFORM recalcula_consumos USING z02rpconsprod-linha
                                         z02rpconsprod-prodp
                                         l_quant_prod.
        PERFORM actualiza_producao USING z02rpconsprod.
      ENDIF.
    ENDIF.
    IF NOT *z02rpconsprod-aufnr IS INITIAL.
      READ TABLE xconsprod WITH KEY linha = *z02rpconsprod-linha.
      IF xconsprod-prodp NE *z02rpconsprod-prodp
         OR l_quant_prod NE l_quant_prod_new.
* quantidade alterada
        l_refresh = 'X'.
        PERFORM recalcula_consumos USING *z02rpconsprod-linha
                                         *z02rpconsprod-prodp
                                         l_quant_prod.
        PERFORM actualiza_producao USING *z02rpconsprod.
      ENDIF.
    ENDIF.
    IF l_refresh = 'X'.
      IF l_prodp1_old NE 0.
        PERFORM recalcula_consumos_lt USING l_prodp1_old
                                            '01'.
      ENDIF.
      IF l_prodp2_old NE 0.
        PERFORM recalcula_consumos_lt USING l_prodp2_old
                                            '02'.
      ENDIF.
      PERFORM recalcula_tempos USING l_quant_prod_new.
* refresh
      PERFORM refresh_log_operacoes.
    ENDIF.
  ENDIF.
ENDFORM.                    " ALTERA_QUANTIDADE_PRODUZIDA
*&---------------------------------------------------------------------*
*&      Form  RECALCULA_CONSUMOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recalcula_consumos USING p_linha
                              p_prodp
                              l_quant_prod_old.
  DATA: l_quant_prod  LIKE z02rpconsprod-prodp,
        l_peso_bobina LIKE lqua-gesme.
  l_quant_prod = z02rpconsprod-prodp + *z02rpconsprod-prodp.
  LOOP AT xconsprod WHERE linha = p_linha.
*    select single gesme into lqua-gesme from lqua
*                        where lenum = xconsprod-lenum.
*    read table xconsprodh with key lenum = xconsprod-lenum.
*    catch system-exceptions bcd_zerodivide = 1.
*      xconsprod-cons = ( lqua-gesme - xconsprodh-resto )
*                              * xconsprod-prodp
*                            /   l_quant_prod.
*    endcatch.
    CATCH SYSTEM-EXCEPTIONS bcd_zerodivide = 1.
* calcula o peso da bobina a quando do consumo
      IF xconsprod-peso_cons = 0.
        l_peso_bobina = ( xconsprod-cons * l_quant_prod_old )
                                / xconsprod-prodp.
      ELSE.
        l_peso_bobina = xconsprod-peso_cons.
      ENDIF.
* calcula o novo consumo
      xconsprod-cons = l_peso_bobina * p_prodp
                            /   l_quant_prod.
    ENDCATCH.
    MODIFY xconsprod.
    UPDATE z02rpconsprodl SET cons = xconsprod-cons
                     WHERE divisao = xconsprod-divisao
                     AND   consprod = xconsprod-consprod
                     AND   aufnr = xconsprod-aufnr
                     AND   linha = xconsprod-linha
                     AND   lenum = xconsprod-lenum.
  ENDLOOP.
ENDFORM.                    " RECALCULA_CONSUMOS
*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_PRODUCAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_*Z02RPCONSPROD  text
*----------------------------------------------------------------------*
FORM actualiza_producao USING    p_consprod STRUCTURE z02rpconsprod.
  xconsprod-prodp = p_consprod-prodp.
  MODIFY xconsprod TRANSPORTING prodp
                   WHERE aufnr = p_consprod-aufnr
                   AND   linha = p_consprod-linha.
  UPDATE z02rpconsprod SET prodp = p_consprod-prodp
                   WHERE divisao = p_consprod-divisao
                   AND   consprod = p_consprod-consprod
                   AND   aufnr = p_consprod-aufnr
                   AND   linha = p_consprod-linha.

ENDFORM.                    " ACTUALIZA_PRODUCAO
*&---------------------------------------------------------------------*
*&      Form  REFRESH_LOG_OPERACOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_log_operacoes.
  DATA:
    ls_excpt TYPE ty_excpt.

  CLEAR ls_excpt.
  READ TABLE gt_excpt INTO ls_excpt
    WITH KEY divisao = xsessaodita-divisao
             werks   = xsessaodita-werks
             area    = xsessaodita-area
             linha   = xsessaodita-linha.
  IF ls_excpt-consprd EQ 'X'.
    IF ls_excpt-reqs EQ 'X'.
      PERFORM escreve_reqpapel.
    ENDIF.
    PERFORM escreve_consprod_alt.
  ELSE.
    PERFORM escreve_reqpapel.
    PERFORM escreve_consprod.
  ENDIF.

  PERFORM escreve_conslt.
  PERFORM escreve_tempos.
  sy-lsind = sy-lsind - 1.
ENDFORM.                    " REFRESH_LOG_OPERACOES
*&---------------------------------------------------------------------*
*&      Form  RECALCULA_CONSUMOS_LT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recalcula_consumos_lt USING l_prodp_old
                                 l_linha.
  DATA: BEGIN OF xmateriais OCCURS 0,
          matnr LIKE mara-matnr,
        END OF xmateriais.
  DATA: e_menge LIKE ekpo-menge.
*  data xconslt1 like z02rpconslt occurs 0 with header line.
*  data xconslt2 like z02rpconslt occurs 0 with header line.
  LOOP AT xconslt WHERE linha = l_linha.
    READ TABLE xmateriais WITH KEY matnr = xconslt-matnr.
    IF sy-subrc NE 0.
      READ TABLE xconsprod WITH KEY aufnr = xconslt-aufnr
                                    linha = xconslt-linha.
      CLEAR afko.
      SELECT SINGLE plnbez gamng rsnum
            INTO (afko-plnbez, afko-gamng, afko-rsnum)
                            FROM afko
                            WHERE aufnr = xconsprod-aufnr.
      CLEAR resb.
      SELECT SINGLE bdmng INTO resb-bdmng FROM resb
                           WHERE rsnum = afko-rsnum
                           AND   matnr = xconslt-matnr
                           AND   shkzg = 'H'
                           AND   xloek = space.
      CLEAR mara.
      SELECT SINGLE meins INTO mara-meins FROM mara
                            WHERE matnr = afko-plnbez.
      CLEAR e_menge.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = afko-plnbez
          i_in_me              = xconsprod-meins_p
          i_out_me             = mara-meins
          i_menge              = xconsprod-prodp
        IMPORTING
          e_menge              = e_menge
        EXCEPTIONS
          error_in_application = 1
          meinh_not_found      = 2
          OTHERS               = 3.
      CATCH SYSTEM-EXCEPTIONS bcd_zerodivide = 1.
        xconslt-menge = resb-bdmng / afko-gamng * e_menge.
      ENDCATCH.
    ELSE.
      CLEAR xconslt-menge.
    ENDIF.
*    xconslt-menge = ( xconslt-menge * xconsprod-prodp )
*                          / l_prodp_old.
    MODIFY xconslt.
    xmateriais-matnr = xconslt-matnr.
    COLLECT xmateriais.
*    move-corresponding xconslt to xconslt1.
*    clear xconslt1-charg.
*    collect xconslt1.
    UPDATE z02rpconslt SET menge = xconslt-menge
                       WHERE divisao = xconslt-divisao
                       AND   conslt = xconslt-conslt
                       AND   aufnr = xconslt-aufnr
                       AND   linha = xconslt-linha
                       AND   matnr = xconslt-matnr
                       AND   charg = xconslt-charg.
  ENDLOOP.
* atribui lotes
*  data: bdbatch like bdbatch occurs 0 with header line,
*        tot_disponivel like bdbatch-erfmg.

*  loop at xconslt1.
*    check xconslt1-menge is initial.
*    select single * from z02rpsessao where divisao = xconslt1-divisao
*                                     and   sessao = xconslt1-sessao.
*    call function 'Z_02RP_DETERMINACAO_LOTE'
*         exporting
*              matnr         = xconslt1-matnr
*              werks         = z02rpsessao-werks
*              lgort         = xconslt1-lgort
*              erfmg         = xconslt1-menge
*              erfme         = xconslt1-meins
*              lfdat         = z02rpsessao-data
*         importing
*              total         = tot_disponivel
*         tables
*              e_bdbatch     = bdbatch
*         exceptions
*              erro_detlotes = 1
*              others        = 2.
*   loop at bdbatch.
*      xconslt2 = xconslt1.
*      xconslt2-menge = bdbatch-erfmg.
*      xconslt2-charg = bdbatch-charg.
*      append xconslt2.
*    endloop.
*  endloop.
ENDFORM.                    " RECALCULA_CONSUMOS_LT
*&---------------------------------------------------------------------*
*&      Form  RECALCULA_TEMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recalcula_tempos USING p_quant_prod.
*  data: tot_prod like t_rst-prodp.
*Total da Produção das ordens
*  loop at t_rst.
*    at last.
*      sum.
*      tot_prod = t_rst-prodp.
*    endat.
* endloop.
*Tempo de marcha por ordem
*  loop at t_rst.
*    t_rst-maufn = ( t_rst-prodp * z02rptmarchale-tmlinha ) / tot_prod.
*    t_rst-meins = 'H'.
*   t_rst-tplinha = ( t_rst-prodp * z02rptmarchale-tplinha ) / tot_prod.
*    t_rst-meinspl = 'H'.
*    modify t_rst.
*  endloop.
  DATA tplinha LIKE z02rptmarchale-tplinha.

** RL -> INS 24.08.2006
  DATA: l_aufnr1 TYPE aufnr,
        l_aufnr2 TYPE aufnr.

  DATA: l_tempo    LIKE z02rptmarchamaq-iserh,
        l_proc     LIKE z02rptmarchamaq-iserh,
        l_manut    LIKE z02rptmarchamaq-iserh,
        l_gest     LIKE z02rptmarchamaq-iserh,
* INETUM - NR - 19.01.2024 - RENPRJ00041 - Inicio
        l_proc_lin TYPE z02rptmarchamaq-iserh,
* INETUM - NR - 19.01.2024 - RENPRJ00041 - Fim
        l_idx_cons LIKE sy-tabix.

  DATA: g_proc     TYPE eauszt,
        g_manut    TYPE eauszt,
        g_gest     TYPE eauszt,
* INETUM - NR - 19.01.2024 - RENPRJ00041 - Inicio
        g_proc_lin TYPE eauszt.
* INETUM - NR - 19.01.2024 - RENPRJ00041 - Fim

  DATA: wa_sessao LIKE z02rpsessao.

  LOOP AT xconsprod.
    l_idx_cons = sy-tabix.

    AT END OF consprod.
      READ TABLE xconsprod INDEX l_idx_cons.
      IF wa_sessao IS INITIAL.
        SELECT SINGLE * FROM z02rpsessao
        INTO wa_sessao
        WHERE divisao EQ xconsprod-divisao
          AND sessao  EQ xconsprod-sessao.

        CLEAR: wa_sessao-sessao.
      ENDIF.
    ENDAT.
  ENDLOOP.

  CLEAR: l_tempo, l_proc, l_manut, l_gest, l_proc_lin.

  CALL FUNCTION 'Z02RP_OBTEM_TEMPOS'
    EXPORTING
      wa_sessao            = wa_sessao
      aufnr1               = wa_sessao-aufnr1
      aufnr2               = wa_sessao-aufnr2
    IMPORTING
      tempo_processo       = g_proc
      tempo_manutencao     = g_manut
      tempo_gestao         = g_gest
* INETUM - NR - 19.01.2024 - RENPRJ00041 - Inicio
      tempo_processo_linha = g_proc_lin
* INETUM - NR - 19.01.2024 - RENPRJ00041 - Fim
    EXCEPTIONS
      sem_dados_sessao     = 1
      sem_notas_criadas    = 2
      sem_dados_divisao    = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    l_proc  = g_proc.
    l_manut = g_manut.
    l_gest  = g_gest.
* INETUM - NR - 19.01.2024 - RENPRJ00041 - Inicio
    l_proc_lin = g_proc_lin.
* INETUM - NR - 19.01.2024 - RENPRJ00041 - Fim

*    l_tempo = l_proc + l_manut + l_gest.
  ENDIF.
** RL <- INS 24.08.2006

  LOOP AT xtempos.
    READ TABLE xconsprod WITH KEY aufnr = xtempos-aufnr
                                  linha = xtempos-linha.
    IF sy-subrc = 0.
      CATCH SYSTEM-EXCEPTIONS bcd_zerodivide = 1.
        xtempos-maufn = ( xconsprod-prodp * xtempos-tmlinha )
                                 / p_quant_prod.
        tplinha = ( ( xtempos-fim - xtempos-inicio ) / 3600 )
                                  - xtempos-tmlinha.
        xtempos-tplinha = ( xconsprod-prodp * tplinha )
                                 / p_quant_prod.
** RL -> INS 24.08.2006
        xtempos-tproc = xconsprod-prodp * l_proc / p_quant_prod.
** RL <- INS 24.08.2006
      ENDCATCH.
      MODIFY xtempos.
      UPDATE z02rptmarchale SET maufn = xtempos-maufn
                                tplinha = xtempos-tplinha
** RL -> INS 24.08.2006
                                tproc = xtempos-tproc
** RL <- INS 24.08.2006
                            WHERE divisao = xtempos-divisao
                            AND   regnum = xtempos-regnum
                            AND   aufnr = xtempos-aufnr
                            AND   linha = xtempos-linha
                            AND   vornr = xtempos-vornr.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " RECALCULA_TEMPOS
*&---------------------------------------------------------------------*
*&      Form  CARREGA_POSICAO_INICIAL_BOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_posicao_inicial_bob.
  REFRESH xbobinas_i.
  LOOP AT xreqpapel.
    SELECT SINGLE lenum lgpla INTO CORRESPONDING FIELDS OF xbobinas_i
                 FROM lqua WHERE lenum = xreqpapel-lenum.
    IF sy-subrc = 0.
*      call function 'CONVERSION_EXIT_LENUM_OUTPUT'
*           exporting
*                input           = xbobinas_i-lenum
*           importing
*                output          = xbobinas_i-lenum
*           exceptions
*                check_failed    = 1
*                not_numeric     = 2
*                t344_get_failed = 3
*                wrong_length    = 4
*                others          = 5.
      APPEND xbobinas_i.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CARREGA_POSICAO_INICIAL_BOB
*&---------------------------------------------------------------------*
*&      Form  LANCAMENTOS_PP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lancamentos_pp.
  DATA tab1 LIKE z02rpconsprod OCCURS 0 WITH HEADER LINE.
  DATA tab2 LIKE z02rpconsprodl OCCURS 0 WITH HEADER LINE.

  SELECT * FROM z02rpconsprod INTO TABLE tab1
                         WHERE divisao = divisao
                         AND   consprod = consprod.
  READ TABLE tab1 INDEX 1.
  SELECT SINGLE * FROM z02rpsessao WHERE divisao = divisao
                                   AND   sessao = tab1-sessao.
  REFRESH tab2.
  SELECT * FROM z02rpconsprodl INTO TABLE tab2
                         WHERE divisao = divisao
                         AND   consprod = consprod.
  SORT tab2 BY aufnr linha buzei.
  CALL FUNCTION 'Z_02RP_LANCAMENTOS'
    EXPORTING
      funcao        = ' '
      i_reprocessar = 'X'
      ok_code       = 'SAVP'
    TABLES
      consprod      = tab1
      consprodl     = tab2
    CHANGING
      sessao        = z02rpsessao
    EXCEPTIONS
      erro_mov      = 1
      erro_bapi     = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " LANCAMENTOS_PP
*&---------------------------------------------------------------------*
*&      Form  LANCAMENTOS_CLT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lancamentos_clt.
*  data tab like z02rpconslt occurs 0 with header line.
  DATA xconslt1 LIKE z02rpconslt OCCURS 0 WITH HEADER LINE.
  DATA xconslt2 LIKE z02rpconslt OCCURS 0 WITH HEADER LINE.
*{   INSERT         DEVK901405                                        3
  DATA: aux_conslt LIKE z02rpconslt OCCURS 0 WITH HEADER LINE,
        labst      TYPE labst,
        lbkum      TYPE lbkum.
*}   INSERT

  LOOP AT xconslt.
    CHECK xconslt-mblnr IS INITIAL.
    AT NEW linha.
      REFRESH: xconslt1, xconslt2.
      CALL FUNCTION 'Z_02RP_ATRIBUICAO_LOTES_CLT'
        EXPORTING
          divisao  = divisao
          numoper  = xconslt-conslt
        TABLES
          xconslt1 = xconslt1
          xconslt2 = xconslt2.
* actualizar tabelas após atribuição de lotes
      DELETE FROM z02rpconslt WHERE divisao = divisao
                              AND   conslt = xconslt-conslt.
      INSERT z02rpconslt FROM TABLE xconslt2.
*      refresh tab.
*      select * from z02rpconslt into table tab
*                             where divisao = divisao
*                             and   conslt = xconslt-conslt.
      READ TABLE xconslt2 INDEX 1.
      SELECT SINGLE * FROM z02rpsessao WHERE divisao = divisao
                                       AND   sessao = xconslt2-sessao.
*{   INSERT         DEVK901405                                        1
      LOOP AT xconslt2.

        SELECT SINGLE mandt FROM z02rploteman
          INTO sy-mandt
         WHERE matnr = xconslt2-matnr.
        IF sy-subrc = 0 AND xconslt2-charg IS INITIAL.
          CONTINUE.
        ENDIF.

        SELECT SINGLE labst FROM mard
          INTO labst
         WHERE matnr = xconslt2-matnr
           AND werks = z02rpsessao-werks
           AND lgort = xconslt2-lgort.
        IF labst GE xconslt2-menge.
          aux_conslt  = xconslt2.
          APPEND  aux_conslt.
          DELETE xconslt2.
        ENDIF.
      ENDLOOP.
      DO 2 TIMES.
*}   INSERT
        CALL FUNCTION 'Z_02RP_LANCAMENTOS'
          EXPORTING
            funcao        = ' '
            i_reprocessar = 'X'
          TABLES
            conslt        = xconslt2
          CHANGING
            sessao        = z02rpsessao
          EXCEPTIONS
            erro_mov      = 1
            erro_bapi     = 2
            OTHERS        = 3.
*{   INSERT         DEVK901405                                        2
        IF aux_conslt[] IS NOT INITIAL.
          REFRESH: xconslt2.
          CLEAR: xconslt2.
          xconslt2[] = aux_conslt[].
          REFRESH: aux_conslt.
          CLEAR: aux_conslt.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
*}   INSERT
*{   DELETE         DEVK901405                                        4
*\      IF sy-subrc EQ 0.
*\      ENDIF.
*}   DELETE
    ENDAT.
  ENDLOOP.
ENDFORM.                    " LANCAMENTOS_CLT

*&---------------------------------------------------------------------*
*&      Form  LANCAMENTOS_TLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lancamentos_tle.
  DATA tab LIKE z02rptmarchale OCCURS 0 WITH HEADER LINE.
  DATA tab1 LIKE z02rptextos OCCURS 0 WITH HEADER LINE.

  SELECT * FROM z02rptextos INTO TABLE tab1
                         WHERE divisao = divisao
                         AND   numregt = xtempos-regnum.
  SELECT * FROM z02rptmarchale INTO TABLE tab
                         WHERE divisao = divisao
                         AND   regnum = xtempos-regnum.
  READ TABLE tab INDEX 1.
  SELECT SINGLE * FROM z02rpsessao WHERE divisao = divisao
                                   AND   sessao = tab-sessao.
  CALL FUNCTION 'Z_02RP_LANCAMENTOS'
    EXPORTING
      funcao        = ' '
      i_reprocessar = 'X'
    TABLES
      tmarchale     = tab
      textos        = tab1
    CHANGING
      sessao        = z02rpsessao
    EXCEPTIONS
      erro_mov      = 1
      erro_bapi     = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " LANCAMENTOS_TLE

*&---------------------------------------------------------------------*
*&      Form  carrega_consprod_alt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carrega_consprod_alt.

  DATA:
    lt_consprod TYPE STANDARD TABLE OF z02rpconsprod.

  DATA:
    ls_consprod TYPE z02rpconsprod.

  REFRESH:
    lt_consprod.

* Roff - NR - 24.06.2019 - Inicio
  REFRESH xconsprod.
* Roff - NR - 24.06.2019 - Fim

  SELECT * FROM z02rpconsprod
    INTO TABLE lt_consprod
    WHERE divisao  EQ divisao
      AND consprod EQ consprod
      AND anulado  EQ space.

  LOOP AT lt_consprod INTO ls_consprod.
    CLEAR xconsprod.
    MOVE-CORRESPONDING ls_consprod TO xconsprod.
    xconsprod-mblnr_p = ls_consprod-mblnr.
    xconsprod-mjahr_p = ls_consprod-mjahr.
    APPEND xconsprod.
  ENDLOOP.

  SORT xconsprod BY aufnr linha.
ENDFORM.                    "carrega_consprod_alt
*&---------------------------------------------------------------------*
*&      Form  escreve_consprod_alt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM escreve_consprod_alt.
  CONSTANTS: col TYPE i VALUE 68.
  FORMAT RESET.
  WRITE: / sy-uline(col).
  WRITE: / sy-vline,
           text-038 COLOR COL_GROUP CENTERED,
           sy-vline.
  operacao = 'PPP'.
  HIDE operacao.
  WRITE: AT 3 icon_generate AS ICON HOTSPOT,
         text-037 COLOR COL_GROUP CENTERED.
  WRITE: AT 61 text-036 COLOR COL_GROUP CENTERED,
              icon_display_text AS ICON HOTSPOT.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_HEADING.
  WRITE: / sy-vline,
**           text-017,
           text-020,
           text-026,
           text-023,
           sy-vline,
           text-024,
           sy-vline.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_NORMAL.
  DATA l_linha_ant LIKE z02rpconsprod-linha.
  LOOP AT xconsprod.

* Roff - NR - 28.08.2019 - Inicio
    PERFORM converte_um USING xconsprod-meins_p
                     CHANGING xconsprod-meins_p.
* Roff - NR - 28.08.2019 - Fim

    WRITE: / sy-vline,
             xconsprod-aufnr,
             xconsprod-matnr,
             xconsprod-prodp,
             xconsprod-meins_p,
             sy-vline.
    IF l_linha_ant NE xconsprod-linha.
      icon_ok_ko2 xconsprod-mblnr.
    ENDIF.
    WRITE AT col sy-vline.
    l_linha_ant = xconsprod-linha.
  ENDLOOP.
  IF sy-subrc = 0.
    WRITE: / sy-uline(col).
  ENDIF.
  CLEAR: xconsprod.
ENDFORM.                    "escreve_consprod_alt
*&---------------------------------------------------------------------*
*&      Form  carrega_consprod_alt_req
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carrega_consprod_alt_req.

  SELECT * FROM z02rpconsprodl
            INTO CORRESPONDING FIELDS OF TABLE xconsprod
            WHERE divisao =  divisao
            AND   consprod = consprod
            AND   anulado = space.
  SORT xconsprod BY aufnr linha.

  SELECT * FROM z02rpconsprodl
          INTO CORRESPONDING FIELDS OF TABLE xconsprod
          WHERE divisao =  divisao
          AND   consprod = consprod
          AND   anulado = space.
  SORT xconsprod BY aufnr linha.
  LOOP AT xconsprod.
    AT NEW linha.
      SELECT SINGLE * FROM z02rpconsprod
                       WHERE divisao = xconsprod-divisao
                       AND   consprod = xconsprod-consprod
                       AND   aufnr = xconsprod-aufnr
                       AND   linha = xconsprod-linha.
    ENDAT.
    xconsprod-mblnr_p = z02rpconsprod-mblnr.
    xconsprod-mjahr_p = z02rpconsprod-mjahr.
    xconsprod-prodp = z02rpconsprod-prodp.
    xconsprod-meins_p = z02rpconsprod-meins_p.
    xconsprod-codeb = z02rpconsprod-codeb.
    MODIFY xconsprod.
  ENDLOOP.
  SELECT * FROM z02rpconsprodh
            INTO CORRESPONDING FIELDS OF TABLE xconsprodh
            WHERE divisao =  divisao
            AND   consprod = consprod.

  SORT xconsprod BY aufnr linha.

ENDFORM.                    "carrega_consprod_alt_req
*&---------------------------------------------------------------------*
*&      Form  escreve_consprod_alt_req
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM escreve_consprod_alt_req.

*  CONSTANTS: col TYPE i VALUE 117.
  CONSTANTS: col TYPE i VALUE 99.
  FORMAT RESET.
  WRITE: / sy-uline(col).
*  FORMAT COLOR COL_GROUP.
  WRITE: / sy-vline,
           text-054 COLOR COL_GROUP CENTERED,
           AT col sy-vline.
  operacao = 'PPR'.
  HIDE operacao.
  WRITE: AT 3 icon_generate AS ICON HOTSPOT,
         text-037 COLOR COL_GROUP CENTERED.
  WRITE: AT 91 text-036 COLOR COL_GROUP,
         icon_display_text AS ICON HOTSPOT.
**  FORMAT RESET.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_HEADING.
  WRITE: / sy-vline,
           text-017,
           text-020,
           text-021,
           text-022,
           text-023,
           sy-vline,
**           text-024,
           text-024,
*           text-a01.
           sy-vline.
  WRITE: / sy-uline(col).
  FORMAT RESET. FORMAT COLOR COL_NORMAL.
  DATA l_linha_ant LIKE z02rpconsprod-linha.
  LOOP AT xconsprod.

* Roff - NR - 28.08.2019 - Inicio
    PERFORM converte_um USING xconsprod-meins_p
                     CHANGING xconsprod-meins_p.

    PERFORM converte_um USING xconsprod-meins_c
                     CHANGING xconsprod-meins_c.
* Roff - NR - 28.08.2019 - Fim

    WRITE: / sy-vline,
             xconsprod-lenum USING EDIT MASK '==ALPHA',
             xconsprod-aufnr,
             xconsprod-prodp,
             xconsprod-meins_p,
             xconsprod-codeb,
             xconsprod-cons,
             xconsprod-meins_c,
             sy-vline.
    IF l_linha_ant NE xconsprod-linha.
*      icon_ok_ko2 xconsprod-mblnr_p.
      icon_ok_ko2 xconsprod-mblnr_p.
    ENDIF.
    WRITE AT col sy-vline.
    l_linha_ant = xconsprod-linha.
  ENDLOOP.
  IF sy-subrc = 0.
    WRITE: / sy-uline(col).
  ENDIF.
  CLEAR: xconsprod.
ENDFORM.                    "escreve_consprod_alt_req
*&---------------------------------------------------------------------*
*&      Form  CONVERTE_UM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XCONSPROD_MEINS_P  text
*      <--P_XCONSPROD_MEINS_P  text
*----------------------------------------------------------------------*
FORM converte_um  USING    pu_meins TYPE meins
                  CHANGING pc_meins TYPE meins.

  DATA: lv_msehi TYPE t006a-msehi.

  SELECT SINGLE msehi
    FROM t006a
    INTO lv_msehi
    WHERE spras = sy-langu
      AND msehi = pu_meins.
  IF sy-subrc NE 0.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = pu_meins
*       LANGUAGE       = SY-LANGU
      IMPORTING
        output         = pc_meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      pc_meins = pu_meins.
    ENDIF.
  ELSE.
    pc_meins = pu_meins.
  ENDIF.

ENDFORM.                    " CONVERTE_UM
****&---------------------------------------------------------------------*
****&      Form  OBTER_SESSOES_SEM_PRODUCAO
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
****  -->  p1        text
****  <--  p2        text
****----------------------------------------------------------------------*
***FORM obter_sessoes_sem_producao .
***
***  DATA: lt_z02rpsessao TYPE STANDARD TABLE OF z02rpsessao.
***
***  DATA: ls_z02rpsessao              TYPE z02rpsessao,
***        ls_xsessaodita_sem_prod_aux TYPE ty_xsessaodita,
***        ls_z02rptmarchale           TYPE z02rptmarchale.
***
***  DATA: lv_append_flag TYPE flag.
***
***  SELECT * FROM z02rpsessao
***    INTO CORRESPONDING FIELDS OF TABLE lt_z02rpsessao
***    WHERE werks IN so_werks
***      AND area IN so_area
***      AND linha IN so_linha
***      AND turno IN so_turno
***      AND data IN so_data.
***
***  LOOP AT lt_z02rpsessao INTO ls_z02rpsessao.
***
***    "Verificar se a entrada já existe na tabela xsessaodita
***    "Se não existir adicionar uma nova entrad à tabela auxiliar
***    READ TABLE xsessaodita TRANSPORTING NO FIELDS WITH KEY werks = ls_z02rpsessao-werks
***                                                            area = ls_z02rpsessao-area
***                                                           linha = ls_z02rpsessao-linha
***                                                           turno = ls_z02rpsessao-turno
***                                                            data = ls_z02rpsessao-data
***                                                          aufnr1 = ls_z02rpsessao-aufnr1
***                                                          aufnr2 = ls_z02rpsessao-aufnr2.
***    IF sy-subrc <> 0.
***
***      "Verificar se a entrada já existe na tabela xsessaodita_sem_prod_aux
***      "Se não existir adicionar uma nova entrad à tabela auxiliar
***      READ TABLE xsessaodita_sem_prod_aux TRANSPORTING NO FIELDS WITH KEY werks = ls_z02rpsessao-werks
***                                                                           area = ls_z02rpsessao-area
***                                                                          linha = ls_z02rpsessao-linha
***                                                                          turno = ls_z02rpsessao-turno
***                                                                           data = ls_z02rpsessao-data
***                                                                         aufnr1 = ls_z02rpsessao-aufnr1
***                                                                         aufnr2 = ls_z02rpsessao-aufnr2.
***
***      IF sy-subrc <> 0.
***
***        CLEAR ls_xsessaodita_sem_prod_aux.
***        ls_xsessaodita_sem_prod_aux-mandt = sy-mandt.
***        ls_xsessaodita_sem_prod_aux-werks = ls_z02rpsessao-werks.
***        ls_xsessaodita_sem_prod_aux-area = ls_z02rpsessao-area.
***        ls_xsessaodita_sem_prod_aux-linha = ls_z02rpsessao-linha.
***        ls_xsessaodita_sem_prod_aux-turno = ls_z02rpsessao-turno.
***        ls_xsessaodita_sem_prod_aux-data = ls_z02rpsessao-data.
***        ls_xsessaodita_sem_prod_aux-aufnr1 = ls_z02rpsessao-aufnr1.
***        ls_xsessaodita_sem_prod_aux-aufnr2 = ls_z02rpsessao-aufnr2.
***        ls_xsessaodita_sem_prod_aux-subsessao = 1.
***        ls_xsessaodita_sem_prod_aux-tempos = 'X'.
***        ls_xsessaodita_sem_prod_aux-divisao = ls_z02rpsessao-divisao.
***        ls_xsessaodita_sem_prod_aux-tempos_par_sem_prod_flg = 'X'.
***        ls_xsessaodita_sem_prod_aux-sessao = ls_z02rpsessao-sessao.
***
***        "Por cada entrada é necessário verificar se existem registos de confirmação de tempos com erro
***        CLEAR: lv_append_flag.
***        SELECT *
***          INTO ls_z02rptmarchale
***          FROM z02rptmarchale
***          WHERE divisao = divisao
***            AND aufnr = ls_z02rpsessao-aufnr1
***            AND tplinha <> space
***            AND tmlinha = space
******          AND confirmacao = space
***            AND consprod = space
***            AND sessao = ls_z02rpsessao-sessao
***            AND anulado = space
***            AND qtd_sac = space.
***
***          IF sy-subrc EQ 0.
***
***            lv_append_flag = 'X'.
***
***            IF ls_z02rptmarchale-confirmacao IS INITIAL.
***
**** existem registo de confirmação de tempos com erro
***              ls_xsessaodita_sem_prod_aux-tempos = '0'.
***              EXIT.
***            ENDIF.
***          ENDIF.
***          CLEAR: ls_z02rptmarchale.
***        ENDSELECT.
***
***        IF lv_append_flag IS NOT INITIAL.
***          APPEND ls_xsessaodita_sem_prod_aux TO xsessaodita_sem_prod_aux.
***        ENDIF.
***
***      ENDIF.
***    ENDIF.
***
***    CLEAR: ls_z02rpsessao.
***  ENDLOOP.
***
***ENDFORM.
****&---------------------------------------------------------------------*
****&      Form  CARREGA_SESSOES_DITA_SEM_PROD
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
****  -->  p1        text
****  <--  p2        text
****----------------------------------------------------------------------*
***FORM carrega_sessoes_dita_sem_prod .
***
***  APPEND LINES OF xsessaodita_sem_prod_aux TO xsessaodita.
***
***  SORT xsessaodita BY data turno area linha.
***
***ENDFORM.
****&---------------------------------------------------------------------*
****&      Form  LOG_SESSAO_DITA_SEM_PROD
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
****  -->  p1        text
****  <--  p2        text
****----------------------------------------------------------------------*
***FORM log_sessao_dita_sem_prod .
***
***  PERFORM carrega_tempos_sem_prod.
***  PERFORM escreve_tempos.
***
***  CLEAR operacao.
***  sy-lsind = sy-lsind - 1.
***
***ENDFORM.
****&---------------------------------------------------------------------*
****&      Form  CARREGA_TEMPOS_SEM_PROD
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
****  -->  p1        text
****  <--  p2        text
****----------------------------------------------------------------------*
***FORM carrega_tempos_sem_prod .
***
***  SELECT *
***    FROM z02rptmarchale
***    INTO TABLE xtempos
***    WHERE divisao =  divisao
***      AND aufnr = aufnr
***      AND tplinha <> space
***      AND tmlinha = space
***      AND consprod = space
***      AND sessao = sessao_sem_prod
***      AND anulado = space
***      AND qtd_sac = space.
***
***  SORT xtempos BY aufnr linha vornr.
***
***ENDFORM.
