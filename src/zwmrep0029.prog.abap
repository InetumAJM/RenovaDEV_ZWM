*&---------------------------------------------------------------------*
*& Report  ZWMREP0029                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0029.

TYPE-POOLS: abap.

TABLES: mara,
        makt,
        vekp,
        vepo,
        kna1,
        vbpa,
        adrc,
        likp,
        lips,
        vbap,
        vbak,
        eket,
        vbep,
        ltap,
        zwm026,
        zwm044,
        zwm069.

DATA: desc1  LIKE makt-maktx,
      desc2  LIKE makt-maktx,
      desc3  LIKE makt-maktx,
      matnr  LIKE mara-matnr,
      sscc   LIKE vekp-exidv,
      lang   LIKE sy-langu,
      linhas TYPE i.

DATA: t_makt LIKE makt OCCURS 0 WITH HEADER LINE.

DATA: linha1(80), linha2(80), linha3(80),linha4(80),
      linha5(80), linha6(80), linha7(80),linha8(80),
      linha9(80), linha10(80), linha11(80),linha12(80),
      linha13(80), linha14(80), linha15(80),linha16(80),
      linha17(80), linha18(80), linha19(80),linha20(80),
      linha21(80), linha22(80),linha23(80).

*DATA: t_vepo LIKE vepo OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF t_vepo OCCURS 1.
        INCLUDE STRUCTURE vepo.
        DATA: pedido LIKE vbak-bstnk.
DATA: END OF t_vepo.


DATA: BEGIN OF tv OCCURS 1.
        INCLUDE STRUCTURE vepo.
        DATA: pedido LIKE vbak-bstnk.
DATA: END OF tv.


DATA: t_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF guias OCCURS 0.
DATA: kunnr      LIKE likp-kunnr,
      vbeln      LIKE likp-vbeln,
      order      LIKE vbak-vbeln,
      order_item LIKE vbap-posnr,
      pedido     LIKE vbak-bstnk.
DATA: END OF guias.

DATA: BEGIN OF wes OCCURS 0,
        kunnr LIKE vbpa-kunnr,
        adrnr LIKE adrc-addrnumber.
DATA:END OF wes.

DATA: BEGIN OF w1s OCCURS 0,
        kunnr LIKE vbpa-kunnr,
        adrnr LIKE adrc-addrnumber.
DATA:END OF w1s.

DATA: we_name2          LIKE adrc-name2,
      we_street         LIKE adrc-street,
      we_postcode1      LIKE adrc-post_code1,
      we_city1          LIKE adrc-city1,
      we_numguia        LIKE likp-vbeln,
      we_numorder       LIKE vbak-bstnk,
      ag_name1          LIKE adrc-name1,
      delivery_date(10) TYPE c.

DATA: totalboxes TYPE i,
      notas(80)  TYPE c,
      notas2(80) TYPE c,
      notas3(80) TYPE c.


DATA: ls_marm  TYPE marm,
      lv_value TYPE menge_d,
      ls_vepo  TYPE vepo,
      ls_ltap  TYPE ltap.
*---------------------------------------------------------------------*
*       FORM get_descricoes                                           *
*---------------------------------------------------------------------*
FORM get_descricoes TABLES in_par STRUCTURE itcsy
                           out_par STRUCTURE itcsy.

  READ TABLE in_par WITH KEY 'EAN128-MATNR'.
  CHECK sy-subrc = 0.

  CLEAR: desc1,
         desc2,
         desc3,
         matnr,
         t_makt.

  REFRESH t_makt.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = in_par-value
    IMPORTING
      output       = matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  SELECT * INTO TABLE t_makt
    FROM makt
      WHERE matnr = matnr.

  READ TABLE t_makt INDEX 1.
  IF sy-subrc = 0.
    desc1 = t_makt-maktx.
  ENDIF.

  READ TABLE t_makt INDEX 2.
  IF sy-subrc = 0.
    desc2 = t_makt-maktx.
  ENDIF.

  READ TABLE t_makt INDEX 3.
  IF sy-subrc = 0.
    desc3 = t_makt-maktx.
  ENDIF.

  READ TABLE out_par WITH KEY 'DESC1'.
  CHECK sy-subrc = 0.
  MOVE desc1 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'DESC2'.
  CHECK sy-subrc = 0.
  MOVE desc2 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'DESC3'.
  CHECK sy-subrc = 0.
  MOVE desc3 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

ENDFORM.                    "get_descricoes

*---------------------------------------------------------------------*
*       form GET_ENTREGA_PICKING                                      *
* PAULO SOUSA EM 2006.04.25 - ADICIONAR DADOS DE GUIA A ETIQUETAS     *
*---------------------------------------------------------------------*

FORM get_entrega_picking TABLES in_par STRUCTURE itcsy
                         out_par STRUCTURE itcsy.

  DATA linhas TYPE i.
  DATA adrn LIKE vbpa-adrnr.
  DATA: sytabix LIKE sy-tabix,
        zkun    LIKE knvp-kunnr.

  DATA: tipoencomenda TYPE c,
*        itemencomenda like vbap-posnr,
        datapedido    LIKE vbak-bstdk,
        encomenda     LIKE vbak-vbeln,
        itemremessa   LIKE lips-posnr,
        gln(13)       TYPE c, nome(35) TYPE c.

  CLEAR: we_name2,
         we_street,
         we_postcode1,
         we_city1,
         we_numguia,
         ag_name1,
         delivery_date.

  DATA: lv_2step       TYPE flag,
        lv_2spart      TYPE flag,
        lv_sscc_master TYPE lenum,
        ls_t311a       TYPE t311a,
        lv_lgnum       TYPE lgnum.

  IF in_par-value IS INITIAL.
    READ TABLE in_par WITH KEY 'EAN128-EXIDV'.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = in_par-value
    IMPORTING
      output = sscc.

  CLEAR: wes, w1s, guias.

  REFRESH wes.
  REFRESH w1s.
  REFRESH guias.

  PERFORM get_whs CHANGING lv_lgnum.

** verificar se a guia está embalada
  CLEAR vekp.
  SELECT SINGLE * FROM vekp WHERE exidv = sscc AND
                                  status <> '0060'.

  IF NOT vekp-vbeln_gen IS INITIAL.
    guias-vbeln = vekp-vbeln_gen.
    APPEND guias.
  ELSE.
**   guia não embalada
**   procurar na tabela de picking
    REFRESH t_zwm026.
    SELECT * INTO TABLE t_zwm026
        FROM zwm026
            WHERE armazem = lv_lgnum
              AND sscc = sscc.
    IF t_zwm026[] IS INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE t_zwm026
          FROM zwm026h
              WHERE armazem = lv_lgnum
                AND sscc = sscc.

      IF sy-subrc IS NOT INITIAL.

*        select single sscc_master from zwm061
*          into lv_sscc_master
*          where lgnum = '100'
*            and sscc = sscc.
*
*        if sy-subrc is initial.
*
*          select single * into t_zwm026
*          from zwm026
*              where armazem = '100'
*                and sscc = lv_sscc_master.
*
*          if sy-subrc is not initial.
*            select single * into t_zwm026
*                from zwm026h
*                    where armazem = '100'
*                      and sscc = lv_sscc_master.
*
*            if sy-subrc is initial.
*              append t_zwm026.
*            endif.
*
*          else.
*            append t_zwm026.
*
*          endif.
*
*        endif.
*
*** <- Pedro F. Lopes - 24.02.2015 16:25:20

      ENDIF.

    ENDIF.
**   determinar remessas com items na palete

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 13.09.2012 11:55:43
*  Motivo: Valida se Grupo é em 2 Passos
*--------------------------------------------------------------------*
    READ TABLE t_zwm026 INDEX 1.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = t_zwm026-armazem
        i_refnr  = t_zwm026-grupo
        i_vbeln  = t_zwm026-remessa
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2step EQ abap_true AND
       lv_2spart EQ abap_false.
      SELECT SINGLE * FROM t311a
                      INTO ls_t311a
                      WHERE lgnum = t_zwm026-armazem AND
                            refnr = t_zwm026-grupo.

      guias-vbeln = ls_t311a-rbnum.
      APPEND guias.
    ELSEIF lv_2spart EQ abap_true.
      guias-vbeln = t_zwm026-remessa.
      APPEND guias.
    ELSE.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

      CLEAR itemremessa.
      LOOP AT t_zwm026.
        sytabix = sy-tabix.
        READ TABLE guias WITH KEY vbeln = t_zwm026-remessa.
        IF sy-subrc NE 0.
          guias-vbeln = t_zwm026-remessa.
          APPEND guias.
        ENDIF.
        sy-tabix = sytabix.
      ENDLOOP.
    ENDIF.
  ENDIF.

** Obter dados adicionais das guias
  LOOP AT guias.
    sytabix = sy-tabix.
    CALL FUNCTION 'Z_GET_ENCOMENDA_DA_REMESSA'
      EXPORTING
        remessa        = guias-vbeln
        item_remessa   = itemremessa
      IMPORTING
        encomenda      = guias-order
        tipo_encomenda = tipoencomenda
        pedido         = guias-pedido
        datapedido     = datapedido
        item_encomenda = guias-order_item.

*      CALL FUNCTION 'Z_EXTRAI_NUM_ENC'
*          EXPORTING
*            REMESSA              = guias-vbeln
*            ITEM_REMESSA         = itemremessa
*          IMPORTING
*            ENCOMENDA            = guias-order
*            TIPO_ENCOMENDA       = tipoencomenda
*            PEDIDO               = guias-pedido
*            DATAPEDIDO           = datapedido.
    MODIFY guias INDEX sytabix.
    sy-tabix = sytabix.
  ENDLOOP.

  IF NOT guias[] IS INITIAL.
    SELECT kunnr adrnr INTO TABLE w1s
        FROM vbpa
            FOR ALL ENTRIES IN guias
                WHERE vbeln = guias-vbeln
                  AND posnr = 0
                  AND parvw = 'W1'.

    SORT w1s.
    DELETE ADJACENT DUPLICATES FROM w1s.

    SELECT kunnr adrnr INTO TABLE wes
        FROM vbpa
            FOR ALL ENTRIES IN guias
                WHERE vbeln = guias-vbeln
                  AND posnr = 0
                  AND parvw = 'WE'.

    SORT wes.
    DELETE ADJACENT DUPLICATES FROM wes.

    DATA n_guia TYPE i.
    CLEAR n_guia.
    DESCRIBE TABLE guias LINES n_guia.
    IF n_guia = 1.
      READ TABLE w1s INDEX 1.
      IF sy-subrc = 0.
        adrn = w1s-adrnr.
        IF w1s-kunnr = '0000300001' OR ( lv_2step EQ abap_true AND lv_2spart EQ abap_false ).
          SELECT SINGLE * FROM kna1 WHERE kunnr = '0000300001'. "w1s-kunnr.
          IF sy-subrc = 0.
            adrn = kna1-adrnr.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE wes INDEX 1.
        IF sy-subrc = 0.
          adrn = wes-adrnr.
          IF w1s-kunnr = '0000300001'.
            SELECT SINGLE * FROM kna1 WHERE kunnr = w1s-kunnr.
            IF sy-subrc = 0.
              adrn = kna1-adrnr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT w1s WHERE kunnr = '0000300001'.
      adrn = w1s-adrnr.
      SELECT SINGLE * FROM kna1 WHERE kunnr = w1s-kunnr.
      IF sy-subrc = 0.
        adrn = kna1-adrnr.
      ENDIF.
      EXIT.
    ENDLOOP.

    IF adrn IS INITIAL.
      LOOP AT wes WHERE kunnr = '0000300001'.
        adrn = wes-adrnr.
        SELECT SINGLE * FROM kna1 WHERE kunnr = wes-kunnr.
        IF sy-subrc = 0.
          adrn = kna1-adrnr.
        ENDIF.
        EXIT.
      ENDLOOP.
      IF adrn IS INITIAL.
        READ TABLE w1s INDEX 1.
        IF sy-subrc = 0.
          adrn = w1s-adrnr.
        ELSE.
          READ TABLE wes INDEX 1.
          IF sy-subrc = 0.
            adrn = wes-adrnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

** alterado paulo sousa 2011.11.18 - we devolve numero de remessa da 1ª guia.

    CLEAR we_numguia.
    READ TABLE guias INDEX 1.
    we_numguia = guias-vbeln.
    encomenda = guias-order.
*
**    2011.08.09 - Paulo Sousa
**    "Martelada" INSCO. para WE= 103345 ou 106719, colocar NAME1 de AG no WE_NAME2
    READ TABLE wes INDEX 1.
    IF sy-subrc = 0.
      IF wes-kunnr = '0000103345' OR wes-kunnr = '0000106719'. "INSCO
        SELECT SINGLE * FROM vbak WHERE vbeln = encomenda.
        IF sy-subrc = 0.
          SELECT SINGLE * FROM kna1 WHERE kunnr = vbak-kunnr.
          IF sy-subrc = 0.
            ag_name1 = kna1-name1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*    ENDIF.
**   Martelada System-U
    SELECT SINGLE * FROM vbak WHERE vbeln = encomenda.
    IF vbak-kunnr = '0000601120'.
      DATA: pdv  LIKE zsystemu-code_pdv,
            jour LIKE zsystemu-jour_liv.

      CLEAR: gln, jour, pdv, nome.
      SELECT SINGLE * FROM vbpa WHERE vbeln = guias-order
                                  AND posnr = guias-order_item
                                  AND parvw = 'EN'.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM kna1 WHERE kunnr = vbpa-kunnr.
        IF sy-subrc = 0.
          CONCATENATE kna1-bbbnr kna1-bbsnr kna1-bubkz INTO gln.
          SELECT SINGLE code_pdv acronyme jour_liv FROM zsystemu INTO (pdv, nome, jour) WHERE gln = gln.
          CONCATENATE pdv nome INTO notas2 SEPARATED BY space.
          CONCATENATE 'JOUR DE LIV = ' jour INTO notas3 SEPARATED BY space.
        ENDIF.
        CLEAR: gln, jour, pdv, nome, zkun.
        SELECT SINGLE kunn2 FROM knvp INTO zkun WHERE kunnr = vbpa-kunnr
                                                  AND parvw = 'W1'.
        IF zkun <> ''.
          SELECT SINGLE * FROM kna1 WHERE kunnr = zkun.
          IF sy-subrc = 0.
            CONCATENATE kna1-bbbnr kna1-bbsnr kna1-bubkz INTO gln.
            SELECT SINGLE acronyme INTO (nome) FROM zsystemu WHERE gln = gln.
            notas = nome.
          ENDIF.
        ENDIF.
      ENDIF.
**      para este cliente, morada nas etiquetas é a do WE
      READ TABLE wes INDEX 1.
      adrn = wes-adrnr.
    ENDIF.


** Determinar dados da morada do local de descarga
    IF NOT adrn IS INITIAL.
      SELECT SINGLE * FROM adrc WHERE addrnumber = adrn.
      IF sy-subrc = 0.
        we_name2 = adrc-name1.
        IF we_name2 IS INITIAL.
          we_name2 = adrc-name2.
        ENDIF.
        we_street = adrc-street.
        we_postcode1 = adrc-post_code1.
        we_city1 = adrc-city1.
      ENDIF.
    ENDIF.
** determinar data da remessa. se existirem varias, todas terao a mesma data.

    SELECT SINGLE * FROM likp WHERE vbeln = guias-vbeln.
    IF sy-subrc = 0.
      delivery_date = likp-lfdat.
      CONCATENATE delivery_date(4) '.' delivery_date+4(2) '.' delivery_date+6(2) INTO delivery_date.
    ENDIF.

  ENDIF.

  IF lv_2step EQ abap_true AND
     lv_2spart EQ abap_false.
    we_numguia = '9999999999'.
  ENDIF.

**  we_name2
  READ TABLE out_par WITH KEY 'WE_NAME2'.
  CHECK sy-subrc = 0.
  MOVE we_name2 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

**we_street
  READ TABLE out_par WITH KEY 'WE_STREET'.
  CHECK sy-subrc = 0.
  MOVE we_street TO out_par-value.
  MODIFY out_par INDEX sy-tabix.
**we_post_code1
  READ TABLE out_par WITH KEY 'WE_POST_CODE1'.
  CHECK sy-subrc = 0.
  MOVE we_postcode1 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.
**we_city
  READ TABLE out_par WITH KEY 'WE_CITY1'.
  CHECK sy-subrc = 0.
  MOVE we_city1 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.
**we_numguia
  READ TABLE out_par WITH KEY 'WE_NUMGUIA'.
  CHECK sy-subrc = 0.
  MOVE we_numguia TO out_par-value.
  MODIFY out_par INDEX sy-tabix.
**DELIVERY_DATE
  READ TABLE out_par WITH KEY 'DELIVERY_DATE'.
  CHECK sy-subrc = 0.
  MOVE delivery_date TO out_par-value.
  MODIFY out_par INDEX sy-tabix.
**we_numorder
  READ TABLE out_par WITH KEY 'WE_NUMORDER'.
  CHECK sy-subrc = 0.
  MOVE we_numorder TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

**  ag_name1
  READ TABLE out_par WITH KEY 'AG_NAME1'.
  CHECK sy-subrc = 0.
  MOVE ag_name1 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

****  Notas
  READ TABLE out_par WITH KEY 'NOTAS'.
  CHECK sy-subrc = 0.
  MOVE notas TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

****  Notas2
  READ TABLE out_par WITH KEY 'NOTAS2'.
  CHECK sy-subrc = 0.
  MOVE notas2 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

****  Notas3
  READ TABLE out_par WITH KEY 'NOTAS3'.
  CHECK sy-subrc = 0.
  MOVE notas3 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.
ENDFORM.                    "get_entrega_picking
*---------------------------------------------------------------------*
*       form GET_NUMERO_ORDEM
* PAULO SOUSA EM 2006.04.25 - ADICIONAR NUMERO DE ORDEM A ETIQUETAS   *
*---------------------------------------------------------------------*

FORM get_entrega_standard TABLES in_par STRUCTURE itcsy
                                 out_par STRUCTURE itcsy.

  DATA adr LIKE vbpa-adrnr.

  DATA: lv_2step  TYPE flag,
        lv_2spart TYPE flag,
        lv_lgnum  TYPE lgnum.

  DATA: lt_ltap_sscc TYPE TABLE OF ltap,
        lt_ltak_sscc TYPE TABLE OF ltak,
        ls_ltak_sscc TYPE ltak.

  DATA: tipoencomenda TYPE c,
        datapedido    LIKE vbak-bstdk,
        encomenda     LIKE vbak-vbeln,
        itemremessa   LIKE lips-posnr.

  CLEAR: we_name2,
           ag_name1,
           we_street,
           we_postcode1,
           we_city1,
           we_numguia,
           delivery_date,
           we_numorder,
           adr.
  DATA: gln(13)  TYPE c, nome(35) TYPE c, zkun LIKE kna1-kunnr.

  DATA lv_sscc_master TYPE lenum.

  CLEAR delivery_date.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = in_par-value
    IMPORTING
      output = sscc.

  PERFORM get_whs CHANGING lv_lgnum.

  SELECT SINGLE * FROM vekp WHERE exidv = sscc AND
                                  status <> '0060'.

  CLEAR we_numguia.
  IF vekp-vbeln_gen IS NOT INITIAL.
**   guia embalada.
    we_numguia = vekp-vbeln_gen.
  ELSE.
    IF ( ( vekp-vpobj = '01' ) AND ( vekp-vpobjkey IS NOT INITIAL ) ).
      we_numguia = vekp-vpobjkey.
    ELSE.
**     palete ainda não embalada
**     verificar se é picking monoreferencia => procurar em zwm026
      SELECT SINGLE * FROM zwm026 WHERE armazem = lv_lgnum AND sscc = sscc.
      IF sy-subrc = 0.
        we_numguia = zwm026-remessa.

      ELSE.
**       verificar se é palete standard homogenea =>  procurar em ltap
        DATA lt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.
        DATA lt_ltap2 LIKE ltap OCCURS 0 WITH HEADER LINE.

        CLEAR:   lt_ltap, lt_ltap2.
        REFRESH: lt_ltap, lt_ltap2.

        SELECT * INTO TABLE lt_ltap
            FROM ltap
               WHERE lgnum = lv_lgnum
                  AND vlenr = sscc.

        SELECT * INTO TABLE lt_ltap2
            FROM ltap
                WHERE lgnum = lv_lgnum
                  AND nlenr = sscc.

        LOOP AT lt_ltap2.
          MOVE-CORRESPONDING lt_ltap2 TO lt_ltap.
          APPEND lt_ltap.
        ENDLOOP.

        DELETE lt_ltap WHERE vorga = 'ST'.

        SORT lt_ltap BY edatu DESCENDING
                        ezeit DESCENDING
                        tanum DESCENDING.
        READ TABLE lt_ltap INDEX 1 INTO ltap.
*
*        SELECT SINGLE *
*           FROM ltap
*               WHERE lgnum = vekp-lgnum
*                 AND vlenr = sscc
*               AND vorga <> 'ST'
*                AND vbeln <> ' '.
*        IF sy-subrc = 0.
        IF ltap-vbeln IS NOT INITIAL.
          we_numguia = ltap-vbeln.
        ELSE.
* -> Pedro F. Lopes - 24.02.2015 16:25:20
          SELECT SINGLE sscc_master FROM zwm061
            INTO lv_sscc_master
            WHERE lgnum = lv_lgnum
              AND sscc = sscc.

          IF sy-subrc IS INITIAL.

            SELECT SINGLE *
            FROM zwm026
                WHERE armazem = lv_lgnum
                  AND sscc = lv_sscc_master.

            we_numguia = zwm026-remessa.

          ENDIF.

** <- Pedro F. Lopes - 24.02.2015 16:25:20

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 13.09.2012 11:55:43
*  Motivo: Valida se Grupo é em 2 Passos
*--------------------------------------------------------------------*
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = zwm026-armazem
      i_refnr  = zwm026-grupo
      i_vbeln  = zwm026-remessa
    IMPORTING
      e_2step  = lv_2step
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF lv_2step EQ abap_true AND
     lv_2spart EQ abap_false.
    SELECT SINGLE rbnum FROM t311a
                        INTO we_numguia
                        WHERE lgnum = zwm026-armazem AND
                              refnr = zwm026-grupo.
  ELSEIF lv_2spart EQ abap_true AND
         we_numguia IS INITIAL.
    we_numguia = zwm026-remessa.
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


  DO 1 TIMES.
    CHECK we_numguia IS INITIAL.
    SELECT * FROM ltap
             INTO TABLE lt_ltap_sscc
             WHERE lgnum = lv_lgnum
             AND vlenr = sscc
             AND vorga <> 'ST'.

    CHECK sy-subrc EQ 0.

    SELECT * FROM ltak
             INTO TABLE lt_ltak_sscc
             FOR ALL ENTRIES IN lt_ltap_sscc
             WHERE lgnum = lt_ltap_sscc-lgnum AND
                   tanum = lt_ltap_sscc-tanum.
    CHECK sy-subrc EQ 0.

    DELETE lt_ltak_sscc WHERE betyp <> 'L' OR
                              benum IS INITIAL.

    CHECK NOT lt_ltak_sscc IS INITIAL.

    SORT lt_ltak_sscc BY bdatu DESCENDING
                         bzeit DESCENDING.

    CLEAR: ls_ltak_sscc.
    READ TABLE lt_ltak_sscc
          INTO ls_ltak_sscc
          INDEX 1.

    we_numguia = ls_ltak_sscc-benum.
  ENDDO.

** INI- Paletização Especial
  SELECT SINGLE * FROM zwm044 WHERE exidv = sscc AND status = 'F'.
  IF sy-subrc = 0.
    we_numguia = zwm044-vbeln.
    APPEND guias.
  ELSE.
** FIM- Paletização Especial
    SELECT SINGLE * FROM zwm069 WHERE sscc = sscc.
    IF sy-subrc = 0.
      we_numguia = zwm069-vbeln.
      APPEND guias.
    ENDIF.
  ENDIF.

  CLEAR adr.
  IF NOT we_numguia IS INITIAL.

    IF lv_2step IS INITIAL OR
       lv_2spart EQ abap_true.
      SELECT SINGLE * FROM vbpa WHERE vbeln = we_numguia
                                               AND posnr = 0
                                               AND parvw = 'W1'.
      IF sy-subrc <> 0.
        SELECT SINGLE * FROM vbpa WHERE vbeln = we_numguia
                                                 AND posnr = 0
                                                 AND parvw = 'WE'.
      ENDIF.

      adr = vbpa-adrnr.
    ELSE.
      SELECT SINGLE adrnr
               FROM kna1
               INTO adr
               WHERE kunnr = '0000300001'.
    ENDIF.

    SELECT SINGLE * FROM likp WHERE vbeln = we_numguia.
    IF sy-subrc = 0.
**      data de entrega
      delivery_date = likp-lfdat.
      CONCATENATE delivery_date(4) '.' delivery_date+4(2) '.' delivery_date+6(2) INTO delivery_date.
    ENDIF.

**  2011.08.10 - Paulo Sousa
**  preencher we_numorder com numero de pedido do cliente
    CLEAR itemremessa.
    DATA: ie LIKE vbap-posnr.
    CALL FUNCTION 'Z_GET_ENCOMENDA_DA_REMESSA'
      EXPORTING
        remessa        = we_numguia
        item_remessa   = itemremessa
      IMPORTING
        encomenda      = encomenda
        tipo_encomenda = tipoencomenda
        pedido         = we_numorder
        datapedido     = datapedido
        item_encomenda = ie.

*********************************************
**    2011.08.09 - Paulo Sousa
**    "Martelada" INSCO. para WE= 103345 ou 106719, colocar NAME1 de AG no WE_NAME2
    IF vbpa-kunnr = '0000103345' OR vbpa-kunnr = '0000106719'. "INSCO
      SELECT SINGLE * FROM vbak WHERE vbeln = encomenda.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM kna1 WHERE kunnr = vbak-kunnr.
        IF sy-subrc = 0.
          ag_name1 = kna1-name1.
        ENDIF.
      ENDIF.
    ENDIF.
**   Martelada System-U
    SELECT SINGLE * FROM vbak WHERE vbeln = encomenda.
    IF vbak-kunnr = '0000601120'.

      SELECT SINGLE * FROM vbpa WHERE vbeln = guias-order
                                  AND posnr = guias-order_item
                                  AND parvw = 'EN'.
      IF sy-subrc = 0.
        CLEAR zkun.
        SELECT SINGLE kunn2 FROM knvp INTO zkun WHERE kunnr = vbpa-kunnr
                                                  AND parvw = 'W1'.
        IF zkun <> ''.
          SELECT SINGLE * FROM kna1 WHERE kunnr = zkun.
          IF sy-subrc = 0.
            CONCATENATE kna1-bbbnr kna1-bbsnr kna1-bubkz INTO gln.
            SELECT SINGLE acronyme INTO (nome) FROM zsystemu WHERE gln = gln.
            CONCATENATE 'PLATEFORME DESTINATAIRE:  ' nome INTO notas.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF adr <> ''.
      SELECT SINGLE * FROM adrc WHERE addrnumber = adr.
      IF sy-subrc = 0.
        we_name2 = adrc-name1.
        IF we_name2 IS INITIAL.
          we_name2 = adrc-name2.
        ENDIF.
        we_street = adrc-street.
        we_postcode1 = adrc-post_code1.
        we_city1 = adrc-city1.
      ENDIF.
    ENDIF.

    IF lv_2step EQ abap_true AND
       lv_2spart EQ abap_false.
      we_numguia = '9999999999'.
    ENDIF.

**  we_name2
    READ TABLE out_par WITH KEY 'WE_NAME2'.
    CHECK sy-subrc = 0.
    MOVE we_name2 TO out_par-value.
    MODIFY out_par INDEX sy-tabix.

**we_street
    READ TABLE out_par WITH KEY 'WE_STREET'.
    CHECK sy-subrc = 0.
    MOVE we_street TO out_par-value.
    MODIFY out_par INDEX sy-tabix.
**we_post_code1
    READ TABLE out_par WITH KEY 'WE_POST_CODE1'.
    CHECK sy-subrc = 0.
    MOVE we_postcode1 TO out_par-value.
    MODIFY out_par INDEX sy-tabix.
**we_city
    READ TABLE out_par WITH KEY 'WE_CITY1'.
    CHECK sy-subrc = 0.
    MOVE we_city1 TO out_par-value.
    MODIFY out_par INDEX sy-tabix.
**we_numguia
    READ TABLE out_par WITH KEY 'WE_NUMGUIA'.
    CHECK sy-subrc = 0.
    MOVE we_numguia TO out_par-value.
    MODIFY out_par INDEX sy-tabix.
**DELIVERY_DATE
    READ TABLE out_par WITH KEY 'DELIVERY_DATE'.
    CHECK sy-subrc = 0.
    MOVE delivery_date TO out_par-value.
    MODIFY out_par INDEX sy-tabix.
**we_numordem
    READ TABLE out_par WITH KEY 'WE_NUMORDER'.
    CHECK sy-subrc = 0.
    MOVE we_numorder TO out_par-value.
    MODIFY out_par INDEX sy-tabix.

**  ag_name1
    READ TABLE out_par WITH KEY 'AG_NAME1'.
    CHECK sy-subrc = 0.
    MOVE ag_name1 TO out_par-value.
    MODIFY out_par INDEX sy-tabix.

****  Notas
    READ TABLE out_par WITH KEY 'NOTAS'.
    CHECK sy-subrc = 0.
    MOVE notas TO out_par-value.
    MODIFY out_par INDEX sy-tabix.
  ENDIF.
ENDFORM.                    "get_entrega_standard

*---------------------------------------------------------------------*
*       FORM get_conteudo                                             *
*---------------------------------------------------------------------*
FORM get_conteudo TABLES in_par STRUCTURE itcsy
                         out_par STRUCTURE itcsy.

  DATA: qtd(20),
        colis(4).
  DATA: linhas        TYPE i,
        sytabix       LIKE sy-tabix,
        itemremessa   LIKE lips-posnr,
        numencomenda  LIKE vbak-vbeln,
        itemencomenda LIKE vbap-posnr,
        tipoencomenda TYPE c,
        datapedido    LIKE vbak-bstdk.

  DATA tipo_palete LIKE vekp-vhilm.
  DATA: isallotie  TYPE c,
        zaugru     TYPE augru,
        allotie(8) TYPE c.

  READ TABLE in_par WITH KEY 'EAN128-EXIDV'.
  CHECK sy-subrc = 0.

  CLEAR: linha1,
         linha2,
         linha3,
         linha4,
         linha5,
         linha6,
         linha7,
         linha8,
         linha9,
         linha10,
         linha11,
         linha12,
         linha13,
         linha14,
         linha15,
         linha16,
         linha17,
         linha18,
         linha19,
         linha20,
         linha21,
         linha22,
         linha23,
         sscc,
         tipo_palete,
         t_vepo.


  totalboxes = 0.
  REFRESH: t_vepo, tv.
  REFRESH t_zwm026.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = in_par-value
    IMPORTING
      output = sscc.

  SELECT SINGLE * FROM vekp WHERE exidv = sscc.
  IF sy-subrc = 0.
    tipo_palete = vekp-vhilm.
  ENDIF.

  SELECT * INTO TABLE t_vepo FROM vepo WHERE venum = vekp-venum.

** As seguintes linhas foram alteradas por paulo sousa
** Descriminar por linha as quantidades do produto.
** implica fazer leitura a partir de t_vepo a partir de zwm026 e não vepo

  SELECT * INTO TABLE t_zwm026 FROM zwm026 WHERE sscc = sscc.
  IF t_zwm026[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_zwm026 FROM zwm026h WHERE sscc = sscc.
  ENDIF.
  DESCRIBE TABLE t_zwm026 LINES linhas.
  IF linhas = 0.
**    salvaguardar situação de picking inverso
**    actualizar num de remessa em t_vepo a partir de T_zwm026
    LOOP AT t_vepo.
      sytabix = sy-tabix.
      totalboxes = totalboxes + t_vepo-vemng.
      READ TABLE t_zwm026 INDEX sy-tabix.
      IF sy-subrc = 0.
        t_vepo-vbeln = t_zwm026-remessa.
      ENDIF.
*     MODIFY t_vepo INDEX sytabix.
**    paulo sousa 2011.11.18 - adicionar PO a cada linha
      CLEAR itemremessa.

      CALL FUNCTION 'Z_GET_ENCOMENDA_DA_REMESSA'
        EXPORTING
          remessa        = t_vepo-vbeln
          item_remessa   = itemremessa
        IMPORTING
          encomenda      = numencomenda
          tipo_encomenda = tipoencomenda
          pedido         = t_vepo-pedido
          datapedido     = datapedido
          item_encomenda = itemencomenda.


      MODIFY t_vepo INDEX sytabix.
*      ENDIF.
      sy-tabix = sytabix.
    ENDLOOP.
  ELSE.
    REFRESH t_vepo.
    LOOP AT t_zwm026.
      CLEAR t_vepo.


      t_vepo-matnr = t_zwm026-material.
**
*      totalboxes = totalboxes + t_zwm026-quantidade.

      SELECT SINGLE * FROM ltap INTO ls_ltap
        WHERE lgnum = t_zwm026-armazem  AND
              tanum = t_zwm026-to_number.
      IF sy-subrc = 0.
        CALL FUNCTION 'ROUND'
          EXPORTING
            decimals      = 0
            input         = ls_ltap-nsola
            sign          = 'X'
          IMPORTING
            output        = t_vepo-vemng
          EXCEPTIONS
            input_invalid = 1
            overflow      = 2
            type_invalid  = 3
            OTHERS        = 4.
        t_vepo-vemeh = ls_ltap-altme.
      ELSE.
        t_vepo-vemng = t_zwm026-quantidade.
        t_vepo-vemeh = t_zwm026-unidade.
      ENDIF.

      totalboxes = totalboxes + t_vepo-vemng.
      t_vepo-charg = t_zwm026-lote.
      t_vepo-vbeln = t_zwm026-remessa.
      t_vepo-posnr = t_zwm026-posnr.
      CLEAR itemremessa.

      DATA: ie LIKE vbap-posnr.
      CALL FUNCTION 'Z_GET_ENCOMENDA_DA_REMESSA'
        EXPORTING
          remessa        = t_vepo-vbeln
          item_remessa   = itemremessa
        IMPORTING
          encomenda      = numencomenda
          tipo_encomenda = tipoencomenda
          pedido         = t_vepo-pedido
          datapedido     = datapedido
          item_encomenda = ie.

*      CALL FUNCTION 'Z_EXTRAI_NUM_ENC'
*          EXPORTING
*            REMESSA              = t_vepo-vbeln
*            ITEM_REMESSA         = itemremessa
*          IMPORTING
*            ENCOMENDA            = numencomenda
*            TIPO_ENCOMENDA       = tipoencomenda
*            PEDIDO               = t_vepo-pedido
*            DATAPEDIDO           = datapedido.
      APPEND t_vepo.
    ENDLOOP.
  ENDIF.
****  Fim das alterações do Paulo Sousa
**  Alteração 2012.03.08 - Paulo Sousa
** verificar se se trata de pedido Allotie. Se sim, colocar o texto 'ALLOTIE' no tipo de palete
  CALL FUNCTION 'Z_GET_IS_ALLOTIE'
    EXPORTING
      delivery      = t_vepo-vbeln
      delivery_item = itemremessa
    CHANGING
      allotie       = isallotie
      augru         = zaugru
      numenc        = numencomenda.
  allotie = ''.
  IF isallotie = 'X'.
    allotie = 'ALLOTIE'.
**   agrupar linhas para calcular totais
    LOOP AT t_vepo.
      MOVE-CORRESPONDING t_vepo TO tv.
      APPEND tv.
    ENDLOOP.
    REFRESH t_vepo.
    LOOP AT tv.
      sytabix = sy-tabix.
      READ TABLE t_vepo WITH KEY vbeln = tv-vbeln posnr = tv-posnr.
      IF sy-subrc = 0.
        t_vepo-vemng = t_vepo-vemng + tv-vemng.
        t_vepo-vemng_flo = t_vepo-vemng_flo + tv-vemng_flo.
        MODIFY t_vepo INDEX sy-tabix.
      ELSE.
        MOVE-CORRESPONDING tv TO t_vepo.
        APPEND t_vepo.
      ENDIF.
      sy-tabix = sytabix.
    ENDLOOP.
  ENDIF.


** Falta ir buscar a lingua do recebedor de mercadoria
  READ TABLE t_vepo INDEX 1.
  SELECT SINGLE * FROM zwm026 WHERE sscc = sscc.

  SELECT SINGLE *
      FROM vbpa
          WHERE vbeln = zwm026-remessa AND
                parvw = 'WE'.

  SELECT SINGLE spras INTO lang
      FROM kna1
          WHERE kunnr = vbpa-kunnr.

  IF lang IS INITIAL.
    lang = 'PT'.
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha1
        FROM makt
            WHERE matnr = t_vepo-matnr
            AND spras = lang.
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha1+40(4).
    MOVE t_vepo-vemeh TO linha1+44(3).
    MOVE t_vepo-charg TO linha1+47(10).
*    MOVE t_vepo-vfdat TO linha1+57(8).
    MOVE t_vepo-vbeln TO linha1+57(10).
    MOVE t_vepo-pedido TO linha1+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 2.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha2
        FROM makt
            WHERE matnr = t_vepo-matnr
            AND spras = lang.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha2+40(4).
    MOVE t_vepo-vemeh TO linha2+44(3).
    MOVE t_vepo-charg TO linha2+47(10).
*    MOVE t_vepo-vfdat TO linha2+57(8).
    MOVE t_vepo-vbeln TO linha2+57(10).
    MOVE t_vepo-pedido TO linha2+67(10).

  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 3.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha3
        FROM makt
            WHERE matnr = t_vepo-matnr
            AND spras = lang.
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha3+40(4).
    MOVE t_vepo-vemeh TO linha3+44(3).
    MOVE t_vepo-charg TO linha3+47(10).
*   MOVE t_vepo-vfdat TO linha3+57(8).
    MOVE t_vepo-vbeln TO linha3+57(10).
    MOVE t_vepo-pedido TO linha3+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 4.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha4
        FROM makt
            WHERE matnr = t_vepo-matnr
            AND spras = lang.
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha4+40(4).
    MOVE t_vepo-vemeh TO linha4+44(3).
    MOVE t_vepo-charg TO linha4+47(10).
*   MOVE t_vepo-vfdat TO linha4+57(8).
    MOVE t_vepo-vbeln TO linha4+57(10).
    MOVE t_vepo-pedido TO linha4+67(10).

  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 5.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha5
        FROM makt
            WHERE matnr = t_vepo-matnr
            AND spras = lang.
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha5+40(4).
    MOVE t_vepo-vemeh TO linha5+44(3).
    MOVE t_vepo-charg TO linha5+47(10).
*   MOVE t_vepo-vfdat TO linha5+57(8).
    MOVE t_vepo-vbeln TO linha5+57(10).
    MOVE t_vepo-pedido TO linha5+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 6.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha6
        FROM makt
            WHERE matnr = t_vepo-matnr
            AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha6+40(4).
    MOVE t_vepo-vemeh TO linha6+44(3).
    MOVE t_vepo-charg TO linha6+47(10).
*   MOVE t_vepo-vfdat TO linha6+57(8).
    MOVE t_vepo-vbeln TO linha6+57(10).
    MOVE t_vepo-pedido TO linha6+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 7.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha7
        FROM makt
            WHERE matnr = t_vepo-matnr
              AND spras = lang.
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha7+40(4).
    MOVE t_vepo-vemeh TO linha7+44(3).
    MOVE t_vepo-charg TO linha7+47(10).
*   MOVE t_vepo-vfdat TO linha7+57(8).
    MOVE t_vepo-vbeln TO linha7+57(10).
    MOVE t_vepo-pedido TO linha7+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 8.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha8
        FROM makt
            WHERE matnr = t_vepo-matnr
            AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha8+40(4).
    MOVE t_vepo-vemeh TO linha8+44(3).
    MOVE t_vepo-charg TO linha8+47(10).
*   MOVE t_vepo-vfdat TO linha8+57(8).
    MOVE t_vepo-vbeln TO linha8+57(10).
    MOVE t_vepo-pedido TO linha8+67(10).

  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 9.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha9
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha9+40(4).
    MOVE t_vepo-vemeh TO linha9+44(3).
    MOVE t_vepo-charg TO linha9+47(10).
*    MOVE t_vepo-vfdat TO linha9+57(8).
    MOVE t_vepo-vbeln TO linha9+57(10).
    MOVE t_vepo-pedido TO linha9+67(10).

  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 10.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha10
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha10+40(4).
    MOVE t_vepo-vemeh TO linha10+44(3).
    MOVE t_vepo-charg TO linha10+47(10).
*    MOVE t_vepo-vfdat TO linha10+57(8).
    MOVE t_vepo-vbeln TO linha10+57(10).
    MOVE t_vepo-pedido TO linha10+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 11.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha11
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha11+40(4).
    MOVE t_vepo-vemeh TO linha11+44(3).
    MOVE t_vepo-charg TO linha11+47(10).
*   MOVE t_vepo-vfdat TO linha11+57(8).
    MOVE t_vepo-vbeln TO linha11+57(10).
    MOVE t_vepo-pedido TO linha11+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 12.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha12
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha12+40(4).
    MOVE t_vepo-vemeh TO linha12+44(3).
    MOVE t_vepo-charg TO linha12+47(10).
*   MOVE t_vepo-vfdat TO linha12+57(8).
    MOVE t_vepo-vbeln TO linha12+57(10).
    MOVE t_vepo-pedido TO linha12+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 13.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha13
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha13+40(4).
    MOVE t_vepo-vemeh TO linha13+44(3).
    MOVE t_vepo-charg TO linha13+47(10).
*   MOVE t_vepo-vfdat TO linha13+57(8).
    MOVE t_vepo-vbeln TO linha13+57(10).
    MOVE t_vepo-pedido TO linha13+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 14.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha14
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha14+40(4).
    MOVE t_vepo-vemeh TO linha14+44(3).
    MOVE t_vepo-charg TO linha14+47(10).
*   MOVE t_vepo-vfdat TO linha14+57(8).
    MOVE t_vepo-vbeln TO linha14+57(10).
    MOVE t_vepo-pedido TO linha14+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 15.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha15
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha15+40(4).
    MOVE t_vepo-vemeh TO linha15+44(3).
    MOVE t_vepo-charg TO linha15+47(10).
*  MOVE t_vepo-vfdat TO linha15+57(8).
    MOVE t_vepo-vbeln TO linha15+57(10).
    MOVE t_vepo-pedido TO linha15+67(10).
  ENDIF.


  CLEAR qtd.
  READ TABLE t_vepo INDEX 16.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha16
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha16+40(4).
    MOVE t_vepo-vemeh TO linha16+44(3).
    MOVE t_vepo-charg TO linha16+47(10).
*   MOVE t_vepo-vfdat TO linha16+57(8).
    MOVE t_vepo-vbeln TO linha16+57(10).
    MOVE t_vepo-pedido TO linha16+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 17.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha17
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha17+40(4).
    MOVE t_vepo-vemeh TO linha17+44(3).
    MOVE t_vepo-charg TO linha17+47(10).
*   MOVE t_vepo-vfdat TO linha17+57(8).
    MOVE t_vepo-vbeln TO linha17+57(10).
    MOVE t_vepo-pedido TO linha17+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 18.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha18
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha18+40(4).
    MOVE t_vepo-vemeh TO linha18+44(3).
    MOVE t_vepo-charg TO linha18+47(10).
*   MOVE t_vepo-vfdat TO linha18+57(8).
    MOVE t_vepo-vbeln TO linha18+57(10).
    MOVE t_vepo-pedido TO linha18+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 19.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha19
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha19+40(4).
    MOVE t_vepo-vemeh TO linha19+44(3).
    MOVE t_vepo-charg TO linha19+47(10).
*   MOVE t_vepo-vfdat TO linha19+57(8).
    MOVE t_vepo-vbeln TO linha19+57(10).
    MOVE t_vepo-pedido TO linha19+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 20.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha20
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha20+40(4).
    MOVE t_vepo-vemeh TO linha20+44(3).
    MOVE t_vepo-charg TO linha20+47(10).
*   MOVE t_vepo-vfdat TO linha20+57(8).
    MOVE t_vepo-vbeln TO linha20+57(10).
    MOVE t_vepo-pedido TO linha20+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 21.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha21
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha21+40(4).
    MOVE t_vepo-vemeh TO linha21+44(3).
    MOVE t_vepo-charg TO linha21+47(10).
*   MOVE t_vepo-vfdat TO linha21+57(8).
    MOVE t_vepo-vbeln TO linha21+57(10).
    MOVE t_vepo-pedido TO linha21+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 22.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO linha22
        FROM makt
            WHERE matnr = t_vepo-matnr
                        AND spras = lang..
    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha22+40(4).
    MOVE t_vepo-vemeh TO linha22+44(3).
    MOVE t_vepo-charg TO linha22+47(10).
*   MOVE t_vepo-vfdat TO linha22+57(8).
    MOVE t_vepo-vbeln TO linha22+57(10).
    MOVE t_vepo-pedido TO linha22+67(10).
  ENDIF.

  DESCRIBE TABLE t_vepo LINES linhas.
  IF linhas > 20. "22.
    linha23 = '**  Consultar Lista de Conteudo do SSCC **'.
  ELSE.
    CLEAR linha23.
  ENDIF.

  READ TABLE out_par WITH KEY 'LINHA1'.
  CHECK sy-subrc = 0.
  MOVE linha1 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA2'.
  CHECK sy-subrc = 0.
  MOVE linha2 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA3'.
  CHECK sy-subrc = 0.
  MOVE linha3 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA4'.
  CHECK sy-subrc = 0.
  MOVE linha4 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA5'.
  CHECK sy-subrc = 0.
  MOVE linha5 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA6'.
  CHECK sy-subrc = 0.
  MOVE linha6 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA7'.
  CHECK sy-subrc = 0.
  MOVE linha7 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA8'.
  CHECK sy-subrc = 0.
  MOVE linha8 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA9'.
  CHECK sy-subrc = 0.
  MOVE linha9 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA10'.
  CHECK sy-subrc = 0.
  MOVE linha10 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA11'.
  CHECK sy-subrc = 0.
  MOVE linha11 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA12'.
  CHECK sy-subrc = 0.
  MOVE linha12 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA13'.
  CHECK sy-subrc = 0.
  MOVE linha13 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA14'.
  CHECK sy-subrc = 0.
  MOVE linha14 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA15'.
  CHECK sy-subrc = 0.
  MOVE linha15 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA16'.
  CHECK sy-subrc = 0.
  MOVE linha16 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA17'.
  CHECK sy-subrc = 0.
  MOVE linha17 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA18'.
  CHECK sy-subrc = 0.
  MOVE linha18 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA19'.
  CHECK sy-subrc = 0.
  MOVE linha19 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA20'.
  CHECK sy-subrc = 0.
  MOVE linha20 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.


  READ TABLE out_par WITH KEY 'LINHA21'.
  CHECK sy-subrc = 0.
  MOVE linha21 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA22'.
  CHECK sy-subrc = 0.
  MOVE linha22 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA23'.
  CHECK sy-subrc = 0.
  MOVE linha23 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'TIPOPALETE'.
  CHECK sy-subrc = 0.
  MOVE tipo_palete TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'TOTALBOXES'.
  CHECK sy-subrc = 0.
  WRITE totalboxes TO colis.
  CONDENSE colis.
  MOVE colis TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'ALLOTIE'.
  CHECK sy-subrc = 0.
  MOVE allotie TO out_par-value.
  MODIFY out_par INDEX sy-tabix.
ENDFORM.                    "get_descricoes

*---------------------------------------------------------------------*
*       FORM get_conteudo                                             *
*---------------------------------------------------------------------*
FORM get_resumo_allot TABLES in_par STRUCTURE itcsy
                         out_par STRUCTURE itcsy.

  DATA: qtd(20),
        colis(4).
  DATA: linhas        TYPE i,
        sytabix       LIKE sy-tabix,
        itemremessa   LIKE lips-posnr,
        numencomenda  LIKE vbak-vbeln,
        itemencomenda LIKE vbap-posnr,
        tipoencomenda TYPE c,
        datapedido    LIKE vbak-bstdk.

  DATA tipo_palete LIKE vekp-vhilm.
  DATA: isallotie TYPE c,
        zaugru    TYPE augru.

  READ TABLE in_par WITH KEY 'EAN128-EXIDV'.
  CHECK sy-subrc = 0.

  CLEAR: linha1,
         linha2,
         linha3,
         linha4,
         linha5,
         linha6,
         linha7,
         linha8,
         linha9,
         linha10,
         linha11,
         linha12,
         linha13,
         linha14,
         linha15,
         linha16,
         linha17,
         linha18,
         linha19,
         linha20,
         linha21,
         linha22,
         linha23,
         sscc,
         tipo_palete,
         t_vepo.


  totalboxes = 0.
  REFRESH t_vepo.
  REFRESH t_zwm026.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = in_par-value
    IMPORTING
      output = sscc.

  SELECT SINGLE * FROM vekp WHERE exidv = sscc.
  IF sy-subrc = 0.
    tipo_palete = vekp-vhilm.
  ENDIF.

  SELECT * INTO TABLE t_vepo FROM vepo WHERE venum = vekp-venum.

** As seguintes linhas foram alteradas por paulo sousa
** Descriminar por linha as quantidades do produto.
** implica fazer leitura a partir de t_vepo a partir de zwm026 e não vepo

  SELECT * INTO TABLE t_zwm026 FROM zwm026 WHERE sscc = sscc.
  IF t_zwm026[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_zwm026 FROM zwm026h WHERE sscc = sscc.
  ENDIF.
  DESCRIBE TABLE t_zwm026 LINES linhas.
  IF linhas = 0.
**    salvaguardar situação de picking inverso
**    actualizar num de remessa em t_vepo a partir de T_zwm026
    LOOP AT t_vepo.
      sytabix = sy-tabix.
      totalboxes = totalboxes + t_vepo-vemng.
      READ TABLE t_zwm026 INDEX sy-tabix.
      IF sy-subrc = 0.
        t_vepo-vbeln = t_zwm026-remessa.
      ENDIF.
*     MODIFY t_vepo INDEX sytabix.
**    paulo sousa 2011.11.18 - adicionar PO a cada linha
      CLEAR itemremessa.

      CALL FUNCTION 'Z_GET_ENCOMENDA_DA_REMESSA'
        EXPORTING
          remessa        = t_vepo-vbeln
          item_remessa   = itemremessa
        IMPORTING
          encomenda      = numencomenda
          tipo_encomenda = tipoencomenda
          pedido         = t_vepo-pedido
          datapedido     = datapedido
          item_encomenda = itemencomenda.


      MODIFY t_vepo INDEX sytabix.
*      ENDIF.
      sy-tabix = sytabix.
    ENDLOOP.
  ELSE.
    REFRESH: t_vepo, tv.
    LOOP AT t_zwm026.
      CLEAR t_vepo.
      totalboxes = totalboxes + t_zwm026-quantidade.
      t_vepo-matnr = t_zwm026-material.
      t_vepo-vemng = t_zwm026-quantidade.
      t_vepo-vemeh = t_zwm026-unidade.
      t_vepo-charg = t_zwm026-lote.
      t_vepo-vbeln = t_zwm026-remessa.
      t_vepo-posnr = t_zwm026-posnr.
      CLEAR itemremessa.

      DATA: ie LIKE vbap-posnr.
      CALL FUNCTION 'Z_GET_ENCOMENDA_DA_REMESSA'
        EXPORTING
          remessa        = t_vepo-vbeln
          item_remessa   = itemremessa
        IMPORTING
          encomenda      = numencomenda
          tipo_encomenda = tipoencomenda
          pedido         = t_vepo-pedido
          datapedido     = datapedido
          item_encomenda = ie.

*      CALL FUNCTION 'Z_EXTRAI_NUM_ENC'
*          EXPORTING
*            REMESSA              = t_vepo-vbeln
*            ITEM_REMESSA         = itemremessa
*          IMPORTING
*            ENCOMENDA            = numencomenda
*            TIPO_ENCOMENDA       = tipoencomenda
*            PEDIDO               = t_vepo-pedido
*            DATAPEDIDO           = datapedido.
      APPEND t_vepo.
    ENDLOOP.
  ENDIF.
****  Fim das alterações do Paulo Sousa
**  Alteração 2012.03.08 - Paulo Sousa
** verificar se se trata de pedido Allotie. Se sim, colocar o texto 'ALLOTIE' no tipo de palete
  CALL FUNCTION 'Z_GET_IS_ALLOTIE'
    EXPORTING
      delivery      = t_vepo-vbeln
      delivery_item = itemremessa
    CHANGING
      allotie       = isallotie
      augru         = zaugru
      numenc        = numencomenda.

  IF isallotie = 'X'.
    tipo_palete = 'ALLOTIE'.
    REFRESH tv.
**   agrupar linhas por item/parceiro para calcular totais
    LOOP AT t_vepo.
      MOVE-CORRESPONDING t_vepo TO tv.
      APPEND tv.
    ENDLOOP.
    REFRESH t_vepo.
    LOOP AT tv.
      sytabix = sy-tabix.
      READ TABLE t_vepo WITH KEY vbeln = tv-vbeln posnr = tv-posnr spe_idplate = tv-spe_idplate.
      IF sy-subrc = 0.
        t_vepo-vemng = t_vepo-vemng + tv-vemng.
*           t_vepo-vemng_flo = t_vepo-vemng_flo + tv-vemng_flo.
        MODIFY t_vepo INDEX sy-tabix.
      ELSE.
        MOVE-CORRESPONDING tv TO t_vepo.
        APPEND t_vepo.
      ENDIF.
      sy-tabix = sytabix.
    ENDLOOP.
  ENDIF.

** Falta ir buscar a lingua do recebedor de mercadoria
  READ TABLE t_vepo INDEX 1.
  SELECT SINGLE * FROM zwm026 WHERE sscc = sscc.

  SELECT SINGLE *
      FROM vbpa
          WHERE vbeln = zwm026-remessa AND
                parvw = 'WE'.

  SELECT SINGLE spras INTO lang
      FROM kna1
          WHERE kunnr = vbpa-kunnr.

  IF lang IS INITIAL.
    lang = 'PT'.
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha1, linha1+47, linha1+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha1+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 2.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha2, linha2+47, linha2+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha2+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 3.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha3, linha3+47, linha3+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha3+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 4.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha4, linha4+47, linha4+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha4+40(4).
  ENDIF.
  CLEAR qtd.
  READ TABLE t_vepo INDEX 5.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha5, linha5+47, linha5+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha5+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 6.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha6, linha6+47, linha6+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha6+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 7.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha7, linha7+47, linha7+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha7+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 8.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha8, linha8+47, linha8+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha8+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 9.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha9, linha9+47, linha9+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha9+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 10.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha10, linha10+47, linha10+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha10+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 11.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha11, linha11+47, linha11+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha11+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 12.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha11, linha11+47, linha11+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha11+40(4).
    MOVE t_vepo-pedido TO linha11+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 13.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha12, linha12+47, linha12+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha12+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 14.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha13, linha13+47, linha13+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha13+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 15.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha14, linha14+47, linha14+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha14+40(4).
  ENDIF.


  CLEAR qtd.
  READ TABLE t_vepo INDEX 16.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha15, linha15+47, linha15+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha15+40(4).
    MOVE t_vepo-pedido TO linha15+67(10).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 17.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha16, linha16+47, linha16+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha16+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 18.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha17, linha17+47, linha17+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha17+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 19.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha18, linha18+47, linha18+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha18+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 20.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha19, linha19+47, linha19+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha19+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 21.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha20, linha20+47, linha20+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha20+40(4).
  ENDIF.

  CLEAR qtd.
  READ TABLE t_vepo INDEX 22.
  IF sy-subrc = 0.
    SELECT SINGLE name1 pstlz ort01 FROM kna1 INTO (linha21, linha21+47, linha21+57)
      WHERE kunnr = t_vepo-spe_idplate.

    MOVE t_vepo-vemng TO qtd.
    MOVE qtd+11(4) TO linha21+40(4).
  ENDIF.

  DESCRIBE TABLE t_vepo LINES linhas.
  IF linhas > 22.
    linha23 = '**  Consultar Lista de Conteudo do SSCC **'.
  ELSE.
    CLEAR linha23.
  ENDIF.

  READ TABLE out_par WITH KEY 'LINHA1'.
  CHECK sy-subrc = 0.
  MOVE linha1 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA2'.
  CHECK sy-subrc = 0.
  MOVE linha2 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA3'.
  CHECK sy-subrc = 0.
  MOVE linha3 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA4'.
  CHECK sy-subrc = 0.
  MOVE linha4 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA5'.
  CHECK sy-subrc = 0.
  MOVE linha5 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA6'.
  CHECK sy-subrc = 0.
  MOVE linha6 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA7'.
  CHECK sy-subrc = 0.
  MOVE linha7 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA8'.
  CHECK sy-subrc = 0.
  MOVE linha8 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA9'.
  CHECK sy-subrc = 0.
  MOVE linha9 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA10'.
  CHECK sy-subrc = 0.
  MOVE linha10 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA11'.
  CHECK sy-subrc = 0.
  MOVE linha11 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA12'.
  CHECK sy-subrc = 0.
  MOVE linha12 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA13'.
  CHECK sy-subrc = 0.
  MOVE linha13 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA14'.
  CHECK sy-subrc = 0.
  MOVE linha14 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA15'.
  CHECK sy-subrc = 0.
  MOVE linha15 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA16'.
  CHECK sy-subrc = 0.
  MOVE linha16 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA17'.
  CHECK sy-subrc = 0.
  MOVE linha17 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA18'.
  CHECK sy-subrc = 0.
  MOVE linha18 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA19'.
  CHECK sy-subrc = 0.
  MOVE linha19 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA20'.
  CHECK sy-subrc = 0.
  MOVE linha20 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.


  READ TABLE out_par WITH KEY 'LINHA21'.
  CHECK sy-subrc = 0.
  MOVE linha21 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA22'.
  CHECK sy-subrc = 0.
  MOVE linha22 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'LINHA23'.
  CHECK sy-subrc = 0.
  MOVE linha23 TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'TIPOPALETE'.
  CHECK sy-subrc = 0.
  MOVE tipo_palete TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'TOTALBOXES'.
  CHECK sy-subrc = 0.
  WRITE totalboxes TO colis.
  CONDENSE colis.
  MOVE colis TO out_par-value.
  MODIFY out_par INDEX sy-tabix.

ENDFORM.                    "get_

*&---------------------------------------------------------------------*
*&      Form  GET_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_LGNUM  text
*----------------------------------------------------------------------*
FORM get_whs CHANGING cv_lgnum TYPE lgnum.

  DATA: lt_user TYPE TABLE OF lrf_wkqu.

  DATA: ls_user TYPE lrf_wkqu.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = lt_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  CHECK sy-subrc EQ 0.

  READ TABLE lt_user
        INTO ls_user
        WITH KEY statu = 'X'. "Util. Atrib. Arm.
  CHECK sy-subrc EQ 0.

  cv_lgnum = ls_user-lgnum.

ENDFORM.                    " GET_WHS

FORM clear.

  CLEAR: mara,
          makt,
          vekp,
          vepo,
          kna1,
          vbpa,
          adrc,
          likp,
          lips,
          vbap,
          vbak,
          eket,
          vbep,
          ltap,
          zwm026,
          zwm044,
          zwm069.

ENDFORM.
