FUNCTION zwm_get_ean128_label_content.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(SSCC) TYPE  EXIDV
*"     VALUE(LBLTYPE) TYPE  EAN128-LBLTYPE DEFAULT 'L0'
*"     REFERENCE(PRINTER) TYPE  RSPOPNAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(EAN128OUT) TYPE  EAN128
*"     REFERENCE(L_PROFILE) TYPE  EAN128_PROFILE
*"     REFERENCE(FORM_NAME) LIKE  TNAPR-FONAM
*"     REFERENCE(NOHEADERLOGO) TYPE  FLAG
*"     REFERENCE(CS_EAN128_DATA_EANBC) TYPE  BARCODE_PLUS_ERRORS_T
*"     REFERENCE(CS_EAN128_DATA_EANRD) TYPE  BARCODE_TXT_T
*"     REFERENCE(WE_NUMGUIA) TYPE  VBELN
*"  EXCEPTIONS
*"      IMPRESSORA_NAO_EXISTE
*"      SSCC_NAO_EXISTE
*"      SSCC_COM_IMPRESSAO_GRUPO
*"----------------------------------------------------------------------
** Paulo Sousa 2011.12.16
**
** funcao que recebe SSCC e devolve nome de formulario, codigo(s) EAN 128 e estrutura
** EAN128OUT (apenas para paletes homogeneas) com dados para impressao de etiquetas.
** O codigo abaixo foi removido da FM: ZWM_IMPRIME_EAN128 por forma a fornecer detalhes de etiquetas
** para outros fins, como impressão de PDFs

  DATA: l_venum      LIKE vekp-venum,
        lv_werks     TYPE werks_d,
        tnapr        LIKE tnapr,
        l_devicetype TYPE t313k-devicetype,
        lf_type      TYPE c,
        qtd_pal      TYPE vemng.

  DATA: wa_values TYPE ean128,
        itab_vepo LIKE vepo OCCURS 0 WITH HEADER LINE.

  DATA: we_name2 LIKE adrc-name2,
        we_street LIKE adrc-street,
        we_post_code1 LIKE adrc-post_code1,
        we_city1 LIKE adrc-city1,
        we_numorder LIKE vbfa-vbeln,
        delivery_date LIKE likp-lfdat.

  DATA: ean_pal LIKE ean128-matean,
        ean_box LIKE ean128-matean,
        data_fabrico LIKE ean128-erdat,
        zprofile TYPE ean128_profile.

  DATA: lv_sscc_master TYPE lenum,
        lv_lgnum       TYPE lgnum.

  DATA: lt_ltap_sscc TYPE TABLE OF ltap,
        lt_ltak_sscc TYPE TABLE OF ltak,
        ls_ltak_sscc TYPE ltak.

  DATA: BEGIN OF i_zwm026 OCCURS 0.
          INCLUDE STRUCTURE zwm026.
  DATA: END OF i_zwm026.

  DATA zzwm044 TYPE zwm044.
  DATA zzwm069 TYPE zwm069.
  DATA: ls_marm TYPE marm,
        lv_value TYPE menge_d.

  FIELD-SYMBOLS:
        <ls_bc>            TYPE barcode_plus_txt,
        <ls_bc_plus_error> TYPE barcode_plus_errors,
        <ls_bc_txt>        TYPE barcode_txt.

  CHECK NOT sscc IS INITIAL.
  REFRESH i_zwm026.

** Armazem
***********************************************************************
  IF lgnum IS INITIAL.
    PERFORM get_whs CHANGING lv_lgnum.
  ELSE.
    lv_lgnum = lgnum.
  ENDIF.

  CLEAR: itab_vepo, wa_values, ean128, lv_werks, vekp, we_numguia.
  FREE: itab_vepo.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = sscc
    IMPORTING
      output = wa_values-exidv.

** Obter Unidade comercial
  SELECT SINGLE venum vhilm vbeln_gen werks FROM vekp
  INTO (l_venum, wa_values-vhilm, we_numguia, lv_werks)
  WHERE exidv EQ wa_values-exidv AND
        status <> '0060'.

**  IF we_numguia IS INITIAL.
**    SELECT SINGLE vpobjkey FROM vekp INTO we_numguia
**      WHERE exidv EQ wa_values-exidv
**        AND vpobj = '01'.
**  ENDIF.

** Obter Lote
  SELECT * FROM vepo   INTO CORRESPONDING FIELDS OF TABLE itab_vepo
    WHERE venum EQ l_venum.

  IF sy-subrc NE 0.
    RAISE sscc_nao_existe.
    EXIT.
  ENDIF.

  DESCRIBE TABLE itab_vepo LINES l_linhas.

** Etiqueta Simples
  READ TABLE itab_vepo INDEX 1.
  IF lv_werks IS INITIAL.
    lv_werks = itab_vepo-werks.
  ENDIF.
** Obter dados do Lote
  SELECT SINGLE vfdat FROM mcha INTO wa_values-vfdat
    WHERE matnr EQ itab_vepo-matnr
      AND werks EQ itab_vepo-werks
      AND charg EQ itab_vepo-charg.

** Obter a Descrição do material
  SELECT SINGLE maktx FROM makt   INTO wa_values-maktx
    WHERE matnr EQ itab_vepo-matnr
      AND spras EQ sy-langu.

** Obter EANs
  CLEAR: ean_pal, ean_box.
** EAN palete
  SELECT SINGLE * FROM marm WHERE matnr = itab_vepo-matnr
                              AND ( ( meinh = 'PAL' ) OR ( meinh = 'pal' ) ).
  IF NOT marm-ean11 IS INITIAL.
    qtd_pal = marm-umrez / marm-umren.
    IF itab_vepo-vemng = qtd_pal.
      ean_pal = marm-ean11.
    ENDIF.
  ENDIF.
** EAN Unidade stock
  SELECT SINGLE ean11 INTO ean_box FROM mara WHERE matnr EQ itab_vepo-matnr.

  wa_values-matnr = itab_vepo-matnr.
  wa_values-charg = itab_vepo-charg.
  CLEAR: wa_values-lfimg, wa_values-vemng.

  LOOP AT itab_vepo.
    IF itab_vepo-vemeh <> itab_vepo-altme.
      SELECT SINGLE * FROM marm INTO ls_marm
        WHERE matnr = itab_vepo-matnr AND
              meinh = itab_vepo-altme.
      IF sy-subrc = 0.
        lv_value = itab_vepo-vemng * ls_marm-umren.
        CALL FUNCTION 'ROUND'
          EXPORTING
            decimals      = 0
            input         = lv_value
            sign          = 'X'
          IMPORTING
            output        = itab_vepo-vemng
          EXCEPTIONS
            input_invalid = 1
            overflow      = 2
            type_invalid  = 3
            OTHERS        = 4.
        itab_vepo-vemeh = itab_vepo-altme.
      ENDIF.
    ENDIF.
    wa_values-lfimg = wa_values-lfimg + itab_vepo-vemng.
    wa_values-vemng = wa_values-vemng + itab_vepo-vemng.
  ENDLOOP.

  wa_values-vrkme = itab_vepo-vemeh.
  wa_values-vemeh = itab_vepo-vemeh.

** INI- Paletização Especial
  SELECT SINGLE * INTO zzwm044 FROM zwm044 WHERE exidv = sscc AND status = 'F'.
  IF sy-subrc = 0.
    IF l_linhas > 1.
      l_profile = '001'.
      wa_values-lbltype = 'G'.
      tnapr-fonam = 'ZWM_EAN128_SSCC2'.
    ELSE.
      l_profile = '000'.
      wa_values-lbltype = 'S'.
      tnapr-fonam = 'ZWM_EAN128_SSCC3'.
    ENDIF.
** FIM- Paletização Especial
  ELSE.
    SELECT SINGLE * INTO zzwm069 FROM zwm069 WHERE sscc = sscc..
    IF sy-subrc = 0.
      IF l_linhas > 1.
        l_profile = '001'.
        wa_values-lbltype = 'G'.
        tnapr-fonam = 'ZWM_EAN128_SSCC2'.
      ELSE.
        l_profile = '000'.
        wa_values-lbltype = 'S'.
        tnapr-fonam = 'ZWM_EAN128_SSCC3'.
      ENDIF.
    ELSE.

**  identificar formulario
**  verificar se palete picking
      SELECT * INTO TABLE i_zwm026 FROM zwm026
              WHERE armazem = lv_lgnum
                AND sscc = sscc.
      IF i_zwm026[] IS INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE i_zwm026 FROM zwm026h
                WHERE armazem = lv_lgnum
                  AND sscc = sscc.

        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE sscc_master FROM zwm061 INTO lv_sscc_master
            WHERE lgnum = lv_lgnum
              AND sscc = sscc.

          IF sy-subrc IS INITIAL.
            SELECT SINGLE * INTO CORRESPONDING FIELDS OF i_zwm026 FROM zwm026
                WHERE armazem = lv_lgnum
                  AND sscc = lv_sscc_master.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE * INTO CORRESPONDING FIELDS OF i_zwm026 FROM zwm026h
                      WHERE armazem = lv_lgnum
                        AND sscc = lv_sscc_master.

              IF sy-subrc IS NOT INITIAL.
                APPEND i_zwm026.
              ENDIF.
            ELSE.
              APPEND i_zwm026.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      DESCRIBE TABLE i_zwm026 LINES l_linhas.
      REFRESH i_zwm026.
      IF l_linhas = 0.
**     não encontrada no historico de picking
**     procurar em LTAP
        DATA lt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.
        DATA lt_ltap2 LIKE ltap OCCURS 0 WITH HEADER LINE.
        CLEAR:   lt_ltap, lt_ltap2.
        REFRESH: lt_ltap, lt_ltap2.

        SELECT * INTO TABLE lt_ltap FROM ltap
                WHERE lgnum = lv_lgnum
                  AND vlenr = sscc.

        SELECT * INTO TABLE lt_ltap2 FROM ltap
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
        IF sy-subrc = 0.
          l_profile = '000'.
          wa_values-lbltype = 'S'.
*        palete completa; ver pelo tipo de movimento se entrada ou saida
          SELECT SINGLE * FROM ltak WHERE lgnum = ltap-lgnum
                                      AND tanum = ltap-tanum.
          IF ltak-bwlvs = 980 OR
             ltak-bwlvs = 981 OR
             ltak-bwlvs = 101 OR
             ltak-bwlvs = 105 OR
             ltak-bwlvs = 103.
**            movimentos de entrada
            tnapr-fonam = 'ZWM_EAN128_SSCC'.
          ELSE.
**            saidas
            tnapr-fonam = 'ZWM_EAN128_SSCC3'.
          ENDIF.
        ELSE.
          SELECT SINGLE * FROM ltak WHERE  lgnum = lv_lgnum AND lznum = sscc.
          IF sy-subrc = 0.
**          palete picking
            l_profile = '001'.
            wa_values-lbltype = 'G'.
            tnapr-fonam = 'ZWM_EAN128_SSCC2'.
          ELSE.
**          nao encontra em zwm026 nem em ltap ->paletes em transito
            DESCRIBE TABLE itab_vepo LINES l_linhas.
            IF l_linhas = 1.
              l_profile = '000'.
              wa_values-lbltype = 'S'.
              tnapr-fonam = 'ZWM_EAN128_SSCC'.
            ELSE.
              l_profile = '001'.
              wa_values-lbltype = 'G'.
              tnapr-fonam = 'ZWM_EAN128_SSCC2'.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
**     palete picking---------------------------------------------
        IF l_linhas = 1.
***       Palete standard nao homogenea (picking monoreferencia)
          l_profile = '000'.
          wa_values-lbltype = 'S'.
          tnapr-fonam = 'ZWM_EAN128_SSCC3'.
        ELSE.
**        palete nao standard nao homogenea (picking multireferencia)
          l_profile = '001'.
          wa_values-lbltype = 'G'.
          tnapr-fonam = 'ZWM_EAN128_SSCC2'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


**  Alterado Paulo Sousa em 2010.04.09
**  determinar data da guia
  IF we_numguia IS INITIAL.
    SELECT SINGLE * FROM vekp WHERE exidv = wa_values-exidv AND lgnum = lv_lgnum.
    IF NOT vekp-vbeln_gen IS INITIAL.
      we_numguia = vekp-vbeln_gen.
    ELSE.
      " palete ainda não embalada
      " verificar se é picking monoreferencia => procurar em zwm026
      SELECT SINGLE * FROM zwm026 WHERE armazem = lv_lgnum AND sscc = wa_values-exidv.
      IF sy-subrc = 0.
        we_numguia = zwm026-remessa.
      ELSEIF lv_sscc_master IS NOT INITIAL.
        SELECT SINGLE * FROM zwm026 WHERE armazem = lv_lgnum AND sscc = lv_sscc_master.
        we_numguia = zwm026-remessa.
      ELSE.
        we_numguia = ltap-vbeln.
        DO 1 TIMES.
          CLEAR zwm044.
          SELECT SINGLE * FROM zwm044 WHERE lgnum = lv_lgnum AND exidv = sscc.
          IF sy-subrc = 0.
            we_numguia = zwm044-vbeln.
          ENDIF.
          CHECK we_numguia IS INITIAL.
          CLEAR:   lt_ltak_sscc, lt_ltap_sscc.
          REFRESH: lt_ltak_sscc, lt_ltap_sscc.
          SELECT * FROM ltap
                   INTO TABLE lt_ltap_sscc
                   WHERE lgnum = lv_lgnum
                   AND vlenr = sscc.
          DELETE lt_ltap_sscc WHERE vorga = 'ST'.
          CHECK lt_ltap_sscc[] IS NOT INITIAL.
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
          READ TABLE lt_ltak_sscc INTO ls_ltak_sscc INDEX 1.
          we_numguia = ls_ltak_sscc-benum.
        ENDDO.
      ENDIF.
    ENDIF.
    " INI- Paletização Especial
    SELECT SINGLE * INTO zzwm044 FROM zwm044 WHERE exidv = sscc AND status = 'F'.
    IF sy-subrc = 0.
      we_numguia = zzwm044-vbeln.
    ENDIF.
    " FIM- Paletização Especial

    SELECT SINGLE * INTO zzwm069 FROM zwm069 WHERE sscc = sscc.
    IF sy-subrc = 0.
      we_numguia = zzwm069-vbeln.
    ENDIF.
  ENDIF.

  IF we_numguia IS INITIAL.
    SELECT SINGLE vpobjkey FROM vekp INTO we_numguia
      WHERE exidv EQ wa_values-exidv
        AND vpobj = '01'.
  ENDIF.

  "Data de fabrico
  SELECT SINGLE data INTO data_fabrico FROM zwm_log_efacec WHERE sscc = sscc.
  IF NOT we_numguia IS INITIAL.
    SELECT SINGLE lfdat INTO data_fabrico FROM likp WHERE vbeln = we_numguia.
  ENDIF.
  " fixa data de fabrico a sy-datum (AI11)
  IF data_fabrico IS INITIAL.
    data_fabrico = sy-datum.
  ENDIF.

** selecionar tipo de EAN128 por cliente
  IF NOT we_numguia = 0.
    "procurar recebedor de mercadoria
    DATA: numenc LIKE vbak-vbeln, nump LIKE vbak-bstnk, st TYPE c, dats LIKE vbak-bstdk.

    CALL FUNCTION 'Z_EXTRAI_NUM_ENC'
      EXPORTING
        remessa        = we_numguia
        item_remessa   = '000000'
      IMPORTING
        encomenda      = numenc
        tipo_encomenda = st
        pedido         = nump
        datapedido     = dats.

    IF NOT numenc IS INITIAL.
      DATA: zkunwe LIKE vbpa-kunnr.
      DATA: zeantype TYPE zean128_type.
      SELECT SINGLE kunnr INTO zkunwe FROM vbpa WHERE vbeln = numenc
                                  AND posnr = '000000'
                                  AND parvw = 'WE'.
      IF sy-subrc = 0.
        SELECT SINGLE eantype INTO zeantype FROM zwm048 WHERE kunwe = zkunwe.
        IF sy-subrc = 0.
          IF wa_values-lbltype = 'S'.
            l_profile = zeantype.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF zkunwe EQ '0000328705'.
    l_profile = '002'.

    SELECT SINGLE ntgew gewei_max FROM vekp
                                  INTO (wa_values-brgew, wa_values-gewei)
                                  WHERE exidv = sscc.
  ENDIF.

** 2015.03.04 - paulo sousa
** alterações para forçar a impressão de AI(02)+AI(37) em vez de AI(10).
  wa_values-matean = ean_box.
  lbltype = 'L1'.
***  DUN14 (PAL/BOX)
*  if not ean_pal is initial.
*    wa_values-matean = ean_pal.
*  else.
*    wa_values-matean = ean_box.
*  endif.
*
*  if l_profile = 'Z00'.
***    DUN un stock
*    wa_values-matean = ean_box.
*    lbltype = 'L1'.
***    lbltype = 1 - É ignorado o DUN14 de palete no user-exit de formatação do EAN128.
*  endif.
**** 2015.03.04 - paulo sousa *** FIM

  wa_values-erdat = data_fabrico.
  wa_values-hsdat = data_fabrico.
  MOVE wa_values TO ean128.
  ean128-lbltype = lbltype.

  CALL FUNCTION 'LE_EAN128_ENCODE'
    EXPORTING
      if_encode_profile         = l_profile
      if_skip_empty_fields      = 'X'
      is_ean128_data            = ean128
      if_devicetype             = l_devicetype
    IMPORTING
      et_barcode                = cs_ean128_data_eanbc
      et_barcode_txt            = cs_ean128_data_eanrd
    EXCEPTIONS
      no_barcode_definition     = 1
      error_in_subfunctions     = 2
      profile_not_for_ean128    = 3
      profile_unknown           = 4
      data_error                = 5
      no_ai_relation            = 6
      no_ai_information         = 7
      profile_error             = 8
      general_customizing_error = 9
      OTHERS                    = 10.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*** Actualização dos BC
** transform the barcode table to the flat structure
  DESCRIBE FIELD barcode TYPE lf_type COMPONENTS l_linhas.

  LOOP AT cs_ean128_data_eanbc ASSIGNING <ls_bc_plus_error>.
    IF sy-tabix > l_linhas.
      EXIT.
    ENDIF.
    ASSIGN COMPONENT sy-tabix OF STRUCTURE barcode TO <ls_bc>.
    <ls_bc>-bc = <ls_bc_plus_error>-barcode.
  ENDLOOP.

  LOOP AT cs_ean128_data_eanrd ASSIGNING <ls_bc_txt>.
    IF sy-tabix > l_linhas.
      EXIT.
    ENDIF.
    ASSIGN COMPONENT sy-tabix OF STRUCTURE barcode TO <ls_bc>.
    <ls_bc>-txt = <ls_bc_txt>.
  ENDLOOP.


  CLEAR noheaderlogo.
** verificar se for pal completa marca branca, retira logo
  DATA: element(20) VALUE 'EAN128', zsozinho(64) TYPE c, grp1 LIKE mvke-mvgr1, grp2 LIKE mvke-mvgr2.
  CLEAR: grp1, grp2, zsozinho.
  IF tnapr-fonam = 'ZWM_EAN128_SSCC' OR tnapr-fonam = 'ZWM_EAN128_SSCC3'.
    SELECT SINGLE mvgr1 mvgr2 INTO ( grp1, grp2 ) FROM mvke WHERE matnr = wa_values-matnr.
    IF grp1 = 'MDD' and grp2 <> 'MCD'.  " marcas da distribuicao não McDonalds - Alterado em 2024.06.11 paulo sousa
      zsozinho = 'SEMLOGO'.
    ELSE.
      SELECT SINGLE lay_etiqueta INTO zsozinho FROM zwm043 WHERE matnr = wa_values-matnr.
    ENDIF.
    IF zsozinho = 'SEMLOGO'.
      element = 'EAN128_SEM_LOGO'.
      noheaderlogo = 'X'.
    ENDIF.
  ENDIF.
*

  " 2019.09.30 - Paulo Sousa; Adicionado suporte para Promineral
  IF lv_werks = 'PROM'.
    CASE tnapr-fonam.
      WHEN 'ZWM_EAN128_SSCC'.
        tnapr-fonam = 'ZWM_SSCC_PROM'.
      WHEN 'ZWM_EAN128_SSCC2'.
        tnapr-fonam = 'ZWM_SSCC_2_PROM'.
      WHEN 'ZWM_EAN128_SSCC3'.
        tnapr-fonam = 'ZWM_SSCC_3_PROM'.
    ENDCASE.
  ENDIF.

** 2014.01.27 Paulo Sousa
** Adicionado suporte para formularios em 200 ou 300 dpi
  IF NOT printer IS INITIAL.
    DATA: dpis LIKE z02rpimp_etiquet-dpi.
    CLEAR dpis.
    SELECT SINGLE dpi INTO dpis FROM z02rpimp_etiquet WHERE pdest = printer.
    IF sy-subrc = 0.
      IF dpis = '300'.
        CASE tnapr-fonam.
          WHEN 'ZWM_EAN128_SSCC'.
            tnapr-fonam = 'ZWM_EN_SSCC_300'.
          WHEN 'ZWM_EAN128_SSCC2'.
            tnapr-fonam = 'ZWM_EN_SSCC2_300'.
          WHEN 'ZWM_EAN128_SSCC3'.
            tnapr-fonam = 'ZWM_EN_SSCC3_300'.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDIF.

  form_name = tnapr-fonam.
  MOVE ean128 TO ean128out.
ENDFUNCTION.
