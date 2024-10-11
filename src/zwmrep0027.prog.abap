************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0027                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Impressão da Folha de Carga (Sapscript)                  *
* Criado por: Luís Rocha                                               *
* Criado em.: 19/11/2004                                               *
* Tipo PRG..: Report                                                   *
************************************************************************

REPORT zwmrep0027 LINE-COUNT 100.

TABLES: likp,
        kna1,
        lfa1,
        adrc,
        mara,
        makt,
        marm, *marm,
        ltap,
        ltak,
        vbsk,
        vttk,
        vttp,
        vekp,
        vepo,
        vbfa,
        vbpa,
        t300,
        itcpo.

TABLES: vbco3,                         "Communicationarea for view
        vbdkl,                         "Headerview
        vbdpl.                         "Itemview

TABLES: zwm002,
        zwm013,
        zwm026,
        zwm040,
        zwm028, *zwm028,
        zwm_folha_cargas,
        zwm_sscc_aux, *zwm_sscc_aux.

DATA: BEGIN OF tvbdpl OCCURS 0.        "Internal table for items
        INCLUDE STRUCTURE vbdpl.
DATA: END OF tvbdpl.

***** variáveis e tabelas internas

DATA: gt_ltak LIKE ltak OCCURS 0 WITH HEADER LINE,
      gt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE,
      gt_adrc LIKE adrc OCCURS 0 WITH HEADER LINE,
      gt_vekp LIKE vekp OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_marm OCCURS 0,
        matnr LIKE marm-matnr,
        meinh LIKE marm-meinh,
        umrez LIKE marm-umrez,
        umren LIKE marm-umren,
      END OF gt_marm.

** RL -> INS 26.04.2005
DATA: BEGIN OF itab_rem OCCURS 0,
        refnr LIKE zwm028-refnr,
        vbeln LIKE zwm028-remessa,
        index LIKE sy-tabix,
      END OF itab_rem.
** RL <- INS 26.04.2005

*data: begin of gt_vekp occurs 0,
*        exidv like vekp-exidv,
*        venum like vekp-venum,
*        vhilm like vekp-vhilm,
*      end of gt_vekp.

DATA: BEGIN OF gt_vepo OCCURS 0,
        venum LIKE vepo-venum,
        vepos LIKE vepo-vepos,
        vbeln LIKE vepo-vbeln,
        posnr LIKE vepo-posnr,
        vemng LIKE vepo-vemng,
        vemeh LIKE vepo-vemeh,
        altme LIKE vepo-altme,
        matnr LIKE vepo-matnr,
        charg LIKE vepo-charg,
      END OF gt_vepo.

DATA: BEGIN OF tab_ger OCCURS 0,
         refnr          LIKE zwm028-refnr,
         ordem          LIKE zwm028-ordem,
         remessa        LIKE zwm028-remessa,
      END OF tab_ger,
      reg_ger LIKE tab_ger,
      key_ger LIKE tab_ger.

DATA: BEGIN OF tab_pal OCCURS 0,
        remessa        LIKE ltap-vbeln,
        sscc           LIKE zwm013-sscc,
        destino        LIKE zwm013-destino,
        posicao_pulmao LIKE zwm013-posicao_pulmao,
      END OF tab_pal.

DATA: BEGIN OF tab_out OCCURS 0,
         refnr          LIKE zwm028-refnr,
         ordem          LIKE zwm028-ordem,
         destino        LIKE zwm013-destino,
         posicao_pulmao LIKE zwm013-posicao_pulmao,
         sscc           LIKE zwm013-sscc,
         remessa        LIKE zwm028-remessa,
         kunnr          LIKE kna1-kunnr,
      END OF tab_out,
      reg_out LIKE tab_out.

DATA: BEGIN OF tab_total OCCURS 0,
        remessa LIKE zwm028-remessa,
        brgew   LIKE zwm_sscc_aux-brgew,
        btvol   LIKE zwm_sscc_aux-btvol,
      END OF tab_total.

DATA: BEGIN OF tab_subtotal OCCURS 0,
        remessa LIKE zwm028-remessa,
        vhilm   LIKE vekp-vhilm,
        paletes LIKE zwm_sscc_aux-paletes,
        brgew   LIKE zwm_sscc_aux-brgew,
        btvol   LIKE zwm_sscc_aux-btvol,
      END OF tab_subtotal.

DATA: BEGIN OF tab_totpal OCCURS 0,
        vhilm   LIKE vekp-vhilm,
        paletes LIKE zwm_sscc_aux-paletes,
      END OF tab_totpal.

DATA: nome_we   LIKE adrc-name1,
      morada_we LIKE adrc-street,
      cod_we    LIKE adrc-post_code1,
      cidade_we LIKE adrc-city1.

*data: g_pchep   like zwm_sscc_aux-pchep,
*      g_prouge  like zwm_sscc_aux-prouge.

DATA: w_subrc LIKE sy-subrc.

DATA: g_index LIKE sy-tabix,
      l_qtd   LIKE marm-meinh.

*constants: c_pchep  like vekp-vhilm value 'PCHEP',
*           c_prouge like vekp-vhilm value 'PROUGE'.

CONSTANTS: c_prefixo(12) VALUE 'DCK 000-000-'.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum LIKE mlgn-lgnum OBLIGATORY MEMORY ID lgn,
            p_refnr LIKE zwm028-refnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

* Initialization
INITIALIZATION.
  DATA: xuser LIKE lrf_wkqu OCCURS 0 WITH HEADER LINE.
* Read the user data from table lrf_wkqu ( for all the warehouses)
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = xuser
    EXCEPTIONS
      no_entry_found = 01.
  IF sy-subrc = 0.
    READ TABLE xuser INDEX 1.
    MOVE xuser-lgnum TO p_lgnum.
  ENDIF.

* Start-of-Selection
START-OF-SELECTION.

  PERFORM valida_parametros_entrada.
  IF sy-subrc <> 0.
*   ERRO: Nº Depósito Inválido !
    MESSAGE s146(zwmmsg001).
    EXIT.
  ENDIF.

  CLEAR: zwm_sscc_aux.

  PERFORM select_data.

  IF tab_out[] IS INITIAL.
*   Não existem dados para processar !
    MESSAGE s147(zwmmsg001).
    EXIT.
  ENDIF.

* Start of Selection
END-OF-SELECTION.

  vbsk-sammg = p_refnr.

  PERFORM fill_itcpo.
  PERFORM open_form USING w_subrc.

  CHECK w_subrc = 0.

  PERFORM item_print.

  PERFORM form_close.

*&--------------------------------------------------------------------*
*&      Form  item_print
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM item_print.

** RL -> INS 26.04.2005
  DATA: l_ordem LIKE zwm_folha_cargas-ordem,
        l_index LIKE sy-tabix.

  DESCRIBE TABLE itab_rem LINES l_index.
  l_index = l_index + 1.
** RL <- INS 26.04.2005

  CALL FUNCTION 'WRITE_FORM'           "First header
       EXPORTING  element = 'HEADER'
       EXCEPTIONS OTHERS  = 1.
  CALL FUNCTION 'WRITE_FORM'           "Activate header
       EXPORTING  element = 'HEADER'
                  type    = 'TOP'
       EXCEPTIONS OTHERS  = 1.

  LOOP AT tab_out.
    CLEAR: nome_we, morada_we, cod_we, cidade_we, l_qtd.
    reg_out = tab_out.
    CLEAR zwm_folha_cargas.
    MOVE-CORRESPONDING tab_out TO zwm_folha_cargas.
    ON CHANGE OF tab_out-remessa.
      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command = 'PROTECT'.

*     Necessário para os textos da remessa
*      perform get_delivery_data using tab_out-remessa.
      vbdkl-tdname = tab_out-remessa.

** Cliente
      PERFORM get_address USING reg_out-kunnr.

** Recebedor de Mercadoria
      PERFORM get_address_we USING reg_out-remessa.

      CLEAR tab_total.
      READ TABLE tab_total WITH KEY tab_out-remessa BINARY SEARCH.
*     Vamos utilizar a estrutura *ZWM_SSCC_AUX porque já estamos a
*     utilizar a ZWM_SSCC_AUX para os totais
      MOVE-CORRESPONDING tab_total TO *zwm_sscc_aux.
*     Utilizamos as unidades de ZWM_SSCC_AUX
      MOVE zwm_sscc_aux-gewei_max TO *zwm_sscc_aux-gewei_max.
      MOVE zwm_sscc_aux-voleh_max TO *zwm_sscc_aux-voleh_max.

** RL -> INS 26.04.2005
      CLEAR: zwm_folha_cargas-ordem.
      l_index  = l_index  - 1.
      l_ordem  = l_index.
      zwm_folha_cargas-ordem = l_ordem.
** RL <- INS 26.04.2005

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'REMESSA'.

      CLEAR tab_subtotal.
      READ TABLE tab_subtotal WITH KEY tab_out-remessa BINARY SEARCH.
      WHILE sy-subrc = 0 AND tab_out-remessa = tab_subtotal-remessa.
        g_index = sy-tabix + 1.
*     Vamos utilizar a estrutura *ZWM_SSCC_AUX porque já estamos a
*     utilizar a ZWM_SSCC_AUX para os totais
        MOVE-CORRESPONDING tab_subtotal TO *zwm_sscc_aux.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'PALETES'.
        READ TABLE tab_subtotal INDEX g_index.
      ENDWHILE.

      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command = 'ENDPROTECT'.
    ENDON.

    CLEAR gt_vekp.
    READ TABLE gt_vekp WITH KEY exidv = tab_out-sscc.
    vekp = gt_vekp.

*   Verifica se existe algum material
    IF NOT gt_vekp-venum IS INITIAL.
      READ TABLE gt_vepo WITH KEY gt_vekp-venum BINARY SEARCH.
      IF sy-subrc = 0.
        zwm_folha_cargas-flag_material = 'X'.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'CONTROL_FORM'
      EXPORTING
        command = 'PROTECT'.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'SSCC'.

    IF NOT gt_vekp-venum IS INITIAL.
      READ TABLE gt_vepo WITH KEY gt_vekp-venum BINARY SEARCH.
      WHILE sy-subrc = 0 AND gt_vepo-venum = gt_vekp-venum.
        g_index = sy-tabix + 1.
        MOVE-CORRESPONDING gt_vepo TO vepo.
        PERFORM get_material USING vepo-matnr.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'MATERIAL'.
        READ TABLE gt_vepo INDEX g_index.
      ENDWHILE.
    ENDIF.

    CALL FUNCTION 'CONTROL_FORM'
      EXPORTING
        command = 'ENDPROTECT'.

  ENDLOOP.

* Total Geral por Paletes
  LOOP AT tab_totpal.
    AT FIRST.
      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command = 'PROTECT'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TOTAL_HEADER'.
    ENDAT.
    MOVE-CORRESPONDING tab_totpal TO *zwm_sscc_aux.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'TOTAL_LINE'.
    AT LAST.
      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command = 'ENDPROTECT'.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "ITEM_PRINT

*&--------------------------------------------------------------------*
*&      Form  fill_itcpo
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM fill_itcpo.
  CLEAR itcpo.
  itcpo-tdimmed    = 'X'.
  itcpo-tddelete   = 'X'.
  itcpo-tdpreview  = 'X'.
  itcpo-tdcopies   = 2.
  itcpo-tdprogram  = sy-repid.
  itcpo-tdteleland = sy-langu.
  itcpo-tdsenddate = sy-datum.
  itcpo-tdtitle    = sy-rtitl.
  itcpo-tdsenddate = sy-datum.
  itcpo-tdsendtime = sy-uzeit.
ENDFORM.                    "FILL_ITCPO

*&--------------------------------------------------------------------*
*&      Form  open_form
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_SUBRC    text
*---------------------------------------------------------------------*
FORM open_form USING f_subrc.
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device   = 'PRINTER'
      dialog   = 'X'
      form     = 'ZWM_FOLHA_CARGA'
      language = sy-langu
      options  = itcpo
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc NE 0.
    f_subrc = sy-subrc.
  ENDIF.
ENDFORM.                    "OPEN_FORM

*&--------------------------------------------------------------------*
*&      Form  form_close
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM form_close.
  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc NE 0.
    w_subrc = 1.
  ENDIF.
ENDFORM.                    "FORM_CLOSE

*&---------------------------------------------------------------------*
*&      Form  valida_parametros_entrada
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_parametros_entrada .

  SELECT SINGLE * FROM t300
                    WHERE
                      lgnum = p_lgnum.
ENDFORM.                    "valida_parametros_entrada

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

  DATA: l_destino LIKE zwm013-destino.

  RANGES: lr_destino FOR zwm013-destino.

  DATA: lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        lt_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE,
** RL -> INS 07.06.2005 -------------------------------------
        lt_zwm040 LIKE zwm040 OCCURS 0 WITH HEADER LINE.
** RL <- INS 07.06.2005 -------------------------------------

  DATA: BEGIN OF lt_zwm013 OCCURS 0,
          sscc           LIKE zwm013-sscc,
          destino        LIKE zwm013-destino,
          posicao_pulmao LIKE zwm013-posicao_pulmao,
        END OF lt_zwm013.

  DATA: BEGIN OF lt_ltap OCCURS 0,
          vbeln LIKE ltap-vbeln,
          vlenr LIKE ltap-vlenr,
        END OF lt_ltap.

  DATA: BEGIN OF lt_vttp OCCURS 0,
          vbeln LIKE vttp-vbeln,
          tknum LIKE vttp-tknum,
          tpnum LIKE vttp-tpnum,
        END OF lt_vttp.

  REFRESH tab_pal. CLEAR tab_pal.

* ZWM028
  SELECT * FROM zwm028 INTO TABLE lt_zwm028
           WHERE lgnum = p_lgnum
             AND refnr = p_refnr.

* ZWM026
  SELECT * FROM zwm026 INTO TABLE lt_zwm026
           WHERE armazem = p_lgnum
             AND grupo   = p_refnr.
  DELETE lt_zwm026 WHERE remessa IS INITIAL.

* Cria TAB_GER
  LOOP AT lt_zwm028.

** RL -> INS 07.06.2005 -------------------------------------
    FREE: lt_zwm040.
    CLEAR: lt_zwm040.
** RL <- INS 07.06.2005 -------------------------------------

    IF lt_zwm028-remessa IS INITIAL.
      CLEAR zwm002.
      SELECT SINGLE * FROM zwm002
             WHERE pulmao_1 = lt_zwm028-pulmao1.

      CONCATENATE c_prefixo zwm002-porta+1(2)
          INTO zwm_sscc_aux-desc_porta.

      CLEAR lr_destino.

      lr_destino-sign = 'I'.
      lr_destino-option = 'EQ'.

      CLEAR l_destino.
      CONCATENATE lt_zwm028-st_pul lt_zwm028-pulmao1
                 INTO l_destino SEPARATED BY space.
      lr_destino-low = l_destino.
      APPEND lr_destino.

      IF NOT lt_zwm028-pulmao2 IS INITIAL.
        CLEAR l_destino.
        CONCATENATE lt_zwm028-st_pul lt_zwm028-pulmao2
                   INTO l_destino SEPARATED BY space.
        lr_destino-low = l_destino.
        APPEND lr_destino.
      ENDIF.

    ELSE.

** RL -> INS 07.06.2005 -------------------------------------
      IF lt_zwm028-servisan = 'X'.
        SELECT * FROM zwm040
        INTO CORRESPONDING FIELDS OF TABLE lt_zwm040
        WHERE id_servisan EQ lt_zwm028-remessa
          AND lgnum       EQ p_lgnum
          AND refnr       EQ p_refnr.

        LOOP AT lt_zwm040.
          MOVE-CORRESPONDING lt_zwm028 TO tab_ger.
          tab_ger-remessa = lt_zwm040-remessa.
          APPEND tab_ger.
          CLEAR tab_ger.
        ENDLOOP.
      ELSE.
** RL <- INS 07.06.2005 -------------------------------------
        CLEAR tab_ger.
        MOVE-CORRESPONDING lt_zwm028 TO tab_ger.
        APPEND tab_ger.
** RL -> INS 07.06.2005 -------------------------------------
      ENDIF.
** RL <- INS 07.06.2005 -------------------------------------
    ENDIF.
  ENDLOOP.

  IF NOT tab_ger[] IS INITIAL.
    SELECT vbeln vlenr
           FROM ltap INTO TABLE lt_ltap
                FOR ALL ENTRIES IN tab_ger
                    WHERE vbeln = tab_ger-remessa.
    DELETE lt_ltap WHERE vlenr IS INITIAL.

    SELECT vbeln tknum tpnum
           FROM vttp INTO TABLE lt_vttp
                FOR ALL ENTRIES IN tab_ger
                    WHERE vbeln = tab_ger-remessa.
  ENDIF.

  IF NOT lr_destino[] IS INITIAL.
    REFRESH lt_zwm013.
    SELECT sscc destino posicao_pulmao
           FROM zwm013 INTO TABLE lt_zwm013
                WHERE armazem = p_lgnum
                  AND destino IN lr_destino.
  ENDIF.

  LOOP AT lt_zwm026.
    lt_ltap-vbeln = lt_zwm026-remessa.
    lt_ltap-vlenr = lt_zwm026-sscc.
    COLLECT lt_ltap.
  ENDLOOP.

  SORT lt_ltap   BY vbeln vlenr.
  SORT lt_zwm013 BY sscc.

  LOOP AT lt_ltap.
    CLEAR: lt_zwm013.
    READ TABLE lt_zwm013 WITH KEY lt_ltap-vlenr BINARY SEARCH.
    tab_pal-remessa        = lt_ltap-vbeln.
    tab_pal-sscc           = lt_zwm013-sscc.
    tab_pal-destino        = lt_zwm013-destino.
    tab_pal-posicao_pulmao = lt_zwm013-posicao_pulmao.
    APPEND tab_pal.
  ENDLOOP.

  SORT tab_ger BY refnr ordem remessa.
  SORT tab_pal BY remessa sscc destino.

  FREE: lt_zwm028, lt_zwm026,lt_zwm013, lt_ltap.

* Cria TAB_OUT
  PERFORM cria_tab_out.

  IF NOT tab_out[] IS INITIAL.
    SELECT * FROM vekp INTO TABLE gt_vekp
                FOR ALL ENTRIES IN tab_out
                    WHERE exidv = tab_out-sscc.
    IF NOT gt_vekp[] IS INITIAL.
      SELECT * FROM vepo INTO CORRESPONDING FIELDS OF TABLE gt_vepo
             FOR ALL ENTRIES IN gt_vekp
                 WHERE venum = gt_vekp-venum.
    ENDIF.
    SORT gt_vekp BY exidv venum.
    SORT gt_vepo BY venum vepos.
  ENDIF.

  REFRESH: tab_total, tab_subtotal.
  LOOP AT tab_out.
    CLEAR gt_vekp.
    READ TABLE gt_vekp WITH KEY exidv = tab_out-sscc.
    CHECK sy-subrc = 0.

    zwm_sscc_aux-paletes   = zwm_sscc_aux-paletes + 1.
    zwm_sscc_aux-brgew     = zwm_sscc_aux-brgew + gt_vekp-brgew.
    zwm_sscc_aux-btvol     = zwm_sscc_aux-btvol + gt_vekp-btvol.
    zwm_sscc_aux-gewei_max = gt_vekp-gewei_max.
    zwm_sscc_aux-voleh_max = gt_vekp-voleh_max.

    CLEAR tab_total.
    tab_total-remessa = tab_out-remessa.
    tab_total-brgew   = gt_vekp-brgew.
    tab_total-btvol   = gt_vekp-btvol.
    COLLECT tab_total.

    CLEAR tab_subtotal.
    tab_subtotal-remessa = tab_out-remessa.
    tab_subtotal-vhilm   = gt_vekp-vhilm.
    tab_subtotal-paletes = 1.
    tab_subtotal-brgew   = gt_vekp-brgew.
    tab_subtotal-btvol   = gt_vekp-btvol.
    COLLECT tab_subtotal.

    CLEAR tab_totpal.
    tab_totpal-vhilm   = gt_vekp-vhilm.
    tab_totpal-paletes = 1.
    COLLECT tab_totpal.

  ENDLOOP.

  SORT tab_total    BY remessa.
  SORT tab_subtotal BY remessa vhilm.
  SORT tab_totpal   BY vhilm.

  CLEAR: vttk, lfa1, lt_vttp.
  READ TABLE lt_vttp INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM vttk
           WHERE tknum = lt_vttp-tknum.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM lfa1 WHERE lifnr = vttk-tdlnr.
    ENDIF.
  ENDIF.

ENDFORM.                    "select_data

*&--------------------------------------------------------------------*
*&      Form  cria_tab_out
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM cria_tab_out.

  REFRESH tab_out. CLEAR tab_out.

  LOOP AT tab_ger.

    CLEAR tab_out.
    tab_out-refnr   = tab_ger-refnr.
    tab_out-ordem   = tab_ger-ordem.
    tab_out-remessa = tab_ger-remessa.

    SELECT SINGLE * FROM likp WHERE vbeln = tab_ger-remessa.
    IF sy-subrc = 0.
      tab_out-kunnr = likp-kunnr.
    ENDIF.

    CLEAR: tab_pal.
    READ TABLE tab_pal WITH KEY tab_ger-remessa BINARY SEARCH.
    WHILE sy-subrc = 0 AND tab_pal-remessa = tab_ger-remessa.
      g_index = sy-tabix + 1.
      tab_out-sscc           = tab_pal-sscc.
      tab_out-destino        = tab_pal-destino.
      tab_out-posicao_pulmao = tab_pal-posicao_pulmao.
      APPEND tab_out.
      READ TABLE tab_pal INDEX g_index.
    ENDWHILE.

    IF g_index IS INITIAL.
      APPEND tab_out.
    ENDIF.

** RL -> INS 26.04.2005
    itab_rem-refnr = tab_ger-refnr.
    itab_rem-vbeln = tab_ger-remessa.
*    itab_rem-index = 1.
    COLLECT itab_rem.
    CLEAR: itab_rem.
    SORT itab_rem.
** RL <- INS 26.04.2005

  ENDLOOP.

  SORT tab_out BY refnr                  DESCENDING
                  ordem                  ASCENDING
                  destino posicao_pulmao ASCENDING.

ENDFORM.                    "cria_tab_out

*&--------------------------------------------------------------------*
*&      Form  get_address
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_KUNNR    text
*---------------------------------------------------------------------*
FORM get_address USING f_kunnr.
  CLEAR: kna1, adrc.
  SELECT SINGLE * FROM kna1 WHERE kunnr = f_kunnr.
  IF sy-subrc = 0 AND NOT kna1-adrnr IS INITIAL.
    CALL FUNCTION 'ADDR_SELECT_ADRC_SINGLE'
      EXPORTING
        addrnumber        = kna1-adrnr
      TABLES
        et_adrc           = gt_adrc
      EXCEPTIONS
        address_not_exist = 1
        parameter_error   = 2
        internal_error    = 3
        OTHERS            = 4.

    IF sy-subrc <> 0.
      adrc-name1      = kna1-name1.
      adrc-city1      = kna1-ort01.
      adrc-post_code1 = kna1-pstlz.
      adrc-street     = kna1-stras.
    ELSE.
      READ TABLE gt_adrc INDEX 1.
      IF sy-subrc = 0.
        adrc = gt_adrc.
      ELSE.
        adrc-name1      = kna1-name1.
        adrc-city1      = kna1-ort01.
        adrc-post_code1 = kna1-pstlz.
        adrc-street     = kna1-stras.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "get_address

*&--------------------------------------------------------------------*
*&      Form  get_delivery_data
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_VBELN    text
*---------------------------------------------------------------------*
FORM get_delivery_data USING f_vbeln.

  vbco3-spras = sy-langu.
  vbco3-vbeln = f_vbeln.

  CALL FUNCTION 'RV_DELIVERY_PRINT_VIEW'
    EXPORTING
      comwa = vbco3
    IMPORTING
      kopf  = vbdkl
    TABLES
      pos   = tvbdpl.

ENDFORM.                    "get_delivery_data

*&--------------------------------------------------------------------*
*&      Form  get_material
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_MATNR    text
*---------------------------------------------------------------------*
FORM get_material USING f_matnr.

  RANGES: r_meinh FOR marm-meinh.

  DATA: l_index LIKE sy-tabix.

  FREE: gt_marm.

  r_meinh-sign = 'E'.
  r_meinh-option = 'EQ'.

  r_meinh-low = 'PAL'. "ZKG
  APPEND r_meinh.

  r_meinh-low = 'ZKG'.
  APPEND r_meinh.

  CLEAR: mara, makt, *marm.
  SELECT SINGLE * FROM mara WHERE matnr = f_matnr.
  SELECT SINGLE * FROM makt WHERE matnr = f_matnr
                              AND spras = sy-langu.

  SELECT matnr meinh umrez umren
         FROM marm INTO TABLE gt_marm
             WHERE matnr EQ f_matnr
               AND meinh IN r_meinh.

  CLEAR gt_marm.
  READ TABLE gt_marm WITH KEY matnr = f_matnr
                              meinh = mara-meins.
  IF sy-subrc = 0.
    l_index = sy-tabix + 1.
    READ TABLE gt_marm INDEX l_index.
    IF sy-subrc NE 0.
** Se não tiver nenhuma unid. seguinte fica com a UMB
      l_index = sy-tabix - 1.
      READ TABLE gt_marm INDEX l_index.
    ENDIF.
  ENDIF.

  MOVE gt_marm-umren TO marm-umren.
  MOVE gt_marm-meinh TO l_qtd.

   *marm-umren = vepo-vemng * marm-umren.

ENDFORM.                    "get_material
*&---------------------------------------------------------------------*
*&      Form  get_address_we
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_REG_OUT_REMESSA  text
*----------------------------------------------------------------------*
FORM get_address_we  USING    p_remessa.

  DATA: l_doc   TYPE vbeln,
        l_ordem TYPE vbeln,
        l_adrnr TYPE ad_addrnum,
        l_kunnr LIKE kna1-kunnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_remessa
    IMPORTING
      output = l_doc.

  CLEAR: vbfa.
  SELECT * FROM vbfa
  WHERE vbeln EQ l_doc
    AND vbtyp_n EQ 'J'
    AND vbtyp_v EQ 'C'.
    l_ordem = vbfa-vbelv.
    EXIT.
  ENDSELECT.

  CHECK NOT l_ordem IS INITIAL.
  CLEAR: vbpa, l_adrnr.
  SELECT * FROM vbpa
  WHERE vbeln EQ l_ordem
    AND parvw EQ 'WE'.
    l_kunnr = vbpa-kunnr.
    l_adrnr = vbpa-adrnr.
    EXIT.
  ENDSELECT.

  CHECK NOT l_kunnr IS INITIAL.

  CLEAR: kna1.

  IF l_adrnr IS INITIAL.
    SELECT SINGLE * FROM kna1 WHERE kunnr = l_kunnr.
    l_adrnr = kna1-adrnr.
  ENDIF.

  IF sy-subrc = 0 AND NOT kna1-adrnr IS INITIAL.
    CALL FUNCTION 'ADDR_SELECT_ADRC_SINGLE'
      EXPORTING
        addrnumber        = l_adrnr
      TABLES
        et_adrc           = gt_adrc
      EXCEPTIONS
        address_not_exist = 1
        parameter_error   = 2
        internal_error    = 3
        OTHERS            = 4.

    IF sy-subrc <> 0.
      nome_we    = kna1-name1.
      morada_we  = kna1-ort01.
      cod_we     = kna1-pstlz.
      cidade_we  = kna1-stras.
    ELSE.
      READ TABLE gt_adrc INDEX 1.
      IF sy-subrc = 0.
        nome_we   = gt_adrc-name1.
        morada_we = gt_adrc-street.
        cod_we    = gt_adrc-post_code1.
        cidade_we = gt_adrc-city1.
      ELSE.
        nome_we    = kna1-name1.
        morada_we  = kna1-ort01.
        cod_we     = kna1-pstlz.
        cidade_we  = kna1-stras.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_address_we
