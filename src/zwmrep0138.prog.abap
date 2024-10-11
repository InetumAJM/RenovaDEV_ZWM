************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0138                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: RF - Armazém Alugado França - Impressão Nota de Entrega  *
* Criado por: Ricardo Sousa                                            *
* Criado em.: 04/06/2019                                               *
* Tipo PRG..: Report                                                   *
************************************************************************
REPORT zwmrep0138 LINE-COUNT 100.

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
        lips,
        t300,
        itcpo.

TABLES: vbco3,                         "Communicationarea for view
        vbdkl,                         "Headerview
        vbdpl.                         "Itemview

TABLES: zwm002,
*        zwm013,
        zwm026,
        zwm040,
        zwm028, *zwm028,
        zwm_folha_cargas,
        zwm_sscc_aux, *zwm_sscc_aux,
        zwm046.

DATA: BEGIN OF tvbdpl OCCURS 0.        "Internal table for items
        INCLUDE STRUCTURE vbdpl.
DATA: END OF tvbdpl.

***** variáveis e tabelas internas

DATA: gt_ltak LIKE ltak OCCURS 0 WITH HEADER LINE,
      gt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE,
      gt_adrc LIKE adrc OCCURS 0 WITH HEADER LINE,
      gt_vekp LIKE vekp OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF itab_likp OCCURS 0,
       vbeln  LIKE likp-vbeln,
       volum  LIKE likp-volum,
       voleh  LIKE likp-voleh,
       btgew  LIKE likp-btgew,
       gewei  LIKE likp-gewei,
      END OF itab_likp.

DATA: BEGIN OF gt_marm OCCURS 0,
        matnr LIKE marm-matnr,
        meinh LIKE marm-meinh,
        umrez LIKE marm-umrez,
        umren LIKE marm-umren,
      END OF gt_marm.

DATA: BEGIN OF itab_rem OCCURS 0,
        refnr LIKE zwm028-refnr,
        vbeln LIKE zwm028-remessa,
        index LIKE sy-tabix,
      END OF itab_rem.

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
         id_servisan    LIKE zwm040-id_servisan,
      END OF tab_ger,
      reg_ger LIKE tab_ger,
      key_ger LIKE tab_ger.

DATA: BEGIN OF tab_pal OCCURS 0,
        remessa  LIKE lips-vbeln,
        matnr    LIKE lips-matnr,
        vrkme    LIKE lips-vrkme,
        lfimg    LIKE lips-lfimg,
      END OF tab_pal.

DATA: BEGIN OF tab_out OCCURS 0,
         refnr          LIKE zwm028-refnr,
         ordem          LIKE zwm028-ordem,
         matnr          LIKE lips-matnr,
         remessa        LIKE zwm028-remessa,
         id_servisan    LIKE zwm040-id_servisan,
         kunnr          LIKE kna1-kunnr,
         lfimg          LIKE lips-lfimg,
         vrkme          LIKE lips-vrkme,
         spras          LIKE makt-spras,
         txt_pal(50),
      END OF tab_out,
      reg_out LIKE tab_out.


DATA: tab_out_merch LIKE tab_out OCCURS 0 WITH HEADER LINE.
*DATA: tab_out_aux   LIKE tab_out OCCURS 0 WITH HEADER LINE.


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

DATA: BEGIN OF tab_marm_he OCCURS 0,
        matnr LIKE marm-matnr,
        meinh LIKE marm-meinh,
        numtp LIKE marm-numtp,
      END OF tab_marm_he.

DATA: BEGIN OF tab_ltak OCCURS 0,
        tanum LIKE ltak-tanum,
        vbeln LIKE ltak-vbeln,
      END OF tab_ltak.

DATA: BEGIN OF tab_ltap OCCURS 0,
        vbeln LIKE ltap-vbeln,
        vlenr LIKE ltap-vlenr,
        vorga LIKE ltap-vorga,
      END OF tab_ltap.

DATA: BEGIN OF tab_026 OCCURS 0,
        remessa LIKE zwm026-remessa,
        sscc    LIKE zwm026-sscc,
      END OF tab_026.

DATA: nome_we   LIKE adrc-name1,
      morada_we LIKE adrc-street,
      cod_we    LIKE adrc-post_code1,
      cidade_we LIKE adrc-city1.

DATA: w_subrc LIKE sy-subrc.

DATA: g_index   LIKE sy-tabix,
      l_qtd     LIKE marm-meinh,
      l_umren   LIKE ekpo-menge,
      l_lfimg   LIKE ekpo-menge,
      l_qtd_he  LIKE ekpo-menge,
      l_pal     TYPE i,
      l_tot_pal TYPE i,
      l_un_he  LIKE marm-meinh,
      l_um(9),
      l_first,
      talao LIKE zwm002-num_entrada.

CONSTANTS: c_prefixo(12) VALUE 'DCK 000-000-'.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
*PARAMETERS: p_lgnum LIKE mlgn-lgnum OBLIGATORY MEMORY ID lgn,
 PARAMETERS: p_refnr LIKE zwm028-refnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

* Initialization
INITIALIZATION.
*
*  DATA: xuser LIKE lrf_wkqu OCCURS 0 WITH HEADER LINE.
*
** Read the user data from table lrf_wkqu ( for all the warehouses)
*  CALL FUNCTION 'L_USER_DATA_GET'
*    EXPORTING
*      i_uname        = sy-uname
*    TABLES
*      t_xuser        = xuser
*    EXCEPTIONS
*      no_entry_found = 01.
*
*  IF sy-subrc = 0.
*    READ TABLE xuser INDEX 1.
**    MOVE xuser-lgnum TO p_lgnum.
*  ENDIF.

* Start-of-Selection
START-OF-SELECTION.

*  PERFORM valida_parametros_entrada.
*  IF sy-subrc <> 0.
**   ERRO: Nº Depósito Inválido !
*    MESSAGE s146(zwmmsg001).
*    EXIT.
*  ENDIF.

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

*  IF sy-tcode = 'ZWM129'.

  "Vai ultilizar o Adobe Form para gerar o PDF
  PERFORM form_adobeprimt. "adicionado por Nuno Bairro

*  ELSE. "usa o sistema antigo
*    PERFORM fill_itcpo.
*    PERFORM open_form USING w_subrc.
**
*    CHECK w_subrc = 0.
**
*    PERFORM item_print. "aqui é onde está o processamento
**
**
*    PERFORM form_close.
*  ENDIF.

*&--------------------------------------------------------------------*
*&      Form  item_print
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM item_print.

  DATA: l_ordem LIKE zwm_folha_cargas-ordem,
        l_index LIKE sy-tabix.

  DATA: lv_remessa LIKE likp-vbeln.
  DATA: lv_first.
  DATA: ft_sscc LIKE zwm026-sscc OCCURS 0 WITH HEADER LINE.

  CLEAR: l_pal, l_tot_pal.

  DESCRIBE TABLE itab_rem LINES l_index.
  l_index = l_index + 1.

  CALL FUNCTION 'WRITE_FORM'           "First header
       EXPORTING  element = 'HEADER'
       EXCEPTIONS OTHERS  = 1.

  CALL FUNCTION 'WRITE_FORM'           "Activate header
       EXPORTING  element = 'HEADER'
                  type    = 'TOP'
       EXCEPTIONS OTHERS  = 1.


** Numero Total de Paletes
  LOOP AT tab_ltap. " Completas
    ft_sscc = tab_ltap-vlenr.
    APPEND ft_sscc.
  ENDLOOP.
  LOOP AT tab_026. " Picking
    ft_sscc = tab_026-sscc.
    APPEND ft_sscc.
  ENDLOOP.

  SORT ft_sscc.
  DELETE ADJACENT DUPLICATES FROM ft_sscc.
  DESCRIBE TABLE ft_sscc LINES l_tot_pal.

  REFRESH ft_sscc.
  CLEAR   ft_sscc.


** Remessas
  LOOP AT tab_out.
    IF sy-tabix <> 1.
      l_first = 'X'.
    ENDIF.

    CLEAR: nome_we, morada_we, cod_we, cidade_we, l_qtd.
    reg_out = tab_out.
    CLEAR zwm_folha_cargas.
    MOVE-CORRESPONDING tab_out TO zwm_folha_cargas.
    ON CHANGE OF tab_out-remessa.
      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command = 'PROTECT'.

*     Necessário para os textos da remessa
      vbdkl-tdname = tab_out-remessa.
      vbdkl-spras  = tab_out-spras.

** Cliente
      PERFORM get_address USING reg_out-kunnr.

** Recebedor de Mercadoria
      PERFORM get_address_we USING reg_out-remessa.

      CLEAR tab_total.
      READ TABLE tab_total WITH KEY remessa = tab_out-remessa
                           BINARY SEARCH.
      IF sy-subrc EQ 0.
*     Vamos utilizar a estrutura *ZWM_SSCC_AUX porque já estamos a
*     utilizar a ZWM_SSCC_AUX para os totais
        MOVE-CORRESPONDING tab_total TO *zwm_sscc_aux.
*     Utilizamos as unidades de ZWM_SSCC_AUX
        MOVE zwm_sscc_aux-gewei_max TO *zwm_sscc_aux-gewei_max.
        MOVE zwm_sscc_aux-voleh_max TO *zwm_sscc_aux-voleh_max.
      ENDIF.

      CLEAR: zwm_folha_cargas-ordem.
      l_index  = l_index  - 1.
      l_ordem  = l_index.
      zwm_folha_cargas-ordem = l_ordem.

**    Numero de Paletes
      LOOP AT tab_ltap WHERE vbeln EQ tab_out-remessa.  " Completas
        ft_sscc = tab_ltap-vlenr.
        APPEND ft_sscc.
      ENDLOOP.
      LOOP AT tab_026 WHERE remessa EQ tab_out-remessa. " Picking
        ft_sscc = tab_026-sscc.
        APPEND ft_sscc.
      ENDLOOP.


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

*        CALL FUNCTION 'WRITE_FORM'
*          EXPORTING
*            element = 'PALETES'.

        READ TABLE tab_subtotal INDEX g_index.
      ENDWHILE.

      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command = 'ENDPROTECT'.
    ENDON.

    zwm_folha_cargas-flag_material = 'X'.

    CALL FUNCTION 'CONTROL_FORM'
      EXPORTING
        command = 'PROTECT'.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'SSCC'.

*   PERFORM get_material USING tab_out-matnr.

    PERFORM unidade_he USING tab_out-matnr
                             tab_out-lfimg
                             tab_out-vrkme.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'MATERIAL'.

    CALL FUNCTION 'CONTROL_FORM'
      EXPORTING
        command = 'ENDPROTECT'.

    MOVE tab_out-remessa TO lv_remessa.

    AT END OF remessa.
      lv_first = 'X'.
      LOOP AT tab_out_merch WHERE remessa = lv_remessa.
        CLEAR: tab_out, makt, l_un_he, l_qtd_he.
        IF lv_first = 'X'.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'MERCH_TITLE'.
          CLEAR: lv_first.
        ENDIF.

        MOVE-CORRESPONDING tab_out_merch TO tab_out.
        MOVE tab_out_merch-txt_pal       TO makt-maktx.
        CLEAR: tab_out-txt_pal.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'MATERIAL_MER'.
      ENDLOOP.
    ENDAT.

    AT END OF ordem.
      SORT ft_sscc.
      DELETE ADJACENT DUPLICATES FROM ft_sscc.
      DESCRIBE TABLE ft_sscc LINES l_pal.

      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command = 'PROTECT'.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'PALETES'.

      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command = 'ENDPROTECT'.

      REFRESH ft_sscc.
      CLEAR:  ft_sscc, l_pal.
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
  itcpo-tdnewid    = 'X'.
*  itcpo-tdcopies   = 2.
  itcpo-tdprogram  = sy-repid.
  itcpo-tdteleland = sy-langu.
* itcpo-tdsenddate = sy-datum.
  itcpo-tdtitle    = sy-rtitl.
* itcpo-tdsenddate = sy-datum.
* itcpo-tdsendtime = sy-uzeit.

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
      form     = 'ZWM_NOTA_ENTR_2'
      language = sy-langu
      OPTIONS  = itcpo
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

*  SELECT SINGLE * FROM t300
*                    WHERE
*                      lgnum = p_lgnum.
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

  DATA: l_destino LIKE zwm013-destino,
        e_2step(1).

  RANGES: lr_destino FOR zwm013-destino.

  DATA: lv_sortf  TYPE sortf4.
  DATA: lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        lt_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE,
        lt_zwm040 LIKE zwm040 OCCURS 0 WITH HEADER LINE,
        lt_t311a  LIKE t311a  OCCURS 0 WITH HEADER LINE,
        lt_vbss   LIKE vbss   OCCURS 0 WITH HEADER LINE,
        lt_likp   LIKE likp   OCCURS 0 WITH HEADER LINE.

  DATA: itab_lips LIKE lips OCCURS 0 WITH HEADER LINE.

  DATA: itab_lips_merch LIKE lips OCCURS 0 WITH HEADER LINE.

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

** Remessas do Grupo
  SELECT *
    FROM vbss INTO TABLE lt_vbss
    WHERE sammg = p_refnr.

  IF lt_vbss[] IS NOT INITIAL.
    SELECT *
      FROM likp INTO TABLE lt_likp
      FOR ALL ENTRIES IN lt_vbss
      WHERE vbeln = lt_vbss-vbeln.
  ENDIF.

** Ordenar remessas
  SORT lt_likp BY lfdat lfuhr.

  LOOP AT lt_likp.

    lv_sortf = lv_sortf + 1.

    READ TABLE lt_vbss WITH KEY vbeln = lt_likp-vbeln.
    CHECK sy-subrc = 0.

    lt_vbss-sortf = lv_sortf.

    MODIFY lt_vbss INDEX sy-tabix.
  ENDLOOP.


* Cria TAB_GER
  LOOP AT lt_vbss.
    CLEAR tab_ger.
    tab_ger-refnr   = lt_vbss-sammg.
    tab_ger-remessa = lt_vbss-vbeln.
    tab_ger-ordem   = lt_vbss-sortf.
    APPEND tab_ger.
  ENDLOOP.

  LOOP AT tab_ger.
    SELECT * FROM lips
    APPENDING CORRESPONDING FIELDS OF TABLE itab_lips
    WHERE vbeln EQ tab_ger-remessa.
*      AND pstyv NE 'ZPAS'
*      AND lgort EQ 'CD'.
*      AND pstyv EQ 'ZOF'
*      OR  uecha EQ '      '.
  ENDLOOP.

  itab_lips_merch[] =  itab_lips[].

  DELETE itab_lips WHERE pstyv = 'ZPAS'.
*  DELETE itab_lips WHERE lgort <> 'CD'.

  IF NOT tab_ger[] IS INITIAL.
    SELECT vbeln tknum tpnum
           FROM vttp INTO TABLE lt_vttp
                FOR ALL ENTRIES IN tab_ger
                    WHERE vbeln = tab_ger-remessa.
  ENDIF.

  LOOP AT itab_lips.
    MOVE-CORRESPONDING itab_lips TO tab_pal.
    tab_pal-remessa = itab_lips-vbeln.
    COLLECT tab_pal.
    SORT tab_pal.
    CLEAR: tab_pal.
  ENDLOOP.

  SORT tab_ger BY refnr ordem remessa.
  SORT tab_pal BY remessa matnr vrkme.

  FREE: lt_zwm028, lt_zwm026.

* Cria TAB_OUT
  PERFORM cria_tab_out.

** cria tabela merch
*  DELETE itab_lips_merch WHERE lgort = 'CD'.
*  DELETE itab_lips_merch WHERE lgort = ' '.
*
*  LOOP AT itab_lips_merch.
*    CLEAR: tab_out_merch.
*
*    READ TABLE tab_out WITH KEY remessa = itab_lips_merch-vbeln.
*    IF sy-subrc = 0.
*      MOVE tab_out-ordem TO tab_out_merch-ordem.
*    ENDIF.
*
*    MOVE itab_lips_merch-matnr TO tab_out_merch-matnr.
*    MOVE itab_lips_merch-arktx TO tab_out_merch-txt_pal.
*    MOVE itab_lips_merch-lfimg TO tab_out_merch-lfimg.
*    MOVE itab_lips_merch-vrkme TO tab_out_merch-vrkme.
*    MOVE itab_lips_merch-vbeln TO tab_out_merch-remessa.
*    APPEND tab_out_merch.
*  ENDLOOP.

  REFRESH: tab_total, tab_subtotal.
*  LOOP AT tab_out.

  LOOP AT itab_likp.
    zwm_sscc_aux-brgew     = zwm_sscc_aux-brgew + itab_likp-btgew.
    zwm_sscc_aux-btvol     = zwm_sscc_aux-btvol + itab_likp-volum.

    CLEAR tab_total.
    tab_total-remessa = itab_likp-vbeln.
    tab_total-brgew   = itab_likp-btgew.
    tab_total-btvol   = itab_likp-volum.
    COLLECT tab_total.

    CLEAR tab_subtotal.
    tab_subtotal-remessa = itab_likp-vbeln.
    tab_subtotal-brgew   = itab_likp-btgew.
    tab_subtotal-btvol   = itab_likp-volum.
    COLLECT tab_subtotal.
  ENDLOOP.

  SORT tab_total    BY remessa.
  SORT tab_subtotal BY remessa vhilm.

  CLEAR: vttk, lfa1, lt_vttp.
  READ TABLE lt_vttp INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM vttk
           WHERE tknum = lt_vttp-tknum.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM lfa1 WHERE lifnr = vttk-tdlnr.
    ENDIF.
  ENDIF.

** Unidades HE
  REFRESH tab_marm_he.
  CLEAR   tab_marm_he.

  IF NOT tab_out[] IS INITIAL.
    SELECT matnr meinh numtp FROM marm INTO TABLE tab_marm_he
             FOR ALL ENTRIES IN tab_out
           WHERE matnr EQ tab_out-matnr.

    DELETE tab_marm_he WHERE numtp NE 'HE'.

    SORT tab_marm_he BY matnr.
  ENDIF.

** Numero de paletes
  REFRESH: tab_ltak, tab_ltap, tab_026.
  CLEAR:   tab_ltak, tab_ltap, tab_026.

*  SELECT tanum vbeln FROM ltak INTO TABLE tab_ltak
*         WHERE lgnum EQ p_lgnum
*           AND refnr EQ p_refnr.
*
*  IF e_2step IS INITIAL.
*    DELETE tab_ltak WHERE vbeln IS INITIAL.
*  ENDIF.
*
*  IF NOT tab_ltak[] IS INITIAL.
*    SELECT vbeln vlenr vorga FROM ltap INTO TABLE tab_ltap
*         FOR ALL ENTRIES IN tab_ltak
*       WHERE lgnum EQ p_lgnum
*         AND tanum EQ tab_ltak-tanum.
*
*    DELETE tab_ltap WHERE vlenr IS INITIAL.
*    DELETE tab_ltap WHERE vorga = 'ST'.
*    SORT tab_ltap BY vbeln vlenr.
*    DELETE ADJACENT DUPLICATES FROM tab_ltap.
*  ENDIF.
*
*** Paletes de Picking
*  SELECT remessa sscc FROM zwm026 INTO TABLE tab_026
*         WHERE armazem EQ p_lgnum
*           AND grupo   EQ p_refnr.
*
*  DELETE tab_026 WHERE sscc IS INITIAL.
*  SORT tab_026 BY remessa sscc.
*  DELETE ADJACENT DUPLICATES FROM tab_026.


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
    tab_out-refnr       = tab_ger-refnr.
    tab_out-ordem       = tab_ger-ordem.
    tab_out-remessa     = tab_ger-remessa.
    tab_out-id_servisan = tab_ger-id_servisan.

    SELECT SINGLE * FROM likp WHERE vbeln = tab_ger-remessa.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING likp TO itab_likp.
      APPEND itab_likp.
      CLEAR: itab_likp.
      tab_out-kunnr = likp-kunnr.
    ENDIF.

    SELECT SINGLE spras INTO tab_out-spras
        FROM kna1
            WHERE kunnr = likp-kunnr.

    CLEAR: tab_pal.
    READ TABLE tab_pal WITH KEY tab_ger-remessa BINARY SEARCH.
    WHILE sy-subrc = 0 AND tab_pal-remessa = tab_ger-remessa.
      g_index = sy-tabix + 1.
      tab_out-matnr = tab_pal-matnr.
      tab_out-lfimg = tab_pal-lfimg.
      tab_out-vrkme = tab_pal-vrkme.

** Incluir texto de Paletes ( Escrava e Meia )
      CLEAR tab_out-txt_pal.
      SELECT SINGLE *
          FROM zwm046
              WHERE kunnr = likp-kunnr
                AND matnr = tab_pal-matnr.

      IF sy-subrc = 0.
        IF NOT zwm046-palete IS INITIAL.
          tab_out-txt_pal = 'Palete Escrava - SIM'.
        ELSE.
          tab_out-txt_pal = 'Palete Escrava - NÃO'.
        ENDIF.
      ELSE.
        SELECT SINGLE *
        FROM zwm046
            WHERE kunnr = '' AND
                  matnr = tab_pal-matnr.
        IF sy-subrc = 0.
          IF NOT zwm046-palete IS INITIAL.
            tab_out-txt_pal = 'Palete Escrava - SIM'.
          ELSE.
            tab_out-txt_pal = 'Palete Escrava - NÃO'.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND tab_out.
      READ TABLE tab_pal INDEX g_index.
    ENDWHILE.

    IF g_index IS INITIAL.
      APPEND tab_out.
    ENDIF.

    itab_rem-refnr = tab_ger-refnr.
    itab_rem-vbeln = tab_ger-remessa.
    COLLECT itab_rem.
    CLEAR: itab_rem.
    SORT itab_rem.

  ENDLOOP.

  DELETE tab_out WHERE lfimg IS INITIAL.

  SORT tab_out BY refnr DESCENDING
                  ordem ASCENDING
                  remessa ASCENDING.

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

*  r_meinh-low = 'PAL'. "ZKG
*  APPEND r_meinh.

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

  MOVE gt_marm-umren TO l_umren.
  MOVE gt_marm-meinh TO l_qtd.
  MOVE tab_out-lfimg TO l_lfimg.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = mara-meins
      language       = sy-langu
    IMPORTING
      output         = mara-meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CONCATENATE gt_marm-meinh '/' mara-meins INTO l_um
              SEPARATED BY space.

  CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
    EXPORTING
      i_matnr              = f_matnr
      i_in_me              = tab_out-vrkme
      i_out_me             = gt_marm-meinh
      i_menge              = l_lfimg
    IMPORTING
      e_menge              = l_lfimg
    EXCEPTIONS
      error_in_application = 1
      error                = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

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
        l_adrnr TYPE ad_addrnum,
        l_kunnr LIKE kna1-kunnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_remessa
    IMPORTING
      output = l_doc.

  CLEAR: vbpa, l_adrnr.
  SELECT * FROM vbpa
  WHERE vbeln EQ l_doc
    AND parvw EQ 'WE'.
    l_kunnr = vbpa-kunnr.
    l_adrnr = vbpa-adrnr.
    EXIT.
  ENDSELECT.

  IF l_kunnr IS INITIAL.
    CLEAR: vbpa, l_adrnr.
    SELECT * FROM vbpa
    WHERE vbeln EQ l_doc
      AND parvw EQ 'W1'.
      l_kunnr = vbpa-kunnr.
      l_adrnr = vbpa-adrnr.
      EXIT.
    ENDSELECT.
  ENDIF.

  CHECK NOT l_kunnr IS INITIAL.

  CLEAR: kna1.

  IF l_adrnr IS INITIAL.
    SELECT SINGLE * FROM kna1 WHERE kunnr = l_kunnr.
    l_adrnr = kna1-adrnr.
  ENDIF.

  IF sy-subrc = 0 AND NOT l_adrnr IS INITIAL.
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

*&---------------------------------------------------------------------*
*&      Form  unidade_he
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM unidade_he USING pu_matnr
                      pu_lfimg
                      pu_vrkme.

  CLEAR: l_qtd_he, l_un_he.

** Descrição do Material
  CLEAR: makt.
  SELECT SINGLE * FROM makt WHERE matnr = pu_matnr
                              AND spras = sy-langu.

  CHECK NOT pu_matnr IS INITIAL AND
        NOT pu_lfimg IS INITIAL AND
        NOT pu_vrkme IS INITIAL.

  READ TABLE tab_marm_he WITH KEY matnr = pu_matnr BINARY SEARCH.
  CHECK sy-subrc EQ 0.

  IF tab_marm_he-meinh NE pu_vrkme.
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = pu_matnr
        i_in_me              = pu_vrkme
        i_out_me             = tab_marm_he-meinh
        i_menge              = pu_lfimg
      IMPORTING
        e_menge              = l_qtd_he
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    IF sy-subrc EQ 0.
      l_un_he = tab_marm_he-meinh.
    ENDIF.

  ELSE.
    l_qtd_he = pu_lfimg.
    l_un_he  = pu_vrkme.

  ENDIF.

ENDFORM.                    " unidade_he
*&---------------------------------------------------------------------*
*&      Form  FORM_PRIMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_adobeprimt .

*************************Tabelas para Adobe***************************
  DATA: tb_remessa TYPE STANDARD TABLE OF zwm_st_notaheader
        WITH HEADER LINE,
        l_remessa TYPE zwm_st_notaheader,

        tb_material TYPE STANDARD TABLE OF zwm_st_notamat
        WITH HEADER LINE,
        l_material TYPE zwm_st_notamat,


        remessa_tab TYPE zwm_tb_notaheader,
        material_tab TYPE zwm_tb_notamat.

  DATA:
        z_pesototal TYPE brgew_vekp,
        z_volumetotal TYPE btvol_vekp,
        z_totalpalete  TYPE char10,
        z_matricula TYPE char20,
        z_nometrans TYPE char80,
        z_codtrans TYPE char10,
        z_transporte TYPE char20,
        z_numporta TYPE char30,
        z_numnota TYPE char10,
        z_talao TYPE char10.




**********************************************************************

  DATA: l_ordem LIKE zwm_folha_cargas-ordem,
        l_index LIKE sy-tabix.


  DATA: lv_remessa LIKE likp-vbeln.
  DATA: lv_first.
  DATA: ft_sscc LIKE zwm026-sscc OCCURS 0 WITH HEADER LINE.

  CLEAR: l_pal, l_tot_pal.

  DESCRIBE TABLE itab_rem LINES l_index.
  l_index = l_index + 1.

** Numero Total de Paletes
  LOOP AT tab_ltap. " Completas
    ft_sscc = tab_ltap-vlenr.
    APPEND ft_sscc.
  ENDLOOP.
  LOOP AT tab_026. " Picking
    ft_sscc = tab_026-sscc.
    APPEND ft_sscc.
  ENDLOOP.

  SORT ft_sscc.
  DELETE ADJACENT DUPLICATES FROM ft_sscc.
  DESCRIBE TABLE ft_sscc LINES l_tot_pal.

  REFRESH ft_sscc.
  CLEAR   ft_sscc.


** Remessas
  LOOP AT tab_out.
    IF sy-tabix <> 1.
      l_first = 'X'.
    ENDIF.

    CLEAR: nome_we, morada_we, cod_we, cidade_we, l_qtd.
    reg_out = tab_out.
    CLEAR zwm_folha_cargas.
    MOVE-CORRESPONDING tab_out TO zwm_folha_cargas.
    ON CHANGE OF tab_out-remessa.

*     Necessário para os textos da remessa
      vbdkl-tdname = tab_out-remessa.
      vbdkl-spras  = tab_out-spras.
      "com esta função vamos popular a tabela de Textos


** Cliente
      PERFORM get_address USING reg_out-kunnr.

** Recebedor de Mercadoria
      PERFORM get_address_we USING reg_out-remessa.

      CLEAR tab_total.
      READ TABLE tab_total WITH KEY remessa = tab_out-remessa
                           BINARY SEARCH.
      IF sy-subrc EQ 0.
*     Vamos utilizar a estrutura *ZWM_SSCC_AUX porque já estamos a
*     utilizar a ZWM_SSCC_AUX para os totais
        MOVE-CORRESPONDING tab_total TO *zwm_sscc_aux.
*     Utilizamos as unidades de ZWM_SSCC_AUX
        MOVE zwm_sscc_aux-gewei_max TO *zwm_sscc_aux-gewei_max.
        MOVE zwm_sscc_aux-voleh_max TO *zwm_sscc_aux-voleh_max.
      ENDIF.

      CLEAR: zwm_folha_cargas-ordem.
      l_index  = l_index  - 1.
      l_ordem  = l_index.
      zwm_folha_cargas-ordem = l_ordem.

**    Numero de Paletes
      LOOP AT tab_ltap WHERE vbeln EQ tab_out-remessa.  " Completas
        ft_sscc = tab_ltap-vlenr.
        APPEND ft_sscc.
      ENDLOOP.
      LOOP AT tab_026 WHERE remessa EQ tab_out-remessa. " Picking
        ft_sscc = tab_026-sscc.
        APPEND ft_sscc.
      ENDLOOP.

****************************REMESSA**********************************

      MOVE: zwm_folha_cargas-remessa TO l_remessa-remessa,
             zwm_folha_cargas-ordem TO l_remessa-ordem,
             nome_we TO l_remessa-nome_we,
             morada_we TO l_remessa-morada_we,
             cod_we TO l_remessa-codpostal_we,
             cidade_we TO l_remessa-cidade_we,
             tab_out-spras TO l_remessa-idioma.

**********************************************************************

      CLEAR tab_subtotal.
      READ TABLE tab_subtotal WITH KEY tab_out-remessa BINARY SEARCH.
      WHILE sy-subrc = 0 AND tab_out-remessa = tab_subtotal-remessa.
        g_index = sy-tabix + 1.
*     Vamos utilizar a estrutura *ZWM_SSCC_AUX porque já estamos a
*     utilizar a ZWM_SSCC_AUX para os totais
        MOVE-CORRESPONDING tab_subtotal TO *zwm_sscc_aux.


***************************Subtotais de peso e volume******************



        MOVE: *zwm_sscc_aux-btvol TO l_remessa-volume,

              *zwm_sscc_aux-brgew TO l_remessa-peso.
*

        CLEAR l_remessa-subtotalpal.

****************************PASSAR DADOS*****************************
        APPEND l_remessa TO tb_remessa.
*********************************************************************


        READ TABLE tab_subtotal INDEX g_index.
      ENDWHILE.
*

    ENDON.

    zwm_folha_cargas-flag_material = 'X'.

    PERFORM unidade_he USING tab_out-matnr
                             tab_out-lfimg
                             tab_out-vrkme.
**************************************************
    MOVE: tab_out-matnr TO l_material-matnr,
          tab_out-remessa TO l_material-remessa,
          makt-maktx TO l_material-descricao,
          tab_out-lfimg TO l_material-qnt_enc,
          tab_out-vrkme TO l_material-un_enc,
          l_qtd_he TO l_material-qnt_vend ,
          l_un_he TO l_material-un_vend.

    APPEND l_material TO tb_material.
    CLEAR l_material.


    MOVE tab_out-remessa TO lv_remessa.
    AT END OF ordem.

      "Merchandize....
      lv_first = 'X'.
      LOOP AT tab_out_merch WHERE ordem = tab_out-ordem. "remessa = lv_remessa. RSOUSA
        CLEAR: tab_out, makt, l_un_he, l_qtd_he.
        IF lv_first = 'X'.

          MOVE: '----MERCHANDISING----' TO l_material-descricao,
          tab_out_merch-remessa TO l_material-remessa.
          APPEND l_material TO  tb_material.
          CLEAR: lv_first.
        ENDIF.
        MOVE-CORRESPONDING tab_out_merch TO tab_out.
        MOVE tab_out_merch-txt_pal TO makt-maktx.
        CLEAR: tab_out-txt_pal.
*********************Não usado*************************
* Adiciona o Merchandize como material
        MOVE: tab_out-matnr TO l_material-matnr,
              tab_out-remessa TO l_material-remessa,
              makt-maktx TO l_material-descricao,
              tab_out-lfimg TO l_material-qnt_enc,
              tab_out-vrkme TO l_material-un_enc,
              l_qtd_he TO l_material-qnt_vend ,
              l_un_he TO l_material-un_vend.

        APPEND l_material TO  tb_material.

**************************************************

      ENDLOOP.

      SORT ft_sscc.
      DELETE ADJACENT DUPLICATES FROM ft_sscc.
      DESCRIBE TABLE ft_sscc LINES l_pal.
*****************************************************

      READ TABLE tb_remessa WITH KEY remessa = lv_remessa INTO l_remessa.
      IF sy-subrc = 0.
        l_remessa-subtotalpal = l_pal.
        MODIFY tb_remessa INDEX sy-tabix FROM l_remessa TRANSPORTING subtotalpal.
      ENDIF.


      REFRESH ft_sscc.
      CLEAR:  ft_sscc, l_pal.
    ENDAT.

  ENDLOOP.

  MOVE tb_remessa[] TO remessa_tab.
  MOVE tb_material[] TO material_tab.


  z_pesototal  =  zwm_sscc_aux-brgew.
  z_volumetotal = zwm_sscc_aux-btvol.
  z_totalpalete = l_tot_pal.
  z_matricula = vttk-signi.
  z_nometrans  = lfa1-name1.
  z_codtrans = vttk-tdlnr.
  z_transporte = vttk-tknum.
  z_numporta = zwm_sscc_aux-desc_porta.
  z_numnota  = vbsk-sammg.
  z_talao = talao.




*
***************************Impressão ADOBE*****************************
********************** Adobe PDF Form data************************
*"DATA I_DATA TYPE PPT_FORM_IID. "structure use in pdf interface
  DATA fm_name  TYPE rs38l_fnam.
  DATA fp_docparams      TYPE sfpdocparams.
  DATA fp_outputparams   TYPE sfpoutputparams.
  DATA error_string      TYPE string.
  DATA l_formname        TYPE fpname VALUE 'ZWMNONTAENTREGAFORM'.
********************* End of Adobe PDF Form data ****************


  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = l_formname
    IMPORTING
      e_funcname = fm_name.

** definição da impressora
  fp_outputparams-reqimm = 'X'. ""saida imediata.
*  fp_outputparams-copies = '1'.
  fp_outputparams-reqdel = 'X'.
  fp_outputparams-reqnew = 'X'.
  fp_outputparams-device = 'PRINTER'.
  fp_outputparams-dest   = 'CD35'.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = fp_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN OTHERS.
    ENDCASE.                           " CASE sy-subrc
  ENDIF.

  fp_docparams-langu   = 'E'.
  fp_docparams-country = 'US'.


  CALL FUNCTION fm_name
    EXPORTING
      /1bcdwb/docparams = fp_docparams
      p_pesototal       = z_pesototal
      p_volumetotal     = z_volumetotal
      p_totalpalete     = z_totalpalete
      p_matricula       = z_matricula
      p_nometrans       = z_nometrans
      p_codtrans        = z_codtrans
      p_transporte      = z_transporte
      p_numporta        = z_numporta
      p_numnota         = z_numnota
      p_talao           = z_talao
      nota_tb           = remessa_tab
      material_tb       = material_tab
    EXCEPTIONS
      usage_error       = 1
      system_error      = 2
      internal_error    = 3.

  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN OTHERS.
    ENDCASE.                           " CASE sy-subrc
  ENDIF.                               " IF sy-subrc <> 0


ENDFORM.                    " FORM_ADOBEPRIMT
