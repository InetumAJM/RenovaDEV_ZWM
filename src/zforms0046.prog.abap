*&---------------------------------------------------------------------*
*&  Include           ZFORMS0046                                       *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dados .

 SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zwmlog02 FROM  zwmlog02
        WHERE sammg = p_sammg
        AND   vbeln IN s_vbeln.


  IF NOT it_zwmlog02[] IS INITIAL.
    LOOP AT it_zwmlog02.

      MOVE-CORRESPONDING it_zwmlog02 TO itab.
      APPEND itab.

    ENDLOOP.
  ELSE.
    MESSAGE i000(zwmmsg001) WITH 'Grupo sem dados a processar'.
*   & & & &

  ENDIF.

ENDFORM.                    " get_dados
*&---------------------------------------------------------------------*
*&      Form  trata_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trata_dados .

  PERFORM layout CHANGING layout.

  PERFORM fieldcatalog.


  IF NOT it_zwmlog02[] IS INITIAL.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
       i_callback_program                = repid
       i_callback_pf_status_set          = 'PFSTATUS'
       i_callback_user_command           = 'HOTSPOT_ACTIVO'
       i_callback_top_of_page            = 'TOP_OF_PAGE'
       is_layout                         = layout
       it_fieldcat                       = it_fieldcat
      TABLES
        t_outtab                          = it_zwmlog02
*     EXCEPTIONS
*       PROGRAM_ERROR                     = 1
*       OTHERS                            = 2
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDFORM.                    " trata_dados
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LAYOUT  text
*----------------------------------------------------------------------*
FORM layout  CHANGING p_layout  TYPE slis_layout_alv.
* Layout
  p_layout-get_selinfos        = 'X'.
*  p_layout-key_hotspot         = 'X'.
  p_layout-totals_before_items = ' '.
  p_layout-group_change_edit   = 'X'.
  p_layout-detail_popup        = 'X'.
  p_layout-zebra               = 'X'.
  p_layout-box_fieldname       = 'MARCA'.
  p_layout-box_tabname         = 'ITAB'.


  CLEAR : repid, reprepid. reprepid = sy-repid. repid = sy-repid.


ENDFORM.                    " LAYOUT

*&---------------------------------------------------------------------*
*&      Form  STATUS
*&---------------------------------------------------------------------*
FORM pfstatus USING lt_extab.

  SET PF-STATUS 'ZST0046'.

ENDFORM.                    "pfstatus

*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  STATICS: lt_list_commentary TYPE slis_t_listheader,
           ls_list_commentary TYPE slis_listheader.

  DATA: l_datum(10),
        l_linhas LIKE sy-tabix,
        l_uzeit(8).

  FREE: lt_list_commentary.
  CLEAR: lt_list_commentary.

*  DESCRIBE TABLE itab LINES l_linhas.
  SELECT SINGLE datfm INTO usr01-datfm FROM  usr01
         WHERE  bname  = sy-uname.

  CASE usr01-datfm.
    WHEN '1' OR '2' OR '3'.
      WRITE sy-datum TO l_datum USING EDIT MASK '__.__.____'.
    WHEN OTHERS.
      WRITE sy-datum TO l_datum USING EDIT MASK '____.__.__'.
  ENDCASE.

  WRITE sy-uzeit TO l_uzeit USING EDIT MASK '__:__:__'.

* Título
  ls_list_commentary-typ  = 'H'.
  ls_list_commentary-info = text-002.
  APPEND ls_list_commentary TO lt_list_commentary.

* Informações adicionais
  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-004.
  ls_list_commentary-info = sy-uname.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-005.
  ls_list_commentary-info = l_datum.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-006.
  ls_list_commentary-info = l_uzeit.
  APPEND ls_list_commentary TO lt_list_commentary.

*  ls_list_commentary-typ  = 'S'.
*  ls_list_commentary-key  = text-007.
*  ls_list_commentary-info = l_linhas.
*  APPEND ls_list_commentary TO lt_list_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_list_commentary
      i_logo             = 'RENOVA_LOGO'.

ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT
*&---------------------------------------------------------------------*
FORM hotspot_activo USING r_ucomm
                 CHANGING ls_selfield TYPE slis_selfield.

***  INCLUDE zbci_eventos_alv.

  PERFORM eventos USING ls_selfield r_ucomm.

  DATA: l_idx   LIKE sy-tabix,
        l_tabix LIKE sy-tabix.

  CLEAR: it_zwmlog02, it_zwmlog02[].

  LOOP AT itab WHERE marca = 'X'.
    MOVE-CORRESPONDING itab TO it_zwmlog02.
    APPEND it_zwmlog02. CLEAR it_zwmlog02.
  ENDLOOP.

  IF sy-subrc NE 0.
    LOOP AT itab.
      MOVE-CORRESPONDING itab TO it_zwmlog02.
      APPEND it_zwmlog02. CLEAR it_zwmlog02.
    ENDLOOP.
  ENDIF.

  CASE r_ucomm.
    WHEN '&IC1'.
*      CHECK ls_selfield-fieldname = 'AUFNR'.
*      READ TABLE itab INDEX ls_selfield-tabindex.
*      CHECK sy-subrc EQ 0.
*      CHECK NOT itab-aufnr IS INITIAL.
*      SET PARAMETER ID 'ANR' FIELD itab-aufnr.
*      CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN.

    WHEN 'PROC'.

      PERFORM reprocesso.

    WHEN 'BACK'.
*      PERFORM actualiza_zwm030 USING 'B'.
*      PERFORM get_dados.

    WHEN 'EXIT'.
*      PERFORM actualiza_zwm030 USING 'D'.
*      PERFORM get_dados.

  ENDCASE.

  ls_selfield-refresh = 'X'.

ENDFORM.                    "hotspot_activo
*&---------------------------------------------------------------------*
*&      Form  eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eventos USING ls_selfield TYPE slis_selfield r_ucomm.

  DATA: zwmlog02_del LIKE zwmlog02 OCCURS 0 WITH HEADER LINE,
        itab_aux     LIKE itab     OCCURS 0 WITH HEADER LINE.

  STATICS: ibloco LIKE sy-tabix,
           fbloco LIKE sy-tabix,
           aux_text(60) TYPE c,
           nreg LIKE sy-tabix.


  IF r_ucomm NE 'BLOC'.

    CLEAR : itab[], itab.

    itab[] = it_zwmlog02[].

  ENDIF.

  READ TABLE itab INDEX ls_selfield-tabindex.

  IF r_ucomm = '&IC1' AND
    ls_selfield-fieldname = 'MARCA'.

    ls_selfield-value = ' '.
    ls_selfield-refresh = 'X'.

    ls_selfield-col_stable = '3'.
    ls_selfield-row_stable = '1'.

    IF itab-marca IS INITIAL.
      itab-marca = 'X'.
    ELSE.
      itab-marca = ' '.
    ENDIF.
    MODIFY itab INDEX ls_selfield-tabindex
      FROM itab.
  ENDIF.

  IF r_ucomm = 'TUDO'.
    LOOP AT itab.
      idx = sy-tabix.
      itab-marca = 'X'.
      MODIFY itab INDEX idx.
    ENDLOOP.
    ls_selfield-value = ' '.
    ls_selfield-refresh = 'X'.
    ls_selfield-col_stable = '3'.
    ls_selfield-row_stable = '1'.
  ENDIF.

  IF r_ucomm = 'NADA'.
    LOOP AT itab.
      idx = sy-tabix.
      itab-marca = ' '.
      MODIFY itab INDEX idx.
    ENDLOOP.
    ls_selfield-value = ' '.
    ls_selfield-refresh = 'X'.
    ls_selfield-col_stable = '3'.
    ls_selfield-row_stable = '1'.
  ENDIF.

  IF r_ucomm = 'BLOC'.
* marcar inicio do bloco
    IF ibloco IS INITIAL.
      ibloco = ls_selfield-tabindex.
*      MESSAGE s000(zmsg) WITH text-020 text-022.
      ls_selfield-value = ' '.
      ls_selfield-refresh = 'X'.
      ls_selfield-col_stable = '3'.
      ls_selfield-row_stable = '1'.

    ELSE.
      fbloco = ls_selfield-tabindex.
* troca limites se for caso disso
      IF ibloco > fbloco.
        idx = ibloco.
        ibloco = fbloco.
        fbloco = idx.
      ENDIF.
      LOOP AT itab FROM ibloco TO fbloco.
        idx = sy-tabix.
        itab-marca = 'X'.
        MODIFY itab INDEX idx.
      ENDLOOP.

      nreg = fbloco - ibloco + 1.

*    aux_text = nreg.
*    CONDENSE aux_text.
*    CONCATENATE aux_text text-021 INTO aux_text SEPARATED BY space.
*    MESSAGE s000(zmsg) WITH aux_text.

      ls_selfield-value = ' '.
      ls_selfield-refresh = 'X'.
      ls_selfield-col_stable = '3'.
      ls_selfield-row_stable = '1'.
      CLEAR: ibloco, fbloco.

    ENDIF.
  ENDIF.

  IF r_ucomm = 'DELE'.

** Tabela dicionário
    CLEAR:   zwmlog02_del, itab_aux.
    REFRESH: zwmlog02_del, itab_aux.

    itab_aux[] = itab[].
    DELETE itab_aux WHERE marca NE 'X'.

    SELECT * FROM zwmlog02 INTO TABLE zwmlog02_del
       FOR ALL ENTRIES IN itab_aux
       WHERE zztpdoc EQ itab_aux-zztpdoc
         AND sammg   EQ itab_aux-sammg
         AND vbeln   EQ itab_aux-vbeln
         AND posnr   EQ itab_aux-posnr.

    DELETE zwmlog02 FROM TABLE zwmlog02_del.

** Tabela interna
    DELETE itab WHERE marca = 'X'.
    DESCRIBE TABLE itab LINES idx.
    IF idx = 0.
      MESSAGE s000(zmsg) WITH 'Registos eliminados com sucesso'.
      SET SCREEN 0.
    ENDIF.

    ls_selfield-value      = ' '.
    ls_selfield-refresh    = 'X'.
    ls_selfield-col_stable = '3'.
    ls_selfield-row_stable = '1'.

  ENDIF.
ENDFORM.                    " eventos
*&---------------------------------------------------------------------*
*&      Form  REPROCESSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reprocesso .
*-------------------------------------------------Reposicão de Items
  PERFORM repor_item_remessa.
*-------------------------------------------------Gravar Qtd no item
  PERFORM gravar_qtd.
*-------------------------------------------------Reposicão de OT
  PERFORM get_ot.
  PERFORM cria_ot.
*-------------------------------------------------Reposicão de HU
  PERFORM associa_hu.
*-------------------------------------------------Reposição de Creditos
  PERFORM associa_creditos.

ENDFORM.                    " REPROCESSO
*&---------------------------------------------------------------------*
*&      Form  REPOE_ITEM_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM repor_item_remessa .
*sempre que não for possivel copiar os items da encomenda, vamos repor
*a situação inicial da remessa.

*----------------------------------------------------------------------
*----------------------------------------------------------------------
  DATA : l_posnr LIKE lips-posnr, zx_modo VALUE 'A', l_data(8),
         wa_zwmlog02 LIKE zwmlog02.

  CLEAR: l_posnr, bdcdata, bdcdata[], l_data.


*-----------------------------------------------------------------
  LOOP AT it_zwmlog02 WHERE sammg = p_sammg
                      AND   vbeln IN s_vbeln
                      AND   zztpdoc = 'REM_COPI'.

    IF it_zwmlog02-pstyv   = 'TAN'.

      CLEAR: l_posnr, wa_zwmlog02.
      l_posnr = it_zwmlog02-posnr + 1.

      READ TABLE it_zwmlog02 WITH KEY sammg = it_zwmlog02-sammg
                                      vbeln = it_zwmlog02-vbeln
                                      posnr = l_posnr
                                  INTO wa_zwmlog02.

      CLEAR likp.

      SELECT SINGLE wadat INTO likp-wadat FROM  likp
             WHERE  vbeln  = it_zwmlog02-vbeln.

      WRITE likp-wadat TO l_data  DDMMYY.

      PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
      PERFORM bdc_field USING 'LIKP-VBELN'            it_zwmlog02-vbeln.
*  '2299210985'.

      PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '=RAUF_T'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'LIPS-MATNR(01)'.

      PERFORM bdc_dynpro   USING 'SAPMV50A'            '0105'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'LV50C-BIPOS'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '=ENT1'.
    PERFORM bdc_field    USING 'LV50C-VBELN'      it_zwmlog02-vbeln_enc.
*  '1299195522'.
*----------------------------------------------------------------------
      PERFORM bdc_field    USING 'LV50C-DATBI'         l_data.

*----------------------------------------------------------------------
      PERFORM bdc_field    USING 'LV50C-ABPOS'        it_zwmlog02-posnr.
*'000010'.
*-----------------------------------se o item de oferta for gt 0, copia
      IF  wa_zwmlog02-lfimg GT 0.
        PERFORM bdc_field    USING 'LV50C-BIPOS'         l_posnr.
      ELSE.
        PERFORM bdc_field    USING 'LV50C-BIPOS'     it_zwmlog02-posnr.

      ENDIF.
*----------------------------------------------------------------------

    ENDIF.

*-----------------------------------DEPOIS DE COPIAR ITEMS MODIFICA QTD
    PERFORM modifica_qtd.

*'000011'.
    IF wa_zwmlog02-posnr = it_zwmlog02-posnr.

      PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '=SICH_T'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'LIPS-MATNR(03)'.

      CLEAR : it_msg, it_msg[].

*---------------------------------------Verifica se a remessa está
*bloqueada
      g_bloq = 2.
      WHILE g_bloq = 2.
        PERFORM gr_bloqueado USING 'REM_COPI' CHANGING g_bloq.
      ENDWHILE.


      CALL TRANSACTION 'VL02N' USING bdcdata
                              UPDATE 's'
                              MODE zx_modo
                              MESSAGES INTO it_msg.
      CLEAR l_zztpdoc.
      l_zztpdoc = 'REM_COPI'.

      PERFORM log USING it_zwmlog02-vbeln it_zwmlog02-posnr .

      IF it_msg-msgtyp = 'S'.
        COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
        DELETE zwmlog02 FROM it_zwmlog02.
      ENDIF.

    ENDIF.


  ENDLOOP.
*----------------------------------------------------------------------
*----------------------------------------------------------------------

*  CLEAR : delivery_items, delivery_items[].

*--------------------------------------------------------------seleccion
*  LOOP AT it_zwmlog02 WHERE zztpdoc(3) = 'REM'.
*
*    SELECT * INTO CORRESPONDING FIELDS OF delivery_items
*                    FROM zwmlipslog
*           WHERE  vbeln  = it_zwmlog02-vbeln
*           AND    posnr  = it_zwmlog02-posnr.
*
*      IF sy-subrc = 0.
*        APPEND delivery_items.
*      ENDIF.
*
*    ENDSELECT.
*
*  ENDLOOP.
*
*
*  CHECK NOT delivery_items[] IS INITIAL.
*
*  READ TABLE delivery_items INDEX 1.
*
**---------------------------------Verifica se a remessa está bloqueada
*  g_bloq = 2.
*  WHILE g_bloq = 2.
*    PERFORM gr_bloqueado USING 'REM_REPO' CHANGING g_bloq.
*  ENDWHILE.
*
*  DATA: it_create LIKE leshp_item_data_to_create
*                OCCURS 0 WITH HEADER LINE,
*        l_ef_error_any TYPE  xfeld,
*        l_ef_error_in_interface TYPE  xfeld,
*        l_ef_error_in_final_check TYPE  xfeld.
*
*
*  LOOP AT delivery_items.
*    MOVE-CORRESPONDING delivery_items TO it_create.
*    APPEND it_create. CLEAR it_create.
*  ENDLOOP.
*
*  CALL FUNCTION 'WS_DELIVERY_UPDATE_TO_CREATE'
*   EXPORTING
**   VBKOK_WA                           =
**   SYNCHRON                           = ' '
**   NO_MESSAGES_UPDATE_1               = ' '
*     UPDATE_PICKING                     = con_x
*     commit                             = con_x
*     delivery                           = delivery_items-vbeln
*   IMPORTING
*    EF_ERROR_ANY                       = l_ef_error_any
*    EF_ERROR_IN_INTERFACE              = l_ef_error_in_interface
*    EF_ERROR_IN_FINAL_CHECK            = l_ef_error_in_final_check
*   TABLES
*      it_item_to_create                  = it_create
*      prot                               = prot
**   VBPOK_TAB                          =
*            .


*  CALL FUNCTION 'LE_MOB_DELIVERY_UPDATE'
*    EXPORTING
*      do_commit                = con_x
*    TABLES
*      t_delivery_items         = delivery_items
*      prot                     = prot
*    EXCEPTIONS
*      conversion_overflow      = 1
*      essential_data_missing   = 2
*      error                    = 3
*      nothing_to_update        = 4
*      lock_after_update_failed = 5
*      error_in_delivery_update = 6
*      OTHERS                   = 7.
*

ENDFORM.                    " REPOE_ITEM_REMESSA
*&---------------------------------------------------------------------*
*&      Form  CRIA_OT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_ot .

  DATA : aa_modo VALUE 'N', l_anfme(17), campo(30), l_pos(2) TYPE n,
         c_anfme(30), c_vltyp(30), c_vlpla(30), c_charg(30),
         l_tabix LIKE sy-tabix, l_idx(2), l_num TYPE i.

*---------------------------------Verifica se a remessa está bloqueada
*  g_bloq = 2.
*  WHILE g_bloq = 2.
*    PERFORM gr_bloqueado USING 'REM_REPO' CHANGING g_bloq.
*  ENDWHILE.

  CLEAR : bdcdata, bdcdata[], campo, l_idx.

  SORT bi_tab BY posnr ASCENDING.

  LOOP AT bi_tab.

    CLEAR: l_tabix, campo, l_anfme, c_anfme, c_vltyp, c_vlpla, c_charg.

    l_tabix = sy-tabix.

    AT NEW posnr.

      CLEAR : bdcdata, bdcdata[], campo, l_idx.

      ADD 1 TO l_idx.

      READ TABLE  bi_tab INDEX l_tabix. CLEAR : l_num, l_pos.
*------------------------------------Ecram inicial
      PERFORM bdc_dynpro   USING 'SAPML03T'            '0151'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          '*VBLKP-POSNR'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
      PERFORM bdc_field    USING 'LTAK-LGNUM'          '100'.
      PERFORM bdc_field    USING 'VBLKK-VBELN'         bi_tab-vbeln.
      PERFORM bdc_field    USING '*VBLKP-POSNR'        bi_tab-posnr.
      PERFORM bdc_field    USING 'RL03T-ALAKT'         'X'.

    ENDAT.

    ADD 1 TO l_num.
    ADD 1 TO l_pos.

    CONCATENATE 'LTAPA-CHARG(' l_pos ')' INTO campo.
    CONDENSE campo NO-GAPS.

    CONCATENATE 'LTAPA-ANFME(' l_pos ')' INTO c_anfme.
    CONDENSE c_anfme NO-GAPS.

    CONCATENATE 'LTAPA-VLTYP(' l_pos ')' INTO c_vltyp.
    CONDENSE c_anfme NO-GAPS.

    CONCATENATE 'LTAPA-VLPLA(' l_pos ')' INTO c_vlpla.
    CONDENSE c_anfme NO-GAPS.

    CONCATENATE 'LTAPA-CHARG(' l_pos ')' INTO c_charg.
    CONDENSE c_anfme NO-GAPS.



    WRITE bi_tab-lfimg TO l_anfme UNIT 'UN'.

    PERFORM bdc_dynpro   USING 'SAPML03T'            '0105'.
    PERFORM bdc_field    USING 'BDC_CURSOR'          campo.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
    PERFORM bdc_field    USING 'T334T-LGTY0'         bi_tab-lgtyp.
    PERFORM bdc_field    USING 'T334T-LGTY1'         ''.
    PERFORM bdc_field    USING 'T334T-LGTY2'         ''.
    PERFORM bdc_field    USING 'T334T-LGTY3'         ''.
    PERFORM bdc_field    USING 'T334T-LGTY4'         ''.
    PERFORM bdc_field    USING  c_anfme              l_anfme.
    PERFORM bdc_field    USING  c_vltyp              bi_tab-lgtyp.
    PERFORM bdc_field    USING  c_vlpla              bi_tab-vbeln.
    PERFORM bdc_field    USING  c_charg              bi_tab-charg.
*-------------------ver necessidade da tela
    PERFORM bdc_dynpro   USING 'SAPML03T'            '0105'.
    PERFORM bdc_field    USING 'BDC_CURSOR'          campo.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.

    AT END OF posnr.

      READ TABLE  bi_tab INDEX l_tabix.

      PERFORM bdc_dynpro   USING 'SAPML03T'            '0105'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'T334T-LGTY0'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '=TAH2'.

*-------------------------------nº de confirmações = nº de lotes usado
      DO l_num TIMES.

        PERFORM bdc_dynpro   USING 'SAPML03T'            '0102'.
        PERFORM bdc_field    USING 'BDC_CURSOR'          'RL03T-SQUIT'.
        PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
        PERFORM bdc_field    USING 'RL03T-SQUIT'         'X'.
        PERFORM bdc_field    USING 'LTAP-VLPLA'          bi_tab-vbeln.
*-------------------ver necessidade da tela
*        PERFORM bdc_dynpro   USING 'SAPML03T'            '0102'.
*        PERFORM bdc_field    USING 'BDC_CURSOR'          'RL03T-SQUIT'.
*        PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.

      ENDDO.

      PERFORM bdc_dynpro   USING 'SAPML03T'            '0105'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'T334T-LGTY0'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '=BU'.
      PERFORM bdc_field    USING 'T334T-LGTY0'         '916'.

      CLEAR : it_msg, it_msg[].

      CALL TRANSACTION 'LT03' USING bdcdata
                              UPDATE 's'
                              MODE aa_modo
                              MESSAGES INTO it_msg.


      PERFORM log USING bi_tab-vbeln bi_tab-posnr.

      IF it_msg-msgtyp = 'S'.
        COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
      ENDIF.

      CLEAR : it_msg, it_msg[].

    ENDAT.

  ENDLOOP.


ENDFORM.                    " CRIA_OT
*&---------------------------------------------------------------------*
*&      Form  ASSOCIA_HU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM associa_hu .

  LOOP AT it_zwmlog02 WHERE sammg = p_sammg
                      AND   vbeln IN s_vbeln
                      AND   zztpdoc(2) = 'HU'.


    MOVE-CORRESPONDING it_zwmlog02 TO it_hu.

    CLEAR: it_msg, it_msg[].

    CALL FUNCTION 'ZWM_EMBALAR_HU'
      EXPORTING
        warehouse                  = it_hu-lgnum   "l_warehouse
        hu                         = it_hu-hu      "l_hu
        vbeln                      = it_hu-vbeln   "l_vbeln
      TABLES
        return_msg                 = it_msg
      EXCEPTIONS
        empty_table                = 1
        reference_document_differs = 2
        empty_delivery_item        = 3
        item_not_found             = 4
        OTHERS                     = 6.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO zdummy.


    ENDIF.

    PERFORM log USING it_hu-hu ' '.

    IF it_msg-msgtyp = 'S'.
      COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
    ENDIF.

    CLEAR : it_msg, it_msg[].

  ENDLOOP.

ENDFORM.                    " ASSOCIA_HU
*&---------------------------------------------------------------------*
*&      Form  gr_bloqueado
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0618   text
*      <--P_G_BLOQ  text
*----------------------------------------------------------------------*
FORM gr_bloqueado USING p_momento  CHANGING p_bloq.

  DATA :  l_enq LIKE seqg3 OCCURS 0 WITH HEADER LINE,
          l_gname LIKE seqg3-gname VALUE 'LIKP',  l_dummy(220),
          l_garg LIKE seqg3-garg.


*  CONCATENATE sy-mandt tabi_guias-vbeln INTO l_garg.
  CONCATENATE sy-mandt it_zwmlog02-vbeln INTO l_garg.


  CONDENSE l_garg NO-GAPS.

  CLEAR: l_enq, l_enq[].

  CALL FUNCTION 'ENQUE_READ'
    EXPORTING
      gclient       = sy-mandt
      gname         = l_gname
      garg          = l_garg
*   GUNAME        = SY-UNAME
* IMPORTING
*   NUMBER        =
*   SUBRC         =
    TABLES
      enq           = l_enq
            .

  READ TABLE l_enq INDEX 1.

  IF sy-subrc = 0.
    p_bloq = 2.
    CONCATENATE 'Fornecimento bloqueado' it_zwmlog02-vbeln INTO
    l_dummy SEPARATED BY space.
  ELSEIF sy-subrc NE 0.
    CLEAR p_bloq.
    CONCATENATE 'Fornecimento desbloqueado' it_zwmlog02-vbeln
    INTO l_dummy SEPARATED BY space.
  ENDIF.

  CONCATENATE l_dummy p_momento INTO l_dummy SEPARATED BY space.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = l_dummy.

ENDFORM.                    " gr_bloqueado

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  get_ot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ot .

  LOOP AT it_zwmlog02 WHERE sammg = p_sammg
                      AND   vbeln IN s_vbeln
                      AND   zztpdoc(2) = 'OT'.

    MOVE-CORRESPONDING it_zwmlog02 TO bi_tab.
    APPEND bi_tab.
  ENDLOOP.

ENDFORM.                    " get_ot
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcatalog .

  DATA l_fieldcat TYPE slis_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
   EXPORTING
     i_program_name               = repid
     i_internal_tabname           = 'ITAB'
*   I_STRUCTURE_NAME             =
*   I_CLIENT_NEVER_DISPLAY       = 'X'
     i_inclname                   = repid
*   I_BYPASSING_BUFFER           =
*   I_BUFFER_ACTIVE              =
    CHANGING
      ct_fieldcat                  = it_fieldcat
* EXCEPTIONS
*   INCONSISTENT_INTERFACE       = 1
*   PROGRAM_ERROR                = 2
*   OTHERS                       = 3
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZWMlog02_VBELN  text
*----------------------------------------------------------------------*
FORM log  USING p_docu p_item.
  IF it_msg[] IS INITIAL.
    PERFORM act_it_msg.
    PERFORM log_processo USING it_msg-msgtyp l_zztpdoc p_docu p_item.
  ELSE.

    SORT it_msg BY msgtyp ASCENDING.
    LOOP AT it_msg WHERE msgtyp = 'E' OR msgtyp = 'A' OR  msgtyp = 'S'.
*---------------------------------------------A msg de sucesso que
*interessa é a nº 311
      IF it_msg-msgtyp = 'S' AND l_zztpdoc(3) = 'REM'.
        CHECK it_msg-msgnr = '311'.
      ENDIF.

      PERFORM log_processo USING it_msg-msgtyp l_zztpdoc p_docu p_item.
      EXIT.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " log
*&---------------------------------------------------------------------*
*&      Form  act_it_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM act_it_msg .
  it_msg-msgtyp = sy-msgty.
  it_msg-msgid  = sy-msgid.
  it_msg-msgnr  = sy-msgno.
  it_msg-msgv1  = sy-msgv1.
  it_msg-msgv2  = sy-msgv2.
  it_msg-msgv3  = sy-msgv3.
  it_msg-msgv4  = sy-msgv4.

ENDFORM.                    " act_it_msg
*&---------------------------------------------------------------------*
*&      Form  log_processo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MSG_MSGTYP  text
*      -->P_L_ZZTPDOC  text
*      -->P_P_DOCU  text
*----------------------------------------------------------------------*
FORM log_processo  USING    p_msgtyp
                            value(p_evento)
                            p_doc
                            p_item.

  DATA: l_texto LIKE t100-text.

  CLEAR: l_texto.

*------------------------------NAS OT o nº criado vem no campo
*it_msg-msgv1
  IF p_evento(2) = 'OT'.
    p_doc = it_msg-msgv1.
  ENDIF.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = it_msg-msgid
      msgnr               = it_msg-msgnr
      msgv1               = it_msg-msgv1
      msgv2               = it_msg-msgv2
      msgv3               = it_msg-msgv3
      msgv4               = it_msg-msgv4
    IMPORTING
      message_text_output = l_texto.

  IF NOT it_zwmlog02-vbeln IS INITIAL.
    zwmlog-vbeln = it_zwmlog02-vbeln.
  ELSE.
    zwmlog-vbeln = p_doc.
  ENDIF.

  zwmlog-sammg   = p_sammg.
  zwmlog-zztpdoc = p_evento.
  zwmlog-zzdoc   = p_doc.
  zwmlog-vgpos   = p_item.
  zwmlog-zztpmsg = p_msgtyp.
  zwmlog-zzmsg   = l_texto.
  zwmlog-uname   = sy-uname.
  zwmlog-datum   = sy-datum.
  zwmlog-uzeit   = sy-uzeit.

  MODIFY zwmlog.
*----------------------------------------------------------------------

  zwmlog02-vbeln = it_zwmlog02-vbeln.

  zwmlog02-sammg   = p_sammg.
  zwmlog02-zztpdoc = p_evento.
*  zwmlog02-zzdoc   = p_doc.
  zwmlog02-posnr   = p_item.
  zwmlog02-vgpos   = p_item.
*  zwmlog02-uname   = sy-uname.
*  zwmlog02-datum   = sy-datum.
*  zwmlog02-uzeit   = sy-uzeit.

  MODIFY zwmlog02.

ENDFORM.                    " log_processo
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_QTD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modifica_qtd .

  CLEAR : g_qtd.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
  EXPORTING
    input                = it_zwmlog02-vrkme
    language             = sy-langu
  IMPORTING
*   LONG_TEXT            =
    output               = it_zwmlog02-vrkme
*   SHORT_TEXT           =
  EXCEPTIONS
    unit_not_found       = 1
    OTHERS               = 2
          .


  WRITE it_zwmlog02-lfimg TO g_qtd UNIT it_zwmlog02-vrkme.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=POPO_T'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '0111'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'RV50A-POSNR'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=WEIT'.
  PERFORM bdc_field    USING 'RV50A-POSNR'         it_zwmlog02-posnr.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
  PERFORM bdc_field    USING 'LIPSD-G_LFIMG(01)'   g_qtd.
  PERFORM bdc_field    USING 'LIPS-VRKME(01)'      it_zwmlog02-vrkme.
*perform bdc_dynpro   using 'SAPMV50A'            '1000'.
*perform bdc_field    using 'BDC_OKCODE'
*                              '=POPO_T'.

ENDFORM.                    " MODIFICA_QTD
*&---------------------------------------------------------------------*
*&      Form  associa_creditos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM associa_creditos .

  DATA: BEGIN OF it_credito OCCURS 0,
          guia  LIKE likp-vbeln,
          vbeln LIKE vbap-vbeln,
          vgpos LIKE vbap-vgpos,
          kbetr(16),
          waerk LIKE z1com_s-waerk,
       END OF it_credito.

  LOOP AT it_zwmlog02 WHERE sammg = p_sammg
                      AND   vbeln IN s_vbeln
                      AND   zztpdoc(2) = 'CR'.

    MOVE-CORRESPONDING it_zwmlog02 TO it_credito.
    it_credito-vbeln = it_zwmlog02-vbeln_enc.
    it_credito-guia  = it_zwmlog02-vbeln.
    APPEND it_credito.

  ENDLOOP.

  LOOP AT it_credito.

    CLEAR : f_salesdocument, it_order_header_in, it_order_header_inx,
          it_return, it_return[], it_conditions_in, it_conditions_in[],
          it_conditions_inx, it_conditions_inx[].

    f_salesdocument =  it_credito-vbeln.

    it_order_header_in-collect_no = it_credito-vbeln.

    it_order_header_inx-collect_no = 'X'.
    it_order_header_inx-updateflag = 'U'.


    it_conditions_in-itm_number = it_credito-vgpos.
    it_conditions_in-cond_st_no = '109'.
*    it_conditions_in-cond_count = '03'.
    it_conditions_in-cond_type  = 'ZCRE'.
    it_conditions_in-cond_value = it_credito-kbetr * -1.
    it_conditions_in-currency   = it_credito-waerk.
    APPEND it_conditions_in.

    it_conditions_inx-itm_number = it_credito-vgpos.
    it_conditions_inx-cond_st_no = '109'.
*    it_conditions_inx-cond_count = '03'.
    it_conditions_inx-cond_type  = 'ZCRE'.
    it_conditions_inx-updateflag = 'I'.
    it_conditions_inx-cond_value = 'X'.
    it_conditions_inx-currency   = 'X'.
    APPEND it_conditions_inx.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument               = f_salesdocument
        order_header_in             = it_order_header_in
        order_header_inx            = it_order_header_inx
*       SIMULATION                  =
*       BEHAVE_WHEN_ERROR           = ' '
*       INT_NUMBER_ASSIGNMENT       = ' '
*       LOGIC_SWITCH                =
      TABLES
        return                      = it_return
*       ORDER_ITEM_IN               =
*       ORDER_ITEM_INX              =
*       PARTNERS                    =
*       PARTNERCHANGES              =
*       PARTNERADDRESSES            =
*       ORDER_CFGS_REF              =
*       ORDER_CFGS_INST             =
*       ORDER_CFGS_PART_OF          =
*       ORDER_CFGS_VALUE            =
*       ORDER_CFGS_BLOB             =
*       ORDER_CFGS_VK               =
*       ORDER_CFGS_REFINST          =
*       SCHEDULE_LINES              =
*       SCHEDULE_LINESX             =
*       ORDER_TEXT                  =
*       ORDER_KEYS                  =
       conditions_in               = it_conditions_in
       conditions_inx              = it_conditions_inx
*       EXTENSIONIN                 =
              .

    LOOP AT it_return WHERE type = 'E'.
      EXIT.
    ENDLOOP.

    IF sy-subrc NE 0.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait          = 'X'
*     IMPORTING
*       RETURN        =
                .
    ELSE.

      it_msg-msgtyp = it_return-type.
      it_msg-msgid  = it_return-id.
      it_msg-msgnr  = it_return-number.
      it_msg-msgv1  = it_return-message_v1.
      it_msg-msgv2  = it_return-message_v2.
      it_msg-msgv3  = it_return-message_v3.
      it_msg-msgv4  = it_return-message_v4.
      APPEND it_msg.

    ENDIF.

    PERFORM log USING it_credito-vbeln it_credito-vgpos.

    IF it_msg-msgtyp = 'S'.
      COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
    ENDIF.

    CLEAR : it_msg, it_msg[].

  ENDLOOP.


ENDFORM.                    " associa_creditos
*&---------------------------------------------------------------------*
*&      Form  gravar_qtd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gravar_qtd .
*----------------------------------------------------------------------
*----------------------------------------------------------------------
  DATA : l_posnr LIKE lips-posnr, zx_modo VALUE 'A', l_data(8),
         wa_zwmlog02 LIKE zwmlog02, l_idx LIKE sy-tabix.

  CLEAR: l_posnr, bdcdata, bdcdata[], l_data, l_idx.


*-----------------------------------------------------------------
  LOOP AT it_zwmlog02 WHERE sammg = p_sammg
                      AND   vbeln IN s_vbeln
                      AND   zztpdoc = 'REM_SAVE'.

    CLEAR l_idx. l_idx = sy-tabix.

    AT FIRST.
      READ TABLE it_zwmlog02 INDEX l_idx.

      PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
      PERFORM bdc_field USING 'LIKP-VBELN'            it_zwmlog02-vbeln.
    ENDAT.

    PERFORM modifica_qtd.

  ENDLOOP.

  READ TABLE it_zwmlog02 INDEX l_idx.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=SICH_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIPS-MATNR(03)'.


*----------------------------------Verifica se a remessa está bloqueada
  g_bloq = 2.
  WHILE g_bloq = 2.
    PERFORM gr_bloqueado USING 'REM_COPI' CHANGING g_bloq.
  ENDWHILE.

  CLEAR : it_msg, it_msg[].

  CALL TRANSACTION 'VL02N' USING bdcdata
                          UPDATE 's'
                          MODE zx_modo
                          MESSAGES INTO it_msg.
  CLEAR l_zztpdoc.
  l_zztpdoc = 'REM_SAVE'.

  PERFORM log USING it_zwmlog02-vbeln ' '.

  IF it_msg-msgtyp = 'S'.
    COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
  ENDIF.

ENDFORM.                    " gravar_qtd
