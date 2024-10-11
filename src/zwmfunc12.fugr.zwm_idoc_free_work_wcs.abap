FUNCTION zwm_idoc_free_work_wcs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM OPTIONAL
*"     REFERENCE(I_VLENR) TYPE  LTAP_VLENR OPTIONAL
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(I_FREE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_ONLY_WCS) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_RESEND) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_STEP) TYPE  NUMC1 DEFAULT '1'
*"     REFERENCE(I_UNLOCK_FIRST) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_ABLAD) TYPE  ABLAD OPTIONAL
*"----------------------------------------------------------------------
  CONSTANTS: lc_free_next TYPE c VALUE 'N',
             lc_free_all  TYPE c VALUE 'A'.

  DATA: lt_ltak         TYPE TABLE OF ltak,
        lt_ltak2        TYPE TABLE OF ltak,
        lt_ltap         TYPE TABLE OF ltap,
        lt_ltak_c       TYPE TABLE OF ltak,
        lt_ltap_vb_c    TYPE TABLE OF ltap_vb,
        lt_ltapc        TYPE TABLE OF ltap_vb,
        lt_ltapc_app    TYPE TABLE OF ltap_vb,
        lt_ltakb        TYPE TABLE OF ltak,
        lt_zwm028       TYPE TABLE OF zwm028,
        lt_zwm020       TYPE TABLE OF zwm020,
        lt_zwmfrt004    TYPE TABLE OF zwmfrt004,
        lt_zwm001_zlock TYPE TABLE OF zwm001,
        lt_ordem        TYPE TABLE OF numc2,
        lt_rem_qtd_comp TYPE TABLE OF name_feld.

  DATA: ls_ltak      TYPE ltak,
        ls_ltakb     TYPE ltak,
        ls_ltap      TYPE ltap,
        ls_ltapc     TYPE ltap_vb,
        ls_zwm028    TYPE zwm028,
        ls_zwm028_h  TYPE zwm028,
        ls_zwm020    TYPE zwm020,
        ls_zwm078    TYPE zwm078,
        ls_zwmfrt004 TYPE zwmfrt004,
        ls_addin     TYPE lwmrref_add.

  DATA: lv_tabix         TYPE sytabix,
        lv_lines         TYPE sytabix,
        lv_queue1        TYPE lrf_queue,
        lv_queue2        TYPE lrf_queue,
        lv_zsyst         TYPE recvsystem VALUE 'WMSRPTA100',
        lv_lgpla_tap_rep TYPE lgpla,
        lv_activated     TYPE flag,
        lv_refnr         TYPE lvs_refnr,
        lv_ordem         TYPE n LENGTH 2,
        lv_tipo_lock     TYPE c,
        lv_next_ordem    TYPE n LENGTH 2,
        lv_free          TYPE c,
        lv_only_wcs      TYPE c,
        lv_resend        TYPE flag,
        lv_idocnum       TYPE edi_docnum,
        lv_sister        TYPE lenum,
        lv_name_feld     TYPE name_feld.

  FIELD-SYMBOLS: <ls_ltak>  TYPE ltak,
                 <ls_data>  TYPE any,
                 <lv_data>  TYPE any,
                 <lt_table> TYPE ANY TABLE,
                 <ls_ltapc> TYPE ltap_vb.

  lv_refnr    = i_refnr.
  lv_free     = i_free.
  lv_only_wcs = i_only_wcs.
  lv_resend   = i_resend.

  CHECK i_lgnum EQ '100'.

** Remontada Qts Components
***********************************************************************
  APPEND 'VSOLM' TO lt_rem_qtd_comp.
  APPEND 'VISTM' TO lt_rem_qtd_comp.
  APPEND 'VDIFM' TO lt_rem_qtd_comp.
  APPEND 'VSOLA' TO lt_rem_qtd_comp.
  APPEND 'VISTA' TO lt_rem_qtd_comp.
  APPEND 'VDIFA' TO lt_rem_qtd_comp.
  APPEND 'NSOLM' TO lt_rem_qtd_comp.
  APPEND 'NISTM' TO lt_rem_qtd_comp.
  APPEND 'NDIFM' TO lt_rem_qtd_comp.
  APPEND 'NSOLA' TO lt_rem_qtd_comp.
  APPEND 'NISTA' TO lt_rem_qtd_comp.
  APPEND 'NDIFA' TO lt_rem_qtd_comp.
  APPEND 'BRGEW' TO lt_rem_qtd_comp.
  APPEND 'VOLUM' TO lt_rem_qtd_comp.

** Retora Primeiras OT's
***********************************************************************
  DO 1 TIMES.
    IF NOT i_tanum IS INITIAL.
      SELECT SINGLE * FROM ltak
                      INTO ls_ltak
                      WHERE lgnum = i_lgnum AND
                            tanum = i_tanum.

      lv_refnr = z_wm_cl_management=>get_refnr( i_lgnum = i_lgnum is_data = ls_ltak ).

      lv_resend = abap_true.

      APPEND ls_ltak TO lt_ltak.

    ELSEIF NOT lv_refnr IS INITIAL.
*      SELECT * FROM ltak
*               INTO TABLE lt_ltak
*               WHERE lgnum = i_lgnum AND
*                     benum = lv_refnr AND
*                     betyp = 'Z' AND
*                     kquit = abap_false.

      SELECT * FROM ltak
               APPENDING TABLE lt_ltak
               WHERE lgnum = i_lgnum AND
                     refnr = lv_refnr AND
                     kquit = abap_false.
    ENDIF.

    CHECK NOT lt_ltak IS INITIAL.
    SORT lt_ltak BY tanum.
    DELETE ADJACENT DUPLICATES FROM lt_ltak COMPARING tanum.

    SELECT * FROM ltap
             INTO TABLE lt_ltap
             FOR ALL ENTRIES IN lt_ltak
             WHERE lgnum = lt_ltak-lgnum AND
                   tanum = lt_ltak-tanum.

  ENDDO.

  CHECK NOT lv_refnr IS INITIAL.

** Dados de Header
***********************************************************************
  SELECT SINGLE * FROM zwm028
                  INTO ls_zwm028_h
                  WHERE lgnum = i_lgnum AND
                        refnr = lv_refnr AND
                        remessa = ''.

  " Carga Direta
  IF ls_zwm028_h-st_dck IS NOT INITIAL AND
     ls_zwm028_h-porta IS NOT INITIAL.

    ls_zwm028_h-st_pul  = ls_zwm028_h-st_dck.
    ls_zwm028_h-pulmao1 = ls_zwm028_h-porta.
  ENDIF.

** Dados de Filtros
***********************************************************************
  CHECK NOT ls_zwm028_h-st_pul IS INITIAL.

  CASE i_step.
    WHEN '1'.
      lv_free = lc_free_all.

      IF lv_only_wcs IS INITIAL.
        CASE ls_zwm028_h-st_pul.
          WHEN 'PUA'.
            lv_only_wcs = abap_false.
          WHEN OTHERS.
            lv_only_wcs = abap_true.
        ENDCASE.
      ENDIF.
    WHEN '2'.
      IF lv_free IS INITIAL.
        CASE ls_zwm028_h-st_pul.
          WHEN 'PUA'.
            lv_free = lc_free_all.
          WHEN OTHERS.
            lv_free = lc_free_next.
        ENDCASE.
      ENDIF.


      IF lv_only_wcs IS INITIAL.
        CASE ls_zwm028_h-st_pul.
          WHEN 'PUA'.
            lv_only_wcs = abap_false.
          WHEN OTHERS.
            lv_only_wcs = abap_true.
        ENDCASE.
      ENDIF.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

** Filtros
***********************************************************************
  IF lv_only_wcs EQ abap_true.
    DELETE lt_ltap WHERE vltyp <> 'AUT'.
  ENDIF.

  " Paletes de Picking
  DELETE lt_ltap WHERE vltyp = 'PCK'.
  DELETE lt_ltap WHERE vltyp = 'PKB'.

  CHECK NOT lt_ltap IS INITIAL.

** Dados Extras
***********************************************************************
  SELECT * FROM zwm028
           INTO TABLE lt_zwm028
           WHERE lgnum = i_lgnum AND
                 refnr = lv_refnr AND
                 remessa <> ''.

** Determina Proximo a Ser Desbloqueado
***********************************************************************
  DO 1 TIMES.
    SORT lt_zwm028 BY ordem.

    lv_tipo_lock = 'R'.
    lv_next_ordem = 1.

    CLEAR: lv_next_ordem.
    LOOP AT lt_zwm028 INTO ls_zwm028.
      lv_tipo_lock = ls_zwm028-tipo_lock.

      IF ls_zwm028-st_dck EQ 'DCK'.
        CHECK  ls_zwm028-total_paletes > ls_zwm028-paletes_carro.
      ELSE.
        CHECK  ls_zwm028-total_paletes > ls_zwm028-paletes_pulmao.
      ENDIF.

      lv_next_ordem = ls_zwm028-ordem.
      EXIT.
    ENDLOOP.

    IF lv_next_ordem EQ '01' AND i_unlock_first EQ abap_false AND ls_zwm028-zlock EQ '1'.
      RETURN.
    ENDIF.
  ENDDO.


  SORT lt_zwm028 BY remessa.


** Envio de IDOCS
***********************************************************************
  CASE i_step.
    WHEN '1'.
**********************************************************************
** CRIAÇÂO DE TAREFAS
**********************************************************************
      LOOP AT lt_ltak INTO ls_ltak.
        LOOP AT lt_ltap INTO ls_ltap WHERE tanum = ls_ltak-tanum.
          ls_ltapc = ls_ltap.

          IF ls_ltap-vbeln IS INITIAL AND ls_ltak-betyp EQ 'L'.
            ls_ltap-vbeln = ls_ltak-benum.
          ENDIF.

          IF ls_ltap-vbeln IS INITIAL AND NOT ls_ltap-vlenr IS INITIAL.
            SELECT SINGLE remessa FROM zwm026
                                  INTO ls_ltap-vbeln
                                  WHERE armazem = ls_ltap-lgnum AND
                                        sscc = ls_ltap-vlenr.
          ENDIF.

          CLEAR ls_ltapc-lanum.

          CLEAR: ls_zwm028.
*          READ TABLE lt_zwm028
*                INTO ls_zwm028
*                WITH KEY remessa = ls_ltap-vbeln
*                BINARY SEARCH.

          CALL FUNCTION 'ZWM_CHECK_TO_EXP'
            EXPORTING
              i_lgnum   = i_lgnum
              i_refnr   = lv_refnr
              i_vbeln   = ls_ltap-vbeln
            IMPORTING
              cs_zwm028 = ls_zwm028.


          CHECK ls_zwm028 IS NOT INITIAL. "EQ 0.

          ls_ltapc-lanum = ls_zwm028-ordem.

          IF lv_free EQ lc_free_next.
            CHECK ls_zwm028-ordem EQ lv_next_ordem.
          ENDIF.

          ls_ltak-tapri = ls_zwm028-prioridade.

          IF NOT ls_ltap-orpos IS INITIAL.
            DELETE lt_ltap WHERE tanum = ls_ltap-tanum AND
                                 tapos = ls_ltap-orpos.

            DELETE lt_ltapc WHERE tanum = ls_ltap-tanum AND
                                  tapos = ls_ltap-orpos.

            ls_ltapc-tapos = ls_ltap-orpos.
          ENDIF.

          IF NOT i_ablad IS INITIAL.
            ls_ltapc-ablad = i_ablad.
          ELSEIF ls_ltapc-vltyp <> 'AUT'.
            ls_ltapc-ablad = 'MANUAL'.
          ENDIF.

          APPEND ls_ltapc TO lt_ltapc.
        ENDLOOP.

        ls_ltakb = ls_ltak.
        APPEND ls_ltakb TO lt_ltakb.
      ENDLOOP.

      SORT lt_ltapc BY vlpla DESCENDING.
      DESCRIBE TABLE lt_ltapc LINES lv_lines.

      " União de idoc de Remontadas
      LOOP AT lt_ltapc ASSIGNING <ls_ltapc>.
        CHECK z_wm_cl_management=>is_remontada( EXPORTING is_data = <ls_ltapc> is_p1 = abap_true IMPORTING e_sister = lv_sister ).
        IF NOT i_vlenr IS INITIAL AND
           lv_lines EQ 1 AND
           NOT lv_sister IS INITIAL.

          ls_ltapc = <ls_ltapc>.
          ls_ltapc-vlenr = lv_sister.
          APPEND ls_ltapc TO lt_ltapc.
          EXIT.
        ELSE.
          LOOP AT lt_ltapc INTO ls_ltapc. " procura irma
            lv_tabix = sy-tabix.

            CHECK z_wm_cl_management=>is_remontada( EXPORTING is_data = ls_ltapc is_p2 = abap_true ).
            CHECK ls_ltapc-vlenr EQ lv_sister OR
                  ls_ltapc-nlenr EQ lv_sister.

            DELETE lt_ltapc INDEX lv_tabix.

            LOOP AT lt_rem_qtd_comp INTO lv_name_feld.
              UNASSIGN <lv_data>.
              ASSIGN COMPONENT lv_name_feld OF STRUCTURE <ls_ltapc> TO <lv_data>.
              CHECK <lv_data> IS ASSIGNED.
              <lv_data> = <lv_data> * 2.
            ENDLOOP.

            CLEAR <ls_ltapc>-vlenr.
          ENDLOOP.
        ENDIF.
      ENDLOOP.


      SORT lt_ltakb BY tanum.

      LOOP AT lt_ltakb INTO ls_ltakb.
        CLEAR: lt_ltapc_app.

        LOOP AT lt_ltapc INTO ls_ltapc WHERE tanum = ls_ltakb-tanum.
          IF i_resend EQ abap_false.
            CHECK z_wm_cl_management=>is_idoc_to_send( is_data = ls_ltapc ) EQ abap_false.
          ENDIF.

          APPEND ls_ltapc TO lt_ltapc_app.
        ENDLOOP.
        CHECK NOT lt_ltapc_app IS INITIAL.

        CALL FUNCTION 'L_IDOC_CREATE_WMTOID02'
          EXPORTING
            i_zsyst = lv_zsyst
            i_ltak  = ls_ltakb
            i_varia = ''
          TABLES
            t_ltap  = lt_ltapc_app.

        COMMIT WORK.

        LOOP AT lt_ltapc_app INTO ls_ltapc.

          IF ls_ltapc-vltyp EQ 'AUT'.
            UPDATE ltap SET kzsub = abap_true
                        WHERE lgnum = ls_ltapc-lgnum AND
                              tanum = ls_ltapc-tanum AND
                              tapos = ls_ltapc-tapos.
          ENDIF.

          SELECT SINGLE *
            FROM zwm078 INTO ls_zwm078
            WHERE lgnum = ls_ltapc-lgnum
            AND   tanum = ls_ltapc-tanum
            AND   tapos = ls_ltapc-tapos.

          IF sy-subrc <> 0.
            CLEAR: ls_zwm078.
            ls_zwm078-lgnum = ls_ltapc-lgnum.
            ls_zwm078-tanum = ls_ltapc-tanum.
            ls_zwm078-tapos = ls_ltapc-tapos.
            ls_zwm078-refnr = lv_refnr.
            ls_zwm078-vbeln = ls_ltakb-vbeln.
            ls_zwm078-vltyp = ls_ltapc-vltyp.
            ls_zwm078-vlpla = ls_ltapc-vlpla.
            ls_zwm078-ordem = ls_ltapc-lanum. "ls_zwm028-ordem.

            IF ls_zwm078-vltyp <> 'AUT'.
              ls_zwm078-exp = 'MAN'.
            ELSE.
              ls_zwm078-exp = 'AUT'.
            ENDIF.

            IF ls_zwm028-st_dck IS NOT INITIAL.
              ls_zwm078-nltyp = ls_zwm028-st_dck.
            ELSE.
              ls_zwm078-nltyp = ls_zwm028-st_pul.
            ENDIF.

            ls_zwm078-erdat = sy-datum.
            ls_zwm078-erzet = sy-uzeit.
            ls_zwm078-ernam = sy-uname.

            MODIFY zwm078 FROM ls_zwm078.
            IF sy-subrc = 0.
              COMMIT WORK.
            ENDIF.
          ENDIF.

*          ENDIF.
        ENDLOOP.
      ENDLOOP.


**********************************************************************
** LIBERAÇÂO DE CARGA
**********************************************************************
    WHEN '2'.
      CLEAR: lt_ltap_vb_c, lt_ltak.

      IF lv_only_wcs EQ abap_false.
        " Desbloqueio de todo o tipo de sequencias, vai sem sequencia
        CLEAR: lt_ordem.
        APPEND '' TO lt_ordem.
      ELSEIF lv_tipo_lock EQ 'G'.
        " Desbloqueio ao Grupo somente envia um release de carga para todo o grupo
        APPEND '1' TO lt_ordem.
      ELSE.
        CHECK NOT lv_next_ordem IS INITIAL.
        CLEAR: lt_ordem.
        APPEND lv_next_ordem TO lt_ordem.
      ENDIF.

      LOOP AT lt_ordem INTO lv_ordem.
        CHECK z_wm_cl_management=>is_idoc_refnr_send( i_lgnum = i_lgnum i_refnr = lv_refnr i_ordem = lv_ordem ) EQ abap_false.

        ls_addin-zzseq = lv_ordem.

        ASSIGN ('(SAPLLIDO)COMM_IDOC_CONTROL[]') TO <lt_table>.
        IF <lt_table> IS ASSIGNED.
          CLEAR: <lt_table>.
          UNASSIGN <lt_table>.
        ENDIF.

        CALL FUNCTION 'L_IDOC_CREATE_WMRRID01'
          EXPORTING
            i_zsyst = lv_zsyst
            i_varia = ''
            i_lgnum = i_lgnum
            i_refnr = lv_refnr
            i_addin = ls_addin
          TABLES
            t_ltap  = lt_ltap_vb_c
            t_ltak  = lt_ltak_c.

        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.

        CLEAR: lv_idocnum.
        DO 1 TIMES.
          ASSIGN ('(SAPLLIDO)COMM_IDOC_CONTROL[]') TO <lt_table>.
          CHECK <lt_table> IS ASSIGNED.

          LOOP AT <lt_table> ASSIGNING <ls_data>.
            EXIT.
          ENDLOOP.
          CHECK <ls_data> IS ASSIGNED.

          ASSIGN COMPONENT 'DOCNUM' OF STRUCTURE <ls_data> TO <lv_data>.
          CHECK <lv_data> IS ASSIGNED.

          lv_idocnum = <lv_data>.
        ENDDO.

        CHECK NOT lv_idocnum IS INITIAL.

        CALL FUNCTION 'DB_COMMIT'.

        CALL FUNCTION 'EDI_DOCUMENT_DEQUEUE_LATER'
          EXPORTING
            docnum                 = lv_idocnum
            synchron               = 'X'
          EXCEPTIONS
            idoc_is_not_to_dequeue = 1
            OTHERS                 = 2.

        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.

        CASE  lv_tipo_lock.
          WHEN 'R'.
            UPDATE zwm028 SET free_idoc = lv_idocnum WHERE lgnum = i_lgnum AND
                                                           refnr = lv_refnr AND
                                                           ordem = lv_ordem.
          WHEN 'G'.
            UPDATE zwm028 SET free_idoc = lv_idocnum WHERE lgnum = i_lgnum AND
                                                           refnr = lv_refnr AND
                                                           ordem IS NOT NULL.
        ENDCASE.
      ENDLOOP.
  ENDCASE.


ENDFUNCTION.
