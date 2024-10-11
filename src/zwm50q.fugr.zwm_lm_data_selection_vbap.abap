function zwm_lm_data_selection_vbap.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IF_CONTROL_FLAG) TYPE  CHAR1 DEFAULT 'B'
*"  TABLES
*"      CT_POSTAB STRUCTURE  ZWMOV
*"      IT_MATNR STRUCTURE  RANGE_MAT OPTIONAL
*"      IT_SPART STRUCTURE  RANGE_C2
*"      IT_EAN11 STRUCTURE  RANGE_C18 OPTIONAL
*"      IT_CHARG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_KDMAT STRUCTURE  RANGE_C35 OPTIONAL
*"      IT_AUFNR STRUCTURE  RANGE_C12 OPTIONAL
*"      IT_WERKS STRUCTURE  RANGE_C4 OPTIONAL
*"  EXCEPTIONS
*"      NO_ITEMS_FOUND
*"----------------------------------------------------------------------

*       Global data declarations

  data: begin of lt_select occurs 10,
          vbeln like vbap-vbeln,
          posnr like vbap-posnr,
        end of lt_select,
        lt_vbap like vbap occurs 10 with header line,
        begin of lt_vbkd occurs 10,
          vbeln like vbkd-vbeln,
          posnr like vbkd-posnr,
          bstkd like vbkd-bstkd,
          bstdk like vbkd-bstdk,
        end of lt_vbkd,
        lf_select_lines type i,
        lf_ean11_lines type i,
        lf_kdmat_lines type i,
        lf_charg_lines type i,                              "46A BEY
        lf_aufnr_lines type i,              "HUM
        lf_spart_lines type i,
        lf_matnr_lines type i,
        lf_postab_tabix like sy-tabix,
        lf_postab_index like sy-tabix,
        lf_flag_items type c.

  data: lt_paletes like zpalete_picking occurs 10 with header line,
        ls_paletes like zpalete_picking.

  data: lf_lgnum like mlgn-lgnum,
        lf_index like sy-tabix.

  field-symbols: <ls_postab> type zwmov.

  describe table ct_postab lines lf_select_lines.
  if lf_select_lines eq 0.
    raise no_items_found.
  endif.

  describe table it_matnr lines lf_matnr_lines.
  describe table it_ean11 lines lf_ean11_lines.
  describe table it_kdmat lines lf_kdmat_lines.
  describe table it_charg lines lf_charg_lines.             "46A BEY
  describe table it_aufnr lines lf_aufnr_lines.             "HUM
  describe table it_spart lines lf_spart_lines.

  refresh: lt_select, lt_vbap.
  clear:   lt_select, lt_vbap.

  case if_control_flag.
    when 'A'. " 'A'dd items to all the deliveries in ct_postab

*     Extract document numbers from ct_postab
      loop at ct_postab assigning <ls_postab>
                        where posnr eq posnr_initial
                        and   not vbeln is initial.
        check <ls_postab>-vbtyp = 'C'.
        move-corresponding <ls_postab> to lt_select.
        append lt_select.
      endloop.

      select * from vbap into table lt_vbap
               for all entries in lt_select
               where vbeln eq lt_select-vbeln.
      if sy-subrc ne 0.
*        refresh ct_postab.
        raise no_items_found.
      endif.

      delete lt_vbap where not matnr in it_matnr.
      delete lt_vbap where not spart in it_spart.
      delete lt_vbap where not ean11 in it_ean11.
      delete lt_vbap where not charg in it_charg.
      delete lt_vbap where not werks in it_werks.

      sort lt_vbap by vbeln posnr.

      select vbeln posnr bstkd bstdk
             from vbkd into table lt_vbkd
                for all entries in lt_select
                    where vbeln eq lt_select-vbeln.

      sort lt_vbkd by vbeln posnr.

*     Cria tabela Paletes Completas e Incompletas
      clear lf_lgnum.
      call function 'ZWM_LER_DEPOSITO_UTILIZADOR'
        importing
          e_lgnum = lf_lgnum.

      loop at lt_vbap.
        clear lt_paletes.
        move-corresponding lt_vbap to lt_paletes.
        move lt_vbap-kwmeng to lt_paletes-lfimg.
        append lt_paletes.
      endloop.

**   Cálculo das paletes
      call function 'ZWM_PAL_PICKING_COMPLETE'
        exporting
          i_lgnum         = lf_lgnum
          i_actualiza     = ' '
        tables
          zpalete_picking = lt_paletes[].

*     Transfer information from vbap to CT_POSTAB
      loop at lt_vbap.
        read table ct_postab with key vbeln = lt_vbap-vbeln
                                      posnr = lt_vbap-posnr
                             binary search.
        lf_postab_tabix = sy-tabix.
        if sy-subrc eq 0.
          perform move_vbap_to_postab using    lt_vbap
                                      changing ct_postab.
          perform find_transp_zone    using    lt_vbap
                                      changing ct_postab.
          read table lt_vbkd with key vbeln = lt_vbap-vbeln
                                      posnr = lt_vbap-posnr
                               binary search.
          if sy-subrc = 0.
            ct_postab-bstnk = lt_vbkd-bstkd.
          endif.
          modify ct_postab index lf_postab_tabix.
        else.
          read table ct_postab with key vbeln = lt_vbap-vbeln
                                        posnr = posnr_initial
                               binary search.
*         Actualiza totais na linha de header
          if sy-subrc = 0.
            lf_postab_index = sy-tabix.
            ct_postab-brgew  = ct_postab-brgew + lt_vbap-brgew.
            ct_postab-ntgew  = ct_postab-ntgew + lt_vbap-ntgew.
            ct_postab-kwmeng = ct_postab-kwmeng + lt_vbap-kwmeng.
            ct_postab-gewei  = lt_vbap-gewei.
            ct_postab-vrkme  = lt_vbap-vrkme.
            ct_postab-vstel  = lt_vbap-vstel.
*           Calcula Paletes Completas e Incompletas
            clear: lt_paletes, ls_paletes.
            read table lt_paletes with key vbeln = lt_vbap-vbeln
                                           posnr = lt_vbap-posnr.
            if sy-subrc = 0.
*             Guarda valores
              ls_paletes-pal_completa = lt_paletes-pal_completa.
              ls_paletes-pal_incompleta = lt_paletes-pal_incompleta.
*             Acumula na tabela header
              ct_postab-pal_completa = ct_postab-pal_completa +
                                       lt_paletes-pal_completa.
              ct_postab-pal_incompleta = ct_postab-pal_incompleta +
                                         lt_paletes-pal_incompleta.
            endif.
            modify ct_postab index lf_postab_index.
          endif.
*         Actualiza totais na linha de header
          clear: ct_postab-pal_completa,
                 ct_postab-pal_incompleta.
          perform move_vbap_to_postab using    lt_vbap
                                      changing ct_postab.
          ct_postab-pal_completa = ls_paletes-pal_completa.
          ct_postab-pal_incompleta = ls_paletes-pal_incompleta.
          perform find_transp_zone    using    lt_vbap
                                      changing ct_postab.
          read table lt_vbkd with key vbeln = lt_vbap-vbeln
                                      posnr = lt_vbap-posnr
                               binary search.
          if sy-subrc = 0.
            ct_postab-bstnk = lt_vbkd-bstkd.
          endif.
          insert ct_postab index lf_postab_tabix.
        endif.
      endloop.

*     Delete entries failing the selection criteria from postab
      if  lf_matnr_lines ne 0
       or lf_spart_lines ne 0
       or lf_ean11_lines ne 0
       or lf_kdmat_lines ne 0
       or lf_charg_lines ne 0                               "46A
       or lf_aufnr_lines ne 0 .                             "46B/HUM
        sort lt_vbap by vbeln posnr.
        loop at ct_postab where posnr ne posnr_initial.
          read table lt_vbap with key vbeln = ct_postab-vbeln
                                      posnr = ct_postab-posnr
                             binary search.
          if sy-subrc ne 0.
            delete ct_postab.
          endif.
        endloop.
      endif.
*      perform delete_header_wo_item tables ct_postab.
*      describe table ct_postab lines lf_select_lines.
*      if lf_select_lines eq 0.
*        raise no_items_found.
*      endif.
*   --------------------------------------------------------------------
    when 'B'. "Add item data to items already existing
*     Extract items already existing
      loop at ct_postab assigning <ls_postab>
                        where posnr ne posnr_initial.
        move-corresponding <ls_postab> to lt_select.
        append lt_select.
      endloop.
      sort lt_select by vbeln posnr.
      select * from vbap
               appending corresponding fields of table lt_vbap
               for all entries in lt_select
               where vbeln eq lt_select-vbeln
               and   posnr eq lt_select-posnr.

      if sy-subrc ne 0.
*        refresh ct_postab.
        raise no_items_found.
      endif.

      delete lt_vbap where not matnr in it_matnr.
      delete lt_vbap where not spart in it_spart.
      delete lt_vbap where not ean11 in it_ean11.
      delete lt_vbap where not charg in it_charg.
      delete lt_vbap where not werks in it_werks.

      sort lt_vbap by vbeln posnr.

      select vbeln posnr bstkd bstdk
             from vbkd into table lt_vbkd
                for all entries in lt_select
                    where vbeln eq lt_select-vbeln.

      sort lt_vbkd by vbeln posnr.

*     Transfer data from vbap to CT_POSTAB; delete items not matching
      loop at ct_postab where posnr ne posnr_initial.
        read table lt_vbap with key vbeln = ct_postab-vbeln
                                    posnr = ct_postab-posnr
                           binary search.
        if sy-subrc eq 0.
          perform move_vbap_to_postab using    lt_vbap
                                      changing ct_postab.   "n_605460
          read table lt_vbkd with key vbeln = ct_postab-vbeln
                                      posnr = ct_postab-posnr
                               binary search.
          if sy-subrc = 0.
            ct_postab-bstnk = lt_vbkd-bstkd.
          endif.
          modify ct_postab.
        else.
          delete ct_postab.
        endif.
      endloop.
*      perform delete_header_wo_item tables ct_postab.
*      describe table ct_postab lines lf_select_lines.
*      if lf_select_lines eq 0.
*        raise no_items_found.
*      endif.
*   --------------------------------------------------------------------
    when 'C'. "'C'heck deliveries in document's table
*     Check whether CT_POSTAB contains item data
      perform item_available_check tables   ct_postab
                                   changing lf_flag_items.
      if lf_flag_items eq charx.       "item data is available
*       Extract items form CT_POSTAB
        loop at ct_postab assigning <ls_postab>
                          where posnr ne posnr_initial.
          move-corresponding <ls_postab> to lt_select.
          append lt_select.
        endloop.
        select vbeln posnr from vbap
                 appending corresponding fields of table lt_vbap
                 for all entries in lt_select
                 where vbeln eq lt_select-vbeln
                 and   posnr eq lt_select-posnr.

        if sy-subrc ne 0.
*          refresh ct_postab.
          raise no_items_found.
        endif.

        delete lt_vbap where not matnr in it_matnr.
        delete lt_vbap where not spart in it_spart.
        delete lt_vbap where not ean11 in it_ean11.
        delete lt_vbap where not charg in it_charg.
        delete lt_vbap where not werks in it_werks.

        sort lt_vbap by vbeln posnr.

*       Delete items not matching from CT_POSTAB
        loop at ct_postab where posnr ne posnr_initial.
          read table lt_vbap with key vbeln = ct_postab-vbeln
                                      posnr = ct_postab-posnr
                             binary search.
          if sy-subrc ne 0.
            delete ct_postab.
          endif.
        endloop.
*        perform delete_header_wo_item tables ct_postab.
*        describe table ct_postab lines lf_select_lines.
*        if lf_select_lines eq 0.
*          raise no_items_found.
*        endif.
      else.                            "no items available
        select vbeln posnr from vbap
                 appending corresponding fields of table lt_vbap
                 for all entries in ct_postab
                 where vbeln eq ct_postab-vbeln.

        if sy-subrc ne 0.
*          refresh ct_postab.
          raise no_items_found.
        endif.

        sort lt_vbap by vbeln posnr.

        delete lt_vbap where not matnr in it_matnr.
        delete lt_vbap where not spart in it_spart.
        delete lt_vbap where not ean11 in it_ean11.
        delete lt_vbap where not charg in it_charg.
        delete lt_vbap where not werks in it_werks.

*       Delete delivery documents with not matching items
        loop at ct_postab where not vbeln is initial.
          read table lt_vbap with key vbeln = ct_postab-vbeln
                             binary search
                             transporting no fields.
          if sy-subrc ne 0.
            delete ct_postab.
          endif.
        endloop.
        describe table ct_postab lines lf_select_lines.
        if lf_select_lines eq 0.
          raise no_items_found.
        endif.
      endif.
  endcase.

endfunction.
