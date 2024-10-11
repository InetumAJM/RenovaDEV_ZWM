FUNCTION zwm_pallet_special_case_sort.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(E_PROCESSED) TYPE  FLAG
*"  CHANGING
*"     REFERENCE(CT_ZPALETE_PICKING) TYPE  ZPALETE_PICKING_TT
*"----------------------------------------------------------------------

  TYPES: BEGIN OF lty_pallet_sort.
          INCLUDE STRUCTURE zpalete_picking.

  TYPES: kunnr_rec TYPE kunnr,
         volum_rec TYPE volum,

        END OF lty_pallet_sort.

  DATA: lt_likp          TYPE TABLE OF likp,
        lt_lips          TYPE TABLE OF lips,
        lt_vbak          TYPE TABLE OF vbak,
        lt_vbpa          TYPE TABLE OF vbpa,
        lt_pallet_sort_h TYPE TABLE OF lty_pallet_sort,
        lt_pallet_sort   TYPE TABLE OF lty_pallet_sort.


  DATA: ls_likp TYPE likp,
        ls_lips TYPE lips,
        ls_vbak TYPE vbak,
        ls_vbpa TYPE vbpa.

  DATA: lv_last_kunnr TYPE kunnr.

  FIELD-SYMBOLS: <ls_pallet_sort>   TYPE lty_pallet_sort,
                 <ls_pallet_sort_h> TYPE lty_pallet_sort.

  CLEAR: e_processed.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  lt_pallet_sort = ct_zpalete_picking[].

** Cabeçalho Remessa
***********************************************************************
  SORT lt_pallet_sort BY vbeln.
  lt_pallet_sort_h = lt_pallet_sort.

  DELETE ADJACENT DUPLICATES FROM lt_pallet_sort_h COMPARING vbeln.
  CHECK NOT lt_pallet_sort_h IS INITIAL.

** Retorna Dados de Remessas
***********************************************************************
  DO 1 TIMES.
    SELECT * FROM likp
        INTO TABLE lt_likp
        FOR ALL ENTRIES IN lt_pallet_sort_h
        WHERE vbeln = lt_pallet_sort_h-vbeln.

    CHECK sy-subrc EQ 0.
    SORT lt_likp BY vbeln.

    SELECT * FROM lips
       INTO TABLE lt_lips
       FOR ALL ENTRIES IN lt_likp
       WHERE vbeln = lt_likp-vbeln.

    CHECK sy-subrc EQ 0.
    SORT lt_lips BY vbeln posnr.

    SELECT * FROM vbak
       INTO TABLE lt_vbak
       FOR ALL ENTRIES IN lt_lips
       WHERE vbeln = lt_lips-vgbel.

    CHECK sy-subrc EQ 0.
    SORT lt_vbak BY vbeln.

    SELECT * FROM vbpa
       INTO TABLE lt_vbpa
       FOR ALL ENTRIES IN lt_lips
       WHERE vbeln = lt_lips-vgbel AND
             posnr = lt_lips-vgpos AND
             parvw = 'EN'.
  ENDDO.

** Validação de Processamento
***********************************************************************
  LOOP AT lt_vbak INTO ls_vbak WHERE augru = 'Z09'.
    EXIT.
  ENDLOOP.
  CHECK sy-subrc EQ 0.

** Processamento d recebedores
***********************************************************************
  LOOP AT lt_pallet_sort_h ASSIGNING <ls_pallet_sort_h>.
    READ TABLE lt_pallet_sort WITH KEY vbeln = <ls_pallet_sort_h>-vbeln
                              TRANSPORTING NO FIELDS BINARY SEARCH.

    LOOP AT lt_pallet_sort ASSIGNING <ls_pallet_sort> FROM sy-tabix.
      IF <ls_pallet_sort>-vbeln <> <ls_pallet_sort_h>-vbeln.
        EXIT.
      ENDIF.

*--> Remessa
      CLEAR ls_lips.
      READ TABLE lt_lips INTO ls_lips
                         WITH KEY vbeln = <ls_pallet_sort>-vbeln
                                  posnr = <ls_pallet_sort>-posnr
                         BINARY SEARCH.

*--> Loja
      CLEAR ls_vbpa.
      READ TABLE lt_vbpa INTO ls_vbpa
                         WITH KEY vbeln = ls_lips-vgbel
                                  posnr = ls_lips-vgpos
                         BINARY SEARCH.

      <ls_pallet_sort>-kunnr_rec = ls_vbpa-kunnr.
    ENDLOOP.
  ENDLOOP.


** Calcula Volumes
***********************************************************************
  SORT lt_pallet_sort BY vbeln kunnr_rec.
  lt_pallet_sort_h = lt_pallet_sort.
  SORT lt_pallet_sort_h BY vbeln kunnr_rec.
  DELETE ADJACENT DUPLICATES FROM lt_pallet_sort_h COMPARING vbeln kunnr_rec.

  LOOP AT lt_pallet_sort_h ASSIGNING <ls_pallet_sort_h>.
    READ TABLE lt_pallet_sort WITH KEY vbeln = <ls_pallet_sort_h>-vbeln
                                       kunnr_rec = <ls_pallet_sort_h>-kunnr_rec
                              TRANSPORTING NO FIELDS BINARY SEARCH.

    CLEAR <ls_pallet_sort_h>-volum_rec.

    LOOP AT lt_pallet_sort ASSIGNING <ls_pallet_sort> FROM sy-tabix.
      IF <ls_pallet_sort>-vbeln <> <ls_pallet_sort_h>-vbeln OR
         <ls_pallet_sort>-kunnr_rec <> <ls_pallet_sort_h>-kunnr_rec.
        EXIT.
      ENDIF.

      IF <ls_pallet_sort>-kunnr_rec <> lv_last_kunnr.
        lv_last_kunnr = <ls_pallet_sort>-kunnr_rec.
        CLEAR <ls_pallet_sort_h>-volum_rec.
      ENDIF.

      <ls_pallet_sort_h>-volum_rec = <ls_pallet_sort_h>-volum_rec + <ls_pallet_sort>-volum.
    ENDLOOP.
  ENDLOOP.


  LOOP AT lt_pallet_sort_h ASSIGNING <ls_pallet_sort_h>.
    READ TABLE lt_pallet_sort WITH KEY vbeln = <ls_pallet_sort_h>-vbeln
                                       kunnr_rec = <ls_pallet_sort_h>-kunnr_rec
                              TRANSPORTING NO FIELDS BINARY SEARCH.

    LOOP AT lt_pallet_sort ASSIGNING <ls_pallet_sort> FROM sy-tabix.
      IF <ls_pallet_sort>-vbeln <> <ls_pallet_sort_h>-vbeln OR
         <ls_pallet_sort>-kunnr_rec <> <ls_pallet_sort_h>-kunnr_rec.
        EXIT.
      ENDIF.

      <ls_pallet_sort>-volum_rec = <ls_pallet_sort_h>-volum_rec.
    ENDLOOP.
  ENDLOOP.


** Ordenação
***********************************************************************
  SORT lt_pallet_sort BY vbeln     ASCENDING
                         volum_rec DESCENDING
                         kunnr_rec ASCENDING
                         sorlp     ASCENDING
                         posnr     ASCENDING
                         sub_item  ASCENDING.

  ct_zpalete_picking = lt_pallet_sort.
  e_processed = abap_true.

ENDFUNCTION.
