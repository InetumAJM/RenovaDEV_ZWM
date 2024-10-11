FUNCTION zwm_estorna_doc_material_email.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MBLNR) TYPE  MBLNR
*"     REFERENCE(MJAHR) TYPE  MJAHR
*"     REFERENCE(BWART) TYPE  BWART OPTIONAL
*"     REFERENCE(BUDAT) TYPE  BUDAT OPTIONAL
*"  EXPORTING
*"     REFERENCE(IT_DOCU) TYPE  ZWM_TT_DOCU_YEAR
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

* Módulo de função com o objectivo de estornar documentos
* do tipo definido na tabela de codificados.
  TYPES:
  abap_bool TYPE c LENGTH 1.

  CONSTANTS: abap_true TYPE abap_bool VALUE 'X',
             abap_false TYPE abap_bool VALUE ' '.

  DATA: lv_wait TYPE i,
        ls_likp TYPE likp,
        lt_lips TYPE TABLE OF lips,
        ls_mkpf TYPE mkpf,
        lt_mseg TYPE TABLE OF mseg,
        ls_mseg type mseg,
        lt_vbfa TYPE TABLE OF vbfa,
        ls_vbfa TYPE vbfa.

  DATA: ls_docu TYPE zwm_docu_year,
       lv_doc TYPE bapi2017_gm_head_02-mat_doc,
       lv_year TYPE bapi2017_gm_head_02-doc_year,
       lv_date TYPE bapi2017_gm_head_02-pstng_date,
       ls_goods TYPE bapi2017_gm_head_ret,
       ls_docout TYPE zwm_docu_year,
       ls_return TYPE bapiret2,
       lt_messages  TYPE tab_bdcmsgcoll,
       ls_messages TYPE bdcmsgcoll,
       lt_zwm001 TYPE TABLE OF zwm001,
       ls_zwm001 TYPE zwm001,
       lv_target   TYPE so_recname.


  DO.

*     Verifica se o documento já foi criado
    CALL FUNCTION 'ENQUEUE_EMMKPF'
      EXPORTING
        mblnr          = mblnr
        mjahr          = mjahr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      EXIT.
    ENDIF.

    lv_wait = lv_wait + 1.

    WAIT UP TO 1 SECONDS.

    IF lv_wait > 60.
      EXIT.
    ENDIF.

  ENDDO.

  CALL FUNCTION 'DEQUEUE_EMMKPF'.

  SELECT SINGLE * FROM mkpf
    INTO ls_mkpf
    WHERE mblnr = mblnr
    AND mjahr = mjahr.

  SELECT * FROM mseg
    INTO TABLE lt_mseg
    WHERE mblnr = ls_mkpf-mblnr
    AND mjahr = ls_mkpf-mjahr.

  SELECT SINGLE * FROM likp
   INTO ls_likp
   WHERE vbeln = ls_mkpf-le_vbeln.

  CHECK sy-subrc EQ 0.
*  SORT lt_likp BY vbeln.

  SELECT * FROM zwm001
      INTO TABLE lt_zwm001
      WHERE armazem  = '' AND
            processo = 'TRANSF_PALLETS'.


  LOOP AT lt_zwm001 INTO ls_zwm001.
    IF ls_zwm001-parametro EQ 'LISTA_DIST_MAIL'.
      lv_target = ls_zwm001-valor.
    ENDIF.
  ENDLOOP.

*  SELECT * FROM lips
*     INTO TABLE lt_lips
*     WHERE vbeln = ls_likp-vbeln.

*  IF lt_lips IS NOT INITIAL.
*    SELECT * FROM vbfa
*      INTO TABLE lt_vbfa
*      FOR ALL ENTRIES IN lt_lips
*      WHERE vbelv = lt_lips-vbeln
*      AND posnv = lt_lips-posnr
*      AND vbtyp_n = 'R'.
**      and bwart <> ''.
*
*  ENDIF.

*  SORT lt_vbfa ASCENDING BY vbeln.
*
*  READ TABLE lt_vbfa INTO ls_vbfa INDEX 1.

  LOOP AT lt_mseg INTO ls_mseg.
    SELECT SINGLE * FROM zwm_docu_year
       INTO ls_docu
       WHERE mblnr = ls_mseg-smbln
       AND mjahr   = ls_mseg-sjahr.
      IF sy-subrc eq 0.
        exit.
      ENDIF.
  ENDLOOP.


  lv_doc = ls_docu-mblnrnew.
  lv_year = ls_docu-mjahrnew.

  SELECT SINGLE budat FROM mkpf
    INTO lv_date
    WHERE mblnr = lv_doc
    AND mjahr = lv_year.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument    = lv_doc
        matdocumentyear     = lv_year
        goodsmvt_pstng_date = budat
      IMPORTING
        goodsmvt_headret    = ls_goods
      TABLES
        return              = return.


    LOOP AT return INTO ls_return WHERE type CA 'AEX'.
      ls_messages-msgtyp = ls_return-type.
      ls_messages-msgid  = ls_return-id.
      ls_messages-msgnr  = ls_return-number.
      ls_messages-msgv1 = ls_return-message_v1.
      ls_messages-msgv2 = ls_return-message_v2.
      ls_messages-msgv3 = ls_return-message_v3.
      ls_messages-msgv4 = ls_return-message_v4.

      APPEND ls_messages TO lt_messages.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.


      CALL FUNCTION 'ZWM_PALLET_TRANSFER_EMAIL'
       EXPORTING
         i_title      = 'Movimento de estorno'
*          is_mseg      = ls_mseg
         is_mkpf_orig = ls_mkpf
         is_likp      = ls_likp
         it_messages  = lt_messages
         i_target     = lv_target
         i_commit     = abap_true.

      RETURN.
    ENDLOOP.
    IF sy-subrc <> 0.
      ls_docout-mblnr = mblnr.
      ls_docout-mjahr = mjahr.
      ls_docout-bwart = bwart.
      ls_docout-mblnrnew = ls_goods-mat_doc.
      ls_docout-mjahrnew = ls_goods-doc_year.
      ls_docout-processado = 'X'.
      APPEND ls_docout TO it_docu.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

ENDFUNCTION.
