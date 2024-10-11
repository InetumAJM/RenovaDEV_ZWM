FUNCTION zwm_hu_transfer.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_EXIDV) TYPE  EXIDV OPTIONAL
*"     REFERENCE(I_WERKS_D) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_LGORT_D) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(IT_ITM_TRANF) TYPE  ZRF01_T_HU_TRANSFER OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_PAL_SIZE) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_SETWERLGOR) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_OVER_PAL) TYPE  FLAG
*"     REFERENCE(ET_ITM_TRANF) TYPE  ZRF01_T_HU_TRANSFER
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  TYPES: BEGIN OF lty_mara,
           matnr       TYPE matnr,
          END OF lty_mara.

  DATA: lt_matnr          TYPE TABLE OF matnr,
        lt_mara           TYPE SORTED TABLE OF lty_mara WITH UNIQUE KEY matnr,
        lt_vekp_d         TYPE TABLE OF vekp,
        lt_vepo_d         TYPE SORTED TABLE OF vepo WITH UNIQUE KEY venum vepos,
        lt_vepo           TYPE SORTED TABLE OF vepo WITH UNIQUE KEY vepos,
        lt_return         TYPE TABLE OF bapiret2,
        lt_exidv          TYPE TABLE OF exidv,
        lt_itm_tranf      TYPE zrf01_t_hu_transfer,
        lt_itm_tranf_temp TYPE zrf01_t_hu_transfer,
        lt_itm_tranf_new  TYPE zrf01_t_hu_transfer,
        lt_itm_tranf_set  TYPE zrf01_t_hu_transfer,
        lt_callstack      TYPE abap_callstack,
        lt_new_exidv      TYPE TABLE OF exidv,
        lt_messages       TYPE tab_bdcmsgcoll.

  DATA: ls_mara             TYPE lty_mara,
        ls_vekp             TYPE vekp,
        ls_vekp_d           TYPE vekp,
        ls_return           TYPE bapiret2,
        ls_header_new_hu    TYPE bapihuhdrproposal,
        ls_itemunpack       TYPE bapihuitmunpack,
        ls_itemproposal	    TYPE bapihuitmproposal,
        ls_vepo             TYPE vepo,
        ls_vepo_d           TYPE vepo,
        ls_vekp_n           TYPE vekp,
        ls_bapihuitem       TYPE bapihuitem,
        ls_itm_tranf        TYPE zrf01_hu_transfer,
        ls_itm_tranf_new    TYPE zrf01_hu_transfer,
        ls_message          TYPE bdcmsgcoll,
        ls_callstack1       TYPE abap_callstack_line,
        ls_callstack2       TYPE abap_callstack_line.

  DATA: lv_exidv_o     TYPE exidv,
        lv_exidv       TYPE exidv,
        lv_new_exidv   TYPE exidv,
        lv_commit      TYPE flag,
        lv_werks_new   TYPE werks_d,
        lv_lgort_new   TYPE lgort_d,
        lv_lgort_hu    TYPE lgort_d,
        lv_werks_hu    TYPE werks_d,
        lv_set_qt      TYPE flag,
        lv_menge_new   TYPE menge_d,
        lv_menge_pal   TYPE menge_d,
        lv_werks_d     TYPE werks_d,
        lv_lgort_d     TYPE lgort_d.

  FIELD-SYMBOLS: <ls_itm_tranf> TYPE zrf01_hu_transfer.

  lt_itm_tranf = it_itm_tranf.
  lt_itm_tranf_set = it_itm_tranf.
  lv_exidv_o   = i_exidv.
  lv_werks_d   = i_werks_d.
  lv_lgort_d   = i_lgort_d.

  DELETE lt_itm_tranf_set WHERE setqt <> abap_true OR
                                exidv_d IS INITIAL.

  CLEAR: et_itm_tranf, et_messages, e_over_pal.

** Parametros
**********************************************************************
  DO 1 TIMES.
**    CALL FUNCTION 'ZWM_GET_PARAMETER'
**      EXPORTING
**        i_lgnum           =
**        i_processo        =
**        i_parametro       =
***       I_ITEM            =
***     IMPORTING
***       E_VALOR           =
***       ET_MESSAGES       =
***     TABLES
***       T_ZWM001          =
***     EXCEPTIONS
***       ERROR             = 1
***       OTHERS            = 2
**              .
**    IF sy-subrc <> 0.
*** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
***         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**    ENDIF.
**
**
**    zrf_configuration=>get_value(
**       EXPORTING
**         i_master      = i_lgnum
**         i_process     = 'GERAL'
**         i_paramet     = 'CENTRO'
**       IMPORTING
**         e_value       = lv_werks_hu
**         et_messages   = et_messages
**       EXCEPTIONS
**         error         = 1
**            ).
**    CHECK sy-subrc EQ 0.
**
**    zrf_configuration=>get_value(
**      EXPORTING
**        i_master      = i_lgnum
**        i_process     = 'GERAL'
**        i_paramet     = 'DEPOSITO_HU'
**      IMPORTING
**        e_value       = lv_lgort_hu
**        et_messages   = et_messages
**      EXCEPTIONS
**        error         = 1
**           ).
**    CHECK sy-subrc EQ 0.
  ENDDO.

*  IF sy-subrc <> 0.
*    RAISE error.
*  ENDIF.

** Hu de Origem
***********************************************************************
  DO 1 TIMES.
    CHECK NOT lv_exidv_o IS INITIAL.

    SELECT SINGLE * FROM vekp
                    INTO ls_vekp
                    WHERE exidv = lv_exidv_o AND
                          status <> '0060'.
    CHECK sy-subrc EQ 0.

    SELECT * FROM vepo
             INTO TABLE lt_vepo
             WHERE venum = ls_vekp-venum.
    CHECK sy-subrc EQ 0.
  ENDDO.
  IF NOT lv_exidv_o IS INITIAL AND
    sy-subrc <> 0.
**  HU & inválida
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZRF001'.
    ls_message-msgnr = '035'.
    ls_message-msgv1 = lv_exidv_o.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

** HU de Destino
**********************************************************************
  DO 1 TIMES.
    CHECK NOT lt_itm_tranf IS INITIAL.

    LOOP AT lt_itm_tranf INTO ls_itm_tranf.
      CHECK NOT ls_itm_tranf-exidv_d IS INITIAL.
      APPEND ls_itm_tranf-exidv_d TO lt_exidv.
    ENDLOOP.

    APPEND ls_vekp-exidv TO lt_exidv.

    SORT lt_exidv.
    DELETE ADJACENT DUPLICATES FROM lt_exidv.
    DELETE lt_exidv WHERE table_line IS INITIAL.
    CHECK NOT lt_exidv IS INITIAL.

    SELECT * FROM vekp
             INTO TABLE lt_vekp_d
             FOR ALL ENTRIES IN lt_exidv
             WHERE exidv = lt_exidv-table_line AND
                   status <> '0060'.
    CHECK sy-subrc EQ 0.
    SORT lt_vekp_d BY exidv ASCENDING
                      erdat DESCENDING
                      eruhr DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_vekp_d COMPARING exidv.

    SELECT * FROM vepo
             INTO TABLE lt_vepo_d
             FOR ALL ENTRIES IN lt_vekp_d
             WHERE venum = lt_vekp_d-venum.
    CHECK sy-subrc EQ 0.
  ENDDO.


** Dados de Material
**********************************************************************
  LOOP AT lt_vepo INTO ls_vepo.
    APPEND ls_vepo-matnr TO lt_matnr.
  ENDLOOP.

  LOOP AT lt_vepo_d INTO ls_vepo_d.
    APPEND ls_vepo_d-matnr TO lt_matnr.
  ENDLOOP.

  LOOP AT lt_itm_tranf INTO ls_itm_tranf.
    CHECK NOT ls_itm_tranf-matnr IS INITIAL.
    APPEND ls_itm_tranf-matnr TO lt_matnr.
  ENDLOOP.

  SORT lt_matnr.
  DELETE ADJACENT DUPLICATES FROM lt_matnr.

  DO 1 TIMES.
    CHECK NOT lt_matnr IS INITIAL.

    SELECT matnr  FROM mara
                  INTO TABLE lt_mara
                  FOR ALL ENTRIES IN lt_matnr
                  WHERE matnr = lt_matnr-table_line.

  ENDDO.

** Transfere Items Entre HU's
***********************************************************************
  SORT lt_itm_tranf BY exidv_d ASCENDING.

  CLEAR: lt_exidv.
  LOOP AT lt_itm_tranf ASSIGNING <ls_itm_tranf>.
    CLEAR: ls_vepo, lv_set_qt, lv_werks_new, lv_lgort_new.

*--> Set de Quantidade
    IF i_exidv IS INITIAL AND
       <ls_itm_tranf>-setqt EQ abap_true.
      lv_exidv_o = <ls_itm_tranf>-exidv_d.
      IF <ls_itm_tranf>-vepos_o IS INITIAL.
        <ls_itm_tranf>-vepos_o = <ls_itm_tranf>-vepos_d.
      ENDIF.
    ELSE.
      lv_exidv_o = i_exidv.
    ENDIF.

*--> Retorna Entrada
    IF NOT lv_exidv_o IS INITIAL.
      CLEAR: ls_vekp.
      READ TABLE lt_vekp_d
            INTO ls_vekp
            WITH KEY exidv = lv_exidv_o
            BINARY SEARCH.
    ELSEIF NOT i_exidv IS INITIAL.
      CLEAR: ls_vekp.
      READ TABLE lt_vekp_d
            INTO ls_vekp
            WITH KEY exidv = i_exidv
            BINARY SEARCH.
    ELSE.
      CLEAR: ls_vekp.
      READ TABLE lt_vekp_d
            INTO ls_vekp
            WITH KEY exidv = <ls_itm_tranf>-exidv_d
            BINARY SEARCH.
    ENDIF.

    IF <ls_itm_tranf>-vepos_o IS INITIAL AND
       NOT <ls_itm_tranf>-matnr IS INITIAL.
      CLEAR: ls_vepo.
      READ TABLE lt_vepo_d
            INTO ls_vepo
            WITH KEY venum = ls_vekp-venum
                     matnr = <ls_itm_tranf>-matnr
                     charg = <ls_itm_tranf>-charg.

      <ls_itm_tranf>-vepos_o = ls_vepo-vepos.
    ELSE.
      CLEAR: ls_vepo.
      READ TABLE lt_vepo_d
            INTO ls_vepo
            WITH TABLE KEY venum = ls_vekp-venum
                           vepos = <ls_itm_tranf>-vepos_o.

      <ls_itm_tranf>-matnr = ls_vepo-matnr.
      <ls_itm_tranf>-charg = ls_vepo-charg.
      <ls_itm_tranf>-werks = ls_vepo-werks.
      <ls_itm_tranf>-lgort = ls_vepo-lgort.
    ENDIF.

    IF NOT i_exidv IS INITIAL AND
       ls_vepo IS INITIAL.
**    Item & inválido na HU &
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'ZRF001'.
      ls_message-msgnr  = '281'.
      ls_message-msgv1  = <ls_itm_tranf>-vepos_o.
      ls_message-msgv2  = lv_exidv_o.
      APPEND ls_message TO et_messages.
      PERFORM delete_hu USING lt_new_exidv i_commit.
      RAISE error.
    ENDIF.

*--> Centro e Deposito
    IF NOT lv_werks_d IS INITIAL AND NOT lv_lgort_d IS INITIAL.
      lv_werks_new = lv_werks_d.
      lv_lgort_new = lv_lgort_d.
    ELSEIF NOT <ls_itm_tranf>-werks IS INITIAL AND NOT <ls_itm_tranf>-lgort IS INITIAL.
      lv_werks_new = <ls_itm_tranf>-werks.
      lv_lgort_new = <ls_itm_tranf>-lgort.
    ELSEIF NOT ls_vepo-werks IS INITIAL AND NOT ls_vepo-lgort IS INITIAL.
      lv_werks_new = ls_vepo-werks.
      lv_lgort_new = ls_vepo-lgort.
    ELSEIF NOT ls_vekp-werks IS INITIAL AND NOT ls_vekp-lgort IS INITIAL.
      lv_werks_new = ls_vekp-werks.
      lv_lgort_new = ls_vekp-lgort.
    ELSE.
      lv_werks_new = lv_werks_hu.
      lv_lgort_new = lv_lgort_hu.
    ENDIF.

*--> Material
    CLEAR: ls_mara.
    READ TABLE lt_mara
          INTO ls_mara
          WITH TABLE KEY matnr = <ls_itm_tranf>-matnr.
    CHECK sy-subrc EQ 0.

*--> Criação de nova HU
    CLEAR: ls_vekp_d.
    READ TABLE lt_vekp_d
          INTO ls_vekp_d
          WITH KEY exidv = <ls_itm_tranf>-exidv_d
          BINARY SEARCH.

    IF (
          ( <ls_itm_tranf>-exidv_d IS INITIAL AND lv_new_exidv IS INITIAL ) OR
          ( NOT <ls_itm_tranf>-exidv_d IS INITIAL AND ls_vekp_d IS INITIAL )

       ) AND
       NOT <ls_itm_tranf>-vhilm IS INITIAL.

      lv_new_exidv = <ls_itm_tranf>-exidv_d.

      CALL FUNCTION 'ZWM_HU_CREATE'
        EXPORTING
          i_werks       = lv_werks_new
          i_lgort       = lv_lgort_new
          i_packing_mat = <ls_itm_tranf>-vhilm
          i_commit      = abap_true
        IMPORTING
          es_vekp       = ls_vekp_n
          et_messages   = et_messages
        CHANGING
          c_exidv       = lv_new_exidv
        EXCEPTIONS
          error         = 1
          OTHERS        = 2.

      IF sy-subrc <> 0.
        IF i_commit EQ abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
        PERFORM delete_hu USING lt_new_exidv i_commit.
        RAISE error.
      ENDIF.

      APPEND lv_new_exidv TO lt_new_exidv.
      <ls_itm_tranf>-exidv_d = lv_new_exidv.

      CLEAR: ls_vekp_d.
      READ TABLE lt_vekp_d
            INTO ls_vekp_d
            WITH KEY exidv = ls_vekp_n-exidv
            BINARY SEARCH.
      IF sy-subrc <> 0.
        INSERT ls_vekp_n INTO TABLE lt_vekp_d.
        ls_vekp_d = ls_vekp_n.
      ENDIF.

    ELSEIF <ls_itm_tranf>-exidv_d IS INITIAL AND NOT <ls_itm_tranf>-vhilm IS INITIAL.
      <ls_itm_tranf>-exidv_d = lv_new_exidv.
    ENDIF.

    <ls_itm_tranf>-venum_d = ls_vekp_d-venum.

    IF ( NOT lv_exidv_o IS INITIAL AND lv_exidv_o <> <ls_itm_tranf>-exidv_d ) OR
       <ls_itm_tranf>-setqt EQ abap_true.

      IF NOT <ls_itm_tranf>-vemeh IS INITIAL AND
         NOT ls_vepo-vemeh IS INITIAL.
        "Unidades
        CALL FUNCTION 'ZWM_MATERIAL_CONVERT_UNIT'
          EXPORTING
            i_matnr     = <ls_itm_tranf>-matnr
            i_menge     = <ls_itm_tranf>-vemng
            i_meinh     = <ls_itm_tranf>-vemeh
          IMPORTING
            e_menge     = <ls_itm_tranf>-vemng
            et_messages = et_messages
          CHANGING
            c_meinh     = ls_vepo-vemeh
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.

        IF sy-subrc <> 0.
          PERFORM delete_hu USING lt_new_exidv i_commit.
          RAISE error.
        ENDIF.

        <ls_itm_tranf>-vemeh = ls_vepo-vemeh.
      ENDIF.

*--> Unpack
      CLEAR: ls_itemunpack.
      ls_itemunpack-hu_item_number  = <ls_itm_tranf>-vepos_o.

      IF <ls_itm_tranf>-setqt EQ abap_true.
        IF <ls_itm_tranf>-vemng < ls_vepo-vemng.
          ls_itemunpack-pack_qty = <ls_itm_tranf>-vemng - ls_vepo-vemng.
        ELSEIF <ls_itm_tranf>-vemng > ls_vepo-vemng AND
               NOT lv_exidv_o IS INITIAL.
          <ls_itm_tranf>-exidv_d = lv_exidv_o.
          lv_set_qt = abap_true.
        ENDIF.

        IF ls_itemunpack-pack_qty < 0.
          ls_itemunpack-pack_qty = ls_itemunpack-pack_qty * -1.
        ENDIF.
      ELSE.
        ls_itemunpack-pack_qty = ls_vepo-vemng - ( ls_vepo-vemng - <ls_itm_tranf>-vemng ).
      ENDIF.

      ls_itemunpack-base_unit_qty = <ls_itm_tranf>-vemeh.
      ls_itemunpack-material      = ls_vepo-matnr.
      ls_itemunpack-batch         = ls_vepo-charg.

      IF ls_itemunpack-pack_qty > 0.

        CLEAR: lt_return.
        CALL FUNCTION 'BAPI_HU_UNPACK'
          EXPORTING
            hukey          = lv_exidv_o
            itemunpack     = ls_itemunpack
          TABLES
            return         = lt_return.

        LOOP AT lt_return INTO ls_return WHERE type = 'E' OR
                                               type = 'A'.
          ls_message-msgtyp = ls_return-type.
          ls_message-msgid  = ls_return-id.
          ls_message-msgnr  = ls_return-number.
          ls_message-msgv1  = ls_return-message_v1.
          ls_message-msgv2  = ls_return-message_v2.
          ls_message-msgv3  = ls_return-message_v3.
          ls_message-msgv4  = ls_return-message_v4.
          APPEND ls_message TO et_messages.
        ENDLOOP.
        IF sy-subrc EQ 0.
          IF i_commit EQ abap_true.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.
          PERFORM delete_hu USING lt_new_exidv i_commit.
          RAISE error.
        ENDIF.

        lv_commit = abap_true.
      ENDIF.

    ENDIF.

    IF ( NOT <ls_itm_tranf>-exidv_d IS INITIAL AND lv_exidv_o <> <ls_itm_tranf>-exidv_d ) OR
       ( NOT <ls_itm_tranf>-exidv_d IS INITIAL AND lv_set_qt EQ abap_true ).

      DO 1 TIMES.
        CLEAR: ls_vekp_d.
        READ TABLE lt_vekp_d
              INTO ls_vekp_d
              WITH KEY exidv = <ls_itm_tranf>-exidv_d
              BINARY SEARCH.

        CHECK sy-subrc EQ 0.

        IF NOT <ls_itm_tranf>-vepos_d IS INITIAL.
          CLEAR: ls_vepo_d.
          READ TABLE lt_vepo_d
                INTO ls_vepo_d
                WITH KEY venum = ls_vekp_d-venum
                         vepos = <ls_itm_tranf>-vepos_d.
        ELSE.
          CLEAR: ls_vepo_d.
          READ TABLE lt_vepo_d
                INTO ls_vepo_d
                WITH KEY venum = ls_vekp_d-venum
                         matnr = <ls_itm_tranf>-matnr
                         charg = <ls_itm_tranf>-charg.
        ENDIF.

        CHECK NOT ls_vepo_d IS INITIAL.

        IF NOT ls_vepo   IS INITIAL AND
           NOT ls_vepo_d IS INITIAL.
          IF ls_vepo_d-matnr <> ls_vepo-matnr OR
             ls_vepo_d-charg <> ls_vepo-charg OR
             ls_vepo_d-werks <> ls_vepo-werks OR
             ls_vepo_d-lgort <> ls_vepo-lgort.
**          Item & da HU & não coincide com item & da HU &
            ls_message-msgtyp = 'E'.
            ls_message-msgid  = 'ZRF001'.
            ls_message-msgnr  = 280.
            ls_message-msgv1  = ls_vepo_d-vepos.
            ls_message-msgv2  = ls_vekp_d-exidv.
            ls_message-msgv3  = ls_vepo-vepos.
            ls_message-msgv4  = ls_vekp_d-exidv.
            APPEND ls_message TO et_messages.
          ENDIF.
        ENDIF.

        <ls_itm_tranf>-vepos_d = ls_vepo_d-vepos.
      ENDDO.


*--> Pack em HU
      CLEAR: ls_itemproposal.
      ls_itemproposal-hu_item_type  = 1.
      ls_itemproposal-material      = <ls_itm_tranf>-matnr.
      ls_itemproposal-batch         = <ls_itm_tranf>-charg.
      ls_itemproposal-plant         = lv_werks_new.
      ls_itemproposal-stge_loc      = lv_lgort_new.

      IF <ls_itm_tranf>-setqt EQ abap_true.
        IF <ls_itm_tranf>-vemng > ls_vepo_d-vemng.
          ls_itemproposal-pack_qty = <ls_itm_tranf>-vemng - ls_vepo_d-vemng.
        ENDIF.
      ELSE.
        ls_itemproposal-pack_qty =  <ls_itm_tranf>-vemng.
      ENDIF.


      ls_itemproposal-base_unit_qty = <ls_itm_tranf>-vemeh.

      IF ls_itemproposal-pack_qty > 0.
        DO 1 TIMES.
          CHECK ls_itemproposal-pack_qty > 0.
          CLEAR: lv_menge_new, lv_menge_pal.
          LOOP AT lt_vepo_d INTO ls_vepo_d WHERE venum = <ls_itm_tranf>-vepos_d AND
                                                 matnr = <ls_itm_tranf>-matnr.

            CALL FUNCTION 'ZWM_MATERIAL_CONVERT_UNIT'
              EXPORTING
                i_matnr     = <ls_itm_tranf>-matnr
                i_menge     = ls_vepo_d-vemng
                i_meinh     = ls_vepo_d-vemeh
              IMPORTING
                e_menge     = ls_vepo_d-vemng
                et_messages = et_messages
              CHANGING
                c_meinh     = <ls_itm_tranf>-vemeh
              EXCEPTIONS
                error       = 1
                OTHERS      = 2.

            IF sy-subrc <> 0.
              IF i_commit EQ abap_true.
                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              ENDIF.
              PERFORM delete_hu USING lt_new_exidv i_commit.
              RAISE error.
            ENDIF.

            lv_menge_new = lv_menge_new + ls_vepo_d-vemng.
          ENDLOOP.

          lv_menge_new = lv_menge_new + ls_itemproposal-pack_qty.

          CALL FUNCTION 'ZWM_MATERIAL_CHECK_PAL'
            EXPORTING
              i_lgnum     = i_lgnum
              i_matnr     = <ls_itm_tranf>-matnr
              i_menge     = lv_menge_new
              i_meins     = <ls_itm_tranf>-vemeh
            IMPORTING
              et_messages = lt_messages
            EXCEPTIONS
              error       = 1
              OTHERS      = 2.

          IF sy-subrc <> 0.
            e_over_pal = abap_true.
            IF i_pal_size EQ abap_true.
              et_messages = lt_messages.
              IF i_commit EQ abap_true.
                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              ENDIF.
              e_over_pal = abap_true.
              PERFORM delete_hu USING lt_new_exidv i_commit.
              RAISE error.
            ENDIF.
          ENDIF.
        ENDDO.


        CLEAR: lt_return.
        CALL FUNCTION 'BAPI_HU_PACK'
          EXPORTING
            hukey            = <ls_itm_tranf>-exidv_d
            itemproposal     = ls_itemproposal
          IMPORTING
            huitem           = ls_bapihuitem
          TABLES
            return           = lt_return.

        LOOP AT lt_return INTO ls_return WHERE type = 'E' OR
                                               type = 'A'.
          ls_message-msgtyp = ls_return-type.
          ls_message-msgid  = ls_return-id.
          ls_message-msgnr  = ls_return-number.
          ls_message-msgv1  = ls_return-message_v1.
          ls_message-msgv2  = ls_return-message_v2.
          ls_message-msgv3  = ls_return-message_v3.
          ls_message-msgv4  = ls_return-message_v4.
          APPEND ls_message TO et_messages.
        ENDLOOP.
        IF sy-subrc EQ 0.
          IF i_commit EQ abap_true.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.
          PERFORM delete_hu USING lt_new_exidv i_commit.
          RAISE error.
        ENDIF.

        lv_commit = abap_true.
      ENDIF.


      <ls_itm_tranf>-vepos_d = ls_bapihuitem-hu_item_number.
      <ls_itm_tranf>-werks   = ls_bapihuitem-plant.
      <ls_itm_tranf>-lgort   = ls_bapihuitem-stge_loc.
    ENDIF.

    IF lv_commit EQ abap_true AND
       i_commit EQ abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.

    IF <ls_itm_tranf>-vepos_d IS INITIAL AND
       lv_exidv_o EQ <ls_itm_tranf>-exidv_d AND
       NOT <ls_itm_tranf>-vepos_o IS INITIAL.
      <ls_itm_tranf>-vepos_d = <ls_itm_tranf>-vepos_o.
    ENDIF.

    IF <ls_itm_tranf>-vepos_o IS INITIAL AND
       lv_exidv_o EQ <ls_itm_tranf>-exidv_d AND
       NOT <ls_itm_tranf>-vepos_d IS INITIAL.
      <ls_itm_tranf>-vepos_o = <ls_itm_tranf>-vepos_d.
    ENDIF.

    APPEND <ls_itm_tranf>-exidv_d TO lt_exidv.
  ENDLOOP.
  IF sy-subrc <> 0.
    APPEND lv_exidv_o TO lt_exidv.
  ENDIF.

  et_itm_tranf = lt_itm_tranf.

** Retifica Pesos
**********************************************************************
  IF NOT lt_itm_tranf_set IS INITIAL AND
     i_commit EQ abap_true.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 0
      IMPORTING
        callstack = lt_callstack.

    READ TABLE lt_callstack
          INTO ls_callstack1
          INDEX 1.

    READ TABLE lt_callstack
          INTO ls_callstack2
          INDEX 2.

    IF ls_callstack1-blockname EQ ls_callstack2-blockname.
      EXIT.
    ENDIF.

    CALL FUNCTION ls_callstack1-blockname
      EXPORTING
        i_lgnum      = i_lgnum
        i_exidv      = i_exidv
        it_itm_tranf = lt_itm_tranf_set
        i_commit     = abap_true
      IMPORTING
        et_messages  = et_messages
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      PERFORM delete_hu USING lt_new_exidv i_commit.
      RAISE error.
    ENDIF.
  ENDIF.

** Uniforme de Centro e Deposito
***********************************************************************
  IF ( lv_werks_d IS INITIAL OR lv_lgort_d IS INITIAL ) AND
     i_setwerlgor EQ abap_true.
    lv_werks_d = lv_werks_hu.
    lv_lgort_d = lv_lgort_hu.
  ENDIF.

** Transfere HU entre Depósitos
***********************************************************************
  CHECK NOT lv_werks_d IS INITIAL AND
        NOT lv_lgort_d IS INITIAL.

  SORT lt_exidv.
  DELETE ADJACENT DUPLICATES FROM lt_exidv.

  LOOP AT lt_exidv INTO lv_exidv.
    CALL FUNCTION 'ZWM_HU_CHANGE_WERKS_LGORT'
      EXPORTING
        i_exidv     = lv_exidv
        i_werks     = lv_werks_d
        i_lgort     = lv_lgort_d
        i_commit    = i_commit
      IMPORTING
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      PERFORM delete_hu USING lt_new_exidv i_commit.
      RAISE error.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
