*&---------------------------------------------------------------------*
*&  Include           ZWMREP003F01                                     *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_ORDENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_ordens .

  IF NOT doc_compra1 IS INITIAL.
    ordens-ebeln = doc_compra1.
    APPEND ordens.
  ELSEIF NOT doc_compra2 IS INITIAL.
    ordens-ebeln = doc_compra2.
    APPEND ordens.
  ELSEIF NOT doc_compra3 IS INITIAL.
    ordens-ebeln = doc_compra3.
    APPEND ordens.
  ELSEIF NOT doc_compra4 IS INITIAL.
    ordens-ebeln = doc_compra4.
    APPEND ordens.
  ELSEIF NOT doc_compra5 IS INITIAL.
    ordens-ebeln = doc_compra5.
    APPEND ordens.
  ELSEIF NOT doc_compra6 IS INITIAL.
    ordens-ebeln = doc_compra6.
    APPEND ordens.
  ELSEIF NOT doc_compra7 IS INITIAL.
    ordens-ebeln = doc_compra7.
    APPEND ordens.
  ELSEIF NOT doc_compra8 IS INITIAL.
    ordens-ebeln = doc_compra8.
    APPEND ordens.
  ELSEIF NOT doc_compra9 IS INITIAL.
    ordens-ebeln = doc_compra9.
    APPEND ordens.
  ELSEIF NOT doc_compra10 IS INITIAL.
    ordens-ebeln = doc_compra10.
    APPEND ordens.
  ENDIF.

ENDFORM.                    " ACTUALIZA_ORDENS

*&---------------------------------------------------------------------*
*&      FORM  GET_PARAMETER
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_WAREHOUSE  TEXT
*      -->P_0035   TEXT
*      -->P_0036   TEXT
*      -->P_HOST   TEXT
*----------------------------------------------------------------------*
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

  IF ti_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = whs
      TABLES
        ti_zwm001 = ti_zwm001.
  ENDIF.

  CLEAR zwm001.
  READ TABLE ti_zwm001 WITH KEY      armazem   = whs
                                     processo  = module
                                     parametro = param
                                     BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE ti_zwm001 TO zwm001.
  ENDIF.
  MOVE zwm001-valor TO valor.

ENDFORM.                    " GET_PARAMETER

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra1 INPUT.

  IF NOT doc_compra1 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra1.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra1.
      MOVE 'DOC_COMPRA1' TO cursorfield.
      CLEAR doc_compra1.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra1.
      MOVE 'DOC_COMPRA1' TO cursorfield.
      CLEAR doc_compra1.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra1  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra2 INPUT.

  IF NOT doc_compra2 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra2.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra2.
      MOVE 'DOC_COMPRA2' TO cursorfield.
      CLEAR doc_compra2.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra2.
      MOVE 'DOC_COMPRA2' TO cursorfield.
      CLEAR doc_compra2.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra2  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra3 INPUT.

  IF NOT doc_compra3 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra3.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra3.
      MOVE 'DOC_COMPRA3' TO cursorfield.
      CLEAR doc_compra3.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra3.
      MOVE 'DOC_COMPRA3' TO cursorfield.
      CLEAR doc_compra3.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra3  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra4 INPUT.

  IF NOT doc_compra4 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra4.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra4.
      MOVE 'DOC_COMPRA4' TO cursorfield.
      CLEAR doc_compra4.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra4.
      MOVE 'DOC_COMPRA4' TO cursorfield.
      CLEAR doc_compra4.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra4  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra5  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra5 INPUT.

  IF NOT doc_compra5 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra5.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra5.
      MOVE 'DOC_COMPRA5' TO cursorfield.
      CLEAR doc_compra5.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra5.
      MOVE 'DOC_COMPRA5' TO cursorfield.
      CLEAR doc_compra5.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra5  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra6  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra6 INPUT.

  IF NOT doc_compra5 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra6.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra6.
      MOVE 'DOC_COMPRA6' TO cursorfield.
      CLEAR doc_compra6.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra6.
      MOVE 'DOC_COMPRA6' TO cursorfield.
      CLEAR doc_compra6.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra6  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra7  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra7 INPUT.

  IF NOT doc_compra7 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra7.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra7.
      MOVE 'DOC_COMPRA7' TO cursorfield.
      CLEAR doc_compra7.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra7.
      MOVE 'DOC_COMPRA7' TO cursorfield.
      CLEAR doc_compra7.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra7  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra8  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra8 INPUT.

  IF NOT doc_compra8 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra8.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra8.
      MOVE 'DOC_COMPRA8' TO cursorfield.
      CLEAR doc_compra8.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra8.
      MOVE 'DOC_COMPRA8' TO cursorfield.
      CLEAR doc_compra8.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra8  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra9  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra9 INPUT.

  IF NOT doc_compra9 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra9.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra9.
      MOVE 'DOC_COMPRA9' TO cursorfield.
      CLEAR doc_compra9.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra9.
      MOVE 'DOC_COMPRA9' TO cursorfield.
      CLEAR doc_compra9.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra9  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra10  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra10 INPUT.

  IF NOT doc_compra10 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra10.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra10.
      MOVE 'DOC_COMPRA10' TO cursorfield.
      CLEAR doc_compra10.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra10.
      MOVE 'DOC_COMPRA10' TO cursorfield.
      CLEAR doc_compra10.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra10  INPUT

*&---------------------------------------------------------------------*
*&      Module  f4_cod_mot  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_cod_mot INPUT.

  DATA: itab_f4 LIKE zwm036 OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF itab_dados OCCURS 0,
          cod_mot LIKE zwm036-cod_motivo,
          motivo  LIKE zwm036-motivo,
        END OF itab_dados.

  DATA: l_cod_dev LIKE zwm036-cod_dev.

  DATA: l_tit(40).

  FREE: itab_dynp, itab_return, itab_f4, itab_dados.
  CLEAR: itab_dynp, itab_return, itab_f4, itab_dados.

  CHECK NOT cod_dev IS INITIAL.

  itab_dynp-fieldname = 'COD_DEV'.
  APPEND itab_dynp.

  itab_dynp-fieldname = 'COD_MOT'.
  APPEND itab_dynp.

** Obtém valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: itab_dynp.
  READ TABLE itab_dynp INDEX 1.
  l_cod_dev = itab_dynp-fieldvalue.

  PERFORM converte_formato_interno USING l_cod_dev
                                   CHANGING l_cod_dev.

  CLEAR: zwm036.
  SELECT * FROM zwm036
  INTO CORRESPONDING FIELDS OF TABLE itab_f4
  WHERE cod_dev EQ cod_dev.

  LOOP AT itab_f4.
    MOVE-CORRESPONDING itab_f4 TO itab_dados.
    itab_dados-cod_mot = itab_f4-cod_motivo.
    APPEND itab_dados.
  ENDLOOP.

  CONCATENATE 'Motivos de devolução para' cod_dev
        INTO l_tit SEPARATED BY space.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COD_MOT'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'COD_MOT'
      value_org       = 'S'
      window_title    = l_tit
    TABLES
      value_tab       = itab_dados
      return_tab      = itab_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE itab_return INDEX 1.
    READ TABLE itab_dynp INDEX 2.
    itab_dynp-fieldvalue = itab_return-fieldval.
    MODIFY itab_dynp INDEX 2.
  ENDIF.

** Passar valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.                 " f4_cod_mot  INPUT

*&---------------------------------------------------------------------*
*&      Form  converte_formato_interno
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IN   text
*      <--P_OUT  text
*----------------------------------------------------------------------*
FORM converte_formato_interno  USING    p_in
                               CHANGING p_out.

  CLEAR: p_out.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_in
    IMPORTING
      output = p_out.

ENDFORM.                    " converte_formato_interno

*&---------------------------------------------------------------------*
*&      Module  f4_lote  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_lote INPUT.

  DATA: BEGIN OF itab_lote_f4 OCCURS 0,
          charg LIKE mch1-charg,
        END OF itab_lote_f4.

  FREE: itab_dynp, itab_return, itab_lote_f4.
  CLEAR: itab_dynp, itab_return, itab_lote_f4.

  CHECK NOT material IS INITIAL.

  itab_dynp-fieldname = 'MATERIAL'.
  APPEND itab_dynp.

  itab_dynp-fieldname = 'LOTE'.
  APPEND itab_dynp.

*** Obtém valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM converte_formato_interno USING doc_renova
                                   CHANGING l_vbeln.

  PERFORM converte_formato_interno USING material
                                   CHANGING l_matnr.

  CLEAR: lips.
  SELECT * FROM lips
  INTO CORRESPONDING FIELDS OF TABLE itab_lote_f4
  WHERE vbeln EQ l_vbeln
    AND matnr EQ l_matnr.

  IF sy-subrc NE 0.
    CLEAR: mch1.
    SELECT * FROM mch1
    INTO CORRESPONDING FIELDS OF TABLE itab_lote_f4
    WHERE matnr EQ l_matnr.

    CONCATENATE 'Lotes para o material' material
          INTO l_tit SEPARATED BY space.
  ELSE.

    CONCATENATE 'Lotes para a remessa' doc_renova
                'e material' material
          INTO l_tit SEPARATED BY space.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LOTE'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'LOTE'
      value_org       = 'S'
      window_title    = l_tit
    TABLES
      value_tab       = itab_lote_f4
      return_tab      = itab_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE itab_return INDEX 1.
    READ TABLE itab_dynp INDEX 2.
    itab_dynp-fieldvalue = itab_return-fieldval.
    MODIFY itab_dynp INDEX 2.
  ENDIF.

** Passar valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.                 " f4_lote  INPUT

*&---------------------------------------------------------------------*
*&      Form  check_cod_dev
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_cod_dev .

  CLEAR: l_text.

  CHECK NOT cod_dev IS INITIAL.

  PERFORM converte_formato_interno USING cod_dev
                                   CHANGING l_cod.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = 'ZWMD_COD_DEV'
      i_domvalue = l_cod
    IMPORTING
      e_ddtext   = l_text
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  IF sy-subrc <> 0.
    MOVE 'COD_DEV' TO cursorfield.
    MESSAGE e000
          WITH 'Erro! O código' cod_dev 'não existe'.
  ELSE.
    MOVE 'COD_MOT' TO cursorfield.
  ENDIF.

ENDFORM.                    " check_cod_dev

*&---------------------------------------------------------------------*
*&      Form  check_cod_mot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_cod_mot .

  CHECK NOT cod_mot IS INITIAL.

  CLEAR: zwm036, motivo.
  SELECT SINGLE motivo FROM zwm036
  INTO motivo
  WHERE cod_dev    EQ cod_dev
    AND cod_motivo EQ cod_mot.

  IF sy-subrc NE 0.
    MESSAGE e000
        WITH 'Erro! O código' cod_mot 'não existe para o cód. motivo'
             cod_dev.
  ENDIF.

ENDFORM.                    " check_cod_mot

*&---------------------------------------------------------------------*
*&      Form  get_f4_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_f4_material .

  DATA: BEGIN OF itab_f4 OCCURS 0,
*          maktx LIKE makt-maktx,
          matnr LIKE mara-matnr,
        END OF itab_f4.

  DATA: itab_dados LIKE itab_f4 OCCURS 0 WITH HEADER LINE.

  FREE: itab_dynp, itab_return, itab_f4, itab_dados.
  CLEAR: itab_dynp, itab_return, itab_f4, itab_dados.

  CHECK NOT doc_renova IS INITIAL.

  itab_dynp-fieldname = 'MATERIAL'.
  APPEND itab_dynp.

** Obtém valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM converte_formato_interno USING doc_renova
                                   CHANGING l_vbeln.

  CLEAR: lips.
  SELECT * FROM lips
  INTO CORRESPONDING FIELDS OF TABLE itab_dados
  WHERE vbeln EQ l_vbeln.

  SORT itab_dados.

  LOOP AT itab_dados.
    MOVE-CORRESPONDING itab_dados TO itab_f4.
    COLLECT itab_f4.
  ENDLOOP.

  CLEAR: l_tit.
  CONCATENATE 'Materiais para a Remessa' doc_renova INTO l_tit
  SEPARATED BY space.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MATERIAL'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'MATERIAL'
      value_org       = 'S'
      window_title    = l_tit
    TABLES
      value_tab       = itab_f4
      return_tab      = itab_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE itab_return INDEX 1.
    READ TABLE itab_dynp INDEX 1.
    itab_dynp-fieldvalue = itab_return-fieldval.
    MODIFY itab_dynp INDEX 1.
  ENDIF.

** Passar valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " get_f4_material

*&---------------------------------------------------------------------*
*&      Form  check_cliente
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_cliente .

  CHECK NOT cliente IS INITIAL.

  PERFORM converte_formato_interno USING cliente
                                   CHANGING l_cliente.

  CLEAR: kna1.
  SELECT SINGLE name1 FROM kna1
  INTO descricao_cliente
  WHERE kunnr EQ l_cliente.

  IF sy-subrc <> 0.
    MESSAGE e082 WITH cliente.
    CLEAR: cliente, descricao_cliente.
    MOVE 'CLIENTE' TO cursorfield.
  ELSE.
    MOVE 'DOC_RENOVA' TO cursorfield.
  ENDIF.

ENDFORM.                    " check_cliente

*&---------------------------------------------------------------------*
*&      Form  check_remessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_remessa .

** Verificar se a factura/Remessa da renova existe ...
** Caso o campo seja preenchido
  CHECK NOT doc_renova IS INITIAL.

  PERFORM converte_formato_interno USING doc_renova
                                   CHANGING l_vbeln.

  PERFORM converte_formato_interno USING cliente
                                   CHANGING l_cliente.

  SELECT SINGLE * FROM likp
                  WHERE vbeln EQ l_vbeln
                    AND kunnr EQ l_cliente.
  IF sy-subrc <> 0.
    MESSAGE e000
        WITH 'A remessa' doc_renova
             'não existe ou não é do cliente' cliente.
    CLEAR doc_renova.
    MOVE 'DOC_RENOVA' TO cursorfield.
*    EXIT.
  ELSE.
    MOVE 'MATERIAL' TO cursorfield.
  ENDIF.

ENDFORM.                    " check_remessa

*&---------------------------------------------------------------------*
*&      Form  check_material_remessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_material_remessa .

  CHECK NOT material IS INITIAL.

  PERFORM converte_formato_interno USING material
                                   CHANGING l_material.

  SELECT SINGLE * FROM mara
  WHERE matnr EQ l_material.

  IF sy-subrc <> 0.
** Material inexistente
    MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '070' WITH material.
    CLEAR material.
    MOVE 'MATERIAL' TO cursorfield.
    EXIT.
  ELSE.

** Material com status 'APAGADO'
    IF mara-lvorm = 'X'.
      MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '070' WITH material.
      CLEAR material.
      MOVE 'MATERIAL' TO cursorfield.
      EXIT.
    ELSE.
      SELECT SINGLE maktx FROM makt INTO descricao
                          WHERE matnr = l_material AND
                                spras = sy-langu.
      uni = mara-meins.
      MOVE 'LOTE' TO cursorfield.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_material_remessa

*&---------------------------------------------------------------------*
*&      Form  check_lote_dev
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lote_dev .

  CHECK NOT lote IS INITIAL.

  PERFORM converte_formato_interno USING material
                                   CHANGING l_matnr.

  CLEAR: mch1.
  SELECT SINGLE * FROM mch1
  WHERE matnr EQ l_matnr
    AND charg EQ lote.

  IF sy-subrc NE 0.
    MOVE 'LOTE' TO cursorfield.
    MESSAGE e000 WITH
      'O lote' lote 'não é um lote do material' material.
  ELSE.
    MOVE 'QUANTIDADE' TO cursorfield.
  ENDIF.

ENDFORM.                    " check_lote_dev

*&---------------------------------------------------------------------*
*&      Form  check_material_lote_remessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_material_lote_remessa .

  CHECK NOT material IS INITIAL.

** Verifica o material
  PERFORM converte_formato_interno USING doc_renova
                                   CHANGING l_vbeln.

  PERFORM converte_formato_interno USING material
                                   CHANGING l_matnr.

  SELECT * FROM lips
  WHERE vbeln EQ l_vbeln
    AND matnr EQ l_matnr.
    EXIT.
  ENDSELECT.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH
      'O material' material 'não é um material da remessa' doc_renova.
    MOVE 'MATERIAL' TO cursorfield.
  ENDIF.

ENDFORM.                    " check_material_lote_remessa
