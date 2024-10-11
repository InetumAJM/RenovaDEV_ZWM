*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1F06 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  regista_entrada
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MODO   text
*      -->P_SUBRC  text
*----------------------------------------------------------------------*
FORM regista_entrada  USING    p_modo
                               p_aufnr
                               p_lgnum
                               p_matnr
                               p_quantidade
                               p_meins
                               p_charg
                      CHANGING p_subrc
                               p_mblnr
                               p_mjahr
                               p_msg.

  DATA: lt_items LIKE zwm018   OCCURS 0 WITH HEADER LINE.
  DATA: l_mov_mm TYPE bwlvs,
        mesa(14),
        l_porta  TYPE ablad,
        l_code   TYPE bapi2017_gm_code,
        l_lgort LIKE afpo-lgort.

  lt_items-armazem    = p_lgnum.
  lt_items-material   = p_matnr.
  lt_items-quantidade = p_quantidade.
  lt_items-uni        = p_meins.
  lt_items-lote       = p_charg.
  APPEND lt_items.

  FREE return_msg.

  CLEAR: p_subrc, p_mblnr, p_mjahr, p_msg, mesa.

  l_mov_mm = '101'.
  l_code   = '01'.

  CALL FUNCTION 'ZWM_GET_MESA'
    EXPORTING
      matnr      = p_matnr
      quantidade = p_quantidade
    IMPORTING
      mesa       = mesa.

  l_porta = mesa+4(10).

  IF p_aufnr IS NOT INITIAL.
    SELECT SINGLE lgort INTO l_lgort
        FROM afpo
            WHERE aufnr = p_aufnr.
  ENDIF.

  CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
    EXPORTING
      lgnum            = p_lgnum
      aufnr            = p_aufnr
      code             = l_code
      porta            = l_porta
      mov_mm           = l_mov_mm
      testrun          = p_modo
      plant_o          = 'RENV'
      sloc_o           = l_lgort
    IMPORTING
      materialdocument = p_mblnr
      matdocumentyear  = p_mjahr
    TABLES
      return_msg       = return_msg
      items            = lt_items
    EXCEPTIONS
      error            = 1
      OTHERS           = 2.

  p_subrc = sy-subrc.

  IF p_subrc NE 0.
    LOOP AT return_msg WHERE msgtyp EQ 'E'.
      CLEAR: t100.
      SELECT SINGLE text FROM t100
      INTO p_msg
      WHERE sprsl EQ sy-langu
        AND arbgb EQ return_msg-msgid
        AND msgnr EQ return_msg-msgnr.

      IF NOT return_msg-msgv1 IS INITIAL.
        PERFORM change_msg USING p_msg
                                 return_msg-msgv1.
      ENDIF.

      IF NOT return_msg-msgv2 IS INITIAL.
        PERFORM change_msg USING p_msg
                                 return_msg-msgv2.
      ENDIF.

      IF NOT return_msg-msgv3 IS INITIAL.
        PERFORM change_msg USING p_msg
                                 return_msg-msgv3.
      ENDIF.

      IF NOT return_msg-msgv4 IS INITIAL.
        PERFORM change_msg USING p_msg
                                 return_msg-msgv4.
      ENDIF.

      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " regista_entrada


*&---------------------------------------------------------------------*
*&      Form  regista_entrada_f03
*&---------------------------------------------------------------------*
*       Este metodo é especifico ao processo de entrada de produção para
*       a área F03, fabrica 1.
*----------------------------------------------------------------------*
*      -->P_MODO   text
*      -->P_SUBRC  text
*----------------------------------------------------------------------*
FORM regista_entrada_f03  USING    p_modo
                               p_aufnr
                               p_lgnum
                               p_matnr
                               p_quantidade
                               p_meins
                               p_charg
*                 Begin GN(ROFF) 30.10.2012
                               p_data
*                 End   GN(ROFF) 30.10.2012
                               p_divisao "JALVES - 03.09.2018
                      CHANGING p_subrc
                               p_mblnr
                               p_mjahr
                               p_msg.

  DATA: lt_items LIKE zwm018   OCCURS 0 WITH HEADER LINE.
  DATA: l_mov_mm TYPE bwlvs,
        mesa(14),
        l_porta  TYPE ablad,
        l_code   TYPE bapi2017_gm_code,
          l_lgort LIKE afpo-lgort.

  lt_items-armazem    = p_lgnum.
  lt_items-material   = p_matnr.
  lt_items-quantidade = p_quantidade.
  lt_items-uni        = p_meins.
  lt_items-lote       = p_charg.
  APPEND lt_items.

  FREE return_msg.

  CLEAR: p_subrc, p_mblnr, p_mjahr, p_msg, mesa.

  l_mov_mm = '101'.
  l_code   = '01'.

  IF p_divisao NE 'PROM'. "(+) JALVES - PROM - 03.09.2018
    CALL FUNCTION 'ZWM_GET_MESA'
      EXPORTING
        matnr      = p_matnr
        quantidade = p_quantidade
      IMPORTING
        mesa       = mesa.

    l_porta = mesa+4(10).
  ENDIF. "JALVES - PROM - 03.09.2018


  IF p_aufnr IS NOT INITIAL.
    SELECT SINGLE lgort INTO l_lgort
        FROM afpo
            WHERE aufnr = p_aufnr.
  ENDIF.

*->JALVES - PROM - 03.09.2018
  IF p_divisao EQ 'PROM'.
    CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL_F03'
      EXPORTING
        lgnum            = p_lgnum
        aufnr            = p_aufnr
        code             = l_code
        porta            = l_porta
        mov_mm           = l_mov_mm
        testrun          = p_modo
*      Begin GN(ROFF) 30.10.2012
        i_data           = p_data
*      End   GN(ROFF) 30.10.2012
        plant_o          = 'PROM'
        sloc_o           = l_lgort
      IMPORTING
        materialdocument = p_mblnr
        matdocumentyear  = p_mjahr
      TABLES
        return_msg       = return_msg
        items            = lt_items
      EXCEPTIONS
        error            = 1
        OTHERS           = 2.
  ELSE.
*<-JALVES - PROM - 03.09.2018
    CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL_F03'
      EXPORTING
        lgnum            = p_lgnum
        aufnr            = p_aufnr
        code             = l_code
        porta            = l_porta
        mov_mm           = l_mov_mm
        testrun          = p_modo
*      Begin GN(ROFF) 30.10.2012
        i_data           = p_data
*      End   GN(ROFF) 30.10.2012
        plant_o          = 'RENV'
        sloc_o           = l_lgort
      IMPORTING
        materialdocument = p_mblnr
        matdocumentyear  = p_mjahr
      TABLES
        return_msg       = return_msg
        items            = lt_items
      EXCEPTIONS
        error            = 1
        OTHERS           = 2.
  ENDIF.




  p_subrc = sy-subrc.

  IF p_subrc NE 0.
    LOOP AT return_msg WHERE msgtyp EQ 'E'.
      CLEAR: t100.
      SELECT SINGLE text FROM t100
      INTO p_msg
      WHERE sprsl EQ sy-langu
        AND arbgb EQ return_msg-msgid
        AND msgnr EQ return_msg-msgnr.

      IF NOT return_msg-msgv1 IS INITIAL.
        PERFORM change_msg USING p_msg
                                 return_msg-msgv1.
      ENDIF.

      IF NOT return_msg-msgv2 IS INITIAL.
        PERFORM change_msg USING p_msg
                                 return_msg-msgv2.
      ENDIF.

      IF NOT return_msg-msgv3 IS INITIAL.
        PERFORM change_msg USING p_msg
                                 return_msg-msgv3.
      ENDIF.

      IF NOT return_msg-msgv4 IS INITIAL.
        PERFORM change_msg USING p_msg
                                 return_msg-msgv4.
      ENDIF.

      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " regista_entrada


*&---------------------------------------------------------------------*
*&      Form  cria_sscc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SUBRC  text
*----------------------------------------------------------------------*
FORM cria_sscc  USING    p_subrc
                         p_lgnum
                         p_matnr
                         p_quantidade
                         p_meins
                         p_charg
                         p_werks
                         p_lgort
                         p_cor
                CHANGING g_hukey
                         g_msg.

  DATA lt_items LIKE zwm_items_hu OCCURS 0 WITH HEADER LINE.

  CLEAR: g_hukey, g_msg.

  CLEAR lt_items.

  lt_items-material    = p_matnr.
  lt_items-quantity    = p_quantidade.

  DATA: l_meins(10).

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input          = p_meins
      language       = sy-langu
    IMPORTING
      output         = l_meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  lt_items-unit        = l_meins.
  lt_items-batch       = p_charg.
  APPEND lt_items.

  REFRESH return_msg.

  CALL FUNCTION 'ZWM_CREATE_HU'
    EXPORTING
      warehouse                  = p_lgnum
      plant                      = p_werks
      s_loc                      = 'CD'
      packing_material           = p_cor
    IMPORTING
      hukey                      = g_hukey
    TABLES
      return_msg                 = return_msg
      items                      = lt_items
    EXCEPTIONS
      empty_table                = 1
      reference_document_differs = 2
      empty_delivery_item        = 3
      item_not_found             = 4
      OTHERS                     = 5.

  IF sy-subrc <> 0.
    LOOP AT return_msg WHERE msgtyp EQ 'E'.
      CLEAR: t100.
      SELECT SINGLE text FROM t100
      INTO g_msg
      WHERE sprsl EQ sy-langu
        AND arbgb EQ return_msg-msgid
        AND msgnr EQ return_msg-msgnr.

      EXIT.
    ENDLOOP.
  ELSE.

*** Obter a impressora
*    PERFORM get_parameter USING p_lgnum
*"    PERFORM get_parameter USING '100'
*                            'EAN128'
*                            'FABRICA2'
*                             l_printer.
*
*    FREE: itab_sscc.
*    CLEAR: itab_sscc.
*    itab_sscc-sscc = g_hukey.
*    APPEND itab_sscc.
*
*** Imprimir EAN128
*    CALL FUNCTION 'ZWM_IMPRIME_EAN128'
*      EXPORTING
*        printer                  = l_printer
*      TABLES
*        sscc                     = itab_sscc
*      EXCEPTIONS
*        impressora_nao_existe    = 1
*        sscc_nao_existe          = 2
*        sscc_com_impressao_grupo = 3
*        OTHERS                   = 4.
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

  ENDIF.

ENDFORM.                    " cria_sscc
*&---------------------------------------------------------------------*
*&      Form  actualiza_zwm013
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_zwm013 USING p_lgnum
                            p_hukey1
                            p_lety1.

  DATA: ls_zwm013 LIKE zwm013.

  SELECT SINGLE * FROM zwm013 WHERE armazem = p_lgnum
                                AND sscc    = p_hukey1.
  CHECK sy-subrc NE 0.

  CLEAR ls_zwm013.

  ls_zwm013-armazem     = p_lgnum.
  ls_zwm013-sscc        = p_hukey1.
  ls_zwm013-bloqueado   = 'X'.
  ls_zwm013-tipo_palete = p_lety1.

  INSERT zwm013 FROM ls_zwm013.

ENDFORM.                    " actualiza_zwm013
*&---------------------------------------------------------------------*
*&      Form  actualiza_zwm020
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_zwm020 USING p_lgnum
                            p_hukey1
                            p_hukey2.

  DATA: ls_zwm020 LIKE zwm020.
  SELECT SINGLE * FROM zwm020 WHERE armazem = p_lgnum
                                AND p1      = p_hukey1
                                AND p2      = p_hukey2.
  CHECK sy-subrc NE 0.

  CLEAR ls_zwm020.

  ls_zwm020-armazem = p_lgnum.
  ls_zwm020-p1      = p_hukey1.
  ls_zwm020-p2      = p_hukey2.

  INSERT zwm020 FROM ls_zwm020.

ENDFORM.                    " actualiza_zwm020
*&---------------------------------------------------------------------*
*&      Form  change_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSG  text
*      -->P_MSGV  text
*----------------------------------------------------------------------*
FORM change_msg  USING    p_msg
                          p_msgv.

  REPLACE '&' WITH p_msgv INTO p_msg.
  CONDENSE p_msg.

ENDFORM.                    " change_msg
