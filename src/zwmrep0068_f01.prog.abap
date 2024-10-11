*&---------------------------------------------------------------------*
*&  Include           ZWMREP0068_F01                                   *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  clear_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_fields .

  CLEAR: pdt_sscc.
  PERFORM clear.

ENDFORM.                    " clear_fields

*&---------------------------------------------------------------------*
*&      Form  clear
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear.

  CLEAR:   pdt_sscc2,
           pdt_matnr,
           pdt_maktx1,
           pdt_maktx2,
           pdt_gesme,
           pdt_meins,
           pdt_charg,
           pdt_lgpla,
           pdt_dest,
           pdt_pck,
           lgtyp,
           lgpla,
           bestq,
           cursorfield,
           it_sscc,
           dest_dri,
           pdt_refnr.

  REFRESH: it_sscc.

ENDFORM.                    " clear

*&---------------------------------------------------------------------*
*&      Form  find_whs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs .

** Wharehouse
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
**   raise no_warehouse_found.
  ELSE.
    READ TABLE l_user WITH KEY statu = 'X'.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
          message_var1   = text1
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ELSE.
      lgnum = l_user-lgnum.
      IF l_user-devty = '8X40'.
        setscreen1 = '0002'.
      ELSE.
        setscreen1 = '0001'.
      ENDIF.


    ENDIF.
  ENDIF.

ENDFORM.                    " find_whs

*&---------------------------------------------------------------------*
*&      Form  get_parametrizacao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_parametrizacao .

  DATA: BEGIN OF it_001 OCCURS 0,
          processo  LIKE zwm001-processo,
          parametro LIKE zwm001-parametro,
          valor     LIKE zwm001-valor,
        END OF it_001.

  REFRESH: it_001.
  CLEAR:   it_001.

** Parametrização
  SELECT processo parametro valor
          FROM  zwm001
          INTO CORRESPONDING FIELDS OF TABLE it_001
         WHERE armazem = lgnum.

  SORT it_001 BY processo parametro.

**** Centro
**  CLEAR it_001.
**  READ TABLE it_001 WITH KEY processo  = 'GERAL'
**                             parametro = 'PLANT'.
**  werks = it_001-valor.
**
**** Depósito
**  CLEAR it_001.
**  READ TABLE it_001 WITH KEY processo  = 'GERAL'
**                             parametro = 'LGORT'.
**  lgort = it_001-valor.

** Tipo Movimento
  CLEAR it_001.
  READ TABLE it_001 WITH KEY processo  = 'TRANSFERENCIA'
                             parametro = 'MOV'.
  bwlvs = it_001-valor.

** Tipos Depósito válidos
  REFRESH: it_lgtyp.
  CLEAR:   it_lgtyp.

  LOOP AT it_001 WHERE processo EQ 'TRANSFERENCIA_PAL'.
    MOVE-CORRESPONDING it_001 TO it_lgtyp.
    APPEND it_lgtyp.
    CLEAR  it_lgtyp.
  ENDLOOP.

  SORT it_lgtyp BY valor.

** Tipo Depósito PCK
  CLEAR it_001.
  READ TABLE it_001 WITH KEY processo  = 'ENTRADA_ARMAZEM'
                             parametro = 'ST_PCK'.
  lgtyp_pck = it_001-valor.

** Tipo Depósito DRI
  CLEAR it_001.
  READ TABLE it_001 WITH KEY processo  = 'ENTRADA_ARMAZEM'
                             parametro = 'ST_DRI'.
  lgtyp_dri = it_001-valor.

  IF "werks      IS INITIAL OR
     "lgort      IS INITIAL OR
     bwlvs      IS INITIAL OR
     lgtyp_pck  IS INITIAL OR
     lgtyp_dri  IS INITIAL OR
     it_lgtyp[] IS INITIAL.
    MESSAGE e000 WITH 'Falha parametrização'.
  ENDIF.

ENDFORM.                    " get_parametrizacao
