*&---------------------------------------------------------------------*
*&  Include           ZWMREP0032O01                                    *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  PERFORM ler_status.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  get_parameters_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_parameters_0100 OUTPUT.

  TABLES: z02rpimp_etiquet.

  IF sy-tcode = 'ZWM036_F03'.
    GET PARAMETER ID 'WRK' FIELD centro.
    GET PARAMETER ID 'ZDIV' FIELD divisao.
    GET PARAMETER ID 'DGR' FIELD area1.
    GET PARAMETER ID 'CFV' FIELD linha.
    GET PARAMETER ID 'ANR' FIELD aufnr.
    GET PARAMETER ID 'ZSESSAO' FIELD z02rpsessao.
    GET PARAMETER ID 'ZOPER' FIELD operador.
    datum = z02rpsessao-data.
  ENDIF.

** alterado 2011.11.09 Paulo Sousa
** Ler default printer de z02rpimp_etiquet-pdest

  CLEAR printer.
* Roff - NR - 18.09.2019 - Inicio
***  SELECT SINGLE * FROM z02rpimp_etiquet WHERE linha = linha.
  SELECT SINGLE * FROM z02rpimp_etiquet WHERE werks = z02rpsessao-werks AND
                                              linha = linha.
* Roff - NR - 18.09.2019 - Fim
  IF sy-subrc = 0.
    IF NOT z02rpimp_etiquet-pdest IS INITIAL.
      printer = z02rpimp_etiquet-pdest.
    ENDIF.
  ENDIF.

  IF printer IS INITIAL.
    SELECT SINGLE pdest INTO printer
        FROM z02rpimp_et_user
            WHERE uname = sy-uname.
  ENDIF.

  IF printer IS INITIAL.
    printer = 'LOCL'.
  ENDIF.
ENDMODULE.                 " get_parameters_0100  OUTPUT
*----------------------------------------------------------------------*
*  MODULE get_parameters_0200 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE get_parameters_0200 OUTPUT.


*  GET PARAMETER ID 'WRK' FIELD centro.
*  GET PARAMETER ID 'ZDIV' FIELD divisao.
*  GET PARAMETER ID 'DGR' FIELD area1.
*  GET PARAMETER ID 'CFV' FIELD linha.
*  GET PARAMETER ID 'ANR' FIELD aufnr.
*  GET PARAMETER ID 'ZSESSAO' FIELD Z02RPSESSAO.
*  GET PARAMETER ID 'ZOPER' FIELD operador.

** alterado 2011.11.09 Paulo Sousa
** Ler default printer de z02rpimp_etiquet-pdest

  printer = 'LOCL'.
  SELECT SINGLE * FROM z02rpimp_etiquet WHERE linha = linha.
  IF sy-subrc = 0.
    IF NOT z02rpimp_etiquet-pdest IS INITIAL.
      printer = z02rpimp_etiquet-pdest.
    ENDIF.
  ENDIF.
ENDMODULE.                 " get_parameters_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  change_campos  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_campos OUTPUT.

  PERFORM check_campos.

  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " change_campos  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ADDITIONAL_DATA_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_additional_data_0100 OUTPUT.

  "Material pelo numero da ordem
  CLEAR: afpo.
  SELECT SINGLE matnr FROM afpo INTO matnr
   WHERE aufnr EQ aufnr.


  CLEAR: mara.
  SELECT SINGLE * FROM mara
   WHERE matnr EQ matnr.

  " Nº europeu de artigo(EAN)
  IF mara-ean11 IS INITIAL.
*   MESSAGE w000 WITH 'Material ' matnr ' não tem nº europeu de artigo(EAN) atribuido.'.
*   CLEAR ean11.
  ELSE.
    ean11 = mara-ean11.
  ENDIF.

  IF mara-mtart IS NOT INITIAL.
    IF mara-mtart EQ 'SMIN'.
      lgnum = 200.
    ELSE.
      lgnum = 100.
    ENDIF.
  ENDIF.

  " Unidade de medida
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = mara-meins
      language       = sy-langu
    IMPORTING
      output         = meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.


  " descrição do material
  PERFORM get_descricao_matnr USING matnr
                              CHANGING maktx.

  " Descrição do centro
  SELECT SINGLE name1 FROM t001w
    INTO desc1
   WHERE werks EQ centro.

  " Lote
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = aufnr
    IMPORTING
      output = l_aufnr.

  CONCATENATE l_aufnr(5) l_aufnr+7(5) INTO charg.

  "Deposito
  PERFORM check_lgnum.


ENDMODULE.                 " GET_ADDITIONAL_DATA_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ADDITIONAL_DATA_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_additional_data_0200 OUTPUT.

  "Material pelo numero da ordem
  CLEAR: afpo.
  SELECT SINGLE matnr FROM afpo INTO matnr
   WHERE aufnr EQ aufnr.

  CLEAR: mara.
  SELECT SINGLE * FROM mara
   WHERE matnr EQ matnr.

  " Nº europeu de artigo(EAN)
  IF mara-ean11 IS INITIAL.
*   MESSAGE w000 WITH 'Material ' matnr ' não tem nº europeu de artigo(EAN) atribuido.'.
*   CLEAR ean11.
  ELSE.
    ean11 = mara-ean11.
  ENDIF.

  IF mara-mtart EQ 'SMIN'.
    lgnum = 200.

  ELSE.

  ENDIF.


  " Unidade de medida
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = mara-meins
      language       = sy-langu
    IMPORTING
      output         = meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.


  " descrição do material
  PERFORM get_descricao_matnr USING matnr
                              CHANGING maktx.

  " Descrição do centro
  SELECT SINGLE name1 FROM t001w
    INTO desc1
   WHERE werks EQ centro.

  " Lote
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = aufnr
    IMPORTING
      output = l_aufnr.

  CONCATENATE l_aufnr(5) l_aufnr+7(5) INTO charg.

  "Deposito
  PERFORM check_lgnum.


ENDMODULE.                    "get_additional_data_0200 OUTPUT

*----------------------------------------------------------------------*
*  MODULE get_field_visibility OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE get_field_visibility OUTPUT.

  IF lgnum IS NOT INITIAL.
*    CLEAR imprimir.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'SD'.
        screen-input = 0.
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                    "get_field_visibility OUTPUT
