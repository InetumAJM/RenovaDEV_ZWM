*&---------------------------------------------------------------------*
*& Report  Z02RP_RP_REPDITA                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

INCLUDE z02rp_rp_ditanovo_top_v1.
INCLUDE z02rp_rp_ditanovo_o01_v1.
INCLUDE z02rp_rp_ditanovo_i01_v1.
INCLUDE z02rp_rp_ditanovo_f01_v1.

INITIALIZATION.

* Quando a transacção Z02RP36 for chamada
* é porque é reprocessamnto DIPE.
  IF sy-tcode = 'Z02RP36'.
    SET TITLEBAR 'DIPE'.
    divisao = 'DIPE'.
  ELSE.
    divisao = 'DITA'.
  ENDIF.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  CLEAR erro.
  PERFORM check_area.

  CHECK erro <> 'X'.

  PERFORM carrega_sessoes_dita.

END-OF-SELECTION.

  PERFORM escreve_sessoesdita.

AT LINE-SELECTION.
  IF NOT xsessaodita IS INITIAL.
    SET PF-STATUS 'ST02'.
    consprod = xsessaodita-numoper1.
    divisao = xsessaodita-divisao.
    area = xsessaodita-area.
    linha = xsessaodita-linha.
* INETUM - NR - 04.10.2023 - RENPRJ00041 - Inicio
***    aufnr = xsessaodita-aufnr1.
***    sessao_sem_prod = xsessaodita-sessao.
* INETUM - NR - 04.10.2023 - RENPRJ00041 - Fim
    PERFORM log_sessao_dita.
  ELSEIF NOT operacao IS INITIAL.
    PERFORM log_operacao.
  ENDIF.
* Roff - NR - 16.09.2019 - Inicio
***  ls_xsessaodita_aux = xsessaodita.
* Roff - NR - 16.09.2019 - Fim
  CLEAR: xsessaodita, operacao.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'AQP'.
      PERFORM altera_quantidade_produzida.
    WHEN 'REFRESH'.
      PERFORM log_sessao_dita.
    WHEN 'NEW'.
*      break roffd.
      DATA: itab TYPE TABLE OF sy-ucomm.
      APPEND 'NEW' TO itab.
      APPEND 'AQP' TO itab.
      APPEND 'REFRESH' TO itab.
*      APPEND 'PICK' TO itab.

      SET PF-STATUS 'ST02' EXCLUDING itab.
**      SET PF-STATUS 'ST01'.
      sy-lsind = 0.
      PERFORM escreve_cabecalho_sessaodita.
      PERFORM escreve_sessoesdita.
    WHEN 'BACK'.
*      break roffd.
  ENDCASE.

TOP-OF-PAGE.
  PERFORM escreve_cabecalho_sessaodita.

**TOP-OF-PAGE DURING LINE-SELECTION.
*  PERFORM escreve_novo_cab.
**
*&---------------------------------------------------------------------*
*&      Form  CHECK_AREA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_area .

  SELECT SINGLE mandt FROM z02rparln
    INTO sy-mandt
  WHERE divisao EQ divisao
    AND werks   IN so_werks
    AND dispo   IN so_area
    AND fevor   IN so_linha.
  IF sy-subrc <> 0.

    MESSAGE i999(z02rp) WITH 'Area/linha não pertence há' divisao '!'.
*   & & & &
    erro = 'X'.
  ENDIF.

ENDFORM.                    " CHECK_AREA
