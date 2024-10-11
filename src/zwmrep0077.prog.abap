*&---------------------------------------------------------------------*
*& Report  ZWMREP0041                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0077 MESSAGE-ID zwmmsg001.

TABLES : vekp,
         zwm041.

DATA: itab_sscc LIKE zwm_ean128 OCCURS 0 WITH HEADER LINE,
      t_vekp    LIKE vekp       OCCURS 0 WITH HEADER LINE.

DATA f_subrc LIKE sy-subrc.

SELECT-OPTIONS s_sscc FOR vekp-exidv  OBLIGATORY.         " default '00356010283001137073'.
PARAMETERS: p_print   LIKE pri_params-pdest OBLIGATORY,   " default 'ZPC1',
            p_msg(1)  TYPE c NO-DISPLAY.

START-OF-SELECTION.

  CLEAR f_subrc.
  FREE: itab_sscc.
  CLEAR: itab_sscc.

  SELECT * INTO TABLE t_vekp
      FROM vekp
          WHERE exidv IN s_sscc.

  IF sy-subrc NE 0.
    MESSAGE i000 WITH 'SSCC inválidos'.
    EXIT.
  ENDIF.

  READ TABLE s_sscc INDEX 1.

  GET TIME.
  zwm041-exidv1 = s_sscc-low.
  zwm041-exidv2 = s_sscc-high.
  zwm041-pdest = p_print.
  zwm041-ernam = sy-uname.
  zwm041-erdat = sy-datum.
  zwm041-uzeit = sy-uzeit.

  INSERT zwm041.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ENDIF.

  LOOP AT t_vekp.
    itab_sscc-sscc = t_vekp-exidv.
    APPEND itab_sscc.
  ENDLOOP.

  CALL FUNCTION 'ZWM_IMPRIME_CONTEUDO_PICKING'
    EXPORTING
      printer                  = p_print
    TABLES
      sscc                     = itab_sscc
    EXCEPTIONS
      impressora_nao_existe    = 1
      sscc_nao_existe          = 2
      sscc_com_impressao_grupo = 3
      OTHERS                   = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF p_msg NE 'X'.
      MESSAGE i000 WITH 'Impressão efectuada'.
    ENDIF.
    CLEAR: s_sscc, p_print.
  ENDIF.
