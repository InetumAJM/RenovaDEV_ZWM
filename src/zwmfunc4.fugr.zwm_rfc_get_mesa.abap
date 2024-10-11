FUNCTION zwm_rfc_get_mesa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_SSCC) LIKE  ZWM_AUX-SSCC
*"     VALUE(I_MESA) LIKE  ZWM_AUX-MESA OPTIONAL
*"     VALUE(I_PESO) LIKE  ZWM_AUX_RFC-PESO OPTIONAL
*"  EXPORTING
*"     VALUE(MESA) LIKE  ZWM_AUX-MESA
*"     VALUE(E_SSCC) LIKE  ZWM_AUX-SSCC
*"     VALUE(RETORNO) LIKE  ZWM_AUX-RETORNO
*"----------------------------------------------------------------------

** Bloquear acesso à tabela zwm_log_efacec
*  PERFORM bloqueia_log_efacec USING 'GET_MESA'.

  CLEAR: wa_log, retorno, mesa, e_sscc.

** Dados table de LOG do interface
  GET TIME.
  wa_log-data     = sy-datum.
  wa_log-processo = 'GET_MESA'.
  wa_log-hora     = sy-uzeit.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_sscc
    IMPORTING
      output = i_sscc.

  wa_log-sscc = i_sscc.

  e_sscc = i_sscc.

  SELECT SINGLE * FROM vekp WHERE exidv = i_sscc.
  IF sy-subrc <> 0.
    retorno = 10.
    wa_log-retorno = retorno.
    wa_log-msg = 'SSCC Não existe'.
    wa_log-utilizador = sy-uname.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.
  SELECT SINGLE * FROM vepo WHERE venum = vekp-venum.
  IF sy-subrc <> 0.
    retorno = 20.
    wa_log-retorno = retorno.
    wa_log-msg = 'SSCC Não existe'.
    wa_log-utilizador = sy-uname.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.

  IF i_mesa IS INITIAL.

    CALL FUNCTION 'ZWM_GET_MESA'
      EXPORTING
        matnr      = vepo-matnr
        quantidade = vepo-vemng
      IMPORTING
        mesa       = mesa.

  ELSE.
    mesa = i_mesa.
  ENDIF.

  IF mesa IS INITIAL.
    retorno = 40.
    wa_log-retorno    = retorno.
    wa_log-msg        = 'Mesa de destino a vazio'.
    wa_log-utilizador = sy-uname.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.


  CLEAR: zwm013.
  SELECT SINGLE * FROM zwm013 WHERE armazem = '100' AND sscc = i_sscc.
  IF sy-subrc = 0.
    IF NOT zwm013-destino IS INITIAL.
      retorno = 30.
      wa_log-retorno = retorno.
      wa_log-msg = 'SSCC já existe com erro'.
      wa_log-utilizador = sy-uname.
      MODIFY zwm_log_efacec FROM wa_log.
      EXIT.
    ELSE.

** Validar se o SSCC já existe no CD
      CLEAR: lein.
      SELECT SINGLE * FROM lein
      WHERE lenum EQ i_sscc
        AND lgnum EQ '100'.
      IF sy-subrc EQ 0.
        retorno = 30.
        wa_log-retorno = retorno.
        wa_log-msg = 'SSCC já existe com erro'.
        wa_log-utilizador = sy-uname.
        MODIFY zwm_log_efacec FROM wa_log.
        EXIT.
      ELSE.
        zwm013-destino = mesa.
        MODIFY zwm013.
      ENDIF.
    ENDIF.
  ELSE.
    retorno = 10.
    wa_log-retorno = retorno.
    wa_log-msg = 'SSCC Não existe'.
    wa_log-utilizador = sy-uname.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.

  retorno = 00.

  wa_log-mesa = mesa.
  wa_log-retorno = retorno.
  MODIFY zwm_log_efacec FROM wa_log.


** Registo do Peso da Palete
***********************************************************************
  DATA: peso_palete LIKE zwm_log_efacec-peso_pal.

  peso_palete = i_peso.
  peso_palete = peso_palete / 10.
* Ezequiel Ferreira - Field l_peso para substituição i_peso no form
* actualizar tabela erros
  DATA: l_peso LIKE i_peso.
  l_peso = peso_palete.
  GET TIME.
  UPDATE zwm_log_efacec SET peso_pal = peso_palete
                            uni_peso = 'KG'
                            ERDAT_PESO = sy-datum
                            ERZET_PESO = sy-uzeit
                      WHERE processo = 'ENTRADA_PRODUCAO'
                        AND sscc     = i_sscc.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

  PERFORM actualiza_taberros USING peso_palete
                                   i_sscc
                                   vepo-matnr.

ENDFUNCTION.


*&--------------------------------------------------------------------*
*&      Form  actualiza_taberros
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM actualiza_taberros USING p_peso p_sscc p_matnr.

  DATA: lv_pesoinf LIKE zmatpesoserros-peso,
        lv_pesosup LIKE zmatpesoserros-peso,
        lv_cor     LIKE zwm_log_efacec-cor,
        lv_aufnr   LIKE afko-aufnr,
        lv_stlal   LIKE afko-stlal,
        lv_budat   LIKE zmatpesoserros-budat,
        lv_uzeit   LIKE zmatpesoserros-uzeit,
        lv_peso    LIKE zmatpesoserros-peso.
  DATA: lt_erros   LIKE zmatpesoserros.

  SELECT SINGLE aufnr cor data hora INTO
                     (lv_aufnr,lv_cor,lv_budat,lv_uzeit)
      FROM zwm_log_efacec
      WHERE processo = 'ENTRADA_PRODUCAO'
        AND sscc     = p_sscc.
  IF sy-subrc = 0.

    SELECT SINGLE stlal INTO lv_stlal
        FROM afko
        WHERE aufnr = lv_aufnr.

    CLEAR lv_peso.

    SELECT SINGLE palet_med INTO lv_peso
        FROM zmatpesos
        WHERE plnbez = p_matnr
          AND matnr  = lv_cor
          AND stlal  = lv_stlal.

    IF sy-subrc EQ 0.
      IF lv_peso <> 0.
        lv_pesoinf = lv_peso * 9 / 10.
        lv_pesosup = lv_peso * 11 / 10.

        IF p_peso < lv_pesoinf OR p_peso > lv_pesosup.
* Actualiza zmatpesoserros
          lt_erros-mandt = sy-mandt.
          lt_erros-matnr = p_matnr.
          lt_erros-aufnr = lv_aufnr.
          lt_erros-sscc = p_sscc.
          lt_erros-peso = p_peso.
          lt_erros-uni_peso = 'KG'.
          lt_erros-budat = lv_budat.
          lt_erros-uzeit = lv_uzeit.
          INSERT zmatpesoserros FROM lt_erros.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "actualiza_taberros
