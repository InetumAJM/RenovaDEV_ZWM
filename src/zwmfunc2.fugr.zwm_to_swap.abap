FUNCTION zwm_to_swap.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM_O) TYPE  TANUM
*"     REFERENCE(I_TAPOS_O) TYPE  TAPOS
*"     REFERENCE(I_LENUM_D) TYPE  LENUM
*"  EXPORTING
*"     REFERENCE(E_TANUM_D) TYPE  TANUM
*"     REFERENCE(E_TAPOS_D) TYPE  TAPOS
*"     REFERENCE(E_CHARG_D) TYPE  CHARG_D
*"     REFERENCE(ES_LTAK_D) TYPE  LTAK
*"     REFERENCE(ES_LTAP_D) TYPE  LTAP
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_ltak    TYPE ltak,
        ls_ltap    TYPE ltap,
        ls_lqua    TYPE lqua,
        ls_lquab   TYPE lquab,
        ls_message TYPE bdcmsgcoll.


  CLEAR: e_tanum_d, e_tapos_d, e_charg_d, es_ltak_d, es_ltap_d, et_messages.

** Checks Current TO
**********************************************************************
  DO 1 TIMES.
    SELECT SINGLE * FROM ltak
                    INTO ls_ltak
                    WHERE lgnum = i_lgnum AND
                          tanum = i_tanum_o.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM ltap
                    INTO ls_ltap
                    WHERE lgnum = i_lgnum AND
                          tanum = i_tanum_o AND
                          tapos = i_tapos_o.
  ENDDO.

  IF sy-subrc <> 0.
**  Tarefa &/& inválida
    CLEAR: ls_message.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'ZWMFR001'.
    ls_message-msgnr  = '068'.
    ls_message-msgv1  = i_tanum_o .
    ls_message-msgv2  = i_tapos_o.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.


** Checks Target SU
**********************************************************************
  SELECT SINGLE * FROM lqua
                  INTO ls_lqua
                  WHERE lgnum = i_lgnum AND
                        lenum = i_lenum_d.

  IF sy-subrc <> 0.
**  A UD & não existe
    CLEAR: ls_message.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'ZWMFR001'.
    ls_message-msgnr  = '069'.
    ls_message-msgv1  = i_lenum_d.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

** Cross Match
**********************************************************************
  IF ls_lqua-matnr <> ls_ltap-matnr.
**  Material & da UD & não é igual ao Material solicitado (&)
    CLEAR: ls_message.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'ZWMFR001'.
    ls_message-msgnr  = '070'.
    ls_message-msgv1  = ls_lqua-matnr.
    ls_message-msgv2  = i_lenum_d.
    ls_message-msgv3  = ls_ltap-matnr.
    APPEND ls_message TO et_messages.
    RAISE error.
  ELSEIF ls_lqua-gesme <> ls_ltap-vsolm.
**  Quantidade & da UD & não é igual à Quantidade solicitada
    CLEAR: ls_message.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'ZWMFR001'.
    ls_message-msgnr  = '071'.
    ls_message-msgv1  = ls_lqua-gesme.
    ls_message-msgv2  = i_lenum_d.
    ls_message-msgv3  = ls_ltap-vsolm.
    APPEND ls_message TO et_messages.
    RAISE error.
  ELSEIF ls_lqua-lgtyp <> ls_ltap-vltyp OR
         ls_lqua-lgpla <> ls_ltap-vlpla.
**  Posição & da UD & não é igual à Posição de Origem solicitada (&)
    CLEAR: ls_message.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'ZWMFR001'.
    ls_message-msgnr  = '072'.
    ls_message-msgv1  = ls_lqua-lgpla.
    ls_message-msgv2  = i_lenum_d.
    ls_message-msgv3  = ls_ltap-vlpla.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

** Checks Available Stock
**********************************************************************
  SELECT SINGLE * FROM lquab
                  INTO ls_lquab
                  WHERE lgnum = i_lgnum AND
                        matnr = ls_lqua-matnr AND
                        werks = ls_lqua-werks AND
                        bestq = ls_lqua-bestq AND
                        sobkz = ls_lqua-sobkz AND
                        charg = ls_lqua-charg AND
                        sonum = ls_lqua-sonum AND
                        lgtyp = ls_ltap-vltyp AND
                        lgpla = ls_ltap-vlpla AND
                        lgort = ls_ltap-lgort.

  IF ls_lquab IS INITIAL OR
     ls_lquab-menge <= 0.
**  Não existe stock disponivel para efetuar a troca para o Lote &
    CLEAR: ls_message.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'ZWMFR001'.
    ls_message-msgnr  = '073'.
    ls_message-msgv1  = ls_lqua-charg.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.




** Cancela TO
**********************************************************************
  CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
    EXPORTING
      warehouse     = i_lgnum
      tanum         = i_tanum_o
      tapos         = i_tapos_o
    TABLES
      return_msg    = et_messages
    EXCEPTIONS
      error_message = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.

** Cria Nova TO
**********************************************************************
  CALL FUNCTION 'ZWM_TO_CREATE_OUT'
    EXPORTING
      warehouse     = i_lgnum
      refnr         = ls_ltak-refnr
      vbeln         = ls_ltak-vbeln
      posnr         = ls_ltap-posnr
      vsola         = ls_ltap-nsolm
      meins         = ls_ltap-meins
      werks         = ls_ltap-werks
      lgort         = ls_ltap-lgort
      matnr         = ls_ltap-matnr
      su            = i_lenum_d
      vltyp         = ls_ltap-vltyp
      vlpla         = ls_ltap-vlpla
      charg         = ls_lqua-charg
    IMPORTING
      to            = e_tanum_d
    TABLES
      return_msg    = et_messages
    EXCEPTIONS
      error_message = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.

** Wait
**********************************************************************
  DO 20 TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    SELECT SINGLE * FROM ltak
                    INTO es_ltak_d
                    WHERE lgnum = i_lgnum AND
                          tanum = e_tanum_d.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM ltap
                    INTO es_ltap_d
                    WHERE lgnum = i_lgnum AND
                          tanum = e_tanum_d.

    CHECK sy-subrc EQ 0.
  ENDDO.

** Export
**********************************************************************
  e_tanum_d = es_ltap_d-tanum.
  e_tapos_d = es_ltap_d-tapos.
  e_charg_d = es_ltap_d-charg.
ENDFUNCTION.
