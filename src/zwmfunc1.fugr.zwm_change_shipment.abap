FUNCTION zwm_change_shipment.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MATRICULA) LIKE  ZWM006-MATRICULA OPTIONAL
*"     REFERENCE(TIPO_CAMIAO) LIKE  ZWM006-TIPO_CAMIAO OPTIONAL
*"     REFERENCE(N_TRANSPORTE) LIKE  ZWM006-N_TRANSPORTE
*"     REFERENCE(TRANSPORTADOR) LIKE  ZWM006-TRANSPORTADOR OPTIONAL
*"     REFERENCE(DAREG) TYPE  VTTK-DAREG OPTIONAL
*"     REFERENCE(UAREG) TYPE  VTTK-UAREG OPTIONAL
*"     REFERENCE(DALBG) LIKE  VTTK-DALBG OPTIONAL
*"     REFERENCE(UALBG) LIKE  VTTK-UALBG OPTIONAL
*"     REFERENCE(DALEN) LIKE  VTTK-DALEN OPTIONAL
*"     REFERENCE(UALEN) LIKE  VTTK-UALEN OPTIONAL
*"     REFERENCE(DATBG) TYPE  VTTK-DATBG OPTIONAL
*"     REFERENCE(UATBG) TYPE  VTTK-UATBG OPTIONAL
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      SHIPMENT_DOES_NOT_EXIST
*"----------------------------------------------------------------------
  DATA : upd_vttk LIKE vttk OCCURS 0 WITH HEADER LINE.

  CLEAR : upd_vttk, vttk.
  REFRESH: upd_vttk.


** Verificar se existe transporte a nivel de cabecalho
  SELECT SINGLE * FROM vttk
                  WHERE tknum = n_transporte.
  IF sy-subrc <> 0.
    return_msg-msgtyp = 'I'.
    return_msg-msgspra = sy-langu.
    return_msg-msgid = 'ZWMMSG001'.
    return_msg-msgnr = '174'.
    return_msg-msgv1 = n_transporte.
    APPEND return_msg.
    CLEAR return_msg.
    RAISE shipment_does_not_exist.
  ELSE.

** a header line ficou preenchida com os dados antigos vamos apenas
** alterar o necessario
    MOVE-CORRESPONDING vttk TO upd_vttk.
    IF NOT matricula IS INITIAL.
      MOVE matricula TO upd_vttk-signi.
    ENDIF.
    IF NOT transportador IS INITIAL.
      MOVE transportador TO upd_vttk-tdlnr.
    ENDIF.
    IF NOT tipo_camiao IS INITIAL.
      MOVE tipo_camiao TO upd_vttk-sdabw.
    ENDIF.
** Datas e Horas
** Data e Hora de Chegada á Portaria
    IF NOT dareg IS INITIAL.
      MOVE dareg TO upd_vttk-dareg.
    ENDIF.
    IF NOT uareg IS INITIAL.
      MOVE uareg TO upd_vttk-uareg.
    ENDIF.
** Data e Hora de Chegada á Porta
    IF NOT dalbg IS INITIAL.
      MOVE dalbg TO upd_vttk-dalbg.
    ENDIF.
    IF NOT ualbg IS INITIAL.
      MOVE ualbg TO upd_vttk-ualbg.
    ENDIF.
** Data e Hora de Saida da Porta e Portaria
    IF NOT dalen IS INITIAL.
      MOVE dalen TO upd_vttk-dalen.
    ENDIF.
    IF NOT ualen IS INITIAL.
      MOVE ualen TO upd_vttk-ualen.
    ENDIF.

    IF NOT datbg IS INITIAL.
      MOVE datbg TO upd_vttk-datbg.
    ENDIF.
    IF NOT uatbg IS INITIAL.
      MOVE uatbg TO upd_vttk-uatbg.
    ENDIF.
    APPEND upd_vttk.

  ENDIF.

** Efectuar as alterações
  IF NOT upd_vttk[] IS INITIAL.

    CALL FUNCTION 'RV_SHIPMENT_UPDATE_DB'
     EXPORTING
       i_flag_tra_complete       = 'X'
*   I_TRA_SAVE_CALLER         =
      TABLES
       db_vttk_upd               = upd_vttk.

  ENDIF.


ENDFUNCTION.
