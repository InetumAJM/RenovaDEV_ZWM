FUNCTION zwm_gcc_bloqueio_porta_sscc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_PORTA) TYPE  ZWM002-PORTA
*"     REFERENCE(I_BLOQ) TYPE  BKK_YESNO OPTIONAL
*"----------------------------------------------------------------------

  IF i_bloq EQ abap_false.
    DATA(lc_bloq) = zcl_wm_gestao_cais_de_carga=>cs_bloqueio_sscc-desbloquear.
  ELSE.
    lc_bloq = zcl_wm_gestao_cais_de_carga=>cs_bloqueio_sscc-bloquear.
  ENDIF.

  "Bloqueio Porta (Bloq SSCC incorreto) X=Bloq. / ""->Desb.
  zcl_wm_gestao_cais_de_carga=>set_bloqueio_porta_sscc(
    EXPORTING
      porta    = i_porta    " Portão para o nº do depósito
      bloqueio = lc_bloq "i_bloq    " Campo Sim/Não
  ).

ENDFUNCTION.
