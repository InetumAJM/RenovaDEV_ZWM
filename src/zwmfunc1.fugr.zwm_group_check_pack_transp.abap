FUNCTION zwm_group_check_pack_transp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"  EXPORTING
*"     REFERENCE(E_PACKTRANP) TYPE  FLAG
*"----------------------------------------------------------------------

  CLEAR: e_packtranp.

** EMBALA SEPRE NO DOCUMENTO DE TRANSPORTE SE NÃO EMBALAR NA REMESSA
** NA NECESSIDADE DE SE QUERER CRITÉRIOS PARA NÃO EMBALAR EM DOC TRANSP
** USAR ESTE MODULO DE FUNÇÃO PARA INSERIR O CODIGO

  e_packtranp = abap_true.




ENDFUNCTION.
