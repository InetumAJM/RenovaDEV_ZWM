FUNCTION zwm_create_idoc_lib_group.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"     REFERENCE(I_ADDIN) TYPE  LWMRREF_ADD
*"  CHANGING
*"     REFERENCE(CS_IDOC_CONTROL) TYPE  EDIDC
*"     REFERENCE(CT_LTAP_VB) TYPE  TT_LTAP_VB
*"     REFERENCE(CT_IDOC_DATA) TYPE  EDIDD_TT
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_zrrfxe     TYPE zrrfxe.
  DATA: ls_idoc_data  TYPE edidd.

** Criar IDOC
**********************************************************************
  CHECK ct_idoc_data[] IS NOT INITIAL.

** Criar Segmento adicional
  CLEAR ls_zrrfxe.
  ls_zrrfxe-zseq = i_addin-zzseq.

  CLEAR ls_idoc_data.
  ls_idoc_data-segnam = 'ZRRFXE'.
  ls_idoc_data-sdata  = ls_zrrfxe.
  APPEND ls_idoc_data TO ct_idoc_data.

** Extension
  IF ct_idoc_data[] IS NOT INITIAL.
    cs_idoc_control-cimtyp = 'ZWMRRIDE'.
  ENDIF.


ENDFUNCTION.
