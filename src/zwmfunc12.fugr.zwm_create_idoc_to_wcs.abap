FUNCTION zwm_create_idoc_to_wcs .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LTAK) TYPE  LTAK
*"  CHANGING
*"     REFERENCE(CS_IDOC_CONTROL) TYPE  EDIDC
*"     REFERENCE(CT_LTAP_VB) TYPE  TT_LTAP_VB
*"     REFERENCE(CT_IDOC_DATA) TYPE  EDIDD_TT
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_bwlvs   TYPE bwlvs.
  DATA: ls_ltap_vb TYPE ltap_vb.

** Obter Parametros
**********************************************************************
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_ltak-lgnum
      i_processo  = 'OT_DUMMY'
      i_parametro = 'MOV_WM_WCS'
    IMPORTING
      e_valor     = lv_bwlvs
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  READ TABLE ct_ltap_vb INDEX 1 INTO ls_ltap_vb.
  CHECK sy-subrc = 0.

** OT Entrada no WCS ou de chamada de aviso
**********************************************************************
  IF ( ls_ltap_vb-vltyp = 'CHD' )                              OR " Aviso de Entrada
     ( ls_ltap_vb-vltyp = 'EAU' AND ls_ltap_vb-nltyp = 'AUT' ) OR " Entrada Automático
     ( ls_ltap_vb-vltyp = 'EAU' AND ls_ltap_vb-nltyp = 'BPE' ) OR " Entrada Buffer Paletização Especial
     ( ls_ltap_vb-vltyp = 'EAU' AND ls_ltap_vb-nltyp = '933' ) OR " Entrada Palete Picking para Pulmão
     ( ls_ltap_vb-vltyp = 'EAU' AND ls_ltap_vb-nltyp = '815' ) OR " Entrada Palete Pal.ESP para Pulmão
     ( ls_ltap_vb-vltyp = 'EAU' AND ls_ltap_vb-nltyp = 'BPK' ) OR " Entrada Palete Picking para BPK
     ( ls_ltap_vb-vltyp = 'EAU' AND ls_ltap_vb-nltyp = 'PRO' ) OR " Entrada Palete para DITA2
     ( ls_ltap_vb-vltyp = 'BPE' AND ls_ltap_vb-nltyp = 'EPE' ) OR " Transferência Buffer Paletização Especial
     ( ls_ltap_vb-vltyp = '922' AND ls_ltap_vb-nltyp = 'AUT' ).   " Bloqueio/Desbloqueio

    CALL FUNCTION 'ZWM_CREATE_IDOC_TO_WCS_IN'
      EXPORTING
        i_ltak          = i_ltak
      CHANGING
        cs_idoc_control = cs_idoc_control
        ct_ltap_vb      = ct_ltap_vb
        ct_idoc_data    = ct_idoc_data
      EXCEPTIONS
        error           = 1
        OTHERS          = 2.

** OT Saida do AUT
**********************************************************************
  ELSE.

    CALL FUNCTION 'ZWM_CREATE_IDOC_TO_WCS_OUT'
      EXPORTING
        i_ltak          = i_ltak
      CHANGING
        cs_idoc_control = cs_idoc_control
        ct_ltap_vb      = ct_ltap_vb
        ct_idoc_data    = ct_idoc_data
      EXCEPTIONS
        error           = 1
        OTHERS          = 2.

  ENDIF.


ENDFUNCTION.
