FUNCTION zwm_customer_max_volume.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_KUNNR) TYPE  KUNNR
*"  CHANGING
*"     REFERENCE(C_VOL_PAL) OPTIONAL
*"     REFERENCE(C_VOL_MPAL) OPTIONAL
*"----------------------------------------------------------------------

  DATA: ls_zwm071    TYPE zwm071.
*        ls_mara_pal  TYPE mara,
*        ls_mara_mpal TYPE mara.

*  DATA: matnr_pal  TYPE matnr,
*        matnr_mpal TYPE matnr.

** Dados de Paletes
***********************************************************************
  IF matnr_pal IS INITIAL.
    PERFORM get_parameter
            USING i_lgnum
                  'CALCULO_VOLUME'
                  'VOLUME_PAL'
                  matnr_pal.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = matnr_pal
      IMPORTING
        output = matnr_pal.
  ENDIF.

  IF matnr_mpal IS INITIAL.
    PERFORM get_parameter
            USING i_lgnum
                  'CALCULO_VOLUME'
                  'VOLUME_MEIA_PAL'
                  matnr_mpal.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = matnr_mpal
      IMPORTING
        output = matnr_mpal.
  ENDIF.


  IF ls_mara_pal IS INITIAL.
    CLEAR: mara.
    CLEAR: ls_mara_pal.
    SELECT SINGLE * FROM mara
                    INTO ls_mara_pal
                    WHERE matnr = matnr_pal.
  ENDIF.


  IF ls_mara_mpal IS INITIAL.
    CLEAR: ls_mara_mpal.
    SELECT SINGLE * FROM mara
                    INTO ls_mara_mpal
                    WHERE matnr = matnr_mpal.
  ENDIF.

** Dados de Cliente
***********************************************************************
*  SELECT SINGLE * FROM zwm071
*                  INTO ls_zwm071
*                  WHERE lgnum = i_lgnum AND
*                        kunnr = i_kunnr.
  IF gt_zwm071 IS INITIAL.
    SELECT *
      FROM zwm071
      INTO TABLE gt_zwm071.
    SORT gt_zwm071 BY lgnum kunnr.
  ENDIF.
  READ TABLE gt_zwm071 INTO ls_zwm071
    WITH KEY lgnum = i_lgnum
             kunnr = i_kunnr
             BINARY SEARCH.

  CHECK sy-subrc EQ 0.

  IF NOT ls_zwm071-vol_pal IS INITIAL.
    c_vol_pal = ls_zwm071-vol_pal - ls_mara_pal-volum.
  ENDIF.

  IF NOT ls_zwm071-vol_mpal IS INITIAL.
    c_vol_mpal = ls_zwm071-vol_mpal - ls_mara_mpal-volum.
  ENDIF.

ENDFUNCTION.
