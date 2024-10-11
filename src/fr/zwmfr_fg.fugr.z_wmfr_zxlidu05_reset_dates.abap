FUNCTION z_wmfr_zxlidu05_reset_dates.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_STDAT) TYPE  LTAK-STDAT
*"     REFERENCE(I_ENDAT) TYPE  LTAK-ENDAT
*"     REFERENCE(I_STUZT) TYPE  LTAK-STUZT
*"     REFERENCE(I_ENUZT) TYPE  LTAK-ENUZT
*"  EXPORTING
*"     REFERENCE(E_STDAT) TYPE  LTAK-STDAT
*"     REFERENCE(E_ENDAT) TYPE  LTAK-ENDAT
*"     REFERENCE(E_STUZT) TYPE  LTAK-STUZT
*"     REFERENCE(E_ENUZT) TYPE  LTAK-ENUZT
*"----------------------------------------------------------------------

  IF i_stdat EQ space.
    e_stdat = '00000000'.
  ELSE.
    e_stdat = i_stdat.
  ENDIF.

  IF i_endat EQ space.
    e_endat = '00000000'.
  ELSE.
    e_endat = i_endat.
  ENDIF.

  IF i_stuzt EQ space.
    e_stuzt = '000000'.
  ELSE.
    e_stuzt = i_stuzt.
  ENDIF.

  IF i_enuzt EQ space.
    e_enuzt = '000000'.
  ELSE.
    e_enuzt = i_enuzt.
  ENDIF.
ENDFUNCTION.
