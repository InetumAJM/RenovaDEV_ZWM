FUNCTION zwm_update_pos_ent_wcs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"     REFERENCE(I_SU) TYPE  LENUM
*"     REFERENCE(I_LGPLA) TYPE  LGPLA
*"     REFERENCE(I_ETIQ) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_ENFA) TYPE  FLAG OPTIONAL
*"----------------------------------------------------------------------
  DATA: ls_zwm077 TYPE zwm077.

  GET TIME.

  CLEAR ls_zwm077.
  ls_zwm077-lgnum = i_lgnum.
  ls_zwm077-exidv = i_su.
  ls_zwm077-tanum = i_tanum.
  ls_zwm077-lgpla = i_lgpla.
  ls_zwm077-rdatu = sy-datum.
  ls_zwm077-rzeit = sy-uzeit.
  ls_zwm077-rname = sy-uname.
  ls_zwm077-zetiq = i_etiq.
  ls_zwm077-zenfa = i_enfa.

  MODIFY zwm077 FROM ls_zwm077.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFUNCTION.
