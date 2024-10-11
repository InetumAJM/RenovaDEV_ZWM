FUNCTION ZWM_LM_CONTACT_ADDRESS_DISPLAY.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_VBELN) LIKE  ZWMOV-VBELN
*"             VALUE(IF_PARNR) LIKE  VBPA-PARNR
*"       EXCEPTIONS
*"              INITIAL_NUMBER
*"              READ_ERROR
*"--------------------------------------------------------------------
  DATA: LS_ADRS   LIKE  ADRS,
        LS_VBPA   LIKE  VBPA,
        LS_VBADR  LIKE  VBADR,
        LF_ADRNR  LIKE  VBPA-ADRNR,
        LF_ADRNP  LIKE  VBPA-ADRNP.

  IF IF_PARNR IS INITIAL.
    RAISE INITIAL_NUMBER.
  ENDIF.
  SELECT ADRNR ADRNP FROM VBPA
           INTO (LF_ADRNR, LF_ADRNP)
           WHERE VBELN EQ IF_VBELN
           AND   POSNR EQ POSNR_INITIAL
           AND   PARNR EQ IF_PARNR.
    EXIT.
  ENDSELECT.
  IF SY-SUBRC NE 0.
    RAISE READ_ERROR.
  ENDIF.
  LS_VBPA-PARNR = IF_PARNR.
  LS_VBPA-ADRNR = LF_ADRNR.
  LS_VBPA-ADRNP = LF_ADRNP.
  CALL FUNCTION 'VIEW_VBADR'
       EXPORTING
            INPUT   = LS_VBPA
            NRART   = 'AP'
       IMPORTING
            ADRESSE = LS_VBADR.
  MOVE-CORRESPONDING LS_VBADR TO LS_ADRS.

  CALL FUNCTION 'RV_ADDRESS_WINDOW_DISPLAY'
       EXPORTING
            ADRSWA_IN = LS_ADRS
            FADRTYPE  = '2'.

ENDFUNCTION.
