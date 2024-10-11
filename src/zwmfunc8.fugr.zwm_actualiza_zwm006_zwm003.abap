FUNCTION zwm_actualiza_zwm006_zwm003.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGNUM) LIKE  T311A-LGNUM
*"     REFERENCE(TKNUM) LIKE  VTTK-TKNUM
*"     REFERENCE(SIGNI) LIKE  VTTK-SIGNI
*"----------------------------------------------------------------------

  TABLES: zwm006_aux, zwm003_aux.

  DATA: itab_06 LIKE zwm006_aux OCCURS 0 WITH HEADER LINE.

  DATA: l_tknum LIKE vttp-tknum,
        wa_06   LIKE zwm006_aux,
        wa_03   LIKE zwm003_aux.

  FREE: itab_06.
  CLEAR: itab_06.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = tknum
    IMPORTING
      output = l_tknum.

  CLEAR: zwm006_aux.
  SELECT * FROM zwm006_aux
  INTO CORRESPONDING FIELDS OF TABLE itab_06
  WHERE armazem      EQ lgnum
    AND n_transporte EQ l_tknum.

  IF sy-subrc EQ 0.
    LOOP AT itab_06 INTO wa_06.
      wa_06-matricula = signi.
      MODIFY zwm006_aux FROM wa_06.

      CLEAR: zwm003_aux.
      SELECT * FROM zwm003_aux
      WHERE armazem EQ lgnum
        AND num_entrada EQ wa_06-num_entrada.
        CLEAR: wa_03.
        MOVE-CORRESPONDING zwm003_aux TO wa_03.
        wa_03-matricula = signi.
        MODIFY zwm003_aux FROM wa_03.
      ENDSELECT.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
