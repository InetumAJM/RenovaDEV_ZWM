FUNCTION zwm_insert_error.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  ZWM015-ARMAZEM
*"     REFERENCE(INCIDENCIA) LIKE  ZWM015-INCIDENCIA
*"     REFERENCE(POSICAO) LIKE  ZWM015-POSICAO OPTIONAL
*"     REFERENCE(SSCC) LIKE  ZWM015-SSCC OPTIONAL
*"     REFERENCE(TANUM) LIKE  LTAP-TANUM OPTIONAL
*"     REFERENCE(TAPOS) LIKE  LTAP-TAPOS OPTIONAL
*"  EXCEPTIONS
*"      NO_COMMIT
*"----------------------------------------------------------------------

  IF NOT armazem IS INITIAL AND NOT incidencia IS INITIAL.

    zwm015-armazem = armazem.
    zwm015-utilizador = sy-uname.
    zwm015-data = sy-datum.
    zwm015-hora = sy-uzeit.
    zwm015-incidencia = incidencia.

    IF NOT posicao IS INITIAL.
      zwm015-posicao = posicao.
    ENDIF.

    IF NOT sscc IS INITIAL.
      zwm015-sscc = sscc.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = tanum
      IMPORTING
        output = zwm015-tanum.

    zwm015-tapos = tapos.

    MODIFY zwm015.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      RAISE no_commit.
      ROLLBACK WORK.
    ENDIF.

  ENDIF.

ENDFUNCTION.
