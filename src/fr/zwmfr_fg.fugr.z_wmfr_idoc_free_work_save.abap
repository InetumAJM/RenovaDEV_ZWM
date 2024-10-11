FUNCTION z_wmfr_idoc_free_work_save.
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM
*"     VALUE(I_REFNR) TYPE  LVS_REFNR
*"     VALUE(I_VBELN) TYPE  VBELN OPTIONAL
*"     VALUE(I_FINAL) TYPE  FLAG OPTIONAL
*"     VALUE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"----------------------------------------------------------------------
  DATA: lv_activated TYPE flag.

** valida se Deve Lançar IDOC
***********************************************************************
  SELECT SINGLE valor FROM zwm001
                      INTO lv_activated
                      WHERE armazem   = i_lgnum AND
                            processo  = 'LIBERACAO_VIA_IDOC' AND
                            parametro = 'ACTIVAR'.

  CHECK lv_activated EQ abap_true.


** Persistencia
***********************************************************************

**   MODIFY TABLE.

  IF i_commit EQ abap_false.
    EXIT.
  ENDIF.

  COMMIT WORK.
ENDFUNCTION.
