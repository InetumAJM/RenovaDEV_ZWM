FUNCTION z_wmfr_idoc_change_grp_prior.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"     REFERENCE(I_PRIOR)
*"----------------------------------------------------------------------
  DATA: lt_ltap	TYPE TABLE OF ltap_vb,
        lt_ltak	TYPE TABLE OF ltak.

  DATA: ls_zwm028 TYPE zwm028,
        ls_zwm001 TYPE zwm001,
        ls_addin  TYPE lwmrref_add.

  DATA: lv_activated TYPE flag,
        lv_prior     TYPE lwmrref_add,
        lv_zsyst     TYPE recvsystem.

  FIELD-SYMBOLS: <lt_comm_idoc_control> TYPE ANY TABLE,
                 <ls_comm_idoc_control> TYPE  edidc.


** valida se Deve Lançar IDOC
***********************************************************************
  SELECT SINGLE valor FROM zwm001
                      INTO lv_activated
                      WHERE armazem   = i_lgnum AND
                            processo  = 'LIBERACAO_VIA_IDOC' AND
                            parametro = 'ACTIVAR'.

  CHECK lv_activated EQ abap_true.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_zsyst
                      WHERE armazem   = i_lgnum AND
                            processo  = 'ENVIO_IDOC_DEST' AND
                            parametro = 'EDIDC_N_PARCEIRO'.


  SELECT SINGLE * FROM zwm028
                  INTO ls_zwm028
                  WHERE lgnum = i_lgnum AND
                        refnr = i_refnr AND
                        remessa = ''.

  CHECK sy-subrc EQ 0.

** Valores validos de Lock
***********************************************************************
  SELECT SINGLE valor FROM zwm001
                      INTO ls_zwm001
                      WHERE armazem   = i_lgnum AND
                            processo  = 'LIBERACAO_VIA_IDOC' AND
                            parametro = 'ZLOCK_OK' AND
                            valor >= ls_zwm028-zlock.
  CHECK sy-subrc EQ 0.

  ls_addin-l2kso = i_prior.

  SET UPDATE TASK LOCAL.

  CALL FUNCTION 'L_IDOC_CREATE_WMRRID01'
    EXPORTING
      i_zsyst = lv_zsyst
      i_varia = ''
      i_lgnum = i_lgnum
      i_refnr = i_refnr
      i_addin = ls_addin
    TABLES
      t_ltap  = lt_ltap
      t_ltak  = lt_ltak.

  ASSIGN ('(SAPLLIDO)COMM_IDOC_CONTROL[]') TO <lt_comm_idoc_control>.
  DO 1 TIMES.
    CHECK <lt_comm_idoc_control> IS ASSIGNED.

    LOOP AT <lt_comm_idoc_control> ASSIGNING <ls_comm_idoc_control>.
      IF <ls_comm_idoc_control>-outmod = '1' OR
         <ls_comm_idoc_control>-outmod = '2'.
        CALL FUNCTION 'EDI_DOCUMENT_DEQUEUE_LATER'
          EXPORTING
            docnum                 = <ls_comm_idoc_control>-docnum
          EXCEPTIONS
            idoc_is_not_to_dequeue = 1
            OTHERS                 = 2.
      ENDIF.
    ENDLOOP.
  ENDDO.


  COMMIT WORK AND WAIT.
ENDFUNCTION.
