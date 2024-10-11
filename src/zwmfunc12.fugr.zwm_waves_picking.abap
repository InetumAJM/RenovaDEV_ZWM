FUNCTION zwm_waves_picking .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_paletes TYPE TABLE OF zpalete_picking.
  DATA: lt_zwm028  TYPE TABLE OF zwm028.

  DATA: ls_zwm028 TYPE zwm028.

  DATA:   cnt_selekt   TYPE i.

  DATA: lv_subrc TYPE sysubrc.

  FIELD-SYMBOLS: <ls_it311> LIKE LINE OF it311.

  CLEAR: cnt_selekt, gv_subrc.

** Validar se Grupo já foi liberado para automático.
  gv_lgnum = i_lgnum.

  SELECT *
    FROM zwm028 INTO TABLE lt_zwm028
    WHERE lgnum = i_lgnum AND
          refnr = i_refnr.

  LOOP AT lt_zwm028 INTO ls_zwm028 WHERE free_idoc IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    " O Grupo & já foi liberado para o Armazém Automático (WCS).
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '377' WITH i_refnr.
    EXIT.
  ENDIF.

  SELECT * FROM t311
           INTO TABLE it311
           WHERE lgnum = i_lgnum AND
                 refnr = i_refnr.

  LOOP AT it311 ASSIGNING <ls_it311>.

    CASE <ls_it311>-rbtyp.
      WHEN con_refnrbelegtyp_b.
        <ls_it311>-tbedn = con_x.             "Transportbedarf
        sum00-rbtyp = con_refnrbelegtyp_b.
        sum00-rtext = text-a30.
        COLLECT sum00.
        sum00-rbtyp = high_value.        "Gesamtsumme fortschreiben.
        sum00-sort = sort_z.
        sum00-rtext = text-a32.
        COLLECT sum00.
      WHEN con_refnrbelegtyp_l.
        <ls_it311>-liefn = con_x.             "Lieferung
        sum00-rbtyp = con_refnrbelegtyp_l.
        sum00-rtext = text-a31.
        COLLECT sum00.
        sum00-rbtyp = high_value.        "Gesamtsumme fortschreiben.
        sum00-sort = sort_z.
        sum00-rtext = text-a32.
        COLLECT sum00.
      WHEN OTHERS.
    ENDCASE.

    <ls_it311>-kreuz = abap_true.

  ENDLOOP.

  READ TABLE it311
       INDEX 1.

  sav_it311 = it311.

  DATA: result_tab TYPE match_result_tab,
        lv_rsp.
  CLEAR: result_tab, lv_rsp. REFRESH: result_tab.

  LOOP AT it311 WHERE kreuz = con_x.
    FIND FIRST OCCURRENCE OF 'XX' IN it311-refnt
         IGNORING CASE RESULTS result_tab.
  ENDLOOP.
  IF NOT result_tab[] IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        defaultoption  = 'N'
        textline1      = 'Grupo não está completo. Pretende continuar?'
        titel          = 'Confirmação de Desbloqueio.'
        cancel_display = ' '
      IMPORTING
        answer         = lv_rsp.

    IF lv_rsp NE 'J'.
      EXIT.
    ENDIF.
  ENDIF.

***  Criar Paletes de Picking
  REFRESH range_refnr.
  LOOP AT it311.
    CLEAR range_refnr.
    MOVE:
          it311-refnr   TO range_refnr-low,
          con_sign_i    TO range_refnr-sign,
          con_option_eq TO range_refnr-option.
    APPEND range_refnr.
  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE ID 'L5' TYPE 'W' NUMBER 090. EXIT.
    sy-ucomm = space.
    EXIT.
  ENDIF.

** Validar Criação de Palete de picking
**********************************************************************
*  SUBMIT zwmrep0017 WITH s_grupo IN range_refnr
*                    WITH p_noalv = abap_false
*                    AND RETURN.

  SUBMIT zwmrep0017 WITH s_grupo IN range_refnr
                    WITH p_noalv = abap_true
                    AND RETURN.


  CLEAR ls_zwm028.
  SELECT SINGLE * FROM zwm028
                  INTO ls_zwm028
                  WHERE lgnum = i_lgnum AND
                        refnr = i_refnr.

  IF ls_zwm028-transporte IS INITIAL.
    MESSAGE ID 'ZWMMSG001' TYPE 'I'
        NUMBER '212' WITH it311-refnr.
    EXIT.
  ENDIF.

  IF z_wm_cl_management=>is_group_completed( i_lgnum = i_lgnum i_refnr = i_refnr ) EQ abap_true.
    IF ls_zwm028-zlock IS INITIAL OR ls_zwm028-zlock EQ '1'.
      PERFORM freigabe USING i_lgnum i_refnr.
      EXIT.
    ELSE.
*     As remessas do grupo & já tem todas as to´s criadas.
      MESSAGE i210(zwmmsg001) WITH i_refnr.
      RAISE error.
    ENDIF.
  ENDIF.

  IF z_wm_cl_management=>is_reabastecimento_running( i_lgnum = i_lgnum i_refnr = i_refnr ) EQ abap_true.
**  Reabastecimento em curso
    MESSAGE i360(zwmmsg001) WITH i_refnr.
    RAISE error.
  ENDIF.


  PERFORM update_picking CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.
  CLEAR flag.
  PERFORM verify_picking_quantity CHANGING lt_paletes.
  IF flag IS INITIAL.
    PERFORM sammelgang_starten CHANGING lt_paletes.
  ENDIF.

** Liberação
***********************************************************************
  CHECK z_wm_cl_management=>is_group_completed( i_lgnum = i_lgnum i_refnr = i_refnr ) EQ abap_true.

  PERFORM freigabe USING i_lgnum i_refnr.
ENDFUNCTION.
