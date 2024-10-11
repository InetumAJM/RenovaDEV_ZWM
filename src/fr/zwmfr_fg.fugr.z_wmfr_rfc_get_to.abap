FUNCTION z_wmfr_rfc_get_to.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     VALUE(I_SSCC) TYPE  ZWM_AUX-SSCC
*"     VALUE(I_TABLE) TYPE  ZWM_AUX-MESA
*"  EXPORTING
*"     VALUE(E_SSCC) TYPE  ZWM_AUX-SSCC
*"     VALUE(E_TO) TYPE  LTAK-TANUM
*"     VALUE(E_RETURN) TYPE  ZWM_AUX-RETORNO
*"     VALUE(R_RETURN_MSG) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  DATA lv_lenum TYPE lenum.
  DATA lv_bwlvs TYPE ltak-bwlvs.
  DATA lv_lgtyp TYPE lagp-lgtyp.
  DATA lv_lgpla TYPE lagp-lgpla.
  DATA lv_lzone TYPE lagp-lzone.

  DATA ls_xuser   TYPE lrf_wkqu.
  DATA ls_zwm013  TYPE zwm013.
  DATA ls_zwm020  TYPE zwm020.

  DATA lt_zwm001  TYPE STANDARD TABLE OF zwm001.

  FIELD-SYMBOLS <fs_zwm001>   LIKE LINE OF gt_zwm001[].
*"----------------------------------------------------------------------
  FREE gt_zwm001[].

  PERFORM f_data_init_get_user_data CHANGING ls_xuser e_return. " get user RF data for WM
  IF e_return NE 0.
    r_return_msg = 'Utilizador sem atribuição de Armazém'.
    RETURN.
  ENDIF.

  IF NOT i_lgnum IS INITIAL.
    ls_xuser-lgnum = i_lgnum.
  ENDIF.

  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs       = ls_xuser-lgnum
    TABLES
      ti_zwm001 = lt_zwm001[].
  LOOP AT lt_zwm001[] ASSIGNING <fs_zwm001>.
    INSERT <fs_zwm001> INTO TABLE gt_zwm001[].
  ENDLOOP.

  e_sscc = i_sscc.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = e_sscc
    IMPORTING
      output = lv_lenum.

  IF i_table IS INITIAL.
    e_return = 41.  " Mesa de Destino Vazia
    r_return_msg = 'Mesa de Destino Vazia'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM zwm013 INTO ls_zwm013
    WHERE armazem EQ ls_xuser-lgnum
      AND sscc EQ lv_lenum.
  IF sy-subrc NE 0.
    e_return = 50. " SSCC não existe
    r_return_msg = 'SSCC não existe'.
    RETURN.
  ENDIF.

  IF ls_zwm013-destino NE i_table.
** Elimina o registo da tabela zwm013
    DELETE zwm013 FROM ls_zwm013.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    e_return = 40. " Mesa errada
    r_return_msg = 'Mesa errada'.
    RETURN.
  ENDIF.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_wmentry
                   parametro  = c_param_bwart_wm
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_bwlvs = <fs_zwm001>-valor.
  ENDIF.

  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = i_table
    IMPORTING
      lgtyp = lv_lgtyp
      lgpla = lv_lgpla.

  SELECT SINGLE lzone
    FROM lagp INTO lv_lzone
    WHERE lgnum EQ ls_xuser-lgnum
      AND lgtyp EQ lv_lgtyp
      AND lgpla EQ lv_lgpla.

  SELECT * UP TO 1 ROWS
    FROM zwm020 INTO ls_zwm020
    WHERE armazem EQ ls_xuser-lgnum
      AND p1 EQ i_sscc.
  ENDSELECT.
  IF sy-subrc EQ 0. " palete remontada
    SELECT SINGLE *
      FROM zwm013 INTO ls_zwm013
      WHERE armazem EQ ls_xuser-lgnum
        AND sscc EQ ls_zwm020-p1.

    PERFORM f_get_to_reass_pall USING ls_xuser ls_zwm020 ls_zwm013 lv_lgpla lv_lgtyp lv_lzone lv_bwlvs CHANGING e_return e_to r_return_msg.
  ELSE. " palete não remontada
    PERFORM f_get_to_non_reass_pall USING ls_xuser i_sscc ls_zwm013 lv_lgpla lv_lgtyp lv_lzone lv_bwlvs CHANGING e_return e_to r_return_msg.
  ENDIF.
ENDFUNCTION.
