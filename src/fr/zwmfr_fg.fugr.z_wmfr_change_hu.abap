FUNCTION z_wmfr_change_hu.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LENUM) TYPE  LENUM
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_LGORT) TYPE  LGORT_D
*"     REFERENCE(I_LGORT_HU) TYPE  HU_LGORT OPTIONAL
*"     REFERENCE(I_STATUS) TYPE  HU_STATUS OPTIONAL
*"     REFERENCE(I_VPOBJ) TYPE  VPOBJ OPTIONAL
*"     REFERENCE(I_FREE_VPOBJKEY) TYPE  FLAG DEFAULT ABAP_TRUE
*"  EXPORTING
*"     REFERENCE(ET_RETURN) TYPE  HUITEM_MESSAGES_T
*"----------------------------------------------------------------------
  DATA lv_exidv TYPE exidv.

  DATA ls_vekp    TYPE vekp.
  DATA ls_hum_mm  TYPE hum_plant_stloc.
  DATA ls_object  TYPE hum_object.
  DATA ls_item    TYPE huitem_change.

  DATA lt_hu_identifications TYPE STANDARD TABLE OF hum_exidv.
  DATA lt_internal_numbers   TYPE STANDARD TABLE OF hum_venum.
  DATA lt_changed            TYPE hum_update_header_t.
  DATA lt_vepo               TYPE STANDARD TABLE OF vepo.

  FIELD-SYMBOLS <fs_hu_identification> LIKE LINE OF lt_hu_identifications[].
  FIELD-SYMBOLS <fs_changed>           LIKE LINE OF lt_changed[].
  FIELD-SYMBOLS <fs_vepo>              LIKE LINE OF lt_vepo[].

*"----------------------------------------------------------------------
  SELECT * UP TO 1 ROWS
    FROM vekp INTO ls_vekp
    WHERE exidv EQ i_lenum  " Index VEKP~C
      AND status NE c_deleted.
  ENDSELECT.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'HU_PACKING_REFRESH'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_lenum
    IMPORTING
      output = lv_exidv.

  IF i_lenum IS NOT INITIAL.
    ls_hum_mm-plant     = ls_vekp-werks.
    ls_hum_mm-stge_loc  = ls_vekp-lgort.
    ls_hum_mm-whse_no   = ls_vekp-lgnum.

    FREE lt_hu_identifications[].

    APPEND INITIAL LINE TO lt_hu_identifications[] ASSIGNING <fs_hu_identification>.
    <fs_hu_identification>-exidv  = lv_exidv.

    CALL FUNCTION 'HU_INITIALIZE_PACKING'
      EXPORTING
        is_object             = ls_object
        is_plant_stloc        = ls_hum_mm
        it_internal_numbers   = lt_internal_numbers[]
        it_hu_identifications = lt_hu_identifications[]
      IMPORTING
        et_messages           = et_return[]
      EXCEPTIONS
        not_possible          = 1
        OTHERS                = 2.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      RETURN.
    ENDIF.
  ENDIF.

  APPEND INITIAL LINE TO lt_changed[] ASSIGNING <fs_changed>.
  <fs_changed>-hdl_unit_itid  = ls_vekp-venum.
  <fs_changed>-hdl_unit_exid  = lv_exidv.
  <fs_changed>-field_name     = 'WERKS'.
  <fs_changed>-field_value    = i_werks.

  APPEND INITIAL LINE TO lt_changed[] ASSIGNING <fs_changed>.
  <fs_changed>-hdl_unit_itid  = ls_vekp-venum.
  <fs_changed>-hdl_unit_exid  = lv_exidv.
  <fs_changed>-field_name     = 'LGORT'.
  <fs_changed>-field_value    = i_lgort.

  IF i_lgort_hu IS SUPPLIED.
    APPEND INITIAL LINE TO lt_changed[] ASSIGNING <fs_changed>.
    <fs_changed>-hdl_unit_itid  = ls_vekp-venum.
    <fs_changed>-hdl_unit_exid  = lv_exidv.
    <fs_changed>-field_name     = 'HU_LGORT'.
    <fs_changed>-field_value    = i_lgort_hu.
  ENDIF.

  IF i_status IS SUPPLIED.
    IF i_status IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_changed[] ASSIGNING <fs_changed>.
      <fs_changed>-hdl_unit_itid  = ls_vekp-venum.
      <fs_changed>-hdl_unit_exid  = lv_exidv.
      <fs_changed>-field_name     = 'STATUS'.
      <fs_changed>-field_value    = i_status.
    ENDIF.
  ENDIF.

  IF i_vpobj IS SUPPLIED.
    IF i_vpobj IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_changed[] ASSIGNING <fs_changed>.
      <fs_changed>-hdl_unit_itid  = ls_vekp-venum.
      <fs_changed>-hdl_unit_exid  = lv_exidv.
      <fs_changed>-field_name     = 'VPOBJ'.
      <fs_changed>-field_value    = i_vpobj.
    ENDIF.
  ENDIF.

  IF i_free_vpobjkey EQ abap_true.
    APPEND INITIAL LINE TO lt_changed[] ASSIGNING <fs_changed>.
    <fs_changed>-hdl_unit_itid  = ls_vekp-venum.
    <fs_changed>-hdl_unit_exid  = lv_exidv.
    <fs_changed>-field_name     = 'VPOBJKEY'.
    <fs_changed>-field_value    = space.
  ENDIF.

  CALL FUNCTION 'HU_HEADER_UPDATE'
    EXPORTING
      it_new_values = lt_changed[]
    IMPORTING
      et_messages   = et_return[]
    EXCEPTIONS
      not_possible  = 1
      OTHERS        = 2.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    RETURN.
  ENDIF.

  SELECT *
    FROM vepo INTO TABLE lt_vepo[]
    WHERE venum EQ ls_vekp-venum.

  LOOP AT lt_vepo[] ASSIGNING <fs_vepo>.
    CLEAR ls_item.

    ls_item-venum         = <fs_vepo>-venum.
    ls_item-vepos         = <fs_vepo>-vepos.
    ls_item-change_werks  = abap_true.
    ls_item-change_lgort  = abap_true.
    ls_item-werks         = i_werks.
    ls_item-lgort         = i_lgort.

    CALL FUNCTION 'HU_ITEM_CHANGE'
      EXPORTING
        is_item = ls_item
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      RETURN.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'HU_POST'
    EXPORTING
      if_synchron   = abap_true
      if_commit     = abap_false
      is_object     = ls_object
    IMPORTING
      et_messages   = et_return[]
    EXCEPTIONS
      error_message = 99.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.
ENDFUNCTION.
