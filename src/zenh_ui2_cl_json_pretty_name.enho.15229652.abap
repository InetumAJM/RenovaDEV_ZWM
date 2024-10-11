"Name: \TY:/UI2/CL_JSON\ME:PRETTY_NAME\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_UI2_CL_JSON_PRETTY_NAME.
*
  IF zcl_wm_gestao_cais_de_carga=>is_enh_pretty_name_active( ) EQ abap_true.
    out = zcl_wm_gestao_cais_de_carga=>get_pretty_name(
      EXPORTING
        in                  = in    " Text to pretty print (ABAP name)
      CHANGING
        mt_name_mappings    = mt_name_mappings       " Código de uma posição
        mt_name_mappings_ex = mt_name_mappings_ex    " Código de uma posição)
    ).
    RETURN.
  ENDIF.
ENDENHANCEMENT.
