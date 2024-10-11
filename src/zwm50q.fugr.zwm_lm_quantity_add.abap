FUNCTION ZWM_LM_QUANTITY_ADD.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_QUAN1) TYPE  MENG15
*"             VALUE(IF_MEH1) TYPE  MEINS
*"             VALUE(IF_QUAN2) TYPE  MENG15
*"             VALUE(IF_MEH2) TYPE  MEINS
*"             VALUE(IF_FLAG_MEH2_FIX) TYPE  CHAR1 DEFAULT SPACE
*"             VALUE(IF_ALLOW_UNIT_CONVERSION) TYPE  CHAR1
*"         DEFAULT SPACE
*"       EXPORTING
*"             VALUE(EF_SUM) TYPE  MENG15
*"             VALUE(EF_MEH) TYPE  MEINS
*"--------------------------------------------------------------------

  constants: lc_meng15_high type meng15 value '999999999999.999'.
  data: ls_t006_1 like t006,
        ls_t006_2 like t006,
        lf_quan1 type meng15,
        lf_quan2 type meng15,
        lf_meh1 type meins,
        lf_meh2 type meins,
        lf_float    type f,                                 "n_563266
        lf_overflow type c,
        lf_field1 type f,
        lf_field2 type f.

  if if_meh1 eq space and if_meh2 ne space.
    ef_sum = if_quan2.
    ef_meh = if_meh2.
    exit.
  endif.
  if if_meh1 ne space and if_meh2 eq space.
    ef_sum = if_quan1.
    ef_meh = if_meh1.
    exit.
  endif.

  if if_meh1 eq if_meh2.
    lf_float = if_quan1 + if_quan2.                         "v_n_563266
    if lf_float lt lc_meng15_high.
      ef_sum = lf_float.
      ef_meh = if_meh1.
      exit.
    else.
*     If change of the unit is allowed try to swith to the next highest
*     unit of measure, otherwise return overflow value
      if if_allow_unit_conversion ne space.
        call function 'UNIT_QUANTITY_SWITCH'
          exporting
            imp_unit          = if_meh1
            imp_value         = lf_float
            tolerance         = 1000
          importing
            exp_value         = ef_sum
            overflow          = lf_overflow
          changing
            exp_unit          = ef_meh
          exceptions
            unit_not_found    = 1
            output_not_type_p = 2
            others            = 3.

        if sy-subrc ne 0.
          ef_meh = if_meh1.
          ef_sum = lc_meng15_high.
        endif.
        if lf_overflow ne space.
          ef_meh = if_meh1.
          ef_sum = lc_meng15_high.
        endif.
        exit.
      else.
        ef_sum = lc_meng15_high.
        ef_meh = if_meh1.
        exit.
      endif.
    endif.                                                  "^_n_563266
  endif.

  select single * from t006
                  into ls_t006_1
                  where msehi = if_meh1.
  if sy-subrc ne 0.
    exit.
  endif.
  select single * from t006
                  into ls_t006_2
                  where msehi = if_meh2.
  if sy-subrc ne 0.
    exit.
  endif.
  lf_field1 = ls_t006_1-zaehl * ls_t006_2-nennr.
  lf_field2 = ls_t006_1-nennr * ls_t006_2-zaehl.

  if lf_field1 gt lf_field2 and if_flag_meh2_fix eq space.
    lf_quan1 = if_quan2.
    lf_meh1 = if_meh2.
    lf_quan2 = if_quan1.
    lf_meh2 = if_meh1.
  else.
    lf_quan1 = if_quan1.
    lf_meh1 = if_meh1.
    lf_quan2 = if_quan2.
    lf_meh2 = if_meh2.
  endif.
* lf_meh2 should now be the biggest UoM or the relevant UoM

  call function 'UNIT_CONVERSION_SIMPLE'
    exporting
      input                = lf_quan1
      unit_in              = lf_meh1
      unit_out             = lf_meh2
    importing
      output               = lf_quan1
    exceptions
      conversion_not_found = 1
      division_by_zero     = 2
      input_invalid        = 3
      output_invalid       = 4
      overflow             = 5
      type_invalid         = 6
      units_missing        = 7
      unit_in_not_found    = 8
      unit_out_not_found   = 9
      others               = 10.
  if sy-subrc eq 0.
    lf_float = lf_quan1 + lf_quan2.                         "v_n_563266
    if lf_float lt lc_meng15_high.
      ef_sum = lf_float.
      ef_meh = lf_meh2.
    else.
*     If change of the unit is allowed try to swith to the next highest
*     unit of measure, otherwise return overflow value
      if if_allow_unit_conversion ne space.
        call function 'UNIT_QUANTITY_SWITCH'
          exporting
            imp_unit          = lf_meh2
            imp_value         = lf_float
            tolerance         = 1000
          importing
            exp_value         = ef_sum
            overflow          = lf_overflow
          changing
            exp_unit          = ef_meh
          exceptions
            unit_not_found    = 1
            output_not_type_p = 2
            others            = 3.

        if sy-subrc ne 0.
          ef_meh = lf_meh2.
          ef_sum = lc_meng15_high.
        endif.
        if lf_overflow ne space.
          ef_meh = lf_meh2.
          ef_sum = lc_meng15_high.
        endif.
      else.
        ef_sum = lc_meng15_high.
        ef_meh = lf_meh2.
      endif.
    endif.                                                  "^_n_563266
  else.
    ef_sum = if_quan2.
    ef_meh = if_meh2.
  endif.

endfunction.
