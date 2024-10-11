FUNCTION zwm_le_sscc_generate.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_WERK) LIKE  T313Y-WERK OPTIONAL
*"             VALUE(IF_LGORT) LIKE  T313Y-LGORT OPTIONAL
*"             VALUE(IF_LGNUM) LIKE  T313Z-LGNUM OPTIONAL
*"             VALUE(IF_HUART) TYPE  EAN128_HU_ART DEFAULT '3'
*"       EXPORTING
*"             VALUE(EF_SSCC) TYPE  EXIDV
*"       EXCEPTIONS
*"              ILN_NOT_FOUND
*"              INVALID_CALL
*"              INVALID_CUSTOMIZING
*"              INTERNAL_ERROR
*"              ERROR_ON_NUMBER_RANGE
*"--------------------------------------------------------------------

* local data ...
* ... tables
  DATA: lt_t313y          TYPE t313y_t
       ,lt_t313z          TYPE t313z_t

* ... structures
       ,ls_t313y          TYPE t313y
       ,ls_t313z          TYPE t313z

* ... fields
       ,lf_werk           LIKE t313y-werk
       ,lf_lgort          LIKE t313y-lgort
       ,ls_sscc_gen_param TYPE sscc_gen_param
       ,lf_retcode(1)     TYPE c

* ... boolean flags
       ,lf_iln_lgort_found(1) TYPE c VALUE gc_false
       ,lf_iln_lgnum_found(1) TYPE c VALUE gc_false
       .

* ... constants

* check import parameter/Initialisation
  PERFORM check_param_sscc_generate USING if_werk
                                          if_lgort
                                          if_lgnum
                                          if_huart.

* look for ILN ...
* ... with plant and storage location
  PERFORM t313y_read_buffered USING    if_werk
                                       if_lgort
                              CHANGING ls_t313y
                                       lt_t313y
                                       lf_iln_lgort_found.

  IF lf_iln_lgort_found = gc_false.
    IF if_lgnum IS INITIAL.
      lf_iln_lgnum_found = gc_false.
    ELSE.
* ... with warehouse number
      PERFORM t313z_read_buffered USING    if_lgnum
                                  CHANGING ls_t313z
                                           lt_t313z
                                           lf_iln_lgnum_found.
    ENDIF.

    IF lf_iln_lgnum_found = gc_false
     AND NOT if_lgort IS INITIAL.      "otherwise it's the same as above
* ... only with plant
      CLEAR lf_lgort.
      PERFORM t313y_read_buffered USING    if_werk
                                           lf_lgort
                                  CHANGING ls_t313y
                                           lt_t313y
                                           lf_iln_lgort_found.
    ENDIF.

    IF     ( lf_iln_lgort_found = gc_false )
       AND ( lf_iln_lgnum_found = gc_false )
     AND NOT if_werk IS INITIAL.       "otherwise it's the same as above
* ... on client level
      CLEAR: lf_werk, lf_lgort.
      PERFORM t313y_read_buffered USING    lf_werk
                                           lf_lgort
                                  CHANGING ls_t313y
                                           lt_t313y
                                           lf_iln_lgort_found.
    ENDIF.
  ENDIF.

  IF    lf_iln_lgort_found = gc_false
    AND lf_iln_lgnum_found = gc_false.
* Keine ILN-Basisnummer und Nummernkreisobjekt gefunden.
    MESSAGE e022 WITH if_werk if_lgort if_lgnum
                 RAISING iln_not_found.
  ELSE.
    IF lf_iln_lgort_found = gc_true.
      MOVE-CORRESPONDING ls_t313y TO ls_sscc_gen_param.
    ELSE.
      MOVE-CORRESPONDING ls_t313z TO ls_sscc_gen_param.
    ENDIF.
  ENDIF.

* Build the SSCC
  PERFORM sscc_concatenate USING if_huart
                                 if_lgnum
                                 ls_sscc_gen_param
                        CHANGING ef_sscc
                                 lf_retcode
  .

  CASE lf_retcode.
    WHEN '2'.      "last number
      IF lf_iln_lgort_found = gc_true.
        PERFORM dont_use_t313y CHANGING ls_t313y lt_t313y.
      ELSE.
        PERFORM dont_use_t313z CHANGING ls_t313z lt_t313z.
      ENDIF.
  ENDCASE.

ENDFUNCTION.
