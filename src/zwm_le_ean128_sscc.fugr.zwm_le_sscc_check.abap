FUNCTION ZWM_LE_SSCC_CHECK.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_SSCC) TYPE  EXIDV OPTIONAL
*"             VALUE(IF_WERK) LIKE  T313Y-WERK OPTIONAL
*"             VALUE(IF_LGORT) LIKE  T313Y-LGORT OPTIONAL
*"             VALUE(IF_LGNUM) LIKE  T313Z-LGNUM OPTIONAL
*"             VALUE(IF_HUART) TYPE  EAN128_HU_ART DEFAULT '3'
*"             VALUE(IF_FOREIGN_SSCC) TYPE  BOOLE_D DEFAULT ' '
*"       EXCEPTIONS
*"              ILN_NOT_FOUND
*"              INVALID_CALL
*"              INVALID_CUSTOMIZING
*"              INVALID_ILN
*"              INVALID_NO_OUT_OF_RANGE
*"              INVALID_SSCC
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

IF if_sscc IS INITIAL.
* Bitte eine SSCC zum Prüfen angeben.
  MESSAGE e034 RAISING invalid_call.
ENDIF.

IF if_foreign_sscc IS INITIAL.                              "SP_380921
*   look for ILN ...
*   ... with plant and storage location
  PERFORM t313y_read_buffered USING    if_werk
                                       if_lgort
                              CHANGING ls_t313y
                                       lt_t313y
                                       lf_iln_lgort_found.

  IF lf_iln_lgort_found = gc_false.
    IF if_lgnum IS INITIAL.
      lf_iln_lgnum_found = gc_false.
    ELSE.
*   ... with warehouse number
      PERFORM t313z_read_buffered USING    if_lgnum
                                  CHANGING ls_t313z
                                           lt_t313z
                                           lf_iln_lgnum_found.
    ENDIF.

    IF lf_iln_lgnum_found = gc_false
       AND NOT if_lgort IS INITIAL.    "otherwise it's the same as above
*   ... only with plant
      CLEAR lf_lgort.
      PERFORM t313y_read_buffered USING    if_werk
                                           lf_lgort
                                  CHANGING ls_t313y
                                           lt_t313y
                                           lf_iln_lgort_found.
    ENDIF.

    IF     ( lf_iln_lgort_found = gc_false )
       AND ( lf_iln_lgnum_found = gc_false )
       AND NOT if_werk IS INITIAL.     "otherwise it's the same as above
*   ... on client level
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
*   Keine ILN-Basisnummer und Nummernkreisobjekt gefunden.
    MESSAGE e022 WITH if_werk if_lgort if_lgnum
                 RAISING iln_not_found.
  ELSE.
    IF lf_iln_lgort_found = gc_true.
      MOVE-CORRESPONDING ls_t313y TO ls_sscc_gen_param.
    ELSE.
      MOVE-CORRESPONDING ls_t313z TO ls_sscc_gen_param.
    ENDIF.
  ENDIF.
ENDIF.                                                    "SP_380921

* Check the SSCC
PERFORM sscc_check USING if_sscc
                         if_huart
                         ls_sscc_gen_param
                         if_foreign_sscc
                CHANGING lf_retcode
.

ENDFUNCTION.
