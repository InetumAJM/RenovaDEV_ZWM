FUNCTION z_wm_filter_table_to_wm.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"  CHANGING
*"     REFERENCE(CT_TABLE) TYPE  ANY
*"----------------------------------------------------------------------
  DATA: lt_t320 TYPE TABLE OF t320.

  FIELD-SYMBOLS: <lt_table> TYPE INDEX TABLE,
                 <ls_line>  TYPE ANY,
                 <lv_datum> TYPE datum,
                 <lv_werks> TYPE werks_d,
                 <lv_lgort> TYPE lgort_d.

  DATA: lv_tabix TYPE sytabix.

** Tabela
***********************************************************************
  ASSIGN ct_table TO <lt_table>.
  CHECK <lt_table> IS ASSIGNED.

  SELECT * FROM t320
           INTO TABLE lt_t320
           WHERE lgnum = i_lgnum.


  LOOP AT <lt_table> ASSIGNING <ls_line>.
    lv_tabix = sy-tabix.

    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <ls_line> TO <lv_werks>.
    IF <lv_werks> IS ASSIGNED AND NOT <lv_werks> IS INITIAL.
      READ TABLE lt_t320
            TRANSPORTING NO FIELDS
            WITH KEY werks = <lv_werks>.

      IF sy-subrc <> 0.
        DELETE <lt_table> INDEX lv_tabix.
        CONTINUE.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'LGORT' OF STRUCTURE <ls_line> TO <lv_lgort>.
    IF <lv_lgort> IS ASSIGNED AND NOT <lv_lgort> IS INITIAL.
      READ TABLE lt_t320
            TRANSPORTING NO FIELDS
            WITH KEY lgort = <lv_lgort>.

      IF sy-subrc <> 0.
        DELETE <lt_table> INDEX lv_tabix.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF ( <lv_werks> IS ASSIGNED AND NOT <lv_werks> IS INITIAL ) AND ( <lv_lgort> IS ASSIGNED AND NOT <lv_lgort> IS INITIAL ).
      READ TABLE lt_t320
            TRANSPORTING NO FIELDS
            WITH KEY werks = <lv_werks>
                     lgort = <lv_lgort>.

      IF sy-subrc <> 0.
        DELETE <lt_table> INDEX lv_tabix.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
