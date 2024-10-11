FUNCTION Z_WM_GENERAL_CONVERSION.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_INPUT_VALUE)
*"     REFERENCE(I_INPUT_OUTPUT) TYPE  C
*"  EXPORTING
*"     REFERENCE(E_OUTPUT_VALUE)
*"----------------------------------------------------------------------

  DATA: lref_input_holder  TYPE REF TO data,
        lref_output_holder TYPE REF TO data,
        lv_length          TYPE i,
        lv_mask            TYPE string,
        lv_convexit        TYPE funcname,
        lv_type            TYPE c,
        lv_output_p_check  TYPE string.

  FIELD-SYMBOLS: <lv_output_holder> TYPE ANY,
                 <lv_input_holder>  TYPE ANY.

  IF i_input_value IS INITIAL.
    CLEAR e_output_value.
    EXIT.
  ENDIF.

** Cria Duplicado de Input
***********************************************************************
  CREATE DATA lref_input_holder LIKE i_input_value.
  ASSIGN lref_input_holder->* TO <lv_input_holder>.
  CHECK <lv_input_holder> IS ASSIGNED.

  <lv_input_holder> = i_input_value.
  CLEAR e_output_value.

** Determina Mascara De Dominios
***********************************************************************
  DESCRIBE FIELD <lv_input_holder> OUTPUT-LENGTH lv_length
                                   EDIT MASK     lv_mask.

  IF lv_mask IS INITIAL.
    DESCRIBE FIELD e_output_value OUTPUT-LENGTH lv_length
                                  EDIT MASK     lv_mask
                                  TYPE          lv_type.
  ENDIF.

  REPLACE '==' IN lv_mask WITH space.
  CONDENSE lv_mask NO-GAPS.

  DESCRIBE FIELD e_output_value TYPE lv_type.

** Cria Duplicado de Output
***********************************************************************
  CREATE DATA lref_output_holder TYPE c LENGTH lv_length.
  ASSIGN lref_output_holder->* TO <lv_output_holder>.
  CHECK <lv_output_holder> IS ASSIGNED.

** Conversão
***********************************************************************
  DO 1 TIMES.

    IF lv_type EQ 'P' AND
       i_input_output EQ 'I'.

      CONDENSE <lv_input_holder>.

      CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
        EXPORTING
          input      = <lv_input_holder>
          internal   = 'X'
        IMPORTING
          output     = <lv_output_holder>
        EXCEPTIONS
          no_numeric = 1
          OTHERS     = 2.

      IF sy-subrc <> 0.
        CLEAR <lv_input_holder>.
        EXIT.
      ENDIF.
    ENDIF.


    CHECK NOT lv_mask IS INITIAL.

    IF i_input_output = 'I'.
      CONCATENATE 'CONVERSION_EXIT_' lv_mask '_INPUT' INTO lv_convexit.
    ELSEIF i_input_output = 'O'.
      CONCATENATE 'CONVERSION_EXIT_' lv_mask '_OUTPUT' INTO lv_convexit.
    ELSE.
      EXIT.
    ENDIF.

    TRY.
        CALL FUNCTION lv_convexit
          EXPORTING
            input  = <lv_input_holder>
          IMPORTING
            output = <lv_output_holder>.
      CATCH cx_sy_dyn_call_illegal_func.
        EXIT.
    ENDTRY.
  ENDDO.

  TRY.
      IF <lv_output_holder> IS INITIAL.
        e_output_value = <lv_input_holder>.
      ELSE.
        e_output_value = <lv_output_holder>.
      ENDIF.
    CATCH: cx_sy_conversion_no_number,
           cx_sy_conversion_overflow.
      CLEAR e_output_value.
  ENDTRY.



ENDFUNCTION.
