FUNCTION zwm_update_bin.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"     REFERENCE(POSICAO) TYPE  LGPLA
*"     REFERENCE(TIPO_DEPOSITO) TYPE  LGTYP
*"     REFERENCE(ENTRADA) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(SAIDA) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(LBINH) LIKE  E1LBINH STRUCTURE  E1LBINH
*"  TABLES
*"      LBINI STRUCTURE  E1LBINI
*"  EXCEPTIONS
*"      INVALID_OPTION
*"      INVALID_BIN
*"      ERROR_BLOCK
*"----------------------------------------------------------------------



  IF NOT entrada IS INITIAL AND NOT saida IS INITIAL.
    RAISE invalid_option.
  ENDIF.

** Verificar se a posição existe
  SELECT SINGLE * FROM lagp
                  WHERE lgnum = armazem AND
                        lgtyp = tipo_deposito AND
                        lgpla = posicao.
  IF sy-subrc <> 0.
    RAISE invalid_bin.
  ENDIF.

  IF NOT entrada IS INITIAL.

    CALL FUNCTION 'L_BLOCK_AISLE'
      EXPORTING
        lbinh             = lbinh
      TABLES
        lbini             = lbini
      EXCEPTIONS
        consistency_check = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      RAISE error_block.
    ENDIF.


  ELSEIF NOT saida IS INITIAL.


    CALL FUNCTION 'L_BLOCK_AISLE'
      EXPORTING
        lbinh             = lbinh
      TABLES
        lbini             = lbini
      EXCEPTIONS
        consistency_check = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      RAISE error_block.
    ENDIF.

  ENDIF.




ENDFUNCTION.
