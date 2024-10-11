FUNCTION zwm_batch_create.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"     REFERENCE(MATERIAL) TYPE  MATNR
*"     REFERENCE(LOTE) TYPE  CHARG_D
*"     REFERENCE(CENTRO) TYPE  WERKS_D OPTIONAL
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERRO
*"      BATCH_NOT_CREATED
*"----------------------------------------------------------------------
  DATA : plant LIKE mchb-werks,
         s_loc LIKE mchb-lgort.

  DATA: ls_mchb TYPE mchb.

*  BREAK-POINT.
  CALL FUNCTION 'ZWM_GET_MATERIAL_PLANT_SLOC'
    EXPORTING
      warehouse    = armazem
* INETUM - NR - 02.03.2022 - RENPRJ00032 - Inicio
      werks        = centro
* INETUM - NR - 02.03.2022 - RENPRJ00032 - Fim
      material     = material
    IMPORTING
      plant        = plant
      s_loc        = s_loc
    TABLES
      return_msg   = return_msg
    EXCEPTIONS
      not_found    = 1
      indetermined = 2
      OTHERS       = 3.
  IF sy-subrc <> 0.
    RAISE erro.
    EXIT.
  ENDIF.

  CLEAR mchb.
  SELECT SINGLE * FROM mchb
         WHERE matnr = material
           AND werks = plant
           AND lgort = s_loc
           AND charg = lote.
  IF sy-subrc = 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'BAPI_BATCH_CREATE'
    EXPORTING
      material             = material
      batch                = lote
      plant                = plant
*     BATCHATTRIBUTES      =
*     BATCHCONTROLFIELDS   =
      batchstoragelocation = s_loc
*     INTERNALNUMBERCOM    =
*     EXTENSION1           =
* IMPORTING
*     BATCH                =
*     BATCHATTRIBUTES      =
* TABLES
*     RETURN               =
    .
  IF sy-subrc <> 0.
    RAISE erro.
    EXIT.
  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.

  DO 20 TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    SELECT SINGLE * FROM mchb
                    INTO ls_mchb
                    BYPASSING BUFFER
                    WHERE matnr = material AND
                          werks = plant AND
                          lgort = s_loc AND
                          charg = lote.

    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.
  ENDDO.
ENDFUNCTION.
