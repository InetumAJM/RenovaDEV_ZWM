FUNCTION zwm_delivery_change .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN_VL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_message        TYPE bdcmsgcoll,
        lv_lfimg          TYPE lips-lfimg,
        ls_lips           TYPE lips,
        lt_lips           TYPE TABLE OF lips,
        ls_marm           TYPE marm.

  CLEAR: et_messages.

  SELECT * FROM lips INTO TABLE lt_lips
   WHERE vbeln = i_vbeln.

  IF lt_lips IS INITIAL.
    RAISE error.
  ENDIF.



  SORT lt_lips DESCENDING BY posnr.

  LOOP AT lt_lips INTO ls_lips.


    CHECK ls_lips-meins NE ls_lips-vrkme.

    SELECT SINGLE * FROM marm INTO ls_marm
      WHERE  matnr = ls_lips-matnr AND
             meinh = ls_lips-vrkme.

    CHECK ls_marm-UMREN > ls_marm-UMREZ.

    IF ls_lips-uecha IS INITIAL .
*    IF ls_lips-pstyv = 'TAN' .

      READ TABLE lt_lips WITH KEY vbeln = ls_lips-vbeln
*                                  pstyv = 'ZTLO'
                                  uecha = ls_lips-posnr
                                  TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.

        CALL FUNCTION 'ROUND'
          EXPORTING
            decimals      = 0
            input         = ls_lips-lfimg
            sign          = 'X'            "ORMNG
          IMPORTING
            output        = lv_lfimg
          EXCEPTIONS
            input_invalid = 1
            overflow      = 2
            type_invalid  = 3
            OTHERS        = 4.
      ELSE.
        lv_lfimg  = 0.

      ENDIF.
      CALL FUNCTION 'ZWM_BATCH_INPUT_DELIV'
        EXPORTING
          i_lfimg      = lv_lfimg
          i_vbeln      = ls_lips-vbeln
          i_item_pai   = ls_lips-posnr
*            i_item_filho = ls_lips-posnr
          i_pstyv      = ls_lips-pstyv.
    ELSE.

      CALL FUNCTION 'ROUND'
        EXPORTING
          decimals      = 0
          input         = ls_lips-lfimg
          sign          = 'X'
        IMPORTING
          output        = lv_lfimg
        EXCEPTIONS
          input_invalid = 1
          overflow      = 2
          type_invalid  = 3
          OTHERS        = 4.

      CALL FUNCTION 'ZWM_BATCH_INPUT_DELIV'
        EXPORTING
          i_lfimg      = lv_lfimg
          i_vbeln      = ls_lips-vbeln
          i_item_pai   = ls_lips-uecha
          i_item_filho = ls_lips-posnr
          i_pstyv      = ls_lips-pstyv.
    ENDIF.
    CLEAR ls_marm.
  ENDLOOP.
ENDFUNCTION.
