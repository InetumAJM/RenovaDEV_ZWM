FUNCTION zwm_get_material_plant_sloc .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(LGORT) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(MATERIAL) TYPE  MATNR
*"  EXPORTING
*"     REFERENCE(PLANT) TYPE  WERKS_D
*"     REFERENCE(S_LOC) TYPE  LGORT_D
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      NOT_FOUND
*"      INDETERMINED
*"----------------------------------------------------------------------
  DATA: aux_t320 LIKE t320 OCCURS 0 WITH HEADER LINE,
        aux_mard LIKE mard OCCURS 0 WITH HEADER LINE,
        linhas   LIKE sy-tabix.

  DATA: lt_mard   TYPE TABLE OF mard,
        lt_mard_t TYPE TABLE OF mard,
        ls_mard   TYPE mard.

  DATA: lr_werks TYPE RANGE OF werks_d.

  DATA: ls_s_werks LIKE LINE OF lr_werks.

  DATA: lr_lgort TYPE RANGE OF lgort_d.

  DATA: ls_s_lgort LIKE LINE OF lr_lgort.


  SELECT SINGLE * FROM mlgn
         WHERE matnr = material
           AND lgnum = warehouse.
  IF sy-subrc <> 0.
    CLEAR return_msg.
    MOVE 'E' TO return_msg-msgtyp.
    MOVE 'ZWMMSG001' TO return_msg-msgid.
    MOVE '046' TO return_msg-msgnr.
    WRITE material TO return_msg-msgv1 LEFT-JUSTIFIED.
    WRITE warehouse TO return_msg-msgv2 LEFT-JUSTIFIED.
    APPEND return_msg.
    RAISE not_found.
  ENDIF.

  IF NOT werks IS INITIAL.
    ls_s_werks-low = werks.
    ls_s_werks-sign   = 'I'.
    ls_s_werks-option = 'EQ'.
    APPEND ls_s_werks TO lr_werks.
  ENDIF.

  IF NOT lgort IS INITIAL.
    ls_s_lgort-low = lgort.
    ls_s_lgort-sign   = 'I'.
    ls_s_lgort-option = 'EQ'.
    APPEND ls_s_lgort TO lr_lgort.
  ENDIF.

  SELECT * FROM t320
           INTO TABLE aux_t320
           WHERE lgnum = warehouse AND
                 werks IN lr_werks AND
                 lgort IN lr_lgort.
  IF sy-subrc = 0.
    DESCRIBE TABLE aux_t320 LINES linhas.
    IF linhas = 0.
      CLEAR return_msg.
      MOVE 'E' TO return_msg-msgtyp.
      MOVE 'ZWMMSG001' TO return_msg-msgid.
      MOVE '047' TO return_msg-msgnr.
      WRITE warehouse TO return_msg-msgv1 LEFT-JUSTIFIED.
      APPEND return_msg.
      RAISE not_found.
    ELSEIF linhas > 1.
      SELECT * FROM mard
              INTO TABLE lt_mard
              FOR ALL ENTRIES IN aux_t320
              WHERE matnr = material
                AND werks = aux_t320-werks
                AND lgort = aux_t320-lgort
                AND lvorm = ' '.

      DO 1 TIMES.
        lt_mard_t = lt_mard.
        SORT lt_mard_t BY werks lgort.
        DELETE ADJACENT DUPLICATES FROM lt_mard_t COMPARING werks lgort.
        DESCRIBE TABLE lt_mard_t LINES linhas.
        IF linhas EQ 1.
          READ TABLE lt_mard_t INTO ls_mard INDEX 1.
          plant = ls_mard-werks.
          s_loc = ls_mard-lgort.
          EXIT.
****  Paulo Sousa, 2022.03.02
****  Cuidado! Martelada para contornar o problema criado pelo novo centro RPFR. Resolver isto ASAP
* INETUM - NR - 10.03.2022 - RENPRJ00032 - Inicio
*Comentado
***        else.
***          if warehouse = '150'.
***            plant = 'RFRA'.
***            s_loc = 'CD'.
***          endif.
* INETUM - NR - 10.03.2022 - RENPRJ00032 - Fim
        ENDIF.
      ENDDO.

      CHECK plant IS INITIAL OR
            s_loc IS INITIAL.

      CLEAR return_msg.
      MOVE 'E' TO return_msg-msgtyp.
      MOVE 'ZWMMSG001' TO return_msg-msgid.
      MOVE '048' TO return_msg-msgnr.
      WRITE warehouse TO return_msg-msgv2 LEFT-JUSTIFIED.
      APPEND return_msg.
      RAISE indetermined.
    ELSE.
      READ TABLE aux_t320 INDEX 1.
      SELECT SINGLE * FROM mard
              WHERE matnr = material
                AND werks = aux_t320-werks
                AND lgort = aux_t320-lgort
                AND lvorm = ' '.
      IF sy-subrc <> 0.
        CLEAR return_msg.
        MOVE 'E' TO return_msg-msgtyp.
        MOVE 'ZWMMSG001' TO return_msg-msgid.
        MOVE '065' TO return_msg-msgnr.
        WRITE material TO return_msg-msgv1 LEFT-JUSTIFIED.
        WRITE aux_t320-werks TO return_msg-msgv1 LEFT-JUSTIFIED.
        WRITE aux_t320-lgort TO return_msg-msgv1 LEFT-JUSTIFIED.
        APPEND return_msg.
        RAISE not_found.
      ELSE.
        READ TABLE aux_t320 INDEX 1.
        plant = aux_t320-werks.
        s_loc = aux_t320-lgort.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
