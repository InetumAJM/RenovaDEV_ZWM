FUNCTION zwm_bapi_get_mesa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(NUM_PALETE) TYPE  I
*"  EXPORTING
*"     REFERENCE(MESA) TYPE  CHAR14
*"     REFERENCE(E_SSCC) TYPE  CHAR20
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2
*"      SSCC STRUCTURE  ZWM_SSCC
*"----------------------------------------------------------------------
  DATA: x_zwm013 LIKE zwm013 OCCURS 0 WITH HEADER LINE,
        x_zwm020 LIKE zwm020 OCCURS 0 WITH HEADER LINE.

* check se user esta ou nao associado ao armazem
  PERFORM user_own_data.
  IF xuser-lgnum IS INITIAL.
    CLEAR mesa.
    READ TABLE sscc INDEX 1.
    e_sscc = sscc-sscc.
    EXIT.
  ENDIF.

  READ TABLE sscc INDEX 1.

  SELECT SINGLE * FROM marc WHERE matnr = sscc-material.
  IF marc-maabc IS INITIAL.
    SELECT SINGLE mesa INTO mesa FROM zwm009 WHERE abc = 'Z'.
  ELSE.
    SELECT SINGLE mesa INTO mesa FROM zwm009 WHERE abc = marc-maabc.
  ENDIF.

  SELECT SINGLE *
    FROM marm
        WHERE matnr = sscc-material AND
              meinh = 'PAL'.

  IF sscc-quantidade < marm-umrez.
    SELECT SINGLE mesa INTO mesa FROM zwm009 WHERE abc = 'Z'.
  ENDIF.

  e_sscc = sscc-sscc.

  CASE num_palete.
    WHEN '1'.
      x_zwm013-armazem = xuser-lgnum.
      x_zwm013-destino = mesa.
      x_zwm013-bloqueado = 'X'.
      x_zwm013-sscc = sscc-sscc.
      x_zwm013-tipo_palete = sscc-tipo_su.
      CLEAR x_zwm013-fabrica_1.
      APPEND x_zwm013.
      INSERT zwm013 FROM x_zwm013.

    WHEN '2'.
      x_zwm013-armazem = xuser-lgnum.
      x_zwm013-destino = mesa.
      x_zwm013-bloqueado = 'X'.
      x_zwm013-sscc = sscc-sscc.
      x_zwm013-tipo_palete = sscc-tipo_su.
      CLEAR x_zwm013-fabrica_1.
      APPEND x_zwm013.
      INSERT zwm013 FROM x_zwm013.

      x_zwm020-armazem = xuser-lgnum.
      x_zwm020-p1 = sscc-sscc.
      READ TABLE sscc INDEX 2.
      x_zwm020-p2 = sscc-sscc.
      APPEND x_zwm020.
      INSERT INTO zwm020 VALUES x_zwm020.
      COMMIT WORK.
  ENDCASE.

ENDFUNCTION.
