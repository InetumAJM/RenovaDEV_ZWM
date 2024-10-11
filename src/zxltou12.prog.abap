*&---------------------------------------------------------------------*
*&  Include           ZXLTOU12
*&---------------------------------------------------------------------*
DATA: ls_mlgn TYPE mlgn.
FIELD-SYMBOLS: <fs_letyp> TYPE lvs_letyp.


DATA: lv_menge TYPE menge_d,
      ls_lips  TYPE lips.
DATA: aux TYPE menge_d.


** Verficar unidade de medida base <> unidade de medida do pedido
**********************************************************************
SELECT SINGLE * FROM lips INTO ls_lips
 WHERE vbeln = i_ltap-vbeln AND
       posnr = i_ltap-posnr.

IF sy-subrc = 0.
** se menor qtd fornecida menor que valor de entrada
** significa que a unidade de medida é igual mas o valor
** já esta arredondado

  IF i_ltap-meins NE i_ltap-altme.

    aux =  c_anfml * i_ltap-umren.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals      = 0
        input         = aux
        sign          = 'X'
      IMPORTING
        output        = aux
      EXCEPTIONS
        input_invalid = 1
        overflow      = 2
        type_invalid  = 3
        OTHERS        = 4.

    c_anfml = aux / i_ltap-umren.

    IF c_anfme < ls_lips-lfimg.
      c_anfml = c_anfml + '0.001'.
    ELSE.
      lv_menge = ( c_anfml * i_ltap-umren ) DIV c_anfme.
      IF lv_menge < 1.
        c_anfml = c_anfml + '0.001'.
      ENDIF.

    ENDIF.

  ENDIF.

ENDIF.

** Saídas de Armazém automático  WCS - OTs Bloco sem UD
**********************************************************************
IF i_lqua-letyp = 'P20'    AND i_ltap-lgnum = '100' AND i_ltap-vltyp = 'AUT' AND
   i_ltap-vlenr IS INITIAL AND i_ltap-nlenr IS INITIAL.

  " Mudar tipo de unidade P20 para Tipo de unidade do Material
  SELECT SINGLE *
    FROM mlgn INTO ls_mlgn
    WHERE matnr = i_ltap-matnr
    AND   lgnum = i_ltap-lgnum.

  IF sy-subrc = 0.
    ASSIGN ('(SAPLL03A)LQUA-LETYP') TO <fs_letyp>.
    IF sy-subrc = 0.
      <fs_letyp> = ls_mlgn-lety1.
    ENDIF.
  ENDIF.
ENDIF.
