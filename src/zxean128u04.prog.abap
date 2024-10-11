*&---------------------------------------------------------------------*
*&  Include           ZXEAN128U04                                      *
*&---------------------------------------------------------------------*

 TABLES: mara, marm.
*
DATA qtd_pal TYPE vemng.
*** alterado Paulo Sousa 2010.04.08
*** testado valor de IS_EAN128_DATA-LBLTYPE
*** initial ou L0 - existente
***            L1 - ignora DUN14 de palete; AI2 + AI37 em vez de AI01
*** -------- Parvoices...
** 2015.03.04 correção para prever minusculas no PAL
** Desactivado o codigo para gerar AI 01.
*
*if ( ( IS_EAN128_DATA-LBLTYPE is initial ) or ( IS_EAN128_DATA-LBLTYPE = 'L0') ).
*
*IF is_ean128_ai_data-aival = '02'.
*
*  SELECT SINGLE * FROM mara
*         WHERE matnr = is_ean128_data-matnr.
*  IF sy-subrc = 0.
*
*    SELECT SINGLE *
*        FROM marm
*            WHERE matnr = is_ean128_data-matnr AND
*                  ( ( meinh = 'PAL' ) or ( meinh = 'pal' ) ) .
*    IF NOT marm-ean11 IS INITIAL.
*
*      CLEAR qtd_pal.
*      qtd_pal = marm-umrez / marm-umren.
*
*      IF is_ean128_data-vemng = qtd_pal.
*
*        CONCATENATE '01' marm-ean11 INTO ef_bc_val.
*
*        MOVE:'16'       TO ef_bc_val_l.
*
*        CONCATENATE '(01)'
*                    marm-ean11
*               INTO ef_bc_txt_val
*               SEPARATED BY space.
*
*        MOVE: '19' TO ef_bc_txt_val_l,
*              'X'  TO ef_use_customer_values.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.
*
*ELSEIF is_ean128_ai_data-aival = '37'.
*
*  SELECT SINGLE * FROM mara
*         WHERE matnr = is_ean128_data-matnr.
*  IF sy-subrc = 0.
*
*    SELECT SINGLE *
*        FROM marm
*            WHERE matnr = is_ean128_data-matnr AND
*                  ( ( meinh = 'PAL' ) or ( meinh = 'pal' ) ) .
*    IF NOT marm-ean11 IS INITIAL.
*      CLEAR qtd_pal.
*      qtd_pal = marm-umrez / marm-umren.
*
*      IF is_ean128_data-vemng = qtd_pal.
*
*        CLEAR: ef_bc_val,
*               ef_bc_val_l,
*               ef_bc_txt_val,
*               ef_bc_txt_val_l.
*
*        MOVE 'X'  TO ef_use_customer_values.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.
*
*ENDIF.
*
*endif.
