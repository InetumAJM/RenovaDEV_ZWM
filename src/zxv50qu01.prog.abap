*&---------------------------------------------------------------------

*&  Include           ZXV50QU01

*&---------------------------------------------------------------------


TABLES: zwm031.

CONSTANTS: cl_posnr_initial LIKE lips-posnr VALUE '000000'.

DATA: l_postab_tabix LIKE sy-tabix.

DATA: l_lgnum LIKE mlgn-lgnum,
      l_index LIKE sy-tabix.

DATA: BEGIN OF lt_vttp OCCURS 0,
        vbeln LIKE vttp-vbeln,
        tknum LIKE vttp-tknum,
      END OF lt_vttp.

DATA: BEGIN OF lt_likp OCCURS 0,
        vbeln LIKE likp-vbeln,
        kunnr LIKE likp-kunnr,
        vkorg LIKE likp-vkorg,
        route LIKE likp-route,
      END OF lt_likp.

DATA: lt_lips LIKE lips OCCURS 0 WITH HEADER LINE.

DATA: lt_paletes LIKE zpalete_picking OCCURS 10 WITH HEADER LINE.

DATA: ls_vbpa  LIKE vbpa,
      ls_adrc  LIKE adrc,
      ls_tzont LIKE tzont.

DATA: lv_vgbel TYPE vgbel.

* Leitura de tabelas
SELECT vbeln tknum
       FROM vttp INTO TABLE lt_vttp
            FOR ALL ENTRIES IN ct_postab
                WHERE vbeln = ct_postab-vbeln.
SORT lt_vttp BY vbeln tknum.
*
*if not lt_vttp[] is initial.

SELECT vbeln kunnr vkorg route
       FROM likp INTO TABLE lt_likp
            FOR ALL ENTRIES IN ct_postab
              WHERE vbeln = ct_postab-vbeln.

SORT lt_likp BY vbeln.

IF NOT lt_likp[] IS INITIAL.

  SELECT * FROM lips INTO TABLE lt_lips
              FOR ALL ENTRIES IN lt_likp
                WHERE vbeln = lt_likp-vbeln.

  DELETE lt_lips WHERE ( pstyv = 'ZPAS' OR pstyv = 'ZPAL' ).

ENDIF.

*endif.

*  Cria tabela Paletes Completas e Incompletas
*clear l_lgnum.
*call function 'ZWM_LER_DEPOSITO_UTILIZADOR'
*  importing
*    e_lgnum = l_lgnum.

l_lgnum = '100'.

LOOP AT lt_lips.
  CLEAR lt_paletes.

  MOVE-CORRESPONDING lt_lips TO lt_paletes.

  APPEND lt_paletes.
ENDLOOP.

REFRESH: lt_likp, lt_lips.

IF NOT l_lgnum IS INITIAL.
**   Cálculo das paletes
  CALL FUNCTION 'ZWM_PAL_PICKING_COMPLETE'
    EXPORTING
      i_lgnum         = l_lgnum
      i_actualiza     = ' '
    TABLES
      zpalete_picking = lt_paletes[].
ENDIF.

LOOP AT ct_postab.
  l_postab_tabix = sy-tabix.
  READ TABLE lt_vttp WITH KEY ct_postab-vbeln BINARY SEARCH.
  IF sy-subrc = 0.
    ct_postab-tknum = lt_vttp-tknum.
  ENDIF.
  IF NOT l_lgnum IS INITIAL.
*   Calcula Paletes Completas e Incompletas
    READ TABLE lt_paletes WITH KEY vbeln = ct_postab-vbeln.
    WHILE sy-subrc = 0 AND lt_paletes-vbeln = ct_postab-vbeln.
      l_index = sy-tabix + 1.
      ct_postab-pal_completa = ct_postab-pal_completa +
                               lt_paletes-pal_completa.
      ct_postab-pal_incompleta = ct_postab-pal_incompleta +
                                 lt_paletes-pal_incompleta.
      READ TABLE lt_paletes INDEX l_index.
    ENDWHILE.
    ct_postab-paletes = ct_postab-pal_completa +
                        ct_postab-pal_incompleta.
  ENDIF.

  SELECT SINGLE * FROM vbpa INTO ls_vbpa
                        WHERE vbeln = ct_postab-vbeln
                              AND posnr = '000000'
                              AND parvw = 'WE'.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM adrc INTO ls_adrc
                              WHERE addrnumber = ls_vbpa-adrnr.
    IF sy-subrc = 0.
**    descricao da zona de transporte
      SELECT SINGLE * FROM tzont INTO ls_tzont
             WHERE  spras  = sy-langu
             AND    land1  = ls_adrc-country
             AND    zone1  = ls_adrc-transpzone.
      IF sy-subrc = 0.
        ct_postab-zona_transporte = ls_tzont-vtext.
      ENDIF.
    ENDIF.
  ENDIF.

* Obter Paletização Especial
  CLEAR: zwm031.
  SELECT SINGLE * FROM zwm031
  WHERE lgnum EQ ct_postab-lgnum
    AND kunnr EQ ls_vbpa-kunnr
    AND matnr EQ ct_postab-matnr.

  IF sy-subrc EQ 0.
    ct_postab-pal_especial = 'PE'.
  ELSE.
    CLEAR: ct_postab-pal_especial.
  ENDIF.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.10.2012 17:06:08
*  Motivo: Data Desejada de Remessa
*--------------------------------------------------------------------*
  DO 1 TIMES.
    CLEAR lv_vgbel.

    IF ct_postab-posnr IS INITIAL.
      SELECT SINGLE vgbel FROM lips
                          INTO lv_vgbel
                          WHERE vbeln = ct_postab-vbeln.
    ELSE.
      SELECT SINGLE vgbel FROM lips
                          INTO lv_vgbel
                          WHERE vbeln = ct_postab-vbeln AND
                                posnr = ct_postab-posnr.
    ENDIF.

    CHECK NOT lv_vgbel IS INITIAL.


    SELECT SINGLE vdatu FROM vbak
                        INTO ct_postab-vdatu
                        WHERE vbeln = lv_vgbel.
  ENDDO.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  MODIFY ct_postab INDEX l_postab_tabix.
ENDLOOP.

REFRESH lt_paletes.
