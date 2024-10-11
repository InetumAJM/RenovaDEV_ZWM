FUNCTION zwm_calcula_palete.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(ACTUALIZA) LIKE  ZWM_AUX-ACTUALIZA
*"     REFERENCE(REMONTADA) TYPE  FLAG OPTIONAL
*"  TABLES
*"      ZPALETE_PICKING STRUCTURE  ZPALETE_PICKING
*"----------------------------------------------------------------------
  CONSTANTS: c_voleh LIKE mara-voleh VALUE 'M3'.

  DATA: mara TYPE mara.
  DATA: mlgt TYPE mlgt.

  DATA : l_total_qtd LIKE zpalete_picking-lfimg,
         save_index LIKE sy-tabix,
         st_type_pck LIKE zwm001-valor,
         st_type_pkb LIKE zwm001-valor,
         resto LIKE zpalete_picking-pal_completa,
         divisao LIKE zpalete_picking-pal_completa.

  CLEAR : l_total_qtd,
          save_index,
          st_type_pck,
          resto,
          divisao.

  DATA: itab_mara LIKE zwm_mara OCCURS 0 WITH HEADER LINE.

  TYPES: BEGIN OF lty_grp,
           refnr TYPE lvs_refnr,
           vbeln TYPE vbeln,
           posnr TYPE posnr,
         END OF lty_grp.

  DATA: BEGIN OF itab_mlgn OCCURS 0,
          lgnum LIKE mlgn-lgnum,
          matnr LIKE mlgn-matnr,
          lhmg1 LIKE mlgn-lhmg1,
          lety1 LIKE mlgn-lety1,
          plkpt LIKE mlgn-plkpt,
        END OF itab_mlgn.

  DATA: BEGIN OF itab_mlgt OCCURS 0,
          lgnum LIKE mlgt-lgnum,
          matnr LIKE mlgt-matnr,
          lgtyp LIKE mlgt-lgtyp,
          lgpla LIKE mlgt-lgpla,
        END OF itab_mlgt.

  DATA: lt_pal_picking  TYPE TABLE OF zpalete_picking,
        lt_new_pallet   TYPE TABLE OF zpalete_picking,
        lt_delitem      TYPE TABLE OF l2skdlitem,
        lt_total        TYPE TABLE OF l2sktotal,
        lt_grp_delete   TYPE TABLE OF lty_grp.

  DATA: ls_pal_picking  TYPE zpalete_picking,
        ls_new_pallet   TYPE zpalete_picking,
        ls_delitem      TYPE l2skdlitem,
        ls_grp_delete   TYPE lty_grp.

  DATA: lv_2step      TYPE flag,
        lv_2spart     TYPE flag,
        lv_step       TYPE c,
        lv_last_step  TYPE c,
        lv_last_refnr TYPE lvs_refnr,
        lv_lfimg      TYPE menge_d,
        lv_tabix      TYPE sytabix.

  FIELD-SYMBOLS: <ls_new_pallet> TYPE zpalete_picking.

  CHECK NOT zpalete_picking[] IS INITIAL.

** Tipo de depósito picking fixo
  PERFORM get_parameter
          USING armazem
                'ENTRADA_ARMAZEM'
                'ST_PCK'
                st_type_pck.

** Tipo de depósito picking variavel
  PERFORM get_parameter
          USING armazem
                'ENTRADA_ARMAZEM'
                'ST_PKB'
                st_type_pkb.

** Cálculo das paletes completas e incompletas
**********************************************************************
  lt_pal_picking = zpalete_picking[].

  SORT lt_pal_picking BY refnr.

  CHECK NOT zpalete_picking[] IS INITIAL.

  SORT zpalete_picking BY vbeln matnr posnr.

**  Esta comentado pois o standard nao agrupa duas linhas do mesmo
**  material o que faz com que crie as OT´s com base numa so linha da
**  remessa
************************************************************************

*  LOOP AT zpalete_picking.
*    AT NEW matnr.
*      SUM.
*      MOVE zpalete_picking-lfimg TO l_total_qtd.
*    ENDAT.
*
*    MOVE l_total_qtd TO zpalete_picking-lfimg.
*    MODIFY zpalete_picking.
*
*    CLEAR zpalete_picking.
*  ENDLOOP.
*
*** Falta o caso em que se repetem items na encomenda e se apaga um de
*** oferta - VER CADERNO
* DELETE ADJACENT DUPLICATES FROM zpalete_picking COMPARING vbeln matnr.

************************************************************************
  SELECT matnr meins volum voleh
  FROM mara
  INTO TABLE itab_mara
  FOR ALL ENTRIES IN zpalete_picking
  WHERE matnr EQ zpalete_picking-matnr.

  SORT itab_mara BY matnr.

  SELECT lgnum matnr lhmg1 lety1
  FROM mlgn
  INTO TABLE itab_mlgn
  FOR ALL ENTRIES IN zpalete_picking
  WHERE matnr EQ zpalete_picking-matnr
    AND lgnum EQ armazem.

  SORT itab_mlgn BY lgnum matnr.

  LOOP AT itab_mara.
    save_index = sy-tabix.
    CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
      EXPORTING
        input                = itab_mara-volum
        kzmeinh              = 'X'
        matnr                = itab_mara-matnr
        meinh                = itab_mara-voleh
        meins                = c_voleh
      IMPORTING
        output               = itab_mara-volum
      EXCEPTIONS
        conversion_not_found = 1
        input_invalid        = 2
        material_not_found   = 3
        meinh_not_found      = 4
        meins_missing        = 5
        no_meinh             = 6
        output_invalid       = 7
        overflow             = 8
        OTHERS               = 9.

    MODIFY itab_mara INDEX save_index.
  ENDLOOP.
*  IF actualiza NE 'X'.
*    LOOP AT itab_mara.
*      READ TABLE itab_mlgn WITH KEY lgnum = armazem
*                                    matnr = itab_mara-matnr.
*      IF sy-subrc NE 0.
*        MESSAGE ID 'ZMSG' TYPE 'I' NUMBER '000'
*          WITH 'Material' itab_mara-matnr 'não existe no dep.'
*               armazem.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  SELECT lgnum matnr lgtyp lgpla
  FROM mlgt
  INTO TABLE itab_mlgt
  FOR ALL ENTRIES IN zpalete_picking
  WHERE matnr EQ zpalete_picking-matnr
    AND lgnum EQ armazem
    AND lgtyp EQ st_type_pck.

  SORT itab_mlgt BY lgnum matnr.

** Ir ao mestre de materiais verificar qual a quantidade que faz uma
** palete completa para fazer os cálculos
  DELETE zpalete_picking WHERE matnr IS INITIAL.

  LOOP AT zpalete_picking.

    save_index = sy-tabix.

    CLEAR: mara.
    READ TABLE itab_mara WITH KEY matnr = zpalete_picking-matnr
                         BINARY SEARCH.
    MOVE-CORRESPONDING itab_mara TO mara.

** quantidade da remessa na unidade medida base
** não é necessário realizar conversão
*    IF mara-meins = zpalete_picking-meins.
    IF mara-meins = zpalete_picking-vrkme.

      CLEAR: mlgn.
      READ TABLE itab_mlgn WITH KEY lgnum = armazem
                                    matnr = zpalete_picking-matnr
                           BINARY SEARCH.

      MOVE-CORRESPONDING itab_mlgn TO mlgn.

      IF NOT mlgn-lhmg1 IS INITIAL.
        IF NOT actualiza IS INITIAL OR NOT remontada IS INITIAL.
          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.

            zpalete_picking-pal_completa =
                        zpalete_picking-lfimg DIV mlgn-lhmg1.

            divisao = zpalete_picking-pal_completa DIV 2.

            resto = zpalete_picking-pal_completa MOD 2.

            IF NOT resto IS INITIAL.
              zpalete_picking-pal_completa = divisao + 1.
            ELSE.
              zpalete_picking-pal_completa = divisao.
            ENDIF.

          ELSE.
            zpalete_picking-pal_completa =
                         zpalete_picking-lfimg DIV mlgn-lhmg1.
          ENDIF.
        ELSE.
          zpalete_picking-pal_completa =
                 zpalete_picking-lfimg DIV mlgn-lhmg1.
        ENDIF.
        zpalete_picking-uni_incompleta =
                    zpalete_picking-lfimg MOD mlgn-lhmg1.

        zpalete_picking-volum =
                    zpalete_picking-uni_incompleta * mara-volum.
        CLEAR zpalete_picking-volum_acumulado.
      ENDIF.
** quantidade da remessa não se encontra na unidade base
** necessário realizar conversão
*    ELSEIF mara-meins <> zpalete_picking-meins.
    ELSEIF mara-meins <> zpalete_picking-vrkme.

      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
        EXPORTING
          input                = zpalete_picking-lfimg
          kzmeinh              = 'X'
          matnr                = zpalete_picking-matnr
*          meinh                = zpalete_picking-meins
          meinh                = zpalete_picking-vrkme
          meins                = mara-meins
        IMPORTING
          output               = zpalete_picking-lfimg
        EXCEPTIONS
          conversion_not_found = 1
          input_invalid        = 2
          material_not_found   = 3
          meinh_not_found      = 4
          meins_missing        = 5
          no_meinh             = 6
          output_invalid       = 7
          overflow             = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

** Depois de convertida os cálculos são os mesmos

      CLEAR: mlgn.
      READ TABLE itab_mlgn WITH KEY lgnum = armazem
                                    matnr = zpalete_picking-matnr
                           BINARY SEARCH.

      MOVE-CORRESPONDING itab_mlgn TO mlgn.

      IF NOT mlgn-lhmg1 IS INITIAL.

        IF NOT actualiza IS INITIAL OR NOT remontada IS INITIAL.

          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.

            zpalete_picking-pal_completa =
                        zpalete_picking-lfimg DIV mlgn-lhmg1.

            divisao = zpalete_picking-pal_completa DIV 2.

            resto = zpalete_picking-pal_completa MOD 2.

            IF NOT resto IS INITIAL.
              zpalete_picking-pal_completa = divisao + 1.
            ELSE.
              zpalete_picking-pal_completa = divisao.
            ENDIF.

          ELSE.
            zpalete_picking-pal_completa =
                         zpalete_picking-lfimg DIV mlgn-lhmg1.
          ENDIF.

        ELSE.
          zpalete_picking-pal_completa =
                 zpalete_picking-lfimg DIV mlgn-lhmg1.
        ENDIF.

        zpalete_picking-uni_incompleta =
                    zpalete_picking-lfimg MOD mlgn-lhmg1.

        zpalete_picking-volum =
                    zpalete_picking-uni_incompleta * mara-volum.
        CLEAR zpalete_picking-volum_acumulado.
      ENDIF.

    ENDIF.

** Verificar qual a posição fixa de picking - MUDAR FIXOS !!!!
** Verificar o q acontece para os q não tem posição fixa

    CLEAR: itab_mlgt, mlgt.
    READ TABLE itab_mlgt WITH KEY matnr = zpalete_picking-matnr
                                  lgnum = armazem
                                  lgtyp = st_type_pck
                         BINARY SEARCH.
    IF sy-subrc = 0.

      IF itab_mlgt-lgpla IS INITIAL.
        zpalete_picking-sorlp = '400'.
      ELSE.

        MOVE-CORRESPONDING itab_mlgt TO mlgt.

        SELECT SINGLE sorlp FROM lagp INTO zpalete_picking-sorlp
                            WHERE lgnum = armazem AND
                                  lgtyp = st_type_pck AND
                                  lgpla = mlgt-lgpla.
      ENDIF.
    ELSE.

      zpalete_picking-sorlp = '400'.

    ENDIF.

    MODIFY zpalete_picking INDEX save_index.
    CLEAR : mlgt-lgpla.

  ENDLOOP.

** Verificar se tem picking mais baixo que a unidade de medida base.
  LOOP AT zpalete_picking WHERE uni_incompleta IS NOT INITIAL.
    zpalete_picking-uni_incom_p = zpalete_picking-uni_incompleta MOD 1.
    MODIFY zpalete_picking.
  ENDLOOP.

** Já tenho as unidades de picking ... calcular paletes de
** picking - para volumes iguais ao da remessa

  CALL FUNCTION 'ZWM_CALCULA_VOLUME'
    EXPORTING
      armazem         = armazem
      actualiza       = actualiza
    TABLES
      zpalete_picking = zpalete_picking
      itab_mara       = itab_mara.

ENDFUNCTION.
