FUNCTION zwm_actualiza_pal_grupo .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGNUM) LIKE  T311-LGNUM
*"     REFERENCE(REFNR) LIKE  T311-REFNR
*"  EXCEPTIONS
*"      ACTUALIZACAO_ZWM0028
*"----------------------------------------------------------------------

  TABLES: likp, lips, vbuk, vbss, mlgn, zwm001, zwm026, zwm028, zwm040.

  DATA: lt_zwm001    LIKE zwm001          OCCURS 0 WITH HEADER LINE,
        lt_lips      LIKE lips            OCCURS 0 WITH HEADER LINE,
        lt_vbss      LIKE vbss            OCCURS 0 WITH HEADER LINE,
        itab_paletes LIKE zpalete_picking OCCURS 0 WITH HEADER LINE,
        t_zwm026     LIKE zwm026          OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF itab_sum OCCURS 0,
          vbeln LIKE lips-vbeln,
          servisan,
          total LIKE sy-tabix,"zwm028-total_paletes,
        END OF itab_sum.

  DATA: BEGIN OF itab_26 OCCURS 0,
          n_pal_picking LIKE zwm026-n_pal_picking,
        END OF itab_26.

  DATA: l_refnr  LIKE t311-refnr,
        l_pstyv1 LIKE lips-pstyv,
        l_pstyv2 LIKE lips-pstyv,
        l_pal    LIKE zwm026-n_pal_picking.

  DATA: wa_zwm028 LIKE zwm028.

  DATA: resto LIKE zpalete_picking-pal_completa,
        divisao LIKE zpalete_picking-pal_completa.

  DATA num_pal LIKE itab_sum-total.

  FREE: itab_paletes, lt_zwm001, lt_lips, lt_vbss, itab_sum, itab_26.
  CLEAR: itab_paletes, lt_zwm001, lt_lips, lt_vbss, l_refnr, l_pstyv1,
         l_pstyv2, itab_sum, itab_26.

  CLEAR: zwm001.
  SELECT SINGLE * FROM zwm001
  WHERE armazem   EQ lgnum
    AND processo  EQ 'REABASTECIMENTO'
    AND parametro EQ 'ST_PST1'.
  IF sy-subrc EQ 0.
    l_pstyv1 = zwm001-valor.
  ENDIF.

  CLEAR: zwm001.
  SELECT SINGLE * FROM zwm001
  WHERE armazem   EQ lgnum
    AND processo  EQ 'REABASTECIMENTO'
    AND parametro EQ 'ST_PST2'.
  IF sy-subrc EQ 0.
    l_pstyv2 = zwm001-valor.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = refnr
    IMPORTING
      output = l_refnr.

** Obter as remessas do grupo
  SELECT * FROM vbss
  INTO TABLE lt_vbss
  WHERE sammg = l_refnr.

*  CHECK NOT lt_vbss[] IS INITIAL.

** Eliminar as remessas que já têm to´s criadas
*  LOOP AT lt_vbss.
*    CLEAR vbuk.
*    SELECT SINGLE * FROM vbuk
*    WHERE vbeln EQ lt_vbss-vbeln
*      AND lvstk EQ 'B'.
*    CHECK sy-subrc EQ 0.
*    DELETE lt_vbss WHERE vbeln = vbuk-vbeln.
*    CLEAR lt_vbss.
*  ENDLOOP.

  CHECK NOT lt_vbss[] IS INITIAL.

  SELECT * FROM lips
  INTO TABLE lt_lips
  FOR ALL ENTRIES IN lt_vbss
  WHERE vbeln EQ lt_vbss-vbeln.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 19.07.2012 11:43:19
*  Motivo: Apaga Items não relevantes para picking
*--------------------------------------------------------------------*
  DELETE lt_lips WHERE lgnum <> lgnum.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

** PSTYV = ZPAL or ZPAS
  DELETE lt_lips WHERE ( pstyv = l_pstyv1 OR pstyv = l_pstyv2 ).

  FREE: lt_vbss.
  CLEAR: lt_vbss.

  LOOP AT lt_lips.
    CLEAR itab_paletes.
    MOVE-CORRESPONDING lt_lips TO itab_paletes.

    CLEAR: likp.
    SELECT SINGLE kunag FROM likp
    INTO itab_paletes-kunag
    WHERE vbeln EQ lt_lips-vbeln.
    itab_paletes-refnr = refnr.
    APPEND itab_paletes.
  ENDLOOP.

** Cálculo das paletes
  CALL FUNCTION 'ZWM_PAL_PICKING_COMPLETE'
    EXPORTING
      i_lgnum           = lgnum
      i_actualiza       = ' '
    TABLES
      zpalete_picking = itab_paletes[].

  break roffd.

** Obter o total de paletes completas
  LOOP AT itab_paletes.

    FREE: itab_26.
    CLEAR: itab_26, itab_sum, zwm028, l_pal.

** Verificar se a Remessa deriva de um cliente Servisan
    SELECT SINGLE * FROM zwm028
    WHERE lgnum   EQ lgnum
      AND refnr   EQ l_refnr
      AND remessa EQ itab_paletes-vbeln.
    IF sy-subrc NE 0.
      CLEAR: zwm040.
      SELECT SINGLE * FROM zwm040
      WHERE lgnum   EQ lgnum
        AND refnr   EQ l_refnr
        AND kunnr   EQ itab_paletes-kunag
        AND remessa EQ itab_paletes-vbeln.
      IF sy-subrc EQ 0.
        itab_sum-vbeln = zwm040-id_servisan.
        itab_sum-servisan = 'X'.
        IF NOT itab_paletes-pal_completa IS INITIAL.
*          itab_sum-total = 1.
** SG
          CLEAR: mlgn, divisao, resto.
          SELECT SINGLE *
              FROM mlgn
                  WHERE matnr = itab_paletes-matnr AND
                        lgnum = lgnum.

          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.

            divisao = itab_paletes-pal_completa DIV 2.
            resto = itab_paletes-pal_completa MOD 2.

            IF NOT resto IS INITIAL.
              itab_sum-total = divisao + 1.
            ELSE.
              itab_sum-total = divisao.
            ENDIF.

          ELSE.
** SG
            itab_sum-total = itab_paletes-pal_completa.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR: itab_sum-servisan.
      itab_sum-vbeln = itab_paletes-vbeln.
      IF NOT itab_paletes-pal_completa IS INITIAL.
*        itab_sum-total = 1.
** SG
        CLEAR: mlgn, divisao, resto.
        SELECT SINGLE *
            FROM mlgn
                WHERE matnr = itab_paletes-matnr AND
                      lgnum = lgnum.

        IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.

          divisao = itab_paletes-pal_completa DIV 2.
          resto = itab_paletes-pal_completa MOD 2.

          IF NOT resto IS INITIAL.
            itab_sum-total = divisao + 1.
          ELSE.
            itab_sum-total = divisao.
          ENDIF.

        ELSE.
** SG
          itab_sum-total = itab_paletes-pal_completa.
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK NOT itab_sum-vbeln IS INITIAL.
*          NOT itab_sum-total IS INITIAL.
    COLLECT itab_sum.
    SORT itab_sum.
  ENDLOOP.

  SORT itab_sum.

  IF itab_sum[] IS INITIAL.
    RAISE actualizacao_zwm0028.
  ENDIF.

** Actualizar a tabela ZWM028
  CLEAR: wa_zwm028.

  LOOP AT itab_sum.

** Obter o total de paletes incompletas
    IF itab_sum-servisan = 'X'.

**  SG 02.06.2005

      CLEAR: zwm040, t_zwm026.
      REFRESH t_zwm026.

      SELECT * FROM zwm040
      WHERE lgnum       EQ lgnum
        AND refnr       EQ l_refnr
        AND id_servisan EQ itab_sum-vbeln.

        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_zwm026
            FROM zwm026
             WHERE armazem   EQ lgnum
               AND grupo     EQ l_refnr
               AND remessa   EQ zwm040-remessa
               AND to_number NE space.

      ENDSELECT.

      SORT t_zwm026 BY n_pal_picking.

      DELETE ADJACENT DUPLICATES FROM t_zwm026 COMPARING n_pal_picking.

      CLEAR num_pal.
      DESCRIBE TABLE t_zwm026 LINES num_pal.

      itab_sum-total = itab_sum-total + num_pal.

**  SG 02.06.2005

*      SELECT * FROM zwm040
*      WHERE lgnum       EQ lgnum
*        AND refnr       EQ l_refnr
*        AND id_servisan EQ itab_sum-vbeln.
*
*        CLEAR: zwm026.
*        SELECT DISTINCT n_pal_picking FROM zwm026
*        INTO l_pal
*        WHERE armazem   EQ lgnum
*          AND grupo     EQ l_refnr
*          AND remessa   EQ zwm040-remessa
*          AND to_number NE space.
*          itab_sum-total = itab_sum-total + 1.
*        ENDSELECT.
*      ENDSELECT.
    ELSE.
      CLEAR: zwm026.
      SELECT DISTINCT n_pal_picking FROM zwm026
      INTO l_pal
      WHERE armazem   EQ lgnum
        AND grupo     EQ l_refnr
        AND remessa   EQ itab_sum-vbeln
        AND to_number NE space.
        itab_sum-total = itab_sum-total + 1.
      ENDSELECT.
    ENDIF.

    CLEAR: zwm028.
    SELECT SINGLE * FROM zwm028
    WHERE lgnum   EQ lgnum
      AND refnr   EQ l_refnr
      AND remessa EQ itab_sum-vbeln.

    CHECK NOT zwm028-remessa IS INITIAL.
    MOVE-CORRESPONDING zwm028 TO wa_zwm028.
    wa_zwm028-total_paletes = itab_sum-total.
    MODIFY zwm028 FROM wa_zwm028.
  ENDLOOP.

  COMMIT WORK AND WAIT.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    RAISE actualizacao_zwm0028.
  ENDIF.

ENDFUNCTION.
