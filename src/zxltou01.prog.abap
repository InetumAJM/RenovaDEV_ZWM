*&---------------------------------------------------------------------*
*&  Include           ZXLTOU01                                         *
*&---------------------------------------------------------------------*
TABLES : lips, t311, vbss, zwm026,
         zwm028, lagp, ltap, zwm001.

INCLUDE zwm_constants.

DATA: t_zwm026  LIKE zwm026 OCCURS 0 WITH HEADER LINE,
      ti_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE,
      w_zwm026  LIKE zwm026,
      wa_zwm026 LIKE zwm026.

DATA: pal_picking  LIKE zwm026-n_pal_picking,
      qtd_aux      LIKE zwm026-quantidade,
      sub_item     LIKE zwm026-sub_item,
      actualiza(1).

DATA: ls_ltap TYPE ltap_vb.

DATA: lv_last      TYPE flag,
      lv_pck_break TYPE flag,
      lv_remessa   TYPE vbeln.

CLEAR : t311, vbss, zwm026, t_zwm026, sub_item,
        actualiza, ti_zwm026.

REFRESH: t_zwm026, ti_zwm026.


DATA: lv_exit   TYPE flag,
      lv_2spart TYPE flag.

*DO.
*  IF lv_exit EQ 'X'.
*    EXIT.
*  ENDIF.
*ENDDO.


** Logica Armazem Automatico
***********************************************************************
CALL FUNCTION 'Z_WMFR_EXIT_TO_CREATE_ARMAUTO'
  EXPORTING
    is_ltak_vb = i_ltak_vb
    it_ltap_vb = t_ltap_vb[]
  EXCEPTIONS
    error      = 1
    not_exit   = 2
    OTHERS     = 3.
***********************************************************************
CALL FUNCTION 'ZWM_EXIT_TO_CREATE_WCS' IN BACKGROUND TASK
  EXPORTING
    is_ltak_vb       = i_ltak_vb
    it_ltap_vb       = t_ltap_vb[]
 EXCEPTIONS
   ERROR            = 1
   OTHERS           = 2.


** Só para TO's de saída de remessa
IF i_ltak_vb-bwlvs = '601'.

  READ TABLE t_ltap_vb
        INTO ls_ltap
        INDEX 1.

  IF ls_ltap-vltyp EQ '815'.
    EXIT.
  ENDIF.

** Grupo de WM
  SELECT SINGLE refnr FROM t311 INTO t311-refnr
                      WHERE sammg = i_ltak_vb-refnr.
  IF sy-subrc = 0.
** Verificar se existe entradas na tabela para a remessa
    SELECT SINGLE * FROM zwm026
             WHERE armazem = i_ltak_vb-lgnum AND
                   remessa = i_ltak_vb-vbeln.
    IF sy-subrc = 0.
** se existir pelo menos uma entrada actualiza-se todas da remessa ...
** porque o grupo é o mesmo
      IF zwm026-grupo IS INITIAL.
        UPDATE zwm026 SET grupo = t311-refnr
                      WHERE armazem = i_ltak_vb-lgnum AND
                            remessa = i_ltak_vb-vbeln.
      ENDIF.

      SORT t_ltap_vb BY lgnum vbeln.
** Actualizar a TO correspondente ao par Remessa/POSNR/Material
      READ TABLE t_ltap_vb WITH KEY lgnum = i_ltak_vb-lgnum
                                    vbeln = i_ltak_vb-vbeln.
      IF sy-subrc = 0.

        SELECT SINGLE *
            FROM zwm001
                WHERE armazem = i_ltak_vb-lgnum AND
                      processo = 'PICKING' AND
                      parametro = 'VOLUME_PAL_PICKING'.

*        CHECK t_ltap_vb-volum < zwm001-valor.

        CLEAR lagp.
        SELECT SINGLE *
            FROM lagp
                WHERE lgnum = t_ltap_vb-lgnum AND
                      lgtyp = t_ltap_vb-vltyp AND
                      lgpla = t_ltap_vb-vlpla.

        CHECK t_ltap_vb-vltyp = 'PCK' OR t_ltap_vb-vltyp = 'PKB' OR
              t_ltap_vb-vltyp = 'PKL'.

        actualiza = 'X'.
        SELECT SINGLE *
            FROM zwm026
                WHERE armazem = t_ltap_vb-lgnum AND
                      remessa = t_ltap_vb-vbeln AND
                      posnr   = t_ltap_vb-posnr AND
                      material = t_ltap_vb-matnr AND
                      quantidade = t_ltap_vb-vsolm AND
                      lote = '          '.

        IF sy-subrc = 0.

          CLEAR wa_zwm026.
          MOVE-CORRESPONDING zwm026 TO wa_zwm026.
          wa_zwm026-to_number = t_ltap_vb-tanum.
          wa_zwm026-lote = t_ltap_vb-charg.
          wa_zwm026-sorlp = lagp-sorlp.

          UPDATE zwm026 FROM wa_zwm026.

*          UPDATE zwm026 SET to_number = t_ltap_vb-tanum
*                            lote = t_ltap_vb-charg
*                            sorlp = lagp-sorlp
*                        WHERE armazem = t_ltap_vb-lgnum AND
*                              remessa = t_ltap_vb-vbeln AND
*                              posnr   = t_ltap_vb-posnr AND
*                              material = t_ltap_vb-matnr AND
*                              quantidade = t_ltap_vb-vsolm.
          CLEAR actualiza.

        ELSE.

          CLEAR t_zwm026.
          REFRESH t_zwm026.

          SELECT * INTO TABLE t_zwm026
              FROM zwm026
                  WHERE armazem = t_ltap_vb-lgnum AND
                        remessa = t_ltap_vb-vbeln AND
                        posnr   = t_ltap_vb-posnr.

          LOOP AT t_zwm026.
            IF t_zwm026-to_number IS INITIAL.

              IF t_zwm026-quantidade <> t_ltap_vb-vsolm.

                CLEAR w_zwm026.

                MOVE-CORRESPONDING t_zwm026 TO w_zwm026.

                DELETE zwm026 FROM t_zwm026.

                w_zwm026-quantidade = t_ltap_vb-vsolm.
                w_zwm026-sorlp = lagp-sorlp.
                w_zwm026-to_number = t_ltap_vb-tanum.
                w_zwm026-lote = t_ltap_vb-charg.
                INSERT INTO zwm026 VALUES w_zwm026.

                CLEAR actualiza.
                EXIT.

              ELSEIF t_zwm026-quantidade = t_ltap_vb-vsolm.

                CLEAR wa_zwm026.
                MOVE-CORRESPONDING t_zwm026 TO wa_zwm026.
                wa_zwm026-to_number = t_ltap_vb-tanum.
                wa_zwm026-lote = t_ltap_vb-charg.
                wa_zwm026-sorlp = lagp-sorlp.

                UPDATE zwm026 FROM wa_zwm026.

*                UPDATE zwm026 SET to_number = t_ltap_vb-tanum
*                            lote = t_ltap_vb-charg
*                            sorlp = lagp-sorlp
*                        WHERE armazem = t_ltap_vb-lgnum AND
*                              remessa = t_ltap_vb-vbeln AND
*                              posnr   = t_ltap_vb-posnr AND
*                              material = t_ltap_vb-matnr AND
*                              quantidade = t_ltap_vb-vsolm.
                CLEAR actualiza.

                EXIT.
              ENDIF.

            ELSEIF NOT t_zwm026-to_number IS INITIAL.
              actualiza = 'X'.
              CONTINUE.
            ENDIF.
          ENDLOOP.

          CLEAR: pal_picking, qtd_aux.
          IF NOT actualiza IS INITIAL.

            CLEAR w_zwm026.
** Juntar esta entrada á palete com menos quantidade
            SELECT * INTO TABLE ti_zwm026
                FROM zwm026
                    WHERE armazem  = t_ltap_vb-lgnum AND
                          remessa  = t_ltap_vb-vbeln AND
                          posnr    = t_ltap_vb-posnr AND
                          material = t_ltap_vb-matnr
                    ORDER BY quantidade ASCENDING.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.08.2012 12:02:35
*  Motivo: Caso exista fica com a entrada da ultima linha da palete
*--------------------------------------------------------------------*
            LOOP AT ti_zwm026.
              IF ti_zwm026-pal_picking > 0.
                EXIT.
              ENDIF.
            ENDLOOP.

*            READ TABLE ti_zwm026 INDEX 1.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

            MOVE-CORRESPONDING ti_zwm026 TO w_zwm026.

            CLEAR sub_item.
            SELECT MAX( sub_item ) INTO sub_item
                FROM zwm026
                     WHERE armazem = t_ltap_vb-lgnum AND
*                           n_pal_picking = pal_picking AND
                           remessa = t_ltap_vb-vbeln AND
                           posnr   = t_ltap_vb-posnr.

            w_zwm026-sub_item = sub_item + 1.
            w_zwm026-quantidade = t_ltap_vb-vsolm.
            w_zwm026-sorlp = lagp-sorlp.
            w_zwm026-to_number = t_ltap_vb-tanum.
            w_zwm026-lote = t_ltap_vb-charg.
            INSERT INTO zwm026 VALUES w_zwm026.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.08.2012 11:45:48
*  Motivo: Altera os restantes para zero
*--------------------------------------------------------------------*
            UPDATE zwm026 SET pal_picking = 0
                    WHERE armazem   = t_ltap_vb-lgnum AND
                          remessa   = t_ltap_vb-vbeln AND
                          posnr     = t_ltap_vb-posnr AND
                          to_number <> t_ltap_vb-tanum.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


            CLEAR actualiza.
            EXIT.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

** Actualização da tabela zwm028 com o grupo
** Verificar se existe entradas na tabela para a remessa
    SELECT SINGLE * FROM zwm028
             WHERE lgnum = i_ltak_vb-lgnum AND
                   remessa = i_ltak_vb-vbeln.
    IF sy-subrc = 0.
** se existir pelo menos uma entrada actualiza-se todas da remessa ...
** porque o grupo é o mesmo
      IF zwm028-refnr IS INITIAL.
        UPDATE zwm028 SET refnr = t311-refnr
                      WHERE lgnum = i_ltak_vb-lgnum AND
                            remessa = i_ltak_vb-vbeln.
      ENDIF.
    ENDIF.

  ENDIF.

  READ TABLE t_ltap_vb INDEX 1.
  IF sy-subrc = 0.
** Actualização do certificado da TO
    SELECT SINGLE * FROM lagp
                    WHERE lgnum = t_ltap_vb-lgnum AND
                          lgtyp = t_ltap_vb-vltyp AND
                          lgpla = t_ltap_vb-vlpla.

    UPDATE ltap SET zeugn = lagp-lzone
                WHERE lgnum = t_ltap_vb-lgnum AND
                      tanum = t_ltap_vb-tanum AND
                      tapos = t_ltap_vb-tapos AND
                      vltyp = t_ltap_vb-vltyp AND
                      vlpla = t_ltap_vb-vlpla.
  ENDIF.

ELSEIF i_ltak_vb-bwlvs = '850'.

** Grupo de WM
  SELECT SINGLE refnr FROM t311 INTO t311-refnr
                      WHERE sammg = i_ltak_vb-refnr.
  IF sy-subrc = 0.
    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = i_ltak_vb-lgnum
        i_refnr  = i_ltak_vb-refnr
      IMPORTING
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2spart EQ 'X'.
      lv_remessa = i_ltak_vb-benum.
    ELSE.
      lv_remessa = gc_vbeln_2step_dummy.
    ENDIF.


**  Verificar se existe entradas na tabela para a remessa
    SELECT SINGLE * FROM zwm026
             WHERE armazem = i_ltak_vb-lgnum AND
                   grupo   = i_ltak_vb-refnr AND
                   remessa = lv_remessa.

    IF sy-subrc = 0.
      SORT t_ltap_vb BY lgnum vbeln.
** Actualizar a TO correspondente ao par Remessa/POSNR/Material
      READ TABLE t_ltap_vb INDEX 1.

      IF sy-subrc = 0.

        SELECT SINGLE *
            FROM zwm001
                WHERE armazem = i_ltak_vb-lgnum AND
                      processo = 'PICKING' AND
                      parametro = 'VOLUME_PAL_PICKING'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 27.09.2012 11:09:17
*  Motivo: Não executa exit no caso de ser palete completa do Pck
*          FM - ZWM_TO_VALIDATE_PICKING
*--------------------------------------------------------------------*
        IMPORT lv_pck_break FROM MEMORY ID 'ZPCK_BREAK'.
        CHECK lv_pck_break IS INITIAL.

        FREE MEMORY ID 'ZPCK_BREAK'.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*        CHECK t_ltap_vb-volum < zwm001-valor.

        CLEAR lagp.
        SELECT SINGLE *
            FROM lagp
                WHERE lgnum = t_ltap_vb-lgnum AND
                      lgtyp = t_ltap_vb-vltyp AND
                      lgpla = t_ltap_vb-vlpla.

        CHECK t_ltap_vb-vltyp = 'PCK' OR t_ltap_vb-vltyp = 'PKB'.

        actualiza = 'X'.


        SELECT SINGLE *
            FROM zwm026
                WHERE armazem = t_ltap_vb-lgnum AND
                      grupo   = i_ltak_vb-refnr AND
                      remessa = lv_remessa AND
                      posnr   = gc_posnr_2step_dummy AND
                      material = t_ltap_vb-matnr AND
                      quantidade = t_ltap_vb-vsolm AND
                      lote = '          '.


        IF sy-subrc = 0.

          CLEAR wa_zwm026.
          MOVE-CORRESPONDING zwm026 TO wa_zwm026.
          wa_zwm026-to_number = t_ltap_vb-tanum.
          wa_zwm026-lote = t_ltap_vb-charg.
          wa_zwm026-sorlp = lagp-sorlp.

          UPDATE zwm026 FROM wa_zwm026.

*          UPDATE zwm026 SET to_number = t_ltap_vb-tanum
*                            lote = t_ltap_vb-charg
*                            sorlp = lagp-sorlp
*                        WHERE armazem = t_ltap_vb-lgnum AND
*                              remessa = t_ltap_vb-vbeln AND
*                              posnr   = t_ltap_vb-posnr AND
*                              material = t_ltap_vb-matnr AND
*                              quantidade = t_ltap_vb-vsolm.
          CLEAR actualiza.

        ELSE.

          CLEAR t_zwm026.
          REFRESH t_zwm026.


          SELECT * INTO TABLE t_zwm026
              FROM zwm026
                  WHERE armazem = t_ltap_vb-lgnum AND
                        grupo   = i_ltak_vb-refnr AND
                        remessa = lv_remessa AND
                        posnr   = gc_posnr_2step_dummy AND
                        material = t_ltap_vb-matnr.


          LOOP AT t_zwm026.

            IF t_zwm026-to_number IS INITIAL.

              IF t_zwm026-quantidade <> t_ltap_vb-vsolm.

                CLEAR w_zwm026.

                MOVE-CORRESPONDING t_zwm026 TO w_zwm026.

                DELETE zwm026 FROM t_zwm026.

                w_zwm026-quantidade = t_ltap_vb-vsolm.
                w_zwm026-sorlp = lagp-sorlp.
                w_zwm026-to_number = t_ltap_vb-tanum.
                w_zwm026-lote = t_ltap_vb-charg.
                INSERT INTO zwm026 VALUES w_zwm026.

                CLEAR actualiza.
                EXIT.

              ELSEIF t_zwm026-quantidade = t_ltap_vb-vsolm.

                CLEAR wa_zwm026.
                MOVE-CORRESPONDING t_zwm026 TO wa_zwm026.
                wa_zwm026-to_number = t_ltap_vb-tanum.
                wa_zwm026-lote = t_ltap_vb-charg.
                wa_zwm026-sorlp = lagp-sorlp.

                UPDATE zwm026 FROM wa_zwm026.

*                UPDATE zwm026 SET to_number = t_ltap_vb-tanum
*                            lote = t_ltap_vb-charg
*                            sorlp = lagp-sorlp
*                        WHERE armazem = t_ltap_vb-lgnum AND
*                              remessa = t_ltap_vb-vbeln AND
*                              posnr   = t_ltap_vb-posnr AND
*                              material = t_ltap_vb-matnr AND
*                              quantidade = t_ltap_vb-vsolm.
                CLEAR actualiza.

                EXIT.
              ENDIF.

            ELSEIF NOT t_zwm026-to_number IS INITIAL.
              actualiza = 'X'.
              CONTINUE.
            ENDIF.
          ENDLOOP.

          CLEAR: pal_picking, qtd_aux.
          IF NOT actualiza IS INITIAL.

            CLEAR w_zwm026.
** Juntar esta entrada á palete com menos quantidade


            SELECT * INTO TABLE ti_zwm026
                FROM zwm026
                    WHERE armazem  = t_ltap_vb-lgnum AND
                          grupo    = i_ltak_vb-refnr AND
                          remessa  = lv_remessa  AND
                          posnr    = gc_posnr_2step_dummy AND
                          material = t_ltap_vb-matnr
                    ORDER BY quantidade ASCENDING.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.08.2012 12:02:35
*  Motivo: Caso exista fica com a entrada da ultima linha da palete
*--------------------------------------------------------------------*
            LOOP AT ti_zwm026.
              IF ti_zwm026-pal_picking > 0.
                EXIT.
              ENDIF.
            ENDLOOP.

*            READ TABLE ti_zwm026 INDEX 1.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

            MOVE-CORRESPONDING ti_zwm026 TO w_zwm026.

            CLEAR sub_item.
            SELECT MAX( sub_item ) INTO sub_item
                FROM zwm026
                     WHERE armazem = t_ltap_vb-lgnum AND
                           grupo   = i_ltak_vb-refnr AND
                           remessa = lv_remessa AND
                           posnr   = gc_posnr_2step_dummy.

            w_zwm026-sub_item = sub_item + 1.
            w_zwm026-quantidade = t_ltap_vb-vsolm.
            w_zwm026-sorlp = lagp-sorlp.
            w_zwm026-to_number = t_ltap_vb-tanum.
            w_zwm026-lote = t_ltap_vb-charg.
            INSERT INTO zwm026 VALUES w_zwm026.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.08.2012 11:45:48
*  Motivo: Altera os restantes para zero
*--------------------------------------------------------------------*
            UPDATE zwm026 SET pal_picking = 0
                    WHERE armazem   = t_ltap_vb-lgnum AND
                          grupo     = i_ltak_vb-refnr AND
                          remessa   = lv_remessa  AND
                          posnr     = gc_posnr_2step_dummy AND
                          material  = t_ltap_vb-matnr AND
                          to_number <> t_ltap_vb-tanum.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

            CLEAR actualiza.
            EXIT.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

** Actualização da tabela zwm028 com o grupo
** Verificar se existe entradas na tabela para a remessa
*    SELECT SINGLE * FROM zwm028
*             WHERE lgnum = i_ltak_vb-lgnum AND
*                   remessa = i_ltak_vb-vbeln.
*    IF sy-subrc = 0.
*** se existir pelo menos uma entrada actualiza-se todas da remessa ...
*** porque o grupo é o mesmo
*      IF zwm028-refnr IS INITIAL.
*        UPDATE zwm028 SET refnr = t311-refnr
*                      WHERE lgnum = i_ltak_vb-lgnum AND
*                            remessa = i_ltak_vb-vbeln.
*      ENDIF.
*    ENDIF.

  ENDIF.

  READ TABLE t_ltap_vb INDEX 1.
  IF sy-subrc = 0.
** Actualização do certificado da TO
    SELECT SINGLE * FROM lagp
                    WHERE lgnum = t_ltap_vb-lgnum AND
                          lgtyp = t_ltap_vb-vltyp AND
                          lgpla = t_ltap_vb-vlpla.

    UPDATE ltap SET zeugn = lagp-lzone
                WHERE lgnum = t_ltap_vb-lgnum AND
                      tanum = t_ltap_vb-tanum AND
                      tapos = t_ltap_vb-tapos AND
                      vltyp = t_ltap_vb-vltyp AND
                      vlpla = t_ltap_vb-vlpla.
  ENDIF.
ENDIF.

** Só para TO's de reabastecimento do PKL
IF i_ltak_vb-bwlvs = '979'.

  READ TABLE t_ltap_vb
        INTO ls_ltap
        INDEX 1.

  CHECK ls_ltap IS NOT INITIAL.

** Não cria OT se origem for PRB (já criou anteriormente OT do INT)
  IF ls_ltap-zeugn = 'X'.

    CALL FUNCTION 'L_TO_CREATE_SINGLE' IN BACKGROUND TASK AS SEPARATE UNIT
      EXPORTING
        i_lgnum = ls_ltap-lgnum
        i_bwlvs = '999'
*       I_BETYP = ' '
*       I_BENUM = ' '
        i_matnr = ls_ltap-matnr
        i_werks = ls_ltap-werks
        i_lgort = ls_ltap-lgort
        i_charg = ls_ltap-charg
*       I_BESTQ = ' '
*       I_SOBKZ = ' '
*       I_SONUM = ' '
*       I_LETYP = ' '
        i_anfme = ls_ltap-nsola
        i_altme = ls_ltap-altme
*       I_WDATU = INIT_DATUM
*       I_VFDAT = INIT_DATUM
*       I_ZEUGN = ' '
*       I_LZNUM = ' '
        i_squit = 'X'
*       I_NIDRU = ' '
*       I_DRUKZ = ' '
*       I_LDEST = ' '
*       I_WEMPF = ' '
*       I_ABLAD = ' '
        i_vltyp = 'INT'
*       I_VLBER = ' '
        i_vlpla = '000-000-01'
*       I_VPPOS = ' '
*       I_VLENR = ' '
*       I_VLQNR = ' '
        i_nltyp = ls_ltap-nltyp
*       I_NLBER = ' '
        i_nlpla = ls_ltap-nlpla
*       I_NPPOS = ' '
*       I_NLENR = ' '
*       I_NLQNR = ' '
*       I_RLTYP = ' '
*       I_RLBER = ' '
*       I_RLPLA = ' '
*       I_RLQNR = ' '
*       I_UPDATE_TASK               = ' '
*       I_COMMIT_WORK               = 'X'
*       I_BNAME = SY-UNAME
*       I_KOMPL = 'X'
*       I_SOLEX = 0
*       I_PERNR = 0
*       I_AUSFB = ' '
*       I_REFNR = ' '
*       I_L2SKA = ' '
*       I_INVENT                    = ' '
*  IMPORTING
*       E_TANUM =
*       E_LTAP  =
*  TABLES
*       T_LTAK  =
*       T_LTAP_VB                   =
*  EXCEPTIONS
*       NO_TO_CREATED               = 1
*       BWLVS_WRONG                 = 2
*       BETYP_WRONG                 = 3
*       BENUM_MISSING               = 4
*       BETYP_MISSING               = 5
*       FOREIGN_LOCK                = 6
*       VLTYP_WRONG                 = 7
*       VLPLA_WRONG                 = 8
*       VLTYP_MISSING               = 9
*       NLTYP_WRONG                 = 10
*       NLPLA_WRONG                 = 11
*       NLTYP_MISSING               = 12
*       RLTYP_WRONG                 = 13
*       RLPLA_WRONG                 = 14
*       RLTYP_MISSING               = 15
*       SQUIT_FORBIDDEN             = 16
*       MANUAL_TO_FORBIDDEN         = 17
*       LETYP_WRONG                 = 18
*       VLPLA_MISSING               = 19
*       NLPLA_MISSING               = 20
*       SOBKZ_WRONG                 = 21
*       SOBKZ_MISSING               = 22
*       SONUM_MISSING               = 23
*       BESTQ_WRONG                 = 24
*       LGBER_WRONG                 = 25
*       XFELD_WRONG                 = 26
*       DATE_WRONG                  = 27
*       DRUKZ_WRONG                 = 28
*       LDEST_WRONG                 = 29
*       UPDATE_WITHOUT_COMMIT       = 30
*       NO_AUTHORITY                = 31
*       MATERIAL_NOT_FOUND          = 32
*       LENUM_WRONG                 = 33
*       OTHERS  = 34
      .
* IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
* ENDIF.
  ENDIF.  .



ENDIF.
