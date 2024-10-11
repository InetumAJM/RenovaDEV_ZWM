FUNCTION zwm_change_pal_pul.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_GRUPO) TYPE  LVS_REFNR
*"     REFERENCE(I_REMESSA) TYPE  VBELN_VL
*"     REFERENCE(I_BIN_OUTPUT_D) TYPE  CHAR14
*"     REFERENCE(I_TIPO_PALETE) TYPE  LVS_LETYP
*"     REFERENCE(I_SU) TYPE  LENUM
*"     REFERENCE(I_POSICAO_PULMAO) TYPE  CHAR2 OPTIONAL
*"     REFERENCE(I_GLOCK_ACTIVE) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_ADD_SU2) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_SU2) TYPE  LENUM OPTIONAL
*"----------------------------------------------------------------------

  DATA lv_glock_active TYPE abap_bool. " << INS ROFF(SDF):TMGP:22.01.2016 14:41:50

  DATA : wait_time(20), lock_key(50).
  DATA: chave_bloq LIKE keyword-keyword.

  DATA: lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        lt_vbpa   LIKE vbpa   OCCURS 0 WITH HEADER LINE,
        lt_zwm013 TYPE zwm013 OCCURS 0 WITH HEADER LINE,
        lt_zwm081 TYPE zwm081 OCCURS 0 WITH HEADER LINE.

  DATA: x_zwm013 LIKE zwm013 OCCURS 0 WITH HEADER LINE.

  DATA: next_desbloq(1), l_kunnr LIKE vbpa-kunnr.

  DATA: lv_2spart     TYPE flag,
        lv_lgtyp_d    TYPE lgtyp,
        lv_pos_pul    TYPE char2,
        lv_lines      TYPE i,
        lv_second_pul TYPE char1.

  DATA: ls_zwm066 TYPE zwm066,
        ls_zwm028 TYPE zwm028.

  DATA: zwm028 TYPE zwm028.
  DATA: zwm013 TYPE zwm013.

** Actualizar Paletes no pulmão
**********************************************************************
  CLEAR : wait_time, lock_key, chave_bloq.

  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = i_bin_output_d
    IMPORTING
      lgtyp = lv_lgtyp_d.

  IF lv_lgtyp_d EQ 'PRM'.
    EXIT.
  ENDIF.

*& Begin of Modification by Carlos Fernandes - ROFF @ 22.02.2016
**& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:42:12
** lv_glock_active = abap_false.
**
**  SELECT valor UP TO 1 ROWS
**    FROM zwm001 INTO lv_glock_active
**    WHERE armazem EQ xuser-lgnum
**      AND processo EQ 'ALTERA_GLOCK'
**      AND parametro EQ 'ACTIVAR'.
**  ENDSELECT.
*
**& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:42:12
  lv_glock_active = i_glock_active.

*  SELECT mandt UP TO 1 ROWS
*    FROM zwm001 INTO sy-mandt
*    WHERE armazem EQ xuser-lgnum
*      AND processo EQ 'CHANGE_ORDEM'
*      AND parametro EQ 'NLTYP_OK'
*      AND valor EQ zwm011-ultimo_tipo_dep.
*  ENDSELECT.
*  IF sy-subrc EQ 0.
*    lv_glock_active = abap_true.
*  ELSE.
*    lv_glock_active = abap_false.
*  ENDIF.
*& End of Modification by Carlos Fernandes - ROFF @ 22.02.2016

** Tempo de espera no lock
  SELECT SINGLE valor
    FROM zwm001 INTO wait_time
    WHERE armazem   = i_lgnum
    AND   processo  = 'ATRIBUICAO_TO'
    AND   parametro = 'WAIT_TIME'.

  MOVE 'PALETES_PULMAO' TO lock_key.
** Fazer Lock para não existirem dois ou
** mais operários a actualizar o número
** de paletes no pulmão
  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM028'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO wait_time SECONDS.
    ENDIF.
  ENDDO.

** RL -> MOD 26.05.2005
** Se a palete ja estiver no pulmao não actualiza as paletes n pulmao
* SELECT SINGLE * FROM zwm013 WHERE armazem = xuser-lgnum AND sscc = su.
  DATA: l_dest LIKE zwm013-destino.

  CONCATENATE i_bin_output_d(3) '%' INTO l_dest.

  SELECT SINGLE * FROM zwm013
  WHERE armazem EQ   i_lgnum
    AND sscc    EQ   i_su
    AND destino LIKE l_dest.

** RL <- MOD 26.05.2005

  IF sy-subrc <> 0.
** Actualizar entrada do grupo com as paletes q já estão no pulmão

    DATA: aux_pal TYPE i,
          l_ordem LIKE zwm028-ordem,
          l_lock  LIKE zwm028-zlock.
    CLEAR: aux_pal, zwm028.

    SELECT SINGLE * FROM zwm028
                    WHERE lgnum = i_lgnum AND
                          refnr = i_grupo AND
                          remessa = ' '.
    IF sy-subrc = 0.
      aux_pal = zwm028-paletes_pulmao + 1.

      UPDATE zwm028 SET paletes_pulmao = aux_pal
          WHERE lgnum = i_lgnum AND
                refnr = i_grupo AND
                remessa = ' '.
      COMMIT WORK AND WAIT.
    ENDIF.

** Actualizar entrada da remessa com as paletes q já estão no pulmão
    CLEAR zwm028.
    SELECT SINGLE * FROM zwm028
                      WHERE lgnum   = i_lgnum AND
                            refnr   = i_grupo AND
                            remessa = i_remessa.
    IF sy-subrc = 0.
      CLEAR aux_pal.
      aux_pal = zwm028-paletes_pulmao + 1.

      UPDATE zwm028 SET paletes_pulmao = aux_pal
          WHERE lgnum   = i_lgnum AND
                refnr   = i_grupo AND
                remessa = i_remessa.
      COMMIT WORK AND WAIT.
    ENDIF.

** Verificar se as paletes no pulmao são iguais á da Remessa para
** desbloquear a remessa de ordem n + 1
    CLEAR zwm028.
    SELECT SINGLE * FROM zwm028
        WHERE lgnum = i_lgnum AND
              refnr = i_grupo AND
              remessa = ' '.

    IF zwm028-st_pul = 'PLT' AND zwm028-tipo_lock = 'R'.
      IF zwm028-paletes_pulmao < zwm028-total_paletes.

        CLEAR: lt_zwm028.
        REFRESH: lt_zwm028.

        SELECT * INTO TABLE lt_zwm028
            FROM zwm028
                WHERE lgnum = i_lgnum
                  AND refnr = i_grupo
                  AND remessa <> ' '.

        CLEAR next_desbloq.
        LOOP AT lt_zwm028 WHERE zlock <> '1'.
          IF lt_zwm028-paletes_pulmao < lt_zwm028-total_paletes.
            CLEAR next_desbloq.
            EXIT.
          ELSE.
            next_desbloq = 'X'.
          ENDIF.
        ENDLOOP.

**  As remessas desbloqueadas já estão todas finalizadas,
**  vamos encontrar a proxima a desbloquear
        IF NOT next_desbloq IS INITIAL.
          DELETE lt_zwm028 WHERE zlock <> '1'.

          IF NOT lt_zwm028[] IS INITIAL.

            CLEAR lt_vbpa.
            REFRESH lt_vbpa.
            SELECT * INTO TABLE lt_vbpa
                FROM vbpa
                    FOR ALL ENTRIES IN lt_zwm028
                        WHERE vbeln EQ lt_zwm028-remessa
                          AND posnr = '000000'
                          AND parvw EQ 'W1'.

*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:01
            IF lv_glock_active EQ abap_false.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:02
              SORT lt_zwm028 BY ordem.
*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
            ELSE.
              SORT lt_zwm028[] BY ordem DESCENDING.
            ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23

            CLEAR lt_zwm028.
            READ TABLE lt_zwm028 INDEX 1.

            CLEAR l_kunnr.
            LOOP AT lt_vbpa WHERE vbeln = lt_zwm028-remessa.
              l_kunnr = lt_vbpa-kunnr.
              EXIT.
            ENDLOOP.

            LOOP AT lt_vbpa WHERE kunnr = l_kunnr.
              UPDATE zwm028 SET zlock = zwm028-zlock
                            WHERE lgnum   = zwm028-lgnum
                              AND refnr   = zwm028-refnr
                              AND remessa = lt_vbpa-vbeln.
              COMMIT WORK AND WAIT.

              CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
                EXPORTING
                  i_lgnum = lt_zwm028-lgnum
                  i_refnr = lt_zwm028-refnr
                  i_vbeln = lt_zwm028-remessa.
            ENDLOOP.

          ENDIF.
        ENDIF.
      ENDIF.

    ELSEIF  ( zwm028-st_pul = 'PUL' OR zwm028-st_pul = 'PUA')  AND zwm028-tipo_lock = 'R'.
      IF zwm028-paletes_pulmao < zwm028-total_paletes.

        CLEAR: lt_zwm028.
        REFRESH: lt_zwm028.

        SELECT * INTO TABLE lt_zwm028
            FROM zwm028
                WHERE lgnum = i_lgnum
                  AND refnr = i_grupo
                  AND remessa <> ' '.

        CLEAR next_desbloq.
        LOOP AT lt_zwm028 WHERE zlock <> '1'.
          IF lt_zwm028-paletes_pulmao < lt_zwm028-total_paletes.
            CLEAR next_desbloq.
            EXIT.
          ELSE.
            next_desbloq = 'X'.
          ENDIF.
        ENDLOOP.

**  As remessas desbloqueadas já estão todas finalizadas,
**  vamos encontrar a proxima a desbloquear
        IF NOT next_desbloq IS INITIAL.
          DELETE lt_zwm028 WHERE zlock <> '1'.

          IF NOT lt_zwm028[] IS INITIAL.

*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:01
            IF lv_glock_active EQ abap_false.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:02
              SORT lt_zwm028 BY ordem.
*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
            ELSE.
              SORT lt_zwm028[] BY ordem DESCENDING.
            ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23

            LOOP AT lt_zwm028.
*            CLEAR lt_zwm028.
*            READ TABLE lt_zwm028 INDEX 1.

              IF lt_zwm028-paletes_pulmao < lt_zwm028-total_paletes.
                UPDATE zwm028 SET zlock = zwm028-zlock
                              WHERE lgnum = zwm028-lgnum
                                AND refnr = zwm028-refnr
                                AND remessa = lt_zwm028-remessa.
                COMMIT WORK AND WAIT.

                CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
                  EXPORTING
                    i_lgnum = lt_zwm028-lgnum
                    i_refnr = lt_zwm028-refnr
                    i_vbeln = lt_zwm028-remessa.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

*        CLEAR zwm028.
*        SELECT SINGLE * FROM zwm028
*            WHERE lgnum = xuser-lgnum AND
*                  refnr = grupo AND
*                  remessa = remessa.
*        IF zwm028-paletes_pulmao >= zwm028-total_paletes.
*
*          CLEAR: l_ordem, l_lock.
*          l_lock = zwm028-zlock.
*          l_ordem = zwm028-ordem + 1.
*          SELECT SINGLE * FROM zwm028
*              WHERE lgnum = xuser-lgnum
*                AND refnr = grupo
*                AND ordem = l_ordem.
*          IF sy-subrc = 0.
*            UPDATE zwm028 SET zlock = l_lock
*                          WHERE lgnum = zwm028-lgnum
*                            AND refnr = zwm028-refnr
*                            AND remessa = zwm028-remessa.
*            COMMIT WORK AND WAIT.
*          ENDIF.
*
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

** Actualizar a posicao no pulmao onde se encontra a palete
  DO 1 TIMES.

    IF ( i_bin_output_d(3) = 'PUL' OR i_bin_output_d(3) = 'PPK' OR
         i_bin_output_d(3) = 'PLM' OR i_bin_output_d(3) = 'PLT' OR
         i_bin_output_d(3) = 'PUA' ) AND NOT i_su IS INITIAL.

      " Carga gravítica
      IF i_bin_output_d(3) = 'PUA'.

        lv_pos_pul = 0.

        SELECT *
          FROM zwm081 INTO TABLE lt_zwm081
          WHERE armazem = i_lgnum
          AND   refnr   = i_grupo.

        DELETE lt_zwm081 WHERE destino <> i_bin_output_d.

        DESCRIBE TABLE lt_zwm081 LINES lv_lines.
        IF lv_lines > 12.
          lv_second_pul = 'X'.
        ENDIF.

        SELECT *
          FROM zwm013 INTO TABLE lt_zwm013
          WHERE armazem = i_lgnum
          AND   destino = i_bin_output_d.

        DELETE lt_zwm013 WHERE refnr <> i_grupo.

        READ TABLE lt_zwm013 WITH KEY sscc = i_su.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.

        SORT lt_zwm013 BY posicao_pulmao DESCENDING.

        READ TABLE lt_zwm013 INDEX 1.
        IF sy-subrc = 0.
          lv_pos_pul = lt_zwm013-posicao_pulmao.
        ELSE.
          lv_pos_pul = 0.
        ENDIF.

        lv_pos_pul = lv_pos_pul + 1.

      ELSE.
        lv_pos_pul = i_posicao_pulmao.
      ENDIF.

      CLEAR x_zwm013.
      REFRESH x_zwm013.
      x_zwm013-armazem        = i_lgnum.
      x_zwm013-sscc           = i_su.
      x_zwm013-destino        = i_bin_output_d.
      x_zwm013-bloqueado      = 'X'.
      x_zwm013-tipo_palete    = i_tipo_palete.
      x_zwm013-posicao_pulmao = lv_pos_pul.
      x_zwm013-second_pulmao  = lv_second_pul.
      x_zwm013-refnr          = i_grupo.
      CLEAR x_zwm013-fabrica_1.
      CLEAR x_zwm013-trans_terc.
      APPEND x_zwm013.

      CLEAR x_zwm013.

** as duas paletes vao para o carro
      IF i_add_su2 IS NOT INITIAL.

        x_zwm013-armazem        = i_lgnum.
        x_zwm013-sscc           = i_su2. "zwm020-p2.
        x_zwm013-destino        = i_bin_output_d.
        x_zwm013-bloqueado      = 'X'.
        x_zwm013-tipo_palete    = i_tipo_palete.
        x_zwm013-posicao_pulmao = lv_pos_pul.
        x_zwm013-second_pulmao  = lv_second_pul.
        x_zwm013-refnr          = i_grupo.

        CLEAR x_zwm013-fabrica_1.
        CLEAR x_zwm013-trans_terc.
        APPEND x_zwm013.
        CLEAR x_zwm013.

** uma palete é devolvida para o PRM
      ELSE.

        DELETE FROM zwm020
            WHERE armazem = i_lgnum AND
                  ( p1 = i_su OR p2 = i_su2 ).
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.

      ENDIF.
      MODIFY zwm013 FROM TABLE x_zwm013.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDDO.

** Update de Paletes de Picking 2 Passos Parcial
***********************************************************************
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = i_lgnum
      i_refnr  = i_grupo
    IMPORTING
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF lv_2spart EQ abap_true.
    SELECT * FROM zwm028
             INTO TABLE lt_zwm028
             WHERE lgnum = i_lgnum AND
                   refnr = i_grupo.

    SORT lt_zwm028 BY zlock DESCENDING.
    READ TABLE lt_zwm028
          INTO ls_zwm028
          INDEX 1.

    DELETE lt_zwm028 WHERE zlock <> ls_zwm028-zlock.

    SORT lt_zwm028 BY ordem DESCENDING.

    CLEAR: ls_zwm028.
    READ TABLE lt_zwm028
          INTO ls_zwm028
          INDEX 1.
    GET TIME.
    CLEAR: ls_zwm066.
    ls_zwm066-lgnum = i_lgnum.
    ls_zwm066-refnr = i_grupo.
    ls_zwm066-vbeln = ls_zwm028-remessa.
    ls_zwm066-lenum = i_su.
    ls_zwm066-erdat = sy-datum.
    ls_zwm066-erzet = sy-uzeit.
    ls_zwm066-ernam = sy-uname.
*    INSERT zwm066 FROM ls_zwm066.
*    COMMIT WORK.
  ENDIF.

** Unlock ao lock efectuado na atribuicao da posicao do pulmao
**********************************************************************
  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM028'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  CLEAR : lock_key, wait_time.


ENDFUNCTION.
