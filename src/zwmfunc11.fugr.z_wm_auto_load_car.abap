FUNCTION z_wm_auto_load_car.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"  EXPORTING
*"     REFERENCE(ET_RETURN_MSG) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  TABLES: ltak, ltap, zwm013, zwm028, zwm002, zwm040, zwm020.

  DATA: lt_messtab TYPE tab_bdcmsgcoll,
        lt_bdc_tab TYPE TABLE OF bdcdata,
        lt_zwm028  TYPE TABLE OF zwm028,
        lt_zwm013  TYPE TABLE OF zwm013,
        ls_zwm002  TYPE zwm002.

  DATA: return_msg TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: l_ltap TYPE zwm01_t_l_ltap WITH HEADER LINE.
  DATA: lt_ltap_conf TYPE TABLE OF ltap_conf WITH HEADER LINE.

  DATA: ls_opt     TYPE ctu_params,
        ls_ltap    TYPE ltap,
        ls_zwm028  TYPE zwm028,
        ls_messtab TYPE bdcmsgcoll.

  DATA: lv_pul  TYPE c LENGTH 14,
        lv_su   TYPE lenum,
        lv_dck  TYPE c LENGTH 14,
        lv_item TYPE ltap-tapos.

  DATA: lv_queue_sai_aut TYPE lrf_queue.

  DATA: lv_2step      TYPE flag,
        lv_tabix      TYPE sytabix,
        porta         LIKE ls_zwm002-porta,
        st_ppk        LIKE zwm028-st_ppk,
        conf_t(1),
        pal_remontada LIKE lein-lenum.

** Retorna Dados de Grupo
**********************************************************************
  SELECT *  FROM zwm028
            INTO TABLE lt_zwm028
            WHERE lgnum = i_lgnum AND
                  refnr = i_refnr.

  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.

** Satus de Bloqueio
**********************************************************************
  LOOP AT lt_zwm028 TRANSPORTING NO FIELDS WHERE zlock <> '5'.
    RAISE error.
  ENDLOOP.

** Retorna Paletes no Pulmão
**********************************************************************
  READ TABLE lt_zwm028
        INTO ls_zwm028
        INDEX 1.

** Retorna OT DCK
**********************************************************************
  SELECT SINGLE * FROM ltap
                  INTO ls_ltap
                  WHERE lgnum = i_lgnum AND
                        tanum = ls_zwm028-ot.

  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.

** apanhar todas as to´s de uma determinada carga para carregar o carro.

  CONCATENATE '0' ls_ltap-nlpla+8(2) INTO porta.

  CLEAR ls_zwm002.
  SELECT SINGLE * INTO ls_zwm002
      FROM zwm002
          WHERE armazem = i_lgnum AND
          porta = porta.

  CLEAR zwm028.
  SELECT SINGLE refnr st_ppk total_paletes INTO (zwm028-refnr, st_ppk, ls_zwm028-total_paletes)
      FROM zwm028
          WHERE lgnum = i_lgnum AND
                remessa = ' ' AND
                pulmao1 = zwm002-pulmao_1 AND
                ot = ls_ltap-tanum.

  CLEAR l_ltap.
  REFRESH l_ltap.

** Get To´s Pal Especial

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltap
           BYPASSING BUFFER
               FROM ltak AS k INNER JOIN ltap AS p
               ON  k~lgnum = p~lgnum AND
                   k~tanum = p~tanum
                   WHERE k~lgnum = i_lgnum AND
                         k~kquit = ' ' AND
                         k~betyp = 'H' AND
                         k~benum = i_refnr AND
                         p~pquit = ' ' AND
                         p~pvqui = ' ' AND
                         vorga <> 'ST'.

** Get To´s Standard
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltap
           BYPASSING BUFFER
               FROM ltak AS k INNER JOIN ltap AS p
               ON  k~lgnum = p~lgnum AND
                   k~tanum = p~tanum
                   WHERE k~lgnum = i_lgnum AND
                         k~kquit = ' ' AND
                         k~refnr = i_refnr AND
                         p~pquit = ' ' AND
                         p~pvqui = 'X' AND
                         vorga <> 'ST'.

  IF NOT l_ltap[] IS INITIAL.

    LOOP AT l_ltap.
      lv_tabix = sy-tabix.

      CLEAR ltak.
      SELECT SINGLE *
          FROM ltak
              WHERE lgnum = i_lgnum AND
                    tanum = l_ltap-tanum.
** Processo Standard

      CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
        EXPORTING
          i_lgnum = i_lgnum
          i_refnr = i_refnr
        IMPORTING
          e_2step = lv_2step
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF lv_2step EQ abap_true.

        CLEAR zwm028.
        SELECT SINGLE ordem kober INTO (l_ltap-pos_remessa,
                                       l_ltap-kober)
               FROM zwm028
                   WHERE lgnum = i_lgnum AND
                         refnr = i_refnr.

        IF l_ltap-vltyp = 'PCK' OR l_ltap-vltyp = 'PKB' OR l_ltap-vltyp = 'PKL'.
          SELECT SINGLE sscc FROM zwm026 INTO l_ltap-vlenr
                              WHERE armazem = i_lgnum AND
                                    grupo = i_refnr AND
                                    to_number = l_ltap-tanum.
        ENDIF.

        IF l_ltap-nltyp <> 'PPK'.
          CLEAR zwm013.
          SELECT SINGLE posicao_pulmao destino
              FROM zwm013 INTO (l_ltap-pos_pulmao, l_ltap-pulmao)
                  WHERE armazem = l_ltap-lgnum AND
                        sscc = l_ltap-vlenr.

        ELSE.

          CLEAR ltak.
          SELECT SINGLE *
              FROM ltak
                  WHERE lgnum = i_lgnum AND
                        lznum = l_ltap-vlenr AND
                        kquit = ' '.

          CLEAR ltap.
          SELECT SINGLE nltyp nlpla INTO (ltap-nltyp, ltap-nlpla)
              FROM ltap
                  WHERE lgnum = ltak-lgnum AND
                        tanum = ltak-tanum.


          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = ltap-nltyp
              lgpla = ltap-nlpla
            IMPORTING
              bin   = l_ltap-pulmao.

          CLEAR l_ltap-pos_pulmao.
        ENDIF.

        MODIFY l_ltap INDEX lv_tabix.


      ELSEIF ltak-betyp = 'L'.
        CLEAR zwm028.
        SELECT SINGLE ordem kober INTO (l_ltap-pos_remessa,
                                       l_ltap-kober)
               FROM zwm028
                   WHERE lgnum = i_lgnum AND
                         refnr = i_refnr AND
                         remessa = l_ltap-vbeln.

        IF sy-subrc <> 0.
          CLEAR zwm040.
          SELECT SINGLE *
              FROM zwm040
                  WHERE lgnum = i_lgnum AND
                        refnr = i_refnr AND
                        remessa = l_ltap-vbeln.
          IF sy-subrc = 0.
            SELECT SINGLE ordem kober INTO (l_ltap-pos_remessa, l_ltap-kober)
                FROM zwm028
                    WHERE lgnum = i_lgnum AND
                          refnr = i_refnr AND
                          remessa = zwm040-id_servisan.
          ENDIF.
        ENDIF.

        IF l_ltap-vltyp = 'PCK' OR l_ltap-vltyp = 'PKB' OR l_ltap-vltyp = 'PKL'.

          SELECT SINGLE sscc FROM zwm026 INTO l_ltap-vlenr
                              WHERE armazem = i_lgnum AND
                                    grupo = i_refnr AND
                                    remessa = l_ltap-vbeln AND
                                    to_number = l_ltap-tanum.
        ENDIF.

        IF l_ltap-nltyp <> 'PPK'.
          CLEAR zwm013.
          SELECT SINGLE posicao_pulmao destino
              FROM zwm013 INTO (l_ltap-pos_pulmao, l_ltap-pulmao)
                  WHERE armazem = l_ltap-lgnum AND
                        sscc = l_ltap-vlenr.

        ELSE.

          CLEAR ltak.
          SELECT SINGLE *
              FROM ltak
                  WHERE lgnum = i_lgnum AND
                        lznum = l_ltap-vlenr AND
                        kquit = ' '.

          CLEAR ltap.
          SELECT SINGLE nltyp nlpla INTO (ltap-nltyp, ltap-nlpla)
              FROM ltap
                  WHERE lgnum = ltak-lgnum AND
                        tanum = ltak-tanum.


          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = ltap-nltyp
              lgpla = ltap-nlpla
            IMPORTING
              bin   = l_ltap-pulmao.

          CLEAR l_ltap-pos_pulmao.
        ENDIF.
        MODIFY l_ltap INDEX lv_tabix.
      ELSEIF ltak-betyp = 'H'.
** Paletização Especial.

        CLEAR ltak.
        SELECT SINGLE *
            FROM ltak
                WHERE lgnum = i_lgnum AND
                      tanum = l_ltap-tanum AND
                      kquit = ' '.

        CLEAR zwm013.
        SELECT SINGLE posicao_pulmao destino
            FROM zwm013 INTO (l_ltap-pos_pulmao, l_ltap-pulmao)
                WHERE armazem = l_ltap-lgnum AND
                      sscc = ltak-lznum.

        SELECT SINGLE ordem INTO l_ltap-pos_remessa
               FROM zwm028
                   WHERE lgnum = i_lgnum AND
                         refnr = ltak-benum AND
                         remessa = l_ltap-vlpla.

        l_ltap-vlenr = ltak-lznum.
        l_ltap-vbeln = l_ltap-vlpla.
        l_ltap-refnr = ltak-benum.
        MODIFY l_ltap INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'ZWM_MPUL_CAMIAO_SEQ_CARGA_CHAN'
      EXPORTING
        i_lgnum   = l_ltap-lgnum
        i_refnr   = l_ltap-refnr
      CHANGING
        ct_l_ltap = l_ltap[].

  ENDIF.

  SELECT SINGLE valor
    FROM zwm001 INTO lv_queue_sai_aut
    WHERE armazem   = l_ltap-lgnum
    AND   processo  = 'GESTAO_FILAS'
    AND   parametro = 'FILA_SAIDA_AUT'.

  LOOP AT l_ltap.

    REFRESH return_msg.
    CLEAR return_msg.

    CLEAR ltak.
    SELECT SINGLE *
        FROM ltak
            WHERE lgnum = l_ltap-lgnum AND
                  tanum = l_ltap-tanum.

    IF ltak-betyp = 'H'.
      conf_t = 'B'.
    ELSE.
      conf_t = 'T'.
    ENDIF.

    DO.
      IF l_ltap-vltyp eq 'AUT'.

        REFRESH lt_ltap_conf.
        CLEAR lt_ltap_conf.
        lt_ltap_conf-tanum = l_ltap-tanum.
        lt_ltap_conf-tapos = l_ltap-tapos.
        lt_ltap_conf-altme = l_ltap-meins.
        lt_ltap_conf-nista = l_ltap-vsolm.
        APPEND lt_ltap_conf.

        CALL FUNCTION 'L_TO_CONFIRM'
          EXPORTING
            i_lgnum       = l_ltap-lgnum
            i_tanum       = l_ltap-tanum
            i_quknz       = '2'
            i_subst       = 'X'
          TABLES
            t_ltap_conf   = lt_ltap_conf
          EXCEPTIONS
            error_message = 99.

      ELSE.
        CALL FUNCTION 'ZWM_CONFIRM_TO'
          EXPORTING
            armazem              = l_ltap-lgnum
            confirm_type         = conf_t
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = l_ltap-tanum
            to_item              = l_ltap-tapos
          EXCEPTIONS
            confirm_type_error   = 1
            to_not_found         = 2
            to_already_confirmed = 3
            to_already_picked    = 4
            to_not_picked        = 5
            wrong_su             = 6
            missing_su           = 7
            error                = 8
            OTHERS               = 9.
      ENDIF.

      IF sy-subrc <> 0.
        WAIT UP TO 1 SECONDS.
      ELSE.
        CLEAR ltap.
        SELECT SINGLE * FROM ltap
              WHERE lgnum = l_ltap-lgnum AND
                    tanum = l_ltap-tanum AND
                    tapos = l_ltap-tapos AND
                    pquit = 'X'.
        IF sy-subrc = 0.
** Apagar Tabela ZWM013
          DELETE FROM zwm013
              WHERE armazem = i_lgnum AND
                       sscc = l_ltap-vlenr.
          IF sy-subrc = 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDIF.
    ENDDO.

*** Verificar se é remontada
*    CLEAR: pal_remontada, zwm020.
*    SELECT SINGLE *
*        FROM zwm020
*            WHERE armazem = l_ltap-lgnum AND
*                  ( p1 = l_ltap-vlenr OR
*                    p2 = l_ltap-vlenr ).
*    IF sy-subrc = 0.
*      SORT l_ltap BY vlenr.
*      IF l_ltap-vlenr = zwm020-p1.
*        READ TABLE l_ltap WITH KEY vlenr = zwm020-p2.
*      ELSEIF l_ltap-vlenr = zwm020-p2.
*        READ TABLE l_ltap WITH KEY vlenr = zwm020-p1.
*      ENDIF.
*      IF sy-subrc = 0.
*
*        pal_remontada = l_ltap-vlenr.
*
*        DO.
*          CALL FUNCTION 'ZWM_CONFIRM_TO'
*            EXPORTING
*              armazem              = l_ltap-lgnum
*              confirm_type         = 'T'
*            TABLES
*              return_msg           = return_msg
*            CHANGING
*              to                   = l_ltap-tanum
*              to_item              = l_ltap-tapos
*            EXCEPTIONS
*              confirm_type_error   = 1
*              to_not_found         = 2
*              to_already_confirmed = 3
*              to_already_picked    = 4
*              to_not_picked        = 5
*              wrong_su             = 6
*              missing_su           = 7
*              error                = 8
*              OTHERS               = 9.
*
*          IF sy-subrc <> 0.
*            WAIT UP TO 1 SECONDS.
*          ELSE.
*            CLEAR ltap.
*            SELECT SINGLE * FROM ltap
*                  WHERE lgnum = l_ltap-lgnum AND
*                        tanum = l_ltap-tanum AND
*                        tapos = l_ltap-tapos AND
*                        pquit = 'X'.
*            IF sy-subrc = 0.
*              IF NOT pal_remontada IS INITIAL.
*** Apagar Tabela ZWM013
*                DELETE FROM zwm013
*                    WHERE armazem = i_lgnum AND
*                          sscc = pal_remontada.
*                IF sy-subrc = 0.
*                  COMMIT WORK.
*                ELSE.
*                  ROLLBACK WORK.
*                ENDIF.
*
*                DELETE l_ltap WHERE vlenr = pal_remontada.
*              ENDIF.
*              EXIT.
*            ELSE.
*              WAIT UP TO 1 SECONDS.
*            ENDIF.
*  ENDIF.
*ENDDO.

*  ENDIF.
*  ENDIF.

  ENDLOOP.

** Actualizar numero de paletes no carro
*  IF ltak-betyp = ' '.
  UPDATE zwm028 SET paletes_carro = ls_zwm028-total_paletes
  WHERE lgnum = i_lgnum AND
        refnr = i_refnr AND
        remessa = ' '.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  CALL FUNCTION 'ZWM_AUTO_TO_DELEVERY_ABAST'
    EXPORTING
      i_lgnum = i_lgnum
      i_refnr = i_refnr.

*  ELSE.
*
*    CLEAR ltap.
*    SELECT SINGLE * FROM ltap
*            WHERE lgnum = i_lgnum
*              AND vlenr = l_ltap-vlenr
*              AND nltyp = '916'.
*
*    IF ltap-vltyp = 'PRM' OR ltap-vltyp = 'CRD'.
*      UPDATE zwm028 SET paletes_carro = ls_zwm028-total_paletes
*      WHERE lgnum = i_lgnum AND
*            refnr = l_ltap-refnr AND
*            remessa = ' '.
*
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*
*      CALL FUNCTION 'ZWM_AUTO_TO_DELEVERY_ABAST'
*        EXPORTING
*          i_lgnum = i_lgnum
*          i_refnr = l_ltap-refnr.
*
*    ENDIF.
*  ENDIF.
** Confirmar OT de chamada
  DO 10 TIMES.

    CALL FUNCTION 'ZWM_CONFIRM_TO'
      EXPORTING
        armazem              = i_lgnum
        confirm_type         = 'P'
      TABLES
        return_msg           = return_msg
      CHANGING
        to                   = ls_ltap-tanum
        to_item              = ls_ltap-tapos
      EXCEPTIONS
        confirm_type_error   = 1
        to_not_found         = 2
        to_already_confirmed = 3
        to_already_picked    = 4
        to_not_picked        = 5
        wrong_su             = 6
        missing_su           = 7
        error                = 8
        OTHERS               = 9.

    IF sy-subrc <> 0.
      WAIT UP TO 1 SECONDS.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  DO 10 TIMES.

    CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
      EXPORTING
        armazem              = i_lgnum
        confirm_type         = 'T'
      TABLES
        return_msg           = return_msg
      CHANGING
        to                   = ls_ltap-tanum
      EXCEPTIONS
        confirm_type_error   = 1
        to_not_found         = 2
        to_already_confirmed = 3
        to_already_picked    = 4
        to_not_picked        = 5
        wrong_su             = 6
        missing_su           = 7
        error                = 8
        OTHERS               = 9.

    IF sy-subrc <> 0.
      WAIT UP TO 1 SECONDS.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  CALL FUNCTION 'ZWM_TO_DELEVERY_ABAST'
    EXPORTING
      i_lgnum  = i_lgnum
      i_refnr  = i_refnr
      i_commit = abap_true
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.


ENDFUNCTION.
