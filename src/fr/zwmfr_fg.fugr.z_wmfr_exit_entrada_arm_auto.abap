FUNCTION z_wmfr_exit_entrada_arm_auto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_LTAK) TYPE  LTAK
*"     REFERENCE(IS_LTAP) TYPE  LTAP
*"     REFERENCE(IS_MLVS) TYPE  MLVS
*"     REFERENCE(IS_MGEF) TYPE  MGEF
*"     REFERENCE(IS_T331) TYPE  T331
*"     REFERENCE(IS_T333) TYPE  T333
*"     REFERENCE(IS_T340D) TYPE  T340D
*"     REFERENCE(I_VORGA) TYPE  LTAP_VORGA
*"  EXPORTING
*"     REFERENCE(E_NLPLA) TYPE  LTAP_NLPLA
*"     REFERENCE(E_NPPOS) TYPE  LTAP_NPPOS
*"     REFERENCE(E_NKDYN) TYPE  LVS_KZDYN
*"     REFERENCE(E_NLENR) TYPE  LTAP_NLENR
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"      NOT_EXIT
*"----------------------------------------------------------------------
  DATA: lv_lgtyp_aut TYPE lgtyp VALUE 'AUT'.


  TYPES: BEGIN OF lty_sort_lgpla,
            lgpla TYPE lgpla,
            tabix TYPE sytabix,
         END OF lty_sort_lgpla.

  DATA: lr_lgpla TYPE RANGE OF lgpla,
        lr_lptyp TYPE RANGE OF lvs_lptyp.

  DATA: lt_lqua       TYPE TABLE OF lqua,
        lt_lqua_stk   TYPE TABLE OF lqua,
        lt_lagp_c     TYPE TABLE OF lagp,
        lt_lagp       TYPE TABLE OF lagp,
        lt_lagp_bk    TYPE TABLE OF lagp,
        lt_sort_lgpla TYPE TABLE OF lty_sort_lgpla.

  DATA: ls_mlgn       TYPE mlgn,
        ls_lqua       TYPE lqua,
        ls_lagp_c     TYPE lagp,
        ls_lqua_last  TYPE lqua,
        ls_t334b      TYPE t334b,
        ls_lagp       TYPE lagp,
        ls_t334p      TYPE t334p,
        ls_zwm020     TYPE zwm020,
        ls_r_lgpla    LIKE LINE OF lr_lgpla,
        ls_r_lptyp    LIKE LINE OF lr_lptyp,
        ls_sort_lgpla TYPE lty_sort_lgpla.

  DATA: lv_last_tabix   TYPE sytabix,
        lv_tabix        TYPE sytabix,
        lv_index        TYPE syindex,
        lv_num          TYPE n LENGTH 2,
        lv_name         TYPE fieldname,
        lv_lgpla        TYPE lgpla,
        lv_stell        TYPE t343_stell,
        lv_valor        TYPE zwm_valor,
        lv_bwlvs_realoc TYPE bwlvs,
        lv_times        TYPE sytabix,
        lv_index2       TYPE syindex.

  FIELD-SYMBOLS: <lv_lgber> TYPE lgber,
                 <lv_lptyp> TYPE lvs_lptyp,
                 <lv_stell> TYPE t343_stell.

  IF is_ltak-lgnum <> '150'.
    RAISE not_exit.
  ENDIF.


  IF is_ltap-nltyp <> lv_lgtyp_aut.
    RAISE error.
  ENDIF.

** Parametros
***********************************************************************
  SELECT SINGLE valor FROM zwm001
                      INTO lv_valor
                      WHERE armazem   = is_ltak-lgnum AND
                            processo  = 'TRANSFERENCIA_AUT' AND
                            parametro = 'MOV_WM'.

  lv_bwlvs_realoc = lv_valor.

  PERFORM exit_armaut_lock USING is_ltap-lgnum is_ltap-nltyp.

** Posições
***********************************************************************
  SELECT * FROM lagp
           INTO TABLE lt_lagp
           WHERE lgnum = is_ltap-lgnum AND
                 lgtyp = is_ltap-nltyp.

  SORT lt_lagp BY sorlp lgpla.

  DELETE lt_lagp WHERE kzler IS INITIAL OR
                       NOT skzue IS INITIAL OR
                       NOT skzsi IS INITIAL.

  IF lt_lagp IS INITIAL.
    PERFORM exit_armaut_unlock.
    RAISE error.
  ENDIF.

** Evita saltar de corredor na realocações
***********************************************************************
  lv_times = 1.

  IF is_ltap-vltyp EQ lv_lgtyp_aut.
    IF is_ltak-bwlvs <> lv_bwlvs_realoc.
      PERFORM exit_armaut_unlock.
      RAISE error.
    ENDIF.

    lv_times = 2.

    SORT lt_lagp BY lgpla sorlp.

    LOOP AT lt_lagp INTO ls_lagp.
      lv_tabix = sy-tabix.
      IF is_ltap-vlpla(2) > ls_lagp-lgpla(2).
        DELETE lt_lagp INDEX lv_tabix.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.


  IF lt_lagp IS INITIAL.
    PERFORM exit_armaut_unlock.
    RAISE error.
  ENDIF.

** Valida se Palete Completa
***********************************************************************
  SELECT SINGLE * FROM mlgn
                  INTO ls_mlgn
                  WHERE lgnum = is_ltap-lgnum AND
                        matnr = is_ltap-matnr.
  IF sy-subrc <> 0.
    PERFORM exit_armaut_unlock.
    RAISE error.
  ENDIF.

  IF ls_mlgn-lhmg1 <> is_ltap-vsolm.
    PERFORM exit_armaut_unlock.
    RAISE error.
  ENDIF.

** Tipos de Posição
***********************************************************************
  SELECT SINGLE * FROM t334p
                  INTO ls_t334p
                  WHERE lgnum = is_ltak-lgnum AND
                        letyp = ls_mlgn-lety1.

  CLEAR: lr_lptyp.
  DO.
    lv_index = sy-index.
    lv_index = lv_index + 3.

    UNASSIGN <lv_lptyp>.
    ASSIGN COMPONENT lv_index OF STRUCTURE ls_t334p TO <lv_lptyp>.
    IF NOT <lv_lptyp> IS ASSIGNED OR
       <lv_lptyp> IS INITIAL.
      EXIT.
    ENDIF.

    CLEAR: ls_r_lptyp.
    ls_r_lptyp-sign   = 'I'.
    ls_r_lptyp-option = 'EQ'.
    ls_r_lptyp-low    = <lv_lptyp>.
    APPEND ls_r_lptyp TO lr_lptyp.
  ENDDO.

  IF lr_lptyp IS INITIAL.
    PERFORM exit_armaut_unlock.
    RAISE error.
  ENDIF.

  DELETE lt_lagp WHERE NOT lptyp IN lr_lptyp.


** Adiciona Remontada
***********************************************************************
  DO 1 TIMES.
    SELECT SINGLE * FROM zwm020
                    INTO ls_zwm020
                    WHERE armazem = is_ltap-lgnum AND
                          (
                            ( p1 = is_ltap-vlenr OR p2 = is_ltap-vlenr ) OR
                            ( p1 = is_ltap-nlenr OR p2 = is_ltap-nlenr )
                          ).
    CHECK sy-subrc EQ 0.
    CHECK NOT ls_zwm020-bin IS INITIAL.

    IF is_ltak-bwlvs EQ lv_bwlvs_realoc AND
       ls_zwm020-bin EQ is_ltap-vlpla.
      EXIT.
    ENDIF.

    e_nlpla = ls_zwm020-bin.
    PERFORM exit_armaut_unlock.
    RETURN.
  ENDDO.

** Retorna Stock
***********************************************************************
  SELECT * FROM lqua
           INTO TABLE lt_lqua_stk
           WHERE lgnum = is_ltap-lgnum AND
                 lgtyp = is_ltap-nltyp.
  SORT lt_lqua_stk BY lgpla.

  "Remove Posições Com Stock
  LOOP AT lt_lqua_stk INTO ls_lqua WHERE verme <> 0.
    DELETE lt_lagp WHERE lgpla = ls_lqua-lgpla.
  ENDLOOP.

  "Remove propria posição
  DELETE lt_lagp WHERE lgtyp = is_ltap-vltyp AND
                       lgpla = is_ltap-vlpla.


  "Remove Alvelos com Stock a Sair em Pelo menos uma das Posições
  LOOP AT lt_lqua_stk INTO ls_lqua WHERE ausme <> 0.
    CLEAR: lr_lgpla, ls_r_lgpla.
    CONCATENATE ls_lqua-lgpla(8) '*' INTO ls_r_lgpla-low.
    ls_r_lgpla-sign   = 'I'.
    ls_r_lgpla-option = 'CP'.
    APPEND ls_r_lgpla TO lr_lgpla.

    DELETE lt_lagp WHERE lgpla IN  lr_lgpla.
  ENDLOOP.

  "Remove Posições com Stock Bloquado Por uma Posição à Frente
  LOOP AT lt_lqua_stk INTO ls_lqua.
    CLEAR: lr_lgpla, ls_r_lgpla.
    CONCATENATE ls_lqua-lgpla(8) '*' INTO ls_r_lgpla-low.
    ls_r_lgpla-sign   = 'I'.
    ls_r_lgpla-option = 'CP'.
    APPEND ls_r_lgpla TO lr_lgpla.

    LOOP AT lt_lagp INTO ls_lagp WHERE lgpla IN lr_lgpla.
      CHECK ls_lqua-lgpla+8(1) > ls_lagp-lgpla+8(1).
      DELETE lt_lagp INDEX sy-tabix.
    ENDLOOP.
  ENDLOOP.

** Remove Alvelo Bloqueado Por Palete com OT Criada nas Realocações
***********************************************************************
  IF is_ltak-bwlvs EQ lv_bwlvs_realoc.
    LOOP AT lt_lqua_stk INTO ls_lqua WHERE einme <> 0 OR
                                           ausme <> 0.
      CLEAR: lr_lgpla, ls_r_lgpla.
      CONCATENATE ls_lqua-lgpla(8) '*' INTO ls_r_lgpla-low.
      ls_r_lgpla-sign   = 'I'.
      ls_r_lgpla-option = 'CP'.
      APPEND ls_r_lgpla TO lr_lgpla.

      DELETE lt_lagp WHERE lgtyp = ls_lqua-lgtyp AND
                           lgpla IN lr_lgpla.
    ENDLOOP.
  ELSE.
    DELETE lt_lagp WHERE lgtyp <> lv_lgtyp_aut.
  ENDIF.

**
***********************************************************************
  lt_lagp_bk = lt_lagp.


** Valida se tem mais paletes para o mesmo material no TpDep AUT
***********************************************************************
  DO 1 TIMES.
    lt_lagp = lt_lagp_bk.
    lt_lqua = lt_lqua_stk.
    DELETE lt_lqua WHERE matnr <> is_ltap-matnr.

    IF ls_mlgn-lety1 = 'P2' OR ls_mlgn-lety1 = 'P5'.
      SORT lt_lqua BY lgpla.
      DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING lgpla.
    ENDIF.

    "Remove Posições com AABBBCCD igual (AABBBCCDE)
    LOOP AT lt_lqua INTO ls_lqua.
      lv_tabix = sy-tabix.

      IF lv_tabix EQ 1.
        ls_lqua_last = ls_lqua.
        CONTINUE.
      ENDIF.

      IF ls_lqua-lgpla(8) EQ ls_lqua_last-lgpla(8).
        lv_last_tabix = lv_tabix - 1.
        DELETE lt_lqua INDEX lv_tabix.
        DELETE lt_lqua INDEX lv_last_tabix.
      ENDIF.

      ls_lqua_last = ls_lqua.
    ENDLOOP.

    "Prioridade a Posição do Mesmo Material Lote
    LOOP AT lt_lqua INTO ls_lqua WHERE charg = is_ltap-charg.
      CLEAR: lr_lgpla, ls_r_lgpla.
      CONCATENATE ls_lqua-lgpla(8) '*' INTO ls_r_lgpla-low.
      ls_r_lgpla-sign   = 'I'.
      ls_r_lgpla-option = 'CP'.
      APPEND ls_r_lgpla TO lr_lgpla.

      LOOP AT lt_lagp INTO ls_lagp WHERE lgpla IN lr_lgpla.
        e_nlpla = ls_lagp-lgpla.
        EXIT.
      ENDLOOP.

      IF NOT e_nlpla IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.


    IF NOT e_nlpla IS INITIAL.
      EXIT.
    ENDIF.


    "Prioridade a Posição do Mesmo Material
    LOOP AT lt_lqua INTO ls_lqua.
      CLEAR: lr_lgpla, ls_r_lgpla.
      CONCATENATE ls_lqua-lgpla(8) '*' INTO ls_r_lgpla-low.
      ls_r_lgpla-sign   = 'I'.
      ls_r_lgpla-option = 'CP'.
      APPEND ls_r_lgpla TO lr_lgpla.

      LOOP AT lt_lagp INTO ls_lagp WHERE lgpla IN lr_lgpla.
        e_nlpla = ls_lagp-lgpla.
        EXIT.
      ENDLOOP.

      IF NOT e_nlpla IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDDO.

** Àrea de Armazenamento
***********************************************************************
  SELECT SINGLE * FROM t334b
                  INTO ls_t334b
                  WHERE lgnum = is_ltap-lgnum AND
                        lgtyp = is_ltap-nltyp AND
                        lgbkz = ls_mlgn-lgbkz.
  CHECK sy-subrc EQ 0.

** Posições Vazias
***********************************************************************
  DO lv_times TIMES.
    lv_index2 = sy-index.
    CHECK e_nlpla IS INITIAL.

    CLEAR: lr_lgpla, ls_r_lgpla.
    CONCATENATE is_ltap-vlpla(2) '*' INTO ls_r_lgpla-low.
    ls_r_lgpla-sign   = 'I'.
    ls_r_lgpla-option = 'CP'.
    APPEND ls_r_lgpla TO lr_lgpla.




    DO 2 TIMES.
      CHECK e_nlpla IS INITIAL.

      lt_lagp = lt_lagp_bk.

      lv_index = sy-index.

      lt_lqua = lt_lqua_stk.
      SORT lt_lqua BY lgpla.

      IF lv_index EQ 1.
        "Remove Alvelos com Stock Em pelo menos uma das posições
        CLEAR: lr_lgpla, ls_r_lgpla.
        LOOP AT lt_lqua INTO ls_lqua.
          CONCATENATE ls_lqua-lgpla(8) '*' INTO ls_r_lgpla-low.
          ls_r_lgpla-sign   = 'I'.
          ls_r_lgpla-option = 'CP'.
          APPEND ls_r_lgpla TO lr_lgpla.
        ENDLOOP.

        DELETE lt_lagp WHERE lgpla IN lr_lgpla.
      ELSE.
        "Remove Posição com Stock
        LOOP AT lt_lqua INTO ls_lqua.
          DELETE lt_lagp WHERE lgpla EQ ls_lqua-lgpla.
        ENDLOOP.
      ENDIF.

      CHECK NOT lt_lagp IS INITIAL.

      DO.
        CLEAR: lt_sort_lgpla.
        lt_lagp_c = lt_lagp.

        CHECK sy-index > 6.

        UNASSIGN <lv_lgber>.
        ASSIGN COMPONENT sy-index OF STRUCTURE ls_t334b TO <lv_lgber>.
        IF NOT <lv_lgber> IS ASSIGNED.
          EXIT.
        ENDIF.


        DELETE lt_lagp_c WHERE lgber <> <lv_lgber>.
        CHECK NOT lt_lagp_c IS INITIAL.


        SORT lt_lagp_c BY sorlp lgpla ASCENDING.
        READ TABLE lt_lagp_c
              INTO ls_lagp_c
              INDEX 1.

        e_nlpla = ls_lagp_c-lgpla.
        EXIT.
      ENDDO.
    ENDDO.
  ENDDO.

** Update de Remontada
***********************************************************************
  DO 1 TIMES.
    CHECK NOT e_nlpla IS INITIAL.
    CHECK NOT ls_zwm020 IS INITIAL.

    IF is_ltak-bwlvs <> lv_bwlvs_realoc.
      CHECK ls_zwm020-bin IS INITIAL.
    ENDIF.

    UPDATE zwm020 SET bin = e_nlpla WHERE armazem = ls_zwm020-armazem AND
                                          p1      = ls_zwm020-p1 AND
                                          p2      = ls_zwm020-p2.
  ENDDO.

  PERFORM exit_armaut_unlock.
ENDFUNCTION.
