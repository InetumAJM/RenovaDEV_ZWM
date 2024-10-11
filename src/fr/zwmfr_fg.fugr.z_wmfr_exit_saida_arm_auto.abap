FUNCTION z_wmfr_exit_saida_arm_auto.
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
*"     REFERENCE(I_ANFML) TYPE  RL03TANFML
*"     REFERENCE(I_ANFME) TYPE  RL03TANFME
*"     REFERENCE(I_VORGA) TYPE  LTAP_VORGA
*"  CHANGING
*"     REFERENCE(CT_TQMAT) TYPE  PDT_T_LQUA_VB
*"  EXCEPTIONS
*"      ERROR
*"      NOT_EXIT
*"----------------------------------------------------------------------
  TYPES: BEGIN OF lty_tqmat_mod.
           INCLUDE STRUCTURE lqua_vb.
           TYPES:    lgpla_in TYPE c LENGTH 8,
           lgpla_cr TYPE c LENGTH 1,
           sorlp    TYPE lagp_sorlp,
         END OF lty_tqmat_mod.

  DATA: lt_tqmat_mod TYPE TABLE OF lty_tqmat_mod,
        lt_ltak      TYPE HASHED TABLE OF ltak WITH UNIQUE KEY tanum,
        lt_ltap      TYPE SORTED TABLE OF ltap WITH NON-UNIQUE KEY vlpla,
        lt_lagp      TYPE SORTED TABLE OF lagp WITH UNIQUE KEY lgtyp lgpla.

  DATA: ls_lagp      TYPE lagp,
        ls_ltak      TYPE ltak,
        ls_ltap      TYPE ltap,
        ls_tqmat     TYPE lqua_vb,
        ls_tqmat_mod TYPE lty_tqmat_mod.

  DATA: lr_lgpla   TYPE RANGE OF lgpla,
        ls_r_lgpla LIKE LINE OF lr_lgpla.

  DATA: lv_lgtyp_aut TYPE lgtyp VALUE 'AUT',
        lv_tabix     TYPE sytabix,
        lv_ok        TYPE flag,
        lv_vbeln     TYPE vbeln.

  IF is_ltak-lgnum <> '150'.
    RAISE not_exit.
  ENDIF.

  IF is_ltap-vltyp <> lv_lgtyp_aut.
    RAISE not_exit.
  ENDIF.

  DELETE ct_tqmat WHERE lqnum IS INITIAL.
  IF ct_tqmat IS INITIAL.
    RAISE error.
  ENDIF.

** Remessa
***********************************************************************
  lv_vbeln = is_ltak-vbeln.
  IF lv_vbeln IS INITIAL AND
     is_ltak-betyp EQ 'L' AND
     NOT is_ltak-benum IS INITIAL.
    lv_vbeln = is_ltak-benum.
  ENDIF.

** Lock
***********************************************************************
  PERFORM exit_armaut_lock USING is_ltap-lgnum is_ltap-vltyp.

** Retorna Posições
***********************************************************************
  SELECT * FROM lagp
           INTO TABLE lt_lagp
           FOR ALL ENTRIES IN ct_tqmat
           WHERE lgnum = ct_tqmat-lgnum AND
                 lgtyp = ct_tqmat-lgtyp AND
                 lgpla = ct_tqmat-lgpla.

** Retorna OT's para AUT
***********************************************************************
  DO 1 TIMES.
    SELECT * FROM ltap
             INTO TABLE lt_ltap
             WHERE lgnum = is_ltak-lgnum AND
                   pquit = abap_false AND
                   vltyp = lv_lgtyp_aut.
    CHECK sy-subrc EQ 0.

    DELETE lt_ltap WHERE pvqui = abap_true.
    CHECK NOT lt_ltap IS INITIAL.

    SELECT * FROM ltak
             INTO TABLE lt_ltak
             FOR ALL ENTRIES IN lt_ltap
             WHERE lgnum = lt_ltap-lgnum AND
                   tanum = lt_ltap-tanum.
  ENDDO.

  DATA: lt_lqua LIKE lqua OCCURS 0 WITH HEADER LINE.

  SELECT * FROM lqua
           INTO TABLE lt_lqua
           FOR ALL ENTRIES IN ct_tqmat
           WHERE lgnum = ct_tqmat-lgnum AND
                 lgtyp = ct_tqmat-lgtyp AND
                 lgpla = ct_tqmat-lgpla.



** Ordenação
***********************************************************************
  LOOP AT ct_tqmat INTO ls_tqmat.
    READ TABLE lt_lagp
          INTO ls_lagp
          WITH TABLE KEY lgtyp = ls_tqmat-lgtyp
                         lgpla = ls_tqmat-lgpla.
    CHECK sy-subrc EQ 0.

    IF ls_lagp-skzua EQ abap_true OR
       ls_lagp-skzsi EQ abap_true.
      CONTINUE.
    ENDIF.


    READ TABLE lt_lqua
          WITH  KEY      lgtyp = ls_tqmat-lgtyp
                         lgpla = ls_tqmat-lgpla
                         skzua = 'X'.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.


    IF ls_tqmat-einme <> 0.
      "Elemina Posições do mesmo alvelo desde que uma tenha stock a entrar
      CLEAR: lr_lgpla, ls_r_lgpla.
      CONCATENATE ls_tqmat-lgpla(8) '*' INTO ls_r_lgpla-low.
      ls_r_lgpla-sign   = 'I'.
      ls_r_lgpla-option = 'CP'.
      APPEND ls_r_lgpla TO lr_lgpla.

      DELETE ct_tqmat WHERE lgpla IN lr_lgpla.
      DELETE lt_tqmat_mod WHERE lgpla IN lr_lgpla.
      CONTINUE.
    ENDIF.

    CLEAR: ls_tqmat_mod.
    ls_tqmat_mod = ls_tqmat.
    ls_tqmat_mod-lgpla_in = ls_tqmat-lgpla(8).
    ls_tqmat_mod-lgpla_cr = ls_tqmat-lgpla+8(1).
    ls_tqmat_mod-sorlp    = ls_lagp-sorlp.
    APPEND ls_tqmat_mod TO lt_tqmat_mod.
  ENDLOOP.

  SORT lt_tqmat_mod BY sorlp ASCENDING
*                       wdatu ASCENDING
                       lgpla_in ASCENDING
                       lgpla_cr DESCENDING
                       lenum ASCENDING.


  CLEAR: ct_tqmat.

** Processa TQMAT
***********************************************************************
  LOOP AT lt_tqmat_mod INTO ls_tqmat_mod.
    CLEAR: lv_ok.

    lv_tabix = sy-tabix.

    "Posições do mesmo alvelo
    CLEAR: lr_lgpla, ls_r_lgpla.
    CONCATENATE ls_tqmat_mod-lgpla(8) '*' INTO ls_r_lgpla-low.
    ls_r_lgpla-sign   = 'I'.
    ls_r_lgpla-option = 'CP'.
    APPEND ls_r_lgpla TO lr_lgpla.

    LOOP AT lt_ltap INTO ls_ltap WHERE vlpla IN lr_lgpla.
      CLEAR: ls_ltak.
      READ TABLE lt_ltak
            INTO ls_ltak
            WITH TABLE KEY tanum = ls_ltap-tanum.
      IF sy-subrc <> 0.
        CLEAR: lv_ok.
        EXIT.
      ENDIF.

      IF ls_ltak-vbeln IS INITIAL AND
         ls_ltak-betyp EQ 'L' AND
         NOT ls_ltak-benum IS INITIAL.
        ls_ltak-vbeln = ls_ltak-benum.
      ENDIF.

      IF NOT ls_ltak-vbeln IS INITIAL AND
         NOT lv_vbeln IS INITIAL AND
         ls_ltak-vbeln EQ lv_vbeln.
        lv_ok = abap_true.
        CONTINUE.
      ELSEIF ls_ltak-betyp EQ 'Z' AND
             NOT ls_ltak-benum IS INITIAL AND
             NOT is_ltak-benum IS INITIAL AND
             ls_ltak-benum EQ is_ltak-benum.
        lv_ok = abap_true.
        CONTINUE.
      ENDIF.

      CLEAR: lv_ok.
      EXIT.
    ENDLOOP.
    IF sy-subrc <> 0.
      "Sem OT's para o alvelo
      lv_ok = abap_true.
    ENDIF.

    IF lv_ok <> abap_true.
      DELETE lt_tqmat_mod WHERE lgpla IN lr_lgpla.
      CONTINUE.
    ENDIF.

    ls_tqmat = ls_tqmat_mod.
    APPEND ls_tqmat TO ct_tqmat.
  ENDLOOP.

  PERFORM exit_armaut_unlock.
ENDFUNCTION.
