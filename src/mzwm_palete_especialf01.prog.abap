************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: MZWM_PALETE_ESPECIALF01                                  *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Manutenção da Tabela de Paletização Especial (Forms 1)   *
* Criado por: Luís Rocha                                               *
* Criado em.: 06/12/2004                                               *
* Tipo PRG..: Include                                                  *
************************************************************************
*---------------------------------------------------------------------*
*       FORM LER_TABELAS_0100                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ler_tabelas_0100.
  SELECT * FROM zwm031 INTO TABLE tab_zwm031
                 WHERE lgnum = zwm_scr-lgnum.
  SORT: tab_zwm031 BY kunnr matnr.
  LOOP AT tab_zwm031.
    CLEAR w_tab_0100.
    MOVE-CORRESPONDING tab_zwm031 TO w_tab_0100.
    PERFORM ler_makt USING tab_zwm031-matnr
                  CHANGING w_tab_0100-maktx.
    PERFORM ler_kna1 USING tab_zwm031-kunnr
                 CHANGING w_tab_0100-name1.
    APPEND w_tab_0100.
  ENDLOOP.
  DESCRIBE TABLE w_tab_0100 LINES scroll_0100-maxlines.
  REFRESH: tab_zwm031.
ENDFORM.                    "ler_tabelas_0100

*---------------------------------------------------------------------*
*       FORM LER_TABELAS_0200                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ler_tabelas_0200.

  SELECT * FROM zwm031 INTO TABLE tab_zwm031
                 WHERE lgnum = zwm_scr-lgnum
                   AND kunnr = zwm_scr-kunnr.
  IF sy-subrc NE 0.
    PERFORM check_kunnr.
  ENDIF.

  SORT: tab_zwm031 BY kunnr matnr.

  LOOP AT tab_zwm031.
    CLEAR w_tab_0200.
    MOVE-CORRESPONDING tab_zwm031 TO w_tab_0200.
    w_tab_0200-kunnr = zwm_scr-kunnr.
    PERFORM ler_makt USING tab_zwm031-matnr
                  CHANGING w_tab_0200-maktx.

    w_tab_0200-name1 = kna1-name1.

*    PERFORM ler_kna1 USING tab_zwm031-kunnr
*                 CHANGING w_tab_0200-name1.
    APPEND w_tab_0200.
  ENDLOOP.

  DESCRIBE TABLE w_tab_0200 LINES scroll_0200-maxlines.
  FREE: tab_zwm031.
  CLEAR: tab_zwm031.


ENDFORM.                    "ler_tabelas_0200

*---------------------------------------------------------------------*
*       FORM modifica_db_tab                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_SUBRC                                                       *
*---------------------------------------------------------------------*
FORM modifica_db_tab USING f_subrc.
  CASE sy-dynnr.
    WHEN '0100'.
      REFRESH tab_zwm031.
      LOOP AT w_tab_0100.
        IF NOT ( w_tab_0100-flg_del IS INITIAL ).
          PERFORM elimina_db_regs_0100.
        ELSE.
          IF NOT ( w_tab_0100-flg_ins IS INITIAL ).
            PERFORM insere_db_regs_0100.
          ELSE.
            IF NOT ( w_tab_0100-flg_mod IS INITIAL ).
              PERFORM modifica_db_regs_0100.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN '0200'.
      FREE tab_zwm031.
      LOOP AT w_tab_0200.
        IF NOT ( w_tab_0200-flg_del IS INITIAL ).
          PERFORM elimina_db_regs_0200.
        ELSE.
          IF NOT ( w_tab_0200-flg_ins IS INITIAL ).
            PERFORM insere_db_regs_0200.
          ELSE.
            IF NOT ( w_tab_0200-flg_mod IS INITIAL ).
              PERFORM modifica_db_regs_0200.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    "modifica_db_tab

*---------------------------------------------------------------------*
*       FORM apaga_entradas                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_CHANGES                                                     *
*  -->  F_INITIAL                                                     *
*---------------------------------------------------------------------*
FORM apaga_entradas USING f_changes f_initial.

  DATA: flg_resposta.
  IF NOT ( f_changes IS INITIAL ).
    PERFORM confirm_step USING ' ' text-p15 text-p17
                         CHANGING flg_resposta.
    IF flg_resposta = 'J'.
      CASE sy-dynnr.
        WHEN '0100'.
*         Prepara dados e envia para orgatex
          REFRESH tab_zwm031.
          LOOP AT w_tab_0100.
            CHECK NOT ( w_tab_0100-flg_del IS INITIAL ).
            PERFORM elimina_db_regs_0100.
          ENDLOOP.
          CLEAR: f_changes, f_initial.
        WHEN '0200'.
*         Prepara dados e envia para orgatex
          REFRESH tab_zwm031.
          LOOP AT w_tab_0200.
            CHECK NOT ( w_tab_0200-flg_del IS INITIAL ).
            PERFORM elimina_db_regs_0200.
          ENDLOOP.
          CLEAR: f_initial.

          DELETE FROM zwm049 WHERE lgnum = zwm_scr-lgnum AND
                                   kunnr = zwm_scr-kunnr.

      ENDCASE.
    ENDIF.
  ENDIF.

ENDFORM.                    "apaga_entradas

*---------------------------------------------------------------------*
*       FORM APAGA_DB_REGS                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM apaga_db_regs.

ENDFORM.                    "apaga_db_regs

*---------------------------------------------------------------------*
*       FORM INSERE_DB_REGS_0100                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM insere_db_regs_0100.
  DATA: l_zwm031 LIKE zwm031.
  SELECT SINGLE * FROM zwm031
     WHERE lgnum = zwm_scr-lgnum
       AND kunnr = w_tab_0100-kunnr
       AND matnr = w_tab_0100-matnr.
  CHECK sy-subrc NE 0.
  MOVE-CORRESPONDING w_tab_0100 TO l_zwm031.
  MOVE zwm_scr-lgnum TO l_zwm031-lgnum.
  MOVE zwm_scr-kunnr TO l_zwm031-kunnr.
  INSERT zwm031 FROM l_zwm031.
ENDFORM.                    "insere_db_regs_0100

*---------------------------------------------------------------------*
*       FORM MODIFICA_DB_REGS_0100                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM modifica_db_regs_0100.
  DATA: l_zwm031 LIKE zwm031.
  SELECT SINGLE * FROM zwm031
     WHERE lgnum = zwm_scr-lgnum
       AND kunnr = w_tab_0100-kunnr
       AND matnr = w_tab_0100-matnr.
  CHECK sy-subrc = 0.
  l_zwm031 = zwm031.
  MOVE-CORRESPONDING w_tab_0100 TO l_zwm031.
  MOVE zwm_scr-lgnum TO l_zwm031-lgnum.
  MOVE zwm_scr-kunnr TO l_zwm031-kunnr.
  UPDATE zwm031 FROM l_zwm031.
ENDFORM.                    "modifica_db_regs_0100

*---------------------------------------------------------------------*
*       FORM ELIMINA_DB_REGS_0100                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM elimina_db_regs_0100.
  SELECT SINGLE * FROM zwm031
     WHERE lgnum = zwm_scr-lgnum
       AND kunnr = w_tab_0100-kunnr
       AND matnr = w_tab_0100-matnr.
  CHECK sy-subrc = 0.
  DELETE zwm031.

  COMMIT WORK.
ENDFORM.                    "elimina_db_regs_0100

*---------------------------------------------------------------------*
*       FORM INSERE_DB_REGS_0200                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM insere_db_regs_0200.

  DATA: l_zwm031 LIKE zwm031.

  CLEAR zwm031.

  WAIT UP TO 1 SECONDS.

  SELECT SINGLE * FROM zwm031
     WHERE lgnum = zwm_scr-lgnum
       AND kunnr = zwm_scr-kunnr
       AND matnr = w_tab_0200-matnr.

*  CHECK sy-subrc NE 0.
  IF sy-subrc EQ 0.
    MESSAGE i000(zwmmsg001) WITH zwm031-kunnr zwm031-matnr.
  ELSE.
    MOVE-CORRESPONDING w_tab_0200 TO l_zwm031.
    MOVE zwm_scr-lgnum TO l_zwm031-lgnum.
    MOVE zwm_scr-kunnr TO l_zwm031-kunnr.
    MODIFY zwm031 FROM l_zwm031.

    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    "insere_db_regs_0200

*---------------------------------------------------------------------*
*       FORM MODIFICA_DB_REGS_0200                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM modifica_db_regs_0200.

  DATA: l_zwm031 LIKE zwm031.

  CLEAR zwm031.

  WAIT UP TO 1 SECONDS.

  SELECT SINGLE * FROM zwm031
     WHERE lgnum = zwm_scr-lgnum
       AND kunnr = zwm_scr-kunnr
       AND matnr = w_tab_0200-matnr.

  CHECK sy-subrc = 0.

  l_zwm031 = zwm031.
  MOVE-CORRESPONDING w_tab_0200 TO l_zwm031.
  MOVE zwm_scr-lgnum TO l_zwm031-lgnum.
  MOVE zwm_scr-kunnr TO l_zwm031-kunnr.
  MODIFY zwm031 FROM l_zwm031.

  COMMIT WORK AND WAIT.

ENDFORM.                    "modifica_db_regs_0200

*---------------------------------------------------------------------*
*       FORM ELIMINA_DB_REGS_0200                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM elimina_db_regs_0200.

  CLEAR: zwm031.

  WAIT UP TO 1 SECONDS.

  SELECT SINGLE * FROM zwm031
     WHERE lgnum = zwm_scr-lgnum
       AND kunnr = zwm_scr-kunnr
       AND matnr = w_tab_0200-matnr.

  CHECK sy-subrc = 0.

  DELETE zwm031.

  COMMIT WORK AND WAIT.

ENDFORM.                    "elimina_db_regs_0200

*&--------------------------------------------------------------------*
*&      Form  enqueue_table
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_LGNUM    text
*---------------------------------------------------------------------*
FORM enqueue_table USING f_lgnum
                         f_kunnr.

  DATA: hlp_uname LIKE sy-uname.

  CALL FUNCTION 'ENQUEUE_EZ_WM_LGNUM'
    EXPORTING
      mandt          = sy-mandt
      lgnum          = f_lgnum
      kunnr          = f_kunnr
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  hlp_uname = sy-msgv1.

  CASE sy-subrc.
    WHEN 0.
      w_zwm031_lock = 'X'.
    WHEN 1.
      MESSAGE e154(zwmmsg001) WITH hlp_uname.
*     Os dados estão bloqueados por usuário &
    WHEN 2.
      MESSAGE e021(m3).
    WHEN OTHERS.
      MESSAGE e899(m3) WITH text-e01.
  ENDCASE.

ENDFORM.                    "enqueue_ezpp_materiais

*&--------------------------------------------------------------------*
*&      Form  dequeue_table
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_LGNUM    text
*---------------------------------------------------------------------*
FORM dequeue_table USING f_lgnum
                         f_kunnr.

  CALL FUNCTION 'DEQUEUE_EZ_WM_LGNUM'
    EXPORTING
      mandt  = sy-mandt
      lgnum  = f_lgnum
      kunnr  = f_kunnr
    EXCEPTIONS
      OTHERS = 1.
ENDFORM.                    "dequeue_ezpp_materiais
*&---------------------------------------------------------------------*
*&      Form  check_kunnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_kunnr .

* Valida se o cliente existe
  CHECK NOT zwm_scr-kunnr IS INITIAL.

  CLEAR: kna1.
  SELECT SINGLE * FROM kna1
  WHERE kunnr EQ zwm_scr-kunnr.

  IF sy-subrc NE 0.
    MESSAGE e000(zwmmsg001) WITH 'O cliente' zwm_scr-kunnr 'não existe'.
  ENDIF.

ENDFORM.                    " check_kunnr
