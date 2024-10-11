*&---------------------------------------------------------------------*
*&  Include           ZWM_PALLET_MOV_JOB_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados CHANGING pu_nodata.
  DATA: lt_docu TYPE TABLE OF zwm_docu_year.
  CLEAR pu_nodata.
  REFRESH: gt_mkpf,
           gt_mseg.
  SELECT * FROM zwm_docu_year
    INTO TABLE lt_docu
*    WHERE bwart in gr_saida
    WHERE processado <> 'X'.


  IF sy-subrc <> 0.
    MESSAGE i040(zwm001) WITH 'ZWM_DOCU_YEAR'.
    pu_nodata = 'X'.
  ENDIF.
  CHECK pu_nodata IS INITIAL.

  SELECT * FROM mkpf
    INTO TABLE gt_mkpf
    FOR ALL ENTRIES IN lt_docu
    WHERE mblnr = lt_docu-mblnr
    AND   mjahr = lt_docu-mjahr.

  IF gt_mkpf IS NOT INITIAL.
    SELECT * FROM mseg
      INTO TABLE gt_mseg
      FOR ALL ENTRIES IN gt_mkpf
      WHERE mblnr = gt_mkpf-mblnr
      AND mjahr = gt_mkpf-mjahr.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_MOVIMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM executa_movimentos .
  DATA: lt_mkpf TYPE ty_t_mkpf,
        lt_mseg TYPE ty_t_mseg,
        ls_mkpf TYPE mkpf,
        ls_mseg TYPE mseg,
        lt_docu TYPE zwm_tt_docu_year,
        ls_docu TYPE zwm_docu_year,
        ls_zwm001 TYPE zwm001,
        lv_estorno TYPE flag,
        lv_saida TYPE flag.

*  gv_update = 'X'.
  SORT: gt_mkpf,
        gt_mseg.

  REFRESH gt_docu.
* Vamos processar cada uma das linhas para o respectivo
* movimento de marcadoria.
  LOOP AT gt_mkpf INTO ls_mkpf.
    REFRESH: lt_mkpf,
             lt_mseg,
             lt_docu.

    APPEND ls_mkpf TO lt_mkpf.
    CLEAR: lv_saida, lv_estorno.
    LOOP AT gt_mseg INTO ls_mseg WHERE mblnr = ls_mkpf-mblnr
                                 AND   mjahr = ls_mkpf-mjahr.

      IF ls_mseg-bwart IN gr_saida.
        lv_saida = 'X'.
      ELSEIF ls_mseg-bwart IN gr_estorno.
        lv_estorno = 'X'.
      ENDIF.

      READ TABLE gt_zwm001 INTO ls_zwm001 WITH KEY parametro = ls_mseg-werks.
      IF sy-subrc EQ 0.
        ls_mseg-kostl = ls_zwm001-valor.
      ENDIF.
      APPEND ls_mseg TO lt_mseg.
    ENDLOOP.
    IF sy-subrc EQ 0.
      IF lv_saida IS NOT INITIAL.
        CALL FUNCTION 'ZWM_BADI_PALLET_TRANSFER'
          EXPORTING
            it_xmkpf = lt_mkpf
            it_xmseg = lt_mseg
          IMPORTING
            it_docu  = lt_docu.
        APPEND LINES OF lt_docu TO gt_docu.
        MODIFY zwm_docu_year FROM TABLE lt_docu.
        COMMIT WORK AND WAIT.
      ENDIF.
      IF lv_estorno IS NOT INITIAL.
        CALL FUNCTION 'ZWM_ESTORNA_DOC_MATERIAL_EMAIL'
          EXPORTING
            mblnr   = ls_mseg-mblnr
            mjahr   = ls_mseg-mjahr
            bwart   = ls_mseg-bwart
            budat   = ls_mkpf-budat
          IMPORTING
            it_docu = lt_docu.
        APPEND LINES OF lt_docu TO gt_docu.
         MODIFY zwm_docu_year FROM TABLE lt_docu.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Se não existirem erros de processamento vamos eliminar
* as entradas da tabela Z.
*  IF gt_docu IS NOT INITIAL.
*    MODIFY zwm_docu_year FROM TABLE gt_docu.
*    COMMIT WORK AND WAIT.
*  ENDIF.

  LOOP AT gt_docu INTO ls_docu.
    IF sy-batch IS NOT INITIAL.
      MESSAGE s041(zwm001) WITH ls_docu-mblnr ls_docu-mjahr.
    ENDIF.
  ENDLOOP.

  REFRESH: gt_docu.
* Executa estornos de material.

*  LOOP AT gt_mkpf INTO ls_mkpf.
*    REFRESH: lt_mkpf,
*             lt_mseg,
*             lt_docu.
*
*    APPEND ls_mkpf TO lt_mkpf.
*
*    LOOP AT  gt_mseg INTO ls_mseg WHERE mblnr = ls_mkpf-mblnr
*                                  AND   mjahr = ls_mkpf-mjahr
*                                  AND bwart IN gr_estorno.
*      CALL FUNCTION 'ZWM_ESTORNA_DOC_MATERIAL_EMAIL'
*        EXPORTING
*          mblnr   = ls_mseg-mblnr
*          mjahr   = ls_mseg-mjahr
*          bwart   = ls_mseg-bwart
*        IMPORTING
*          it_docu = lt_docu.
*      APPEND LINES OF lt_docu TO gt_docu.
*      CONTINUE.
*    ENDLOOP.
*  ENDLOOP.


** Se não existirem erros de processamento vamos eliminar
** as entradas da tabela Z.
*  IF gt_docu IS NOT INITIAL.
*    MODIFY zwm_docu_year FROM TABLE gt_docu.
*  ENDIF.
*
*  LOOP AT gt_docu INTO ls_docu.
*    IF sy-batch IS NOT INITIAL.
*      MESSAGE s041(zwm001) WITH ls_docu-mblnr ls_docu-mjahr.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " EXECUTA_MOVIMENTOS
*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETRIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_parametrizacao .

  CONSTANTS: lc_proc TYPE zwm001-processo VALUE 'ZWM_PALLET_MOV_JOB',
             lc_saida TYPE zwm001-parametro VALUE 'MOVIMENTO_SAIDA',
             lc_estorno TYPE zwm001-parametro VALUE 'MOVIMENTO_ESTORNO'.

  REFRESH: gr_estorno,
           gr_saida.


  DATA: ls_saida LIKE LINE OF gr_saida,
        ls_estorno LIKE LINE OF gr_estorno,
        ls_zwm001 TYPE zwm001.

*  DATA: lt_zwm001 TYPE TABLE OF zwm001.
  SELECT * FROM zwm001
    INTO TABLE gt_zwm001
    WHERE processo = lc_proc.

  LOOP AT gt_zwm001 INTO ls_zwm001.
    IF  ls_zwm001-parametro(15) EQ lc_saida.
      ls_saida-option = 'EQ'.
      ls_saida-sign = 'I'.
      ls_saida-low = ls_zwm001-valor.
      APPEND ls_saida TO gr_saida.
    ENDIF.
    IF  ls_zwm001-parametro(17) EQ lc_estorno.
      ls_estorno-option = 'EQ'.
      ls_estorno-sign = 'I'.
      ls_estorno-low = ls_zwm001-valor.
      APPEND ls_estorno TO gr_estorno.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_PARAMETRIZACAO
