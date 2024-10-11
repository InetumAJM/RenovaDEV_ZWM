FUNCTION zwm_get_idoc_wmtord_m_from_wcs .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_IDOC_CONTROL) TYPE  EDIDC
*"  CHANGING
*"     REFERENCE(CT_IDOC_DATA) TYPE  EDIDD_TT
*"     REFERENCE(CT_LTORI) TYPE  ZWM_TT_LTAP_CREAT
*"     REFERENCE(C_LZNUM) TYPE  LVS_LZNUM
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_matnr     TYPE matnr.
  DATA: lv_tabix     TYPE sy-tabix.
  DATA: lv_tabix2    TYPE sy-tabix.
  DATA: lv_exidv     TYPE exidv.
  DATA: lv_lznum     TYPE lznum.
  DATA: lv_tanum     TYPE tanum.
  DATA: ls_zwm080    TYPE zwm080.

  DATA: ls_e1ltori   TYPE e1ltori.
  DATA: ls_ltori     TYPE ltap_creat.
  DATA: ls_idoc_data TYPE edidd.

  READ TABLE ct_idoc_data INTO ls_idoc_data WITH KEY segnam = 'E1LTORI'.
  IF sy-subrc = 0.
    lv_tabix = sy-tabix.

    MOVE ls_idoc_data-sdata TO ls_e1ltori.

** OT de pedido de pilha de palete
**********************************************************************
    IF ls_e1ltori-matnr = 'PILHA'.

      CLEAR ls_e1ltori-vlenr.

      " Alterar para material de Palete
      CALL FUNCTION 'ZWM_GET_PARAMETER'
        EXPORTING
          i_lgnum     = i_lgnum
          i_processo  = 'PALETIZACAO_ESPECIAL'
          i_parametro = 'PALETE'
        IMPORTING
          e_valor     = lv_matnr
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      ls_e1ltori-matnr = lv_matnr.
      ls_e1ltori-vlpla = lv_matnr.

      READ TABLE ct_ltori INTO ls_ltori WITH KEY matnr = 'PILHA'.
      IF sy-subrc = 0.
        CLEAR ls_ltori-vlenr.

        ls_ltori-matnr = lv_matnr.
        ls_ltori-vlpla = lv_matnr.
        ls_ltori-letyp = 'PAL'.
        ls_ltori-nltyp = 'EAU'.
        ls_ltori-nlpla = 'MAN_ENT2'.

        MODIFY ct_ltori FROM ls_ltori INDEX sy-tabix.
      ENDIF.

** Chamadas Elevador
**********************************************************************
    ELSEIF ls_e1ltori-vltyp = 'CHE'.

      LOOP AT ct_ltori INTO ls_ltori.

        ls_ltori-ablad = ls_ltori-vlenr.

        CLEAR ls_ltori-vlenr.

        MODIFY ct_ltori FROM ls_ltori INDEX sy-tabix.
      ENDLOOP.

** Bloqueio/Desbloqueio de Paletes no Automático
**********************************************************************
    ELSEIF ls_e1ltori-vltyp = 'CHD' AND ls_e1ltori-vlpla = 'LOCK' OR ls_e1ltori-vlpla = 'UNLOCK'.

      LOOP AT ct_ltori INTO ls_ltori.
        lv_tabix2 = sy-tabix.

        lv_lznum = ls_ltori-vlenr.

        CLEAR lv_tanum.
        CALL FUNCTION 'ZWM_CREATE_TO_CHD'
          EXPORTING
            i_lgnum    = i_lgnum
            i_lgpla    = ls_ltori-vlpla
            i_lznum    = lv_lznum
            i_type_mov = 'B'
          IMPORTING
            e_to_dummy = lv_tanum
          EXCEPTIONS
            error      = 1
            OTHERS     = 2.

        IF lv_tanum IS NOT INITIAL.
          CLEAR ls_zwm080.

          GET TIME STAMP FIELD ls_zwm080-timestamp.

          ls_zwm080-lgnum  = i_lgnum.
          ls_zwm080-exidv  = lv_lznum.
          ls_zwm080-tanum  = lv_tanum.
          ls_zwm080-docnum = i_idoc_control-docnum.

          ls_zwm080-lgpla  = ls_ltori-vlpla.
          ls_zwm080-datum  = sy-datum.
          ls_zwm080-uzeit  = sy-uzeit.
          ls_zwm080-uname  = sy-uname.

          MODIFY zwm080 FROM ls_zwm080.
          IF sy-subrc = 0.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.

        DELETE ct_ltori INDEX lv_tabix2.
      ENDLOOP.

      c_lznum = '@CHD_BLOQ@'.

    ENDIF.
  ENDIF.

ENDFUNCTION.
