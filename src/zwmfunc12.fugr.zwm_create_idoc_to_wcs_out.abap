FUNCTION zwm_create_idoc_to_wcs_out .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LTAK) TYPE  LTAK
*"  CHANGING
*"     REFERENCE(CS_IDOC_CONTROL) TYPE  EDIDC
*"     REFERENCE(CT_LTAP_VB) TYPE  TT_LTAP_VB
*"     REFERENCE(CT_IDOC_DATA) TYPE  EDIDD_TT
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_tabix     TYPE sy-tabix.
  DATA: lv_bwlvs     TYPE bwlvs.
  DATA: lv_exidv     TYPE exidv.
  DATA: lv_letyp     TYPE lvs_letyp1.
  DATA: lv_lgpla_bpe TYPE lgpla.
  DATA: lv_maabc     TYPE maabc.
  DATA: ls_ztorie    TYPE ztorie.
  DATA: ls_ztorhe    TYPE ztorhe.
  DATA: ls_e1ltorh   TYPE e1ltorh.
  DATA: ls_zwm026    TYPE zwm026.
  DATA: ls_zwm028    TYPE zwm028.
  DATA: ls_zwm040    TYPE zwm040.
  DATA: ls_ltap_vb   TYPE ltap_vb.
  DATA: ls_vekp      TYPE vekp.
  DATA: ls_ltak      TYPE ltak.
  DATA: ls_idoc_data TYPE edidd.
  DATA: ls_e1ltori   TYPE e1ltori.
  DATA: lt_idoc_data TYPE edidd_tt.
  DATA: lt_vepo      TYPE TABLE OF vepo WITH HEADER LINE.

** IDOC de OT de Saida
**********************************************************************
*WHILE 1 > 0.
*
*  ENDWHILE.

** Parametros Abastecimento Paletização Especial
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_ltak-lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_ABAST_BPE'
    IMPORTING
      e_valor     = lv_bwlvs
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_ltak-lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'POSICAO'
    IMPORTING
      e_valor     = lv_lgpla_bpe
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  READ TABLE ct_ltap_vb INDEX 1 INTO ls_ltap_vb.
  CHECK sy-subrc = 0.

  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum = ls_ltap_vb-lgnum
    AND   tanum = ls_ltap_vb-tanum.

  " Palete Picking
  IF ls_ltap_vb-vlenr IS NOT INITIAL.
    SELECT SINGLE *
       FROM zwm026 INTO ls_zwm026
       WHERE armazem = ls_ltap_vb-lgnum
       AND   sscc    = ls_ltap_vb-vlenr.
  ENDIF.

  IF ls_zwm026 IS INITIAL.
    IF ls_ltak-refnr IS NOT INITIAL.
      ls_zwm026-armazem = ls_ltak-lgnum.
      ls_zwm026-grupo   = ls_ltak-refnr.
      ls_zwm026-remessa = ls_ltak-benum.
    ENDIF.
  ENDIF.

  " Validar se é Expedição
  IF ls_zwm026 IS NOT INITIAL.
    CALL FUNCTION 'ZWM_CHECK_TO_EXP'
      EXPORTING
        i_lgnum   = ls_zwm026-armazem
        i_refnr   = ls_zwm026-grupo
        i_vbeln   = ls_zwm026-remessa
      IMPORTING
        cs_zwm028 = ls_zwm028.
  ENDIF.

** Dados Cabeçalho IDOC
  READ TABLE ct_idoc_data INTO ls_idoc_data WITH KEY segnam = 'E1LTORH'.
  IF sy-subrc = 0.
    MOVE ls_idoc_data-sdata TO ls_e1ltorh.
    ls_e1ltorh-trart = 'A'.

    " Palete BPE
    IF ls_ltak-tbnum IS NOT INITIAL.
      ls_e1ltorh-benum = ls_ltak-tbnum.
    ENDIF.

    MOVE ls_e1ltorh TO ls_idoc_data-sdata.

    MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
  ENDIF.

  lt_idoc_data[] = ct_idoc_data[].

  DELETE ct_idoc_data WHERE segnam = 'E1LTORI'.

  CLEAR ls_ztorhe.

  " Palete picking
  IF ls_zwm026-sscc IS NOT INITIAL.
    ls_ztorhe-zpick = 'X'.
  ENDIF.

  ls_ztorhe-zbcr = 'X'.

  CLEAR ls_idoc_data.
  ls_idoc_data-segnam = 'ZTORHE'.
  ls_idoc_data-sdata  = ls_ztorhe.
  APPEND ls_idoc_data TO ct_idoc_data.

** Dados Item IDOC
  LOOP AT lt_idoc_data INTO ls_idoc_data WHERE segnam = 'E1LTORI'.

    lv_tabix = sy-tabix.

    MOVE ls_idoc_data-sdata TO ls_e1ltori.

    SELECT SINGLE *
      FROM vekp INTO ls_vekp
      WHERE exidv = ls_e1ltori-vlenr.

** Rotação
    SELECT SINGLE maabc
      FROM marc INTO lv_maabc
      WHERE matnr = ls_e1ltori-matnr
      AND   werks = ls_e1ltori-werks.

    IF sy-subrc = 0.
      SELECT SINGLE wcs_maabc
        FROM zwm009 INTO lv_maabc
        WHERE abc = lv_maabc.
    ENDIF.

** Tipo de palete
    lv_letyp = ls_e1ltori-letyp.

    CLEAR ls_e1ltori-letyp.
    CALL FUNCTION 'ZWM_GET_TUD_WCS'
      EXPORTING
        i_lgnum     = i_ltak-lgnum
        i_letyp     = lv_letyp
      IMPORTING
        e_wcs_letyp = ls_e1ltori-letyp
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

** Mesa origem
    IF ls_e1ltori-ablad IS NOT INITIAL.
      ls_e1ltori-vlpla = ls_e1ltori-ablad.

      CLEAR ls_e1ltori-ablad.
    ENDIF.

** Mesa destino
    CLEAR ls_e1ltori-nlpla.

    " Buffer Paletização Especial
    IF i_ltak-bwlvs = lv_bwlvs. " 947

      ls_e1ltori-nlpla = lv_lgpla_bpe. " MAN_PEBUF

      " OT Expedição
    ELSEIF i_ltak-refnr     IS NOT INITIAL AND
         ( ls_zwm028-st_pul IS NOT INITIAL OR ls_zwm028-st_dck IS NOT INITIAL ).

      IF ls_zwm028-st_dck IS NOT INITIAL.
        ls_zwm028-st_pul  = ls_zwm028-st_dck.
        ls_zwm028-pulmao1 = ls_zwm028-porta.
      ENDIF.

      ls_e1ltori-nltyp = ls_zwm028-st_pul.

      CALL FUNCTION 'ZWM_GET_MESA_WCS'
        EXPORTING
          i_lgnum = i_ltak-lgnum
          i_lgtyp = ls_zwm028-st_pul
          i_lgpla = ls_zwm028-pulmao1
          i_type  = 'S'
        IMPORTING
          e_mesa  = ls_e1ltori-nlpla
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.

    ELSE.
      CALL FUNCTION 'ZWM_GET_MESA_WCS'
        EXPORTING
          i_lgnum = i_ltak-lgnum
          i_lgtyp = ls_e1ltori-nltyp
          i_type  = 'S'
        IMPORTING
          e_mesa  = ls_e1ltori-nlpla
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.
    ENDIF.

    MOVE ls_e1ltori TO ls_idoc_data-sdata.

    APPEND ls_idoc_data TO ct_idoc_data.

    " Dados adicionais
*    ls_ztorie-zbrgew = ls_vekp-brgew.
*    ls_ztorie-zgewei = ls_vekp-gewei.
*    ls_ztorie-zrot   = lv_maabc.
    ls_ztorie-zocme  = '0000'.

    " Sequencia carga
    ls_ztorie-zseq = '00'.

    IF ls_zwm028-ordem IS NOT INITIAL.
      IF ls_zwm028-tipo_lock = 'G'.
        ls_ztorie-zseq = '01'.
      ELSE.
        ls_ztorie-zseq = ls_zwm028-ordem.
      ENDIF.
    ENDIF.

    CLEAR ls_idoc_data.
    ls_idoc_data-segnam = 'ZTORIE'.
    ls_idoc_data-sdata  = ls_ztorie.
    APPEND ls_idoc_data TO ct_idoc_data.

  ENDLOOP.

** Extension
  IF ct_idoc_data[] IS NOT INITIAL.
    cs_idoc_control-cimtyp = 'ZWMTOID2'.
  ENDIF.

ENDFUNCTION.
