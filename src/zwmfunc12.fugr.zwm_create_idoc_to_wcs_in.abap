FUNCTION zwm_create_idoc_to_wcs_in .
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
  DATA: lv_linha     TYPE lgpla.
  DATA: lv_lznum     TYPE lznum.
  DATA: lv_letyp     TYPE lvs_letyp1.
  DATA: lv_maabc     TYPE maabc.
  DATA: lv_lgpla     TYPE lgpla.
  DATA: lv_lgtyp     TYPE lgtyp.
  DATA: lv_valor     TYPE zwm_valor.
  DATA: lv_flag_epe  TYPE flag.
  DATA: lv_flag_bpe  TYPE flag.
  DATA: lv_benum     TYPE ltak-benum.
  DATA: lv_dummy     TYPE char10.
  DATA: lv_process   TYPE char10.
  DATA: lv_remessa   TYPE vbeln.

  DATA: ls_ztorie    TYPE ztorie.
  DATA: ls_ztorhe    TYPE ztorhe.
  DATA: ls_e1ltorh   TYPE e1ltorh.
  DATA: ls_zwm026    TYPE zwm026.
  DATA: ls_zwm028    TYPE zwm028.
  DATA: ls_zwm040    TYPE zwm040.
  DATA: ls_ltap_vb   TYPE ltap_vb.
  DATA: ls_vekp      TYPE vekp.
  DATA: ls_idoc_data TYPE edidd.
  DATA: ls_e1ltori   TYPE e1ltori.
  DATA: ls_lagp      TYPE lagp.

  DATA: lt_idoc_data TYPE edidd_tt.

  DATA: lt_vepo      TYPE TABLE OF vepo   WITH HEADER LINE.
  DATA: lt_ltap      TYPE TABLE OF ltap   WITH HEADER LINE.
  DATA: lt_ltak      TYPE TABLE OF ltak   WITH HEADER LINE.
  DATA: lt_zwm077    TYPE TABLE OF zwm077 WITH HEADER LINE.

** IDOC de OT de Entrada
**********************************************************************
*  WHILE 1 > 0.
*
*  ENDWHILE.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_ltak-lgnum
      i_processo  = 'OT_DUMMY'
      i_parametro = 'MOV_WM_WCS'
    IMPORTING
      e_valor     = lv_bwlvs
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  READ TABLE ct_ltap_vb INDEX 1 INTO ls_ltap_vb.
  CHECK sy-subrc = 0.

** OT - Aviso de Chamada
**********************************************************************
  IF ( ls_ltap_vb-vltyp = 'CHD' ). "AND i_ltak-bwlvs = lv_bwlvs AND i_ltak-lznum = 'DITA' ).

** Dados Cabeçalho de IDOC
    READ TABLE ct_idoc_data INTO ls_idoc_data WITH KEY segnam = 'E1LTORH'.
    IF sy-subrc = 0.
      MOVE ls_idoc_data-sdata TO ls_e1ltorh.

      " Aviso de Entrada de Produção
      IF i_ltak-bwlvs = lv_bwlvs.
        ls_e1ltorh-trart = 'E'.

        " Aviso de Retirada da Gravitica
      ELSE.
        ls_e1ltorh-trart = 'A'.
      ENDIF.

      " Validar se é um aviso de entrada de produção
      lv_lznum = ls_e1ltorh-lznum.

      " Linha
*      IF lv_lznum(1) = 'H'.
      lv_linha = lv_lznum.

      " CoPacking
      IF lv_lznum(3) = 'GMA'.
        lv_linha = 'MAN_ENT1'.
      ENDIF.

      CLEAR ls_e1ltorh-lznum.

      MOVE ls_e1ltorh TO ls_idoc_data-sdata.

      MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
    ENDIF.

    lt_idoc_data[] = ct_idoc_data[].

    DELETE ct_idoc_data WHERE segnam = 'E1LTORI'.

    CLEAR ls_ztorhe.

    " CoPacking
    IF lv_lznum(3) = 'GMA'.
      ls_ztorhe-zenfa = 'X'.
      ls_ztorhe-zetiq = 'X'.
    ENDIF.

    " Validar se uma palete de Reintrodução de Rejeição
    IF ls_ltap_vb-zeugn(1) = '@'.
      CLEAR: ls_ztorhe-zenfa, ls_ztorhe-zetiq.

      IF ls_ltap_vb-zeugn+1(1) = '1'.
        ls_ztorhe-zetiq = 'X'.
      ENDIF.

      IF ls_ltap_vb-zeugn+2(1) = '1'.
        ls_ztorhe-zenfa = 'X'.
      ENDIF.
    ENDIF.

    ls_ztorhe-zbcr = 'X'.

    CLEAR ls_idoc_data.
    ls_idoc_data-segnam = 'ZTORHE'.
    ls_idoc_data-sdata  = ls_ztorhe.
    APPEND ls_idoc_data TO ct_idoc_data.

*    REFRESH ct_idoc_data.

** Dados Items de IDOC
    LOOP AT lt_idoc_data INTO ls_idoc_data WHERE segnam = 'E1LTORI'.

      lv_tabix = sy-tabix.

      MOVE ls_idoc_data-sdata TO ls_e1ltori.

      " Dados da HU
      lv_exidv = ls_e1ltori-ablad.

      CLEAR ls_e1ltori-ablad.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_exidv
        IMPORTING
          output = lv_exidv.

      SELECT SINGLE *
        FROM vekp INTO ls_vekp
        WHERE exidv = lv_exidv.

      SELECT *
        FROM vepo INTO TABLE lt_vepo
        WHERE venum = ls_vekp-venum.

      READ TABLE lt_vepo INDEX 1.
      IF sy-subrc = 0.
        ls_e1ltori-werks = lt_vepo-werks.
        ls_e1ltori-lgort = lt_vepo-lgort.
        ls_e1ltori-matnr = lt_vepo-matnr.
        ls_e1ltori-charg = lt_vepo-charg.
        ls_e1ltori-vfdat = lt_vepo-vfdat.
        ls_e1ltori-vsolm = lt_vepo-vemng.
        ls_e1ltori-nsolm = lt_vepo-vemng.
        ls_e1ltori-meins = lt_vepo-vemeh.
        ls_e1ltori-vlenr = ls_vekp-exidv.
        ls_e1ltori-wdatu = sy-datum.
*        ls_e1ltori-nlenr = ls_vekp-exidv.

        " Descrição
        SELECT SINGLE maktx
          FROM makt INTO ls_e1ltori-maktx
          WHERE matnr = lt_vepo-matnr
          AND   spras = sy-langu.

        " Rotação
        SELECT SINGLE maabc
          FROM marc INTO lv_maabc
          WHERE matnr = lt_vepo-matnr
          AND   werks = lt_vepo-werks.

        IF sy-subrc = 0.
          SELECT SINGLE wcs_maabc
            FROM zwm009 INTO lv_maabc
            WHERE abc = lv_maabc.
        ENDIF.

        " Linha de Entrada de produção / CoPacking
        IF lv_linha IS NOT INITIAL.
          ls_e1ltori-vlpla = lv_linha.
        ENDIF.

        " Tipo de palete
        CLEAR ls_e1ltori-letyp.

        SELECT SINGLE lety1
          FROM mlgn INTO lv_letyp
          WHERE matnr = lt_vepo-matnr
          AND   lgnum = i_ltak-lgnum.

        CALL FUNCTION 'ZWM_GET_TUD_WCS'
          EXPORTING
            i_lgnum     = i_ltak-lgnum
            i_letyp     = lv_letyp
          IMPORTING
            e_wcs_letyp = ls_e1ltori-letyp
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
      ENDIF.

      CLEAR ls_e1ltori-zeugn.

      MOVE ls_e1ltori TO ls_idoc_data-sdata.

      APPEND ls_idoc_data TO ct_idoc_data.

      " Dados adicionais
      ls_ztorie-zbrgew = ls_vekp-brgew.
      ls_ztorie-zgewei = ls_vekp-gewei.
      ls_ztorie-zrot   = lv_maabc.
      ls_ztorie-zocme  = '0000'.

      " Identificação da Linha de Produção para OCME
      IF lv_linha IS NOT INITIAL.
        SELECT SINGLE cod_linha
          FROM z02rplinhas INTO ls_ztorie-zocme
          WHERE werks = lt_vepo-werks
          AND   fevor = lv_linha(3).
      ENDIF.

      CLEAR ls_idoc_data.
      ls_idoc_data-segnam = 'ZTORIE'.
      ls_idoc_data-sdata  = ls_ztorie.
      APPEND ls_idoc_data TO ct_idoc_data.
    ENDLOOP.

** OT de Entrada/Transferência/Bloqueio
**********************************************************************
  ELSEIF ( ls_ltap_vb-vltyp = 'EAU' OR ls_ltap_vb-vltyp = 'BPE' OR ls_ltap_vb-vltyp = '922'). "AND ls_ltap_vb-nltyp = 'AUT' AND ls_ltap_vb-nlpla = 'AUT' ).

** Palete Picking
    IF ls_ltap_vb-vlenr IS NOT INITIAL.
      SELECT SINGLE *
         FROM zwm026 INTO ls_zwm026
         WHERE armazem = ls_ltap_vb-lgnum
         AND   sscc    = ls_ltap_vb-vlenr.

      IF sy-subrc = 0.
        SELECT SINGLE *
          FROM zwm040 INTO ls_zwm040
          WHERE lgnum   = ls_zwm026-armazem
          AND   refnr   = ls_zwm026-grupo
          AND   remessa = ls_zwm026-remessa.

        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM zwm028 INTO ls_zwm028
            WHERE lgnum   = ls_zwm026-armazem
            AND   refnr   = ls_zwm026-grupo
            AND   remessa = ls_zwm040-id_servisan.

          lv_remessa = ls_zwm040-id_servisan.

        ELSE.
          SELECT SINGLE *
            FROM zwm028 INTO ls_zwm028
            WHERE lgnum   = ls_zwm026-armazem
            AND   refnr   = ls_zwm026-grupo
            AND   remessa = ls_zwm026-remessa.

          lv_remessa = ls_zwm026-remessa.
        ENDIF.
      ENDIF.
    ENDIF.

** Palete de Saída da Estação Paletização Especial
    lv_lgpla = ls_ltap_vb-ablad.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_ltak-lgnum
        i_processo  = 'PALETIZACAO_ESPECIAL'
        i_parametro = 'TIPO_DEP_ESTACAO'
      IMPORTING
        e_valor     = lv_valor
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc = 0.
      lv_lgtyp = lv_valor.
    ENDIF.

    SELECT SINGLE *
      FROM lagp INTO ls_lagp
      WHERE lgnum = i_ltak-lgnum
      AND   lgtyp = lv_lgtyp
      AND   lgpla = lv_lgpla.

    IF sy-subrc <> 0.
      lv_lgpla = ls_ltap_vb-zeugn.

      SELECT SINGLE *
        FROM lagp INTO ls_lagp
        WHERE lgnum = i_ltak-lgnum
        AND   lgtyp = lv_lgtyp
        AND   lgpla = lv_lgpla.
    ENDIF.

    IF sy-subrc = 0.
      lv_flag_epe = 'X'.
    ENDIF.

** Palete de Entrada no Buffer Paletização Especial
    IF ls_ltap_vb-nltyp = 'BPE'.

      lv_flag_bpe = 'X'.

      SELECT *
        FROM ltap INTO TABLE lt_ltap
        WHERE lgnum = i_ltak-lgnum
        AND   nlenr = ls_ltap_vb-vlenr.

      IF lt_ltap[] IS NOT INITIAL.
        SELECT *
          FROM ltak INTO TABLE lt_ltak
          FOR ALL ENTRIES IN lt_ltap
          WHERE lgnum = lt_ltap-lgnum
          AND   tanum = lt_ltap-tanum.

        DELETE lt_ltak WHERE tbnum IS INITIAL.
      ENDIF.

      READ TABLE lt_ltak INDEX 1.
      IF sy-subrc = 0.
        lv_benum = lt_ltak-tbnum.
      ENDIF.
    ENDIF.

** Dados Cabeçalho IDOC
    READ TABLE ct_idoc_data INTO ls_idoc_data WITH KEY segnam = 'E1LTORH'.
    IF sy-subrc = 0.
      MOVE ls_idoc_data-sdata TO ls_e1ltorh.

      ls_e1ltorh-trart = 'E'.

      lv_lznum = ls_e1ltorh-lznum.

      " Palete picking
      IF ls_zwm026-grupo IS NOT INITIAL.
        ls_e1ltorh-refnr = ls_zwm026-grupo.
        ls_e1ltorh-lznum = lv_remessa.
      ENDIF.

      " Palete BPE
      IF lv_flag_bpe IS NOT INITIAL.
        ls_e1ltorh-benum = lv_benum.
      ENDIF.

      MOVE ls_e1ltorh TO ls_idoc_data-sdata.

      MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
    ENDIF.

    lt_idoc_data[] = ct_idoc_data[].

    DELETE ct_idoc_data WHERE segnam = 'E1LTORI'.

    CLEAR ls_ztorhe.

    " Palete picking
    IF ls_zwm026 IS NOT INITIAL.

      CLEAR ls_ltap_vb-zeugn.

      SPLIT lv_lznum AT '#' INTO lv_dummy ls_ltap_vb-zeugn.

      ls_ztorhe-zenfa = 'X'.
      ls_ztorhe-zetiq = 'X'.
      ls_ztorhe-zpick = 'X'.

      " Palete EPE
    ELSEIF lv_flag_epe IS NOT INITIAL.
      ls_ztorhe-zenfa = 'X'.
      ls_ztorhe-zetiq = 'X'.
    ENDIF.

    " Validar se é uma palete de Reintrodução de Rejeição
    IF ls_ltap_vb-zeugn(1) = '@'.
      CLEAR: ls_ztorhe-zenfa, ls_ztorhe-zetiq.

      IF ls_ltap_vb-zeugn+1(1) = '1'.
        ls_ztorhe-zetiq = 'X'.
      ENDIF.

      IF ls_ltap_vb-zeugn+2(1) = '1'.
        ls_ztorhe-zenfa = 'X'.
      ENDIF.
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

      " OT bloqueio/desbloqueio
      IF ls_ltap_vb-vltyp = '922'.
        ls_e1ltori-vlenr = ls_e1ltori-nlenr.
        CLEAR ls_e1ltori-nlenr.
      ENDIF.

      SELECT SINGLE *
        FROM vekp INTO ls_vekp
        WHERE exidv = ls_e1ltori-vlenr.

      " Rotação
      SELECT SINGLE maabc
        FROM marc INTO lv_maabc
        WHERE matnr = ls_e1ltori-matnr
        AND   werks = ls_e1ltori-werks.

      IF sy-subrc = 0.
        SELECT SINGLE wcs_maabc
          FROM zwm009 INTO lv_maabc
          WHERE abc = lv_maabc.
      ENDIF.

      " Tipo de palete
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

      " Posição de Origem
      IF ls_ltap_vb-vltyp = '922'. "Bloqueio/Desbloqueio
        ls_e1ltori-vlpla = 'AUT'.

      ELSEIF ls_e1ltori-ablad IS NOT INITIAL. " Entrada Automático
        ls_e1ltori-vlpla = ls_e1ltori-ablad.

        CLEAR ls_e1ltori-ablad.

      ELSEIF ls_e1ltori-zeugn IS NOT INITIAL. " Entr. Aut. Palete Picking

        ls_e1ltori-vlpla = ls_e1ltori-zeugn.

*        CLEAR ls_e1ltori-zeugn.
      ENDIF.

      CLEAR ls_e1ltori-zeugn.

      MOVE ls_e1ltori TO ls_idoc_data-sdata.

      APPEND ls_idoc_data TO ct_idoc_data.

      " Dados adicionais
      ls_ztorie-zbrgew = ls_vekp-brgew.
      ls_ztorie-zgewei = ls_vekp-gewei.
      ls_ztorie-zrot   = lv_maabc.

      ls_ztorie-zseq   = '00'.
      ls_ztorie-zocme  = '0000'.

      " Palete picking
      IF ls_zwm028-ordem IS NOT INITIAL.
        ls_ztorie-zseq = ls_zwm028-ordem.
      ENDIF.

      CLEAR ls_idoc_data.
      ls_idoc_data-segnam = 'ZTORIE'.
      ls_idoc_data-sdata  = ls_ztorie.
      APPEND ls_idoc_data TO ct_idoc_data.
    ENDLOOP.

  ENDIF.

ENDFUNCTION.
