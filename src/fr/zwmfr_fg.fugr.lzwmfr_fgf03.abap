*----------------------------------------------------------------------*
***INCLUDE LZWMFR_FGF03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  f_data_init_get_user_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--CS_XUSER   text
*      <--C_RETURN   text
*----------------------------------------------------------------------*
FORM f_data_init_get_user_data CHANGING cs_xuser  TYPE lrf_wkqu
                                        c_return  TYPE zwm_aux-retorno.

  DATA lt_xuser TYPE STANDARD TABLE OF lrf_wkqu WITH DEFAULT KEY.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = lt_xuser[]
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc NE 0.
    c_return = c_return_no_lgnum.
    RETURN.
  ENDIF.

  SORT lt_xuser[] BY statu.
  DELETE lt_xuser[] WHERE statu NE abap_true.

  IF lt_xuser[] IS INITIAL.
    c_return = c_return_no_lgnum.
    RETURN.
  ENDIF.

  READ TABLE lt_xuser[] INTO cs_xuser INDEX 1.  " only on entry with active status is allowed
ENDFORM.                    " F_DATA_INIT_GET_USER_DATA
*&---------------------------------------------------------------------*
*&      Form  f_get_to_reass_pall
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_XUSER   text
*      -->IS_ZWM020  text
*      -->IS_ZWM013  text
*      -->I_LGPLA    text
*      -->I_LGTYP    text
*      -->I_LZONE    text
*      -->I_BWLVS    text
*      <--C_RETURN   text
*      <--C_TANUM    text
*----------------------------------------------------------------------*
FORM f_get_to_reass_pall USING is_xuser   TYPE lrf_wkqu
                               is_zwm020  TYPE zwm020
                               is_zwm013  TYPE zwm013
                               i_lgpla    TYPE lagp-lgpla
                               i_lgtyp    TYPE lagp-lgtyp
                               i_lzone    TYPE lagp-lzone
                               i_bwlvs    TYPE ltak-bwlvs
                         CHANGING c_return TYPE zwm_aux-retorno
                                  c_tanum  TYPE ltak-tanum
                                  c_return_msg TYPE bapi_msg.
  DATA lv_venum   TYPE vekp-venum.
  DATA lv_werks   TYPE vepo-werks.
  DATA lv_lgort   TYPE vepo-lgort.
  DATA lv_gmblnr  TYPE mkpf-mblnr.                          "#EC NEEDED
  DATA lv_xmblnr  TYPE mkpf-mblnr.
  DATA lv_gmjahr  TYPE mkpf-mjahr.                          "#EC NEEDED
  DATA lv_xmjahr  TYPE mkpf-mjahr.
  DATA lv_tanum   TYPE ltap-tanum.

  DATA ls_xsscc TYPE zwm_sscc.
  DATA ls_ltap  TYPE ltap.

  DATA lt_xsscc   TYPE STANDARD TABLE OF zwm_sscc.
  DATA lt_return  TYPE tab_bdcmsgcoll.                      "#EC NEEDED
  DATA ls_return  LIKE LINE OF lt_return.

  FIELD-SYMBOLS <fs_xsscc> LIKE LINE OF lt_xsscc[].

  SELECT venum exidv vhilm UP TO 1 ROWS
    FROM vekp INTO (lv_venum, ls_xsscc-sscc, ls_xsscc-vhilm)
    WHERE exidv EQ is_zwm020-p1.
  ENDSELECT.

  SELECT SINGLE vemng vemeh matnr charg werks lgort
    FROM vepo INTO (ls_xsscc-quantidade, ls_xsscc-uni, ls_xsscc-material,
                    ls_xsscc-lote_producao, lv_werks, lv_lgort)
    WHERE venum EQ lv_venum
      AND vepos EQ '000001'.

  ls_xsscc-tipo_su = is_zwm013-tipo_palete.

  APPEND ls_xsscc TO lt_xsscc[].

  CLEAR ls_xsscc.

  CLEAR lv_venum.
  CLEAR lv_werks.
  CLEAR lv_lgort.

  SELECT venum exidv vhilm UP TO 1 ROWS
    FROM vekp INTO (lv_venum, ls_xsscc-sscc, ls_xsscc-vhilm)
    WHERE exidv EQ is_zwm020-p2.
  ENDSELECT.

  SELECT SINGLE vemng vemeh matnr charg werks lgort
    FROM vepo INTO (ls_xsscc-quantidade, ls_xsscc-uni, ls_xsscc-material,
                    ls_xsscc-lote_producao, lv_werks, lv_lgort)
    WHERE venum EQ lv_venum
      AND vepos EQ '000001'.

  ls_xsscc-tipo_su = is_zwm013-tipo_palete.

  APPEND ls_xsscc TO lt_xsscc[].

  CLEAR ls_xsscc.

  " Transferir dados do armazem BA para o CD
*  PERFORM f_do_goodsmvmnt_transf USING lt_xsscc[] is_xuser-lgnum i_lgpla abap_false CHANGING c_return lv_gmblnr lv_gmjahr.
*  IF c_return IS NOT INITIAL.
*    DELETE zwm013 FROM is_zwm013.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = abap_true.
*    RETURN.
*  ENDIF.

  PERFORM f_do_goodsmvmnt_transf USING lt_xsscc[] is_xuser-lgnum i_lgpla abap_true CHANGING c_return lv_xmblnr lv_xmjahr.
  IF c_return IS NOT INITIAL.
    IF c_return = 98.
      c_return = 0.
    ENDIF.
  ENDIF.

  READ TABLE lt_xsscc[] ASSIGNING <fs_xsscc> INDEX 1.

  CALL FUNCTION 'ZWM_CROSS_DOCKING'
    EXPORTING
      lgnum       = is_xuser-lgnum
      material    = <fs_xsscc>-material
      lote        = <fs_xsscc>-lote_producao
      quantidade  = <fs_xsscc>-quantidade
      uni         = <fs_xsscc>-uni
      tipo_palete = <fs_xsscc>-tipo_su
      su          = is_zwm020-p1
      su2         = is_zwm020-p2
      st_origem   = i_lgtyp
      bin_origem  = i_lgpla
    IMPORTING
      to          = lv_tanum.

  IF lv_tanum IS NOT INITIAL.
    c_tanum = lv_tanum.
  ELSE.
    CLEAR lv_tanum.

    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM014'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.                                 "#EC *

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse   = is_xuser-lgnum
        mov_type    = i_bwlvs
        plant       = lv_werks
        s_loc       = lv_lgort
        certificado = i_lzone
        req_number  = i_lgpla
        req_type    = 'E'
      IMPORTING
        to          = lv_tanum
      TABLES
        return_msg  = lt_return[]
        sscc        = lt_xsscc[]
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc NE 0.
      DELETE zwm013 FROM is_zwm013.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      c_return = 10.

      READ TABLE lt_return INTO ls_return INDEX 1.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = ls_return-msgid
          msgnr               = ls_return-msgnr
          msgv1               = ls_return-msgv1
          msgv2               = ls_return-msgv2
          msgv3               = ls_return-msgv3
          msgv4               = ls_return-msgv4
        IMPORTING
          message_text_output = c_return_msg.

*      PERFORM f_do_goodsmvmnt_transf_cancel USING lv_gmblnr lv_gmjahr CHANGING c_return.
      PERFORM f_do_goodsmvmnt_transf_cancel USING lv_xmblnr lv_xmjahr CHANGING c_return.

      IF c_return IS NOT INITIAL.

        DELETE zwm013 FROM is_zwm013.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.
    ELSE.
      SELECT * UP TO 1 ROWS
        FROM ltap INTO ls_ltap
        WHERE lgnum EQ is_xuser-lgnum
          AND tanum EQ lv_tanum.
      ENDSELECT.

      c_tanum = lv_tanum.

      IF ls_ltap-nltyp EQ c_nltyp_cma.
        DELETE zwm013 FROM is_zwm013.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        c_return = 20. " Zona Cheia
        c_return_msg = 'Zona Cheia'.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword = 'X'
        keyword_     = 'ZWM014'.
  ENDIF.
ENDFORM.                    " F_GET_TO_REASS_PALL
*&---------------------------------------------------------------------*
*&      Form  f_get_to_non_reass_pall
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_XUSER   text
*      -->I_SSCC     text
*      -->IS_ZWM013  text
*      -->I_LGPLA    text
*      -->I_LGTYP    text
*      -->I_LZONE    text
*      -->I_BWLVS    text
*      <--C_RETURN   text
*      <--C_TANUM    text
*----------------------------------------------------------------------*
FORM f_get_to_non_reass_pall USING is_xuser   TYPE lrf_wkqu
                                   i_sscc     TYPE zwm_aux-sscc
                                   is_zwm013  TYPE zwm013
                                   i_lgpla    TYPE lagp-lgpla
                                   i_lgtyp    TYPE lagp-lgtyp
                                   i_lzone    TYPE lagp-lzone
                                   i_bwlvs    TYPE ltak-bwlvs
                             CHANGING c_return TYPE zwm_aux-retorno
                                      c_tanum  TYPE ltak-tanum
                                      c_return_msg TYPE bapi_msg.
  DATA lv_venum   TYPE vekp-venum.
  DATA lv_werks   TYPE vepo-werks.
  DATA lv_lgort   TYPE vepo-lgort.
  DATA lv_lgtyp   TYPE lgtyp.                               "#EC NEEDED
  DATA lv_gmblnr  TYPE mkpf-mblnr.                          "#EC NEEDED
  DATA lv_xmblnr  TYPE mkpf-mblnr.
  DATA lv_gmjahr  TYPE mkpf-mjahr.                          "#EC NEEDED
  DATA lv_xmjahr  TYPE mkpf-mjahr.
  DATA lv_tanum   TYPE ltap-tanum.

  DATA ls_xsscc     TYPE zwm_sscc.
  DATA ls_marm      TYPE marm.
  DATA ls_ltap      TYPE ltap.
  DATA ls_matstruct TYPE zwm_material.

  DATA lt_xsscc     TYPE STANDARD TABLE OF zwm_sscc.
  DATA lt_return    TYPE tab_bdcmsgcoll.                    "#EC NEEDED
  DATA ls_return    LIKE LINE OF lt_return.

  FIELD-SYMBOLS <fs_xsscc>      LIKE LINE OF lt_xsscc[].
  FIELD-SYMBOLS <fs_zwm001>     LIKE LINE OF gt_zwm001[].

  SELECT venum exidv vhilm UP TO 1 ROWS
    FROM vekp INTO (lv_venum, ls_xsscc-sscc, ls_xsscc-vhilm)
    WHERE exidv EQ i_sscc.
  ENDSELECT.

  SELECT SINGLE vemng vemeh matnr charg werks lgort
    FROM vepo INTO (ls_xsscc-quantidade, ls_xsscc-uni, ls_xsscc-material,
                    ls_xsscc-lote_producao, lv_werks, lv_lgort)
    WHERE venum EQ lv_venum
      AND vepos EQ '000001'.

  ls_xsscc-tipo_su = is_zwm013-tipo_palete.

  APPEND ls_xsscc TO lt_xsscc[].

  CLEAR ls_xsscc.

  READ TABLE lt_xsscc[] ASSIGNING <fs_xsscc> INDEX 1.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  ls_matstruct-material = <fs_xsscc>-material.
  ls_matstruct-menge    = <fs_xsscc>-quantidade.
  ls_matstruct-meins    = <fs_xsscc>-uni.
  ls_matstruct-charg    = <fs_xsscc>-lote_producao.

  SELECT SINGLE *
    FROM marm INTO ls_marm
    WHERE matnr EQ <fs_xsscc>-material
    AND meinh EQ c_meinh_pal.
  IF <fs_xsscc>-quantidade NE ls_marm-umrez.
    READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
      WITH TABLE KEY processo   = c_process_wmentry
                     parametro  = c_param_lgtyp
                     item       = 0.
    IF sy-subrc EQ 0.
      lv_lgtyp = <fs_zwm001>-valor.
    ENDIF.
  ENDIF.

*  PERFORM f_do_goodsmvmnt_transf USING lt_xsscc[] is_xuser-lgnum i_lgpla abap_false CHANGING c_return lv_gmblnr lv_gmjahr.
*  IF c_return IS NOT INITIAL.
*    DELETE zwm013 FROM is_zwm013.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = abap_true.
*    RETURN.
*  ENDIF.

  PERFORM f_do_goodsmvmnt_transf USING lt_xsscc[] is_xuser-lgnum i_lgpla abap_true CHANGING c_return lv_xmblnr lv_xmjahr.
  IF c_return IS NOT INITIAL.
    IF c_return = 98.
      c_return = 0.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZWM_CROSS_DOCKING'
    EXPORTING
      lgnum       = is_xuser-lgnum
      material    = <fs_xsscc>-material
      lote        = <fs_xsscc>-lote_producao
      quantidade  = <fs_xsscc>-quantidade
      uni         = <fs_xsscc>-uni
      tipo_palete = <fs_xsscc>-tipo_su
      su          = i_sscc
      st_origem   = i_lgtyp
      bin_origem  = i_lgpla
    IMPORTING
      to          = lv_tanum.

  IF lv_tanum IS NOT INITIAL.
    c_tanum = lv_tanum.
  ELSE.
    CLEAR lv_tanum.

    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM014'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.                                 "#EC *

    CALL FUNCTION 'ZWM_CREATE_STORAGE_UNIT'
      EXPORTING
        warehouse   = is_xuser-lgnum
        mov_type    = i_bwlvs
*        st_type     = lv_lgtyp
        plant       = lv_werks
        s_loc       = lv_lgort
        su_type     = <fs_xsscc>-tipo_su
        certificado = i_lzone
        mat_struct  = ls_matstruct
        req_number  = i_lgpla
        req_type    = 'E'
      IMPORTING
        to          = lv_tanum
      TABLES
        result_msg  = lt_return[]
      CHANGING
        su_number   = <fs_xsscc>-sscc
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc NE 0.
      DELETE zwm013 FROM is_zwm013.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      c_return = 30.

      READ TABLE lt_return INTO ls_return INDEX 1.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = ls_return-msgid
          msgnr               = ls_return-msgnr
          msgv1               = ls_return-msgv1
          msgv2               = ls_return-msgv2
          msgv3               = ls_return-msgv3
          msgv4               = ls_return-msgv4
        IMPORTING
          message_text_output = c_return_msg.

*      PERFORM f_do_goodsmvmnt_transf_cancel USING lv_gmblnr lv_gmjahr CHANGING c_return.
      PERFORM f_do_goodsmvmnt_transf_cancel USING lv_xmblnr lv_xmjahr CHANGING c_return.

      IF c_return IS NOT INITIAL.
        DELETE zwm013 FROM is_zwm013.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.
    ELSE.
      SELECT * UP TO 1 ROWS
        FROM ltap INTO ls_ltap
        WHERE lgnum EQ is_xuser-lgnum
          AND tanum EQ lv_tanum.
      ENDSELECT.

      c_tanum = lv_tanum.

      IF ls_ltap-nltyp EQ c_nltyp_cma.
        DELETE zwm013 FROM is_zwm013.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        c_return = 20. " Zona Cheia
        c_return_msg = 'Zona Cheia'.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword = 'X'
        keyword_     = 'ZWM014'.
  ENDIF.
ENDFORM.                    " F_GET_TO_NON_REASS_PALL
*&---------------------------------------------------------------------*
*&      Form  f_do_goodsmvmnt_transf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_XSSCC      text
*      -->I_LGNUM       text
*      -->I_LGPLA       text
*      -->I_ISPALL      text
*      -->I_ISREVERSAL  text
*      <--C_RETURN      text
*      <--C_MBLNR       text
*      <--C_MJAHR       text
*----------------------------------------------------------------------*
FORM f_do_goodsmvmnt_transf USING it_xsscc      TYPE ty_t_zwm_sscc
                                  i_lgnum       TYPE lgnum
                                  i_lgpla       TYPE lagp-lgpla
                                  i_ispall      TYPE abap_bool
                            CHANGING c_return TYPE zwm_aux-retorno "#EC NEEDED
                                     c_mblnr  TYPE mkpf-mblnr "#EC NEEDED
                                     c_mjahr  TYPE mkpf-mjahr. "#EC NEEDED
  DATA lv_werks     TYPE werks_d.
  DATA lv_lgort_ba  TYPE lgort_d.
  DATA lv_mov_mm    TYPE bwlvs.                             "#EC NEEDED
  DATA lv_code      TYPE bapi2017_gm_code.                  "#EC NEEDED
  DATA lv_ablad     TYPE ablad.                             "#EC NEEDED

  DATA ls_item  TYPE zwm018.

  DATA lr_block       TYPE RANGE OF lvs_block2.
  DATA lr_block_0306  TYPE RANGE OF lvs_block2.
  DATA lr_block_0710  TYPE RANGE OF lvs_block2.

  DATA lt_items  TYPE STANDARD TABLE OF zwm018.
  DATA lt_return TYPE tab_bdcmsgcoll.                       "#EC NEEDED
  DATA lt_mlgn   TYPE SORTED TABLE OF mlgn WITH UNIQUE KEY matnr.

  FIELD-SYMBOLS <fs_xsscc>  LIKE LINE OF it_xsscc[].
  FIELD-SYMBOLS <fs_zwm001> LIKE LINE OF gt_zwm001[].
  FIELD-SYMBOLS <fs_block>  LIKE LINE OF lr_block[].
  FIELD-SYMBOLS <fs_mlgn>   LIKE LINE OF lt_mlgn[].

  WAIT UP TO 3 SECONDS.

  lv_ablad  = i_lgpla.

  IF i_ispall EQ abap_true.
    READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
      WITH TABLE KEY processo   = c_process_prodentry
                     parametro  = c_param_bwart_pal
                     item       = 0.
    IF sy-subrc EQ 0.
      lv_mov_mm = <fs_zwm001>-valor.
    ENDIF.
  ELSE.
    READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
      WITH TABLE KEY processo   = c_process_prodentry
                     parametro  = c_param_bwart_t
                     item       = 0.
    IF sy-subrc EQ 0.
      lv_mov_mm = <fs_zwm001>-valor.
    ENDIF.
  ENDIF.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_prodentry
                   parametro  = c_param_gmcode
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_code = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_plant
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_werks = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_lgort_ba
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_lgort_ba = <fs_zwm001>-valor.
  ENDIF.

  IF sy-tcode EQ 'ZWM088_A'.
    lv_lgort_ba = 'A'.
  ENDIF.

  IF it_xsscc[] IS NOT INITIAL.
    APPEND INITIAL LINE TO lr_block[] ASSIGNING <fs_block>.
    <fs_block>-sign   = c_rsig_i.
    <fs_block>-option = c_ropt_bt.
    <fs_block>-low    = c_block_03.
    <fs_block>-high   = c_block_10.

    APPEND INITIAL LINE TO lr_block_0306[] ASSIGNING <fs_block>.
    <fs_block>-sign   = c_rsig_i.
    <fs_block>-option = c_ropt_bt.
    <fs_block>-low    = c_block_03.
    <fs_block>-high   = c_block_06.

    APPEND INITIAL LINE TO lr_block_0710[] ASSIGNING <fs_block>.
    <fs_block>-sign   = c_rsig_i.
    <fs_block>-option = c_ropt_bt.
    <fs_block>-low    = c_block_07.
    <fs_block>-high   = c_block_10.

    SELECT *
      FROM mlgn INTO TABLE lt_mlgn[]
      FOR ALL ENTRIES IN it_xsscc[]
      WHERE matnr EQ it_xsscc-material
        AND lgnum EQ i_lgnum
        AND block IN lr_block[].
  ENDIF.

  LOOP AT it_xsscc[] ASSIGNING <fs_xsscc>.
    CLEAR ls_item.

    IF i_ispall EQ abap_true.
      ls_item-material    = <fs_xsscc>-vhilm.
      ls_item-quantidade  = 1.
      ls_item-uni         = c_meinh_pal.

      COLLECT ls_item INTO lt_items[].

      LOOP AT lt_mlgn[] ASSIGNING <fs_mlgn> WHERE matnr EQ <fs_xsscc>-material.
** Verificar se o material é gerido á semi-palete e fazer transferencia
** das semi-paletes.
        IF <fs_mlgn>-block IN lr_block_0306[].
          READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
            WITH TABLE KEY processo   = c_process_halfpall
                           parametro  = <fs_xsscc>-vhilm
                           item       = 0.
          CHECK sy-subrc EQ 0.

          CLEAR ls_item.

          ls_item-material    = <fs_zwm001>-valor.
          ls_item-uni         = c_meinh_pal.

          CASE <fs_mlgn>-block.
            WHEN c_block_03 OR c_block_04.
              ls_item-quantidade = 2.
            WHEN c_block_05 OR c_block_06.
              ls_item-quantidade = 4.
          ENDCASE.

          COLLECT ls_item INTO lt_items[].
** Verificar se o material é gerido à quarto-palete e fazer transferencia
** das semi-paletes.
        ELSEIF <fs_mlgn>-block IN lr_block_0710[].
          READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
            WITH TABLE KEY processo   = c_process_quarterpall
                           parametro  = <fs_xsscc>-vhilm
                           item       = 0.
          CHECK sy-subrc EQ 0.

          CLEAR ls_item.

          ls_item-material    = <fs_zwm001>-valor.
          ls_item-uni         = c_meinh_pal.

          CASE <fs_mlgn>-block.
            WHEN c_block_07 OR c_block_08.
              ls_item-quantidade = 4.
            WHEN c_block_09 OR c_block_10.
              ls_item-quantidade = 8.
          ENDCASE.

          COLLECT ls_item INTO lt_items[].
        ENDIF.
      ENDLOOP.
    ELSE.
      ls_item-material   = <fs_xsscc>-material.
      ls_item-quantidade = <fs_xsscc>-quantidade.
      ls_item-uni        = <fs_xsscc>-uni.
      ls_item-lote       = <fs_xsscc>-lote_producao.
      COLLECT ls_item INTO lt_items[].
    ENDIF.
  ENDLOOP.

  IF i_ispall EQ abap_true.
    CALL FUNCTION 'Z_WMFR_SET_BUFFER'
      EXPORTING
        i_lgnum = i_lgnum.

    CALL FUNCTION 'ZWM_ENTRADAS_PALETES_PRODUCAO'
      EXPORTING
        i_lgnum  = i_lgnum
        i_werks  = lv_werks
        i_lgort  = lv_lgort_ba
        it_items = lt_items[].
  ENDIF.

*  CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
*    EXPORTING
*      lgnum            = i_lgnum
*      code             = lv_code
*      porta            = lv_ablad
*      mov_mm           = lv_mov_mm
*      testrun          = abap_false
*      plant_o          = lv_werks
*      sloc_o           = lv_lgort_ba
*    IMPORTING
*      materialdocument = c_mblnr
*      matdocumentyear  = c_mjahr
*    TABLES
*      return_msg       = lt_return[]
*      items            = lt_items[]
*    EXCEPTIONS
*      error            = 1
*      OTHERS           = 2.
*  IF sy-subrc NE 0.
*    IF i_ispall EQ abap_true.
*      c_return = 98.
*    ELSE.
*      c_return = 99.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " F_DO_GOODSMVMNT_TRANSF
*&---------------------------------------------------------------------*
*&      Form  F_DO_GOODSMVMNT_TRANSF_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_MBLNR    text
*      -->I_MJAHR    text
*      <--C_RETURN   text
*----------------------------------------------------------------------*
FORM f_do_goodsmvmnt_transf_cancel USING i_mblnr  TYPE mkpf-mblnr
                                         i_mjahr  TYPE mkpf-mjahr
                                   CHANGING c_return TYPE zwm_aux-retorno.
  DATA lt_return TYPE tab_bdcmsgcoll.                       "#EC NEEDED

  WAIT UP TO 3 SECONDS.

  CALL FUNCTION 'ZWM_ESTORNA_DOC_MATERIAL'
    EXPORTING
      mblnr       = i_mblnr
      mjahr       = i_mjahr
    TABLES
      return      = lt_return[]
    EXCEPTIONS
      erro_commit = 1
      OTHERS      = 2.
  IF sy-subrc NE 0.
    c_return = 97.
  ENDIF.
ENDFORM.                    " F_DO_GOODSMVMNT_TRANSF_CANCEL
