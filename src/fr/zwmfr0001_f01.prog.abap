*----------------------------------------------------------------------*
* Include: ZWMFR0001_F01
*----------------------------------------------------------------------*
* Description: Monitor de planeamento do abastecimento à produção
*----------------------------------------------------------------------*
* Author........: [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date: 2015-10-23
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_LAYO  text
*----------------------------------------------------------------------*
FORM f_get_layout CHANGING c_layo TYPE slis_vari.
  DATA ls_layout      TYPE salv_s_layout_key.
  DATA ls_layout_info TYPE salv_s_layout_info.

  ls_layout-report = sy-repid.

  ls_layout_info = cl_salv_layout_service=>f4_layouts( ls_layout ).

  c_layo = ls_layout_info-layout.
ENDFORM.                    " F_GET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_SET_SOPTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_soptions.
  DATA ls_soptions TYPE sscr_restrict.

  FIELD-SYMBOLS <fs_ltab>   LIKE LINE OF ls_soptions-opt_list_tab[].
  FIELD-SYMBOLS <fs_atab>   LIKE LINE OF ls_soptions-ass_tab[].
  FIELD-SYMBOLS <fs_gstrp>  LIKE LINE OF s_gstrp[].

  APPEND INITIAL LINE TO ls_soptions-opt_list_tab[] ASSIGNING <fs_ltab>.
  <fs_ltab>-name        = c_ropt_eq.
  <fs_ltab>-options-eq  = abap_true.

  APPEND INITIAL LINE TO ls_soptions-ass_tab[] ASSIGNING <fs_atab>.
  <fs_atab>-name    = c_scrname_werks.
  <fs_atab>-kind    = c_scr_kind.
  <fs_atab>-sg_main = c_rsig_i.
  <fs_atab>-sg_addy = space.
  <fs_atab>-op_main = c_ropt_eq.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = ls_soptions.

  APPEND INITIAL LINE TO s_gstrp[] ASSIGNING <fs_gstrp>.
  <fs_gstrp>-sign   = c_rsig_i.
  <fs_gstrp>-option = c_ropt_bt.
  <fs_gstrp>-low    = sy-datum - 10.
  <fs_gstrp>-high   = sy-datum.
ENDFORM.                    " F_SET_SOPTIONS
*&---------------------------------------------------------------------*
*&      Form  f_data_free
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_SOFTREFRESH  text
*----------------------------------------------------------------------*
FORM f_data_free USING i_softrefresh TYPE abap_bool.
  IF i_softrefresh EQ abap_false.
    CLEAR gs_xuser.

    FREE gr_lgnum[].

    FREE gt_hardcodes[].
    FREE gt_zwmmpt001[].
  ENDIF.

  FREE gt_afko[].
  FREE gt_afpo[].
  FREE gt_aufk[].
  FREE gt_resb[].
  FREE gt_mara[].
  FREE gt_makt[].
  FREE gt_t024f[].
  FREE gt_kostl[].
  FREE gt_output_tmp[].
  FREE gt_output[].
  FREE gt_lqua[].
  FREE gt_lqua_coll[].
  FREE gt_dupview[].
ENDFORM.                    " F_DATA_FREE
*&---------------------------------------------------------------------*
*&      Form  f_data_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_SOFTREFRESH  text
*----------------------------------------------------------------------*
FORM f_data_init USING i_softrefresh TYPE abap_bool.
  IF i_softrefresh EQ abap_true.
    RETURN.
  ENDIF.

  PERFORM f_data_init_get_user_data.    " get user RF data for WM
  PERFORM f_data_init_get_hardcodes.    " get hardcodes for program
  PERFORM f_data_init_get_plant_lgnum.  " get selected plant's WM storage locations
  PERFORM f_data_init_check_user_lgnum. " check if user is active for obtained storage locations
ENDFORM.                    " F_DATA_INIT
*&---------------------------------------------------------------------*
*&      Form  F_DATA_INIT_GET_HARDCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_init_get_hardcodes.
  DATA: lt_hardcodes TYPE STANDARD TABLE OF zhardcode_table,
        lt_zwmmpt001 TYPE TABLE OF zwmmpt001.

  DATA: ls_zwmmpt001 TYPE zwmmpt001.

  FIELD-SYMBOLS <fs_hardc> LIKE LINE OF gt_hardcodes[].

  SELECT *
    FROM zhardcode_table INTO TABLE lt_hardcodes[]
    WHERE inc_meth EQ c_syrepid.

  SORT lt_hardcodes[] BY inc_meth occurrence counter.
  DELETE ADJACENT DUPLICATES FROM lt_hardcodes[] COMPARING inc_meth occurrence counter.

  LOOP AT lt_hardcodes[] ASSIGNING <fs_hardc>.
    INSERT <fs_hardc> INTO TABLE gt_hardcodes[].
  ENDLOOP.

  SELECT processo parametro item valor
    FROM zwmmpt001 INTO TABLE gt_zwmmpt001[]
    WHERE armazem EQ gs_xuser-lgnum.

  SELECT * FROM zwmmpt001
           INTO TABLE lt_zwmmpt001
           WHERE armazem = gs_xuser-lgnum AND
                 processo = 'DEVOLUCAO_PRODUCAO'.

  LOOP AT lt_zwmmpt001 INTO ls_zwmmpt001.
    CASE ls_zwmmpt001-parametro.
      WHEN 'MAT_GERAL'.
        MOVE ls_zwmmpt001-valor TO gv_dummy_mat.
      WHEN 'MOV_NEC'.
        MOVE ls_zwmmpt001-valor TO gv_dummy_dev.
      WHEN 'QUEUE_NEC'.
        MOVE  ls_zwmmpt001-valor TO gv_queue_dev.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " F_DATA_INIT_GET_HARDCODES
*&---------------------------------------------------------------------*
*&      Form  F_DATA_INIT_GET_USER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_init_get_user_data.
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
    MESSAGE e001 WITH sy-uname.
  ENDIF.

  SORT lt_xuser[] BY statu.
  DELETE lt_xuser[] WHERE statu NE abap_true.

  IF lt_xuser[] IS INITIAL.
    MESSAGE e001 WITH sy-uname.
  ENDIF.

  READ TABLE lt_xuser[] INTO gs_xuser INDEX 1.  " only on entry with active status is allowed
ENDFORM.                    " F_DATA_INIT_GET_USER_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DATA_INIT_GET_PLANT_LGNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_init_get_plant_lgnum.
  DATA lt_lgort TYPE STANDARD TABLE OF lgort_d WITH DEFAULT KEY.
  DATA lt_lgnum TYPE STANDARD TABLE OF lgnum WITH DEFAULT KEY.

  FIELD-SYMBOLS <fs_lgnum> LIKE LINE OF lt_lgnum[].
  FIELD-SYMBOLS <fs_rlgnu> LIKE LINE OF gr_lgnum[].

  SELECT lgort
    FROM t001l INTO TABLE lt_lgort[]
    WHERE werks IN s_werks[].                           "#EC CI_GENBUFF
  IF sy-subrc NE 0.
    MESSAGE e002.
  ENDIF.

  SELECT lgnum
    FROM t320 INTO TABLE lt_lgnum[]
    FOR ALL ENTRIES IN lt_lgort[]
    WHERE werks IN s_werks[]
      AND lgort EQ lt_lgort-table_line.
  IF sy-subrc NE 0.
    MESSAGE e002.
  ENDIF.

  SORT lt_lgnum[] BY table_line.
  DELETE ADJACENT DUPLICATES FROM lt_lgnum[] COMPARING table_line.

  LOOP AT lt_lgnum[] ASSIGNING <fs_lgnum>.
    APPEND INITIAL LINE TO gr_lgnum[] ASSIGNING <fs_rlgnu>.
    <fs_rlgnu>-sign   = c_rsig_i.
    <fs_rlgnu>-option = c_ropt_eq.
    <fs_rlgnu>-low    = <fs_lgnum>.
  ENDLOOP.
ENDFORM.                    " F_DATA_INIT_GET_PLANT_LGNUM
*&---------------------------------------------------------------------*
*&      Form  F_DATA_INIT_CHECK_USER_LGNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_init_check_user_lgnum.
  IF gs_xuser-lgnum NOT IN gr_lgnum[].
    MESSAGE e003 WITH sy-uname.
  ENDIF.
ENDFORM.                    " F_DATA_INIT_CHECK_USER_LGNUM
*&---------------------------------------------------------------------*
*&      Form  F_DATA_QUERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_query.
  IF s_aufnr[] IS NOT INITIAL.  " use primary key from AFKO
    PERFORM f_data_query_by_afko USING abap_false.
    PERFORM f_data_query_by_resb USING abap_true.
  ELSEIF s_matnr[] IS NOT INITIAL. " use index RESB~M for material
    PERFORM f_data_query_by_resb USING abap_false.
    PERFORM f_data_query_by_afko USING abap_true.
  ELSE. " use index AUFK~B/AUFK~C from AUFK for plant and order category/type
    PERFORM f_data_query_by_afko USING abap_false.
    PERFORM f_data_query_by_resb USING abap_true.
  ENDIF.

  PERFORM f_data_query_get_descriptions.
  PERFORM f_data_query_get_kostl.
ENDFORM.                    " F_DATA_QUERY
*&---------------------------------------------------------------------*
*&      Form  f_data_query_by_resb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_AFTERQUERY  text
*----------------------------------------------------------------------*
FORM f_data_query_by_resb USING i_afterquery TYPE abap_bool.
  DATA lt_resb        TYPE STANDARD TABLE OF ty_resb.
  DATA lt_resb_matnr  TYPE ty_st_resb_matnr.

  FIELD-SYMBOLS <fs_resb> LIKE LINE OF gt_resb[].

  IF i_afterquery EQ abap_false.  " initial query by s_matnr[]
    SELECT rsnum rspos rsart xloek xwaok kzear matnr
           werks bdmng enmng meins aufnr
      FROM resb INTO TABLE lt_resb[]
      WHERE matnr IN s_matnr[]
        AND werks IN s_werks[]
        AND xloek EQ space
        AND kzear EQ space. " partial index RESB~M
  ELSE. " use obtained materials for each AFKO-RSNUM
    SELECT rsnum rspos rsart xloek xwaok kzear matnr
           werks bdmng enmng meins aufnr
      FROM resb INTO TABLE lt_resb[]
      FOR ALL ENTRIES IN gt_afko[]
      WHERE rsnum EQ gt_afko-rsnum. " partial primary key

    SORT lt_resb[] BY xloek.
    DELETE lt_resb[] WHERE xloek NE space.
    SORT lt_resb[] BY kzear.
    DELETE lt_resb[] WHERE kzear NE space.
  ENDIF.

  SORT lt_resb[] BY xwaok.
  DELETE lt_resb[] WHERE xwaok EQ space.
  SORT lt_resb[] BY bdmng.
  DELETE lt_resb[] WHERE bdmng EQ 0.

  IF lt_resb[] IS INITIAL.
    MESSAGE e006.
  ENDIF.

  LOOP AT lt_resb[] ASSIGNING <fs_resb>.
    INSERT <fs_resb> INTO TABLE lt_resb_matnr[].
  ENDLOOP.

  PERFORM f_data_query_get_matnr CHANGING lt_resb_matnr[].

  IF lt_resb_matnr[] IS INITIAL.
    MESSAGE e006.
  ENDIF.

  LOOP AT lt_resb_matnr[] ASSIGNING <fs_resb>.
    INSERT <fs_resb> INTO TABLE gt_resb[].
  ENDLOOP.
ENDFORM.                    " F_DATA_QUERY_BY_RESB
*&---------------------------------------------------------------------*
*&      Form  F_DATA_QUERY_GET_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--CT_RESB  text
*----------------------------------------------------------------------*
FORM f_data_query_get_matnr CHANGING ct_resb TYPE ty_st_resb_matnr.
  DATA lt_matnr TYPE HASHED TABLE OF mlgn-matnr WITH UNIQUE KEY table_line.

  DATA lr_mtart TYPE RANGE OF mara-mtart.

  FIELD-SYMBOLS <fs_mtart>  LIKE LINE OF lr_mtart[].
  FIELD-SYMBOLS <fs_mara>   LIKE LINE OF gt_mara[].
  FIELD-SYMBOLS <fs_hardc>  LIKE LINE OF gt_hardcodes[].

  LOOP AT gt_hardcodes[] ASSIGNING <fs_hardc> WHERE occurrence EQ c_occur_mtart.
    APPEND INITIAL LINE TO lr_mtart[] ASSIGNING <fs_mtart>.
    <fs_mtart>-sign   = <fs_hardc>-ue_sign.
    <fs_mtart>-option = <fs_hardc>-ue_option.
    <fs_mtart>-low    = <fs_hardc>-ue_low.
    <fs_mtart>-high   = <fs_hardc>-ue_high.
  ENDLOOP.

  SELECT matnr mtart extwg
    FROM mara INTO TABLE gt_mara[]
    FOR ALL ENTRIES IN ct_resb[]
    WHERE matnr EQ ct_resb-matnr
      AND mtart IN lr_mtart[].
  IF sy-subrc NE 0.
    MESSAGE e007.
  ENDIF.

  SELECT matnr
    FROM mlgn INTO TABLE lt_matnr[]
    FOR ALL ENTRIES IN gt_mara[]
    WHERE matnr EQ gt_mara-matnr
      AND lgnum EQ gs_xuser-lgnum.

  LOOP AT gt_mara[] ASSIGNING <fs_mara>.
    READ TABLE lt_matnr[] TRANSPORTING NO FIELDS
      WITH TABLE KEY table_line = <fs_mara>-matnr.
    CHECK sy-subrc NE 0.

    DELETE ct_resb[] WHERE matnr = <fs_mara>-matnr.

    DELETE TABLE gt_mara[]
      WITH TABLE KEY matnr = <fs_mara>-matnr.
  ENDLOOP.

  IF gt_mara[] IS INITIAL.
    MESSAGE e008 WITH sy-uname gs_xuser-lgnum.
  ENDIF.

  PERFORM f_data_query_get_lqua USING gt_mara[].
ENDFORM.                    " F_DATA_QUERY_GET_MATNR
*&---------------------------------------------------------------------*
*&      Form  f_data_query_get_lqua
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_MARA    text
*----------------------------------------------------------------------*
FORM f_data_query_get_lqua USING it_mara TYPE ty_ht_mara.
  DATA ls_lqua_collect  TYPE ty_lqua_collect.

  DATA lr_lgtyp TYPE RANGE OF lqua-lgtyp.

  FIELD-SYMBOLS <fs_lgtyp>      LIKE LINE OF lr_lgtyp[].
  FIELD-SYMBOLS <fs_lqua>       LIKE LINE OF gt_lqua[].
  FIELD-SYMBOLS <fs_hardc>      LIKE LINE OF gt_hardcodes[].

  " Exclude all deposit types started with 9*
  LOOP AT gt_hardcodes[] ASSIGNING <fs_hardc> WHERE occurrence EQ c_occur_lgtyp.
    APPEND INITIAL LINE TO lr_lgtyp[] ASSIGNING <fs_lgtyp>.
    <fs_lgtyp>-sign   = <fs_hardc>-ue_sign.
    <fs_lgtyp>-option = <fs_hardc>-ue_option.
    <fs_lgtyp>-low    = <fs_hardc>-ue_low.
    <fs_lgtyp>-high   = <fs_hardc>-ue_high.
  ENDLOOP.

  FREE gt_lqua[].

  SELECT lgnum lqnum matnr werks charg bestq sobkz sonum lgtyp
         lgpla skzue skzua skzsi wdatu meins gesme verme lenum lgort
    FROM lqua INTO TABLE gt_lqua[]
    FOR ALL ENTRIES IN it_mara[]
    WHERE matnr EQ it_mara-matnr
      AND lgnum EQ gs_xuser-lgnum
      AND lgtyp IN lr_lgtyp[].  " Partial index LQUA~M

  " Delete all entries where stock status is not initial
  SORT gt_lqua[] BY bestq.
  DELETE gt_lqua[] WHERE bestq IS NOT INITIAL.
  " Delete all entries where available stock is initial
  SORT gt_lqua[] BY verme.
  DELETE gt_lqua[] WHERE verme IS INITIAL.

  " Delete all entries that have blocked positions
  LOOP AT gt_lqua[] ASSIGNING <fs_lqua>.
    CHECK <fs_lqua>-skzue IS INITIAL AND <fs_lqua>-skzua IS INITIAL AND <fs_lqua>-skzsi IS INITIAL.

    ls_lqua_collect-matnr = <fs_lqua>-matnr.
    ls_lqua_collect-verme = <fs_lqua>-verme.
    COLLECT ls_lqua_collect INTO gt_lqua_coll[].
  ENDLOOP.
ENDFORM.                    "f_data_query_get_lqua
*&---------------------------------------------------------------------*
*&      Form  f_data_query_by_afko
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_AFTERQUERY  text
*----------------------------------------------------------------------*
FORM f_data_query_by_afko USING i_afterquery TYPE abap_bool.
  DATA lt_aufk TYPE ty_ht_aufk.
  DATA lt_afko TYPE STANDARD TABLE OF ty_afko WITH KEY aufnr.
  DATA lt_afpo TYPE STANDARD TABLE OF ty_afpo WITH KEY aufnr.

  FIELD-SYMBOLS <fs_afko> LIKE LINE OF gt_afko[].
  FIELD-SYMBOLS <fs_afpo> LIKE LINE OF gt_afpo[].
  FIELD-SYMBOLS <fs_aufk> LIKE LINE OF gt_aufk[].

  IF i_afterquery EQ abap_false. " initial query by s_aufnr[] or s_werks[]
    IF s_aufnr[] IS INITIAL.
      SELECT aufnr auart autyp werks objnr
        FROM aufk INTO TABLE lt_aufk[]
        WHERE autyp EQ '10' " Production order category
          AND werks IN s_werks[]. " Index AUFK~B
    ELSE.
      SELECT aufnr auart autyp werks objnr
        FROM aufk INTO TABLE lt_aufk[]
        WHERE aufnr IN s_aufnr[] " primary key of AUFK
          AND werks IN s_werks[].
    ENDIF.
  ELSE. " use obtained orders for each RESB-AUFNR
    SELECT aufnr auart autyp werks objnr
      FROM aufk INTO TABLE lt_aufk[]
      FOR ALL ENTRIES IN gt_resb[]
      WHERE aufnr EQ gt_resb-aufnr. " primary key of AUFK
  ENDIF.

  IF lt_aufk[] IS INITIAL.
    MESSAGE e004.
  ENDIF.

  PERFORM f_data_query_filter_jest CHANGING lt_aufk[].

  IF lt_aufk[] IS INITIAL.
    MESSAGE e005.
  ENDIF.

  SELECT aufnr gstrp rsnum gamng gmein plnbez fevor igmng
    FROM afko INTO TABLE lt_afko[]
    FOR ALL ENTRIES IN lt_aufk[]
    WHERE aufnr EQ lt_aufk-aufnr.
  IF sy-subrc NE 0.
    MESSAGE e004.
  ENDIF.

  IF s_fevor[] IS NOT INITIAL.
    SORT lt_afko[] BY fevor.
    DELETE lt_afko[] WHERE fevor NOT IN s_fevor[].
  ENDIF.

  IF s_gstrp[] IS NOT INITIAL.
    SORT lt_afko[] BY gstrp.
    DELETE lt_afko[] WHERE gstrp NOT IN s_gstrp[].
  ENDIF.

  IF lt_afko[] IS INITIAL.
    MESSAGE e004.
  ENDIF.

  LOOP AT lt_afko[] ASSIGNING <fs_afko>.
    READ TABLE lt_aufk[] ASSIGNING <fs_aufk>
      WITH TABLE KEY aufnr = <fs_afko>-aufnr.
    CHECK sy-subrc EQ 0.

    INSERT <fs_afko> INTO TABLE gt_afko[].
    INSERT <fs_aufk> INTO TABLE gt_aufk[].
  ENDLOOP.

  IF gt_afko[] IS INITIAL OR gt_aufk[] IS INITIAL.
    MESSAGE e004.
  ENDIF.

  SELECT aufnr posnr wemng
    FROM afpo INTO TABLE lt_afpo[]
    FOR ALL ENTRIES IN gt_afko[]
    WHERE aufnr EQ gt_afko-aufnr.

  SORT lt_afpo[] BY aufnr.
  DELETE ADJACENT DUPLICATES FROM lt_afpo[] COMPARING aufnr.

  LOOP AT lt_afpo[] ASSIGNING <fs_afpo>.
    INSERT <fs_afpo> INTO TABLE gt_afpo[].
  ENDLOOP.
ENDFORM.                    " F_DATA_QUERY_BY_AFKO
*&---------------------------------------------------------------------*
*&      Form  F_DATA_QUERY_FILTER_JEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--CT_AUFK  text
*----------------------------------------------------------------------*
FORM f_data_query_filter_jest CHANGING ct_aufk TYPE ty_ht_aufk.
  DATA lv_hardc_stat TYPE jest-stat.

  DATA lt_jest TYPE ty_ht_jest.

  FIELD-SYMBOLS <fs_aufk>   LIKE LINE OF ct_aufk[].
  FIELD-SYMBOLS <fs_hardc>  LIKE LINE OF gt_hardcodes[].

  READ TABLE gt_hardcodes[] ASSIGNING <fs_hardc>
    WITH TABLE KEY occurrence = c_occur_stat
                   counter    = c_count_stat.
  IF sy-subrc EQ 0.
    lv_hardc_stat = <fs_hardc>-ue_low.
  ENDIF.

  SELECT objnr stat inact
    FROM jest INTO TABLE lt_jest[]
    FOR ALL ENTRIES IN ct_aufk[]
    WHERE objnr EQ ct_aufk-objnr
      AND stat EQ lv_hardc_stat
      AND inact EQ abap_false.

  LOOP AT ct_aufk[] ASSIGNING <fs_aufk>.
    READ TABLE lt_jest[] TRANSPORTING NO FIELDS
      WITH TABLE KEY objnr = <fs_aufk>-objnr
                     stat  = lv_hardc_stat.
    CHECK sy-subrc NE 0.  " if entry not found then inact was set or entry doesn't exist
    DELETE TABLE ct_aufk[]
      WITH TABLE KEY aufnr = <fs_aufk>-aufnr.
  ENDLOOP.
ENDFORM.                    " F_DATA_QUERY_FILTER_JEST
*&---------------------------------------------------------------------*
*&      Form  F_DATA_QUERY_GET_DESCRIPTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_query_get_descriptions.
  DATA lt_makt TYPE STANDARD TABLE OF ty_makt WITH KEY matnr.

  FIELD-SYMBOLS <fs_makt> LIKE LINE OF gt_makt[].

  SELECT matnr maktx
    FROM makt INTO TABLE lt_makt[]
    FOR ALL ENTRIES IN gt_mara[]
    WHERE matnr EQ gt_mara-matnr
      AND spras EQ sy-langu.

  SELECT matnr maktx
    FROM makt APPENDING TABLE lt_makt[]
    FOR ALL ENTRIES IN gt_afko[]
    WHERE matnr EQ gt_afko-plnbez
      AND spras EQ sy-langu.

  SORT lt_makt[] BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_makt[] COMPARING matnr.

  LOOP AT lt_makt[] ASSIGNING <fs_makt>.
    INSERT <fs_makt> INTO TABLE gt_makt[].
  ENDLOOP.

  SELECT fevor txt
    FROM t024f INTO TABLE gt_t024f[]
    FOR ALL ENTRIES IN gt_afko[]
    WHERE werks IN s_werks[]
      AND fevor EQ gt_afko-fevor.                     "#EC CI_SGLSELECT
ENDFORM.                    " F_DATA_QUERY_GET_DESCRIPTIONS
*&---------------------------------------------------------------------*
*&      Form  F_DATA_QUERY_GET_KOSTL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_query_get_kostl.
  DATA lt_kostl TYPE STANDARD TABLE OF ty_kostl.

  FIELD-SYMBOLS <fs_kostl> LIKE LINE OF gt_kostl[].

  SELECT extwg kostl fevor
    FROM zwmmpt008 INTO TABLE lt_kostl[]
    FOR ALL ENTRIES IN gt_mara[]
    WHERE extwg EQ gt_mara-extwg.

  SORT lt_kostl[] BY extwg fevor.
  DELETE ADJACENT DUPLICATES FROM lt_kostl[] COMPARING extwg fevor.

  LOOP AT lt_kostl[] ASSIGNING <fs_kostl>.
    INSERT <fs_kostl> INTO TABLE gt_kostl[].
  ENDLOOP.
ENDFORM.                    " F_DATA_QUERY_GET_KOSTL
*&---------------------------------------------------------------------*
*&      Form  F_DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_prepare.
  FIELD-SYMBOLS <fs_resb>     LIKE LINE OF gt_resb[].
  FIELD-SYMBOLS <fs_afko>     LIKE LINE OF gt_afko[].
  FIELD-SYMBOLS <fs_afpo>     LIKE LINE OF gt_afpo[].
  FIELD-SYMBOLS <fs_mara>     LIKE LINE OF gt_mara[].
  FIELD-SYMBOLS <fs_makt>     LIKE LINE OF gt_makt[].
  FIELD-SYMBOLS <fs_maktfg>   LIKE LINE OF gt_makt[].
  FIELD-SYMBOLS <fs_t024f>    LIKE LINE OF gt_t024f[].
  FIELD-SYMBOLS <fs_output>   LIKE LINE OF gt_output_tmp[].
  FIELD-SYMBOLS <fs_dupview>  LIKE LINE OF gt_dupview[].

  DATA ls_lqua_c  TYPE ty_lqua_collect.
  DATA ls_kostl   TYPE ty_kostl.
  DATA ls_output  TYPE zwmfr_sprodplan_output.
  DATA ls_dupview TYPE ty_duplicates.

  LOOP AT gt_resb[] ASSIGNING <fs_resb>.
    CLEAR ls_output.
    CLEAR ls_lqua_c.

    READ TABLE gt_lqua_coll[] INTO ls_lqua_c
      WITH TABLE KEY matnr = <fs_resb>-matnr.

    READ TABLE gt_mara[] ASSIGNING <fs_mara>
      WITH TABLE KEY matnr = <fs_resb>-matnr.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_makt[] ASSIGNING <fs_makt>
      WITH TABLE KEY matnr = <fs_resb>-matnr.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_afko[] ASSIGNING <fs_afko>
      WITH TABLE KEY aufnr = <fs_resb>-aufnr.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_afpo[] ASSIGNING <fs_afpo>
      WITH TABLE KEY aufnr = <fs_resb>-aufnr.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_makt[] ASSIGNING <fs_maktfg>
      WITH TABLE KEY matnr = <fs_afko>-plnbez.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_t024f[] ASSIGNING <fs_t024f>
      WITH TABLE KEY fevor = <fs_afko>-fevor.
    CHECK sy-subrc EQ 0.

    CLEAR ls_kostl.
    READ TABLE gt_kostl[] INTO ls_kostl
      WITH TABLE KEY extwg = <fs_mara>-extwg
                     fevor = <fs_afko>-fevor.

    IF ls_kostl-kostl IS INITIAL. " if doesn't have cost center
      READ TABLE gt_output_tmp[] ASSIGNING <fs_output>
        WITH TABLE KEY fevor  = <fs_afko>-fevor
                       kostl  = ls_kostl-kostl
                       gstrp  = <fs_afko>-gstrp
                       aufnr  = <fs_afko>-aufnr
                       plnbez = <fs_afko>-plnbez
                       matnr  = <fs_resb>-matnr.
    ELSE. " if a cost center is assigned, perform a collect of values to aggregate at this level
      READ TABLE gt_output_tmp[] ASSIGNING <fs_output>
        WITH TABLE KEY fevor  = <fs_afko>-fevor
                       kostl  = ls_kostl-kostl
                       gstrp  = '00000000'
                       aufnr  = space
                       plnbez = space
                       matnr  = <fs_resb>-matnr.
    ENDIF.

    IF sy-subrc NE 0. " entry doesn't exists yet
      ls_output-fevor   = <fs_afko>-fevor.
      ls_output-kostl   = ls_kostl-kostl.

      IF ls_kostl-kostl IS INITIAL.
        ls_output-gstrp   = <fs_afko>-gstrp.
        ls_output-aufnr   = <fs_afko>-aufnr.
        ls_output-plnbez  = <fs_afko>-plnbez.
      ENDIF.

      ls_output-matnr   = <fs_resb>-matnr.
      ls_output-tfevo   = <fs_t024f>-txt.
      ls_output-tplnbe  = <fs_maktfg>-maktx.

      IF <fs_mara>-extwg EQ c_extwg_hybrid.
        CONCATENATE <fs_makt>-maktx '***' INTO ls_output-tmatn SEPARATED BY space.
      ELSE.
        ls_output-tmatn  = <fs_makt>-maktx.
      ENDIF.

      ls_output-gamng   = <fs_afko>-gamng.
      ls_output-igmng   = <fs_afpo>-wemng.
      ls_output-dimng   = <fs_afko>-gamng - <fs_afpo>-wemng.
      ls_output-gmein   = <fs_afko>-gmein.
      ls_output-verme   = ls_lqua_c-verme.

      ls_output-bdmng   = <fs_resb>-bdmng.
      ls_output-efmng   = <fs_resb>-enmng.
      ls_output-remng   = <fs_resb>-bdmng - <fs_resb>-enmng.
      ls_output-meins   = <fs_resb>-meins.

      INSERT ls_output INTO TABLE gt_output_tmp[].
    ELSE.
      <fs_output>-bdmng   = <fs_output>-bdmng + <fs_resb>-bdmng.
      <fs_output>-efmng   = <fs_output>-efmng + <fs_resb>-enmng.
      <fs_output>-remng   = <fs_output>-bdmng - <fs_output>-efmng.

      <fs_output>-gamng   = <fs_output>-gamng + <fs_afko>-gamng.
      <fs_output>-igmng   = <fs_output>-igmng + <fs_afpo>-wemng.
      <fs_output>-dimng   = <fs_output>-gamng - <fs_output>-igmng.
    ENDIF.

    READ TABLE gt_dupview[] ASSIGNING <fs_dupview>
      WITH TABLE KEY matnr = <fs_resb>-matnr.
    IF sy-subrc NE 0.
      CLEAR ls_dupview.

      ls_dupview-matnr  = <fs_resb>-matnr.
      ls_dupview-maktx  = <fs_makt>-maktx.
      ls_dupview-dbcnt  = 1.
      ls_dupview-bdmng  = <fs_resb>-bdmng.
      ls_dupview-enmng  = <fs_resb>-enmng.
      ls_dupview-meins  = <fs_resb>-meins.

      INSERT ls_dupview INTO TABLE gt_dupview[].
    ELSE.
      <fs_dupview>-dbcnt  = <fs_dupview>-dbcnt + 1.
      <fs_dupview>-bdmng  = <fs_dupview>-bdmng + <fs_resb>-bdmng.
      <fs_dupview>-enmng  = <fs_dupview>-enmng + <fs_resb>-enmng.
    ENDIF.
  ENDLOOP.

  FREE gt_afko[].
  FREE gt_afpo[].
  FREE gt_aufk[].
  FREE gt_resb[].
  FREE gt_mara[].
  FREE gt_makt[].
  FREE gt_t024f[].
  FREE gt_kostl[].
ENDFORM.                    " F_DATA_PREPARE
*&---------------------------------------------------------------------*
*&      Form  F_DATA_CHECK_PENDING_TO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_DATA     text
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_data_check_pending_to USING i_data TYPE zwmfr_sprodplan_output
                             CHANGING c_subrc TYPE sysubrc.
  DATA lt_ltak    TYPE ty_ht_ltak.
  DATA lt_ltap    TYPE ty_st_ltap_matnr.

  DATA lr_queues  TYPE RANGE OF ltak-queue.

  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].
  FIELD-SYMBOLS <fs_queue>      LIKE LINE OF lr_queues[].

  LOOP AT gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001> WHERE processo EQ c_process_system_guide
                                                    AND parametro EQ c_param_queue_lin.
    APPEND INITIAL LINE TO lr_queues[] ASSIGNING <fs_queue>.
    <fs_queue>-sign     = c_rsig_i.
    <fs_queue>-option   = c_ropt_eq.
    <fs_queue>-low      = <fs_zwmmpt001>-valor.
  ENDLOOP.

  SELECT lgnum tanum kquit lznum queue
    FROM ltak INTO TABLE lt_ltak[]
    WHERE lgnum EQ gs_xuser-lgnum
      AND kquit EQ space
      AND queue IN lr_queues[].
  IF sy-subrc NE 0.
    c_subrc = 0. " no pending TOs, can create
    RETURN.
  ENDIF.

  SELECT lgnum tanum tapos matnr meins altme pquit
         qname vltyp vlpla vsolm vsola pvqui
    FROM ltap INTO TABLE lt_ltap[]
    FOR ALL ENTRIES IN lt_ltak[]
    WHERE lgnum EQ lt_ltak-lgnum
      AND tanum EQ lt_ltak-tanum.
  IF sy-subrc NE 0.
    c_subrc = 0. " no pending TOs, can create
    RETURN.
  ENDIF.

  READ TABLE lt_ltap[] TRANSPORTING NO FIELDS
    WITH TABLE KEY matnr = i_data-matnr.
  IF sy-subrc EQ 0.
    c_subrc = 4. " pending TOs, can't create
    RETURN.
  ELSE.
    c_subrc = 0. " no pending TOs, can create
    RETURN.
  ENDIF.
ENDFORM.                    " F_DATA_CHECK_PENDING_TO
*&---------------------------------------------------------------------*
*&      Form  F_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_display.
  DATA lv_message TYPE string.

  DATA lt_data_tree TYPE STANDARD TABLE OF zwmfr_sprodplan_output.

  DATA lo_salv_msg TYPE REF TO cx_salv_error.

  IF gt_output_tmp[] IS INITIAL.
    MESSAGE s009 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  IF go_tree IS NOT BOUND.
    TRY.
        cl_salv_tree=>factory( IMPORTING r_salv_tree = go_tree
                               CHANGING  t_table     = lt_data_tree[] ).
      CATCH cx_salv_error INTO lo_salv_msg.
        lv_message = lo_salv_msg->get_text( ).
        MESSAGE lv_message TYPE c_msgty_s DISPLAY LIKE c_msgty_e.
        RETURN.
    ENDTRY.

    PERFORM f_data_set_tree.
    PERFORM f_data_set_layout.
    PERFORM f_data_set_toolbar.
    PERFORM f_data_set_columns.
    PERFORM f_data_set_top_of_page.
    PERFORM f_data_set_handler.

    go_tree->display( ).
  ELSE.
    PERFORM f_data_set_tree.
  ENDIF.
ENDFORM.                    " F_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SET_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_set_tree.
  DATA lv_key_fevor   TYPE salv_de_node_key.
  DATA lv_key_gstrp   TYPE salv_de_node_key.
  DATA lv_key_kostl   TYPE salv_de_node_key.
  DATA lv_key_plnbez  TYPE salv_de_node_key.
  DATA lv_key         TYPE salv_de_node_key.
  DATA lv_text        TYPE lvc_value.
  DATA lv_text_aufnr  TYPE lvc_value.
  DATA lv_text_plnbez TYPE lvc_value.
  DATA lv_level       TYPE i.

  DATA lo_nodes       TYPE REF TO cl_salv_nodes.
  DATA lo_node        TYPE REF TO cl_salv_node.
  DATA lo_data        TYPE REF TO data.

  DATA ls_output  TYPE zwmfr_sprodplan_output.

  DATA lt_nodes         TYPE salv_t_nodes.
  DATA lt_nodes_child   TYPE salv_t_nodes.
  DATA lt_output        TYPE STANDARD TABLE OF zwmfr_sprodplan_output.

  FIELD-SYMBOLS <fs_output>     LIKE LINE OF gt_output_tmp[].
  FIELD-SYMBOLS <fs_node>       LIKE LINE OF lt_nodes[].
  FIELD-SYMBOLS <fs_node_c>     LIKE LINE OF lt_nodes_child[].

  lo_nodes = go_tree->get_nodes( ).

  lt_output[] = gt_output_tmp[].
  SORT lt_output[] BY fevor ASCENDING kostl gstrp ASCENDING aufnr DESCENDING plnbez matnr.

  TRY.
      lo_nodes->delete_all( ).

      LOOP AT lt_output[] ASSIGNING <fs_output>.
        AT NEW fevor.
          CLEAR lv_key_fevor.
          CLEAR lv_text.
          CLEAR ls_output.

          ls_output-nodty = c_nodty_fevor.
          ls_output-fevor = <fs_output>-fevor.

          CONCATENATE <fs_output>-fevor <fs_output>-tfevo INTO lv_text SEPARATED BY space.

          lo_node = lo_nodes->add_node( related_node    = space
                                        relationship    = if_salv_c_node_relation=>parent
                                        collapsed_icon  = c_icon_fevor
                                        expanded_icon   = c_icon_fevor ).
          lo_node->set_text( lv_text ).
          lo_node->set_data_row( ls_output ).

          lv_key_fevor = lo_node->get_key( ).
        ENDAT.

        AT NEW kostl.
          IF <fs_output>-kostl IS NOT INITIAL.
            CLEAR lv_key_kostl.
            CLEAR lv_key.
            CLEAR ls_output.
            CLEAR lv_text.

            ls_output-nodty   = c_nodty_kostl.
            ls_output-fevor   = <fs_output>-fevor.
            ls_output-kostl   = <fs_output>-kostl.

            CONCATENATE text-004 <fs_output>-kostl INTO lv_text SEPARATED BY space.


            lo_node = lo_nodes->add_node( related_node    = lv_key_fevor
                                          relationship    = if_salv_c_node_relation=>first_child
                                          collapsed_icon  = c_icon_kostl
                                          expanded_icon   = c_icon_kostl ).
            lo_node->set_text( lv_text ).
            lo_node->set_data_row( ls_output ).

            lv_key_kostl = lo_node->get_key( ).
            lv_key       = lv_key_kostl.
          ELSE.
            CLEAR lv_key_kostl.
          ENDIF.
        ENDAT.

        AT NEW gstrp.
          IF lv_key_kostl IS INITIAL.
            CLEAR lv_key_gstrp.
            CLEAR lv_text.
            CLEAR ls_output.

            ls_output-nodty = c_nodty_gstrp.
            ls_output-fevor = <fs_output>-fevor.
            ls_output-kostl = <fs_output>-kostl.
            ls_output-gstrp = <fs_output>-gstrp.

            CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
              EXPORTING
                input  = <fs_output>-gstrp
              IMPORTING
                output = lv_text.

            lo_node = lo_nodes->add_node( related_node    = lv_key_fevor
                                          relationship    = if_salv_c_node_relation=>first_child
                                          collapsed_icon  = c_icon_gstrp
                                          expanded_icon   = c_icon_gstrp ).
            lo_node->set_text( lv_text ).
            lo_node->set_data_row( ls_output ).

            lv_key_gstrp = lo_node->get_key( ).
          ENDIF.
        ENDAT.

        AT NEW plnbez.
          IF lv_key_kostl IS INITIAL.
            CLEAR lv_key_plnbez.
            CLEAR lv_key.
            CLEAR lv_text.
            CLEAR ls_output.

            ls_output-nodty   = c_nodty_aufnr.
            ls_output-fevor   = <fs_output>-fevor.
            ls_output-kostl   = <fs_output>-kostl.
            ls_output-gstrp   = <fs_output>-gstrp.
            ls_output-aufnr   = <fs_output>-aufnr.
            ls_output-plnbez  = <fs_output>-plnbez.
            ls_output-gamng   = <fs_output>-gamng.
            ls_output-igmng   = <fs_output>-igmng.
            ls_output-dimng   = <fs_output>-dimng.
            ls_output-gmein   = <fs_output>-gmein.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = <fs_output>-aufnr
              IMPORTING
                output = lv_text_aufnr.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = <fs_output>-plnbez
              IMPORTING
                output = lv_text_plnbez.

            CONCATENATE lv_text_plnbez <fs_output>-tplnbe INTO lv_text SEPARATED BY space.
            CONCATENATE '(' lv_text ')' INTO lv_text SEPARATED BY space.
            CONCATENATE lv_text_aufnr lv_text INTO lv_text SEPARATED BY space.

            lo_node = lo_nodes->add_node( related_node    = lv_key_gstrp
                                          relationship    = if_salv_c_node_relation=>first_child
                                          collapsed_icon  = c_icon_aufnr
                                          expanded_icon   = c_icon_aufnr ).
            lo_node->set_text( lv_text ).
            lo_node->set_data_row( ls_output ).

            lv_key_plnbez = lo_node->get_key( ).
            lv_key        = lv_key_plnbez.
          ENDIF.
        ENDAT.

        CLEAR lv_text.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_output>-matnr
          IMPORTING
            output = lv_text.

        CONCATENATE lv_text <fs_output>-tmatn INTO lv_text SEPARATED BY space.

        ls_output = <fs_output>.

        ls_output-nodty = c_nodty_matnr.

        lo_node = lo_nodes->add_node( related_node    = lv_key
                                      relationship    = if_salv_c_node_relation=>first_child
                                      collapsed_icon  = c_icon_matnr
                                      expanded_icon   = c_icon_matnr ).

        ls_output-nodek = lo_node->get_key( ).

        INSERT ls_output INTO TABLE gt_output[].

        ls_output-gamng = 0.
        ls_output-igmng = 0.
        ls_output-dimng = 0.
        ls_output-gmein = space.

        lo_node->set_text( lv_text ).
        lo_node->set_data_row( ls_output ).
      ENDLOOP.

      lo_node    = lo_nodes->get_node( cl_salv_node=>c_virtual_root_node  ).
      lt_nodes[] = lo_node->get_children( ).
      LOOP AT lt_nodes[] ASSIGNING <fs_node>.
        <fs_node>-node->expand( level = 1 ).

        FREE lt_nodes_child[].
        lt_nodes_child[] = <fs_node>-node->get_children( ).
        LOOP AT lt_nodes_child[] ASSIGNING <fs_node_c>.
          lo_data = <fs_node_c>-node->get_data_row( ).
          ASSIGN lo_data->* TO <fs_output>.
          CHECK sy-subrc EQ 0.

          IF <fs_output>-nodty EQ c_nodty_kostl.
            lv_level   = 2.
          ELSE.
            lv_level   = 1.
          ENDIF.

          <fs_node_c>-node->expand( level = lv_level ).
        ENDLOOP.
      ENDLOOP.
    CATCH cx_salv_msg cx_salv_error.
      RETURN.
  ENDTRY.
ENDFORM.                    " F_DATA_SET_TREE
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_set_layout.
  DATA lv_tree_title TYPE salv_de_tree_text.

  DATA ls_key TYPE salv_s_layout_key.

  DATA lo_tree_settings TYPE REF TO cl_salv_tree_settings.
  DATA lo_layout        TYPE REF TO cl_salv_layout.

  lv_tree_title = sy-title.

  ls_key-report = sy-repid.

  lo_tree_settings = go_tree->get_tree_settings( ).
  lo_tree_settings->set_hierarchy_header( text-003 ).
  lo_tree_settings->set_hierarchy_tooltip( text-003 ).
  lo_tree_settings->set_hierarchy_size( 100 ).
  lo_tree_settings->set_header( lv_tree_title ).

  lo_layout = go_tree->get_layout( ).
  lo_layout->set_initial_layout( p_layo ).
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_default( abap_true ).
ENDFORM.                    " F_DATA_SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SET_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_set_toolbar.
  DATA lo_functions TYPE REF TO cl_salv_functions.

  go_tree->set_screen_status( report    = sy-repid
                              pfstatus  = c_pfstatus ).

  lo_functions = go_tree->get_functions( ).
  lo_functions->set_all( abap_true ).
ENDFORM.                    " F_DATA_SET_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SET_COLUMNS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_set_columns.
  DATA lt_fcat    TYPE lvc_t_fcat.

  DATA lo_columns       TYPE REF TO cl_salv_columns_tree.
  DATA lo_column        TYPE REF TO cl_salv_column_tree.
  DATA lo_aggregations  TYPE REF TO cl_salv_aggregations.

  FIELD-SYMBOLS <fs_fcat> LIKE LINE OF lt_fcat[].

  lo_columns = go_tree->get_columns( ).
  lo_columns->set_optimize( abap_true ).

  lo_aggregations = go_tree->get_aggregations( ).
  lo_aggregations->clear( ).

  lt_fcat[] = cl_salv_controller_metadata=>get_tree_fieldcatalog( r_columns      = lo_columns
                                                                  r_aggregations = lo_aggregations ).

  LOOP AT lt_fcat[] ASSIGNING <fs_fcat>.
    TRY.
        lo_column ?= lo_columns->get_column( <fs_fcat>-fieldname ).

        lo_column->set_optimized( abap_true ).
        IF <fs_fcat>-fieldname NE c_fname_meins AND <fs_fcat>-fieldname NE c_fname_gmein.
          lo_column->set_short_text( space ).
          lo_column->set_medium_text( space ).
        ENDIF.

        CASE <fs_fcat>-fieldname.
          WHEN c_fname_fevor OR c_fname_gstrp OR c_fname_aufnr OR c_fname_kostl OR c_fname_plnbez OR
               c_fname_matnr OR c_fname_tfevo OR c_fname_tplnbe OR c_fname_tmatn OR c_fname_nodek OR
               c_fname_nodty.
            lo_column->set_technical( abap_true ).
          WHEN c_fname_gamng OR c_fname_igmng OR c_fname_dimng OR c_fname_bdmng OR c_fname_efmng OR
               c_fname_remng.
            lo_column->set_zero( abap_true ).
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

      CATCH cx_salv_not_found.
        CONTINUE.
    ENDTRY.
  ENDLOOP.
ENDFORM.                    " F_DATA_SET_COLUMNS
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SET_HANDLER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_set_handler.
  DATA lo_events_table TYPE REF TO cl_salv_events_tree.

  lo_events_table = go_tree->get_event( ).

  SET HANDLER lcl_mc_events=>hndl_user_command FOR lo_events_table.
ENDFORM.                    " F_DATA_SET_HANDLER
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_set_top_of_page.
  DATA lv_text TYPE string.

  DATA lo_content TYPE REF TO cl_salv_form_header_info.

  lv_text = sy-title.

  CREATE OBJECT lo_content
    EXPORTING
      text    = lv_text
      tooltip = lv_text.

  go_tree->set_top_of_list( lo_content ).
ENDFORM.                    " F_DATA_SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_DATA_UCOMM_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_ucomm_refresh.
  PERFORM f_data_free USING abap_true.
  PERFORM f_data_init USING abap_true.
  PERFORM f_data_query.
  PERFORM f_data_prepare.
  PERFORM f_data_display.
ENDFORM.                    " F_DATA_UCOMM_REFRESH
*&---------------------------------------------------------------------*
*&      Form  F_DATA_UCOMM_TOCREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_ucomm_tocreate.
  DATA lv_key   TYPE salv_de_node_key.
  DATA lv_subrc TYPE sysubrc.

  DATA lt_nodes   TYPE salv_t_nodes.

  DATA lo_selections  TYPE REF TO cl_salv_selections_tree.

  FIELD-SYMBOLS <fs_node>       LIKE LINE OF lt_nodes[].
  FIELD-SYMBOLS <fs_output>     LIKE LINE OF gt_output[].

  lo_selections = go_tree->get_selections( ).

  lt_nodes[] = lo_selections->get_selected_nodes( ).
  IF LINES( lt_nodes[] ) NE 1.
    MESSAGE s010 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  READ TABLE lt_nodes[] ASSIGNING <fs_node> INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s010 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  lv_key = <fs_node>-node->get_key( ).

  READ TABLE gt_output[] ASSIGNING <fs_output>
    WITH TABLE KEY nodek = lv_key.
  IF sy-subrc NE 0.
    MESSAGE s016 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  IF <fs_output>-nodty NE c_nodty_matnr.
    MESSAGE s016 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

*  Comentado para não fazer validação quando arrancou a linha H10
*  PERFORM f_data_check_pending_to USING <fs_output> CHANGING lv_subrc.
*  IF lv_subrc NE 0.
*    MESSAGE s017 DISPLAY LIKE c_msgty_e.
*    RETURN.
*  ENDIF.

  PERFORM f_data_ucomm_tocreate_popup USING <fs_output>.
ENDFORM.                    " F_DATA_UCOMM_TOCREATE
*&---------------------------------------------------------------------*
*&      Form  f_data_ucomm_tocreate_popup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_DATA     text
*----------------------------------------------------------------------*
FORM f_data_ucomm_tocreate_popup USING i_data TYPE zwmfr_sprodplan_output.
  DATA lv_lgtyp TYPE ltap-vltyp.

  DATA lt_ltap  TYPE ty_st_ltap_matnr.

  FIELD-SYMBOLS <fs_lqua>       LIKE LINE OF gt_lqua[].
  FIELD-SYMBOLS <fs_ltap>       LIKE LINE OF lt_ltap[].
  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].

  FREE gt_lqua[].

  CLEAR gs_topopup.

  PERFORM f_data_ucomm_tocreate_get_lqua USING i_data.

  IF gt_lqua[] IS INITIAL.
    MESSAGE s020 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_lgtyp_bulk
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_lgtyp  = <fs_zwmmpt001>-valor.
  ENDIF.

  LOOP AT gt_lqua[] ASSIGNING <fs_lqua>.
    IF gs_topopup-meins IS INITIAL.
      gs_topopup-meins  = <fs_lqua>-meins.
    ENDIF.

    gs_topopup-verme    = gs_topopup-verme + <fs_lqua>-verme.
    gs_topopup-npall_wm = gs_topopup-npall_wm + 1.
  ENDLOOP.

  " Consider BLK deposit type to subtract quantities
  SELECT lgnum tanum tapos matnr meins altme pquit
         qname vltyp vlpla vsolm vsola pvqui
    FROM ltap INTO TABLE lt_ltap[]
    WHERE lgnum EQ gs_xuser-lgnum
      AND pquit EQ space
      AND vltyp EQ lv_lgtyp. " Partial Index LTAP~V

  LOOP AT lt_ltap[] ASSIGNING <fs_ltap> WHERE matnr EQ i_data-matnr.
    gs_topopup-npall_wm = gs_topopup-npall_wm - 1.
    gs_topopup-verme    = gs_topopup-verme - <fs_ltap>-vsolm.
  ENDLOOP.

  gs_topopup-fevor        = i_data-fevor.
  gs_topopup-aufnr        = i_data-aufnr.
  gs_topopup-kostl        = i_data-kostl.
  gs_topopup-matnr        = i_data-matnr.
  gs_topopup-maktx        = i_data-tmatn.
  gs_topopup-tpall_wm     = text-006.
  gs_topopup-tpall_scr    = text-006.
  gs_topopup-npall_scr    = 0.
  gs_topopup-verme_scr    = 0.
  gs_topopup-verme_final  = 0.
  gs_topopup-meins_scr    = gs_topopup-meins.
  gs_topopup-title001     = text-005.
  gs_topopup-title002     = text-007.

  CALL SCREEN 0100 STARTING AT 3 3.
ENDFORM.                    "f_data_ucomm_tocreate_popup
*&---------------------------------------------------------------------*
*&      Form  f_data_ucomm_tocreate_get_lqua
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_DATA    text
*----------------------------------------------------------------------*
FORM f_data_ucomm_tocreate_get_lqua USING is_data TYPE zwmfr_sprodplan_output.
  DATA lv_tabix   TYPE sytabix.

  DATA lt_mchb  TYPE ty_ht_mchb.

  DATA lr_lgtyp TYPE RANGE OF lqua-lgtyp.

  FIELD-SYMBOLS <fs_lgtyp>      LIKE LINE OF lr_lgtyp[].
  FIELD-SYMBOLS <fs_hardc>      LIKE LINE OF gt_hardcodes[].
  FIELD-SYMBOLS <fs_lqua>       LIKE LINE OF gt_lqua[].
  FIELD-SYMBOLS <fs_mchb>       LIKE LINE OF lt_mchb[].

  FREE gt_lqua[].

  " Exclude all deposit types started with 9*
  LOOP AT gt_hardcodes[] ASSIGNING <fs_hardc> WHERE occurrence EQ c_occur_lgtyp.
    APPEND INITIAL LINE TO lr_lgtyp[] ASSIGNING <fs_lgtyp>.
    <fs_lgtyp>-sign   = <fs_hardc>-ue_sign.
    <fs_lgtyp>-option = <fs_hardc>-ue_option.
    <fs_lgtyp>-low    = <fs_hardc>-ue_low.
    <fs_lgtyp>-high   = <fs_hardc>-ue_high.
  ENDLOOP.

  SELECT lgnum lqnum matnr werks charg bestq sobkz sonum lgtyp
         lgpla skzue skzua skzsi wdatu meins gesme verme lenum lgort
    FROM lqua INTO TABLE gt_lqua[]
    WHERE matnr EQ is_data-matnr
      AND lgnum EQ gs_xuser-lgnum
      AND lgtyp IN lr_lgtyp[].  " Partial index LQUA~M

  " Delete all entries where stock status is not initial
  SORT gt_lqua[] BY bestq.
  DELETE gt_lqua[] WHERE bestq IS NOT INITIAL.
  " Delete all entries where available stock is initial
  SORT gt_lqua[] BY verme.
  DELETE gt_lqua[] WHERE verme IS INITIAL.

  " Delete all entries that have blocked positions
  LOOP AT gt_lqua[] ASSIGNING <fs_lqua>.
    lv_tabix = sy-tabix.

    CHECK <fs_lqua>-skzue IS NOT INITIAL OR <fs_lqua>-skzua IS NOT INITIAL OR <fs_lqua>-skzsi IS NOT INITIAL.

    DELETE gt_lqua[] INDEX lv_tabix.
  ENDLOOP.

  FREE lr_lgtyp[].

  " Exclude all deposit types started with 9*
  LOOP AT gt_hardcodes[] ASSIGNING <fs_hardc> WHERE occurrence EQ c_occur_lgtyp_cfo.
    APPEND INITIAL LINE TO lr_lgtyp[] ASSIGNING <fs_lgtyp>.
    <fs_lgtyp>-sign   = <fs_hardc>-ue_sign.
    <fs_lgtyp>-option = <fs_hardc>-ue_option.
    <fs_lgtyp>-low    = <fs_hardc>-ue_low.
    <fs_lgtyp>-high   = <fs_hardc>-ue_high.
  ENDLOOP.

  IF gt_lqua[] IS NOT INITIAL.
    SELECT matnr werks lgort charg lvorm ersda
      FROM mchb INTO TABLE lt_mchb[]
      FOR ALL ENTRIES IN gt_lqua[]
      WHERE matnr EQ gt_lqua-matnr
        AND werks EQ gt_lqua-werks
        AND lgort EQ gt_lqua-lgort
        AND charg EQ gt_lqua-charg.

    LOOP AT gt_lqua[] ASSIGNING <fs_lqua>.
      CHECK <fs_lqua>-lgtyp IN lr_lgtyp[].

      READ TABLE lt_mchb[] ASSIGNING <fs_mchb>
        WITH TABLE KEY matnr = <fs_lqua>-matnr
                       werks = <fs_lqua>-werks
                       lgort = <fs_lqua>-lgort
                       charg = <fs_lqua>-charg.
      CHECK sy-subrc EQ 0.
      <fs_lqua>-wdatu = <fs_mchb>-ersda.
    ENDLOOP.
  ENDIF.

  SORT gt_lqua[] BY lgtyp DESCENDING wdatu ASCENDING verme ASCENDING.
ENDFORM.                    "f_data_ucomm_tocreate_get_lqua
*&---------------------------------------------------------------------*
*&      Form  f_data_ucomm_tocreate_set_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_data_ucomm_tocreate_set_qty.
  DATA lv_curr_qtd TYPE lqua-verme.
  DATA lv_npall_c  TYPE i.

  FIELD-SYMBOLS <fs_lqua> LIKE LINE OF gt_lqua[].

  IF gs_topopup-verme_scr EQ 0.
    RETURN.
  ENDIF.

  READ TABLE gt_lqua ASSIGNING <fs_lqua> WITH KEY lgtyp = 'ABS'.
  IF sy-subrc = 0.
    SORT gt_lqua BY lgtyp ASCENDING
                    wdatu ASCENDING
                    verme ASCENDING.
  ENDIF.

  LOOP AT gt_lqua[] ASSIGNING <fs_lqua>.
    lv_curr_qtd     = lv_curr_qtd + <fs_lqua>-verme.
    lv_npall_c      = lv_npall_c + 1.
    <fs_lqua>-chckd = abap_true.    " mark this entry for TO use

    IF lv_curr_qtd GE gs_topopup-verme_scr.
      EXIT.
    ENDIF.
  ENDLOOP.

  gs_topopup-verme_final = lv_curr_qtd.
  gs_topopup-npall_scr   = lv_npall_c.

  IF gs_topopup-npall_scr EQ 0 OR gs_topopup-verme_final EQ 0.
    CLEAR gs_topopup-verme_final.
    CLEAR gs_topopup-npall_scr.
  ENDIF.
ENDFORM.                    "f_data_ucomm_tocreate_set_qty
*&---------------------------------------------------------------------*
*&      Form  f_data_ucomm_tocreate_select
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_data_ucomm_tocreate_select.
  DATA lv_tabix   TYPE sytabix.

  DATA lt_makt    TYPE ty_ht_makt.

  DATA lo_columns TYPE REF TO cl_salv_columns_table.
  DATA lo_column  TYPE REF TO cl_salv_column_table.
  DATA lo_events  TYPE REF TO cl_salv_events_table.

  FIELD-SYMBOLS <fs_lqua>   LIKE LINE OF gt_lqua[].
  FIELD-SYMBOLS <fs_makt>   LIKE LINE OF lt_makt[].
  FIELD-SYMBOLS <fs_popup>  LIKE LINE OF gt_popup_lqua[].

  FREE gt_popup_lqua[].

  IF go_popup_to IS BOUND.
    FREE go_popup_to.
  ENDIF.

  SELECT matnr maktx
    FROM makt INTO TABLE lt_makt[]
    FOR ALL ENTRIES IN gt_lqua[]
    WHERE matnr EQ gt_lqua-matnr
      AND spras EQ sy-langu.

  " Flush all marked entries
  LOOP AT gt_lqua[] ASSIGNING <fs_lqua>.
    <fs_lqua>-chckd = abap_false.

    READ TABLE lt_makt[] ASSIGNING <fs_makt>
      WITH TABLE KEY matnr = <fs_lqua>-matnr.
    CHECK sy-subrc EQ 0.

    APPEND INITIAL LINE TO gt_popup_lqua[] ASSIGNING <fs_popup>.
    <fs_popup>-chckd  = <fs_lqua>-chckd.
    <fs_popup>-fevor  = gs_topopup-fevor.
    <fs_popup>-kostl  = gs_topopup-kostl.
    <fs_popup>-aufnr  = gs_topopup-aufnr.
    <fs_popup>-matnr  = <fs_lqua>-matnr.
    <fs_popup>-lqnum  = <fs_lqua>-lqnum.
    <fs_popup>-maktx  = <fs_makt>-maktx.
    <fs_popup>-verme  = <fs_lqua>-verme.
    <fs_popup>-meins  = <fs_lqua>-meins.
    <fs_popup>-lgtyp  = <fs_lqua>-lgtyp.
    <fs_popup>-lgpla  = <fs_lqua>-lgpla.
    <fs_popup>-lenum  = <fs_lqua>-lenum.
    <fs_popup>-charg  = <fs_lqua>-charg.
    <fs_popup>-werks  = <fs_lqua>-werks.
    <fs_popup>-lgort  = <fs_lqua>-lgort.
    <fs_popup>-wdatu  = <fs_lqua>-wdatu.

    IF <fs_lqua>-lgtyp = 'ABS'.
      DATA : gt_color     TYPE lvc_t_scol,
             gs_color     TYPE lvc_s_scol.

      CLEAR : gt_color, gs_color.
      FREE gt_color.

*      detail.
      gs_color-color-col = 7.
      gs_color-color-int = 0.
      gs_color-color-inv = 0.
      APPEND gs_color TO gt_color.

      <fs_popup>-color = gt_color.

    ENDIF.
  ENDLOOP.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = go_popup_to
                              CHANGING t_table      = gt_popup_lqua[] ).
    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

  lo_columns = go_popup_to->get_columns( ).
  lo_columns->set_optimize( abap_true ).
  lo_columns->set_color_column( 'COLOR' ).

  go_popup_to->set_screen_status( report    = sy-repid
                                  pfstatus  = c_pfstatus_popup2 ).

  go_popup_to->set_screen_popup( start_column = 1
                                 end_column   = 120
                                 start_line   = 1
                                 end_line     = 25 ).

  TRY.
      lo_column ?= lo_columns->get_column( c_fname_check ).
      lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
    CATCH cx_salv_not_found.
      RETURN.
  ENDTRY.

  lo_events = go_popup_to->get_event( ).
  SET HANDLER lcl_mc_events=>hndl_link_click FOR lo_events.
  SET HANDLER lcl_mc_events=>hndl_popup_user_command FOR lo_events.

  CLEAR gv_okcode_popup.

  go_popup_to->display( ).

  IF gv_okcode_popup EQ c_ucomm_cancel.
    RETURN.
  ENDIF.

  LOOP AT gt_popup_lqua[] ASSIGNING <fs_popup>.
    lv_tabix  = sy-tabix.

    READ TABLE gt_lqua[] ASSIGNING <fs_lqua> INDEX lv_tabix.
    CHECK sy-subrc EQ 0.

    <fs_lqua>-chckd = <fs_popup>-chckd.
  ENDLOOP.
ENDFORM.                    "f_data_ucomm_tocreate_select
*&---------------------------------------------------------------------*
*&      Form  F_DATA_UCOMM_TOCREATE_SETCHCKD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_ROW      text
*----------------------------------------------------------------------*
FORM f_data_ucomm_tocreate_setchckd USING i_row TYPE sytabix.
  FIELD-SYMBOLS <fs_popup> LIKE LINE OF gt_popup_lqua[].

  READ TABLE gt_popup_lqua[] ASSIGNING <fs_popup> INDEX i_row.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  IF <fs_popup>-chckd EQ abap_true.
    <fs_popup>-chckd = abap_false.
  ELSE.
    <fs_popup>-chckd = abap_true.
  ENDIF.

  go_popup_to->refresh( ).
ENDFORM.                    " F_DATA_UCOMM_TOCREATE_SETCHCKD
*&---------------------------------------------------------------------*
*&      Form  F_DATA_UCOMM_TOCREATE_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_ucomm_tocreate_save.
  DATA lv_bwlvs           TYPE ltak-bwlvs.
  DATA lv_bwlvs_cc        TYPE ltak-bwlvs.
  DATA lv_bwlvs_aufnr     TYPE ltak-bwlvs.
  DATA lv_lgort           TYPE ltap-lgort.
  DATA lv_benum           TYPE ltak-benum.
  DATA lv_aufnr           TYPE afko-aufnr.
  DATA lv_kostl           TYPE zwmmpt008-kostl.
  DATA lv_lznum           TYPE ltak-lznum.
  DATA lv_answer          TYPE char1.
  DATA lv_betyp           TYPE lvs_betyp.

  DATA lr_lgtyp TYPE RANGE OF lqua-lgtyp.

  DATA lt_lqua    TYPE ty_t_lqua.
  DATA lt_bapiret TYPE bapiret2_t.

  FIELD-SYMBOLS <fs_lgtyp>      LIKE LINE OF lr_lgtyp[].
  FIELD-SYMBOLS <fs_lqua>       LIKE LINE OF gt_lqua[].
  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].
  FIELD-SYMBOLS <fs_bapiret>    LIKE LINE OF lt_bapiret[].

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question = text-008
    IMPORTING
      answer        = lv_answer.
  IF lv_answer NE '1'.
    RETURN.
  ENDIF.

  " Get parameters for TO creation
  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo   = c_process_prod_cons
                   parametro  = c_param_mov_wm_op_aufnr
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_bwlvs_aufnr  = <fs_zwmmpt001>-valor.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo   = c_process_prod_cons
                   parametro  = c_param_mov_wm_op_cc
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_bwlvs_cc  = <fs_zwmmpt001>-valor.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_lgort
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_lgort  = <fs_zwmmpt001>-valor.
  ENDIF.

  LOOP AT gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001> WHERE processo EQ c_process_general
                                                    AND parametro EQ c_param_lgtyp_rack.
    APPEND INITIAL LINE TO lr_lgtyp[] ASSIGNING <fs_lgtyp>.
    <fs_lgtyp>-sign     = c_rsig_i.
    <fs_lgtyp>-option   = c_ropt_eq.
    <fs_lgtyp>-low      = <fs_zwmmpt001>-valor.
  ENDLOOP.

  LOOP AT gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001> WHERE processo EQ c_process_general
                                                    AND parametro EQ c_param_lgtyp_locker.
    APPEND INITIAL LINE TO lr_lgtyp[] ASSIGNING <fs_lgtyp>.
    <fs_lgtyp>-sign     = c_rsig_i.
    <fs_lgtyp>-option   = c_ropt_eq.
    <fs_lgtyp>-low      = <fs_zwmmpt001>-valor.
  ENDLOOP.

  IF gs_topopup-kostl IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_topopup-aufnr
      IMPORTING
        output = lv_aufnr.

    CONCATENATE gs_topopup-fevor '#' c_totype_po '#' lv_aufnr INTO lv_lznum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lv_aufnr
      IMPORTING
        output = lv_benum.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_topopup-kostl
      IMPORTING
        output = lv_kostl.

    CONCATENATE gs_topopup-fevor '#' c_totype_cc '#' lv_kostl INTO lv_lznum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lv_kostl
      IMPORTING
        output = lv_benum.
  ENDIF.

  SORT gt_lqua[] BY lenum.
  DELETE ADJACENT DUPLICATES FROM gt_lqua[] COMPARING lenum.
  SORT gt_lqua[] BY lgtyp DESCENDING wdatu ASCENDING verme ASCENDING.

  LOOP AT gt_lqua[] ASSIGNING <fs_lqua>.
    CHECK <fs_lqua>-chckd EQ abap_true.

    IF <fs_lqua>-lgtyp IN lr_lgtyp[].
      IF lv_kostl IS NOT INITIAL. " WM movement for Cost Center
        lv_bwlvs = lv_bwlvs_cc.
      ELSE. " WM movement for Prod. Order
        lv_bwlvs = lv_bwlvs_aufnr.
      ENDIF.

      CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
        EXPORTING
          i_lenum               = <fs_lqua>-lenum
          i_bwlvs               = lv_bwlvs
          i_lznum               = lv_lznum
        EXCEPTIONS
          not_confirmed_to      = 1
          foreign_lock          = 2
          bwlvs_wrong           = 3
          betyp_wrong           = 4
          nltyp_wrong           = 5
          nlpla_wrong           = 6
          nltyp_missing         = 7
          nlpla_missing         = 8
          squit_forbidden       = 9
          lgber_wrong           = 10
          xfeld_wrong           = 11
          drukz_wrong           = 12
          ldest_wrong           = 13
          no_stock_on_su        = 14
          su_not_found          = 15
          update_without_commit = 16
          no_authority          = 17
          benum_required        = 18
          ltap_move_su_wrong    = 19
          lenum_wrong           = 20
          error_message         = 99
          OTHERS                = 21.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO lt_bapiret[] ASSIGNING <fs_bapiret>.
        <fs_bapiret>-type       = c_msgty_e.
        <fs_bapiret>-id         = c_msgid_zwmfr001.
        <fs_bapiret>-number     = '030'.
        <fs_bapiret>-message_v1 = <fs_lqua>-lenum.

        APPEND INITIAL LINE TO lt_bapiret[] ASSIGNING <fs_bapiret>.
        <fs_bapiret>-type       = sy-msgty.
        <fs_bapiret>-id         = sy-msgid.
        <fs_bapiret>-number     = sy-msgno.
        <fs_bapiret>-message_v1 = sy-msgv1.
        <fs_bapiret>-message_v2 = sy-msgv2.
        <fs_bapiret>-message_v3 = sy-msgv3.
        <fs_bapiret>-message_v4 = sy-msgv4.
      ELSE.
        APPEND INITIAL LINE TO lt_bapiret[] ASSIGNING <fs_bapiret>.
        <fs_bapiret>-type       = c_msgty_s.
        <fs_bapiret>-id         = c_msgid_zwmfr001.
        <fs_bapiret>-number     = '031'.
        <fs_bapiret>-message_v1 = <fs_lqua>-lenum.
      ENDIF.
    ELSE.
      APPEND <fs_lqua> TO lt_lqua[].
    ENDIF.
  ENDLOOP.

  LOOP AT lt_lqua[] ASSIGNING <fs_lqua>.
    IF lv_kostl IS NOT INITIAL. " WM movement for Cost Center
      lv_bwlvs = lv_bwlvs_cc.
      lv_betyp = 'K'.
    ELSE. " WM movement for Prod. Order
      lv_bwlvs = lv_bwlvs_aufnr.
      lv_betyp = 'F'.
    ENDIF.

    CALL FUNCTION 'L_TO_CREATE_SINGLE'
      EXPORTING
        i_lgnum               = <fs_lqua>-lgnum
        i_bwlvs               = lv_bwlvs
        i_betyp               = lv_betyp
        i_benum               = lv_benum
        i_matnr               = <fs_lqua>-matnr
        i_werks               = <fs_lqua>-werks
        i_lgort               = lv_lgort
        i_anfme               = <fs_lqua>-verme
        i_altme               = <fs_lqua>-meins
        i_lznum               = lv_lznum
        i_vlenr               = <fs_lqua>-lenum
        i_vltyp               = <fs_lqua>-lgtyp
        i_vlpla               = <fs_lqua>-lgpla
      EXCEPTIONS
        no_to_created         = 1
        bwlvs_wrong           = 2
        betyp_wrong           = 3
        benum_missing         = 4
        betyp_missing         = 5
        foreign_lock          = 6
        vltyp_wrong           = 7
        vlpla_wrong           = 8
        vltyp_missing         = 9
        nltyp_wrong           = 10
        nlpla_wrong           = 11
        nltyp_missing         = 12
        rltyp_wrong           = 13
        rlpla_wrong           = 14
        rltyp_missing         = 15
        squit_forbidden       = 16
        manual_to_forbidden   = 17
        letyp_wrong           = 18
        vlpla_missing         = 19
        nlpla_missing         = 20
        sobkz_wrong           = 21
        sobkz_missing         = 22
        sonum_missing         = 23
        bestq_wrong           = 24
        lgber_wrong           = 25
        xfeld_wrong           = 26
        date_wrong            = 27
        drukz_wrong           = 28
        ldest_wrong           = 29
        update_without_commit = 30
        no_authority          = 31
        material_not_found    = 32
        lenum_wrong           = 33
        error_message         = 99
        OTHERS                = 34.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO lt_bapiret[] ASSIGNING <fs_bapiret>.
      <fs_bapiret>-type       = c_msgty_e.
      <fs_bapiret>-id         = c_msgid_zwmfr001.
      <fs_bapiret>-number     = '032'.
      <fs_bapiret>-message_v1 = <fs_lqua>-lgpla.
      <fs_bapiret>-message_v2 = <fs_lqua>-lenum.

      APPEND INITIAL LINE TO lt_bapiret[] ASSIGNING <fs_bapiret>.
      <fs_bapiret>-type       = sy-msgty.
      <fs_bapiret>-id         = sy-msgid.
      <fs_bapiret>-number     = sy-msgno.
      <fs_bapiret>-message_v1 = sy-msgv1.
      <fs_bapiret>-message_v2 = sy-msgv2.
      <fs_bapiret>-message_v3 = sy-msgv3.
      <fs_bapiret>-message_v4 = sy-msgv4.
    ELSE.
      APPEND INITIAL LINE TO lt_bapiret[] ASSIGNING <fs_bapiret>.
      <fs_bapiret>-type       = c_msgty_s.
      <fs_bapiret>-id         = c_msgid_zwmfr001.
      <fs_bapiret>-number     = '033'.
      <fs_bapiret>-message_v1 = <fs_lqua>-lgpla.
      <fs_bapiret>-message_v2 = <fs_lqua>-lenum.
    ENDIF.
  ENDLOOP.

  IF lt_bapiret[] IS INITIAL.
    APPEND INITIAL LINE TO lt_bapiret[] ASSIGNING <fs_bapiret>.
    <fs_bapiret>-type       = c_msgty_w.
    <fs_bapiret>-id         = c_msgid_zwmfr001.
    <fs_bapiret>-number     = '043'.
  ENDIF.

  CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
    TABLES
      i_bapiret2_tab = lt_bapiret[].
ENDFORM.                    " F_DATA_UCOMM_TOCREATE_SAVE
*&---------------------------------------------------------------------*
*&      Form  F_DATA_UCOMM_TOCANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_ucomm_tocancel.
  DATA lt_rows TYPE salv_t_row.
  DATA lt_ltap TYPE STANDARD TABLE OF ltap_cancl.

  DATA lo_selections TYPE REF TO cl_salv_selections.

  FIELD-SYMBOLS <fs_row>    LIKE LINE OF lt_rows[].
  FIELD-SYMBOLS <fs_toview> LIKE LINE OF gt_toview[].
  FIELD-SYMBOLS <fs_ltap>   LIKE LINE OF lt_ltap[].

  lo_selections = go_popup->get_selections( ).

  lt_rows[] = lo_selections->get_selected_rows( ).
  IF LINES( lt_rows[] ) NE 1.
    MESSAGE s010 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  READ TABLE lt_rows[] ASSIGNING <fs_row> INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s010 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  READ TABLE gt_toview[] ASSIGNING <fs_toview> INDEX <fs_row>.
  IF sy-subrc NE 0.
    MESSAGE s010 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  APPEND INITIAL LINE TO lt_ltap[] ASSIGNING <fs_ltap>.
  <fs_ltap>-tanum = <fs_toview>-tanum.
  <fs_ltap>-tapos = <fs_toview>-tapos.

  CALL FUNCTION 'L_TO_CANCEL'
    EXPORTING
      i_lgnum                      = gs_xuser-lgnum
      i_tanum                      = <fs_toview>-tanum
      i_commit_work                = abap_false
    TABLES
      t_ltap_cancl                 = lt_ltap[]
    EXCEPTIONS
      to_confirmed                 = 1
      to_doesnt_exist              = 2
      item_confirmed               = 3
      item_doesnt_exist            = 4
      foreign_lock                 = 5
      double_lines                 = 6
      nothing_to_do                = 7
      xfeld_wrong                  = 8
      su_movement_partly_confirmed = 9
      update_without_commit        = 10
      no_authority                 = 11
      OTHERS                       = 12.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    MESSAGE ID sy-msgid TYPE c_msgty_s NUMBER sy-msgno DISPLAY LIKE c_msgty_e
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    MESSAGE s015.

    PERFORM f_data_ucomm_toview USING abap_true.

    go_popup->refresh( ).
  ENDIF.
ENDFORM.                    " F_DATA_UCOMM_TOCANCEL
*&---------------------------------------------------------------------*
*&      Form  f_data_ucomm_toview
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_REFRESH  text
*----------------------------------------------------------------------*
FORM f_data_ucomm_toview USING i_refresh TYPE abap_bool.
  DATA lv_icon_pquit  TYPE icon_d.
  DATA lv_fevor       TYPE afko-fevor.
  DATA lv_aufnr       TYPE afko-aufnr.
  DATA lv_kostl_aufnr TYPE char12.
  DATA lv_kostl       TYPE kostl.                           "#EC NEEDED
  DATA lv_totype      TYPE char2.                           "#EC NEEDED

  DATA lo_selections  TYPE REF TO cl_salv_selections_tree.
  DATA lo_data        TYPE REF TO data.
  DATA lo_nodes       TYPE REF TO cl_salv_nodes.
  DATA lo_node        TYPE REF TO cl_salv_node.

  DATA lt_nodes   TYPE salv_t_nodes.
  DATA lt_ltak    TYPE ty_ht_ltak.
  DATA lt_ltap    TYPE ty_st_ltap.
  DATA lt_makt    TYPE ty_ht_makt.

  DATA lr_queues  TYPE RANGE OF ltak-queue.
  DATA lr_kquit   TYPE RANGE OF ltak-kquit.
  DATA lr_fevor   TYPE RANGE OF zwmfr_sprodplan_output-fevor.
  DATA lr_aufnr   TYPE RANGE OF zwmfr_sprodplan_output-aufnr.
  DATA lr_kostl   TYPE RANGE OF zwmfr_sprodplan_output-kostl.
  DATA lr_matnr   TYPE RANGE OF zwmfr_sprodplan_output-matnr.

  FIELD-SYMBOLS <fs_fevor>      LIKE LINE OF lr_fevor[].
  FIELD-SYMBOLS <fs_aufnr>      LIKE LINE OF lr_aufnr[].
  FIELD-SYMBOLS <fs_matnr>      LIKE LINE OF lr_matnr[].
  FIELD-SYMBOLS <fs_kostl>      LIKE LINE OF lr_kostl[].
  FIELD-SYMBOLS <fs_node>       LIKE LINE OF lt_nodes[].
  FIELD-SYMBOLS <fs_queue>      LIKE LINE OF lr_queues[].
  FIELD-SYMBOLS <fs_kquit>      LIKE LINE OF lr_kquit[].
  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].
  FIELD-SYMBOLS <fs_output>     LIKE LINE OF gt_output_tmp[].
  FIELD-SYMBOLS <fs_ltap>       LIKE LINE OF lt_ltap[].
  FIELD-SYMBOLS <fs_ltak>       LIKE LINE OF lt_ltak[].
  FIELD-SYMBOLS <fs_makt>       LIKE LINE OF lt_makt[].
  FIELD-SYMBOLS <fs_toview>     LIKE LINE OF gt_toview[].
  FIELD-SYMBOLS <fs_data>       TYPE zwmfr_sprodplan_output.

  LOOP AT gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001> WHERE processo EQ c_process_system_guide
                                                    AND parametro EQ c_param_queue_lin.
    APPEND INITIAL LINE TO lr_queues[] ASSIGNING <fs_queue>.
    <fs_queue>-sign     = c_rsig_i.
    <fs_queue>-option   = c_ropt_eq.
    <fs_queue>-low      = <fs_zwmmpt001>-valor.
  ENDLOOP.

*  APPEND INITIAL LINE TO lr_kquit[] ASSIGNING <fs_kquit>.
*  <fs_kquit>-sign   = c_rsig_i.
*  <fs_kquit>-option = c_ropt_eq.
*  <fs_kquit>-low    = abap_true.
  APPEND INITIAL LINE TO lr_kquit[] ASSIGNING <fs_kquit>.
  <fs_kquit>-sign   = c_rsig_i.
  <fs_kquit>-option = c_ropt_eq.
  <fs_kquit>-low    = abap_false.

  FREE gt_toview[].

  lo_selections = go_tree->get_selections( ).

  lt_nodes[] = lo_selections->get_selected_nodes( ).
  IF LINES( lt_nodes[] ) EQ 0.  " no nodes selected, display all
    lo_nodes    = go_tree->get_nodes( ).
    TRY.
        lo_node     = lo_nodes->get_node( cl_salv_node=>c_virtual_root_node  ).
        lt_nodes[]  = lo_node->get_children( ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.
  ELSEIF LINES( lt_nodes[] ) GT 1.
    MESSAGE s010 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  LOOP AT lt_nodes[] ASSIGNING <fs_node>.
    lo_data = <fs_node>-node->get_data_row( ).
    ASSIGN lo_data->* TO <fs_data> CASTING.
    CHECK sy-subrc EQ 0.

    CASE <fs_data>-nodty.
      WHEN c_nodty_fevor.
        APPEND INITIAL LINE TO lr_fevor[] ASSIGNING <fs_fevor>.
        <fs_fevor>-sign   = c_rsig_i.
        <fs_fevor>-option = c_ropt_eq.
        <fs_fevor>-low    = <fs_data>-fevor.

        LOOP AT gt_output_tmp[] ASSIGNING <fs_output> WHERE fevor EQ <fs_data>-fevor.
          APPEND INITIAL LINE TO lr_kostl[] ASSIGNING <fs_kostl>.
          <fs_kostl>-sign   = c_rsig_i.
          <fs_kostl>-option = c_ropt_eq.
          <fs_kostl>-low    = <fs_output>-kostl.
          APPEND INITIAL LINE TO lr_aufnr[] ASSIGNING <fs_aufnr>.
          <fs_aufnr>-sign   = c_rsig_i.
          <fs_aufnr>-option = c_ropt_eq.
          <fs_aufnr>-low    = <fs_output>-aufnr.
          APPEND INITIAL LINE TO lr_matnr[] ASSIGNING <fs_matnr>.
          <fs_matnr>-sign   = c_rsig_i.
          <fs_matnr>-option = c_ropt_eq.
          <fs_matnr>-low    = <fs_output>-matnr.
        ENDLOOP.
      WHEN c_nodty_kostl.
        APPEND INITIAL LINE TO lr_fevor[] ASSIGNING <fs_fevor>.
        <fs_fevor>-sign   = c_rsig_i.
        <fs_fevor>-option = c_ropt_eq.
        <fs_fevor>-low    = <fs_data>-fevor.
        APPEND INITIAL LINE TO lr_kostl[] ASSIGNING <fs_kostl>.
        <fs_kostl>-sign   = c_rsig_i.
        <fs_kostl>-option = c_ropt_eq.
        <fs_kostl>-low    = <fs_data>-kostl.

        LOOP AT gt_output_tmp[] ASSIGNING <fs_output> WHERE fevor EQ <fs_data>-fevor
                                                        AND kostl EQ <fs_data>-kostl.
          APPEND INITIAL LINE TO lr_matnr[] ASSIGNING <fs_matnr>.
          <fs_matnr>-sign   = c_rsig_i.
          <fs_matnr>-option = c_ropt_eq.
          <fs_matnr>-low    = <fs_output>-matnr.
        ENDLOOP.
      WHEN c_nodty_gstrp.
        APPEND INITIAL LINE TO lr_fevor[] ASSIGNING <fs_fevor>.
        <fs_fevor>-sign   = c_rsig_i.
        <fs_fevor>-option = c_ropt_eq.
        <fs_fevor>-low    = <fs_data>-fevor.

        LOOP AT gt_output_tmp[] ASSIGNING <fs_output> WHERE fevor EQ <fs_data>-fevor
                                                        AND kostl EQ <fs_data>-kostl
                                                        AND gstrp EQ <fs_data>-gstrp.
          APPEND INITIAL LINE TO lr_aufnr[] ASSIGNING <fs_aufnr>.
          <fs_aufnr>-sign   = c_rsig_i.
          <fs_aufnr>-option = c_ropt_eq.
          <fs_aufnr>-low    = <fs_output>-aufnr.
          APPEND INITIAL LINE TO lr_matnr[] ASSIGNING <fs_matnr>.
          <fs_matnr>-sign   = c_rsig_i.
          <fs_matnr>-option = c_ropt_eq.
          <fs_matnr>-low    = <fs_output>-matnr.
        ENDLOOP.
      WHEN OTHERS.
        APPEND INITIAL LINE TO lr_fevor[] ASSIGNING <fs_fevor>.
        <fs_fevor>-sign   = c_rsig_i.
        <fs_fevor>-option = c_ropt_eq.
        <fs_fevor>-low    = <fs_data>-fevor.
        APPEND INITIAL LINE TO lr_aufnr[] ASSIGNING <fs_aufnr>.
        <fs_aufnr>-sign   = c_rsig_i.
        <fs_aufnr>-option = c_ropt_eq.
        <fs_aufnr>-low    = <fs_data>-aufnr.
        APPEND INITIAL LINE TO lr_kostl[] ASSIGNING <fs_kostl>.
        <fs_kostl>-sign   = c_rsig_i.
        <fs_kostl>-option = c_ropt_eq.
        <fs_kostl>-low    = <fs_data>-kostl.

        LOOP AT gt_output_tmp[] ASSIGNING <fs_output> WHERE fevor EQ <fs_data>-fevor
                                                        AND kostl EQ <fs_data>-kostl
                                                        AND gstrp EQ <fs_data>-gstrp
                                                        AND aufnr EQ <fs_data>-aufnr.
          IF <fs_data>-nodty EQ c_nodty_matnr.
            CHECK <fs_output>-matnr EQ <fs_data>-matnr.
          ENDIF.

          APPEND INITIAL LINE TO lr_matnr[] ASSIGNING <fs_matnr>.
          <fs_matnr>-sign   = c_rsig_i.
          <fs_matnr>-option = c_ropt_eq.
          <fs_matnr>-low    = <fs_output>-matnr.
        ENDLOOP.
    ENDCASE.
  ENDLOOP.

  SORT lr_fevor[] BY low.
  DELETE ADJACENT DUPLICATES FROM lr_fevor[] COMPARING low.
  SORT lr_aufnr[] BY low.
  DELETE ADJACENT DUPLICATES FROM lr_aufnr[] COMPARING low.
  SORT lr_matnr[] BY low.
  DELETE ADJACENT DUPLICATES FROM lr_matnr[] COMPARING low.
  SORT lr_kostl[] BY low.
  DELETE ADJACENT DUPLICATES FROM lr_kostl[] COMPARING low.

  SORT lr_fevor[] BY sign option low.
  SORT lr_kostl[] BY sign option low.
  SORT lr_aufnr[] BY sign option low.
  SORT lr_matnr[] BY sign option low.

  SELECT lgnum tanum kquit lznum queue
    FROM ltak INTO TABLE lt_ltak[]
    WHERE lgnum EQ gs_xuser-lgnum
      AND kquit IN lr_kquit[]
      AND queue IN lr_queues[]. " Index LTAK~Q
  IF sy-subrc NE 0.
    MESSAGE s014 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  SELECT lgnum tanum tapos matnr meins altme pquit
         qname vltyp vlpla vsolm vsola pvqui
    FROM ltap INTO TABLE lt_ltap[]
    FOR ALL ENTRIES IN lt_ltak[]
    WHERE lgnum EQ lt_ltak-lgnum
      AND tanum EQ lt_ltak-tanum.
  IF sy-subrc NE 0.
    MESSAGE s014 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  SELECT matnr maktx
    FROM makt INTO TABLE lt_makt[]
    FOR ALL ENTRIES IN lt_ltap[]
    WHERE matnr EQ lt_ltap-matnr
      AND spras EQ sy-langu.

  LOOP AT lt_ltap[] ASSIGNING <fs_ltap>.
    CLEAR lv_kostl.
    CLEAR lv_aufnr.

    READ TABLE lt_ltak[] ASSIGNING <fs_ltak>
      WITH TABLE KEY lgnum = <fs_ltap>-lgnum
                     tanum = <fs_ltap>-tanum.
    CHECK sy-subrc EQ 0.

    READ TABLE lt_makt[] ASSIGNING <fs_makt>
      WITH TABLE KEY matnr = <fs_ltap>-matnr.
    CHECK sy-subrc EQ 0.

    SPLIT <fs_ltak>-lznum AT '#' INTO lv_fevor lv_totype lv_kostl_aufnr.
    IF lv_totype EQ c_totype_cc.
      lv_kostl  = lv_kostl_aufnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_kostl
        IMPORTING
          output = lv_kostl.
    ELSEIF lv_totype EQ c_totype_po.
      lv_aufnr  = lv_kostl_aufnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_aufnr
        IMPORTING
          output = lv_aufnr.
    ENDIF.

    CHECK <fs_ltap>-matnr IN lr_matnr[].
    CHECK lv_fevor IN lr_fevor[].

    IF lv_kostl IS INITIAL.
      CHECK lv_aufnr IN lr_aufnr[].
    ELSE.
      CHECK lv_kostl IN lr_kostl[].
    ENDIF.

    IF <fs_ltap>-pquit IS INITIAL AND <fs_ltap>-pvqui IS INITIAL.
      lv_icon_pquit = c_icon_red.
    ELSEIF <fs_ltap>-pquit IS INITIAL AND <fs_ltap>-pvqui IS NOT INITIAL.
      lv_icon_pquit = c_icon_yellow.
    ELSEIF <fs_ltap>-pquit IS NOT INITIAL AND <fs_ltap>-pvqui IS NOT INITIAL.
      lv_icon_pquit = c_icon_green.
    ENDIF.

    APPEND INITIAL LINE TO gt_toview[] ASSIGNING <fs_toview>.
    <fs_toview>-fevor = lv_fevor.
    <fs_toview>-kostl = lv_kostl.
    <fs_toview>-aufnr = lv_aufnr.
    <fs_toview>-matnr = <fs_ltap>-matnr.
    <fs_toview>-maktx = <fs_makt>-maktx.
    <fs_toview>-tanum = <fs_ltap>-tanum.
    <fs_toview>-tapos = <fs_ltap>-tapos.
    <fs_toview>-pquit = lv_icon_pquit.
    <fs_toview>-vltyp = <fs_ltap>-vltyp.
    <fs_toview>-vlpla = <fs_ltap>-vlpla.
    <fs_toview>-vsola = <fs_ltap>-vsola.
    <fs_toview>-altme = <fs_ltap>-altme.
    <fs_toview>-qname = <fs_ltap>-qname.
  ENDLOOP.

  IF gt_toview[] IS INITIAL.
    MESSAGE s014 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  SORT gt_toview[] BY fevor aufnr matnr pquit.

  IF i_refresh EQ abap_false.
    PERFORM f_data_ucomm_toview_display.
  ENDIF.
ENDFORM.                    " F_DATA_UCOMM_TOVIEW
*&---------------------------------------------------------------------*
*&      Form  F_DATA_UCOMM_TOVIEW_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_ucomm_toview_display.
  IF go_popup IS BOUND.
    FREE go_popup.
  ENDIF.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table  = go_popup
                              CHANGING t_table        = gt_toview[] ).
    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

  PERFORM f_data_ucomm_toview_layout.
  PERFORM f_data_ucomm_toview_toolbar USING c_pfstatus_popup.
  PERFORM f_data_ucomm_toview_selection.
  PERFORM f_data_ucomm_toview_handler.

  go_popup->set_screen_popup( start_column = 1
                              end_column   = 140
                              start_line   = 1
                              end_line     = 16 ).

  go_popup->display( ).
ENDFORM.                    " F_DATA_UCOMM_TOVIEW_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  f_data_ucomm_toview_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_data_ucomm_toview_layout.
  DATA lo_display TYPE REF TO cl_salv_display_settings.
  DATA lo_layout  TYPE REF TO cl_salv_layout.

  DATA ls_key TYPE salv_s_layout_key.

  lo_display = go_popup->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

  ls_key-report = sy-repid.

  lo_layout = go_popup->get_layout( ).
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_default( abap_true ).
ENDFORM.                    " F_DATA_UCOMM_TOVIEW_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  f_data_ucomm_toview_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_PFSTATUS text
*----------------------------------------------------------------------*
FORM f_data_ucomm_toview_toolbar USING i_pfstatus TYPE sypfkey.
  DATA lo_functions TYPE REF TO cl_salv_functions.

  go_popup->set_screen_status( report    = sy-repid
                               pfstatus  = i_pfstatus ).

  lo_functions = go_popup->get_functions( ).
  lo_functions->set_all( abap_true ).
ENDFORM.                    "f_data_ucomm_toview_toolbar
*&---------------------------------------------------------------------*
*&      Form  f_data_ucomm_toview_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_data_ucomm_toview_selection.
  DATA lo_selections TYPE REF TO cl_salv_selections.

  lo_selections = go_popup->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
ENDFORM.                    "f_data_ucomm_toview_selection
*&---------------------------------------------------------------------*
*&      Form  f_data_ucomm_toview_handler
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_data_ucomm_toview_handler.
  DATA lo_events_table TYPE REF TO cl_salv_events_table.

  lo_events_table = go_popup->get_event( ).

  SET HANDLER lcl_mc_events=>hndl_popup_user_command FOR lo_events_table.
ENDFORM.                    "f_data_ucomm_toview_handler
*&---------------------------------------------------------------------*
*&      Form  F_DATA_UCOMM_DUPVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_ucomm_dupview.
  DATA lt_dupview TYPE STANDARD TABLE OF ty_duplicates.

  lt_dupview[] = gt_dupview[].

  SORT lt_dupview[] BY dbcnt.
  DELETE lt_dupview[] WHERE dbcnt EQ 1.
  SORT lt_dupview[] BY matnr.

  IF go_popup IS BOUND.
    FREE go_popup.
  ENDIF.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table  = go_popup
                              CHANGING t_table        = lt_dupview[] ).
    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

  PERFORM f_data_ucomm_toview_layout.
  PERFORM f_data_ucomm_toview_toolbar USING c_pfstatus_popup3.
  PERFORM f_data_ucomm_toview_selection.
  PERFORM f_data_ucomm_toview_handler.

  go_popup->set_screen_popup( start_column = 1
                              end_column   = 140
                              start_line   = 1
                              end_line     = 16 ).

  go_popup->display( ).
ENDFORM.                    " F_DATA_UCOMM_DUPVIEW



*&---------------------------------------------------------------------*
*&      Form  f_create_ot_dev
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA     text
*----------------------------------------------------------------------*
FORM f_create_ot_dev.
**  criar OT Dummy para chamada do empilhador á linha
  DATA lo_selections  TYPE REF TO cl_salv_selections_tree.
  DATA lo_data        TYPE REF TO data.
  DATA lo_nodes       TYPE REF TO cl_salv_nodes.
  DATA lo_node        TYPE REF TO cl_salv_node.

  DATA lt_nodes   TYPE salv_t_nodes.

  FIELD-SYMBOLS <fs_data>       TYPE zwmfr_sprodplan_output.
  FIELD-SYMBOLS <fs_node>       LIKE LINE OF lt_nodes[].

  DATA: lv_lznum TYPE lvs_lznum.


  DATA: lv_lgpla TYPE lgpla.
  DATA: lt_ltak LIKE ltak OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.




  lo_selections = go_tree->get_selections( ).

  lt_nodes[] = lo_selections->get_selected_nodes( ).
  IF LINES( lt_nodes[] ) EQ 0.  " no nodes selected, display all
    lo_nodes    = go_tree->get_nodes( ).
    TRY.
        lo_node     = lo_nodes->get_node( cl_salv_node=>c_virtual_root_node  ).
        lt_nodes[]  = lo_node->get_children( ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.
  ELSEIF LINES( lt_nodes[] ) GT 1.
    MESSAGE s010 DISPLAY LIKE c_msgty_e.
    RETURN.
  ENDIF.

  LOOP AT lt_nodes[] ASSIGNING <fs_node>.
    lo_data = <fs_node>-node->get_data_row( ).
    ASSIGN lo_data->* TO <fs_data> CASTING.
    CHECK sy-subrc EQ 0.

    CASE <fs_data>-nodty.
      WHEN c_nodty_fevor.
        MOVE <fs_data>-fevor TO lv_lgpla.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDLOOP.

  SELECT * INTO TABLE lt_ltak FROM ltak
  WHERE lgnum = gs_xuser-lgnum AND
        kquit = '' AND
        queue = gv_queue_dev.

  IF sy-subrc = 0.
    SELECT * INTO TABLE lt_ltap FROM ltap FOR ALL ENTRIES IN
    lt_ltak WHERE lgnum = gs_xuser-lgnum AND
                  tanum = lt_ltak-tanum.

    READ TABLE lt_ltap WITH KEY vlpla = lv_lgpla
                                nlpla = lv_lgpla.
    IF sy-subrc = 0.
      MESSAGE i000 WITH 'Já foi efectuada chamada de devolução para zona'
                        lv_lgpla.
      EXIT.
    ENDIF.

  ENDIF.

  lv_lznum = lv_lgpla.
  CONDENSE lv_lznum.


  CALL FUNCTION 'L_TO_CREATE_SINGLE'
    EXPORTING
      i_lgnum                     = gs_xuser-lgnum
      i_bwlvs                     = gv_dummy_dev
      i_matnr                     = gv_dummy_mat
      i_werks                     = s_werks-low
      i_anfme                     = 1
      i_altme                     = 'PAL'
*     I_SQUIT                     = ' '
     i_vlpla                     = lv_lgpla
     i_nlpla                     = lv_lgpla
     i_lznum                     = lv_lznum
   EXCEPTIONS
     error_message               = 1
     OTHERS                      = 2.
  IF sy-subrc <> 0.
    MESSAGE i000 WITH 'Falha a requisitar Devolução de Volumes.'.
  ELSE.
    MESSAGE i000 WITH 'Requisição devolução de volumes'
                      ' efectuada com sucesso.'.
  ENDIF.
ENDFORM.                    "create_ot_dev
