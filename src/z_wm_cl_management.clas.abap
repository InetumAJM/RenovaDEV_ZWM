class Z_WM_CL_MANAGEMENT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF quantity_needed,
        refnr     TYPE lvs_refnr,
        vbeln     TYPE vbeln,
        posnr     TYPE posnr,
        matnr     TYPE matnr,
        werks     TYPE werks_d,
        lgort     TYPE lgort_d,
        menge     TYPE menge_d,
        meinh     TYPE meinh,
        lfimg     TYPE lfimg,
        pikmg     TYPE pikmg,
        kosta     TYPE kosta,
        two_step  TYPE flag,
        two_spart TYPE flag,
      END OF quantity_needed .
  types:
    t_quantity_needed TYPE TABLE OF quantity_needed .
  types:
    BEGIN OF matnr_prd_components,
        lgnum      TYPE lgnum,
        matnr      TYPE matnr,
        werks      TYPE werks_d,
        lgort      TYPE lgort_d,
        menge      TYPE menge_d,
        menge_real TYPE menge_d,
        meins      TYPE meins,
        pal        TYPE i,
        pal_menge  TYPE menge_d,
        pick_menge TYPE menge_d,
        lhme1      TYPE lhmeh1,
        lhmg1      TYPE lvs_lhmng1,
        letyp      TYPE lvs_letyp,
        remontada  TYPE flag,
      END OF matnr_prd_components .
  types:
    t_matnr_prd_components TYPE TABLE OF matnr_prd_components .

  class-methods GET_LENUM
    importing
      !I_LGNUM type LGNUM optional
      !I_LENUM type LENUM optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !IS_DATA type ANY
      !I_USETO type FLAG default 'X'
      !I_USEPICKING type FLAG default 'X'
    exporting
      !E_LGNUM type LGNUM
    returning
      value(R_RESULT) type ZWM_TAB_LENUM .
  class-methods GET_LETYP
    importing
      !I_LGNUM type LGNUM optional
      !I_LETYP type LVS_LETYP optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !I_LENUM type LENUM optional
      !IS_DATA type ANY optional
    returning
      value(R_RESULT) type LVS_LETYP .
  class-methods GET_LGNUM
    importing
      !I_LGNUM type LGNUM optional
      !I_LENUM type LENUM optional
      !IS_DATA type ANY
    returning
      value(R_RESULT) type LGNUM .
  class-methods GET_LGNUM_FROM_LENUM
    importing
      !I_LENUM type LENUM optional
      !IT_LENUM type ZWM_TAB_LENUM optional
      !IS_DATA type ANY optional
    returning
      value(R_RESULT) type LGNUM .
  class-methods GET_MATNR
    importing
      !I_LGNUM type LGNUM optional
      !I_MATNR type MATNR optional
      !IS_DATA type ANY
    exporting
      !E_LHMNG type MENGE_D
      !E_LHMEN type MEINS
      !E_LETYP type LVS_LETYP
    returning
      value(R_RESULT) type MATNR .
  class-methods GET_REFNR
    importing
      !I_LGNUM type LGNUM optional
      !I_REFNR type LVS_REFNR optional
      !I_TKNUM type TKNUM optional
      !I_VBELN type VBELN optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !I_LENUM type LENUM optional
      !IS_DATA type ANY optional
    exporting
      !E_TKNUM type TKNUM
    returning
      value(R_RESULT) type LVS_REFNR .
  class-methods GET_TANUM
    importing
      !I_LGNUM type LGNUM optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !IS_DATA type ANY
    exporting
      !E_TAPOS type TAPOS
      !E_REFNR type LVS_REFNR
      !E_VBELN type VBELN
      !ET_LENUM type ZWM_TAB_LENUM
    returning
      value(R_RESULT) type TANUM .
  class-methods GET_VBELN
    importing
      !I_LGNUM type LGNUM optional
      !I_VBELN type VBELN optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !I_LENUM type LENUM optional
      !IS_DATA type ANY optional
    exporting
      !E_REFNR type LVS_REFNR
    returning
      value(R_RESULT) type VBELN .
  class-methods GET_TKNUM
    importing
      !I_LGNUM type LGNUM optional
      !I_TKNUM type TKNUM optional
      !I_VBELN type VBELN optional
      !I_REFNR type LVS_REFNR optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !IS_DATA type ANY optional
    exporting
      !E_REFNR type LVS_REFNR
    returning
      value(R_RESULT) type TKNUM .
  class-methods IS_ALLOW_FREE_WORK
    importing
      !I_LGNUM type LGNUM
      !I_REFNR type LVS_REFNR
    exporting
      !ET_MESSAGES type TAB_BDCMSGCOLL
    returning
      value(R_RESULT) type FLAG .
  class-methods IS_ALLOW_PUL
    importing
      !I_LGNUM type LGNUM
      !I_REFNR type LVS_REFNR
      !I_LOCK type ZLOCK optional
    exporting
      !ET_MESSAGES type TAB_BDCMSGCOLL
    returning
      value(R_RESULT) type FLAG .
  class-methods IS_ALLOW_PUA
    importing
      !I_LGNUM type LGNUM
      !I_REFNR type LVS_REFNR
      !I_LOCK type ZLOCK optional
    exporting
      !ET_MESSAGES type TAB_BDCMSGCOLL
    returning
      value(R_RESULT) type FLAG .
  class-methods IS_IDOC_REFNR_SEND
    importing
      !I_LGNUM type LGNUM optional
      !I_REFNR type LVS_REFNR optional
      !I_VBELN type VBELN optional
      !I_ORDEM type ZWM_ZSEQ optional
      !IS_DATA type ANY optional
    exporting
      !E_IDOCNUM type EDI_DOCNUM
    returning
      value(R_RESULT) type FLAG .
  class-methods IS_IDOC_TO_SEND
    importing
      !I_LGNUM type LGNUM optional
      !I_TANUM type TANUM optional
      !IS_DATA type ANY optional
    exporting
      !E_IDOCNUM type EDI_DOCNUM
    returning
      value(R_RESULT) type FLAG .
  class-methods IS_PICKING
    importing
      !I_LGNUM type LGNUM optional
      !IS_DATA type ANY optional
      !I_LENUM type LENUM optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
    exporting
      !E_REFNR type LVS_REFNR
      !E_VBELN type VBELN
      !E_LENUM type LENUM
    returning
      value(R_RESULT) type FLAG .
  class-methods IS_REMONTADA
    importing
      !I_LGNUM type LGNUM optional
      !I_LENUM type LENUM optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !I_MATNR type MATNR optional
      !IS_DATA type ANY optional
      !I_LETYP type LVS_LETYP optional
      !IS_P1 type FLAG optional
      !IS_P2 type FLAG optional
    preferred parameter IS_DATA
    exporting
      !E_MATNR type MATNR
      !E_IS_P1 type FLAG
      !E_IS_P2 type FLAG
      !E_LENUM type LENUM
      !E_SISTER type LENUM
      !E_LETYP type LVS_LETYP
    returning
      value(R_RESULT) type FLAG .
  class-methods IS_SPLIT_REMONTADA
    importing
      !I_LGNUM type LGNUM optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !IS_LTAP type ANY optional
      !IT_LTAP type ANY TABLE optional
    returning
      value(R_RESULT) type FLAG .
  class-methods IS_SSCC
    importing
      !I_SSCC type ANY
    returning
      value(R_RESULT) type FLAG .
  class-methods R_LETYP_REMONTADA
    importing
      !I_LGNUM type LGNUM optional
    returning
      value(R_RESULT) type ZWM_R_LETYP .
  class-methods GET_COMPONETS
    importing
      !I_LGNUM type LGNUM optional
      !I_WERKS type WERKS_D optional
      !I_MATNR type MATNR
      !I_STLNR type STNUM optional
      !I_STLAL type STALT optional
      !I_MENGE_PROD type MENGE_D optional
    exporting
      !E_STLNR type STNUM
      !ET_MESSAGES type TAB_BDCMSGCOLL
      !ET_COMPONENTS type T_MATNR_PRD_COMPONENTS
      !E_STLAL type STLAL
    exceptions
      ERROR .
  class-methods GET_QTD_PAL_MATERIAL
    importing
      !I_LGNUM type LGNUM
      !I_MATNR type MATNR
      !I_MENGE type MENGE_D optional
      !I_MEINS type MEINS optional
      !I_REMONTADAS_DOUBLE type FLAG default 'X'
      !I_SPLIT_PICK type FLAG optional
    exporting
      !E_LHME1 type LHMEH1
      !E_MENGE type MENGE_D
      !E_MENGE_PICK type MENGE_D
      !E_MEINS type MEINS
      !E_LHMG1 type LVS_LHMNG1
      !E_LETYP type LVS_LETYP
      !E_REMONTADA type FLAG
    returning
      value(R_RESULT) type MENGE_D .
  class-methods IS_TR_MATNR_CREATED
    importing
      !I_LGNUM type LGNUM optional
      !I_MATNR type MATNR optional
      !IS_DATA type ANY optional
    exporting
      !E_TBNUM type TBNUM
      !ES_LTBK type LTBK
      !ET_LTBP type LEINT_LTBP_T
    returning
      value(R_RESULT) type FLAG .
  class-methods GET_LISTA_TECNICA
    importing
      !I_LGNUM type LGNUM optional
      !I_MATNR type MATNR optional
      !I_STLNR type STNUM optional
      !I_STLAL type STLAL optional
      !IS_DATA type ANY optional
    exporting
      !E_STLAL type STLAL
      !ET_MESSAGES type TAB_BDCMSGCOLL
    returning
      value(R_RESULT) type STNUM .
  class-methods IS_DELIVERY_COMPLETED
    importing
      !I_LGNUM type LGNUM optional
      !I_REFNR type LVS_REFNR optional
      !I_VBELN type VBELN optional
      !IS_DATA type ANY optional
      !I_GET_ALL type FLAG optional
    exporting
      !E_2STEP type FLAG
      !E_2SPART type FLAG
      !E_REFNR type LVS_REFNR
      !E_VBELN type VBELN
      !ET_QUANTITY_NEEDED type T_QUANTITY_NEEDED
      !E_CREATED type KOSTK
    returning
      value(R_RESULT) type FLAG .
  class-methods IS_GROUP_COMPLETED
    importing
      !I_LGNUM type LGNUM optional
      !I_REFNR type LVS_REFNR optional
      !I_VBELN type VBELN optional
      !IS_DATA type ANY optional
      !I_GET_ALL type FLAG optional
    exporting
      !E_2STEP type FLAG
      !E_2SPART type FLAG
      !E_REFNR type LVS_REFNR
      !ET_QUANTITY_NEEDED type T_QUANTITY_NEEDED
      !E_CREATED type KOSTK
    returning
      value(R_RESULT) type FLAG .
  class-methods SET_STATUS
    importing
      !I_IN_STATUS type CHAR1
    changing
      !C_OUT_STATUS type CHAR1 .
  class-methods IS_REABASTECIMENTO_RUNNING
    importing
      !I_LGNUM type LGNUM optional
      !I_REFNR type LVS_REFNR optional
      !IS_DATA type ANY optional
      !I_VLTYP type LTAP_VLTYP optional
    returning
      value(R_RESULT) type FLAG .
  class-methods COPACKING_PRODUCTION_ENTRY
    importing
      !I_LGNUM type LGNUM
      !I_EAN11 type EAN11 optional
      !I_MATNR type MATNR optional
      !I_MENGE type MENGE_D optional
      !I_VHILM type VHILM optional
    exporting
      !ET_MESSAGES type TAB_BDCMSGCOLL
    changing
      !C_EXIDV type EXIDV optional
      !C_EXIDV2 type EXIDV optional
    exceptions
      ERROR .
  class-methods IS_GROUP_LOADED
    importing
      !I_LGNUM type LGNUM optional
      !I_REFNR type LVS_REFNR optional
      !I_VBELN type VBELN optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !I_LENUM type LENUM optional
      !IS_DATA type ANY optional
    exporting
      !E_PORTA type LGTOR
      !E_LGTYP_PUL type LGTYP
      !E_PULMAO1 type LGPLA
      !E_PULMAO2 type LGPLA
      !ES_ZWM028 type ZWM028
      !E_TALAO type CHAR5
    returning
      value(R_RESULT) type FLAG .
  class-methods FREE_PUL
    importing
      !I_LGNUM type LGNUM optional
      !I_REFNR type LVS_REFNR optional
      !I_VBELN type VBELN optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !I_LENUM type LENUM optional
      !IS_DATA type ANY optional .
  class-methods FREE_DOOR
    importing
      !I_LGNUM type LGNUM optional
      !I_REFNR type LVS_REFNR optional
      !I_VBELN type VBELN optional
      !I_TANUM type TANUM optional
      !I_TAPOS type TAPOS optional
      !I_LENUM type LENUM optional
      !IS_DATA type ANY optional .
protected section.
private section.

  class-methods LENUM_EXTRACT
    importing
      !IS_DATA type ANY
    returning
      value(R_RESULT) type ZWM_TAB_LENUM .
  class-methods SECURE_CALL_STACK
    returning
      value(R_RESULT) type FLAG .
ENDCLASS.



CLASS Z_WM_CL_MANAGEMENT IMPLEMENTATION.


  METHOD copacking_production_entry.
    TYPES: BEGIN OF lty_string,
             ean11        TYPE c LENGTH 18,
             linha        TYPE c LENGTH 3,
             altura       TYPE c LENGTH 4,
             cor          TYPE c LENGTH 6,
             quantidade   TYPE c LENGTH 4,
             aufnr        TYPE c LENGTH 12,
             pal_completa TYPE c LENGTH 1,
           END OF lty_string.

    DATA: ls_string  TYPE lty_string,
          ls_message TYPE bdcmsgcoll.

    DATA: lv_in_string TYPE c LENGTH 48,
          lv_retorno   TYPE zedretorno_producao,
          lv_matnr     TYPE matnr,
          lv_menge     TYPE menge_d,
          lv_menge_i   TYPE i,
          lv_menge_pal TYPE menge_d,
          lv_sscc      TYPE exidv,
          lv_sscc2     TYPE exidv.

    IF NOT i_ean11 IS INITIAL.
      ls_string-ean11 = i_ean11.

      SELECT SINGLE matnr FROM marm
                          INTO lv_matnr
                          WHERE ean11 = i_ean11.

    ELSE.
      lv_matnr = i_matnr.

      SELECT SINGLE ean11 FROM marm
                          INTO ls_string-ean11
                          WHERE matnr = i_matnr AND
                                numtp = 'IC'.
    ENDIF.

    CHECK NOT ls_string-ean11 IS INITIAL AND
          NOT lv_matnr IS INITIAL.

    get_qtd_pal_material( EXPORTING i_lgnum = i_lgnum i_matnr = lv_matnr IMPORTING e_menge = lv_menge_pal ).

    IF NOT i_menge IS INITIAL.
      lv_menge = i_menge.
    ELSE.
      lv_menge = lv_menge_pal.
    ENDIF.


    lv_menge_i = lv_menge.
    WRITE lv_menge_i TO ls_string-quantidade.

    ls_string-cor = i_vhilm.

    ls_string-linha = 'GMA'.

    lv_in_string = ls_string.

    CALL FUNCTION 'ZWM_RFC_ENTRADA_PRODUCAO'
      EXPORTING
        in_string   = lv_in_string
        sscc        = c_exidv
        sscc2       = c_exidv2
      IMPORTING
        e_sscc      = lv_sscc
        e_sscc2     = lv_sscc2
        retorno     = lv_retorno
        et_messages = et_messages
      EXCEPTIONS
        exception   = 1
        OTHERS      = 2.

    IF lv_retorno <> 0.
      IF et_messages IS INITIAL.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '116'.
        ls_message-msgtyp = 'E'.
        APPEND ls_message TO et_messages.
      ENDIF.
      RAISE error.
    ENDIF.

    c_exidv  = lv_sscc.
    c_exidv2 = lv_sscc2.
  ENDMETHOD.


  METHOD free_door.
    DATA: lt_zwm002 TYPE TABLE OF zwm002.

    DATA: ls_zwm002 TYPE zwm002,
          ls_lagp   TYPE lagp,
          ls_lagpv  TYPE lagpv,
          ls_zwm028 TYPE zwm028.

    DATA: lv_lgnum   TYPE lgnum,
          lv_talao   TYPE char5,
          lv_porta   TYPE lgtor,
          lv_refnr   TYPE lvs_refnr,
          lv_pulmao  TYPE lgpla,
          lv_stype   TYPE lgtyp,
          lv_pulmao1 TYPE c LENGTH 14.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum i_lenum = i_lenum is_data = is_data ).
    lv_refnr = get_refnr(
                  i_lgnum = lv_lgnum
                  i_refnr = i_refnr
                  i_tanum = i_tanum
                  i_tapos = i_tapos
                  i_lenum = i_lenum
                  i_vbeln = i_vbeln
                  is_data = is_data
               ).


    CHECK z_wm_cl_management=>is_group_loaded(
            EXPORTING
              i_lgnum = lv_lgnum
              i_refnr = lv_refnr
            IMPORTING
              e_lgtyp_pul = lv_stype
              e_porta = lv_porta
              e_talao = lv_talao
              es_zwm028 = ls_zwm028
          ) EQ abap_true.

    CHECK NOT lv_porta IS INITIAL.

    SELECT SINGLE * FROM zwm002
                    INTO ls_zwm002
                    WHERE armazem = lv_lgnum AND
                          porta   = lv_porta.

    CHECK ls_zwm002-num_entrada EQ lv_talao.

    IF NOT ls_zwm002-pulmao_1 IS INITIAL OR
       NOT ls_zwm002-pulmao_2 IS INITIAL.
      z_wm_cl_management=>free_pul( i_lgnum = lv_lgnum i_refnr = lv_refnr ).

    ENDIF.


    ls_zwm002-estado = 'D'.

    CLEAR: ls_zwm002-bloqueio, ls_zwm002-pulmao_1, ls_zwm002-pulmao_2,
           ls_zwm002-num_entrada, ls_zwm002-user_name.

    MODIFY zwm002 FROM ls_zwm002.
    COMMIT WORK.
  ENDMETHOD.


  METHOD free_pul.
    DATA: ls_zwm002     TYPE zwm002,
          ls_zwm002_d   TYPE zwm002,
          ls_lagp       TYPE lagp,
          ls_lagpv      TYPE lagpv,
          ls_zwm028     TYPE zwm028,
          ls_zwm006_aux TYPE zwm006_aux.

    DATA: lv_lgnum   TYPE lgnum,
          lv_talao   TYPE char5,
          lv_porta   TYPE lgtor,
          lv_refnr   TYPE lvs_refnr,
          lv_pulmao  TYPE lgpla,
          lv_stype   TYPE lgtyp,
          lv_pulmao1 TYPE c LENGTH 14.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum i_lenum = i_lenum is_data = is_data ).
    lv_refnr = get_refnr(
                  i_lgnum = lv_lgnum
                  i_refnr = i_refnr
                  i_tanum = i_tanum
                  i_tapos = i_tapos
                  i_lenum = i_lenum
                  i_vbeln = i_vbeln
                  is_data = is_data
               ).


    CHECK z_wm_cl_management=>is_group_loaded(
            EXPORTING
              i_lgnum = lv_lgnum
              i_refnr = lv_refnr
            IMPORTING
              e_lgtyp_pul = lv_stype
              e_porta = lv_porta
              e_talao  = lv_talao
              es_zwm028 = ls_zwm028
          ) EQ abap_true.

    CHECK NOT lv_porta IS INITIAL.

    SELECT SINGLE * FROM zwm002
                    INTO ls_zwm002
                    WHERE armazem = lv_lgnum AND
                          porta   = lv_porta.

    CHECK sy-subrc EQ 0.

    CHECK lv_talao EQ ls_zwm002-num_entrada.


    CLEAR : lv_pulmao.
    DO 2 TIMES VARYING lv_pulmao FROM
                                      ls_zwm002-pulmao_1 NEXT
                                      ls_zwm002-pulmao_2.

      CHECK NOT lv_pulmao IS INITIAL.

      CLEAR ls_lagpv.
      SELECT SINGLE * FROM lagp
                      INTO ls_lagp
                      WHERE lgnum = lv_lgnum AND
                            lgtyp = lv_stype AND
                            lgpla = lv_pulmao.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_lagp TO ls_lagpv.
        IF lv_lgnum <> '150'.
          CALL FUNCTION 'ZWM_MPUL_FREE_PULMAO'
            EXPORTING
              i_lgnum   = lv_lgnum
              i_refnr   = lv_refnr
              is_zwm028 = ls_zwm028
            CHANGING
              cs_lagpv  = ls_lagpv.
        ELSE.
          ls_lagpv-brand = ' '.
        ENDIF.

        CALL FUNCTION 'L_LAGP_VERAENDERN'
          EXPORTING
            xlagpv = ls_lagpv.

        COMMIT WORK AND WAIT.
      ENDIF.


      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = lv_stype
          lgpla = lv_pulmao
        IMPORTING
          bin   = lv_pulmao1.
      COMMIT WORK.
    ENDDO.

** Limpa Pulmão da Porta
***********************************************************************
    CLEAR: ls_zwm002-pulmao_1, ls_zwm002-pulmao_2.

    ls_zwm002_d-armazem = ls_zwm002-armazem.
    ls_zwm002_d-porta   = ls_zwm002-porta.
    ls_zwm002_d-estado  = 'D'.

    MODIFY zwm002 FROM ls_zwm002_d.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.


    SELECT SINGLE * FROM zwm006_aux INTO ls_zwm006_aux
                    WHERE armazem     = lv_lgnum
                    AND   num_entrada = ls_zwm002-num_entrada
                    AND   finalizada  EQ space.

    IF sy-subrc = 0.
      UPDATE zwm006_aux SET carregada  = 'X'
                  WHERE armazem      = ls_zwm006_aux-armazem
                  AND   num_entrada  = ls_zwm006_aux-num_entrada
                  AND   n_transporte = ls_zwm006_aux-n_transporte
                  AND   data_reg     = ls_zwm006_aux-data_reg
                  AND   hora_reg     = ls_zwm006_aux-hora_reg.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_componets.
    DATA: lt_mast TYPE TABLE OF mast,
          lt_stko TYPE TABLE OF stko,
          lt_stpo TYPE TABLE OF stpo,
          lt_mlgn TYPE SORTED TABLE OF mlgn WITH UNIQUE KEY matnr.

    DATA: ls_mast      TYPE mast,
          ls_stko      TYPE stko,
          ls_stpo      TYPE stpo,
          ls_mlgn      TYPE mlgn,
          ls_message   TYPE bdcmsgcoll,
          ls_component TYPE matnr_prd_components.

    DATA: lv_lgnum      TYPE lgnum,
          lv_werks      TYPE werks_d,
          lv_lgort      TYPE lgort_d,
          lv_lines      TYPE sytabix,
          lv_menge_prod TYPE sytabix,
          lv_ratio      TYPE menge_d.

    CLEAR: et_messages, et_components, e_stlnr, e_stlal.

    lv_lgnum = i_lgnum.
    lv_werks = i_werks.

    CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
      EXPORTING
        i_matnr     = i_matnr
        i_get_lgnum = abap_true
        i_get_werks = abap_true
        i_get_lgort = abap_false
      IMPORTING
        et_messages = et_messages
      CHANGING
        c_lgnum     = lv_lgnum
        c_werks     = lv_werks
        c_lgort     = lv_lgort
      EXCEPTIONS
        error       = 1
        user_back   = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
      RAISE error.
    ENDIF.

    e_stlnr = get_lista_tecnica(
                  EXPORTING
                    i_lgnum = lv_lgnum
                    i_matnr = i_matnr
                    i_stlnr = i_stlnr
                    i_stlal = i_stlal
                  IMPORTING
                    e_stlal = e_stlal
                    et_messages = et_messages
                 ).

    IF NOT et_messages IS INITIAL.
      RAISE error.
    ENDIF.


** Retorna Lista Técnica
***********************************************************************

    SELECT * FROM mast
             INTO TABLE lt_mast
             WHERE matnr = i_matnr AND
                   werks = lv_werks.

    IF NOT e_stlal IS INITIAL.
      DELETE lt_mast WHERE stlal <> e_stlal.
    ENDIF.

    IF NOT e_stlnr IS INITIAL.
      DELETE lt_mast WHERE stlnr <> e_stlnr.
    ENDIF.

    IF lt_mast IS INITIAL.
      RAISE error.
    ENDIF.

** Retorna Informação de Lista Técnica
***********************************************************************
    SELECT * FROM stko
             INTO TABLE lt_stko
             FOR ALL ENTRIES IN lt_mast
             WHERE stlty = 'M' AND
                   stlnr = lt_mast-stlnr AND
                   stlal = lt_mast-stlal. "AND
**                   datuv <= sy-datum AND
**                   valid_to >= sy-datum.

    IF lt_stko IS INITIAL.
      RAISE error.
    ENDIF.

    DESCRIBE TABLE lt_stko LINES lv_lines.
    IF lv_lines > 1.
**    Mais que uma Lista Técnica determinada para o Material & no Centro &
      CLEAR: ls_message.
      ls_message-msgid = 'ZWM001'.
      ls_message-msgtyp = 'E'.
      ls_message-msgnr = '089'.
      ls_message-msgv1 = i_matnr.
      ls_message-msgv2 = lv_werks.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.

** Retorna Componentes
***********************************************************************
    CLEAR: ls_stko.
    READ TABLE lt_stko
          INTO ls_stko
          INDEX 1.

    SELECT * FROM stpo
             INTO TABLE lt_stpo
             WHERE stlty = ls_stko-stlty AND
                   stlnr = ls_stko-stlnr.

    IF lt_stpo IS INITIAL.
      RAISE error.
    ENDIF.

** Somente Componentes WM
***********************************************************************
    SELECT * FROM mlgn
             INTO TABLE lt_mlgn
             FOR ALL ENTRIES IN lt_stpo
             WHERE matnr = lt_stpo-idnrk AND
                   lgnum = lv_lgnum.

** Quantidades de Produção
***********************************************************************
    IF NOT i_menge_prod IS INITIAL.
      lv_menge_prod = i_menge_prod.
    ELSE.
      lv_menge_prod = ls_stko-bmeng.
    ENDIF.

    lv_ratio = lv_menge_prod / ls_stko-bmeng.

    LOOP AT lt_stpo INTO ls_stpo .
      LOOP AT lt_mlgn INTO ls_mlgn WHERE matnr = ls_stpo-idnrk.

        ls_component-lgnum      = ls_mlgn-lgnum.
        ls_component-matnr      = ls_mlgn-matnr.
        ls_component-werks      = lv_werks.
        ls_component-lgort      = lv_lgort.
        ls_component-menge      = ls_stpo-menge * lv_ratio.
        ls_component-menge_real = ls_stpo-menge.
        ls_component-meins      = ls_stpo-meins.

        ls_component-pal        = get_qtd_pal_material(
                                    EXPORTING
                                      i_lgnum      = ls_mlgn-lgnum
                                      i_matnr      = ls_mlgn-matnr
                                      i_menge      = ls_component-menge
                                      i_split_pick = 'X'
                                    IMPORTING
                                      e_letyp      = ls_component-letyp
                                      e_menge      = ls_component-pal_menge
                                      e_menge_pick = ls_component-pick_menge
                                      e_lhme1      = ls_component-lhme1
                                      e_lhmg1      = ls_component-lhmg1
                                      e_remontada  = ls_component-remontada
                                  ).

        APPEND ls_component TO et_components.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_lenum.
    FIELD-SYMBOLS: <lv_data> TYPE any.

    DATA: lt_ltap  TYPE TABLE OF ltap,
          lt_lgnum TYPE TABLE OF lgnum.

    DATA: ls_ltap TYPE ltap.

    DATA: lv_tanum TYPE tanum,
          lv_tapos TYPE tapos,
          lv_lenum TYPE lenum.

    CHECK secure_call_stack( ) EQ abap_true.

    IF NOT i_lenum IS INITIAL.
      APPEND i_lenum TO r_result.
      RETURN.
    ENDIF.

    r_result = lenum_extract( is_data ).

    e_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).

    DO 1 TIMES.
      CHECK NOT r_result IS INITIAL.
      CHECK e_lgnum IS INITIAL.

      e_lgnum = get_lgnum_from_lenum( it_lenum = r_result ).
    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.
      CHECK i_usepicking = abap_true.

      is_picking( EXPORTING i_lgnum = i_lgnum i_tanum = i_tanum i_tapos = i_tapos is_data = is_data  IMPORTING e_lenum = lv_lenum ).
      CHECK NOT lv_lenum IS INITIAL.

      APPEND lv_lenum TO r_result.
    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.
      CHECK i_useto = abap_true.
      lv_tanum = get_tanum( EXPORTING i_lgnum = i_lgnum i_tanum = i_tanum i_tapos = i_tapos is_data = is_data IMPORTING e_tapos = lv_tapos ).

      CHECK NOT e_lgnum IS INITIAL AND
            NOT lv_tanum IS INITIAL.

      SELECT * FROM ltap
               INTO TABLE lt_ltap
               WHERE lgnum = e_lgnum AND
                     tanum = lv_tanum.

      IF NOT lv_tapos IS INITIAL.
        DELETE lt_ltap WHERE tapos <> lv_tapos.
      ENDIF.
      CHECK NOT lt_ltap IS INITIAL.

      LOOP AT lt_ltap INTO ls_ltap.
        APPEND LINES OF get_lenum( is_data = ls_ltap i_useto = abap_false ) TO r_result.
      ENDLOOP.
    ENDDO.

    SORT r_result.
    DELETE ADJACENT DUPLICATES FROM r_result.
  ENDMETHOD.


  METHOD get_letyp.
    TYPES: BEGIN OF lty_i_data,
             lgnum TYPE lgnum,
             tanum TYPE tanum,
             tapos TYPE tapos,
             lenum TYPE lenum,
           END OF lty_i_data.

    DATA: lt_lenum TYPE TABLE OF lenum,
          lt_letyp TYPE TABLE OF lvs_letyp.

    DATA: ls_i_data TYPE lty_i_data.

    DATA: lv_lgnum TYPE lgnum,
          lv_lenum TYPE lenum,
          lv_tanum TYPE tanum,
          lv_lines TYPE sytabix.

    FIELD-SYMBOLS: <lv_data>   TYPE any,
                   <ls_i_data> TYPE any.

    CHECK secure_call_stack( ) EQ abap_true.


    IF NOT i_letyp IS INITIAL.
      r_result = i_letyp.
      RETURN.
    ENDIF.

    IF NOT is_data IS INITIAL.
      ASSIGN is_data TO <ls_i_data>.
    ELSE.
      ls_i_data-lgnum = i_lgnum.
      ls_i_data-tanum = i_tanum.
      ls_i_data-tapos = i_tapos.
      ls_i_data-lenum = i_lenum.
      ASSIGN ls_i_data TO <ls_i_data>.
    ENDIF.

    DO 1 TIMES.
      ASSIGN COMPONENT 'LETYP' OF STRUCTURE <ls_i_data> TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.

      ASSIGN COMPONENT 'LETY1' OF STRUCTURE <ls_i_data> TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.

    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      lt_lenum = get_lenum( i_lgnum = i_lgnum i_lenum = i_lenum is_data = <ls_i_data> ).
      CHECK NOT lt_lenum IS INITIAL.


      SELECT letyp FROM lein
                   INTO TABLE lt_letyp
                   FOR ALL ENTRIES IN lt_lenum
                   WHERE lenum = lt_lenum-table_line.

      SORT lt_letyp.
      DELETE ADJACENT DUPLICATES FROM lt_letyp.

      DESCRIBE TABLE lt_letyp LINES lv_lines.
      CHECK lv_lines EQ 1.

      READ TABLE lt_letyp
            INTO r_result
            INDEX 1.
    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = <ls_i_data> ).
      CHECK NOT lv_lgnum IS INITIAL.

      lv_tanum = get_tanum( i_lgnum = i_lgnum i_tanum = i_tanum i_tapos = i_tapos is_data = <ls_i_data> ).
      CHECK NOT lv_tanum IS INITIAL.


      SELECT letyp FROM ltap
                   INTO TABLE lt_letyp
                   WHERE lgnum = lv_lgnum AND
                         tanum = lv_tanum.


      SORT lt_letyp.
      DELETE ADJACENT DUPLICATES FROM lt_letyp.

      DESCRIBE TABLE lt_letyp LINES lv_lines.
      CHECK lv_lines EQ 1.

      READ TABLE lt_letyp
            INTO r_result
            INDEX 1.
    ENDDO.

  ENDMETHOD.


  METHOD get_lgnum.
    DATA: lt_lenum TYPE TABLE OF lenum,
          lt_lgnum TYPE TABLE OF lgnum.

    DATA: lv_lines TYPE sytabix.

    FIELD-SYMBOLS: <lv_data> TYPE any.

    CHECK secure_call_stack( ) eq abap_true.

    IF NOT i_lgnum IS INITIAL.
      r_result = i_lgnum.
      RETURN.
    ENDIF.


    DO 1 TIMES.
      ASSIGN COMPONENT 'LGNUM' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        RETURN.
      ENDIF.

      ASSIGN COMPONENT 'ARMAZEM' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        RETURN.
      ENDIF.
    ENDDO.

    IF r_result IS INITIAL.
      r_result = get_lgnum_from_lenum( EXPORTING i_lenum = i_lenum is_data = is_data ).


    ENDIF.
  ENDMETHOD.


  METHOD get_lgnum_from_lenum.
    DATA: lt_lgnum TYPE TABLE OF lgnum,
          lt_lenum TYPE TABLE OF lenum.

    DATA: lv_lines TYPE sytabix.

    lt_lenum = it_lenum.
    IF NOT i_lenum IS INITIAL.
      APPEND i_lenum TO lt_lenum.
    ENDIF.

    IF NOT is_data IS INITIAL.
      APPEND LINES OF lenum_extract( lt_lenum ) TO lt_lenum.
    ENDIF.

    DO 1 TIMES.
      CHECK NOT it_lenum IS INITIAL.
      SORT lt_lenum.
      DELETE ADJACENT DUPLICATES FROM lt_lenum.

      SELECT lgnum FROM lein
                   INTO TABLE lt_lgnum
                   FOR ALL ENTRIES IN it_lenum
                   WHERE lenum = it_lenum-table_line.
      SORT lt_lgnum.
      DELETE ADJACENT DUPLICATES FROM lt_lgnum.
      DELETE lt_lgnum WHERE table_line IS INITIAL.
      CHECK NOT lt_lgnum IS INITIAL.
      DESCRIBE TABLE lt_lgnum LINES lv_lines.
      CHECK lv_lines EQ 1.

      READ TABLE lt_lgnum
            INTO r_result
            INDEX 1.
    ENDDO.
  ENDMETHOD.


  METHOD get_lista_tecnica.
    DATA: lt_zwm067 TYPE TABLE OF zwm067.

    DATA: ls_zwm067  TYPE zwm067,
          ls_message TYPE bdcmsgcoll.

    DATA: lv_lgnum TYPE lgnum,
          lv_matnr TYPE matnr.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).
    lv_matnr = get_matnr( i_lgnum = lv_lgnum i_matnr = i_matnr is_data = is_data ).

    SELECT * FROM zwm067
             INTO TABLE lt_zwm067
             WHERE lgnum = lv_lgnum AND
                   matnr = lv_matnr AND
                   datbe >= sy-datum AND
                   datuv <= sy-datum AND
                   deleted = abap_false.

    IF sy-subrc <> 0.
**    Nenhuma lista técnica válida para Material &
      ls_message-msgid = 'ZWM001'.
      ls_message-msgnr = '094'.
      ls_message-msgtyp = 'E'.
      ls_message-msgv1 = lv_matnr.
      APPEND ls_message TO et_messages.
      EXIT.
    ENDIF.

    IF NOT i_stlnr IS INITIAL.
      DELETE lt_zwm067 WHERE stlnr <> i_stlnr.

      IF lt_zwm067 IS INITIAL.
**      Lista técnica & inválida para Material &
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '095'.
        ls_message-msgtyp = 'E'.
        ls_message-msgv1 = i_stlnr.
        ls_message-msgv2 = lv_matnr.
        APPEND ls_message TO et_messages.
        EXIT.
      ENDIF.
    ENDIF.

    IF NOT i_stlal IS INITIAL.
      DELETE lt_zwm067 WHERE stlal <> i_stlal.
      IF lt_zwm067 IS INITIAL.
**      Lista técnica & alternativa & inválida para Material &
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '095'.
        ls_message-msgtyp = 'E'.
        ls_message-msgv1 = i_stlnr.
        ls_message-msgv2 = i_stlal.
        ls_message-msgv3 = lv_matnr.
        APPEND ls_message TO et_messages.
        EXIT.
      ENDIF.
    ENDIF.

    READ TABLE lt_zwm067
          INTO ls_zwm067
          INDEX 1.

    r_result = ls_zwm067-stlnr.
    e_stlal  = ls_zwm067-stlal.
  ENDMETHOD.


  METHOD get_matnr.
    DATA: lt_lenum TYPE TABLE OF lenum,
          lt_lgnum TYPE TABLE OF lgnum.

    DATA: ls_mlgn TYPE mlgn.

    DATA: lv_lines TYPE sytabix,
          lv_lgnum TYPE lgnum.

    FIELD-SYMBOLS: <lv_data> TYPE any.


    CHECK secure_call_stack( ) EQ abap_true.

    IF NOT i_matnr IS INITIAL.
      r_result = i_matnr.
    ENDIF.


    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      ASSIGN COMPONENT 'MATNR' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
      ENDIF.

      ASSIGN COMPONENT 'MATERIAL' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
      ENDIF.
    ENDDO.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).

    DO 1 TIMES.
      CHECK NOT lv_lgnum IS INITIAL AND
            NOT r_result IS INITIAL.


      SELECT SINGLE * FROM mlgn
                      INTO ls_mlgn
                      WHERE matnr = r_result AND
                            lgnum = lv_lgnum.

      CHECK sy-subrc EQ 0.

      IF NOT ls_mlgn-lety1 IS INITIAL.
        e_letyp = ls_mlgn-lety1.
        e_lhmng = ls_mlgn-lhmg1.
        e_lhmen = ls_mlgn-lhme1.
      ENDIF.

    ENDDO.
  ENDMETHOD.


  METHOD get_qtd_pal_material.
    DATA: ls_mlgn TYPE mlgn.

    DATA: lv_menge TYPE menge_d.

    SELECT SINGLE * FROM mlgn
                    INTO ls_mlgn
                    WHERE matnr = i_matnr AND
                          lgnum = i_lgnum.
    CHECK sy-subrc EQ 0.


    e_menge = ls_mlgn-lhmg1.
    e_meins = ls_mlgn-lhme1.

    e_lhmg1 = ls_mlgn-lhmg1.
    e_lhme1 = ls_mlgn-lhme1.

    IF NOT i_menge IS INITIAL.
      lv_menge = i_menge.
    ELSE.
      lv_menge = ls_mlgn-lhmg1.
    ENDIF.

    r_result = lv_menge / ls_mlgn-lhmg1.

    IF i_split_pick = 'X'.
      r_result = floor( r_result ).

      e_menge = e_menge * r_result.

      e_menge_pick = lv_menge - e_menge.

    ELSE.
      r_result = ceil( r_result ).

      e_menge = e_menge * r_result.
    ENDIF.

    e_remontada = is_remontada( EXPORTING is_data = ls_mlgn IMPORTING e_letyp = e_letyp ).

    IF i_remontadas_double EQ abap_true AND
       e_remontada EQ abap_true.
      r_result = r_result * 2.
      e_menge = e_menge * 2.
    ENDIF.
  ENDMETHOD.


  METHOD get_refnr.
    TYPES: BEGIN OF lty_i_data,
             lgnum TYPE lgnum,
             tanum TYPE tanum,
             tapos TYPE tapos,
             lenum TYPE lenum,
           END OF lty_i_data.

    DATA: lv_tknum TYPE tknum.

    DATA: ls_i_data TYPE lty_i_data.

    FIELD-SYMBOLS: <lv_data>   TYPE any,
                   <ls_i_data> TYPE any.

    CHECK secure_call_stack( ) EQ abap_true.

    IF NOT i_refnr IS INITIAL.
      r_result = i_refnr.
      RETURN.
    ENDIF.


    IF NOT is_data IS INITIAL.
      ASSIGN is_data TO <ls_i_data>.
    ELSE.
      ls_i_data-lgnum = i_lgnum.
      ls_i_data-tanum = i_tanum.
      ls_i_data-tapos = i_tapos.
      ls_i_data-lenum = i_lenum.
      ASSIGN ls_i_data TO <ls_i_data>.
    ENDIF.


    DO 1 TIMES.
      ASSIGN COMPONENT 'REFNR' OF STRUCTURE <ls_i_data> TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.

      ASSIGN COMPONENT 'GRUPO' OF STRUCTURE <ls_i_data> TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.
    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.
      get_vbeln( EXPORTING i_lgnum = i_lgnum i_vbeln = i_vbeln i_tanum = i_tanum i_tapos = i_tapos i_lenum = i_lenum is_data = <ls_i_data> IMPORTING e_refnr = r_result ).
    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.
      get_tanum( EXPORTING i_lgnum = i_lgnum i_tanum = i_tanum i_tapos = i_tapos is_data = <ls_i_data> IMPORTING e_refnr = r_result ).
    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      e_tknum = get_tknum( EXPORTING i_lgnum = i_lgnum i_tknum = i_tknum is_data = <ls_i_data> IMPORTING e_refnr = r_result ).
    ENDDO.
  ENDMETHOD.


  METHOD get_tanum.
    TYPES: BEGIN OF lty_ltap,
             tabum TYPE tanum,
             tapos TYPE tapos,
             vlenr TYPE ltap_vlenr,
             nlenr TYPE ltap_nlenr,
           END OF lty_ltap.

    DATA: lt_ltap TYPE TABLE OF lty_ltap.

    DATA: ls_ltap TYPE lty_ltap.

    DATA: lv_lgnum TYPE lgnum,
          lv_vbeln TYPE vbeln,
          lv_benum TYPE lvs_benum.

    FIELD-SYMBOLS: <lv_data> TYPE any.

    CLEAR: e_tapos, e_refnr.

    CHECK secure_call_stack( ) EQ abap_true.

    IF NOT i_tanum IS INITIAL.
      r_result = i_tanum.
    ENDIF.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'TANUM' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.

      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'TO' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.
    ENDDO.

    CHECK NOT r_result IS INITIAL.

    IF NOT i_tapos IS INITIAL.
      e_tapos = i_tapos.
    ELSE.
      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'TAPOS' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        e_tapos = <lv_data>.
      ENDIF.
    ENDIF.

    DO  1 TIMES.
      lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).
      CHECK NOT lv_lgnum IS INITIAL.

      SELECT SINGLE refnr benum FROM ltak
                                INTO ( e_refnr, lv_benum )
                                WHERE lgnum = lv_lgnum AND
                                      tanum = r_result.

      CHECK sy-subrc EQ 0.

      SELECT tanum tapos vlenr
             nlenr             FROM ltap
                               INTO TABLE lt_ltap
                               WHERE lgnum = lv_lgnum AND
                                     tanum = r_result.

      IF NOT e_tapos IS INITIAL.
        DELETE lt_ltap WHERE tapos <> e_tapos.
      ENDIF.

      LOOP AT lt_ltap INTO ls_ltap.
        APPEND LINES OF lenum_extract( ls_ltap ) TO et_lenum.
      ENDLOOP.

      SORT et_lenum.
      DELETE ADJACENT DUPLICATES FROM et_lenum.


      CHECK e_refnr IS INITIAL.

      IF NOT lv_benum IS INITIAL.
        DO 1 TIMES.

          SELECT SINGLE sammg FROM vbsk
                              INTO e_refnr
                              WHERE sammg = lv_benum.
          CHECK e_refnr IS INITIAL.

          SELECT SINGLE vbeln FROM likp
                              INTO lv_vbeln
                              WHERE vbeln = lv_benum.
          CHECK NOT lv_vbeln IS INITIAL.
          e_vbeln = get_vbeln( EXPORTING i_lgnum = i_lgnum i_vbeln = lv_vbeln IMPORTING e_refnr = e_refnr ).
        ENDDO.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD get_tknum.
    DATA: lt_t311a TYPE TABLE OF t311a,
          lt_refnr TYPE TABLE OF lvs_refnr,
          lt_tknum TYPE TABLE OF tknum,
          lt_vbeln TYPE TABLE OF vbeln.

    DATA: ls_t311a TYPE t311a.

    DATA: lv_lgnum TYPE lgnum,
          lv_vbeln TYPE vbeln,
          lv_refnr TYPE lvs_refnr,
          lv_lines TYPE sytabix.

    FIELD-SYMBOLS: <lv_data> TYPE any.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).

    IF NOT i_tknum IS INITIAL.
      r_result = i_tknum.
    ENDIF.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      ASSIGN COMPONENT 'TKNUM' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.

      ASSIGN COMPONENT 'N_TRANSPORTE' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.
    ENDDO.


    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      lv_vbeln = get_vbeln( i_lgnum = lv_lgnum i_vbeln = i_vbeln i_tanum = i_tanum i_tapos = i_tapos is_data = is_data ).
      CHECK NOT lv_vbeln IS INITIAL.

      SELECT SINGLE tknum FROM vttp
                          INTO r_result
                          WHERE vbeln = lv_vbeln.

    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      lv_refnr = get_refnr( i_lgnum = lv_lgnum i_tknum = i_tknum i_refnr = i_refnr i_vbeln = i_vbeln i_tanum = i_tanum i_tapos = i_tapos is_data = is_data ).
      CHECK NOT lv_refnr IS INITIAL.

      SELECT * FROM t311a
               INTO TABLE lt_t311a
               WHERE lgnum = lv_lgnum AND
                     refnr = lv_refnr.

      CHECK NOT lt_t311a IS INITIAL.

      LOOP AT lt_t311a INTO ls_t311a.
        lv_vbeln = get_vbeln( i_lgnum = lv_lgnum is_data = ls_t311a ).
        CHECK NOT lv_vbeln IS INITIAL.
        APPEND lv_vbeln TO lt_vbeln.
      ENDLOOP.

      SORT lt_vbeln.
      DELETE ADJACENT DUPLICATES FROM lt_vbeln.

      SELECT tknum FROM vttp
                   INTO TABLE lt_tknum
                   FOR ALL ENTRIES IN lt_vbeln
                   WHERE vbeln = lt_vbeln-table_line.

      CHECK sy-subrc EQ 0.
      SORT lt_tknum.
      DELETE ADJACENT DUPLICATES FROM lt_tknum.
      DESCRIBE TABLE lt_tknum LINES lv_lines.
      CHECK lv_lines EQ 1.

      READ TABLE lt_tknum
            INTO r_result
            INDEX 1.

    ENDDO.


    CHECK NOT r_result IS INITIAL.

** Grupo
***********************************************************************
    IF NOT lv_refnr IS INITIAL.
      e_refnr = lv_refnr.
    ELSE.
      DO 1 TIMES.
        SELECT vbeln FROM vttp
                     INTO TABLE lt_vbeln
                     WHERE tknum = r_result.
        CHECK sy-subrc EQ 0.

        LOOP AT lt_vbeln INTO lv_vbeln.

          lv_refnr = get_refnr( i_lgnum = lv_lgnum i_vbeln = lv_vbeln ).
          APPEND lv_refnr TO lt_refnr.
        ENDLOOP.

        SORT lt_refnr.
        DELETE ADJACENT DUPLICATES FROM lt_refnr.
        DESCRIBE TABLE lt_refnr LINES lv_lines.
        IF lv_lines EQ 1.
          READ TABLE lt_refnr
                INTO e_refnr
                INDEX 1.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD get_vbeln.
    TYPES: BEGIN OF lty_i_data,
             lgnum TYPE lgnum,
             tanum TYPE tanum,
             tapos TYPE tapos,
             bdatu TYPE ltak_bdatu,
             bzeit TYPE ltak_bzeit,
             lenum TYPE lenum,
           END OF lty_i_data.

    DATA: lt_ltap   TYPE TABLE OF ltap,
          lt_lenum  TYPE TABLE OF lenum,
          lt_i_data TYPE TABLE OF lty_i_data,
          lt_result TYPE TABLE OF vbeln.

    DATA: ls_ltap   TYPE ltap,
          ls_i_data TYPE lty_i_data.

    DATA: lv_lgnum TYPE lgnum,
          lv_tanum TYPE tanum,
          lv_tapos TYPE tapos,
          lv_betyp TYPE lvs_betyp,
          lv_benum TYPE lvs_benum,
          lv_lines TYPE sytabix.

    FIELD-SYMBOLS: <lv_data>   TYPE any,
                   <ls_i_data> TYPE any.

    CHECK secure_call_stack( ) EQ abap_true.

    CLEAR: e_refnr.

    IF NOT i_vbeln IS INITIAL.
      r_result = i_vbeln.
    ENDIF.

    IF NOT is_data IS INITIAL.
      ASSIGN is_data TO <ls_i_data>.
    ELSE.
      ls_i_data-lgnum = i_lgnum.
      ls_i_data-tanum = i_tanum.
      ls_i_data-tapos = i_tapos.
      ls_i_data-lenum = i_lenum.
      ASSIGN ls_i_data TO <ls_i_data>.
    ENDIF.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum i_lenum = i_lenum is_data = <ls_i_data> ).


    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      ASSIGN COMPONENT 'VBELN' OF STRUCTURE <ls_i_data> TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.

      ASSIGN COMPONENT 'RBNUM' OF STRUCTURE <ls_i_data> TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.

      ASSIGN COMPONENT 'REMESSA' OF STRUCTURE <ls_i_data> TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          NOT <lv_data> IS INITIAL.
        r_result = <lv_data>.
        EXIT.
      ENDIF.
    ENDDO.

    DO  1 TIMES.
      CHECK r_result IS INITIAL.

      lv_tanum = get_tanum( EXPORTING i_lgnum = lv_lgnum i_tanum = i_tanum i_tapos = i_tapos is_data = <ls_i_data> IMPORTING e_tapos = lv_tapos e_vbeln = r_result e_refnr = e_refnr ).
      CHECK NOT lv_lgnum IS INITIAL AND
            NOT lv_tanum IS INITIAL.

      SELECT SINGLE  vbeln betyp benum FROM ltak
                                       INTO ( r_result, lv_betyp, lv_benum )
                                       WHERE lgnum = lv_lgnum AND
                                             tanum = lv_tanum.
      CHECK r_result IS INITIAL.

      IF lv_betyp EQ 'L' AND
         NOT lv_benum IS INITIAL.
        r_result = lv_benum.
      ELSEIF lv_betyp EQ 'I' AND
             NOT lv_benum IS INITIAL.

        SELECT SINGLE vbeln FROM likp
                            INTO r_result
                            WHERE vbeln = lv_benum.

      ENDIF.
      CHECK r_result IS INITIAL.

      SELECT * FROM ltap
               INTO TABLE lt_ltap
               WHERE lgnum = lv_lgnum AND
                     tanum = lv_tanum.

      IF NOT lv_tapos IS INITIAL.
        DELETE lt_ltap WHERE tapos <> lv_tapos.
      ENDIF.

      CHECK NOT lt_ltap IS INITIAL.

      DELETE lt_ltap WHERE vbeln IS INITIAL.
      SORT lt_ltap BY vbeln.
      DELETE ADJACENT DUPLICATES FROM lt_ltap COMPARING vbeln.
      DESCRIBE TABLE lt_ltap LINES lv_lines.
      CHECK lv_lines EQ 1.

      CLEAR: ls_ltap.
      READ TABLE lt_ltap
            INTO ls_ltap
            INDEX 1.

      r_result = ls_ltap-vbeln.
    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.
      is_picking( EXPORTING i_lgnum = lv_lgnum i_lenum = i_lenum is_data = <ls_i_data> IMPORTING e_vbeln = r_result ).
    ENDDO.

    DO 1 TIMES.
      CHECK r_result IS INITIAL.

      CHECK lv_tanum IS INITIAL. " ignora se ja tiver tanum

      CHECK NOT lv_lgnum IS INITIAL.

      lt_lenum = get_lenum( i_lgnum = lv_lgnum i_lenum = i_lenum is_data =  <ls_i_data> ).
      CHECK NOT lt_lenum IS INITIAL.

      CLEAR: lt_i_data.

      SELECT a~lgnum a~tanum a~tapos
             b~bdatu b~bzeit         FROM ltap AS a
                                     INNER JOIN ltak AS b
                                        ON a~lgnum = b~lgnum AND
                                           a~tanum = b~tanum
                                     APPENDING TABLE lt_i_data
                                     FOR ALL ENTRIES IN lt_lenum
                                     WHERE a~lgnum = lv_lgnum AND
                                           a~vlenr = lt_lenum-table_line.

      SELECT a~lgnum a~tanum a~tapos
             b~bdatu b~bzeit         FROM ltap AS a
                                     INNER JOIN ltak AS b
                                        ON a~lgnum = b~lgnum AND
                                           a~tanum = b~tanum
                                     APPENDING TABLE lt_i_data
                                     FOR ALL ENTRIES IN lt_lenum
                                     WHERE a~lgnum = lv_lgnum AND
                                           a~nlenr = lt_lenum-table_line.

      SORT lt_i_data BY bdatu DESCENDING bzeit DESCENDING.

      LOOP AT lt_i_data INTO ls_i_data.
        r_result = get_vbeln( i_lgnum = lv_lgnum is_data = ls_i_data ).
        CHECK not r_result is INITIAL.
        EXIT.
      ENDLOOP.
    ENDDO.

    CHECK NOT r_result IS INITIAL.

    SELECT SINGLE sammg FROM vbss
                        INTO e_refnr
                        WHERE vbeln = r_result.
  ENDMETHOD.


  METHOD is_allow_free_work.
    DATA: lt_zwm078  TYPE TABLE OF zwm078,
          lt_ltak    TYPE TABLE OF ltak,
          lt_ltap    TYPE TABLE OF ltap,
          ls_zwm078  TYPE zwm078,
          ls_zwm028  TYPE zwm028,
          ls_ltap    TYPE ltap,
          ls_ltak    TYPE ltak,
          ls_message TYPE bdcmsgcoll.

    CLEAR: et_messages, r_result.

** Validar OTs
**********************************************************************
    SELECT SINGLE * FROM zwm078
                    INTO ls_zwm078
                    WHERE lgnum   = i_lgnum AND
                          refnr   = i_refnr.
    IF sy-subrc <> 0.
      r_result = abap_false.

      " Grupo & sem tarefas para libertar do automático!
      CLEAR: ls_message.
      ls_message-msgid = 'ZWM001'.
      ls_message-msgtyp = 'E'.
      ls_message-msgnr = '153'.
      ls_message-msgv1 = i_refnr.
      APPEND ls_message TO et_messages.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM zwm028
                    INTO ls_zwm028
                    WHERE lgnum = i_lgnum AND
                          refnr = i_refnr AND
                          remessa = ''.

    IF ( ls_zwm028-st_pul IS INITIAL AND ls_zwm028-st_dck IS INITIAL ) OR ls_zwm028-st_pul = 'AUT'.
      r_result = abap_false.

      " Grupo & sem pulmão ou gravítica atribuida!
      CLEAR: ls_message.
      ls_message-msgid = 'ZWM001'.
      ls_message-msgtyp = 'E'.
      ls_message-msgnr = '152'.
      ls_message-msgv1 = i_refnr.
      APPEND ls_message TO et_messages.
      RETURN.
    ENDIF.

*    SELECT * FROM ltak
*                 INTO TABLE lt_ltak
*                 WHERE lgnum = i_lgnum AND
*                       benum = i_refnr AND
*                       betyp = 'Z' AND
*                       kquit = abap_false.

    SELECT * FROM ltak
             APPENDING TABLE lt_ltak
             WHERE lgnum = i_lgnum AND
                   refnr = i_refnr AND
                   kquit = abap_false.

    IF lt_ltak[] IS NOT INITIAL.
      SELECT * FROM ltap
               INTO TABLE lt_ltap
               FOR ALL ENTRIES IN lt_ltak
               WHERE lgnum = lt_ltak-lgnum AND
                     tanum = lt_ltak-tanum.

      " Paletes de Picking
      DELETE lt_ltap WHERE vltyp = 'PCK'.
      DELETE lt_ltap WHERE vltyp = 'PKB'.
      DELETE lt_ltap WHERE nltyp = 'REP'.
      DELETE lt_ltap WHERE nltyp = 'INC'.

      IF ls_zwm028-st_pul <> 'PUA'.
        DELETE lt_ltap WHERE vltyp <> 'AUT'.
      ENDIF.
    ENDIF.

    LOOP AT lt_ltak INTO ls_ltak.

      LOOP AT lt_ltap INTO ls_ltap WHERE tanum = ls_ltak-tanum.

        IF ls_ltap-vbeln IS INITIAL AND ls_ltak-betyp EQ 'L'.
          ls_ltap-vbeln = ls_ltak-benum.
        ENDIF.

        IF ls_ltap-vbeln IS INITIAL AND NOT ls_ltap-vlenr IS INITIAL.
          SELECT SINGLE remessa FROM zwm026
                                INTO ls_ltap-vbeln
                                WHERE armazem = ls_ltap-lgnum AND
                                      sscc    = ls_ltap-vlenr.
        ENDIF.

        CLEAR: ls_zwm028.

        CALL FUNCTION 'ZWM_CHECK_TO_EXP'
          EXPORTING
            i_lgnum   = i_lgnum
            i_refnr   = i_refnr
            i_vbeln   = ls_ltap-vbeln
          IMPORTING
            cs_zwm028 = ls_zwm028.

        CHECK ls_zwm028 IS NOT INITIAL.

        SELECT SINGLE *
           FROM zwm078 INTO ls_zwm078
           WHERE lgnum = ls_ltap-lgnum
           AND   tanum = ls_ltap-tanum
           AND   tapos = ls_ltap-tapos.

        IF sy-subrc <> 0.
          r_result = abap_false.

          " Falta enviar IDOC para OT &!
          CLEAR: ls_message.
          ls_message-msgid = 'ZWM001'.
          ls_message-msgtyp = 'E'.
          ls_message-msgnr = '140'.
          ls_message-msgv1 = ls_ltap-tanum.
          APPEND ls_message TO et_messages.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    IF et_messages[] IS NOT INITIAL.
      RETURN.
    ENDIF.

** Validar reserva
**********************************************************************
    SELECT * FROM zwm078
                    INTO TABLE lt_zwm078
                    WHERE lgnum   = i_lgnum AND
                          refnr   = i_refnr AND
                          reserva = abap_false.

    DELETE lt_zwm078 WHERE estorno IS NOT INITIAL.

    LOOP AT lt_zwm078 INTO ls_zwm078.
**    Falta confirmação de reserva de WCS da OT - &
      CLEAR: ls_message.
      ls_message-msgid  = 'ZWM001'.
      ls_message-msgtyp = 'E'.
      ls_message-msgnr  = '154'.
      ls_message-msgv1  = ls_zwm078-tanum.
      APPEND ls_message TO et_messages.
    ENDLOOP.

    IF sy-subrc = 0.
      r_result = abap_false.
      RETURN.
    ENDIF.


    IF z_wm_cl_management=>is_idoc_refnr_send( i_lgnum = i_lgnum i_refnr = i_refnr ) EQ abap_true.

      r_result = abap_false.

**    Carga & já liberada
      CLEAR: ls_message.
      ls_message-msgid = 'ZWM001'.
      ls_message-msgtyp = 'E'.
      ls_message-msgnr = '086'.
      ls_message-msgv1 = i_refnr.
      APPEND ls_message TO et_messages.
      RETURN.
    ENDIF.

    r_result = abap_true.
  ENDMETHOD.


  METHOD is_allow_pua.
    TYPES: BEGIN OF lty_picking,
             n_pal_picking TYPE nrto,
             peso          TYPE brgew,
           END OF lty_picking.

    DATA: lt_zwm026    TYPE TABLE OF zwm026,
          lt_mara      TYPE HASHED TABLE OF mara WITH UNIQUE KEY matnr,
          lt_picking   TYPE SORTED TABLE OF lty_picking WITH UNIQUE KEY n_pal_picking,
          lt_ordem     TYPE TABLE OF numc2,
          lt_ltak      TYPE TABLE OF ltak,
          lt_ltap      TYPE TABLE OF ltap,
          lt_ltap_calc TYPE TABLE OF ltap.


    DATA: ls_zwm026  TYPE zwm026,
          ls_mara    TYPE mara,
          ls_ltap    TYPE ltap,
          ls_picking TYPE lty_picking,
          ls_message TYPE bdcmsgcoll.


    DATA: lv_max_pua_pal    TYPE i VALUE 10,
          lv_min_pua_peso   TYPE f VALUE 10,
          lv_max_pua_ordem  TYPE n LENGTH 2,
          lv_max_pua_manual TYPE n LENGTH 2,

          lv_lgnum          TYPE lgnum,
          lv_refnr          TYPE lvs_refnr,
          lv_total_paletes  TYPE n LENGTH 2,
          lv_ordem          TYPE n LENGTH 2,
          lv_manual         TYPE sytabix,
          lv_pal_num        TYPE sytabix.


    FIELD-SYMBOLS:  <ls_picking> TYPE lty_picking.

    CLEAR: r_result.

    lv_lgnum = i_lgnum.
    lv_refnr = i_refnr.


** Parametros
***********************************************************************
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = lv_lgnum
        i_processo  = 'EXPED_AUT'
        i_parametro = 'EXP_GRAV_MAXPAL'
      IMPORTING
        e_valor     = lv_max_pua_pal
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      lv_max_pua_pal = 99.
    ENDIF.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = lv_lgnum
        i_processo  = 'EXPED_AUT'
        i_parametro = 'PESO_MIN_GRAVITICA'
      IMPORTING
        e_valor     = lv_min_pua_peso
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      lv_min_pua_peso = 0.
    ENDIF.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = lv_lgnum
        i_processo  = 'EXPED_AUT'
        i_parametro = 'EXP_GRAV_MAXSEQ'
      IMPORTING
        e_valor     = lv_max_pua_ordem
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      lv_max_pua_ordem = 99.
    ENDIF.


    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = lv_lgnum
        i_processo  = 'EXPED_AUT'
        i_parametro = 'EXP_GRAV_MAXMAN'
      IMPORTING
        e_valor     = lv_max_pua_manual
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      lv_max_pua_manual = 99.
    ENDIF.

** Maximo de Sequencias
***********************************************************************
    DO 1 TIMES.
      SELECT ordem FROM zwm028
                   INTO TABLE lt_ordem
                   WHERE lgnum = lv_lgnum AND
                         refnr = lv_refnr.

      CHECK sy-subrc EQ 0.

      SORT lt_ordem DESCENDING.
      READ TABLE lt_ordem
            INTO lv_ordem
            INDEX 1.

      IF lv_ordem > lv_max_pua_ordem.
        r_result = abap_false.

        CLEAR: ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr = '080'.
        APPEND ls_message TO et_messages.
        RETURN.
      ENDIF.

    ENDDO.


** Maximo de Cargas Manual
***********************************************************************
    DO 1 TIMES.
      SELECT * FROM ltak
               INTO TABLE lt_ltak
               WHERE lgnum = lv_lgnum AND
                     refnr = lv_refnr.

      CHECK sy-subrc EQ 0.

      SELECT * FROM ltap
               INTO TABLE lt_ltap
               FOR ALL ENTRIES IN lt_ltak
               WHERE lgnum = lt_ltak-lgnum AND
                     tanum = lt_ltak-tanum AND
                     pquit = abap_false.

      DELETE lt_ltap WHERE vorga = 'ST' OR vorga = 'SL'.

      lt_ltap_calc = lt_ltap.
      DELETE lt_ltap_calc WHERE vltyp = 'AUT'.

      DESCRIBE TABLE lt_ltap_calc LINES lv_manual.

      IF lv_manual > lv_max_pua_manual.
        r_result = abap_false.

        CLEAR: ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr = '081'.
        APPEND ls_message TO et_messages.
        RETURN.
      ENDIF.
    ENDDO.

** Tipo de Paletes
***********************************************************************
    DO 1 TIMES.
      lt_ltap_calc = lt_ltap.
      DELETE lt_ltap_calc WHERE letyp <> 'P1' AND letyp <> 'P12' AND letyp <> 'P13'.

      DESCRIBE TABLE lt_ltap_calc LINES lv_pal_num.

      IF NOT lt_ltap_calc IS INITIAL.
        r_result = abap_false.

        CLEAR: ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr = '084'.
        ls_message-msgv1 = '(P1, P12, P13)'.
        APPEND ls_message TO et_messages.
        RETURN.
      ENDIF.
    ENDDO.

** maximo de paletes
**********************************************************************
    DO 1 TIMES.
      SELECT SINGLE total_paletes FROM zwm028
                                  INTO lv_total_paletes
                                  WHERE lgnum = lv_lgnum AND
                                        refnr = lv_refnr.

      IF sy-subrc <> 0.
        r_result = abap_false.
        RETURN.
      ENDIF.

      IF lv_total_paletes > lv_max_pua_pal.
        r_result = abap_false.

        CLEAR: ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr = '082'.
        APPEND ls_message TO et_messages.
        RETURN.
      ENDIF.

    ENDDO.


** minimo de peso picking
***********************************************************************
    DO 1 TIMES.
      SELECT * FROM zwm026
               INTO TABLE lt_zwm026
               WHERE armazem = i_lgnum AND
                     grupo   = i_refnr.

      CHECK sy-subrc EQ 0.

      SELECT * FROM  mara
               INTO TABLE lt_mara
               FOR ALL ENTRIES IN lt_zwm026
               WHERE matnr = lt_zwm026-material.

      LOOP AT lt_zwm026 INTO ls_zwm026.

        CLEAR: ls_mara.
        READ TABLE lt_mara
              INTO ls_mara
              WITH TABLE KEY matnr = ls_zwm026-material.
        CHECK sy-subrc EQ 0.

        UNASSIGN: <ls_picking>.
        READ TABLE lt_picking
              ASSIGNING <ls_picking>
              WITH TABLE KEY n_pal_picking = ls_zwm026-n_pal_picking.

        IF sy-subrc <> 0.
          ls_picking-n_pal_picking = ls_zwm026-n_pal_picking.
          ls_picking-peso = 0.
          INSERT ls_picking INTO TABLE lt_picking ASSIGNING <ls_picking>.
        ENDIF.

        CHECK <ls_picking> IS ASSIGNED.

        <ls_picking>-peso = <ls_picking>-peso + ( ls_zwm026-quantidade * ls_mara-brgew ).
      ENDLOOP.

      LOOP AT lt_picking TRANSPORTING NO FIELDS WHERE peso > 0 AND
                                                      peso < lv_min_pua_peso.
        r_result = abap_false.

        CLEAR: ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr = '083'.
        APPEND ls_message TO et_messages.
        RETURN.
      ENDLOOP.
    ENDDO.


** Valida Picking
***********************************************************************
    DO 1 TIMES.
      IF NOT i_lock IS INITIAL.
        CHECK i_lock EQ '4'.
      ENDIF.

      SELECT * FROM zwm026
               INTO TABLE lt_zwm026
               WHERE armazem = i_lgnum AND
                     grupo   = i_refnr.

      CHECK sy-subrc EQ 0.

      DELETE lt_zwm026 WHERE wcs_ent_aut = abap_true.
      DELETE lt_zwm026 WHERE trf_bpk     = abap_true.

      IF NOT lt_zwm026 IS INITIAL.
**      Não é possivel selecionar Gravitica, picking a decorrer
        CLEAR: ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr = '087'.
        APPEND ls_message TO et_messages.
        RETURN.
      ENDIF.
    ENDDO.

** Valida Paletização Especial
**********************************************************************
    DO 1 TIMES.
      IF NOT i_lock IS INITIAL.
        CHECK i_lock EQ '4'.
      ENDIF.

      CALL FUNCTION 'ZWM_AUT_PAL_ESPECIAL'
        EXPORTING
          i_lgnum     = i_lgnum
          i_refnr     = i_refnr
          i_simu      = 'X'
        IMPORTING
          et_messages = et_messages
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDDO.

** Reabastecimento de PRM
***********************************************************************
    DO 1 TIMES.
      IF NOT i_lock IS INITIAL.
        CHECK i_lock EQ '4'.
      ENDIF.

      CHECK is_reabastecimento_running( i_lgnum = i_lgnum i_refnr = i_refnr i_vltyp = 'AUT' ) EQ abap_true.


**    Não é possivel selecionar Gravitica, reabastecimento em curso
      CLEAR: ls_message.
      ls_message-msgid = 'ZWM001'.
      ls_message-msgtyp = 'E'.
      ls_message-msgnr = '088'.
      APPEND ls_message TO et_messages.
      RETURN.
    ENDDO.


** Tudo Ok
***********************************************************************
    r_result = abap_true.
    RETURN.
  ENDMETHOD.


  METHOD is_allow_pul.
    TYPES: BEGIN OF lty_picking,
             n_pal_picking TYPE nrto,
             peso          TYPE brgew,
           END OF lty_picking.

    DATA: lt_zwm026    TYPE TABLE OF zwm026,
          lt_mara      TYPE HASHED TABLE OF mara WITH UNIQUE KEY matnr,
          lt_picking   TYPE SORTED TABLE OF lty_picking WITH UNIQUE KEY n_pal_picking,
          lt_ordem     TYPE TABLE OF numc2,
          lt_ltak      TYPE TABLE OF ltak,
          lt_ltap      TYPE TABLE OF ltap,
          lt_ltap_calc TYPE TABLE OF ltap,
          lt_sscc      TYPE TABLE OF lenum,
          lt_lqua      TYPE TABLE OF lqua.


    DATA: ls_zwm026  TYPE zwm026,
          ls_mara    TYPE mara,
          ls_ltap    TYPE ltap,
          ls_picking TYPE lty_picking,
          ls_message TYPE bdcmsgcoll,
          ls_lqua    TYPE lqua.


    DATA: lv_max_pua_pal    TYPE i VALUE 10,
          lv_min_pua_peso   TYPE f VALUE 10,
          lv_max_pua_ordem  TYPE n LENGTH 2,
          lv_max_pua_manual TYPE n LENGTH 2,

          lv_lgnum          TYPE lgnum,
          lv_refnr          TYPE lvs_refnr,
          lv_total_paletes  TYPE n LENGTH 2,
          lv_ordem          TYPE n LENGTH 2,
          lv_manual         TYPE sytabix,
          lv_pal_num        TYPE sytabix,
          lv_tabix          TYPE sytabix,
          lv_sscc           TYPE lenum,
          lv_lgtyp          TYPE lgtyp.


    FIELD-SYMBOLS:  <ls_picking> TYPE lty_picking.

    CLEAR: r_result.

    lv_lgnum = i_lgnum.
    lv_refnr = i_refnr.


** Desbloquei de PUL por AUT
***********************************************************************
    DO 1 TIMES.
      SELECT SINGLE st_pul FROM zwm028
                           INTO lv_lgtyp
                           WHERE lgnum = lv_lgnum AND
                                 refnr = lv_refnr AND
                                 remessa = ''.

      CHECK lv_lgtyp EQ 'AUT'.

** Paletes Picking
      SELECT sscc FROM zwm026
                  INTO TABLE lt_sscc
                  WHERE armazem = lv_lgnum AND
                        grupo = lv_refnr AND
                        estado = 'T'.
      CHECK NOT lt_sscc IS INITIAL.

      LOOP AT lt_sscc INTO lv_sscc.
        lv_tabix = sy-tabix.
        CHECK is_sscc( lv_sscc ) EQ abap_false.
        DELETE lt_sscc INDEX lv_tabix.
      ENDLOOP.
      CHECK NOT lt_sscc IS INITIAL.

      SELECT * FROM lqua
               INTO TABLE lt_lqua
               FOR ALL ENTRIES IN lt_sscc
               WHERE lgnum = lv_lgnum AND
                     lenum = lt_sscc-table_line.

      IF sy-subrc <> 0.

        " Não é possivel selecionar Pulmão, picking a entrar em Automático
        CLEAR: ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr = '098'.
        APPEND ls_message TO et_messages.
        r_result = abap_false.
        RETURN.
      ENDIF.

      LOOP AT lt_lqua INTO ls_lqua WHERE einme > 0.

        " Não é possivel selecionar Pulmão, picking a entrar em Automático
        CLEAR: ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr = '098'.
        APPEND ls_message TO et_messages.
        r_result = abap_false.
        RETURN.
      ENDLOOP.

      " Paletes Paletização Especial
      CALL FUNCTION 'ZWM_AUT_PAL_ESPECIAL'
        EXPORTING
          i_lgnum     = i_lgnum
          i_refnr     = i_refnr
          i_simu      = 'X'
        IMPORTING
          et_messages = et_messages
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDDO.

** Tudo Ok
***********************************************************************
    r_result = abap_true.
    RETURN.
  ENDMETHOD.


  METHOD is_delivery_completed.
    TYPES: BEGIN OF lty_lips.
             INCLUDE TYPE lips.
           TYPES:
                    lfimg_orig TYPE lfimg,
                    pikmg      TYPE pikmg,
                  END OF lty_lips.

    DATA: lt_vbup TYPE TABLE OF vbup,
          lt_lips TYPE SORTED TABLE OF lty_lips WITH UNIQUE KEY vbeln posnr,
          lt_ltak TYPE TABLE OF ltak,
          lt_ltap TYPE TABLE OF ltap,
          lt_t320 TYPE TABLE OF t320.

    DATA: ls_vbup            TYPE vbup,
          ls_lips            TYPE lty_lips,
          ls_ltak            TYPE ltak,
          ls_ltap            TYPE ltap,
          ls_quantity_needed TYPE quantity_needed,
          ls_t320            TYPE t320.

    DATA: lv_lgnum TYPE lgnum,
          lv_pikmg TYPE	pikmg,
          lv_tabix TYPE sy-tabix.

    FIELD-SYMBOLS: <ls_lips> TYPE lty_lips.

    CLEAR: et_quantity_needed.

    r_result = abap_false.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).
    e_vbeln = get_vbeln( i_lgnum = lv_lgnum i_vbeln = i_vbeln is_data = is_data ).
    e_refnr = get_refnr( i_lgnum = lv_lgnum i_refnr = i_refnr i_vbeln = e_vbeln is_data = is_data ).

    CHECK NOT e_vbeln IS INITIAL.

    SELECT * FROM t320 INTO TABLE lt_t320
      WHERE lgnum = i_lgnum.

    SELECT * FROM lips
             INTO TABLE lt_lips
             WHERE vbeln = e_vbeln.
    CHECK sy-subrc EQ 0.

    DELETE lt_lips WHERE komkz IS INITIAL.

    LOOP AT lt_lips INTO ls_lips.
      lv_tabix = sy-tabix.

      READ TABLE lt_t320 INTO ls_t320 WITH KEY werks = ls_lips-werks
                                               lgort = ls_lips-lgort.
      CHECK sy-subrc <> 0.

      DELETE lt_lips INDEX lv_tabix.
    ENDLOOP.

    CHECK NOT lt_lips IS INITIAL.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = lv_lgnum
        i_refnr  = e_refnr
        i_vbeln  = e_vbeln
      IMPORTING
        e_2step  = e_2step
        e_2spart = e_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF e_2step EQ abap_false.
      SELECT * FROM vbup
               INTO TABLE lt_vbup
               WHERE vbeln = e_vbeln.




      LOOP AT lt_vbup INTO ls_vbup.
        CLEAR: ls_lips.
        READ TABLE lt_lips
              INTO ls_lips
              WITH TABLE KEY vbeln = ls_vbup-vbeln
                             posnr = ls_vbup-posnr.
        CHECK sy-subrc EQ 0.

        CALL FUNCTION 'ZWM_GET_PICKING'
          EXPORTING
            i_vbeln             = e_vbeln
            i_posnr             = ls_vbup-posnr
          IMPORTING
            e_pikmg             = lv_pikmg
          EXCEPTIONS
            document_read_error = 1
            OTHERS              = 2.
        IF sy-subrc <> 0.
          lv_pikmg = ls_lips-lfimg.
        ENDIF.

        CLEAR: ls_quantity_needed.
        ls_quantity_needed-refnr = e_refnr.
        ls_quantity_needed-vbeln = e_vbeln.
        ls_quantity_needed-posnr = ls_lips-posnr.
        ls_quantity_needed-matnr = ls_lips-matnr.
        ls_quantity_needed-werks = ls_lips-werks.
        ls_quantity_needed-lgort = ls_lips-lgort.
        ls_quantity_needed-menge = ls_lips-lfimg - lv_pikmg.
        ls_quantity_needed-meinh = ls_lips-vrkme.
        ls_quantity_needed-lfimg = ls_lips-lfimg.
        ls_quantity_needed-pikmg = lv_pikmg.
        ls_quantity_needed-two_step = e_2step.
        ls_quantity_needed-two_spart = e_2spart.

        IF ls_vbup-kosta EQ 'C' OR ls_quantity_needed-menge <= 0.
          ls_quantity_needed-kosta = 'C'.
        ELSEIF ls_quantity_needed-pikmg EQ 0.
          ls_quantity_needed-kosta = 'A'.
        ELSEIF lv_pikmg <> 0.
          ls_quantity_needed-kosta = 'B'.
        ENDIF.

        set_status( EXPORTING i_in_status = ls_quantity_needed-kosta CHANGING c_out_status = e_created ).

        CHECK i_get_all EQ abap_true OR ls_quantity_needed-kosta <> 'C'.

        COLLECT ls_quantity_needed INTO et_quantity_needed.
      ENDLOOP.

      IF e_created EQ 'C'.
        r_result = abap_true.
      ENDIF.
    ELSEIF e_2spart EQ abap_true.
      CALL FUNCTION 'Z_WM_FILTER_TABLE_TO_WM'
        EXPORTING
          i_lgnum  = lv_lgnum
        CHANGING
          ct_table = lt_lips.

      DO 1 TIMES.

        SELECT * FROM ltak
                 INTO TABLE lt_ltak
                 WHERE lgnum = lv_lgnum AND
                       benum = e_vbeln AND
                       betyp = 'L' AND
                       refnr = e_refnr.

        CHECK sy-subrc EQ 0.

        SELECT * FROM ltap
                 INTO TABLE lt_ltap
                 FOR ALL ENTRIES IN lt_ltak
                 WHERE lgnum = lt_ltak-lgnum AND
                       tanum = lt_ltak-tanum.

        DELETE lt_ltap WHERE vorga EQ 'ST' " Estornadas
                          OR vorga EQ 'SL'.

      ENDDO.

      LOOP AT lt_ltap INTO ls_ltap.
        LOOP AT lt_lips ASSIGNING <ls_lips> WHERE werks = ls_ltap-werks AND
                                                  lgort = ls_ltap-lgort AND
                                                  matnr = ls_ltap-matnr AND
                                                  lfimg > 0.

          IF <ls_lips>-lfimg_orig IS INITIAL.
            <ls_lips>-lfimg_orig = <ls_lips>-lfimg.
          ENDIF.


          IF ls_ltap-vsolm > <ls_lips>-lfimg.
            <ls_lips>-pikmg = <ls_lips>-pikmg + <ls_lips>-lfimg.
            ls_ltap-vsolm = ls_ltap-vsolm - <ls_lips>-lfimg.
            <ls_lips>-lfimg = 0.
          ELSE.
            <ls_lips>-pikmg = <ls_lips>-pikmg + ls_ltap-vsolm.
            <ls_lips>-lfimg = <ls_lips>-lfimg - ls_ltap-vsolm.
            ls_ltap-vsolm = 0.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      IF sy-subrc <> 0.
        LOOP AT lt_lips ASSIGNING <ls_lips>.
          IF <ls_lips>-lfimg_orig IS INITIAL.
            <ls_lips>-lfimg_orig = <ls_lips>-lfimg.
          ENDIF.
        ENDLOOP.
      ENDIF.

      LOOP AT lt_lips INTO ls_lips.
        CLEAR: ls_quantity_needed.
        ls_quantity_needed-refnr = e_refnr.
        ls_quantity_needed-vbeln = e_vbeln.
        ls_quantity_needed-posnr = ls_lips-posnr.
        ls_quantity_needed-matnr = ls_lips-matnr.
        ls_quantity_needed-werks = ls_lips-werks.
        ls_quantity_needed-lgort = ls_lips-lgort.
        ls_quantity_needed-menge = ls_lips-lfimg.
        ls_quantity_needed-meinh = ls_lips-vrkme.
        ls_quantity_needed-lfimg = ls_lips-lfimg_orig.
        ls_quantity_needed-pikmg = ls_lips-pikmg.
        ls_quantity_needed-two_step = e_2step.
        ls_quantity_needed-two_spart = e_2spart.

        IF ls_lips-lfimg EQ ls_lips-lfimg_orig.
          ls_quantity_needed-kosta = 'A'.
        ELSEIF ls_lips-lfimg EQ 0.
          ls_quantity_needed-kosta = 'C'.
        ELSE.
          ls_quantity_needed-kosta = 'B'.
        ENDIF.

        set_status( EXPORTING i_in_status = ls_quantity_needed-kosta CHANGING c_out_status = e_created ).

        CHECK i_get_all EQ abap_true OR ls_quantity_needed-kosta <> 'C'.

        COLLECT ls_quantity_needed INTO et_quantity_needed.
      ENDLOOP.

      IF e_created EQ 'C'.
        r_result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD is_group_completed.
    DATA: lt_total           TYPE TABLE OF l2sktotal,
          lt_t311a           TYPE TABLE OF t311a,
          lt_quantity_needed TYPE t_quantity_needed.

    DATA: ls_total           TYPE l2sktotal,
          ls_t311a           TYPE t311a,
          ls_quantity_needed TYPE quantity_needed.

    DATA: lv_lgnum   TYPE lgnum,
          lv_tabix   TYPE sytabix,
          lv_created TYPE kostk.

    CLEAR: et_quantity_needed.

    r_result = abap_false.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).
    e_refnr = get_refnr( i_lgnum = lv_lgnum i_refnr = i_refnr i_vbeln = i_vbeln is_data = is_data ).

    CHECK NOT e_refnr IS INITIAL.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = lv_lgnum
        i_refnr  = e_refnr
      IMPORTING
        e_2step  = e_2step
        e_2spart = e_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF e_2step EQ abap_false OR e_2spart EQ abap_true.
      SELECT * FROM t311a
               INTO TABLE lt_t311a
               WHERE lgnum = lv_lgnum AND
                     refnr = e_refnr.

      CHECK sy-subrc EQ 0.

      LOOP AT lt_t311a INTO ls_t311a.
        lv_tabix = sy-tabix.
        IF is_delivery_completed( EXPORTING i_lgnum = lv_lgnum i_vbeln = ls_t311a-rbnum i_refnr = e_refnr is_data = is_data i_get_all = i_get_all IMPORTING et_quantity_needed = lt_quantity_needed e_created = lv_created ).
          DELETE lt_t311a INDEX lv_tabix.
        ENDIF.
        APPEND LINES OF lt_quantity_needed TO et_quantity_needed.
        set_status( EXPORTING i_in_status = lv_created CHANGING c_out_status = e_created ).
      ENDLOOP.

      IF lt_t311a IS INITIAL.
        r_result = abap_true.
      ENDIF.
    ELSE.
      CALL FUNCTION 'L_2_STEP_QUANTITY_REMOVAL'
        EXPORTING
          i_lgnum                       = lv_lgnum
          i_refnr                       = e_refnr
        TABLES
          t_total                       = lt_total
        EXCEPTIONS
          refnr_no_found                = 1
          refnr_documents_no_found      = 2
          no_relevant_for_2step_picking = 3
          item_for_removal_not_found    = 4
          OTHERS                        = 5.

      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        LOOP AT lt_total INTO ls_total.
          CLEAR: ls_quantity_needed.
          ls_quantity_needed-refnr = e_refnr.
          ls_quantity_needed-matnr = ls_total-matnr.
          ls_quantity_needed-werks = ls_total-werks.
          ls_quantity_needed-lgort = ls_total-lgort.
          ls_quantity_needed-menge = ls_total-ofmng.
          ls_quantity_needed-meinh = ls_total-meins.
          ls_quantity_needed-lfimg = ls_total-bdmng.
          ls_quantity_needed-pikmg = ls_total-enmng.
          ls_quantity_needed-two_step = e_2step.
          ls_quantity_needed-two_spart = e_2spart.

          IF ls_total-enmng IS INITIAL.
            ls_quantity_needed-kosta = 'A'.
          ELSEIF ls_total-ofmng IS INITIAL.
            ls_quantity_needed-kosta = 'C'.
          ELSE.
            ls_quantity_needed-kosta = 'B'.
          ENDIF.

          set_status( EXPORTING i_in_status = ls_quantity_needed-kosta CHANGING c_out_status = e_created ).

          CHECK i_get_all EQ abap_true OR ls_quantity_needed-kosta <> 'C'.

          APPEND ls_quantity_needed TO et_quantity_needed.
        ENDLOOP.

        IF et_quantity_needed[] IS INITIAL.
          r_result = abap_true.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD is_group_loaded.
    DATA: ls_zwm028     TYPE zwm028,
          ls_zwm006_aux TYPE zwm006_aux.

    DATA: lv_lgnum TYPE lgnum,
          lv_refnr TYPE lvs_refnr,
          lv_tknum TYPE tknum.

    CLEAR: e_porta.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum i_lenum = i_lenum is_data = is_data ).
    lv_refnr = get_refnr(
                  i_lgnum = lv_lgnum
                  i_refnr = i_refnr
                  i_tanum = i_tanum
                  i_tapos = i_tapos
                  i_lenum = i_lenum
                  i_vbeln = i_vbeln
                  is_data = is_data
               ).


    CHECK NOT lv_lgnum IS INITIAL AND NOT lv_refnr IS INITIAL.

    CHECK is_group_completed( i_lgnum = lv_lgnum i_refnr = lv_refnr ) EQ abap_true.

    DO 10 TIMES.
      CLEAR ls_zwm028.

      SELECT SINGLE * FROM zwm028
                      INTO ls_zwm028
                      WHERE lgnum = i_lgnum AND
                            refnr = i_refnr AND
                            remessa = ''.

      CHECK sy-subrc = 0.

      IF ls_zwm028-paletes_carro >= ls_zwm028-total_paletes.
        r_result = abap_true.
        EXIT.
      ENDIF.

      IF ls_zwm028-porta IS INITIAL.
        r_result = abap_false.
        EXIT.
      ENDIF.

      WAIT UP TO 1 SECONDS.

    ENDDO.

    lv_tknum = get_tknum( i_lgnum = lv_lgnum i_refnr = lv_refnr ).
    CHECK NOT lv_tknum IS INITIAL.

    SELECT SINGLE * FROM zwm006_aux
                    INTO ls_zwm006_aux
                    WHERE armazem     = lv_lgnum AND
                          n_transporte = lv_tknum.

    e_porta     = ls_zwm006_aux-porta.
    e_talao     = ls_zwm006_aux-num_entrada.
    e_pulmao1   = ls_zwm028-pulmao1.
    e_pulmao2   = ls_zwm028-pulmao1.
    e_lgtyp_pul = ls_zwm028-st_pul.
    es_zwm028   = ls_zwm028.

  ENDMETHOD.


  METHOD is_idoc_refnr_send.
    DATA: lt_ltap    TYPE TABLE OF ltap,
          lt_idocnum TYPE TABLE OF edi_docnum.

    DATA: ls_ltap    TYPE ltap,
          ls_ltap_in TYPE ltap.

    DATA: lv_lgnum TYPE lgnum,
          lv_refnr TYPE lvs_refnr,
          lv_ordem TYPE numc2,
          lv_vbeln TYPE vbeln,
          lv_tabix TYPE sytabix.

    FIELD-SYMBOLS: <lv_lgnum> TYPE lgnum,
                   <lv_refnr> TYPE lvs_refnr,
                   <lv_vbeln> TYPE vbeln,
                   <lv_ordem> TYPE numc2.

    CLEAR: r_result, e_idocnum.

    lv_ordem = i_ordem.
    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).
    lv_refnr = get_refnr( i_lgnum = i_lgnum i_refnr = i_refnr  is_data = is_data ).
    lv_vbeln = get_vbeln( i_lgnum = i_lgnum i_vbeln = i_vbeln is_data = is_data ).

    DO 1 TIMES.
      CHECK lv_ordem IS INITIAL.

      UNASSIGN <lv_ordem>.
      ASSIGN COMPONENT 'ORDEM' OF STRUCTURE is_data TO <lv_ordem>.
      IF <lv_ordem> IS ASSIGNED AND
         NOT <lv_ordem> IS INITIAL.
        lv_ordem = <lv_ordem>.
      ENDIF.
      CHECK lv_ordem IS INITIAL.

      CHECK NOT lv_lgnum IS INITIAL AND
            NOT lv_refnr IS INITIAL AND
            NOT lv_vbeln IS INITIAL.

      SELECT SINGLE ordem FROM zwm028
                          INTO lv_ordem
                          WHERE lgnum = lv_lgnum AND
                                refnr = lv_refnr AND
                                remessa = lv_vbeln.
    ENDDO.




    CHECK NOT lv_lgnum IS INITIAL AND
          NOT lv_refnr IS INITIAL.

    DO 1 TIMES.

      IF NOT lv_ordem IS INITIAL.
        SELECT SINGLE free_idoc FROM zwm028
                                INTO e_idocnum
                                WHERE lgnum = lv_lgnum AND
                                      refnr = lv_refnr AND
                                      ordem = lv_ordem.
      ENDIF.
      CHECK e_idocnum IS INITIAL.

      IF NOT lv_vbeln IS  INITIAL.
        SELECT SINGLE free_idoc FROM zwm028
                                INTO e_idocnum
                                WHERE lgnum = lv_lgnum AND
                                      refnr = lv_refnr AND
                                      remessa = lv_vbeln.
      ENDIF.
      CHECK e_idocnum IS INITIAL.

      SELECT free_idoc FROM zwm028
                       INTO TABLE lt_idocnum
                       WHERE lgnum = lv_lgnum AND
                             refnr = lv_refnr AND
                             remessa = ''.

      DELETE lt_idocnum WHERE table_line IS INITIAL.
      READ TABLE lt_idocnum
            INTO e_idocnum
            INDEX 1.
    ENDDO.

    IF NOT e_idocnum IS INITIAL.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_idoc_to_send.
    TYPES: BEGIN OF lty_remontada_calc,
             vltyp TYPE ltap_vltyp,
             vlpla TYPE ltap_vlpla,
             matnr TYPE matnr,
             count TYPE i,
           END OF lty_remontada_calc.

    DATA: lt_ltap           TYPE TABLE OF ltap,
          lt_remontada_calc TYPE SORTED TABLE OF lty_remontada_calc WITH UNIQUE KEY vltyp vlpla matnr.

    DATA: ls_ltap            TYPE ltap,
          ls_ltap_in         TYPE ltap,
          ls_remontada_calc  TYPE lty_remontada_calc,
          ls_remontada_check TYPE lty_remontada_calc.

    DATA: lv_lgnum TYPE lgnum,
          lv_tanum TYPE tanum,
          lv_tabix TYPE sytabix,
          lv_calc  TYPE f.

    DATA: lref_data TYPE REF TO data.

    CLEAR: r_result, e_idocnum.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).
    lv_tanum = get_tanum( i_lgnum = i_lgnum i_tanum = i_tanum is_data = is_data ).

    CHECK NOT lv_lgnum IS INITIAL AND
          NOT lv_tanum IS INITIAL.

    CALL FUNCTION 'ZWM_TO_GET_IDOC'
      EXPORTING
        i_lgnum   = lv_lgnum
        i_tanum   = lv_tanum
      IMPORTING
        e_idocnum = e_idocnum.

    IF NOT e_idocnum IS INITIAL.
      r_result =  abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_picking.

    DATA: lt_lenum  TYPE TABLE OF lenum,
          lt_zwm026 TYPE TABLE OF zwm026.

    DATA: ls_zwm026 TYPE zwm026.

    DATA: lv_lgnum TYPE lgnum,
          lv_lenum TYPE lenum,
          lv_tanum TYPE tanum.

    CLEAR: r_result, e_refnr, e_vbeln.

    CHECK secure_call_stack( ) EQ abap_true.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).

    DO 1 TIMES.
      lt_lenum = get_lenum( i_lgnum = i_lgnum i_tanum = i_tanum is_data = is_data i_usepicking = abap_false i_useto = abap_false ).

      CHECK NOT lv_lgnum IS INITIAL AND
            NOT lt_lenum IS INITIAL.

      SELECT * FROM zwm026
                INTO TABLE lt_zwm026
                FOR ALL ENTRIES IN lt_lenum
                WHERE armazem = lv_lgnum AND
                      sscc = lt_lenum-table_line.

      CHECK sy-subrc EQ 0.
      r_result = abap_true.
    ENDDO.


    DO 1 TIMES.
      CHECK r_result IS INITIAL.
      CHECK NOT lv_lgnum IS INITIAL.

      lv_tanum = get_tanum( EXPORTING i_lgnum = i_lgnum i_tanum = i_tanum i_tapos = i_tapos is_data = is_data IMPORTING et_lenum = lt_lenum ).

      IF NOT lt_lenum IS INITIAL.
        SELECT * FROM zwm026
                  INTO TABLE lt_zwm026
                  FOR ALL ENTRIES IN lt_lenum
                  WHERE armazem = lv_lgnum AND
                        sscc = lt_lenum-table_line.
      ELSEIF NOT lv_lgnum IS INITIAL AND
             NOT lv_tanum IS INITIAL.
        SELECT * FROM zwm026
                 INTO TABLE lt_zwm026
                 WHERE armazem = lv_lgnum AND
                       to_number = lv_tanum.
      ENDIF.

      CHECK NOT lt_zwm026 IS INITIAL.
      r_result = abap_true.
    ENDDO.

    CHECK NOT lt_zwm026 IS INITIAL.

    SORT lt_zwm026.
    DELETE ADJACENT DUPLICATES FROM lt_zwm026.

    CLEAR: ls_zwm026.
    READ TABLE lt_zwm026
          INTO ls_zwm026
          INDEX 1.

    r_result = abap_true.
    e_vbeln = ls_zwm026-remessa.
    e_refnr = ls_zwm026-grupo.
    e_lenum = ls_zwm026-sscc.

  ENDMETHOD.


  METHOD is_reabastecimento_running.
    DATA: lt_ltap TYPE TABLE OF ltap,
          lt_ltak TYPE TABLE OF ltak.

    DATA: ls_ltap TYPE ltap.

    DATA: lv_lgnum TYPE lgnum,
          lv_refnr TYPE lvs_refnr.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).
    CHECK NOT lv_lgnum IS INITIAL.

    lv_refnr = get_refnr( i_lgnum = lv_lgnum i_refnr = i_refnr is_data = is_data ).
    CHECK NOT lv_refnr IS INITIAL.


** Reabastecimento de PRM
***********************************************************************
    SELECT * FROM ltak
             INTO TABLE lt_ltak
             WHERE lgnum = lv_lgnum AND
                   refnr = lv_refnr AND
                   kquit = abap_false.
    CHECK sy-subrc EQ 0.

    SELECT * FROM ltap
             INTO TABLE lt_ltap
             FOR ALL ENTRIES IN lt_ltak
             WHERE lgnum = i_lgnum AND
                   tanum = lt_ltak-tanum AND
                   nltyp = 'PRM'.

    IF NOT i_vltyp IS INITIAL.
      DELETE lt_ltap WHERE vltyp <> i_vltyp.
    ENDIF.

    LOOP AT lt_ltap INTO ls_ltap.
      CHECK is_remontada( i_lgnum = lv_lgnum is_data = ls_ltap ) EQ abap_true.
      r_result = abap_true.
      RETURN.
    ENDLOOP.


  ENDMETHOD.


  METHOD is_remontada.
    DATA: lt_lenum  TYPE TABLE OF lenum,
          lt_zwm020 TYPE TABLE OF zwm020.

    DATA: ls_zwm020 TYPE zwm020.

    DATA: lv_lgnum TYPE lgnum,
          lv_letyp TYPE lvs_letyp,
          lv_lines TYPE sytabix.

    CLEAR r_result.

    CHECK secure_call_stack( ) EQ abap_true.

** Determina Armazem
***********************************************************************
    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).

** Retorno de Palete
***********************************************************************
    lt_lenum = get_lenum( EXPORTING i_lgnum = lv_lgnum i_lenum = i_lenum i_tanum = i_tanum i_tapos = i_tapos is_data = is_data IMPORTING e_lgnum = lv_lgnum ).


** Determina LETYP
***********************************************************************
    e_letyp = get_letyp( i_lgnum = lv_lgnum i_letyp = i_letyp i_lenum = i_lenum is_data = is_data ).

    e_matnr = get_matnr( EXPORTING i_lgnum = lv_lgnum i_matnr = i_matnr is_data = is_data IMPORTING e_letyp = lv_letyp ).

    IF e_letyp IS INITIAL AND
       NOT lv_letyp IS INITIAL.
      e_letyp = lv_letyp.
    ENDIF.

    IF ( NOT e_letyp IS INITIAL ) AND ( e_letyp IN r_letyp_remontada( lv_lgnum ) ).
      r_result = abap_true.
    ENDIF.


** Obriga a ser P1 ou P2
***********************************************************************
    IF lt_lenum IS INITIAL AND
       ( is_p1 EQ abap_true OR is_p2 EQ abap_true ).
      r_result = abap_false.
      RETURN.
    ENDIF.


**** Check de Tabela de Remontadas
*************************************************************************
    DO 1 TIMES.
      CHECK NOT lt_lenum IS INITIAL.
      SORT lt_lenum.
      DELETE ADJACENT DUPLICATES FROM lt_lenum.

      SELECT * FROM zwm020
         INTO TABLE lt_zwm020
         FOR ALL ENTRIES IN lt_lenum
         WHERE armazem = lv_lgnum AND
              p1 = lt_lenum-table_line.

      SELECT * FROM zwm020
               APPENDING TABLE lt_zwm020
               FOR ALL ENTRIES IN lt_lenum
               WHERE armazem = lv_lgnum AND
                    p2 = lt_lenum-table_line.

      SORT lt_zwm020.
      DELETE ADJACENT DUPLICATES FROM lt_zwm020 COMPARING ALL FIELDS.

      DESCRIBE TABLE lt_lenum LINES lv_lines.
      IF lv_lines EQ 1.
        READ TABLE lt_lenum
              INTO e_lenum
              INDEX 1.
      ENDIF.

      LOOP AT lt_zwm020 INTO ls_zwm020.
        READ TABLE lt_lenum
              TRANSPORTING NO FIELDS
              WITH KEY table_line = ls_zwm020-p1.
        IF sy-subrc EQ 0.
          e_is_p1 = abap_true.
          e_sister = ls_zwm020-p2.
          CONTINUE.
        ENDIF.

        READ TABLE lt_lenum
              TRANSPORTING NO FIELDS
              WITH KEY table_line = ls_zwm020-p2.
        IF sy-subrc EQ 0.
          e_is_p2 = abap_true.
          e_sister = ls_zwm020-p1.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      IF is_p1 EQ abap_true AND e_is_p1 EQ abap_false.
        r_result = abap_false.
        RETURN.
      ENDIF.

      IF is_p2 EQ abap_true AND e_is_p2 EQ abap_false.
        r_result = abap_false.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD is_split_remontada.
    TYPES: BEGIN OF lty_remontada_calc,
             vltyp TYPE ltap_vltyp,
             vlpla TYPE ltap_vlpla,
             matnr TYPE matnr,
             count TYPE i,
           END OF lty_remontada_calc.

    DATA: lt_ltap           TYPE TABLE OF ltap,
          lt_remontada_calc TYPE SORTED TABLE OF lty_remontada_calc WITH UNIQUE KEY vltyp vlpla matnr.

    DATA: ls_ltap            TYPE ltap,
          ls_ltap_in         TYPE ltap,
          ls_remontada_calc  TYPE lty_remontada_calc,
          ls_remontada_check TYPE lty_remontada_calc.

    DATA: lv_lgnum TYPE lgnum,
          lv_tabix TYPE sytabix,
          lv_calc  TYPE f.

    DATA: lref_ltap TYPE REF TO data.

    FIELD-SYMBOLS: <ls_ltap_in>        TYPE any,
                   <ls_ltap>           TYPE any,
                   <lv_vltyp>          TYPE ltap_vltyp,
                   <lv_nltyp>          TYPE ltap_nltyp,
                   <lv_vlpla>          TYPE ltap_vlpla,
                   <lv_matnr>          TYPE matnr,
                   <lv_lgnum>          TYPE lgnum,
                   <ls_remontada_calc> TYPE lty_remontada_calc.

     CHECK secure_call_stack( ) eq abap_true.

    IF NOT i_lgnum IS INITIAL AND
       NOT i_tanum IS INITIAL.

      SELECT * FROM ltap
               INTO TABLE lt_ltap
               WHERE lgnum = i_lgnum AND
                     tanum = i_tanum.

      IF NOT i_tapos IS INITIAL.
        SELECT SINGLE * FROM ltap
                        INTO  ls_ltap_in
                        WHERE lgnum = i_lgnum AND
                              tanum = i_tanum AND
                              tapos = i_tapos.
      ENDIF.

      ASSIGN ls_ltap_in TO <ls_ltap_in>.

    ELSE.
      CREATE DATA lref_ltap LIKE is_ltap.
      CHECK lref_ltap IS BOUND.
      ASSIGN lref_ltap->* TO <ls_ltap_in>.
      <ls_ltap_in> = is_ltap.

      LOOP AT it_ltap ASSIGNING <ls_ltap>.
        MOVE-CORRESPONDING <ls_ltap> TO ls_ltap.
        APPEND ls_ltap TO lt_ltap.
      ENDLOOP.
    ENDIF.

    DO 1 TIMES.
      CHECK <ls_ltap_in> IS ASSIGNED AND NOT <ls_ltap_in> IS INITIAL.

      lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = <ls_ltap_in> ).

      UNASSIGN <lv_vltyp>.
      ASSIGN COMPONENT 'VLTYP' OF STRUCTURE <ls_ltap_in> TO <lv_vltyp>.
      CHECK <lv_vltyp> IS ASSIGNED AND NOT <lv_vltyp> IS INITIAL.

      UNASSIGN <lv_nltyp>.
      ASSIGN COMPONENT 'NLTYP' OF STRUCTURE <ls_ltap_in> TO <lv_nltyp>.
      CHECK <lv_nltyp> IS ASSIGNED AND NOT <lv_nltyp> IS INITIAL.

      UNASSIGN <lv_vlpla>.
      ASSIGN COMPONENT 'VLPLA' OF STRUCTURE <ls_ltap_in> TO <lv_vlpla>.
      CHECK <lv_vlpla> IS ASSIGNED AND NOT <lv_vlpla> IS INITIAL.

      UNASSIGN <lv_matnr>.
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ls_ltap_in> TO <lv_matnr>.
      CHECK <lv_matnr> IS ASSIGNED AND NOT <lv_matnr> IS INITIAL.

      ls_remontada_check-vltyp = <lv_vltyp>.
      ls_remontada_check-vlpla = <lv_vlpla>.
      ls_remontada_check-matnr = <lv_matnr>.

      CHECK NOT is_ltap IS INITIAL AND
         z_wm_cl_management=>is_remontada( is_data =  ls_ltap_in ) EQ abap_true.

**      CHECK <lv_vltyp> EQ 'AUT' OR
**            <lv_vltyp> EQ 'DRI'.
**
**      CHECK <lv_nltyp> EQ 'PRM'.

      SELECT * FROM ltap
               INTO TABLE lt_ltap
               WHERE lgnum = <lv_lgnum> AND
                     pquit = abap_false AND
                     vltyp = <lv_vltyp> AND
                     vlpla = <lv_vlpla> AND
                     matnr = <lv_matnr>.


    ENDDO.


    LOOP AT lt_ltap INTO ls_ltap.
      CHECK z_wm_cl_management=>is_remontada( is_data =  ls_ltap ) EQ abap_true.

**      CHECK ls_ltap-vltyp EQ 'AUT' OR
**            ls_ltap-vltyp EQ 'DRI'.
**
**      CHECK ls_ltap-nltyp EQ 'PRM'.

      IF lv_lgnum IS INITIAL.
        lv_lgnum = ls_ltap-lgnum.
      ENDIF.

      READ TABLE lt_remontada_calc
       ASSIGNING <ls_remontada_calc>
       WITH TABLE KEY vltyp = ls_ltap-vltyp
                      vlpla = ls_ltap-vlpla
                      matnr = ls_ltap-matnr.
      IF sy-subrc <> 0.
        ls_remontada_calc-vltyp = ls_ltap-vltyp.
        ls_remontada_calc-vlpla = ls_ltap-vlpla.
        ls_remontada_calc-matnr = ls_ltap-matnr.
        INSERT ls_remontada_calc INTO TABLE lt_remontada_calc ASSIGNING <ls_remontada_calc>.
      ENDIF.

      <ls_remontada_calc>-count = <ls_remontada_calc>-count + ls_ltap-vsola.
    ENDLOOP.


    LOOP AT lt_remontada_calc INTO ls_remontada_calc.
      lv_tabix = sy-tabix.
      lv_calc = ls_remontada_calc-count MOD 2.
      IF lv_calc <> 0.
        DELETE lt_remontada_calc INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

    IF NOT ls_remontada_check IS INITIAL.
      DELETE lt_remontada_calc WHERE vltyp <> ls_remontada_check-vltyp OR
                                     vlpla <> ls_remontada_check-vlpla OR
                                     matnr <> ls_remontada_check-matnr.
    ENDIF.

    IF NOT lt_remontada_calc IS INITIAL.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_sscc.
    DATA: lv_lenum TYPE lenum.

    lv_lenum = i_sscc.

    IF lv_lenum IS INITIAL.
      RETURN.
    ENDIF.

    IF strlen( lv_lenum ) <> 20.
      RETURN.
    ENDIF.

    IF lv_lenum(4) <> '0035'.
      RETURN.
    ENDIF.

    r_result = abap_true.
    RETURN.
  ENDMETHOD.


  METHOD is_tr_matnr_created.
    DATA: lv_benum TYPE lvs_benum,
          lv_lgnum TYPE lgnum,
          lv_matnr TYPE matnr.

    lv_lgnum = get_lgnum( i_lgnum = i_lgnum is_data = is_data ).
    lv_matnr = get_matnr( i_lgnum = lv_lgnum i_matnr = i_matnr is_data = is_data ).

    CHECK NOT lv_lgnum IS INITIAL AND
          NOT lv_matnr IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_matnr
      IMPORTING
        output = lv_benum.



    SELECT SINGLE *
             FROM ltbk
             INTO es_ltbk
             WHERE lgnum = lv_lgnum AND
                   statu in ( '', 'T' ) AND
                   betyp = 'M' AND
                   benum = lv_benum AND
                   bwlvs = '934'.

    CHECK sy-subrc EQ 0.


    SELECT * FROM ltbp
             INTO TABLE et_ltbp
             WHERE lgnum = es_ltbk-lgnum AND
                   tbnum = es_ltbk-tbnum.

    CHECK sy-subrc EQ 0.

    r_result = abap_true.
  ENDMETHOD.


  METHOD lenum_extract.

    FIELD-SYMBOLS: <lv_data> TYPE any.

    DO 1 TIMES.
      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'LENUM' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          is_sscc( <lv_data> ) EQ abap_true.
        APPEND <lv_data> TO r_result.
      ENDIF.

      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'VLENR' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          is_sscc( <lv_data> ) EQ abap_true.
        APPEND <lv_data> TO r_result.
      ENDIF.

      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'NLENR' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          is_sscc( <lv_data> ) EQ abap_true.
        APPEND <lv_data> TO r_result.
      ENDIF.

      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'SSCC' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          is_sscc( <lv_data> ) EQ abap_true.
        APPEND <lv_data> TO r_result.
      ENDIF.

      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'P1' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          is_sscc( <lv_data> ) EQ abap_true.
        APPEND <lv_data> TO r_result.
      ENDIF.

      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'P2' OF STRUCTURE is_data TO <lv_data>.
      IF <lv_data> IS ASSIGNED AND
          is_sscc( <lv_data> ) EQ abap_true.
        APPEND <lv_data> TO r_result.
      ENDIF.

      SORT r_result.
      DELETE ADJACENT DUPLICATES FROM r_result.
    ENDDO.

  ENDMETHOD.


  METHOD R_LETYP_REMONTADA.
    DATA: ls_r_letyp TYPE zwm_sr_letyp.

    ls_r_letyp-low = 'P2'.
    ls_r_letyp-option = 'EQ'.
    ls_r_letyp-sign = 'I'.
    APPEND ls_r_letyp TO r_result.

    ls_r_letyp-low = 'P5'.
    ls_r_letyp-option = 'EQ'.
    ls_r_letyp-sign = 'I'.
    APPEND ls_r_letyp TO r_result.

    IF i_lgnum EQ '150'.
      EXIT.
    ENDIF.

    ls_r_letyp-low = 'P9'.
    ls_r_letyp-option = 'EQ'.
    ls_r_letyp-sign = 'I'.
    APPEND ls_r_letyp TO r_result.

    ls_r_letyp-low = 'P11'.
    ls_r_letyp-option = 'EQ'.
    ls_r_letyp-sign = 'I'.
    APPEND ls_r_letyp TO r_result.

    ls_r_letyp-low = 'P13'.
    ls_r_letyp-option = 'EQ'.
    ls_r_letyp-sign = 'I'.
    APPEND ls_r_letyp TO r_result.

    ls_r_letyp-low = 'P15'.
    ls_r_letyp-option = 'EQ'.
    ls_r_letyp-sign = 'I'.
    APPEND ls_r_letyp TO r_result.

    ls_r_letyp-low = 'P17'.
    ls_r_letyp-option = 'EQ'.
    ls_r_letyp-sign = 'I'.
    APPEND ls_r_letyp TO r_result.

    ls_r_letyp-low = 'P19'.
    ls_r_letyp-option = 'EQ'.
    ls_r_letyp-sign = 'I'.
    APPEND ls_r_letyp TO r_result.
  ENDMETHOD.


  METHOD secure_call_stack.
    CONSTANTS: lc_max_stack TYPE sytabix VALUE 10.

    DATA: lt_abap_callstack TYPE  abap_callstack,
          lt_sys_callstack  TYPE  sys_callst.

    DATA: ls_abap_callstack_main TYPE abap_callstack_line,
          ls_abap_callstack      TYPE abap_callstack_line.

    DATA: lv_stack TYPE sytabix.

    r_result = abap_true.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level    = 0
      IMPORTING
        callstack    = lt_abap_callstack
        et_callstack = lt_sys_callstack.

    CHECK NOT lt_abap_callstack IS INITIAL.

    READ TABLE lt_abap_callstack
          INTO ls_abap_callstack_main
          INDEX 1.

    lv_stack = 0.
    LOOP AT lt_abap_callstack INTO ls_abap_callstack.
      IF ls_abap_callstack_main-mainprogram <> ls_abap_callstack-mainprogram.
        lv_stack = 0.
        CONTINUE.
      ENDIF.

      lv_stack = lv_stack + 1.

      IF lv_stack > lc_max_stack.
        break roff-dsilva.
*        break inet-apais.
        """"" FALAR COM O D
        CLEAR r_result.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_status.
    IF c_out_status IS INITIAL.
      c_out_status = i_in_status.
      EXIT.
    ENDIF.

    IF c_out_status EQ i_in_status.
      EXIT.
    ELSEIF  c_out_status <> i_in_status.
      c_out_status = 'B'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
