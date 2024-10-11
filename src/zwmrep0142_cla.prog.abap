*&---------------------------------------------------------------------*
*&  Include           ZWMREP0142_CLA
*&---------------------------------------------------------------------*

CLASS zcl_acerto_pal_916 DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS run.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_lqua_aux,
             lgnum TYPE lgnum,
             lgtyp TYPE lgtyp,
             lgpla TYPE lgpla,
             matnr TYPE matnr,
             meins TYPE meins,
             verme TYPE lqua_verme,
           END OF ty_lqua_aux.

    TYPES ltap_create TYPE STANDARD TABLE OF ltap_creat.

    CLASS-METHODS create_ot_multiple
      IMPORTING it_ltap_create  TYPE  ltap_create
      RETURNING VALUE(rv_tanum) TYPE tanum.

ENDCLASS.

CLASS zcl_acerto_pal_916 IMPLEMENTATION.

  METHOD run.

    DATA: lv_quant_positive TYPE lqua_verme,
          lv_matnr_curr     TYPE matnr,
          lv_tanum          TYPE tanum.

    DATA: ls_lqua_aux         TYPE ty_lqua_aux,
          ls_ltap_creat       TYPE ltap_creat,
          ls_ltap_creat_block TYPE ltap_creat.

    DATA: lt_lqua_aux         TYPE TABLE OF ty_lqua_aux,
          lt_ltap_creat       TYPE STANDARD TABLE OF ltap_creat,
          lt_ltap_creat_block TYPE STANDARD TABLE OF ltap_creat.

**********************************************************************

    " Valida se o Tip. Dep Origem e Dest estão correctos
    IF p_lgtypo <> 'PAL' OR p_lgtypd <> '916'.
      " Tipo/s de Depósito/s inválidos!
      MESSAGE s347(zwmmsg001) DISPLAY LIKE 'E'.
      REFRESH s_matnr.
      RETURN.
    ENDIF.

    " Verifica se o Material introduzido existem no tip. dep. Origem
    SELECT lgnum, lgtyp, lgpla, matnr, meins, verme
      FROM lqua
      INTO TABLE @DATA(lt_lqua_o)
      WHERE lgnum = @p_lgnum
        AND lgtyp = @p_lgtypo
        AND matnr IN @s_matnr.
    IF sy-subrc <> 0.
      " Material/s não existe/m no tipo de deposito &.
      MESSAGE s344(zwmmsg001) WITH p_lgtypo DISPLAY LIKE 'E'.
      REFRESH s_matnr.
      RETURN.
    ENDIF.

    " Verifica se Existem entradas para o/s Material/s no Tip. Dep. Destino
    SELECT lgnum, lgtyp, lgpla, matnr, meins, verme, werks, lgort
      FROM lqua
      INTO TABLE @DATA(lt_lqua_d)
      FOR ALL ENTRIES IN @lt_lqua_o
      WHERE lgnum = @lt_lqua_o-lgnum
        AND lgtyp = @p_lgtypd
        AND matnr = @lt_lqua_o-matnr.
    IF sy-subrc <> 0.
      " Não existem acertos possiveis para o tipo de deposito &.
      MESSAGE s345(zwmmsg001) WITH p_lgtypd DISPLAY LIKE 'E'.
      REFRESH s_matnr.
      RETURN.
    ENDIF.

    SORT lt_lqua_d BY lgnum lgtyp lgpla matnr.
    SORT lt_lqua_o BY lgnum lgtyp lgpla matnr.

    READ TABLE lt_lqua_o INTO DATA(ls_lqua_o) INDEX 1.
    IF sy-subrc = 0.
      lv_matnr_curr = ls_lqua_o-matnr.
    ENDIF.

    " Cria tabela Auxiliar para cada Material ter apenas uma linha
    LOOP AT lt_lqua_o INTO ls_lqua_o.

      IF lines( lt_lqua_o ) = 1.

        ls_lqua_aux-lgnum = ls_lqua_o-lgnum.
        ls_lqua_aux-lgtyp = ls_lqua_o-lgtyp.
        ls_lqua_aux-lgpla = ls_lqua_o-lgpla.
        ls_lqua_aux-matnr = ls_lqua_o-matnr.
        ls_lqua_aux-meins = ls_lqua_o-meins.
        ls_lqua_aux-verme = ls_lqua_o-verme.

        APPEND ls_lqua_aux TO lt_lqua_aux.

      ELSE.

        " Materiais Diferentes
        IF lv_matnr_curr <> ls_lqua_o-matnr.

          APPEND ls_lqua_aux TO lt_lqua_aux.

          CLEAR ls_lqua_aux.

          lv_matnr_curr = ls_lqua_o-matnr.

          ADD ls_lqua_o-verme TO ls_lqua_aux-verme.

        ELSE.

          ls_lqua_aux-lgnum = ls_lqua_o-lgnum.
          ls_lqua_aux-lgtyp = ls_lqua_o-lgtyp.
          ls_lqua_aux-lgpla = ls_lqua_o-lgpla.
          ls_lqua_aux-matnr = ls_lqua_o-matnr.
          ls_lqua_aux-meins = ls_lqua_o-meins.

          ADD ls_lqua_o-verme TO ls_lqua_aux-verme.

        ENDIF.

        " Verifica se é o ultimo registo
        IF lines( lt_lqua_o ) = sy-tabix.

          ls_lqua_aux-lgnum = ls_lqua_o-lgnum.
          ls_lqua_aux-lgtyp = ls_lqua_o-lgtyp.
          ls_lqua_aux-lgpla = ls_lqua_o-lgpla.
          ls_lqua_aux-matnr = ls_lqua_o-matnr.
          ls_lqua_aux-meins = ls_lqua_o-meins.

          APPEND ls_lqua_aux TO lt_lqua_aux.
        ENDIF.

      ENDIF.

    ENDLOOP.

    LOOP AT lt_lqua_d INTO DATA(ls_lqua_d).

      CLEAR lv_quant_positive.

      lv_quant_positive = abs( ls_lqua_d-verme ).

      READ TABLE lt_lqua_aux INTO ls_lqua_aux WITH KEY matnr = ls_lqua_d-matnr.
      IF sy-subrc = 0.

        " Verifica se ainda ha stock disponivel a transf. para o 916
        IF ls_lqua_aux-verme >= lv_quant_positive.

          ls_ltap_creat-matnr = ls_lqua_aux-matnr.
          ls_ltap_creat-vltyp = ls_lqua_aux-lgtyp. "Origem
          ls_ltap_creat-vlpla = ls_lqua_aux-lgpla. "Origem
          ls_ltap_creat-nltyp = ls_lqua_d-lgtyp. "Destino
          ls_ltap_creat-nlpla = ls_lqua_d-lgpla. "Destino
          ls_ltap_creat-squit = abap_true.
          ls_ltap_creat-werks = ls_lqua_d-werks.
          ls_ltap_creat-lgort = ls_lqua_d-lgort.
          ls_ltap_creat-anfme = lv_quant_positive.
          ls_ltap_creat-altme = ls_lqua_d-meins.

          APPEND ls_ltap_creat TO lt_ltap_creat.
        ENDIF.

      ENDIF.

    ENDLOOP.

** Criação de Blocos ate 9999 registos para criação OT´s
**********************************************************************

    LOOP AT lt_ltap_creat INTO ls_ltap_creat.

      CLEAR ls_ltap_creat_block.

      MOVE-CORRESPONDING ls_ltap_creat TO ls_ltap_creat_block.
      APPEND ls_ltap_creat_block TO lt_ltap_creat_block.

      CHECK ( sy-tabix MOD 9999 EQ 0 ).

      CLEAR: lv_tanum.

** Criação OT´s bloco de 9999
**********************************************************************

      lv_tanum = create_ot_multiple( it_ltap_create = lt_ltap_creat_block ).

      IF lv_tanum IS NOT INITIAL.
        REFRESH lt_ltap_creat_block.
      ELSE.
        REFRESH s_matnr.
        RETURN.
      ENDIF.

    ENDLOOP.

    CLEAR lv_tanum.

    " Restantes registos
    IF lt_ltap_creat_block IS NOT INITIAL.

      lv_tanum = create_ot_multiple( it_ltap_create = lt_ltap_creat_block ).

      IF lv_tanum IS INITIAL.
        REFRESH s_matnr.
        RETURN.
      ENDIF.
    ENDIF.

    " Acerto realizado com sucesso!
    MESSAGE s346(zwmmsg001) DISPLAY LIKE 'S'.
    REFRESH s_matnr.

  ENDMETHOD.

  METHOD create_ot_multiple.

    DATA: lt_ltap_create_ot TYPE STANDARD TABLE OF ltap_creat.

**********************************************************************

    lt_ltap_create_ot = it_ltap_create.

    CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
      EXPORTING
        i_lgnum       = p_lgnum
        i_bwlvs       = '999'
*       I_BETYP       = ' '
*       I_BENUM       = ' '
*       I_LZNUM       = ' '
*       I_NIDRU       = ' '
*       I_DRUKZ       = ' '
*       I_NOSPL       = ' '
*       I_UPDATE_TASK = ' '
        i_commit_work = 'X'
*       I_BNAME       = SY-UNAME
        i_kompl       = 'X'
      IMPORTING
        e_tanum       = rv_tanum
      TABLES
        t_ltap_creat  = lt_ltap_create_ot
*       T_LTAK        =
*       T_LTAP_VB     =
      EXCEPTIONS
        error_message = 99.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
