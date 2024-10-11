REPORT zwmrep0011 MESSAGE-ID zwmmsg001.

INCLUDE <icon>.
TYPE-POOLS: slis.

TABLES: zwm002, zwm005, zwm013, zwm015, zwm016, zwm017,
        t311, lrf_wkqu, vekp, vepo, makt.

DATA: BEGIN OF sscc OCCURS 0,
        exidv LIKE vekp-exidv,
        incidencia LIKE zwm013-incidencia,
END OF sscc.

DATA: BEGIN OF qtd_reais OCCURS 0,
        exidv LIKE vekp-exidv,
        matnr LIKE vepo-matnr,
        vemng LIKE vepo-vemng,
        vemeh LIKE vepo-vemeh,
        inhalt LIKE vekp-inhalt,
        incidencia LIKE zwm013-incidencia,
        descricao LIKE zwm022-descricao,
END OF qtd_reais.

DATA: BEGIN OF qtd_teoricas OCCURS 0,
        material LIKE zwm017-material,
        quantidade LIKE zwm017-quantidade,
        uni LIKE mara-meins,

END OF qtd_teoricas.

DATA: BEGIN OF qtd_totais OCCURS 0,
        exidv LIKE vekp-exidv,
        matnr LIKE vepo-matnr,
        maktx LIKE makt-maktx,
        vemng LIKE vepo-vemng,
        vemeh LIKE vepo-vemeh,
        quantidade LIKE zwm017-quantidade,
        uni LIKE mara-meins,
        incidencia LIKE zwm015-incidencia,
        descricao LIKE zwm022-descricao,
        inhalt LIKE vekp-inhalt,
END OF qtd_totais.

DATA: r-ucomm     LIKE sy-ucomm,
      rs_selfield TYPE slis_selfield.

DATA: indice LIKE sy-tabix.

******* ALV data declare **********

*DATA: extab TYPE slis_t_extab.

DATA: fieldcat  TYPE slis_t_fieldcat_alv,
      sort      TYPE slis_t_sortinfo_alv,
      sort1     TYPE slis_t_sortinfo_alv,
      layout    TYPE slis_layout_alv.

DATA:   g_user_command TYPE slis_formname VALUE 'USER_COMMAND',
        g_status       TYPE slis_formname VALUE 'STANDARD_FULLSCREEN'.


DATA: flag_list.  " R - refresh  C - cancela refresh
DATA: flag_outb, disp_whs.

DATA: gt_list_top_of_page TYPE slis_t_listheader,
      gt_events   TYPE slis_t_event.
DATA: ls_line TYPE slis_listheader.
DATA:   g_repid LIKE sy-repid,
        g_title(70).
DATA:   g_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE'.

DATA: flag TYPE i VALUE 0.

DATA: flag_edit.

RANGES: pulmao FOR zwm013-destino.
******************************************
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP.
PARAMETERS: p_lgnum LIKE zwm016-armazem OBLIGATORY.
PARAMETERS: p_porta LIKE zwm016-porta OBLIGATORY.
PARAMETERS: p_user LIKE sy-uname OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b0.


AT SELECTION-SCREEN ON p_lgnum.
  SELECT SINGLE * FROM lrf_wkqu WHERE lgnum = p_lgnum
                                  AND bname = sy-uname.
  IF sy-subrc <> 0.
    MESSAGE e051 WITH sy-uname p_lgnum.
  ENDIF.


INITIALIZATION.
  DATA: xuser LIKE lrf_wkqu OCCURS 0 WITH HEADER LINE.
* Read the user data from table lrf_wkqu ( for all the warehouses)
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = xuser
    EXCEPTIONS
      no_entry_found = 01.
  IF sy-subrc = 0.
    READ TABLE xuser INDEX 1.
    MOVE xuser-lgnum TO p_lgnum.
  ENDIF.

START-OF-SELECTION.
  SELECT SINGLE * FROM lrf_wkqu WHERE lgnum = p_lgnum
                                  AND bname = sy-uname.
  IF sy-subrc <> 0.
    MESSAGE e051 WITH sy-uname p_lgnum.
  ENDIF.


  PERFORM e03_eventtab_build USING gt_events[].
  PERFORM ajusta_propriedades TABLES sort.
  PERFORM ajusta_layout.
  g_repid = sy-repid.
  g_title = sy-title.

  flag_list = 'R'.

  WHILE NOT flag_list IS INITIAL.

    PERFORM fieldcat_init TABLES fieldcat.

    IF flag_list = 'R'.
      PERFORM get_dados.
      PERFORM lista_dados.
    ELSEIF flag_list = 'C'.
      PERFORM change_priority.
    ENDIF.


  ENDWHILE.




END-OF-SELECTION.
*---------------------------------------------------------------------*
*                                                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
*       FORM preenche_fieldname_tables                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fieldcat_init TABLES fieldcat TYPE slis_t_fieldcat_alv.
  DATA: pos TYPE i VALUE 1,
        aux_counter(2) TYPE c,
        fieldname(30) TYPE c.

  REFRESH fieldcat.


  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'EXIDV'.
  fieldcat-key           = 'X'.
  fieldcat-seltext_m     = 'SSCC'.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'NUMC'.
  APPEND fieldcat.
  ADD 1 TO pos.

  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'MATNR'.
  fieldcat-seltext_m     = 'Material'.
  fieldcat-key           = 'X'.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'NUMC'.
  APPEND fieldcat.
  ADD 1 TO pos.


  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'MAKTX'.
  fieldcat-seltext_m     = 'Material Description'.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'CHAR'.
  APPEND fieldcat.
  ADD 1 TO pos.

  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'VEMNG'.
  fieldcat-seltext_m     = 'Quantidade Real'.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'CHAR'.
  APPEND fieldcat.
  ADD 1 TO pos.

  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'VEMEH'.
  fieldcat-seltext_m     = 'Unidade'.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'CHAR'.
  APPEND fieldcat.
  ADD 1 TO pos.

  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'QUANTIDADE'.
  fieldcat-seltext_m     = 'Quantidade Teorica'.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'CHAR'.
  APPEND fieldcat.
  ADD 1 TO pos.

  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'UNI'.
  fieldcat-seltext_m     = 'Unidade'.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'CHAR'.
  APPEND fieldcat.
  ADD 1 TO pos.

  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'INCIDENCIA'.
  fieldcat-seltext_m     = 'Incidencia'.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'CHAR'.
  APPEND fieldcat.
  ADD 1 TO pos.

  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'DESCRICAO'.
  fieldcat-seltext_m     = 'Descrição Inc.'.
  fieldcat-outputlen     = 20.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'CHAR'.
  APPEND fieldcat.
  ADD 1 TO pos.

  CLEAR fieldcat.
  fieldcat-col_pos       =  pos.
  fieldcat-fieldname     = 'INHALT'.
  fieldcat-seltext_m     = 'Texto Aclaratório                      '.
  fieldcat-outputlen     = 40.
  fieldcat-sp_group      = '0001'.
  fieldcat-datatype      = 'CHAR'.

  IF NOT flag_edit IS INITIAL.
    fieldcat-edit          = 'X'.
    fieldcat-input         = 'X'.
  ELSE.
    fieldcat-edit          = ' '.
    fieldcat-input         = ' '.
  ENDIF.
  APPEND fieldcat.
  ADD 1 TO pos.


ENDFORM.                    "FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  Lista_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lista_dados.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            i_background_id          = 'ALV_BACKGROUND'
*              i_background_id         = 'ALV_WALLPAPER'
            i_grid_title             = g_title
            i_callback_pf_status_set = g_status
            i_callback_user_command  = g_user_command
            it_events                = gt_events[]
            i_callback_top_of_page   = g_top_of_page
            it_fieldcat              = fieldcat[]
            it_sort                  = sort[]
*            it_filter          = filter1[]
*              i_save             = 'A'
            is_layout                = layout
       TABLES
            t_outtab                 = qtd_totais.

  COMMIT WORK.
ENDFORM.                    " Lista_dados.
*&---------------------------------------------------------------------*
*&      Form  ajusta_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_layout.
  CLEAR layout.

  layout-colwidth_optimize = 'X'.
  layout-cell_merge = ' '.
  layout-zebra = 'X'.
*  LAYOUT-EDIT = 'X'.
*  layout-numc_sum = 'X'.

ENDFORM.                    " ajusta_layout


*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  REFRESH gt_list_top_of_page.
  PERFORM e04_comment_build  USING gt_list_top_of_page[].

*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*            i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
            i_logo             = 'RENOVA_LOGO'
*            i_logo             = 'UNIFY_IMAGE'
*             i_logo             = 'YMOCHO_IMT1'
            it_list_commentary = gt_list_top_of_page.


ENDFORM.                    "TOP_OF_PAGE


*---------------------------------------------------------------------*
*       FORM e04_comment_build                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  E04_LT_TOP_OF_PAGE                                            *
*---------------------------------------------------------------------*
FORM e04_comment_build USING e04_lt_top_of_page TYPE slis_t_listheader.
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  IF flag_list = 'R'.
    ls_line-info = 'Introdução de textos - Fecho do Processo'.
*  ELSEIF flag_list = 'C'.
*    ls_line-info = 'Cancel'.
  ENDIF.
  APPEND ls_line TO e04_lt_top_of_page.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'SapConsole-Warehouse'.
  ls_line-info = p_lgnum.
  APPEND ls_line TO e04_lt_top_of_page.

  CLEAR flag_list.
ENDFORM.                    "E04_COMMENT_BUILD


*&---------------------------------------------------------------------*
*&      Form  e03_eventtab_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS[]  text
*----------------------------------------------------------------------*
FORM e03_eventtab_build USING e03_lt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE g_top_of_page TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.

ENDFORM.                    " e03_eventtab_build


*---------------------------------------------------------------------*
*       FORM STANDARD_FULLSCREEN                                      *
*---------------------------------------------------------------------**
*---------------------------------------------------------------------*
*  -->  EXTAB                                                         *
*---------------------------------------------------------------------*
FORM standard_fullscreen USING  extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_ALV2' EXCLUDING extab.
ENDFORM.                    "STANDARD_FULLSCREEN

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  R_UCOMM                                                       *
*  -->  RS_SELFIELD                                                   *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
  DATA: l_ta TYPE sy-tcode VALUE 'SLIS_DUMMY'.
*

  CASE r_ucomm.
    WHEN 'EDIT'.
      IF flag_edit IS INITIAL.
        flag_edit = 'X'.
      ELSE.
        CLEAR flag_edit.
      ENDIF.
      flag_list = 'R'.
      r_ucomm = '&F03'.
    WHEN '&DATA_SAVE'.
      PERFORM update_queue.
      CLEAR flag_edit.
*      FLAG_LIST = 'R'.
      r_ucomm = '&F03'.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    "USER_COMMAND


*&---------------------------------------------------------------------*
*&      Form  set_variants
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_variants.

  DATA: filter1 TYPE slis_t_filter_alv WITH HEADER LINE,
        filter2 TYPE slis_t_filter_alv.

  filter1-fieldname = 'NAME1'.
  filter1-valuf     = ' '.
  APPEND filter1.

  DATA: variant LIKE disvariant.
  variant-report = sy-repid.


  CALL FUNCTION 'REUSE_ALV_VARIANT_SAVE'
    EXPORTING
*   I_TABNAME_HEADER       =
*   I_TABNAME_ITEM         =
      it_fieldcat            = fieldcat[]
*   IT_SORT                =
      it_filter              = filter1[]
*   I_DIALOG               = 'X'
*   I_OVERWRITE            = ' '
*   I_USER_SPECIFIC        = ' '
* IMPORTING
*   E_EXIT                 =
   CHANGING
      cs_variant             = variant
   EXCEPTIONS
     wrong_input            = 1
     fc_not_complete        = 2
     foreign_lock           = 3
     varian_exists         = 4
     name_reserved          = 5
     program_error          = 6
     OTHERS                 = 7.

ENDFORM.                    " set_variants
*&---------------------------------------------------------------------*
*&      Form  get_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dados.

  DATA: BEGIN OF itab_aux OCCURS 0,
          matnr LIKE mara-matnr,
          index LIKE sy-tabix,
        END OF itab_aux.

  DATA porta(3).
  REFRESH: sscc, qtd_reais, qtd_teoricas, qtd_totais, pulmao.
  CLEAR: sscc, qtd_reais, qtd_teoricas, qtd_totais, porta, pulmao.
* quantidade reais

  CONCATENATE '0' p_porta+12(2) INTO porta.
  SELECT SINGLE *
      FROM zwm002
          WHERE armazem = p_lgnum AND
                porta = porta.

  pulmao-sign = 'I'.
  pulmao-option = 'EQ'.
  CONCATENATE 'PUL'  zwm002-pulmao_1 INTO pulmao-low SEPARATED BY ' '.
  APPEND pulmao.

  pulmao-sign = 'I'.
  pulmao-option = 'EQ'.
  CONCATENATE 'PUL'  zwm002-pulmao_2 INTO pulmao-low SEPARATED BY ' '.

  APPEND pulmao.

  SELECT p~sscc p~incidencia INTO (sscc-exidv,sscc-incidencia)
      FROM zwm016 AS k INNER JOIN zwm013 AS p
          ON  k~armazem = p~armazem
              WHERE k~armazem = p_lgnum AND
                    k~user_name = p_user AND
                    k~porta = p_porta AND
                    k~finalizada = ' '
                    AND
                    p~destino IN pulmao.

    APPEND sscc.
    CLEAR sscc.
  ENDSELECT.

  LOOP AT sscc.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF qtd_reais
      FROM vekp AS k INNER JOIN vepo AS p
          ON  k~venum = p~venum
              WHERE k~exidv = sscc-exidv AND
                    p~vepos = '000001'.
** Descrição da incidência
    SELECT SINGLE descricao FROM zwm022 INTO qtd_reais-descricao
                            WHERE armazem = p_lgnum AND
                                  incidencia = sscc-incidencia.

    qtd_reais-incidencia = sscc-incidencia.

** Quantidades teóricas
    itab_aux-matnr = qtd_reais-matnr.
    itab_aux-index = '1'.
    COLLECT itab_aux.
    CLEAR: itab_aux.
    SORT itab_aux.

    APPEND qtd_reais.
    CLEAR qtd_reais.
  ENDLOOP.

* quantidades teoricas
  MOVE p_porta+12(2) TO porta.
  CONCATENATE '0' porta INTO porta.
  SELECT * INTO CORRESPONDING FIELDS OF qtd_teoricas
        FROM zwm005 AS k INNER JOIN zwm017 AS p
            ON  k~armazem = p~armazem AND
                k~num_entrada = p~num_entrada
                WHERE k~armazem = p_lgnum AND
                      k~porta = porta.

    COLLECT qtd_teoricas.
    CLEAR qtd_teoricas.
  ENDSELECT.

  SORT qtd_reais BY exidv matnr.
  SORT qtd_teoricas BY material.

  LOOP AT qtd_reais.
    AT NEW matnr.
      CLEAR makt.
      READ TABLE qtd_teoricas WITH KEY material = qtd_reais-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input                = qtd_teoricas-uni
*         LANGUAGE             = SY-LANGU
       IMPORTING
*         LONG_TEXT            =
          output               = qtd_teoricas-uni
*         SHORT_TEXT           =
*       EXCEPTIONS
*         UNIT_NOT_FOUND       = 1
*         OTHERS               = 2
                .
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      MOVE-CORRESPONDING qtd_teoricas TO qtd_totais.
      SELECT SINGLE * FROM makt WHERE matnr = qtd_reais-matnr.
      MOVE makt-maktx TO qtd_totais-maktx.


    ENDAT.
** BS - 20.01.2004
*    SELECT SINGLE incidencia INTO qtd_totais-incidencia
*        FROM zwm015
*          WHERE armazem = p_lgnum AND
*                utilizador = sy-uname AND
*                sscc = qtd_reais-exidv.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input                = qtd_reais-vemeh
*         LANGUAGE             = SY-LANGU
       IMPORTING
*         LONG_TEXT            =
          output               = qtd_reais-vemeh
*         SHORT_TEXT           =
       EXCEPTIONS
         unit_not_found       = 1
         OTHERS               = 2
          .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    MOVE-CORRESPONDING qtd_reais TO qtd_totais.

** Quantidades teóricas
    READ TABLE itab_aux WITH KEY matnr = qtd_totais-matnr.
    IF sy-subrc EQ 0.
      qtd_totais-quantidade = qtd_totais-quantidade / itab_aux-index.
    ENDIF.

    APPEND qtd_totais.
    CLEAR qtd_totais.

  ENDLOOP.


ENDFORM.                    " get_dados
*&---------------------------------------------------------------------*
*&      Form  change_priority
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_priority.

*  READ TABLE TYTWM015 INDEX INDICE.

*  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*       EXPORTING
*            FUNCTIONCODE           = 'REF'
*       EXCEPTIONS
*            FUNCTION_NOT_SUPPORTED = 1
*            OTHERS                 = 2.

  MOVE 'R' TO flag_list.

ENDFORM.                    " change_priority

*---------------------------------------------------------------------*
*       FORM AJUSTA_PROPRIEDADES                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  SORT                                                          *
*---------------------------------------------------------------------*
FORM ajusta_propriedades TABLES sort     TYPE slis_t_sortinfo_alv.
* Ordenação
*  REFRESH SORT.
*
*  CLEAR SORT.
*  SORT-SPOS = 1.
*  SORT-FIELDNAME = 'USER_NAME'.
*  SORT-DOWN = 'X'.
*  APPEND SORT.
*
*  CLEAR SORT.
*  SORT-SPOS = 2.
*  SORT-FIELDNAME = 'LOCK'.
*  SORT-DOWN = 'X'.
*  APPEND SORT.
*
*  CLEAR SORT.
*  SORT-SPOS = 2.
*  SORT-FIELDNAME = 'GRUPO'.
*  SORT-DOWN = 'X'.
*  APPEND SORT.
*
*  CLEAR SORT.
*  SORT-SPOS = 3.
*  SORT-FIELDNAME = 'DEST_BIN'.
*  SORT-UP = 'X'.
*  APPEND SORT.

ENDFORM.                    " ajusta_propriedades
*&---------------------------------------------------------------------*
*&      Form  update_queue
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_queue.
  DATA: huchanged LIKE bapihuheader,
        return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  LOOP AT qtd_totais WHERE inhalt <> ' '.
    MODIFY qtd_totais INDEX sy-tabix.

    huchanged-content = qtd_totais-inhalt.
    .
    CALL FUNCTION 'BAPI_HU_CHANGE_HEADER'
      EXPORTING
        hukey           = qtd_totais-exidv
        huchanged       = huchanged
*     IMPORTING
*       HUHEADER        =
      TABLES
        return          = return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      IMPORTING
        return = return.


  ENDLOOP.

  UPDATE zwm016 SET finalizada = 'X'
      WHERE armazem = p_lgnum AND
            user_name = p_user AND
            porta = p_porta.
  COMMIT WORK.
ENDFORM.                    " update_queue
