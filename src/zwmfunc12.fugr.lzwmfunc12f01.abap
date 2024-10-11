*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC12F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SAMMELGANG_STARTEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_PALETES  text
*----------------------------------------------------------------------*
FORM sammelgang_starten CHANGING ct_paletes TYPE zpalete_picking_tt.
  DATA: lt_messages	       TYPE tab_bdcmsgcoll,
        lt_messages_b      TYPE tab_bdcmsgcoll,
        lt_total           TYPE TABLE OF l2sktotal,
        lt_ltak            TYPE TABLE OF ltak,
        lt_ltap            TYPE TABLE OF ltap,
        lt_ltak_sum        TYPE SORTED TABLE OF ltak WITH NON-UNIQUE KEY benum,
        lt_ltap_sum        TYPE SORTED TABLE OF ltap WITH NON-UNIQUE KEY tanum,
        lt_queue           TYPE TABLE OF lrf_queue,
        lt_lgpla           TYPE TABLE OF lgpla,
        lt_mlgn            TYPE SORTED TABLE OF mlgn WITH UNIQUE KEY matnr,
        lt_quantity_needed TYPE z_wm_cl_management=>t_quantity_needed,
        lt_delit           TYPE l03b_delit_t.

  DATA: ls_message         TYPE bdcmsgcoll,
        ls_total           TYPE l2sktotal,
        ls_t311            TYPE t311,
        lt_t311a_2s        TYPE TABLE OF t311a,
        ls_t311a_2s        TYPE t311a,
        lt_lips            TYPE TABLE OF lips,
        ls_lips            TYPE lips,
        lt_likp            TYPE TABLE OF likp,
        ls_likp            TYPE likp,
        ls_ltak            TYPE ltak,
        ls_ltap            TYPE ltap,
        ls_ltap_last       TYPE ltap,
        lt_vbuk            LIKE vbuk OCCURS 0 WITH HEADER LINE,
        lt_t311a           LIKE t311a OCCURS 0 WITH HEADER LINE,
        lt_ltap_create     TYPE TABLE OF ltap_creat,
        lt_ltap_create_c   TYPE TABLE OF ltap_creat,
        ls_ltap_create     TYPE ltap_creat,
        ls_ltap_create_c   TYPE ltap_creat,
        ls_mlgn            TYPE mlgn,
        lt_ltap_vb         TYPE TABLE OF ltap_vb,
        ls_ltap_vb         TYPE ltap_vb,
        ls_quantity_needed TYPE z_wm_cl_management=>quantity_needed,
        ls_delit           TYPE l03b_delit.

  DATA: lv_valor1         LIKE zwm001-valor,
        lv_kzakt_kzerl    TYPE flag,
        lv_2step          TYPE flag,
        lv_success        TYPE flag,
        lv_material_error TYPE flag,
        lv_tabix          TYPE sytabix,
        lv_lgpla          TYPE lgpla,
        lv_activo         TYPE flag,
        lv_2spart         TYPE flag,
        lv_2step_vb       TYPE flag,
        lv_um_pal         TYPE meins,
        lv_quantity       TYPE menge_d,
        lv_quantity_pal   TYPE menge_d.

  DATA: lr_lgpla TYPE RANGE OF lgpla.

  DATA:lr_s_lgpla LIKE LINE OF lr_lgpla.

  FIELD-SYMBOLS: <lv_matnr>         TYPE matnr,
                 <ls_ltap_create>   TYPE ltap_creat,
                 <ls_ltap_create_c> TYPE ltap_creat.

  it311 = sav_it311.

  CHECK NOT it311-tbedn IS INITIAL OR
        NOT it311-liefn IS INITIAL.


  CLEAR lcoms.
  MOVE:
        it311-lgnum    TO lcoms-lgnum ,
        it311-refnr    TO lcoms-refnr,
        it311-rbtyp    TO lcoms-rbtyp,
        con_dunkel     TO lcoms-dunkl,
        fcode_strt     TO lcoms-fcode.
  CLEAR lcoms-subrc.

  ls_t311 = it311.

  CHECK z_wm_cl_management=>is_group_completed(
            EXPORTING
              is_data = ls_t311
            IMPORTING
              e_2step = lv_2step
              e_2spart = lv_2spart
              et_quantity_needed = lt_quantity_needed
         ) EQ abap_false.


  IF NOT lt_quantity_needed IS INITIAL.
    SELECT * FROM likp
             INTO TABLE lt_likp
             FOR ALL ENTRIES IN lt_quantity_needed
             WHERE vbeln = lt_quantity_needed-vbeln.
  ENDIF.

  IF lv_2spart EQ abap_true.

    DO 1 TIMES.
      CHECK NOT lt_likp IS INITIAL AND
            NOT lt_quantity_needed IS INITIAL.


      SELECT * FROM mlgn
               INTO TABLE lt_mlgn
               FOR ALL ENTRIES IN lt_quantity_needed
               WHERE matnr = lt_quantity_needed-matnr AND
                     lgnum = ls_t311-lgnum.


      LOOP AT lt_likp INTO ls_likp.
        CLEAR: lt_ltap_create.

        LOOP AT lt_quantity_needed INTO ls_quantity_needed WHERE vbeln = ls_likp-vbeln AND
                                                                 two_step EQ abap_true.
          CLEAR: ls_ltap_create.
          ls_ltap_create-matnr = ls_quantity_needed-matnr.
          ls_ltap_create-werks = ls_quantity_needed-werks.
          ls_ltap_create-lgort = ls_quantity_needed-lgort.
          ls_ltap_create-anfme = ls_quantity_needed-menge.
          ls_ltap_create-altme = ls_quantity_needed-meinh.
          COLLECT ls_ltap_create INTO lt_ltap_create.
        ENDLOOP.
        DELETE lt_ltap_create  WHERE anfme <= 0.

        LOOP AT lt_ltap_create INTO ls_ltap_create.
          CLEAR: ls_mlgn.
          READ TABLE lt_mlgn
                INTO ls_mlgn
                WITH TABLE KEY matnr = ls_ltap_create-matnr.
          CHECK sy-subrc EQ 0.

          WHILE ls_ltap_create-anfme > 0.
            CLEAR: lt_ltap_create_c, ls_ltap_create_c, lv_quantity, lt_ltap_vb.

            lv_quantity = ls_ltap_create-anfme / ls_mlgn-lhmg1.
            lv_quantity_pal = 1.

            IF lv_quantity < lv_quantity_pal.
              lv_quantity = ls_ltap_create-anfme.
            ELSE.
              lv_quantity = ls_mlgn-lhmg1 * lv_quantity_pal.
            ENDIF.

            ls_ltap_create-anfme = ls_ltap_create-anfme - lv_quantity.

            ls_ltap_create_c = ls_ltap_create.
            ls_ltap_create_c-anfme = lv_quantity.

            IF ls_ltap_create-anfme > 0.
              IF z_wm_cl_management=>is_remontada( is_data = ls_mlgn ) EQ abap_true.
                ls_ltap_create_c-posnr = '99999'.
              ENDIF.
            ENDIF.

            APPEND ls_ltap_create_c TO lt_ltap_create_c.
            lv_tabix = sy-tabix.

            IF ls_ltap_create-anfme > 0.
              IF z_wm_cl_management=>is_remontada( is_data = ls_mlgn ) EQ abap_true.

                " cria tarefa com o dobro da quantidade para rementadas

                UNASSIGN <ls_ltap_create_c>.
                READ TABLE lt_ltap_create_c
                 ASSIGNING <ls_ltap_create_c>
                 INDEX lv_tabix.

                IF <ls_ltap_create_c> IS ASSIGNED.
                  ls_ltap_create-anfme = ls_ltap_create-anfme - <ls_ltap_create_c>-anfme.
                  <ls_ltap_create_c>-anfme = <ls_ltap_create_c>-anfme * 2.
                ENDIF.
              ENDIF.
            ENDIF.

            CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
              EXPORTING
                i_lgnum       = ls_t311-lgnum
                i_bwlvs       = '850'
                i_betyp       = 'L'
                i_benum       = ls_likp-vbeln
                i_commit_work = abap_true
                i_refnr       = ls_t311-refnr
                i_l2ska       = '1'
                i_kompl       = abap_false
              TABLES
                t_ltap_creat  = lt_ltap_create_c
                t_ltap_vb     = lt_ltap_vb
              EXCEPTIONS
                error_message = 1
                OTHERS        = 2.

            IF sy-subrc <> 0.
*               Não foi possivel criar OT para o material &!
              IF sy-msgid = 'L3' AND
                 ( sy-msgno = '008' OR sy-msgno = '332' ).
                ASSIGN ('(SAPLL03B)LTAP-MATNR') TO <lv_matnr>.
                IF <lv_matnr> IS ASSIGNED.
                  MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '278' WITH <lv_matnr>.
                ENDIF.
              ENDIF.

              MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                      WITH sy-msgv1
                           sy-msgv2
                           sy-msgv3
                           sy-msgv4.
              CONTINUE.
            ENDIF.

            CLEAR: ls_ltap_vb.
            READ TABLE lt_ltap_vb
                  INTO ls_ltap_vb
                  INDEX 1.

            CLEAR: ls_ltap_create_c.
            READ TABLE lt_ltap_create_c
                  INTO ls_ltap_create_c
                  INDEX 1.

            IF NOT ls_ltap_vb-vsolm IS INITIAL AND
               ls_ltap_vb-vsolm <> ls_ltap_create_c-anfme AND
               ls_ltap_create_c-anfme > ls_ltap_vb-vsolm.
              ls_ltap_create-anfme = ls_ltap_create-anfme + ( ls_ltap_create_c-anfme - ls_ltap_vb-vsolm ).
            ENDIF.
          ENDWHILE.
        ENDLOOP.
      ENDLOOP.
      CHECK sy-subrc EQ 0.
    ENDDO.

  ELSEIF lv_2step EQ abap_true AND
         lv_2spart EQ abap_false.

    CALL FUNCTION 'ZWM_TO_CREATE_2_STEP_PICKING'
      EXPORTING
        i_lgnum          = it311-lgnum
        i_refnr          = it311-refnr
        i_up_grp_anl     = ''
      IMPORTING
        e_material_error = lv_material_error
        et_messages      = lt_messages
      EXCEPTIONS
        error            = 1
        OTHERS           = 2.

    IF lv_material_error EQ abap_true.
*-->    Tenta retirar stock de picking
      CALL FUNCTION 'ZWM_TO_VALIDATE_PICKING'
        EXPORTING
          i_lgnum   = it311-lgnum
          i_refnr   = it311-refnr
        IMPORTING
          e_success = lv_success
        EXCEPTIONS
          error     = 1
          OTHERS    = 2.
    ENDIF.

    IF lv_success IS INITIAL.
      DELETE lt_messages WHERE msgid = 'L3' AND
                               msgnr = '726'.

      IF NOT lt_messages IS INITIAL.
        READ TABLE lt_messages
              INTO ls_message
              INDEX 1.
        MESSAGE ID ls_message-msgid TYPE ls_message-msgtyp NUMBER ls_message-msgnr
                WITH ls_message-msgv1
                     ls_message-msgv2
                     ls_message-msgv3
                     ls_message-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.

  DO 1 TIMES.
    SELECT SINGLE valor FROM zwm001
                        INTO lv_activo
                        WHERE armazem   = it311-lgnum AND
                              processo  = 'LIBERACAO_VIA_IDOC' AND
                              parametro = 'ACTIVAR'.
    CHECK lv_activo EQ abap_true.

    SELECT valor FROM zwm001
                 INTO TABLE lt_queue
                 WHERE armazem   = it311-lgnum AND
                       processo  = 'GESTAO_FILAR' AND
                       parametro IN ('FILA_AUT_REP','FILA_AUT_SD','FILA_AUT_PRM').
    CHECK NOT lt_queue IS INITIAL.

    SELECT * FROM ltak
             INTO TABLE lt_ltak
             FOR ALL ENTRIES IN lt_queue
             WHERE lgnum = it311-lgnum AND
                   refnr = it311-refnr AND
                   kquit = abap_false AND
                   queue = lt_queue-table_line.

    SELECT * FROM ltak
             APPENDING TABLE lt_ltak
             FOR ALL ENTRIES IN lt_queue
             WHERE lgnum = it311-lgnum AND
                   betyp = 'Z' AND
                   benum = it311-refnr AND
                   kquit = abap_false AND
                   queue = lt_queue-table_line.
    CHECK NOT lt_ltak IS INITIAL.

    SELECT * FROM ltap
             INTO TABLE lt_ltap
             FOR ALL ENTRIES IN lt_ltak
             WHERE lgnum = lt_ltak-lgnum AND
                   tanum = lt_ltak-tanum AND
                   pvqui = abap_false.
    CHECK sy-subrc EQ 0.

    LOOP AT lt_ltap INTO ls_ltap.
      CONCATENATE ls_ltap-vlpla(8) '*' INTO lv_lgpla.
      CLEAR: lr_s_lgpla.
      lr_s_lgpla-low = lv_lgpla.
      lr_s_lgpla-option = 'CP'.
      lr_s_lgpla-sign   = 'I'.
      APPEND lr_s_lgpla TO lr_lgpla.
    ENDLOOP.
    CHECK NOT lr_lgpla IS INITIAL.
    SORT lr_lgpla.
    DELETE ADJACENT DUPLICATES FROM lr_lgpla.

    SELECT * FROM ltap
             INTO TABLE lt_ltap
             WHERE lgnum = it311-lgnum AND
                   pvqui = abap_false AND
                   vlpla IN lr_lgpla.
    CHECK sy-subrc EQ 0.

    SELECT * FROM ltak
             INTO TABLE lt_ltak
             FOR ALL ENTRIES IN lt_ltap
             WHERE lgnum = lt_ltap-lgnum AND
                   tanum = lt_ltap-tanum.

    LOOP AT lt_ltak INTO ls_ltak.
      lv_tabix = sy-tabix.
      READ TABLE lt_queue
        WITH KEY table_line = ls_ltak-queue
        TRANSPORTING NO FIELDS.
      CHECK sy-subrc <> 0.
      DELETE lt_ltak WHERE tanum = ls_ltak-tanum.
      DELETE lt_ltap WHERE tanum = ls_ltak-tanum.
    ENDLOOP.

    CHECK NOT lt_ltak IS INITIAL.

    SORT lt_ltap BY vlpla DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_ltap COMPARING vlpla.

    LOOP AT lt_ltap INTO ls_ltap.
      IF ls_ltap_last IS INITIAL.
        ls_ltap_last = ls_ltap.
        CONTINUE.
      ENDIF.

      IF ls_ltap-vlpla(8) EQ ls_ltap_last-vlpla(8).
        MESSAGE s296(zwmmsg001).
        RETURN.
      ENDIF.


      ls_ltap_last = ls_ltap.
    ENDLOOP.
  ENDDO.

  LOOP AT lt_likp INTO ls_likp.

    CLEAR: lt_delit.
    LOOP AT lt_quantity_needed INTO ls_quantity_needed WHERE vbeln = ls_likp-vbeln AND
                                                             two_step EQ abap_false.
      ls_delit-posnr = ls_quantity_needed-posnr.
      ls_delit-anfme = ls_quantity_needed-menge.
      ls_delit-altme = ls_quantity_needed-meinh.
      APPEND ls_delit TO lt_delit.
    ENDLOOP.
    CHECK NOT lt_delit IS INITIAL.

    CALL FUNCTION 'ZWM_TO_CREATE_DN'
      EXPORTING
        i_lgnum     = ls_t311-lgnum
        i_vbeln     = ls_likp-vbeln
        i_refnr     = ls_t311-refnr
        i_squit     = abap_false
        i_commit    = abap_true
        i_teilk     = abap_true
        it_delit    = lt_delit
      IMPORTING
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      PERFORM show_message
                  USING
                     lt_messages.
    ENDIF.

  ENDLOOP.


  PERFORM verify_volum_pal_picking.

  PERFORM delete_lines_zwm026.

  PERFORM split_to_dri.

  PERFORM corrige_to_aut.

  PERFORM verify_paletes_remontadas.

**  CALL FUNCTION 'ZWM_UPDATE_ANALISE_GRUPOS'
**    EXPORTING
**      i_lgnum = it311-lgnum
**      i_refnr = it311-refnr.
**
**  CLEAR: ls_t311.

****  IF lv_2step EQ 'X'.
****    CALL FUNCTION 'L_2_STEP_QUANTITY_REMOVAL'
****      EXPORTING
****        i_lgnum                       = lcoms-lgnum
****        i_refnr                       = lcoms-refnr
****      TABLES
****        t_total                       = lt_total
****      EXCEPTIONS
****        refnr_no_found                = 1
****        refnr_documents_no_found      = 2
****        no_relevant_for_2step_picking = 3
****        item_for_removal_not_found    = 4
****        OTHERS                        = 5.
****
****    IF sy-subrc <> 0.
****      CLEAR lv_kzakt_kzerl.
****    ELSE.
****      DELETE lt_total WHERE ofmng = 0.
****
****      IF lt_total IS INITIAL.
****        lv_kzakt_kzerl = 'X'.
****      ENDIF.
****    ENDIF.
****  ENDIF.
****
****
****  CLEAR ls_t311.
****  SELECT SINGLE * FROM t311
****                  INTO ls_t311
****                  WHERE lgnum = lcoms-lgnum AND
****                        refnr = lcoms-refnr.
****
****  LOOP AT it311 WHERE refnr = lcoms-refnr.
****    IF NOT ls_t311 IS INITIAL.
****
****      IF ls_t311-kzakt = 'X'.
****        SELECT * FROM t311a INTO TABLE lt_t311a
****            WHERE lgnum = gv_lgnum
****              AND refnr = lcoms-refnr.
****        IF lt_t311a[] IS NOT INITIAL.
****          SELECT * FROM vbuk INTO TABLE lt_vbuk
****              FOR ALL ENTRIES IN lt_t311a
****                  WHERE vbeln = lt_t311a-rbnum.
****
****          "remove remessas em 2 passos
****          LOOP AT lt_vbuk.
****            lv_tabix = sy-tabix.
****
****            CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
****              EXPORTING
****                i_lgnum = ls_t311-lgnum
****                i_refnr = ls_t311-refnr
****                i_vbeln = lt_vbuk-vbeln
****              IMPORTING
****                e_2step = lv_2step_vb
****              EXCEPTIONS
****                error   = 1
****                OTHERS  = 2.
****
****            IF lv_2step_vb EQ abap_true.
****              DELETE lt_vbuk INDEX lv_tabix.
****            ENDIF.
****          ENDLOOP.
****
****          DELETE lt_vbuk WHERE kostk = 'C'.
****        ENDIF.
****        IF lt_vbuk[] IS INITIAL.
****          lv_kzakt_kzerl = abap_true.
****        ELSE.
****          lv_kzakt_kzerl = abap_false.
****        ENDIF.
****      ENDIF.
****    ENDIF.
****
****    it311-kreuz = space.
****
****    MODIFY it311.
****  ENDLOOP.
****
****  IF lv_kzakt_kzerl EQ abap_true.
****    UPDATE  t311 SET kzakt = lv_kzakt_kzerl
****                     kzerl = lv_kzakt_kzerl
****                WHERE lgnum = lcoms-lgnum AND
****                      refnr = lcoms-refnr.
****    COMMIT WORK.
****  ENDIF.


  MOVE sav_fname TO hlp_fname.
  MOVE sav_rbtyp TO sum00-rbtyp.

  sy-lsind = sy-lsind - 1.
  PERFORM detailliste1.
ENDFORM.                    "sammelgang_starten
*&---------------------------------------------------------------------*
*&      Form  CORRIGE_TO_AUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM corrige_to_aut .
  DATA: valor1     LIKE zwm001-valor,
        um_pal     LIKE marm-meinh,
        l_tanum    LIKE ltap-tanum,
        quantidade TYPE int2,
        resto      TYPE int2,
        n_pal      TYPE i,
        aux_benum  LIKE ltak-benum,
        l_vsola    LIKE ltap-vsola.

  DATA t_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: lv_2step TYPE flag.

  CLEAR: t_ltak, t_ltap, c_ltap, return_msg, t_sscc,
         to_prm, to, qtd_total, to_remontada, n_pal.

  REFRESH: t_ltak, t_ltap, c_ltap, return_msg, t_sscc.

  READ TABLE it311 WITH KEY kreuz = 'X'.

  CHECK it311-lgnum EQ '150'.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = it311-lgnum
      i_refnr = it311-refnr
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

  SELECT * INTO TABLE t_ltak
        FROM ltak
            WHERE lgnum = it311-lgnum AND
                  refnr = it311-refnr.

  CHECK NOT t_ltak[] IS INITIAL.

  SELECT * FROM ltap INTO TABLE t_ltap
      FOR ALL ENTRIES IN t_ltak
            WHERE lgnum = t_ltak-lgnum AND
                  tanum = t_ltak-tanum.

  IF lv_2step EQ abap_false.
    DELETE t_ltap WHERE vorga = 'ST'
                     OR vbeln = ' '
                     OR vltyp <> 'AUT'.

  ELSE.
    DELETE t_ltap WHERE vorga = 'ST'
                     OR vltyp <> 'AUT'.
  ENDIF.

  CLEAR: valor1, um_pal, l_tanum.
  SELECT SINGLE valor INTO valor1
      FROM zwm001
          WHERE armazem = it311-lgnum AND
                processo = 'PALETIZACAO' AND
                parametro = 'PALETE'.

  um_pal = valor1.

  LOOP AT t_ltap.
    READ TABLE t_ltak
      WITH KEY tanum = t_ltap-tanum.

    CLEAR: resto, quantidade, marm, t_sscc.
    REFRESH: t_sscc.

    SELECT SINGLE * FROM marm
    WHERE matnr EQ t_ltap-matnr
      AND meinh EQ um_pal.

    quantidade = t_ltap-vsolm DIV marm-umrez.

    resto = t_ltap-vsolm MOD marm-umrez.

    IF quantidade > 1 OR resto IS NOT INITIAL.

      CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
        EXPORTING
          warehouse     = t_ltap-lgnum
          tanum         = t_ltap-tanum
          tapos         = t_ltap-tapos
        TABLES
          return_msg    = return_msg
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

        l_vsola = marm-umrez.

        DO 30 TIMES.
          CALL FUNCTION 'ZWM_TO_CREATE_OUT'
            EXPORTING
              warehouse     = gv_lgnum
              refnr         = it311-refnr
              vbeln         = t_ltap-vbeln
              posnr         = t_ltap-posnr
              vsola         = l_vsola
              meins         = t_ltap-altme
              vltyp         = t_ltap-vltyp
              vlpla         = t_ltap-vlpla
              werks         = t_ltap-werks
              lgort         = t_ltap-lgort
              matnr         = t_ltap-matnr
              benum         = t_ltak-benum
            IMPORTING
              to            = to
            TABLES
              return_msg    = return_msg
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.

          IF sy-subrc <> 0.
            CLEAR to.
            WAIT UP TO 1 SECONDS.
          ELSE.
            EXIT.
          ENDIF.

        ENDDO.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " corrige_to_aut
*&---------------------------------------------------------------------*
*&      Form  DELETE_LINES_ZWM026
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_lines_zwm026 .

  DATA: l_lips   LIKE lips   OCCURS 0 WITH HEADER LINE,
        l_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        l_zwm040 LIKE zwm040 OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm026 TYPE zwm026 OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap   TYPE ltap   OCCURS 0 WITH HEADER LINE.

  DATA: lt_t311a TYPE TABLE OF t311a.

  DATA: ls_t311  TYPE t311,
        ls_t311a TYPE t311a.

  DATA: lv_2step TYPE flag.

  CLEAR: l_lips, l_zwm028, l_zwm040.
  REFRESH: l_lips, l_zwm028, l_zwm040.

  READ TABLE it311 WITH KEY kreuz = 'X'.

  ls_t311 = it311.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      is_t311 = ls_t311
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE l_lips BYPASSING BUFFER
      FROM lips AS s INNER JOIN t311a AS p
           ON s~vbeln = p~rbnum
           WHERE p~lgnum = it311-lgnum AND
                 p~refnr = it311-refnr AND
               ( s~pstyv <> 'ZPAS' AND
                 s~pstyv <> 'ZPAL' ).

  SORT l_lips BY vbeln posnr.

  SELECT * INTO TABLE l_zwm028
      FROM zwm028
          WHERE lgnum = it311-lgnum AND
                refnr = it311-refnr.

  SORT l_zwm028 BY refnr remessa.

  DELETE l_zwm028 WHERE remessa IS INITIAL.

  SELECT * INTO TABLE l_zwm040
      FROM zwm040
          WHERE lgnum = it311-lgnum AND
                refnr = it311-refnr.

  SORT l_zwm040 BY refnr remessa.

  LOOP AT l_zwm028.
    READ TABLE l_zwm040 WITH KEY id_servisan = l_zwm028-remessa.
    IF sy-subrc <> 0.
      READ TABLE l_lips WITH KEY vbeln = l_zwm028-remessa.
      IF sy-subrc <> 0.
        DELETE FROM zwm028
            WHERE refnr = l_zwm028-refnr AND
                  remessa = l_zwm028-remessa.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_2step EQ 'X'.
    SELECT * FROM t311a
       INTO TABLE lt_t311a
       WHERE lgnum = ls_t311-lgnum AND
             refnr = ls_t311-refnr.

    LOOP AT lt_t311a INTO ls_t311a.
      READ TABLE l_lips WITH KEY vbeln = ls_t311a-rbnum.
      IF sy-subrc <> 0.
        DELETE FROM zwm040
            WHERE refnr = l_zwm040-refnr.
        COMMIT WORK.
      ENDIF.
    ENDLOOP.

  ELSE.

    LOOP AT l_zwm040.
      READ TABLE l_lips WITH KEY vbeln = l_zwm040-remessa.
      IF sy-subrc <> 0.
        DELETE FROM zwm040
            WHERE refnr = l_zwm040-refnr AND
                  remessa = l_zwm040-remessa.
        COMMIT WORK.
      ENDIF.
    ENDLOOP.
  ENDIF.

** Apagar da tabela de picking - Paletes com OTs estornadas
  SELECT *
    FROM zwm026 INTO TABLE lt_zwm026
    WHERE armazem = it311-lgnum
    AND   grupo   = it311-refnr.

  IF lt_zwm026[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_zwm026
      WHERE lgnum = it311-lgnum
      AND   tanum = lt_zwm026-to_number.

    DELETE lt_ltap WHERE vorga = 'ST' OR  vorga = 'SL'.
  ENDIF.

  LOOP AT lt_zwm026 WHERE to_number IS NOT INITIAL.

    READ TABLE lt_ltap WITH KEY tanum = lt_zwm026-to_number.
    CHECK sy-subrc <> 0.

    UPDATE zwm026 SET to_number = ''
                     WHERE armazem       = lt_zwm026-armazem
                       AND n_pal_picking = lt_zwm026-n_pal_picking
                       AND i_pal_picking = lt_zwm026-i_pal_picking
                       AND grupo         = lt_zwm026-grupo
                       AND remessa       = lt_zwm026-remessa
                       AND posnr         = lt_zwm026-posnr
                       AND sub_item      = lt_zwm026-sub_item.

    CHECK sy-subrc = 0.

    COMMIT WORK.

  ENDLOOP.

ENDFORM.                    " delete_lines_zwm026
*&---------------------------------------------------------------------*
*&      Form  DETAILLISTE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detailliste1.

  such_k_ers = ' '.
  such_k_off = ' '.
  such_k_erl = ' '.
  such_k_fre = ' '.
  such_k_tao = ' '.
*-- Suchfelder zuweisen -----------------------------------------------*
  CASE hlp_fname.
    WHEN erstellte.
      such_k_ers = 'X'.
      liste1_titel2 = text-a80.
    WHEN offene.
      such_k_off = 'X'.
      liste1_titel2 = text-a81.
    WHEN erledigte.
      such_k_erl = 'X'.
      liste1_titel2 = text-a82.
    WHEN freigegebene.
      such_k_fre = 'X'.
      liste1_titel2 = text-a83.
    WHEN alle.                         "->Zeilensumme, dh. kein Status
      liste1_titel2 = text-a84.
    WHEN offene_ta.
      such_k_tao = 'X'.
      liste1_titel2 = text-a85.
  ENDCASE.

  CASE sum00-rbtyp.
    WHEN con_refnrbelegtyp_b.
      ASSIGN it311-tbedn TO <feldname>.
      liste1_titel1 = text-a30.
    WHEN con_refnrbelegtyp_l.
      ASSIGN it311-liefn TO <feldname>.
      liste1_titel1 = text-a31.
    WHEN high_value(1).
      liste1_titel1 = text-a32.
  ENDCASE.

  such_rbtyp = sum00-rbtyp.

*-- IT311 gemäß der Suchfelder durchsuchen und ausgeben----------------*
  LOOP AT it311.

    CLEAR: it311-linno, it311-pagno.

    IF hlp_fname = alle.                      "Zeilensumme?
      IF such_rbtyp(1) = high_value(1).    "Gesamtsumme?

        IF it311-rbtyp = con_refnrbelegtyp_b OR "Selektion aller Sätze
           it311-rbtyp = con_refnrbelegtyp_l.         "Doppelsumme
          PERFORM detailliste1_ausgabe.
        ENDIF.

      ELSE.                                   "Keine Gesamtsumme!

        IF <feldname> = con_x.             "Selektion der Sätze in Zeile
          PERFORM detailliste1_ausgabe.
        ENDIF.

      ENDIF.
    ELSE.
      IF such_rbtyp(1) = high_value(1). "Gesamtsumme?

        IF hlp_fname = offene_ta.             "offene TA ?           "
          IF it311-k_tao = such_k_tao.                               "
            IF it311-rbtyp = con_refnrbelegtyp_b OR                  "
               it311-rbtyp = con_refnrbelegtyp_l.                    "
              PERFORM detailliste1_ausgabe.                          "
            ENDIF.                                                   "
          ENDIF.                                                     "
        ELSE.                                                        "
          IF it311-k_ers = such_k_ers AND       "Selektion der Sätze
             it311-k_off = such_k_off AND       "Gesamtsumme statusabh.
             it311-k_erl = such_k_erl AND
             it311-k_fre = such_k_fre.
            IF it311-rbtyp = con_refnrbelegtyp_b OR
               it311-rbtyp = con_refnrbelegtyp_l.
              PERFORM detailliste1_ausgabe.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.

        IF hlp_fname = offene_ta.             "offene TA ?           "
          IF it311-k_tao = such_k_tao.                               "
            IF <feldname> = con_x.            "Selektion der Sätze   "
              PERFORM detailliste1_ausgabe.                          "
            ENDIF.                                                   "
          ENDIF.                                                     "
        ELSE.                                                        "
          IF it311-k_ers = such_k_ers AND     "Selektion der Sätze
             it311-k_off = such_k_off AND     "Einzelsumme
             it311-k_erl = such_k_erl AND
             it311-k_fre = such_k_fre.
            IF <feldname> = con_x.            "Selektion der Sätze
              PERFORM detailliste1_ausgabe.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

    MODIFY it311.
    CLEAR it311.
  ENDLOOP.

  IF gv_offta EQ space.
    WRITE: /1(83) sy-uline.
  ELSE.
    WRITE: /1(87) sy-uline.
  ENDIF.

ENDFORM.                    "detailliste1
*&---------------------------------------------------------------------*
*&      Form  DETAILLISTE1_AUSGABE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detailliste1_ausgabe.

  PERFORM change_intensified.
  WRITE: /1 sy-vline.
  it311-linno = sy-linno.
  it311-pagno = sy-pagno.
  IF gv_offta EQ space.
    WRITE:  3(81) space  COLOR COL_NORMAL.
  ELSE.
    WRITE:  3(85) space  COLOR COL_NORMAL.
  ENDIF.
  WRITE:  2 it311-kreuz AS CHECKBOX INPUT,
          3 sy-vline,
          4 it311-refnr COLOR COL_NORMAL,
         14 sy-vline,
         15 it311-tbedn COLOR COL_NORMAL,
         18 it311-liefn COLOR COL_NORMAL,
         21 sy-vline,
         22 it311-refnt COLOR COL_NORMAL,
         62 sy-vline,
         63 it311-datum DD/MM/YY COLOR COL_NORMAL,
         71 sy-vline,
         72 it311-l2skr COLOR COL_NORMAL,
         74 sy-vline,
         76 it311-kzakt COLOR COL_NORMAL,
         79 it311-kzerl COLOR COL_NORMAL,
         82 it311-kzdru COLOR COL_NORMAL,
         83 sy-vline.
  IF gv_offta NE space.
    WRITE: 84 it311-k_tao COLOR COL_NORMAL,
           87 sy-vline.
  ENDIF.

  HIDE:     hlp_fname,
            it311-linno,
            it311-pagno,
            it311-refnr,
            it311-tbedn,
            it311-liefn,
            it311-refnt,
            it311-datum,
            it311-l2skr,
            it311-kzakt,
            it311-kzerl,
            it311-kzdru,
            it311-rbtyp,
            it311-k_tao,
            sy-tabix.

ENDFORM.                    "detailliste1_ausgabe
*&---------------------------------------------------------------------*
*&      Form  CHANGE_INTENSIFIED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_intensified.
  IF flg_intens = 0.
    FORMAT INTENSIFIED ON.
    flg_intens = 1.
  ELSE.
    FORMAT INTENSIFIED OFF.
    flg_intens = 0.
  ENDIF.
ENDFORM.                    "change_intensified
*&---------------------------------------------------------------------*
*&      Form  SPLIT_TO_DRI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM split_to_dri .
  DATA: valor1     LIKE zwm001-valor,
        um_pal     LIKE marm-meinh,
        l_tanum    LIKE ltap-tanum,
        quantidade TYPE int2,
        resto      TYPE int2,
        n_pal      TYPE i,
        aux_benum  LIKE ltak-benum,
        l_vsola    LIKE ltap-vsola.

  DATA t_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: lv_2step TYPE flag.

  CLEAR: t_ltak, t_ltap, c_ltap, return_msg, t_sscc,
         to_prm, to, qtd_total, to_remontada, n_pal.

  REFRESH: t_ltak, t_ltap, c_ltap, return_msg, t_sscc.

  READ TABLE it311 WITH KEY kreuz = 'X'.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = it311-lgnum
      i_refnr = it311-refnr
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

  SELECT * INTO TABLE t_ltak
        FROM ltak
            WHERE lgnum = it311-lgnum AND
                  refnr = it311-refnr.

  CHECK NOT t_ltak[] IS INITIAL.

  SELECT * FROM ltap INTO TABLE t_ltap
      FOR ALL ENTRIES IN t_ltak
            WHERE lgnum = t_ltak-lgnum AND
                  tanum = t_ltak-tanum.

  IF lv_2step EQ abap_false.
*    DELETE t_ltap WHERE vorga = 'ST'
*                     OR vbeln = ' '
*                     OR ( vltyp <> 'DRI' AND vltyp <> 'BLK').

    IF it311-lgnum EQ '100'.
      DELETE t_ltap WHERE vorga = 'ST'
                       OR vbeln = ' '
                       OR ( vltyp <> 'DRI' AND vltyp <> 'BLK' AND vltyp <> 'AUT').

    ELSE.
      DELETE t_ltap WHERE vorga = 'ST'
                       OR vbeln = ' '
                       OR ( vltyp <> 'DRI' AND vltyp <> 'BLK').
    ENDIF.

  ELSE.
    IF it311-lgnum EQ '100'.
      DELETE t_ltap WHERE vorga = 'ST'
                       OR ( vltyp <> 'DRI' AND vltyp <> 'BLK' AND vltyp <> 'AUT').
    ELSE.
      DELETE t_ltap WHERE vorga = 'ST'
                       OR ( vltyp <> 'DRI' AND vltyp <> 'BLK').
    ENDIF.
  ENDIF.

  CLEAR: valor1, um_pal, l_tanum.
  SELECT SINGLE valor INTO valor1
      FROM zwm001
          WHERE armazem = it311-lgnum AND
                processo = 'PALETIZACAO' AND
                parametro = 'PALETE'.

  um_pal = valor1.

  LOOP AT t_ltap.
    READ TABLE t_ltak
      WITH KEY tanum = t_ltap-tanum.


    CLEAR: resto, quantidade, marm, t_sscc.
    REFRESH: t_sscc.

    SELECT SINGLE * FROM marm
    WHERE matnr EQ t_ltap-matnr
      AND meinh EQ um_pal.

    quantidade = t_ltap-vsolm DIV marm-umrez.

    resto = t_ltap-vsolm MOD marm-umrez.

    " skip no caso de ser somente uma remontada
    IF quantidade EQ 2 AND z_wm_cl_management=>is_remontada( is_data = t_ltap ) EQ abap_true.
      CONTINUE.
    ENDIF.

    IF quantidade > 1  OR resto IS NOT INITIAL.

** To´s do Drive-in para cancelar
      CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
        EXPORTING
          warehouse     = t_ltap-lgnum
          tanum         = t_ltap-tanum
          tapos         = t_ltap-tapos
        TABLES
          return_msg    = return_msg
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

** Criar to´s do drive mas com a partição por paletes
        WHILE quantidade > 0.
          CLEAR l_vsola.
          IF z_wm_cl_management=>is_remontada( is_data = t_ltap ) EQ abap_true.
            IF quantidade >= 2.
              l_vsola = marm-umrez * 2.
              quantidade = quantidade - 2.
            ELSE.

              IF lv_2step EQ abap_true.
                l_vsola = marm-umrez.
              ENDIF.

              CLEAR quantidade.
            ENDIF.
          ELSE.
            l_vsola = marm-umrez.
            quantidade = quantidade - 1.
          ENDIF.

          DO 30 TIMES.

            CALL FUNCTION 'ZWM_TO_CREATE_OUT'
              EXPORTING
                warehouse     = gv_lgnum
                refnr         = it311-refnr
                vbeln         = t_ltap-vbeln
                posnr         = t_ltap-posnr
                vsola         = l_vsola
                meins         = t_ltap-altme
                vltyp         = t_ltap-vltyp
                vlpla         = t_ltap-vlpla
                werks         = t_ltap-werks
                lgort         = t_ltap-lgort
                matnr         = t_ltap-matnr
                charg         = t_ltap-charg
                benum         = t_ltak-benum
              IMPORTING
                to            = to
              TABLES
                return_msg    = return_msg
              EXCEPTIONS
                error_message = 1
                OTHERS        = 2.


            IF sy-subrc <> 0.
              CLEAR to.
              WAIT UP TO 1 SECONDS.
            ELSE.

              EXIT.
            ENDIF.

          ENDDO.
        ENDWHILE.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " split_to_dri
*&---------------------------------------------------------------------*
*&      Form  VERIFY_PALETES_REMONTADAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verify_paletes_remontadas.
  DATA: valor1     LIKE zwm001-valor,
        um_pal     LIKE marm-meinh,
        l_tanum    LIKE ltap-tanum,
        quantidade TYPE int2,
        resto      TYPE int2,
        n_pal      TYPE i,
        pal_quant  TYPE menge_d,
        rem_quant  TYPE menge_d,
        aux_benum  LIKE ltak-benum,
        aux_betyp  LIKE ltak-betyp.

  DATA: lt_ltap_sum TYPE TABLE OF ltap,
        lt_ltap_par TYPE SORTED TABLE OF ltap WITH UNIQUE KEY tanum tapos,
        lt_ltap_tri TYPE SORTED TABLE OF ltap WITH UNIQUE KEY tanum tapos,
        ls_ltap     TYPE ltap,
        ls_mlgn     TYPE mlgn.

  DATA: lt_ltak  TYPE SORTED TABLE OF ltak WITH UNIQUE KEY tanum,
        lt_t311a TYPE TABLE OF t311a,
        lt_ltap  TYPE TABLE OF ltap,
        lt_mlgn  TYPE TABLE OF mlgn.

  DATA: ls_ltak     TYPE ltak,
        ls_t311a    TYPE t311a,
        ls_ltak_sum TYPE ltak,
        ls_ltap_par TYPE ltap.

  DATA t_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: lv_2step  TYPE flag,
        lv_2spart TYPE flag,
        lv_tabix  TYPE sytabix.

  DATA: ls_return_msg	TYPE bdcmsgcoll.

  FIELD-SYMBOLS: <ls_ltap> TYPE ltap,
                 <ls_ltak> TYPE ltak.

  CLEAR: t_ltak, t_ltap, c_ltap, return_msg, t_sscc,
  to_prm, to, qtd_total, to_remontada, n_pal, pal_quant,
  rem_quant.

  REFRESH: t_ltak, t_ltap, c_ltap, return_msg, t_sscc.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  READ TABLE it311 WITH KEY kreuz = 'X'.

  CHECK sy-subrc = 0.

  SELECT * INTO TABLE t_ltak
      FROM ltak
          WHERE lgnum = it311-lgnum AND
                 refnr = it311-refnr.

  CHECK NOT t_ltak[] IS INITIAL.

  SELECT * FROM ltap INTO TABLE t_ltap
      FOR ALL ENTRIES IN t_ltak
          WHERE lgnum = t_ltak-lgnum AND
                tanum = t_ltak-tanum.


  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = it311-lgnum
      i_refnr  = it311-refnr
    IMPORTING
      e_2step  = lv_2step
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF lv_2step EQ abap_true AND
     lv_2spart EQ abap_false.
    LOOP AT t_ltap ASSIGNING <ls_ltap>.
      <ls_ltap>-vbeln = gc_vbeln_2step_dummy.
    ENDLOOP.
  ELSEIF lv_2spart EQ abap_true.

    LOOP AT t_ltap ASSIGNING <ls_ltap>.
      READ TABLE t_ltak
        ASSIGNING <ls_ltak>
        WITH KEY tanum = <ls_ltap>-tanum.
      CHECK sy-subrc EQ 0.
      CHECK <ls_ltak>-betyp = 'L'.
      CHECK NOT <ls_ltak>-benum IS INITIAL.

      <ls_ltap>-vbeln = <ls_ltak>-benum.
      <ls_ltak>-vbeln = <ls_ltak>-benum.
    ENDLOOP.
  ENDIF.


  DELETE t_ltap WHERE vorga = 'ST'
                         OR vbeln = ' '.

  DELETE t_ltap WHERE letyp NOT IN z_wm_cl_management=>r_letyp_remontada( it311-lgnum ).

  CLEAR: valor1, um_pal, l_tanum.
  SELECT SINGLE valor INTO valor1
      FROM zwm001
          WHERE armazem = it311-lgnum AND
                processo = 'PALETIZACAO' AND
                parametro = 'PALETE'.

  um_pal = valor1.

  lt_ltap_sum = t_ltap[].

  SORT t_ltap BY vbeln ASCENDING
                 matnr ASCENDING
                 vsolm ASCENDING.



  IF it311-lgnum EQ '100'.

    LOOP AT t_ltap
  WHERE ( vltyp = 'TRI' OR vltyp = 'DRI' OR vltyp = 'BLK' OR vltyp = 'AUT' ) AND
  ( letyp IN z_wm_cl_management=>r_letyp_remontada( it311-lgnum ) ) AND
  vbeln <> ' '.

*      IF t_ltap-vltyp = 'TRI' AND t_ltap-nltyp <> 'PRM'.
*        CONTINUE.
*      ENDIF.

      CLEAR: resto, quantidade, marm, t_sscc, ls_ltap_par.
      FREE: t_sscc.

      SELECT SINGLE * FROM marm
      WHERE matnr EQ t_ltap-matnr
      AND meinh EQ um_pal.

*      IF lv_2spart EQ abap_true.
      " Validar o Par da Palete
      IF t_ltap-vltyp = 'TRI'.
        CLEAR: ls_ltap.

        DELETE lt_ltap_sum WHERE tanum = t_ltap-tanum AND
                                 tapos = t_ltap-tapos.

        READ TABLE lt_ltap_par
             WITH TABLE KEY tanum = t_ltap-tanum
                            tapos = t_ltap-tapos
             TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          CONTINUE.
        ENDIF.

        LOOP AT lt_ltap_sum INTO ls_ltap WHERE vltyp = t_ltap-vltyp AND
                                               vlpla = t_ltap-vlpla AND
                                               nltyp = t_ltap-nltyp AND
                                               nlpla = t_ltap-nlpla AND
                                               matnr = t_ltap-matnr AND
                                               charg = t_ltap-charg AND
                                               vbeln = t_ltap-vbeln.
          lv_tabix = sy-tabix.

          DELETE lt_ltap_sum INDEX lv_tabix.
          t_ltap-vsolm = t_ltap-vsolm + ls_ltap-vsolm.
          INSERT ls_ltap INTO TABLE lt_ltap_par.
          EXIT.
        ENDLOOP.

        LOOP AT lt_ltap_sum INTO ls_ltap_par WHERE vltyp = t_ltap-vltyp AND
                                                   vlpla = t_ltap-vlpla AND
                                                   matnr = t_ltap-matnr AND
                                                   charg = t_ltap-charg.
          EXIT.
        ENDLOOP.
      ENDIF.

      quantidade = t_ltap-vsolm DIV marm-umrez.

      resto = quantidade MOD 2.

      IF NOT resto IS INITIAL.

        t_sscc-material = t_ltap-matnr.
        t_sscc-quantidade = '1'.
        t_sscc-uni = um_pal.
*        t_sscc-lote_producao = t_ltap-charg.
        APPEND t_sscc.

        APPEND t_sscc.

        CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
          EXPORTING
            warehouse     = t_ltap-lgnum
            tanum         = t_ltap-tanum
            tapos         = t_ltap-tapos
          TABLES
            return_msg    = return_msg
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        " Estornar OT do Palete do PAR
        IF ls_ltap_par IS NOT INITIAL.

          CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
            EXPORTING
              warehouse     = ls_ltap_par-lgnum
              tanum         = ls_ltap_par-tanum
              tapos         = ls_ltap_par-tapos
            TABLES
              return_msg    = return_msg
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF.

        CLEAR t_sscc.

        CLEAR aux_benum.

        aux_betyp = 'I'.
        aux_benum = it311-refnr.
        IF lv_2step EQ abap_false OR
           lv_2spart EQ abap_true.
          aux_betyp = 'L'.
          aux_benum = t_ltap-vbeln.
        ENDIF.

        CLEAR: l_tanum.
        DO 2 TIMES.
          CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
            EXPORTING
              warehouse  = t_ltap-lgnum
              mov_type   = '940'
              st_type_o  = t_ltap-vltyp
*             bin_origem = t_ltap-vlpla
              plant      = t_ltap-werks
              s_loc      = t_ltap-lgort
              req_number = aux_benum
              req_type   = 'I'
              origem     = 'X'
              refnr      = it311-refnr
            IMPORTING
              to         = l_tanum
            TABLES
              return_msg = return_msg
              sscc       = t_sscc
            EXCEPTIONS
              error      = 1
              OTHERS     = 2.
          IF sy-subrc <> 0.
            WAIT UP TO 1 SECONDS.
            PERFORM complete_log_message USING 'REMONTADA_001'
                                               aux_benum
                                               '' ''
                                               t_ltap-matnr
                                         CHANGING return_msg[].
            CALL FUNCTION 'ZWM_LOG_MESSAGE'
              EXPORTING
                i_master    = t_ltap-lgnum
                i_object    = 'ZWM001'
                i_subobject = 'ZWM006'
                i_extnumber = '1'
                i_commit    = 'X'
                it_messages = return_msg[].
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        IF ls_ltap_par IS NOT INITIAL.
          CLEAR t_sscc.

          CLEAR aux_benum.

          aux_betyp = 'I'.
          aux_benum = it311-refnr.
          IF lv_2step EQ abap_false OR
             lv_2spart EQ abap_true.
            aux_betyp = 'L'.
            aux_benum = ls_ltap_par-vbeln.
          ENDIF.

          CLEAR: l_tanum.
          DO 2 TIMES.
            CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
              EXPORTING
                warehouse  = ls_ltap_par-lgnum
                mov_type   = '940'
                st_type_o  = ls_ltap_par-vltyp
*               bin_origem = t_ltap-vlpla
                plant      = ls_ltap_par-werks
                s_loc      = ls_ltap_par-lgort
                req_number = aux_benum
                req_type   = 'I'
                origem     = 'X'
                refnr      = it311-refnr
              IMPORTING
                to         = l_tanum
              TABLES
                return_msg = return_msg
                sscc       = t_sscc
              EXCEPTIONS
                error      = 1
                OTHERS     = 2.
            IF sy-subrc <> 0.
              WAIT UP TO 1 SECONDS.
              PERFORM complete_log_message USING 'REMONTADA_001'
                                                 aux_benum
                                                 '' ''
                                                 t_ltap-matnr
                                           CHANGING return_msg[].
              CALL FUNCTION 'ZWM_LOG_MESSAGE'
                EXPORTING
                  i_master    = t_ltap-lgnum
                  i_object    = 'ZWM001'
                  i_subobject = 'ZWM006'
                  i_extnumber = '1'
                  i_commit    = 'X'
                  it_messages = return_msg[].
            ELSE.

              INSERT ls_ltap_par INTO TABLE lt_ltap_par.
              EXIT.
            ENDIF.
          ENDDO.
        ENDIF.

      ENDIF.
    ENDLOOP.

    EXIT.
  ENDIF.

*  LOOP AT t_ltap
*  WHERE ( vltyp = 'DRI' OR vltyp = 'BLK' OR vltyp = 'AUT' ) AND
*  ( letyp IN z_wm_cl_management=>r_letyp_remontada( it311-lgnum ) ) AND
*  vbeln <> ' '.
*
*    IF t_ltap-lgnum <> '100' AND t_ltap-vltyp EQ 'AUT'.
*      CONTINUE.
*    ENDIF.
*
*    CLEAR: resto, quantidade, marm, t_sscc.
*    FREE: t_sscc.
*
*    SELECT SINGLE * FROM marm
*    WHERE matnr EQ t_ltap-matnr
*    AND meinh EQ um_pal.
*
*    IF lv_2spart EQ abap_true.
*      CLEAR: ls_ltap.
*      DELETE lt_ltap_sum WHERE tanum = t_ltap-tanum AND
*                               tapos = t_ltap-tapos.
*
*      READ TABLE lt_ltap_par
*           WITH TABLE KEY tanum = t_ltap-tanum
*                          tapos = t_ltap-tapos
*           TRANSPORTING NO FIELDS.
*      IF sy-subrc EQ 0.
*        CONTINUE.
*      ENDIF.
*
*
*
*      LOOP AT lt_ltap_sum INTO ls_ltap WHERE vltyp = t_ltap-vltyp AND
*                                             vlpla = t_ltap-vlpla AND
*                                             nltyp = t_ltap-nltyp AND
*                                             nlpla = t_ltap-nlpla AND
*                                             matnr = t_ltap-matnr AND
*                                             charg = t_ltap-charg AND
*                                             vbeln = t_ltap-vbeln.
*        lv_tabix = sy-tabix.
*
*        DELETE lt_ltap_sum INDEX lv_tabix.
*        t_ltap-vsolm = t_ltap-vsolm + ls_ltap-vsolm.
*        INSERT ls_ltap INTO TABLE lt_ltap_par.
*        EXIT.
*      ENDLOOP.
*    ENDIF.
*
*    quantidade = t_ltap-vsolm DIV marm-umrez.
*
*    resto = quantidade MOD 2.
*    IF NOT resto IS INITIAL.
*      t_sscc-material = t_ltap-matnr.
*      t_sscc-quantidade = '1'.
*      t_sscc-uni = um_pal.
*      t_sscc-lote_producao = t_ltap-charg.
*      APPEND t_sscc.
*
*      IF t_ltap-vltyp EQ 'AUT'.
*        APPEND t_sscc.
*
*        CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
*          EXPORTING
*            warehouse     = t_ltap-lgnum
*            tanum         = t_ltap-tanum
*            tapos         = t_ltap-tapos
*          TABLES
*            return_msg    = return_msg
*          EXCEPTIONS
*            error_message = 1
*            OTHERS        = 2.
*        IF sy-subrc <> 0.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        ENDIF.
*      ENDIF.
*
*      CLEAR t_sscc.
*
*      CLEAR aux_benum.
*
*      aux_benum = it311-refnr.
*      IF lv_2spart EQ abap_true.
*        aux_benum = t_ltap-vbeln.
*      ENDIF.
*
*      CLEAR: l_tanum.
*      DO 30 TIMES.
*        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
*          EXPORTING
*            warehouse  = t_ltap-lgnum
*            mov_type   = '940'
*            st_type_o  = t_ltap-vltyp
*            bin_origem = t_ltap-vlpla
*            plant      = t_ltap-werks
*            s_loc      = t_ltap-lgort
*            req_number = aux_benum
*            req_type   = 'I'
*            origem     = 'X'
*          IMPORTING
*            to         = l_tanum
*          TABLES
*            return_msg = return_msg
*            sscc       = t_sscc
*          EXCEPTIONS
*            error      = 1
*            OTHERS     = 2.
*        IF sy-subrc <> 0.
*          WAIT UP TO 1 SECONDS.
*          PERFORM complete_log_message USING 'REMONTADA_001'
*                                             aux_benum
*                                             '' ''
*                                             t_ltap-matnr
*                                       CHANGING return_msg[].
*          CALL FUNCTION 'ZWM_LOG_MESSAGE'
*            EXPORTING
*              i_master    = t_ltap-lgnum
*              i_object    = 'ZWM001'
*              i_subobject = 'ZWM006'
*              i_extnumber = '1'
*              i_commit    = 'X'
*              it_messages = return_msg[].
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDDO.
*
*
*    ENDIF.
*
*  ENDLOOP.
*
*  CLEAR c_ltap.
*  REFRESH c_ltap.
*** Verifcar se tem paletes remontadas vindas dos trilaterais
*** para a remessa
*  SORT t_ltap BY vlenr.
*
*  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.
*
*  LOOP AT t_ltap
*  WHERE ( vltyp = 'TRI' OR vltyp = 'AUT' ) AND
*  ( letyp IN z_wm_cl_management=>r_letyp_remontada( it311-lgnum ) ) AND
*  vbeln <> ' '.
*
*    IF t_ltap-vltyp EQ 'AUT' AND
*       t_ltap-lgnum EQ '100'.
*      CONTINUE.
*    ENDIF.
*
*    CLEAR zwm020.
*    SELECT SINGLE * FROM zwm020
*    WHERE armazem = t_ltap-lgnum AND
*    ( p1 = t_ltap-vlenr OR p2 = t_ltap-vlenr ).
*
*    IF sy-subrc = 0.
*      CLEAR to_prm.
*      CLEAR c_ltap.
*      REFRESH c_ltap.
*      MOVE-CORRESPONDING t_ltap TO c_ltap.
*      APPEND c_ltap.
*      CLEAR c_ltap.
*
*      IF zwm020-p1 = t_ltap-vlenr.
*** Verificar se existem duas to´s para as duas paletes remontadas
*
*** Se tiverem as duas paletes tem de se estornar as duas tos e voltar a
*** criar as duas to's sendo a primeira a da to de baixo
*
*** Senão estorna a to e cria a de baixo para a remessa e a de cima para
*** a zona PRM
*
*        READ TABLE t_ltap WITH KEY vlenr = zwm020-p2.
*        IF sy-subrc = 0.
*          MOVE-CORRESPONDING t_ltap TO c_ltap.
*          APPEND c_ltap.
*          CLEAR c_ltap.
*        ELSE.
*          to_prm = 'X'.
*        ENDIF.
*
*      ELSEIF zwm020-p2 = t_ltap-vlenr.
*
*        READ TABLE t_ltap WITH KEY vlenr = zwm020-p1.
*        IF sy-subrc = 0.
*          MOVE-CORRESPONDING t_ltap TO c_ltap.
*          APPEND c_ltap.
*          CLEAR c_ltap.
*        ELSE.
*          to_prm = 'X'.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*** Verificar se estao as duas paletes remontadas para a saida.
*** Se for as duas paletes para saida verificar se ja estao criadas pela
*** ordem correcta
*
*    CLEAR: n_pal, pal_quant, rem_quant.
*
*    DESCRIBE TABLE c_ltap LINES n_pal.
*    IF n_pal = 2.
*      SORT c_ltap BY lgnum tanum tapos.
*      READ TABLE c_ltap INDEX 1.
*      IF c_ltap-vlenr = zwm020-p1.
*** Validar se as duas paletes são para a mesma remessa
*        CLEAR n_pal.
*        LOOP AT c_ltap WHERE vbeln = t_ltap-vbeln.
*          n_pal = n_pal + 1.
*        ENDLOOP.
*        IF n_pal = 1.
*** As duas paletes são para remessas diferentes
*
***        No caso de ser grupo em 2 passos, nunca vai ser chamado este PERFORM pois todas as
***        paletes são para o grupo e a remessa é igual (remessa dummy)
*          PERFORM split_paletes_remontadas.
*          CONTINUE.
*        ELSEIF n_pal = 2.
*** As duas paletes são para a mesma remessa
*          CONTINUE.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
*
*    LOOP AT c_ltap.
*
*      CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
*        EXPORTING
*          warehouse     = c_ltap-lgnum
*          tanum         = c_ltap-tanum
*          tapos         = c_ltap-tapos
*        TABLES
*          return_msg    = return_msg
*        EXCEPTIONS
*          error_message = 1
*          OTHERS        = 2.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*    ENDLOOP.
*
*    READ TABLE c_ltap INDEX 1.
*
*    READ TABLE t_ltak WITH KEY tanum = c_ltap-tanum.
*
*    IF to_prm IS INITIAL.
*
***    No caso de ser grupo em 2 passos, nunca vai ser chamado este IF pois todas as
***    paletes são para o grupo e a remessa é igual (remessa dummy)
*
*      WAIT UP TO 10 SECONDS.
*
*      DATA flag(1).
*      CLEAR flag.
*
*      DO 60 TIMES. "WHILE flag IS INITIAL.
*        CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
*          EXPORTING
*            warehouse     = c_ltap-lgnum
*            refnr         = t_ltak-refnr
*            vbeln         = t_ltak-vbeln
*            posnr         = c_ltap-posnr
*            vsola         = c_ltap-vsola
*            su            = zwm020-p1
*            su2           = zwm020-p2
*          IMPORTING
*            to            = to
*          TABLES
*            return_msg    = return_msg
*          EXCEPTIONS
*            error_message = 1
*            OTHERS        = 2.
*        IF sy-subrc EQ 0.
*          flag = 'X'.
*          EXIT.
*        ELSE.
*          WAIT UP TO 1 SECONDS.
*          CLEAR flag.
*        ENDIF.
*      ENDDO. "ENDWHILE.
*
*    ELSE.
*
*      WAIT UP TO 10 SECONDS.
*
*      CLEAR flag.
*
*      DO 60 TIMES.
*        CALL FUNCTION 'ZWM_TO_CREATE_OUT'
*          EXPORTING
*            warehouse     = c_ltap-lgnum
*            refnr         = t_ltak-refnr
*            vbeln         = t_ltak-vbeln
*            posnr         = c_ltap-posnr
*            vsola         = c_ltap-vsola
*            meins         = c_ltap-altme
*            su            = zwm020-p1
**           SU2           =
**           VLTYP         =
**           VLPLA         =
**           BACKGROUND    =
*            werks         = c_ltap-werks
*            lgort         = c_ltap-lgort
*            matnr         = c_ltap-matnr
*          IMPORTING
*            to            = to
*          TABLES
*            return_msg    = return_msg
*          EXCEPTIONS
*            error_message = 1
*            OTHERS        = 2.
*
*        IF sy-subrc EQ 0.
*          flag = 'X'.
*          EXIT.
*        ELSE.
*          WAIT UP TO 1 SECONDS.
*          CLEAR flag.
*        ENDIF.
*      ENDDO.
*
*      t_sscc-sscc = zwm020-p2.
*      t_sscc-tipo_su = c_ltap-letyp.
*      t_sscc-material = c_ltap-matnr.
*      t_sscc-quantidade = 1.
*      t_sscc-uni = um_pal.
*      t_sscc-lote_producao = c_ltap-charg.
*      APPEND t_sscc.
*      CLEAR t_sscc.
*
*      CLEAR aux_benum.
*      aux_benum = it311-refnr.
*      IF lv_2spart EQ abap_true.
*        aux_benum = t_ltap-vbeln.
*      ENDIF.
*
*      CLEAR flag.
*
*      DO 10 TIMES.
*
*        CALL FUNCTION 'ZWM_CREATE_TO'
*          EXPORTING
*            warehouse  = c_ltap-lgnum
*            mov_type   = '940'
*            material   = c_ltap-matnr
*            quantity   = '1'
*            unit       = 'PAL'
*            plant      = c_ltap-werks
*            s_loc      = c_ltap-lgort
*            lote       = c_ltap-charg
*            source_sty = c_ltap-vltyp
*            source_bin = c_ltap-vlpla
*            req_type   = 'I'
*            req_number = aux_benum
*            su         = zwm020-p2
*          IMPORTING
*            to         = to_remontada
*          TABLES
*            return_msg = return_msg
*          EXCEPTIONS
*            error      = 1
*            OTHERS     = 2.
*
*        IF sy-subrc = 0.
*          flag = 'X'.
*          EXIT.
*        ELSE.
*          CLEAR flag.
*          WAIT UP TO 1 SECONDS.
*
*          PERFORM complete_log_message USING 'REMONTADA_002'
*                                             aux_benum
*                                             zwm020-p1
*                                             zwm020-p2
*                                             c_ltap-matnr
*                                       CHANGING return_msg[].
*
*          CALL FUNCTION 'ZWM_LOG_MESSAGE'
*            EXPORTING
*              i_master    = c_ltap-lgnum
*              i_object    = 'ZWM001'
*              i_subobject = 'ZWM006'
*              i_extnumber = '2'
*              i_commit    = 'X'
*              it_messages = return_msg[].
*        ENDIF.
*
*      ENDDO.
*    ENDIF.
*
*    DELETE t_ltap WHERE vlenr = zwm020-p1 OR vlenr = zwm020-p2.
*    CLEAR c_ltap.
*    REFRESH c_ltap.
*
*  ENDLOOP.
*
*  READ TABLE it311 WITH KEY kreuz = 'X'.
*
*  REFRESH: t_ltak, t_ltap.
*  CLEAR: t_ltak, t_ltap.
*
*  CHECK sy-subrc = 0.
*
*  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
*    EXPORTING
*      i_lgnum  = it311-lgnum
*      i_refnr  = it311-refnr
*    IMPORTING
*      e_2spart = lv_2spart
*    EXCEPTIONS
*      error    = 1
*      OTHERS   = 2.
*
*  DO 1 TIMES.
*    CHECK lv_2spart EQ abap_true.
*
*    SELECT * FROM t311a
*             INTO TABLE lt_t311a
*             WHERE lgnum = it311-lgnum AND
*                   refnr = it311-refnr.
*
*    CHECK sy-subrc EQ 0.
*
*    SELECT * FROM ltak
*             INTO TABLE lt_ltak
*             FOR ALL ENTRIES IN lt_t311a
*             WHERE lgnum = lt_t311a-lgnum AND
*                   benum = lt_t311a-rbnum.
*
*    CHECK sy-subrc EQ 0.
*    DELETE lt_ltak WHERE kquit = abap_true.
*    CHECK NOT lt_ltak IS INITIAL.
*
*    SELECT * FROM ltap
*             INTO TABLE lt_ltap
*             FOR ALL ENTRIES IN lt_ltak
*             WHERE lgnum = lt_ltak-lgnum AND
*                   tanum = lt_ltak-tanum.
*
*    CHECK sy-subrc EQ 0.
*    DELETE lt_ltap WHERE pquit EQ abap_true.
*    CHECK NOT lt_ltap IS INITIAL.
*
*
*    SELECT * FROM mlgn
*             INTO TABLE lt_mlgn
*             FOR ALL ENTRIES IN lt_ltap
*             WHERE matnr = lt_ltap-matnr AND
*                   lgnum = lt_ltap-lgnum.
*    SORT lt_mlgn BY matnr.
*
*    CLEAR: lt_ltap_par.
*    lt_ltap_sum = lt_ltap.
*
*    LOOP AT lt_t311a INTO ls_t311a.
*      LOOP AT lt_ltak INTO ls_ltak WHERE benum = ls_t311a-rbnum.
*        LOOP AT lt_ltap INTO ls_ltap WHERE tanum = ls_ltak-tanum.
*          CHECK ls_ltap-vltyp EQ 'DRI' OR
*                ls_ltap-vltyp EQ 'BLK' OR
*                ls_ltap-vltyp EQ 'TRI'.
*
*          IF ls_ltap-lgnum EQ '100'.
*            CHECK ls_ltap-vltyp EQ 'AUT'.
*          ENDIF.
*
*          CHECK z_wm_cl_management=>is_remontada( is_data = ls_ltap ) EQ abap_true.
*
*          READ TABLE lt_ltap_par
*               WITH TABLE KEY tanum = ls_ltap-tanum
*                              tapos = ls_ltap-tapos
*               TRANSPORTING NO FIELDS.
*          IF sy-subrc EQ 0.
*            CONTINUE.
*          ENDIF.
*
*          CLEAR: n_pal, pal_quant, rem_quant.
*          LOOP AT lt_ltap_sum INTO ls_ltap WHERE vltyp = ls_ltap-vltyp AND
*                                                 vlpla = ls_ltap-vlpla AND
*                                                 matnr = ls_ltap-matnr AND
*                                                 charg = ls_ltap-charg.
*            lv_tabix = sy-tabix.
*
*            CLEAR: ls_ltak_sum.
*            READ TABLE lt_ltak
*                  INTO ls_ltak_sum
*                  WITH TABLE KEY tanum = ls_ltap-tanum.
*            CHECK sy-subrc EQ 0.
*
*            IF ls_ltak_sum-benum <> ls_ltak-benum.
*              CONTINUE.
*            ENDIF.
*
*            CLEAR: ls_mlgn.
*            READ TABLE lt_mlgn
*                  INTO ls_mlgn
*                  WITH KEY matnr = ls_ltap-matnr
*                  BINARY SEARCH.
*
*            CHECK sy-subrc EQ 0.
*
*            rem_quant = ls_mlgn-lhme1 * 2.
*
*
*            DELETE lt_ltap_sum INDEX lv_tabix.
*            INSERT ls_ltap INTO TABLE lt_ltap_par.
*
*            pal_quant = pal_quant + ls_ltap-vsolm.
*            n_pal = n_pal + 1.
*            IF pal_quant EQ rem_quant.
*              EXIT.
*            ENDIF.
*
*          ENDLOOP.
*
*          IF pal_quant <> rem_quant.
*            CALL FUNCTION 'ZWM_CREATE_TO'
*              EXPORTING
*                warehouse  = ls_ltap-lgnum
*                mov_type   = '940'
*                material   = ls_ltap-matnr
*                quantity   = '1'
*                unit       = 'PAL'
*                plant      = ls_ltap-werks
*                s_loc      = ls_ltap-lgort
*                lote       = ls_ltap-charg
*                source_sty = ls_ltap-vltyp
*                source_bin = ls_ltap-vlpla
*                req_type   = 'I'
*                req_number = ls_ltak-benum
*              TABLES
*                return_msg = return_msg
*              EXCEPTIONS
*                error      = 1
*                OTHERS     = 2.
*
*            IF sy-subrc <> 0.
*              PERFORM complete_log_message USING 'REMONTADA_003'
*                                                 ls_ltak-benum
*                                                 '' ''
*                                                 ls_ltap-matnr
*                                           CHANGING return_msg[].
*
*              CALL FUNCTION 'ZWM_LOG_MESSAGE'
*                EXPORTING
*                  i_master    = ls_ltap-lgnum
*                  i_object    = 'ZWM001'
*                  i_subobject = 'ZWM006'
*                  i_extnumber = '3'
*                  i_commit    = 'X'
*                  it_messages = return_msg[].
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDLOOP.
*    ENDLOOP.
*  ENDDO.
*
*
*
*
*  SELECT * INTO TABLE t_ltak
*  FROM ltak
*  WHERE lgnum = it311-lgnum AND
*  benum = it311-refnr AND
*  betyp = 'Z'.
*
*  CHECK NOT t_ltak[] IS INITIAL.
*
*  SELECT * FROM ltap INTO TABLE t_ltap
*  FOR ALL ENTRIES IN t_ltak
*  WHERE lgnum = t_ltak-lgnum AND
*  tanum = t_ltak-tanum.
*
*  DELETE t_ltap WHERE vorga = 'ST'.
*
*
*  CLEAR c_ltap.
*  REFRESH c_ltap.
*** Verifcar se tem paletes remontadas vindas dos trilaterais
*** para o reabastecimento
*  LOOP AT t_ltap
*  WHERE vltyp = 'TRI' AND
*  ( letyp IN z_wm_cl_management=>r_letyp_remontada( it311-lgnum ) ) AND
*  vbeln = ' '.
*
*    CLEAR zwm020.
*    SELECT SINGLE * FROM zwm020
*    WHERE armazem = t_ltap-lgnum AND
*    ( p1 = t_ltap-vlenr OR p2 = t_ltap-vlenr ).
*
*    IF sy-subrc = 0.
*      CLEAR to_prm.
*      CLEAR c_ltap.
*      REFRESH c_ltap.
*      MOVE-CORRESPONDING t_ltap TO c_ltap.
*      APPEND c_ltap.
*      CLEAR c_ltap.
*
*      IF zwm020-p1 = t_ltap-vlenr.
*** Verificar se existem duas to´s para as duas paletes remontadas
*
*** Se tiverem as duas paletes tem de se estornar as duas tos e voltar a
*** criar as duas to's sendo a primeira a da to de baixo
*
*        READ TABLE t_ltap WITH KEY vlenr = zwm020-p2.
*        IF sy-subrc = 0.
*          MOVE-CORRESPONDING t_ltap TO c_ltap.
*          APPEND c_ltap.
*          CLEAR c_ltap.
*        ENDIF.
*
*      ELSEIF zwm020-p2 = t_ltap-vlenr.
*
*        READ TABLE t_ltap WITH KEY vlenr = zwm020-p1.
*        IF sy-subrc = 0.
*          MOVE-CORRESPONDING t_ltap TO c_ltap.
*          APPEND c_ltap.
*          CLEAR c_ltap.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*** Verificar se estao as duas paletes remontadas para a saida.
*** Se for as duas paletes para saida verificar se ja estao criadas pela
*** ordem correcta
*    CLEAR n_pal.
*
*    DESCRIBE TABLE c_ltap LINES n_pal.
*
*    IF n_pal = 2.
*      SORT c_ltap BY lgnum tanum tapos.
*      READ TABLE c_ltap INDEX 1.
*      IF c_ltap-vlenr = zwm020-p1.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*
*    CLEAR t_sscc.
*    REFRESH t_sscc.
*    LOOP AT c_ltap.
*
***************************
***   Cancelar as OT
***************************
*
*      CLEAR t_ltap_cancl.
*      REFRESH t_ltap_cancl.
*
*      t_ltap_cancl-tanum = c_ltap-tanum.
*      t_ltap_cancl-tapos = c_ltap-tapos.
*      APPEND t_ltap_cancl.
*      CLEAR t_ltap_cancl.
*
*      CALL FUNCTION 'ZWM_CANCEL_TO'
*        EXPORTING
*          armazem      = c_ltap-lgnum
*        TABLES
*          t_ltap_cancl = t_ltap_cancl
*        EXCEPTIONS
*          error        = 1
*          OTHERS       = 2.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*    ENDLOOP.
*
***************************
***   Tabela para Criar Novas OT´s
***************************
*    READ TABLE c_ltap WITH KEY vlenr = zwm020-p1.
*    IF sy-subrc = 0.
*      t_sscc-sscc = c_ltap-vlenr.
*      t_sscc-tipo_su = c_ltap-letyp.
*      t_sscc-material = c_ltap-matnr.
*      t_sscc-quantidade = c_ltap-vsola.
*      t_sscc-uni = c_ltap-altme.
*      t_sscc-lote_producao = c_ltap-charg.
*      APPEND t_sscc.
*      CLEAR t_sscc.
*    ENDIF.
*
*    READ TABLE t_ltak WITH KEY lgnum = c_ltap-lgnum
*    tanum = c_ltap-tanum.
*
*    CLEAR c_ltap.
*    READ TABLE c_ltap WITH KEY vlenr = zwm020-p2.
*    IF sy-subrc = 0.
*      t_sscc-sscc = c_ltap-vlenr.
*      t_sscc-tipo_su = c_ltap-letyp.
*      t_sscc-material = c_ltap-matnr.
*      t_sscc-quantidade = c_ltap-vsola.
*      t_sscc-uni = c_ltap-altme.
*      t_sscc-lote_producao = c_ltap-charg.
*      APPEND t_sscc.
*      CLEAR t_sscc.
*    ENDIF.
*
*    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
*      EXPORTING
*        warehouse      = c_ltap-lgnum
*        mov_type       = '972'
*        st_type_o      = c_ltap-vltyp
*        bin_origem     = c_ltap-vlpla
*        st_type_d      = c_ltap-nltyp
*        bin_destino    = c_ltap-nlpla
*        plant          = c_ltap-werks
*        s_loc          = c_ltap-lgort
*        origem         = 'X'
*        req_number     = t_ltak-benum
*        req_type       = t_ltak-betyp
*        sscc_adicional = t_ltak-lznum
*      IMPORTING
*        to             = to
*      TABLES
*        return_msg     = return_msg
*        sscc           = t_sscc
*      EXCEPTIONS
*        error          = 1
*        OTHERS         = 2.
*    IF sy-subrc <> 0.
*
*    ENDIF.
*
*    CLEAR t_sscc.
*    REFRESH t_sscc.
*
*
*
*    DELETE t_ltap WHERE vlenr = zwm020-p1 OR vlenr = zwm020-p2.
*    CLEAR c_ltap.
*    REFRESH c_ltap.
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COMPLETE_LOG_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3678   text
*      -->P_AUX_BENUM  text
*      -->P_3680   text
*      -->P_3681   text
*      -->P_T_LTAP_MATNR  text
*      <--P_RETURN_MSG[]  text
*----------------------------------------------------------------------*
FORM complete_log_message  USING uv_log_zone
                                 uv_ref
                                 uv_lenum_v1
                                 uv_lenum_v2
                                 uv_matnr
                           CHANGING ct_return_msg TYPE tab_bdcmsgcoll.

  DATA: ls_return TYPE bdcmsgcoll.

  IF NOT uv_matnr IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Material'.
    ls_return-msgv2 = uv_matnr.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.

  IF NOT uv_lenum_v2 IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Palete 2'.
    ls_return-msgv2 = uv_lenum_v2.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.

  IF NOT uv_lenum_v1 IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Palete 1'.
    ls_return-msgv2 = uv_lenum_v1.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.

  IF NOT uv_ref IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Referencia'.
    ls_return-msgv2 = uv_ref.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.

  IF NOT uv_log_zone IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Zona de Log'.
    ls_return-msgv2 = uv_log_zone.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SPLIT_PALETES_REMONTADAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM split_paletes_remontadas .

  DATA: um_pal    LIKE marm-meinh,
        aux_benum LIKE ltak-benum,
        aux_betyp TYPE lvs_betyp,
        pal_p1    LIKE zwm020-p1,
        pal_p2    LIKE zwm020-p2.

  DATA: ls_zwm047     TYPE zwm047.
  DATA: lt_ltap_cancl LIKE ltap_cancl OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap       LIKE ltap       OCCURS 0 WITH HEADER LINE.

  DATA: flag_nao_estornar.
  DATA: flag_add_ot.

  DATA: lv_2spart TYPE flag.

  CLEAR: um_pal, aux_benum, pal_p1, pal_p2.

** Estornar as duas paletes
  LOOP AT c_ltap.

    CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
      EXPORTING
        warehouse     = c_ltap-lgnum
        tanum         = c_ltap-tanum
        tapos         = c_ltap-tapos
      TABLES
        return_msg    = return_msg
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.

  WAIT UP TO 1 SECONDS.
** Criar para a remessa a ot com a palete P1
  READ TABLE c_ltap INDEX 1.
  READ TABLE t_ltak WITH KEY tanum = c_ltap-tanum.

  CLEAR flag.
  DO 60 TIMES.
    CALL FUNCTION 'ZWM_TO_CREATE_OUT'
      EXPORTING
        warehouse     = c_ltap-lgnum
        refnr         = t_ltak-refnr
        vbeln         = t_ltak-vbeln
        posnr         = c_ltap-posnr
        vsola         = c_ltap-vsola
        meins         = c_ltap-altme
        su            = zwm020-p1
        werks         = c_ltap-werks
        lgort         = c_ltap-lgort
        matnr         = c_ltap-matnr
      IMPORTING
        to            = to
      TABLES
        return_msg    = return_msg
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

    IF sy-subrc EQ 0.
      flag = 'X'.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
      CLEAR flag.
    ENDIF.
  ENDDO. "ENDWHILE.

** Criar para o PRM a ot com a palete P2
  t_sscc-sscc = zwm020-p2.
  t_sscc-tipo_su = c_ltap-letyp.
  t_sscc-material = c_ltap-matnr.
  t_sscc-quantidade = 1.
  t_sscc-uni = um_pal.
  t_sscc-lote_producao = c_ltap-charg.
  APPEND t_sscc.
  CLEAR t_sscc.

  WAIT UP TO 1 SECONDS.

  CLEAR: aux_benum, aux_betyp.
  aux_benum = it311-refnr.
  aux_betyp = 'I'.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = c_ltap-lgnum
      i_refnr  = it311-refnr
    IMPORTING
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF lv_2spart EQ abap_true.
    aux_benum = t_ltak-vbeln.
  ENDIF.


  CLEAR flag.

  DO 60 TIMES.
    CALL FUNCTION 'ZWM_CREATE_TO'
      EXPORTING
        warehouse  = c_ltap-lgnum
        mov_type   = '940'
        material   = c_ltap-matnr
        quantity   = '1'
        unit       = 'PAL'
        plant      = c_ltap-werks
        s_loc      = c_ltap-lgort
        lote       = c_ltap-charg
        source_sty = c_ltap-vltyp
        source_bin = c_ltap-vlpla
        req_type   = aux_betyp
        req_number = aux_benum
        su         = zwm020-p2
      IMPORTING
        to         = to_remontada
      TABLES
        return_msg = return_msg
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    IF sy-subrc = 0.
      flag = 'X'.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
      CLEAR flag.

      PERFORM complete_log_message USING 'REMONTADA_004'
                                         aux_benum
                                         zwm020-p1
                                         zwm020-p2
                                         c_ltap-matnr
                                   CHANGING return_msg[].

      CALL FUNCTION 'ZWM_LOG_MESSAGE'
        EXPORTING
          i_master    = c_ltap-lgnum
          i_object    = 'ZWM001'
          i_subobject = 'ZWM006'
          i_extnumber = '4'
          i_commit    = 'X'
          it_messages = return_msg[].
    ENDIF.
  ENDDO.


  WAIT UP TO 1 SECONDS.

** Criar para a segunda delivery a ot segundo estratégia e
** verificar a palete que foi determinada
  READ TABLE c_ltap INDEX 2.
  READ TABLE t_ltak WITH KEY tanum = c_ltap-tanum.

  CLEAR: flag, to.
  DO 60 TIMES.
    CLEAR return_msg. REFRESH return_msg.

    CALL FUNCTION 'ZWM_TO_CREATE_OUT'
      EXPORTING
        warehouse     = c_ltap-lgnum
        refnr         = t_ltak-refnr
        vbeln         = t_ltak-vbeln
        posnr         = c_ltap-posnr
        vsola         = c_ltap-vsola
        meins         = c_ltap-altme
        werks         = c_ltap-werks
        lgort         = c_ltap-lgort
        matnr         = c_ltap-matnr
        charg         = c_ltap-charg
      IMPORTING
        to            = to
      TABLES
        return_msg    = return_msg
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

    IF sy-subrc EQ 0.
      flag = 'X'.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.
**********************************************************************
** Não existe mais stock, é única palete disponível
**********************************************************************
  IF to IS INITIAL.

    READ TABLE return_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      IF return_msg-msgid = 'L3' AND return_msg-msgnr = '008'.

*       Estornar OT que foi criada para PRM
        IF to_remontada IS NOT INITIAL.
          SELECT *
            FROM ltap INTO TABLE lt_ltap
            WHERE lgnum = c_ltap-lgnum AND
                  tanum = to_remontada.

          LOOP AT lt_ltap.
            CLEAR lt_ltap_cancl.
            lt_ltap_cancl-tanum = lt_ltap-tanum.
            lt_ltap_cancl-tapos = lt_ltap-tapos.
            APPEND lt_ltap_cancl.
          ENDLOOP.

          CALL FUNCTION 'L_TO_CANCEL_SU'
            EXPORTING
              i_lenum       = zwm020-p2
            TABLES
              t_ltap_cancl  = lt_ltap_cancl
            EXCEPTIONS
              error_message = 99.

*         Criar uma nova OT, para outra remessa
          IF sy-subrc = 0.
            CLEAR to.
            DO 10 TIMES.
              CLEAR return_msg. REFRESH return_msg.

              CALL FUNCTION 'ZWM_TO_CREATE_OUT'
                EXPORTING
                  warehouse     = c_ltap-lgnum
                  refnr         = t_ltak-refnr
                  vbeln         = c_ltap-vbeln
                  posnr         = c_ltap-posnr
                  vsola         = c_ltap-vsola
                  meins         = c_ltap-altme
                  werks         = c_ltap-werks
                  lgort         = c_ltap-lgort
                  matnr         = c_ltap-matnr
                  su            = zwm020-p2
                IMPORTING
                  to            = to
                TABLES
                  return_msg    = return_msg
                EXCEPTIONS
                  error_message = 1
                  OTHERS        = 2.



              IF sy-subrc EQ 0.
                flag = 'X'.
                EXIT.
              ELSE.
                WAIT UP TO 1 SECONDS.
              ENDIF.
            ENDDO.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

**********************************************************************
  ELSE.

** Se for a palete P1 criar a outra ot para o PRM com a P2
    CLEAR ltap.
    SELECT SINGLE *
        FROM ltap
            WHERE lgnum = c_ltap-lgnum
              AND tanum = to.

    CLEAR zwm020.
    SELECT SINGLE p1 p2 INTO (pal_p1, pal_p2)
        FROM zwm020
            WHERE armazem = c_ltap-lgnum
              AND ( p1 = ltap-vlenr OR
                    p2 = ltap-vlenr ).

    IF sy-subrc = 0.

**    Validar se outra palete remontada tem já uma OT para o grupo
      CLEAR flag_nao_estornar.
      IF ltap-vlenr = pal_p1.
        READ TABLE t_ltap WITH KEY vlenr = pal_p2.
        IF sy-subrc = 0.
          flag_nao_estornar = 'X'.
          flag_add_ot = 'X'.
        ENDIF.

      ELSEIF ltap-vlenr = pal_p2.
        READ TABLE t_ltap WITH KEY vlenr = pal_p1.
        IF sy-subrc = 0.
          flag_nao_estornar = 'X'.
          flag_add_ot = 'X'.
        ENDIF.
      ENDIF.

      IF flag_nao_estornar IS INITIAL." Palete já tem OT para o grupo

        WAIT UP TO 1 SECONDS.

** CC estornar a ot com a P2 e criar para a remessa com a P1
        IF ltap-vlenr = pal_p2.
          CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
            EXPORTING
              warehouse     = ltap-lgnum
              tanum         = ltap-tanum
              tapos         = ltap-tapos
            TABLES
              return_msg    = return_msg
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          CLEAR: flag, to.
          DO 60 TIMES.
            CALL FUNCTION 'ZWM_TO_CREATE_OUT'
              EXPORTING
                warehouse     = c_ltap-lgnum
                refnr         = t_ltak-refnr
                vbeln         = t_ltak-vbeln
                posnr         = c_ltap-posnr
                vsola         = c_ltap-vsola
                meins         = c_ltap-altme
                werks         = c_ltap-werks
                lgort         = c_ltap-lgort
                matnr         = c_ltap-matnr
                su            = pal_p1
              IMPORTING
                to            = to
              TABLES
                return_msg    = return_msg
              EXCEPTIONS
                error_message = 1
                OTHERS        = 2.


            IF sy-subrc EQ 0.
              flag = 'X'.
              EXIT.
            ELSE.
              CLEAR flag.
              WAIT UP TO 1 SECONDS.
            ENDIF.
          ENDDO.

        ENDIF.

        WAIT UP TO 1 SECONDS.

        CLEAR flag.
        DO 60 TIMES.

          CALL FUNCTION 'ZWM_CREATE_TO'
            EXPORTING
              warehouse  = ltap-lgnum
              mov_type   = '940'
              material   = ltap-matnr
              quantity   = '1'
              unit       = 'PAL'
              plant      = ltap-werks
              s_loc      = ltap-lgort
              lote       = ltap-charg
              source_sty = ltap-vltyp
              source_bin = ltap-vlpla
              req_type   = aux_betyp
              req_number = aux_benum
              su         = pal_p2
            IMPORTING
              to         = to_remontada
            TABLES
              return_msg = return_msg
            EXCEPTIONS
              error      = 1
              OTHERS     = 2.

          IF sy-subrc = 0.
            flag = 'X'.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
            CLEAR flag.

            PERFORM complete_log_message USING 'REMONTADA_005'
                                               aux_benum
                                               pal_p1
                                               pal_p2
                                               ltap-matnr
                                         CHANGING return_msg[].

            CALL FUNCTION 'ZWM_LOG_MESSAGE'
              EXPORTING
                i_master    = ltap-lgnum
                i_object    = 'ZWM001'
                i_subobject = 'ZWM006'
                i_extnumber = '3'
                i_commit    = 'X'
                it_messages = return_msg[].
          ENDIF.
        ENDDO.

      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT c_ltap.
    DELETE t_ltap WHERE vlenr = c_ltap-vlenr.
  ENDLOOP.

  IF flag_add_ot = 'X'.
    APPEND ltap TO t_ltap.
  ENDIF.

  CLEAR c_ltap.
  REFRESH c_ltap.

ENDFORM.                    " SPLIT_PALETES_REMONTADAS
*&---------------------------------------------------------------------*
*&      Form  VERIFY_VOLUM_PAL_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verify_volum_pal_picking .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM update_picking CHANGING cv_subrc TYPE sysubrc.

  DATA: lt_lips LIKE lips OCCURS 0 WITH HEADER LINE,
        lt_vbss LIKE vbss OCCURS 0 WITH HEADER LINE.

  DATA: ls_lips           TYPE lips,
        ls_zwm001         TYPE zwm001,
        ls_unpaired_stock TYPE lqua.

  DATA: lt_unpaired_stock TYPE TABLE OF lqua.

  CLEAR: cv_subrc.

  DATA l_qtd(17).
  LOOP AT it311 WHERE kreuz = con_x.
    EXIT.
  ENDLOOP.

  CHECK sy-subrc = 0.

  SELECT * FROM vbss INTO TABLE lt_vbss
                WHERE sammg = it311-sammg.

  CHECK NOT lt_vbss[] IS INITIAL.

  SELECT * FROM lips INTO TABLE lt_lips
              FOR ALL ENTRIES IN lt_vbss
                WHERE vbeln = lt_vbss-vbeln.

** Valida Remontadas
***********************************************************************
  SELECT SINGLE * FROM zwm001
                  INTO ls_zwm001
                  WHERE armazem = gv_lgnum AND
                        processo = 'REMONTADA' AND
                        parametro = 'CHECK'.

  IF ls_zwm001-valor EQ abap_true.

    LOOP AT lt_lips INTO ls_lips.
      CALL FUNCTION 'ZWM_CHECK_REMONTADAS'
        EXPORTING
          i_lgnum           = gv_lgnum
          i_matnr           = ls_lips-matnr
        IMPORTING
          et_unpaired_stock = lt_unpaired_stock.

      LOOP AT lt_unpaired_stock INTO ls_unpaired_stock.
        MESSAGE ID 'ZWM001'
              TYPE 'I'
              NUMBER 079
              DISPLAY LIKE 'E'
              WITH ls_unpaired_stock-lenum ls_unpaired_stock-matnr.
      ENDLOOP.
      IF sy-subrc EQ 0.
        cv_subrc = 4.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDIF.


**********************************************************************

* Eliminar itens que sejam do WM
  CALL FUNCTION 'Z_WM_FILTER_TABLE_TO_WM'
    EXPORTING
      i_lgnum  = gv_lgnum
    CHANGING
      ct_table = lt_lips[].

  CHECK lt_lips[] IS NOT INITIAL.


  LOOP AT lt_lips.

    CALL FUNCTION 'ZWM_UPDATE_PICKING'
      EXPORTING
        i_lfimg = lt_lips-lfimg
        i_vbeln = lt_lips-vbeln
        i_posnr = lt_lips-posnr.

  ENDLOOP.
ENDFORM.                    " UPDATE_PICKING
FORM verify_picking_quantity CHANGING ct_paletes TYPE zpalete_picking_tt.

  DATA: lt_lips LIKE lips OCCURS 0 WITH HEADER LINE,
        lt_vbss LIKE vbss OCCURS 0 WITH HEADER LINE,
        lt_ltak LIKE ltak OCCURS 0 WITH HEADER LINE,
        lt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

  DATA: lt_paletes LIKE zpalete_picking OCCURS 10 WITH HEADER LINE.
  DATA: lt_paletes_pkl LIKE zpalete_picking OCCURS 10 WITH HEADER LINE.

  DATA: BEGIN OF lt_grupos OCCURS 0,
          sammg LIKE vbss-sammg,
        END OF lt_grupos,
        ls_grupos LIKE lt_grupos.

  DATA: BEGIN OF lt_totais OCCURS 0,
          matnr LIKE lips-matnr,
          werks LIKE lips-werks,
          lgort LIKE lips-lgort,
          quant LIKE zpalete_picking-uni_incompleta,
        END OF lt_totais,
        ls_totais LIKE lt_totais,
        BEGIN OF lk_totais,
          vbeln LIKE lips-vbeln,
          matnr LIKE lips-matnr,
        END OF lk_totais.

  DATA: BEGIN OF lt_totais_lote OCCURS 0,
          matnr LIKE ltap-matnr,
          werks LIKE ltap-werks,
          lgort LIKE ltap-lgort,
          charg LIKE ltap-charg,
          vsola LIKE ltap-vsola,
        END OF lt_totais_lote,
        ls_totais_lote LIKE lt_totais_lote.

  DATA: BEGIN OF lt_totais_lo OCCURS 0,
          matnr LIKE ltap-matnr,
          charg LIKE ltap-charg,
          vsola LIKE ltap-vsola,
        END OF lt_totais_lo,
        ls_totais_lo LIKE lt_totais_lo.

  DATA: l_tanum LIKE ltak-tanum,
        l_meins LIKE lqua-meins.

  DATA: lt_return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  DATA: lt_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: lt_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE.

  DATA: l_quant     LIKE zpalete_picking-uni_incompleta,
        l_quant_pkr LIKE zpalete_picking-uni_incompleta,
        l_quant_pkl LIKE zpalete_picking-uni_incom_p,
        lv_qtd      TYPE menge_d,
        lv_verme    TYPE menge_d.

  DATA: l_quant_aux TYPE i.

  DATA: l_mov_type  LIKE ltak-bwlvs,
        l_mov_type1 LIKE ltak-bwlvs,
        l_mov_type2 LIKE ltak-bwlvs,
        l_mov       LIKE ltak-bwlvs,
        l_mov11     LIKE ltak-bwlvs,
        l_mov12     LIKE ltak-bwlvs,
        l_mov21     LIKE ltak-bwlvs,
        l_mov22     LIKE ltak-bwlvs,
        l_movpkl    LIKE ltak-bwlvs,
        l_lgtyp     LIKE mlgt-lgtyp,
        l_plkpt     LIKE mlgn-plkpt,
        l_betyp     LIKE ltak-betyp,
        l_pstyv1    LIKE lips-pstyv,
        l_pstyv2    LIKE lips-pstyv.

  DATA: lv_lines TYPE sytabix.

  DATA: lv_refnr LIKE ltak-refnr.

  DATA: ls_marm TYPE marm.

  REFRESH: lt_vbss,   lt_lips,
           lt_grupos, lt_totais, lt_paletes.

  LOOP AT it311.
    CHECK it311-kreuz = con_x.
    ls_grupos-sammg = it311-sammg.
    COLLECT ls_grupos INTO lt_grupos.
  ENDLOOP.

  SELECT * FROM zwm001 INTO TABLE lt_zwm001
                 WHERE armazem   = gv_lgnum
                   AND processo  = 'REABASTECIMENTO'.
  IF lt_zwm001[] IS INITIAL.
*    Não existem parametros definidos para este processo (tabela &)
    MESSAGE i156(zwmmsg001) WITH 'ZWM001'.
    EXIT.
  ENDIF.

  CLEAR lt_zwm001.
  LOOP AT lt_zwm001.
    CASE lt_zwm001-parametro.
      WHEN 'ST_PCK'.
        l_lgtyp = lt_zwm001-valor.
      WHEN 'ST_BET'.
        l_betyp = lt_zwm001-valor.
      WHEN 'ST_PKB'.
        l_plkpt = lt_zwm001-valor.
      WHEN 'MOV1'.
        l_movpkl = lt_zwm001-valor.
      WHEN 'ST_MOV'.
        l_mov = lt_zwm001-valor.
      WHEN 'ST_MOV11'.
        l_mov11 = lt_zwm001-valor.
      WHEN 'ST_MOV12'.
        l_mov12 = lt_zwm001-valor.
      WHEN 'ST_MOV21'.
        l_mov21 = lt_zwm001-valor.
      WHEN 'ST_MOV22'.
        l_mov22 = lt_zwm001-valor.
      WHEN 'ST_PST1'.
        l_pstyv1 = lt_zwm001-valor.
      WHEN 'ST_PST2'.
        l_pstyv2 = lt_zwm001-valor.
    ENDCASE.
  ENDLOOP.

* Só haverá 1 grupo selecionado
  READ TABLE lt_grupos INDEX 1.
  CHECK sy-subrc = 0.

  SELECT * FROM vbss INTO TABLE lt_vbss
                WHERE sammg = lt_grupos-sammg.

  CHECK NOT lt_vbss[] IS INITIAL.

* Eliminar as remessas que ja têm to´s criadas
  CLEAR vbuk.
  LOOP AT lt_vbss.
    CHECK z_wm_cl_management=>is_delivery_completed( is_data = lt_vbss ) EQ abap_true.

    DELETE lt_vbss WHERE vbeln = lt_vbss-vbeln.
    CLEAR lt_vbss.
  ENDLOOP.

** Garantir que existem remessas a selecionar
  IF lt_vbss[] IS INITIAL.
* As remessas do grupo & já tem todas as to´s criadas.
    MESSAGE i210(zwmmsg001) WITH lt_grupos-sammg.
  ENDIF.

  CHECK NOT lt_vbss[] IS INITIAL.

  SELECT * FROM lips INTO TABLE lt_lips
              FOR ALL ENTRIES IN lt_vbss
                WHERE vbeln = lt_vbss-vbeln.

***************************************************
* Eliminar itens que não sejam do CD - Inicio
***************************************************
  CALL FUNCTION 'Z_WM_FILTER_TABLE_TO_WM'
    EXPORTING
      i_lgnum  = gv_lgnum
    CHANGING
      ct_table = lt_lips[].


* PSTYV = ZPAL or ZPAS
  DELETE lt_lips WHERE ( pstyv = l_pstyv1 OR pstyv = l_pstyv2 ).

  REFRESH: lt_vbss.

  LOOP AT lt_lips.
    CLEAR lt_paletes.
    MOVE-CORRESPONDING lt_lips TO lt_paletes.
    lt_paletes-refnr = lt_grupos-sammg.
    APPEND lt_paletes.
  ENDLOOP.

  REFRESH: lt_lips.

**   Cálculo das paletes
  CALL FUNCTION 'ZWM_PAL_PICKING'
    EXPORTING
      armazem         = gv_lgnum
      actualiza       = ' '
    TABLES
      zpalete_picking = lt_paletes[].

  REFRESH: lt_lips.

  DESCRIBE TABLE lt_paletes LINES lv_lines.

  LOOP AT lt_paletes.

    IF sy-tabix EQ lv_lines AND
       lt_paletes-pal_picking EQ 0 AND
       lt_paletes-pal_completa EQ 0.
      MESSAGE ID 'L5' TYPE 'I' NUMBER 000 DISPLAY LIKE 'E' WITH 'Contactar ROFF. Erro: Palete não terminada, OT Pendente'.
    ENDIF.

    CLEAR ls_totais.
    ls_totais-matnr = lt_paletes-matnr.
    ls_totais-werks = lt_paletes-werks.
    ls_totais-lgort = lt_paletes-lgort.
    ls_totais-quant = lt_paletes-uni_incompleta - lt_paletes-uni_incom_p.
    COLLECT ls_totais INTO lt_totais.
  ENDLOOP.

** Picking Loja Online
  DATA lt_lqua LIKE lqua OCCURS 0 WITH HEADER LINE.

  CLEAR lt_paletes_pkl.
  REFRESH lt_paletes_pkl.

  lt_paletes_pkl[] = lt_paletes[].
  LOOP AT lt_paletes WHERE uni_incom_p IS NOT INITIAL.


    PERFORM get_plant_data USING lt_paletes-refnr CHANGING lt_paletes-werks lt_paletes-lgort.
    CHECK lt_paletes-lgort EQ lt_paletes-lgort.

    CLEAR lt_paletes-uni_incom_p.
    LOOP AT lt_paletes_pkl WHERE uni_incom_p IS NOT INITIAL
                           AND werks = lt_paletes-werks
                           AND lgort = lt_paletes-lgort
                           AND matnr = lt_paletes-matnr.
      lt_paletes-uni_incom_p = lt_paletes-uni_incom_p + lt_paletes_pkl-uni_incom_p.
    ENDLOOP.

    REFRESH: lt_return_msg, lt_sscc, lt_lqua.
    CLEAR: lt_return_msg, lt_sscc, lt_lqua.
    CLEAR l_tanum.


    SELECT * FROM lqua INTO TABLE lt_lqua
            WHERE lgnum = gv_lgnum
              AND lgtyp = 'PKL'
              AND matnr = lt_paletes-matnr
              AND werks = lt_paletes-werks
              AND lgort = lt_paletes-lgort
              AND lenum = ' '.

    IF lt_paletes-charg IS NOT INITIAL.
      DELETE lt_lqua WHERE charg <> lt_paletes-charg.
    ENDIF.

    DATA l_quant_aux_pkl LIKE zpalete_picking-uni_incom_p.

    CLEAR l_quant_pkl.
    IF NOT lt_lqua[] IS INITIAL.

      LOOP AT lt_lqua.

        CLEAR ls_marm.
        SELECT SINGLE * FROM marm
                  INTO ls_marm
                  WHERE matnr = lt_lqua-matnr AND
                        meinh = lt_paletes-vrkme.

        lv_qtd = ( lt_lqua-verme * ls_marm-umren ) / ls_marm-umrez.

        CALL FUNCTION 'ROUND'
          EXPORTING
            input         = lv_qtd
            sign          = '-'
          IMPORTING
            output        = lv_verme
          EXCEPTIONS
            input_invalid = 1
            overflow      = 2
            type_invalid  = 3
            OTHERS        = 4.

        IF lv_verme <= 1.
          CONTINUE.
        ENDIF.

        CLEAR l_quant_aux_pkl.
        l_quant_aux_pkl = lt_lqua-verme.
        l_quant_pkl = l_quant_pkl + l_quant_aux_pkl.
      ENDLOOP.
    ENDIF.

    CHECK l_quant_pkl <= lt_paletes-uni_incom_p.

    lt_sscc-material      = lt_paletes-matnr.
    lt_sscc-quantidade    = 1.
    lt_sscc-uni           = l_meins.
    APPEND lt_sscc.

    CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse   = gv_lgnum
        mov_type    = l_movpkl                          "979
        plant       = lt_paletes-werks
        s_loc       = lt_paletes-lgort
        certificado = 'X'
        req_number  = lt_paletes-refnr
        req_type    = l_betyp
      IMPORTING
        to          = l_tanum
      TABLES
        return_msg  = lt_return_msg
        sscc        = lt_sscc
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF l_tanum IS INITIAL.
      CLEAR ls_totais.
      READ TABLE lt_totais INTO ls_totais WITH KEY matnr = lt_paletes-matnr.
      IF sy-subrc = 0.
        ls_totais-quant = lt_paletes-uni_incom_p.
        COLLECT ls_totais INTO lt_totais.
      ENDIF.
    ELSE.
      DO 10 TIMES.

        CLEAR: l_quant_pkl, lt_lqua.
        REFRESH lt_lqua.

        SELECT * FROM lqua INTO TABLE lt_lqua
        WHERE lgnum = gv_lgnum
          AND lgtyp = 'PKL'
          AND matnr = lt_paletes-matnr
          AND werks = lt_paletes-werks
          AND lgort = lt_paletes-lgort
          AND lenum = ' '.

        IF lt_paletes-charg IS NOT INITIAL.
          DELETE lt_lqua WHERE charg <> lt_paletes-charg.
        ENDIF.

        IF NOT lt_lqua[] IS INITIAL.
          LOOP AT lt_lqua.

            CLEAR ls_marm.
            SELECT SINGLE * FROM marm
                      INTO ls_marm
                      WHERE matnr = lt_lqua-matnr AND
                            meinh = lt_paletes-vrkme.

            lv_qtd = ( lt_lqua-verme * ls_marm-umren ) / ls_marm-umrez.

            CALL FUNCTION 'ROUND'
              EXPORTING
                input         = lv_qtd
                sign          = '-'
              IMPORTING
                output        = lv_verme
              EXCEPTIONS
                input_invalid = 1
                overflow      = 2
                type_invalid  = 3
                OTHERS        = 4.

            IF lv_verme <= 1.
              CONTINUE.
            ENDIF.

            CLEAR l_quant_aux_pkl.
            l_quant_aux_pkl = lt_lqua-verme.
            l_quant_pkl = l_quant_pkl + l_quant_aux_pkl.
          ENDLOOP.
        ENDIF.

        IF l_quant_pkl < lt_paletes-uni_incom_p.
          WAIT UP TO 1 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDLOOP.

  IF NOT lt_totais[] IS INITIAL.
**  Verificar quantas posicoes vazias existem no rep

    SORT lt_totais BY quant DESCENDING.

** So fazer esta validação no caso de existir quantidades incompletas
    READ TABLE lt_totais INDEX 1.
    CHECK NOT lt_totais-quant IS INITIAL.

    DATA n_posi TYPE i.
    CLEAR n_posi.

    SELECT COUNT(*)
      FROM lagp INTO n_posi
      WHERE lgnum EQ gv_lgnum
        AND lgtyp EQ 'REP'.
    IF n_posi NE 1.
      CLEAR n_posi.

      SELECT COUNT(*) FROM lagp INTO n_posi
          WHERE lgnum = gv_lgnum AND
                lgtyp = 'REP' AND
                kzler = 'X' AND
                kzvol = ' ' AND
                anzqu = 0   AND
                skzue = ' ' AND
                skzsi = ' ' AND
                skzse = ' '.


      IF n_posi <= 15.
        flag = 'X'.
        MESSAGE ID 'ZWMMSG001' TYPE 'I'
              NUMBER '206' WITH it311-refnr.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  SORT lt_totais BY matnr.

  LOOP AT lt_totais.

    PERFORM get_plant_data USING lt_grupos-sammg CHANGING lt_totais-werks lt_totais-lgort.

    CHECK NOT lt_totais-quant IS INITIAL.
    CLEAR l_quant.

    CLEAR mara.
    SELECT SINGLE * FROM mara WHERE matnr = lt_totais-matnr.
    CHECK sy-subrc = 0.

    CLEAR l_meins.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = mara-meins
      IMPORTING
        output         = l_meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      l_meins = mara-meins.
    ENDIF.

    l_mov_type = l_mov.  "972

    CLEAR: mlgn, mlgt, lqua.
    SELECT SINGLE * FROM mlgn
            WHERE matnr = lt_totais-matnr
              AND lgnum = gv_lgnum.
    IF mlgn-plkpt = l_plkpt.
      l_mov_type1 = l_mov21.  "976
      l_mov_type2 = l_mov22.  "975
    ELSE.
      l_mov_type1 = l_mov11.  "971
      l_mov_type2 = l_mov12.  "970
    ENDIF.
    SELECT SINGLE * FROM mlgt
            WHERE matnr = lt_totais-matnr
              AND lgnum = gv_lgnum
              AND lgtyp = l_lgtyp.
    CHECK sy-subrc = 0.
    IF mlgn-plkpt IS INITIAL.
      CHECK NOT mlgt-lgpla IS INITIAL.
    ENDIF.

    DATA t_lqua LIKE lqua OCCURS 0 WITH HEADER LINE.

** Verificar stock no picking fixo
    REFRESH t_lqua.
    CLEAR: t_lqua, l_quant.

    IF mlgn-plkpt IS INITIAL.
      SELECT * FROM lqua INTO TABLE t_lqua
              WHERE lgnum = mlgt-lgnum
                AND lgtyp = mlgt-lgtyp
                AND lgpla = mlgt-lgpla
                AND werks = lt_totais-werks
                AND lgort = lt_totais-lgort
                AND lenum = ' '.

      IF NOT t_lqua[] IS INITIAL.
        LOOP AT t_lqua.
          CLEAR l_quant_aux.
          l_quant_aux = trunc( t_lqua-verme ).
          l_quant = l_quant + l_quant_aux.
        ENDLOOP.
      ENDIF.

    ELSE.
** Verificar stock no picking variavel
      SELECT * FROM lqua INTO TABLE t_lqua
              WHERE lgnum = mlgt-lgnum
                AND lgtyp = 'PKB'
                AND matnr = lt_totais-matnr
                AND werks = lt_totais-werks
                AND lgort = lt_totais-lgort
                AND lenum = ' '.

      IF NOT t_lqua[] IS INITIAL.
        LOOP AT t_lqua.
          CLEAR l_quant_aux.
          l_quant_aux = trunc( t_lqua-verme ).
          l_quant = l_quant + l_quant_aux.
        ENDLOOP.
      ENDIF.
    ENDIF.

    CHECK l_quant < lt_totais-quant.

* Verificar se já foi efectuado algum reabastecimento
    DATA: t_ltak LIKE ltak OCCURS 0 WITH HEADER LINE,
          t_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

    DATA qtd_reab LIKE ltap-vsolm.
    CLEAR qtd_reab.

    IF mlgn-plkpt IS INITIAL.
      SELECT * INTO TABLE t_ltak
          FROM ltak
              WHERE lgnum = gv_lgnum AND
                    betyp = 'Z' AND
                    benum = lt_grupos-sammg.

      LOOP AT t_ltak.
        CLEAR: ltap.
        SELECT *
            FROM ltap
                WHERE lgnum = t_ltak-lgnum AND
                      tanum = t_ltak-tanum AND
                      matnr = lt_totais-matnr AND
                      werks = lt_totais-werks AND
                      lgort = lt_totais-lgort AND
                      vorga <> 'ST' AND
                      pquit <> 'X'.

          qtd_reab = qtd_reab + ltap-vsolm.
        ENDSELECT.
      ENDLOOP.

    ELSE.

      CLEAR: t_ltak, t_ltap.
      REFRESH:  t_ltak, t_ltap.
      SELECT * INTO TABLE t_ltak
               FROM ltak
                   WHERE lgnum = gv_lgnum AND
                         betyp = 'Z' AND
                         kquit = ' '.

      DELETE t_ltak WHERE bwlvs = '976'.

      IF NOT t_ltak[] IS INITIAL.

        SELECT * FROM ltap INTO TABLE t_ltap
            FOR ALL ENTRIES IN t_ltak
                  WHERE lgnum = t_ltak-lgnum AND
                        tanum = t_ltak-tanum.

        DELETE t_ltap WHERE matnr <> lt_totais-matnr
                         OR vorga = 'ST'
                         OR pquit = 'X'.

      ENDIF.


      IF gv_lgnum <> '150'.
        IF NOT t_ltap[] IS INITIAL.
          flag = 'X'.
          SORT: t_ltak, t_ltap.
          LOOP AT t_ltap.
            READ TABLE t_ltak WITH KEY lgnum = t_ltap-lgnum
                                       tanum = t_ltap-tanum.
            MESSAGE ID 'ZWMMSG001' TYPE 'I'
                  NUMBER '266' WITH t_ltak-benum t_ltap-matnr.
          ENDLOOP.
          EXIT.
        ENDIF.
      ENDIF.

    ENDIF.

    CHECK qtd_reab < lt_totais-quant.
******************************************************
** Verfificar quantidade na material na reserva de picking (PKR)

    REFRESH t_lqua.
    CLEAR: t_lqua, l_quant_pkr, l_quant_aux.

    IF mlgn-plkpt IS INITIAL.
      SELECT * FROM lqua INTO TABLE t_lqua
              WHERE lgnum = mlgt-lgnum
                AND lgtyp = 'PKR'
                AND matnr = lt_totais-matnr
                AND werks = lt_totais-werks
                AND lgort = lt_totais-lgort.

      IF NOT t_lqua[] IS INITIAL.
        LOOP AT t_lqua.
          CLEAR l_quant_aux.
          l_quant_aux = trunc( t_lqua-verme ).
          l_quant_pkr = l_quant_pkr + l_quant_aux.
        ENDLOOP.
      ENDIF.

    ELSE.
** Verificar stock na reserva do picking variavel
      SELECT * FROM lqua INTO TABLE t_lqua
              WHERE lgnum = mlgt-lgnum
                AND lgtyp = 'PRB'
                AND matnr = lt_totais-matnr
                AND werks = lt_totais-werks
                AND lgort = lt_totais-lgort.

      IF NOT t_lqua[] IS INITIAL.
        LOOP AT t_lqua.
          CLEAR l_quant_aux.
          l_quant_aux = trunc( t_lqua-verme ).
          l_quant_pkr = l_quant_pkr + l_quant_aux.
        ENDLOOP.
      ENDIF.
    ENDIF.

    REFRESH: lt_return_msg, lt_sscc.
    CLEAR: lt_return_msg, lt_sscc.
    CLEAR l_tanum.

    lt_sscc-material      = lt_totais-matnr.
** so vou pedir a diferenca do que quero com o que esta no picking
** no maximo o que estiver no PKR
    lt_sscc-quantidade    = lt_totais-quant - l_quant.

    IF lt_sscc-quantidade > l_quant_pkr.
      lt_sscc-quantidade  = l_quant_pkr.
    ENDIF.

    lt_sscc-uni           = l_meins.

    CLEAR mlgn.
    SELECT SINGLE *
        FROM mlgn
            WHERE matnr = lt_totais-matnr AND
                  lgnum = gv_lgnum.
    IF sy-subrc = 0.
      IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
        IF l_mov_type1 = '971'.
          lt_sscc-quantidade    = 2.
          lt_sscc-uni           = 'PAL'.
        ENDIF.
      ENDIF.
    ENDIF.
    APPEND lt_sscc.

    CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse  = gv_lgnum
        mov_type   = l_mov_type1                            "971 ou 976
        plant      = lt_totais-werks
        s_loc      = lt_totais-lgort
        req_number = lt_grupos-sammg
        req_type   = l_betyp
      IMPORTING
        to         = l_tanum
      TABLES
        return_msg = lt_return_msg
        sscc       = lt_sscc
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    IF NOT l_tanum IS INITIAL.
      CLEAR ltak.
      SELECT * FROM ltak INTO TABLE lt_ltak
                          WHERE lgnum = gv_lgnum
                            AND betyp = l_betyp
                            AND benum = lt_grupos-sammg
                            AND kquit = ' '.

      IF NOT lt_ltak[] IS INITIAL.

        SELECT * FROM ltap INTO TABLE lt_ltap
                    FOR ALL ENTRIES IN lt_ltak
                      WHERE lgnum = lt_ltak-lgnum
                        AND tanum = lt_ltak-tanum.

        DELETE lt_ltap WHERE matnr <> lt_totais-matnr.
        DELETE lt_ltap WHERE pquit = 'X'.

        IF NOT lt_ltap[] IS INITIAL.
          LOOP AT lt_ltap.
            IF z_wm_cl_management=>is_remontada( is_data = lt_ltap ) EQ abap_true.
              IF lt_ltap-tapos = '0001'.
                l_quant_aux = trunc( lt_ltap-vsola ).
                l_quant = l_quant + l_quant_aux.
              ENDIF.
            ELSE.
              l_quant_aux = trunc( lt_ltap-vsola ).
              l_quant = l_quant + l_quant_aux.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR l_quant.
    ENDIF.

    IF l_quant < lt_totais-quant.

      l_quant = lt_totais-quant - l_quant.

      REFRESH: lt_return_msg, lt_sscc.
      CLEAR: lt_return_msg, lt_sscc.
      CLEAR l_tanum.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = l_meins
          language       = sy-langu
        IMPORTING
          output         = l_meins
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.

      ENDIF.

      lt_sscc-material      = lt_totais-matnr.
      lt_sscc-quantidade    = l_quant.
      lt_sscc-uni           = l_meins.

      DATA: adicional   TYPE lvs_lznum,
            qt_aux      LIKE lt_sscc-quantidade,
            qt_pal      LIKE mlgn-lhmg1,
            n_pal       TYPE i,
            qt_resto    TYPE i,
            bin_destino TYPE lgpla.

      CLEAR bin_destino.

      IF gv_lgnum = '150' AND mlgn-plkpt = 'PKB'.
        CLEAR t_lqua.
        REFRESH t_lqua.
        SELECT * FROM lqua INTO TABLE t_lqua
        WHERE lgnum = mlgt-lgnum
          AND lgtyp = 'PKB'
          AND matnr = lt_totais-matnr
          AND werks = lt_totais-werks
          AND lgort = lt_totais-lgort
          AND lenum = ' '.

        SORT t_lqua BY verme.
        READ TABLE t_lqua INDEX 1.
        bin_destino = t_lqua-lgpla.
      ENDIF.

      CLEAR mlgn.
      SELECT SINGLE *
          FROM mlgn
              WHERE matnr = lt_totais-matnr AND
                    lgnum = gv_lgnum.
      IF sy-subrc = 0.
        IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
          APPEND lt_sscc.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
          n_pal = 2.
        ELSE.
          n_pal = 1.
        ENDIF.
      ENDIF.
      APPEND lt_sscc.

      CLEAR: qt_aux, qt_pal.
      IF mlgn-plkpt = 'PKB' AND gv_lgnum <> '150'.
        adicional = l_quant.
      ELSE.
        CLEAR adicional.
      ENDIF.

      qt_pal = mlgn-lhmg1 * n_pal.

      IF l_quant > qt_pal.
        CLEAR lt_sscc.
        REFRESH lt_sscc.

        qt_aux = l_quant DIV qt_pal.
        qt_resto = l_quant MOD qt_pal.

        IF NOT qt_resto IS INITIAL.
          IF mlgn-plkpt = 'PKB' AND gv_lgnum <> '150'.
            adicional = qt_resto.
          ELSE.
            CLEAR adicional.
          ENDIF.

          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
            APPEND lt_sscc.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
          ELSE.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
          ENDIF.
          APPEND lt_sscc.

          IF adicional IS NOT INITIAL.
            adicional = ceil( adicional ).
          ENDIF.

          CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
            EXPORTING
              warehouse      = gv_lgnum
              mov_type       = l_mov_type "972
              bin_destino    = bin_destino
              plant          = lt_totais-werks
              s_loc          = lt_totais-lgort
              req_number     = lt_grupos-sammg
              req_type       = l_betyp
              sscc_adicional = adicional
            IMPORTING
              to             = l_tanum
            TABLES
              return_msg     = lt_return_msg
              sscc           = lt_sscc
            EXCEPTIONS
              error          = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            IF sy-msgid = 'L3' AND sy-msgno = '008'. " Msg não diz o Material
              MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '278' WITH lt_totais-matnr.

            ELSE.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            RETURN.
          ENDIF.

          CLEAR lt_sscc.
          REFRESH lt_sscc.
        ENDIF.

        DO qt_aux TIMES.

          IF mlgn-plkpt = 'PKB' AND gv_lgnum <> '150'.
            adicional = mlgn-lhmg1.
          ELSE.
            CLEAR adicional.
          ENDIF.

          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
            APPEND lt_sscc.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
          ELSE.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
          ENDIF.
          APPEND lt_sscc.

          IF adicional IS NOT INITIAL.
            adicional = ceil( adicional ).
          ENDIF.

          CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
            EXPORTING
              warehouse      = gv_lgnum
              mov_type       = l_mov_type "972
              bin_destino    = bin_destino
              plant          = lt_totais-werks
              s_loc          = lt_totais-lgort
              req_number     = lt_grupos-sammg
              req_type       = l_betyp
              sscc_adicional = adicional
            IMPORTING
              to             = l_tanum
            TABLES
              return_msg     = lt_return_msg
              sscc           = lt_sscc
            EXCEPTIONS
              error          = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            IF sy-msgid = 'L3' AND sy-msgno = '008'. " Msg não diz o Material
              MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '278' WITH lt_totais-matnr.

            ELSE.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDIF.

          CLEAR lt_sscc.
          REFRESH lt_sscc.

        ENDDO.
*      ENDIF.


      ELSE.
        IF adicional IS NOT INITIAL.
          adicional = ceil( adicional ).
        ENDIF.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse      = gv_lgnum
            mov_type       = l_mov_type "972
            bin_destino    = bin_destino
            plant          = lt_totais-werks
            s_loc          = lt_totais-lgort
            req_number     = lt_grupos-sammg
            req_type       = l_betyp
            sscc_adicional = adicional
          IMPORTING
            to             = l_tanum
          TABLES
            return_msg     = lt_return_msg
            sscc           = lt_sscc
          EXCEPTIONS
            error          = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          IF sy-msgid = 'L3' AND sy-msgno = '008'. " Msg não diz o Material
            MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '278' WITH lt_totais-matnr.

          ELSE.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    CLEAR ltak.
    SELECT * FROM ltak INTO TABLE lt_ltak
                        WHERE lgnum = gv_lgnum
                          AND betyp = l_betyp
                          AND benum = lt_grupos-sammg
                          AND kquit = ' '.

    CHECK NOT lt_ltak[] IS INITIAL.

    SELECT * FROM ltap INTO TABLE lt_ltap
                FOR ALL ENTRIES IN lt_ltak
                  WHERE lgnum = lt_ltak-lgnum
                    AND tanum = lt_ltak-tanum.

    DELETE lt_ltap WHERE matnr <> lt_totais-matnr.
    DELETE lt_ltap WHERE werks <> lt_totais-werks.
    DELETE lt_ltap WHERE lgort <> lt_totais-lgort.
    DELETE lt_ltap WHERE pquit = 'X'.
    DELETE lt_ltap WHERE zeugn = 'X'.

    CHECK NOT lt_ltap[] IS INITIAL.

    REFRESH lt_totais_lote.
    SORT lt_ltak.
    LOOP AT lt_ltap.
      CLEAR ls_totais_lote.

      IF z_wm_cl_management=>is_remontada( is_data = lt_ltap ) EQ abap_true.
        IF lt_ltap-tapos = '0001'.
          READ TABLE lt_ltak WITH KEY lgnum = lt_ltap-lgnum
                                      tanum = lt_ltap-tanum
                                      BINARY SEARCH.
          IF sy-subrc = 0.
            IF NOT lt_ltak-lznum IS INITIAL.
              MOVE lt_ltak-lznum TO ls_totais_lote-vsola.
            ELSE.
              ls_totais_lote-vsola = lt_ltap-vsolm.
            ENDIF.
          ENDIF.

          ls_totais_lote-matnr = lt_ltap-matnr.
          ls_totais_lote-werks = lt_ltap-werks.
          ls_totais_lote-lgort = lt_ltap-lgort.
          ls_totais_lote-charg = lt_ltap-charg.
          COLLECT ls_totais_lote INTO lt_totais_lote.
        ENDIF.
      ELSE.
        READ TABLE lt_ltak WITH KEY lgnum = lt_ltap-lgnum
                                              tanum = lt_ltap-tanum
                                              BINARY SEARCH.
        IF sy-subrc = 0.
          IF NOT lt_ltak-lznum IS INITIAL.
            MOVE lt_ltak-lznum TO ls_totais_lote-vsola.
          ELSE.
            ls_totais_lote-vsola = lt_ltap-vsolm.
          ENDIF.
        ENDIF.

        ls_totais_lote-matnr = lt_ltap-matnr.
        ls_totais_lote-werks = lt_ltap-werks.
        ls_totais_lote-lgort = lt_ltap-lgort.
        ls_totais_lote-charg = lt_ltap-charg.
        COLLECT ls_totais_lote INTO lt_totais_lote.

      ENDIF.
    ENDLOOP.

    LOOP AT lt_totais_lote.
      REFRESH: lt_return_msg, lt_sscc.
      CLEAR: lt_return_msg, lt_sscc.
      CLEAR l_tanum.


      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = l_meins
          language       = sy-langu
        IMPORTING
          output         = l_meins
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.

      ENDIF.

      lt_sscc-material      = lt_totais_lote-matnr.
      lt_sscc-quantidade    = trunc( lt_totais_lote-vsola ) .
      lt_sscc-uni           = l_meins.
      lt_sscc-lote_producao = lt_totais_lote-charg.
      APPEND lt_sscc.

      CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse  = gv_lgnum
          mov_type   = l_mov_type2                          "970 ou 975
          plant      = lt_totais_lote-werks
          s_loc      = lt_totais_lote-lgort
          req_number = lt_grupos-sammg
          req_type   = l_betyp
        IMPORTING
          to         = l_tanum
        TABLES
          return_msg = lt_return_msg
          sscc       = lt_sscc
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      DATA uni_incom_p TYPE zpalete_picking-uni_incom_p.
      uni_incom_p = lt_totais_lote-vsola MOD 1.

      IF uni_incom_p IS NOT INITIAL.

        CLEAR lt_sscc.
        REFRESH lt_sscc.
        lt_sscc-material      = lt_totais_lote-matnr.
        lt_sscc-quantidade    = 1.
        lt_sscc-uni           = l_meins.
        lt_sscc-lote_producao = lt_totais_lote-charg.
        APPEND lt_sscc.

        DO 20 TIMES.
          CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
            EXPORTING
              warehouse  = gv_lgnum
              mov_type   = '983'
              plant      = lt_totais_lote-werks
              s_loc      = lt_totais_lote-lgort
              req_number = lt_grupos-sammg
              req_type   = l_betyp
            IMPORTING
              to         = l_tanum
            TABLES
              return_msg = lt_return_msg
              sscc       = lt_sscc
            EXCEPTIONS
              error      = 1
              OTHERS     = 2.

          IF l_tanum IS INITIAL.
            WAIT UP TO 1 SECONDS.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  REFRESH lt_totais.


  LOOP AT lt_paletes WHERE uni_incom_p IS NOT INITIAL.

    REFRESH: lt_return_msg, lt_sscc, lt_lqua.
    CLEAR: lt_return_msg, lt_sscc, lt_lqua, l_quant_pkl, l_quant_aux_pkl.
    CLEAR l_tanum.

    PERFORM get_plant_data USING lt_grupos-sammg CHANGING lt_paletes-werks lt_paletes-lgort.
    CHECK lt_paletes-lgort EQ lt_paletes-lgort.

    SELECT * FROM lqua INTO TABLE lt_lqua
            WHERE lgnum = gv_lgnum
              AND lgtyp = 'PKL'
              AND matnr = lt_paletes-matnr
              AND werks = lt_paletes-werks
              AND lgort = lt_paletes-lgort
              AND lenum = ' '.

    IF lt_paletes-charg IS NOT INITIAL.
      DELETE lt_lqua WHERE charg <> lt_paletes-charg.
      DELETE lt_lqua WHERE werks <> lt_paletes-werks.
      DELETE lt_lqua WHERE lgort <> lt_paletes-lgort.
    ENDIF.

    IF NOT lt_lqua[] IS INITIAL.
      LOOP AT lt_lqua.
        CLEAR l_quant_aux_pkl.
        l_quant_aux_pkl = lt_lqua-verme.
        l_quant_pkl = l_quant_pkl + l_quant_aux_pkl.
      ENDLOOP.
    ENDIF.

    CHECK l_quant_pkl < lt_paletes-uni_incom_p.

    lt_sscc-material      = lt_paletes-matnr.
    lt_sscc-quantidade    = 1.
    lt_sscc-uni           = l_meins.
    APPEND lt_sscc.

    DO 20 TIMES.
      CLEAR:   lt_return_msg, flag.
      REFRESH: lt_return_msg.
      CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse   = gv_lgnum
          mov_type    = l_movpkl                          "979
          plant       = lt_paletes-werks
          s_loc       = lt_paletes-lgort
          certificado = 'X'
          req_number  = lt_paletes-refnr
          req_type    = l_betyp
        IMPORTING
          to          = l_tanum
        TABLES
          return_msg  = lt_return_msg
          sscc        = lt_sscc
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      DELETE lt_return_msg WHERE msgtyp <> 'E'.
      IF lt_return_msg[] IS NOT INITIAL.
        flag = 'X'.
        WAIT UP TO 1 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.

  IF flag IS NOT INITIAL.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '301' WITH lt_paletes-matnr.
  ENDIF.

  REFRESH lt_paletes.
  IF flag IS INITIAL.
    IF gv_lgnum EQ '150'.
      WAIT UP TO 30 SECONDS.
    ENDIF.

    READ TABLE lt_grupos INDEX 1.

    CLEAR lv_refnr.
    lv_refnr = lt_grupos-sammg.
    CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
      EXPORTING
        i_lgnum = gv_lgnum
        i_refnr = lv_refnr
        i_step  = 1.

  ENDIF.

ENDFORM.                    "verify_picking_quantity
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PALETES_REFNR  text
*      <--P_LT_PALETES_WERKS  text
*      <--P_LT_PALETES_LGORT  text
*----------------------------------------------------------------------*
FORM get_plant_data  USING u_refnr TYPE lvs_refnr
                     CHANGING c_werks TYPE werks_d
                              c_lgort TYPE lgort_d.

  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_refnr     = u_refnr
      i_recall    = 'X'
      i_usewm     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
    CHANGING
      c_lgnum     = gv_lgnum
      c_werks     = c_werks
      c_lgort     = c_lgort
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.


ENDFORM.                    " GET_PLANT_DATA
*&---------------------------------------------------------------------*
*&      Form  FREIGABE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM freigabe USING uv_lgnum uv_refnr.

  SELECT * FROM t311
           INTO TABLE it311
           WHERE lgnum = uv_lgnum AND
                 refnr = uv_refnr.


  CLEAR cnt.
  LOOP AT it311.
    EXIT.
  ENDLOOP.
  CHECK NOT it311 IS INITIAL.

  PERFORM sammelgang_freigeben.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAMMELGANG_FREIGEBEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sammelgang_freigeben .
  DATA: lt_total  TYPE TABLE OF l2sktotal.

  DATA: lv_error TYPE flag,
        lv_2step TYPE flag.

  DATA: ls_t311 TYPE t311.

  DATA: lt_ltap TYPE TABLE OF ltap WITH HEADER LINE.

  DATA qtd_pal LIKE ltap-vsolm.

** Verificar se tem ot´s partidas
  IF confirmado = '0'.

    CLEAR: t_ltak, t_ltap.
    REFRESH: t_ltak, t_ltap.

    SELECT * INTO TABLE t_ltak
          FROM ltak
              WHERE lgnum = it311-lgnum AND
                    refnr = it311-refnr.

    CHECK NOT t_ltak[] IS INITIAL.

    SELECT * FROM ltap INTO TABLE t_ltap
        FOR ALL ENTRIES IN t_ltak
              WHERE lgnum = t_ltak-lgnum AND
                    tanum = t_ltak-tanum.

    DELETE t_ltap WHERE vorga = 'ST'.

    " Validar Paletes Pal. Especial
    lt_ltap[] = t_ltap[].

    DELETE lt_ltap WHERE vlenr IS INITIAL.

    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM ltap INTO TABLE lt_ltap
        FOR ALL ENTRIES IN lt_ltap
        WHERE lgnum = lt_ltap-lgnum
        AND   nlenr = lt_ltap-vlenr
        AND   pquit = 'X'.

      DELETE lt_ltap WHERE vltyp <> 'EPE'.
    ENDIF.

    LOOP AT t_ltap.

      IF t_ltap-vltyp = 'DRI' OR
         t_ltap-vltyp = 'BLK' OR
         t_ltap-vltyp = 'TRI' OR
         t_ltap-vltyp = 'PRM' OR
         t_ltap-vltyp = 'AUT'.

        " Paletes Pal. Especial
        IF t_ltap-vltyp = 'AUT'.

          " Não Valida OT partidas
          READ TABLE lt_ltap WITH KEY nlenr = t_ltap-vlenr.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        " Paletes Completas
        CLEAR qtd_pal.
        SELECT SINGLE *
            FROM marm
                WHERE matnr = t_ltap-matnr AND
                      meinh = 'PAL'.
        IF sy-subrc = 0.

          qtd_pal = marm-umrez / marm-umren.
          qtd_pal = t_ltap-nsolm MOD qtd_pal.
          IF qtd_pal NE 0.
            MESSAGE ID 'ZWMMSG001' TYPE 'I'
                    NUMBER '243' WITH it311-refnr.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.

      IF t_ltap-vltyp = 'PCK' OR
         t_ltap-vltyp = 'PKB'.

        IF t_ltap-nsolm < 1.
**          Error, existem OT's com quantidades inferiores à UMB!
          MESSAGE ID 'ZWMMSG001' TYPE 'I'
                  NUMBER '349' WITH it311-refnr.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

*************************************

    DATA: i_vbss LIKE vbss OCCURS 0 WITH HEADER LINE.
    SELECT SINGLE *
        FROM t311
            WHERE lgnum = it311-lgnum AND
                  refnr = it311-refnr.
    IF NOT t311-kzdru IS INITIAL.
      MESSAGE ID 'ZWMMSG001' TYPE 'I'
             NUMBER '218' WITH it311-refnr.
      RETURN.
    ELSE.
      SELECT SINGLE *
        FROM zwm028
            WHERE lgnum = it311-lgnum AND
                  refnr = it311-refnr.
      IF sy-subrc <> 0.
        MESSAGE ID 'ZWMMSG001' TYPE 'I'
            NUMBER '099' WITH it311-refnr.
        RETURN.
      ELSE.
        IF zwm028-transporte IS INITIAL.
          MESSAGE ID 'ZWMMSG001' TYPE 'I'
              NUMBER '212' WITH it311-refnr.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    ls_t311 = it311.

    IF z_wm_cl_management=>is_group_completed( is_data = ls_t311 ) EQ abap_false.
**    Atenção, ainda existem OT's por criar para o Grupo &
      MESSAGE i011(zwm001) WITH it311-refnr.
      EXIT.
    ENDIF.


*    DATA lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.
*
*    CLEAR lt_zwm028.
*    REFRESH lt_zwm028.
*
*    SELECT SINGLE * FROM zwm026
*    WHERE armazem   EQ it311-lgnum
*      AND grupo     EQ it311-refnr
*      AND to_number NE space.
*    IF sy-subrc = 0.
*      SELECT * INTO TABLE lt_zwm028
*          FROM zwm028
*              WHERE lgnum = it311-lgnum AND
*                    refnr = it311-refnr.
*
*      LOOP AT lt_zwm028.
*        CLEAR: lt_zwm028-st_pul,
*               lt_zwm028-st_dck,
*               lt_zwm028-st_ppk.
*        MODIFY lt_zwm028 INDEX sy-tabix.
*      ENDLOOP.
*
*      MODIFY zwm028 FROM TABLE lt_zwm028.
*      COMMIT WORK AND WAIT.
*      DELETE FROM zwm028  WHERE lgnum = it311-lgnum AND
*                                 refnr = it311-refnr AND
*                                 remessa = ' '.
*      COMMIT WORK AND WAIT.
*
*      PERFORM actualiza_zwm028.
*
*    ELSE.
*      SELECT * INTO TABLE lt_zwm028
*           FROM zwm028
*               WHERE lgnum = it311-lgnum AND
*                     refnr = it311-refnr.
*
*
*      LOOP AT lt_zwm028.
*        CLEAR: lt_zwm028-st_pul,
*               lt_zwm028-st_dck,
*               lt_zwm028-st_ppk.
*        MODIFY lt_zwm028 INDEX sy-tabix.
*      ENDLOOP.
*
*      MODIFY zwm028 FROM TABLE lt_zwm028.
*      COMMIT WORK AND WAIT.
*      DELETE FROM zwm028  WHERE lgnum = it311-lgnum AND
*                                refnr = it311-refnr AND
*                                remessa = ' '.
*      COMMIT WORK AND WAIT.
*
*      PERFORM actualiza_zwm028.
*    ENDIF.

    PERFORM actualiza_zwm028.
  ENDIF.

  CLEAR lcoms.
  MOVE:
        it311-lgnum    TO lcoms-lgnum,
        it311-refnr    TO lcoms-refnr,
        it311-rbtyp    TO lcoms-rbtyp,
        con_dunkel     TO lcoms-dunkl,
        fcode_frei     TO lcoms-fcode.
  CLEAR lcoms-subrc.

  IF lv_2step <> 'X'.
    CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
      EXPORTING
        i_lgnum = lcoms-lgnum
        i_refnr = lcoms-refnr
        i_step  = 1.
  ENDIF.

**  Confirma as rotas automaticamente
  PERFORM confirma_rotas USING lcoms-lgnum
                               lcoms-refnr.

  LOOP AT it311 WHERE refnr = lcoms-refnr.
    it311-kreuz = space.
    it311-kzdru = 'X'.
    MODIFY it311.
  ENDLOOP.

  MOVE sav_fname TO hlp_fname.
  MOVE sav_rbtyp TO sum00-rbtyp.

  sy-lsind = sy-lsind - 1.
  PERFORM detailliste1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIRMA_ROTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LCOMS_LGNUM  text
*      -->P_LCOMS_REFNR  text
*----------------------------------------------------------------------*
FORM confirma_rotas USING pu_lgnum
                          pu_refnr.

  REFRESH range_refnr.
  CLEAR   range_refnr.

  MOVE: pu_refnr      TO range_refnr-low,
        con_sign_i    TO range_refnr-sign,
        con_option_eq TO range_refnr-option.
  APPEND range_refnr.

** Verificar se ja executou a rota de picking para o grupo
  SELECT SINGLE * FROM zwm026
          WHERE armazem = pu_lgnum AND
                  grupo = pu_refnr.

  IF sy-subrc <> 0.
    EXIT.

  ELSE.
    IF zwm026-num_recorrido <> '                    '.
      EXIT.
    ENDIF.
  ENDIF.

  SUBMIT zwmrep0018 WITH s_grupo IN range_refnr
                    WITH p_back = 'X'
                    AND RETURN.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_ZWM028
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_zwm028 .

  DATA: ls_zwm028 TYPE zwm028.
  DATA: i_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        i_lagp   LIKE lagp   OCCURS 0 WITH HEADER LINE,
        t_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE..

  DATA: num_paletes(2),
        pos            TYPE i,
        aux_pos.

  CALL FUNCTION 'ZWM_ACTUALIZA_PAL_GRUPO'
    EXPORTING
      lgnum                = it311-lgnum
      refnr                = it311-refnr
    EXCEPTIONS
      actualizacao_zwm0028 = 1
      OTHERS               = 2.

** Actualizar o total de paletes a nivel do grupo
  CLEAR t_zwm028.
  REFRESH t_zwm028.

  SELECT * INTO TABLE t_zwm028
      FROM zwm028
       WHERE lgnum = it311-lgnum
       AND   refnr = it311-refnr.

  DELETE t_zwm028 WHERE remessa IS INITIAL.

  CLEAR num_paletes.

  LOOP AT t_zwm028.
    CLEAR zwm028.
    SELECT SINGLE *
      FROM zwm028
          WHERE lgnum = t_zwm028-lgnum AND
                refnr = t_zwm028-refnr AND
                remessa = t_zwm028-remessa.
    IF sy-subrc = 0.
      num_paletes = num_paletes + zwm028-total_paletes.
    ENDIF.
  ENDLOOP.


  SELECT SINGLE *
    FROM zwm028 INTO ls_zwm028
    WHERE lgnum  = it311-lgnum AND
          refnr  = it311-refnr.

  IF sy-subrc = 0.
    CLEAR ls_zwm028-remessa.
    CLEAR ls_zwm028-ordem.
    CLEAR: ls_zwm028-servisan, ls_zwm028-emissor.

    ls_zwm028-total_paletes = num_paletes.

    " Pisco Liberação do Grupo de criação de todas OTs
      GET TIME.
      ls_zwm028-kzdru = 'X'.
      ls_zwm028-kzdat = sy-datum.
      ls_zwm028-kzuze = sy-uzeit.

    MODIFY zwm028 FROM ls_zwm028.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  CLEAR: i_zwm028.
  REFRESH: i_zwm028.

  SELECT * INTO TABLE i_zwm028
     FROM zwm028
      WHERE lgnum = it311-lgnum
      AND   refnr = it311-refnr.

  CLEAR aux_pos.
  pos = 1.

  DELETE i_zwm028 WHERE remessa IS INITIAL.

  SORT i_zwm028 BY ordem ASCENDING.

  LOOP AT i_zwm028.

    i_zwm028-posicao_ini_pul = pos.

    MODIFY i_zwm028 INDEX sy-tabix.
    pos = pos + i_zwm028-total_paletes.

  ENDLOOP.

  MODIFY zwm028 FROM TABLE i_zwm028.
  COMMIT WORK AND WAIT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MESSAGES  text
*----------------------------------------------------------------------*
FORM show_message  USING ut_messages TYPE tab_bdcmsgcoll.
  READ TABLE ut_messages
        INTO DATA(ls_message)
        INDEX 1.


  MESSAGE ID ls_message-msgid TYPE 'I'
                              NUMBER ls_message-msgnr
                              DISPLAY LIKE ls_message-msgtyp
                              WITH ls_message-msgv1 ls_message-msgv2
                                   ls_message-msgv3 ls_message-msgv4.
ENDFORM.
