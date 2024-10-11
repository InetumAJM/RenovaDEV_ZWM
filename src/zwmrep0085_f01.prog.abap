*&---------------------------------------------------------------------*
*&  Include           ZWMREP0085_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_whs .

  CLEAR gv_lgnum.
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = gs_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   ERRO: Utilizador não tem armazem atribuído!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '001'.
  ELSE.
    READ TABLE gs_user WITH KEY statu = abap_true. "Util. Atrib. Arm.
    IF sy-subrc <> 0.
      WRITE gs_user-lgnum TO gv_text1 LEFT-JUSTIFIED.
*     ERRO: Utilizador não está atribuído ao armazém &!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMPMSG'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '002'
          message_var1   = gv_text1.

    ELSE.
      gv_lgnum = gs_user-lgnum.
      gv_lgnum = 100.
      IF gs_user-devty(5) = '16X20'.
        setscreen1 = '0001'.
      ELSE.
        setscreen1 = '0011'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_WHS
*&---------------------------------------------------------------------*
*&      Form  CHK_LENUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_exidv.

*  DATA: lt_vekp TYPE TABLE OF vekp.
*  data: lt_vepo TYPE TABLE OF vepo.
  DATA: qtd TYPE ekpo-menge.
  DATA: BEGIN OF lt_vekp OCCURS 0,
          exidv LIKE vekp-exidv,
          venum LIKE vekp-venum,
          erdat LIKE vekp-erdat,
          werks LIKE vekp-werks,
          lgort LIKE vekp-lgort,
          vkorg LIKE vekp-vkorg,
          vtweg LIKE vekp-vtweg,
        END OF lt_vekp.

  DATA: BEGIN OF lt_vepo OCCURS 0,
          matnr LIKE vepo-matnr,
          charg LIKE vepo-charg,
          vemng LIKE vepo-vemng,
          vemeh LIKE vepo-vemeh,
          altme LIKE vepo-altme,
        END OF lt_vepo.

  DATA: BEGIN OF lt_makt OCCURS 0,
          matnr LIKE makt-matnr,
          maktx LIKE makt-maktx,
        END OF lt_makt.

  DATA: lv_exidv   TYPE vekp-exidv.
  DATA: lv_retcode TYPE flag.

  DATA: ls_lqua  TYPE lqua.
  DATA: ls_mvke  TYPE mvke.
  DATA: lt_lqua  TYPE TABLE OF lqua WITH HEADER LINE.
  DATA: lv_zwm055 TYPE zwm055.
**********************************************************************
  CHECK NOT scr0001-exidv IS INITIAL.

  READ TABLE gt_mat WITH KEY exidv = scr0001-exidv.

  IF sy-subrc = 0.
    WRITE scr0001-exidv TO gv_text1 LEFT-JUSTIFIED.
** SSCC & já adicionado
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '287'
        message_var1   = gv_text1.
    CLEAR scr0001-exidv.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

  lv_exidv = scr0001-exidv.
*  CLEAR scr0001.
  PERFORM clear.
*  CLEAR:  scr0001-total, scr0001-actual.


  SELECT venum erdat exidv werks lgort vkorg
          FROM vekp
          INTO CORRESPONDING FIELDS OF TABLE lt_vekp
         WHERE exidv EQ lv_exidv.

  IF sy-subrc <> 0.
** Erro! SSCC & invalido!
    WRITE lv_exidv TO gv_text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '113'
        message_var1   = gv_text1.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Obtem o mais recente
  SORT lt_vekp BY erdat DESCENDING.

  READ TABLE lt_vekp INDEX 1.

  SELECT matnr charg vemng vemeh altme
          FROM vepo
          INTO CORRESPONDING FIELDS OF TABLE lt_vepo
         WHERE venum EQ lt_vekp-venum.


  IF sy-subrc <> 0.
** Erro! SSCC & sem conteudo!
    WRITE lv_exidv TO gv_text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '241'
        message_var1   = gv_text1.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Descrição materiais
  SELECT matnr maktx
          FROM  makt
          INTO CORRESPONDING FIELDS OF TABLE lt_makt
           FOR ALL ENTRIES IN lt_vepo
         WHERE matnr = lt_vepo-matnr
          AND   spras = sy-langu.

  SORT lt_makt BY matnr.

*  SELECT * FROM lqua INTO TABLE lt_lqua
*    FOR ALL ENTRIES IN lt_vekp
*        WHERE lgnum = gv_lgnum
*          AND lenum = lt_vekp-exidv.
*
*  SORT lt_lqua BY lenum.
*
*  READ TABLE lt_lqua WITH KEY  lenum = lt_vekp-exidv BINARY SEARCH.
*  IF sy-subrc = 0.
*    CONCATENATE lt_lqua-lgtyp lt_lqua-lgpla INTO gt_mat-pos
*    SEPARATED BY space.
*  ENDIF.

** Validar organização de vendas
  LOOP AT lt_vepo.

    SELECT SINGLE *
      FROM mvke INTO ls_mvke
      WHERE matnr =  lt_vepo-matnr
      AND   vkorg = 'RP01'
      AND   vtweg = '99'.

    IF sy-subrc <> 0.
      gv_text1 = lt_vepo-matnr.

      " Material & sem org. vendas & e canal distr. & definidos! Deseja avançar?
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMPMSG'
          message_lang   = sy-langu
          message_type   = 'W'
          message_number = '178'
          message_var1   = gv_text1
          message_var2   = 'RP01'
          message_var3   = '99'
        IMPORTING
          ret_code       = lv_retcode.

      IF lv_retcode <> 'O'.
        CLEAR scr0001-exidv.

        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.

  ENDLOOP.

** Obter dados
**********************************************************************
  LOOP AT lt_vepo.
    gt_mat-exidv = lt_vekp-exidv.
    gt_mat-werks = lt_vekp-werks.
    gt_mat-lgort = lt_vekp-lgort.

    MOVE-CORRESPONDING lt_vepo TO gt_mat.

** Descrição material
    CLEAR lt_makt.
    READ TABLE lt_makt WITH KEY matnr = lt_vepo-matnr BINARY SEARCH.
    gt_mat-maktx_a = lt_makt-maktx(20).
    gt_mat-maktx_b = lt_makt-maktx+20(20).
    APPEND gt_mat.
    CLEAR  gt_mat.
  ENDLOOP.


** Processamento páginas
  scr0001-total   = lines( gt_mat ).
  scr0001-actual  = lines( gt_mat ).

  READ TABLE gt_mat INDEX scr0001-actual.
  scr0001-matnr    = gt_mat-matnr.
  scr0001-maktx_a  = gt_mat-maktx_a.
  scr0001-maktx_b  = gt_mat-maktx_b.
  scr0001-vemng    = gt_mat-vemng.
  scr0001-vemeh    = gt_mat-vemeh.
  scr0001-altme    = gt_mat-altme.
  scr0001-charg    = gt_mat-charg.
  scr0001-exidv1   = gt_mat-exidv.
  scr0001-origem   = gt_mat-pos.

  PERFORM regista_tab.

ENDFORM.                    " CHK_LENUM
*&---------------------------------------------------------------------*
*&      Form  LIST_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_sscc .
  READ TABLE gt_mat INDEX scr0001-actual.
  scr0001-matnr    = gt_mat-matnr.
  scr0001-maktx_a  = gt_mat-maktx_a.
  scr0001-maktx_b  = gt_mat-maktx_b.
  scr0001-vemng    = gt_mat-vemng.
  scr0001-vemeh    = gt_mat-vemeh.
  scr0001-charg    = gt_mat-charg.
  scr0001-origem   = gt_mat-pos.
  scr0001-exidv1   = gt_mat-exidv.
ENDFORM.                    " LIST_SSCC
*&---------------------------------------------------------------------*
*&      Form  CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear .

  CLEAR:scr0001-exidv,
        scr0001-exidv1,
        scr0001-matnr,
        scr0001-maktx_a,
        scr0001-maktx_b,
        scr0001-ean11,
        scr0001-charg,
        scr0001-vemng,
        scr0001-altme,
        scr0001-origem.
ENDFORM.                    " CLEAR
*&---------------------------------------------------------------------*
*&      Form  REGISTA_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM regista_tab .
  DATA: ls_zwm055 TYPE zwm055.
**********************************************************************

  ls_zwm055-matnr   = gt_mat-matnr.
  ls_zwm055-maktx_a = gt_mat-maktx_a.
  ls_zwm055-maktx_b = gt_mat-maktx_b.
  ls_zwm055-vemng   = gt_mat-vemng.
  ls_zwm055-altme   = gt_mat-altme.
  ls_zwm055-charg   = gt_mat-charg.
  ls_zwm055-exidv   = gt_mat-exidv.
  ls_zwm055-pos     = gt_mat-pos.
  ls_zwm055-uname   = sy-uname.
  ls_zwm055-erdat   = sy-datum.
  ls_zwm055-erzeit  = sy-uzeit.
  ls_zwm055-werks   = gt_mat-werks.
  ls_zwm055-lgort   = gt_mat-lgort.
  ls_zwm055-vemeh   = gt_mat-vemeh.

  INSERT INTO zwm055 VALUES ls_zwm055.
ENDFORM.                    " REGISTA_TAB
*&---------------------------------------------------------------------*
*&      Form  GET_LOST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_lost_data .

  DATA: lt_zwm055 TYPE TABLE OF zwm055.
  DATA: ls_zwm055 TYPE zwm055.
  DATA: ls_mat    TYPE gty_mat.
**********************************************************************
  CHECK gt_mat IS INITIAL.

  SELECT * FROM zwm055 INTO TABLE lt_zwm055
   WHERE uname = sy-uname.

  IF lt_zwm055 IS NOT INITIAL.
    LOOP AT lt_zwm055 INTO ls_zwm055.
      MOVE-CORRESPONDING ls_zwm055 TO ls_mat.
      APPEND ls_mat TO gt_mat.
    ENDLOOP.

    scr0001-actual = 1.
    scr0001-total = lines( gt_mat ).

  ENDIF.

ENDFORM.                    " GET_LOST_DATA
*&---------------------------------------------------------------------*
*&      Form  CLEAR_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_all .
  CLEAR scr0001.
  CLEAR gt_mat.
  REFRESH gt_mat.
  DELETE FROM zwm055 WHERE uname = sy-uname.
ENDFORM.                    " CLEAR_ALL
*&---------------------------------------------------------------------*
*&      Form  TRF_FORN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trf_forn .

  DATA: ls_t001w       TYPE t001w.
  DATA: lt_dlv_items   TYPE lips OCCURS 0 WITH HEADER LINE.
  DATA: lv_vbeln       TYPE vbeln_vl.
  DATA: lv_vkorg       TYPE vkoiv.
  DATA: lv_vstel       TYPE vstel.
  DATA: lv_vtweg       TYPE vtweg.
  DATA: lv_kunwe       TYPE kunwe.
  DATA: lv_spart       TYPE spaiv.
  DATA: lt_return_msg  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: ls_msg         TYPE bdcmsgcoll.
** Criar Remessa de transferencia
**********************************************************************
  CHECK gt_mat IS NOT INITIAL.
** Obter recebedor mercadoria
  READ TABLE gt_mat INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE *
      FROM t001w INTO ls_t001w
      WHERE werks = gt_mat-werks.

    IF sy-subrc = 0.
*      lv_vkorg = ls_t001w-vkorg.
*      lv_vstel = ls_t001w-vstel.
*      lv_vtweg = ls_t001w-vtweg.
*      lv_spart = ls_t001w-spart.
      lv_kunwe = ls_t001w-kunnr.
    ENDIF.
  ENDIF.

** Obter Items
  REFRESH: lt_dlv_items.

  LOOP AT gt_mat.
    CLEAR lt_dlv_items.
    lt_dlv_items-werks = gt_mat-werks.
    lt_dlv_items-lgort = gt_mat-lgort.
    lt_dlv_items-matnr = gt_mat-matnr.
    lt_dlv_items-charg = gt_mat-charg.
    lt_dlv_items-lfimg = gt_mat-vemng.
    lt_dlv_items-vrkme = gt_mat-altme.
    lt_dlv_items-meins = gt_mat-vemeh.
    lt_dlv_items-kdmat = gt_mat-exidv.
    APPEND lt_dlv_items.
  ENDLOOP.

  CLEAR lv_vbeln.
  CALL FUNCTION 'ZWMMP_CREATE_DELIVERY_TRF'
    EXPORTING
      i_kunwe  = lv_kunwe
      i_lfart  = 'ZRTR'
      i_pstyv  = 'ZTRF'
*     i_vstel  = lv_vstel
*     i_vkorg  = lv_vkorg
*     i_vtweg  = lv_vtweg
*     i_spart  = lv_spart
    IMPORTING
      e_vbeln  = lv_vbeln
    TABLES
      t_return = lt_return_msg
      t_items  = lt_dlv_items
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF sy-subrc <> 0.

    READ TABLE lt_return_msg INDEX 1 INTO ls_msg.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_msg-msgid
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = ls_msg-msgnr
          message_var1   = ls_msg-msgv1
          message_var2   = ls_msg-msgv2
          message_var3   = ls_msg-msgv3
          message_var4   = ls_msg-msgv4.
    ENDIF.
    EXIT.
  ENDIF.

** Imprime guia de remessa
**********************************************************************
  CALL FUNCTION 'Z_PRINT_PDF_REMESSA'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_vbeln     = lv_vbeln
      i_print     = gv_printer
*     I_COPIES    =
    EXCEPTIONS
      error_vbeln = 1
      error_print = 2
      error_dados = 3
      OTHERS      = 4.

  MOVE lv_vbeln       TO gv_text1.

* Foi criada a Remessa & !
  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '286'
      message_var1   = gv_text1.
  PERFORM clear_all.
ENDFORM.                    " TRF_FORN
*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_parameters .
  DATA: lv_valor       LIKE zwm001-valor.
**********************************************************************
  DATA: lt_messages TYPE tab_bdcmsgcoll,
        ls_message  TYPE bdcmsgcoll.
**********************************************************************
  DO 1 TIMES.
** Impressora PDF
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'GERAL'
        i_parametro = 'PRINTER_PDF'
      IMPORTING
        e_valor     = gv_printer
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDDO.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " GET_PARAMETERS
