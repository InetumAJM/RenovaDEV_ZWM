*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0066_F01                                             *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  clear
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear.

  CLEAR: pdt_origem,
         pdt_ean11,
         cursorfield,
         pdt_total,
         pdt_pag,
         currpage,
         lincount,
         ok_code_0001,
         it_mat.

  REFRESH: it_mat.

  PERFORM clear_ecran.

ENDFORM.                    " clear

*&---------------------------------------------------------------------*
*&      Form  clear_ecran
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_ecran.

  CLEAR:
        pdt_matnr   ,
        pdt_maktx_a ,
        pdt_maktx_b ,
        pdt_meins_1 ,
        pdt_charg_1 ,
        pdt_gesme_1 ,
        pdt_verme_1 ,
        pdt_einme_1 ,
        pdt_ausme_1 .

ENDFORM.                    " clear_ecran

*&---------------------------------------------------------------------*
*&      Form  FIND_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs.

  CLEAR whs.
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
**   raise no_warehouse_found.
  ELSE.
    READ TABLE l_user WITH KEY statu = gc_true.  " con_x.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
        IMPORTING
          ret_code       = resposta.
      IF resposta = 'O'.
        LEAVE TO SCREEN '0001'.
      ENDIF.
    ELSE.
      whs = l_user-lgnum.

*      IF l_user-devty = '8X40'.
*        setscreen1 = '0002'.
*      ELSE.
      setscreen1 = '0001'.
*      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " FIND_WHS
*&---------------------------------------------------------------------*
*&      Form  CHECK_EAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ean .
*  DATA: ls_marm TYPE marm.
*        ls_lqua TYPE lqua.
**********************************************************************
  CHECK pdt_ean11 IS NOT INITIAL.

  SELECT SINGLE * FROM lqua INTO gs_lqua
    WHERE lgnum = whs       AND
          lgtyp = aux_lgtyp AND
          lgpla = aux_lgpla.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '285'
        message_var1   = text1.

    CLEAR : pdt_ean11, pdt_origem .
    MOVE 'PDT_EAN11' TO cursorfield.
  ENDIF.

** Verifica se exite codigo de ean11 para o material e U.M.
  SELECT SINGLE * FROM marm INTO gs_marm
    WHERE matnr = gs_lqua-matnr AND
          ean11 = pdt_ean11.

  IF sy-subrc <> 0.
    MOVE pdt_ean11 TO text1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '284'
        message_var1   = text1.

    CLEAR : pdt_ean11.
    MOVE 'PDT_EAN11' TO cursorfield.
  ELSE.
    PERFORM get_dados.
  ENDIF.
ENDFORM.                    " CHECK_EAN
*&---------------------------------------------------------------------*
*&      Form  GET_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dados .
  CLEAR:   it_lqua, it_mat, it_makt, lincount, pdt_total, pdt_pag,
           matnr.
  REFRESH: it_lqua, it_mat, it_makt.

  SELECT matnr charg meins gesme verme einme ausme
         INTO CORRESPONDING FIELDS OF TABLE it_lqua
         FROM  lqua
         WHERE lgnum = whs
         AND   lgtyp = aux_lgtyp
         AND   lgpla = aux_lgpla.

  IF NOT it_lqua[] IS INITIAL.
    SELECT matnr maktx
           FROM  makt
           INTO CORRESPONDING FIELDS OF TABLE it_makt
           FOR ALL ENTRIES IN it_lqua
           WHERE matnr = it_lqua-matnr
           AND   spras = sy-langu.

    SORT it_makt BY matnr.
  ENDIF.

  LOOP AT it_lqua.
    IF matnr IS INITIAL.
      matnr = it_lqua-matnr.
    ELSE.
      IF it_lqua-matnr <> matnr.
** Mais do que 1 material
        WRITE pdt_origem TO text1 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '225'
            message_var1   = text1
          IMPORTING
            ret_code       = resposta.
        IF resposta = 'O'.
          PERFORM clear.
          MOVE 'PDT_ORIGEM' TO cursorfield.
          LEAVE TO SCREEN setscreen1.
        ENDIF.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING it_lqua TO it_mat.

    CLEAR it_makt.
    READ TABLE it_makt WITH KEY matnr = it_lqua-matnr BINARY SEARCH.
    it_mat-maktx = it_makt-maktx.

    COLLECT it_mat.
    CLEAR   it_mat.
  ENDLOOP.

  SORT it_mat BY matnr charg.

  DESCRIBE TABLE it_mat LINES lincount.
  totalpage = lincount.
  pdt_total = totalpage.
  currpage  = 1.
  pdt_pag   = currpage.
  READ TABLE it_mat INDEX 1.

  pdt_matnr    = it_mat-matnr.
  pdt_maktx_a  = it_mat-maktx(20).
  pdt_maktx_b  = it_mat-maktx+20(20).

  IF pdt_origem(3) = 'PKL'.

    PERFORM calc USING it_mat-gesme CHANGING pdt_gesme_1.
    PERFORM calc USING it_mat-verme CHANGING pdt_verme_1.
    PERFORM calc USING it_mat-einme CHANGING pdt_einme_1.
    PERFORM calc USING it_mat-ausme CHANGING pdt_ausme_1.
*  pdt_gesme_1  = it_mat-gesme.
*  pdt_verme_1  = it_mat-verme.
*  pdt_einme_1  = it_mat-einme.
*  pdt_ausme_1  = it_mat-ausme.

    pdt_meins_1  = gs_marm-meinh .
  ELSE.
    pdt_gesme_1  = it_mat-gesme.
    pdt_verme_1  = it_mat-verme.
    pdt_einme_1  = it_mat-einme.
    pdt_ausme_1  = it_mat-ausme.

    pdt_meins_1  = it_mat-meins.
  ENDIF.

  pdt_charg_1  = it_mat-charg.

ENDFORM.                    " GET_DADOS
*&---------------------------------------------------------------------*
*&      Form  CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MAT_GESME  text
*      <--P_PDT_GESME_1  text
*----------------------------------------------------------------------*
FORM calc  USING    p_value TYPE menge_d
           CHANGING p_pdt_value.

  DATA: lv_menge TYPE menge_d.

  lv_menge = p_value * gs_marm-umren / gs_marm-umrez.
  CALL FUNCTION 'ROUND'
    EXPORTING
      decimals      = 0
      input         = lv_menge
      sign          = 'X'
    IMPORTING
      output        = lv_menge
    EXCEPTIONS
      input_invalid = 1
      overflow      = 2
      type_invalid  = 3
      OTHERS        = 4.

p_pdt_value = lv_menge.
ENDFORM.                    ". CALC
