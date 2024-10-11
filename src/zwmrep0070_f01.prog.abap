*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0070_F01                                             *
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

  REFRESH: it_vekp, it_vepo, it_makt, it_mat.
  CLEAR:   it_vekp, it_vepo, it_makt, it_mat, linhas.

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

  CLEAR: pdt_matnr1,
         pdt_maktx1a,
         pdt_maktx1b,
         pdt_vemng1,
         pdt_altme1,
         pdt_charg1,

         pdt_matnr2,
         pdt_maktx2a,
         pdt_maktx2b,
         pdt_vemng2,
         pdt_altme2,
         pdt_charg2,

         cursorfield,
         ok_code_0001.

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

*  CLEAR lgnum.
*  CALL FUNCTION 'L_USER_DATA_GET'
*    EXPORTING
*      i_uname        = sy-uname
*    TABLES
*      t_xuser        = l_user
*    EXCEPTIONS
*      no_entry_found = 1
*      OTHERS         = 2.
*  IF sy-subrc <> 0.
***   raise no_warehouse_found.
*  ELSE.
*    READ TABLE l_user WITH KEY statu = gc_true.  " con_x.
*    IF sy-subrc <> 0.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '003'
*        IMPORTING
*          ret_code       = resposta.
*      IF resposta = 'O'.
*        LEAVE TO SCREEN '0001'.
*      ENDIF.
*    ELSE.
*      lgnum = l_user-lgnum.
*
**      IF l_user-devty = '8X40'.
**        setscreen1 = '0002'.
**      ELSE.
      setscreen1 = '0001'.
**      ENDIF.
*
*    ENDIF.
*  ENDIF.

ENDFORM.                    " FIND_WHS
