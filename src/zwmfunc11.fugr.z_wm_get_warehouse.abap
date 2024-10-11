FUNCTION z_wm_get_warehouse.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_UNAME) TYPE  UNAME DEFAULT SY-UNAME
*"     REFERENCE(I_RF) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_OPTIONAL) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_LGNUM) TYPE  LGNUM
*"     REFERENCE(E_WERKS) TYPE  WERKS_D
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------


  DATA: lt_user     TYPE TABLE OF lrf_wkqu,
        ls_user     TYPE lrf_wkqu.

***********************************************************************
  CLEAR: e_lgnum, e_werks.


** Obtem dados associados ao utilizador
***********************************************************************
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = i_uname
    TABLES
      t_xuser        = lt_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    CLEAR lt_user.
  ENDIF.

  READ TABLE lt_user INTO ls_user WITH KEY statu = 'X'.

  IF sy-subrc <> 0.
    IF NOT i_optional IS INITIAL.
      EXIT.
    ENDIF.

**  User & is invalid

    RAISE error.
  ENDIF.


** Dados mantidos correctamente para o utilizador
***********************************************************************

** Centro
  SELECT SINGLE werks FROM t320 INTO e_werks
          WHERE lgnum EQ ls_user-lgnum.




** Telas
***********************************************************************

  e_lgnum = ls_user-lgnum.
  SET PARAMETER ID 'LGN' FIELD e_lgnum.

ENDFUNCTION.
