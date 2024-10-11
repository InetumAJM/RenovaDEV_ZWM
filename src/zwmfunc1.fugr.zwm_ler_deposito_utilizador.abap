FUNCTION ZWM_LER_DEPOSITO_UTILIZADOR .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(E_LGNUM) LIKE  MLGN-LGNUM
*"----------------------------------------------------------------------
*...Internal table for user parameters...................
  data: begin of xuser occurs 0.
          include structure lrf_wkqu.
  data: end of xuser.

  constants: con_true type c value '1',
             con_x    type c value 'X'.

  data: whs_id          like lein-lgnum,
        present         like lrf_wkqu-devty,   " PRESENTATION TYPE
        exver           like lrf_wkqu-exver,   " EXIT VERSION
        docnum          like lrf_wkqu-docnum,    " AFTER 1STEP CONFIRM
        personal_number like ltak-pernr,
        sort_value      like lrf_wkqu-sorsq,    "user prm for TO Sort
        autho           like lrf_wkqu-autho,     " AUTHORIZATION PARAM
        user_prf        like lrf_wkqu-queue, "shortened User_profile
        b_mnu_scr       like lrf_wkqu-mmenu.

* Read the user data from table lrf_wkqu ( for all the warehouses)
  call function 'L_USER_DATA_GET'
    exporting
      i_uname        = sy-uname
    tables
      t_xuser        = xuser
    exceptions
      no_entry_found = 01.

  if sy-subrc <> 0.
    message e192 with sy-uname.

*    MESSAGE_NUMBER = '192'.
*    MESSAGE_VAR1  = SY-UNAME .
*    MESSAGE_LANG = SY-LANGU.
*    PERFORM ERROR_MESSAGE.
*    EXIT.
  else.
* read the user record of the active whs
    read table xuser with key statu = con_x.
    if sy-subrc <> 0.
      message e191 with sy-uname.

*      MESSAGE_NUMBER = '191'.
*      MESSAGE_VAR1  = SY-UNAME .
*      MESSAGE_LANG = SY-LANGU.
*      PERFORM ERROR_MESSAGE.
*      EXIT.
    endif.
* fill user parameters for the session
    move-corresponding xuser to lrf_wkqu.
    whs_id          = lrf_wkqu-lgnum.  " WAREHOUSE ID
    b_mnu_scr       = lrf_wkqu-mmenu.  " BASE MENU SCREEN
    present         = lrf_wkqu-devty.  " PRESENTATION TYPE
    exver           = lrf_wkqu-exver.  " EXIT VERSION
    docnum          = lrf_wkqu-docnum.
    personal_number = lrf_wkqu-pernr.
    sort_value      = lrf_wkqu-sorsq.
    user_prf        = lrf_wkqu-queue.
    autho           = lrf_wkqu-autho.

  endif.

  move xuser-lgnum to e_lgnum.

endfunction.
