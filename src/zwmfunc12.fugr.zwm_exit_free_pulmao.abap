FUNCTION zwm_exit_free_pulmao .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     VALUE(I_TANUM) TYPE  TANUM OPTIONAL
*"     VALUE(IS_LTAK_VB) TYPE  LTAK_VB OPTIONAL
*"     VALUE(IT_LTAP_VB) TYPE  TT_LTAP_VB OPTIONAL
*"----------------------------------------------------------------------
  DATA: ls_ltak TYPE ltak.
  DATA: lt_ltap TYPE TABLE OF ltap WITH HEADER LINE.

  DO 10 TIMES.
    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = is_ltak_vb-lgnum
      AND   tanum = is_ltak_vb-tanum.

    IF ls_ltak-kquit = 'X'.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

*  DO.
*    IF lv_exit EQ 'X'.
*      EXIT.
*    ENDIF.
*  ENDDO.

** Process
***********************************************************************
  z_wm_cl_management=>free_pul( is_data = is_ltak_vb ).

ENDFUNCTION.
