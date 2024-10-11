*&---------------------------------------------------------------------*
*&  Include           ZXLIDU01
*&---------------------------------------------------------------------*

*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_LTAK) LIKE  LTAK STRUCTURE  LTAK
*"             VALUE(X_IDOC_CONTROL) LIKE  EDIDC STRUCTURE  EDIDC
*"       EXPORTING
*"             VALUE(X_IDOC_CONTROL) LIKE  EDIDC STRUCTURE  EDIDC
*"       TABLES
*"              T_LTAP STRUCTURE  LTAP_VB
*"              T_IDOC_DATA STRUCTURE  EDIDD
*"              T_LTHU STRUCTURE  LTHU OPTIONAL


** Pedidos de saidas ad-hoc de paletes
" Local Structures
DATA: ls_sdata  TYPE e1ltori,
      ls_header TYPE e1ltorh,
      ls_h      LIKE LINE OF t_idoc_data.

FIELD-SYMBOLS <fs_idoc_data> LIKE LINE OF t_idoc_data.




READ TABLE t_idoc_data INTO ls_h WITH KEY segnam = 'E1LTORH'.
IF sy-subrc IS INITIAL.

  MOVE ls_h-sdata TO ls_header.

** Armazém Automático França
**********************************************************************
  IF ls_header-lgnum = '150'.

    CALL FUNCTION 'Z_WMFR_EXIT_CANCEL_IDOC_REM'
      EXPORTING
        is_ltak         = i_ltak
      CHANGING
        cs_idoc_control = x_idoc_control
        ct_ltap_vb      = t_ltap[]
        ct_idoc_data    = t_idoc_data[].

    IF ls_header-bwlvs = '891'.

      LOOP AT t_idoc_data ASSIGNING <fs_idoc_data> WHERE segnam = 'E1LTORI'.

        CLEAR: ls_sdata.
        MOVE <fs_idoc_data>-sdata TO ls_sdata.

        IF ls_sdata-nltyp = 'REP'.
          ls_sdata-nlpla = 'REP'.
        ENDIF.

        MOVE ls_sdata TO <fs_idoc_data>-sdata.

      ENDLOOP.

    ENDIF.

** Armazém Automático Torres Novas (WCS)
**********************************************************************
  ELSEIF ls_header-lgnum = '100'.

    CALL FUNCTION 'ZWM_CREATE_IDOC_TO_WCS'
      EXPORTING
        i_ltak          = i_ltak
      CHANGING
        cs_idoc_control = x_idoc_control
        ct_ltap_vb      = t_ltap[]
        ct_idoc_data    = t_idoc_data[]
      EXCEPTIONS
        error           = 1
        OTHERS          = 2.
  ENDIF.

ENDIF.
