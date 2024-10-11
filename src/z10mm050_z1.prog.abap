*&---------------------------------------------------------------------*
*&  Include           Z10MM050_V1                                      *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZRF1'.

  CLEAR vbss-sammg.
  SET PARAMETER ID 'GRN' FIELD vbss-sammg.
ENDMODULE.                             " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LE_ITM_GUIA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE le_itm_guia OUTPUT.
  loop_lines = sy-loopc.
  read_index = page_begin + sy-stepl.
  READ TABLE tabi_guias_por_item-linha INTO *z1com_s INDEX read_index.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING *z1com_s TO lips.
*   move-corresponding *z1com_s to z1mom.
    MOVE-CORRESPONDING *z1com_s TO z1com_s.
    z1com_s-posnr = *z1com_s-vgpos.
  ELSE.
    EXIT FROM STEP-LOOP.
  ENDIF.

ENDMODULE.                             " LE_ITM_GUIA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST02'.
  SET TITLEBAR 'T02'.
ENDMODULE.                             " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MOD_ECRA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mod_ecra OUTPUT.
  CHECK z1com_s-pstyv = z1om_pstyv_oferta.
* linhas de oferta e taras
  LOOP AT SCREEN.
    screen-intensified = '1'.
    IF screen-group1 = 'TOT'.
      screen-active = '0'.
    ENDIF.
    screen-input = '0'.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                             " MOD_ECRA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0250  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0250 OUTPUT.
  SET PF-STATUS 'ST03'.
  SET TITLEBAR 'T01'.
ENDMODULE.                             " STATUS_0250  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LISTA_GUIAS_PICKING  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lista_guias_picking OUTPUT.
  INCLUDE <icon>.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  LOOP AT tabi_guias_por_item.
    WRITE / tabi_guias_por_item-guia.
    IF tabi_guias_por_item-guia_bloqueada = 'X'.
      WRITE icon_red_light AS ICON.
    ELSEIF tabi_guias_por_item-oferta_processada = 'X'.
      WRITE icon_green_light AS ICON.
    ELSE.
      WRITE icon_yellow_light AS ICON.
    ENDIF.
    HIDE tabi_guias_por_item-guia.
  ENDLOOP.
  CLEAR tabi_guias_por_item-guia.
  LEAVE SCREEN.
ENDMODULE.                             " LISTA_GUIAS_PICKING  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ACT_ICON_OFERTA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE act_icon_oferta OUTPUT.
  IF tabi_guias_por_item-guia_bloqueada = 'X'.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = 'ICON_RED_LIGHT'
      IMPORTING
        RESULT                = icon_oferta
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
  ELSEIF tabi_guias_por_item-oferta_processada = 'X'.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = 'ICON_GREEN_LIGHT'
      IMPORTING
        RESULT                = icon_oferta
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
  ELSE.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = 'ICON_YELLOW_LIGHT'
      IMPORTING
        RESULT                = icon_oferta
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
  ENDIF.

ENDMODULE.                 " ACT_ICON_OFERTA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0150  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0150 OUTPUT.
  SET PF-STATUS 'ZRF1'.
  SET TITLEBAR '150'.

*& Begin of Modification by Tiago Pateiro - ROFF @ 06.01.2016 17:20:11
  PERFORM f_user_get_lgnum.
*& End of Modification by Tiago Pateiro - ROFF @ 06.01.2016 17:20:11
ENDMODULE.                 " STATUS_0150  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  inicializar  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE inicializar OUTPUT.
  DATA : wa_screen LIKE screen.


  IF NOT vbss-sammg IS INITIAL.
    LOOP AT SCREEN INTO wa_screen.
      IF wa_screen-name = 'VTTK-DATEN' OR wa_screen-name = 'VTTK-UATEN'.
        wa_screen-input = '1'.
        MODIFY SCREEN FROM wa_screen.
      ENDIF.
    ENDLOOP.
  ENDIF.


  IF NOT vttk-daten IS INITIAL  OR NOT vttk-uaten IS INITIAL
     OR setcursor = 'VTTK-DATEN' OR setcursor = 'VTTK-UATEN'.

    LOOP AT SCREEN INTO wa_screen.
      IF wa_screen-name = 'VBSS-SAMMG'.
        wa_screen-input = '0'.
        MODIFY SCREEN FROM wa_screen.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF setcursor NE space.
    SET CURSOR FIELD setcursor.
  ENDIF.

ENDMODULE.                 " inicializar  OUTPUT
