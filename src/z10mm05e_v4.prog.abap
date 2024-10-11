*&---------------------------------------------------------------------*
*&  Include           Z10MM05E_V1                                      *
*&---------------------------------------------------------------------*

AT LINE-SELECTION.

  CHECK NOT tabi_guias_por_item-guia IS INITIAL.
  READ TABLE tabi_guias_por_item
             WITH KEY guia = tabi_guias_por_item-guia.
  IF sy-subrc = 0.
    tabix = sy-tabix.
  ENDIF.
  LEAVE TO SCREEN 0.
