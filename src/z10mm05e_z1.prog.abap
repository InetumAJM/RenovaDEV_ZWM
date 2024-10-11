*&---------------------------------------------------------------------*
*&  Include           Z10MM05E_V1                                      *
*&---------------------------------------------------------------------*
AT LINE-SELECTION.

  CHECK NOT TABI_GUIAS_POR_ITEM-GUIA IS INITIAL.
  READ TABLE TABI_GUIAS_POR_ITEM
             WITH KEY GUIA = TABI_GUIAS_POR_ITEM-GUIA.
  IF SY-SUBRC = 0.
    TABIX = SY-TABIX.
  ENDIF.
  LEAVE TO SCREEN 0.
