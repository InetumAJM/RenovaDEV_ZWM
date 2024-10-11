*----------------------------------------------------------------------*
* Include: ZWMFR0002_PBO
*----------------------------------------------------------------------*
* Description: RF - Entrada de Produto Acabado/Bobines PT
* RICEFW: WM.02/WM.03
*----------------------------------------------------------------------*
* Author........: [Pedro Silva] [ROFFD] [ROFF(SDF)]
*                 [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date:  2015-10-26
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  M_STATUS_0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_status_0010 OUTPUT.
  SET PF-STATUS c_pfstatus_scr001.

  SET CURSOR FIELD gv_cursor.
ENDMODULE.                 " M_STATUS_0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_SET_FIELDS_0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_set_fields_0010 OUTPUT.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN c_cursor_ebeln.
        IF gv_cursor NE c_cursor_ebeln.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN 'GS_SCR001-DATE'.
          screen-input = 0.
          MODIFY SCREEN.
      WHEN c_cursor_xblnr.
        IF gv_cursor NE c_cursor_xblnr.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN c_cursor_licha.
        IF gv_cursor NE c_cursor_licha.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN c_cursor_npall.
        IF gv_cursor NE c_cursor_npall.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF sy-tcode NE c_tcode_zwmfr0002c AND screen-group1 EQ 'BFO'.
      screen-input = 0.
      screen-invisible = 1.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " M_SET_FIELDS_0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_STATUS_0020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_status_0020 OUTPUT.
  SET PF-STATUS c_pfstatus_scr002.

  IF gs_scr001-cpall EQ gs_scr001-npall AND gs_scr002 IS NOT INITIAL.
    gv_cursor = c_cursor_psave.
  ENDIF.

  SET CURSOR FIELD gv_cursor.
ENDMODULE.                 " M_STATUS_0020  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_SET_FIELDS_0020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_set_fields_0020 OUTPUT.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN c_cursor_lenum.
        IF gs_scr001-cpall LE gs_scr001-npall AND gs_scr002 IS INITIAL.
          screen-input  = 1.
        ELSEIF gs_scr001-cpall LT gs_scr001-npall AND gs_scr002 IS NOT INITIAL.
          screen-input = 0.
        ELSE.
          screen-input  = 0.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " M_SET_FIELDS_0020  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_STATUS_0030  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_status_0030 OUTPUT.
  SET PF-STATUS c_pfstatus_scr002.

  IF gs_scr001-cpall EQ gs_scr001-npall.
    IF gs_scr002-zeugn IS NOT INITIAL AND gs_scr002-matnr IS NOT INITIAL AND
       gs_scr002-charg IS NOT INITIAL AND gs_scr002-vsolm IS NOT INITIAL.
      gv_cursor = c_cursor_psave.
    ENDIF.
  ENDIF.

  SET CURSOR FIELD gv_cursor.
ENDMODULE.                 " M_STATUS_0030  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_SET_FIELDS_0030  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_set_fields_0030 OUTPUT.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN c_cursor_zeugn.
        IF gs_scr001-cpall LE gs_scr001-npall AND gs_scr002 IS INITIAL.
          IF gv_cursor NE c_cursor_zeugn.
            screen-input = 0.
          ELSE.
            screen-input = 1.
          ENDIF.
        ELSEIF gs_scr001-cpall LT gs_scr001-npall AND gs_scr002 IS NOT INITIAL.
          screen-input = 0.
        ELSE.
          screen-input  = 0.
        ENDIF.
      WHEN c_cursor_matnr.
        IF gv_cursor NE c_cursor_matnr.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN c_cursor_charg.
        IF gv_cursor NE c_cursor_charg.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN c_cursor_vsolm.
        IF gv_cursor NE c_cursor_vsolm.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " M_SET_FIELDS_0030  OUTPUT
