*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O08 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0021  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0021 OUTPUT.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

  IF xuser-lgnum IS INITIAL.
    PERFORM user_own_data.
  ENDIF.

** Quando um operario quer sair da pistola
  IF NOT f3_activo IS INITIAL.
    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.

*  break roffd.
  IF  bin_output_d(3) = 'PUL'.
    LOOP AT SCREEN.
      IF screen-name = 'POSICAO_PULMAO2'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name = 'POSICAO_PULMAO2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF bin_output_d(3) = 'PLT' OR bin_output_d(3) = 'DCK' OR
     bin_output_d(3) = 'PUL' OR bin_output_d(3) = 'PLM'.
    txt_num_pal = 'Num. Pal. Carga'.
  ELSE.
    CLEAR: txt_num_pal, num_pal_carga.
  ENDIF.

ENDMODULE.                 " STATUS_0021  OUTPUT
