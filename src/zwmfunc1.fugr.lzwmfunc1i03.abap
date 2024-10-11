*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT9  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit9 INPUT.

  CLEAR: ok_code_0009,
         incidencia,
         incidencia_terceiros,
         desc_incidencia,
         sscc_errado,
         cursorfield,
         l_tanum,
         l_tapos,
         l_vlpla,
         l_vltyp,
         l_letyp,
         l_vlenr,
         l_linhas,
         l_remontada,
         itab_lqua.

  CLEAR: incidencia, desc_incidencia, sscc_errado.

** Só no caso em que vem de um ecrã da entrada de terceiros
  IF ecra_chamador = '0013'.
    MOVE 'TIPO_PALETE' TO cursorfield.
    CLEAR ecra_chamador.
  ENDIF.

** Só no caso em que vem de um ecrã da entrada da fábrica 1
  IF ecra_chamador = '0017'.
    MOVE 'TIPO_PALETE' TO cursorfield.
    CLEAR ecra_chamador.
  ENDIF.

*  F3_ACTIVO = 'X'.
  SET SCREEN '0000'. LEAVE SCREEN.

ENDMODULE.                 " EXIT9  INPUT
