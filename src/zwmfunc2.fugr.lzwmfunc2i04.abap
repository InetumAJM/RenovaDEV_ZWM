*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC2I04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0004 INPUT.

  case ok_code4.
    when 'NEXT'.
      retcode = 'O'.
      SET SCREEN '0000'. LEAVE SCREEN.
    when 'CANC'.
      retcode = 'C'.
      SET SCREEN '0000'. LEAVE SCREEN.
    when others.

  endcase.

clear : msg1,msg2,msg3,msg4,msg5.

ENDMODULE.                 " USER_COMMAND_0004  INPUT
