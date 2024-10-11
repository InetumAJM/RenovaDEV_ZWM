*&---------------------------------------------------------------------*
*& Include ZWMREP003TOP                                      PoolMóds. *
*&                                                                     *
*&---------------------------------------------------------------------*
PROGRAM ZWMREP003 MESSAGE-ID ZWMMSG001.

INCLUDE RLMOBINC.
TABLES: ZWM001,ZWM002,ZWM003,T300T.

*dados gerais
DATA: OK_CODE_0001 LIKE SY-UCOMM,
      OK_CODE_0002 LIKE SY-UCOMM,
      CURSORFIELD(20),
      RETURN_MSG TYPE BDCMSGCOLL OCCURS 0.


*dados ecran 2
DATA: MSG(21) VALUE 'FOI ATRIBUIDO Á PORTA',
      MATRICULA(20) VALUE 'XP-99-99',
      PORTA(3) VALUE '010',
      MSG_CAMIAO(10) VALUE 'O CAMIÃO ',
      T_ARMAZEM(38).

DATA: IZWM003 TYPE ZWM003 OCCURS 0 WITH HEADER LINE.
