*&---------------------------------------------------------------------*
*& Include ZWMREP0078TOP                                               *
*&                                                                     *
*&---------------------------------------------------------------------*

DATA:  text1  LIKE bdcmsgcoll-msgv1,
       text2  LIKE bdcmsgcoll-msgv2,
       resposta,
       ok_code_0001 LIKE sy-ucomm,
       screen1(4),
       cursorfield(20),
       lgnum        LIKE ltap-lgnum,
       scr_sscc     TYPE lenum,
       scr_printer  LIKE tsp03l-padest.

DATA : BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA : END OF l_user.
