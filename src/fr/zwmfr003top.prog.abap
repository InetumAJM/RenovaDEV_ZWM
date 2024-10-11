*&---------------------------------------------------------------------*
*& Include ZWMREP0021TOP                                               *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  zwmrep0021 MESSAGE-ID zwmmsg001.

TABLES : zwm026, zwm028, vekp, vepo, lagp, zwm001.

DATA:  text1  LIKE bdcmsgcoll-msgv1,
       text2  LIKE bdcmsgcoll-msgv2,
       retcode,
       resposta,
       cursorfield(20),
       lgnum LIKE ltap-lgnum,
       ok_code_0001 LIKE sy-ucomm,
       ok_code_0002 LIKE sy-ucomm,
       sscc TYPE exidv,
       sscc_ant TYPE exidv,
       mov TYPE bwlvs,
       plant TYPE werks_d,
       s_loc TYPE lgort_d,
       valor LIKE zwm001-valor,
       imp LIKE nast-ldest.

DATA : BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA : END OF l_user.

DATA : BEGIN OF return_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF return_msg.

DATA i_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.

DATA t_sscc LIKE zwm_ean128 OCCURS 0 WITH HEADER LINE.

DATA items LIKE zwm_items_hu OCCURS 0 WITH HEADER LINE.
