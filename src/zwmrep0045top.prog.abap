*&---------------------------------------------------------------------*
*& Include ZWMREP0045TOP                                               *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  zwmrep0045 MESSAGE-ID zwmmsg001.

TYPE-POOLS: abap.

TABLES : zwm014, zwm013, zwm026, vekp, vepo, lqua, makt, likp.

DATA:  text1  LIKE bdcmsgcoll-msgv1,
       text2  LIKE bdcmsgcoll-msgv2,
       resposta,
       posicao(14) TYPE c,
       grupo LIKE t311-refnr,
       posicao_pulmao LIKE zwm013-posicao_pulmao,
       cursorfield(20),
       lgnum LIKE ltap-lgnum,
       ok_code_0001 LIKE sy-ucomm,
       ok_code_0002 LIKE sy-ucomm,
       sscc TYPE lenum,
       l_quant(4),
       l_pulmao,
       l_index LIKE sy-tabix,
       mat1  LIKE mara-matnr,
       qtd1(4),
       umb1(3),
       desc1 LIKE makt-maktx,
       mat2  LIKE mara-matnr,
       qtd2(4),
       umb2(3),
       desc2 LIKE makt-maktx,
       mat3  LIKE mara-matnr,
       qtd3(4),
       umb3(3),
       desc3 LIKE makt-maktx,
       mat4  LIKE mara-matnr,
       qtd4(4),
       umb4(3),
       desc4 LIKE makt-maktx,
       mat5  LIKE mara-matnr,
       qtd5(4),
       umb5(3),
       desc5 LIKE makt-maktx,
       mat6  LIKE mara-matnr,
       qtd6(4),
       umb6(3),
       desc6 LIKE makt-maktx,
       mat7  LIKE mara-matnr,
       qtd7(4),
       umb7(3),
       desc7 LIKE makt-maktx,
       kober TYPE kober.

DATA: BEGIN OF itab_dados OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
        vemng LIKE vepo-vemng,
        vemeh LIKE vepo-vemeh,
        vbeln LIKE vepo-vbeln,
      END OF itab_dados.

DATA : BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA : END OF l_user.

DATA : BEGIN OF return_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF return_msg.
