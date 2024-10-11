*&---------------------------------------------------------------------*
*&  Include           ZWMREP0071_TOP                                   *
*&---------------------------------------------------------------------*
PROGRAM  ZWMREP0071 MESSAGE-ID zwmmsg001.

INCLUDE: rlmobinc.

TABLES : zwm038,
         lqua,
         link,
         linp,
         afko,
         afvc,
         afpo,
         afru,
         affw,
         mch1.

DATA : ok_code_0001 LIKE sy-ucomm,
       posicao(20),
       l_pos LIKE bdcmsgcoll-msgv1,
       qtd_existente LIKE lagp-anzqu,
       sscc LIKE lein-lenum,
       ordem_producao LIKE afko-aufnr,
       ean LIKE mara-ean11,
       qtd LIKE lagp-anzqu,
       uni LIKE afpo-meins,
       quantidade(18) type c,
       maktx(20),
       cursorfield(30),
       text1(100),
       text2(100),
       ret_code,
       werks LIKE mseg-werks,
       lgort LIKE mseg-lgort,
       whs TYPE lgnum,
       tanum LIKE mseg-tanum,
       mat_doc LIKE mkpf-mblnr,
       mat_yea LIKE mkpf-mjahr,
       su LIKE lein-lenum,
       unidade_pre_contagem(30),
       hukey LIKE bapihukey-hu_exid.

DATA: itab_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF return_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF return_msg.

DATA BEGIN OF items OCCURS 0.
        INCLUDE STRUCTURE zwm_items_hu_inv.
DATA END OF items.

DATA : BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA : END OF l_user.

DATA : BEGIN OF mat_st OCCURS 0.
        INCLUDE STRUCTURE zwm_entry_material.
DATA : END OF mat_st.

DATA : BEGIN OF l_bapibatchatt OCCURS 0.
        INCLUDE STRUCTURE bapibatchatt.
DATA : END OF l_bapibatchatt.

DATA : BEGIN OF l_bapibatchattx OCCURS 0.
        INCLUDE STRUCTURE bapibatchattx.
DATA : END OF l_bapibatchattx.

DATA : BEGIN OF return OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
DATA : END OF return.
