*&---------------------------------------------------------------------*
*&  Include           ZWM_LISTA_STOCK_ROLOS_BOB_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: so_lgnum FOR lqua-lgnum OBLIGATORY DEFAULT 200,     " Nºdepósito
                so_charg FOR lqua-charg,                            " Número do lote
                so_lenum FOR lqua-lenum,                            " Nº unidade de depósito
                so_lgpla FOR lqua-lgpla,                            " Posição no depósito
                so_matnr FOR lqua-matnr,                            " Nº do material
                so_mtart FOR mara-mtart,                            " Tipo de material
                so_prdha FOR mara-prdha,                            " Hierarquia de produtos
                so_kunnr FOR kna1-kunnr,                            " Nº cliente
                so_werks FOR lqua-werks OBLIGATORY DEFAULT 'RENV',  " Centro
                so_cor FOR z02rpmateriais-cor,                      " Cor
                so_bismt FOR mara-bismt.                            " Nº material antigo

SELECTION-SCREEN END OF BLOCK b1.
