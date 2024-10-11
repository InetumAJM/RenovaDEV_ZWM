*&---------------------------------------------------------------------*
*&  Include           ZWMREP0134_D01
*&---------------------------------------------------------------------*

TYPE-POOLS: abap.

TYPES: BEGIN OF gty_data.
         INCLUDE STRUCTURE zwm067.
       TYPES:
                check TYPE flag,
                maktx TYPE maktx,
              END OF gty_data.

TYPES: gty_t_data TYPE TABLE OF gty_data.

TYPES: BEGIN OF gty_scr0001,
         okcode TYPE syucomm,
       END OF gty_scr0001.

DATA: gt_data TYPE TABLE OF gty_data.

DATA: gs_data TYPE gty_data.

DATA: scr0001 TYPE gty_scr0001.

DATA: gv_lgnum    TYPE lgnum VALUE '100',
      gv_werks    TYPE werks_d VALUE 'RENV',
      gv_bwlvs_nt TYPE bwlvs VALUE '934'.
