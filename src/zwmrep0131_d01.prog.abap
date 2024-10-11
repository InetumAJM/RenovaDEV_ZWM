*&---------------------------------------------------------------------*
*&  Include           ZWMREP0130_D01
*&---------------------------------------------------------------------*
***********************************************************************
** TYPE POOL'S
***********************************************************************
TYPE-POOLS: abap.

data: gv_dep_loja_online   TYPE LGTYP,
      gv_dep_ori           TYPE lgtyp,
      gv_pos_ori           TYPE lgpla,
      gv_to_mov            TYPE bwlvs.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum TYPE lgnum OBLIGATORY MEMORY ID lgn.
SELECTION-SCREEN END   OF BLOCK b1.
