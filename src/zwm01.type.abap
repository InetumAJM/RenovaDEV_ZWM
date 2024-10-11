TYPE-POOL zwm01.


TYPES: zwm01_r_refnr TYPE RANGE OF lvs_refnr,
       zwm01_r_datum TYPE RANGE OF datum,
       zwm01_t_t311  TYPE TABLE OF t311.

** Dados para atribuição de TO ao operário
TYPES : BEGIN OF zwm01_l_ltap.
          INCLUDE STRUCTURE ltap.
** RL -> INS 27.04.2005
** Prioridades
          TYPES: prioridade LIKE zwm028-prioridade.
** RL <- INS 27.04.2005
TYPES : queue             LIKE ltak-queue,
        tipo              LIKE zwm010-tipo,
        upri              TYPE i,
        prioridade_queue,
*       prioridade_to(5),
        prioridade_to     TYPE p DECIMALS 0,
        prioridade_stress TYPE i,
        refnr             LIKE ltak-refnr,
        pos_remessa       LIKE zwm028-ordem,
        pos_pulmao        LIKE zwm013-posicao_pulmao,
        second_pulmao     LIKE zwm013-second_pulmao,
        pulmao            LIKE zwm013-destino,
        userass           TYPE uname.
** RL -> INS 27.04.2005
** Prioridades
TYPES: benum LIKE ltak-benum,
       lznum LIKE ltak-lznum.
** RL <- INS 27.04.2005

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 21.06.2012 10:03:11
*  Motivo: Prioridade de OT em System Guide
*--------------------------------------------------------------------*
TYPES: tapri TYPE tapri.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
TYPES : END OF zwm01_l_ltap.

TYPES: zwm01_t_l_ltap TYPE TABLE OF zwm01_l_ltap.

** Pack Transportation
***********************************************************************
TYPES: BEGIN OF zwm01_pack_in_transp.
         INCLUDE STRUCTURE zwm_s_pack_in_transp.
       TYPES:  END OF zwm01_pack_in_transp.

TYPES: zwm01_t_pack_in_transp TYPE TABLE OF zwm01_pack_in_transp.


** Range de Remessas
***********************************************************************
TYPES: zwm01_r_vbeln TYPE RANGE OF vbeln_vl.
