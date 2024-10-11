*&---------------------------------------------------------------------*
*&  Include           ZWM_PALLET_MOV_JOB_TOP
*&---------------------------------------------------------------------*

DATA: gv_nodata TYPE flag,
      gt_mkpf TYPE ty_t_mkpf,
      gt_mseg TYPE ty_t_mseg,
      gt_docu TYPE TABLE OF ZWM_DOCU_YEAR,
      gt_zwm001 TYPE TABLE OF zwm001,
      gv_update TYPE flag,
      gr_saida TYPE RANGE OF bwart,
      gr_estorno TYPE RANGE OF bwart.
