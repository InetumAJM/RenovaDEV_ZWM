FUNCTION z_mrm_entry_ers_pdf.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_NAST) LIKE  NAST STRUCTURE  NAST
*"     VALUE(I_FONAM) LIKE  TNAPR-FONAM
*"     VALUE(I_XSCREEN) LIKE  BOOLE-BOOLE
*"     VALUE(I_ARC_PARAMS) LIKE  ARC_PARAMS STRUCTURE  ARC_PARAMS
*"     VALUE(I_TOA_DARA) LIKE  TOA_DARA STRUCTURE  TOA_DARA
*"  EXCEPTIONS
*"      ERROR_MESSAGE_RECEIVED
*"--------------------------------------------------------------------
*"  Description..: FM para impressão de auto-factura PDF
*"  Author.......: Miguel Almeida
*"  Date.........: 30.12.2014
*"----------------------------------------------------------------------
*- local data definition ----------------------------------------------
  INCLUDE lmrmnld0.


  PERFORM init.

* archive
  arc_params = i_arc_params.
  toa_dara = i_toa_dara.

* fill S_OBJKY (convert CHAR30 to structure)
  PERFORM typ_objky_test.
  PERFORM fill_objky  USING    i_nast
                      CHANGING s_objky
                               gf_own_log_sys.

* fill global (!) data
  gf_ident  =  c_ident_ers.
  nast      =  i_nast.
  g_fonam   =  i_fonam.                "only used in form FORM_WRITE
  g_xscreen =  i_xscreen.              "only used in form PROTOCOL

*- data selection -----------------------------------------------------
  PERFORM select_data_ers  USING     s_objky               "       --->
                           CHANGING  i_rseg_x              "       <---
                                     i_ekbe1               "       <---
                                     i_ekbe2_x             "       <---
                                     i_ekko                "       <---
                                     i_ekpo                "       <---
                                     i_bset                "       <---
                                     i_t001w               "       <---
                                     i_mara                "       <---
                                     i_makt                "       <---
                                     i_marm                "       <---
                                     i_eina                "       <---
                                     i_essr                "       <---
                                     i_pritab_scd          "       <---
                                     i_scd_tab             "       <---
                                     i_shp_tab.            "       <---

*- create printout --------------------------------------------------
  PERFORM z_create_printout_ers_pdf  USING
                                       i_rseg_x            "       --->
                                       i_ekbe1             "       --->
                                       i_ekbe2_x           "       --->
                                       i_ekko              "       --->
                                       i_ekpo              "       --->
                                       i_bset              "       --->
                                       i_t001w             "       --->
                                       i_mara              "       --->
                                       i_makt              "       --->
                                       i_marm              "       --->
                                       i_eina              "       --->
                                       i_essr              "       --->
                                       s_objky             "       --->
                                       i_pritab_scd        "       --->
                                       i_scd_tab           "       --->
                                       i_shp_tab.          "       --->

*- clear settings ------------------------------------------------------
  PERFORM output_post  USING  rbkp-belnr                   "       --->
                              rbkp-gjahr                   "       --->
                              rbkp-bukrs.                  "       --->

ENDFUNCTION.
