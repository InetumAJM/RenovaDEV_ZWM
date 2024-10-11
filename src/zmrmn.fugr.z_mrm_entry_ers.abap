FUNCTION Z_MRM_ENTRY_ERS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_NAST) LIKE  NAST STRUCTURE  NAST
*"     VALUE(I_FONAM) LIKE  TNAPR-FONAM
*"     VALUE(I_XSCREEN) LIKE  BOOLE-BOOLE
*"     VALUE(I_ARC_PARAMS) LIKE  ARC_PARAMS STRUCTURE  ARC_PARAMS
*"     VALUE(I_TOA_DARA) LIKE  TOA_DARA STRUCTURE  TOA_DARA
*"  EXCEPTIONS
*"      ERROR_MESSAGE_RECEIVED
*"----------------------------------------------------------------------

*- local data definition ----------------------------------------------
  include lmrmnld0.


  PERFORM INIT.

* archive
  ARC_PARAMS = I_ARC_PARAMS.
  TOA_DARA = I_TOA_DARA.

* fill S_OBJKY (convert CHAR30 to structure)
  PERFORM TYP_OBJKY_TEST.
  PERFORM FILL_OBJKY  USING    I_NAST
                      CHANGING S_OBJKY
                               GF_OWN_LOG_SYS.

* fill global (!) data
  GF_IDENT  =  C_IDENT_ERS.
  NAST      =  I_NAST.
  G_FONAM   =  I_FONAM.                "only used in form FORM_WRITE
  G_XSCREEN =  I_XSCREEN.              "only used in form PROTOCOL

*- data selection -----------------------------------------------------
  PERFORM SELECT_DATA_ERS  USING     S_OBJKY               "       --->
                           CHANGING  I_RSEG_X              "       <---
                                     I_EKBE1               "       <---
                                     I_EKBE2_X             "       <---
                                     I_EKKO                "       <---
                                     I_EKPO                "       <---
                                     I_BSET                "       <---
                                     I_T001W               "       <---
                                     I_MARA                "       <---
                                     I_MAKT                "       <---
                                     I_MARM                "       <---
                                     I_EINA                "       <---
                                     I_ESSR                "       <---
                                     I_PRITAB_SCD          "       <---
                                     I_SCD_TAB             "       <---
                                     I_SHP_TAB.            "       <---

*- create printout --------------------------------------------------
  PERFORM Z_CREATE_PRINTOUT_ERS  USING   I_RSEG_X            "       --->
                                       I_EKBE1             "       --->
                                       I_EKBE2_X           "       --->
                                       I_EKKO              "       --->
                                       I_EKPO              "       --->
                                       I_BSET              "       --->
                                       I_T001W             "       --->
                                       I_MARA              "       --->
                                       I_MAKT              "       --->
                                       I_MARM              "       --->
                                       I_EINA              "       --->
                                       I_ESSR              "       --->
                                       S_OBJKY             "       --->
                                       I_PRITAB_SCD        "       --->
                                       I_SCD_TAB           "       --->
                                       I_SHP_TAB.          "       --->

*- clear settings ------------------------------------------------------
  PERFORM OUTPUT_POST  USING  RBKP-BELNR                   "       --->
                              RBKP-GJAHR                   "       --->
                              RBKP-BUKRS.                  "       --->

ENDFUNCTION.
