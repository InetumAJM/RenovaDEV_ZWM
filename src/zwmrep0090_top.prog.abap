*&---------------------------------------------------------------------*
*&  Include           ZWMREP0090_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_tsel,
         refnr  TYPE t311-refnr,
         refnt  TYPE t311-refnt,
         vstel  TYPE likp-vstel,
         sdabw  TYPE vttk-sdabw,
         tdlnr  TYPE vttk-tdlnr,
         vkorg  TYPE likp-vkorg,
         datum  TYPE t311-datum,
         dtdis  TYPE vttk-dtdis,
         dalbg  TYPE vttk-dalbg,
         dplbg  TYPE vttk-dplbg,
         tknum  TYPE vttk-tknum,
         matnr  TYPE lips-matnr,
         trfzn  TYPE tvfptz-trfzn,
         wbstk  TYPE vbuk-wbstk,
         etdat  TYPE vbep-edatu,
         stprop TYPE dd07t-ddtext,
       END OF ty_tsel.
DATA gs_tsel TYPE ty_tsel.
DATA: gv_cargas TYPE int4.

DATA: ok_code TYPE syucomm.
*DATA: gv_col TYPE i.

*... §4 Definition is later
*CLASS lcl_alv DEFINITION DEFERRED.
CLASS lcl_handle_events DEFINITION DEFERRED.
DATA: gr_events TYPE REF TO lcl_handle_events.
FIELD-SYMBOLS <gft_alv2> TYPE STANDARD TABLE.

DATA: p_faltat VALUE abap_true.

* -----------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
*PARAMETERS:     p_lgnum LIKE t320-lgnum MEMORY ID lgn. " DEFAULT '100' .
PARAMETERS:     p_lgnum LIKE t320-lgnum DEFAULT '100' .
SELECT-OPTIONS: s_refnr FOR gs_tsel-refnr,
                s_refnt FOR gs_tsel-refnt,
                s_vstel FOR gs_tsel-vstel,
                s_sdabw FOR gs_tsel-sdabw,
                s_tdlnr FOR gs_tsel-tdlnr,
                s_vkorg FOR gs_tsel-vkorg,
                s_datum FOR gs_tsel-datum OBLIGATORY,
                s_dtdis FOR gs_tsel-dtdis,
                s_dalbg FOR gs_tsel-dalbg,
                s_dplbg FOR gs_tsel-dplbg,
                s_matnr FOR gs_tsel-matnr,
                s_trfzn FOR gs_tsel-trfzn,
                s_wbstk FOR gs_tsel-wbstk NO INTERVALS
                                          NO-EXTENSION,
                s_etdat FOR gs_tsel-etdat,
                s_dtext FOR gs_tsel-stprop MATCHCODE OBJECT zwm_status_proposta
                                           NO INTERVALS,
                s_tknum FOR gs_tsel-tknum
                                          .
PARAMETERS p_falta AS CHECKBOX.

SELECTION-SCREEN SKIP.
PARAMETERS p_explvl TYPE i OBLIGATORY DEFAULT 4.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
*PARAMETERS: p_faltat AS CHECKBOX.
PARAMETERS p_varia LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b1.
