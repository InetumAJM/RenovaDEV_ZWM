*&---------------------------------------------------------------------*
*&  Include           ZWMFR0007
*&---------------------------------------------------------------------*



REPORT  zwmfr0007.

DATA: lt_t311   TYPE TABLE OF t311,
      lt_zwm028 TYPE TABLE OF zwm028.

DATA: ls_t311    TYPE t311,
      ls_zwm028  TYPE zwm028.

DATA: BEGIN OF gty_search_help,
        refnr TYPE t311a-refnr,
        vbeln TYPE t311a-rbnum,
       END OF gty_search_help.


PARAMETERS: p_lgnum   TYPE lgnum.
SELECT-OPTIONS s_refnr FOR gty_search_help-refnr OBLIGATORY.
SELECT-OPTIONS s_vbeln FOR gty_search_help-vbeln OBLIGATORY.

CHECK NOT s_refnr[] IS INITIAL.

SELECT * FROM t311
         INTO TABLE lt_t311
         WHERE lgnum = p_lgnum AND
               refnr IN s_refnr.

CHECK sy-subrc EQ 0.


SELECT * FROM zwm028
         INTO TABLE lt_zwm028
         WHERE lgnum = p_lgnum AND
               refnr   IN s_refnr AND
               remessa IN s_vbeln.

SORT lt_zwm028 BY ordem ASCENDING.

LOOP AT lt_t311 INTO ls_t311.
  CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
    EXPORTING
      i_lgnum = ls_t311-lgnum
      i_refnr = ls_t311-refnr
      i_step  = 1.
ENDLOOP.

LOOP AT lt_zwm028 INTO ls_zwm028.
  CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
    EXPORTING
      i_lgnum = ls_zwm028-lgnum
      i_refnr = ls_zwm028-refnr
      i_vbeln = ls_zwm028-remessa
      i_step  = 2.
ENDLOOP.
