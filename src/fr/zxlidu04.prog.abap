*&---------------------------------------------------------------------*
*&  Include           ZXLIDU04
*&---------------------------------------------------------------------*
*"*"Local interface:
*"       IMPORTING
*"             VALUE(I_MESTYP) LIKE  EDIDC-MESTYP
*"             VALUE(I_DOCNUM) LIKE  EDIDC-DOCNUM
*"             VALUE(I_SUBRC) LIKE  SY-SUBRC
*"             VALUE(I_FLG_ERROR_ROLLBACK)
*"             VALUE(X_LMESS) LIKE  LMESS STRUCTURE  LMESS
*"             VALUE(X_RETURNCODE) LIKE  SY-SUBRC
*"             VALUE(X_CATEGORIE) LIKE  BDWFRETVAR-WF_PARAM
*"             VALUE(X_RESULT) LIKE  BDWFAP_PAR-RESULT
*"             VALUE(X_FLG_ERROR_ROLLBACK) LIKE  LTAK-TBPRI
*"       EXPORTING
*"             VALUE(X_LMESS) LIKE  LMESS STRUCTURE  LMESS
*"             VALUE(X_RETURNCODE) LIKE  SY-SUBRC
*"             VALUE(X_CATEGORIE) LIKE  BDWFRETVAR-WF_PARAM
*"             VALUE(X_RESULT) LIKE  BDWFAP_PAR-RESULT
*"             VALUE(X_FLG_ERROR_ROLLBACK) LIKE  LTAK-TBPRI
*"----------------------------------------------------------------------
DATA lt_zwmfrt004 TYPE zwmfr_ttzwmfrt004.
DATA lt_zwmfrt005 TYPE zwmfr_ttzwmfrt005.
*"----------------------------------------------------------------------

**CALL FUNCTION 'Z_WMFR_ZXLIDU04_GET_NLPLA'
**  IMPORTING
**    et_zwmfrt004 = lt_zwmfrt004[]
**    et_zwmfrt005 = lt_zwmfrt005[].

**CALL FUNCTION 'Z_WMFR_ZXLIDU04_ROLLBACK_NLPLA'
**  STARTING NEW TASK 'LOCAL'
**  EXPORTING
**    i_subrc      = i_subrc
**    it_zwmfrt004 = lt_zwmfrt004[]
**    it_zwmfrt005 = lt_zwmfrt005[]

** WCS - Armazém Automático de Torres Novas
**********************************************************************
CALL FUNCTION 'ZWM_CHECK_ERROR_IDOC_TO_WCS'
  EXPORTING
    i_docnum     = i_docnum
    i_mestyp     = i_mestyp
  CHANGING
    c_returncode = x_returncode.
