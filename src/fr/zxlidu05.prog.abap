*&---------------------------------------------------------------------*
*&  Include           ZXLIDU05
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_IDOC_CONTROL) LIKE  EDIDC STRUCTURE  EDIDC
*"             VALUE(I_FLG_CONF_SU)
*"             VALUE(I_LGNUM) LIKE  LTAK-LGNUM
*"             VALUE(I_TANUM) LIKE  LTAK-TANUM
*"             VALUE(I_LENUM) LIKE  LEIN-LENUM
*"             VALUE(X_CUSER) LIKE  E1LTCOH-QNAME
*"             VALUE(X_SOLEX) LIKE  LTAK-SOLEX
*"             VALUE(X_PERNR) LIKE  LTAK-PERNR OPTIONAL
*"             VALUE(X_STDAT) LIKE  LTAK-STDAT
*"             VALUE(X_ENDAT) LIKE  LTAK-ENDAT
*"             VALUE(X_STUZT) LIKE  LTAK-STUZT
*"             VALUE(X_ENUZT) LIKE  LTAK-ENUZT
*"             VALUE(X_ISTWM) LIKE  LTAK-ISTWM
*"             VALUE(X_KOMIM) LIKE  RL03T-KOMIM
*"             VALUE(X_AUSFB) LIKE  LTAK-AUSFB
*"             VALUE(X_EINLM) LIKE  E1LTCOH-EINLM
*"             VALUE(X_TBELI) LIKE  E1LTCOH-TBELI
*"             VALUE(X_NLPLA) LIKE  LTAP-NLPLA
*"             VALUE(X_NPPOS) LIKE  LTAP-NPPOS
*"       EXPORTING
*"             VALUE(X_CUSER) LIKE  E1LTCOH-QNAME
*"             VALUE(X_SOLEX) LIKE  LTAK-SOLEX
*"             VALUE(X_PERNR) LIKE  LTAK-PERNR
*"             VALUE(X_STDAT) LIKE  LTAK-STDAT
*"             VALUE(X_ENDAT) LIKE  LTAK-ENDAT
*"             VALUE(X_STUZT) LIKE  LTAK-STUZT
*"             VALUE(X_ENUZT) LIKE  LTAK-ENUZT
*"             VALUE(X_ISTWM) LIKE  LTAK-ISTWM
*"             VALUE(X_KOMIM) LIKE  RL03T-KOMIM
*"             VALUE(X_AUSFB) LIKE  LTAK-AUSFB
*"             VALUE(X_EINLM) LIKE  E1LTCOH-EINLM
*"             VALUE(X_TBELI) LIKE  E1LTCOH-TBELI
*"             VALUE(X_NLPLA) LIKE  LTAP-NLPLA
*"             VALUE(X_NPPOS) LIKE  LTAP-NPPOS
*"       TABLES
*"              T_IDOC_DATA STRUCTURE  EDIDD
*"              T_LTAP_CONF STRUCTURE  LTAP_CONF
*"              T_LTAP_CONF_HU STRUCTURE  LTAP_CONF_HU
*"              T_LTAP_CONF_HU_SERIAL STRUCTURE  LTAP_CONF_HU_SERIAL
*"       CHANGING
*"             REFERENCE(X_ENAME) LIKE  LTAP-ENAME OPTIONAL
*"             REFERENCE(X_QUKNZ) LIKE  RL03T-QUKNZ OPTIONAL
*"----------------------------------------------------------------------

** Armazém Automático de França
**********************************************************************
IF i_lgnum = '150'.

*& Begin of Modification by Tiago Pateiro - ROFF @ 21.01.2016 11:43:14
*/ Reset LTAK dates from space to 00000000
  CALL FUNCTION 'Z_WMFR_ZXLIDU05_RESET_DATES'
    EXPORTING
      i_stdat = x_stdat
      i_endat = x_endat
      i_stuzt = x_stuzt
      i_enuzt = x_enuzt
    IMPORTING
      e_stdat = x_stdat
      e_endat = x_endat
      e_stuzt = x_stuzt
      e_enuzt = x_enuzt.
*& End of Modification by Tiago Pateiro - ROFF @ 21.01.2016 11:43:37

*& Begin of Modification by Tiago Pateiro - ROFF @ 04.02.2016 11:45:53
**CALL FUNCTION 'Z_WMFR_ZXLIDU05_SET_NLPLA'
**  EXPORTING
**    i_lgnum = i_lgnum
**    i_tanum = i_tanum
**    i_nlpla = x_nlpla
**    i_lenum = i_lenum
**  IMPORTING
**    e_nlpla = x_nlpla
**  CHANGING
**    ct_ltap = t_ltap_conf[].
*& End of Modification by Tiago Pateiro - ROFF @ 04.02.2016 11:45:54

** Armazém Automático de Torres Novas (WCS)
**********************************************************************
ELSEIF i_lgnum = '100'.

** Dados de IDOC de confirmação de OT
  CALL FUNCTION 'ZWM_CONFIRM_IDOC_TO_WCS'
    EXPORTING
      i_lgnum       = i_lgnum
      i_tanum       = i_tanum
      i_flg_conf_su = i_flg_conf_su
    CHANGING
      c_quknz       = x_quknz
      ct_ltap_conf  = t_ltap_conf[]
      ct_idoc_data  = t_idoc_data[]
    EXCEPTIONS
      error         = 1
      OTHERS        = 2.

ENDIF.
