FUNCTION zwm_create_idoc_to_cancel.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM
*"     VALUE(I_TANUM) TYPE  TANUM
*"  TABLES
*"      T_LTAP_VB STRUCTURE  LTAP_VB
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CONSTANTS: lc_object    TYPE balhdr-object    VALUE 'ZWCS'.
  CONSTANTS: lc_subobject TYPE balhdr-subobject VALUE 'ZWCS_13'.

  DATA: lv_extnumber  TYPE balnrext.

  DATA: lv_valor      TYPE char20.
  DATA: lv_wait       TYPE i.
  DATA: lv_zsyst      TYPE t327a-zsyst.
  DATA: lv_refnr      TYPE lvs_refnr.
  DATA: lv_docnum     TYPE edi_docnum.

  DATA: ls_ltak       TYPE ltak.
  DATA: ls_ltap       TYPE ltap.
  DATA: ls_t327a      TYPE t327a.
  DATA: lt_ltap       TYPE ltap       OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap_vb    TYPE ltap_vb    OCCURS 0 WITH HEADER LINE.
  DATA: lt_msg_log    TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

** Validar OT
**********************************************************************
  SELECT SINGLE *
   FROM ltak INTO ls_ltak
   WHERE lgnum = i_lgnum AND
         tanum = i_tanum.

  SELECT SINGLE *
    FROM ltap INTO ls_ltap
    WHERE lgnum =  i_lgnum AND
           tanum = i_tanum.

  " Validar se foi enviado IDOC para WCS
  SELECT SINGLE *
    FROM t327a INTO ls_t327a
    WHERE lgnum = i_lgnum
    AND   vltyp = ls_ltap-vltyp
    AND   nltyp = ls_ltap-nltyp
    AND   bwlvs = ls_ltak-bwlvs.

  CHECK ls_t327a-kzina = 'X'.

*  IF ls_ltak-refnr IS NOT INITIAL.
*    SELECT SINGLE *
*      FROM zwm078 INTO ls_zwm078
*      WHERE lgnum = i_lgnum
*      AND   tanum = i_tanum.
*  ENDIF.
*
*  CHECK ls_zwm078 IS NOT INITIAL.

  CALL FUNCTION 'ZWM_CHECK_IDOC_TO'
    EXPORTING
      i_lgnum  = ls_ltak-lgnum
      i_tanum  = ls_ltak-tanum
    IMPORTING
      e_docnum = lv_docnum.

** Envio de IDOC de estorno
**********************************************************************
  CHECK lv_docnum IS NOT INITIAL.

  lv_zsyst = 'WMSRPTA100'.

  CALL FUNCTION 'L_IDOC_CREATE_WMCAID01'
    EXPORTING
      i_zsyst = lv_zsyst
      i_ltak  = ls_ltak
      i_varia = ''
    TABLES
      t_ltap  = t_ltap_vb.

  " Gerado IDOC de estorno da OT & de saida!
  CLEAR lt_msg_log.
  lt_msg_log-msgid  = 'ZWMMSG001'.
  lt_msg_log-msgtyp = 'S'.
  lt_msg_log-msgnr  = '368'.
  lt_msg_log-msgv1  = ls_ltak-tanum.
  APPEND lt_msg_log.

  lv_extnumber = ls_ltak-refnr.

** Log
  CALL FUNCTION 'ZWM_MSG_LOG_WCS'
    EXPORTING
      i_object    = lc_object
      i_subobject = lc_subobject
      i_extnumber = lv_extnumber
      i_state     = 'A'
    TABLES
      t_log2      = lt_msg_log
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

ENDFUNCTION.
