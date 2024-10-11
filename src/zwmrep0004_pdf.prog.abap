*&---------------------------------------------------------------------*
*& Report  ZWMREP0004                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0004_pdf.

TABLES : zwm003_aux, zwm005, zwm006_aux, vttk.

DATA: lvs_itcpo     LIKE itcpo,
      it_trans      LIKE zwm006_aux,
      name1         LIKE lfa1-NAME1,
      tipo_camiao   type char4,
      desc_camiao   TYPE CHAR50,
      tipo_Operacao TYPE char20,
      g_dat         LIKE sy-datum,

      tpoper(10).

PARAMETERS: p_talao LIKE zwm003-num_entrada,
            p_tknum like vttk-tknum.


"SELECT-OPTIONS: s_tknum FOR it_trans-n_transporte NO-EXTENSION NO INTERVALS.

START-OF-SELECTION.



*  IF p_talao IS INITIAL AND
*     s_tknum[] IS INITIAL.
*    EXIT.
*  ELSE.
  PERFORM selecao_dados.
  "PERFORM get_info USING it_trans.
  IF tpoper = 'CARGA' .
    PERFORM imprime_talao.
  else  .
    SUBMIT zwmrep0004 WITH p_talao = p_talao AND RETURN.
  ENDIF.
  " ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  IMPRIME_TALAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_talao .
  DATA l_formname        TYPE fpname VALUE 'ZWMN_TALAO_CARGA_DESCARGA'.
  DATA fp_docparams      TYPE sfpdocparams.
  DATA fp_outputparams   TYPE sfpoutputparams.
  DATA fm_name  TYPE rs38l_fnam.
  data: zuserdata type USR01.


  CALL FUNCTION 'GET_PRINT_PARAM'
    EXPORTING
      I_BNAME = sy-uname
    IMPORTING
      E_USR01 = zuserdata.

  IF zuserdata is not INITIAL.
    fp_outputparams-dest = zuserdata-SPLD.
  ELSE.
    fp_outputparams-dest = 'LOCA'.
  ENDIF.

  FP_OUTPUTPARAMS-NODIALOG = ''.
  FP_OUTPUTPARAMS-PREVIEW =  ''. " temp
  FP_OUTPUTPARAMS-BUMODE = 'M'.
  fp_outputparams-arcmode = 1.
  "fp_outputparams-dest = 'PR13'.
  fp_outputparams-reqimm = 'X'.
  fp_outputparams-reqnew = 'X'.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      IE_OUTPUTPARAMS = FP_OUTPUTPARAMS
    EXCEPTIONS
      CANCEL          = 1
      USAGE_ERROR     = 2
      SYSTEM_ERROR    = 3
      INTERNAL_ERROR  = 4
      OTHERS          = 5.
  IF SY-SUBRC <> 0.
    CASE SY-SUBRC.
      WHEN OTHERS.

    ENDCASE.                           " CASE sy-subrc
    "RETCODE = SY-SUBRC.
    "PERFORM PROTOCOL_UPDATE.
  ENDIF.

  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = l_formname
    IMPORTING
      e_funcname = fm_name.



  FP_DOCPARAMS-LANGU   = 'PT'.
  FP_DOCPARAMS-COUNTRY = 'PT'.
  "FP_DOCPARAMS-DARATAB = lt_toa_dara.
  fp_docparams-langu   = 'PT'.
  fp_docparams-country = 'PT'.
  CALL FUNCTION fm_name
    EXPORTING
      /1bcdwb/docparams = fp_docparams
      ARMAZEM           = it_trans-ARMAZEM
      NUM_ENTRADA       = IT_TRANS-NUM_ENTRADA
      N_TRANSPORTE      = IT_TRANS-N_TRANSPORTE
      HORA_REG          = IT_TRANS-HORA_REG
      DATA_REG          = IT_TRANS-DATA_REG
      MATRICULA         = IT_TRANS-MATRICULA
      HORA_SAIDA        = IT_TRANS-HORA_SAIDA
      DATA_SAIDA        = IT_TRANS-DATA_SAIDA
      PORTA             = IT_TRANS-PORTA
      FINALIZADA        = IT_TRANS-FINALIZADA
      OBSERVACOES       = IT_TRANS-OBSERVACOES
      OBSERVACOES2      = IT_TRANS-OBSERVACOES2
      PESINI            = IT_TRANS-PESINI
      PESFIM            = IT_TRANS-PESFIM
      PESTG             = IT_TRANS-PESTG
      DIFKG             = IT_TRANS-DIFKG
      DIFP              = IT_TRANS-DIFP
      "ORD_COMPRA     = IT_TRANS-
      TIPO_CAMIAO       = TIPO_CAMIAO
      DESC_CAMIAO       = DESC_CAMIAO
      "TRANSPORTADOR  = it
      NAME1             = NAME1
      "LOCAL_ARMAZEM  = IT_TRANS-
      tipo_Operacao     = tipo_Operacao
    EXCEPTIONS
      usage_error       = 1
      system_error      = 2
      internal_error    = 3.

  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN OTHERS.
    ENDCASE.                           " CASE sy-subrc
  ENDIF.


ENDFORM.                    " IMPRIME_TALAO
*&---------------------------------------------------------------------*
*&      Form  selecao_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selecao_dados .

  CLEAR : it_trans, g_dat, tpoper.

  if p_talao is not initial.
    select single * INTO CORRESPONDING FIELDS OF it_trans from zwm005 where num_entrada = p_talao.
    if sy-subrc = 0.
      tpoper = 'DESCARGA'.
    else.
      select single * INTO CORRESPONDING FIELDS OF it_trans from zwm006_aux where num_entrada = p_talao.
      if sy-subrc = 0.
        tpoper = 'CARGA'.
      endif.
    endif.
  endif.

  if p_tknum is not initial.
    select single * INTO CORRESPONDING FIELDS OF it_trans from zwm006_aux where n_transporte = p_tknum.
    if sy-subrc = 0.
      tpoper = 'CARGA'.
    endif.
  endif.

  "dados extra

  CLEAR tipo_camiao.

  SELECT SINGLE sdabw tdlnr INTO (tipo_camiao, vttk-tdlnr)
                            FROM  vttk
         WHERE  tknum  = it_trans-n_transporte.

  IF tipo_camiao is NOT INITIAL.
    SELECT SINGLE BEZEI INTO DESC_CAMIAO
      FROM TVSAKT
      WHERE spras = 'P'
      and SDABW = tipo_camiao.
  ENDIF.

  SELECT SINGLE name1 INTO name1 FROM  lfa1
       WHERE  lifnr  = vttk-tdlnr.


  tipo_Operacao = tpoper.

ENDFORM.                    " selecao_dados
*&---------------------------------------------------------------------*
*&      Form  get_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TRANS  text
*----------------------------------------------------------------------*
FORM get_info  USING  it_trans STRUCTURE it_trans.

*  CLEAR it_trans-DATA_REG.
*
*  it_trans-DATA_REG = zwm006_aux-data_reg.
*  it_trans-HORA_SAIDA = zwm006_aux-HORA_REG.
*
*  CLEAR tipo_camiao.
*
*  SELECT SINGLE sdabw tdlnr INTO (tipo_camiao, vttk-tdlnr)
*                            FROM  vttk
*         WHERE  tknum  = it_trans-n_transporte.
*
*  IF tipo_camiao is NOT INITIAL.
*    SELECT SINGLE BEZEI INTO DESC_CAMIAO
*      FROM TVSAKT
*      WHERE spras = 'P'
*      and SDABW = tipo_camiao.
*  ENDIF.
*
*  SELECT SINGLE name1 INTO name1 FROM  lfa1
*       WHERE  lifnr  = vttk-tdlnr.


ENDFORM.                    " get_info
