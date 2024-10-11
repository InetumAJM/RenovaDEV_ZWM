FUNCTION z_print_pdf_remessa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_PRINT) TYPE  FPMEDIUM
*"     REFERENCE(I_COPIES) TYPE  FPCOPIES DEFAULT 3
*"  TABLES
*"      LT_LOCAL_SAIDA TYPE  IEADRC OPTIONAL
*"      LT_LOCAL_ENTREGA TYPE  IEADRC OPTIONAL
*"  EXCEPTIONS
*"      ERROR_VBELN
*"      ERROR_PRINT
*"      ERROR_DADOS
*"----------------------------------------------------------------------
********************* Adobe PDF Form data************************
  DATA fm_name           TYPE rs38l_fnam.
  DATA fp_docparams      TYPE sfpdocparams.
  DATA fp_outputparams   TYPE sfpoutputparams.
  DATA error_string      TYPE string.
  DATA formname          TYPE fpname.
  DATA: cliente       TYPE ieadrc,
        pedido_por    TYPE ieadrc,
        local_saida   TYPE ieadrc,
        local_entrega TYPE ieadrc,
        transporte    TYPE vttk_tab,
        items         TYPE tab_lips,
        textos        TYPE wcb_tdline_tab,
        textos_abap   TYPE wcb_tdline_tab,
        logotipo      TYPE char1,
        guia          TYPE shp_ylikp_t.
  DATA: contribuinte  TYPE stceg,
        via           TYPE char20,
        indice        TYPE i.
  DATA: z_adrnr       LIKE adrc-addrnumber.
  DATA: ls_likp       LIKE likp,
        ls_kna1       LIKE kna1,
        ls_adrc       TYPE adrc.
  DATA: ztransnum     LIKE vttp-tknum.
  DATA:   codigoat TYPE  char50,
          cert_id     TYPE  sipt_cert_id,
          cert_id_ass TYPE  sipt_print_char  .
  DATA: qtd_bobinas TYPE i,
        peso_total  TYPE i.
  DATA: ls_items       LIKE lips.
  DATA: lt_lenum_data  TYPE z02rp_tt_general_data.
  DATA: ls_lenum_data TYPE z02rp_s_general_data.
  DATA: lv_lenum   LIKE lein-lenum.
**********************************************************************
*********Tabelas para o ADOBE*********************************
  DATA: lt_guia   TYPE TABLE OF likp.
  DATA: lt_items  TYPE TABLE OF lips.
**********************************************************************

* Obter dados da remessa
*********************************************************************

  SELECT * FROM likp INTO  TABLE guia
   WHERE vbeln = i_vbeln.

  SELECT * FROM lips INTO TABLE lt_items
    FOR ALL ENTRIES IN guia
    WHERE vbeln = guia-vbeln.

  CLEAR qtd_bobinas.
  LOOP AT lt_items INTO ls_items.

    REFRESH: lt_lenum_data.
    lv_lenum = ls_items-kdmat.

    CALL FUNCTION 'Z02RP_RETRIEVE_DATA_FROM_UD'
      EXPORTING
        iv_lenum           = lv_lenum
        iv_get_volume      = 'X'
      IMPORTING
        et_lenum_data      = lt_lenum_data
      EXCEPTIONS
        material_not_found = 1
        OTHERS             = 2.

    CLEAR ls_lenum_data.
    READ TABLE lt_lenum_data INTO ls_lenum_data INDEX 1.

    IF ls_lenum_data-id_conjunto IS NOT INITIAL.

      READ TABLE items INTO ls_items WITH KEY kdmat = ls_lenum_data-id_conjunto.
      IF sy-subrc <> 0.
        ls_items-kdmat = ls_lenum_data-id_conjunto.
        ls_items-posar = 'X'.
        CLEAR: ls_items-lfimg, ls_items-meins.
        APPEND ls_items TO items .
      ENDIF.

      ls_items-kdmat = lv_lenum.
      ls_items-posar = ' '.
      qtd_bobinas = qtd_bobinas + 1.
      APPEND ls_items TO items .
    ELSE.
      ls_items-posar = 'X'.
      qtd_bobinas = qtd_bobinas + 1.
      APPEND ls_items TO items .
    ENDIF.

  ENDLOOP.

** Local saida
  CLEAR: ls_adrc ,ls_kna1.
  READ TABLE guia INTO ls_likp INDEX 1.
  IF ls_likp IS NOT INITIAL.
    SELECT SINGLE * FROM kna1 INTO ls_kna1
     WHERE kunnr = ls_likp-kunag.
  ENDIF.


  IF ls_kna1 IS NOT INITIAL.
    SELECT SINGLE * FROM adrc INTO ls_adrc
      WHERE addrnumber = ls_kna1-adrnr.
  ENDIF.

  IF ls_adrc IS NOT INITIAL.
    PERFORM formata_endereco USING ls_adrc.
    MOVE ls_adrc TO lt_local_saida.

    APPEND lt_local_saida.
    local_saida[] = lt_local_saida[].
  ENDIF.

** local entrega
  CLEAR:ls_adrc ,ls_kna1 .
  IF ls_likp IS NOT INITIAL.
    SELECT SINGLE * FROM kna1 INTO ls_kna1
     WHERE kunnr = ls_likp-kunnr.
  ENDIF.

  IF ls_kna1 IS NOT INITIAL.
    SELECT SINGLE * FROM adrc INTO ls_adrc
      WHERE addrnumber = ls_kna1-adrnr.
  ENDIF.

  IF ls_adrc IS NOT INITIAL.

    PERFORM formata_endereco USING ls_adrc.
    MOVE ls_adrc TO lt_local_entrega.

    APPEND lt_local_entrega.
    local_entrega[] = lt_local_entrega[].
  ENDIF.

** transporte

  SELECT tknum INTO ztransnum FROM vttp WHERE vbeln = i_vbeln.
  ENDSELECT.

  SELECT * FROM vttk APPENDING CORRESPONDING FIELDS OF TABLE
transporte WHERE tknum = ztransnum.

  CLEAR: cert_id_ass, cert_id, codigoat.
**Assinatura, certificacao e codigo AT
*** certificação das remessas
  CALL FUNCTION 'Z_GET_CERT_ID_REMESSA'
    EXPORTING
      zvbeln      = i_vbeln
      z_certifica = ' '
    IMPORTING
      cert_id_ass = cert_id_ass
      cert_id     = cert_id.

*** numero da Autoridade Tributaria
  CALL FUNCTION 'Z_GET_AT_ID_REMESSA'
    EXPORTING
      zvbeln  = i_vbeln
    IMPORTING
      cert_id = codigoat.

  peso_total = ls_likp-btgew.
** Imprime adobe form
**********************************************************************
  formname = 'ZSD_PDF_SSCC_REMESSA'.

  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = formname
    IMPORTING
      e_funcname = fm_name.

  fp_outputparams-nodialog = 'X'.
*  fp_outputparams-preview  = 'X'.

  fp_outputparams-reqimm = 'X'. ""saida imediata.
  fp_outputparams-copies = '1'.
*  fp_outputparams-reqdel = 'X'.
  fp_outputparams-reqnew = 'X'.
  fp_outputparams-device = 'PRINTER'.
  fp_outputparams-dest   = i_print.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = fp_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN OTHERS.
    ENDCASE.                           " CASE sy-subrc
  ENDIF.

  fp_docparams-langu   = 'P'.
  fp_docparams-country = 'PT'.

  CALL FUNCTION fm_name
    EXPORTING
      /1bcdwb/docparams = fp_docparams
      via               = via
      textos_abap       = textos_abap
      contribuinte      = contribuinte
*      SYINDEX           = sy-index
      cliente           = cliente
      pedido_por        = pedido_por
      local_saida       = local_saida
      local_entrega     = local_entrega
      guia              = guia
      transporte        = transporte
      textos            = textos
      items             = items
      logotipo          =  'X'
      codigo_at         = codigoat
      cert_id           = cert_id
      cert_id_ass       = cert_id_ass
      qtd_bobinas       = qtd_bobinas
      peso_total        = peso_total
     EXCEPTIONS
      usage_error       = 1
      system_error      = 2
      internal_error    = 3.

*****************************************************
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
********************************************************

ENDFUNCTION.
