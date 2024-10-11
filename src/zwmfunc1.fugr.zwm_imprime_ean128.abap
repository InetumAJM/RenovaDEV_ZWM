FUNCTION zwm_imprime_ean128.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(PRINTER) LIKE  NAST-LDEST
*"     VALUE(LBLTYPE) TYPE  EAN128-LBLTYPE DEFAULT 'L0'
*"     REFERENCE(COPIES) TYPE  ITCPO-TDCOPIES DEFAULT 3
*"     REFERENCE(GET_ZPL) TYPE  FLAG DEFAULT ''
*"  TABLES
*"      SSCC STRUCTURE  ZWM_EAN128
*"      ZPL STRUCTURE  TLINE OPTIONAL
*"  EXCEPTIONS
*"      IMPRESSORA_NAO_EXISTE
*"      SSCC_NAO_EXISTE
*"      SSCC_COM_IMPRESSAO_GRUPO
*"----------------------------------------------------------------------
** alterado Paulo Sousa 2010.04.08
** adicionados parametros lbltype e copies
** lbltype  L0 - formato existente (opcional, default L0)
**          L1 - ignora DUN14 de palete (AI02 + AI37 em vez de AI01)
** copies   Numero de copias de etiqueta a imprimir. Default 3. Opcional
**
************
** 2011.12.16 - Adicionada a funcao ZWM_GET_EAN128_LABEL_CONTENT que devolve a maioria dos
* valores da etiqueta, sem fazer impressão. Objectivo: usar aquela funcao para formularios PDF

** 2016.01.20 - Paulo Sousa
**  Adicionado parametros get_zpl e zpl. (Permite redericionar codigo ZPL para buffer sem imprimir)

  DATA: tnapr        LIKE tnapr,
        x_nast       TYPE nast,
        lf_progname  TYPE syrepid,
        noheaderlogo TYPE c,
        l_profile    TYPE ean128_profile,
        l_devicetype TYPE t313k-devicetype,
        ok           TYPE i,
        linhas       TYPE i,
        lv_lgnum     TYPE lgnum.

  DATA: wa_values            TYPE ean128,
        cs_ean128_data-eanbc TYPE barcode_plus_errors_t,
        cs_ean128_data-eanrd TYPE barcode_txt_t,
        itab_vepo            LIKE vepo OCCURS 0 WITH HEADER LINE.

  DATA: we_numguia    LIKE vbfa-vbeln,
        delivery_date LIKE likp-lfdat.

  DATA element(20) VALUE 'EAN128'.

  FIELD-SYMBOLS:
    <ls_bc>            TYPE barcode_plus_txt,
    <ls_bc_plus_error> TYPE barcode_plus_errors,
    <ls_bc_txt>        TYPE barcode_txt.

  DATA: del_item  TYPE posnr_vl,
        isallotie TYPE c,
        zaugru    TYPE augru,
        znumenc   TYPE vbeln.

  DATA: BEGIN OF it_boxlabels OCCURS 1.
          INCLUDE STRUCTURE zwm_print_label_details.
        DATA: END OF it_boxlabels.

  DATA: lf_type TYPE c,
        zean128 TYPE ean128.

  DATA: zdevice(10) TYPE c.

** Armazem
***********************************************************************
  IF lgnum IS INITIAL.
    PERFORM get_whs CHANGING lv_lgnum.
  ELSE.
    lv_lgnum = lgnum.
  ENDIF.


  zdevice = 'PRINTER'.
  IF get_zpl = 'X'.
    zdevice = 'OTF_MEM'.
  ENDIF.
  CHECK NOT sscc[] IS INITIAL.

** Impressora
  CLEAR: tsp03.
  SELECT SINGLE * FROM tsp03 WHERE padest = printer.
  IF sy-subrc NE 0.
    RAISE impressora_nao_existe.
    EXIT.
  ENDIF.
  x_nast-ldest = printer.
** Programa
  lf_progname = 'RLE_EAN128_LABEL'.
** Estrutura com dados para a impressora
  CLEAR itcpo.

* alterado Paulo Sousa 2010.04.27
*  itcpo-tdcopies = copies.
  itcpo-tddest = x_nast-ldest.
  itcpo-tddataset = 'LABEL'.
  itcpo-tdsuffix1 = x_nast-ldest.
  itcpo-tdnoprev = 'X'.
  itcpo-tdimmed = 'X'.
  itcpo-tddelete = ' '.
  itcpo-tdprogram = sy-repid.
  itcpo-tdteleland = 'PT'.
  itcpo-tdtitle = 'SSCC de entrada'.
  itcpo-tdnewid = 'X'.

** Main LOOP

  LOOP AT sscc.
    itcpo-tdcopies = copies.
    l_devicetype = 'ZLB_ZEB'.
    CLEAR noheaderlogo.

**  obter dados para cada SSCC
    CALL FUNCTION 'ZWM_GET_EAN128_LABEL_CONTENT'
      EXPORTING
        lgnum                    = lv_lgnum
        sscc                     = sscc-sscc
        lbltype                  = lbltype
        printer                  = itcpo-tddest
      IMPORTING
        ean128out                = ean128
        l_profile                = l_profile
        form_name                = tnapr-fonam
        noheaderlogo             = noheaderlogo
        cs_ean128_data_eanbc     = cs_ean128_data-eanbc
        cs_ean128_data_eanrd     = cs_ean128_data-eanrd
        we_numguia               = we_numguia
      EXCEPTIONS
        impressora_nao_existe    = 1
        sscc_nao_existe          = 2
        sscc_com_impressao_grupo = 3
        OTHERS                   = 4.

    IF sy-subrc <> 0.
      " 2023.03.15 - Se erro, não imprime nada
      exit.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

** Verificar se se trata de ALLOTIE
    IF we_numguia IS NOT INITIAL.
      CLEAR: isallotie, del_item.
      REFRESH: it_boxlabels.
      CALL FUNCTION 'Z_GET_IS_ALLOTIE'
        EXPORTING
          delivery      = we_numguia
          delivery_item = del_item
        CHANGING
          allotie       = isallotie
          augru         = zaugru
          numenc        = znumenc.
    ENDIF.

** Martelada System-U
    DATA: zkun LIKE vbak-kunnr.
    SELECT SINGLE kunnr INTO zkun FROM vbak WHERE vbeln = znumenc.
    IF zkun = '0000601120'.
      IF tnapr-fonam = 'ZWM_EAN128_SSCC2'.
        tnapr-fonam = 'ZWM_SYSTEM_U'.
      ENDIF.
    ENDIF.

** executar impressao em SapScript de label de palete
** martelada para GET_ZPL
*
* 2023.09.11 -  Alterado. Armazem automatico necessita
* reimprimit todo tipo de etiquetas, nao apenas homegenia standard. Manter efacec por enquanto
*    IF get_zpl = 'X'.
**    IF sy-uname = 'EFACEC'.
*      data: dpi type zdpis.
*      clear dpi.
*      if tnapr-fonam <> 'ZWM_EAN128_SSCC' AND tnapr-fonam <> 'ZWM_EN_SSCC_300'.
*        " Temporario para identificar possiveis probemas...
*        data: html type string.
*
*        html = sscc-sscc && '->' && tnapr-fonam.
*        CALL FUNCTION 'ZSEND_QUICK_MAIL'
*          EXPORTING
*            ASSUNTO            = 'ERRO etiqueta'
*            CORPO_HTML         = html
*            MAILS_DESTINATARIO = 'paulo.sousa@renova.pt'
*            LOGIN_EMISSOR      = sy-uname.
*
*        tnapr-fonam = 'ZWM_EAN128_SSCC'.
*      endif.
*      select single dpi into dpi from Z02RPIMP_ETIQUET where pdest = printer.
*      if dpi = '300'.
*        tnapr-fonam = 'ZWM_EN_SSCC_300'.
*      endif.
*      "clear noheaderlogo.
*    ENDIF.

    CALL FUNCTION 'OPEN_FORM'
      EXPORTING
        device   = zdevice  " PRINTER ou OTF_MEM
        dialog   = ' '
        form     = tnapr-fonam
        language = sy-langu
        OPTIONS  = itcpo.

    CALL FUNCTION 'START_FORM'
      EXPORTING
        form        = tnapr-fonam
        language    = sy-langu
        program     = sy-repid
      EXCEPTIONS
        form        = 1
        format      = 2
        unended     = 3
        unopened    = 4
        unused      = 5
        spool_error = 6
        codepage    = 7
        OTHERS      = 8.
    IF sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF  noheaderlogo = 'X'.
      element = 'EAN128_SEM_LOGO'.
    ENDIF.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = element
        window  = 'MAIN'
      EXCEPTIONS
        OTHERS  = 0.

    CALL FUNCTION 'END_FORM'
      EXCEPTIONS
        unopened                 = 1
        bad_pageformat_for_print = 2
        spool_error              = 3
        codepage                 = 4
        OTHERS                   = 5.

    CALL FUNCTION 'CLOSE_FORM'.

    IF get_zpl = 'X'.
      CALL FUNCTION 'CONVERT_OTF_MEMORY'
        EXPORTING
          format                = 'ASCII'
          max_linewidth         = 132
          ascii_bidi            = ' '
          "IMPORTING
          "BIN_FILESIZE                =
        TABLES
          lines                 = zpl
        EXCEPTIONS
          memory_empty          = 1
          err_max_linewidth     = 2
          err_format            = 3
          err_conv_not_possible = 4
          OTHERS                = 5.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      DELETE zpl WHERE tdline = space.
    ENDIF.
** verificar se se trata de um pedido ALLOTIE. Neste caso poderão existir paletes multi-destino para as
** quais se deve imprimir etiquetas por caixa

    IF isallotie = 'X'.
**      Obter tabela de dados para impressao de etiquetas de palete
      CALL FUNCTION 'ZWM_GET_BOX_LABEL_CONTENT'
        EXPORTING
          sscc   = sscc-sscc
        TABLES
          labels = it_boxlabels.

      DESCRIBE TABLE it_boxlabels LINES linhas.
      CHECK linhas > 0.
**      Imprimir etiqueta resumo allotie
      CALL FUNCTION 'OPEN_FORM'
        EXPORTING
          device   = 'PRINTER'
          dialog   = ' '
          form     = tnapr-fonam
          language = sy-langu
          OPTIONS  = itcpo.

      CALL FUNCTION 'START_FORM'
        EXPORTING
          form        = 'ZWM_EAN128_ALLOT'
          language    = sy-langu
          program     = sy-repid
        EXCEPTIONS
          form        = 1
          format      = 2
          unended     = 3
          unopened    = 4
          unused      = 5
          spool_error = 6
          codepage    = 7
          OTHERS      = 8.
      IF sy-subrc <> 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = element
          window  = 'MAIN'
        EXCEPTIONS
          OTHERS  = 0.

      CALL FUNCTION 'END_FORM'
        EXCEPTIONS
          unopened                 = 1
          bad_pageformat_for_print = 2
          spool_error              = 3
          codepage                 = 4
          OTHERS                   = 5.

      CALL FUNCTION 'CLOSE_FORM'.


**      imprimir cada etiqueta de caixa
      itcpo-tdcopies = 1.
      LOOP AT it_boxlabels.
* check sy-tabix < 2.
**       label é a variavel usada na form da zebra
        MOVE-CORRESPONDING it_boxlabels TO zlabel.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = it_boxlabels-exidv
          IMPORTING
            output = zlabel-exidv.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = it_boxlabels-matnr
          IMPORTING
            output = zlabel-matnr.

        CLEAR zean128.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = it_boxlabels-p_material
          IMPORTING
            output = zean128-exidv.
        zean128-vhilmean = it_boxlabels-matean.
        zean128-charg = it_boxlabels-lote.
        CLEAR: cs_ean128_data-eanbc,
               cs_ean128_data-eanrd.

        CALL FUNCTION 'LE_EAN128_ENCODE'
          EXPORTING
            if_encode_profile         = 'ZBOX'
            is_ean128_data            = zean128
          IMPORTING
            et_barcode                = cs_ean128_data-eanbc
            et_barcode_txt            = cs_ean128_data-eanrd
          EXCEPTIONS
            no_barcode_definition     = 1
            error_in_subfunctions     = 2
            profile_not_for_ean128    = 3
            profile_unknown           = 4
            data_error                = 5
            no_ai_relation            = 6
            no_ai_information         = 7
            profile_error             = 8
            general_customizing_error = 9
            OTHERS                    = 10.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
***       Actualização dos BC
**        transform the barcode table to the flat structure
*         DESCRIBE FIELD barcode TYPE lf_type COMPONENTS lf_comp_count.
        DESCRIBE FIELD barcode TYPE lf_type COMPONENTS l_linhas.

        LOOP AT cs_ean128_data-eanbc ASSIGNING <ls_bc_plus_error>.
          IF sy-tabix > l_linhas.
            EXIT.
          ENDIF.
          ASSIGN COMPONENT sy-tabix OF STRUCTURE barcode TO <ls_bc>.
          <ls_bc>-bc = <ls_bc_plus_error>-barcode.
        ENDLOOP.

        LOOP AT cs_ean128_data-eanrd ASSIGNING <ls_bc_txt>.
          IF sy-tabix > l_linhas.
            EXIT.
          ENDIF.
          ASSIGN COMPONENT sy-tabix OF STRUCTURE barcode TO <ls_bc>.
          <ls_bc>-txt = <ls_bc_txt>.
        ENDLOOP.

        CALL FUNCTION 'OPEN_FORM'
          EXPORTING
            form     = 'ZWM_SSCC_CAIXA'
            language = 'P'
            OPTIONS  = itcpo
            device   = 'PRINTER'
            dialog   = ' '
          IMPORTING
            RESULT   = itcpp
          EXCEPTIONS
            OTHERS   = 1.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING itcpp TO itcpo.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element                  = ' '
              function                 = 'DELETE'
              type                     = 'BODY'
              window                   = 'MAIN'
            EXCEPTIONS
              element                  = 1
              function                 = 2
              type                     = 3
              unopened                 = 4
              unstarted                = 5
              window                   = 6
              bad_pageformat_for_print = 7
              OTHERS                   = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ELSE.
          EXIT.
        ENDIF.
        CALL FUNCTION 'CLOSE_FORM'
          EXCEPTIONS
            OTHERS = 1.
        IF sy-subrc NE 0.
*    retcode = sy-subrc.
        ENDIF.
      ENDLOOP.
*      endif.
    ENDIF.
  ENDLOOP.




ENDFUNCTION.
