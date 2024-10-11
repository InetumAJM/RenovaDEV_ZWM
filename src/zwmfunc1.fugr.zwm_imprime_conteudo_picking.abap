FUNCTION zwm_imprime_conteudo_picking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PRINTER) LIKE  NAST-LDEST
*"  TABLES
*"      SSCC STRUCTURE  ZWM_EAN128
*"  EXCEPTIONS
*"      IMPRESSORA_NAO_EXISTE
*"      SSCC_NAO_EXISTE
*"      SSCC_COM_IMPRESSAO_GRUPO
*"----------------------------------------------------------------------

  DATA: tnapr        LIKE tnapr,
        x_nast       TYPE nast,
        lf_progname  TYPE syrepid,
        l_venum      LIKE vekp-venum,
        l_linhas     LIKE sy-tabix,
        oldlinhas    LIKE sy-tabix,

        l_profile    TYPE ean128_profile,
        l_devicetype TYPE t313k-devicetype,
        itcpo        TYPE itcpo,
        lf_type      TYPE c,
        qtd_pal      TYPE vemng.

  DATA: wa_values TYPE ean128,
        cs_ean128_data-eanbc TYPE barcode_plus_errors_t,
        cs_ean128_data-eanrd TYPE barcode_txt_t,
        itab_vepo LIKE vepo OCCURS 0 WITH HEADER LINE.

  DATA: we_name2 LIKE adrc-name2,
        we_street LIKE adrc-street,
        we_post_code1 LIKE adrc-post_code1,
        we_city1 LIKE adrc-city1,
        we_numguia LIKE vbfa-vbeln,
        we_numorder LIKE vbfa-vbeln,
        delivery_date LIKE likp-lfdat.

  DATA: BEGIN OF i_zwm026 OCCURS 0.
          INCLUDE STRUCTURE zwm026.
  DATA: END OF i_zwm026.


  DATA zvenum LIKE vekp-venum.


  FIELD-SYMBOLS:
        <ls_bc>            TYPE barcode_plus_txt,
        <ls_bc_plus_error> TYPE barcode_plus_errors,
        <ls_bc_txt>        TYPE barcode_txt.

  CHECK NOT sscc[] IS INITIAL.


  REFRESH i_zwm026.
** Impressora
  CLEAR: tsp03.
  SELECT SINGLE * FROM tsp03
  WHERE padest = printer.

  IF sy-subrc NE 0.
    RAISE impressora_nao_existe.
    EXIT.
  ENDIF.
  x_nast-ldest = printer.

** Programa
  lf_progname = 'RLE_EAN128_LABEL'.

** Estrutura com dados para a impressora
  CLEAR itcpo.
  itcpo-tdcopies = 1.
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

  LOOP AT sscc.
** Imprime Etiqueta
    SELECT SINGLE * FROM vekp WHERE exidv = sscc.
    IF sy-subrc = 0.
      zvenum = vekp-venum.
    ENDIF.

    CLEAR: itab_vepo, wa_values, ean128.
    FREE: itab_vepo.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = sscc-sscc
      IMPORTING
        output = wa_values-exidv.

**  Obter Unidade comercial
    CLEAR: vekp.
    SELECT SINGLE venum vhilm vbeln_gen FROM vekp
    INTO (l_venum, wa_values-vhilm, we_numguia)
    WHERE exidv EQ wa_values-exidv.

**  Obter Lote

    CLEAR: vepo.
    SELECT * FROM vepo
       INTO CORRESPONDING FIELDS OF TABLE itab_vepo
       WHERE venum EQ l_venum.

    DESCRIBE TABLE itab_vepo LINES l_linhas.


    LOOP AT itab_vepo.

**     Obter dados do Lote
      CLEAR: mcha.
      CLEAR wa_values.
      SELECT SINGLE vfdat FROM mcha  INTO wa_values-vfdat
              WHERE matnr EQ itab_vepo-matnr
                AND werks EQ itab_vepo-werks
                AND charg EQ itab_vepo-charg.

**     Obter a Descrição do material
      CLEAR: makt.
      SELECT SINGLE maktx FROM makt INTO wa_values-maktx
             WHERE matnr EQ itab_vepo-matnr
             AND spras EQ sy-langu.

** Obter o Ean
      CLEAR marm.
      SELECT SINGLE * FROM marm
              WHERE matnr = itab_vepo-matnr AND
                    meinh = 'PAL'.
      IF NOT marm-ean11 IS INITIAL.
        CLEAR qtd_pal.
        qtd_pal = marm-umrez / marm-umren.
        IF itab_vepo-vemng = qtd_pal.
          wa_values-matean = marm-ean11.
        ELSE.
          CLEAR: mara.
          SELECT SINGLE ean11 INTO wa_values-matean FROM mara
             WHERE matnr EQ itab_vepo-matnr.
        ENDIF.
      ELSE.
        CLEAR: mara.
        SELECT SINGLE ean11 INTO wa_values-matean FROM mara
            WHERE matnr EQ itab_vepo-matnr.
      ENDIF.

      wa_values-matnr = itab_vepo-matnr.
      wa_values-charg = itab_vepo-charg.
      wa_values-lfimg = itab_vepo-vemng.
      wa_values-vrkme = itab_vepo-vemeh.
      wa_values-vemng = itab_vepo-vemng.
      wa_values-vemeh = itab_vepo-vemeh.

      l_profile = '000'.
      wa_values-lbltype = 'G'.
      tnapr-fonam = 'ZWM_ITEM_PICKING'.
      l_devicetype = 'ZLB_ZEB'.

      CALL FUNCTION 'OPEN_FORM'
        EXPORTING
          device   = 'PRINTER'
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
*            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      MOVE wa_values TO ean128.
      ean128-exidv = sscc-sscc.

      CALL FUNCTION 'LE_EAN128_ENCODE'
        EXPORTING
          if_encode_profile         = l_profile
          if_skip_empty_fields      = 'X'
          is_ean128_data            = ean128
          if_devicetype             = l_devicetype
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
*            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

***     Actualização dos BC
**      transform the barcode table to the flat structure
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

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'EAN128'
          window  = 'MAIN'
        EXCEPTIONS
          OTHERS  = 0.

      CALL FUNCTION 'END_FORM'
*           IMPORTING
*           RESULT                         =
          EXCEPTIONS
             unopened                       = 1
             bad_pageformat_for_print       = 2
             spool_error                    = 3
             codepage                       = 4
             OTHERS                         = 5
              .
      IF sy-subrc <> 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION 'CLOSE_FORM'.
    ENDLOOP.
  ENDLOOP.
ENDFUNCTION.
