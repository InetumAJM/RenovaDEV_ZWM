************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0030                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Impressão da Paletização Especial (Sapscript)            *
* Criado por: Luís Rocha                                               *
* Criado em.: 14/12/2004                                               *
* Tipo PRG..: Report                                                   *
************************************************************************

REPORT zwmrep0030 LINE-COUNT 100.

TABLES: likp,
        lips,
        kna1,
        mara,
        makt,
        marm, *marm,
        vbss,
        vbsk,
        t300,
        vbpa,
        itcpo,
        zwm031,
        zwm049.
data: wa_zwm049 type zwm049.
***** variáveis e tabelas internas

DATA: gt_adrc LIKE adrc OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_marm OCCURS 0,
        matnr LIKE marm-matnr,
        meinh LIKE marm-meinh,
        umrez LIKE marm-umrez,
        umren LIKE marm-umren,
        hoehe LIKE marm-hoehe,
      END OF gt_marm.

DATA: BEGIN OF tab_out OCCURS 0,
         kunnr     LIKE zwm031-kunnr,
         vbeln     LIKE likp-vbeln,
         matnr     LIKE zwm031-matnr,
         niveis    LIKE zwm031-niveis,
         lastro    LIKE zwm031-lastro,
         unporpal  LIKE zwm031-unporpal,
         hoehe     LIKE zwm031-hoehe,
         meabm     LIKE zwm031-meabm,
         remontada LIKE zwm031-remontada,
      END OF tab_out,
      reg_out LIKE tab_out.

DATA: w_subrc LIKE sy-subrc.

DATA: g_index LIKE sy-tabix.
data: tab_out2 like tab_out occurs 0 with header line.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum LIKE mlgn-lgnum OBLIGATORY MEMORY ID lgn,
            p_refnr LIKE zwm028-refnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

* Initialization
INITIALIZATION.
  DATA: xuser LIKE lrf_wkqu OCCURS 0 WITH HEADER LINE.
* Read the user data from table lrf_wkqu ( for all the warehouses)
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = xuser
    EXCEPTIONS
      no_entry_found = 01.
  IF sy-subrc = 0.
    READ TABLE xuser INDEX 1.
    MOVE xuser-lgnum TO p_lgnum.
  ENDIF.

* Start-of-Selection
START-OF-SELECTION.

  PERFORM valida_parametros_entrada.
  IF sy-subrc <> 0.
*   ERRO: Nº Depósito Inválido !
    MESSAGE s146(zwmmsg001).
    EXIT.
  ENDIF.

  PERFORM select_data.

  IF tab_out[] IS INITIAL.
*   Não existem dados para processar !
    MESSAGE s147(zwmmsg001).
    EXIT.
  ENDIF.

* Start of Selection
END-OF-SELECTION.

  vbsk-sammg = p_refnr.

  PERFORM fill_itcpo.
  PERFORM open_form USING w_subrc.

  CHECK w_subrc = 0.

  PERFORM item_print.

  PERFORM form_close.

*&--------------------------------------------------------------------*
*&      Form  item_print
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM item_print.

  LOOP AT tab_out.
    reg_out = tab_out.
    clear: wa_zwm049.
    select single * into wa_zwm049 from zwm049 where lgnum = p_lgnum and
                                                 kunnr = tab_out-kunnr.
    if wa_zwm049-PALTXT is not initial.
      concatenate 'Obs:'wa_zwm049-PALTXT into wa_zwm049-PALTXT separated by space.
    endif.

    AT NEW kunnr.
      PERFORM get_cliente USING tab_out-kunnr.
      PERFORM start_form.
      PERFORM header_init.

    ENDAT.

    AT NEW vbeln.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'REMESSA'.
    ENDAT.

    PERFORM get_material USING tab_out-matnr.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'LINHA'.

    AT END OF kunnr.
      PERFORM end_form.
      PERFORM header_close.
    ENDAT.

  ENDLOOP.

ENDFORM.                    "ITEM_PRINT

*&--------------------------------------------------------------------*
*&      Form  header_init
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM header_init.
  CALL FUNCTION 'WRITE_FORM'           "First header
       EXPORTING  element = 'HEADER'
       EXCEPTIONS OTHERS  = 1.
  CALL FUNCTION 'WRITE_FORM'           "Activate header
       EXPORTING  element = 'HEADER'
                  type    = 'TOP'
       EXCEPTIONS OTHERS  = 1.
ENDFORM.                    "header_init

*&--------------------------------------------------------------------*
*&      Form  header_close
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM header_close.
  CALL FUNCTION 'WRITE_FORM'           "Activate header
       EXPORTING  element = 'HEADER'
                  type    = 'DELETE'
       EXCEPTIONS OTHERS  = 1.
ENDFORM.                    "header_close

*&--------------------------------------------------------------------*
*&      Form  fill_itcpo
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM fill_itcpo.
  CLEAR itcpo.
  itcpo-tdimmed    = 'X'.
  itcpo-tddelete   = 'X'.
  itcpo-tdpreview  = 'X'.
  itcpo-tdcopies   = 1.
  itcpo-tdprogram  = sy-repid.
  itcpo-tdteleland = sy-langu.
  itcpo-tdsenddate = sy-datum.
  itcpo-tdtitle    = sy-rtitl.
  itcpo-tdsenddate = sy-datum.
  itcpo-tdsendtime = sy-uzeit.
ENDFORM.                    "FILL_ITCPO

*&--------------------------------------------------------------------*
*&      Form  open_form
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_SUBRC    text
*---------------------------------------------------------------------*
FORM open_form USING f_subrc.
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device   = 'PRINTER'
      dialog   = 'X'
      form     = 'ZWM_PAL_ESPECIAL'
      language = sy-langu
      options  = itcpo
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc NE 0.
    f_subrc = sy-subrc.
  ENDIF.
ENDFORM.                    "OPEN_FORM

*&--------------------------------------------------------------------*
*&      Form  form_close
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM form_close.
  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc NE 0.
    w_subrc = 1.
  ENDIF.
ENDFORM.                    "FORM_CLOSE

*&--------------------------------------------------------------------*
*&      Form  start_form
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM start_form.
  CALL FUNCTION 'START_FORM'
* EXPORTING
*   ARCHIVE_INDEX          =
*   FORM                   = ' '
*   LANGUAGE               = ' '
*   STARTPAGE              = ' '
*   PROGRAM                = ' '
*   MAIL_APPL_OBJECT       =
* IMPORTING
*   LANGUAGE               =
* EXCEPTIONS
*   FORM                   = 1
*   FORMAT                 = 2
*   UNENDED                = 3
*   UNOPENED               = 4
*   UNUSED                 = 5
*   SPOOL_ERROR            = 6
*   CODEPAGE               = 7
*   OTHERS                 = 8
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "start_form

*&--------------------------------------------------------------------*
*&      Form  end_form
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM end_form.
  CALL FUNCTION 'END_FORM'
* IMPORTING
*   RESULT                         =
* EXCEPTIONS
*   UNOPENED                       = 1
*   BAD_PAGEFORMAT_FOR_PRINT       = 2
*   SPOOL_ERROR                    = 3
*   CODEPAGE                       = 4
*   OTHERS                         = 5
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "end_form

*&---------------------------------------------------------------------*
*&      Form  valida_parametros_entrada
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_parametros_entrada .

  SELECT SINGLE * FROM t300
                    WHERE
                      lgnum = p_lgnum.
ENDFORM.                    "valida_parametros_entrada

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

  DATA: lt_vbss LIKE vbss OCCURS 0 WITH HEADER LINE,
        lt_lips LIKE lips OCCURS 0 WITH HEADER LINE,
        kunnr_aux like likp-kunnr.

  REFRESH tab_out. CLEAR tab_out.

  SELECT * FROM vbss INTO TABLE lt_vbss WHERE sammg = p_refnr.

  CHECK NOT lt_vbss[] IS INITIAL.

  LOOP AT lt_vbss.

    SELECT SINGLE * FROM likp WHERE vbeln = lt_vbss-vbeln.
    CHECK sy-subrc = 0.

    SELECT * FROM lips INTO TABLE lt_lips
                        WHERE vbeln = likp-vbeln.

    LOOP AT lt_lips.
      CLEAR : tab_out,
              kunnr_aux,
              vbpa.

** INS BS - 11.07.2006
** Alteração efectuado a pedido de Sr. Ferreira,
** a validação da paletização especial é feita SEMPRE
** pelo recebedor da mercadoria - tipo W1 ou WE
      SELECT SINGLE kunnr INTO vbpa-kunnr
          FROM vbpa
              WHERE vbeln EQ lt_lips-vbeln
                AND posnr = '000000'
                AND parvw EQ 'W1'.

      IF SY-SUBRC EQ 0.
        kunnr_aux = vbpa-kunnr.
      ELSE.
        SELECT SINGLE adrnr kunnr INTO vbpa-kunnr
            FROM vbpa
                WHERE vbeln EQ lt_lips-vbeln
                  AND posnr = '000000'
                  AND parvw EQ 'WE'.
        kunnr_aux = vbpa-kunnr.
      ENDIF.
** INS BS - 11.07.2006
** Alteração efectuado a pedido de Sr. Ferreira,
** a validação da paletização especial é feita SEMPRE
** pelo recebedor da mercadoria - tipo W1 ou WE

      CLEAR zwm031.
      SELECT SINGLE * FROM  zwm031
             WHERE lgnum  = p_lgnum
*               AND kunnr  = likp-kunnr
                AND KUNNR = KUNNR_AUX
               AND matnr  = lt_lips-matnr.

     if  sy-subrc = 0.
       MOVE-CORRESPONDING zwm031 TO tab_out.
     else.
       SELECT single * from zwm049 where lgnum = p_lgnum and
                                         kunnr = kunnr_aux.
       check sy-subrc = 0 and
             zwm049-PALTXT is not initial.
       move kunnr_aux to tab_out-kunnr.
     endif.

      MOVE lt_lips-vbeln TO tab_out-vbeln.

      APPEND tab_out.

    ENDLOOP.

  ENDLOOP.

  SORT tab_out.

  DELETE adjacent duplicates FROM tab_out comparing kunnr vbeln matnr.
  tab_out2[] = tab_out[].

  data: nlinhas type i.
  delete adjacent duplicates from tab_out2 comparing kunnr.
  loop at tab_out2.
    clear: nlinhas.
    loop at tab_out where kunnr = tab_out2-kunnr.
      add 1 to nlinhas.
    endloop.
    if nlinhas > 1.
      delete tab_out where kunnr = tab_out2-kunnr and
                           matnr is initial.
    endif.
  endloop.

ENDFORM.                    "cria_tab_out

*&--------------------------------------------------------------------*
*&      Form  get_material
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_MATNR    text
*---------------------------------------------------------------------*
FORM get_material USING f_matnr.
  DATA: l_index LIKE sy-tabix.

  CLEAR: mara, makt, *marm.
  SELECT SINGLE * FROM mara WHERE matnr = f_matnr.
  SELECT SINGLE * FROM makt WHERE matnr = f_matnr
                              AND spras = sy-langu.

  SELECT SINGLE * FROM marm WHERE matnr = f_matnr
                              AND meinh = mara-meins.
*  move gt_marm-umren to marm-umren.
*
** Utiliza *MARM para salvaguardar a MARM
*   *marm-umren = vepo-vemng * marm-umren.

ENDFORM.                    "get_material

*&--------------------------------------------------------------------*
*&      Form  get_cliente
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_KUNNR    text
*---------------------------------------------------------------------*
FORM get_cliente USING f_kunnr.

  CLEAR: kna1.
  SELECT SINGLE * FROM kna1 WHERE kunnr = f_kunnr.

ENDFORM.                    "get_cliente
