FUNCTION zwm_entrada_producao_f03.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MATNR) LIKE  MARA-MATNR
*"     REFERENCE(LGNUM) LIKE  MLGN-LGNUM
*"     REFERENCE(AUFNR) LIKE  AFKO-AUFNR
*"     REFERENCE(COR) LIKE  MARA-MATNR
*"     REFERENCE(CHARG) LIKE  AFPO-CHARG OPTIONAL
*"     REFERENCE(LHMG1) LIKE  MLGN-LHMG1
*"     REFERENCE(MEINS) LIKE  MARA-MEINS
*"     REFERENCE(CRIA_SSCC) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(WA_SESSAO) TYPE  Z02RPSESSAO OPTIONAL
*"     REFERENCE(OPER) TYPE  CHAR80
*"     REFERENCE(I_PRINTER) TYPE  RSPOPNAME
*"  EXPORTING
*"     REFERENCE(MSG) TYPE  CHAR255
*"     REFERENCE(SSCC1) LIKE  BAPIHUKEY-HU_EXID
*"     REFERENCE(SSCC2) LIKE  BAPIHUKEY-HU_EXID
*"  EXCEPTIONS
*"      EXCEPTION
*"----------------------------------------------------------------------
*{   INSERT         DEVK904539                                        1

  "TABLES: t100.

  DATA : return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  DATA: g_hukey  LIKE bapihukey-hu_exid,
        l_hukey1 LIKE bapihukey-hu_exid,
        l_hukey2 LIKE bapihukey-hu_exid,
        l_subrc  LIKE sy-subrc,
        l_mblnr  LIKE mseg-mblnr,
        l_mjahr  LIKE mseg-mjahr,
        l_werks  LIKE mseg-werks,
        l_lgort  LIKE mseg-lgort,
        g_matnr  LIKE mara-matnr,
        l_charg  LIKE mseg-charg,
        l_aux(15).

  DATA: s_quant TYPE rl03t-anfme,
        s_aufnr  TYPE string,
        inteiro TYPE i.
*        l_quant(15) type c,
*        L_MATNR(18) type c,
*        L_NUMPALETE(6) type c,
*        L_DESCRICAO(40) type c,
*        L_AUFNR(12) type c.

  g_matnr = matnr.
  l_aux = lhmg1.
  l_quant = l_aux.

  DATA: formname TYPE tdform.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = g_matnr
    IMPORTING
      output = g_matnr.

** Executar movimento em modo de teste
  PERFORM regista_entrada_f03 USING 'X'
                                aufnr
                                lgnum
                                g_matnr
                                l_quant
                                meins
                                charg
*                 Begin GN(ROFF) 30.10.2012
                                wa_sessao-data
*                 End   GN(ROFF) 30.10.2012
                                wa_sessao-divisao "JALVES - 03.09.2018
                          CHANGING l_subrc
                                   l_mblnr
                                   l_mjahr
                                   msg.

  CHECK msg IS INITIAL.
*  IF l_subrc NE 0.
*    retorno = '60'. EXIT.
*  ENDIF.

** Depois em modo NORMAL
  PERFORM regista_entrada_f03 USING ' '
                                aufnr
                                lgnum
                                g_matnr
                                l_quant
                                meins
                                charg
*                 Begin GN(ROFF) 30.10.2012
                                wa_sessao-data
*                 End   GN(ROFF) 30.10.2012
                                wa_sessao-divisao "JALVES - 03.09.2018
                          CHANGING l_subrc
                                   l_mblnr
                                   l_mjahr
                                   msg.

  CHECK msg IS INITIAL.

** Se o lote não estiver preenchido ler no Doc. Material
*  IF charg IS INITIAL.
  CLEAR: mseg.
  SELECT * FROM mseg WHERE mblnr = l_mblnr
                       AND mjahr = l_mjahr.
    l_werks = mseg-werks.
    l_charg = mseg-charg.
    l_lgort = mseg-lgort.
    EXIT.
  ENDSELECT.
  IF sy-subrc NE 0.
    msg = 'ERRO ao dar entrada de material'.
  ENDIF.
*  ELSE.
*    l_charg = charg.
*  ENDIF.

  IF lgnum <> '100'.
    CHECK msg IS INITIAL.
    DATA wa_t320 TYPE t320.
    SELECT SINGLE * FROM t320 INTO wa_t320 WHERE werks EQ l_werks AND lgnum EQ lgnum .

    IF sy-subrc EQ 0.

      DATA  aux_tab TYPE STANDARD TABLE OF ltap_vb.

      s_aufnr = aufnr.
      s_quant =  l_quant.

      CALL FUNCTION 'L_TO_CREATE_SINGLE'
        EXPORTING
          i_lgnum                     = lgnum
          i_bwlvs                     = 913
          i_betyp                     = 'F'
          i_benum                     = aufnr(10)
          i_matnr                     = g_matnr
          i_werks                     = l_werks
          i_lgort                     = wa_t320-lgort
          i_charg                     = charg
*   I_BESTQ                     = ' '
*   I_SOBKZ                     = ' '
*   I_SONUM                     = ' '
          i_letyp                     = 'Z01'
          i_anfme                     = s_quant
          i_altme                     = meins
          i_commit_work               = 'X'
          i_bname                     = sy-uname
          i_kompl                     = 'X'
* IMPORTING
*   E_TANUM                     =
*   E_LTAP                      =
       TABLES
*   T_LTAK                      =
         t_ltap_vb                   = aux_tab
* EXCEPTIONS
*   NO_TO_CREATED               = 1
*   BWLVS_WRONG                 = 2
*   BETYP_WRONG                 = 3
*   BENUM_MISSING               = 4
*   BETYP_MISSING               = 5
*   FOREIGN_LOCK                = 6
*   VLTYP_WRONG                 = 7
*   VLPLA_WRONG                 = 8
*   VLTYP_MISSING               = 9
*   NLTYP_WRONG                 = 10
*   NLPLA_WRONG                 = 11
*   NLTYP_MISSING               = 12
*   RLTYP_WRONG                 = 13
*   RLPLA_WRONG                 = 14
*   RLTYP_MISSING               = 15
*   SQUIT_FORBIDDEN             = 16
*   MANUAL_TO_FORBIDDEN         = 17
*   LETYP_WRONG                 = 18
*   VLPLA_MISSING               = 19
*   NLPLA_MISSING               = 20
*   SOBKZ_WRONG                 = 21
*   SOBKZ_MISSING               = 22
*   SONUM_MISSING               = 23
*   BESTQ_WRONG                 = 24
*   LGBER_WRONG                 = 25
*   XFELD_WRONG                 = 26
*   DATE_WRONG                  = 27
*   DRUKZ_WRONG                 = 28
*   LDEST_WRONG                 = 29
*   UPDATE_WITHOUT_COMMIT       = 30
*   NO_AUTHORITY                = 31
*   MATERIAL_NOT_FOUND          = 32
*   LENUM_WRONG                 = 33
*   OTHERS                      = 34
                .
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.
  ENDIF.

** Ler MLGN
  IF cria_sscc = 'X'.

    FIELD-SYMBOLS <wa_ltap> LIKE LINE OF aux_tab.

    LOOP AT aux_tab ASSIGNING <wa_ltap>.
*
      MOVE-CORRESPONDING wa_sessao TO z02rpsessao.
      lenum = <wa_ltap>-nlenr.
      makt-matnr = <wa_ltap>-matnr.
      makt-maktx = <wa_ltap>-maktx.
      operador = oper.
      fabrico = aufnr.
      peso = l_quant.

      CONDENSE peso.
* Open form
*  itcpo-tdcopies = 0.
*  IF p_tddest IS INITIAL.
      itcpo-tddest = i_printer.
*  ENDIF.
      itcpo-tdnewid = 'X'.
      itcpo-tdimmed = 'X'.


** alteração Paulo Sousa - suporte para novo layout de etiqueta ZEBRA_PALETE
      formname = 'ZEBRA_BOBINA'.

      IF wa_sessao-area = 'F03'.
        formname = 'ZEBRA_PALETE'.
**   variaveis da form
        l_aufnr = aufnr.
        l_numpalete = ''.
        peso = 0.

**   quantidade inteira
        inteiro = l_quant.
        l_qtyint = inteiro.
        CONDENSE l_qtyint.
        SELECT SINGLE * FROM marm WHERE matnr = matnr
                                    AND meinh = meins.
        IF sy-subrc = 0.
          peso = marm-brgew * l_quant.
        ENDIF.
        CONDENSE peso.
        l_descricao = makt-maktx.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = matnr
          IMPORTING
            output = l_matnr.
      ENDIF.

** 1 copia

      itcpo-tdcopies = 1.

      CALL FUNCTION 'OPEN_FORM'
        EXPORTING
          form                        = formname
          language                    = 'E'
          OPTIONS                     = itcpo
          device                      = 'PRINTER'
          dialog                      = ' '
        IMPORTING
          RESULT                      = itcpp
        EXCEPTIONS
          canceled                    = 1
          device                      = 2
          form                        = 3
          OPTIONS                     = 4
          unclosed                    = 5
          mail_options                = 6
          archive_error               = 7
          more_params_needed_in_batch = 8.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ENDIF.

      " End open form


      CALL FUNCTION 'START_FORM'
       EXPORTING
*   ARCHIVE_INDEX          =
         form                   = formname
*   LANGUAGE               = ' '
*   STARTPAGE              = ' '
         program                = 'SAPLZWMFUNC1'
*   MAIL_APPL_OBJECT       =
* IMPORTING
*   LANGUAGE               =
       EXCEPTIONS
         form                   = 1
         format                 = 2
         unended                = 3
         unopened               = 4
         unused                 = 5
         spool_error            = 6
         codepage               = 7
         OTHERS                 = 8
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.



      " Write form

      CALL FUNCTION 'WRITE_FORM'
          EXPORTING
*      element                  = ' '
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
      IF    sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


*
*CALL FUNCTION 'END_FORM'
** IMPORTING
**   RESULT                         =
** EXCEPTIONS
**   UNOPENED                       = 1
**   BAD_PAGEFORMAT_FOR_PRINT       = 2
**   SPOOL_ERROR                    = 3
**   CODEPAGE                       = 4
**   OTHERS                         = 5
*          .
*IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.



      CALL FUNCTION 'CLOSE_FORM'
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*
*          SUBMIT z02rp_imp_etiquetas AND RETURN
*              WITH dialog = space
*              WITH so_lenum = <wa_ltap>-nlenr
*              WITH tdcopies = 1
*              WITH tddest = 'ZCD1'
*              WITH tdimmed = 'X'.

    ENDLOOP.

  ENDIF.

  IF lgnum = '100'.
    CLEAR: mlgn.
    SELECT SINGLE * FROM mlgn WHERE matnr = g_matnr
                                AND lgnum = lgnum.
    IF sy-subrc NE 0.
      msg = text-003.
    ELSE.
** Cria o SSCC (HU)

      DATA lv_qtd LIKE mlgn-lhmg1.

      IF l_quant < mlgn-lhmg1.
        lv_qtd = l_quant.
      ELSE.
        lv_qtd = mlgn-lhmg1.
      ENDIF.

      PERFORM cria_sscc USING l_subrc
                              lgnum
                              g_matnr
                              lv_qtd
                              meins
                              l_charg
                              l_werks
                              l_lgort
                              cor
                         CHANGING g_hukey
                                  msg.

      CHECK msg IS INITIAL.

*    IF l_subrc NE 0.
*      retorno = l_subrc. EXIT.
*    ENDIF.
*    retorno = 90.

      l_hukey1 = g_hukey.

** Actualiza ZWM013
      PERFORM actualiza_zwm013 USING lgnum
                                     l_hukey1
                                     mlgn-lety1.

** Se LETY1 = P2 ou P5 cria mais uma SSCC
      IF z_wm_cl_management=>is_remontada( i_lgnum = lgnum is_data = mlgn ) eq abap_true.

        IF l_quant <= mlgn-lhmg1.
          lv_qtd = mlgn-lhmg1.
        ELSE.

          lv_qtd = mlgn-lhmg1.
          PERFORM cria_sscc USING l_subrc
                                  lgnum
                                  g_matnr
                                  lv_qtd
                                  meins
                                  l_charg
                                  l_werks
                                  l_lgort
                                  cor
                           CHANGING g_hukey
                                    msg.

          IF l_subrc = 0.
            l_hukey2 = g_hukey.

** Actualiza ZWM020
            PERFORM actualiza_zwm020 USING lgnum
                                           l_hukey1
                                           l_hukey2.

          ENDIF.
        ENDIF.
      ENDIF.
      sscc1 = l_hukey1.
      sscc2 = l_hukey2.
    ENDIF.
  ELSE.
    sscc1 = l_mblnr.
    sscc2 = l_hukey2.
  ENDIF.
** SSCC criado com sucesso.
*  sscc1 = l_mblnr.
*  sscc2 = l_hukey2.
  msg = text-001.
*
*}   INSERT
ENDFUNCTION.
