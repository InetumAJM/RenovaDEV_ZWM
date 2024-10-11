FUNCTION zwm_entrada_producao.
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
*"  EXPORTING
*"     REFERENCE(MSG) TYPE  CHAR255
*"     REFERENCE(SSCC1) LIKE  BAPIHUKEY-HU_EXID
*"     REFERENCE(SSCC2) LIKE  BAPIHUKEY-HU_EXID
*"  EXCEPTIONS
*"      EXCEPTION
*"----------------------------------------------------------------------

  TABLES: mseg, t100.

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
        l_quant  LIKE vepo-vemng,
        l_aux(15).

  g_matnr = matnr.
  l_aux = lhmg1.
  l_quant = l_aux.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = g_matnr
    IMPORTING
      output = g_matnr.

** Executar movimento em modo de teste
  PERFORM regista_entrada USING 'X'
                                aufnr
                                lgnum
                                g_matnr
                                l_quant
                                meins
                                charg
                          CHANGING l_subrc
                                   l_mblnr
                                   l_mjahr
                                   msg.

  CHECK msg IS INITIAL.
*  IF l_subrc NE 0.
*    retorno = '60'. EXIT.
*  ENDIF.

** Depois em modo NORMAL
  PERFORM regista_entrada USING ' '
                                aufnr
                                lgnum
                                g_matnr
                                l_quant
                                meins
                                charg
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

  CHECK msg IS INITIAL.

** Ler MLGN
  IF cria_sscc = 'X'.
    CLEAR: mlgn.
    SELECT SINGLE * FROM mlgn WHERE matnr = g_matnr
                                AND lgnum = lgnum.
    IF sy-subrc NE 0.
      msg = text-003.
    ELSE.
** Cria o SSCC (HU)
      PERFORM cria_sscc USING l_subrc
                              lgnum
                              g_matnr
                              l_quant
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

        PERFORM cria_sscc USING l_subrc
                                lgnum
                                g_matnr
                                l_quant
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
  ENDIF.

** SSCC criado com sucesso.
  sscc1 = l_hukey1.
  sscc2 = l_hukey2.
  msg = text-001.
*
ENDFUNCTION.
