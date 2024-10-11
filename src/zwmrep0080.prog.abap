*&---------------------------------------------------------------------*
*& Report  ZWMREP0080                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0080                              .

INCLUDE zwmrept080.
INCLUDE zwmrepf080.


SELECTION-SCREEN BEGIN OF BLOCK bl0 WITH FRAME TITLE text-001.
PARAMETERS: zmatnr LIKE vepo-matnr OBLIGATORY.

*            zexidv LIKE vekp-exidv OBLIGATORY.

SELECT-OPTIONS:
            zexidv FOR vekp-exidv,
            zerdat FOR vekp-erdat,
            zeruhr FOR vekp-eruhr.

SELECTION-SCREEN END   OF BLOCK bl0.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-002.
*PARAMETERS: zvbeln LIKE likp-vbeln,
*            zkunnr LIKE likp-kunnr.
SELECT-OPTIONS: zvbeln FOR likp-vbeln,
                zkunnr FOR likp-kunnr.
SELECTION-SCREEN END   OF BLOCK bl1.

AT SELECTION-SCREEN.
  IF zerdat IS INITIAL AND zexidv IS INITIAL.
    MESSAGE e220(z1).
  ENDIF.


START-OF-SELECTION.
  REFRESH tabi.

**************************************************
* dados entradas de producao e saidas paletes completas
**************************************************
  SELECT * FROM vekp WHERE exidv IN zexidv AND
                           exida = 'C' AND
                           erdat IN zerdat AND
                           eruhr IN zeruhr.
    CLEAR tabi.
    tabi-tipo = '1'.
    tabi-exidv = vekp-exidv.
    tabi-erdat_sscc = vekp-erdat.
    tabi-eruhr_sscc = vekp-eruhr.

*    SELECT SINGLE * FROM vepo WHERE venum = vekp-venum AND
    SELECT * FROM vepo WHERE venum = vekp-venum AND
                             matnr = zmatnr.
*                             vepos = '000001'.
*    IF sy-subrc = 0.
      tabi-matnr = vepo-matnr.
      tabi-charg = vepo-charg.
      tabi-vemng = tabi-vemng + vepo-vemng.
*    ENDIF.
    ENDSELECT.
*       le dados de posição
    SELECT SINGLE * FROM lein WHERE lenum = vekp-exidv.
    IF sy-subrc = 0.
      tabi-lgtyp = lein-lgtyp.
      tabi-lgpla = lein-lgpla.
    ENDIF.
*       le dados de saida - guias de remessa
    SELECT SINGLE * FROM ltap WHERE vlenr = vekp-exidv.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM ltak
                      WHERE lgnum = ltap-lgnum AND
                            tanum = ltap-tanum AND
                            ( bwlvs = '601' OR bwlvs = '850' ).
      IF sy-subrc = 0.
        IF ltak-vbeln IS INITIAL AND
           ltak-betyp EQ 'L'.
          ltak-vbeln = ltak-benum.
        ENDIF.


        tabi-vbeln = ltak-vbeln.
        SELECT SINGLE * FROM likp WHERE vbeln = ltak-vbeln.
        IF sy-subrc = 0.
          tabi-bdatu_sai = likp-wadat_ist.
          tabi-kunnr = likp-kunnr.
          PERFORM nome_recebedor USING likp-vkorg
                                       tabi-vbeln
                                       tabi-kunnr
                                       tabi-nome.
          APPEND tabi.
        ENDIF.
      ELSE.
        APPEND tabi.
      ENDIF.
    ELSE.
      APPEND tabi.
    ENDIF.
  ENDSELECT.
*
************************************
*  Dados de Picking e REP
************************************
*
  CLEAR: zcontapik,
         ztotrep,
         zmaispik.

  SELECT * FROM ltap WHERE nlenr IN zexidv AND
                           matnr = zmatnr AND
                           nltyp = 'REP'.

******
*   REP
******
    SELECT SINGLE * FROM ltak WHERE lgnum = ltap-lgnum AND
                                    tanum = ltap-tanum.

*                                  ( bwlvs = '972' OR
*                                    bwlvs = '973' or
*                                    bwlvs = '826' or
*                                    bwlvs = '827' or
*                                    bwlvs = '828').
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    CLEAR tabi.
    tabi-tipo = '2'.
    tabi-texto = 'REP'.
    tabi-exidv = ltap-nlenr.
    tabi-erdat_sscc = ltap-edatu.
    tabi-eruhr_sscc = ltap-ezeit.
    tabi-matnr = ltap-matnr.
    tabi-charg = ltap-charg.
    tabi-benum = ltak-benum.
*   quando o registo é do REP coloca o numero do grupo no campo
*   do sscc de picking.
    tabi-exidv_pik = ltak-benum.

    SELECT SINGLE * FROM vekp WHERE exidv = ltap-nlenr AND
                             exida NE 'H'.
    IF sy-subrc = 0.
      CLEAR vepo.
      SELECT SINGLE * FROM vepo WHERE venum = vekp-venum.
      IF sy-subrc = 0.
        ztotrep_g = ztotrep_g + vepo-vemng.
        tabi-vemng = vepo-vemng.
      ENDIF.
    ENDIF.
    APPEND tabi.
  ENDSELECT.

  CLEAR ztotpik.

  LOOP AT tabi WHERE tipo = '2'.
*   guarda o total do rep do sscc de producao
    ztotrep = tabi-vemng.
*
    REFRESH pi_tabi.
    CLEAR: pi_tabi, ztotpik.
*
    pi_tabi-tipo = '3'.
    pi_tabi-texto = 'PICK'.
    fl_sai = 0.

    PERFORM le_historico USING fl_sai zmatnr.
*   verifica se o total do picking apanhado é >= ao total geral do REP
*   se for não lê mais nada
*   Goncalo guterres pediu assim depois desistiu do processo
*   27/10/08
*    if ztotpik >= ztotrep_g.
*       exit.
*    endif.

*   se encontrou registos no historico ligados ao grupo, lê novamente
*   o historico agora pelo dia/hora superior ao ultimo sscc
*   senão vai ler a tabela zwm026 ( registos em aberto)
    IF fl_sai = 0.
      CLEAR: zmaispik,
             zvbeln_ant.
*     qtd do rep ainda > que qtd do picking já apurada
      IF ztotrep > ztotpik.
        PERFORM le_historico2.
      ENDIF.
    ENDIF.
*   le ficheiro em aberto se qtd rep > qtd Pik
    IF ztotrep > ztotpik.
      IF fl_sai = 0.
        PERFORM registo_aberto_semgrupo.
      ELSE.
        PERFORM registo_aberto_grupo USING zmatnr.
      ENDIF.
      IF ztotrep > ztotpik.
        PERFORM registo_aberto2.
      ENDIF.
    ENDIF.
  ENDLOOP.
*  inclui na tabela interna principal 'TABI' os registos da PX_TABI
  APPEND LINES OF px_tabi TO tabi.
****** new
  CLEAR zmaispik.

  SORT tabi BY tipo matnr charg exidv erdat_sscc
               eruhr_sscc exidv_pik vbeln.

****
* Pedido por Goncalo Guterres em 21/10/08
* Abandonou o criterio em 27/10/08

*  LOOP AT tabi WHERE tipo = '3'.
*    zkpik = zkpik + tabi-vemng.
*    ztotpik = ztotpik + zwm026h-quantidade.
*    IF zkpik >= ztotrep_g.
*      zmaispik = zmaispik + 1.
*      IF zmaispik > 1.
*        DELETE tabi.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
******
* ordena por data e hora
*  SORT tabi BY tipo erdat_sscc eruhr_sscc.
*  SORT tabi BY tipo.
************************
  PERFORM get_event CHANGING events.
  PERFORM f_header.
  PERFORM get_campos CHANGING it_fieldcat.
  PERFORM get_layout_sort CHANGING layout sort.
  PERFORM imprime_dados.
