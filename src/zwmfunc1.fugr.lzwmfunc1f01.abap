*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WAREHOUSE  text
*      -->P_0035   text
*      -->P_0036   text
*      -->P_HOST   text
*----------------------------------------------------------------------*
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

  IF ti_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = whs
      TABLES
        ti_zwm001 = ti_zwm001.
  ENDIF.

  CLEAR zwm001.
  READ TABLE ti_zwm001 WITH KEY      armazem   = whs
                                     processo  = module
                                     parametro = param
                                     BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE ti_zwm001 TO zwm001.
  ENDIF.
  MOVE zwm001-valor TO valor.

ENDFORM.                    " GET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  REGISTA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM regista_dados .

  DATA : st_type_aux(20),
         mov_aux(20),
         mov             LIKE ltak-bwlvs,
         st_type         TYPE lgtyp.


  CLEAR : st_type_aux,
          mov_aux,
          mov,
          st_type.

** Dados de entrada em STOCK ... NÃO SEI O QUE É ????

** Storage Type para devoluções
  PERFORM get_parameter USING xuser-lgnum
                           'DEVOLUCAO_ARMAZEM'
                           'ST_DEV'
                           st_type_aux.

  IF NOT st_type_aux IS INITIAL.
    WRITE st_type_aux TO st_type.
  ENDIF.


** Movimento de entrada para devoluções
  PERFORM get_parameter USING xuser-lgnum
                           'DEVOLUCAO_ARMAZEM'
                           'MOV'
                           mov_aux.

  IF NOT mov_aux IS INITIAL.
    WRITE mov_aux TO mov.
  ENDIF.


** Regista na tabela interna os dados
  READ TABLE l_su WITH KEY su = su.
  IF sy-subrc <> 0.
    l_su-su = su.
    APPEND l_su.

*** Cria TO com base na Palete
*    CALL FUNCTION 'ZWM_CREATE_STORAGE_UNIT'
*      EXPORTING
*        WAREHOUSE        = XUSER-LGNUM
*        MOV_TYPE         = MOV
*        ST_TYPE          = ST_TYPE
**   REQ_TYPE         =
**   REQ_NUMBER       =
**   STOCK_CAT        =
*        PLANT            =
*        S_LOC            =
*        SU_TYPE          =
**   DEST_BIN         =
*        MAT_STRUCT       =
** IMPORTING
**   TO               =
*      TABLES
*        RESULT_MSG       = result_msg
*      CHANGING
*        SU_NUMBER        =
*     EXCEPTIONS
*       OTHERS           = 1
*       OTHERS           = 2
*              .
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*
  ELSE.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '038'.

  ENDIF.

** Coloca no campo pos o total de SU´s já inseridas
  DESCRIBE TABLE l_su LINES pos.
  CLEAR : su.
  MOVE 'SU' TO cursorfield.

ENDFORM.                    " REGISTA_DADOS

*&---------------------------------------------------------------------*
*&      Form  GET_BEST_TO_TRI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LTAP  text
*      -->P_CORREDOR  text
*      -->P_ULTIMO_TIPO_DEP  text
*      <--P_NOVA_TO  text
*----------------------------------------------------------------------*
FORM get_best_to_tri  TABLES   p_ltap STRUCTURE l_ltap
                  USING    p_corredor
                           p_profundidade
                           p_nivel
                           p_ultimo_tipo_dep
                           p_armazem
                           p_equipamento
                           p_production_queue
                           p_tri_out_queue
                           p_queue_pick_tri
                           p_dev_inc_queue
                           p_sentido
                  CHANGING p_nova_to STRUCTURE zwm011
                           p_tipo_queue.

  DATA : numero_corredores_tri   TYPE i,
         numero_profundidade_tri TYPE i,
         numero_nivel_tri        TYPE i,
         num_max_st_types        TYPE i,
         peso_st_type            TYPE i,
         prioridade_to           LIKE zwm014-prioridade,
         peso_corredor           TYPE i VALUE '100',
         queue_saida_tri         LIKE zwm001-valor,
         queue_saida_tri_prm     LIKE zwm001-valor,
         queue_saida_tri_cpk     LIKE zwm001-valor.

  DATA valor LIKE zwm001-valor.

*****************************
  DATA : BEGIN OF st_ltap_xls,
           tanum            LIKE ltap-tanum,
           vltyp            LIKE ltap-vltyp,
           vlpla            LIKE ltap-vlpla,
           nltyp            LIKE ltap-nltyp,
           nlpla            LIKE ltap-nlpla,
           queue            LIKE ltak-queue,
           tipo             LIKE zwm010-tipo,
           prioridade_queue,
           prioridade_to    TYPE p DECIMALS 0.
  DATA : END OF st_ltap_xls.

  DATA ltap_xls LIKE st_ltap_xls OCCURS 0 WITH HEADER LINE.
*****************************



** Número máximo de CORREDORES no depósito TRILATERAL
**  Dá prioridade aos corredores do lado direito
  CLEAR valor.
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_PRIORIDADES'
                'CORREDOR_TRI'
                valor.

  MOVE valor TO numero_corredores_tri.

** Número máximo de PROFUNDIDADES no depósito TRILATERAL
  CLEAR valor.
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_PRIORIDADES'
                'PROFUNDIDADE_TRI'
                valor.

  MOVE valor TO numero_profundidade_tri.

*  WRITE numero_profundidade_tri TO
*        numero_profundidade_tri LEFT-JUSTIFIED.

** Número máximo de NIVEIS no depósito TRILATERAL
  CLEAR valor.
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_PRIORIDADES'
                'NIVEL_TRI'
                valor.

  MOVE valor TO numero_nivel_tri.

*  WRITE numero_nivel_tri TO
*        numero_nivel_tri LEFT-JUSTIFIED.

** Fila de saida de trilateral
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_TRI'
                queue_saida_tri.

** Fila de saida de trilateral para PRM
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_FILAS'
                'FILA_S_TRI_PAL_PRM'
                queue_saida_tri_prm.

** Fila de saida de trilateral para Copacking
  PERFORM get_parameter
            USING p_armazem
                  'GESTAO_FILAS'
                  'FILA_S_TRI_CPK'
                  queue_saida_tri_cpk.


  IF p_sentido EQ 'E' AND p_corredor IS INITIAL.
**  Dá prioridade aos corredores do lado esquerdo
    p_corredor = numero_corredores_tri.
  ENDIF.


** Quando o trilateral tem de saltar de corredor ... temos de verificar
** qual é o mais próximo e atribuir-lhe a maior prioridade

** Ordem de prioridades
** 1 - Prioridade associada à queue
** 2 - Mais próxima do destino da anterior
** 3 - TO com mais antiguidade ??
  DATA: corredor(4).
  DATA s_index LIKE sy-tabix.
  DATA n_mensulas_ocupadas TYPE i.

  LOOP AT l_ltap.
    s_index = sy-tabix.
    CLEAR : prioridade_to.
    CLEAR corredor.

** Saídas de trilateral
    IF l_ltap-queue = p_tri_out_queue     OR
       l_ltap-queue = p_queue_pick_tri    OR
       l_ltap-queue = queue_saida_tri_prm OR
       l_ltap-queue = queue_saida_tri_cpk.
      .
** verificar se tem mensula vaga

      CONCATENATE l_ltap-vlpla(3) '%' INTO corredor.

      SELECT SINGLE *
          FROM zwm014
              WHERE armazem = l_ltap-lgnum AND
                    estado = ' ' AND
                    mensula LIKE corredor.
      .
*      Não existe mensula para fazer a saída
      IF sy-subrc <> 0.
        DELETE l_ltap INDEX s_index.
        CONTINUE.
      ENDIF.


** Carregar prioridade associada à TO/SU que está na mensula
      CLEAR prioridade_to.
      SELECT MAX( prioridade )
          FROM zwm014 INTO prioridade_to
              WHERE armazem = l_ltap-lgnum AND
                    mensula LIKE corredor.
*                    su = l_ltap-vlenr.

** Origem

      IF p_ultimo_tipo_dep = 'MEN'.
        peso_st_type = 1.
      ELSEIF p_ultimo_tipo_dep = 'TRI'.
        peso_st_type = 10.
      ELSE.
        peso_st_type = 1.
      ENDIF.


      IF l_ltap-tapos <> 2.
        CLEAR l_ltap-prioridade_to.
        l_ltap-prioridade_to =
              ( ( numero_corredores_tri -
            ( abs( p_corredor - l_ltap-vlpla(3) ) ) ) * peso_corredor )
              +
              ( ( numero_profundidade_tri -
              ( abs( p_profundidade - l_ltap-vlpla+4(3) ) ) ) / 10 ) +
              ( numero_nivel_tri -
              ( abs( p_nivel - l_ltap-vlpla+8(2) ) ) )
              * peso_st_type +
               ( prioridade_to * 500 ).
      ENDIF.
** Entradas para trilateral
    ELSEIF l_ltap-queue = p_production_queue OR
           l_ltap-queue = 'QUEUEDIC'.

** Carregar prioridade associada à TO/SU que está na mensula
      CLEAR prioridade_to.
      SELECT SINGLE prioridade FROM zwm014 INTO prioridade_to
                               WHERE armazem = l_ltap-lgnum AND
                                     su = l_ltap-nlenr.


      IF p_ultimo_tipo_dep = 'MEN'.
        peso_st_type = 10.
      ELSEIF p_ultimo_tipo_dep = 'TRI'.
        peso_st_type = 1.
      ELSE.
        peso_st_type = 1.
      ENDIF.

      IF l_ltap-tapos <> 2.
        CLEAR l_ltap-prioridade_to.
        l_ltap-prioridade_to =
              ( ( numero_corredores_tri -
            ( abs( p_corredor - l_ltap-nlpla(3) ) ) ) * peso_corredor )
              +
              ( ( numero_profundidade_tri -
              ( abs( p_profundidade - l_ltap-nlpla+4(3) ) ) ) / 10 ) +
              ( numero_nivel_tri -
              ( abs( p_nivel - l_ltap-nlpla+8(2) ) ) )
              * peso_st_type +
              ( prioridade_to * 500 ).
      ENDIF.
    ENDIF.

    MODIFY l_ltap INDEX s_index.

  ENDLOOP.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.06.2012 15:07:13
*  Motivo: Remove prioridades não desejadas
*--------------------------------------------------------------------*
  PERFORM remove_tapri CHANGING l_ltap[].
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  SORT l_ltap BY tapri DESCENDING "<- DS - Let Down 25/06/2012
                 prioridade DESCENDING
                 prioridade_queue DESCENDING
                 prioridade_to DESCENDING
                 tanum ASCENDING
                 tapos ASCENDING.

  LOOP AT l_ltap.
    IF l_ltap-queue = p_tri_out_queue     OR
       l_ltap-queue = queue_saida_tri_prm OR
       l_ltap-queue = queue_saida_tri_cpk OR
       l_ltap-queue = p_queue_pick_tri.
      CLEAR corredor.


      CONCATENATE l_ltap-vlpla(3) '%' INTO corredor.

      DATA: t_zwm014  LIKE zwm014 OCCURS 0 WITH HEADER LINE,
            wa_zwm014 LIKE zwm014.

      CLEAR: t_zwm014, wa_zwm014.
      REFRESH t_zwm014.

******
      SELECT SINGLE *
          FROM zwm014
              WHERE armazem = l_ltap-lgnum AND
                     su = l_ltap-vlenr.
      IF sy-subrc = 0.
        MOVE sy-uname TO p_nova_to-user_name.
        MOVE l_ltap-tanum TO p_nova_to-to_number.
        MOVE l_ltap-tapos TO p_nova_to-to_item.
        MOVE p_armazem TO p_nova_to-armazem.
        MOVE 'C' TO p_nova_to-status.
        MOVE l_ltap-queue TO p_nova_to-queue.
        MOVE p_equipamento TO p_nova_to-equipamento.
        SELECT SINGLE * FROM zwm014
                        WHERE su = l_ltap-vlenr.
        IF sy-subrc = 0.
          MOVE zwm014-mensula TO p_nova_to-ultimo_bin.
          MOVE 'MEN' TO p_nova_to-ultimo_tipo_dep.
        ENDIF.
        MOVE l_ltap-tipo TO p_tipo_queue.
        EXIT.
      ENDIF.
******
      SELECT  * INTO TABLE t_zwm014
          FROM zwm014
              WHERE armazem = l_ltap-lgnum AND
                    estado = ' ' AND
                    mensula LIKE corredor.
*      Não existe mensula para fazer a saída a to nao pode ser dada
      IF sy-subrc <> 0.
        CONTINUE.
      ELSE.

        DATA: l_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.
        CLEAR l_lagp.
        REFRESH l_lagp.

        SELECT * INTO TABLE l_lagp
            FROM lagp
                FOR ALL ENTRIES IN t_zwm014
                    WHERE lgnum = t_zwm014-armazem AND
                          lgtyp = 'MEN' AND
                          lgpla = t_zwm014-mensula.

        SORT l_lagp BY lgpla.
        LOOP AT l_lagp.
          CLEAR lagp.
          SELECT SINGLE *
              FROM lagp
                  WHERE lgnum = l_ltap-lgnum AND
                        lgtyp = l_ltap-vltyp AND
                        lgpla = l_ltap-vlpla.
          IF l_lagp-brand = lagp-brand.
            EXIT.
          ENDIF.
        ENDLOOP.

        SORT t_zwm014 BY mensula.
        READ TABLE t_zwm014 WITH KEY mensula = l_lagp-lgpla.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        MOVE-CORRESPONDING t_zwm014 TO wa_zwm014.
        wa_zwm014-su = l_ltap-vlenr.
        wa_zwm014-su_transito = l_ltap-vlenr.
        wa_zwm014-estado = 'X'.
        wa_zwm014-bin = l_ltap-vlpla.
        MODIFY zwm014 FROM wa_zwm014.
        COMMIT WORK AND WAIT.

        WHILE 1 = 1.

          SELECT SINGLE *
              FROM zwm014
                  WHERE su = l_ltap-vlenr.
          IF sy-subrc = 0.
            EXIT.
          ENDIF.

        ENDWHILE.

** verificar se neste corredor se está a ocupar
** a terceira mensula do corredor ...se sim colocar
** nas TOs que pertencem ao corredor prioridade máxima

        DATA: n_stress TYPE i.

        SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                        WHERE armazem = l_ltap-lgnum AND
                              mensula LIKE corredor AND
                              estado = 'X'.

        IF corredor(3) EQ '001'.
          n_stress = 2.
        ELSE.
          n_stress = 3.
        ENDIF.

        IF n_mensulas_ocupadas >= n_stress.
          UPDATE zwm014 SET prioridade = 9
                        WHERE armazem = l_ltap-lgnum AND
                              mensula LIKE corredor AND
                              estado = 'X'.
          COMMIT WORK AND WAIT.

        ENDIF.

        MOVE sy-uname TO p_nova_to-user_name.
        MOVE l_ltap-tanum TO p_nova_to-to_number.
        MOVE l_ltap-tapos TO p_nova_to-to_item.
        MOVE p_armazem TO p_nova_to-armazem.
        MOVE 'C' TO p_nova_to-status.
        MOVE l_ltap-queue TO p_nova_to-queue.
        MOVE p_equipamento TO p_nova_to-equipamento.

        IF l_ltap-queue = queue_saida_tri     OR
           l_ltap-queue = queue_saida_tri_prm OR
           l_ltap-queue = queue_saida_tri_cpk OR
           l_ltap-queue = p_queue_pick_tri.
** Simular destino para a zwm011 - SAIDAS DO TRILATERAL
*          SELECT SINGLE * FROM zwm014
*                          WHERE su_transito = l_ltap-vlenr.
          SELECT SINGLE * FROM zwm014
                          WHERE su = l_ltap-vlenr.
          IF sy-subrc = 0.
            MOVE zwm014-mensula TO p_nova_to-ultimo_bin.
            MOVE 'MEN' TO p_nova_to-ultimo_tipo_dep.
          ENDIF.

        ELSE.
          MOVE l_ltap-nlpla TO p_nova_to-ultimo_bin.
          MOVE l_ltap-nltyp TO p_nova_to-ultimo_tipo_dep.
        ENDIF.
        MOVE l_ltap-tipo TO p_tipo_queue.
        EXIT.
      ENDIF.
    ELSEIF l_ltap-queue = p_production_queue OR
           l_ltap-queue = p_dev_inc_queue.
      READ TABLE l_ltap INDEX 1.
      MOVE sy-uname TO p_nova_to-user_name.
      MOVE l_ltap-tanum TO p_nova_to-to_number.
      MOVE l_ltap-tapos TO p_nova_to-to_item.
      MOVE p_armazem TO p_nova_to-armazem.
      MOVE 'C' TO p_nova_to-status.
      MOVE l_ltap-queue TO p_nova_to-queue.
      MOVE p_equipamento TO p_nova_to-equipamento.
      IF l_ltap-queue = queue_saida_tri.
** Simular destino para a zwm011 - SAIDAS DO TRILATERAL
        SELECT SINGLE * FROM zwm014
                        WHERE su_transito = l_ltap-vlenr.
        IF sy-subrc = 0.
          MOVE zwm014-mensula TO p_nova_to-ultimo_bin.
          MOVE 'MEN' TO p_nova_to-ultimo_tipo_dep.
        ENDIF.
      ELSE.
        MOVE l_ltap-nlpla TO p_nova_to-ultimo_bin.
        MOVE l_ltap-nltyp TO p_nova_to-ultimo_tipo_dep.
      ENDIF.
      MOVE l_ltap-tipo TO p_tipo_queue.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_BEST_TO_TRI
*&---------------------------------------------------------------------*
*&      Form  GET_BEST_TO_PCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LTAP  text
*      -->P_CORREDOR  text
*      -->P_ULTIMO_TIPO_DEP  text
*      -->P_ARMAZEM  text
*      -->P_EQUIPAMENTO_ACTUAL  text
*      -->P_PRODUCTION_QUEUE  text
*      -->P_TRI_OUT_QUEUE  text
*      <--P_NOVA_TO  text
*      <--P_TIPO_QUEUE  text
*----------------------------------------------------------------------*
FORM get_best_to_pck  TABLES   p_ltap STRUCTURE l_ltap
                  USING    p_corredor
                           p_profundidade
                           p_nivel
                           p_ultimo_tipo_dep
                           p_armazem
                           p_equipamento
                  CHANGING p_nova_to STRUCTURE zwm011
                           p_tipo_queue.

  DATA : numero_corredores_pck   LIKE zwm001-valor,
         numero_profundidade_pck LIKE zwm001-valor,
         numero_nivel_pck        LIKE zwm001-valor,
         num_max_st_types        LIKE zwm001-valor,
         peso_st_type(2),
         prioridade_to           LIKE zwm014-prioridade,
         peso_corredor           TYPE i VALUE '2'.
*  break-point.
** Número máximo de corredores no depósito REPOSIÇÃO PICKING
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_PRIORIDADES'
                'CORREDORES_PCK'
                numero_corredores_pck.

** Número máximo de PROFUNDIDADE no depósito REPOSIÇÃO PICKING
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_PRIORIDADES'
                'PROFUNDIDADE_PCK'
                numero_profundidade_pck.


** Número máximo de NIVEIS no depósito REPOSIÇÃO PICKING
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_PRIORIDADES'
                'NIVEL_PCK'
                numero_nivel_pck.


** Ordem de prioridades
** 1 - Prioridade associada à queue
** 2 - TO com mais antiguidade
** 3 - Mais próxima do destino da anterior
  LOOP AT l_ltap.

** Peso para o tipo de depósito
    CLEAR peso_st_type.
    IF p_ultimo_tipo_dep = l_ltap-nltyp.
      peso_st_type = 2.
    ELSEIF p_ultimo_tipo_dep <> l_ltap-nltyp.
      peso_st_type = 1.
    ENDIF.

    l_ltap-prioridade_to =
            ( ( numero_corredores_pck -
            ( abs( p_corredor - l_ltap-nlpla(3) ) ) ) * peso_corredor )
            +
            ( numero_profundidade_pck -
            ( abs( p_profundidade - l_ltap-nlpla+4(3) ) ) ) +
            ( numero_nivel_pck -
            ( abs( p_nivel - l_ltap-nlpla+8(2) ) ) ) * peso_st_type.

    MODIFY l_ltap INDEX sy-tabix.
  ENDLOOP.

** RL -> MOD 27.04.2005
** Prioridades
*  SORT l_ltap BY prioridade_queue DESCENDING
*                 prioridade_to DESCENDING
*                 tanum ASCENDING.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.06.2012 15:07:13
*  Motivo: Remove prioridades não desejadas
*--------------------------------------------------------------------*
  PERFORM remove_tapri CHANGING l_ltap[].
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  SORT l_ltap BY tapri DESCENDING" <- DS - LetDown - 25/06/2012
                 prioridade DESCENDING
                 prioridade_queue DESCENDING
                 prioridade_to DESCENDING
                 tanum ASCENDING.
** RL <- MOD 27.04.2005

  READ TABLE l_ltap INDEX 1.

  MOVE sy-uname TO p_nova_to-user_name.
  MOVE l_ltap-tanum TO p_nova_to-to_number.
  MOVE l_ltap-tapos TO p_nova_to-to_item.
  MOVE p_armazem TO p_nova_to-armazem.
  MOVE 'C' TO p_nova_to-status.
  MOVE l_ltap-queue TO p_nova_to-queue.
  MOVE p_equipamento TO p_nova_to-equipamento.
  MOVE l_ltap-nlpla TO p_nova_to-ultimo_bin.
  MOVE l_ltap-nltyp TO p_nova_to-ultimo_tipo_dep.
  MOVE l_ltap-tipo TO p_tipo_queue.



ENDFORM.                    " GET_BEST_TO_PCK
*&---------------------------------------------------------------------*
*&      Form  GET_BEST_TO_CPES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LTAP  text
*      -->P_CORREDOR  text
*      -->P_PROFUNDIDADE  text
*      -->P_NIVEL  text
*      -->P_ULTIMO_TIPO_DEP  text
*      -->P_ARMAZEM  text
*      -->P_EQUIPAMENTO_ACTUAL  text
*      <--P_NOVA_TO  text
*      <--P_TIPO_QUEUE  text
*----------------------------------------------------------------------*
FORM get_best_to_cpes  TABLES   p_ltap STRUCTURE l_ltap
                       USING    p_corredor
                                p_profundidade
                                p_nivel
                                p_ultimo_tipo_dep
                                p_ultimo_bin
                                p_armazem
                                p_equipamento
                       CHANGING p_nova_to STRUCTURE zwm011
                                p_tipo_queue.

  DATA : numero_maximo_cp    LIKE zwm001-valor,
         decremento_cp       LIKE zwm001-valor,
         queue_saida_pkf     LIKE zwm001-valor,
         queue_saida_spf     LIKE zwm001-valor,
         queue_saida_dri     LIKE zwm001-valor,
         queue_saida_tri_men LIKE zwm001-valor,
         queue_pick_tri      LIKE zwm001-valor,
         queue_saida_aut     LIKE zwm001-valor,
         queue_saida_rem     LIKE zwm001-valor,
         queue_saida_bpk     LIKE zwm001-valor,
         queue_abast_bpe     LIKE zwm001-valor,
         stress_conv         LIKE zwm001-valor,
         nivel_stress        TYPE i,
         nivel_stress_001    TYPE i,
         flag_stress         LIKE zwm001-valor,
         prioridade_maxima   TYPE i,
         flag_zona.

  DATA: lv_tabix   TYPE sytabix,
        lv_refnr   TYPE lvs_refnr,
        lv_userass TYPE uname,
        lv_lgtyp   TYPE lgtyp,
        lv_lgpla   TYPE lgpla.


  PERFORM get_parameter
          USING p_armazem
                'GESTAO_PRIORIDADES'
                'DECREMENTO_PRIO_CP'
                decremento_cp.


  PERFORM get_parameter
          USING p_armazem
                'GESTAO_PRIORIDADES'
                'MAXIMA_PRIORIDADE_CP'
                numero_maximo_cp.

** Fila de saida de picking
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_PKF'
                queue_saida_pkf.

** Nova Fila de saida de picking
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_SPF'
                queue_saida_spf.

** Fila de saida de drive-in
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_DRI'
                queue_saida_dri.

** Fila de saida de trilateral - mensula
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_TRI_MEN'
                queue_saida_tri_men.

** Fila de saida de trilateral - mensula - REP
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_FILAS'
                'FILA_PICK_TRI'
                queue_pick_tri.

** Fila de saida de automático -
  PERFORM get_parameter
          USING p_armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_AUT'
                queue_saida_aut.

  PERFORM get_parameter
        USING p_armazem
              'GESTAO_FILAS'
              'FILA_SAIDA_BPK'
              queue_saida_bpk.

** Fila de saida de remontada
  PERFORM get_parameter
        USING p_armazem
              'GESTAO_FILAS'
              'FILA_S_PAL_REMONTADA'
              queue_saida_rem.

** Fila de abastecimento BPE
  PERFORM get_parameter
        USING p_armazem
              'GESTAO_FILAS'
              'FILA_ABAST_BPE'
              queue_abast_bpe.

** Nível de Stress convencional
  PERFORM get_parameter
          USING p_armazem
                'STRESS'
                'CONVENCIONAL'
                stress_conv.
  IF stress_conv IS INITIAL.
    nivel_stress = 2.
  ELSE.
    nivel_stress = stress_conv.
  ENDIF.

** Nível de Stress convencional Corredor 1
  CLEAR stress_conv.
  PERFORM get_parameter
          USING p_armazem
                'STRESS'
                'CONVENCIONAL_001'
                stress_conv.
  IF stress_conv IS INITIAL.
    nivel_stress_001 = 1.
  ELSE.
    nivel_stress_001 = stress_conv.
  ENDIF.

** Ordenação atribuição tarefas
  PERFORM get_parameter
          USING p_armazem
                'STRESS'
                'FLAG'
                flag_stress.

** Avaliação das TO dos Contra-Pesados

** Verificação do sitio onde o contrapesado deixou a última tarefa -
** Posição e Tipo de Depósito
  CLEAR flag_zona.
  SELECT SINGLE lzone FROM lagp INTO lagp-lzone
                      WHERE lgnum = p_armazem AND
                            lgtyp = p_ultimo_tipo_dep AND
                            lgpla = p_ultimo_bin.
  IF sy-subrc = 0.

** Tendo em conta a zona de destino da última tarefa verifica quais as
**zonas de origem mais próximas
    SELECT SINGLE * FROM zwm024
                    WHERE armazem = p_armazem AND
                          zona_destino = lagp-lzone.
    IF sy-subrc = 0.
      flag_zona = 'X'.
    ENDIF.
  ENDIF.

** Ja tenho as zonas mais próximas ... atribuir a prioridade conforme as
** origens das TO q vamos avaliar
  LOOP AT l_ltap.

    lv_tabix = sy-tabix.

    IF flag_zona EQ 'X'.

** Prioridade máxima ... e depois vai decrescendo
      IF l_ltap-zeugn = zwm024-zona_origem1.
        l_ltap-prioridade_to = numero_maximo_cp.
      ELSEIF l_ltap-zeugn = zwm024-zona_origem2.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 1
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem3.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 2
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem4.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 3
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem5.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 4
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem6.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 5
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem7.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 6
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem8.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 7
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem9.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 8
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem10.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 9
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem11.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 10
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem12.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 11
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem13.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 12
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem14.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 13
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem15.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 14
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem16.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 15
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem17.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 16
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem18.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 17
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem19.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 18
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem20.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 19
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem21.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 20
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem22.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 21
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem23.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 22
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem24.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 23
        ).
      ELSEIF l_ltap-zeugn = zwm024-zona_origem25.
        l_ltap-prioridade_to = numero_maximo_cp - ( decremento_cp * 24
        ).
      ENDIF.

    ENDIF.

** Verificar se a TO  q esta a ser avaliada vem de uma mensula ... se
** sim verificar se a mensula está em stress
    CLEAR l_ltap-prioridade_stress.
    IF l_ltap-queue = queue_saida_tri_men.
      CLEAR ltak.
      SELECT SINGLE lznum FROM ltak INTO ltak-lznum
                          WHERE lgnum = l_ltap-lgnum AND
                                tanum = l_ltap-tanum.
      IF sy-subrc = 0.
        PERFORM verifica_mensulas    USING nivel_stress
                                           nivel_stress_001
                                  CHANGING prioridade_maxima.
        l_ltap-prioridade_stress = prioridade_maxima.
      ENDIF.

    ELSEIF l_ltap-queue = queue_pick_tri.
      ltak-lznum = l_ltap-vlenr.
      PERFORM verifica_mensulas    USING nivel_stress
                                         nivel_stress_001
                                CHANGING prioridade_maxima.
      l_ltap-prioridade_stress = prioridade_maxima.
    ENDIF.

    DO 1 TIMES.
      CLEAR: lv_refnr.

      SELECT SINGLE refnr FROM ltak
                          INTO lv_refnr
                          WHERE lgnum = l_ltap-lgnum AND
                                tanum = l_ltap-tanum.

      CHECK NOT lv_refnr IS INITIAL.

      CLEAR: lv_userass.
      SELECT SINGLE userass INTO lv_userass
                            FROM zwm028
                            WHERE lgnum = l_ltap-lgnum AND
                                  refnr = lv_refnr AND
                                  remessa = ''.
      CHECK NOT lv_userass IS INITIAL.

      IF lv_userass EQ sy-uname.
        l_ltap-upri = 999.
      ELSEIF lv_userass <> lv_tabix.
        DELETE  l_ltap INDEX lv_tabix.
      ENDIF.

    ENDDO.

    MODIFY l_ltap INDEX lv_tabix.

  ENDLOOP.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.06.2012 15:07:13
*  Motivo: Remove prioridades não desejadas
*--------------------------------------------------------------------*
  PERFORM remove_tapri CHANGING l_ltap[].
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


  IF flag_stress IS INITIAL.
    SORT l_ltap BY upri DESCENDING
                   tapri DESCENDING "<- DS - Let Down 25/06/2012
                   prioridade DESCENDING
                   prioridade_stress DESCENDING  " <- 27/03/2007
                   prioridade_queue DESCENDING
                   prioridade_to DESCENDING
                   pos_pulmao ASCENDING
                   vlpla ASCENDING
                   tanum ASCENDING
                   tapos ASCENDING.
  ELSE.
    SORT l_ltap BY tapri DESCENDING "<- DS - Let Down 25/06/2012
                   prioridade DESCENDING
                   prioridade_queue DESCENDING
                   prioridade_stress DESCENDING  " <- 27/03/2007
                   prioridade_to DESCENDING
                   pos_pulmao ASCENDING
                   vlpla ASCENDING
                   tanum ASCENDING
                   tapos ASCENDING.
  ENDIF.

  READ TABLE l_ltap INDEX 1.

  MOVE sy-uname      TO p_nova_to-user_name.
  MOVE l_ltap-tanum  TO p_nova_to-to_number.
  MOVE l_ltap-tapos  TO p_nova_to-to_item.
  MOVE p_armazem     TO p_nova_to-armazem.
  MOVE 'C'           TO p_nova_to-status.
  MOVE l_ltap-queue  TO p_nova_to-queue.
  MOVE p_equipamento TO p_nova_to-equipamento.
  MOVE l_ltap-nlpla  TO p_nova_to-ultimo_bin.
  MOVE l_ltap-nltyp  TO p_nova_to-ultimo_tipo_dep.

** SAIDAS DO PICKING/SAIDAS DRIVE-IN
  IF l_ltap-queue = queue_saida_pkf     OR
     l_ltap-queue = queue_saida_spf     OR
     l_ltap-queue = queue_saida_dri     OR
     l_ltap-queue = queue_saida_tri_men OR
     l_ltap-queue = queue_saida_aut     OR
     l_ltap-queue = queue_saida_rem     OR
     l_ltap-queue = queue_saida_bpk.

    CLEAR : zwm026,
            zwm028.
** Para carregar o número adicional = SU
    SELECT SINGLE * FROM ltak
                    WHERE lgnum = l_ltap-lgnum AND
                          tanum = l_ltap-tanum.
    IF sy-subrc = 0.
** Verificar se TO já tem grupo
      IF NOT ltak-refnr IS INITIAL.
        MOVE ltak-refnr TO zwm026-grupo.
      ELSE.
        SELECT SINGLE grupo FROM zwm026 INTO zwm026-grupo
                                  WHERE armazem = ltak-lgnum AND
                                        sscc = ltak-lznum.
      ENDIF.
      IF NOT zwm026-grupo IS INITIAL.
        SELECT SINGLE * FROM zwm028
                        WHERE lgnum = ltak-lgnum AND
                              refnr = zwm026-grupo.
        IF sy-subrc = 0.
          IF NOT zwm028-st_ppk IS INITIAL.
** DESCOBRIR POSIÇÃO dentro da zona - PRE PICKING - FALTA
            p_nova_to-ultimo_bin = zwm028-pre_pick.
            p_nova_to-ultimo_tipo_dep = zwm028-st_ppk.
          ELSEIF NOT zwm028-st_pul IS INITIAL.

            IF zwm028-st_pul = 'PUA'.
              p_nova_to-ultimo_bin = zwm028-pulmao1.
              p_nova_to-ultimo_tipo_dep = zwm028-st_pul.

            ELSE.
              " No primeiro pulmão
              IF zwm028-paletes_pulmao <= 17.
                p_nova_to-ultimo_bin = zwm028-pulmao1.
                p_nova_to-ultimo_tipo_dep = zwm028-st_pul.
                " No segundo pulmão
              ELSEIF zwm028-paletes_pulmao > 17.
                p_nova_to-ultimo_bin = zwm028-pulmao2.
                p_nova_to-ultimo_tipo_dep = zwm028-st_pul.
              ENDIF.
            ENDIF.
          ELSEIF NOT zwm028-st_dck IS INITIAL.
            p_nova_to-ultimo_bin = zwm028-porta.
            p_nova_to-ultimo_tipo_dep = zwm028-st_dck.
          ENDIF.
        ENDIF.
      ELSEIF zwm026-grupo IS INITIAL.
** Grupo composto por paletes completas apenas
        SELECT SINGLE * FROM ltap
                        WHERE lgnum = ltak-lgnum AND
                              vlenr = ltak-lznum AND
                              pvqui = 'X' AND
                              pquit = ' '.
        IF sy-subrc = 0.
          SELECT SINGLE refnr FROM ltak INTO zwm026-grupo
                              WHERE lgnum = ltap-lgnum AND
                                    tanum = ltap-tanum.
          IF NOT zwm026-grupo IS INITIAL.
            SELECT SINGLE * FROM zwm028
                            WHERE lgnum = ltak-lgnum AND
                                  refnr = zwm026-grupo.
            IF sy-subrc = 0.
              IF NOT zwm028-st_ppk IS INITIAL.
                p_nova_to-ultimo_bin = zwm028-pre_pick.
                p_nova_to-ultimo_tipo_dep = zwm028-st_ppk.
              ELSEIF NOT zwm028-st_pul IS INITIAL.

                IF zwm028-st_pul = 'PUA'.
                  p_nova_to-ultimo_bin = zwm028-pulmao1.
                  p_nova_to-ultimo_tipo_dep = zwm028-st_pul.
                ELSE.
                  IF zwm028-paletes_pulmao <= 17.
                    p_nova_to-ultimo_bin = zwm028-pulmao1.
                    p_nova_to-ultimo_tipo_dep = zwm028-st_pul.
                  ELSEIF zwm028-paletes_pulmao > 17.
                    p_nova_to-ultimo_bin = zwm028-pulmao2.
                    p_nova_to-ultimo_tipo_dep = zwm028-st_pul.
                  ENDIF.
                ENDIF.

              ELSEIF NOT zwm028-st_dck IS INITIAL.
                p_nova_to-ultimo_bin = zwm028-porta.
                p_nova_to-ultimo_tipo_dep = zwm028-st_dck.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
*      ENDIF.
    ENDIF.
*  ENDIF.
*ENDIF.

  ELSE.
    MOVE l_ltap-nlpla TO p_nova_to-ultimo_bin.
    MOVE l_ltap-nltyp TO p_nova_to-ultimo_tipo_dep.
  ENDIF.

** Entradas Paletes no Automático Torres Novas (WCS)
  IF p_nova_to-ultimo_tipo_dep = 'PUA' OR l_ltap-nltyp = 'EAU'.

    IF l_ltap-queue = queue_abast_bpe.
      lv_lgtyp = 'BPE'.

    ELSEIF  p_nova_to-ultimo_tipo_dep = 'PUA'.
      lv_lgtyp = 'PUA'.

    ELSEIF l_ltap-nltyp = 'EAU'.
      lv_lgtyp = l_ltap-vltyp.
    ENDIF.

    CALL FUNCTION 'ZWM_GET_MESA_WCS'
      EXPORTING
        i_lgnum = l_ltap-lgnum
        i_lgtyp = lv_lgtyp
        i_type  = 'E'
      IMPORTING
        e_mesa  = lv_lgpla
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.

    IF lv_lgpla IS NOT INITIAL.
      IF lv_lgtyp = 'PUA'.
        p_nova_to-ultimo_tipo_dep = 'PUA'.
      ELSE.
        p_nova_to-ultimo_tipo_dep = 'EAU'.
      ENDIF.

      p_nova_to-ultimo_bin = lv_lgpla.
    ENDIF.

  ENDIF.


  MOVE l_ltap-tipo TO p_tipo_queue.

ENDFORM.                    " GET_BEST_TO_CPES
*&---------------------------------------------------------------------*
*&      Form  confirma_to_parcialmente
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirma_to_parcialmente USING p_lgpla.

  DATA: lv_queue_tri_prm TYPE lrf_queue.
  DATA: ls_ltak          TYPE ltak.
  DATA: ls_ltap          TYPE ltap.

** Fazer lock antes de actualizar a tabela das mensulas
**********************************************************************
  SELECT SINGLE * FROM zwm014
                WHERE armazem = xuser-lgnum AND
                      mensula = lgpla.

  CHECK sy-subrc = 0.

  SELECT SINGLE *
      FROM ltap INTO ls_ltap
          WHERE lgnum = xuser-lgnum AND
                nlenr = zwm020-p2   AND
                pquit = ''.

  IF sy-subrc = 0.
    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = ls_ltap-lgnum
      AND   tanum = ls_ltap-tanum.
  ENDIF.

  PERFORM get_parameter
           USING xuser-lgnum
                 'GESTAO_FILAS'
                 'FILA_S_TRI_PAL_PRM'
                 lv_queue_tri_prm.

** Confirmar OT
**********************************************************************
  PERFORM free_mensula.

  CLEAR : return_msg.
  REFRESH : return_msg.

  CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
    EXPORTING
      armazem              = xuser-lgnum
      confirm_type         = 'P'
    TABLES
      return_msg           = return_msg
    CHANGING
      to                   = to_ret
    EXCEPTIONS
      confirm_type_error   = 1
      to_not_found         = 2
      to_already_confirmed = 3
      to_already_picked    = 4
      to_not_picked        = 5
      wrong_su             = 6
      missing_su           = 7
      error                = 8
      OTHERS               = 9.
  IF sy-subrc <> 0.

    READ TABLE return_msg INDEX 1.
    IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1
          message_var2   = return_msg-msgv2
          message_var3   = return_msg-msgv3.

      CLEAR: cursorfield, su, ok_code_0001.
      MOVE 'SU' TO cursorfield.
      EXIT.

*        SET SCREEN '0000'.LEAVE SCREEN.
*        CLEAR : cursorfield.
    ENDIF.

  ELSE.

    " PROCESSAMENTO OK
    " actualizar tabela ZWM011 com STATUS T pq é TO
    " de putaway de mesa -> mensula
    CALL FUNCTION 'ZWM_MODIFY_ZWM011'
      EXPORTING
        armazem            = xuser-lgnum
        to_number          = to_ret
        to_item            = '0001'
        status             = 'P'
      EXCEPTIONS
        error_update_table = 1
        OTHERS             = 2.

** Palete para o PRM
    IF ls_ltak-queue = lv_queue_tri_prm.

      su  = ls_ltap-vlenr.
      su1 = su.

      CONCATENATE ls_ltap-nltyp ls_ltap-nlpla INTO bin_output_d SEPARATED BY space.

      " Palete remontada para o PRM, Arrumar primeiro palete de cima
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '364'.
    ENDIF.

    MOVE 'BIN_INPUT' TO cursorfield.

    CLEAR : return_msg.
    REFRESH : return_msg.
  ENDIF.

ENDFORM.                    " confirma_to_parcialmente
*&---------------------------------------------------------------------*
*&      Form  confirma_to_total
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirma_to_total USING p_lgpla p_lgtyp.

  CLEAR : confirm_type,
          lgtyp_des,
          lgpla_des,
          corredor,
          n_mensulas_ocupadas.

** 1 - Confirmar TO totalmente
** 2 - Actualizar status da zwm011
** 3 - Actualizar mensula (su_transito)
** 4 - Actualizar número de paletes no pulmão

  IF p_lgtyp = 'DRI' OR p_lgtyp = 'BLK'.
    confirm_type = 'B'.
  ELSE.
    confirm_type = 'T'.
    to_ret2 = to_ret.
  ENDIF.

  IF p_lgtyp <> 'DRI' AND p_lgtyp <> 'BLK'.

    CLEAR : return_msg.
    REFRESH : return_msg.
** Confirmar totalmente
    CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
      EXPORTING
        armazem              = xuser-lgnum
        confirm_type         = confirm_type
      TABLES
        return_msg           = return_msg
      CHANGING
        to                   = to_ret2
      EXCEPTIONS
        confirm_type_error   = 1
        to_not_found         = 2
        to_already_confirmed = 3
        to_already_picked    = 4
        to_not_picked        = 5
        wrong_su             = 6
        missing_su           = 7
        error                = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
      READ TABLE return_msg INDEX 1.
      IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1
            message_var2   = return_msg-msgv2
            message_var3   = return_msg-msgv3.

        CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
               cursorfield, ok_code_0001.
        MOVE 'BIN_INPUT' TO cursorfield.
        EXIT.
      ENDIF.
    ELSE.
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = '0001'
          status             = 'T'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.

    ENDIF.
*    ENDIF.
  ENDIF.
*** actualizar tabela ZWM011 com STATUS T
*  CALL FUNCTION 'ZWM_MODIFY_ZWM011'
*    EXPORTING
*      armazem            = xuser-lgnum
*      to_number          = to_ret
*      to_item            = '0001'
*      status             = 'T'
*    EXCEPTIONS
*      error_update_table = 1
*      OTHERS             = 2.
** Só actualiza Mensulas se a TO for proveniente de lá
  IF p_lgtyp <> 'DRI' AND p_lgtyp <> 'BLK'.

    DATA wa_zwm014 LIKE zwm014.

    CLEAR: zwm014, wa_zwm014.
    SELECT SINGLE * FROM zwm014
                    WHERE armazem = xuser-lgnum AND
                          mensula = p_lgpla.
    IF sy-subrc = 0.

* Fazer lock antes de actualizar a tabela das mensulas
      DO.
        CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'ZWM014'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc = 0.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.

** Verificar se o corredor depois da retirada da SU em análise
** ainda deve manter a prioridade máxima
      CLEAR : corredor,
              n_mensulas_ocupadas.
      CONCATENATE zwm014-mensula(3) '%' INTO corredor.
      SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                      WHERE armazem = xuser-lgnum AND
                            mensula LIKE corredor AND
                            estado = 'X'.
      IF n_mensulas_ocupadas < 3.
        UPDATE zwm014 SET prioridade = ' '
                      WHERE armazem = xuser-lgnum AND
                            mensula LIKE corredor AND
                            estado = 'X'.
        COMMIT WORK.
        CLEAR : n_mensulas_ocupadas, corredor.
      ENDIF.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'ZWM014'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

    ENDIF.
  ENDIF.
** Se o destino for a PORTA ... o processo necessita
** de ser fechado ... é preciso confirmar a TO da delivery
** por completo
  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = bin_output_d
    IMPORTING
      lgtyp = lgtyp_des
      lgpla = lgpla_des.
** Actualizar numero de paletes carregadas no carro no caso de ser
** carga directa, bem como fazer o embalamento na remessa
** Caso Contrario
** Actualizar número de paletes no pulmão

** Destino = PORTA
  IF lgtyp_des = 'DCK'.
    PERFORM confirma_to_delivery USING p_lgtyp.
    CLEAR : lgtyp_des,
            lgpla_des,
            cursorfield.

** Destino = Gravítica Automático
  ELSEIF lgtyp_des = 'PUA'.
    CLEAR : lgtyp_des,
            lgpla_des,
            cursorfield.
    EXIT.

  ELSE.
    IF p_lgtyp = 'DRI' OR p_lgtyp = 'BLK'.
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = '0001'
          status             = 'T'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.
    ENDIF.

    PERFORM actualiza_paletes_pulmao.
    CLEAR : to_ret2, to_ret3, to3.
  ENDIF.

ENDFORM.                    " confirma_to_total
*&---------------------------------------------------------------------*
*&      Form  confirma_to_crd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirma_to_crd USING p_lgpla p_lgtyp.

  CLEAR : confirm_type,
          lgtyp_des,
          lgpla_des,
          corredor,
          n_mensulas_ocupadas.

** 1 - Confirmar TO totalmente
** 2 - Actualizar status da zwm011
** 3 - Actualizar número de paletes no pulmão


  confirm_type = 'P'.

  CLEAR : return_msg.
  REFRESH : return_msg.
** Confirmar totalmente
  CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
    EXPORTING
      armazem              = xuser-lgnum
      confirm_type         = confirm_type
    TABLES
      return_msg           = return_msg
    CHANGING
      to                   = to_ret
    EXCEPTIONS
      confirm_type_error   = 1
      to_not_found         = 2
      to_already_confirmed = 3
      to_already_picked    = 4
      to_not_picked        = 5
      wrong_su             = 6
      missing_su           = 7
      error                = 8
      OTHERS               = 9.
  IF sy-subrc <> 0.
    READ TABLE return_msg INDEX 1.
    IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1
          message_var2   = return_msg-msgv2
          message_var3   = return_msg-msgv3.

      CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
             ok_code_0001.
      MOVE 'BIN_INPUT' TO cursorfield.
      EXIT.
*      CLEAR : cursorfield, bin_output_d.
*      SET SCREEN '0000'.LEAVE SCREEN.

    ENDIF.
  ELSE.
** actualizar tabela ZWM011 com STATUS T
    CALL FUNCTION 'ZWM_MODIFY_ZWM011'
      EXPORTING
        armazem            = xuser-lgnum
        to_number          = to_ret
        to_item            = '0001'
        status             = 'T'
      EXCEPTIONS
        error_update_table = 1
        OTHERS             = 2.

** Se o destino for a PORTA ... o processo necessita
** de ser fechado ... é preciso confirmar a TO da delivery
** por completo
    CALL FUNCTION 'ZWM_SPLIT_BIN'
      EXPORTING
        bin   = bin_output_d
      IMPORTING
        lgtyp = lgtyp_des
        lgpla = lgpla_des.
** Actualizar numero de paletes carregadas no carro no caso de ser
** carga directa, bem como fazer o embalamento na remessa
** Caso Contrario
** Actualizar número de paletes no pulmão
** Destino = PORTA
    IF lgtyp_des = 'DCK'.
      PERFORM confirma_to_delivery USING p_lgtyp.
      CLEAR : lgtyp_des,
              lgpla_des,
              cursorfield.
    ELSE.
      PERFORM actualiza_paletes_pulmao.
      CLEAR : to_ret2, to_ret3, to3.
    ENDIF.
  ENDIF.

ENDFORM.                    " confirma_to_crd
*&---------------------------------------------------------------------*
*&      Form  valida_saida_dri
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_saida_dri USING p_lgtyp p_lgpla.

  DATA : bin_incidencia(14).
  CLEAR : bin_incidencia, lqua, zwm020.

  DATA: lv_queue_prm TYPE lrf_queue.
  DATA: lv_tapos     TYPE tapos.

  DATA: lt_ltap_rem TYPE TABLE OF ltap,
        lt_ltak_rem TYPE TABLE OF ltak.

  DATA: ls_ltap_rem TYPE ltap,
        ls_ltak_rem TYPE ltak,
        ls_ltak     TYPE ltak.

  DATA: lv_2spart TYPE flag.

** Verificar se a SU lida existe na posição
**********************************************************************
  SELECT SINGLE * FROM lqua
                  WHERE lgnum = xuser-lgnum AND
                        lgtyp = p_lgtyp AND
                        lgpla = p_lgpla AND
                        lenum = su.
  IF sy-subrc <> 0.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = p_lgtyp
        lgpla = p_lgpla
      IMPORTING
        bin   = bin_incidencia.

    CLEAR : text,incidencia.
    WRITE su TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '119'
        message_var1   = text.
    CLEAR: su, ok_code_0001.
    MOVE 'SU' TO cursorfield.
  ELSE.


** Fila palete remontadas p/ o PRM
    PERFORM get_parameter
            USING xuser-lgnum
                  'GESTAO_FILAS'
                  'FILA_S_DRI_PAL_PRM'
                  lv_queue_prm.

** OT
    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = xuser-lgnum
      AND   tanum = to_ret.

** Palete Remontada
**********************************************************************
    CLEAR zwm020.

    SELECT SINGLE *
        FROM zwm020
            WHERE armazem = xuser-lgnum AND
                  ( p1 = su OR p2 = su ).

    IF sy-subrc = 0.

** Arrumação no PRM - Transfere as duas paletes
      IF bin_output_d(3) = 'PRM' AND ls_ltak-queue = lv_queue_prm.

        CLEAR : return_msg.
        REFRESH : return_msg.

        lv_tapos = '0001'.

        " Confirma a palete de baixo
        CALL FUNCTION 'ZWM_CONFIRM_TO'
          EXPORTING
            armazem              = xuser-lgnum
            confirm_type         = 'P'
            su                   = zwm020-p2
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = to_ret
            to_item              = lv_tapos
          EXCEPTIONS
            confirm_type_error   = 1
            to_not_found         = 2
            to_already_confirmed = 3
            to_already_picked    = 4
            to_not_picked        = 5
            wrong_su             = 6
            missing_su           = 7
            error                = 8
            OTHERS               = 9.

        IF sy-subrc <> 0.
          READ TABLE return_msg INDEX 1.
          IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = return_msg-msgid
                message_lang   = sy-langu
                message_type   = return_msg-msgtyp
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1
                message_var2   = return_msg-msgv2
                message_var3   = return_msg-msgv3.

            CLEAR : cursorfield, su, su1, ok_code_0001.
            MOVE 'SU' TO cursorfield.
            EXIT.
*            SET SCREEN '0000'.LEAVE SCREEN.
          ENDIF.
        ENDIF.

        " Confirma a palete de cima
        lv_tapos = '0002'.

        CALL FUNCTION 'ZWM_CONFIRM_TO'
          EXPORTING
            armazem              = xuser-lgnum
            confirm_type         = 'P'
            su                   = zwm020-p1
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = to_ret
            to_item              = lv_tapos
          EXCEPTIONS
            confirm_type_error   = 1
            to_not_found         = 2
            to_already_confirmed = 3
            to_already_picked    = 4
            to_not_picked        = 5
            wrong_su             = 6
            missing_su           = 7
            error                = 8
            OTHERS               = 9.

        IF sy-subrc <> 0.
          READ TABLE return_msg INDEX 1.
          IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = return_msg-msgid
                message_lang   = sy-langu
                message_type   = return_msg-msgtyp
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1
                message_var2   = return_msg-msgv2
                message_var3   = return_msg-msgv3.

            CLEAR : cursorfield, su, su1, ok_code_0001.
            MOVE 'SU' TO cursorfield.
            EXIT.
*            SET SCREEN '0000'.LEAVE SCREEN.
          ENDIF.
        ENDIF.

        CLEAR ltap.
        SELECT SINGLE *
            FROM ltap
                WHERE lgnum = xuser-lgnum AND
                      tanum = to_ret AND
                      vlenr = zwm020-p2.

        IF sy-subrc = 0.
          CALL FUNCTION 'ZWM_MODIFY_ZWM011'
            EXPORTING
              armazem            = xuser-lgnum
              to_number          = to_ret
              to_item            = ltap-tapos
              status             = 'P'
            EXCEPTIONS
              error_update_table = 1
              OTHERS             = 2.

          MOVE zwm020-p2 TO su.
          MOVE zwm020-p2 TO su1.

          CONCATENATE ltap-nltyp ltap-nlpla INTO bin_output_d SEPARATED BY space.

          item_ret = ltap-tapos.
        ENDIF.

        " Palete remontada para o PRM, Arrumar primeiro palete de cima
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '364'.

        MOVE 'BIN_INPUT' TO cursorfield.

        CLEAR : return_msg.
        REFRESH : return_msg.

        EXIT.

** Outras arrumações
      ELSE.

        IF zwm020-p1 = su.

          MOVE su TO su1.
          CLEAR : return_msg.
          REFRESH : return_msg.

          " Verifica se confirma a saida das duas paletes ou se confirma apenas
          "  uma e confirma outra to com a palete de cima para a zona de paletes
          "  remontadas (PRM) só com uma palete

          " Confirma a palete de baixo
          CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
            EXPORTING
              armazem              = xuser-lgnum
              confirm_type         = 'P'
              su                   = su
            TABLES
              return_msg           = return_msg
            CHANGING
              to                   = to_ret
            EXCEPTIONS
              confirm_type_error   = 1
              to_not_found         = 2
              to_already_confirmed = 3
              to_already_picked    = 4
              to_not_picked        = 5
              wrong_su             = 6
              missing_su           = 7
              error                = 8
              OTHERS               = 9.
          IF sy-subrc <> 0.
            READ TABLE return_msg INDEX 1.
            IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = return_msg-msgid
                  message_lang   = sy-langu
                  message_type   = return_msg-msgtyp
                  message_number = return_msg-msgnr
                  message_var1   = return_msg-msgv1
                  message_var2   = return_msg-msgv2
                  message_var3   = return_msg-msgv3.

              CLEAR : cursorfield, su, su1, ok_code_0001.
              MOVE 'SU' TO cursorfield.
              EXIT.
*            SET SCREEN '0000'.LEAVE SCREEN.

            ENDIF.
          ELSE.

            " Se for saida do drive-in tem de se alterar a tabela Zwm011 com o item
            " que foi desdobrado com a SU correcta
            CLEAR ltap.
            SELECT SINGLE *
                FROM ltap
                    WHERE lgnum = xuser-lgnum AND
                          tanum = to_ret AND
                          vlenr = su.


            CALL FUNCTION 'ZWM_MODIFY_ZWM011'
              EXPORTING
                armazem            = xuser-lgnum
                to_number          = to_ret
                to_item            = ltap-tapos
                status             = 'P'
              EXCEPTIONS
                error_update_table = 1
                OTHERS             = 2.

            " Verifica se confirma tb a palete de cima para a remessa ou se cria
            " uma outra TO para a zona PRM
            CLEAR: ltap, lqua.
            SELECT SINGLE *
                FROM ltap
                    WHERE lgnum = xuser-lgnum AND
                          tanum = to_ret AND
                          tapos = '0001'.

            SELECT SINGLE *
                FROM lqua
                    WHERE lgnum = xuser-lgnum AND
                          lenum = zwm020-p2.


            " Valida Picking em 2 Passos (no caso de parcial, as OT's já estão desdobradas)
            CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
              EXPORTING
                i_lgnum  = xuser-lgnum
                i_tanum  = to_ret
              IMPORTING
                e_2spart = lv_2spart
              EXCEPTIONS
                error    = 1
                OTHERS   = 2.


            " Confirma a palete de cima para a remessa
            DATA qtd_rest LIKE ltap-vsolm.
            qtd_rest = ltap-vsolm - ltap-vistm.

            IF lv_2spart EQ abap_true.
              SELECT SINGLE * FROM ltak
                              INTO ls_ltak
                              WHERE lgnum = xuser-lgnum AND
                                    tanum = ltap-tanum.

              DO 1 TIMES.
                CLEAR ls_ltap_rem.
                SELECT * FROM ltap
                         INTO TABLE lt_ltap_rem
                         WHERE lgnum = xuser-lgnum AND
                               pquit = ' ' AND
                               vltyp = ltap-vltyp AND
                               vlpla = ltap-vlpla AND
                               vlenr = ' '.
                CHECK sy-subrc EQ 0.

                SELECT * FROM ltak
                         INTO TABLE lt_ltak_rem
                         FOR ALL ENTRIES IN lt_ltap_rem
                         WHERE lgnum = lt_ltap_rem-lgnum AND
                               tanum = lt_ltap_rem-tanum.
                CHECK sy-subrc EQ 0.

                LOOP AT lt_ltak_rem INTO ls_ltak_rem WHERE benum = ls_ltak-benum.
                  CLEAR: ls_ltap_rem.
                  READ TABLE lt_ltap_rem
                        INTO ls_ltap_rem
                        WITH KEY tanum = ls_ltak_rem-tanum.
                  IF sy-subrc EQ 0.
                    EXIT.
                  ENDIF.
                ENDLOOP.
              ENDDO.

              DO 60 TIMES.
                IF ls_ltap_rem-tanum IS INITIAL.
                  EXIT.
                ENDIF.

                CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
                  EXPORTING
                    armazem              = xuser-lgnum
                    confirm_type         = 'P'
                    su                   = zwm020-p2
                  TABLES
                    return_msg           = return_msg
                  CHANGING
                    to                   = ls_ltap_rem-tanum
                  EXCEPTIONS
                    confirm_type_error   = 1
                    to_not_found         = 2
                    to_already_confirmed = 3
                    to_already_picked    = 4
                    to_not_picked        = 5
                    wrong_su             = 6
                    missing_su           = 7
                    error                = 8
                    OTHERS               = 9.
                IF sy-subrc = 0.
                  EXIT.
                ELSE.
                  WAIT UP TO 1 SECONDS.
                ENDIF.
              ENDDO.
            ELSEIF qtd_rest >= lqua-verme.

              CLEAR : return_msg.
              REFRESH : return_msg.

              DO 60 TIMES.
                CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
                  EXPORTING
                    armazem              = xuser-lgnum
                    confirm_type         = 'P'
                    su                   = zwm020-p2
                  TABLES
                    return_msg           = return_msg
                  CHANGING
                    to                   = ltap-tanum
                  EXCEPTIONS
                    confirm_type_error   = 1
                    to_not_found         = 2
                    to_already_confirmed = 3
                    to_already_picked    = 4
                    to_not_picked        = 5
                    wrong_su             = 6
                    missing_su           = 7
                    error                = 8
                    OTHERS               = 9.
                IF sy-subrc = 0.
                  EXIT.
                ELSE.
                  WAIT UP TO 1 SECONDS.
                ENDIF.
              ENDDO.
            ELSE.

              "  tem de encontrar a to que ja foi criada para o prm e confirma com a Ot de baixo
              CLEAR ltap.
              SELECT SINGLE *
                  FROM ltap
                      WHERE lgnum = xuser-lgnum AND
                            pquit = ' ' AND
                            vltyp = lqua-lgtyp AND
                            vlpla = lqua-lgpla AND
                            ( nltyp = 'PRM' OR nltyp = 'INC' ) AND
                            pvqui = ' ' AND
                            werks = lqua-werks AND
                            lgort = lqua-lgort AND
                            matnr = lqua-matnr AND
                            charg = lqua-charg.


              CLEAR : return_msg.
              REFRESH : return_msg.

              DO 60 TIMES.
                IF ltap-tanum IS INITIAL.
                  EXIT.
                ENDIF.

                CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
                  EXPORTING
                    armazem              = xuser-lgnum
                    confirm_type         = 'P'
                    su                   = zwm020-p2
                  TABLES
                    return_msg           = return_msg
                  CHANGING
                    to                   = ltap-tanum
                  EXCEPTIONS
                    confirm_type_error   = 1
                    to_not_found         = 2
                    to_already_confirmed = 3
                    to_already_picked    = 4
                    to_not_picked        = 5
                    wrong_su             = 6
                    missing_su           = 7
                    error                = 8
                    OTHERS               = 9.
                IF sy-subrc = 0.
                  EXIT.
                ELSE.
                  WAIT UP TO 1 SECONDS.
                ENDIF.
              ENDDO.

            ENDIF.

            MOVE 'BIN_INPUT' TO cursorfield.

            CLEAR : return_msg.
            REFRESH : return_msg.

          ENDIF.
        ELSEIF zwm020-p2 = su.
          CLEAR : text.
          WRITE su TO text LEFT-JUSTIFIED.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '153'
              message_var1   = text.
          CLEAR: su, ok_code_0001.
          MOVE 'SU' TO cursorfield.
        ENDIF.

      ENDIF.

** Palete Simples
**********************************************************************
    ELSE.
      MOVE su TO su1.

** Faz pick à TO
      CLEAR : return_msg.
      REFRESH : return_msg.

      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'P'
          su                   = su
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = to_ret
        EXCEPTIONS
          confirm_type_error   = 1
          to_not_found         = 2
          to_already_confirmed = 3
          to_already_picked    = 4
          to_not_picked        = 5
          wrong_su             = 6
          missing_su           = 7
          error                = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
        READ TABLE return_msg INDEX 1.
        IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = return_msg-msgid
              message_lang   = sy-langu
              message_type   = return_msg-msgtyp
              message_number = return_msg-msgnr
              message_var1   = return_msg-msgv1
              message_var2   = return_msg-msgv2
              message_var3   = return_msg-msgv3.

          CLEAR : cursorfield, su, su1, ok_code_0001.
          MOVE 'SU' TO cursorfield.
          EXIT.
*          SET SCREEN '0000'.LEAVE SCREEN.
*          CLEAR : cursorfield.
        ENDIF.
      ELSE.
** Se for saida do drive-in tem de se alterar a tabela Zwm011 com o item
** que foi desdobrado com a SU correcta
        CLEAR ltap.
        SELECT SINGLE *
            FROM ltap
                WHERE lgnum = xuser-lgnum AND
                      tanum = to_ret AND
                      vlenr = su.

        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
          EXPORTING
            armazem            = xuser-lgnum
            to_number          = to_ret
            to_item            = ltap-tapos
            status             = 'P'
          EXCEPTIONS
            error_update_table = 1
            OTHERS             = 2.

        MOVE 'BIN_INPUT' TO cursorfield.

        CLEAR : return_msg.
        REFRESH : return_msg.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " valida_saida_dri

*&---------------------------------------------------------------------*
*&      Form  valida_saida_dri_to_rep
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_saida_dri_to_rep USING p_lgtyp p_lgpla.

  DATA : bin_incidencia(14).
  CLEAR : bin_incidencia.

  DATA: lv_tapos TYPE tapos.

  break roffd.
** Verificar se a SU lida existe na posição
  SELECT SINGLE * FROM lqua
                  WHERE lgnum = xuser-lgnum AND
                        lgtyp = p_lgtyp AND
                        lgpla = p_lgpla AND
                        lenum = su.
  IF sy-subrc <> 0.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = p_lgtyp
        lgpla = p_lgpla
      IMPORTING
        bin   = bin_incidencia.

    CLEAR : text,incidencia.
    WRITE su TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '119'
        message_var1   = text.
    CLEAR: su, ok_code_0001.
    MOVE 'SU' TO cursorfield.
  ELSE.
** Verificar se é uma palete remontada e se esta a ler o sscc de baixo
    SELECT SINGLE *
        FROM zwm020
            WHERE armazem = xuser-lgnum AND
                  ( p1 = su OR p2 = su ).
** Palete Remontada
    IF sy-subrc = 0.
      IF zwm020-p1 = su.

        MOVE su TO su1.
        CLEAR : return_msg.
        REFRESH : return_msg.

** Se é uma palete remontada confirma as duas

        CALL FUNCTION 'ZWM_CONFIRM_TO'
          EXPORTING
            armazem              = xuser-lgnum
            confirm_type         = 'P'
            su                   = su
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = to_ret
            to_item              = item_ret
          EXCEPTIONS
            confirm_type_error   = 1
            to_not_found         = 2
            to_already_confirmed = 3
            to_already_picked    = 4
            to_not_picked        = 5
            wrong_su             = 6
            missing_su           = 7
            error                = 8
            OTHERS               = 9.
        IF sy-subrc <> 0.

          READ TABLE return_msg INDEX 1.
          IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = return_msg-msgid
                message_lang   = sy-langu
                message_type   = return_msg-msgtyp
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1
                message_var2   = return_msg-msgv2
                message_var3   = return_msg-msgv3.

            CLEAR: cursorfield, su, su1, ok_code_0001.
            MOVE 'SU' TO cursorfield.
            EXIT.
*            SET SCREEN '0000'.LEAVE SCREEN.
*            CLEAR : cursorfield.
          ENDIF.
        ELSE.
** Se for saida do dirve-in tem de se alterar a tabela Zwm011 com o item
** que foi desdobrado com a SU correcta
          SELECT SINGLE *
              FROM ltap
                  WHERE lgnum = xuser-lgnum AND
                        tanum = to_ret AND
                        vlenr = su.

          IF sy-subrc = 0.
            CALL FUNCTION 'ZWM_MODIFY_ZWM011'
              EXPORTING
                armazem            = xuser-lgnum
                to_number          = to_ret
                to_item            = ltap-tapos
                status             = 'P'
              EXCEPTIONS
                error_update_table = 1
                OTHERS             = 2.

            item_ret =  ltap-tapos.
          ENDIF.

** Confirma a palete de cima
          IF bin_output_d(3) = 'CPK'.
            lv_tapos = item_ret.
          ELSE.
            lv_tapos = '0002'.
          ENDIF.

          CALL FUNCTION 'ZWM_CONFIRM_TO'
            EXPORTING
              armazem              = xuser-lgnum
              confirm_type         = 'P'
              su                   = zwm020-p2
            TABLES
              return_msg           = return_msg
            CHANGING
              to                   = ltap-tanum
              to_item              = lv_tapos
            EXCEPTIONS
              confirm_type_error   = 1
              to_not_found         = 2
              to_already_confirmed = 3
              to_already_picked    = 4
              to_not_picked        = 5
              wrong_su             = 6
              missing_su           = 7
              error                = 8
              OTHERS               = 9.

          IF sy-subrc <> 0.

          ENDIF.

          MOVE 'BIN_INPUT' TO cursorfield.

          CLEAR : return_msg.
          REFRESH : return_msg.

        ENDIF.
      ELSEIF zwm020-p2 = su.
        CLEAR : text.
        WRITE su TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '153'
            message_var1   = text.
        CLEAR: su, ok_code_0001.
        MOVE 'SU' TO cursorfield.
      ENDIF.

** Palete Simples
    ELSE.
      MOVE su TO su1.
** Faz pick à TO
      CLEAR : return_msg.
      REFRESH : return_msg.

      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'P'
          su                   = su
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = to_ret
        EXCEPTIONS
          confirm_type_error   = 1
          to_not_found         = 2
          to_already_confirmed = 3
          to_already_picked    = 4
          to_not_picked        = 5
          wrong_su             = 6
          missing_su           = 7
          error                = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
        READ TABLE return_msg INDEX 1.
        IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = return_msg-msgid
              message_lang   = sy-langu
              message_type   = return_msg-msgtyp
              message_number = return_msg-msgnr
              message_var1   = return_msg-msgv1
              message_var2   = return_msg-msgv2
              message_var3   = return_msg-msgv3.

          CLEAR: cursorfield, su, su1, ok_code_0001.
          MOVE 'SU' TO cursorfield.
          EXIT.
*          SET SCREEN '0000'.LEAVE SCREEN.
*          CLEAR : cursorfield.
        ENDIF.
      ELSE.
** Se for saida do drive-in tem de se alterar a tabela Zwm011 com o item
** que foi desdobrado com a SU correcta
        SELECT SINGLE *
            FROM ltap
                WHERE lgnum = xuser-lgnum AND
                      tanum = to_ret AND
                      vlenr = su.

        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
          EXPORTING
            armazem            = xuser-lgnum
            to_number          = to_ret
            to_item            = ltap-tapos
            status             = 'P'
          EXCEPTIONS
            error_update_table = 1
            OTHERS             = 2.

        item_ret = ltap-tapos.

        MOVE 'BIN_INPUT' TO cursorfield.

        CLEAR : return_msg.
        REFRESH : return_msg.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " valida_saida_dri_to_rep

*&---------------------------------------------------------------------*
*&      Form  cria_to2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_to2 USING p_lgtyp p_lgpla.

  CHECK NOT su IS INITIAL.

  CLEAR i_sscc.
  REFRESH i_sscc.
  CLEAR: mov, plant, s_loc, valor, ltap, lagp.

** Dados da OT
  SELECT SINGLE * FROM ltap
                  WHERE lgnum = xuser-lgnum AND
                        tanum = to_ret AND
                        vlenr = su.
  IF sy-subrc = 0.
    i_sscc-material = ltap-matnr.
    i_sscc-quantidade = ltap-vsola.
    i_sscc-uni = ltap-altme.
    i_sscc-lote_producao = ltap-charg.
    APPEND i_sscc.
    CLEAR i_sscc.
  ENDIF.

** Movimento
  SELECT SINGLE valor INTO valor
      FROM zwm001
          WHERE armazem = xuser-lgnum AND
                processo = 'SAIDA_ARMAZEM_DRI' AND
                parametro = 'MOV_WM'.

  MOVE valor TO mov.
  CLEAR valor.
** Centro
  SELECT SINGLE valor INTO valor
     FROM zwm001
         WHERE armazem = xuser-lgnum AND
               processo = 'GERAL' AND
               parametro = 'PLANT'.
  MOVE valor TO plant.
  CLEAR valor.
** Depósito
  SELECT SINGLE valor INTO valor
   FROM zwm001
       WHERE armazem = xuser-lgnum AND
             processo = 'GERAL' AND
             parametro = 'LGORT'.
  MOVE valor TO s_loc.
  CLEAR valor.
** Zona dentro do armazém
  SELECT SINGLE lzone FROM lagp INTO lagp-lzone
                      WHERE lgnum = xuser-lgnum AND
                            lgtyp = p_lgtyp AND
                            lgpla = p_lgpla.
* criar a to2
  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse      = xuser-lgnum
      mov_type       = mov
      plant          = plant
      s_loc          = s_loc
      certificado    = lagp-lzone
      sscc_adicional = su
    TABLES
      return_msg     = return_msg
      sscc           = i_sscc
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*    READ TABLE return_msg INDEX 1.
*    IF sy-subrc = 0.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = return_msg-msgid
*          message_lang   = sy-langu
*          message_type   = return_msg-msgtyp
*          message_number = return_msg-msgnr
*          message_var1   = return_msg-msgv1.
*
*      CLEAR: su, cursorfield.
*    ENDIF.

  ENDIF.

ENDFORM.                                                    " cria_to2
*&---------------------------------------------------------------------*
*&      Form  descobre_to_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descobre_to_2 .

** Descobrir qual foi a segunda TO q foi criada
** para a saída do drive in ... su no campo LZNUM
  CLEAR to_ret2.
  SELECT SINGLE tanum FROM ltak INTO to_ret2
                  WHERE lgnum = xuser-lgnum AND
                        lznum = su1 AND
                        kquit = ' '.

ENDFORM.                    " descobre_to_2
*&---------------------------------------------------------------------*
*&      Form  descobre_to_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descobre_to_3.

** Descobrir qual foi a segunda TO q foi criada
** para a saída do drive in ... su no campo LZNUM
  SELECT SINGLE tanum FROM ltak INTO to_ret3
                  WHERE lgnum = xuser-lgnum AND
                        lznum = zwm020-p2 AND
                        kquit = ' '.

ENDFORM.                    " descobre_to_3

*&---------------------------------------------------------------------*
*&      Form  confirma_to_delivery
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirma_to_delivery USING p_lgtyp_ori.

** Não vem de um Drive In temos de ir achar a TO
** de saída para a delivery
  IF p_lgtyp_ori = 'MEN'.
    PERFORM find_to_delivery.
  ENDIF.

  CLEAR : return_msg.
  REFRESH : return_msg.

** tem de confirmar as duas paletes
  IF p_lgtyp_ori <> 'BPK'.
    IF NOT to3 IS INITIAL AND to_remontada IS INITIAL.

      SELECT SINGLE tanum tapos FROM ltap
          INTO (ltap-tanum, ltap-tapos)
              WHERE lgnum = xuser-lgnum AND
                    vlenr = zwm020-p1 AND
                    pquit = ' '.

      CALL FUNCTION 'ZWM_CONFIRM_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'T'
          su                   = zwm020-p1
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = ltap-tanum
          to_item              = ltap-tapos
        EXCEPTIONS
          confirm_type_error   = 1
          to_not_found         = 2
          to_already_confirmed = 3
          to_already_picked    = 4
          to_not_picked        = 5
          wrong_su             = 6
          missing_su           = 7
          error                = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
*      READ TABLE return_msg INDEX 1.
*      IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.
*
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = return_msg-msgid
*            message_lang   = sy-langu
*            message_type   = return_msg-msgtyp
*            message_number = return_msg-msgnr
*            message_var1   = return_msg-msgv1
*            message_var2   = return_msg-msgv2
*            message_var3   = return_msg-msgv3.
*
*        CLEAR : cursorfield.
*        SET SCREEN '0000'.LEAVE SCREEN.
*
*      ENDIF.
      ELSE.
        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
          EXPORTING
            armazem            = xuser-lgnum
            to_number          = to_ret
            to_item            = '0001'
            status             = 'T'
          EXCEPTIONS
            error_update_table = 1
            OTHERS             = 2.
      ENDIF.

      CLEAR ltap.
      SELECT SINGLE tanum tapos FROM ltap
            INTO (ltap-tanum, ltap-tapos)
                WHERE lgnum = xuser-lgnum AND
                      vlenr = zwm020-p2 AND
                      pquit = ' '.


      CALL FUNCTION 'ZWM_CONFIRM_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'T'
          su                   = zwm020-p2
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = ltap-tanum
          to_item              = ltap-tapos
        EXCEPTIONS
          confirm_type_error   = 1
          to_not_found         = 2
          to_already_confirmed = 3
          to_already_picked    = 4
          to_not_picked        = 5
          wrong_su             = 6
          missing_su           = 7
          error                = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
*      READ TABLE return_msg INDEX 1.
*      IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.
*
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = return_msg-msgid
*            message_lang   = sy-langu
*            message_type   = return_msg-msgtyp
*            message_number = return_msg-msgnr
*            message_var1   = return_msg-msgv1
*            message_var2   = return_msg-msgv2
*            message_var3   = return_msg-msgv3.
*
*        SET SCREEN '0000'.LEAVE SCREEN.
*        CLEAR : cursorfield.
*      ENDIF.
      ENDIF.

    ELSE.

** Confirmar totalmente - TO DA DELIVERY
      DATA: flag(1),
            tanum   LIKE ltap-tanum,
            tapos   LIKE ltap-tapos.
      CLEAR: flag, tanum, tapos.

      WHILE flag IS INITIAL.
        CLEAR ltap.
        SELECT SINGLE tanum tapos FROM ltap
              INTO (tanum, tapos)
                  WHERE lgnum = xuser-lgnum AND
                        vlenr = su1 AND
                        pquit = ' '.

        CALL FUNCTION 'ZWM_CONFIRM_TO'
          EXPORTING
            armazem              = xuser-lgnum
            confirm_type         = 'T'
            su                   = su1
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = tanum
            to_item              = tapos
          EXCEPTIONS
            confirm_type_error   = 1
            to_not_found         = 2
            to_already_confirmed = 3
            to_already_picked    = 4
            to_not_picked        = 5
            wrong_su             = 6
            missing_su           = 7
            error                = 8
            OTHERS               = 9.

        IF sy-subrc <> 0.
          READ TABLE return_msg WITH KEY msgid = 'L3'
                                         msgnr = '146'.
          IF sy-subrc EQ 0.
**        Remessa & bloqueada por &
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '254'
                message_var1   = return_msg-msgv1
                message_var2   = return_msg-msgv2.
          ENDIF.
          CLEAR flag.

        ELSE.
          CLEAR ltap.
          SELECT SINGLE * FROM ltap
                WHERE lgnum = xuser-lgnum AND
                      tanum = tanum AND
                      tapos = tapos AND
                      pquit = 'X'.

          IF sy-subrc = 0.
            CALL FUNCTION 'ZWM_MODIFY_ZWM011'
              EXPORTING
                armazem            = xuser-lgnum
                to_number          = to_ret
                to_item            = '0001'
                status             = 'T'
              EXCEPTIONS
                error_update_table = 1
                OTHERS             = 2.
            flag = 'X'.
          ELSE.
            CLEAR flag.
          ENDIF.
        ENDIF.


      ENDWHILE.

    ENDIF.
  ENDIF.


**  Verificar se é a ultima palete da remessa para fazer a saida das
**  paletes
*  SELECT SINGLE *
*      FROM ltak
*          WHERE lgnum = xuser-lgnum AND
*                refnr = grupo AND
*                vbeln = remessa AND
*                kquit = ' '.
*  IF sy-subrc <> 0.
*    CLEAR porta1.
*    porta1 = bin_output_d.
*    PERFORM cria_doc_material_paletes_tr.
*  ENDIF.

** Actualizar paletes no carro e embalar
*  IF p_lgtyp_ori = 'BPK'.
  PERFORM confirm_ots_pal_picking.
*  ENDIF.

  PERFORM actualiza_paletes_carro.



ENDFORM.                    " confirma_to_delivery
*&---------------------------------------------------------------------*
*&      Form  find_to_delivery
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_to_delivery .

** To inicial que se encontra PICKED e que possui a SU
  SELECT SINGLE tanum FROM ltap INTO to_ret
                      WHERE lgnum = xuser-lgnum AND
                            vlenr = su1 AND
                            pquit = ' '.

ENDFORM.                    " find_to_delivery

*&---------------------------------------------------------------------
*
*&      Form  verifica_mensulas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_mensulas    USING pu_nivel
                                pu_nivel_001
                       CHANGING pc_pr_max.

  DATA : corredor(4),
         n_mensulas_ocupadas.

  CLEAR : corredor,
          n_mensulas_ocupadas,
          zwm014,
          pc_pr_max.


  SELECT SINGLE mensula FROM zwm014 INTO zwm014-mensula
                        WHERE armazem     = l_ltap-lgnum AND
                              su_transito = ltak-lznum.
  IF sy-subrc = 0.
    CONCATENATE zwm014-mensula(3) '%' INTO corredor.

    SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                    WHERE armazem = xuser-lgnum AND
                          mensula LIKE corredor AND
                          estado = 'X'.

    IF zwm014-mensula(3) EQ '001'. " Corredor 1
      IF n_mensulas_ocupadas > pu_nivel_001.
        pc_pr_max = 9.
      ENDIF.
    ELSE.                          " Restantes Corredores
      IF n_mensulas_ocupadas > pu_nivel.
        pc_pr_max = 9.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " verifica_mensulas
*&---------------------------------------------------------------------*
*&      Form  cria_to3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_to3.
*  break roffd.
  CLEAR i_sscc.
  REFRESH i_sscc.
  SELECT SINGLE * FROM ltap
                 WHERE lgnum = xuser-lgnum AND
                       tanum = to_ret AND
                       vlenr = zwm020-p2.
  IF sy-subrc = 0.
    i_sscc-material = ltap-matnr.
    i_sscc-quantidade = ltap-vsola.
    i_sscc-uni = ltap-altme.
    i_sscc-lote_producao = ltap-charg.
    APPEND i_sscc.
    CLEAR i_sscc.
  ENDIF.
* criar a to3
  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse      = xuser-lgnum
      mov_type       = mov
      plant          = plant
      s_loc          = s_loc
      certificado    = lagp-lzone
      sscc_adicional = zwm020-p2
    TABLES
      return_msg     = return_msg
      sscc           = i_sscc
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    READ TABLE return_msg INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1.

      CLEAR: su, cursorfield.
    ENDIF.
  ENDIF.
ENDFORM.                                                    " cria_to3
*&---------------------------------------------------------------------*
*&      Form  actualiza_paletes_carro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_paletes_carro.
  DATA lv_glock_active TYPE abap_bool. " << INS ROFF(SDF):TMGP:22.01.2016 14:41:50

  DATA: lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        lt_vbpa   LIKE vbpa   OCCURS 0 WITH HEADER LINE.

  DATA: ls_zwm028 TYPE zwm028,
        ls_zwm066 TYPE zwm066.

  DATA: next_desbloq(1),
        l_kunnr         LIKE vbpa-kunnr,
        num_pal_remessa TYPE i.

  DATA: lv_refnr  TYPE lvs_refnr,
        lv_2spart TYPE flag.

  CLEAR: ltak, ltap, return_msg.
  REFRESH return_msg.

*& Begin of Modification by Carlos Fernandes - ROFF @ 23.01.2016
**& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:42:12
*  lv_glock_active = abap_false.
*
*  SELECT valor UP TO 1 ROWS
*    FROM zwm001 INTO lv_glock_active
*    WHERE armazem EQ xuser-lgnum
*      AND processo EQ 'ALTERA_GLOCK'
*      AND parametro EQ 'ACTIVAR'.
*  ENDSELECT.
**& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:42:12

  SELECT mandt UP TO 1 ROWS
    FROM zwm001 INTO sy-mandt
    WHERE armazem EQ xuser-lgnum
      AND processo EQ 'CHANGE_ORDEM'
      AND parametro EQ 'NLTYP_OK'
      AND valor EQ zwm011-ultimo_tipo_dep.
  ENDSELECT.
  IF sy-subrc EQ 0.
    lv_glock_active = abap_true.
  ELSE.
    lv_glock_active = abap_false.
  ENDIF.
*& End of Modification by Carlos Fernandes - ROFF @ 23.01.2016 14:42:12

  SELECT SINGLE *
    FROM ltap
        WHERE lgnum = xuser-lgnum AND
              tanum = to_ret AND
              vlenr = su1.

  IF remessa IS INITIAL.
    SELECT SINGLE *
        FROM ltak
            WHERE lgnum = xuser-lgnum AND
                  tanum = ltap-tanum AND
                  kquit = ' '.
  ELSE.
    ltak-vbeln = remessa.
    ltak-refnr = grupo.
  ENDIF.

  IF ltak-refnr IS INITIAL.
    lv_refnr = grupo.
  ELSE.
    lv_refnr = ltak-refnr.
  ENDIF.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = xuser-lgnum
      i_refnr  = lv_refnr
    IMPORTING
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF lv_2spart EQ abap_true.
    ltak-refnr = lv_refnr.

    SELECT * FROM zwm028
             INTO TABLE lt_zwm028
             WHERE lgnum = xuser-lgnum AND
                   refnr = lv_refnr.

    SORT lt_zwm028 BY zlock DESCENDING.
    READ TABLE lt_zwm028
          INTO ls_zwm028
          INDEX 1.

    DELETE lt_zwm028 WHERE zlock <> ls_zwm028-zlock.

    SORT lt_zwm028 BY ordem DESCENDING.

    CLEAR: ls_zwm028.
    READ TABLE lt_zwm028
          INTO ls_zwm028
          INDEX 1.
    GET TIME.
    CLEAR: ls_zwm066.
    ls_zwm066-lgnum = xuser-lgnum.
    ls_zwm066-refnr = lv_refnr.
    ls_zwm066-vbeln = ls_zwm028-remessa.
    ls_zwm066-lenum = su1.
    ls_zwm066-tanum = to_ret.
    ls_zwm066-erdat = sy-datum.
    ls_zwm066-erzet = sy-uzeit.
    ls_zwm066-ernam = sy-uname.
*    INSERT zwm066 FROM ls_zwm066.
*    COMMIT WORK.
  ENDIF.

** Apagar da Tabela ZWM013
  DELETE FROM zwm013
      WHERE armazem   = xuser-lgnum AND
            sscc = su1.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

** criar OT inversa Destino - Origem

  CLEAR i_sscc.
  REFRESH i_sscc.
  i_sscc-material = ltap-matnr.
  i_sscc-quantidade = ltap-vsola.
  i_sscc-uni = ltap-altme.
  i_sscc-lote_producao = ltap-charg.
  APPEND i_sscc.
  CLEAR i_sscc.

  PERFORM get_parameter
              USING xuser-lgnum
                    'GERAL'
                    'PLANT'
                    valor.
  MOVE valor TO plant.

  CLEAR valor.
  PERFORM get_parameter
             USING xuser-lgnum
                   'GERAL'
                   'LGORT'
                   valor.
  MOVE valor TO lgort.

  CASE ltap-vltyp.
    WHEN 'DRI' OR 'BLK'.
      CLEAR valor.
      PERFORM get_parameter
                 USING xuser-lgnum
                       'SAIDA_ARMAZEM_DRI'
                       'MOV_WM_INV'
                       valor.
      MOVE valor TO mov.
    WHEN 'TRI'.
      CLEAR valor.
      PERFORM get_parameter
                 USING xuser-lgnum
                       'SAIDA_ARMAZEM_TRI'
                       'MOV_WM_INV'
                       valor.
      MOVE valor TO mov.
  ENDCASE.

  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse      = xuser-lgnum
      mov_type       = mov
      plant          = plant
      s_loc          = lgort
      sscc_adicional = ltap-vlenr
    TABLES
      return_msg     = return_msg
      sscc           = i_sscc
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*      read table return_msg index 1.
*      IF sy-subrc = 0.
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = return_msg-msgid
*            message_lang   = sy-langu
*            message_type   = return_msg-msgtyp
*            message_number = return_msg-msgnr
*            message_var1   = return_msg-msgv1.
*
*        CLEAR: su, cursorfield.
*      ENDIF.

  ENDIF.
  IF NOT to3 IS INITIAL AND to_remontada IS INITIAL.

** Apagar da tabela zwm013
    DELETE FROM zwm013
        WHERE armazem   = xuser-lgnum AND
              sscc = su1.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

** Apagar da Tabela ZWM020

*    DELETE FROM zwm020
*        WHERE armazem = xuser-lgnum AND
*              p1 = su1 AND
*              p2 = zwm020-p2.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.

  ENDIF.
*  ENDIF.

** Actualizar numero de paletes no carro
  UPDATE zwm028 SET paletes_carro = paletes_carro + 1
  WHERE lgnum = xuser-lgnum AND
        refnr = ltak-refnr AND
        remessa = ' '.

  UPDATE zwm028 SET paletes_carro = paletes_carro + 1
  WHERE lgnum = xuser-lgnum AND
        refnr = ltak-refnr AND
        remessa = remessa.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 21.09.2012 16:02:05
*  Motivo: Valida se todas as paletes estão no pulmão e se deve
*          fazer abastecimento
*--------------------------------------------------------------------*
  CALL FUNCTION 'ZWM_AUTO_TO_DELEVERY_ABAST'
    EXPORTING
      i_lgnum = xuser-lgnum
      i_refnr = ltak-refnr.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


** Verificar se as paletes no carro são iguais á da Remessa para
** desbloquear a remessa de ordem n + 1
  break roffd.
  CLEAR zwm028.
  SELECT SINGLE * FROM zwm028
      WHERE lgnum = xuser-lgnum AND
            refnr = ltak-refnr AND
            remessa = ' '.
  IF zwm028-st_dck = 'DCK' AND zwm028-tipo_lock = 'R'.
    IF zwm028-paletes_carro < zwm028-total_paletes.

      CLEAR: lt_zwm028.
      REFRESH: lt_zwm028.

      SELECT * INTO TABLE lt_zwm028
          FROM zwm028
              WHERE lgnum = xuser-lgnum
                AND refnr = ltak-refnr
                AND remessa <> ' '.

      CLEAR: next_desbloq, num_pal_remessa.
      LOOP AT lt_zwm028 WHERE zlock <> '1'.
        num_pal_remessa = num_pal_remessa + lt_zwm028-total_paletes.
      ENDLOOP.

** Neste caso vamos desbloquear a proxima
      IF num_pal_remessa <= zwm028-paletes_carro.
        DELETE lt_zwm028 WHERE zlock <> '1'.

        IF NOT lt_zwm028[] IS INITIAL.

          CLEAR lt_vbpa.
          REFRESH lt_vbpa.
          SELECT * INTO TABLE lt_vbpa
              FROM vbpa
                  FOR ALL ENTRIES IN lt_zwm028
                      WHERE vbeln EQ lt_zwm028-remessa
                        AND posnr = '000000'
                        AND parvw EQ 'W1'.

*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:01
          IF lv_glock_active EQ abap_false.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:02
            SORT lt_zwm028 BY ordem.
*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
          ELSE.
            SORT lt_zwm028[] BY ordem DESCENDING.
          ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23

          CLEAR lt_zwm028.
          READ TABLE lt_zwm028 INDEX 1.

          CLEAR l_kunnr.
          LOOP AT lt_vbpa WHERE vbeln = lt_zwm028-remessa.
            l_kunnr = lt_vbpa-kunnr.
            EXIT.
          ENDLOOP.

          IF NOT l_kunnr IS INITIAL.
            LOOP AT lt_vbpa WHERE kunnr = l_kunnr.
              UPDATE zwm028 SET zlock = zwm028-zlock
                            WHERE lgnum = zwm028-lgnum
                            AND refnr = zwm028-refnr
                            AND remessa = lt_vbpa-vbeln.
              COMMIT WORK AND WAIT.

              CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
                EXPORTING
                  i_lgnum = zwm028-lgnum
                  i_refnr = zwm028-refnr
                  i_vbeln = lt_zwm028-remessa.
            ENDLOOP.
          ELSE.
            DELETE lt_zwm028 WHERE zlock <> 1.

            IF NOT lt_zwm028[] IS INITIAL.
*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:01
              IF lv_glock_active EQ abap_false.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:02
                SORT lt_zwm028 BY ordem.
*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
              ELSE.
                SORT lt_zwm028[] BY ordem DESCENDING.
              ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
              CLEAR lt_zwm028.
              READ TABLE lt_zwm028 INDEX 1.

              UPDATE zwm028 SET zlock = zwm028-zlock
                            WHERE lgnum = zwm028-lgnum
                            AND refnr = zwm028-refnr
                            AND ordem = lt_zwm028-ordem.
              COMMIT WORK AND WAIT.

              CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
                EXPORTING
                  i_lgnum = zwm028-lgnum
                  i_refnr = zwm028-refnr
                  i_vbeln = lt_zwm028-remessa.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

** Envio de IDOC para liberar próxima sequencia do grupo
  CALL FUNCTION 'ZWM_IDOC_FREE_WORK_WCS'
    EXPORTING
      i_lgnum = xuser-lgnum
      i_refnr = ltak-refnr
      i_step  = '2'.

ENDFORM.                    " actualiza_paletes_carro


*&---------------------------------------------------------------------*
*&      Form  actualiza_paletes_carro_prm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_paletes_carro_prm.
  DATA lv_glock_active TYPE abap_bool. " << INS ROFF(SDF):TMGP:22.01.2016 14:41:50

  DATA: lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        lt_vbpa   LIKE vbpa   OCCURS 0 WITH HEADER LINE.

  DATA: next_desbloq(1),
        l_kunnr         LIKE vbpa-kunnr,
        num_pal_remessa TYPE i.

  CLEAR: ltak, ltap.

*& Begin of Modification by Carlos Fernandes - ROFF @ 23.01.2016
**& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:42:12
*  lv_glock_active = abap_false.
*
*  SELECT valor UP TO 1 ROWS
*    FROM zwm001 INTO lv_glock_active
*    WHERE armazem EQ xuser-lgnum
*      AND processo EQ 'ALTERA_GLOCK'
*      AND parametro EQ 'ACTIVAR'.
*  ENDSELECT.
**& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:42:12

  SELECT mandt UP TO 1 ROWS
    FROM zwm001 INTO sy-mandt
    WHERE armazem EQ xuser-lgnum
      AND processo EQ 'CHANGE_ORDEM'
      AND parametro EQ 'NLTYP_OK'
      AND valor EQ zwm011-ultimo_tipo_dep.
  ENDSELECT.
  IF sy-subrc EQ 0.
    lv_glock_active = abap_true.
  ELSE.
    lv_glock_active = abap_false.
  ENDIF.
*& End of Modification by Carlos Fernandes - ROFF @ 23.01.2016

  SELECT SINGLE *
    FROM ltap
        WHERE lgnum = xuser-lgnum AND
              tanum = to_ret AND
              vlenr = su1.

  SELECT SINGLE *
      FROM ltak
          WHERE lgnum = xuser-lgnum AND
                tanum = ltap-tanum AND
                kquit = 'X'.

  CHECK sy-subrc EQ 0.

** Apagar da Tabela ZWM013
  DELETE FROM zwm013
      WHERE armazem = xuser-lgnum AND
               sscc = su1.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

** Actualizar numero de paletes no carro
  UPDATE zwm028 SET paletes_carro = paletes_carro + 1
  WHERE lgnum = xuser-lgnum AND
        refnr = ltak-refnr AND
        remessa = ' '.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 21.09.2012 16:02:05
*  Motivo: Valida se todas as paletes estão no pulmão e se deve
*          fazer abastecimento
*--------------------------------------------------------------------*
  CALL FUNCTION 'ZWM_AUTO_TO_DELEVERY_ABAST'
    EXPORTING
      i_lgnum = xuser-lgnum
      i_refnr = ltak-refnr.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

** Verificar se as paletes no carro são iguais á da Remessa para
** desbloquear a remessa de ordem n + 1
  break roffd.
  CLEAR zwm028.
  SELECT SINGLE * FROM zwm028
      WHERE lgnum = xuser-lgnum AND
            refnr = grupo AND
            remessa = ' '.
  IF zwm028-st_dck = 'DCK' AND zwm028-tipo_lock = 'R'.
    IF zwm028-paletes_carro < zwm028-total_paletes.

      CLEAR: lt_zwm028.
      REFRESH: lt_zwm028.

      SELECT * INTO TABLE lt_zwm028
          FROM zwm028
              WHERE lgnum = xuser-lgnum
                AND refnr = grupo
                AND remessa <> ' '.

      CLEAR: next_desbloq, num_pal_remessa.
      LOOP AT lt_zwm028 WHERE zlock <> '1'.
        num_pal_remessa = num_pal_remessa + lt_zwm028-total_paletes.
      ENDLOOP.

** Neste caso vamos desbloquear a proxima
      IF num_pal_remessa <= zwm028-paletes_carro.
        DELETE lt_zwm028 WHERE zlock <> '1'.

        IF NOT lt_zwm028[] IS INITIAL.

          CLEAR lt_vbpa.
          REFRESH lt_vbpa.
          SELECT * INTO TABLE lt_vbpa
              FROM vbpa
                  FOR ALL ENTRIES IN lt_zwm028
                      WHERE vbeln EQ lt_zwm028-remessa
                        AND posnr = '000000'
                        AND parvw EQ 'W1'.

*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:01
          IF lv_glock_active EQ abap_false.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:02
            SORT lt_zwm028 BY ordem.
*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
          ELSE.
            SORT lt_zwm028[] BY ordem DESCENDING.
          ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23

          CLEAR lt_zwm028.
          READ TABLE lt_zwm028 INDEX 1.

          CLEAR l_kunnr.
          LOOP AT lt_vbpa WHERE vbeln = lt_zwm028-remessa.
            l_kunnr = lt_vbpa-kunnr.
            EXIT.
          ENDLOOP.

          IF NOT l_kunnr IS INITIAL.
            LOOP AT lt_vbpa WHERE kunnr = l_kunnr.
              UPDATE zwm028 SET zlock = zwm028-zlock
                            WHERE lgnum = zwm028-lgnum
                              AND refnr = zwm028-refnr
                              AND remessa = lt_vbpa-vbeln.
              COMMIT WORK AND WAIT.

              CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
                EXPORTING
                  i_lgnum = lt_zwm028-lgnum
                  i_refnr = lt_zwm028-refnr
                  i_vbeln = lt_zwm028-remessa.
            ENDLOOP.
          ELSE.
            DELETE lt_zwm028 WHERE zlock <> 1.

            IF NOT lt_zwm028[] IS INITIAL.
*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:01
              IF lv_glock_active EQ abap_false.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:02
                SORT lt_zwm028 BY ordem.
*& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
              ELSE.
                SORT lt_zwm028[] BY ordem DESCENDING.
              ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
              CLEAR lt_zwm028.
              READ TABLE lt_zwm028 INDEX 1.

              UPDATE zwm028 SET zlock = zwm028-zlock
                  WHERE lgnum = zwm028-lgnum
                    AND refnr = zwm028-refnr
                    AND ordem = lt_zwm028-ordem.
              COMMIT WORK AND WAIT.

              CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
                EXPORTING
                  i_lgnum = lt_zwm028-lgnum
                  i_refnr = lt_zwm028-refnr
                  i_vbeln = lt_zwm028-remessa.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

** Envio de IDOC para liberar próxima sequencia do grupo
  CALL FUNCTION 'ZWM_IDOC_FREE_WORK_WCS'
    EXPORTING
      i_lgnum = xuser-lgnum
      i_refnr = ltak-refnr
      i_step  = '2'.

ENDFORM.                    " actualiza_paletes_carro_prm

*&---------------------------------------------------------------------*
*&      Form  call_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PRIMEIRA  text
*      -->P_0513   text
*----------------------------------------------------------------------*
FORM call_screen  USING  p_primeira
                         VALUE(p_ecran).

** Gestão de chamada de número de ecrans
  IF p_primeira EQ 'X'.
    CALL SCREEN p_ecran.
  ELSE.
    SET SCREEN p_ecran.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.                    " call_screen

*&---------------------------------------------------------------------*
*&      Form  valida_bloq
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ARMAZEM  text
*      -->P_TO  text
*----------------------------------------------------------------------*
FORM valida_bloq  USING    pu_armazem
                           pu_to.

  DATA: f_remessa LIKE ltak-vbeln.

  DATA: l_enq    LIKE seqg3 OCCURS 0 WITH HEADER LINE,
        l_gname  LIKE seqg3-gname,
        l_garg   LIKE seqg3-garg,
        l_guname LIKE sy-uname.

** Valida se é de uma Remessa
  SELECT SINGLE vbeln FROM ltak INTO f_remessa
         WHERE lgnum EQ pu_armazem
           AND tanum EQ pu_to.

  CHECK NOT f_remessa IS INITIAL.

  CONCATENATE sy-mandt f_remessa INTO l_garg.

  CONDENSE l_garg NO-GAPS.

** Efectua uma espera pelo libertar do bloqueio da Remessa
  DO 10 TIMES.
    CLEAR:   l_enq.
    REFRESH: l_enq.
    CALL FUNCTION 'ENQUE_READ'
      EXPORTING
        gclient = sy-mandt
        gname   = l_gname
        garg    = l_garg
        guname  = l_guname
*     IMPORTING
*       NUMBER  =
*       SUBRC   =
      TABLES
        enq     = l_enq.

    IF NOT l_enq[] IS INITIAL.
      WAIT UP TO 1 SECONDS.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " valida_bloq
*&---------------------------------------------------------------------*
*&      Form  UPDATE_LX39
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_T311  text
*----------------------------------------------------------------------*
FORM update_lx39 USING ut_t311   TYPE zwm01_t_t311
                       ur_datum  TYPE zwm01_r_datum
                       uv_commit TYPE flag.

  DATA: lt_rspar TYPE TABLE OF rsparams.

  DATA: ls_rspar   TYPE rsparams,
        ls_t311    TYPE t311,
        ls_r_datum LIKE LINE OF ur_datum.

  CHECK NOT ut_t311 IS INITIAL.

  READ TABLE ut_t311 INTO ls_t311 INDEX 1.

  CLEAR ls_rspar.
  ls_rspar-selname = 'LGNUM'.
  ls_rspar-kind = 'P'.
  ls_rspar-sign = 'I'.
  ls_rspar-option = 'EQ'.
  ls_rspar-low = ls_t311-lgnum.
  APPEND ls_rspar TO lt_rspar.

  LOOP AT ut_t311 INTO ls_t311.
    CLEAR ls_rspar.
    ls_rspar-selname = 'REFNR'.
    ls_rspar-kind = 'S'.
    ls_rspar-sign = 'I'.
    ls_rspar-option = 'EQ'.
    ls_rspar-low = ls_t311-refnr.
    APPEND ls_rspar TO lt_rspar.
  ENDLOOP.
  IF sy-subrc <> 0.
    CLEAR ls_rspar.
    ls_rspar-selname = 'REFNR'.
    ls_rspar-kind = 'S'.
    APPEND ls_rspar TO lt_rspar.
  ENDIF.

  LOOP AT ur_datum INTO ls_r_datum.
    CLEAR ls_rspar.
    ls_rspar-selname = 'DATUM'.
    ls_rspar-kind = 'S'.
    MOVE-CORRESPONDING ls_r_datum TO ls_rspar.
    APPEND ls_rspar TO lt_rspar.
  ENDLOOP.
  IF sy-subrc <> 0.
    CLEAR ls_rspar.
    ls_rspar-selname = 'DATUM'.
    ls_rspar-kind = 'S'.
    APPEND ls_rspar TO lt_rspar.
  ENDIF.

  CLEAR ls_rspar.
  ls_rspar-selname = 'ANZEIGE'.
  ls_rspar-kind = 'P'.
  ls_rspar-sign = 'I'.
  ls_rspar-option = 'EQ'.
  ls_rspar-low = ' '.
  APPEND ls_rspar TO lt_rspar.

  SUBMIT rl2stk00 WITH SELECTION-TABLE lt_rspar AND RETURN.

** Status de Grupos
***********************************************************************
  CALL FUNCTION 'ZWM_2STEP_REDETERMINANTE'
    EXPORTING
      it_t311 = ut_t311.

** Iguala Satatus de Liberado
***********************************************************************
  LOOP AT ut_t311 INTO ls_t311.
    UPDATE t311 SET kzdru = ls_t311-kzdru
                WHERE lgnum = ls_t311-lgnum AND
                      refnr = ls_t311-refnr.
  ENDLOOP.

** Commit
***********************************************************************
  IF uv_commit EQ abap_true.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " UPDATE_LX39
*&---------------------------------------------------------------------*
*&      Form  REMOVE_TAPRI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_LTAP  text
*----------------------------------------------------------------------*
FORM remove_tapri CHANGING ct_ltap TYPE zwm01_t_l_ltap.
  FIELD-SYMBOLS: <ls_ltap> TYPE zwm01_l_ltap.

  LOOP AT ct_ltap ASSIGNING <ls_ltap> WHERE tapri <> gc_tapri_let_down_pri.
    CLEAR <ls_ltap>-tapri.
  ENDLOOP.
ENDFORM.                    " REMOVE_TAPRI
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_TKNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_TKNUM  text
*----------------------------------------------------------------------*
FORM dequeue_tknum  USING uv_tknum TYPE tknum.

  DATA: lt_vttp TYPE TABLE OF vttp,
        lt_vbss TYPE TABLE OF vbss,
        lt_enq  TYPE TABLE OF seqg3.

  DATA: ls_vttp TYPE vttp,
        ls_vbss TYPE vbss.

  DATA: lv_garg TYPE eqegraarg.

  CALL FUNCTION 'DEQUEUE_EVVTTKE'
    EXPORTING
      mode_vttk = 'E'
      mandt     = sy-mandt
      tknum     = uv_tknum.

  SELECT * FROM vttp
           INTO TABLE lt_vttp
           WHERE tknum = uv_tknum.

  DELETE ADJACENT DUPLICATES FROM lt_vttp COMPARING vbeln.

  IF NOT lt_vttp IS INITIAL.
    SELECT * FROM vbss
       INTO TABLE lt_vbss
       FOR ALL ENTRIES IN lt_vttp
       WHERE vbeln = lt_vttp-vbeln.

    SORT lt_vbss BY vbeln.
  ENDIF.

  LOOP AT lt_vttp INTO ls_vttp.
    CONCATENATE sy-mandt ls_vttp-vbeln INTO lv_garg.
    DO 20 TIMES.
      IF sy-index > 1.
        WAIT UP TO 1 SECONDS.
      ENDIF.

      DO 3 TIMES.

*--> Remessa
        CALL FUNCTION 'DEQUEUE_EVVBLKE'
          EXPORTING
            mode_likp = 'E'
            mandt     = sy-mandt
            vbeln     = ls_vttp-vbeln.

        CONCATENATE sy-mandt ls_vttp-vbeln INTO lv_garg.

        CALL FUNCTION 'ENQUEUE_READ'
          EXPORTING
            gclient               = sy-mandt
            gname                 = 'LIKP'
            garg                  = lv_garg
            guname                = sy-uname
          TABLES
            enq                   = lt_enq
          EXCEPTIONS
            communication_failure = 1
            system_failure        = 2
            OTHERS                = 3.

        IF NOT lt_enq IS INITIAL.
          CONTINUE.
        ENDIF.

*--> Grupo
        CLEAR ls_vbss.
        READ TABLE lt_vbss
              INTO ls_vbss
              WITH KEY vbeln = ls_vttp-vbeln
              BINARY SEARCH.

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        CALL FUNCTION 'DEQUEUE_EVVBSK'
          EXPORTING
            mode_vbsk = 'E'
            mandt     = sy-mandt
            sammg     = ls_vbss-sammg.


        CONCATENATE sy-mandt ls_vbss-sammg INTO lv_garg.


*        CALL FUNCTION 'ENQUEUE_READ'
*          EXPORTING
*            gclient               = sy-mandt
*            gname                 = 'VBSK'
*            garg                  = lv_garg
*            guname                = sy-uname
*          TABLES
*            enq                   = lt_enq
*          EXCEPTIONS
*            communication_failure = 1
*            system_failure        = 2
*            OTHERS                = 3.
*
*        IF NOT lt_enq IS INITIAL.
*          CONTINUE.
*        ENDIF.

        EXIT.
      ENDDO.

      IF lt_enq IS INITIAL.
        EXIT.
      ENDIF.

    ENDDO.
  ENDLOOP.

ENDFORM.                    " DEQUEUE_TKNUM
*&---------------------------------------------------------------------*
*&      Form  CHECK_TO_SWAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_to_swap  CHANGING cv_subrc TYPE sysubrc.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: ls_message TYPE bdcmsgcoll.

  DATA: ls_ltak   TYPE ltak,
        ls_ltap   TYPE ltap,
        ls_zwm011 TYPE zwm011.

** cv_subrc = 0 To Swap OK
** cv_subrc = 99 No To Swap
** Other cv_subrc = Error in TO SWAP

  cv_subrc = 99."assumed error

  CHECK xuser-lgnum EQ '150'. "Only TO France
  CHECK ltap-vlenr IS INITIAL. "Only Emty Source SU
  CHECK ltap-nltyp EQ '916'. "Only TO to Expedicion
  CHECK ltap-vltyp EQ 'DRI' OR ltap-vltyp EQ 'BLK'. "Only TO from Drive In

  CALL FUNCTION 'ZWM_TO_SWAP'
    EXPORTING
      i_lgnum     = xuser-lgnum
      i_tanum_o   = ltap-tanum
      i_tapos_o   = ltap-tapos
      i_lenum_d   = su
    IMPORTING
      es_ltak_d   = ls_ltak
      es_ltap_d   = ls_ltap
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CLEAR: ls_message.
    READ TABLE lt_messages
          INTO ls_message
          INDEX 1.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = ls_message-msgid
        message_lang   = ls_message-msgspra
        message_type   = 'E'
        message_number = ls_message-msgnr
        message_var1   = ls_message-msgv1
        message_var2   = ls_message-msgv2
        message_var3   = ls_message-msgv3
        message_var4   = ls_message-msgv4.

    cv_subrc = 4.
    EXIT.
  ENDIF.

** Remove OT Antiga
**********************************************************************
  SELECT SINGLE * FROM zwm011
                  INTO ls_zwm011
                  WHERE armazem   = xuser-lgnum AND
                        to_number = ltap-tanum AND
                        to_item   = ltap-tapos AND
                        user_name = sy-uname.

  ls_zwm011-to_number = ls_ltap-tanum.
  ls_zwm011-to_item   = ls_ltap-tapos.
  MODIFY zwm011 FROM ls_zwm011.

  DELETE FROM zwm011 WHERE armazem   = xuser-lgnum AND
                           to_number = ltap-tanum AND
                           to_item   = ltap-tapos AND
                           user_name = sy-uname AND
                           status <> 'P'.


  COMMIT WORK.

** Update de Variaveis Internas
**********************************************************************
  ltap = ls_ltap.
  ltak = ls_ltak.
  gv_charg = ls_ltap-charg.
  to_ret = ls_ltak-tanum.

  CLEAR: cv_subrc.
ENDFORM.                    " CHECK_TO_SWAP
*&---------------------------------------------------------------------*
*&      Form  CHECK_CHD_ELV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SU  text
*----------------------------------------------------------------------*
FORM check_chd_elv USING p_lgpla TYPE lgpla CHANGING p_su TYPE lenum.

  DATA: lv_su             TYPE lenum.
  DATA: lv_subst          TYPE rl03t-subst.
  DATA: lv_queue_aut_prm  TYPE lrf_queue.
  DATA: lv_pick_man       TYPE bwlvs.
  DATA: lv_pick_aut       TYPE bwlvs.
  DATA: lv_wm_rej         TYPE bwlvs.
  DATA: lv_tanum          TYPE tanum.
  DATA: lv_lznum          TYPE lznum.
  DATA: lv_ent_pick       TYPE lgpla.
  DATA: lv_ot_rej_dummy   TYPE flag.

  DATA: ls_zwm011         TYPE zwm011.
  DATA: ls_zwm013         TYPE zwm013.
  DATA: ls_zwm020         TYPE zwm020.
  DATA: ls_zwm077         TYPE zwm077.
  DATA: ls_t327a          TYPE t327a.
  DATA: ls_ltap           TYPE ltap.
  DATA: ls_lqua           TYPE lqua.
  DATA: ls_lagp           TYPE lagp.
  DATA: ls_ltak           TYPE ltak.
  DATA: ls_return         TYPE bdcmsgcoll.

  DATA: lt_zwm001         TYPE TABLE OF zwm001     WITH HEADER LINE.
  DATA: lr_chd_elv_queues TYPE RANGE OF lrf_queue  WITH HEADER LINE.
  DATA: lt_return_msg     TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.
  DATA: lt_ltak           TYPE TABLE OF ltak       WITH HEADER LINE.
  DATA: lt_ltap           TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_ltap_epe       TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_zwm077         TYPE TABLE OF zwm077     WITH HEADER LINE.
  DATA: lt_zwm078         TYPE TABLE OF zwm078     WITH HEADER LINE.
  DATA: lt_ltap_conf      TYPE TABLE OF ltap_conf  WITH HEADER LINE.
  DATA: lt_ltap_cancl     TYPE TABLE OF ltap_cancl WITH HEADER LINE.
  DATA: lt_ltap_vb        TYPE TABLE OF ltap_vb    WITH HEADER LINE.

** Obter parametros
**********************************************************************

** Movimento WM - Paletes Picking Entrada na mesa do WCS
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = xuser-lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_MAN_PICK'
    IMPORTING
      e_valor     = lv_pick_man
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

** Fila Picking - WCS Automático
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = xuser-lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_ENT'
    IMPORTING
      e_valor     = lv_pick_aut
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

** Movimento WM - Rejeições Paletes
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = xuser-lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_REJ'
    IMPORTING
      e_valor     = lv_wm_rej
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

*  CALL FUNCTION 'ZWM_GET_PARAMETER'
*    EXPORTING
*      i_lgnum     = xuser-lgnum
*      i_processo  = 'CHAMADA_ELV'
*      i_parametro = 'FILAS'
*    TABLES
*      t_zwm001    = lt_zwm001
*    EXCEPTIONS
*      error       = 1
*      OTHERS      = 2.
*
*  LOOP AT lt_zwm001.
*    CLEAR lr_chd_elv_queues.
*    lr_chd_elv_queues-sign   = 'I'.
*    lr_chd_elv_queues-option = 'EQ'.
*    lr_chd_elv_queues-low    = lt_zwm001-valor.
*    APPEND lr_chd_elv_queues.
*  ENDLOOP.


** Validar palete remontada
  SELECT SINGLE *
    FROM zwm020 INTO ls_zwm020
    WHERE armazem = xuser-lgnum
    AND   p2      = p_su.

  IF sy-subrc = 0.
    CLEAR text.
    WRITE ls_zwm020-p1 TO text LEFT-JUSTIFIED.

    "Palete remontada. Picar SSCC & de palete de baixo.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '320'
        message_var1   = text.

    CLEAR p_su.
    EXIT.
  ENDIF.

** Validar Paletes de saida nas mesas de rejeição
**********************************************************************
  IF lgtyp = 'CHE' AND p_lgpla = 'EXP2_ENT'.
    p_lgpla     = 'EXP2_REJ'.
    lv_ent_pick = 'EXP2_ENT'.
  ENDIF.

  SELECT SINGLE *
    FROM lagp INTO ls_lagp
    WHERE lgnum = xuser-lgnum
    AND   lgtyp = 'REJ'
    AND   lgpla = p_lgpla.

  IF sy-subrc = 0.
    SELECT *
      FROM zwm077 INTO TABLE lt_zwm077
      WHERE lgnum = xuser-lgnum
      AND   exidv = p_su.

    DELETE lt_zwm077 WHERE pvqui IS NOT INITIAL.

    SORT lt_zwm077 BY rdatu DESCENDING rzeit DESCENDING.

    " Validar Posição
    READ TABLE lt_zwm077 INDEX 1.
    IF sy-subrc = 0 AND lt_zwm077-lgpla <> p_lgpla.

      CLEAR: text, text1.
      WRITE p_su    TO text  LEFT-JUSTIFIED.
      WRITE p_lgpla TO text1 LEFT-JUSTIFIED.

      " SU & não pertence à posição & de chamada.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '353'
          message_var1   = text
          message_var2   = text1.

      CLEAR p_su.
      EXIT.
    ENDIF.

** Validar Palete
    SELECT *
     FROM ltap INTO TABLE lt_ltap
     WHERE lgnum = xuser-lgnum
     AND   vlenr = p_su.

    " Palete Copacking
    IF lt_ltap[] IS INITIAL.

      SELECT SINGLE *
        FROM zwm013 INTO ls_zwm013
        WHERE armazem = xuser-lgnum
        AND   sscc    = p_su.

      IF sy-subrc = 0.
        SELECT *
          FROM ltap INTO TABLE lt_ltap
          WHERE lgnum = xuser-lgnum
          AND   ablad = p_su.
      ENDIF.

      " Palete de expedição - Carregar na Gravitica
    ELSE.
      SELECT *
        FROM zwm078 INTO TABLE lt_zwm078
        FOR ALL ENTRIES IN lt_ltap
        WHERE lgnum = lt_ltap-lgnum
        AND   tanum = lt_ltap-tanum.
    ENDIF.

    IF ls_zwm013 IS NOT INITIAL OR lt_zwm078[] IS NOT INITIAL.
      lv_ot_rej_dummy = 'X'.
    ELSE.
      DELETE lt_ltap WHERE vltyp <> 'EAU'.
      DELETE lt_ltap WHERE nltyp = 'REJ'.
    ENDIF.

    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM ltak INTO TABLE lt_ltak
        FOR ALL ENTRIES IN lt_ltap
        WHERE lgnum = lt_ltap-lgnum
        AND   tanum = lt_ltap-tanum.

      SORT lt_ltak BY bdatu DESCENDING bzeit DESCENDING.
    ENDIF.

    CLEAR lt_ltak.
    READ TABLE lt_ltak INDEX 1.

    READ TABLE lt_ltap WITH KEY tanum = lt_ltak-tanum.
    IF sy-subrc <> 0.
      CLEAR text.
      WRITE p_su TO text LEFT-JUSTIFIED.

      " Para SU & não existe OT de entrada no WCS
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '370'
          message_var1   = text.

      CLEAR p_su.
      EXIT.

    ELSE.
      SELECT SINGLE *
        FROM ltak INTO ls_ltak
        WHERE lgnum = lt_ltap-lgnum
        AND   tanum = lt_ltap-tanum.
    ENDIF.

** Rejeição do Picking
    IF p_lgpla = 'EXP2_REJ'.

      " OT com IDOC de entrada no WCS
      IF ls_ltak-bwlvs <> lv_pick_man AND ls_ltak-bwlvs <> lv_pick_aut.
        CLEAR text.
        WRITE p_su TO text LEFT-JUSTIFIED.

        " Para SU & não existe OT de picking de entrada no WCS
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '372'
            message_var1   = text.

        CLEAR p_su.
        EXIT.
      ENDIF.

** Rejeição Entradas de Paletes e Paletização Especial
    ELSEIF p_lgpla = 'MAN_REJ'.

*      SELECT *
*       FROM ltap INTO TABLE lt_ltap_epe
*       WHERE lgnum = xuser-lgnum
*       AND   vlenr = p_su.
*
*      READ TABLE lt_ltap_epe WITH KEY vltyp = 'EPE'.
*
*      IF sy-subrc <> 0 OR lt_ltap-vltyp <> 'EAU'.
*        CLEAR text.
*        WRITE p_su TO text LEFT-JUSTIFIED.
*
*        " Para SU & não existe OT de entrada no WCS
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = 'ZWMMSG001'
*            message_lang   = sy-langu
*            message_type   = 'E'
*            message_number = '370'
*            message_var1   = text.
*
*        CLEAR p_su.
*        EXIT.
*      ENDIF.

** Rejeição - Entrada de Terceiros
    ELSEIF p_lgpla = 'EXP1_REJ'.

    ELSEIF p_lgpla = 'EXP4_ENTRJ'.

    ENDIF.

** Validar Estorno da OT
    IF lt_ltap-vorga <> 'ST' AND lt_ltap-vorga <> 'SL'.

      " OT de Expedição
      READ TABLE lt_zwm078 INDEX 1.
      IF sy-subrc = 0.
        IF lt_zwm078-rej IS INITIAL.

          LOOP AT lt_zwm078.

            READ TABLE lt_ltap WITH KEY tanum = lt_zwm078-tanum
                                        tapos = lt_zwm078-tapos.
            CHECK sy-subrc = 0.

            MOVE-CORRESPONDING lt_ltap TO lt_ltap_vb.
            APPEND lt_ltap_vb.
          ENDLOOP.

          CALL FUNCTION 'ZWM_CREATE_IDOC_TO_CANCEL'
            EXPORTING
              i_lgnum   = lt_ltap-lgnum
              i_tanum   = lt_ltap-tanum
            TABLES
              t_ltap_vb = lt_ltap_vb
            EXCEPTIONS
              error     = 1
              OTHERS    = 2.

          IF sy-subrc <> 0.
            text = lt_ltap-tanum.

            " Erro ao cancelar ot &!
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '371'
                message_var1   = text.

            CLEAR p_su.
            EXIT.

          ELSE.
            LOOP AT lt_zwm078.
              UPDATE zwm078 SET rej = 'X'
              WHERE lgnum = lt_zwm078-lgnum
              AND   tanum = lt_zwm078-tanum
              AND   tapos = lt_zwm078-tapos.
            ENDLOOP.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.

        " Estornar OT
        REFRESH: lt_ltap_cancl.

        SELECT tanum tapos
          FROM ltap INTO TABLE lt_ltap_cancl
          WHERE lgnum = lt_ltap-lgnum
          AND   tanum = lt_ltap-tanum.

        " Validar se IDOC foi enviado manualmanete para WCS
        SELECT SINGLE *
          FROM t327a INTO ls_t327a
          WHERE lgnum = lt_ltap-lgnum
          AND   vltyp = lt_ltap-vltyp
          AND   nltyp = lt_ltap-nltyp
          AND   bwlvs = ls_ltak-bwlvs.

        IF ls_t327a-kzina IS NOT INITIAL.

          " 1- Sistema SAP + IDOC WCS (EXIT)
          CALL FUNCTION 'L_TO_CANCEL'
            EXPORTING
              i_lgnum       = lt_ltap-lgnum
              i_tanum       = lt_ltap-tanum
              i_subst       = 'X'
            TABLES
              t_ltap_cancl  = lt_ltap_cancl
            EXCEPTIONS
              error_message = 99.

        ELSE.

          " 1 - IDOC WCS
          CALL FUNCTION 'L_TO_CANCEL'
            EXPORTING
              i_lgnum       = lt_ltap-lgnum
              i_tanum       = lt_ltap-tanum
*             i_subst       = lt_ltap-kzsub
            TABLES
              t_ltap_cancl  = lt_ltap_cancl
            EXCEPTIONS
              error_message = 99.

          IF sy-subrc = 0.
            COMMIT WORK AND WAIT.

            " 2- Sistema SAP
            CALL FUNCTION 'L_TO_CANCEL'
              EXPORTING
                i_lgnum       = lt_ltap-lgnum
                i_tanum       = lt_ltap-tanum
                i_subst       = 'X'
              TABLES
                t_ltap_cancl  = lt_ltap_cancl
              EXCEPTIONS
                error_message = 99.
          ENDIF.

        ENDIF.

        IF sy-subrc <> 0.
          text = lt_ltap-tanum.

          " Erro ao cancelar ot &!
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '371'
              message_var1   = text.

          CLEAR p_su.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

** Criar OT Dummy de Rejeição
    IF lv_ot_rej_dummy = 'X'.

      CLEAR lv_tanum.

      lv_lznum = p_su.

      CALL FUNCTION 'ZWM_CREATE_TO_CHD'
        EXPORTING
          i_lgnum    = xuser-lgnum
          i_lgpla    = p_lgpla
          i_lznum    = lv_lznum
          i_type_mov = 'R'
        IMPORTING
          e_to_dummy = lv_tanum
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

** Criar OT para Tipo deposito Rejeição
    ELSE.
      CLEAR lv_tanum.

      CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
        EXPORTING
          i_lenum       = p_su
          i_bwlvs       = lv_wm_rej
          i_lznum       = ls_ltak-lznum
          i_letyp       = lt_ltap-letyp
          i_nltyp       = ls_lagp-lgtyp
          i_nlpla       = ls_lagp-lgpla
          i_commit_work = 'X'
        IMPORTING
          e_tanum       = lv_tanum
        EXCEPTIONS
          error_message = 99.
    ENDIF.

    IF sy-subrc <> 0 OR lv_tanum IS INITIAL.

      ls_return-msgid  = sy-msgid.
      ls_return-msgtyp = sy-msgty.
      ls_return-msgnr  = sy-msgno.
      ls_return-msgv1  = sy-msgv1.
      ls_return-msgv2  = sy-msgv2.
      ls_return-msgv3  = sy-msgv3.
      ls_return-msgv4  = sy-msgv4.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_return-msgid
          message_lang   = sy-langu
          message_type   = ls_return-msgtyp
          message_number = ls_return-msgnr
          message_var1   = ls_return-msgv1
          message_var2   = ls_return-msgv2
          message_var3   = ls_return-msgv3
          message_var4   = ls_return-msgv4.

      CLEAR p_su.
      EXIT.
    ELSE.
      GET TIME.

      IF lv_ent_pick IS NOT INITIAL.
        p_lgpla = lv_ent_pick.
      ENDIF.

      CLEAR ls_zwm077.
      ls_zwm077-lgnum = xuser-lgnum.
      ls_zwm077-exidv = p_su.
      ls_zwm077-tanum = lv_tanum.
      ls_zwm077-lgpla = p_lgpla.
      ls_zwm077-rdatu = sy-datum.
      ls_zwm077-rzeit = sy-uzeit.
      ls_zwm077-rname = sy-uname.

      MODIFY zwm077 FROM ls_zwm077.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDIF.

** Validar Paletes de saída nas mesas p/ arm. convencional
**********************************************************************
  REFRESH lt_zwm077.

  SELECT *
    FROM zwm077 INTO TABLE lt_zwm077
    WHERE lgnum = xuser-lgnum
    AND   exidv = p_su.

  DELETE lt_zwm077 WHERE pvqui IS NOT INITIAL.

  SORT lt_zwm077 BY rdatu DESCENDING rzeit DESCENDING.

  READ TABLE lt_zwm077 INDEX 1.
  IF sy-subrc <> 0.
    CLEAR text.
    WRITE p_su TO text LEFT-JUSTIFIED.

    " Para SU & não existe nenhuma chamada criada!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '354'
        message_var1   = text.

    CLEAR p_su.
    EXIT.
  ENDIF.

** Validar OT
  SELECT *
    FROM ltak INTO TABLE lt_ltak
    WHERE lgnum = xuser-lgnum
    AND   tanum = lt_zwm077-tanum.

  IF lt_ltak[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_ltak
      WHERE lgnum = lt_ltak-lgnum
      AND   tanum = lt_ltak-tanum.
  ENDIF.

  DELETE lt_ltap WHERE vorga = 'ST' OR
                       vorga = 'SL'.

  DELETE lt_ltap WHERE pquit = 'X'  OR
                       pvqui = 'X'.

  SORT lt_ltap BY tapos.

  READ TABLE lt_ltap INDEX 1.
  IF sy-subrc <> 0.
    CLEAR text.
    WRITE p_su TO text LEFT-JUSTIFIED.

    " Para SU & não existe nenhuma OT em aberto
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '352'
        message_var1   = text.

    CLEAR p_su.
    EXIT.
  ENDIF.

** Validar Posição
  IF lt_zwm077-lgpla <> p_lgpla.

    CLEAR: text, text1.
    WRITE p_su    TO text  LEFT-JUSTIFIED.
    WRITE p_lgpla TO text1 LEFT-JUSTIFIED.

    " SU & não pertence à posição & de chamada.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '353'
        message_var1   = text
        message_var2   = text1.

    CLEAR p_su.
    EXIT.
  ENDIF.

** Pick da Nova OT
**********************************************************************
  CLEAR lv_su.

  IF lt_ltap-vltyp <> 'CHD'.
    READ TABLE lt_ltap WITH KEY vlenr = p_su.
    IF sy-subrc <> 0.
      READ TABLE lt_ltap WITH KEY nlenr = p_su.
      IF sy-subrc <> 0.
        lv_su = p_su. " OT Bloco
      ENDIF.
    ENDIF.
  ENDIF.

** Validar Palete remontada
  SELECT SINGLE *
    FROM zwm020 INTO ls_zwm020
    WHERE armazem = xuser-lgnum
    AND   p1      = p_su.

** Confirmar
  READ TABLE lt_ltak INDEX 1.

  PERFORM get_parameter
          USING xuser-lgnum
                'GESTAO_FILAS'
                'FILA_S_AUT_PAL_PRM'
                  lv_queue_aut_prm.

** Fila AUT -> PRM (OT de bloco com 2 items)
  IF lt_ltak-queue = lv_queue_aut_prm.

    LOOP AT lt_ltap.
      CLEAR lt_ltap_conf.
      lt_ltap_conf-tanum = lt_ltap-tanum.
      lt_ltap_conf-tapos = lt_ltap-tapos.

      " OT de Bloco
      IF lv_su IS NOT INITIAL.

        IF sy-tabix = 1.
          lt_ltap_conf-lenum = ls_zwm020-p2.
        ELSEIF sy-tabix = 2.
          lt_ltap_conf-lenum = p_su.
        ENDIF.

        SELECT SINGLE *
          FROM lqua INTO ls_lqua
          WHERE lgnum = xuser-lgnum
          AND   lenum = lt_ltap_conf-lenum.

        IF sy-subrc = 0.
          MOVE ls_lqua-verme TO lt_ltap_conf-pickm.
          MOVE ls_lqua-meins TO lt_ltap_conf-altme.
        ENDIF.

        APPEND lt_ltap_conf.
      ENDIF.
    ENDLOOP.

  ELSE.
    LOOP AT lt_ltap.
      CLEAR lt_ltap_conf.
      lt_ltap_conf-tanum = lt_ltap-tanum.
      lt_ltap_conf-tapos = lt_ltap-tapos.

      IF lv_su IS NOT INITIAL.

        lt_ltap_conf-lenum = p_su.

        SELECT SINGLE *
          FROM lqua INTO ls_lqua
          WHERE lgnum = xuser-lgnum
          AND   lenum = p_su.

        IF sy-subrc = 0.
          MOVE ls_lqua-verme TO lt_ltap_conf-pickm.
          MOVE ls_lqua-meins TO lt_ltap_conf-altme.
        ENDIF.

        APPEND lt_ltap_conf.

        " Palete remontada
        IF ls_zwm020-p2 IS NOT INITIAL.
          lt_ltap_conf-lenum = ls_zwm020-p2.

          CLEAR ls_lqua.
          SELECT SINGLE *
            FROM lqua INTO ls_lqua
            WHERE lgnum = xuser-lgnum
            AND   lenum = ls_zwm020-p2.

          IF sy-subrc = 0.
            MOVE ls_lqua-verme TO lt_ltap_conf-pickm.
            MOVE ls_lqua-meins TO lt_ltap_conf-altme.
          ENDIF.

          APPEND lt_ltap_conf.
        ENDIF.

      ELSE.
        lt_ltap_conf-altme = lt_ltap-meins.
        lt_ltap_conf-nista = lt_ltap-vsolm.
        APPEND lt_ltap_conf.
      ENDIF.
    ENDLOOP.
  ENDIF.

  " Pisco sistema externo
  IF lt_ltap-kzsub = 'X'. "vltyp = 'AUT' OR lt_ltap-vltyp = 'EAU'. "lt_ltak-bwlvs = lv_pick_man.
    lv_subst = 'X'.
  ENDIF.

  " Pick da OT
  CALL FUNCTION 'L_TO_CONFIRM'
    EXPORTING
      i_lgnum       = xuser-lgnum
      i_tanum       = lt_ltap-tanum
      i_quknz       = '1'
      i_subst       = lv_subst
    TABLES
      t_ltap_conf   = lt_ltap_conf
    EXCEPTIONS
      error_message = 99.

  IF sy-subrc <> 0.
    ls_return-msgid  = sy-msgid.
    ls_return-msgtyp = sy-msgty.
    ls_return-msgnr  = sy-msgno.
    ls_return-msgv1  = sy-msgv1.
    ls_return-msgv2  = sy-msgv2.
    ls_return-msgv3  = sy-msgv3.
    ls_return-msgv4  = sy-msgv4.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = ls_return-msgid
        message_lang   = sy-langu
        message_type   = ls_return-msgtyp
        message_number = ls_return-msgnr
        message_var1   = ls_return-msgv1
        message_var2   = ls_return-msgv2
        message_var3   = ls_return-msgv3
        message_var4   = ls_return-msgv4.

    CLEAR: p_su.
    EXIT.
  ENDIF.

** Atualizar Status de Pick de Palete de saída de mesa
  GET TIME.
  lt_zwm077-pvqui = 'X'.
  lt_zwm077-edatu = sy-datum.
  lt_zwm077-ezeit = sy-uzeit.
  lt_zwm077-ename = sy-uname.

  MODIFY zwm077 FROM lt_zwm077.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ENDIF.

** Guardar na persistência de system-guided

  " Arrumação no PRM (subtituir pela palete de cima)
  IF lt_ltak-queue = lv_queue_aut_prm AND
     ls_zwm020-p2 IS NOT INITIAL.

    p_su = ls_zwm020-p2.
  ENDIF.

  REFRESH: lt_ltap.

  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = xuser-lgnum
    AND   tanum = lt_ltap-tanum.

  READ TABLE lt_ltap WITH KEY vlenr = p_su.
  IF sy-subrc <> 0.
    READ TABLE lt_ltap WITH KEY nlenr = p_su.
    IF sy-subrc <> 0.
      READ TABLE lt_ltap INDEX 1.
    ENDIF.
  ENDIF.

  IF sy-subrc = 0.
    CLEAR ls_zwm011.
    ls_zwm011-armazem     = xuser-lgnum.
    ls_zwm011-to_number   = lt_ltap-tanum.
    ls_zwm011-to_item     = lt_ltap-tapos.
    ls_zwm011-status      = 'P'.
    ls_zwm011-user_name   = sy-uname.
    ls_zwm011-equipamento = equipamento_.

    READ TABLE lt_ltak WITH KEY tanum = lt_ltap-tanum.
    IF sy-subrc = 0.
      ls_zwm011-queue = lt_ltak-queue.
    ENDIF.

    MODIFY zwm011 FROM ls_zwm011.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

** Confirmar OT Dummy
**********************************************************************
  CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
    EXPORTING
      armazem              = xuser-lgnum
      confirm_type         = 'B'
    TABLES
      return_msg           = lt_return_msg
    CHANGING
      to                   = to_ret
    EXCEPTIONS
      confirm_type_error   = 1
      to_not_found         = 2
      to_already_confirmed = 3
      to_already_picked    = 4
      to_not_picked        = 5
      wrong_su             = 6
      missing_su           = 7
      error                = 8
      OTHERS               = 9.

  IF sy-subrc <> 0.
    READ TABLE lt_return_msg INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = lt_return_msg-msgid
          message_lang   = sy-langu
          message_type   = lt_return_msg-msgtyp
          message_number = lt_return_msg-msgnr
          message_var1   = lt_return_msg-msgv1
          message_var2   = lt_return_msg-msgv2
          message_var3   = lt_return_msg-msgv3.
    ENDIF.
  ENDIF.

** Anular entrada da tabela de assignação de utilizador a TO e criar
  DELETE FROM zwm011
         WHERE armazem   = xuser-lgnum AND
               to_number = to_ret      AND
               user_name = sy-uname.
  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.

** Palete remontada para o PRM
  IF lt_ltak-queue = lv_queue_aut_prm AND ls_zwm020-p2 IS NOT INITIAL.

*    CONCATENATE lt_ltap-nltyp lt_ltap-nlpla INTO bin_output_d.

    " Palete remontada para o PRM. Arrumar primeiro palete de cima
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '364'.
  ENDIF.

** Atribuição de nova tarefa
**********************************************************************
  REFRESH tab_zwm010.

  SELECT * FROM zwm010 INTO TABLE tab_zwm010
       WHERE armazem = xuser-lgnum AND
             equipamento = equipamento_.

  CALL FUNCTION 'ZWM_GET_TO_RET'
    EXPORTING
      armazem           = xuser-lgnum
      tamanho           = lrf_wkqu-devty(5)
      add_new_to        = ' '
    IMPORTING
      nova_to           = tab_zwm011
      tipo_queue        = tipo
    TABLES
      l_zwm010          = tab_zwm010
      return_msg        = return_msg
    EXCEPTIONS
      no_equipment      = 1
      no_work_available = 2
      OTHERS            = 3.

  IF sy-subrc = 0.
    CLEAR: to_ret, item_ret.

    CALL FUNCTION 'ZWM_CALL_TASK_RET'
      EXPORTING
        armazem     = xuser-lgnum
        ecran       = ecran
        tab_zwm011  = tab_zwm011
        tipo_queue  = tipo
        equipamento = equipamento_.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_MESA_AUT_WCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_BIN_OUTPUT_D  text
*----------------------------------------------------------------------*
FORM check_mesa_aut_wcs.

  DATA: lv_param           TYPE zwm_parametro.
  DATA: lv_lgtyp           TYPE lgtyp.
  DATA: lv_lgpla           TYPE lgpla.
  DATA: lv_queue_abast_bpe TYPE lrf_queue.
  DATA: lv_queue_sai_aut   TYPE lrf_queue.
  DATA: lv_queue           TYPE lrf_queue.
  DATA: ls_zwm077          TYPE zwm077.
  DATA: ls_zwm028          TYPE zwm028.
  DATA: ls_ltak            TYPE ltak.
  DATA: lt_ltap            TYPE TABLE OF ltap WITH HEADER LINE.

** Mesa de Entrada WCS
**********************************************************************
  IF bin_output_d(3) = 'EAU'.

** Buffer Paletização Especial
    PERFORM get_parameter
       USING xuser-lgnum
             'GESTAO_FILAS'
             'FILA_ABAST_BPE'
             lv_queue_abast_bpe.

** Validar fila
    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = xuser-lgnum
      AND   tanum = to_ret.

    IF sy-subrc = 0.
      SELECT *
        FROM ltap INTO TABLE lt_ltap
        WHERE lgnum = xuser-lgnum
        AND   tanum = to_ret.

      READ TABLE lt_ltap INDEX 1.
    ENDIF.

    " Se é uma OT de Abastecimento BPE
    IF ls_ltak-queue = lv_queue_abast_bpe.
      lv_lgtyp = 'BPE'.

    ELSE.
      lv_lgtyp = bin_output_o(3).
    ENDIF.

    CALL FUNCTION 'ZWM_GET_MESA_WCS'
      EXPORTING
        i_lgnum = xuser-lgnum
        i_lgtyp = lv_lgtyp
        i_type  = 'E'
      IMPORTING
        e_mesa  = lv_lgpla
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.

** Obter Mesa de Entrada no WCS
    IF lv_lgpla IS NOT INITIAL.
      bin_output_d+4(10) = lv_lgpla.
    ENDIF.

** Expedição Gravítica no Automático
**********************************************************************
  ELSEIF bin_output_d(3) = 'PUA'.

    lv_lgtyp = bin_output_d(3).

    CALL FUNCTION 'ZWM_GET_MESA_WCS'
      EXPORTING
        i_lgnum = xuser-lgnum
        i_lgtyp = lv_lgtyp
        i_type  = 'E'
      IMPORTING
        e_mesa  = lv_lgpla
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.

    IF lv_lgpla IS NOT INITIAL.
      bin_output_d+4(10) = lv_lgpla.
    ENDIF.

** Mesa de Saída WCS
**********************************************************************
  ELSE.
    SELECT SINGLE *
      FROM zwm077 INTO ls_zwm077
      WHERE lgnum = xuser-lgnum
      AND   tanum = to_ret.

    IF sy-subrc = 0.
      bin_output_o+4(10) = ls_zwm077-lgpla.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIRMA_TO_ENT_AUT_WCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LGPLA  text
*      -->P_LGTYP  text
*----------------------------------------------------------------------*
FORM confirma_to_ent_aut_wcs USING p_lgpla p_lgtyp.

  DATA: lv_to_tri TYPE tanum.
  DATA: lv_lgpla  TYPE lgpla.
  DATA: lv_ablad  TYPE ablad.
  DATA: lt_ltap   TYPE TABLE OF ltap WITH HEADER LINE.
  DATA: ls_zwm011 TYPE zwm011.
  DATA: ls_ltak   TYPE ltak.

** Mensulas
**********************************************************************
  CLEAR : confirm_type,
        lgtyp_des,
        lgpla_des,
        corredor,
        n_mensulas_ocupadas.

  IF p_lgtyp = 'MEN'.
    PERFORM confirma_to_total USING lgpla lgtyp.

    CHECK bin_input IS NOT INITIAL.

    " Substituir OT de Mensula por OT real do Trilateral
    IF p_lgtyp = 'MEN'.
      SELECT *
        FROM ltap INTO TABLE lt_ltap
        WHERE lgnum = xuser-lgnum
        AND   vlenr = su.

      DELETE lt_ltap WHERE pquit = 'X'.

      READ TABLE lt_ltap INDEX 1.
      IF sy-subrc = 0.
        lv_to_tri = lt_ltap-tanum.
      ENDIF.
    ENDIF.

  ENDIF.

** Gravitica do automático
**********************************************************************
  lv_lgpla = bin_output_d+4(10).

  IF bin_output_d(3) = 'PUA'.

    IF lv_to_tri IS NOT INITIAL.
      to_ret = lv_to_tri.
    ENDIF.

    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = xuser-lgnum
      AND   tanum = to_ret.

    lv_ablad = lv_lgpla.

    " Atualizar ponto de entrega
*    CALL FUNCTION 'ZWM_UPDATE_POS_ENT_WCS'
*      EXPORTING
*        i_lgnum = xuser-lgnum
*        i_tanum = to_ret
*        i_su    = su
*        i_lgpla = lv_lgpla.

    " Gerar novo idoc com os SSCCs das paletes
    CALL FUNCTION 'ZWM_IDOC_FREE_WORK_WCS'
      EXPORTING
        i_lgnum  = ls_ltak-lgnum
        i_tanum  = ls_ltak-tanum
        i_vlenr  = su
        i_refnr  = ls_ltak-refnr
        i_vbeln  = ls_ltak-benum
        i_resend = 'X'
        i_ablad  = lv_ablad.

** Mesa de entrada
**********************************************************************
  ELSE.

    " Confirmar OT
    IF p_lgtyp = 'DRI' OR p_lgtyp = 'BLK'.
      confirm_type = 'B'.
      to_ret2 = to_ret.
    ELSE.
      confirm_type = 'T'.
      to_ret2 = to_ret.

      IF lv_to_tri IS NOT INITIAL.
        to_ret2 = lv_to_tri.
      ENDIF.

    ENDIF.

    " Registar ponto de entrega da OT
    CALL FUNCTION 'ZWM_UPDATE_POS_ENT_WCS'
      EXPORTING
        i_lgnum = xuser-lgnum
        i_tanum = to_ret2
        i_su    = su
        i_lgpla = lv_lgpla.

    CLEAR : return_msg.
    REFRESH : return_msg.

    CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
      EXPORTING
        armazem              = xuser-lgnum
        confirm_type         = confirm_type
      TABLES
        return_msg           = return_msg
      CHANGING
        to                   = to_ret2
      EXCEPTIONS
        confirm_type_error   = 1
        to_not_found         = 2
        to_already_confirmed = 3
        to_already_picked    = 4
        to_not_picked        = 5
        wrong_su             = 6
        missing_su           = 7
        error                = 8
        OTHERS               = 9.

    IF sy-subrc <> 0.
      READ TABLE return_msg INDEX 1.
      IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1
            message_var2   = return_msg-msgv2
            message_var3   = return_msg-msgv3.

        CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
               cursorfield, ok_code_0001.
        MOVE 'BIN_INPUT' TO cursorfield.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZWM_MODIFY_ZWM011'
    EXPORTING
      armazem            = xuser-lgnum
      to_number          = to_ret
      to_item            = item_ret
      status             = 'T'
    EXCEPTIONS
      error_update_table = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIRMA_TO_REM_ENT_PRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LGPLA  text
*      -->P_LGTYP  text
*----------------------------------------------------------------------*
FORM confirma_to_rem_ent_prm  USING  p_lgpla p_lgtyp.

  DATA: lv_subst     TYPE rl03t-subst.
  DATA: lv_tanum     TYPE tanum.
  DATA: ls_ltak      TYPE ltak.
  DATA: ls_ltap      TYPE ltap.
  DATA: ls_zwm020    TYPE zwm020.
  DATA: ls_return    TYPE bdcmsgcoll.
  DATA: ls_zwm011    TYPE zwm011.
  DATA: lt_ltap      TYPE TABLE OF ltap      WITH HEADER LINE.
  DATA: lt_ltap_conf TYPE TABLE OF ltap_conf WITH HEADER LINE.

** Validar OT
**********************************************************************
  lv_tanum = to_ret.

** Origem Mensula
  IF p_lgtyp = 'MEN'.

    SELECT SINGLE *
      FROM ltap INTO ls_ltap
      WHERE lgnum = xuser-lgnum
      AND   nlenr = su
      AND   pquit = ''.

    IF sy-subrc = 0.
      to_ret   = ls_ltap-tanum.
      item_ret = ls_ltap-tapos.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum = xuser-lgnum
    AND   tanum = to_ret.

  IF sy-subrc = 0.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = ls_ltak-lgnum
      AND   tanum = ls_ltak-tanum.
  ENDIF.

  DELETE lt_ltap WHERE vorga = 'ST' OR
                       vorga = 'SL'.

  DELETE lt_ltap WHERE pquit = 'X'.

** Confirmar OT
**********************************************************************
  READ TABLE lt_ltap INDEX 1.

  CLEAR zwm020.
  SELECT SINGLE *
    FROM zwm020
    WHERE armazem = xuser-lgnum
    AND ( p1 = su OR p2 = su ).

  " Paletes para zona de remontadas
  READ TABLE lt_ltap WITH KEY tanum = to_ret
                              tapos = item_ret.
  IF sy-subrc = 0.
    CLEAR lt_ltap_conf.
    lt_ltap_conf-tanum = lt_ltap-tanum.
    lt_ltap_conf-tapos = lt_ltap-tapos.
    lt_ltap_conf-altme = lt_ltap-meins.
    lt_ltap_conf-nista = lt_ltap-vsolm.
    APPEND lt_ltap_conf.
  ENDIF.

  " Transfer da OT
  CALL FUNCTION 'L_TO_CONFIRM'
    EXPORTING
      i_lgnum       = xuser-lgnum
      i_tanum       = lt_ltap-tanum
      i_quknz       = '2'
      i_subst       = lv_subst
    TABLES
      t_ltap_conf   = lt_ltap_conf
    EXCEPTIONS
      error_message = 99.

  IF sy-subrc <> 0.
    ls_return-msgid  = sy-msgid.
    ls_return-msgtyp = sy-msgty.
    ls_return-msgnr  = sy-msgno.
    ls_return-msgv1  = sy-msgv1.
    ls_return-msgv2  = sy-msgv2.
    ls_return-msgv3  = sy-msgv3.
    ls_return-msgv4  = sy-msgv4.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = ls_return-msgid
        message_lang   = sy-langu
        message_type   = ls_return-msgtyp
        message_number = ls_return-msgnr
        message_var1   = ls_return-msgv1
        message_var2   = ls_return-msgv2
        message_var3   = ls_return-msgv3
        message_var4   = ls_return-msgv4.

    CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
               cursorfield, ok_code_0001.
    MOVE 'BIN_INPUT' TO cursorfield.
    EXIT.
  ENDIF.

** Confirmação depósito de remontadas (separação paletes)
  IF lt_ltap-nltyp = 'PRM' AND zwm020 IS NOT INITIAL.

    DELETE FROM zwm020 WHERE armazem = xuser-lgnum AND
                             p2      = zwm020-p2.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.

    DELETE FROM zwm011
             WHERE armazem   = xuser-lgnum AND
                   to_number = lv_tanum    AND
                   user_name = sy-uname.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.

    " Executar a tarefa de arrumação da palete remontada
    READ TABLE lt_ltap WITH KEY vlenr = zwm020-p1.
    IF sy-subrc = 0.
      CLEAR ls_zwm011.
      ls_zwm011-armazem     = xuser-lgnum.
      ls_zwm011-to_number   = lt_ltap-tanum.
      ls_zwm011-to_item     = lt_ltap-tapos.

      ls_zwm011-status      = 'P'.
      ls_zwm011-user_name   = sy-uname.
      ls_zwm011-equipamento = equipamento_.
      ls_zwm011-queue       = ls_ltak-queue.

      MODIFY zwm011 FROM ls_zwm011.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    " Confirmar OT da Mensula
    IF p_lgtyp = 'MEN'.
      CLEAR : return_msg.
      REFRESH : return_msg.

      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'T'
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = lv_tanum
        EXCEPTIONS
          confirm_type_error   = 1
          to_not_found         = 2
          to_already_confirmed = 3
          to_already_picked    = 4
          to_not_picked        = 5
          wrong_su             = 6
          missing_su           = 7
          error                = 8
          OTHERS               = 9.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIRMA_TO_SAI_AUT_WCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LGPLA  text
*      -->P_LGTYP  text
*----------------------------------------------------------------------*
FORM confirma_to_sai_aut_wcs USING p_lgpla p_lgtyp.

  DATA: lv_subst      TYPE rl03t-subst.
  DATA: lv_tanum      TYPE tanum.
  DATA: lv_bwlvs_pick TYPE bwlvs.
  DATA: ls_ltak       TYPE ltak.
  DATA: ls_zwm020     TYPE zwm020.
  DATA: ls_return     TYPE bdcmsgcoll.
  DATA: ls_zwm011     TYPE zwm011.
  DATA: lt_ltap       TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_ltap_conf  TYPE TABLE OF ltap_conf  WITH HEADER LINE.
  DATA: lt_ltap_creat TYPE TABLE OF ltap_creat WITH HEADER LINE.

** Validar OT
**********************************************************************
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = xuser-lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_MAN_PICK'
    IMPORTING
      e_valor     = lv_bwlvs_pick
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum = xuser-lgnum
    AND   tanum = to_ret.

  IF sy-subrc = 0.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = ls_ltak-lgnum
      AND   tanum = ls_ltak-tanum.
  ENDIF.

  DELETE lt_ltap WHERE vorga = 'ST' OR
                       vorga = 'SL'.

  DELETE lt_ltap WHERE pquit = 'X'.

** Confirmar OT
**********************************************************************
  READ TABLE lt_ltap INDEX 1.

  CLEAR zwm020.
  SELECT SINGLE *
    FROM zwm020
    WHERE armazem = xuser-lgnum
    AND ( p1 = su OR p2 = su ).

  IF sy-subrc = 0.
    to3 = 'X'.
  ENDIF.

** Confirmação no pulmão
  IF bin_output_d(3) = 'PLT' OR bin_output_d(3) = 'PUL'.

    " Palete Picking
    IF ls_ltak-bwlvs = lv_bwlvs_pick.

      LOOP AT lt_ltap.
        CLEAR lt_ltap_conf.
        lt_ltap_conf-tanum = lt_ltap-tanum.
        lt_ltap_conf-tapos = lt_ltap-tapos.
        lt_ltap_conf-altme = lt_ltap-meins.
        lt_ltap_conf-nista = lt_ltap-vsolm.
        APPEND lt_ltap_conf.
      ENDLOOP.

      lv_subst = 'X'.

      " Transfer da OT
      CALL FUNCTION 'L_TO_CONFIRM'
        EXPORTING
          i_lgnum       = xuser-lgnum
          i_tanum       = lt_ltap-tanum
          i_quknz       = '2'
          i_subst       = lv_subst
        TABLES
          t_ltap_conf   = lt_ltap_conf
        EXCEPTIONS
          error_message = 99.

      IF sy-subrc <> 0.
        ls_return-msgid  = sy-msgid.
        ls_return-msgtyp = sy-msgty.
        ls_return-msgnr  = sy-msgno.
        ls_return-msgv1  = sy-msgv1.
        ls_return-msgv2  = sy-msgv2.
        ls_return-msgv3  = sy-msgv3.
        ls_return-msgv4  = sy-msgv4.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = ls_return-msgid
            message_lang   = sy-langu
            message_type   = ls_return-msgtyp
            message_number = ls_return-msgnr
            message_var1   = ls_return-msgv1
            message_var2   = ls_return-msgv2
            message_var3   = ls_return-msgv3
            message_var4   = ls_return-msgv4.

        CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
               cursorfield, ok_code_0001.

        MOVE 'BIN_INPUT' TO cursorfield.
        EXIT.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ZWM_MODIFY_ZWM011'
      EXPORTING
        armazem            = xuser-lgnum
        to_number          = to_ret
        to_item            = item_ret
        status             = 'T'
      EXCEPTIONS
        error_update_table = 1
        OTHERS             = 2.

    PERFORM actualiza_paletes_pulmao.

    " Atualizar tabela de controlo de expedição de WCS
    GET TIME.

    UPDATE zwm078 SET pulmao  = abap_true
                       pdatu  = sy-datum
                       pzeit  = sy-uzeit
                       pname  = sy-uname
                       nlpla  = bin_output_d+4(10)
                       lenum  = su
                      WHERE  lgnum = xuser-lgnum
                      AND    tanum = to_ret.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.

  ELSE.

    " Paletes para zona de remontadas
    IF lt_ltap-nltyp = 'PRM'.

      READ TABLE lt_ltap WITH KEY tanum = to_ret
                                  tapos = item_ret.
      IF sy-subrc = 0.
        CLEAR lt_ltap_conf.
        lt_ltap_conf-tanum = lt_ltap-tanum.
        lt_ltap_conf-tapos = lt_ltap-tapos.
        lt_ltap_conf-altme = lt_ltap-meins.
        lt_ltap_conf-nista = lt_ltap-vsolm.
        APPEND lt_ltap_conf.
      ENDIF.

    ELSE.
      LOOP AT lt_ltap.
        CLEAR lt_ltap_conf.
        lt_ltap_conf-tanum = lt_ltap-tanum.
        lt_ltap_conf-tapos = lt_ltap-tapos.
        lt_ltap_conf-altme = lt_ltap-meins.
        lt_ltap_conf-nista = lt_ltap-vsolm.
        APPEND lt_ltap_conf.
      ENDLOOP.
    ENDIF.

    " Pisco sistema externo
    IF lt_ltap-vlpla = 'AUT' OR
       ls_ltak-bwlvs = lv_bwlvs_pick.
      lv_subst = 'X'.
    ENDIF.

    " Transfer da OT
    CALL FUNCTION 'L_TO_CONFIRM'
      EXPORTING
        i_lgnum       = xuser-lgnum
        i_tanum       = lt_ltap-tanum
        i_quknz       = '2'
        i_subst       = lv_subst
      TABLES
        t_ltap_conf   = lt_ltap_conf
      EXCEPTIONS
        error_message = 99.

    IF sy-subrc <> 0.
      ls_return-msgid  = sy-msgid.
      ls_return-msgtyp = sy-msgty.
      ls_return-msgnr  = sy-msgno.
      ls_return-msgv1  = sy-msgv1.
      ls_return-msgv2  = sy-msgv2.
      ls_return-msgv3  = sy-msgv3.
      ls_return-msgv4  = sy-msgv4.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_return-msgid
          message_lang   = sy-langu
          message_type   = ls_return-msgtyp
          message_number = ls_return-msgnr
          message_var1   = ls_return-msgv1
          message_var2   = ls_return-msgv2
          message_var3   = ls_return-msgv3
          message_var4   = ls_return-msgv4.

      CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
                 cursorfield, ok_code_0001.
      MOVE 'BIN_INPUT' TO cursorfield.
      EXIT.

    ENDIF.

** Confirmação depósito de remontadas (separação paletes)
    IF lt_ltap-nltyp = 'PRM' AND zwm020 IS NOT INITIAL.

      DELETE FROM zwm020 WHERE armazem = xuser-lgnum AND
                                p2     = zwm020-p2.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.

      DELETE FROM zwm011
               WHERE armazem   = xuser-lgnum AND
                     to_number = to_ret      AND
                     user_name = sy-uname.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.

      " Executar a tarefa de arrumação da palete de baixo da remontada
      READ TABLE lt_ltap WITH KEY vlenr = zwm020-p1.
      IF sy-subrc = 0.
        CLEAR ls_zwm011.
        ls_zwm011-armazem     = xuser-lgnum.
        ls_zwm011-to_number   = lt_ltap-tanum.
        ls_zwm011-to_item     = lt_ltap-tapos.

        ls_zwm011-status      = 'P'.
        ls_zwm011-user_name   = sy-uname.
        ls_zwm011-equipamento = equipamento_.
        ls_zwm011-queue       = ls_ltak-queue.

        MODIFY zwm011 FROM ls_zwm011.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.

** Transfer OT
    ELSE.
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = item_ret
          status             = 'T'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.
    ENDIF.

** Validar Se é uma posição de rejeição
    IF bin_output_d(3) = 'REJ'.
      PERFORM actualiza_posicao_rejeicao.
    ENDIF.

** Actualizar paletes no carro e embalar
    IF bin_output_d(3) = 'DCK'.

      " Atualizar tabela de controlo de expedição de WCS
      GET TIME.

      UPDATE zwm078 SET pulmao  = abap_true
                         pdatu  = sy-datum
                         pzeit  = sy-uzeit
                         pname  = sy-uname
                         nlpla  = bin_output_d+4(10)
                         lenum  = su
                         carga  = abap_true
                         cdatu  = sy-datum
                         czeit  = sy-uzeit
                         cname  = sy-uname
                        WHERE  lgnum = xuser-lgnum
                        AND    tanum = to_ret.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.

      PERFORM confirm_ots_pal_picking.

      PERFORM actualiza_paletes_carro.
    ENDIF.

** Buffer Paletes de Picking
    IF bin_output_d(3) = 'BPK'.
*      PERFORM create_ot_bpk.
    ENDIF.
  ENDIF.

** Validar reposição de picking
  PERFORM cria_to_picking.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXP_PAL_AUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SU  text
*----------------------------------------------------------------------*
FORM check_exp_pal_aut USING p_lgtyp p_lgpla CHANGING p_su.

  DATA: lv_bin TYPE char14.

** Mensula
  IF p_lgtyp = 'MEN'.

    PERFORM confirma_to_parcialmente USING p_lgpla.

** Outros tipos depósitos
  ELSE.

    " Verificar se a SU lida existe na posição
    SELECT SINGLE * FROM lqua
                    WHERE lgnum = xuser-lgnum AND
                          lgtyp = p_lgtyp AND
                          lgpla = p_lgpla AND
                          lenum = su.
    IF sy-subrc <> 0.
      CLEAR text.
      WRITE su TO text LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '119'
          message_var1   = text.

      CLEAR: su, ok_code_0001.
      MOVE 'SU' TO cursorfield.
    ENDIF.

    CALL FUNCTION 'ZWM_MODIFY_ZWM011'
      EXPORTING
        armazem            = xuser-lgnum
        to_number          = to_ret
        to_item            = item_ret
        su                 = su
        status             = 'P'
      EXCEPTIONS
        error_update_table = 1
        OTHERS             = 2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FREE_MENSULA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_mensula.

* fazer lock antes de actualizar a tabela das mensulas
  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM014'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

** Actualizar Mensula(desocupa-la)
  DATA wa_zwm014 LIKE zwm014.
  CLEAR wa_zwm014.

  SELECT SINGLE * FROM zwm014
                  WHERE armazem = xuser-lgnum AND
                        mensula = lgpla.
  IF sy-subrc = 0.
    CLEAR : zwm014-su,
            zwm014-estado,
            zwm014-bin,
            zwm014-prioridade.

    MOVE-CORRESPONDING zwm014 TO wa_zwm014.
    MODIFY zwm014 FROM wa_zwm014.
    COMMIT WORK AND WAIT.

** Verificar se o corredor depois da retirada da SU em análise
** ainda deve manter a prioridade máxima
    CLEAR : corredor,
            n_mensulas_ocupadas.
    CONCATENATE zwm014-mensula(3) '%' INTO corredor.
    SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                    WHERE armazem = xuser-lgnum AND
                          mensula LIKE corredor AND
                          estado = 'X'.
    IF n_mensulas_ocupadas < 3.

      UPDATE zwm014 SET prioridade = ' '
                    WHERE armazem = xuser-lgnum AND
                          mensula LIKE corredor AND
                          estado = 'X'.
      COMMIT WORK AND WAIT.

    ENDIF.

  ENDIF.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM014'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXP_AUT_WCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_EXP_AUT  text
*----------------------------------------------------------------------*
FORM check_exp_aut_wcs  CHANGING pv_exp_aut.

  DATA: ls_ltak   TYPE ltak.
  DATA: ls_zwm028 TYPE zwm028.

** Validar Expedição gravítica do Automático
  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum = armazem_tri
    AND   tanum = to_tri.

  IF ls_ltak-refnr IS NOT INITIAL.

    CALL FUNCTION 'ZWM_CHECK_TO_EXP'
      EXPORTING
        i_lgnum   = xuser-lgnum
        i_refnr   = ls_ltak-refnr
        i_vbeln   = ls_ltak-benum
      IMPORTING
        cs_zwm028 = ls_zwm028.

    " Gravítica Automático
    IF ls_zwm028-st_pul = 'PUA'.
      pv_exp_aut = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILTER_SELECT_PUL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_LAGP  text
*----------------------------------------------------------------------*
FORM filter_select_pul USING uv_lgnum uv_lgtyp CHANGING ct_bins TYPE STANDARD TABLE.

  DATA: lt_posicoes TYPE TABLE OF zwm027,
        lt_lagp     TYPE TABLE OF lagp.

  DATA: ls_posicao   TYPE zwm027,
        ls_selection TYPE slis_selfield,
        ls_lagp      TYPE lagp.

  DATA: lv_tabix      TYPE sytabix,
        lv_tabix2     TYPE sytabix,
        lv_pulmao_aux TYPE lgpla,
        lv_bin1       TYPE lgpla,
        lv_bin2       TYPE lgpla.

  FIELD-SYMBOLS: <ls_bin>         TYPE any,
                 <lv_data>        TYPE any,
                 <ls_bin2>        TYPE any,
                 <lv_data2>       TYPE any,
                 <ls_bin_select1> TYPE any,
                 <ls_bin_select2> TYPE any.

  CHECK NOT ct_bins IS INITIAL.

  LOOP AT ct_bins ASSIGNING <ls_bin>.
    lv_tabix = sy-tabix.

    UNASSIGN <lv_data>.
    ASSIGN COMPONENT 'LGPLA' OF STRUCTURE <ls_bin> TO <lv_data>.
    CHECK <lv_data> IS ASSIGNED.
    CHECK NOT <lv_data> IS INITIAL.

    lv_tabix = lv_tabix + 1.
    LOOP AT ct_bins ASSIGNING <ls_bin2> FROM lv_tabix.
      lv_tabix2 = sy-tabix.
      ASSIGN COMPONENT 'LGPLA' OF STRUCTURE <ls_bin2> TO <lv_data2>.
      CHECK <lv_data2> IS ASSIGNED.
      CHECK NOT <lv_data2> IS INITIAL.
      CHECK <lv_data> EQ <lv_data2>.
      DELETE ct_bins INDEX lv_tabix2.
    ENDLOOP.

    ls_posicao-lgnum = uv_lgnum.
    ls_posicao-lgtyp = uv_lgtyp.
    ls_posicao-lgpla = <lv_data>.
    APPEND ls_posicao TO lt_posicoes.
  ENDLOOP.

  IF lt_posicoes IS INITIAL.
    CLEAR: ct_bins.
    EXIT.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title          = 'Posicoes'
      i_selection      = 'X'
      i_tabname        = 'posicoes'
      i_structure_name = 'ZWM027'
    IMPORTING
      es_selfield      = ls_selection
    TABLES
      t_outtab         = lt_posicoes
    EXCEPTIONS
      program_error    = 1
      OTHERS           = 2.

  CLEAR: ls_posicao.
  READ TABLE lt_posicoes INTO ls_posicao INDEX ls_selection-tabindex.
  IF sy-subrc <> 0.
    CLEAR: ct_bins.
    EXIT.
  ENDIF.

** Filtra Pares
**********************************************************************
  UNASSIGN <ls_bin2>.
  LOOP AT ct_bins ASSIGNING <ls_bin>.
    lv_tabix = sy-tabix.

    UNASSIGN <lv_data>.
    ASSIGN COMPONENT 'LGPLA' OF STRUCTURE <ls_bin> TO <lv_data>.
    CHECK <lv_data> IS ASSIGNED.
    CHECK NOT <lv_data> IS INITIAL.
    CHECK <lv_data> EQ ls_posicao-lgpla.

    CLEAR : lv_pulmao_aux.
    lv_bin1 = <lv_data>.
    lv_pulmao_aux = <lv_data>+4(3).

    lv_tabix = lv_tabix + 1.
    LOOP AT ct_bins ASSIGNING <ls_bin2> FROM lv_tabix.
      UNASSIGN <lv_data2>.
      ASSIGN COMPONENT 'LGPLA' OF STRUCTURE <ls_bin2> TO <lv_data2>.
      CHECK <lv_data2> IS ASSIGNED.
      CHECK NOT <lv_data2> IS INITIAL.
      CHECK <lv_data> <> <lv_data2>.

      SELECT SINGLE * FROM lagp
                      INTO ls_lagp
                      WHERE lgnum = uv_lgnum AND
                            lgtyp = uv_lgtyp AND
                            lgpla = <lv_data2> AND
                            lgber = lv_pulmao_aux AND
                            brand = ' '.
      CHECK sy-subrc EQ 0.
      lv_bin2 = <lv_data2>.
    ENDLOOP.
  ENDLOOP.

** Filtro Final
***********************************************************************
  LOOP AT ct_bins ASSIGNING <ls_bin>.
    lv_tabix = sy-tabix.

    UNASSIGN <lv_data>.
    ASSIGN COMPONENT 'LGPLA' OF STRUCTURE <ls_bin> TO <lv_data>.
    CHECK <lv_data> IS ASSIGNED.
    CHECK NOT <lv_data> IS INITIAL.

    CHECK <lv_data> <> lv_bin1 AND
          <lv_data> <> lv_bin2.

    DELETE ct_bins INDEX lv_tabix.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INCIDENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incidencia.

  DATA:  valor_aux        LIKE zwm001-valor.

** Para as saídas do DRIVE-IN ... não se pode invocar o ecrã
** de incidências caso já tenha lido o SSCC
  CLEAR : valor_aux.
  PERFORM get_parameter USING xuser-lgnum
                             'ENTRADA_ARMAZEM'
                             'ST_DRI'
                              valor_aux.

*      IF NOT su IS INITIAL AND bin_output_o <> valor_aux.
  CLEAR ecra_chamador.
  IF lrf_wkqu-devty = '8X40'.
    CALL SCREEN '0009'.
  ELSEIF lrf_wkqu-devty = '16X20'.
    CALL SCREEN '0010'.
  ENDIF.

*      ELSE.
*** Apenas para as saídas dos DRIVE-IN a su não pode estar preenchida
*        IF bin_output_o(3) = valor_aux.
*          CLEAR ecra_chamador.
*          IF lrf_wkqu-devty = '8X40'.
*            CALL SCREEN '0009'.
*          ELSEIF lrf_wkqu-devty = '16X20'.
*            CALL SCREEN '0010'.
*          ENDIF.
*        ENDIF.
*      ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirm.

  DATA aux_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.
  DATA bin LIKE ltap-nlpla.

  DATA: ls_ltak          TYPE ltak.
  DATA: ls_ltap          TYPE ltap.
  DATA: ls_zwm077        TYPE zwm077.
  DATA: lv_queue_dri_prm TYPE lrf_queue.
  DATA: lv_queue_tri_prm TYPE lrf_queue.
  DATA: lv_bwlvs_pick    TYPE bwlvs.

** Validar confirmação
**********************************************************************
  CLEAR: aux_ltap, bin, return_msg,
         zwm020, to3, to_remontada.

  REFRESH: aux_ltap, return_msg.

  CHECK NOT su IS INITIAL.

  IF bin_output_d(3) = 'PUL'.
    CHECK NOT posicao_pulmao2 IS INITIAL.
  ENDIF.

  CHECK NOT bin_input IS INITIAL AND bin_input = bin_output_d.

  " Validar Palete em porta com portico
  IF bin_output_d(3) = 'DCK'.

    PERFORM check_pal_porta_portico CHANGING sy-subrc.
    IF sy-subrc <> 0.
      CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2, ok_code_0001.
      MOVE 'BIN_INPUT' TO cursorfield.
      EXIT.
    ENDIF.

  ENDIF.

** Validar OT
  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum = xuser-lgnum
    AND   tanum = to_ret.

  IF lgtyp = 'MEN'.
    SELECT SINGLE *
      FROM ltap INTO ls_ltap
      WHERE lgnum = xuser-lgnum
      AND   nlenr = su
      AND   pquit = ''.

    IF sy-subrc = 0.
      SELECT SINGLE *
        FROM ltak INTO ls_ltak
        WHERE lgnum = xuser-lgnum
        AND   tanum = ls_ltap-tanum.
    ENDIF.
  ENDIF.

** Movimento de WM
** Palete de Picking saida do automático para expedição no Pulmão
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = xuser-lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_MAN_PICK'
    IMPORTING
      e_valor     = lv_bwlvs_pick
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

** Filas paletes remontadas para PRM
  PERFORM get_parameter
        USING xuser-lgnum
              'GESTAO_FILAS'
              'FILA_S_DRI_PAL_PRM'
              lv_queue_dri_prm.

  PERFORM get_parameter
        USING xuser-lgnum
              'GESTAO_FILAS'
              'FILA_S_TRI_PAL_PRM'
              lv_queue_tri_prm.

** Mensula
**********************************************************************
  CLEAR zwm014.
  SELECT SINGLE * FROM zwm014
    WHERE armazem   = xuser-lgnum AND
          su        = su AND
          estado    = 'X'.

  IF sy-subrc = 0.
*TO que passa por mensula, FAZ SO PICK DEPOIS DE PREENCHER O BIN
* E CHAMA OUTRA TAREFA FAZENDO O CLEAR DA VARIAVEL TO_RET
* (faz com que quando processa o PBO chame outra tarefa)

    " Palete de saída do automático torres novas (WCS)
    SELECT SINGLE *
       FROM zwm077 INTO ls_zwm077
       WHERE lgnum = xuser-lgnum
       AND   exidv = su
      AND    tanum = to_ret.

* Verificar se é uma PALETE REMONTADA ou é apenas INDIVIDUAL
    CLEAR zwm020.
    SELECT SINGLE * FROM zwm020
                    WHERE armazem = xuser-lgnum AND
                          ( p1 = su OR
                            p2 = su ).
    IF sy-subrc = 0.
** Palete Remontada

      IF ls_zwm077 IS INITIAL.
        CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
          EXPORTING
            armazem              = xuser-lgnum
            confirm_type         = 'P'
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = to_ret
          EXCEPTIONS
            confirm_type_error   = 1
            to_not_found         = 2
            to_already_confirmed = 3
            to_already_picked    = 4
            to_not_picked        = 5
            wrong_su             = 6
            missing_su           = 7
            error                = 8
            OTHERS               = 9.
        IF sy-subrc <> 0.
          READ TABLE return_msg INDEX 1.
          IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = return_msg-msgid
                message_lang   = sy-langu
                message_type   = return_msg-msgtyp
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1
                message_var2   = return_msg-msgv2
                message_var3   = return_msg-msgv3.

            CLEAR: bin_input, posicao_pulmao1,
                   posicao_pulmao2, ok_code_0001.
            MOVE 'BIN_INPUT' TO cursorfield.
            EXIT.
*                SET SCREEN '0000'.LEAVE SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.

*PROCESSAMENTO OK
*actualizar tabela ZWM011 com STATUS T pq é TO
*de putaway de mesa -> mensula
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = '0001'
          status             = 'T'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.

      IF sy-subrc = 0.
        DELETE FROM zwm013
        WHERE armazem   = xuser-lgnum AND
              ( sscc = zwm020-p1 OR
                sscc = zwm020-p2 ).
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.

        MOVE 'SU' TO cursorfield.
        CLEAR: to_ret, item_ret.
      ENDIF.

    ELSE.
** Palete INDIVIDUAL
** no caso de ser uma palete para o rep faz transfer
      DATA conf(1).
      CLEAR conf.
      IF bin_output_d(3) = 'REP'.
        conf = 'T'.
      ELSE.
        conf = 'P'.
      ENDIF.

      IF ls_zwm077 IS INITIAL.
        CALL FUNCTION 'ZWM_CONFIRM_TO'
          EXPORTING
            armazem              = xuser-lgnum
            confirm_type         = conf
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = to_ret
            to_item              = item_ret
          EXCEPTIONS
            confirm_type_error   = 1
            to_not_found         = 2
            to_already_confirmed = 3
            to_already_picked    = 4
            to_not_picked        = 5
            wrong_su             = 6
            missing_su           = 7
            error                = 8
            OTHERS               = 9.

        IF sy-subrc <> 0.
          READ TABLE return_msg INDEX 1.
          IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = return_msg-msgid
                message_lang   = sy-langu
                message_type   = return_msg-msgtyp
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1
                message_var2   = return_msg-msgv2
                message_var3   = return_msg-msgv3.

            CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
                   ok_code_0001.
            MOVE 'BIN_INPUT' TO cursorfield.
            EXIT.
*                SET SCREEN '0000'.LEAVE SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.

*PROCESSAMENTO OK
*actualizar tabela ZWM011 com STATUS T pq é TO
*de putaway de mesa -> mensula
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = item_ret
          status             = 'T'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.

      IF sy-subrc = 0.
        DELETE FROM zwm013
        WHERE armazem   = xuser-lgnum AND
              sscc = su.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.

        MOVE 'SU' TO cursorfield.
        CLEAR: to_ret,item_ret.
      ENDIF.
    ENDIF.           "Palete Remontada ou não?

** Confirma Palete no Copacking
**********************************************************************
  ELSEIF bin_output_d(3) = 'CPK'.

    PERFORM confirma_to_copacking USING lgpla lgtyp.

** Confirma Palete de mesa de saída do Automático (WCS)
**********************************************************************
  ELSEIF lgtyp = 'AUT' OR lgtyp = 'EAU' OR bin_output_d(3) = 'REJ'."ls_ltak-bwlvs = lv_bwlvs_pick .

    PERFORM confirma_to_sai_aut_wcs USING lgpla lgtyp.

** Confirma Palete Mesa de Entrada no Arm. Automático (WCS)
**********************************************************************
  ELSEIF bin_output_d(3) = 'EAU' OR  bin_output_d(3) = 'PUA'.

    PERFORM confirma_to_ent_aut_wcs USING lgpla lgtyp.

** Confirma Palete Remontada no PRM
**********************************************************************
  ELSEIF bin_output_d(3) = 'PRM' AND ls_ltak-queue = lv_queue_dri_prm OR
                                     ls_ltak-queue = lv_queue_tri_prm.

    PERFORM confirma_to_rem_ent_prm USING lgpla lgtyp.

** Outras Confirmações
**********************************************************************
  ELSE.
* TO QUE NAO PASSA POR MENSULA, FALTA SO FAZER O transfer
* O pick foi feito no chain



* Verificar se é uma palete remontada
    SELECT SINGLE * FROM zwm020
                    WHERE armazem = xuser-lgnum AND
                          ( p1 = su OR p2 = su ).
    IF sy-subrc = 0.
      to3 = 'X'.
      CLEAR ltap.
** Verificar se palete de cima vai para a zona PRM
      SELECT SINGLE *
          FROM ltap
              WHERE lgnum = xuser-lgnum AND
                    vlenr = zwm020-p2 AND
                    pquit <> 'X'.
      IF ltap-nltyp = 'PRM' OR ltap-nltyp = 'INC'.
        to_remontada = ltap-tanum.
      ELSE.
        CLEAR to_remontada.
      ENDIF.
    ELSE.
      CLEAR to3.
    ENDIF.

** Se for uma palete de saida proveniente da mensula ... confirma
** a TO totalmente

    IF ( lgtyp = 'MEN' OR lgtyp = ' ' )
         AND bin_output_d(3) <> 'REP'.

      PERFORM confirma_to_total USING lgpla lgtyp.

    ELSEIF lgtyp = 'CRD'.

      PERFORM confirma_to_crd USING lgpla lgtyp.

** Se for uma palete de saida proveniente do drive in ... confirma
** a TO totalmente
    ELSEIF ( lgtyp = 'DRI' OR lgtyp = 'BLK' OR lgtyp = 'BPK' ) AND bin_output_d(3) <> 'REP' AND xuser-lgnum = '100'.

      PERFORM confirma_to_total USING lgpla lgtyp.

    ELSEIF ( lgtyp = 'DRI' OR lgtyp = 'BLK' ) AND bin_output_d(3) <> 'PKB' AND xuser-lgnum = '150'.

      PERFORM confirma_to_total USING lgpla lgtyp.

    ELSEIF lgtyp = 'PRM'.

      CLEAR to3.
      IF bin_output_d(3) = 'DCK'.
        CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
          EXPORTING
            armazem              = xuser-lgnum
            confirm_type         = 'T'
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = to_ret
          EXCEPTIONS
            confirm_type_error   = 1
            to_not_found         = 2
            to_already_confirmed = 3
            to_already_picked    = 4
            to_not_picked        = 5
            wrong_su             = 6
            missing_su           = 7
            error                = 8
            OTHERS               = 9.

        IF sy-subrc <> 0.
          READ TABLE return_msg INDEX 1.
          IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = return_msg-msgid
                message_lang   = sy-langu
                message_type   = return_msg-msgtyp
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1
                message_var2   = return_msg-msgv2
                message_var3   = return_msg-msgv3.

            CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
                   ok_code_0001.
            MOVE 'BIN_INPUT' TO cursorfield.
            EXIT.
*                  SET SCREEN '0000'.LEAVE SCREEN.
          ENDIF.
        ELSE.
*PROCESSAMENTO OK
*actualizar tabela ZWM011 com STATUS T pq é TO
*de DE CONFIRMAÇÃO NO DESTINO
          CALL FUNCTION 'ZWM_MODIFY_ZWM011'
            EXPORTING
              armazem            = xuser-lgnum
              to_number          = to_ret
              to_item            = item_ret
              status             = 'T'
            EXCEPTIONS
              error_update_table = 1
              OTHERS             = 2.

          IF sy-subrc <> 0.
          ENDIF.

**              Actualiza a 13 e 28
          PERFORM actualiza_paletes_carro_prm.

        ENDIF.
      ELSE.

        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
          EXPORTING
            armazem            = xuser-lgnum
            to_number          = to_ret
            to_item            = item_ret
            status             = 'T'
          EXCEPTIONS
            error_update_table = 1
            OTHERS             = 2.

        IF sy-subrc <> 0.

        ENDIF.

        PERFORM actualiza_paletes_pulmao.
      ENDIF.
    ELSE.
      IF lgtyp = 'PKF'.
        IF bin_output_d(3) = 'DCK'.

          PERFORM confirm_ots_pal_picking.

          PERFORM actualiza_paletes_carro.
        ELSE.
          PERFORM actualiza_paletes_pulmao.
        ENDIF.
      ENDIF.

      IF lgtyp = 'REP'.
        CLEAR ltak.
        SELECT SINGLE *
            FROM ltak
                WHERE lgnum = xuser-lgnum AND
                      tanum = to_ret.

        IF ltak-bwlvs <> '973'.

          CLEAR: to_remontada, zwm020.
          SELECT SINGLE * FROM zwm020
              WHERE armazem = xuser-lgnum AND
                    ( p1 = su OR p2 = su ).

          IF sy-subrc = 0.
            bin = bin_output_d+4(10).
          ENDIF.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'T'
          bin                  = bin
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = to_ret
        EXCEPTIONS
          confirm_type_error   = 1
          to_not_found         = 2
          to_already_confirmed = 3
          to_already_picked    = 4
          to_not_picked        = 5
          wrong_su             = 6
          missing_su           = 7
          error                = 8
          OTHERS               = 9.

      IF sy-subrc <> 0.
        READ TABLE return_msg INDEX 1.
        IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = return_msg-msgid
              message_lang   = sy-langu
              message_type   = return_msg-msgtyp
              message_number = return_msg-msgnr
              message_var1   = return_msg-msgv1
              message_var2   = return_msg-msgv2
              message_var3   = return_msg-msgv3.

          CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
                 ok_code_0001.
          MOVE 'BIN_INPUT' TO cursorfield.
          EXIT.
        ENDIF.
      ELSE.
*PROCESSAMENTO OK
*actualizar tabela ZWM011 com STATUS T pq é TO
*de DE CONFIRMAÇÃO NO DESTINO
        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
          EXPORTING
            armazem            = xuser-lgnum
            to_number          = to_ret
            to_item            = item_ret
            status             = 'T'
          EXCEPTIONS
            error_update_table = 1
            OTHERS             = 2.

        IF sy-subrc <> 0.

        ELSE.
*Processamento OK
*apaga a TO da tabela zwm013
*se for remontada tem de apagar as duas entradas
          IF NOT zwm020-p1 IS INITIAL.
            DELETE FROM zwm013
              WHERE armazem   = xuser-lgnum AND
                    sscc = zwm020-p1 OR
                    sscc = zwm020-p2.

            IF sy-subrc = 0.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.

          ELSE.
            DELETE FROM zwm013
            WHERE armazem   = xuser-lgnum AND
                  sscc = su.
            IF sy-subrc = 0.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.
          ENDIF.

**  apartir daqui a palete deixa de ser remontada, logo tem de se apagar
**  da tabela zwm020
          IF NOT to_remontada IS INITIAL AND
             ( ltap-nltyp = 'PRM' OR ltap-nltyp = 'INC' ).

            DELETE FROM zwm020 WHERE armazem = xuser-lgnum AND
                                    ( p1 = su OR p2 = su ).
            IF sy-subrc = 0.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.
            CLEAR to_remontada.
          ENDIF.

          CLEAR to.
** Se for uma TO com destino no REP - temos de criar uma nova TO para a
** zona de picking associada à queue - QUEUEP
** A verificação é feita dentro do perform

*                CLEAR aux_to.
*                aux_to = to_ret.
*                CLEAR to_ret.
          CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

          PERFORM cria_to_picking.
**  Se for uma to com origem no REP e destino no PRB tem de ser criada
**  uma to com a quantidade que vem no campo adicional para a zona
**  Interina
          IF lgtyp = 'REP' AND bin_output_d(3) = 'PRB'.
            PERFORM cria_to_picking_variavel USING lgpla lgtyp.
          ENDIF.
*                to_ret = aux_to.
** Verifica se o Int fica positivo.
          break roffd.
          CLEAR ltap.
          SELECT SINGLE *
              FROM ltap
                  WHERE lgnum = xuser-lgnum AND
                        tanum = to_ret AND
                        tapos = item_ret.
          IF sy-subrc = 0.
            IF ltap-nltyp = 'INT'.
              CLEAR to.
              to = ltap-tanum.
              PERFORM acerto_int.
            ENDIF.
          ENDIF.

          MOVE 'SU' TO cursorfield.
          CLEAR: to_ret,item_ret.
        ENDIF.
      ENDIF.
    ENDIF.    " palete q vem da mensula
  ENDIF.

** No caso de ser uma palete remontada em que a palete de cima tem de ir
** para o PRM associa-se logo a OT pretendida

  IF NOT to_remontada IS INITIAL.
** Fazer Pick á TO

    CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
      EXPORTING
        armazem              = xuser-lgnum
        confirm_type         = 'P'
      TABLES
        return_msg           = return_msg
      CHANGING
        to                   = to_remontada
      EXCEPTIONS
        confirm_type_error   = 1
        to_not_found         = 2
        to_already_confirmed = 3
        to_already_picked    = 4
        to_not_picked        = 5
        wrong_su             = 6
        missing_su           = 7
        error                = 8
        OTHERS               = 9.

    CLEAR: ltak.
    SELECT SINGLE *
        FROM ltak
            WHERE lgnum = xuser-lgnum AND
                  tanum = to_remontada.

    CLEAR: tab_zwm011.

    MOVE ltak-lgnum TO tab_zwm011-armazem.
    MOVE sy-uname TO tab_zwm011-user_name.
    MOVE to_remontada TO tab_zwm011-to_number.
** INI - Achar o item onde se encontra a SU
    CLEAR zwm020.
*          SELECT SINGLE *
*              FROM zwm020
*                  WHERE armazem = ltak-lgnum AND
*                        p1 = su.

    CLEAR ltap.
    DO 30 TIMES.
      SELECT SINGLE *
          FROM ltap
              WHERE lgnum = ltak-lgnum AND
                    tanum = to_remontada AND
                    pquit <> 'X'.
*                 AND vlenr = zwm020-p2.
      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.
    tab_zwm011-to_item = ltap-tapos.

*          MOVE '0001' TO tab_zwm011-to_item.
** FIM - Achar o item onde se encontra a SU
    MOVE 'P' TO tab_zwm011-status.
    MOVE ltak-queue TO tab_zwm011-queue.
    MOVE equipamento_ TO tab_zwm011-equipamento.
    MODIFY zwm011 FROM tab_zwm011.
    COMMIT WORK.

    DELETE FROM zwm011 WHERE armazem = ltak-lgnum AND
                   equipamento = equipamento_ AND
                   user_name = sy-uname AND
                   status = 'T'.
    COMMIT WORK.

    CLEAR to_remontada.
    CALL FUNCTION 'ZWM_CALL_TASK_RET'
      EXPORTING
        armazem     = xuser-lgnum
        ecran       = ecran
        tab_zwm011  = tab_zwm011
        tipo_queue  = tipo
        equipamento = equipamento_.

  ENDIF.

  DATA : valor_aux        LIKE zwm001-valor,
         equipamento_con  TYPE zwm001-valor,
         equipamento_ret  TYPE zwm001-valor,
         equipamento_retp TYPE zwm001-valor.

  CLEAR : tab_zwm011, return_msg,equipamento_con,equipamento_ret.
  REFRESH : return_msg.

** Equipamento Convencional
  CLEAR valor_aux.
  PERFORM get_parameter USING xuser-lgnum
                             'GERAL'
                             'EQUIPAMENTO_CON'
                              valor_aux.
  WRITE valor_aux TO equipamento_con LEFT-JUSTIFIED.

  CLEAR valor_aux.
  PERFORM get_parameter USING xuser-lgnum
                              'GERAL'
                              'EQUIPAMENTO_RET'
                               valor_aux.
  WRITE valor_aux TO equipamento_ret LEFT-JUSTIFIED.

  CLEAR valor_aux.
  PERFORM get_parameter USING xuser-lgnum
        'GERAL'
        'EQUIPAMENTO_RETP'
        valor_aux.
  WRITE valor_aux TO equipamento_retp LEFT-JUSTIFIED.

  IF equipamento_ = equipamento_con.

    SELECT * FROM zwm010 INTO TABLE tab_zwm010
    WHERE armazem = xuser-lgnum AND
          equipamento = equipamento_.

    CALL FUNCTION 'ZWM_GET_TO_RET'
      EXPORTING
        armazem           = xuser-lgnum
        tamanho           = lrf_wkqu-devty(5)
        add_new_to        = ' '
      IMPORTING
        nova_to           = tab_zwm011
        tipo_queue        = tipo
      TABLES
        l_zwm010          = tab_zwm010
        return_msg        = return_msg
      EXCEPTIONS
        no_equipment      = 1
        no_work_available = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
** ERRO
      READ TABLE return_msg INDEX 1.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1.
        CLEAR equipamento_.
        f3_activo = 'X'.
        IF lrf_wkqu-devty(5) = '16X20'.
          SET SCREEN '0001'.LEAVE SCREEN.
        ELSE.
          SET SCREEN '0002'.LEAVE SCREEN.
        ENDIF.
      ENDIF.

    ELSE.
** Atribuição de nova tarefa
      CALL FUNCTION 'ZWM_CALL_TASK_RET'
        EXPORTING
          armazem     = xuser-lgnum
          ecran       = ecran
          tab_zwm011  = tab_zwm011
          tipo_queue  = tipo
          equipamento = equipamento_.

    ENDIF.

  ELSEIF equipamento_ = equipamento_ret OR
         equipamento_ = equipamento_retp.

    SELECT * FROM zwm010 INTO TABLE tab_zwm010
    WHERE armazem = xuser-lgnum AND
          equipamento = equipamento_.

    CALL FUNCTION 'ZWM_GET_TO_PIC'
      EXPORTING
        armazem           = xuser-lgnum
        tamanho           = lrf_wkqu-devty(5)
        add_new_to        = ' '
      IMPORTING
        nova_to           = tab_zwm011
        tipo_queue        = tipo
      TABLES
        l_zwm010          = tab_zwm010
        return_msg        = return_msg
      EXCEPTIONS
        no_equipment      = 1
        no_work_available = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
** ERRO
      READ TABLE return_msg INDEX 1.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1.
        CLEAR equipamento_.
        f3_activo = 'X'.
        IF lrf_wkqu-devty(5) = '16X20'.
          SET SCREEN '0001'.LEAVE SCREEN.
        ELSE.
          SET SCREEN '0002'.LEAVE SCREEN.
        ENDIF.
      ENDIF.
    ELSE.
** Atribuição de nova tarefa
      CALL FUNCTION 'ZWM_CALL_TASK_RET'
        EXPORTING
          armazem     = xuser-lgnum
          ecran       = ecran
          tab_zwm011  = tab_zwm011
          tipo_queue  = tipo
          equipamento = equipamento_
          onlyone     = gv_only_one. " Alteração - ROFF SDF Carlos Fernandes - 09.03.2016

*--------------------------------------------------------------------*
* Início de Alteração - ROFF SDF Carlos Fernandes - 09.03.2016
*--------------------------------------------------------------------*
      IF gv_only_one EQ abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.
*--------------------------------------------------------------------*
* Fim de Alteração - ROFF SDF Carlos Fernandes - 09.03.2016
*--------------------------------------------------------------------*

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIRMA_TO_COPACKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LGPLA  text
*      -->P_LGTYP  text
*----------------------------------------------------------------------*
FORM confirma_to_copacking  USING  p_lgpla p_lgtyp.

  DATA: lv_subst           TYPE rl03t-subst.
  DATA: lv_goodsmvt_code   LIKE bapi2017_gm_code.
  DATA: lv_werks           TYPE werks_d.
  DATA: lv_lgort_o         TYPE lgort_d.
  DATA: lv_lgort_d         TYPE lgort_d.
  DATA: lv_mov_mm          TYPE bwart.
  DATA: lv_gm_code         TYPE gm_code.
  DATA: lv_matdocument     TYPE bapi2017_gm_head_ret-mat_doc.
  DATA: lv_matdocyear      TYPE bapi2017_gm_head_ret-doc_year.
  DATA: lv_tanum           TYPE tanum.
  DATA: ls_ltak            TYPE ltak.
  DATA: ls_ltap            TYPE ltap.
  DATA: ls_zwm020          TYPE zwm020.
  DATA: ls_return          TYPE bdcmsgcoll.
  DATA: ls_msg             TYPE bdcmsgcoll.
  DATA: ls_zwm011          TYPE zwm011.
  DATA: ls_goodsmvt_header LIKE bapi2017_gm_head_01.

  DATA: lt_goodsmvt_item   TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE.
  DATA: lt_ltap            TYPE TABLE OF ltap                    WITH HEADER LINE.
  DATA: lt_ltap_conf       TYPE TABLE OF ltap_conf               WITH HEADER LINE.
  DATA: lt_return          TYPE TABLE OF bapiret2                WITH HEADER LINE.

** Validar OT
**********************************************************************
  lv_tanum = to_ret.

** Origem Mensula
  IF p_lgtyp = 'MEN'.

    SELECT SINGLE *
      FROM ltap INTO ls_ltap
      WHERE lgnum = xuser-lgnum
      AND   vlenr = su
      AND   pquit = ''.

    IF sy-subrc = 0.
      to_ret   = ls_ltap-tanum.
      item_ret = ls_ltap-tapos.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum = xuser-lgnum
    AND   tanum = to_ret.

  IF sy-subrc = 0.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = ls_ltak-lgnum
      AND   tanum = ls_ltak-tanum.
  ENDIF.

  DELETE lt_ltap WHERE vorga = 'ST' OR
                       vorga = 'SL'.

  DELETE lt_ltap WHERE pquit = 'X'.

** Confirmar OT
**********************************************************************
  READ TABLE lt_ltap INDEX 1.

  LOOP AT lt_ltap.
    CLEAR lt_ltap_conf.
    lt_ltap_conf-tanum = lt_ltap-tanum.
    lt_ltap_conf-tapos = lt_ltap-tapos.
    lt_ltap_conf-altme = lt_ltap-meins.
    lt_ltap_conf-nista = lt_ltap-vsolm.
    APPEND lt_ltap_conf.
  ENDLOOP.

  " Pisco sistema externo
  IF lt_ltap-vlpla = 'AUT'.
    lv_subst = 'X'.
  ENDIF.

  " Transfer da OT
  CALL FUNCTION 'L_TO_CONFIRM'
    EXPORTING
      i_lgnum       = xuser-lgnum
      i_tanum       = lt_ltap-tanum
      i_quknz       = '2'
      i_subst       = lv_subst
    TABLES
      t_ltap_conf   = lt_ltap_conf
    EXCEPTIONS
      error_message = 99.

  IF sy-subrc <> 0.
    ls_return-msgid  = sy-msgid.
    ls_return-msgtyp = sy-msgty.
    ls_return-msgnr  = sy-msgno.
    ls_return-msgv1  = sy-msgv1.
    ls_return-msgv2  = sy-msgv2.
    ls_return-msgv3  = sy-msgv3.
    ls_return-msgv4  = sy-msgv4.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = ls_return-msgid
        message_lang   = sy-langu
        message_type   = ls_return-msgtyp
        message_number = ls_return-msgnr
        message_var1   = ls_return-msgv1
        message_var2   = ls_return-msgv2
        message_var3   = ls_return-msgv3
        message_var4   = ls_return-msgv4.

    CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
           cursorfield, ok_code_0001.

    MOVE 'BIN_INPUT' TO cursorfield.
    EXIT.
  ENDIF.

** Transfer OT
  CALL FUNCTION 'ZWM_MODIFY_ZWM011'
    EXPORTING
      armazem            = xuser-lgnum
      to_number          = to_ret
      to_item            = item_ret
      status             = 'T'
    EXCEPTIONS
      error_update_table = 1
      OTHERS             = 2.

  " Confirmar OT da Mensula
  IF p_lgtyp = 'MEN'.

    CLEAR : return_msg.
    REFRESH : return_msg.

    CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
      EXPORTING
        armazem              = xuser-lgnum
        confirm_type         = 'T'
      TABLES
        return_msg           = return_msg
      CHANGING
        to                   = lv_tanum
      EXCEPTIONS
        confirm_type_error   = 1
        to_not_found         = 2
        to_already_confirmed = 3
        to_already_picked    = 4
        to_not_picked        = 5
        wrong_su             = 6
        missing_su           = 7
        error                = 8
        OTHERS               = 9.
  ENDIF.


** Tranferencia de Stock de CD -> RETR
**********************************************************************
  PERFORM get_parameter
         USING xuser-lgnum
               'GERAL'
               'PLANT'
               lv_werks.

  PERFORM get_parameter
        USING xuser-lgnum
              'GERAL'
              'LGORT'
              lv_lgort_o.

  PERFORM get_parameter
        USING xuser-lgnum
              'GERAL'
              'LGORTRETR'
              lv_lgort_d.

  PERFORM get_parameter
        USING xuser-lgnum
              'TRANSFER_DEPOSITO'
              'MOV_MM'
              lv_mov_mm.

  PERFORM get_parameter
        USING xuser-lgnum
              'TRANSFER_DEPOSITO'
              'GM_CODE'
              lv_gm_code.

  CLEAR: lv_goodsmvt_code, ls_goodsmvt_header.
  CLEAR:   lt_goodsmvt_item.
  REFRESH: lt_goodsmvt_item.

** Dados Cabeçalho
  lv_goodsmvt_code              = lv_gm_code.
  ls_goodsmvt_header-pstng_date = sy-datum.
*  ls_goodsmvt_header-header_txt = gv_bktxt.

** Dados Items
  LOOP AT lt_ltap.

    CLEAR lt_goodsmvt_item.
    lt_goodsmvt_item-move_type     = lv_mov_mm.
    lt_goodsmvt_item-plant         = lv_werks.
    lt_goodsmvt_item-stge_loc      = lv_lgort_o.
    lt_goodsmvt_item-move_plant    = lv_werks.
    lt_goodsmvt_item-move_stloc    = lv_lgort_d.
    lt_goodsmvt_item-material      = lt_ltap-matnr.
    lt_goodsmvt_item-batch         = lt_ltap-charg.
    lt_goodsmvt_item-entry_qnt     = lt_ltap-vsolm.
    lt_goodsmvt_item-entry_uom     = lt_ltap-meins.
    lt_goodsmvt_item-entry_uom_iso = lt_goodsmvt_item-entry_uom.
    lt_goodsmvt_item-spec_mvmt     = 'C'.
    APPEND lt_goodsmvt_item.
  ENDLOOP.

**  Movimento de MM
  CLEAR: lv_matdocument, lv_matdocyear.

  CLEAR: lt_return.
  REFRESH lt_return.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_goodsmvt_header
      goodsmvt_code    = lv_goodsmvt_code
    IMPORTING
      materialdocument = lv_matdocument
      matdocumentyear  = lv_matdocyear
    TABLES
      goodsmvt_item    = lt_goodsmvt_item
      return           = lt_return.

  READ TABLE lt_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CLEAR ls_msg.
    ls_msg-msgid = lt_return-id.
    ls_msg-msgnr = lt_return-number.
    ls_msg-msgv1 = lt_return-message_v1.
    ls_msg-msgv2 = lt_return-message_v2.
    ls_msg-msgv3 = lt_return-message_v3.
    ls_msg-msgv4 = lt_return-message_v4.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = ls_msg-msgid
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = ls_msg-msgnr
        message_var1   = ls_msg-msgv1
        message_var2   = ls_msg-msgv2
        message_var3   = ls_msg-msgv3
        message_var4   = ls_msg-msgv4.

    REFRESH: lt_return.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = lt_return.
    EXIT.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_OT_BPK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_ot_bpk .

  DATA: lv_tanum      TYPE tanum.
  DATA: lv_lznum      TYPE ltak-lznum.
  DATA: lt_ltap       TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_ltap_creat TYPE TABLE OF ltap_creat WITH HEADER LINE.
  DATA: lt_zwm026     TYPE TABLE OF zwm026     WITH HEADER LINE.

** Palete Picking
  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = xuser-lgnum
    AND   tanum = to_ret.

  IF lt_ltap[] IS NOT INITIAL.
    SELECT *
      FROM zwm026 INTO TABLE lt_zwm026
      FOR ALL ENTRIES IN lt_ltap
      WHERE armazem = lt_ltap-lgnum
      AND   sscc    = lt_ltap-nlenr.
  ENDIF.

  " Items
  LOOP AT lt_ltap.
    CLEAR lt_ltap_creat.
    lt_ltap_creat-werks = lt_ltap-werks.
    lt_ltap_creat-lgort = lt_ltap-lgort.
    lt_ltap_creat-matnr = lt_ltap-matnr.
    lt_ltap_creat-charg = lt_ltap-charg.
    lt_ltap_creat-anfme = lt_ltap-nsola.
    lt_ltap_creat-altme = lt_ltap-altme.
    lt_ltap_creat-vltyp = lt_ltap-nltyp.
    lt_ltap_creat-vlpla = lt_ltap-nlpla.
    lt_ltap_creat-vlenr = lt_ltap-nlenr.
    lt_ltap_creat-nltyp = '932'.
    lt_ltap_creat-nlpla = 'ORIGEM'.
    lt_ltap_creat-letyp = lt_ltap-letyp.
    APPEND lt_ltap_creat.
  ENDLOOP.

  lv_lznum = lt_ltap-nlenr.

  READ TABLE lt_zwm026 INDEX 1.

  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
    EXPORTING
      i_lgnum       = xuser-lgnum
      i_bwlvs       = '965' "lv_bwlvs
      i_lznum       = lv_lznum
      i_refnr       = lt_zwm026-grupo
    IMPORTING
      e_tanum       = lv_tanum
    TABLES
      t_ltap_creat  = lt_ltap_creat
    EXCEPTIONS
      error_message = 99.

  IF lv_tanum IS INITIAL.
    return_msg-msgid = sy-msgid.
    return_msg-msgnr = sy-msgno.
    return_msg-msgv1 = sy-msgv1.
    return_msg-msgv2 = sy-msgv2.
    return_msg-msgv3 = sy-msgv3.
    return_msg-msgv4 = sy-msgv4.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = return_msg-msgid
        message_lang   = sy-langu
        message_type   = return_msg-msgtyp
        message_number = return_msg-msgnr
        message_var1   = return_msg-msgv1
        message_var2   = return_msg-msgv2
        message_var3   = return_msg-msgv3
        message_var4   = return_msg-msgv4.

  ELSE.

    " Atualizar Tabela de picking de Palete Tranferida para BPK
    LOOP AT lt_zwm026.
      UPDATE zwm026 SET trf_bpk = 'X'
      WHERE armazem       = lt_zwm026-armazem
      AND   n_pal_picking = lt_zwm026-n_pal_picking
      AND   i_pal_picking = lt_zwm026-i_pal_picking
      AND   grupo         = lt_zwm026-grupo
      AND   remessa       = lt_zwm026-remessa
      AND   posnr         = lt_zwm026-posnr
      AND   sub_item      = lt_zwm026-sub_item.
    ENDLOOP.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_POSICAO_REJEICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_posicao_rejeicao.

  DATA: ls_lein   TYPE lein.
  DATA: ls_zwm013 TYPE zwm013.
  DATA: ls_zwm078 TYPE zwm078.

** Palete Copacking
  SELECT SINGLE *
    FROM lein INTO ls_lein
    WHERE lenum = su.

  IF sy-subrc <> 0.
    SELECT SINGLE *
        FROM zwm013 INTO ls_zwm013
        WHERE armazem = xuser-lgnum
        AND   sscc    = su.

    IF sy-subrc = 0.
      UPDATE zwm013 SET pos_rej = bin_output_d+4(10)
      WHERE armazem = xuser-lgnum
      AND   sscc    = su.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDIF.

** Palete Expedição
  SELECT SINGLE *
    FROM zwm078 INTO ls_zwm078
    WHERE lgnum = xuser-lgnum
    AND   tanum = to_ret.

  IF sy-subrc = 0.
    UPDATE zwm078 SET rlpla = bin_output_d+4(10)
                WHERE lgnum = xuser-lgnum
                AND   tanum = to_ret.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_PAL_PORTA_PORTICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_pal_porta_portico CHANGING p_subrc.

  DATA: lt_return_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

  CLEAR p_subrc.

  CALL FUNCTION 'ZWM_CHECK_PAL_PORTICO'
    EXPORTING
      i_lgnum  = xuser-lgnum
      i_porta  = porta_destino
      i_sscc   = su
    TABLES
      t_return = lt_return_msg
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF sy-subrc <> 0.
    READ TABLE lt_return_msg INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = lt_return_msg-msgid
          message_lang   = sy-langu
          message_type   = lt_return_msg-msgtyp
          message_number = lt_return_msg-msgnr
          message_var1   = lt_return_msg-msgv1
          message_var2   = lt_return_msg-msgv2
          message_var3   = lt_return_msg-msgv3.
    ENDIF.

    p_subrc = 4.
  ELSE.
    p_subrc = 0.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_OTS_PAL_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirm_ots_pal_picking.

  DATA lt_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.
  DATA flag.

  CLEAR lt_zwm026.
  REFRESH lt_zwm026.

  SELECT * INTO TABLE lt_zwm026
      FROM zwm026
          WHERE armazem = xuser-lgnum
            AND sscc = su.

** Confirmar os itens picking
  LOOP AT lt_zwm026.
    CLEAR flag.
    WHILE flag IS INITIAL.
      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'T'
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = lt_zwm026-to_number
        EXCEPTIONS
          confirm_type_error   = 1
          to_not_found         = 2
          to_already_confirmed = 3
          to_already_picked    = 4
          to_not_picked        = 5
          wrong_su             = 6
          missing_su           = 7
          error                = 8
          OTHERS               = 9.

      IF sy-subrc <> 0.
        READ TABLE return_msg INDEX 1.
        IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = return_msg-msgid
              message_lang   = sy-langu
              message_type   = return_msg-msgtyp
              message_number = return_msg-msgnr
              message_var1   = return_msg-msgv1
              message_var2   = return_msg-msgv2
              message_var3   = return_msg-msgv3.

          CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
                 ok_code_0001.
          MOVE 'BIN_INPUT' TO cursorfield.
          EXIT.
        ENDIF.
      ELSE.
        CLEAR ltap.
        SELECT SINGLE * FROM ltap
              WHERE lgnum = xuser-lgnum AND
                    tanum = lt_zwm026-to_number AND
                    tapos = '0001' AND
                    pquit = 'X'.
        IF sy-subrc = 0.
          flag = 'X'.
        ELSE.
          CLEAR flag.
        ENDIF.
      ENDIF.
    ENDWHILE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_WCS_CHE_TOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_wcs_che_tos USING armazem.

  DATA: lt_zwm077_che TYPE TABLE OF zwm077 WITH HEADER LINE,
        lt_zwm011     TYPE TABLE OF zwm011 WITH HEADER LINE,
        lt_zwm001_che TYPE TABLE OF zwm001 WITH HEADER LINE,
        lt_lagp_che   TYPE TABLE OF lagp   WITH HEADER LINE.

  DATA: ls_ltak TYPE ltak.

  DATA: lv_index     TYPE sy-tabix,
        lv_count_che TYPE i,
        lv_tot_che   TYPE i,
        lv_dummy1    TYPE char10,
        lv_dummy2    TYPE char10,
        lv_val       TYPE numc3,
        lv_duration  TYPE sytabix,
        lv_cap_max   TYPE numc3.

** Posições de saida de Elevadores
  SELECT *
    FROM lagp INTO TABLE lt_lagp_che
    WHERE lgnum = armazem
    AND   lgtyp = 'CHE'
    AND   lgpla LIKE '%SAI'.

  CHECK sy-subrc = 0.

** Parametros
  SELECT *
    FROM zwm001 INTO TABLE lt_zwm001_che
    WHERE armazem  = armazem
    AND   processo = 'CHE_CHECK'.

  SORT lt_zwm001_che BY parametro.

** Paletes nas mesas de saida
  SELECT *
    FROM zwm077 INTO TABLE lt_zwm077_che
    WHERE lgnum = armazem
    AND   pvqui = ''.

** Validar OTs CHE
  REFRESH: l_ltap_che, l_ltap_che_p.

  LOOP AT l_ltap WHERE vltyp = 'CHE'.
    lv_index = sy-tabix.

    READ TABLE lt_lagp_che WITH KEY lgpla = l_ltap-vlpla.
    CHECK sy-subrc = 0.

    APPEND l_ltap TO l_ltap_che.

    DELETE l_ltap INDEX lv_index.
  ENDLOOP.

  l_ltap_che_p[] = l_ltap_che[].

  SORT l_ltap_che_p BY vlpla.
  DELETE ADJACENT DUPLICATES FROM l_ltap_che_p COMPARING vlpla.

  SELECT *
    FROM zwm011 INTO TABLE lt_zwm011
    WHERE armazem  = armazem.

  IF lt_zwm011[] IS NOT INITIAL.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltap_che BYPASSING BUFFER
      FROM ltap
      FOR ALL ENTRIES IN lt_zwm011
      WHERE lgnum = lt_zwm011-armazem
      AND   tanum = lt_zwm011-to_number
      AND   tapos = lt_zwm011-to_item.
  ENDIF.

  SORT l_ltap_che BY tanum tapos.

  LOOP AT l_ltap_che_p.

    CLEAR: lv_count_che, lv_tot_che.

    LOOP AT l_ltap_che WHERE vlpla = l_ltap_che_p-vlpla.
      lv_tot_che = lv_tot_che + 1.
    ENDLOOP.

    LOOP AT lt_zwm077_che WHERE lgpla = l_ltap_che_p-vlpla.
      lv_count_che = lv_count_che + 1.
    ENDLOOP.

    CHECK lv_count_che > 0.

    LOOP AT l_ltap_che WHERE vlpla = l_ltap_che_p-vlpla.

      lv_index = sy-tabix.

      READ TABLE lt_zwm011 WITH KEY armazem   = l_ltap_che-lgnum
                                    to_number = l_ltap_che-tanum
                                    to_item   = l_ltap_che-tapos.
      CHECK sy-subrc = 0.

      lv_count_che = lv_count_che - 1.

      DELETE l_ltap_che INDEX lv_index.
    ENDLOOP.

    CHECK lv_count_che > 0.

    " Numero de chamadas possiveis por mesa
    DO lv_count_che TIMES.

      LOOP AT l_ltap_che WHERE vlpla = l_ltap_che_p-vlpla.

        " Validar prioridade da OT
        READ TABLE lt_lagp_che WITH KEY lgpla = l_ltap-vlpla.
        IF sy-subrc = 0 AND lt_lagp_che-lptyp CO '0123456789'.
          lv_cap_max = lt_lagp_che-lptyp.

          lt_lagp_che-maxqu = lv_tot_che / lv_cap_max * 100.

          LOOP AT lt_zwm001_che.

            SPLIT lt_zwm001_che-parametro AT '%' INTO lv_dummy1 lv_dummy2.
            CHECK sy-subrc = 0.

            CHECK lv_dummy1 CO '0123456789'.

            lv_val = lv_dummy1.

            IF lt_lagp_che-maxqu >= lv_val AND lt_zwm001_che-valor IS NOT INITIAL.
              l_ltap_che-tapri = lt_zwm001_che-valor.
            ENDIF.
          ENDLOOP.
        ENDIF.

        " Tempo de espera máximo para recolher palete
        SELECT SINGLE *
          FROM ltak INTO ls_ltak
          WHERE lgnum = l_ltap_che-lgnum
          AND   tanum = l_ltap_che-tanum.

        IF sy-subrc = 0.

          CALL FUNCTION 'SWI_DURATION_DETERMINE'
            EXPORTING
              start_date = ls_ltak-bdatu
              end_date   = sy-datum
              start_time = ls_ltak-bzeit
              end_time   = sy-uzeit
            IMPORTING
              duration   = lv_duration.

          lv_duration = lv_duration / 60.

          READ TABLE lt_zwm001_che WITH KEY parametro = 'MESA_MAX_ANTIQ'.
          IF sy-subrc = 0.
            lv_val = lt_zwm001_che-valor.

            IF lv_duration > lv_val.
              l_ltap_che-tapri = lt_zwm001_che-valor.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND l_ltap_che TO l_ltap.
      ENDLOOP.
    ENDDO.
  ENDLOOP.


ENDFORM.
