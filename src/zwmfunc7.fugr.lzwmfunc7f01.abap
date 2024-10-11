*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC7F01 .
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
                  CHANGING p_nova_to STRUCTURE zwm011
                           p_tipo_queue.


*  DATA : numero_corredores_tri LIKE zwm001-valor,
*         numero_profundidade_tri LIKE zwm001-valor,
*         numero_nivel_tri LIKE zwm001-valor,
*         num_max_st_types LIKE zwm001-valor,
*         peso_st_type(2),
*         prioridade_to LIKE zwm014-prioridade,
*         peso_corredor TYPE i VALUE '100',
*         queue_saida_tri LIKE zwm001-valor.

  DATA : numero_corredores_tri   TYPE i,
         numero_profundidade_tri TYPE i,
         numero_nivel_tri        TYPE i,
         num_max_st_types        TYPE i,
         peso_st_type            TYPE i,
         prioridade_to           LIKE zwm014-prioridade,
         peso_corredor           TYPE i VALUE '100',
         queue_saida_tri         LIKE zwm001-valor.

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
    IF l_ltap-queue = p_tri_out_queue OR
       l_ltap-queue = p_queue_pick_tri.
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
*            ( prioridade_to * prioridade_to ).
      ENDIF.
** Entradas para trilateral
    ELSEIF l_ltap-queue = p_production_queue.

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
*              ( prioridade_to * prioridade_to ).
              ( prioridade_to * 500 ).
      ENDIF.
    ENDIF.

*    UNPACK l_ltap-prioridade_to TO l_ltap-prioridade_to.
    MODIFY l_ltap INDEX sy-tabix.

  ENDLOOP.

  SORT l_ltap BY prioridade_queue DESCENDING
                 prioridade_to DESCENDING
                 tanum ASCENDING
                 tapos ASCENDING.

  LOOP AT l_ltap.
    MOVE-CORRESPONDING l_ltap TO ltap_xls.
    APPEND ltap_xls.
    CLEAR ltap_xls.
  ENDLOOP.

  LOOP AT l_ltap.
    IF l_ltap-queue = p_tri_out_queue OR
       l_ltap-queue = p_queue_pick_tri.
      CLEAR corredor.


      CONCATENATE l_ltap-vlpla(3) '%' INTO corredor.

      DATA: t_zwm014  LIKE zwm014 OCCURS 0 WITH HEADER LINE,
            wa_zwm014 LIKE zwm014.

      CLEAR: t_zwm014, wa_zwm014.
      REFRESH t_zwm014.

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

        READ TABLE t_zwm014 WITH KEY mensula = l_lagp-lgpla.

*        READ TABLE t_zwm014 INDEX 1.

        MOVE-CORRESPONDING t_zwm014 TO wa_zwm014.
        wa_zwm014-su = l_ltap-vlenr.
        wa_zwm014-su_transito = l_ltap-vlenr.
        wa_zwm014-estado = 'X'.
        wa_zwm014-bin = l_ltap-vlpla.
        MODIFY zwm014 FROM wa_zwm014.
        COMMIT WORK.

** verificar se neste corredor se está a ocupar
** a terceira mensula do corredor ...se sim colocar
** nas TOs que pertencem ao corredor prioridade máxima

        SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                        WHERE armazem = l_ltap-lgnum AND
                              mensula LIKE corredor AND
                              estado = 'X'.

        IF n_mensulas_ocupadas >= 3.
          UPDATE zwm014 SET prioridade = 9
                        WHERE armazem = l_ltap-lgnum AND
                              mensula LIKE corredor AND
                              estado = 'X'.
          COMMIT WORK.

        ENDIF.

        MOVE sy-uname TO p_nova_to-user_name.
        MOVE l_ltap-tanum TO p_nova_to-to_number.
        MOVE l_ltap-tapos TO p_nova_to-to_item.
        MOVE p_armazem TO p_nova_to-armazem.
        MOVE 'C' TO p_nova_to-status.
        MOVE l_ltap-queue TO p_nova_to-queue.
        MOVE p_equipamento TO p_nova_to-equipamento.
        IF l_ltap-queue = queue_saida_tri OR
           l_ltap-queue = p_queue_pick_tri.
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

** RL -> INS 27.04.2005
** Prioridades
    l_index = sy-tabix.
** RL <- INS 27.04.2005

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


** RL -> MOD 27.04.2005
** Prioridades
    MODIFY l_ltap INDEX sy-tabix.

*    CLEAR: zwm028.
*    SELECT SINGLE prioridade FROM zwm028
*    INTO l_ltap-prioridade
*    WHERE lgnum   EQ l_ltap-lgnum
*      AND refnr   EQ l_ltap-refnr
*      AND remessa EQ space.
*
*    MODIFY l_ltap INDEX l_index.
** RL <- MOD 27.04.2005

  ENDLOOP.

** RL -> MOD 27.04.2005
  SORT l_ltap BY prioridade_queue DESCENDING
                 prioridade_to DESCENDING
                 tanum ASCENDING.

*  SORT l_ltap BY prioridade DESCENDING
*                 prioridade_queue DESCENDING
*                 prioridade_to DESCENDING
*                 tanum ASCENDING.
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
         queue_saida_dri     LIKE zwm001-valor,
         queue_saida_tri_men LIKE zwm001-valor,
         prioridade_maxima   TYPE i.

*  BREAK-POINT.
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

** Avaliação das TO dos Contra-Pesados

** Verificação do sitio onde o contrapesado deixou a última tarefa -
** Posição e Tipo de Depósito

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

** Ja tenho as zonas mais próximas ... atribuir a prioridade conforme as
** origens das TO q vamos avaliar
      LOOP AT l_ltap.

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

** Verificar se a TO  q esta a ser avaliada vem de uma mensula ... se
** sim verificar se a mensula está em stress
        IF l_ltap-queue = queue_saida_tri_men.
          CLEAR ltak.
          SELECT SINGLE lznum FROM ltak INTO ltak-lznum
                              WHERE lgnum = l_ltap-lgnum AND
                                    tanum = l_ltap-tanum.
          IF sy-subrc = 0.
            PERFORM verifica_mensulas USING prioridade_maxima.
            IF NOT prioridade_maxima IS INITIAL.
              l_ltap-prioridade_to = l_ltap-prioridade_to +
                                     prioridade_maxima.
            ENDIF.
          ENDIF.
        ENDIF.

** RL -> INS 27.04.2005
** Prioridades
*        CLEAR: zwm028.
*        SELECT SINGLE prioridade FROM zwm028
*        INTO l_ltap-prioridade
*        WHERE lgnum   EQ l_ltap-lgnum
*          AND refnr   EQ l_ltap-refnr
*          AND remessa EQ space.
** RL <- INS 27.04.2005

*        UNPACK l_ltap-prioridade_to TO l_ltap-prioridade_to.
        MODIFY l_ltap INDEX sy-tabix.

      ENDLOOP.

    ENDIF.

  ENDIF.

** RL -> MOD 27.04.2005
** Prioridades
  SORT l_ltap BY prioridade_queue DESCENDING
                 prioridade_to DESCENDING
                 pos_pulmao ASCENDING
                 vlpla ASCENDING
                 tanum ASCENDING
                 tapos ASCENDING.

*  SORT l_ltap BY prioridade DESCENDING
*                 prioridade_queue DESCENDING
*                 prioridade_to DESCENDING
*                 pos_pulmao ASCENDING
*                 vlpla ASCENDING
*                 tanum ASCENDING
*                 tapos ASCENDING.
** RL <- MOD 27.04.2005

  READ TABLE l_ltap INDEX 1.

  MOVE sy-uname TO p_nova_to-user_name.
  MOVE l_ltap-tanum TO p_nova_to-to_number.
  MOVE l_ltap-tapos TO p_nova_to-to_item.
  MOVE p_armazem TO p_nova_to-armazem.
  MOVE 'C' TO p_nova_to-status.
  MOVE l_ltap-queue TO p_nova_to-queue.
  MOVE p_equipamento TO p_nova_to-equipamento.

** SAIDAS DO PICKING/SAIDAS DRIVE-IN
  IF l_ltap-queue = queue_saida_pkf OR
     l_ltap-queue = queue_saida_dri OR
     l_ltap-queue = queue_saida_tri_men.

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
** No primeiro pulmão
            IF zwm028-paletes_pulmao <= 17.
              p_nova_to-ultimo_bin = zwm028-pulmao1.
              p_nova_to-ultimo_tipo_dep = zwm028-st_pul.
** No segundo pulmão
            ELSEIF zwm028-paletes_pulmao > 17.
              p_nova_to-ultimo_bin = zwm028-pulmao2.
              p_nova_to-ultimo_tipo_dep = zwm028-st_pul.
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
                IF zwm028-paletes_pulmao <= 17.
                  p_nova_to-ultimo_bin = zwm028-pulmao1.
                  p_nova_to-ultimo_tipo_dep = zwm028-st_pul.
                ELSEIF zwm028-paletes_pulmao > 17.
                  p_nova_to-ultimo_bin = zwm028-pulmao2.
                  p_nova_to-ultimo_tipo_dep = zwm028-st_pul.
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


  CLEAR : return_msg.
  REFRESH : return_msg.

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
                        mensula = p_lgpla.
  IF sy-subrc = 0.

*SG
    CLEAR : zwm014-su,
            zwm014-estado,
            zwm014-bin,
            zwm014-prioridade.

    MOVE-CORRESPONDING zwm014 TO wa_zwm014.

    MODIFY zwm014 FROM wa_zwm014.
    COMMIT WORK.

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

    ENDIF.

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

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'ZWM014'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

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

        SET SCREEN '0000'.LEAVE SCREEN.
        CLEAR : cursorfield.
      ENDIF.

    ELSE.
*PROCESSAMENTO OK
*actualizar tabela ZWM011 com STATUS T pq é TO
*de putaway de mesa -> mensula
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = '0001'
          status             = 'P'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'ZWM014'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      MOVE 'BIN_INPUT' TO cursorfield.

      CLEAR : return_msg.
      REFRESH : return_msg.
    ENDIF.
  ELSE.
** Mensula Invalida

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
*  break roffd.
  DATA : confirm_type,
         lgtyp_des           LIKE lagp-lgtyp,
         lgpla_des           LIKE lagp-lgpla,
         corredor(4),
         n_mensulas_ocupadas TYPE i.

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

      CLEAR : cursorfield, bin_output_d.
      SET SCREEN '0000'.LEAVE SCREEN.

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
**  SG
*        MOVE-CORRESPONDING zwm014 TO wa_zwm014.
*
*        CLEAR : wa_zwm014-su,
*                wa_zwm014-estado,
*                wa_zwm014-bin,
*                wa_zwm014-su_transito,
*                wa_zwm014-prioridade.
*
*
*        MODIFY zwm014 FROM wa_zwm014.
*        COMMIT WORK.

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
    ELSE.
      PERFORM actualiza_paletes_pulmao.
      CLEAR : to_ret2, to_ret3, to3.
    ENDIF.


  ENDIF.

ENDFORM.                    " confirma_to_total

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
  CLEAR : bin_incidencia.

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

** 04.03.2005 - ROFFD ** DEL
** Actualizar incidências
*    CALL FUNCTION 'ZWM_INSERT_ERROR'
*      EXPORTING
*        armazem    = xuser-lgnum
*        incidencia = '1'
*        posicao    = bin_incidencia
*        sscc       = su
*      EXCEPTIONS
*        no_commit  = 1
*        OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

    CLEAR : text,incidencia.
    WRITE su TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '119'
        message_var1   = text.
    CLEAR su.
    MOVE 'SU' TO cursorfield.
  ELSE.
    break roffd.
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

**  Verifica se confirma a saida das duas paletes ou se confirma apenas
**  uma e confirma outra to com a palete de cima para a zona de paletes
**  remontadas (PRM) só com uma palete


** Confirma a palete de baixo
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

            SET SCREEN '0000'.LEAVE SCREEN.
            CLEAR : cursorfield.
          ENDIF.
        ELSE.
** Se for saida do dirve-in tem de se alterar a tabela Zwm011 com o item
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
**  Verifica se confirma tb a palete de cima para a remessa ou se cria
**  uma outra TO para a zona PRM

          SELECT SINGLE *
              FROM ltap
                  WHERE lgnum = xuser-lgnum AND
                        tanum = to_ret AND
                        tapos = '0001'.

          SELECT SINGLE *
              FROM lqua
                  WHERE lgnum = xuser-lgnum AND
                        lenum = zwm020-p2.
** Confirma a palete de cima para a remessa
          DATA qtd_rest LIKE ltap-vsolm.
          qtd_rest = ltap-vsolm - ltap-vistm.
          IF qtd_rest >= lqua-verme.

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
*** É necessário criar uma to3 para a palete de cima
*              to3 = 'X'.
            ENDIF.
          ELSE.

**  tem de encontrar a to que ja foi criada para o prm e confirma com a
**  Ot de baixo
            break roffd.
            SELECT SINGLE *
                FROM ltap
                    WHERE lgnum = xuser-lgnum AND
                          vltyp = lqua-lgtyp AND
                          vlpla = lqua-lgpla AND
                          nltyp = 'PRM' AND
                          pquit = ' ' AND
                          pvqui = ' '.

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
            IF sy-subrc <> 0.

            ENDIF.
*** Tem de Criar uma to com a palete de cima para a zona PRM
*            DATA t_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.
*
*            t_sscc-sscc = lqua-lenum.
*            t_sscc-tipo_su = lqua-letyp.
*            t_sscc-material = lqua-matnr.
*            t_sscc-quantidade = lqua-verme.
*            t_sscc-uni = lqua-meins.
*            t_sscc-lote_producao = lqua-charg.
*            APPEND t_sscc.
*            CLEAR t_sscc.
*
*            CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
*              EXPORTING
*                warehouse  = xuser-lgnum
*                mov_type   = '940'
*                st_type_o  = lqua-lgtyp
*                bin_origem = lqua-lgpla
*                plant      = lqua-werks
*                s_loc      = lqua-lgort
*                origem     = 'X'
*              IMPORTING
*                to         = to_remontada
*              TABLES
*                return_msg = return_msg
*                sscc       = t_sscc
*              EXCEPTIONS
*                error      = 1
*                OTHERS     = 2.
*            IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*            ENDIF.

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
        CLEAR su.
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

          SET SCREEN '0000'.LEAVE SCREEN.
          CLEAR : cursorfield.
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

** 04.03.2005 - ROFFD ** DEL
** Actualizar incidências
*    CALL FUNCTION 'ZWM_INSERT_ERROR'
*      EXPORTING
*        armazem    = xuser-lgnum
*        incidencia = '1'
*        posicao    = bin_incidencia
*        sscc       = su
*      EXCEPTIONS
*        no_commit  = 1
*        OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

    CLEAR : text,incidencia.
    WRITE su TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '119'
        message_var1   = text.
    CLEAR su.
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

            SET SCREEN '0000'.LEAVE SCREEN.
            CLEAR : cursorfield.
          ENDIF.
        ELSE.
** Se for saida do dirve-in tem de se alterar a tabela Zwm011 com o item
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


** Confirma a palete de cima
          ltap-tapos = '0002'.

          CALL FUNCTION 'ZWM_CONFIRM_TO'
            EXPORTING
              armazem              = xuser-lgnum
              confirm_type         = 'P'
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
        CLEAR su.
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

          SET SCREEN '0000'.LEAVE SCREEN.
          CLEAR : cursorfield.
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
*  break roffd.


  CHECK NOT su IS INITIAL.

  CLEAR i_sscc.
  REFRESH i_sscc.
  CLEAR: mov, plant, s_loc, valor.

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
  break roffd.
** Não vem de um Drive In temos de ir achar a TO
** de saída para a delivery
  IF p_lgtyp_ori = 'MEN'.
    PERFORM find_to_delivery.
  ENDIF.

  CLEAR : return_msg.
  REFRESH : return_msg.

** tem de confirmar as duas paletes
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

        CLEAR : cursorfield.
        SET SCREEN '0000'.LEAVE SCREEN.

      ENDIF.
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

        SET SCREEN '0000'.LEAVE SCREEN.
        CLEAR : cursorfield.
      ENDIF.
    ENDIF.

  ELSE.

** Confirmar totalmente - TO DA DELIVERY
    CLEAR ltap.
    SELECT SINGLE tanum tapos FROM ltap
          INTO (ltap-tanum, ltap-tapos)
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

        SET SCREEN '0000'.LEAVE SCREEN.
        CLEAR : cursorfield.
      ENDIF.
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
FORM verifica_mensulas USING p_prioridade_maxima.

  DATA : corredor(4),
         n_mensulas_ocupadas.

  CLEAR : corredor,
          n_mensulas_ocupadas,
          zwm014.

  SELECT SINGLE mensula FROM zwm014 INTO zwm014-mensula
                        WHERE armazem = l_ltap-lgnum AND
                              su_transito = ltak-lznum.
  IF sy-subrc = 0.
    CONCATENATE zwm014-mensula(3) '%' INTO corredor.
    SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                    WHERE armazem = xuser-lgnum AND
                          mensula LIKE corredor AND
                          estado = 'X'.
    IF n_mensulas_ocupadas > 2.
      p_prioridade_maxima = '999'.
    ELSE.
      CLEAR p_prioridade_maxima.
    ENDIF.
    CLEAR : corredor.
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

  CLEAR: ltak, ltap, return_msg.
  REFRESH return_msg.

  SELECT SINGLE *
    FROM ltap
        WHERE lgnum = xuser-lgnum AND
              tanum = to_ret AND
              vlenr = su1.
  .
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

*  CALL FUNCTION 'ZWM_EMBALAR_HU'
*    EXPORTING
*      warehouse                  = xuser-lgnum
*      hu                         = su1
*      vbeln                      = ltak-vbeln
*    TABLES
*      return_msg                 = return_msg
*    EXCEPTIONS
*      empty_table                = 1
*      reference_document_differs = 2
*      empty_delivery_item        = 3
*      item_not_found             = 4
*      OTHERS                     = 5.
*  IF sy-subrc <> 0.
*    READ TABLE return_msg INDEX 1.
*    IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.
*
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = return_msg-msgid
*          message_lang   = sy-langu
*          message_type   = return_msg-msgtyp
*          message_number = return_msg-msgnr
*          message_var1   = return_msg-msgv1
*          message_var2   = return_msg-msgv2
*          message_var3   = return_msg-msgv3.
*
*      SET SCREEN '0000'.LEAVE SCREEN.
*      CLEAR : cursorfield.
*    ENDIF.

*  ELSE.
** Apagar da Tabela ZWM013
  DELETE FROM zwm013
      WHERE armazem   = xuser-lgnum AND
            sscc = su1.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

*  ENDIF.
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
*    CALL FUNCTION 'ZWM_EMBALAR_HU'
*      EXPORTING
*        warehouse                  = xuser-lgnum
*        hu                         = zwm020-p2
*        vbeln                      = ltak-vbeln
*      TABLES
*        return_msg                 = return_msg
*      EXCEPTIONS
*        empty_table                = 1
*        reference_document_differs = 2
*        empty_delivery_item        = 3
*        item_not_found             = 4
*        OTHERS                     = 5.
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ELSE.
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

    DELETE FROM zwm020
        WHERE armazem = xuser-lgnum AND
              p1 = su1 AND
              p2 = zwm020-p2.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDIF.
*  ENDIF.

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
ENDFORM.                    " actualiza_paletes_carro
*&---------------------------------------------------------------------*
*&      Form  actualiza_paletes_pulmao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_paletes_pulmao .
  DATA : wait_time(20), lock_key(50).
  CLEAR : wait_time, lock_key.

  break roffd.
** Tempo de espera no lock
  PERFORM get_parameter
          USING xuser-lgnum
                'ATRIBUICAO_TO'
                'WAIT_TIME'
                wait_time.

  MOVE 'PALETES_PULMAO' TO lock_key.
** Fazer Lock para não existirem dois ou
** mais operários a actualizar o número
** de paletes no pulmão
  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM028'
*       keyword_       = lock_key
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO wait_time SECONDS.
    ENDIF.
  ENDDO.

** Actualizar entrada do grupo com as paletes q já estão no pulmão

  DATA aux_pal TYPE i.
  CLEAR: aux_pal, zwm028.

  SELECT SINGLE * FROM zwm028
                  WHERE lgnum = xuser-lgnum AND
                        refnr = grupo AND
                        remessa = ' '.
  IF sy-subrc = 0.
    aux_pal = zwm028-paletes_pulmao + 1.

    UPDATE zwm028 SET paletes_pulmao = aux_pal
        WHERE lgnum = xuser-lgnum AND
              refnr = grupo AND
              remessa = ' '.
    COMMIT WORK.
  ENDIF.

** Actualizar entrada da remessa com as paletes q já estão no pulmão
  CLEAR zwm028.
  SELECT SINGLE * FROM zwm028
                    WHERE lgnum = xuser-lgnum AND
                          refnr = grupo AND
                          remessa = remessa.
  IF sy-subrc = 0.
    CLEAR aux_pal.
    aux_pal = zwm028-paletes_pulmao + 1.

    UPDATE zwm028 SET paletes_pulmao = aux_pal
        WHERE lgnum = xuser-lgnum AND
              refnr = grupo AND
              remessa = remessa.
    COMMIT WORK.
  ENDIF.

** Actualizar a posicao no pulmao onde se encontra a palete
  IF bin_output_d(3) = 'PUL' OR bin_output_d(3) = 'PPK'.
    DATA: x_zwm013 LIKE zwm013 OCCURS 0 WITH HEADER LINE.
    CLEAR x_zwm013.
    REFRESH x_zwm013.
    x_zwm013-armazem = xuser-lgnum.
    x_zwm013-sscc = su.
    x_zwm013-destino = bin_output_d.
    x_zwm013-bloqueado = 'X'.
    x_zwm013-tipo_palete = tipo_palete.
    x_zwm013-posicao_pulmao = posicao_pulmao2.
    CLEAR x_zwm013-fabrica_1.
    APPEND x_zwm013.
    CLEAR x_zwm013.
    IF NOT to3 IS INITIAL AND to_remontada IS INITIAL.
** as duas paletes vao para o carro
      x_zwm013-armazem = xuser-lgnum.
      x_zwm013-sscc = zwm020-p2.
      x_zwm013-destino = bin_output_d.
      x_zwm013-bloqueado = 'X'.
      x_zwm013-tipo_palete = tipo_palete.
      x_zwm013-posicao_pulmao = posicao_pulmao2.
      CLEAR x_zwm013-fabrica_1.
      APPEND x_zwm013.
      CLEAR x_zwm013.
    ELSE.
** uma palete é devolvida para o PRM
      DELETE FROM zwm020
          WHERE armazem = xuser-lgnum AND
                ( p1 = su OR p2 = zwm020-p2 ).
      COMMIT WORK.
    ENDIF.
    INSERT zwm013 FROM TABLE x_zwm013.
    COMMIT WORK.
  ENDIF.

* Unlock ao lock efectuado na atribuicao da posicao do pulmao
  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM028'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  CLEAR : lock_key, wait_time,cursorfield.

ENDFORM.                    " actualiza_paletes_pulmao
*&---------------------------------------------------------------------*
*&      Form  actualiza_lista_espera
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ARMAZEM  text
*      -->P_MATRICULA  text
*      -->P_NUM_ENTRADA  text
*      -->P_OBSERVACOES  text
*      -->P_0478   text
*      -->P_SY_UZEIT  text
*      -->P_SY_DATUM  text
*      -->P_0481   text
*----------------------------------------------------------------------*
FORM actualiza_lista_espera  USING    p_armazem
                                      p_matricula
                                      p_num_entrada
                                      p_observacoes
                                      p_observacoes2
                                      VALUE(p_operacao)
                                      p_uzeit
                                      p_datum
                                      VALUE(p_estado).

  DATA : it_dd07v_tab LIKE dd07v OCCURS 0 WITH HEADER LINE.


  SELECT n_transporte INTO ti_zwm003-n_transporte
                      FROM  zwm006_aux UP TO 1 ROWS
         WHERE  num_entrada  = p_num_entrada
         AND    finalizada   = ' '.

  ENDSELECT.

  CLEAR : it_dd07v_tab[], it_dd07v_tab.

  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      name      = 'ZESTADO_FE'
      langu     = sy-langu
*     TEXTS_ONLY          = ' '
    TABLES
      dd07v_tab = it_dd07v_tab
*   EXCEPTIONS
*     ILLEGAL_INPUT       = 1
*     OTHERS    = 2
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  READ TABLE it_dd07v_tab WITH KEY domvalue_l = p_estado.

  IF sy-subrc = 0.
    ti_zwm003-estado_desc = it_dd07v_tab-ddtext(21).
  ENDIF.

  ti_zwm003-armazem       = p_armazem.
  ti_zwm003-matricula     = p_matricula.
  ti_zwm003-num_entrada   = p_num_entrada.
  IF NOT p_observacoes IS INITIAL.
    ti_zwm003-observacoes = p_observacoes.
  ELSE.
    CLEAR ti_zwm003-observacoes.
  ENDIF.
  ti_zwm003-observacoes2 = p_observacoes2.
  CLEAR ti_zwm003-porta.
  ti_zwm003-operacao     = p_operacao.    "'CARGA'.
  ti_zwm003-hora_entrada = p_uzeit.
  ti_zwm003-data         = p_datum.
  ti_zwm003-estado       = p_estado.
  APPEND ti_zwm003.

** Registo da entrada na fila de espera na BD
  CALL FUNCTION 'ZWM_MANAGE_PARKING_V1'
    EXPORTING
      operacao                = '2'
      armazem                 = p_armazem
    TABLES
      l_zwm003                = ti_zwm003
      return_msg              = return_msg
    EXCEPTIONS
      tab_zwm003_not_filled   = 1
      invalid_parameter       = 2
      tab_l_zwm003_filled     = 3
      tab_l_zwm003_not_filled = 4
      no_warehouse            = 5
      OTHERS                  = 6.
  IF sy-subrc <> 0.
    RAISE error_in_manage_parking.
  ENDIF.


ENDFORM.                    " actualiza_lista_espera
*&---------------------------------------------------------------------*
*&      Form  get_porta_descarga
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_porta_descarga TABLES return_truck STRUCTURE zwm_return_truck
                        USING armazem st_pul.

  DATA : BEGIN OF lagp_aux OCCURS 2,
           lgpla LIKE lagp-lgpla.
  DATA : END OF lagp_aux.

  CLEAR lagp_aux.
  REFRESH lagp_aux.
*--------------------------------------------------------------New
  CLEAR: l_portas_descarga, l_portas_descarga[].
*--------------------------------------------------------------
  IF l_portas_descarga[] IS INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE
                                l_portas_descarga
             FROM zwm002 AS w INNER JOIN zwm007 AS m
             ON  w~armazem = m~armazem
             AND w~porta = m~porta
             WHERE w~armazem  = armazem
             AND   w~bloqueio = ' '
             AND   w~estado   = 'D'
             AND   m~tipo    <> 'C'.
  ENDIF.

  IF sy-subrc = 0.

    SORT l_portas_descarga BY porta ASCENDING.

    LOOP AT l_portas_descarga WHERE atribuida = ' '.

      DO 12 TIMES VARYING pulmao FROM
                         l_portas_descarga-pulmao1 NEXT
                         l_portas_descarga-pulmao2.

** Lê os três caracteres do meio q correspondem à área e identifica
** sempre os pulmões dois a dois
        CLEAR : pulmao_aux.
        pulmao_aux = pulmao+4(3).
*                break roffd.
** Possivelmente falta a área '001' para verificar se um conjunto de
** DOIS CANAIS estão LIVRES
        SELECT * INTO CORRESPONDING FIELDS OF TABLE t_lagp
            FROM lagp
                WHERE lgnum = armazem AND
                      lgtyp = st_pul AND
                      lgber = pulmao_aux AND
                      brand = ' '.

        IF t_lagp[] IS INITIAL.
          CLEAR encontrou_pulmao.
          CONTINUE.
        ENDIF.

        DESCRIBE TABLE t_lagp LINES num_pulmao.
        IF num_pulmao = 1.
          CLEAR encontrou_pulmao.
          CONTINUE.
        ENDIF.

        READ TABLE t_lagp INDEX 1.
        CLEAR: pulmao1, pulmao2.

        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = t_lagp-lgtyp
            lgpla = t_lagp-lgpla
          IMPORTING
            bin   = pulmao1.

        READ TABLE t_lagp INDEX 2.
        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = t_lagp-lgtyp
            lgpla = t_lagp-lgpla
          IMPORTING
            bin   = pulmao2.

        SELECT COUNT(*) INTO num_quantos
                FROM zwm013
                    WHERE armazem = armazem AND
                          ( destino = pulmao1 OR
                            destino = pulmao2 ).
        IF sy-subrc = 0.
          IF num_quantos <> 0.
            CLEAR encontrou_pulmao.
            CONTINUE.
          ENDIF.
        ENDIF.

        CLEAR num_quantos.
        encontrou_pulmao = 'X'.
        EXIT.

      ENDDO.
** É pq a porta que encontrou não é válida
      IF encontrou_pulmao IS INITIAL.
        CLEAR : porta, encontrou_pulmao, num_quantos.
      ELSEIF NOT encontrou_pulmao IS INITIAL.
        CLEAR : encontrou_pulmao, num_quantos, sy-subrc.

*-----------------------------------------------------------Ins Mai2005
        porta = l_portas_descarga-porta.
*----------------------------------------------------------------------

        EXIT.
      ENDIF.

    ENDLOOP.

*=========================eventualmente redundante==============
** Existe pelo menos um pulmão LIVRE de descarga pode prosseguir para o
** ALGORITMO de atribuição de PORTA
    IF num_quantos = 0 AND sy-subrc = 0.

*      CALL FUNCTION 'ZWM_GET_DOOR'
*        EXPORTING
*          armazem                  = armazem
*          tipo_camiao              = tipo_camiao
*          matricula                = l_zwm003-matricula
*          num_entrada              = l_zwm003-num_entrada
*        IMPORTING
*          porta                    = porta
*        TABLES
*          return_msg               = return_msg
*          l_zwm002                 = l_zwm002
*        EXCEPTIONS
*          no_warehouse             = 1
*          wrong_tipo_camiao        = 2
*          error_in_manage_doors    = 3
*          error_in_doors           = 4
*          no_door_to_direct_charge = 5
*          OTHERS                   = 6.
*      IF sy-subrc <> 0.
*
*      ELSE.
      IF NOT porta IS INITIAL.
**TABELA AUXILIAR
** Seleccionar os pulmões atribuídos à porta
        SELECT * FROM lagp
                  WHERE lgnum = armazem
                  AND   lgtyp = st_pul
                  AND   lgber = pulmao_aux
                  AND   brand = ' '.

          IF sy-subrc = 0.
            lagp_aux-lgpla = lagp-lgpla.
            APPEND lagp_aux.
          ENDIF.
        ENDSELECT.

** Actualização da tabela de retorno com camião e porta atribuida
        return_truck-porta       = porta.
        return_truck-matricula   = l_zwm003-matricula.
        return_truck-observacoes = l_zwm003-observacoes.
        return_truck-num_entrada = l_zwm003-num_entrada.
** Transportador
        SELECT SINGLE transportador FROM zwm005 INTO
                               return_truck-transportador
                 WHERE num_entrada = l_zwm003-num_entrada.
** Nome do transportador
        SELECT SINGLE name1 FROM lfa1 INTO
                      return_truck-desc_transportador
                 WHERE lifnr = return_truck-transportador.

** Seleccionar os pulmões atribuídos à porta **************************
        READ TABLE lagp_aux INDEX 1.
        IF sy-subrc = 0.
          return_truck-pulmao_1 = lagp_aux-lgpla.
        ENDIF.

        READ TABLE lagp_aux INDEX 2.
        IF sy-subrc = 0.
          return_truck-pulmao_2 = lagp_aux-lgpla.
        ENDIF.
** Seleccionar os pulmões atribuídos à porta
*        APPEND return_truck.
        COLLECT return_truck.

*-----------------------------------------------------------Ins Mai2005
*==============================ini com=================================
** Actualizar fila de espera a indicar q o camião foi atribuido
*        l_zwm003-atribuida = 'X'.
*        MODIFY l_zwm003 INDEX s_tabix.
*
*** Actualizar estado da porta a indicar q a porta está atribuida
*        READ TABLE l_portas_descarga WITH KEY porta = porta.
*        IF sy-subrc = 0.
*          index = sy-tabix.
*          l_portas_descarga-atribuida = 'X'.
*          MODIFY l_portas_descarga INDEX index.
*        ENDIF.

*** Actualizar tabela com o estado GLOBAL
*        CLEAR index.
*        READ TABLE l_zwm002 WITH KEY porta = porta.
*        IF sy-subrc = 0.
*          index = sy-tabix.
*          l_zwm002-bloqueio = 'X'.
*
*** Seleccionar os pulmões atribuídos à porta
*          READ TABLE lagp_aux INDEX 1.
*          IF sy-subrc = 0.
*            l_zwm002-pulmao_1 = lagp_aux-lgpla.
*          ENDIF.
*
*          READ TABLE lagp_aux INDEX 2.
*          IF sy-subrc = 0.
*            l_zwm002-pulmao_2 = lagp_aux-lgpla.
*          ENDIF.
*** Seleccionar os pulmões atribuídos à porta
*
*          MODIFY l_zwm002 INDEX index.
*        ENDIF.
*=========================fim com================================
** Actualizar tabela de posições de depósito (lagp) com o indicador de
** OCUPADO para os dois pulmões de descarga
**        LOOP AT lagp_aux.
**          UPDATE lagp SET brand = 'X'
**                      WHERE lgnum = armazem AND
**                            lgtyp = st_pul AND
**                            lgpla = lagp_aux-lgpla.
**          COMMIT WORK.
**        ENDLOOP.

        DELETE return_truck WHERE porta = space.

        CLEAR : return_truck, l_zwm003, zwm007, index ,
                l_zwm002, lagp_aux, pulmao_aux.
        CLEAR : num_quantos, pulmao, porta, tipo_camiao.

        REFRESH : lagp_aux.
      ENDIF.
****      ENDIF.

    ENDIF.   " num_quantos = 0

  ENDIF.

ENDFORM.                    " get_porta_descarga
*&---------------------------------------------------------------------*
*&      Form  tipo_camiao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tipo_camiao USING armazem.
  IF l_zwm003-operacao = 'DESCARGA'.
    tipo_camiao = 'R'.
  ELSEIF l_zwm003-operacao = 'CARGA'.
** Seleccionar o transporte
    SELECT SINGLE n_transporte
                    FROM  zwm006_aux
                    INTO  zwm006_aux-n_transporte
                    WHERE armazem = armazem
                    AND   num_entrada = l_zwm003-num_entrada.

    IF sy-subrc = 0.

** Verificar se o transporte tem carga assignada

* Remessa
      SELECT SINGLE vbeln FROM vttp INTO vttp-vbeln
                          WHERE tknum = zwm006_aux-n_transporte.
      IF sy-subrc = 0.
        SELECT * FROM vbss WHERE vbeln = vttp-vbeln.

          SELECT SINGLE *
              FROM vbsk
                  WHERE sammg = vbss-sammg AND
                        smart = 'W'.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            CLEAR vbss.
          ENDIF.
        ENDSELECT.

        SELECT SINGLE * FROM zwm028
                WHERE lgnum   = armazem
                AND   refnr   = vbss-sammg
                AND   remessa = space.             "Ins Mai2005


        IF zwm028-st_pul = 'PUL' OR
           zwm028-st_dck = 'PUL' OR
           zwm028-st_pul = 'PLT' OR
           zwm028-st_pul = 'PLM' OR
           zwm028-st_pul = 'PUA'.
          tipo_camiao = 'P'.
        ELSEIF zwm028-st_dck = 'DCK'.
          tipo_camiao = 'E'.
        ELSE.
          tipo_camiao = 'E'.
        ENDIF.
      ELSE.
        tipo_camiao = 'E'.
      ENDIF.
    ELSE.
      tipo_camiao = 'E'.
    ENDIF.

** Verificar se o transporte tem carga assignada
  ELSE.
    tipo_camiao = 'E'.
  ENDIF.

ENDFORM.                    " tipo_camiao
*&---------------------------------------------------------------------*
*&      Form  get_portas_carga_directa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_portas_carga_directa
                         TABLES return_truck STRUCTURE zwm_return_truck
                         USING armazem.

*-----------------------------------------------------------new
  CLEAR : l_portas_cargad[], l_portas_cargad.
*--------------------------------------------------------------

  IF l_portas_cargad[] IS INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE
                                l_portas_cargad
             FROM zwm002 AS w INNER JOIN zwm007 AS m
             ON  w~armazem   = m~armazem
             AND w~porta     = m~porta
             WHERE w~armazem  = armazem
             AND   w~bloqueio = ' '
             AND   w~estado   = 'D'
             AND   m~tipo    <> 'D'.
  ENDIF.

** Só executa o algoritmo de atribuição de porta caso exista um
** transporte SAP associado ao camião
  IF NOT zwm006_aux-n_transporte IS INITIAL.

*----------------------------------------------------------Ins Mai2005
    CALL FUNCTION 'ZWM_GET_DOOR_S'
      EXPORTING
        armazem                  = armazem
        tipo_camiao              = tipo_camiao
        matricula                = l_zwm003-matricula
        num_entrada              = l_zwm003-num_entrada
      IMPORTING
        porta                    = porta
      TABLES
        return_msg               = return_msg
        l_zwm002                 = l_zwm002
        t_portas                 = t_portas
      EXCEPTIONS
        no_warehouse             = 1
        wrong_tipo_camiao        = 2
        error_in_manage_doors    = 3
        error_in_doors           = 4
        no_door_to_direct_charge = 5
        no_door_exists           = 6
        no_delivery_exists       = 7
        OTHERS                   = 8.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

    ELSE.
      IF NOT t_portas[] IS INITIAL.
** Actualização da tabela de retorno com camião e porta atribuida
        return_truck-porta       = porta.
        return_truck-matricula   = l_zwm003-matricula.
        return_truck-observacoes = l_zwm003-observacoes.
        return_truck-num_entrada = l_zwm003-num_entrada.
** Transportador
        SELECT SINGLE transportador FROM zwm005 INTO
                                return_truck-transportador
                   WHERE num_entrada = l_zwm003-num_entrada.
** Nome do transportador
        SELECT SINGLE name1 FROM lfa1 INTO
                      return_truck-desc_transportador
                   WHERE lifnr = return_truck-transportador.

        CLEAR return_truck-pulmao_1.
        CLEAR return_truck-pulmao_2.
*        APPEND return_truck.
        COLLECT return_truck.

*-----------------------------------------------------------Ins Mai2005
        LOOP AT t_portas.
          IF porta IS NOT INITIAL AND porta = t_portas-porta.
            CONTINUE.
          ENDIF.
          return_truck-porta = t_portas-porta.
          APPEND return_truck.
        ENDLOOP.
        DELETE return_truck WHERE porta = space.

        CLEAR: t_portas, t_portas[].

*----------------------------------------------------------------------

        CLEAR : return_truck, l_zwm003, zwm007.
        CLEAR : num_quantos, pulmao, porta, tipo_camiao.

      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " get_portas_carga_directa
*&---------------------------------------------------------------------*
*&      Form  get_porta_pulmao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN_TRUCK  text
*      -->P_ARMAZEM  text
*----------------------------------------------------------------------*
FORM get_porta_pulmao TABLES return_truck STRUCTURE zwm_return_truck
                      USING armazem.
*-----------------------------------------------------------new
  CLEAR : l_portas_cargap[], l_portas_cargap.
*--------------------------------------------------------------
  SELECT SINGLE * FROM vttp WHERE tknum = zwm006_aux-n_transporte.

  IF sy-subrc = 0.

    SELECT * FROM vbss WHERE vbeln = vttp-vbeln.

      SELECT SINGLE *
          FROM vbsk
              WHERE sammg = vbss-sammg AND
                    smart = 'W'.
      IF sy-subrc = 0.
        EXIT.
      ELSE.
        CLEAR vbss.
      ENDIF.

    ENDSELECT.

    SELECT SINGLE * FROM zwm028
          WHERE lgnum   = armazem
          AND   refnr   = vbss-sammg
          AND   remessa = space.                 "Ins Mai2005

    IF zwm028-total_paletes <> zwm028-paletes_pulmao.
      IF zwm028-st_pul <> 'PUA'.
        incompleta = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

** Se a carga já está completa ... segue-se ... senão passa-se para o
** próximo camião
  IF incompleta IS INITIAL.

** Descobrir quais as portas associadas ao pulmão e que estejam
** desbloqueadas
    IF l_portas_cargap[] IS INITIAL.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE l_portas_cargap
               FROM zwm002 AS w INNER JOIN zwm007 AS m
               ON  w~armazem = m~armazem
               AND w~porta   = m~porta
               WHERE w~armazem  = armazem
               AND   w~bloqueio = ' '
               AND   w~estado   = 'D'
               AND   m~tipo    <> 'D' AND
                   ( m~pulmao1 = zwm028-pulmao1 OR
                     m~pulmao2 = zwm028-pulmao1 OR
                     m~pulmao3 = zwm028-pulmao1 OR
                     m~pulmao4 = zwm028-pulmao1 OR
                     m~pulmao5 = zwm028-pulmao1 OR
                     m~pulmao6 = zwm028-pulmao1 OR
                     m~pulmao7 = zwm028-pulmao1 OR
                     m~pulmao8 = zwm028-pulmao1 OR
                     m~pulmao9 = zwm028-pulmao1 OR
                     m~pulmao10 = zwm028-pulmao1 OR
                     m~pulmao11 = zwm028-pulmao1 OR
                     m~pulmao12 = zwm028-pulmao1 OR
                     m~pulmao13 = zwm028-pulmao1 OR
                     m~pulmao14 = zwm028-pulmao1 OR
                     m~pulmao15 = zwm028-pulmao1 OR
                     m~pulmao16 = zwm028-pulmao1 OR
                     m~pulmao17 = zwm028-pulmao1 OR
                     m~pulmao18 = zwm028-pulmao1 OR
                     m~pulmao19 = zwm028-pulmao1 OR
                     m~pulmao20 = zwm028-pulmao1 OR
                     m~pulmao21 = zwm028-pulmao1 OR
                     m~pulmao22 = zwm028-pulmao1 OR
                     m~pulmao23 = zwm028-pulmao1 OR
                     m~pulmao24 = zwm028-pulmao1 OR
                     m~pulmao25 = zwm028-pulmao1 OR
                     m~pulmao26 = zwm028-pulmao1 OR
                     m~pulmao27 = zwm028-pulmao1 OR
                     m~pulmao28 = zwm028-pulmao1 ).
      IF sy-subrc = 0.

        encontrou_porta = 'X'.

      ENDIF.
    ENDIF.

    IF sy-subrc = 0 AND NOT encontrou_porta IS INITIAL.
** Achou portas desbloqueadas ... algoritmo de escolha de porta
*----------------------------------------------------------Ins Mai2005
      CALL FUNCTION 'ZWM_GET_DOOR_S'
        EXPORTING
          armazem                  = armazem
          tipo_camiao              = tipo_camiao
          matricula                = l_zwm003-matricula
          num_entrada              = l_zwm003-num_entrada
        IMPORTING
          porta                    = porta
        TABLES
          return_msg               = return_msg
          l_zwm002                 = l_zwm002
          t_portas                 = t_portas
        EXCEPTIONS
          no_warehouse             = 1
          wrong_tipo_camiao        = 2
          error_in_manage_doors    = 3
          error_in_doors           = 4
          no_door_to_direct_charge = 5
          no_door_exists           = 6
          no_delivery_exists       = 7
          OTHERS                   = 8.
      IF sy-subrc <> 0.

      ELSE.

        IF NOT t_portas[] IS INITIAL.
** Actualização da tabela de retorno com camião e porta atribuida
          return_truck-porta       = porta.
          return_truck-matricula   = l_zwm003-matricula.
          return_truck-observacoes = l_zwm003-observacoes.
          return_truck-num_entrada = l_zwm003-num_entrada.
** Transportador
          SELECT SINGLE transportador FROM zwm005 INTO
                             return_truck-transportador
               WHERE num_entrada = l_zwm003-num_entrada.
** Nome do transportador
          SELECT SINGLE name1 FROM lfa1 INTO
                        return_truck-desc_transportador
               WHERE lifnr = return_truck-transportador.


          IF zwm028-st_pul = 'PUL' OR
             zwm028-st_pul = 'PLT' OR
             zwm028-st_pul = 'PLM' OR
             zwm028-st_pul = 'PUA'.
            return_truck-pulmao_1 = zwm028-pulmao1.
            return_truck-pulmao_2 = zwm028-pulmao2.
          ELSE.
            CLEAR: return_truck-pulmao_1,
                   return_truck-pulmao_2.
          ENDIF.
**          APPEND return_truck.
          COLLECT return_truck.

*-----------------------------------------------------------Ins Mai2005
          LOOP AT t_portas.
            return_truck-porta = t_portas-porta.
*            APPEND return_truck.
            COLLECT return_truck.

          ENDLOOP.

          DELETE return_truck WHERE porta = space.

          CLEAR: t_portas, t_portas[].

*----------------------------------------------------------------------

          CLEAR : return_truck, l_zwm003, zwm007, remessas,
                  l_zwm002, l_lips.
          CLEAR : num_quantos, pulmao, porta, tipo_camiao,
                  encontrou_porta.

          REFRESH : remessas, l_lips.

        ENDIF.
      ENDIF.             "Get_door
    ENDIF.
  ENDIF.          " incompleta

ENDFORM.                    " get_porta_pulmao
