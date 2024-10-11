FUNCTION zwm_get_row_users_tri.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"  TABLES
*"      T_ZWM011 STRUCTURE  ZWM011
*"      S_ZWM_ROW_USERS STRUCTURE  ZWM_ROW_USERS
*"      S_ZWM_QUEUE STRUCTURE  ZWM_QUEUE
*"----------------------------------------------------------------------
  DATA : x_zwm_queue LIKE zwm_queue OCCURS 0 WITH HEADER LINE,
         x_zwm011 LIKE zwm011 OCCURS 0 WITH HEADER LINE.

  DATA: production_queue(20),
        tri_out_queue(20),
        tri_out_rep(20),
        dev_inc_queue(20).

  DATA indice LIKE sy-tabix.

  x_zwm011[] = t_zwm011[].
  x_zwm_queue[] = s_zwm_queue[].

  LOOP AT x_zwm011.
    indice = sy-tabix.

** Apagar entradas de filas não interessantes
    READ TABLE x_zwm_queue WITH KEY queue = x_zwm011-queue.
    IF sy-subrc <> 0.
      DELETE x_zwm011 INDEX indice.
      CLEAR indice.
    ENDIF.

  ENDLOOP.

  CLEAR indice.
  LOOP AT x_zwm011.
    indice = sy-tabix.
** Apagar entradas do nosso utilizador
    IF x_zwm011-user_name = sy-uname.
      DELETE x_zwm011 INDEX indice.
    ENDIF.
  ENDLOOP.

** Apagar os User que não estejam no trilateral
  DELETE x_zwm011 WHERE equipamento(10) <> 'TRILATERAL'.

** Fila de entrada de produção
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PRODUCAO'
                production_queue.


** Fila Entrada das Devolucoes e Incidencias
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_DEV_INC'
                dev_inc_queue.

** Fila de saida da zona de TRILATERIAIS
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_TRI'
                tri_out_queue.


** Fila de saida da zona de TRILATERIAIS para o reabastecimento
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PICK_TRI'
                tri_out_rep.


** Eliminar o q não interessa
  LOOP AT x_zwm011.

** Para queues de entrada ve-se na posição de destino
    IF x_zwm011-queue = production_queue OR
       x_zwm011-queue = dev_inc_queue.

** verificar se na posição de destino da TO está englobado o q vem dos
** outros utilizadores ( APENAS PELO CORREDOR )
      SELECT SINGLE *
        FROM ltap
          WHERE lgnum = x_zwm011-armazem AND
                tanum = x_zwm011-to_number AND
                tapos = x_zwm011-to_item.
      MOVE ltap-nlpla(3) TO s_zwm_row_users-corredor.
      MOVE x_zwm011-user_name TO s_zwm_row_users-users.
      APPEND s_zwm_row_users.
      CLEAR s_zwm_row_users.
** Para queues de saida ve-se na posicao de origem
    ELSEIF x_zwm011-queue = tri_out_queue OR
           x_zwm011-queue = tri_out_rep.
      SELECT SINGLE *
       FROM ltap
         WHERE lgnum = x_zwm011-armazem AND
               tanum = x_zwm011-to_number AND
               tapos = x_zwm011-to_item.
      MOVE ltap-vlpla(3) TO s_zwm_row_users-corredor.
      MOVE x_zwm011-user_name TO s_zwm_row_users-users.
      APPEND s_zwm_row_users.
      CLEAR s_zwm_row_users.
    ENDIF.

  ENDLOOP.


ENDFUNCTION.
