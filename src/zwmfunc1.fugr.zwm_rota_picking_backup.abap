FUNCTION ZWM_ROTA_PICKING_BACKUP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      L_ZWM026 STRUCTURE  ZWM026
*"      PARES_PALETES_AUX STRUCTURE  ZWMPARES_PALETES
*"----------------------------------------------------------------------
  DATA : l_total_ext LIKE zwm026-quantidade,
         n_materiais TYPE i.


** Tabela com o total de extracções por palete de picking
  DATA : BEGIN OF rota_zwm026 OCCURS 0.
          INCLUDE STRUCTURE zwm026.
  DATA : n_ext LIKE zwm026-quantidade.            "número de extracções
  DATA : END OF rota_zwm026.


** Tabela auxiliar
  DATA : BEGIN OF rota_zwm026_aux OCCURS 0.
          INCLUDE STRUCTURE rota_zwm026.
  DATA : END OF rota_zwm026_aux.


** Tabela com a relação entre pares de paletes e
** as correspondentes referências comuns
  DATA : BEGIN OF pares_paletes OCCURS 0,
          pal1 LIKE zwm026-n_pal_picking,                   " 1 palete
          pal2 LIKE zwm026-n_pal_picking,                   " 2 palete
          ref_com TYPE i,                   " nº referências comuns
** diferença entre o total de materiais de uma palete e o total
** de referências comuns
          contidos TYPE i,
          n_ext LIKE zwm026-quantidade,     " nº extracções
          sorlp LIKE zwm026-sorlp.
  DATA : END OF pares_paletes.


**  DATA : pares_paletes_aux LIKE pares_paletes OCCURS 0 WITH HEADER
**LINE.

** Tabela que contém a palete q está a ser avaliada
  DATA : BEGIN OF pal_actual OCCURS 0.
          INCLUDE STRUCTURE rota_zwm026.
  DATA : END OF pal_actual.


** Tabela que contém a próxima palete q está a ser avaliada
  DATA : BEGIN OF pal_prox OCCURS 0.
          INCLUDE STRUCTURE rota_zwm026.
  DATA : END OF pal_prox.


** Tabela que contém a próxima palete actual q está a ser avaliada
  DATA : BEGIN OF pal_prox_actual OCCURS 0.
          INCLUDE STRUCTURE rota_zwm026.
  DATA : END OF pal_prox_actual.

** Tabela auxiliar para o cálculo das paletes que não possuem
** pares
  DATA : BEGIN OF palete_unica OCCURS 0,
            n_pal_picking LIKE zwm026-n_pal_picking,
            material LIKE zwm026-material,
            sorlp LIKE zwm026-sorlp.
  DATA : END OF palete_unica.


  CLEAR : pares_paletes,
          pal_actual,
          pal_prox,
          pal_prox_actual,
          rota_zwm026,
          rota_zwm026_aux.

  REFRESH : pares_paletes,
            pal_actual,
            pal_prox,
            pal_prox_actual,
            rota_zwm026,
            rota_zwm026_aux.

** Actualização da tabela auxiliar para cálculos
  LOOP AT l_zwm026.
    MOVE-CORRESPONDING l_zwm026 TO rota_zwm026.
    APPEND rota_zwm026.
  ENDLOOP.


** Cálculo do número de extracções por palete de picking
  SORT rota_zwm026 BY n_pal_picking.
  LOOP AT rota_zwm026.
    AT NEW n_pal_picking.
      SUM.
      CLEAR : l_total_ext.
      MOVE rota_zwm026-quantidade TO l_total_ext.
    ENDAT.

    MOVE l_total_ext TO rota_zwm026-n_ext.
    MODIFY rota_zwm026.

    CLEAR : rota_zwm026.

  ENDLOOP.


** Actualização da tabela auxiliar
  rota_zwm026_aux[] = rota_zwm026[].


** ALGORITMO DE CÁLCULO DE ROTA

** Ordenação pelo menor número de extracções
** SG - alteração pedida pela Miebach
  SORT rota_zwm026 BY n_ext DESCENDING n_pal_picking ASCENDING.
  SORT rota_zwm026_aux BY n_ext DESCENDING n_pal_picking ASCENDING.


  LOOP AT rota_zwm026.

    AT NEW n_pal_picking.
** Palete actual - INICIO
      CLEAR : pal_actual.
      REFRESH : pal_actual.

      LOOP AT rota_zwm026_aux WHERE n_pal_picking =
                                    rota_zwm026-n_pal_picking.
        MOVE-CORRESPONDING rota_zwm026_aux TO pal_actual.
        APPEND pal_actual.
      ENDLOOP.
** Palete actual - FIM

** Palete seguinte - INICIO
      SORT rota_zwm026_aux BY n_ext DESCENDING n_pal_picking ASCENDING.
      LOOP AT rota_zwm026_aux
              WHERE n_pal_picking <> rota_zwm026-n_pal_picking AND
                    remessa <> rota_zwm026-remessa.
        MOVE-CORRESPONDING rota_zwm026_aux TO pal_prox.
        APPEND pal_prox.
      ENDLOOP.
** Palete seguinte - FIM

** Verificação do número de referências comuns
      SORT pal_actual BY n_ext DESCENDING n_pal_picking ASCENDING
                         material ASCENDING.
      SORT pal_prox BY n_ext DESCENDING n_pal_picking ASCENDING
                         material ASCENDING.

      LOOP AT pal_prox.
        AT NEW n_pal_picking.

** A próxima palete a ser avaliada

          LOOP AT rota_zwm026_aux WHERE n_pal_picking =
                                     pal_prox-n_pal_picking.
            MOVE-CORRESPONDING rota_zwm026_aux TO pal_prox_actual.
            APPEND pal_prox_actual.
          ENDLOOP.


** Descobrir quantos materiais tem a palete actual
          CLEAR n_materiais.
          DESCRIBE TABLE pal_actual LINES n_materiais.

          LOOP AT pal_actual.

            READ TABLE pal_prox_actual
                 WITH KEY material = pal_actual-material.
            IF sy-subrc = 0.
** Se encontra a mesma referência actualizar
** número de referências comuns
              READ TABLE pares_paletes WITH KEY
                         pal1 = pal_actual-n_pal_picking
                         pal2 = pal_prox_actual-n_pal_picking.
              IF sy-subrc <> 0.
                pares_paletes-pal1 = pal_actual-n_pal_picking.
                pares_paletes-pal2 = pal_prox_actual-n_pal_picking.
                pares_paletes-n_ext = pal_actual-n_ext +
                                      pal_prox_actual-n_ext.
                pares_paletes-ref_com = 1.
                pares_paletes-contidos = n_materiais -
                                         pares_paletes-ref_com.
                APPEND pares_paletes.
                CLEAR pares_paletes.
              ELSE.
                pares_paletes-ref_com = pares_paletes-ref_com + 1.
                pares_paletes-contidos = n_materiais -
                                         pares_paletes-ref_com.
                MODIFY pares_paletes INDEX sy-tabix.
                CLEAR pares_paletes.
              ENDIF.
** Não tem a referência actual
            ELSE.
              READ TABLE pares_paletes WITH KEY
                                     pal1 = pal_actual-n_pal_picking
                                   pal2 = pal_prox_actual-n_pal_picking.
              IF sy-subrc <> 0.
                pares_paletes-pal1 = pal_actual-n_pal_picking.
                pares_paletes-pal2 = pal_prox_actual-n_pal_picking.
                pares_paletes-n_ext = pal_actual-n_ext +
                                      pal_prox_actual-n_ext.
                pares_paletes-ref_com = 0.
                pares_paletes-contidos = n_materiais -
                                         pares_paletes-ref_com.
                APPEND pares_paletes.
                CLEAR pares_paletes.
              ENDIF.
            ENDIF.
          ENDLOOP.

          CLEAR : pal_prox_actual.
          REFRESH : pal_prox_actual.

        ENDAT.
      ENDLOOP.

      CLEAR : pal_prox.
      REFRESH : pal_prox.

** Apagar da tabela auxiliar a entrada que já foi avaliada
      DELETE rota_zwm026_aux WHERE
             n_pal_picking = rota_zwm026-n_pal_picking.

    ENDAT.

  ENDLOOP.

*  LOOP AT pares_paletes.
*
*    WRITE: / pares_paletes-pal1,
*             pares_paletes-pal2,
*             pares_paletes-ref_com,
*             pares_paletes-n_ext,
*             pares_paletes-contidos.
*
*  ENDLOOP.
*
*  WRITE : /.
*  WRITE : /.

** Ordenação da tabela de acordo com o número de referências comuns
  SORT pares_paletes BY contidos ASCENDING ref_com DESCENDING
                        n_ext DESCENDING.
*
*  LOOP AT pares_paletes.
*
*    WRITE: / pares_paletes-pal1,
*             pares_paletes-pal2,
*             pares_paletes-ref_com,
*             pares_paletes-n_ext,
*             pares_paletes-contidos.
*
*  ENDLOOP.

** Apagar entradas desnecessárias
  LOOP AT pares_paletes.
    MOVE-CORRESPONDING pares_paletes TO pares_paletes_aux.
    APPEND pares_paletes_aux.

    DELETE pares_paletes WHERE ( pal1 = pares_paletes-pal1 OR
                                 pal1 = pares_paletes-pal2 OR
                                 pal2 = pares_paletes-pal1 OR
                                 pal2 = pares_paletes-pal2 OR
                                 ref_com = 0 ).
  ENDLOOP.

** Verificar se faltam paletes que não formaram pares
  LOOP AT rota_zwm026.
    AT NEW n_pal_picking.
** verificar se a primeira palete existe
      READ TABLE pares_paletes_aux
                 WITH KEY pal1 = rota_zwm026-n_pal_picking.
      IF sy-subrc <> 0.
** verificar se a segunda palete existe
        READ TABLE pares_paletes_aux
                   WITH KEY pal2 = rota_zwm026-n_pal_picking.
        IF sy-subrc <> 0.
** Acrescentar entrada na tabela dos pares
          CLEAR pares_paletes_aux.
          pares_paletes_aux-pal1 = rota_zwm026-n_pal_picking.
          APPEND pares_paletes_aux.
        ENDIF.
      ENDIF.
    ENDAT.
  ENDLOOP.


** Para as paletes sem par vamos calcular as agregações
  DATA : save_index LIKE sy-tabix.
  CLEAR : save_index, pares_paletes.

  LOOP AT pares_paletes_aux WHERE pal2 IS INITIAL.
    save_index = sy-tabix.
   SELECT * FROM zwm026 INTO CORRESPONDING FIELDS OF TABLE palete_unica
            WHERE n_pal_picking = pares_paletes_aux-pal1.

    IF sy-subrc = 0.
** Material com posição fixa mais distante do ínicio
** das rotas de picking
      SORT palete_unica BY sorlp DESCENDING.
      READ TABLE palete_unica INDEX 1.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING pares_paletes_aux TO pares_paletes.
        pares_paletes-sorlp = palete_unica-sorlp.
      ENDIF.

      APPEND pares_paletes.
      CLEAR : palete_unica.
      REFRESH : palete_unica.
    ENDIF.

  ENDLOOP.


  SORT pares_paletes BY sorlp ASCENDING.
** Realizar pares
  DATA : contador TYPE i VALUE 1,
         n_pal_picking1 LIKE zwm026-n_pal_picking,
         n_pal_picking2 LIKE zwm026-n_pal_picking,
         n_times TYPE i,
         n_entradas TYPE i.

  CLEAR : n_pal_picking1,
          n_pal_picking2,
          n_times,
          n_entradas.

  DESCRIBE TABLE pares_paletes LINES n_entradas.
  n_times = n_entradas / 2.
*  n_times = ( n_entradas MOD 2 ) + n_times.

  DO n_times TIMES.

    READ TABLE pares_paletes INDEX contador.
    IF sy-subrc = 0.
      n_pal_picking1 = pares_paletes-pal1.
    ENDIF.
    contador = contador + 1.

    READ TABLE pares_paletes INDEX contador.
    IF sy-subrc = 0.
      n_pal_picking2 = pares_paletes-pal1.
    ELSE.
** Colocar entrada q não tem par no fim
      DELETE pares_paletes_aux WHERE pal1 = n_pal_picking1.
      CLEAR pares_paletes_aux.
      pares_paletes_aux-pal1 = n_pal_picking1.
      APPEND pares_paletes_aux.
      EXIT.
    ENDIF.
    contador = contador + 1.

** Seleccionar entrada da tabela principal
    READ TABLE pares_paletes_aux WITH KEY pal1 = n_pal_picking1.
    IF sy-subrc = 0.
      pares_paletes_aux-pal2 = n_pal_picking2.
      APPEND pares_paletes_aux.
    ENDIF.

    DELETE pares_paletes_aux WHERE ( ( pal1 = n_pal_picking2 OR
                                    pal1 = n_pal_picking1 ) AND
                                    pal2 IS INITIAL ).

    CLEAR : n_pal_picking1,n_pal_picking2.
  ENDDO.

*  WRITE : /.
*  WRITE : /.
*
*  LOOP AT pares_paletes_aux.
*
*    WRITE: / pares_paletes_aux-pal1,
*             pares_paletes_aux-pal2,
*             pares_paletes_aux-ref_com,
*             pares_paletes_aux-n_ext,
*             pares_paletes_aux-contidos.
*
*  ENDLOOP.
ENDFUNCTION.
