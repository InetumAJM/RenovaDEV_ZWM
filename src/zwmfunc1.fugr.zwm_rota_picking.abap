FUNCTION zwm_rota_picking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      L_ZWM026 STRUCTURE  ZWM026
*"      PARES_PALETES_AUX STRUCTURE  ZWMPARES_PALETES
*"----------------------------------------------------------------------
  CONSTANTS: lc_num_max_recor TYPE i VALUE 4,"Dependente de Field Symbols com lty_pares_paletes, não alterar
             lc_max_pal_rec   TYPE i VALUE 2." Numero total de paletes em recorrido.
***********************************************************************
  TYPES: BEGIN OF lty_rota_zwm026.
          INCLUDE STRUCTURE zwm026.
  TYPES:  n_ext       TYPE lfimg, "número de extracções
          sorlp_min   TYPE lagp_sorlp,
          sorlp_max   TYPE lagp_sorlp.
  TYPES: END OF lty_rota_zwm026.

  TYPES: BEGIN OF lty_pares_paletes,"Dependente de Field Symbols com lc_num_max_recor, não alterar
          pal1        TYPE exidv,
          pal2        TYPE exidv,
          pal3        TYPE exidv,
          pal4        TYPE exidv,
          ref_com     TYPE i,
          pal_picking TYPE zwn_pal_picking,
          sorlp_min   TYPE lagp_sorlp,
          sorlp_max   TYPE lagp_sorlp,
         END OF lty_pares_paletes.

***********************************************************************
  DATA: lt_rota_zwm026       TYPE TABLE OF lty_rota_zwm026,
        lt_rota_zwm026_h_aux TYPE TABLE OF lty_rota_zwm026,
        lt_rota_zwm026_h     TYPE TABLE OF lty_rota_zwm026,
        lt_rota_zwm026_h2    TYPE TABLE OF lty_rota_zwm026,
        lt_pares_paletes     TYPE TABLE OF lty_pares_paletes,
        lt_pares_paletes_new TYPE TABLE OF lty_pares_paletes.

  DATA: ls_rota_zwm026        TYPE lty_rota_zwm026,
        ls_rota_zwm026_2      TYPE lty_rota_zwm026,
        ls_rota_zwm026_h      TYPE lty_rota_zwm026,
        ls_rota_zwm026_h2     TYPE lty_rota_zwm026,
        ls_pares_paletes      TYPE lty_pares_paletes,
        ls_pares_paletes2     TYPE lty_pares_paletes,
        ls_pares_paletes_test TYPE lty_pares_paletes.

  DATA: lv_total_ext    TYPE lfimg,
        lv_combine      TYPE i,
        lv_cobine_back  TYPE i,
        lv_col_num      TYPE i,
        lv_name         TYPE c LENGTH 50,
        lv_name2        TYPE c LENGTH 50,
        lv_index        TYPE syindex,
        lv_tabix        TYPE sytabix,
        lv_tabix2       TYPE sytabix,
        lv_index_c      TYPE c LENGTH 4,
        lv_where        TYPE string,
        lv_skip         TYPE flag,
        lv_max_pal_type TYPE zwn_pal_picking,
        lv_break        TYPE flag,
        lv_lines        TYPE sytabix.

  FIELD-SYMBOLS: <ls_pares_paletes> TYPE lty_pares_paletes,
                 <ls_rota_zwm026_h> TYPE lty_rota_zwm026,
                 <ls_rota_zwm026>   TYPE lty_rota_zwm026,
                 <lv_pal_col>       TYPE exidv,
                 <lv_pal_col2>      TYPE exidv,
                 <lv_pal_col_back>  TYPE exidv.


** Converte para tabela interna
************************************************************************
  LOOP AT l_zwm026.
    MOVE-CORRESPONDING l_zwm026 TO ls_rota_zwm026.

    APPEND ls_rota_zwm026 TO lt_rota_zwm026.
  ENDLOOP.

** Numero de Extrações
***********************************************************************
  lt_rota_zwm026_h = lt_rota_zwm026.
  DELETE lt_rota_zwm026_h WHERE pal_picking = 0.

  IF lt_rota_zwm026_h IS INITIAL.
    lt_rota_zwm026_h = lt_rota_zwm026.
    SORT lt_rota_zwm026_h BY n_pal_picking.
    DELETE ADJACENT DUPLICATES FROM lt_rota_zwm026_h COMPARING n_pal_picking.

    LOOP AT lt_rota_zwm026_h ASSIGNING <ls_rota_zwm026_h>.
      <ls_rota_zwm026_h>-pal_picking = 1.
    ENDLOOP.
  ELSE.
    SORT lt_rota_zwm026_h BY n_pal_picking.
    DELETE ADJACENT DUPLICATES FROM lt_rota_zwm026_h COMPARING n_pal_picking.
  ENDIF.

  LOOP AT lt_rota_zwm026_h ASSIGNING <ls_rota_zwm026_h>.

    READ TABLE lt_rota_zwm026
      WITH KEY n_pal_picking = <ls_rota_zwm026_h>-n_pal_picking
      BINARY SEARCH
      TRANSPORTING NO FIELDS.
    LOOP AT lt_rota_zwm026 INTO ls_rota_zwm026 FROM sy-tabix.
      IF ls_rota_zwm026-n_pal_picking <> <ls_rota_zwm026_h>-n_pal_picking.
        EXIT.
      ENDIF.

      IF NOT ls_rota_zwm026-pal_picking IS INITIAL.
        <ls_rota_zwm026_h>-pal_picking = ls_rota_zwm026-pal_picking.
      ENDIF.


      IF <ls_rota_zwm026_h>-sorlp_min IS INITIAL.
        <ls_rota_zwm026_h>-sorlp_min = ls_rota_zwm026-sorlp.
      ELSEIF ls_rota_zwm026-sorlp < <ls_rota_zwm026_h>-sorlp_min.
        <ls_rota_zwm026_h>-sorlp_min = ls_rota_zwm026-sorlp.
      ENDIF.

      IF <ls_rota_zwm026_h>-sorlp_max IS INITIAL.
        <ls_rota_zwm026_h>-sorlp_max = ls_rota_zwm026-sorlp.
      ELSEIF ls_rota_zwm026-sorlp > <ls_rota_zwm026_h>-sorlp_max.
        <ls_rota_zwm026_h>-sorlp_max = ls_rota_zwm026-sorlp.
      ENDIF.

      <ls_rota_zwm026_h>-n_ext = <ls_rota_zwm026_h>-n_ext + ls_rota_zwm026-quantidade.

    ENDLOOP.
  ENDLOOP.

  LOOP AT lt_rota_zwm026 ASSIGNING <ls_rota_zwm026>.
    READ TABLE lt_rota_zwm026_h
          INTO ls_rota_zwm026_h
          WITH KEY n_pal_picking = <ls_rota_zwm026>-n_pal_picking
          BINARY SEARCH.

    <ls_rota_zwm026>-n_ext = ls_rota_zwm026_h-n_ext.
    <ls_rota_zwm026>-pal_picking = ls_rota_zwm026_h-pal_picking.
  ENDLOOP.


  SORT lt_rota_zwm026 BY n_ext DESCENDING n_pal_picking ASCENDING.


** Calcula todas as combinações possiveis
***********************************************************************
  lv_combine = lc_num_max_recor.

  DESCRIBE TABLE lt_rota_zwm026_h LINES lv_lines.

  IF lv_lines < lv_combine.
    lv_combine = lv_lines.
  ENDIF.

  DO.
    lv_combine = lv_combine - 1.
    IF lv_combine EQ 0.
      EXIT.
    ENDIF.

    LOOP AT lt_rota_zwm026_h INTO ls_rota_zwm026_h.

      lt_rota_zwm026_h2 = lt_rota_zwm026_h.
      DELETE lt_rota_zwm026_h2 WHERE n_pal_picking = ls_rota_zwm026_h-n_pal_picking.

      IF lt_rota_zwm026_h2 IS INITIAL.
        EXIT.
      ENDIF.

      DO.
        CLEAR ls_pares_paletes.

        IF lt_rota_zwm026_h2 IS INITIAL.
          EXIT.
        ENDIF.

        ls_pares_paletes-pal1 = ls_rota_zwm026_h-n_pal_picking.
        ls_pares_paletes-pal_picking = ls_rota_zwm026_h-pal_picking.

        DO lv_combine TIMES.
          lv_col_num = sy-index + 1.

          CLEAR ls_rota_zwm026_h2.
          READ TABLE lt_rota_zwm026_h2
                INTO ls_rota_zwm026_h2
                INDEX 1.

          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          DELETE lt_rota_zwm026_h2 INDEX 1.

          ASSIGN COMPONENT lv_col_num
              OF STRUCTURE ls_pares_paletes
                        TO <lv_pal_col>.
          CHECK <lv_pal_col> IS ASSIGNED.

          <lv_pal_col> = ls_rota_zwm026_h2-n_pal_picking.
          ls_pares_paletes-pal_picking = ls_pares_paletes-pal_picking + ls_rota_zwm026_h2-pal_picking.

*-->      Valida Materias Comuns
          lv_cobine_back = lv_col_num - 1.

          DO lv_cobine_back TIMES.
            ASSIGN COMPONENT sy-index
                OF STRUCTURE ls_pares_paletes
                TO <lv_pal_col_back>.
            CHECK <lv_pal_col_back> IS ASSIGNED.

            LOOP AT lt_rota_zwm026 INTO ls_rota_zwm026 WHERE n_pal_picking = <lv_pal_col>.

              IF ls_pares_paletes-sorlp_min IS INITIAL.
                ls_pares_paletes-sorlp_min = ls_rota_zwm026-sorlp.
              ELSEIF ls_rota_zwm026-sorlp < ls_pares_paletes-sorlp_min.
                ls_pares_paletes-sorlp_min = ls_rota_zwm026-sorlp.
              ENDIF.

              IF ls_pares_paletes-sorlp_max IS INITIAL.
                ls_pares_paletes-sorlp_max = ls_rota_zwm026-sorlp.
              ELSEIF ls_rota_zwm026-sorlp > ls_pares_paletes-sorlp_max.
                ls_pares_paletes-sorlp_max = ls_rota_zwm026-sorlp.
              ENDIF.

              LOOP AT lt_rota_zwm026 INTO ls_rota_zwm026_2 WHERE n_pal_picking = <lv_pal_col_back>.

                IF ls_rota_zwm026_2-sorlp < ls_pares_paletes-sorlp_min.
                  ls_pares_paletes-sorlp_min = ls_rota_zwm026_2-sorlp.
                ENDIF.

                IF ls_rota_zwm026_2-sorlp > ls_pares_paletes-sorlp_max.
                  ls_pares_paletes-sorlp_max = ls_rota_zwm026_2-sorlp.
                ENDIF.

                CHECK ls_rota_zwm026_2-material EQ ls_rota_zwm026-material.


                ls_pares_paletes-ref_com = ls_pares_paletes-ref_com + 1.
              ENDLOOP.
            ENDLOOP.

          ENDDO.
        ENDDO.
        APPEND ls_pares_paletes TO lt_pares_paletes.

      ENDDO.
    ENDLOOP.
  ENDDO.

** Escolhe Melhores Recorridos
***********************************************************************
  DELETE lt_pares_paletes WHERE pal_picking > lc_max_pal_rec.
  DELETE lt_pares_paletes WHERE ref_com EQ 0.
  SORT lt_pares_paletes BY ref_com DESCENDING pal_picking ASCENDING.

  LOOP AT lt_pares_paletes INTO ls_pares_paletes.
    lv_tabix = sy-tabix.

    lv_tabix = lv_tabix + 1.

    DO lc_num_max_recor TIMES.
      lv_index_c = sy-index.

      ASSIGN COMPONENT sy-index
          OF STRUCTURE ls_pares_paletes
                    TO <lv_pal_col>.
      CHECK <lv_pal_col> IS ASSIGNED.

      CHECK NOT <lv_pal_col> IS INITIAL.


      LOOP AT lt_pares_paletes INTO ls_pares_paletes2 FROM lv_tabix.
        lv_tabix2 = sy-tabix.

        DO lc_num_max_recor TIMES.
          ASSIGN COMPONENT sy-index
              OF STRUCTURE ls_pares_paletes2
                        TO <lv_pal_col2>.

          CHECK <lv_pal_col2> IS ASSIGNED.
          CHECK NOT <lv_pal_col2> IS INITIAL.

          CHECK <lv_pal_col> EQ <lv_pal_col2>.

          DELETE lt_pares_paletes INDEX lv_tabix2.
          EXIT.
        ENDDO.
      ENDLOOP.
    ENDDO.
  ENDLOOP.

  lt_rota_zwm026_h_aux = lt_rota_zwm026_h.

** Apaga Linhas Completas
***********************************************************************
  LOOP AT lt_pares_paletes INTO ls_pares_paletes.
    DO lc_num_max_recor TIMES.
      lv_index_c = sy-index.

      ASSIGN COMPONENT sy-index
          OF STRUCTURE ls_pares_paletes
                    TO <lv_pal_col>.
      CHECK <lv_pal_col> IS ASSIGNED.
      CHECK NOT <lv_pal_col> IS INITIAL.

      DELETE lt_rota_zwm026_h_aux WHERE n_pal_picking = <lv_pal_col>.
    ENDDO.
  ENDLOOP.

** Processa Linhas sem Par - Dentro do mesmo percurso
***********************************************************************
  LOOP AT lt_pares_paletes ASSIGNING <ls_pares_paletes> WHERE pal_picking < lc_max_pal_rec.
    DO lc_num_max_recor TIMES.
      lv_index_c = sy-index.

      ASSIGN COMPONENT sy-index
          OF STRUCTURE <ls_pares_paletes>
                    TO <lv_pal_col>.
      CHECK <lv_pal_col> IS ASSIGNED.

      CHECK <lv_pal_col> IS INITIAL.

      lv_max_pal_type = lc_max_pal_rec - <ls_pares_paletes>-pal_picking.

      LOOP AT lt_rota_zwm026_h_aux INTO ls_rota_zwm026_h WHERE pal_picking <= lv_max_pal_type AND
                                                               (
                                                                 sorlp_min >= <ls_pares_paletes>-sorlp_min OR
                                                                 sorlp_max <= <ls_pares_paletes>-sorlp_max
                                                               ).

        DELETE lt_rota_zwm026_h_aux INDEX sy-tabix.

        <lv_pal_col> = ls_rota_zwm026_h-n_pal_picking.
        <ls_pares_paletes>-pal_picking = <ls_pares_paletes>-pal_picking + ls_rota_zwm026_h-pal_picking.
        EXIT.
      ENDLOOP.
    ENDDO.
  ENDLOOP.

** Apaga Linhas Completas
***********************************************************************
  LOOP AT lt_pares_paletes INTO ls_pares_paletes.
    DO lc_num_max_recor TIMES.
      lv_index_c = sy-index.

      ASSIGN COMPONENT sy-index
          OF STRUCTURE ls_pares_paletes
                    TO <lv_pal_col>.
      CHECK <lv_pal_col> IS ASSIGNED.
      CHECK NOT <lv_pal_col> IS INITIAL.

      DELETE lt_rota_zwm026_h_aux WHERE n_pal_picking = <lv_pal_col>.
    ENDDO.
  ENDLOOP.

** Insere Paletes Não Atribuidas
***********************************************************************
  SORT lt_rota_zwm026_h_aux BY pal_picking ASCENDING.

  LOOP AT lt_rota_zwm026_h_aux INTO ls_rota_zwm026_h.
    CLEAR: lv_break,
           ls_pares_paletes.

*--> Completa Espaços em Branco
    LOOP AT lt_pares_paletes ASSIGNING <ls_pares_paletes> WHERE pal_picking < lc_max_pal_rec.

      DO lc_num_max_recor TIMES.
        lv_index_c = sy-index.

        ASSIGN COMPONENT sy-index
            OF STRUCTURE <ls_pares_paletes>
                      TO <lv_pal_col>.
        CHECK <lv_pal_col> IS ASSIGNED.
        CHECK <lv_pal_col> IS INITIAL.

        lv_max_pal_type = lc_max_pal_rec - <ls_pares_paletes>-pal_picking.

        IF ls_rota_zwm026_h-pal_picking > lv_max_pal_type.
          EXIT.
        ENDIF.

        lv_break = abap_true.
        <lv_pal_col> = ls_rota_zwm026_h-n_pal_picking.
        <ls_pares_paletes>-pal_picking = <ls_pares_paletes>-pal_picking + ls_rota_zwm026_h-pal_picking.
        EXIT.
      ENDDO.

      IF lv_break EQ abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    CHECK lv_break EQ abap_false.

*--> Cria Nova Linha
    CLEAR ls_pares_paletes.

    ls_pares_paletes-pal1          = ls_rota_zwm026_h-n_pal_picking.
    ls_pares_paletes-pal_picking   = ls_rota_zwm026_h-pal_picking.
    APPEND ls_pares_paletes TO lt_pares_paletes.
  ENDLOOP.

** Ordena Por Tipo de Palete
***********************************************************************
  CLEAR lt_pares_paletes_new.

  LOOP AT lt_pares_paletes ASSIGNING <ls_pares_paletes>.
    CLEAR: lv_index, ls_pares_paletes.

    ls_pares_paletes = <ls_pares_paletes>.

*--> Limpa Antigos
    DO lc_num_max_recor TIMES.
      ASSIGN COMPONENT sy-index
          OF STRUCTURE ls_pares_paletes
                    TO <lv_pal_col>.

      CLEAR <lv_pal_col>.
    ENDDO.

*--> Adiciona Meias Paletes Primeiro
    DO lc_num_max_recor TIMES.

      ASSIGN COMPONENT sy-index
          OF STRUCTURE <ls_pares_paletes>
                    TO <lv_pal_col>.

      IF NOT <lv_pal_col> IS ASSIGNED OR
         <lv_pal_col> IS INITIAL.
        EXIT.
      ENDIF.

      READ TABLE lt_rota_zwm026_h
            INTO ls_rota_zwm026_h
            WITH KEY n_pal_picking = <lv_pal_col>.

      CHECK ls_rota_zwm026_h-pal_picking < 1.

      lv_index = lv_index + 1.

      ASSIGN COMPONENT lv_index
          OF STRUCTURE ls_pares_paletes
          TO <lv_pal_col2>.

      <lv_pal_col2> = <lv_pal_col>.
      CLEAR <lv_pal_col>.
    ENDDO.

*--> Adiciona Restantes
    DO lc_num_max_recor TIMES.

      IF lv_index > lc_num_max_recor.
        EXIT.
      ENDIF.

      ASSIGN COMPONENT sy-index
          OF STRUCTURE <ls_pares_paletes>
                    TO <lv_pal_col>.

      IF NOT <lv_pal_col> IS ASSIGNED.
        EXIT.
      ENDIF.

      IF <lv_pal_col> IS INITIAL.
        CONTINUE.
      ENDIF.

      lv_index = lv_index + 1.

      ASSIGN COMPONENT lv_index
          OF STRUCTURE ls_pares_paletes
          TO <lv_pal_col2>.

      <lv_pal_col2> = <lv_pal_col>.
      CLEAR <lv_pal_col>.
    ENDDO.

    APPEND ls_pares_paletes TO lt_pares_paletes_new.
  ENDLOOP.

  lt_pares_paletes = lt_pares_paletes_new.

** Exporta
***********************************************************************
  LOOP AT lt_pares_paletes INTO ls_pares_paletes.
    MOVE-CORRESPONDING ls_pares_paletes TO pares_paletes_aux.
    APPEND pares_paletes_aux.
  ENDLOOP.
ENDFUNCTION.
