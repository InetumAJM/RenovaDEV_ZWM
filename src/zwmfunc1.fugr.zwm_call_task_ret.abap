FUNCTION zwm_call_task_ret.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"     REFERENCE(ECRAN) TYPE  CHAR4 OPTIONAL
*"     REFERENCE(TAB_ZWM011) LIKE  ZWM011 STRUCTURE  ZWM011
*"     REFERENCE(TIPO_QUEUE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(EQUIPAMENTO) TYPE  ZWM010-EQUIPAMENTO OPTIONAL
*"     REFERENCE(PRIMEIRA) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(ONLYONE) TYPE  FLAG OPTIONAL
*"----------------------------------------------------------------------
*FUNCAO QUE CHAMA ECRANS REFERENTES AO TIPO DE EQUIPAMENTO RETRACTIL
*E/OU OUTROS EQUIPAMENTOS QUE ESTEJAM ASSOCIADOS ÁS MESMAS QUEUES

  DATA : queue_producao        LIKE zwm001-valor,
         queue_descarga        LIKE zwm001-valor,
         queue_carga           LIKE zwm001-valor,
         queue_picking         LIKE zwm001-valor,
         queue_fabrica1        LIKE zwm001-valor,
         queue_saida_pkf       LIKE zwm001-valor,
         queue_saida_tri       LIKE zwm001-valor,
         queue_saida_dri       LIKE zwm001-valor,
         queue_picking_let     LIKE zwm001-valor,
         queue_pal_remontada   LIKE zwm001-valor,
         queue_s_pal_remontada LIKE zwm001-valor,
         queue_pick_dri        LIKE zwm001-valor,
         queue_pick_tri        LIKE zwm001-valor,
         queue_pick_int        LIKE zwm001-valor,
         queue_rep_pick        LIKE zwm001-valor,
         queue_rep_int         LIKE zwm001-valor,
         queue_dev_inc_p       LIKE zwm001-valor,
         queue_dev_inc_comp    LIKE zwm001-valor,
         queue_pck_pkl         LIKE zwm001-valor,
         queue_prb_pkl         LIKE zwm001-valor,
         queue_chd_elv         LIKE zwm001-valor,
         queue_abast_bpe       LIKE zwm001-valor,
         queue_tri_prm         LIKE zwm001-valor,
         queue_dri_prm         LIKE zwm001-valor,
         queue_dri_cpk         LIKE zwm001-valor,
         queue_pilha_vazia     LIKE zwm001-valor,
         queue_saida_spf       LIKE zwm001-valor,
         queue_saida_bpk       LIKE zwm001-valor,
         queue_ent_rej         LIKE zwm001-valor.

  DATA : st_dck         LIKE lagp-lgtyp,
         st_pul         LIKE lagp-lgtyp,
         st_men         LIKE lagp-lgtyp,
         st_902         LIKE lagp-lgtyp,
         porta_aux      LIKE zwm002-porta,
         paletes_pulmao TYPE i,
         lt_equip       TYPE STANDARD TABLE OF zwm011-equipamento,
         lv_ord_pul_on  TYPE flag,
         lv_queue       TYPE ltak-queue,
         ls_zwm077      TYPE zwm077,
         ls_zwm020      TYPE zwm020,
         ls_makt        TYPE makt,
         ls_vekp        TYPE vekp,
         ls_vepo        TYPE vepo.

  DATA t_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.

  CLEAR t_zwm028.
  REFRESH t_zwm028.

  CLEAR : queue_producao,
          queue_descarga,
          queue_carga,
          queue_picking,
          queue_fabrica1,
          queue_saida_pkf,
          queue_saida_tri,
          queue_saida_dri,
          queue_picking_let,
          queue_pal_remontada,
          queue_s_pal_remontada,
          queue_pick_dri,
          queue_pick_tri,
          queue_pick_int,
          queue_rep_pick,
          queue_dev_inc_p,
          queue_dev_inc_comp,
          queue_pck_pkl,
          queue_prb_pkl.

  DATA: lv_2step  TYPE flag,
        lv_2spart TYPE flag.

  CLEAR: f3_activo, gv_only_one.

  gv_only_one = onlyone.

  PERFORM user_own_data.

** Função que carrega tabelas de parametrização para memória
  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs = tab_zwm011-armazem.

** Dados de tabelas de parametrização
  CLEAR : st_men, st_dck, st_pul, st_902.

** Storage type das mensulas
  PERFORM get_parameter USING armazem
                          'ENTRADA_ARMAZEM'
                          'ST_MEN'
                           st_men.

** Storage type do pulmão
  PERFORM get_parameter USING armazem
                          'ENTRADA_ARMAZEM'
                          'ST_PUL'
                           st_pul.

** Storage type da porta
  PERFORM get_parameter USING armazem
                          'ENTRADA_ARMAZEM'
                          'ST_DCK'
                           st_dck.

** Storage type 902
  PERFORM get_parameter USING armazem
                          'ENTRADA_ARMAZEM'
                          'ST_902'
                           st_902.

  PERFORM get_parameter USING armazem
                          'TROCA_ORDEM_PULMAO'
                          'ACTIVA'
                           lv_ord_pul_on.

*LIMPA os campos PARA TODOS OS ECRANS.

  CLEAR: to_ret, item_ret, su, su1, descricao, bin_origem,matnr,
         texto_processo, bin_destino, bin_destino1, store_destino,
         store_origem, bin_input, bin_output_d, bin_output_o, pulmao1,
         queue, porta1, grupo, remessa, posicao_pulmao, posicao_pulmao1,
         posicao_pulmao2, cursorfield, tipo_palete, paletes_pulmao,
         bin_output_kober_d,kober,pallet_type, porta_destino.

  MOVE equipamento TO equipamento_.

  CLEAR: ltap, ltak.
*preenche os campos PARA TODOS OS ECRANS - referentes às TOs.
  SELECT SINGLE *
      FROM ltap
          WHERE lgnum = tab_zwm011-armazem
            AND tanum = tab_zwm011-to_number
            AND tapos = tab_zwm011-to_item.

  CHECK sy-subrc = 0.

  SELECT SINGLE *
      FROM ltak
          WHERE lgnum = tab_zwm011-armazem
            AND tanum = tab_zwm011-to_number.

  CHECK sy-subrc = 0.

  MOVE : ltak-queue           TO queue,
         ltap-nlenr           TO su1,
         ltap-vlpla           TO bin_origem,
         ltap-nlpla           TO bin_destino1,
         ltap-lgnum           TO armazem_ret,
         ltap-nltyp           TO store_destino,
         ltap-vltyp           TO store_origem,
         ltap-letyp           TO tipo_palete,
         tab_zwm011-to_number TO to_ret,
         tab_zwm011-to_item   TO item_ret,
         ltap-maktx(20)       TO descricao,
         ltap-matnr           TO matnr.

*& Begin of Modification by Tiago Pateiro - ROFF @ 21.01.2016 09:56:55
  CLEAR gv_show_charg.
  CLEAR gv_charg.

  FREE lt_equip[].

  SELECT valor
    FROM zwm001 INTO TABLE lt_equip[]
    WHERE armazem EQ tab_zwm011-armazem
      AND processo EQ 'ZWM006'
      AND parametro EQ 'EQUIPAMENTO'.

  SORT lt_equip[] BY table_line.
  DELETE ADJACENT DUPLICATES FROM lt_equip[] COMPARING table_line.

  READ TABLE lt_equip[] TRANSPORTING NO FIELDS
    WITH KEY table_line = tab_zwm011-equipamento
    BINARY SEARCH.
  IF sy-subrc EQ 0.
    SELECT valor UP TO 1 ROWS
      FROM zwm001 INTO gv_show_charg
      WHERE armazem EQ tab_zwm011-armazem
        AND processo EQ 'ZWM006'
        AND parametro EQ 'APRESENTA_CHARG'.
    ENDSELECT.
    IF gv_show_charg EQ abap_true.
      gv_charg = ltap-charg.
    ENDIF.
  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 21.01.2016 09:56:55

** Inicio - Pilha de Paletes Vazias
**********************************************************************
  PERFORM get_parameter
           USING armazem
                 'GESTAO_FILAS'
                 'FILA_PILHA_VAZIA'
                  queue_pilha_vazia.

  IF queue = queue_pilha_vazia.

    " Posição Origem
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    " Limpa SU e Posição destino
    CLEAR: su1,  bin_output_d.

    "Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      SELECT SINGLE * FROM ltap
      WHERE lgnum = tab_zwm011-armazem   AND
            tanum = tab_zwm011-to_number AND
            tapos = tab_zwm011-to_item   AND
            pquit = ''.
      IF sy-subrc = 0.
        MOVE : bin_origem TO su,
               bin_origem TO su1.

        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.
    ENDIF.

    " Destino da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.

  ENDIF.
** Fim - Pilha de Paletes Vazias

** Inicio - Chamada de Empilhador para mesa de saída Automático (WCS)
**********************************************************************
  PERFORM get_parameter
           USING armazem
                 'GESTAO_FILAS'
                 'FILA_CHAMADA_ELV'
                  queue_chd_elv.

  IF queue = queue_chd_elv.

    " Posição Origem
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    " Limpa SU e Posição destino
    CLEAR: su1,  bin_output_d.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.

  ENDIF.
** Fim - Chamada de Empilhador para mesa de saída Automático (WCS)

** INICIO - Fila Saida Drive-in e Trilateral para arrumação no PRM
**********************************************************************
  PERFORM get_parameter
        USING armazem
              'GESTAO_FILAS'
              'FILA_S_DRI_PAL_PRM'
              queue_dri_prm.

  PERFORM get_parameter
      USING armazem
            'GESTAO_FILAS'
            'FILA_S_TRI_PAL_PRM'
            queue_tri_prm.

  IF queue = queue_dri_prm OR queue = queue_tri_prm.

** Origem da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

** Para Drive In não podemos sugerir SU ...
    CLEAR : su1.

** Destino da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      SELECT SINGLE * FROM ltap
      WHERE lgnum = tab_zwm011-armazem AND
            tanum = tab_zwm011-to_number AND
            tapos = tab_zwm011-to_item AND
            pquit = ' '.
      IF sy-subrc = 0.
        MOVE : ltap-vlenr TO su,
               ltap-vlenr TO su1.

        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.
  ENDIF.
** FIM - Fila Saida Drive-in e Trilateral para arrumação no PRM

** INICIO - Fila Saida Drive-in para arrumação no Copacking
**********************************************************************
  PERFORM get_parameter
        USING armazem
              'GESTAO_FILAS'
              'FILA_S_DRI_CPK'
              queue_dri_cpk.

  IF queue = queue_dri_cpk.

** Origem da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

** Para Drive In não podemos sugerir SU ...
    CLEAR : su1.

** Destino da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      SELECT SINGLE * FROM ltap
      WHERE lgnum = tab_zwm011-armazem AND
            tanum = tab_zwm011-to_number AND
            tapos = tab_zwm011-to_item AND
            pquit = ' '.
      IF sy-subrc = 0.
        MOVE : ltap-vlenr TO su,
               ltap-vlenr TO su1.

        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.
  ENDIF.
** FIM - Fila Saida Drive-in e Trilateral para arrumação no PRM


** INICIO - Saída do automático para armazém convencional (WCS)
**********************************************************************
  SELECT SINGLE *
    FROM zwm077 INTO ls_zwm077
    WHERE lgnum = tab_zwm011-armazem
    AND   tanum = tab_zwm011-to_number.

  IF sy-subrc = 0.

    MOVE ltap-vlenr TO su1.

    IF su1 IS INITIAL.
      su1 = ltap-nlenr.
    ENDIF.

    IF su1 IS INITIAL.
      su1 = ltap-ablad.

      SELECT SINGLE *
        FROM vekp INTO ls_vekp
        WHERE exidv = su1.

      IF sy-subrc = 0.
        SELECT SINGLE *
          FROM vepo INTO ls_vepo
          WHERE venum = ls_vekp-venum.

        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM makt INTO ls_makt
            WHERE matnr = ls_vepo-matnr
            AND   spras = sy-langu.

          descricao = ls_makt-maktx.
          matnr     = ls_vepo-matnr.
        ENDIF.
      ENDIF.
    ENDIF.

    " Posição Origem
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    " Posição Destino
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_CARGA'
                  queue_carga.

** Validar se é Carga direta para a porta
    IF ltak-queue = queue_carga.

      CLEAR : porta_aux, porta_destino.

      MOVE bin_destino1+8(2) TO porta_aux.
      UNPACK porta_aux TO porta_aux.

      SELECT SINGLE pulmao_1 FROM zwm002 INTO bin_destino1
                             WHERE armazem = armazem AND
                                   porta = porta_aux.
      IF sy-subrc = 0.
        porta_destino = porta_aux.
      ENDIF.

      "  Descobrir qual foi o pulmao atribuido à porta
      CLEAR zwm028.
      SELECT SINGLE st_pul kober INTO (zwm028-st_pul,zwm028-kober)
          FROM zwm028
              WHERE lgnum = xuser-lgnum AND
                    remessa = ' ' AND
                    pulmao1 = bin_destino1 AND
                    ot = ltak-tanum.

      bin_output_kober_d = zwm028-kober.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = zwm028-st_pul
          lgpla = bin_destino1
        IMPORTING
          bin   = pulmao1.

      " Quando a TO não está confirmada parcialmente é para iniciar a CARGA  FISICA PARA A PORTA
      IF ltap-pvqui IS INITIAL.
        CLEAR pulmao2.
        IF lrf_wkqu-devty(5) = '16X20'.
          PERFORM call_screen USING primeira '0023'.
        ELSE.
          PERFORM call_screen USING primeira '0024'.
        ENDIF.

        " Quando a TO está confirmada parcialmente é para carregar as paletes para o carro
      ELSEIF NOT ltap-pvqui IS INITIAL.
        IF lrf_wkqu-devty(5) = '16X20'.
          PERFORM call_screen USING primeira '0025'.
        ELSE.
          PERFORM call_screen USING primeira '0026'.
        ENDIF.
      ENDIF.

** Validar se é Carga para Pulmão
    ELSE.

      " Validar se é palete picking
      SELECT SINGLE grupo remessa pallet_type INTO (zwm026-grupo, zwm026-remessa, zwm026-pallet_type)
          FROM zwm026
              WHERE armazem = ltak-lgnum AND
                    sscc    = su1.

      IF sy-subrc = 0.

        MOVE zwm026-grupo   TO grupo.
        MOVE zwm026-remessa TO remessa.

        IF zwm026-pallet_type EQ 'M'.
          pallet_type = 'MP'.
        ELSE.
          CLEAR pallet_type.
        ENDIF.

        " Validar se é uma expedição
      ELSE.
        SELECT SINGLE * FROM ltap
                WHERE lgnum = tab_zwm011-armazem AND
                      vlenr = su1 AND
                      pquit = ' '.
        IF sy-subrc = 0.
          SELECT SINGLE refnr vbeln betyp benum FROM ltak INTO (grupo, remessa, betyp, benum)
                              WHERE lgnum = tab_zwm011-armazem   AND
                                    tanum = tab_zwm011-to_number AND
                                    kquit = ' '.
          tipo_palete = ltap-letyp.

          IF remessa IS INITIAL AND betyp = 'L'.
            remessa = benum.
          ENDIF.

        ENDIF.
      ENDIF.

      " Valida se picking em 2 passos
      CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
        EXPORTING
          i_lgnum  = armazem
          i_refnr  = grupo
        IMPORTING
          e_2step  = lv_2step
          e_2spart = lv_2spart
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF lv_2step EQ abap_true AND
         lv_2spart EQ abap_false.
        remessa = gc_vbeln_2step_dummy.
      ENDIF.

      " Obter Pulmao destino
      CLEAR: zwm040.
      SELECT * FROM zwm040
      WHERE lgnum   EQ ltak-lgnum
        AND refnr   EQ grupo
        AND remessa EQ remessa.
        remessa = zwm040-id_servisan.
        EXIT.
      ENDSELECT.

      SELECT * INTO TABLE t_zwm028
          FROM zwm028
              WHERE lgnum = ltak-lgnum AND
                    refnr = grupo AND
                    remessa = ' '.
      IF sy-subrc = 0.
        CLEAR num_pal_carga.

        READ TABLE t_zwm028 INDEX 1.
        num_pal_carga = t_zwm028-total_paletes.

        " passa àrea de meio pulmão
        bin_output_kober_d = t_zwm028-kober.


        IF NOT t_zwm028-st_ppk IS INITIAL.

          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = t_zwm028-st_ppk
              lgpla = bin_destino1
            IMPORTING
              bin   = bin_output_d.

        ELSEIF NOT t_zwm028-st_dck IS INITIAL.

          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = t_zwm028-st_dck
              lgpla = t_zwm028-porta
            IMPORTING
              bin   = bin_output_d.

          MOVE t_zwm028-porta+8(2) TO porta_aux.
          UNPACK porta_aux TO porta_aux.

          porta_destino = porta_aux.

        ELSEIF NOT t_zwm028-st_pul IS INITIAL.

          CALL FUNCTION 'Z_WM_IS_SPECIAL_PICK_TRANS_OV'
            EXPORTING
              i_lgnum = ltak-lgnum
              i_refnr = grupo
              i_tknum = zwm028-transporte
            EXCEPTIONS
              error   = 1
              OTHERS  = 2.

          IF sy-subrc EQ 0.
            SELECT SINGLE * FROM zwm028
                            WHERE lgnum   = ltak-lgnum AND
                                  refnr   = grupo AND
                                  remessa = remessa.

          ELSE.
            SELECT SINGLE * FROM zwm028
                            WHERE lgnum   = ltak-lgnum AND
                                  refnr   = grupo AND
                                  remessa = ''.
          ENDIF.

          IF lv_ord_pul_on <> abap_true.
            paletes_pulmao = zwm028-paletes_pulmao + zwm028-posicao_ini_pul.
          ELSE.
            SELECT SINGLE * FROM zwm028
                            WHERE lgnum = ltak-lgnum AND
                                  refnr = grupo AND
                                  remessa = ''.

            paletes_pulmao = zwm028-paletes_pulmao + 1.
          ENDIF.

          IF zwm028-st_pul = 'PUL'.
            IF paletes_pulmao <= 17.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao1
                IMPORTING
                  bin   = bin_output_d.
            ELSEIF paletes_pulmao >= 18.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao2
                IMPORTING
                  bin   = bin_output_d.
            ENDIF.
          ELSEIF zwm028-st_pul = 'PLM'.
            IF paletes_pulmao <= 2.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao1
                IMPORTING
                  bin   = bin_output_d.
            ELSEIF paletes_pulmao >= 3.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao2
                IMPORTING
                  bin   = bin_output_d.
            ENDIF.
          ELSEIF zwm028-st_pul = 'PLT'.
            IF paletes_pulmao <= 34.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao1
                IMPORTING
                  bin   = bin_output_d.
            ELSEIF paletes_pulmao >= 35.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao2
                IMPORTING
                  bin   = bin_output_d.
            ENDIF.
          ELSEIF zwm028-st_pul = 'PUA'.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ENDIF.
      ENDIF.

      " Buffer de Paletes Picking
      IF store_destino = 'BPK'.
        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = store_destino
            lgpla = bin_destino1
          IMPORTING
            bin   = bin_output_d.

        " Chamada de Rejeição
      ELSEIF store_destino = 'CHD'.

        PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_ENT_REJ'
                queue_ent_rej.

        IF ltak-queue = queue_ent_rej.
          bin_output_d(3) = 'REJ'.
        ENDIF.

        " Palete de Rejeição
      ELSEIF  store_destino = 'REJ'.
        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = store_destino
            lgpla = bin_destino1
          IMPORTING
            bin   = bin_output_d.
      ENDIF.

      " Paletes Mensula
      CLEAR zwm014.
      SELECT SINGLE *
          FROM zwm014
              WHERE armazem =  armazem
                AND su      =  su1
                AND su      <> ' '
                AND estado  = 'X'.

      IF sy-subrc = 0.
        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = st_men
            lgpla = zwm014-mensula
          IMPORTING
            bin   = bin_output_d.
      ENDIF.

      " Pick da SU
      IF tab_zwm011-status = 'P' OR ltap-pvqui IS NOT INITIAL.
        IF su IS INITIAL.
          MOVE su1 TO su.
        ENDIF.

        MOVE 'BIN_INPUT' TO cursorfield.
      ELSE.
        CLEAR : su1.
      ENDIF.

      IF lrf_wkqu-devty(5) = '16X20'.
        PERFORM call_screen USING primeira '0001'.
      ELSE.
        PERFORM call_screen USING primeira '0002'.
      ENDIF.
    ENDIF.
  ENDIF.
** Fim - Saída do automático para armazém convencional


** INICIO - Fila saida picking acabado
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_PKF'
                queue_saida_pkf.

  PERFORM get_parameter
        USING armazem
              'GESTAO_FILAS'
              'FILA_SAIDA_SPF'
               queue_saida_spf.

  PERFORM get_parameter
    USING armazem
          'GESTAO_FILAS'
          'FILA_SAIDA_BPK'
          queue_saida_bpk.

  IF queue = queue_saida_pkf OR queue = queue_saida_spf OR queue = queue_saida_bpk.

** Bin de destino
    IF ltap-vlenr IS NOT INITIAL.
      ltak-lznum =  ltap-vlenr.
    ENDIF.

    SELECT SINGLE grupo remessa pallet_type INTO (zwm026-grupo, zwm026-remessa, zwm026-pallet_type)
        FROM zwm026
            WHERE armazem = ltak-lgnum AND
                  sscc = ltak-lznum.

    IF NOT zwm026-grupo IS INITIAL.
      MOVE zwm026-grupo TO grupo.
      MOVE zwm026-remessa TO remessa.

      IF zwm026-pallet_type EQ 'M'.
        pallet_type = 'MP'.
        pallet_type = 'MP'.
      ELSE.
        CLEAR pallet_type.
      ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 15:10:27
*  Motivo: Valida se picking em 2 passos
*--------------------------------------------------------------------*
      CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
        EXPORTING
          i_lgnum  = armazem
          i_refnr  = grupo
        IMPORTING
          e_2step  = lv_2step
          e_2spart = lv_2spart
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF lv_2step EQ abap_true AND
         lv_2spart EQ abap_false.
        remessa = gc_vbeln_2step_dummy.
      ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
      CLEAR: zwm040.
      SELECT * FROM zwm040
      WHERE lgnum   EQ ltak-lgnum
        AND refnr   EQ grupo
        AND remessa EQ remessa.
        remessa = zwm040-id_servisan.
        EXIT.
      ENDSELECT.

      SELECT * INTO TABLE t_zwm028
          FROM zwm028
              WHERE lgnum = ltak-lgnum AND
                    refnr = grupo AND
                    remessa = ' '.
      IF sy-subrc = 0.
        CLEAR num_pal_carga.

        READ TABLE t_zwm028 INDEX 1.
        num_pal_carga = t_zwm028-total_paletes.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 14:15:08
*  Motivo: passa àrea de meio pulmão
*--------------------------------------------------------------------*
        bin_output_kober_d = t_zwm028-kober.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

        IF NOT t_zwm028-st_ppk IS INITIAL.

          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = t_zwm028-st_ppk
              lgpla = bin_destino1
            IMPORTING
              bin   = bin_output_d.

        ELSEIF  t_zwm028-st_dck = 'DCK'.
          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = t_zwm028-st_dck
              lgpla = t_zwm028-porta
            IMPORTING
              bin   = bin_output_d.

          MOVE t_zwm028-porta+8(2) TO porta_aux.
          UNPACK porta_aux TO porta_aux.

          porta_destino = porta_aux.

        ELSEIF NOT t_zwm028-st_pul IS INITIAL.

          CALL FUNCTION 'Z_WM_IS_SPECIAL_PICK_TRANS_OV'
            EXPORTING
              i_lgnum = ltak-lgnum
              i_refnr = grupo
              i_tknum = zwm028-transporte
            EXCEPTIONS
              error   = 1
              OTHERS  = 2.

          IF sy-subrc EQ 0.
            SELECT SINGLE * FROM zwm028
                            WHERE lgnum = ltak-lgnum AND
                                  refnr = grupo AND
                                  remessa = remessa.

          ELSE.
            SELECT SINGLE * FROM zwm028
                            WHERE lgnum = ltak-lgnum AND
                                  refnr = grupo AND
                                  remessa = ''.
          ENDIF.

          IF lv_ord_pul_on <> abap_true.
            paletes_pulmao = zwm028-paletes_pulmao + zwm028-posicao_ini_pul.
          ELSE.
            SELECT SINGLE * FROM zwm028
                            WHERE lgnum = ltak-lgnum AND
                                  refnr = grupo AND
                                  remessa = ''.

            paletes_pulmao = zwm028-paletes_pulmao + 1.
          ENDIF.

          IF zwm028-st_pul = 'PUL'.
            IF paletes_pulmao <= 17.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao1
                IMPORTING
                  bin   = bin_output_d.
            ELSEIF paletes_pulmao >= 18.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao2
                IMPORTING
                  bin   = bin_output_d.
            ENDIF.
          ELSEIF zwm028-st_pul = 'PLM'.
            IF paletes_pulmao <= 2.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao1
                IMPORTING
                  bin   = bin_output_d.
            ELSEIF paletes_pulmao >= 3.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao2
                IMPORTING
                  bin   = bin_output_d.
            ENDIF.
          ELSEIF zwm028-st_pul = 'PLT'.
            IF paletes_pulmao <= 34.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao1
                IMPORTING
                  bin   = bin_output_d.
            ELSEIF paletes_pulmao >= 35.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_zwm028-st_pul
                  lgpla = t_zwm028-pulmao2
                IMPORTING
                  bin   = bin_output_d.
            ENDIF.
          ELSEIF zwm028-st_pul = 'PUA'.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    bin_output_o = 'PKF 000-000-01'.

** Arrumação no Buffer Paletes Picking
    IF store_destino = 'BPK'.
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = store_destino
          lgpla = bin_destino1
        IMPORTING
          bin   = bin_output_d.

** Saída do Buffer Paletes Picking
    ELSEIF ltap-vltyp = 'BPK'.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = store_origem
          lgpla = bin_origem
        IMPORTING
          bin   = bin_output_o.
    ENDIF.

    MOVE ltak-lznum TO su1.

    " por defeito o tipo de palete das paltes de picking é P6
    tipo_palete = 'P6'.

    " Pick da SU
    IF tab_zwm011-status = 'P' OR ltap-pvqui IS NOT INITIAL.
      IF su IS INITIAL.
        MOVE su1 TO su.
      ENDIF.

      MOVE 'BIN_INPUT' TO cursorfield.
    ELSE.
      CLEAR : su.
    ENDIF.

    IF ltap-vltyp = 'BPK'.

      IF lrf_wkqu-devty(5) = '16X20'.
        PERFORM call_screen USING primeira '0001'.
      ELSE.
        PERFORM call_screen USING primeira '0002'.
      ENDIF.

    ELSE.
      IF lrf_wkqu-devty(5) = '16X20'.
        PERFORM call_screen USING primeira '0021'.
      ELSE.
        PERFORM call_screen USING primeira '0022'.
      ENDIF.
    ENDIF.
  ENDIF.
** FIM - Fila saida picking acabado


** INICIO - Fila saida trilateral
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_TRI_MEN'
                queue_saida_tri.

  IF queue = queue_saida_tri.
** Descobrir qual a mensula em que se encontra a SU - ORIGEM
    SELECT SINGLE * FROM zwm014
                    WHERE su = ltak-lznum.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM zwm014
                 WHERE su_transito = ltak-lznum.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = st_men
          lgpla = zwm014-mensula
        IMPORTING
          bin   = bin_output_o.

    ELSEIF sy-subrc = 0.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = st_men
          lgpla = zwm014-mensula
        IMPORTING
          bin   = bin_output_o.
    ENDIF.
** SU vai no campo número adicional
    MOVE ltak-lznum TO su1.

** Destino
    SELECT SINGLE * FROM ltap
                    WHERE lgnum = tab_zwm011-armazem AND
                          vlenr = su1 AND
                          pquit = ' '.
    IF sy-subrc = 0.
      SELECT SINGLE refnr vbeln betyp benum queue FROM ltak INTO (grupo, remessa,betyp,benum,lv_queue)
                          WHERE lgnum = tab_zwm011-armazem AND
                                tanum = ltap-tanum AND
                                kquit = ' '.
      tipo_palete = ltap-letyp.
    ENDIF.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 15:10:27
*  Motivo: Valida se picking em 2 passos
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = armazem
        i_refnr  = grupo
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2step EQ abap_true AND
       lv_2spart EQ abap_false.
      remessa = gc_vbeln_2step_dummy.
    ELSEIF lv_2spart EQ abap_true AND
           betyp EQ 'L' AND
           remessa IS INITIAL.
      remessa = benum.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


    CLEAR: zwm040.
    SELECT * FROM zwm040
    WHERE lgnum   EQ tab_zwm011-armazem
      AND refnr   EQ grupo
      AND remessa EQ remessa.
      remessa = zwm040-id_servisan.
      EXIT.
    ENDSELECT.

    SELECT * INTO TABLE t_zwm028
        FROM zwm028
            WHERE lgnum = ltak-lgnum AND
                  refnr = grupo AND
                  remessa = ' '.

    IF sy-subrc = 0.
      CLEAR num_pal_carga.
      READ TABLE t_zwm028 INDEX 1.
      num_pal_carga = t_zwm028-total_paletes.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 14:15:08
*  Motivo: passa àrea de meio pulmão
*--------------------------------------------------------------------*
      bin_output_kober_d = t_zwm028-kober.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

      IF NOT t_zwm028-st_dck IS INITIAL.

        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = t_zwm028-st_dck
            lgpla = t_zwm028-porta
          IMPORTING
            bin   = bin_output_d.
      ELSEIF NOT t_zwm028-st_pul IS INITIAL.

        SELECT SINGLE * FROM zwm028
                        WHERE lgnum = ltak-lgnum AND
                              refnr = grupo AND
                              remessa = remessa.

        IF lv_ord_pul_on <> abap_true.
          paletes_pulmao = zwm028-paletes_pulmao + zwm028-posicao_ini_pul.
        ELSE.
          SELECT SINGLE * FROM zwm028
                          WHERE lgnum = ltak-lgnum AND
                                refnr = grupo AND
                                remessa = ''.

          paletes_pulmao = zwm028-paletes_pulmao + 1.
        ENDIF.

        IF zwm028-st_pul = 'PUL'.
          IF paletes_pulmao <= 17.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 18.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ELSEIF zwm028-st_pul = 'PLM'.
          IF paletes_pulmao <= 2.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 3.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ELSEIF zwm028-st_pul = 'PLT'.
          IF paletes_pulmao <= 34.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 35.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.

        ELSEIF zwm028-st_pul = 'PUA'.
          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = t_zwm028-st_pul
              lgpla = t_zwm028-pulmao1
            IMPORTING
              bin   = bin_output_d.
        ENDIF.
      ENDIF.

    ELSE.
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = ltap-nltyp
          lgpla = ltap-nlpla
        IMPORTING
          bin   = bin_output_d.

    ENDIF.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.

      " Palete Remontada para PRM (Arruma primeiro palete de cima)
      DO 1 TIMES.
        PERFORM get_parameter
              USING armazem
                    'GESTAO_FILAS'
                    'FILA_S_TRI_PAL_PRM'
                   queue_tri_prm.

        CHECK lv_queue = queue_tri_prm.

        SELECT SINGLE *
          FROM zwm020 INTO ls_zwm020
          WHERE armazem = armazem AND
              ( p1 = su1 OR p2 = su1 ).

        CHECK sy-subrc = 0.

        SELECT SINGLE *
          FROM ltap
          WHERE lgnum = armazem AND
                vlenr = ls_zwm020-p2 AND
                pquit = ' '.

        CHECK sy-subrc = 0.

        su1 = ls_zwm020-p2.

        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = ltap-nltyp
            lgpla = ltap-nlpla
          IMPORTING
            bin   = bin_output_d.
      ENDDO.

      MOVE su1 TO su.
      tipo_palete = ltap-letyp.

      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.
  ENDIF.
** FIM - Fila saida trilateral


** INICIO -Fila saida trilateral para o REP
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PICK_TRI'
                queue_pick_tri.

  IF queue = queue_pick_tri.
** Descobrir qual a mensula em que se encontra a SU - ORIGEM
    SELECT SINGLE * FROM zwm014
                    WHERE su = ltap-nlenr.

    IF sy-subrc <> 0.
      SELECT SINGLE * FROM zwm014
                   WHERE su_transito =  ltap-nlenr.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = st_men
          lgpla = zwm014-mensula
        IMPORTING
          bin   = bin_output_o.

    ELSEIF sy-subrc = 0.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = st_men
          lgpla = zwm014-mensula
        IMPORTING
          bin   = bin_output_o.
    ENDIF.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      MOVE su1 TO su.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.

  ENDIF.
** FIM -Fila saida trilateral para o REP

** INICIO - Fila saida DRIVE IN
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_SAIDA_DRI'
                queue_saida_dri.

  IF queue = queue_saida_dri.
** Origem da TO
**    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
**      EXPORTING
**        lgtyp = 'DRI'
**        lgpla = bin_origem
**      IMPORTING
**        bin   = bin_output_o.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

*& Begin of Modification by Tiago Pateiro - ROFF @ 30.12.2015 11:28:32
*/ Ajustar DRIVE-IN para França - Manter tipo de depósito ENT
    IF store_origem EQ 'ENT' AND armazem_ret EQ '150'.
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = store_origem
          lgpla = bin_origem
        IMPORTING
          bin   = bin_output_o.
    ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 30.12.2015 11:28:33

** Para Drive In não podemos sugerir SU ...
    CLEAR : su1.
** Destino da TO
    SELECT SINGLE refnr vbeln betyp benum FROM ltak INTO (grupo, remessa,betyp,benum)
                               WHERE lgnum = tab_zwm011-armazem AND
                                   tanum = tab_zwm011-to_number AND
                                     kquit = ' '.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 15:10:27
*  Motivo: Valida se picking em 2 passos
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = armazem
        i_refnr  = grupo
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2step  EQ abap_true AND
       lv_2spart EQ abap_false.
      remessa = gc_vbeln_2step_dummy.
    ELSEIF lv_2spart EQ abap_true AND
           betyp EQ 'L' AND
           remessa IS INITIAL.
      remessa = benum.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    CLEAR: zwm040.
    SELECT * FROM zwm040
    WHERE lgnum   EQ tab_zwm011-armazem
      AND refnr   EQ grupo
      AND remessa EQ remessa.
      remessa = zwm040-id_servisan.
      EXIT.
    ENDSELECT.

    SELECT * INTO TABLE t_zwm028
        FROM zwm028
            WHERE lgnum = ltak-lgnum AND
                  refnr = grupo AND
                  remessa = ' '.

    IF sy-subrc = 0.
      CLEAR num_pal_carga.

      READ TABLE t_zwm028 INDEX 1.
      num_pal_carga = t_zwm028-total_paletes.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 14:15:08
*  Motivo: passa àrea de meio pulmão
*--------------------------------------------------------------------*
      bin_output_kober_d = t_zwm028-kober.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

      IF NOT t_zwm028-st_dck IS INITIAL.

        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = t_zwm028-st_dck
            lgpla = t_zwm028-porta
          IMPORTING
            bin   = bin_output_d.

      ELSEIF NOT t_zwm028-st_pul IS INITIAL.

        SELECT SINGLE * FROM zwm028
                        WHERE lgnum = ltak-lgnum AND
                              refnr = grupo AND
                              remessa = remessa.

        IF lv_ord_pul_on <> abap_true.
          paletes_pulmao = zwm028-paletes_pulmao + zwm028-posicao_ini_pul.
        ELSE.
          SELECT SINGLE * FROM zwm028
                          WHERE lgnum = ltak-lgnum AND
                                refnr = grupo AND
                                remessa = ''.

          paletes_pulmao = zwm028-paletes_pulmao + 1.
        ENDIF.

        IF zwm028-st_pul = 'PUL'.
          IF paletes_pulmao <= 17.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 18.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ELSEIF zwm028-st_pul = 'PLM'.
          IF paletes_pulmao <= 2.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 3.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ELSEIF zwm028-st_pul = 'PLT'.
          IF paletes_pulmao <= 34.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 35.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.

        ELSEIF zwm028-st_pul = 'PUA'.
          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = t_zwm028-st_pul
              lgpla = t_zwm028-pulmao1
            IMPORTING
              bin   = bin_output_d.
        ENDIF.
*        IF paletes_pulmao <= 17.
*
*          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
*            EXPORTING
*              lgtyp = t_zwm028-st_pul
*              lgpla = t_zwm028-pulmao1
*            IMPORTING
*              bin   = bin_output_d.
*
*        ELSEIF paletes_pulmao >= 18.
*
*          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
*            EXPORTING
*              lgtyp = t_zwm028-st_pul
*              lgpla = t_zwm028-pulmao2
*            IMPORTING
*              bin   = bin_output_d.
*        ENDIF.
      ENDIF.
    ENDIF.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      IF tab_zwm011-su IS NOT INITIAL.
        su1 = tab_zwm011-su.
        su  = tab_zwm011-su.

      ELSE.
        SELECT SINGLE * FROM ltap
        WHERE lgnum = tab_zwm011-armazem AND
              tanum = tab_zwm011-to_number AND
              tapos = tab_zwm011-to_item AND
              pquit = ' '.
        IF sy-subrc = 0.
          MOVE : ltap-vlenr TO su,
                 ltap-vlenr TO su1.
        ENDIF.
      ENDIF.

      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.
  ELSE.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 11.10.2012 16:17:14
*  Motivo: Total de Paletes
*--------------------------------------------------------------------*
    DO 1 TIMES.
      CLEAR num_pal_carga.

      SELECT SINGLE refnr vbeln betyp benum FROM ltak INTO (grupo, remessa, betyp, benum)
                                            WHERE lgnum = tab_zwm011-armazem AND
                                                  tanum = tab_zwm011-to_number AND
                                                  kquit = ' '.
      CHECK grupo IS INITIAL.

      grupo = benum.
    ENDDO.

    SELECT * INTO TABLE t_zwm028
                   FROM zwm028
                   WHERE lgnum = ltak-lgnum AND
                         refnr = grupo.

    DELETE t_zwm028 WHERE remessa EQ ''.

    LOOP AT t_zwm028.
      num_pal_carga = num_pal_carga + t_zwm028-total_paletes.
    ENDLOOP.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
  ENDIF.
** FIM - Fila saida DRIVE IN

** INICIO - Fila Reabastecimento p/ Buffer Paletização Especial
**********************************************************************
  PERFORM get_parameter
        USING armazem
              'GESTAO_FILAS'
              'FILA_ABAST_BPE'
              queue_abast_bpe.

  IF queue = queue_abast_bpe.
** Origem da TO
**    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
**      EXPORTING
**        lgtyp = 'DRI'
**        lgpla = bin_origem
**      IMPORTING
**        bin   = bin_output_o.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

** Para Drive In não podemos sugerir SU ...
    CLEAR : su1.

** Destino da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      SELECT SINGLE * FROM ltap
      WHERE lgnum = tab_zwm011-armazem AND
            tanum = tab_zwm011-to_number AND
            tapos = tab_zwm011-to_item AND
            pquit = ' '.
      IF sy-subrc = 0.
        MOVE : ltap-vlenr TO su,
               ltap-vlenr TO su1.

        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.
  ENDIF.
** FIM - Fila Reabastecimento p/ Buffer Paletização Especial

** INICIO - Fila saida DRI para o REP
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PICK_DRI'
                queue_pick_dri.

  IF queue = queue_pick_dri.
** Origem da TO
**    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
**      EXPORTING
**        lgtyp = 'DRI'
**        lgpla = bin_origem
**      IMPORTING
**        bin   = bin_output_o.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

** Para Drive In não podemos sugerir SU ...
    CLEAR : su1.
** Destino da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      SELECT SINGLE * FROM ltap
      WHERE lgnum = tab_zwm011-armazem AND
            tanum = tab_zwm011-to_number AND
            tapos = tab_zwm011-to_item AND
            pquit = ' '.
      IF sy-subrc = 0.
        MOVE : ltap-vlenr TO su,
               ltap-vlenr TO su1.

        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.
  ENDIF.
** FIM - Fila saida DRI para o REP

** INICIO - Saida de Palete Remontada da Zona PRM
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_S_PAL_REMONTADA'
                queue_s_pal_remontada.

  IF queue = queue_s_pal_remontada.
** Origem da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = 'PRM'
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.
    MOVE : ltap-vlenr TO su1.

** Destino da TO
    SELECT SINGLE refnr vbeln betyp benum FROM ltak INTO (grupo, remessa, betyp,benum)
                                          WHERE lgnum = tab_zwm011-armazem AND
                                                tanum = tab_zwm011-to_number AND
                                                kquit = ' '.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 15:10:27
*  Motivo: Valida se picking em 2 passos
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = armazem
        i_refnr  = grupo
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2step  EQ abap_true AND
       lv_2spart EQ abap_false.
      remessa = gc_vbeln_2step_dummy.
    ELSEIF lv_2spart EQ abap_true AND
           betyp EQ 'L' AND
           remessa IS INITIAL.
      remessa = benum.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    CLEAR: zwm040.
    SELECT * FROM zwm040
    WHERE lgnum   EQ tab_zwm011-armazem
      AND refnr   EQ grupo
      AND remessa EQ remessa.
      remessa = zwm040-id_servisan.
      EXIT.
    ENDSELECT.

    SELECT * INTO TABLE t_zwm028
        FROM zwm028
            WHERE lgnum = ltak-lgnum AND
                  refnr = grupo AND
                  remessa = ' '.

    IF sy-subrc = 0.
      CLEAR num_pal_carga.

      READ TABLE t_zwm028 INDEX 1.
      num_pal_carga = t_zwm028-total_paletes.
      IF NOT t_zwm028-st_dck IS INITIAL.

        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = t_zwm028-st_dck
            lgpla = t_zwm028-porta
          IMPORTING
            bin   = bin_output_d.
      ELSEIF NOT t_zwm028-st_pul IS INITIAL.

        SELECT SINGLE * FROM zwm028
                        WHERE lgnum = ltak-lgnum AND
                              refnr = grupo AND
                              remessa = remessa.

        IF lv_ord_pul_on <> abap_true.
          paletes_pulmao = zwm028-paletes_pulmao + zwm028-posicao_ini_pul.
        ELSE.
          SELECT SINGLE * FROM zwm028
                          WHERE lgnum = ltak-lgnum AND
                                refnr = grupo AND
                                remessa = ''.

          paletes_pulmao = zwm028-paletes_pulmao + 1.
        ENDIF.

        IF zwm028-st_pul = 'PUL'.
          IF paletes_pulmao <= 17.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 18.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ELSEIF zwm028-st_pul = 'PLM'.
          IF paletes_pulmao <= 2.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 3.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ELSEIF zwm028-st_pul = 'PLT'.
          IF paletes_pulmao <= 34.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 35.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ELSEIF zwm028-st_pul = 'PUA'.
          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = t_zwm028-st_pul
              lgpla = t_zwm028-pulmao1
            IMPORTING
              bin   = bin_output_d.
        ENDIF.
*        IF paletes_pulmao <= 17.
*
*          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
*            EXPORTING
*              lgtyp = t_zwm028-st_pul
*              lgpla = t_zwm028-pulmao1
*            IMPORTING
*              bin   = bin_output_d.
*
*        ELSEIF paletes_pulmao >= 18.
*
*          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
*            EXPORTING
*              lgtyp = t_zwm028-st_pul
*              lgpla = t_zwm028-pulmao2
*            IMPORTING
*              bin   = bin_output_d.
*        ENDIF.
      ENDIF.
    ENDIF.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      SELECT SINGLE * FROM ltap
      WHERE lgnum = tab_zwm011-armazem AND
            tanum = tab_zwm011-to_number AND
            pquit = ' '.
      IF sy-subrc = 0.
        MOVE : ltap-vlenr TO su.
        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.
  ENDIF.
** FIM - Saida de Palete Remontada da Zona PRM


** INICIO - Palete Remontada que vai devolvida do Pulmao para PRM
**********************************************************************
** Fila devolucao da segunda palete remontada
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PAL_REMONTADA'
                queue_pal_remontada.

  IF queue = queue_pal_remontada.

    CLEAR bin_output_o.

    SELECT SINGLE *
        FROM ltap
           WHERE lgnum = tab_zwm011-armazem AND
                 tanum = tab_zwm011-to_number AND
                 tapos = tab_zwm011-to_item.

    su1 = ltap-vlenr.

    SELECT SINGLE *
        FROM ltak
            WHERE lgnum = ltap-lgnum AND
                  tanum = ltap-tanum.

    SELECT SINGLE *
        FROM zwm028
            WHERE lgnum = ltak-lgnum AND
                  refnr = ltak-benum.

    IF zwm028-st_pul = st_pul OR
       zwm028-st_pul = 'PLT' OR
       zwm028-st_pul = 'PLM'.

      SELECT SINGLE *
          FROM zwm020
              WHERE armazem = tab_zwm011-armazem AND
                    p2 = su1.

      SELECT SINGLE *
          FROM zwm013
              WHERE armazem = tab_zwm011-armazem AND
                    sscc = zwm020-p1.

      bin_output_o = zwm013-destino.

    ELSEIF zwm028-st_dck = st_dck.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = st_dck
          lgpla = zwm028-porta
        IMPORTING
          bin   = bin_output_o.
    ENDIF.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      MOVE su1 TO su.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.

  ENDIF.
** FIM - Palete Remontada que vai devolvida do Pulmao para PRM
** Fila devolucao da segunda palete remontada

** INICIO - Fila do REP para o Picking
**********************************************************************
  PERFORM get_parameter
           USING armazem
                 'GESTAO_FILAS'
                 'FILA_REP_PICK'
                 queue_rep_pick.

  IF queue = queue_rep_pick.
** bin origem
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    MOVE ltap-vlenr TO su1.
** bin destino
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
** temos de preencher o campo da SU e mudar o cursor
    IF tab_zwm011-status = 'P'.
      MOVE su1 TO su.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.

  ENDIF.
** FIM - Fila do REP para o Picking


** INICIO -  Fila do REP para o INT.
**********************************************************************
  PERFORM get_parameter
           USING armazem
                 'GESTAO_FILAS'
                 'FILA_REP_INT'
                 queue_rep_int.

  IF queue = queue_rep_int.
** bin origem
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    MOVE ltap-vlenr TO su1.

** bin destino
    CLEAR: mlgt.
    SELECT SINGLE * FROM mlgt
    WHERE matnr EQ ltap-matnr
      AND lgnum EQ ltap-lgnum
      AND lgtyp = 'PCK'.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = mlgt-lgtyp
        lgpla = mlgt-lgpla
      IMPORTING
        bin   = bin_output_d.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
** temos de preencher o campo da SU e mudar o cursor
    IF tab_zwm011-status = 'P'.
      MOVE su1 TO su.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.

  ENDIF.
** FIM -  Fila do REP para o INT.


** INICIO - Fila de Let Down
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PICKING_LETDOWN'
                queue_picking_let.

** AS TO da queue de picking nao passam pela tabela 13
  IF queue = queue_picking_let.
** bin origem
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.
** SU
    MOVE ltap-vlenr TO su1.
** bin destino
    SELECT SINGLE *
        FROM mlgt
            WHERE matnr = ltap-matnr AND
                  lgnum = ltap-lgnum AND
                  lgtyp = 'PCK'.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = mlgt-lgtyp
        lgpla = mlgt-lgpla
      IMPORTING
        bin   = bin_output_d.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
** temos de preencher o campo da SU e mudar o cursor
    IF tab_zwm011-status = 'P'.
      MOVE su1 TO su.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.

  ENDIF.
** FIM - Fila de Let Down


** INICIO - Fila do PRB para o INT
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PICK_INT'
                queue_pick_int.

  IF queue = queue_pick_int.
    CLEAR: bin_output_o.
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    CLEAR: lqua, bin_output_d.
    SELECT * FROM lqua
    WHERE lgnum EQ armazem
      AND matnr EQ ltap-matnr
      AND charg EQ ltap-charg
      AND lgtyp EQ 'PKB'.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = 'PKB'
          lgpla = lqua-lgpla
        IMPORTING
          bin   = bin_output_d.
      EXIT.
    ENDSELECT.

    IF bin_output_d+3(11) IS INITIAL.
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = 'PKB'
          lgpla = '000-000-00'
        IMPORTING
          bin   = bin_output_d.
    ENDIF.

    CLEAR: bin_output_r.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_r.

    MOVE ltap-vlenr TO su1.
** quantidade
    IF ltak-lznum IS INITIAL.
      MOVE ltap-vsola TO quant_out.
    ELSE.
      MOVE ltak-lznum TO quant_out.
    ENDIF.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.

      MOVE su1 TO su.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = 'PKB'
          lgpla = lqua-lgpla
        IMPORTING
          bin   = bin_input.

      MOVE 'QUANT_IN' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0027'.
    ELSE.
      PERFORM call_screen USING primeira '0028'.
    ENDIF.

  ENDIF.
** FIM - Fila do PRB para o INT

** INICIO - Fila do PCK para o PKL
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PICK_PKL'
                queue_pck_pkl.

  IF queue = queue_pck_pkl.
    CLEAR: bin_output_o.
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

** quantidade
    MOVE ltap-vsola TO quant_out.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.

      MOVE su1 TO su.

*      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
*        EXPORTING
*          lgtyp = store_destino
*          lgpla = bin_destino1
*        IMPORTING
*          bin   = bin_input.
      MOVE ltap-vsola TO quant_in.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0032'.
    ELSE.
      PERFORM call_screen USING primeira '0033'.
    ENDIF.

  ENDIF.
** FIM - Fila do PCK para o PKL

** INICIO - Fila do PRB para o PKL
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PRB_PKL'
                queue_prb_pkl.

  IF queue = queue_prb_pkl.
    CLEAR: bin_output_o.
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

    CLEAR: bin_output_r.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_r.

    MOVE ltap-vlenr TO su1.
** quantidade
    MOVE ltap-vsola TO quant_out.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.

      MOVE su1 TO su.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = store_destino
          lgpla = bin_destino1
        IMPORTING
          bin   = bin_input.

      MOVE 'QUANT_IN' TO cursorfield.
*
*      MOVE ltap-vsola TO quant_in.
*      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0027'.
    ELSE.
      PERFORM call_screen USING primeira '0028'.
    ENDIF.

  ENDIF.
** FIM - Fila do PCK para o PKL

******** CARGAS DE CAMIAO ***********
** INICIO - Fila de Cargas
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_CARGA'
                queue_carga.

  IF ltak-queue = queue_carga.

    CLEAR : porta_aux, porta_destino.

*** Descobrir qual foi o pulmao atribuido à porta
    MOVE bin_destino1+8(2) TO porta_aux.
    UNPACK porta_aux TO porta_aux.

    SELECT SINGLE pulmao_1 FROM zwm002 INTO bin_destino1
                           WHERE armazem = armazem AND
                                 porta = porta_aux.
    IF sy-subrc = 0.
      porta_destino = porta_aux.
    ENDIF.


    CLEAR zwm028.
    SELECT SINGLE st_pul kober INTO (zwm028-st_pul,zwm028-kober)
        FROM zwm028
            WHERE lgnum = xuser-lgnum AND
                  remessa = ' ' AND
                  pulmao1 = bin_destino1 AND
                  ot = ltak-tanum.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 14:15:08
*  Motivo: passa àrea de meio pulmão
*--------------------------------------------------------------------*
    bin_output_kober_d = zwm028-kober.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

** para ecran cargas
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = zwm028-st_pul
        lgpla = bin_destino1
      IMPORTING
        bin   = pulmao1.

** Quando a TO não está confirmada parcialmente é para iniciar a CARGA
** FISICA PARA A PORTA
    IF ltap-pvqui IS INITIAL.
      CLEAR pulmao2.
      IF lrf_wkqu-devty(5) = '16X20'.
        PERFORM call_screen USING primeira '0023'.
      ELSE.
        PERFORM call_screen USING primeira '0024'.
      ENDIF.
** Quando a TO está confirmada parcialmente é para carregar as paletes
** para o carro
    ELSEIF NOT ltap-pvqui IS INITIAL.
      IF lrf_wkqu-devty(5) = '16X20'.
        PERFORM call_screen USING primeira '0025'.
      ELSE.
        PERFORM call_screen USING primeira '0026'.
      ENDIF.
    ENDIF.
  ENDIF.
** FIM - Fila de Cargas


********
******** DESCARGAS DE CAMIAO/DEVOLUÇÕES ***********
********

** INICIO -  Fila da Fábrica 1
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_FABRICA1'
                queue_fabrica1.

  IF ltak-queue = queue_fabrica1.

    CLEAR : porta_aux,
            porta1,
            pulmao1,
            pulmao2.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = st_dck
        lgpla = bin_origem
      IMPORTING
        bin   = porta1.


** Descobrir qual foi o pulmao atribuido à porta
    MOVE bin_destino1+8(2) TO porta_aux.
    UNPACK porta_aux TO porta_aux.
    SELECT SINGLE pulmao_1 FROM zwm002 INTO bin_destino1
                           WHERE armazem = armazem AND
                                 porta = porta_aux.

** para ecran descargas
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = st_pul
        lgpla = bin_destino1
      IMPORTING
        bin   = pulmao1.

** Quando a TO não está confirmada parcialmente é DESCARGA FISICA PARA O
** PULMAO
    IF ltap-pvqui IS INITIAL.
      IF lrf_wkqu-devty(5) = '16X20'.
        PERFORM call_screen USING primeira '0005'.
      ELSE.
        PERFORM call_screen USING primeira '0006'.
      ENDIF.
** Quando a TO está confirmada parcialmente é CONFERIR PULMÃO
    ELSEIF NOT ltap-pvqui IS INITIAL.
      IF lrf_wkqu-devty(5) = '16X20'.
        PERFORM call_screen USING primeira '0019'.
      ELSE.
        PERFORM call_screen USING primeira '0020'.
      ENDIF.
    ENDIF.
  ENDIF.
** FIM -  Fila da Fábrica 1


** INICIO - Fila de Descargas
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_DESCARGA'
                queue_descarga.

  IF ltak-queue = queue_descarga.
************Tarefa vinda da entrada de camiao(terceiros e/ou devoluções)

** Tipo de depósito das portas
    CLEAR : porta_aux.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = st_dck
        lgpla = bin_origem
      IMPORTING
        bin   = porta1.


** Descobrir qual foi o pulmao atribuido à porta
    MOVE bin_destino1+8(2) TO porta_aux.
    UNPACK porta_aux TO porta_aux.
    SELECT SINGLE pulmao_1 FROM zwm002 INTO bin_destino1
                           WHERE armazem = armazem AND
                                 porta = porta_aux.

** para ecran descargas
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = st_pul
        lgpla = bin_destino1
      IMPORTING
        bin   = pulmao1.
** Quando a TO não está confirmada parcialmente é DESCARGA FISICA PARA O
** PULMAO
    IF ltap-pvqui IS INITIAL.
      IF lrf_wkqu-devty(5) = '16X20'.
        PERFORM call_screen USING primeira '0005'.
      ELSE.
        PERFORM call_screen USING primeira '0006'.
      ENDIF.
** Quando a TO está confirmada parcialmente é CONFERÊNCIA DE PALETES
    ELSEIF NOT ltap-pvqui IS INITIAL.
      IF lrf_wkqu-devty(5) = '16X20'.
        PERFORM call_screen USING primeira '0007'.
      ELSE.
        PERFORM call_screen USING primeira '0008'.
      ENDIF.
    ENDIF.
  ENDIF.

** Inicio - Cross Docking
**********************************************************************

*    PERFORM get_parameter
*            USING armazem
*                  'GESTAO_FILAS'
*                  'FILA_SAIDA_DRI'
*                  queue_saida_dri.

  IF queue = 'QUEUECRD'.
    IF bin_origem <> '000-000-01' AND
       bin_origem <> '000-000-02'.
      store_origem = 'PUL'.
    ENDIF.
** Origem da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    MOVE ltap-vlenr TO su1.
** Destino da TO
    SELECT SINGLE refnr vbeln betyp
                  benum             FROM ltak INTO (grupo, remessa,betyp,benum)
                                    WHERE lgnum = tab_zwm011-armazem AND
                                          tanum = tab_zwm011-to_number AND
                                          kquit = ' '.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 15:10:27
*  Motivo: Valida se picking em 2 passos
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = armazem
        i_refnr  = grupo
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2step  EQ abap_true AND
       lv_2spart EQ abap_false.
      remessa = gc_vbeln_2step_dummy.
    ELSEIF lv_2spart EQ abap_true AND
           betyp EQ 'L' AND
           remessa IS INITIAL.
      remessa = benum.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

** Remessas Servisan
    CLEAR: zwm040.
    SELECT * FROM zwm040
    WHERE lgnum   EQ tab_zwm011-armazem
      AND refnr   EQ grupo
      AND remessa EQ remessa.
      remessa = zwm040-id_servisan.
      EXIT.
    ENDSELECT.

    SELECT * INTO TABLE t_zwm028
        FROM zwm028
            WHERE lgnum = ltak-lgnum AND
                  refnr = grupo AND
                  remessa = ' '.

    IF sy-subrc = 0.
      CLEAR num_pal_carga.
      READ TABLE t_zwm028 INDEX 1.
      num_pal_carga = t_zwm028-total_paletes.
      IF NOT t_zwm028-st_dck IS INITIAL.
** DOCA
        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = t_zwm028-st_dck
            lgpla = t_zwm028-porta
          IMPORTING
            bin   = bin_output_d.
      ELSEIF NOT t_zwm028-st_pul IS INITIAL.

        SELECT SINGLE * FROM zwm028
                        WHERE lgnum = ltak-lgnum AND
                              refnr = grupo AND
                              remessa = remessa.

        IF lv_ord_pul_on <> abap_true.
          paletes_pulmao = zwm028-paletes_pulmao + zwm028-posicao_ini_pul.
        ELSE.
          SELECT SINGLE * FROM zwm028
                          WHERE lgnum = ltak-lgnum AND
                                refnr = grupo AND
                                remessa = ''.

          paletes_pulmao = zwm028-paletes_pulmao + 1.
        ENDIF.

        IF t_zwm028-st_pul = 'PUL'.
          IF paletes_pulmao <= 17.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 18.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ELSEIF t_zwm028-st_pul = 'PLM'.
          IF paletes_pulmao <= 2.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 3.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ELSEIF t_zwm028-st_pul = 'PLT'.
          IF paletes_pulmao <= 34.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao1
              IMPORTING
                bin   = bin_output_d.
          ELSEIF paletes_pulmao >= 35.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_zwm028-st_pul
                lgpla = t_zwm028-pulmao2
              IMPORTING
                bin   = bin_output_d.
          ENDIF.
        ENDIF.
*        IF paletes_pulmao <= 17.
*          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
*            EXPORTING
*              lgtyp = t_zwm028-st_pul
*              lgpla = t_zwm028-pulmao1
*            IMPORTING
*              bin   = bin_output_d.
*        ELSEIF paletes_pulmao >= 18.
*          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
*            EXPORTING
*              lgtyp = t_zwm028-st_pul
*              lgpla = t_zwm028-pulmao2
*            IMPORTING
*              bin   = bin_output_d.
*        ENDIF.
      ENDIF.
    ENDIF.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
    IF tab_zwm011-status = 'P'.
      SELECT SINGLE * FROM ltap
      WHERE lgnum = tab_zwm011-armazem AND
            tanum = tab_zwm011-to_number AND
            tapos = tab_zwm011-to_item AND
            pquit = ' '.
      IF sy-subrc = 0.
        MOVE : ltap-vlenr TO su,
               ltap-vlenr TO su1.

        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.
  ENDIF.
** Fim - Cross Docking

** Inicio - Fila do REP para PCK/PKR
**********************************************************************
  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PICKING'
                queue_picking.

  IF queue = queue_picking.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    MOVE ltap-vlenr TO su1.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.

* Se for uma TO confirmada parcialmente é porque existiu um crash ....
* temos de preencher o campo da SU e mudar o cursor
    IF tab_zwm011-status = 'P'.
      IF su IS INITIAL.
        MOVE su1 TO su.
      ENDIF.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.

  ENDIF.
** Fim - Fila do REP para PCK/PKR

** Inicio - Fila Entradas PRO/PUL para TRI/DRI/REP
**********************************************************************
  PERFORM get_parameter
           USING armazem
                 'GESTAO_FILAS'
                 'FILA_PRODUCAO'
                 queue_producao.

  PERFORM get_parameter
          USING armazem
                'GESTAO_FILAS'
                'FILA_PRODUCAO'
                queue_producao.

  IF queue = queue_producao.

    IF ltap-vltyp = st_902.
      store_origem = st_pul.
    ENDIF.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    CLEAR zwm014.
    SELECT SINGLE *
        FROM zwm014
            WHERE armazem =  armazem
              AND su      = ltap-nlenr
              AND su      <> ' '
              AND estado  = 'X'.

    IF sy-subrc = 0.
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = st_men
          lgpla = zwm014-mensula
        IMPORTING
          bin   = bin_output_d.
    ELSE.
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = store_destino
          lgpla = bin_destino1
        IMPORTING
          bin   = bin_output_d.
    ENDIF.

** Se for uma TO confirmada parcialmente é porque existiu um crash ....
** temos de preencher o campo da SU e mudar o cursor
    IF tab_zwm011-status = 'P'.
      IF su IS INITIAL.
        MOVE su1 TO su.
      ENDIF.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.

  ENDIF.
** Fim - Fila Entradas PRO/PUL para TRI/DRI/REP

** Inicio - Queue Devoluções Paletes Completas
**********************************************************************
  PERFORM get_parameter
        USING armazem
              'GESTAO_FILAS'
              'FILA_DEV_INC'
              queue_dev_inc_comp.

  IF queue =  queue_dev_inc_comp.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

    SELECT SINGLE * FROM zwm014
      WHERE armazem   =  armazem AND
            su        = ltap-nlenr AND
            su        <> ' ' AND
            estado    = 'X'.

    IF sy-subrc = 0.
** significa que existe uma mensula com referencia
** à SU da TO que estamos a processar

** é de entrada, logo destino = mensula
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = st_men
          lgpla = zwm014-mensula
        IMPORTING
          bin   = bin_output_d.

    ELSE.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = store_destino
          lgpla = bin_destino1
        IMPORTING
          bin   = bin_output_d.
    ENDIF.
* Se for uma TO confirmada parcialmente é porque existiu um crash ....
* temos de preencher o campo da SU e mudar o cursor
    IF tab_zwm011-status = 'P'.
      IF su IS INITIAL.
        MOVE su1 TO su.
      ENDIF.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

    IF lrf_wkqu-devty(5) = '16X20'.
      PERFORM call_screen USING primeira '0001'.
    ELSE.
      PERFORM call_screen USING primeira '0002'.
    ENDIF.
  ENDIF.

** Fim - Queue Devoluções Paletes Completas

** Inicio - Queue Devolucoes Incompletas
**********************************************************************
  PERFORM get_parameter
        USING armazem
              'GESTAO_FILAS'
              'FILA_DEV_INC_P'
              queue_dev_inc_p.

  IF queue =  queue_dev_inc_p.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_origem_o.

    sscc1 = ltak-lznum.

    IF tab_zwm011-status = 'P'.
      IF su IS INITIAL.
        MOVE ltak-lznum TO sscc2.
        MOVE ltap-maktx TO desc.
        MOVE ltap-vsolm TO qtd.
        MOVE ltap-meins TO uni.
        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = ltap-nltyp
            lgpla = ltap-nlpla
          IMPORTING
            bin   = bin_dest1.
      ENDIF.
      MOVE 'BIN_DEST2' TO cursorfield.
    ENDIF.

    PERFORM call_screen USING primeira '0029'.

  ENDIF.
** Fim - Queue Devolucoes Incompletas

ENDFUNCTION.
