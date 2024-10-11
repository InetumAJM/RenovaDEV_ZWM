
*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE status_0003 OUTPUT.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

  CLEAR ecran.

  IF to_tri IS INITIAL.
**CHAMA NOVA TAREFA PARA EXECUTAR
    CLEAR tipo_tri.

    IF xuser-lgnum IS INITIAL.
      PERFORM user_own_data.
    ENDIF.

* Quando um operario quer sair da pistola
    IF NOT f3_activo IS INITIAL.
      CLEAR : cursorfield.
      IF lrf_wkqu-devty(5) = '16X20'.
        SET SCREEN '0001'.
      ELSE.
        SET SCREEN '0002'.
      ENDIF.

      LEAVE SCREEN.
    ENDIF.


** Se não limpar fica com entradas antigas e não carrega
** as queues certas - qd se muda de equipamento
    CLEAR : tab_zwm010.
    REFRESH : tab_zwm010.

    IF tab_zwm010[] IS INITIAL.
*preenche tabela do equipamento
      SELECT * FROM zwm010 INTO TABLE tab_zwm010
         WHERE armazem = xuser-lgnum AND
               equipamento = equipamento_.
    ENDIF.

    REFRESH return_msg.
    CALL FUNCTION 'ZWM_GET_TO_TRI'
      EXPORTING
        armazem           = xuser-lgnum
        tamanho           = lrf_wkqu-devty(5)
      IMPORTING
        nova_to           = tab_zwm011
        tipo_queue        = tipo_tri
      TABLES
        l_zwm010          = tab_zwm010
        return_msg        = return_msg
      EXCEPTIONS
        no_equipment      = 1
        no_work_available = 2
        OTHERS            = 3.

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
            message_var2   = return_msg-msgv2.

        f3_activo = 'X'.
        SET SCREEN '0000'.LEAVE SCREEN.
      ENDIF.
    ELSE.
      CALL FUNCTION 'ZWM_CALL_TASK_TRI'
        EXPORTING
          armazem     = xuser-lgnum
          ecran       = ecran
          tab_zwm011  = tab_zwm011
          tipo_queue  = tipo_tri
          equipamento = equipamento_.

* Quando um operario quer sair da pistola
      IF NOT f3_activo IS INITIAL.
        CLEAR : cursorfield.
        IF lrf_wkqu-devty(5) = '16X20'.
          SET SCREEN '0001'.
        ELSE.
          SET SCREEN '0002'.
        ENDIF.

        LEAVE SCREEN.
      ENDIF.


    ENDIF.

  ELSE.
* Quando um operario quer sair da pistola
    IF NOT f3_activo IS INITIAL.
      CLEAR : cursorfield.
      IF lrf_wkqu-devty(5) = '16X20'.
        SET SCREEN '0001'.
      ELSE.
        SET SCREEN '0002'.
      ENDIF.

      LEAVE SCREEN.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0007  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0007 OUTPUT.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

  IF xuser-lgnum IS INITIAL.
    PERFORM user_own_data.
  ENDIF.

** Variável global que tem a TO - TO_RET
** Total de items na TO
*  SELECT COUNT(*) FROM LTAP INTO DE
*                  WHERE LGNUM = XUSER-LGNUM AND
*                        TANUM = TO_RET.
** Total de items na TO


* Quando um operario quer sair da pistola
  IF NOT f3_activo IS INITIAL.
    SET SCREEN '0000'.
*    CLEAR F3_ACTIVO.
    LEAVE SCREEN.
  ENDIF.


** Depois de inserido o pulmao fecha-se o campo da confirmação do pulmão
  IF NOT pulmao2 IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'PULMAO2'.
        screen-input = 0.
        MODIFY SCREEN.

        MOVE 'PED_COMPRA' TO cursorfield.
        SET CURSOR FIELD cursorfield.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0007  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.


  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

  IF xuser-lgnum IS INITIAL.
    PERFORM user_own_data.
  ENDIF.

  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs = xuser-lgnum.

** Validar Mesa no armazém automático (WCS)
  PERFORM check_mesa_aut_wcs.

** Quando um operario quer sair da pistola
  IF NOT f3_activo IS INITIAL.
    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.

** Colocar no ecra a posicao do pulmão qd a TO é vinda do pulmão
  IF bin_output_o(3) = 'PUL'.
    SELECT SINGLE posicao_pulmao FROM zwm013 INTO posicao_pulmao
                                 WHERE armazem = xuser-lgnum AND
                                       sscc = su1.
    IF sy-subrc = 0.
      WRITE posicao_pulmao TO posicao_pulmao RIGHT-JUSTIFIED.
    ELSE.
      CLEAR: posicao_pulmao, kober.
    ENDIF.
  ENDIF.

  IF  bin_output_d(3) = 'PUL'.
    LOOP AT SCREEN.
      IF screen-name = 'POSICAO_PULMAO2'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name = 'POSICAO_PULMAO2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF bin_output_d(3) = 'PLT' OR bin_output_d(3) = 'DCK' OR
     bin_output_d(3) = 'PUL' OR bin_output_d(3) = 'PLM'.
    txt_num_pal = 'Num. Pal. Carga'.
  ELSE.
    CLEAR: txt_num_pal, num_pal_carga.
  ENDIF.

  IF to_ret IS INITIAL.
**CHAMA NOVA TAREFA PARA EXECUTAR
    DATA: tipo_ret.
    CLEAR tipo_ret.

    CLEAR : tab_zwm010.
    REFRESH : tab_zwm010.

    IF tab_zwm010[] IS INITIAL.
*preenche tabela do equipamento
      SELECT * FROM zwm010 INTO TABLE tab_zwm010
         WHERE armazem = xuser-lgnum AND
               equipamento = equipamento_.
    ENDIF.
    REFRESH return_msg.

    CALL FUNCTION 'ZWM_GET_TO_RET'
      EXPORTING
        armazem           = xuser-lgnum
        tamanho           = lrf_wkqu-devty(5)
      IMPORTING
        nova_to           = tab_zwm011
        tipo_queue        = tipo_ret
      TABLES
        l_zwm010          = tab_zwm010
        return_msg        = return_msg
      EXCEPTIONS
        no_equipment      = 1
        no_work_available = 2
        OTHERS            = 3.

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

        CLEAR:to_ret,item_ret,su,su1,descricao,bin_origem,
        texto_processo,matnr,
        bin_destino,bin_destino1,store_destino,
        store_origem,bin_input,bin_output_d,bin_output_o,pulmao1,
        queue.

        SET SCREEN '0000'.LEAVE SCREEN.
      ENDIF.
    ENDIF.
*LIMPA os campos PARA TODOS OS ECRANS.
    CLEAR:to_ret,item_ret,su,su1,descricao,bin_origem,texto_processo,
          bin_destino,bin_destino1,store_destino,matnr,
          store_origem,bin_input,bin_output_d,bin_output_o,pulmao1,
          queue, bin_output_kober_d.

*preenche os campos PARA TODOS OS ECRANS.
    SELECT SINGLE * FROM ltap
    WHERE lgnum = tab_zwm011-armazem AND
          tanum = tab_zwm011-to_number AND
          tapos = tab_zwm011-to_item.

    IF sy-subrc = 0.
      SELECT SINGLE * FROM ltak
      WHERE lgnum = tab_zwm011-armazem AND
            tanum = tab_zwm011-to_number.

      MOVE ltak-queue TO queue.

      MOVE : ltap-nlenr TO su1,
             ltap-vlpla TO bin_origem,
             ltap-nlpla TO bin_destino1,
             ltap-lgnum TO armazem_ret,
             ltap-nltyp TO store_destino,
             ltap-vltyp TO store_origem,
             tab_zwm011-to_number TO to_ret,
             tab_zwm011-to_item TO item_ret.

      IF tab_zwm011-su IS NOT INITIAL.
        su1 = tab_zwm011-su.
      ENDIF.

      IF tab_zwm011-status = 'P'.

        MOVE su1 TO su.
        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.

*get material description
      CLEAR makt.
      SELECT SINGLE * FROM makt
      WHERE matnr = ltap-matnr AND
            spras = sy-langu.

      MOVE makt-maktx(20) TO descricao.
      matnr = ltap-matnr.
*overwrite values if mensula
      SELECT SINGLE * FROM zwm014
        WHERE armazem   = xuser-lgnum AND
              su        = ltap-nlenr AND
              estado    = 'X'.

      IF sy-subrc = 0.
*significa que existe uma mensula com referencia
*à SU da TO que estamos a processar

        IF tipo_ret = 'E'.
*é de entrada, logo destino = mensula
          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = 'MEN'
              lgpla = zwm014-mensula
            IMPORTING
              bin   = bin_output_d.

          IF posicao_pulmao IS INITIAL.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = store_origem
                lgpla = bin_origem
              IMPORTING
                bin   = bin_output_o.
          ELSEIF NOT posicao_pulmao IS INITIAL.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = 'PUL'
                lgpla = bin_origem
              IMPORTING
                bin   = bin_output_o.
          ENDIF.
        ELSEIF tipo_ret = 'S'.
*é de saida logo a origem é a mensula
          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = 'MEN'
              lgpla = zwm014-mensula
            IMPORTING
              bin   = bin_output_o.
        ENDIF.
      ELSE.
**significa que estamos a processar uma TO
**que nao passa por mensula

*bin origem
        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = store_origem
            lgpla = bin_origem
          IMPORTING
            bin   = bin_output_o.
*bin destino
        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = store_destino
            lgpla = bin_destino1
          IMPORTING
            bin   = bin_output_d.
      ENDIF.

** Validar Mesa no armazém automático (WCS)
      PERFORM check_mesa_aut_wcs.

    ENDIF.
  ELSE.             " TO preenchida - caso F3


  ENDIF.

*& Begin of Modification by Tiago Pateiro - ROFF @ 21.01.2016 10:03:07
  IF ltap-vltyp EQ 'BLK'.
    gv_show_charg = abap_true.
    gv_charg = ltap-charg.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'CHG'.
      IF gv_show_charg EQ abap_true.
        screen-invisible = '0'.
        screen-active    = '1'.
      ELSE.
        screen-invisible = '1'.
        screen-active    = '0'.
      ENDIF.
    ENDIF.

    IF su IS NOT INITIAL.
      IF screen-name = 'SU'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
*& End of Modification by Tiago Pateiro - ROFF @ 21.01.2016 10:03:07
ENDMODULE.                 " STATUS_0001  OUTPUT
