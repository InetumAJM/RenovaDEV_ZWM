*&---------------------------------------------------------------------*
*& Report  ZWMREP0019                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0019 MESSAGE-ID zwmmsg001.

TYPE-POOLS: abap.

INCLUDE: rlmobinc.

TABLES: zwm001, lqua, zwm010, mlgt, zwm026.

DATA : BEGIN OF ti_zwm001 OCCURS 0.
         INCLUDE STRUCTURE zwm001.
       DATA: END OF ti_zwm001.

*dados gerais
DATA: ok_code_0001 LIKE sy-ucomm,
      ok_code_0003 LIKE sy-ucomm,
      ok_code_0005 LIKE sy-ucomm,
      ok_code_0007 LIKE sy-ucomm,
      ok_code_0009 LIKE sy-ucomm.
DATA: equipamento TYPE zwm010-equipamento.
DATA: text TYPE  bdcmsgcoll-msgv1.
DATA: cursorfield(50),
      return_msg TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: tab_zwm010  LIKE zwm010 OCCURS 0 WITH HEADER LINE,
      tab_zwm026  LIKE zwm026 OCCURS 0 WITH HEADER LINE,
      itab_zwm026 LIKE zwm_s026 OCCURS 0 WITH HEADER LINE.

DATA wa_zwm026 LIKE zwm026.

DATA: ecran(4).

DATA: tab_zwm011        LIKE zwm011,
      valor             TYPE zwm001-valor,
      valor_pal1        TYPE zwm001-valor,
      valor_pal2        TYPE zwm001-valor,
      valor_pal3        TYPE zwm001-valor,
      valor_pal4        TYPE zwm001-valor,
      equipamento_pic   TYPE zwm001-valor,
      n_paletes         TYPE i,
      resposta,
      user_assignado(1).

** Variáveis para 3 e 4 ecrã

DATA: BEGIN OF scr0009,

        tit1 TYPE c LENGTH 20,
        pos1 TYPE c LENGTH 20,
        tp1  TYPE c LENGTH 20,

        tit2 TYPE c LENGTH 20,
        pos2 TYPE c LENGTH 20,
        tp2  TYPE c LENGTH 20,

      END OF scr0009.

FIELD-SYMBOLS: <gv_0009_sscc1>          TYPE any,
               <gv_0009_paltyp1>        TYPE any,
               <gv_0009_pal_destino1>   TYPE any,
               <gv_0009_pack_material1> TYPE any,

               <gv_0009_sscc2>          TYPE any,
               <gv_0009_paltyp2>        TYPE any,
               <gv_0009_pal_destino2>   TYPE any,
               <gv_0009_pack_material2> TYPE any.

DATA : ind1            TYPE sytabix,
       ind2            TYPE sytabix,
       sscc1(20),
       sscc2(20),
       sscc3(20),
       sscc4(20),
       ptyp1(2),
       ptyp2(2),
       ptyp3(2),
       ptyp4(2),
       sscc_1(20),
       sscc_2(20),
       sscc_3(20),
       sscc_4(20),
       pal_destino1    LIKE zwm026-pal_destino,
       pal_destino2    LIKE zwm026-pal_destino,
       pal_destino3    LIKE zwm026-pal_destino,
       pal_destino4    LIKE zwm026-pal_destino,
       pal_destino_a1  LIKE zwm026-pal_destino,
       pal_destino_a2  LIKE zwm026-pal_destino,
       pal_destino_a3  LIKE zwm026-pal_destino,
       pal_destino_a4  LIKE zwm026-pal_destino,
       pack_material_1 LIKE mara-matnr,
       pack_material_2 LIKE mara-matnr,
       pack_material_3 LIKE mara-matnr,
       pack_material_4 LIKE mara-matnr,
       gv_caixa        TYPE c.

DATA t_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.

** Variáveis para 5 e 6 ecrã
DATA : pos_origem_1(14),
       pos_origem_2(14),
       material         LIKE mara-matnr,
       descricao(20),
       descricao2(20),
       qtd              LIKE mseg-menge,
       uni              LIKE mara-meins,
       pal_destino_1    LIKE zwm026-pal_destino,
       pal_destino_2    LIKE zwm026-pal_destino,
       pal_type         TYPE c LENGTH 2,
       pal_type2        TYPE c LENGTH 2,
       to               LIKE ltak-tanum,
       to_number        TYPE tanum,
       lote             LIKE ltap-charg,
       refnr            TYPE lvs_refnr,
       ean11            TYPE marm-ean11,
       i_sscc           LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

** Variáveis para 7 e 8 ecrã
DATA : pos_destino_1(14),
       pos_destino_2(14).

START-OF-SELECTION.
  message_lang = sy-langu.
  PERFORM user_own_data.

  CLEAR equipamento.
  IF lrf_wkqu-devty(5) = '16X20'.
    CALL SCREEN '0001'.
  ELSE.
    CALL SCREEN '0002'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'ZRF_PIC'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CLEAR: equipamento,ok_code_0001.

  SET SCREEN '0000'.
  LEAVE SCREEN.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  DATA: lv_num          TYPE i,
        lv_num_c        TYPE c LENGTH 5,
        lv_name         TYPE c LENGTH 50,
        lv_message_mpal TYPE bdc_vtext1,
        lv_message_v1   TYPE bdc_vtext1,
        lv_message_v2   TYPE bdc_vtext1.

  DATA: ls_zwm028 TYPE zwm028,
        ls_zwm060 TYPE zwm060.

  FIELD-SYMBOLS: <lv_target> TYPE any.

  DATA: tipo.
  CLEAR tipo.

  CASE ok_code_0001.

    WHEN 'NEXT'.
      IF NOT equipamento IS INITIAL.
** Verificar se existem Recorridos Pendentes - Se sim assignar recorrido

        CALL FUNCTION 'ZWM_GET_TO_RECORRIDO'
          EXPORTING
            armazem           = xuser-lgnum
          IMPORTING
            n_paletes         = n_paletes
            user_assignado    = user_assignado
          TABLES
            l_zwm010          = tab_zwm010
            return_msg        = return_msg
            l_zwm026          = itab_zwm026
          EXCEPTIONS
            no_work_available = 1
            no_equipment      = 2
            OTHERS            = 3.
        IF sy-subrc <> 0.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '103'.

          CLEAR : equipamento,tab_zwm010,tab_zwm026,
                  n_paletes,return_msg.
          REFRESH : tab_zwm010,tab_zwm026,return_msg.
          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0001'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0002'.LEAVE SCREEN.
          ENDIF.

        ELSE.
** Existem recorridos para executar

          FREE: tab_zwm026.
          CLEAR: tab_zwm026.

          CLEAR: ptyp1, ptyp2,
                 ptyp3, ptyp4,
                 lv_num.

          SORT itab_zwm026 BY pal_destino ASCENDING.

          LOOP AT itab_zwm026.

            PERFORM set_mpal_info.

            MOVE-CORRESPONDING itab_zwm026 TO tab_zwm026.
            APPEND tab_zwm026.
          ENDLOOP.

          FREE: itab_zwm026.
          CLEAR: itab_zwm026.

          IF  NOT user_assignado IS INITIAL.
            IF lrf_wkqu-devty(5) = '16X20'.
              SET SCREEN '0005'.LEAVE SCREEN.
            ELSE.
              SET SCREEN '0006'.LEAVE SCREEN.
            ENDIF.
          ENDIF.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Pedro F. Lopes <<ROFF>>
*  Data: 24.02.2015 11:38:27
*  Motivo: Regras por Cliente
*--------------------------------------------------------------------*
*          select single * from zwm028
*                          into ls_zwm028
*                          where lgnum   = tab_zwm026-armazem and
*                                refnr   = tab_zwm026-grupo   and
*                                remessa = tab_zwm026-remessa.
*
*          if ls_zwm028 is not initial.
*
*            select single * from zwm060
*              into ls_zwm060
*              where kunnr = ls_zwm028-emissor.
*
*            if ls_zwm060 is not initial.
*
*              clear lv_message_v1.
*              lv_message_v1 = ls_zwm060-message.
*
*              " Mensagem Parametrizada por Cliente
*              call function 'YWM_MESSAGE_SCREEN'
*                exporting
*                  message_id     = 'ZWM001'
*                  message_lang   = sy-langu
*                  message_type   = 'E'
*                  message_number = '000'
*                  message_var1   = lv_message_v1
*                importing
*                  ret_code       = returncode.
*
*            endif.
*
*          endif.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Pedro F. Lopes <<ROFF>>
*--------------------------------------------------------------------*


** 1 - Ecrã para informar o operário de qual
** a qtd de paletes q tem de carregar

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 03.08.2012 12:41:19
*  Motivo: Mensagem de Palete
*--------------------------------------------------------------------*
          PERFORM send_to_pal_message CHANGING returncode.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*





          CASE n_paletes.
            WHEN '1'.

              LOOP AT tab_zwm026.
                CHECK tab_zwm026-pal_destino = 'PICKING 02'.
                tab_zwm026-pal_destino = 'PICKING 01'.
                MODIFY tab_zwm026.
                UPDATE zwm026 FROM tab_zwm026.
                COMMIT WORK.
              ENDLOOP.

*              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                EXPORTING
*                  message_id     = 'ZWM001'
*                  message_lang   = sy-langu
*                  message_type   = 'W'
*                  message_number = '030'
*                  message_var1   = lv_message_mpal
*                IMPORTING
*                  ret_code       = returncode.
              IF returncode = 'C'.
                CLEAR equipamento.
** Actualização da BD limpando o user que vai processar o recorrido
                READ TABLE tab_zwm026 WITH KEY estado = 'P'.
                IF sy-subrc <> 0.
                  IF NOT tab_zwm026[] IS INITIAL.

                    LOOP AT tab_zwm026.
                      CLEAR: tab_zwm026-user_name, tab_zwm026-estado.
                      MODIFY tab_zwm026.
                      UPDATE zwm026 FROM tab_zwm026.
                      COMMIT WORK AND WAIT.
                    ENDLOOP.

                  ENDIF.
                ENDIF.

                IF lrf_wkqu-devty(5) = '16X20'.
                  SET SCREEN '0001'.LEAVE SCREEN.
                ELSE.
                  SET SCREEN '0002'.LEAVE SCREEN.
                ENDIF.
              ENDIF.


            WHEN '2'.
**           	São precisas mais que uma paletes para executar o recorrido de picking !


*              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                EXPORTING
*                  message_id     = 'ZWM001'
*                  message_lang   = sy-langu
*                  message_type   = 'W'
*                  message_number = '021'
*                  message_var1   = lv_message_mpal
*                IMPORTING
*                  ret_code       = returncode.
              IF returncode = 'C'.
                CLEAR equipamento.
** Actualização da BD limpando o user que vai processar o recorrido
                READ TABLE tab_zwm026 WITH KEY estado = 'P'.
                IF sy-subrc <> 0.

                  IF NOT tab_zwm026[] IS INITIAL.
                    LOOP AT tab_zwm026.
                      CLEAR: tab_zwm026-user_name, tab_zwm026-estado.
                      MODIFY tab_zwm026.
                      UPDATE zwm026 FROM tab_zwm026.
                      COMMIT WORK AND WAIT.
                    ENDLOOP.

                  ENDIF.
                ENDIF.

                IF lrf_wkqu-devty(5) = '16X20'.
                  SET SCREEN '0001'.LEAVE SCREEN.
                ELSE.
                  SET SCREEN '0002'.LEAVE SCREEN.
                ENDIF.
              ENDIF.
            WHEN OTHERS.
*--> Mais que 2
*              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                EXPORTING
*                  message_id     = 'ZWM001'
*                  message_lang   = sy-langu
*                  message_type   = 'W'
*                  message_number = '021'
*                  message_var1   = lv_message_mpal
*                IMPORTING
*                  ret_code       = returncode.
              IF returncode = 'C'.
                CLEAR equipamento.
** Actualização da BD limpando o user que vai processar o recorrido
                READ TABLE tab_zwm026 WITH KEY estado = 'P'.
                IF sy-subrc <> 0.

                  IF NOT tab_zwm026[] IS INITIAL.
                    LOOP AT tab_zwm026.
                      CLEAR: tab_zwm026-user_name, tab_zwm026-estado.
                      MODIFY tab_zwm026.
                      UPDATE zwm026 FROM tab_zwm026.
                      COMMIT WORK AND WAIT.
                    ENDLOOP.
                  ENDIF.
                ENDIF.

                IF lrf_wkqu-devty(5) = '16X20'.
                  SET SCREEN '0001'.LEAVE SCREEN.
                ELSE.
                  SET SCREEN '0002'.LEAVE SCREEN.
                ENDIF.
              ENDIF.
          ENDCASE.

          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0003'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0004'.LEAVE SCREEN.
          ENDIF.

        ENDIF.

      ELSE.
*Obrigatoria a introdução do equipamento
        CLEAR text.
        WRITE equipamento TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '030'
            message_var1   = text.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_EQUIPAMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_equipamento INPUT.

  CLEAR : tab_zwm010.
  REFRESH : tab_zwm010.

  IF NOT equipamento IS INITIAL.

** Verificar se é um equipamento para realização
** dos recorridos de picking
    PERFORM get_parameter USING xuser-lgnum
                               'GERAL'
                               'EQUIPAMENTO_PIC'
                                valor.
    WRITE valor TO equipamento_pic LEFT-JUSTIFIED.

    IF equipamento <> equipamento_pic.
      CLEAR text.
      WRITE equipamento TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '102'
          message_var1   = text.

      CLEAR equipamento.
      IF lrf_wkqu-devty(5) = '16X20'.
        SET SCREEN '0001'.LEAVE SCREEN.
      ELSE.
        SET SCREEN '0002'.LEAVE SCREEN.
      ENDIF.
    ENDIF.

** Carregamento de dados de parametrização relativos
** ao equipamento
    SELECT * FROM zwm010 INTO TABLE tab_zwm010
    WHERE armazem = xuser-lgnum AND
          equipamento = equipamento.

    IF sy-subrc <> 0.
*equipamento invalido
      CLEAR text.
      WRITE equipamento TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '031'
          message_var1   = text.

      CLEAR equipamento.
      IF lrf_wkqu-devty(5) = '16X20'.
        SET SCREEN '0001'.LEAVE SCREEN.
      ELSE.
        SET SCREEN '0002'.LEAVE SCREEN.
      ENDIF.

    ENDIF.
  ELSE.
*Obrigatoria a introdução do equipamento
    CLEAR text.
    WRITE equipamento TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '030'
        message_var1   = text.

    CLEAR equipamento.
    IF lrf_wkqu-devty(5) = '16X20'.
      SET SCREEN '0001'.LEAVE SCREEN.
    ELSE.
      SET SCREEN '0002'.LEAVE SCREEN.
    ENDIF.

  ENDIF.

ENDMODULE.                 " CHECK_EQUIPAMENTO  INPUT

*****************************************************
*
*  FORM GET_PARAMETER
*
*****************************************************
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

  IF ti_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = xuser-lgnum
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
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.
  SET PF-STATUS 'ZRF_PIC'.
  SET CURSOR FIELD cursorfield.

** Os campos do ecrã para a segunda palete
** ficam fechados
*  IF n_paletes = 1.
  LOOP AT SCREEN.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 05.06.2012 11:53:03
*  Motivo: Desactivação de Campos
*--------------------------------------------------------------------*
    IF screen-name(4) = 'SSCC'.
      lv_num_c = screen-name+4(1).
      lv_num = lv_num_c.

      IF lv_num > n_paletes.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name(11) = 'PAL_DESTINO'.
      lv_num_c = screen-name+11(1).
      lv_num = lv_num_c.

      IF lv_num > n_paletes.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

*      IF screen-name = 'SSCC2'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*      IF screen-name = 'PAL_DESTINO2'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
  ENDLOOP.
*  ENDIF.

ENDMODULE.                 " STATUS_0003  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0009  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0009 OUTPUT.
  DATA: lv_text       TYPE c LENGTH 20,
        lv_page       TYPE i,
        lv_page_index TYPE i,
        lv_data_index TYPE i,
        lv_full       TYPE c LENGTH 1,
        lv_numc2      TYPE c LENGTH 5,
        lv_paletes_f  TYPE i.


  FIELD-SYMBOLS: <lv_target_sscc>     TYPE any,
                 <lv_target_pal_dest> TYPE any,
                 <lv_target_pack_mat> TYPE any,
                 <lv_target_paltype>  TYPE any.

  SET PF-STATUS 'ZRF_PIC'.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 05.06.2012 12:06:05
*  Motivo: Assign de Variaveis de Dados
*--------------------------------------------------------------------*
  lv_page = ceil( n_paletes / 2 ).

  lv_paletes_f = n_paletes.

  CLEAR lv_full.

  DO lv_page TIMES.
    lv_page_index = sy-index.

    IF lv_page_index EQ 1.
      lv_page_index = lv_page_index - 1.
    ENDIF.

    DO 2 TIMES.
      lv_data_index = sy-index.

      lv_num = sy-index + lv_page_index.
      lv_num_c = lv_num.

** Vaariaveis
***********************************************************************
      CONCATENATE 'SSCC_'
                  lv_num_c
             INTO lv_name.

      CONDENSE lv_name NO-GAPS.

      ASSIGN (lv_name) TO <lv_target_sscc>.
      CHECK <lv_target_sscc> IS ASSIGNED.

      CONCATENATE 'PAL_DESTINO_A'
                  lv_num_c
             INTO lv_name.

      CONDENSE lv_name NO-GAPS.

      ASSIGN (lv_name) TO <lv_target_pal_dest>.
      CHECK <lv_target_pal_dest> IS ASSIGNED.

      CONCATENATE 'PACK_MATERIAL_'
                  lv_num_c
             INTO lv_name.

      CONDENSE lv_name NO-GAPS.

      ASSIGN (lv_name) TO <lv_target_pack_mat>.
      CHECK <lv_target_pack_mat> IS ASSIGNED.

      CONCATENATE 'PTYP'
                  lv_num_c
             INTO lv_name.

      CONDENSE lv_name NO-GAPS.

      ASSIGN (lv_name) TO <lv_target_paltype>.
      CHECK <lv_target_paltype> IS ASSIGNED.

** Tela Cheia
***********************************************************************
      IF <lv_target_sscc> IS INITIAL OR
         <lv_target_pal_dest> IS INITIAL OR
         <lv_target_pack_mat> IS INITIAL.
        lv_full = '0'.
      ELSEIF lv_full IS INITIAL.
        lv_full = '1'.
      ENDIF.


***********************************************************************
      IF sy-index EQ 1.
        ASSIGN <lv_target_sscc>     TO <gv_0009_sscc1>.
        ASSIGN <lv_target_pal_dest> TO <gv_0009_pal_destino1>.
        ASSIGN <lv_target_pack_mat> TO <gv_0009_pack_material1>.
        ASSIGN <lv_target_paltype>  TO <gv_0009_paltyp1>.

        IF <gv_0009_sscc1> IS INITIAL.
          CLEAR: <gv_0009_sscc1>,
                 <gv_0009_pal_destino1>,
                 <gv_0009_pack_material1>,
                 <gv_0009_paltyp1>.
        ENDIF.
      ELSE.
        ASSIGN <lv_target_sscc>     TO <gv_0009_sscc2>.
        ASSIGN <lv_target_pal_dest> TO <gv_0009_pal_destino2>.
        ASSIGN <lv_target_pack_mat> TO <gv_0009_pack_material2>.
        ASSIGN <lv_target_paltype>  TO <gv_0009_paltyp2>.

        IF <gv_0009_sscc2> IS INITIAL.
          CLEAR: <gv_0009_sscc2>,
                 <gv_0009_pal_destino2>,
                 <gv_0009_pack_material2>,
                 <gv_0009_paltyp2>.
        ENDIF.
      ENDIF.




** Textos
***********************************************************************
      lv_numc2 = lv_num_c.
      lv_num_c = lv_data_index.

*--> Titulo
      CONCATENATE 'TIT'
                  lv_num_c
             INTO lv_name.

      CONDENSE lv_name NO-GAPS.

      ASSIGN COMPONENT lv_name
          OF STRUCTURE scr0009
                    TO <lv_target>.

      CHECK <lv_target> IS ASSIGNED.

      CONCATENATE 'Palete'(001)
                  lv_numc2
             INTO lv_text
             SEPARATED BY space.

      <lv_target> = lv_text.

*--> Posição
      CONCATENATE 'POS'
                  lv_num_c
             INTO lv_name.

      CONDENSE lv_name NO-GAPS.

      ASSIGN COMPONENT lv_name
          OF STRUCTURE scr0009
                    TO <lv_target>.

      CHECK <lv_target> IS ASSIGNED.

      CONCATENATE 'Posição Palete'(002)
                  lv_numc2
             INTO lv_text
             SEPARATED BY space.

      <lv_target> = lv_text.

*--> Tipo
      CONCATENATE 'TP'
                  lv_num_c
             INTO lv_name.

      CONDENSE lv_name NO-GAPS.

      ASSIGN COMPONENT lv_name
          OF STRUCTURE scr0009
                    TO <lv_target>.

      CHECK <lv_target> IS ASSIGNED.

      CONCATENATE 'Tipo de Palete'(003)
                  lv_numc2
             INTO lv_text
             SEPARATED BY space.

      <lv_target> = lv_text.
    ENDDO.

    lv_paletes_f = lv_paletes_f - 2.

    IF lv_full <> '1'.
      EXIT.
    ENDIF.
  ENDDO.

  IF <gv_0009_sscc1> IS INITIAL.
    MOVE '<GV_0009_SSCC1>' TO cursorfield.
  ENDIF.

  SET CURSOR FIELD cursorfield.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

** Os campos do ecrã para a segunda palete
** ficam fechados
  IF lv_paletes_f < 0.
    LOOP AT SCREEN.
      IF screen-name = '<GV_0009_SSCC2>'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = '<GV_0009_PAL_DESTINO2>'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = '<GV_0009_PACK_MATERIAL2>'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0009  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  EXIT3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit3 INPUT.
  CLEAR: equipamento,ok_code_0003,
         n_paletes, tab_zwm026, tab_zwm010,
         pal_destino1,sscc1,
         pal_destino2,sscc2,
         cursorfield.


  READ TABLE tab_zwm026 WITH KEY estado = 'P'.
  IF sy-subrc <> 0.
    IF NOT tab_zwm026[] IS INITIAL.
      LOOP AT tab_zwm026.
        CLEAR: tab_zwm026-user_name, tab_zwm026-estado.
        MODIFY tab_zwm026.
        UPDATE zwm026 FROM tab_zwm026.
        COMMIT WORK AND WAIT.
      ENDLOOP.

    ENDIF.
  ENDIF.

  REFRESH: tab_zwm026, tab_zwm010.

  IF lrf_wkqu-devty(5) = '16X20'.
    SET SCREEN '0001'.LEAVE SCREEN.
  ELSE.
    SET SCREEN '0002'.LEAVE SCREEN.
  ENDIF.


ENDMODULE.                 " EXIT3  INPUT



*&---------------------------------------------------------------------*
*&      Module  EXIT9  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit9 INPUT.

ENDMODULE.                 " EXIT9  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.
  DATA: lv_subrc TYPE sysubrc.

  CASE ok_code_0003.

    WHEN 'NEXT'.

      IF n_paletes > 1.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 05.06.2012 11:43:11
*  Motivo: Validação de Dados de Paletes
*--------------------------------------------------------------------*
        PERFORM validate_0003_pallet_data CHANGING lv_subrc.
        CHECK lv_subrc EQ 0.

*        CHECK NOT sscc1 IS INITIAL AND
*              NOT pal_destino1 IS INITIAL AND
*              NOT sscc2 IS INITIAL AND
*              NOT pal_destino2 IS INITIAL.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

** Actualização da BD com as novas associações
        PERFORM actualiza_bd.
** Verificar se os items ja estao todos a 'P'
** Caso se verifique chamar o ecran de confirmacao do PKE
        READ TABLE tab_zwm026 WITH KEY estado = 'C'.
        IF sy-subrc <> 0.
          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0009'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0010'.LEAVE SCREEN.
          ENDIF.
        ELSE.
** Segue para a realização física do PICKING
          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0005'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0006'.LEAVE SCREEN.
          ENDIF.
        ENDIF.
      ELSE.
        CHECK NOT sscc1 IS INITIAL AND
        NOT pal_destino1 IS INITIAL.
** Actualização da BD com as novas associações
        PERFORM actualiza_bd.

** Verificar se os items ja estao todos a 'P'
** Caso se verifique chamar o ecran de confirmacao do PKE
        READ TABLE tab_zwm026 WITH KEY estado = 'C'.
        IF sy-subrc <> 0.
          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0009'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0010'.LEAVE SCREEN.
          ENDIF.
        ELSE.
** Segue para a realização física do PICKING
          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0005'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0006'.LEAVE SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'CLEAR'.

      CLEAR : sscc1,
              sscc2,
              sscc3,
              sscc4,
              pal_destino1,
              pal_destino2,
              pal_destino3,
              pal_destino4,
              cursorfield.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0003  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_sscc1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc1 INPUT.

  DATA: checkdigit_ok TYPE boolean,
        i_ean         LIKE marm-ean11.

  CHECK NOT sscc1 IS INITIAL.

  IF sscc1(2) <> '00'.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '259'.
    CLEAR : sscc1.
    MOVE 'SSCC_1' TO cursorfield.
    EXIT.
  ENDIF.

  CLEAR: checkdigit_ok, i_ean.
*
*  MOVE sscc1+2(18) TO i_ean.
*
*  CALL FUNCTION 'EAN_VERIFY_CHECKDIGIT'
*  IMPORTING
*    checkdigit_ok = checkdigit_ok
*  CHANGING
*    i_ean         = i_ean.
*
*  IF checkdigit_ok IS INITIAL.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*    EXPORTING
*      message_id     = 'ZWMMSG001'
*      message_lang   = sy-langu
*      message_type   = 'E'
*      message_number = '260'.
*    CLEAR : sscc1, i_ean.
*    MOVE 'SSCC1' TO cursorfield.
*    EXIT.
*  ENDIF.

** Verificar se já existe uma HU associada ao SSCC
  SELECT SINGLE * FROM vekp WHERE lgnum = xuser-lgnum AND
                                  exidv = sscc1.
  IF sy-subrc = 0.
    WRITE sscc1 TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '115'
        message_var1   = text.
    CLEAR : sscc1.
    MOVE 'SSCC1' TO cursorfield.
  ELSE.
**  Verificar se já existe uma palete criada com o SSCC
    CLEAR: t_zwm026, zwm026.
    REFRESH t_zwm026.
    SELECT * INTO TABLE t_zwm026
        FROM zwm026
            WHERE armazem = xuser-lgnum AND
                  sscc = sscc1 AND
                  estado <> 'T'.

    IF NOT t_zwm026[] IS INITIAL.
      SORT t_zwm026 BY user_name.

      READ TABLE t_zwm026 WITH KEY user_name = sy-uname.
      IF sy-subrc <> 0.
        WRITE sscc1 TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '115'
            message_var1   = text.
        CLEAR : sscc1.
        MOVE 'SSCC1' TO cursorfield.
      ELSE.
        MOVE 'PAL_DESTINO1' TO cursorfield.
      ENDIF.
    ELSE.
** Verificar se este SSCC ja foi utilizado e ja esta finalizado
      CLEAR: t_zwm026, zwm026.
      REFRESH t_zwm026.
      SELECT * INTO TABLE t_zwm026
          FROM zwm026
              WHERE armazem = xuser-lgnum AND
                    sscc = sscc1 AND
                    estado = 'T'.
      IF NOT t_zwm026[] IS INITIAL.
        WRITE sscc1 TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '115'
            message_var1   = text.
        CLEAR : sscc1.
        MOVE 'SSCC1' TO cursorfield.
      ELSE.
        MOVE 'PAL_DESTINO1' TO cursorfield.
      ENDIF.
    ENDIF.

  ENDIF.

ENDMODULE.                 " check_sscc1  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pal_destino1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pal_destino1 INPUT.

** Na primeira palete de destino tem de estar PICKING 01
  CLEAR valor_pal1.
  PERFORM get_parameter USING xuser-lgnum
                             'PICKING'
                             'PAL1'
                              valor_pal1.
  IF pal_destino1 <> valor_pal1.
    WRITE valor_pal1 TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '108'
        message_var1   = text.

    CLEAR : pal_destino1.
    MOVE 'PAL_DESTINO1' TO cursorfield.
  ELSE.
    IF n_paletes <> 1.
      MOVE 'SSCC2' TO cursorfield.
    ENDIF.
  ENDIF.
ENDMODULE.                 " check_pal_destino1  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pack_material_a  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pack_material_a INPUT.
  DATA: lv_paltype TYPE c LENGTH 4.

  CLEAR text.

** verificar se o tipo do packing material é = 'PALT' e se existe no SAP
  SELECT SINGLE * FROM mara
                  WHERE matnr = <gv_0009_pack_material1>.
  IF sy-subrc <> 0.

    WRITE <gv_0009_pack_material1> TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text.

    CLEAR : <gv_0009_pack_material1>.
    MOVE '<GV_0009_PACK_MATERIAL1>' TO cursorfield.

  ELSE.

    IF mara-mtart = 'PALT'.
      IF n_paletes <> 1.
        MOVE '<GV_0009_SSCC2>' TO cursorfield.
      ENDIF.
    ELSE.

      WRITE <gv_0009_pack_material1> TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '077'
          message_var1   = text.

      CLEAR : <gv_0009_pack_material1>.
      MOVE '<GV_0009_PACK_MATERIAL1>' TO cursorfield.
    ENDIF.
  ENDIF.

  IF xuser-lgnum <> '150'.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 05.06.2012 15:25:47
*  Motivo: Valida Se é Meia Palete
*--------------------------------------------------------------------*
    DO 1 TIMES.
      READ TABLE tab_zwm026 WITH KEY sscc = <gv_0009_sscc1>.
      CHECK sy-subrc EQ 0.

      CONCATENATE tab_zwm026-pallet_type
                  'PAL'
             INTO lv_paltype.

      CONDENSE lv_paltype NO-GAPS.

      IF lv_paltype <> mara-groes.
**    Tipo de palete & inválido. Esperado &

        PERFORM show_rf_message USING 'ZWM001' 'E'
                                      '022'
                                      mara-groes lv_paltype
                                      '' ''.

        CLEAR <gv_0009_pack_material1>.
        MOVE '<GV_0009_PACK_MATERIAL1>' TO cursorfield.
      ENDIF.
    ENDDO .
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
  ENDIF.



*  CLEAR text.
*
*** verificar se o tipo do packing material é = 'PALT' e se existe no SAP
*  SELECT SINGLE * FROM mara
*                  WHERE matnr = pack_material_a.
*  IF sy-subrc <> 0.
*
*    WRITE pack_material_a TO text LEFT-JUSTIFIED.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '070'
*        message_var1   = text.
*
*    CLEAR : pack_material_a.
*    MOVE 'PACK_MATERIAL_A' TO cursorfield.
*
*  ELSE.
*
*    IF mara-mtart = 'PALT'.
*      IF n_paletes <> 1.
*        MOVE 'SSCC_B' TO cursorfield.
*      ENDIF.
*
*    ELSE.
*
*      WRITE pack_material_a TO text LEFT-JUSTIFIED.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '077'
*          message_var1   = text.
*
*      CLEAR : pack_material_a.
*      MOVE 'PACK_MATERIAL_A' TO cursorfield.
*    ENDIF.
*  ENDIF.


ENDMODULE.                 " check_pack_material_a  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_sscc2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc2 INPUT.

  CHECK NOT sscc2 IS INITIAL.

  IF sscc2(2) <> '00'.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '259'.
    CLEAR : sscc2.
    MOVE 'SSCC2' TO cursorfield.
    EXIT.
  ENDIF.

  CLEAR: checkdigit_ok, i_ean.

*  MOVE sscc2+2(18) TO i_ean.
*
*  CALL FUNCTION 'EAN_VERIFY_CHECKDIGIT'
*  IMPORTING
*    checkdigit_ok = checkdigit_ok
*  CHANGING
*    i_ean         = i_ean.
*
*  IF checkdigit_ok IS INITIAL.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*    EXPORTING
*      message_id     = 'ZWMMSG001'
*      message_lang   = sy-langu
*      message_type   = 'E'
*      message_number = '260'.
*    CLEAR : sscc2, i_ean.
*    MOVE 'SSCC2' TO cursorfield.
*    EXIT.
*  ENDIF.

  IF n_paletes <> 1.
    IF sscc1 = sscc2.
      WRITE sscc2 TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '106'
          message_var1   = text.

      CLEAR : sscc2.
      MOVE 'SSCC2' TO cursorfield.
    ELSE.

** Verificar se já existe uma HU associada ao SSCC
      SELECT SINGLE * FROM vekp WHERE lgnum = xuser-lgnum AND
                                      exidv = sscc2.
      IF sy-subrc = 0.
        WRITE sscc2 TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '115'
            message_var1   = text.
        CLEAR : sscc2.
        MOVE 'SSCC2' TO cursorfield.
      ELSE.
**  Verificar se já existe uma palete criada com o SSCC
        CLEAR: t_zwm026, zwm026.
        REFRESH t_zwm026.
        SELECT * INTO TABLE t_zwm026
            FROM zwm026
                WHERE armazem = xuser-lgnum AND
                      sscc = sscc2 AND
                      estado <> 'T'.

        IF NOT t_zwm026[] IS INITIAL.
          SORT t_zwm026 BY user_name.

          READ TABLE t_zwm026 WITH KEY user_name = sy-uname.
          IF sy-subrc <> 0.
            WRITE sscc2 TO text LEFT-JUSTIFIED.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '115'
                message_var1   = text.
            CLEAR : sscc2.
            MOVE 'SSCC2' TO cursorfield.
          ELSE.
            MOVE 'PAL_DESTINO2' TO cursorfield.
          ENDIF.
        ELSE.
** Verificar se este SSCC ja foi utilizado e ja esta finalizado
          CLEAR: t_zwm026, zwm026.
          REFRESH t_zwm026.
          SELECT * INTO TABLE t_zwm026
              FROM zwm026
                  WHERE armazem = xuser-lgnum AND
                        sscc = sscc2 AND
                        estado = 'T'.
          IF NOT t_zwm026[] IS INITIAL.
            WRITE sscc2 TO text LEFT-JUSTIFIED.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '115'
                message_var1   = text.
            CLEAR : sscc2.
            MOVE 'SSCC2' TO cursorfield.
          ELSE.
            MOVE 'PAL_DESTINO2' TO cursorfield.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " check_sscc2  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_pal_destino2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pal_destino2 INPUT.

  IF n_paletes <> 1.
    IF pal_destino1 = pal_destino2.
      WRITE pal_destino2 TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '107'
          message_var1   = text.

      CLEAR : pal_destino2.
      MOVE 'PAL_DESTINO2' TO cursorfield.
    ENDIF.

*** Na segunda palete de destino tem de estar PICKING 02
    CLEAR valor_pal2.
    PERFORM get_parameter USING xuser-lgnum
                               'PICKING'
                               'PAL2'
                                valor_pal2.

    IF pal_destino2 <> valor_pal2.
      WRITE valor_pal2 TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '108'
          message_var1   = text.

      CLEAR : pal_destino2.
      MOVE 'PAL_DESTINO2' TO cursorfield.
    ELSE.
      IF n_paletes > 2.
        MOVE 'SSCC3' TO cursorfield.
      ENDIF.
    ENDIF.

  ENDIF.
ENDMODULE.                 " check_pal_destino2  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pack_material_b  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pack_material_b INPUT.

  CLEAR text.

** verificar se o tipo do packing material é = 'PALT' e se existe no SAP
  SELECT SINGLE * FROM mara
                  WHERE matnr = <gv_0009_pack_material2>.
  IF sy-subrc <> 0.

    WRITE <gv_0009_pack_material2> TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text.

    CLEAR : <gv_0009_pack_material2>.
    MOVE '<GV_0009_PACK_MATERIAL2>' TO cursorfield.

  ELSE.

    IF mara-mtart = 'PALT'.
      IF n_paletes <> 1.
        MOVE '<GV_0009_PACK_MATERIAL2>' TO cursorfield.
      ENDIF.

    ELSE.

      WRITE <gv_0009_pack_material2> TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '077'
          message_var1   = text.

      CLEAR : <gv_0009_pack_material2>.
      MOVE '<GV_0009_PACK_MATERIAL2>' TO cursorfield.
    ENDIF.
  ENDIF.

  IF xuser-lgnum <> '150'.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 05.06.2012 15:25:47
*  Motivo: Valida Se é Meia Palete
*--------------------------------------------------------------------*
    DO 1 TIMES.
      READ TABLE tab_zwm026 WITH KEY sscc = <gv_0009_sscc2>.
      CHECK sy-subrc EQ 0.

      CONCATENATE tab_zwm026-pallet_type
                  'PAL'
             INTO lv_paltype.

      CONDENSE lv_paltype NO-GAPS.

      IF lv_paltype <> mara-groes.
**    Tipo de palete & inválido. Esperado &

        PERFORM show_rf_message USING 'ZWM001' 'E'
                                      '022'
                                      mara-groes lv_paltype
                                      '' ''.

        CLEAR <gv_0009_pack_material2>.
        MOVE '<GV_0009_PACK_MATERIAL2>' TO cursorfield.
      ENDIF.
    ENDDO.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
  ENDIF.

ENDMODULE.                 " check_pack_material_b  INPUT
*&---------------------------------------------------------------------*
*&      Form  actualiza_bd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_bd .

  IF NOT tab_zwm026[] IS INITIAL.
    LOOP AT tab_zwm026.

      IF tab_zwm026-pal_destino = valor_pal1.
        tab_zwm026-sscc = sscc1.
      ELSEIF tab_zwm026-pal_destino = valor_pal2.
        tab_zwm026-sscc = sscc2.
      ELSEIF tab_zwm026-pal_destino = valor_pal3.
        tab_zwm026-sscc = sscc3.
      ELSEIF tab_zwm026-pal_destino = valor_pal4.
        tab_zwm026-sscc = sscc4.
      ENDIF.

      MODIFY tab_zwm026.
      UPDATE zwm026 FROM tab_zwm026.
      COMMIT WORK.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " actualiza_bd

*&---------------------------------------------------------------------*
*&      Module  status_0005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0005 OUTPUT.
  SET PF-STATUS 'ZRF_PIC'.
  SET CURSOR FIELD cursorfield.

** Mostrar dados relativos à TO
** Primeiro mostra dados ordenados pela posição
** dentro da zona de picking
  SORT tab_zwm026 BY sorlp material pal_destino
                     quantidade DESCENDING.
  READ TABLE tab_zwm026 WITH KEY estado = 'C'
                                 let_down = ' '.
  IF sy-subrc = 0.
    PERFORM dados_ot.
  ELSE.
** Depois mostra dados que tenham ficados pendentes
    READ TABLE tab_zwm026 WITH KEY estado = 'C'
                                   let_down = 'X'.
    IF sy-subrc = 0.
      PERFORM dados_ot.
    ENDIF.
  ENDIF.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'EAN11'.
        IF pos_origem_1(3) = 'PKL'.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " status_0005  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit5  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit5 INPUT.

  CLEAR: ok_code_0005,
         pal_destino1,sscc1,
         pal_destino2,sscc2,
         pos_origem_1,pal_destino_1,
         pos_origem_2,pal_destino_2,
         material,descricao,descricao2,qtd,uni,
         pal_destino_1,tab_zwm026,ltap,lote,
         cursorfield, equipamento, refnr, gv_caixa, ean11.

  IF lrf_wkqu-devty(5) = '16X20'.
    SET SCREEN '0001'.LEAVE SCREEN.
  ELSE.
    SET SCREEN '0002'.LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " exit5  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0005 INPUT.

  DATA : plant TYPE werks_d,
         s_loc TYPE lgort_d,
         mov   TYPE bwlvs.

  DATA: lt_zwm026       TYPE TABLE OF zwm026,
        lt_ltak         TYPE TABLE OF ltak,
        lt_ltap         TYPE TABLE OF ltap,
        ls_ltap         TYPE ltap,
        lv_bin          TYPE c LENGTH 14,
        lv_matnr        TYPE matnr,
        lv_ret_code     TYPE c,
        ls_zwm052       TYPE zwm052,
        lv_set_let_down TYPE flag,
        lv_tot_ot       TYPE i.

  DATA: ls_ltak TYPE ltak.

  CLEAR valor.
  CLEAR: mov, plant, s_loc.

  CLEAR : return_msg,to.
  REFRESH : return_msg.

  CASE ok_code_0005.

    WHEN 'NEXT'.
** Quando tudo estiver preenchido
** 1 - Fazer PICK à TO q está a ser processada
** 2 - Actualizar tabelas com essa informação

      CHECK NOT pos_origem_2 IS INITIAL AND
            NOT material IS INITIAL AND
            NOT descricao IS INITIAL AND
            NOT qtd IS INITIAL AND
            NOT uni IS INITIAL AND
            NOT pal_destino_2 IS INITIAL.

      IF pos_origem_1(3) = 'PKL'.
        CHECK ean11 IS NOT INITIAL.
      ENDIF.
** Pick à TO
      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'P'
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = tab_zwm026-to_number
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
          CLEAR : ltap,tab_zwm026,lote,
                  material,descricao,descricao2,
                  qtd,uni,pos_origem_1,
                  pos_origem_2,pal_destino_1,
                  pal_destino_2,cursorfield.

        ENDIF.
      ELSE.
** Tudo ok
        PERFORM actualiza_estruturas.
        CLEAR : ltap, tab_zwm026,lote,
                material,descricao,descricao2,
                qtd,uni,pos_origem_1,
                pos_origem_2,pal_destino_1,
                pal_destino_2,cursorfield, refnr, ean11.

** Verificar se atingiu o stock minimo na posicao.
** Caso se verifique criar to de reabastecimento.
        SELECT SINGLE verme INTO lqua-verme
            FROM lqua
                WHERE lgnum = ltap-lgnum AND
                      lgtyp = ltap-nltyp AND
                      lgpla = ltap-nlpla.

        SELECT SINGLE lpmin INTO mlgt-lpmin
            FROM mlgt
                WHERE lgnum = xuser-lgnum AND
                      matnr = ltap-matnr AND
                      lgtyp = ltap-nltyp AND
                      lgpla = ltap-nlpla.
        IF lqua-verme <= mlgt-lpmin.
*          PERFORM cria_to_abastecimento_picking.
          CLEAR: lqua, mlgt, ltap.
        ENDIF.
** Verificar se existem mais items de picking
** para processar
        READ TABLE tab_zwm026 WITH KEY estado = 'C'.
        IF sy-subrc <> 0.
** Tudo processado ... próximo ecrã
          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0009'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0010'.LEAVE SCREEN.
          ENDIF.
        ENDIF.

      ENDIF.

    WHEN 'LET_DOWN'.

      CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 21.06.2012 16:03:07
*  Motivo: Prioridade de Let Down
*--------------------------------------------------------------------*
      CHECK NOT pos_origem_2 IS INITIAL AND
            NOT material IS INITIAL AND
            NOT descricao IS INITIAL AND
            NOT qtd IS INITIAL AND
            NOT uni IS INITIAL.

      CALL FUNCTION 'ZWM_GET_PARAMETER'
        EXPORTING
          i_lgnum     = xuser-lgnum
          i_processo  = 'LET_DOWN'
          i_parametro = 'QTD_PLTS_PRI'
        IMPORTING
          e_valor     = lv_tot_ot
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      lv_set_let_down = abap_true.

      DO 1 TIMES.
        CLEAR: lt_ltap, lt_ltak.


        SELECT * FROM ltap
           INTO TABLE lt_ltap
           WHERE lgnum = xuser-lgnum AND
                 pquit = abap_false  AND
                 matnr = material.

        CHECK sy-subrc EQ 0.

        SELECT * FROM ltak
           INTO TABLE lt_ltak
           FOR ALL ENTRIES IN lt_ltap
           WHERE lgnum = lt_ltap-lgnum AND
                 tanum = lt_ltap-tanum.

        DELETE lt_ltak WHERE queue <> 'QUEUEPD' AND
                             queue <> 'QUEUEPT' AND
                             queue <> 'QUEUELD' AND
                             queue <> 'QUEUERI'.

        CHECK NOT lt_ltak IS INITIAL.

        DO lv_tot_ot TIMES.
          READ TABLE lt_ltak
                INTO ls_ltak
                INDEX sy-index.

          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          CALL FUNCTION 'ZWM_TO_SET_LET_DOWN_PRIORITY'
            EXPORTING
              i_lgnum = ls_ltak-lgnum
              i_tanum = ls_ltak-tanum
            EXCEPTIONS
              error   = 1
              OTHERS  = 2.
        ENDDO.


**      Prioridade atribuida. Aguarde Reabastecimentos
        CALL FUNCTION 'ZWM_RF_MESSAGE'
          EXPORTING
            i_message_id     = 'ZWMSG001'
            i_message_type   = 'E'
            i_message_number = '003'.
      ENDDO.

      IF lt_ltak IS INITIAL.
        CLEAR: lv_ret_code.

        SELECT SINGLE * FROM zwm052
                        INTO ls_zwm052
                        WHERE refnr = refnr AND
                              pos_str = pos_origem_2.

        IF sy-subrc EQ 0.
**        Já foi criado reabastecimento para o Grupo & Posição &
          CALL FUNCTION 'ZWM_RF_MESSAGE'
            EXPORTING
              i_message_id     = 'ZWMSG001'
              i_message_type   = 'E'
              i_message_number = '002'
              i_message_var1   = refnr
              i_message_var2   = pos_origem_2.

        ELSE.
**        Sem OTs de Reab. &/&/&. Deseja Criar?
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = material
            IMPORTING
              output = lv_matnr.

          CALL FUNCTION 'ZWM_RF_MESSAGE'
            EXPORTING
              i_message_id     = 'ZWMSG001'
              i_message_type   = 'W'
              i_message_number = '001'
              i_message_var1   = lv_matnr
              i_message_var2   = pos_origem_2
              i_message_var3   = refnr
            IMPORTING
              e_ret_code       = lv_ret_code.
        ENDIF.


        IF lv_ret_code EQ 'O'.
          PERFORM reabast_batch_input USING refnr
                                            pos_origem_2
                                            lv_matnr.
        ELSE.
          CLEAR lv_set_let_down.
        ENDIF.
      ENDIF.

      IF lv_set_let_down EQ abap_true.
        lt_zwm026 = tab_zwm026[].
        DELETE lt_zwm026 WHERE num_recorrido <> tab_zwm026-num_recorrido OR
                               grupo <> tab_zwm026-grupo OR
                               material <> material.

        CHECK NOT lt_zwm026 IS INITIAL.

        CLEAR lt_ltap.
        SELECT * FROM ltap
           INTO TABLE lt_ltap
           FOR ALL ENTRIES IN lt_zwm026
           WHERE lgnum = lt_zwm026-armazem AND
                 tanum = lt_zwm026-to_number.
        CHECK sy-subrc EQ 0.

        SORT lt_ltap BY tanum.

        LOOP AT tab_zwm026.
          READ TABLE lt_ltap
                INTO ls_ltap
                WITH KEY tanum = tab_zwm026-to_number
                BINARY SEARCH.
          CHECK sy-subrc EQ 0.

          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = ls_ltap-vltyp
              lgpla = ls_ltap-vlpla
            IMPORTING
              bin   = lv_bin.

          CHECK lv_bin EQ pos_origem_2.


          tab_zwm026-let_down = abap_true.
          MODIFY tab_zwm026.

          MODIFY zwm026 FROM tab_zwm026.
        ENDLOOP.

        COMMIT WORK AND WAIT.
      ENDIF.

      CLEAR : ltap,tab_zwm026,lote,
              material,descricao,descricao2,
              qtd,uni,pos_origem_1,
              pos_origem_2,pal_destino_1,
              pal_destino_2,cursorfield.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


** Criar TO de reabastecimento ...
** Registar incidência
** Mostrar próxima ordem

*      CLEAR i_sscc.
*      REFRESH i_sscc.
*
*      i_sscc-material = material.
*** Mudar fixo
*      i_sscc-quantidade = '10'.
*      i_sscc-uni = 'CX'.
**      i_sscc-lote_producao = i_zwm026-lote.
*      APPEND i_sscc.
*      CLEAR i_sscc.
*
*** Plant
*      SELECT SINGLE valor INTO valor
*         FROM zwm001
*             WHERE armazem = xuser-lgnum AND
*                   processo = 'GERAL' AND
*                   parametro = 'PLANT'.
*      MOVE valor TO plant.
*      CLEAR valor.
*
*** Storage location
*      SELECT SINGLE valor INTO valor
*       FROM zwm001
*           WHERE armazem = xuser-lgnum AND
*                 processo = 'GERAL' AND
*                 parametro = 'LGORT'.
*      MOVE valor TO s_loc.
*      CLEAR valor.
*
*      CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
*        EXPORTING
*          warehouse  = xuser-lgnum
*          mov_type   = '821'
*          plant      = plant
*          s_loc      = s_loc
*        IMPORTING
*          to         = to_number
*        TABLES
*          return_msg = return_msg
*          sscc       = i_sscc
*        EXCEPTIONS
*          error      = 1
*          OTHERS     = 2.

** 04.03.2005 - ROFFD ** DEL
*** Incidência de desajuste de stock
*      CALL FUNCTION 'ZWM_INSERT_ERROR'
*        EXPORTING
*          armazem    = xuser-lgnum
*          incidencia = '8'
*          posicao    = pos_origem_2
*        EXCEPTIONS
*          no_commit  = 1
*          OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

** Actualização de estrutura interna para passar
** à próxima extracção
*      MOVE tab_zwm026-to_number TO to.
*      CLEAR tab_zwm026.
*** interna
*      LOOP AT tab_zwm026 WHERE to_number = to
*** FL -> 24/11/2005
*                           AND armazem = xuser-lgnum.
*** FL <- 24/11/2005
*        tab_zwm026-estado = 'C'.
*        tab_zwm026-lote = lote.
*        tab_zwm026-let_down = 'X'.
*        MODIFY tab_zwm026.
*      ENDLOOP.

*** RL -> LOG ZWM026
*      CLEAR: zwm026_log, wa_log.
*      GET TIME.
*      LOOP AT tab_zwm026.
*        MOVE-CORRESPONDING tab_zwm026 TO wa_log.
*        wa_log-data = sy-datum.
*        wa_log-hora = sy-uzeit.
*        wa_log-user_tarefa = sy-uname.
*        wa_log-programa = sy-repid.
*        MODIFY zwm026_log FROM wa_log.
*      ENDLOOP.
*** RL <- LOG ZWM026

** bd
*      UPDATE zwm026 FROM TABLE tab_zwm026.
*      COMMIT WORK.
*      CLEAR : ltap,tab_zwm026,lote,
*              material,descricao,descricao2,
*              qtd,uni,pos_origem_1,
*              pos_origem_2,pal_destino_1,
*              pal_destino_2,cursorfield.
  ENDCASE.

ENDMODULE.                 " user_command_0005  INPUT
*&---------------------------------------------------------------------*
*&      Form  dados_ot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dados_ot.
  DATA: ls_ltak   TYPE ltak,
        ls_zwm026 TYPE zwm026.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 03.08.2012 12:30:15
*  Motivo: Tipo de Palete
*--------------------------------------------------------------------*
  CLEAR: pal_type.
  LOOP AT tab_zwm026 INTO ls_zwm026 WHERE n_pal_picking = tab_zwm026-n_pal_picking AND
                                          pal_picking > 0.

    IF tab_zwm026-pallet_type EQ 'M'.
      pal_type = 'MP'.
    ENDIF.

    EXIT.
  ENDLOOP.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  CLEAR ltap.
  SELECT SINGLE * FROM ltap
                  WHERE lgnum = xuser-lgnum AND
                        tanum = tab_zwm026-to_number.

  IF sy-subrc = 0.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 21.06.2012 12:10:46
*  Motivo: Retorna Cabeçalho
*--------------------------------------------------------------------*
    SELECT SINGLE * FROM ltak
                    INTO ls_ltak
                    WHERE lgnum = xuser-lgnum AND
                          tanum = tab_zwm026-to_number.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    refnr = ls_ltak-refnr.


** Posição de origem
    CONCATENATE ltap-vltyp ltap-vlpla INTO
                pos_origem_1 SEPARATED BY space.

    MOVE ltap-charg TO lote.

    IF ltap-vltyp = 'PKL' AND gv_caixa IS INITIAL.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '283'.
      gv_caixa = 'X'.
    ENDIF.

  ENDIF.
ENDFORM.                    " dados_ot


*&---------------------------------------------------------------------*
*&      Module  check_pos_origem_2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pos_origem_2 INPUT.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

**********************************************************************

  CHECK pos_origem_2 IS NOT INITIAL.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = pos_origem_2
    IMPORTING
      ev_bin      = pos_origem_2
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    READ TABLE lt_messages INTO DATA(ls_messages) INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_messages-msgid
          message_lang   = sy-langu
          message_type   = ls_messages-msgtyp
          message_number = ls_messages-msgnr
          message_var1   = ls_messages-msgv1
          message_var2   = ls_messages-msgv2
          message_var3   = ls_messages-msgv3
          message_var4   = ls_messages-msgv4.
      CLEAR : pos_origem_2.
      MOVE 'POS_ORIGEM_2' TO cursorfield.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  IF pos_origem_2 <> pos_origem_1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '109'
        message_var1   = text.

    CLEAR : pos_origem_2.
    MOVE 'POS_ORIGEM_2' TO cursorfield.

*    IF pos_origem_1(3) = 'PKL'.
*      MOVE 'EAN11' TO cursorfield.
*    ELSE.
*      MOVE 'POS_ORIGEM_2' TO cursorfield.
*    ENDIF.

  ELSE.
** tudo ok
    CLEAR: material, descricao, descricao2, qtd, uni, pal_destino_1.

** Mensagem de Pal XP
***********************************************************************
    PERFORM show_special_pal_message.

** Material
    MOVE ltap-matnr TO material.
** Descrição
    SELECT SINGLE * FROM makt
        WHERE matnr = material AND
              spras = sy-langu.

    MOVE makt-maktx(20) TO descricao.
    MOVE makt-maktx+20(20) TO descricao2.

    IF ltap-vltyp = 'PKL'.
** Quantidade
*    MOVE trunc( ltap-vsola ) TO qtd.
      CALL FUNCTION 'ROUND'
        EXPORTING
          decimals      = 0
          input         = ltap-vsola
          sign          = 'X'
        IMPORTING
          output        = qtd
        EXCEPTIONS
          input_invalid = 1
          overflow      = 2
          type_invalid  = 3
          OTHERS        = 4.
** Unidade
      MOVE ltap-altme TO uni.
** Palete de destino
      MOVE tab_zwm026-pal_destino TO pal_destino_1.
*      MOVE 'PAL_DESTINO_2' TO cursorfield.
      MOVE 'EAN11' TO cursorfield.
    ELSE.
** Quantidade
      MOVE ltap-vsolm TO qtd.
** Unidade
      MOVE ltap-meins TO uni.
** Palete de destino
      MOVE tab_zwm026-pal_destino TO pal_destino_1.
      MOVE 'PAL_DESTINO_2' TO cursorfield.
    ENDIF.

  ENDIF.

ENDMODULE.                 " check_pos_origem_2  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pal_destino_2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pal_destino_2 INPUT.

  IF pal_destino_2 <> pal_destino_1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '110'
        message_var1   = text.

    CLEAR : pal_destino_2.
    MOVE 'PAL_DESTINO_2' TO cursorfield.
  ENDIF.

ENDMODULE.                 " check_pal_destino_2  INPUT

*&---------------------------------------------------------------------*
*&      Form  actualiza_estruturas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_estruturas .

  DATA: wa_026 LIKE zwm026.

  MOVE tab_zwm026-to_number TO to.
  CLEAR tab_zwm026.
** interna
  LOOP AT tab_zwm026 WHERE to_number = to
                       AND armazem = xuser-lgnum.
    tab_zwm026-estado = 'P'.
    tab_zwm026-lote   = lote.
    CLEAR tab_zwm026-let_down.
    MODIFY tab_zwm026.
  ENDLOOP.


  LOOP AT tab_zwm026 INTO wa_026.
    UPDATE zwm026 FROM wa_026.
    COMMIT WORK.
  ENDLOOP.

ENDFORM.                    " actualiza_estruturas

*&---------------------------------------------------------------------*
*&      Module  status_0007  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0007 OUTPUT.
  SET PF-STATUS 'ZRF_PIC'.
  SET CURSOR FIELD cursorfield.

  MOVE 'PKE 000-000-01' TO pos_destino_1.

ENDMODULE.                 " status_0007  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit7  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit7 INPUT.

ENDMODULE.                 " exit7  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0007  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0007 INPUT.

  CASE ok_code_0007.

    WHEN 'NEXT'.

      CHECK NOT pos_destino_2 IS INITIAL.
** Actualizar status de picking a 'T'
      PERFORM actualiza_status.

      CLEAR : return_msg,
              tab_zwm026,
              sscc1, sscc2,
              sscc3, sscc4,
              ptyp1, ptyp2,
              ptyp3, ptyp4,
              sscc_1, sscc_2,
              sscc_3, sscc_4,
              pal_destino1,pal_destino2,
              pal_destino3,pal_destino4,
              pal_destino_a1,pal_destino_a2,
              pal_destino_a3,pal_destino_a4,
              pack_material_1, pack_material_2,
              pack_material_3, pack_material_4,
              pos_destino_2.

      REFRESH : return_msg,
                tab_zwm026.

** Novo recorrido a ser executado
      CALL FUNCTION 'ZWM_GET_TO_RECORRIDO'
        EXPORTING
          armazem           = xuser-lgnum
        IMPORTING
          n_paletes         = n_paletes
          user_assignado    = user_assignado
        TABLES
          l_zwm010          = tab_zwm010
          return_msg        = return_msg
          l_zwm026          = itab_zwm026
        EXCEPTIONS
          no_work_available = 1
          no_equipment      = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '103'.

        CLEAR : equipamento,tab_zwm010,tab_zwm026,
                n_paletes,return_msg,cursorfield.
        REFRESH : tab_zwm010,tab_zwm026,return_msg.
        IF lrf_wkqu-devty(5) = '16X20'.
          SET SCREEN '0001'.LEAVE SCREEN.
        ELSE.
          SET SCREEN '0002'.LEAVE SCREEN.
        ENDIF.

      ELSE.
** Existem recorridos para executar

        FREE: tab_zwm026.
        CLEAR: tab_zwm026.

        CLEAR: ptyp1, ptyp2,
               ptyp3, ptyp4,
               lv_num.

        LOOP AT itab_zwm026.

          PERFORM set_mpal_info.

          MOVE-CORRESPONDING itab_zwm026 TO tab_zwm026.
          APPEND tab_zwm026.
        ENDLOOP.

        FREE: itab_zwm026.
        CLEAR: itab_zwm026.

        IF NOT user_assignado IS INITIAL.
          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0005'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0006'.LEAVE SCREEN.
          ENDIF.
        ENDIF.

** 1 - Ecrã para informar o operário de qual
** a qtd de paletes q tem de carregar

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 03.08.2012 12:41:19
*  Motivo: Mensagem de Palete
*--------------------------------------------------------------------*
        PERFORM send_to_pal_message CHANGING returncode.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

        CASE n_paletes.
          WHEN '1'.

            LOOP AT tab_zwm026.
              CHECK tab_zwm026-pal_destino = 'PICKING 02'.
              tab_zwm026-pal_destino = 'PICKING 01'.
              MODIFY tab_zwm026.
              UPDATE zwm026 FROM tab_zwm026.
              COMMIT WORK.
            ENDLOOP.

*            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*              EXPORTING
*                message_id     = 'ZWMMSG001'
*                message_lang   = sy-langu
*                message_type   = 'W'
*                message_number = '104'
*              IMPORTING
*                ret_code       = returncode.

            IF returncode = 'C'.
              CLEAR equipamento.
** Actualização da BD limpando o user que vai processar o recorrido
              IF NOT tab_zwm026[] IS INITIAL.
                LOOP AT tab_zwm026.
                  CLEAR: tab_zwm026-user_name, tab_zwm026-estado.
                  MODIFY tab_zwm026.
                  UPDATE zwm026 FROM tab_zwm026.
                  COMMIT WORK AND WAIT.
                ENDLOOP.

              ENDIF.

              IF lrf_wkqu-devty(5) = '16X20'.
                SET SCREEN '0001'.LEAVE SCREEN.
              ELSE.
                SET SCREEN '0002'.LEAVE SCREEN.
              ENDIF.
            ENDIF.

          WHEN '2'.
*            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*              EXPORTING
*                message_id     = 'ZWMMSG001'
*                message_lang   = sy-langu
*                message_type   = 'W'
*                message_number = '105'
*              IMPORTING
*                ret_code       = returncode.

            IF returncode = 'C'.
              CLEAR equipamento.
** Actualização da BD limpando o user que vai processar o recorrido
              IF NOT tab_zwm026[] IS INITIAL.
                LOOP AT tab_zwm026.
                  CLEAR: tab_zwm026-user_name, tab_zwm026-estado.
                  MODIFY tab_zwm026.
                  UPDATE zwm026 FROM tab_zwm026.
                  COMMIT WORK AND WAIT.
                ENDLOOP.

              ENDIF.

              IF lrf_wkqu-devty(5) = '16X20'.
                SET SCREEN '0001'.LEAVE SCREEN.
              ELSE.
                SET SCREEN '0002'.LEAVE SCREEN.
              ENDIF.
            ENDIF.

          WHEN OTHERS.

*--> Mais que 2
*              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                EXPORTING
*                  message_id     = 'ZWM001'
*                  message_lang   = sy-langu
*                  message_type   = 'W'
*                  message_number = '021'
*                  message_var1   = lv_message_mpal
*                IMPORTING
*                  ret_code       = returncode.
            IF returncode = 'C'.
              CLEAR equipamento.
** Actualização da BD limpando o user que vai processar o recorrido
              READ TABLE tab_zwm026 WITH KEY estado = 'P'.
              IF sy-subrc <> 0.

                IF NOT tab_zwm026[] IS INITIAL.
                  LOOP AT tab_zwm026.
                    CLEAR: tab_zwm026-user_name, tab_zwm026-estado.
                    MODIFY tab_zwm026.
                    UPDATE zwm026 FROM tab_zwm026.
                    COMMIT WORK AND WAIT.
                  ENDLOOP.
                ENDIF.
              ENDIF.

              IF lrf_wkqu-devty(5) = '16X20'.
                SET SCREEN '0001'.LEAVE SCREEN.
              ELSE.
                SET SCREEN '0002'.LEAVE SCREEN.
              ENDIF.
            ENDIF.
        ENDCASE.

        IF lrf_wkqu-devty(5) = '16X20'.
          SET SCREEN '0003'.LEAVE SCREEN.
        ELSE.
          SET SCREEN '0004'.LEAVE SCREEN.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " user_command_0007  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pos_destino_2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pos_destino_2 INPUT.

  CHECK pos_destino_2 IS NOT INITIAL.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = pos_destino_2
    IMPORTING
      ev_bin      = pos_destino_2
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    READ TABLE lt_messages INTO ls_messages INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_messages-msgid
          message_lang   = sy-langu
          message_type   = ls_messages-msgtyp
          message_number = ls_messages-msgnr
          message_var1   = ls_messages-msgv1
          message_var2   = ls_messages-msgv2
          message_var3   = ls_messages-msgv3
          message_var4   = ls_messages-msgv4.
      CLEAR : pos_destino_2.
      MOVE 'POS_DESTINO_2' TO cursorfield.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.


  IF pos_destino_1 <> pos_destino_2.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '114'
        message_var1   = text.

    CLEAR : pos_destino_2.
    MOVE 'POS_DESTINO_2' TO cursorfield.
    IF lrf_wkqu-devty(5) = '16X20'.
      SET SCREEN '0007'.LEAVE SCREEN.
    ELSE.
      SET SCREEN '0008'.LEAVE SCREEN.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_pos_destino_2  INPUT
*&---------------------------------------------------------------------*
*&      Form  actualiza_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_status .

  LOOP AT tab_zwm026.
    tab_zwm026-estado = 'T'.
    MODIFY tab_zwm026.
    UPDATE zwm026 FROM tab_zwm026.
    COMMIT WORK.
  ENDLOOP.

ENDFORM.                    " actualiza_status
*&---------------------------------------------------------------------*
*&      Form  cria_to_abastecimento_picking
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_to_abastecimento_picking .

  DATA to LIKE ltap-tanum.
  DATA i_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

**     Cria to do PKR. Senão existir palete no PKR procura no resto do
**     armazem e envia para o REP

*SSCC
*TIPO_SU
  i_sscc-material = ltap-matnr.
  APPEND i_sscc.

  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse  = xuser-lgnum
      mov_type   = '825'
*     ST_TYPE_O  =
*     BIN_ORIGEM =
*     ST_TYPE_D  =
*     BIN_DESTINO          =
*     STOCK_CAT  =
      plant      = ltap-werks
      s_loc      = ltap-lgort
*     CERTIFICADO          =
*     ORIGEM     =
*     REQ_NUMBER =
*     REQ_TYPE   =
*     SSCC_ADICIONAL       =
    IMPORTING
      to         = to
    TABLES
      return_msg = return_msg
      sscc       = i_sscc
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " cria_to_abastecimento_picking

*&---------------------------------------------------------------------*
*&      Module  check_sscc_a  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc_a INPUT.
  CHECK NOT <gv_0009_sscc1> IS INITIAL.

  IF <gv_0009_sscc1>(2) <> '00'.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '259'.
    CLEAR : <gv_0009_sscc1>, <gv_0009_pal_destino1>.
    MOVE '<GV_0009_SSCC1>' TO cursorfield.
    EXIT.
  ENDIF.

  pal_type = pal_type.

  CLEAR: checkdigit_ok, i_ean.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 05.06.2012 16:15:17
*  Motivo: Valida SSCC
*--------------------------------------------------------------------*
  PERFORM validate_sscc_data USING <gv_0009_sscc1>
                             CHANGING lv_subrc.
  IF lv_subrc <> 0.
    CLEAR : <gv_0009_sscc1>, <gv_0009_pal_destino1>.
    MOVE '<GV_0009_SSCC1>' TO cursorfield.
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


*  IF <gv_0009_pal_destino1> EQ <gv_0009_pal_destino2>.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '253'.
*    CLEAR : <gv_0009_sscc1>, <gv_0009_pal_destino1>.
*    MOVE '<GV_0009_SSCC1>' TO cursorfield.
*    EXIT.
*  ENDIF.

  READ TABLE tab_zwm026 WITH KEY sscc = <gv_0009_sscc1>.
  IF sy-subrc EQ 0.
    IF tab_zwm026-pallet_type EQ 'M'.
      <gv_0009_paltyp1> = 'MP'.
    ELSE.
      CLEAR: <gv_0009_paltyp1>.
    ENDIF.

    <gv_0009_pal_destino1> = tab_zwm026-pal_destino.
    MOVE '<GV_0009_PACK_MATERIAL1>' TO cursorfield.
  ELSE.
    WRITE <gv_0009_sscc1> TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '113'
        message_var1   = text.
    CLEAR : <gv_0009_sscc1>, <gv_0009_pal_destino1>.
    MOVE '<GV_0009_SSCC1>' TO cursorfield.
  ENDIF.


*  CHECK NOT sscc_a IS INITIAL.
*
*  IF sscc_a(2) <> '00'.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '259'.
*    CLEAR : sscc_a, pal_destino_a.
*    MOVE 'SSCC_A' TO cursorfield.
*    EXIT.
*  ENDIF.
*
*  CLEAR: checkdigit_ok, i_ean.
*
**  MOVE sscc_a+2(18) TO i_ean.
**
**  CALL FUNCTION 'EAN_VERIFY_CHECKDIGIT'
**    IMPORTING
**      checkdigit_ok = checkdigit_ok
**    CHANGING
**      i_ean         = i_ean.
**
**  IF checkdigit_ok IS INITIAL.
**    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
**      EXPORTING
**        message_id     = 'ZWMMSG001'
**        message_lang   = sy-langu
**        message_type   = 'E'
**        message_number = '260'.
**    CLEAR : sscc_a, pal_destino_a, i_ean.
**    MOVE 'SSCC_A' TO cursorfield.
**    EXIT.
**  ENDIF.
*
*  IF sscc_a EQ sscc_b.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '253'.
*    CLEAR : sscc_a, pal_destino_a.
*    MOVE 'SSCC_A' TO cursorfield.
*    EXIT.
*  ENDIF.
*
*  READ TABLE tab_zwm026 WITH KEY sscc = sscc_a.
*  IF sy-subrc EQ 0.
*    pal_destino_a = tab_zwm026-pal_destino.
*    MOVE 'PACK_MATERIAL_A' TO cursorfield.
*  ELSE.
*    WRITE sscc_a TO text LEFT-JUSTIFIED.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '113'
*        message_var1   = text.
*    CLEAR : sscc_a, pal_destino_a.
*    MOVE 'SSCC_A' TO cursorfield.
*  ENDIF.
ENDMODULE.                 " check_sscc_a  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_sscc_b  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc_b INPUT.

  CHECK NOT <gv_0009_sscc2> IS INITIAL.

  IF <gv_0009_sscc2>(2) <> '00'.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '259'.
    CLEAR : <gv_0009_sscc2>, <gv_0009_pal_destino2>.
    MOVE '<GV_0009_SSCC2>' TO cursorfield.
    EXIT.
  ENDIF.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 05.06.2012 16:15:17
*  Motivo: Valida SSCC
*--------------------------------------------------------------------*
  PERFORM validate_sscc_data USING <gv_0009_sscc2>
                             CHANGING lv_subrc.
  IF lv_subrc <> 0.
    CLEAR : <gv_0009_sscc2>, <gv_0009_pal_destino2>.
    MOVE '<GV_0009_SSCC2>' TO cursorfield.
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


*  IF <GV_0009_SSCC1> EQ <GV_0009_SSCC2>.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '253'.
*    CLEAR : sscc_b, pal_destino_b.
*    MOVE 'SSCC_B' TO cursorfield.
*    EXIT.
*  ENDIF.

  READ TABLE tab_zwm026 WITH KEY sscc = <gv_0009_sscc2>.
  IF sy-subrc EQ 0.

    IF tab_zwm026-pallet_type EQ 'M'.
      <gv_0009_paltyp2> = 'MP'.
    ELSE.
      CLEAR: <gv_0009_paltyp2>.
    ENDIF.

    <gv_0009_pal_destino2> = tab_zwm026-pal_destino.
    MOVE '<GV_0009_PACK_MATERIAL2>' TO cursorfield.
  ELSE.
    WRITE <gv_0009_sscc2> TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '113'
        message_var1   = text.
    CLEAR : <gv_0009_sscc2>, <gv_0009_pal_destino2>.
    MOVE '<GV_0009_SSCC2>' TO cursorfield.
  ENDIF.





*  CHECK NOT sscc_b IS INITIAL.
*
*  IF sscc_b(2) <> '00'.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '259'.
*    CLEAR : sscc_b, pal_destino_b.
*    MOVE 'SSCC_B' TO cursorfield.
*    EXIT.
*  ENDIF.
*
**  CLEAR: checkdigit_ok, i_ean.
**  MOVE sscc_b+2(18) TO i_ean.
**
**  CALL FUNCTION 'EAN_VERIFY_CHECKDIGIT'
**  IMPORTING
**    checkdigit_ok = checkdigit_ok
**  CHANGING
**    i_ean         = i_ean.
**
**  IF checkdigit_ok IS INITIAL.
**    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
**    EXPORTING
**      message_id     = 'ZWMMSG001'
**      message_lang   = sy-langu
**      message_type   = 'E'
**      message_number = '260'.
**    CLEAR : sscc_b, pal_destino_b, i_ean.
**    MOVE 'SSCC_B' TO cursorfield.
**    EXIT.
**  ENDIF.
*
*  IF sscc_a EQ sscc_b.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '253'.
*    CLEAR : sscc_b, pal_destino_b.
*    MOVE 'SSCC_B' TO cursorfield.
*    EXIT.
*  ENDIF.
*
*  READ TABLE tab_zwm026 WITH KEY sscc = sscc_b.
*  IF sy-subrc EQ 0.
*    pal_destino_b = tab_zwm026-pal_destino.
*    MOVE 'PACK_MATERIAL_B' TO cursorfield.
*  ELSE.
*    WRITE sscc_b TO text LEFT-JUSTIFIED.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '113'
*        message_var1   = text.
*    CLEAR : sscc_b, pal_destino_b.
*    MOVE 'SSCC_B' TO cursorfield.
*  ENDIF.

ENDMODULE.                 " check_sscc_b  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0009  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0009 INPUT.

  CASE ok_code_0009.

    WHEN 'SAVE'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 05.06.2012 15:48:12
*  Motivo: Salva Dados
*--------------------------------------------------------------------*
      PERFORM validate_save_0009 CHANGING lv_subrc.
      CHECK lv_subrc EQ 0.

      DO n_paletes TIMES.
        lv_num_c = sy-index.

        CONCATENATE 'SSCC_'
                    lv_num_c
               INTO lv_name.

        CONDENSE lv_name NO-GAPS.
        ASSIGN (lv_name) TO <lv_target_sscc>.
        CHECK <lv_target_sscc> IS ASSIGNED.

        CONCATENATE 'PAL_DESTINO_A'
                    lv_num_c
               INTO lv_name.

        CONDENSE lv_name NO-GAPS.

        ASSIGN (lv_name) TO <lv_target_pal_dest>.
        CHECK <lv_target_pal_dest> IS ASSIGNED.

        CONCATENATE 'PACK_MATERIAL_'
                    lv_num_c
               INTO lv_name.

        CONDENSE lv_name NO-GAPS.

        ASSIGN (lv_name) TO <lv_target_pack_mat>.
        CHECK <lv_target_pack_mat> IS ASSIGNED.

        LOOP AT tab_zwm026 WHERE sscc EQ <lv_target_sscc>.
          tab_zwm026-pack_material = <lv_target_pack_mat>.
          MODIFY tab_zwm026.
          UPDATE zwm026 FROM tab_zwm026.
          COMMIT WORK.
        ENDLOOP.

        CLEAR: <lv_target_sscc>, <lv_target_pack_mat>,
               <lv_target_pal_dest>.
      ENDDO.

      IF lrf_wkqu-devty(5) = '16X20'.
        SET SCREEN '0007'.LEAVE SCREEN.
      ELSE.
        SET SCREEN '0008'.LEAVE SCREEN.
      ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*



*      IF n_paletes > 1.
*
*        CHECK NOT sscc_a          IS INITIAL AND
*              NOT pal_destino_a   IS INITIAL AND
*              NOT pack_material_a IS INITIAL AND
*              NOT sscc_b          IS INITIAL AND
*              NOT pal_destino_b   IS INITIAL AND
*              NOT pack_material_b IS INITIAL.
*
*        LOOP AT tab_zwm026 WHERE sscc EQ sscc_a.
*          tab_zwm026-pack_material = pack_material_a.
*          MODIFY tab_zwm026.
*          UPDATE zwm026 FROM tab_zwm026.
*          COMMIT WORK.
*        ENDLOOP.
*
*        LOOP AT tab_zwm026 WHERE sscc EQ sscc_b.
*          tab_zwm026-pack_material = pack_material_b.
*          MODIFY tab_zwm026.
*          UPDATE zwm026 FROM tab_zwm026.
*          COMMIT WORK.
*        ENDLOOP.
*
*        CLEAR : sscc_a, sscc_b,
*                pal_destino_a,pal_destino_b,
*                pack_material_a, pack_material_b.
*
*        IF lrf_wkqu-devty(5) = '16X20'.
*          SET SCREEN '0007'.LEAVE SCREEN.
*        ELSE.
*          SET SCREEN '0008'.LEAVE SCREEN.
*        ENDIF.
*
*      ELSE.                                                 " 1 Palete
*        CHECK NOT sscc_a          IS INITIAL AND
*              NOT pal_destino_a   IS INITIAL AND
*              NOT pack_material_a IS INITIAL.
*
*        LOOP AT tab_zwm026 WHERE sscc EQ sscc_a.
*          tab_zwm026-pack_material = pack_material_a.
*          MODIFY tab_zwm026.
*          UPDATE zwm026 FROM tab_zwm026.
*          COMMIT WORK.
*        ENDLOOP.
*
*        CLEAR : sscc_a, sscc_b,
*                pal_destino_a,pal_destino_b,
*                pack_material_a, pack_material_b.
*
*        IF lrf_wkqu-devty(5) = '16X20'.
*          SET SCREEN '0007'.LEAVE SCREEN.
*        ELSE.
*          SET SCREEN '0008'.LEAVE SCREEN.
*        ENDIF.
*      ENDIF.

    WHEN 'CLEAR'.

      CLEAR : sscc_1,
              sscc_2,
              sscc_3,
              sscc_4,
              pal_destino_a1,
              pal_destino_a2,
              pal_destino_a3,
              pal_destino_a4,
              pack_material_1,
              pack_material_2,
              pack_material_3,
              pack_material_4.

      cursorfield = 'SSCC_A'.

  ENDCASE.

  CLEAR ok_code_0009.

ENDMODULE.                 " user_command_0009  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SSCC3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc3 INPUT.
  CHECK NOT sscc3 IS INITIAL.

  IF sscc3(2) <> '00'.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '259'.
    CLEAR : sscc3.
    MOVE 'SSCC3' TO cursorfield.
    EXIT.
  ENDIF.

  CLEAR: checkdigit_ok, i_ean.
*
*  MOVE sscc3+2(18) TO i_ean.
*
*  CALL FUNCTION 'EAN_VERIFY_CHECKDIGIT'
*  IMPORTING
*    checkdigit_ok = checkdigit_ok
*  CHANGING
*    i_ean         = i_ean.
*
*  IF checkdigit_ok IS INITIAL.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*    EXPORTING
*      message_id     = 'ZWMMSG001'
*      message_lang   = sy-langu
*      message_type   = 'E'
*      message_number = '260'.
*    CLEAR : sscc3, i_ean.
*    MOVE 'sscc3' TO cursorfield.
*    EXIT.
*  ENDIF.

** Verificar se já existe uma HU associada ao SSCC
  SELECT SINGLE * FROM vekp WHERE lgnum = xuser-lgnum AND
                                  exidv = sscc3.
  IF sy-subrc = 0.
    WRITE sscc3 TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '115'
        message_var1   = text.
    CLEAR : sscc3.
    MOVE 'SSCC3' TO cursorfield.
  ELSE.
**  Verificar se já existe uma palete criada com o SSCC
    CLEAR: t_zwm026, zwm026.
    REFRESH t_zwm026.
    SELECT * INTO TABLE t_zwm026
        FROM zwm026
            WHERE armazem = xuser-lgnum AND
                  sscc = sscc3 AND
                  estado <> 'T'.

    IF NOT t_zwm026[] IS INITIAL.
      SORT t_zwm026 BY user_name.

      READ TABLE t_zwm026 WITH KEY user_name = sy-uname.
      IF sy-subrc <> 0.
        WRITE sscc3 TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '115'
            message_var1   = text.
        CLEAR : sscc3.
        MOVE 'SSCC3' TO cursorfield.
      ELSE.
        MOVE 'PAL_DESTINO3' TO cursorfield.
      ENDIF.
    ELSE.
** Verificar se este SSCC ja foi utilizado e ja esta finalizado
      CLEAR: t_zwm026, zwm026.
      REFRESH t_zwm026.
      SELECT * INTO TABLE t_zwm026
          FROM zwm026
              WHERE armazem = xuser-lgnum AND
                    sscc = sscc3 AND
                    estado = 'T'.
      IF NOT t_zwm026[] IS INITIAL.
        WRITE sscc3 TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '115'
            message_var1   = text.
        CLEAR : sscc3.
        MOVE 'SSCC3' TO cursorfield.
      ELSE.
        MOVE 'PAL_DESTINO3' TO cursorfield.
      ENDIF.
    ENDIF.


  ENDIF.
ENDMODULE.                 " CHECK_SSCC3  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SSCC4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc4 INPUT.
  CHECK NOT sscc4 IS INITIAL.

  IF sscc4(2) <> '00'.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '259'.
    CLEAR : sscc4.
    MOVE 'SSCC4' TO cursorfield.
    EXIT.
  ENDIF.

  CLEAR: checkdigit_ok, i_ean.
*
*  MOVE sscc4+2(18) TO i_ean.
*
*  CALL FUNCTION 'EAN_VERIFY_CHECKDIGIT'
*  IMPORTING
*    checkdigit_ok = checkdigit_ok
*  CHANGING
*    i_ean         = i_ean.
*
*  IF checkdigit_ok IS INITIAL.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*    EXPORTING
*      message_id     = 'ZWMMSG001'
*      message_lang   = sy-langu
*      message_type   = 'E'
*      message_number = '260'.
*    CLEAR : sscc4, i_ean.
*    MOVE 'sscc4' TO cursorfield.
*    EXIT.
*  ENDIF.

** Verificar se já existe uma HU associada ao SSCC
  SELECT SINGLE * FROM vekp WHERE lgnum = xuser-lgnum AND
                                  exidv = sscc4.
  IF sy-subrc = 0.
    WRITE sscc4 TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '115'
        message_var1   = text.
    CLEAR : sscc4.
    MOVE 'SSCC4' TO cursorfield.
  ELSE.
**  Verificar se já existe uma palete criada com o SSCC
    CLEAR: t_zwm026, zwm026.
    REFRESH t_zwm026.
    SELECT * INTO TABLE t_zwm026
        FROM zwm026
            WHERE armazem = xuser-lgnum AND
                  sscc = sscc4 AND
                  estado <> 'T'.

    IF NOT t_zwm026[] IS INITIAL.
      SORT t_zwm026 BY user_name.

      READ TABLE t_zwm026 WITH KEY user_name = sy-uname.
      IF sy-subrc <> 0.
        WRITE sscc4 TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '115'
            message_var1   = text.
        CLEAR : sscc4.
        MOVE 'SSCC4' TO cursorfield.
      ELSE.
        MOVE 'PAL_DESTINO4' TO cursorfield.
      ENDIF.
    ELSE.
** Verificar se este SSCC ja foi utilizado e ja esta finalizado
      CLEAR: t_zwm026, zwm026.
      REFRESH t_zwm026.
      SELECT * INTO TABLE t_zwm026
          FROM zwm026
              WHERE armazem = xuser-lgnum AND
                    sscc = sscc4 AND
                    estado = 'T'.
      IF NOT t_zwm026[] IS INITIAL.
        WRITE sscc4 TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '115'
            message_var1   = text.
        CLEAR : sscc4.
        MOVE 'SSCC4' TO cursorfield.
      ELSE.
        MOVE 'PAL_DESTINO4' TO cursorfield.
      ENDIF.
    ENDIF.

  ENDIF.
ENDMODULE.                 " CHECK_SSCC4  INPUT
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_0003_PALLET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_0003_pallet_data CHANGING cv_subrc TYPE sysubrc.
  DATA: lv_index_c TYPE c LENGTH 3,
        lv_name    TYPE c LENGTH 50.

  FIELD-SYMBOLS: <lv_test> TYPE any.

  CLEAR cv_subrc.

  DO n_paletes TIMES.
    lv_index_c = sy-index.

    CONCATENATE 'SSCC'
                lv_index_c
           INTO lv_name.

    CONDENSE lv_name NO-GAPS.

    ASSIGN (lv_name) TO <lv_test>.
    CHECK <lv_test> IS ASSIGNED.

    IF <lv_test> IS INITIAL.
      cv_subrc = 4.
      EXIT.
    ENDIF.

    UNASSIGN <lv_test>.

    CONCATENATE 'PAL_DESTINO'
                lv_index_c
           INTO lv_name.

    CONDENSE lv_name NO-GAPS.

    ASSIGN (lv_name) TO <lv_test>.
    CHECK <lv_test> IS ASSIGNED.

    IF <lv_test> IS INITIAL.
      cv_subrc = 4.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " VALIDATE_0003_PALLET_DATA
*&---------------------------------------------------------------------*
*&      Module  CHECK_PAL_DESTINO3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pal_destino3 INPUT.
** Na primeira palete de destino tem de estar PICKING 01
  CLEAR valor_pal3.
  PERFORM get_parameter USING xuser-lgnum
                             'PICKING'
                             'PAL3'
                              valor_pal3.
  IF pal_destino3 <> valor_pal3.
    WRITE valor_pal3 TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '108'
        message_var1   = text.

    CLEAR : pal_destino1.
    MOVE 'PAL_DESTINO3' TO cursorfield.
  ELSE.
    IF n_paletes > 3.
      MOVE 'SSCC4' TO cursorfield.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_PAL_DESTINO3  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PAL_DESTINO4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pal_destino4 INPUT.
** Na primeira palete de destino tem de estar PICKING 01
  CLEAR valor_pal4.
  PERFORM get_parameter USING xuser-lgnum
                             'PICKING'
                             'PAL4'
                              valor_pal4.
  IF pal_destino4 <> valor_pal4.
    WRITE valor_pal4 TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '108'
        message_var1   = text.

    CLEAR : pal_destino1.
    MOVE 'PAL_DESTINO4' TO cursorfield.
  ENDIF.
ENDMODULE.                 " CHECK_PAL_DESTINO4  INPUT
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SAVE_0009
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM validate_save_0009 CHANGING cv_subrc TYPE sysubrc.
  CLEAR cv_subrc.

  DO n_paletes TIMES.
    lv_num_c = sy-index.

    CONCATENATE 'SSCC_'
                lv_num_c
           INTO lv_name.

    CONDENSE lv_name NO-GAPS.

    ASSIGN (lv_name) TO <lv_target_sscc>.
    CHECK <lv_target_sscc> IS ASSIGNED.

    CONCATENATE 'PAL_DESTINO_A'
                lv_num_c
           INTO lv_name.

    CONDENSE lv_name NO-GAPS.

    ASSIGN (lv_name) TO <lv_target_pal_dest>.
    CHECK <lv_target_pal_dest> IS ASSIGNED.

    CONCATENATE 'PACK_MATERIAL_'
                lv_num_c
           INTO lv_name.

    CONDENSE lv_name NO-GAPS.

    ASSIGN (lv_name) TO <lv_target_pack_mat>.
    CHECK <lv_target_pack_mat> IS ASSIGNED.

    IF <lv_target_sscc> IS INITIAL OR
       <lv_target_pal_dest> IS INITIAL OR
       <lv_target_pack_mat> IS INITIAL.
      cv_subrc = 4.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " VALIDATE_SAVE_0009

*&---------------------------------------------------------------------*
*&      Form  show_rf_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_MESSAGE_ID      text
*      -->UV_MESSAGE_TYPE    text
*      -->UV_MESSAGE_NUMBER  text
*      -->UV_MESSAGE_VAR1    text
*      -->UV_MESSAGE_VAR2    text
*      -->UV_MESSAGE_VAR3    text
*      -->UV_MESSAGE_VAR4    text
*----------------------------------------------------------------------*
FORM show_rf_message  USING uv_message_id
                            uv_message_type
                            uv_message_number
                            uv_message_var1
                            uv_message_var2
                            uv_message_var3
                            uv_message_var4.

  DATA: lv_message_id     TYPE  bdcmsgcoll-msgid,
        lv_message_type   TYPE  bdcmsgcoll-msgtyp,
        lv_message_number TYPE  bdcmsgcoll-msgnr,
        lv_message_var1   TYPE  bdcmsgcoll-msgv1,
        lv_message_var2   TYPE  bdcmsgcoll-msgv2,
        lv_message_var3   TYPE  bdcmsgcoll-msgv3,
        lv_message_var4   TYPE  bdcmsgcoll-msgv4.

  lv_message_id     = uv_message_id.
  lv_message_type   = uv_message_type.
  lv_message_number = uv_message_number.
  lv_message_var1   = uv_message_var1.
  lv_message_var2   = uv_message_var2.
  lv_message_var3   = uv_message_var3.
  lv_message_var4   = uv_message_var4.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = lv_message_id
      message_lang   = sy-langu
      message_type   = lv_message_type
      message_number = lv_message_number
      message_var1   = lv_message_var1
      message_var2   = lv_message_var2
      message_var3   = lv_message_var3
      message_var4   = lv_message_var4.

ENDFORM.                    "show_rf_message
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SSCC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<GV_0009_SSCC1>  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM validate_sscc_data  USING    uv_sscc1
                         CHANGING cv_subrc.

  DATA: lv_equal TYPE i.

  CLEAR cv_subrc.

  DO n_paletes TIMES.
    lv_num_c = sy-index.

    CONCATENATE 'SSCC_'
                lv_num_c
           INTO lv_name.

    CONDENSE lv_name NO-GAPS.
    ASSIGN (lv_name) TO <lv_target_sscc>.
    CHECK <lv_target_sscc> IS ASSIGNED.

    IF uv_sscc1 EQ <lv_target_sscc>.
      lv_equal = lv_equal + 1.
    ENDIF.

    IF lv_equal > 1.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '253'.
      cv_subrc = 4.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.                    " VALIDATE_SSCC_DATA
*&---------------------------------------------------------------------*
*&      Form  SEND_TO_PAL_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_RETURNCODE  text
*----------------------------------------------------------------------*
FORM send_to_pal_message CHANGING returncode.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 03.08.2012 12:03:47
*  Motivo: Numero de Paletes
*--------------------------------------------------------------------*
  DATA: lv_tot_mpal TYPE i,
        lv_tot_pal  TYPE i.
*  DATA: lv_box      TYPE c,
*        lt_ltap     TYPE TABLE OF ltap.

**********************************************************************

  LOOP AT tab_zwm026 WHERE pal_picking > 0.
    IF tab_zwm026-pallet_type EQ 'M'.
      lv_tot_mpal = lv_tot_mpal + 1.
    ELSE.
      lv_tot_pal = lv_tot_pal + 1.
    ENDIF.
  ENDLOOP.

  CLEAR: returncode.

*  SELECT * FROM ltap INTO TABLE lt_ltap
*  FOR ALL ENTRIES IN tab_zwm026
*   WHERE lgnum = tab_zwm026-armazem    AND
*         tanum = tab_zwm026-to_number  AND
*         vltyp = 'PKL'.
*
*  IF lt_ltap IS NOT INITIAL.
*    lv_box = 'X'.
*  ENDIF.

  IF lv_tot_mpal > 0 AND
     lv_tot_pal  > 0.

    WRITE lv_tot_pal TO lv_message_v1.
    CONDENSE lv_message_v1.

    WRITE lv_tot_mpal TO lv_message_v2.
    CONDENSE lv_message_v2.


**      São necessárias & & + & &
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWM001'
        message_lang   = sy-langu
        message_type   = 'W'
        message_number = '032'
        message_var1   = lv_message_v1
        message_var2   = 'PAL'
        message_var3   = lv_message_v2
        message_var4   = 'MPAL'
      IMPORTING
        ret_code       = returncode.
*
***      São necessárias:  & & + & & e 1 caixa
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWM001'
*          message_lang   = sy-langu
*          message_type   = 'W'
*          message_number = '042'
*          message_var1   = lv_message_v1
*          message_var2   = 'PAL'
*          message_var3   = lv_message_v2
*          message_var4   = 'MPAL'
*        IMPORTING
*          ret_code       = returncode.


  ELSEIF lv_tot_mpal > 0.

    WRITE lv_tot_mpal TO lv_message_v1.
    CONDENSE lv_message_v1.


**    São necessárias & &
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWM001'
        message_lang   = sy-langu
        message_type   = 'W'
        message_number = '033'
        message_var1   = lv_message_v1
        message_var2   = 'MPAL'
      IMPORTING
        ret_code       = returncode.


  ELSEIF lv_tot_pal > 0.

    WRITE lv_tot_pal TO lv_message_v1.
    CONDENSE lv_message_v1.


**    São necessárias & &
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWM001'
        message_lang   = sy-langu
        message_type   = 'W'
        message_number = '033'
        message_var1   = lv_message_v1
        message_var2   = 'PAL'
      IMPORTING
        ret_code       = returncode.

  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

ENDFORM.                    " SEND_TO_PAL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  REABAST_BATCH_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_REFNR  text
*      -->P_POS_ORIGEM_2  text
*      -->P_LV_MATNR  text
*----------------------------------------------------------------------*
FORM reabast_batch_input  USING uv_refnr TYPE lvs_refnr
                                uv_pos   TYPE c
                                uv_matnr TYPE matnr.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: ls_zwm052 TYPE zwm052.

  DATA: lv_lgtyp TYPE lgtyp,
        lv_lgpla TYPE lgpla.


  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = uv_pos
    IMPORTING
      lgtyp = lv_lgtyp
      lgpla = lv_lgpla.


  CALL FUNCTION 'ZWM_CREATE_REABAST'
    EXPORTING
      i_lgnum     = xuser-lgnum
      i_lgtyp     = lv_lgtyp
      i_lgpla     = lv_lgpla
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    EXIT.
  ENDIF.


  GET TIME.
  ls_zwm052-refnr   = uv_refnr.
  ls_zwm052-pos_str = uv_pos.
  ls_zwm052-erdat   = sy-datum.
  ls_zwm052-erzeit  = sy-uzeit.
  ls_zwm052-ername  = sy-uname.
  MODIFY zwm052 FROM ls_zwm052.
  COMMIT WORK AND WAIT.
ENDFORM.                    " REABAST_BATCH_INPUT

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_BDC       text
*      -->PU_DYNBEGIN  text
*      -->PU_NAME      text
*      -->PU_VALUE     text
*----------------------------------------------------------------------*
FORM dynpro USING pt_bdc   TYPE bdcdata_tab
                  pu_dynbegin
                  pu_name
                  pu_value.

  DATA: ls_bdc TYPE bdcdata.

  IF NOT pu_dynbegin IS INITIAL.
    ls_bdc-program  = pu_name.
    ls_bdc-dynpro   = pu_value.
    ls_bdc-dynbegin = 'X'.
  ELSE.
    ls_bdc-fnam = pu_name.
    ls_bdc-fval = pu_value.
  ENDIF.

  APPEND ls_bdc TO pt_bdc.

ENDFORM.                    " dynpro

*&---------------------------------------------------------------------*
*&      Form  set_mpal_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_mpal_info .

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 05.06.2012 14:21:00
*  Motivo: Tipo de Palete
*--------------------------------------------------------------------*
  IF itab_zwm026-pal_picking > 0.
    lv_num = lv_num + 1.


    lv_num_c = lv_num.

    CONCATENATE 'PTYP'
                lv_num_c
           INTO lv_name.

    CONDENSE lv_name NO-GAPS.

    ASSIGN (lv_name) TO <lv_target>.

    IF <lv_target> IS ASSIGNED.
      IF itab_zwm026-pallet_type EQ 'M'.
        <lv_target> = 'MP'.

** 031            ATENÇÃO: MEIAS PALETES NO RECORRIDO
        MESSAGE s031(zwm001) INTO lv_message_mpal.
      ELSE.
        CLEAR: <lv_target>.
      ENDIF.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

ENDFORM.                    "set_mpal_info
*&---------------------------------------------------------------------*
*&      Form  SHOW_SPECIAL_PAL_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_special_pal_message.
  DATA: lt_t311a TYPE TABLE OF t311a,
        lt_vbpa  TYPE TABLE OF vbpa.

  DATA: ls_vbpa   TYPE vbpa,
        ls_zwm040 TYPE zwm040,
        ls_zwm049 TYPE zwm049,
        ls_zwm028 TYPE zwm028.

  DATA: lv_2step   TYPE flag,
        lv_lines   TYPE sytabix,
        lv_message TYPE char80,
        lv_kunnr   TYPE kunnr.

** Valida se grupo é em 2 passos
***********************************************************************
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = tab_zwm026-armazem
      i_refnr = tab_zwm026-grupo
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

  IF lv_2step EQ abap_true AND tab_zwm026-remessa = '9999999999'.

    DO 1 TIMES.
*--> Retorna Remessas do Grupo
      SELECT * FROM t311a
         INTO TABLE lt_t311a
         WHERE lgnum = tab_zwm026-armazem AND
               refnr = tab_zwm026-grupo.

      CHECK sy-subrc EQ 0.

*--> Retorna Parceiros
      SELECT * FROM vbpa
         INTO TABLE lt_vbpa
         FOR ALL ENTRIES IN lt_t311a
         WHERE vbeln = lt_t311a-rbnum.

      DELETE lt_vbpa WHERE parvw <> 'W1'.

      SORT lt_vbpa BY kunnr.
      DELETE ADJACENT DUPLICATES FROM lt_vbpa COMPARING kunnr.

      DESCRIBE TABLE lt_vbpa LINES lv_lines.
      CHECK lv_lines EQ 1.

      READ TABLE lt_vbpa
            INTO ls_vbpa
            INDEX 1.

      lv_kunnr = ls_vbpa-kunnr.
    ENDDO.
  ELSE.
    SELECT SINGLE * FROM zwm040
                    INTO ls_zwm040
                    WHERE lgnum   = tab_zwm026-armazem AND
                          refnr   = tab_zwm026-grupo   AND
                          remessa = tab_zwm026-remessa.

    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM zwm028
                      INTO ls_zwm028
                      WHERE lgnum   = tab_zwm026-armazem AND
                            refnr   = tab_zwm026-grupo   AND
                            remessa = ls_zwm040-id_servisan.
    ELSE.
      SELECT SINGLE * FROM zwm028
                      INTO ls_zwm028
                      WHERE lgnum   = tab_zwm026-armazem AND
                            refnr   = tab_zwm026-grupo   AND
                            remessa = tab_zwm026-remessa.
    ENDIF.

    lv_kunnr = ls_zwm028-emissor.
  ENDIF.

  CHECK NOT lv_kunnr IS INITIAL.

  SELECT SINGLE * FROM zwm049
                  INTO ls_zwm049
                  WHERE lgnum = tab_zwm026-armazem AND
                        kunnr = lv_kunnr.

  CHECK sy-subrc EQ 0 AND
        NOT ls_zwm049-paltxt IS INITIAL.

  lv_message = ls_zwm049-paltxt.

  CALL FUNCTION 'ZWM_RF_MESSAGE'
    EXPORTING
      i_message_id     = 'ZWMSG001'
      i_message_type   = 'E'
      i_message_number = '000'
      i_message_var1   = lv_message.
ENDFORM.                    " SHOW_SPECIAL_PAL_MESSAGE
*&---------------------------------------------------------------------*
*&      Module  CHECK_EAN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ean INPUT.
  DATA: ls_marm TYPE marm.
**********************************************************************

  CHECK ean11 IS NOT INITIAL.
** Verifica se exite codigo de ean11 para o material e U.M.
  SELECT SINGLE * FROM marm INTO ls_marm
    WHERE matnr = material AND
          meinh = uni.

  IF ls_marm-ean11 <> ean11.
    MOVE ean11 TO text.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '284'
        message_var1   = text.

    CLEAR : ean11.
    MOVE 'EAN11' TO cursorfield.
  ELSE.
    MOVE 'PAL_DESTINO_2' TO cursorfield.
  ENDIF.


ENDMODULE.                 " CHECK_EAN  INPUT
