************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0024                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Monitor de Entregas                                      *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 2004/06/03                                               *
* Tipo PRG..: Report ALV Tree                                          *
************************************************************************
* Alteração : Luís Rocha                                               *
*        em.: 2004/12/10                                               *
************************************************************************
* Alteração : Ricardo Lopes                                            *
*        em.: 2005/10/11                                               *
************************************************************************
* Alteração : Diogo Silva                                              *
*        em.: 2012/05/23                                               *
************************************************************************

REPORT zwmrep0024 NO STANDARD PAGE HEADING MESSAGE-ID zwmmsg001.

*--------------------------------------------------- Dados Globais
INCLUDE zdados.

*----------------------------------------------------Ecran de selecção
INCLUDE zecram.

*--------------------------------------------------- Initialization
INITIALIZATION.
*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:32:34
*/ Considerar depósito França
  PERFORM get_user_data.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:32:49

  CLEAR valor.
  PERFORM get_parameter
          USING lgnum
                'MONITOR_ENTREGAS'
                'NUM_DIAS'
                valor.

  MOVE valor TO num_dias.

  datum-sign = 'I'.
  datum-option = 'GE'.
  datum-low = sy-datum - num_dias.
  APPEND datum.

  refnr-sign = 'I'.
  refnr-option = 'GE'.

  SELECT refnr INTO refnr-low
      FROM t311
          WHERE lgnum = lgnum
            AND datum IN datum
       ORDER BY refnr.
    EXIT.
  ENDSELECT.
  APPEND refnr.

*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:32:34
*/ Considerar depósito França
*  PERFORM get_user_data.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:32:49

START-OF-SELECTION.

  IF lgnum = '100' AND sy-uname <> 'GGUTERRES'.
    MESSAGE i000 WITH 'Transação ZWM031 obsoleta, usar nova transação ZWM031B'.
    RETURN.
  ENDIF.

  IF p_mod = 'X'.
    PERFORM check_bloqueios.
  ENDIF.

*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:46:29
*/ Validar se armzem por defeito = armazem introduzido
  IF gv_init_lgnum NE lgnum.
    MESSAGE e050(zwmfr001).
    FREE ti_zwm001[].
  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:46:29
  PERFORM get_costumizing.

  PERFORM constroi_ranges.

  PERFORM select_data.

  IF all_tasks[] IS INITIAL.
*   Não existem dados para processar !
    MESSAGE s147(zwmmsg001).
    EXIT.
  ENDIF.

  PERFORM tab_final.

  IF tab_out[] IS INITIAL.
*   Não existem dados para processar !
    MESSAGE s147(zwmmsg001).
    EXIT.
  ENDIF.

*  SORT tab_out.

  CLEAR not_first_time.

END-OF-SELECTION.

  CALL SCREEN 100.

*---------------------------------------------------------------------*
*  MODULE status_0100 OUTPUT
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'GERAL'.
  IF tree1 IS INITIAL.
    PERFORM init_tree.
  ENDIF.

  IF NOT all_tasks[] IS INITIAL.
    IF flag_tree IS INITIAL.
      PERFORM cria_hierarquia.
      flag_tree = 'X'.
    ENDIF.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                    "status_0100 OUTPUT

*---------------------------------------------------------------------*
*  MODULE user_command_0100 INPUT
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: selected_node TYPE lvc_nkey,
        item_name     TYPE lvc_fname.

  CASE ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      PERFORM exit_program.
    WHEN 'REFR'.
      CLEAR flag_tree.
      not_first_time = 'X'.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.
  CLEAR ok_code.
*  call method cl_gui_cfw=>flush.

ENDMODULE.                    "user_command_0100 INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'DESTINO'.
*  SET TITLEBAR 'xxx'.

  LOOP AT SCREEN.
*    IF NOT zwm028-st_pul IS INITIAL.
*      IF zwm028-total_paletes < 17.
*        IF screen-name = 'BIN_DESTINO2'.
*          screen-input = ' '.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*    ELSEIF NOT zwm028-st_dck IS INITIAL.
    IF screen-name = 'BIN_DESTINO1'.
      screen-input = ' '.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'BIN_DESTINO2'.
      screen-input = ' '.
      MODIFY SCREEN.
    ENDIF.
*    ENDIF.
  ENDLOOP.
ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  user_exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit INPUT.

  g_ok_code = ok_code_0200.
  IF ok_code_0200 EQ 'CANCE'.
    CLEAR:  st_destino, st_destino2, bin_destino1,
            bin_destino2,ok_code_0200.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " user_exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  DATA: lv_werks_u TYPE werks_d,
        lv_lgort_u TYPE lgort_d.

  IF g_lock = '2' AND zwm028-st_ppk = ' '.
    CHECK NOT bin_destino1 IS INITIAL.
  ENDIF.

  CHECK NOT lgnum   IS INITIAL AND
        NOT g_grupo IS INITIAL AND
        NOT g_lock  IS INITIAL AND
        NOT st_destino IS INITIAL.
*        AND
*        NOT bin_destino1 IS INITIAL.


** Centro
***********************************************************************
  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user        = sy-uname
      i_refnr       = zwm028-refnr
      i_recall      = 'X'
      i_usewm       = 'X'
      i_usemm       = 'X'
      i_useaut      = 'X'
      i_get_lgnum   = 'X'
      i_get_werks   = 'X'
      i_get_lgort   = 'X'
      i_first_werks = 'X'
      i_first_lgort = 'X'
*   IMPORTING
*     ET_MESSAGES   =
    CHANGING
      c_lgnum       = zwm028-lgnum
      c_werks       = lv_werks_u
      c_lgort       = lv_lgort_u
    EXCEPTIONS
      error         = 1
      user_back     = 2
      OTHERS        = 3.




  g_ok_code = ok_code_0200.
  CASE ok_code_0200.
    WHEN 'CONFIRMAR'.
** Seleccionar o transporte para o grupo.



      IF NOT zwm028-st_pul IS INITIAL AND st_destino = 'PUL'.
*-----------------------------------------------------------ins Mar2005
*os criterios de selecção foram alterados logo alteramos tambem a
*actualização do status de bloqueio

        UPDATE zwm028 SET  zlock = g_lock
                           st_pul = st_destino
                           pulmao1 = bin_destino1
                           pulmao2 = bin_destino2
                           transporte = trans
                                WHERE lgnum = lgnum AND
                                      refnr = g_grupo.       " AND
*                                      remessa = '          '.
*-----------------------------------------------------------------------
*        Ocupar Pulmao

** FL -> 18/01/2006
*        UPDATE lagp SET brand = 'X'
*                       WHERE lgnum = lgnum AND
*                             lgtyp = st_destino AND
*                             lgpla = bin_destino1.
*        COMMIT WORK AND WAIT.

        CLEAR wa_lagpv.
        SELECT SINGLE * FROM lagp
                WHERE lgnum = lgnum
                  AND lgtyp = st_destino
                  AND lgpla = bin_destino1.

        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lagp TO wa_lagpv.
          wa_lagpv-brand = 'X'.
          CALL FUNCTION 'L_LAGP_VERAENDERN'
            EXPORTING
              xlagpv = wa_lagpv.

          COMMIT WORK AND WAIT.
        ENDIF.
** FL <- 18/01/2006

        IF NOT bin_destino2 IS INITIAL.

** FL -> 18/01/2006
*          UPDATE lagp SET brand = 'X'
*              WHERE lgnum = lgnum AND
*                    lgtyp = st_destino AND
*                    lgpla = bin_destino2.
*          COMMIT WORK AND WAIT.

          CLEAR wa_lagpv.
          SELECT SINGLE * FROM lagp
                  WHERE lgnum = lgnum
                    AND lgtyp = st_destino
                    AND lgpla = bin_destino2.

          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING lagp TO wa_lagpv.
            wa_lagpv-brand = 'X'.
            CALL FUNCTION 'L_LAGP_VERAENDERN'
              EXPORTING
                xlagpv = wa_lagpv.

            COMMIT WORK AND WAIT.
          ENDIF.
** FL <- 18/01/2006

        ENDIF.



      ELSEIF NOT zwm028-st_dck IS INITIAL.

** Get Matricula
        SELECT SINGLE signi
            FROM vttk INTO vttk-signi
              WHERE tknum = zwm028-transporte.
*-----------------------------------------------------------ins Mar2005
*os criterios de selecção foram alterados logo alteramos tambem a
*actualização do status de bloqueio

        UPDATE zwm028 SET zlock = g_lock
                        st_dck = st_destino
                        porta = bin_destino1
                        transporte = trans
                        WHERE lgnum = lgnum AND
                                  refnr = g_grupo.                "AND
*                                  remessa = '          '.
*----------------------------------------------------------------------

        COMMIT WORK.
      ENDIF.

*      Verificar se tem mais grupos associados nas rotas de picking e

      IF g_lock = '2' OR g_lock = '4'.

*        SELECT * INTO TABLE i_zwm026
*            FROM zwm026
*                WHERE armazem = lgnum AND
*                      grupo = g_grupo.
*
*        LOOP AT i_zwm026.
*
*          SELECT *
*              FROM zwm026
*                  WHERE armazem = i_zwm026-armazem AND
*                        num_recorrido = i_zwm026-num_recorrido AND
*                        grupo <> g_grupo.
*
*            SELECT SINGLE *
*                FROM zwm028
*                    WHERE lgnum = zwm026-armazem AND
*                          refnr = zwm026-grupo.
*
*            IF zwm028-zlock <> '2' AND zwm028-zlock <> '4'.
*              READ TABLE i_grupo WITH KEY grupo = zwm026-grupo.
*              IF sy-subrc <> 0.
*                i_grupo-grupo = zwm026-grupo.
*                APPEND i_grupo.
*                CLEAR i_grupo.
*              ENDIF.
*            ENDIF.
*          ENDSELECT.
*        ENDLOOP.
*        IF NOT i_grupo[] IS INITIAL.
*          READ TABLE i_grupo INDEX 1.
*          g_grupo = i_grupo-grupo.
*          DELETE i_grupo INDEX 1.
*          CLEAR: st_destino, st_destino2, bin_destino1,
*                 bin_destino2,ok_code_0200.
*          SET SCREEN '0000'.
*          LEAVE SCREEN.
*        ELSE.
        CLEAR:  st_destino, st_destino2, bin_destino1,
                bin_destino2,ok_code_0200.
        SET SCREEN '0000'.
        LEAVE SCREEN.
*        ENDIF.
      ELSEIF g_lock = '5'.
**      carga directa nao cria OT de Carga
        CHECK zwm028-st_dck IS INITIAL.
        CHECK zwm028-ot IS INITIAL.
** Criar Ot de carga
        DATA: i_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.
        DATA return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
        DATA certificado LIKE lagp-lzone.
        DATA to LIKE ltak-tanum.

        CLEAR: i_sscc, return_msg, certificado, to.
        REFRESH: i_sscc, return_msg.

        MOVE 'PORTA' TO i_sscc-material.
        MOVE '1' TO i_sscc-quantidade.
        MOVE 'UN' TO i_sscc-uni.
        i_sscc-tipo_su = 'P6'."por defeito pq nao interessa o tipo
        APPEND i_sscc.

        SELECT SINGLE lzone INTO certificado
            FROM lagp WHERE lgnum = lgnum AND
                            lgtyp = 'DCK' AND
                            lgpla = bin_destino1.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse   = lgnum
            mov_type    = '930'
*           ST_TYPE_O   = ST_TYPE_O
            bin_origem  = bin_destino1
*           ST_TYPE_D   = ST_TYPE_D
            bin_destino = bin_destino1
            plant       = lv_werks_u
            s_loc       = lv_lgort_u
            certificado = certificado
          IMPORTING
            to          = to
          TABLES
            return_msg  = return_msg
            sscc        = i_sscc
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
        IF sy-subrc <> 0.
          LOOP AT return_msg WHERE msgtyp = 'E'.
            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                    NUMBER return_msg-msgnr
                    WITH return_msg-msgv1 return_msg-msgv2.
          ENDLOOP.
        ELSE.
*-----------------------------------------------------------ins Mar2005
*os criterios de selecção foram alterados logo alteramos tambem a
*actualização do status de bloqueio

*        UPDATE zwm028 SET  zlock = g_lock
*                           st_pul = st_destino
*                           pulmao1 = bin_destino1
*                           pulmao2 = bin_destino2
*                                WHERE lgnum = lgnum AND
*                                      refnr = g_grupo.

          UPDATE zwm028 SET  ot = to
                             zlock = g_lock
              WHERE lgnum = lgnum AND
                    refnr = g_grupo.                          " AND
*                    remessa = '          '.
*----------------------------------------------------------------------
          IF sy-subrc = 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.

          CLEAR:  st_destino, st_destino2, bin_destino1,
                  bin_destino2,ok_code_0200.
          SET SCREEN '0000'.
          LEAVE SCREEN.
        ENDIF.
      ELSE.
        CLEAR:  st_destino, st_destino2, bin_destino1,
                bin_destino2,ok_code_0200.
        SET SCREEN '0000'.
        LEAVE SCREEN.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_zlock  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zlock INPUT.

*  CHECK NOT zlock IS INITIAL.
*
*  IF zwm028-zlock <> zlock.
*    CASE zwm028-zlock.
*      WHEN '1'.
*        IF zlock = '5' AND zwm028-porta IS INITIAL.
*          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '120'
*                WITH  zwm028-zlock zlock.
*          CLEAR: zlock.
*        ENDIF.
*      WHEN '2'.
*        IF zlock <> '4'.
*          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '120'
*                WITH  zwm028-zlock zlock.
*          CLEAR: zlock.
*        ENDIF.
*      WHEN '3'.
*        IF zlock <> '4' AND zlock <> '5'.
*          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '120'
*                WITH  zwm028-zlock zlock.
*          CLEAR: zlock.
*        ENDIF.
*      WHEN '4'.
*        IF zlock <> '5'.
*          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '120'
*                WITH  zwm028-zlock zlock.
*          CLEAR: zlock.
*        ENDIF.
*
*    ENDCASE.
*
*  ENDIF.
*
ENDMODULE.                 " check_zlock  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_destino1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_destino1 INPUT.

  DATA porta(3).
  CLEAR porta.
  CHECK NOT bin_destino1 IS INITIAL.

  SELECT SINGLE *
      FROM lagp WHERE
          lgnum = lgnum AND
          lgtyp = st_destino AND
          lgpla = bin_destino1.

  IF sy-subrc <> 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '034'
           WITH  bin_destino1.
    CLEAR bin_destino1.
  ELSE.
    IF st_destino = 'PUL'.
*    verificar se ja há paletes no pulmao, nao pode alterar pulmao
      IF NOT zwm028-paletes_pulmao IS INITIAL.
        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '121'
          WITH  g_grupo.
        CLEAR bin_destino1.
*      verificar se o pulmao esta desocupado
      ELSE.
        IF NOT lagp-brand IS INITIAL OR NOT lagp-anzqu IS INITIAL.
          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '123'
                   WITH bin_destino1.
          CLEAR bin_destino1.
        ENDIF.
      ENDIF.
*      verificar se é uma porta de carga e está desocupada
    ELSEIF st_destino = 'DCK'.
      IF zwm028-porta IS INITIAL.
        CONCATENATE '0' bin_destino1+8(2) INTO porta.

        SELECT * INTO CORRESPONDING FIELDS OF TABLE l_portas_carga
            FROM zwm002 AS w INNER JOIN zwm007 AS m
                 ON w~armazem = m~armazem AND
                    w~porta = m~porta
                    WHERE w~armazem = lgnum AND
                          w~bloqueio = ' ' AND
                          w~porta = porta AND
                          w~estado = 'D' AND
                          m~tipo <> 'D'.
        IF sy-subrc <> 0.
          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '124'
               WITH bin_destino1.
          CLEAR bin_destino1.
        ENDIF.
      ELSE.
** Carga directa
        SELECT SINGLE *
             FROM zwm006_aux
                 WHERE n_transporte = zwm028-transporte AND
                       porta = zwm028-porta.
        IF sy-subrc <> 0.
          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '133'
                WITH bin_destino1.

        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " check_destino1  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_destino2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_destino2 INPUT.

  CHECK NOT bin_destino2 IS INITIAL.

  SELECT SINGLE *
      FROM lagp WHERE
          lgnum = lgnum AND
          lgtyp = st_destino AND
          lgpla = bin_destino2.

  IF sy-subrc <> 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '034'
           WITH  bin_destino2.
    CLEAR bin_destino2.
  ELSE.
    IF st_destino = 'PUL'.
*    verificar se ja há paletes no pulmao, nao pode alterar pulmao
      IF NOT zwm028-paletes_pulmao IS INITIAL.
        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '121'
          WITH  g_grupo.
        CLEAR bin_destino2.
*      verificar se o pulmao esta desocupado
      ELSEIF NOT lagp-brand IS INITIAL OR NOT lagp-anzqu IS INITIAL.
        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '123'
                 WITH bin_destino2.
        CLEAR bin_destino2.
      ELSEIF bin_destino1+4(3) <> bin_destino2+4(3).
        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '125'
         WITH bin_destino1 bin_destino2.
        CLEAR: bin_destino1, bin_destino2.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_destino2  INPUT

*&--------------------------------------------------------------------*
*&      Form  cancela_0200
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM cancela_0200.
*  data: l_resposta.
*
*  if gs_out-lock_grupo <> ecran-lock_grupo or
*     gs_out-prioridade <> ecran-prioridade.
*
*    perform popup using text-m23   ' '   ' '
*                        text-m29   ' '   text-m26
*                        l_resposta.
*
*    check l_resposta = 'J'.
*  endif.
*
*  set screen 0. leave screen.
ENDFORM.                    "cancela_0200

*&---------------------------------------------------------------------*
*&      Form  exit_program
*&---------------------------------------------------------------------*
*       free object and leave program
*----------------------------------------------------------------------*
FORM exit_program.

  CALL METHOD tree1->free.
  SET SCREEN 0. LEAVE SCREEN.

ENDFORM.                               " exit_program

*&--------------------------------------------------------------------*
*&      Form  check_grupo
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_GRUPO    text
*      -->F_SUBRC    text
*---------------------------------------------------------------------*
FORM check_grupo USING f_grupo f_subrc.
  DATA: lv_num_entrada  TYPE char5.

  CLEAR lt_zwm026.
  REFRESH lt_zwm026.

  CLEAR: bin_destino1, bin_destino2.

  CHECK NOT f_grupo IS INITIAL.

  SELECT SINGLE * FROM t311 WHERE lgnum = lgnum AND refnr = f_grupo.
  IF t311-kzdru IS INITIAL.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '100'
        WITH  f_grupo.
    f_subrc = 9.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE lt_zwm026
      FROM zwm026
          WHERE armazem = lgnum
            AND grupo = f_grupo.
  IF sy-subrc = 0.
    DELETE lt_zwm026 WHERE num_recorrido IS INITIAL.

    DELETE lt_zwm026 WHERE to_number IS INITIAL.

    IF lt_zwm026[] IS INITIAL.
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '182'
         WITH  f_grupo.
      f_subrc = 9.
      EXIT.
    ENDIF.
  ENDIF.

  CLEAR zwm028.
  SELECT SINGLE *
      FROM zwm028
          WHERE lgnum = lgnum AND
                refnr = f_grupo AND
                remessa = ' '.

  IF sy-subrc = 0.
    CLEAR: grupo, transporte.
    REFRESH: grupo, transporte.
    MOVE: 'I'   TO grupo-sign,
                  'EQ'  TO grupo-option,
                  zwm028-refnr   TO grupo-low.
    APPEND grupo.

    CALL FUNCTION 'SD_SHIPMENT_FOR_DELGROUP'
      TABLES
*       SHIPMENT_I       =
        delgroup_i       = grupo
        shipment_o       = transporte
      EXCEPTIONS
        delgroup_i_empty = 1
        no_entries_found = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
** erro, ainda não tem transporte associado
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '162' WITH  f_grupo.
      f_subrc = 9.
      EXIT.
    ENDIF.

    CLEAR trans.
    READ TABLE transporte INDEX 1.
    trans = transporte-low.

  ENDIF.

  IF zwm028-total_paletes > zwm028-paletes_pulmao.

    IF zwm028-pulmao1  IS INITIAL AND
       zwm028-pulmao2  IS INITIAL AND
       zwm028-pre_pick IS INITIAL AND
       zwm028-porta    IS INITIAL.

      IF NOT zwm028-st_pul IS INITIAL.
        MOVE zwm028-st_pul TO st_destino.
        IF NOT zwm028-st_ppk IS INITIAL.
          MOVE zwm028-st_ppk TO st_destino2.
        ENDIF.
      ELSEIF NOT zwm028-st_dck IS INITIAL.

        IF g_lock = 5.
          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '177' WITH  f_grupo.
          f_subrc = 9.
          EXIT.
        ENDIF.

        MOVE zwm028-st_dck TO st_destino.
**      Verificar se o carro ja esta na portaria
        CLEAR: grupo, transporte.
        REFRESH: grupo, transporte.

        MOVE: 'I'   TO grupo-sign,
              'EQ'  TO grupo-option,
              f_grupo   TO grupo-low.
        APPEND grupo.

        CALL FUNCTION 'SD_SHIPMENT_FOR_DELGROUP'
          TABLES
*           SHIPMENT_I       =
            delgroup_i       = grupo
            shipment_o       = transporte
          EXCEPTIONS
            delgroup_i_empty = 1
            no_entries_found = 2
            OTHERS           = 3.
        IF sy-subrc <> 0.
** erro, ainda não tem transporte associado
          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '162' WITH  f_grupo.
          f_subrc = 9.
          EXIT.
        ENDIF.

        READ TABLE transporte INDEX 1.
        SELECT SINGLE *
            FROM zwm006_aux
                WHERE n_transporte = transporte-low AND
                      porta <> ' '.
        IF sy-subrc = 0.
          CONCATENATE '000-000-' zwm006_aux-porta+1(2)
                 INTO bin_destino1.
          EXIT.
        ELSE.
          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '163'.
          f_subrc = 9.
          EXIT.
        ENDIF.
      ENDIF.

      IF g_lock = 2 AND zwm028-st_ppk = 'PPK'.
** Não faz nada
      ELSE.

*        CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*          EXPORTING
*            mode_keyword   = 'X'
*            keyword_       = 'G_PORTARIA'
*          EXCEPTIONS
*            foreign_lock   = 1
*            system_failure = 2
*            OTHERS         = 3.
*        IF sy-subrc <> 0.
*          l_user = sy-msgv1.
*          MESSAGE i249 WITH l_user.
*          f_subrc = 9.
*          EXIT.
*        ENDIF.
*
*        CLEAR: bin_destino1, bin_destino2.
*        CALL FUNCTION 'ZWM_FIND_BIN'
*          EXPORTING
*            st_type             = st_destino
*            num_paletes         = zwm028-total_paletes
*            armazem             = lgnum
*          IMPORTING
*            posicao1            = bin_destino1
*            posicao2            = bin_destino2
*          EXCEPTIONS
*            num_paletes_initial = 1
*            OTHERS              = 2.
*        IF sy-subrc <> 0.
**        message id sy-msgid type sy-msgty number sy-msgno
**                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        ENDIF.
*
*        IF bin_destino1 IS INITIAL AND bin_destino2 IS INITIAL.
*
*          CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*            EXPORTING
*              mode_keyword   = 'X'
*              keyword_       = 'G_PORTARIA'
*            EXCEPTIONS
*              foreign_lock   = 1
*              system_failure = 2
*              OTHERS         = 3.
*
*          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '205'.
*          f_subrc = 4.
*          EXIT.
*        ENDIF.
      ENDIF.

    ELSE.

      IF g_lock <> 5.
        IF NOT zwm028-st_pul IS INITIAL.
          MOVE zwm028-st_pul TO st_destino.
          MOVE zwm028-pulmao1 TO bin_destino1.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 12:29:28
*  Motivo: Passa Zona do Pulmão
*--------------------------------------------------------------------*
          MOVE zwm028-kober   TO bin_kober1.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
          MOVE zwm028-pulmao2 TO bin_destino2.
          IF NOT zwm028-st_ppk IS INITIAL.
            MOVE zwm028-st_ppk TO st_destino2.
          ENDIF.
        ELSEIF NOT zwm028-st_dck IS INITIAL.
          MOVE zwm028-st_dck TO st_destino.
          MOVE zwm028-porta TO bin_destino1.
          CLEAR bin_destino2.
        ENDIF.
      ELSE.
        f_subrc = 4.
        EXIT.
      ENDIF.
    ENDIF.

  ELSEIF zwm028-total_paletes <= zwm028-paletes_pulmao AND g_lock > 2.

    MOVE 'DCK' TO st_destino.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 19.09.2012 15:05:11
*  Motivo: Retorna porta por talão e pulmão
*--------------------------------------------------------------------*
    SELECT SINGLE num_entrada FROM zwm006_aux
                              INTO lv_num_entrada
                              WHERE armazem = lgnum AND
                                    n_transporte = zwm028-transporte AND
                                    finalizada   = abap_false.


    SELECT SINGLE * FROM zwm002 WHERE armazem = lgnum AND
                                      num_entrada = lv_num_entrada AND
                                      pulmao_1 = zwm028-pulmao1.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*    SELECT SINGLE * FROM zwm002 WHERE pulmao_1 = zwm028-pulmao1.

    IF sy-subrc <> 0.
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '151'.
      f_subrc = 4.
      EXIT.
    ELSE.
      CONCATENATE '000-000-' zwm002-porta+1(2) INTO bin_destino1.
    ENDIF.
  ENDIF.

ENDFORM.                    "check_grupo

*&---------------------------------------------------------------------*
*&      Form  init_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree.

  DATA: lt_expand_nodes TYPE lvc_t_nkey.

  DATA: l_toolbar_excluding TYPE ui_functions.

  REFRESH gt_out.

* create fieldcatalog for structure t001
  PERFORM build_fieldcatalog.

*  perform ajusta_propriedades.

* create container for alv-tree
  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container.

  l_tree_container_name = 'TREE1'.

  IF sy-batch IS INITIAL.
    CREATE OBJECT l_custom_container
      EXPORTING
        container_name              = l_tree_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.

    ENDIF.
  ENDIF.

* create tree control
  CREATE OBJECT tree1
    EXPORTING
      parent                      = l_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
      item_selection              = 'X'
      no_html_header              = ''
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  IF sy-subrc <> 0.

  ENDIF.

* create Hierarchy-header
  DATA l_hierarchy_header TYPE treev_hhdr.
  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.

* create info-table for html-header
  DATA: lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value.

  PERFORM build_comment USING
                 lt_list_commentary
                 l_logo.

* repid for saving variants
  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-repid.

*  perform define_toolbar_excluding changing l_toolbar_excluding.

  l_logo = 'RENOVA_LOGO'.

** Opções da Toolbar a excluir
  PERFORM exluir_toolbar CHANGING l_toolbar_excluding.

* create empty tree-control
  CALL METHOD tree1->set_table_for_first_display
    EXPORTING
      is_hierarchy_header  = l_hierarchy_header
      it_list_commentary   = lt_list_commentary
      i_logo               = l_logo
      i_background_id      = 'ALV_BACKGROUND'
      i_save               = 'A'
      is_variant           = ls_variant
      it_toolbar_excluding = l_toolbar_excluding
    CHANGING
      it_outtab            = gt_out "table must be empty !!
      it_fieldcatalog      = gt_fieldcatalog.

* register events
  PERFORM register_events.

* add own functioncodes to the toolbar
  PERFORM change_toolbar.

ENDFORM.                    "init_tree

*&--------------------------------------------------------------------*
*&      Form  change_toolbar
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM change_toolbar.

* get toolbar control
  CALL METHOD tree1->get_toolbar_object
    IMPORTING
      er_toolbar = mr_toolbar.

  CHECK NOT mr_toolbar IS INITIAL.

* add separator to toolbar
  IF p_mod = 'X'.
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = ''
        icon      = ''
        butn_type = cntb_btype_sep
        text      = ''
        quickinfo = 'Separador'.

* add Standard Button to toolbar (for Priority)
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = 'PRIO'
        icon      = icon_status_critical
        butn_type = cntb_btype_button
        text      = 'Prioridade'(002)
        quickinfo = 'Prioridade'(002).

* add Standard Button to toolbar (for Priority)
    IF lgnum EQ '150'.
      CALL METHOD mr_toolbar->add_button
        EXPORTING
          fcode     = 'USERADD'
          icon      = icon_system_user_menu
          butn_type = cntb_btype_button
          text      = 'Assignar'(071)
          quickinfo = 'Assignar'(071).
    ENDIF.
  ENDIF.

* add separator to toolbar
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = ''
      icon      = ''
      butn_type = cntb_btype_sep
      text      = ''
      quickinfo = 'Separador'.

  IF p_mod = 'X'.

** add Standard Button to toolbar (for Tipo de Carga)
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = 'CARGA'
*        icon      = icon_proposition
*        butn_type = cntb_btype_button
*        text      = 'Tipo de Carga'
*        quickinfo = 'Tipo de Carga'.
*
** add separator to toolbar
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = ''
*        icon      = ''
*        butn_type = cntb_btype_sep
*        text      = ''
*        quickinfo = 'Separador'.

* add Standard Button to toolbar (for Lock Total)
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = 'LOCK1'
        icon      = icon_unspecified_one
        butn_type = cntb_btype_button
        text      = 'Bloqueio Total'(003)
        quickinfo = 'Bloqueio Total'(003).

* add Standard Button to toolbar (for Unlock Picking)
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = 'LOCK2'
        icon      = icon_unspecified_two
        butn_type = cntb_btype_button
        text      = 'Picking'(004)
        quickinfo = 'Desbloquear Picking'(005).

* add Standard Button to toolbar (for Unlock Paletes Completas)
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = 'LOCK3'
        icon      = icon_unspecified_three
        butn_type = cntb_btype_button
        text      = 'Pal. Completas'(006)
        quickinfo = 'Desbloquear Paletes Completas'(007).

* add Standard Button to toolbar (for Unlock Pick & Pal. Completas)
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = 'LOCK4'
        icon      = icon_unspecified_four
        butn_type = cntb_btype_button
        text      = 'Picking+Pal.Comp.'(008)
        quickinfo = 'Desbl. Pick. e Pal. Completas'(009).

* add Standard Button to toolbar (for Unlock Carga)
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = 'LOCK5'
        icon      = icon_unspecified_five
        butn_type = cntb_btype_button
        text      = 'Carga'(010)
        quickinfo = 'Desbloquear Carga'(011).

* add Standard Button Load Car
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = 'LOADCAR'
        icon      = icon_ws_truck
        butn_type = cntb_btype_button
        text      = 'Carrega Carro'(070)
        quickinfo = 'Carrega Carro'(070).

* add separator to toolbar
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = ''
        icon      = ''
        butn_type = cntb_btype_sep
        text      = ''
        quickinfo = 'Separador'.

* add Standard Button to toolbar (for correct to's)
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = 'TO_PAR'
        icon      = icon_generate
        butn_type = cntb_btype_button
        text      = 'OT Partida'(012)
        quickinfo = 'OT Partida'(012).

* add Standard Button to toolbar (for correct to's)
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = 'TO_PKL'
        icon      = icon_generate
        butn_type = cntb_btype_button
        text      = 'OT PKL'(074)
        quickinfo = 'OT PKL'(074).

* add separator to toolbar
    CALL METHOD mr_toolbar->add_button
      EXPORTING
        fcode     = ''
        icon      = ''
        butn_type = cntb_btype_sep
        text      = ''
        quickinfo = 'Separador'.
  ENDIF.

* add Standard Button to toolbar (for Print Paletização Especial)
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = 'PRINT_PAL'
      icon      = icon_print
      butn_type = cntb_btype_button
      text      = 'Pal. Especial'(013)
      quickinfo = 'Imprime Paletização Especial'(014).

* set event-handler for toolbar-control
  CREATE OBJECT toolbar_event_receiver.
  SET HANDLER toolbar_event_receiver->on_function_selected
                                                      FOR mr_toolbar.
*  set handler toolbar_event_receiver->on_toolbar_dropdown
*                                                      for mr_toolbar.

ENDFORM.                               " change_toolbar

*&--------------------------------------------------------------------*
*&      Form  cria_hierarquia
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM cria_hierarquia.

  CALL METHOD tree1->delete_all_nodes.

* create hierarchy
  PERFORM create_hierarchy.

* this method must be called to send the data to the frontend
*  call method tree1->frontend_update.

* Expand first level
  CALL METHOD tree1->expand_node
    EXPORTING
      i_node_key          = g_top_node_key
    EXCEPTIONS
      failed              = 1
      illegal_level_count = 2
      cntl_system_error   = 3
      node_not_found      = 4
      cannot_expand_leaf  = 5.

* Determina top_node
  CALL METHOD tree1->get_top_node
    IMPORTING
      e_node_key = g_top_node.

* adjust column_width
  CALL METHOD tree1->column_optimize.


*  check not reg_ger is initial.

*  clear g_selected_node.
*  loop at gt_out into gs_out.
*    if gs_out-refnr   = reg_ger-refnr   and
*       gs_out-ordem   = reg_ger-ordem   and
*       gs_out-remessa = reg_ger-remessa and
*       gs_out-sscc is initial.
*      write sy-tabix to g_selected_node.
*      g_selected_node = g_top_node_key + g_selected_node - 1.
*      exit.
*    endif.
*  endloop.
*  check not g_selected_node is initial.
*
*  if not g_fieldname is initial.
*    call method tree1->set_selected_item
*      exporting
*        i_node_key  = g_selected_node
*        i_fieldname = g_fieldname.
*  else.
*    clear g_selected_nodes.
*    append g_selected_node to g_selected_nodes.
*    call method tree1->set_selected_nodes
*      exporting
*        it_selected_nodes = g_selected_nodes.
*  endif.

ENDFORM.                    " init_tree

*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM build_fieldcatalog.

  DATA: pos TYPE i VALUE 1.

  DATA: aux_cat TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE.

  REFRESH aux_cat.
  CLEAR tab_out.

*  CLEAR aux_cat.
*  aux_cat-col_pos       =  pos.
*  aux_cat-fieldname     = 'ALL'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
*  aux_cat-do_sum        = 'X'.
*  aux_cat-coltext       = 'Grupo Entrega'.
*  APPEND aux_cat.
*  ADD 1 TO pos.
*--------------organizar por Org. Vendas--------------------Mar2005
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ALL'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Org. Vendas'(015).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VKORG_OUT'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Org. Vendas'(015).
  APPEND aux_cat.
  ADD 1 TO pos.

*  CLEAR aux_cat.
*  aux_cat-col_pos       =  pos.
*  aux_cat-fieldname     = 'VKORG'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
*  aux_cat-do_sum        = 'X'.
*  aux_cat-coltext       = 'Org. Vendas'.
*  APPEND aux_cat.
*  ADD 1 TO pos.

*-------------------------------------------------------------------end
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'REFNR_OUT'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Grupo'(016).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VBELN_OUT'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Remessa'(017).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'TANUM'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Ordem Transf.'(018).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ICON_PALETE'.
*  aux_cat-do_sum         = 'X'.
  aux_cat-icon           = 'X'.
  aux_cat-coltext       = 'Pal.'.
  aux_cat-outputlen     =  4.
  aux_cat-tooltip       = 'Indicador do Tipo de Paletização'(019).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ICON_LOCK'.
*  aux_cat-do_sum         = 'X'.
  aux_cat-icon           = 'X'.
*------------------------------------------------------------com 17Mar
*  aux_cat-coltext       = 'Lock'.
  aux_cat-coltext       = 'Estado'.
*------------------------------------------------------------end

  aux_cat-outputlen     =  4.
  aux_cat-tooltip       = 'Indicador do Estado de Bloqueio'(020).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'DESC_LOCK'.
*  aux_cat-do_sum         = 'X'.
  aux_cat-coltext       = 'Estado'(021).
  aux_cat-outputlen     =  30.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       = pos.
  aux_cat-fieldname     = 'ICON_PORTARIA'.
  aux_cat-coltext       = 'Registo'(022).
  aux_cat-icon           = 'X'.
  aux_cat-outputlen     = 4.
  aux_cat-just          = 'C'.
  aux_cat-tooltip       = 'Indicador Carro Registado Portaria'(023).
*  aux_cat-edit          = 'X'.
*  aux_cat-emphasize     = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** RL -> INS 27.04.2005 -----------------------------------------------
  CLEAR aux_cat.
  aux_cat-col_pos       = pos.
  aux_cat-fieldname     = 'PRIORIDADE'.
  aux_cat-coltext       = 'Prioridade'(024).
  aux_cat-outputlen     = 10.
  aux_cat-just          = 'C'.
*  aux_cat-edit          = 'X'.
*  aux_cat-emphasize     = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.
** RL <- INS 27.04.2005 -----------------------------------------------

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VLTYP'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  10.
  aux_cat-coltext       = 'Dep.'(025).
  aux_cat-tooltip       = 'Tipo de Depósito'(026).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VLPLA'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     = 15.
  aux_cat-coltext       = 'Origem'(027).
  aux_cat-tooltip       = 'Posição de Origem'(028).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MATNR'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 18.
  aux_cat-coltext       = 'Material'(029).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MAKTX'.
  aux_cat-outputlen     =  40.
  aux_cat-coltext       = 'Descrição'(030).
  aux_cat-tooltip       = 'Descrição do Material'(031).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VSOLM'.
  aux_cat-do_sum         = 'X'.
  aux_cat-datatype       ='QUAN'.
  aux_cat-outputlen     =  18.
  aux_cat-coltext       = 'Quantidade'(032).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MEINS'.
  aux_cat-outputlen     =  6.
  aux_cat-coltext       = 'Unidade'(033).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'DESTINO'.
  aux_cat-outputlen     =  15.
  aux_cat-coltext       = 'Destino'(034).
  aux_cat-tooltip       = 'Posição de Destino'(035).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PUL_SIDE'.
  aux_cat-outputlen     =  15.
  aux_cat-coltext       = 'Zona Pulmão'(036).
  aux_cat-tooltip       = 'Zona Pulmão'(036).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'NUM_RECORRIDO'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  16.
  aux_cat-coltext       = 'Num Recolha'(037).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'STATUS'.
*  aux_cat-no_out        = 'X'.
  aux_cat-icon           = 'X'.
  aux_cat-outputlen     =  15.
  aux_cat-coltext       = 'Status Ordem'(038).
  aux_cat-tooltip       = 'Status da Ordem'(039).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'USER'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  10.
  aux_cat-coltext       = 'Utilizador'(040).
  APPEND aux_cat.
  ADD 1 TO pos.
*----------------------------------------------ins Mar2005
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'TOTAL_PALETES'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  2.
  aux_cat-coltext       = 'Total de Paletes'(041).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALETES_PULMAO'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  2.
  aux_cat-coltext       = 'Paletes no Pulmao'(042).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALETES_CARRO'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  2.
  aux_cat-coltext       = 'Paletes no Carro'(043).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALETES_DRI'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  6.
  aux_cat-coltext       = 'DRI Paletes'(044).
  APPEND aux_cat.
  ADD 1 TO pos.

  IF lgnum EQ '100'.
    CLEAR aux_cat.
    aux_cat-col_pos       =  pos.
    aux_cat-fieldname     = 'PALETES_TRI'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
    aux_cat-outputlen     =  6.
    aux_cat-coltext       = 'TRI Paletes'(045).
    APPEND aux_cat.
    ADD 1 TO pos.
  ELSEIF lgnum EQ '150'.
    CLEAR aux_cat.
    aux_cat-col_pos       =  pos.
    aux_cat-fieldname     = 'PALETES_AUT'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
    aux_cat-outputlen     =  6.
    aux_cat-coltext       = 'AUT Paletes'(069).
    APPEND aux_cat.
    ADD 1 TO pos.
  ENDIF.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALETES_PRM'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  6.
  aux_cat-coltext       = 'PRM Paletes'(046).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALETES_CRD'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  6.
  aux_cat-coltext       = 'CRD Paletes'(047).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALETES_PICKING'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  6.
  aux_cat-coltext       = 'Picking Paletes'(048).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PORTA'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  10.
  aux_cat-coltext       = 'Porta'(049).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'POSICAO_PULMAO'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-outputlen     =  10.
  aux_cat-coltext       = 'Posição Pulmão'(050).
  APPEND aux_cat.
  ADD 1 TO pos.

*---------------------------------------------------------
  gt_fieldcatalog[] = aux_cat[].

ENDFORM.                               " build_fieldcatalog

*&--------------------------------------------------------------------*
*&      Form  ajusta_propriedades
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM ajusta_propriedades. " TABLES sort     TYPE slis_t_sortinfo_alv.

  DATA ls_sort_wa TYPE lvc_s_sort.

* Ordenação
  REFRESH gt_sort.

* create sort-table
  ls_sort_wa-spos = 1.
  ls_sort_wa-fieldname = 'ALL'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  APPEND ls_sort_wa TO gt_sort.

* create sort-table
  ls_sort_wa-spos = 2.
  ls_sort_wa-fieldname = 'REFNR_OUT'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  APPEND ls_sort_wa TO gt_sort.

  ls_sort_wa-spos = 3.
  ls_sort_wa-fieldname = 'VBELN_OUT'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  APPEND ls_sort_wa TO gt_sort.

  ls_sort_wa-spos = 4.
  ls_sort_wa-fieldname = 'TANUM'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  ls_sort_wa-no_out = 'X'.
  APPEND ls_sort_wa TO gt_sort.
*
*  ls_sort_wa-spos = 5.
*  ls_sort_wa-fieldname = 'TAPOS'.
*  ls_sort_wa-up = 'X'.
*  APPEND ls_sort_wa TO gt_sort.

ENDFORM.                    " AJUSTA_PROPRIEDADES

*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
*       build hierarchy-header-information
*----------------------------------------------------------------------*
*      -->P_L_HIERARCHY_HEADER  strucxture for hierarchy-header
*----------------------------------------------------------------------*
FORM build_hierarchy_header CHANGING
                               p_hierarchy_header TYPE treev_hhdr.

*  p_hierarchy_header-heading =
*             'Grupo Entrega/Grupo/Remessa/Ordem Transf.'.
*  p_hierarchy_header-tooltip =
*             'Grupo Entrega/Grupo/Remessa/Ordem Transf.'.
*  p_hierarchy_header-width = 50.
*  p_hierarchy_header-width_pix = ' '.
*---------------------------------------------------------------
  p_hierarchy_header-heading =
             'Org. Vendas/Grupo Entrega/Grupo/Remessa/Ordem Transf.'(051).
  p_hierarchy_header-tooltip =
             'Org. Vendas/Grupo Entrega/Grupo/Remessa/Ordem Transf.'(051).
  p_hierarchy_header-width = 50.
  p_hierarchy_header-width_pix = ' '.

ENDFORM.                               " build_hierarchy_header

*&---------------------------------------------------------------------*
*&      Form  create_hierarchy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_hierarchy.

  DATA: ls_out  TYPE t_out,
        lsx_out TYPE t_out.

  DATA: l_all_key       TYPE lvc_nkey,
        l_refnr_out_key TYPE lvc_nkey,
        l_vkorg_key     TYPE lvc_nkey,
        l_refnr_key     TYPE lvc_nkey,
        l_servisan_key  TYPE lvc_nkey,
        l_vbeln_key     TYPE lvc_nkey,
        l_tanum_key     TYPE lvc_nkey,
        l_last_key      TYPE lvc_nkey.

  IF not_first_time = 'X'.
    PERFORM select_data.

    CHECK NOT all_tasks[] IS INITIAL.

    PERFORM tab_final.

    PERFORM nova_ordenacao.

    IF tab_out[] IS INITIAL.
*     Não existem dados para processar !
      MESSAGE s196(zwmmsg001).
*     Coloca apenas 1o Nível
      ls_out-all = 'Org. Vendas'(052).
      PERFORM add_all_line USING ls_out
                                 ''
                        CHANGING l_all_key.
      g_top_node = l_all_key.
    ENDIF.
  ENDIF.

** Não lista se não existirem dados
  CHECK NOT tab_out[] IS INITIAL.

  REFRESH gt_out.

  CLEAR: tab_out, ls_out, num_prio.

  SORT tab_out BY all vkorg refnr kunnr vbeln.

  LOOP AT tab_out INTO ls_out.

    AT NEW all.
*    on change of ls_out-all.
      PERFORM add_all_line USING ls_out
                                 ''
                        CHANGING l_all_key.
      g_top_node_key = l_all_key.
*    endon.
    ENDAT.
    lsx_out = ls_out.

    AT NEW vkorg.
      PERFORM add_vkorg_line USING lsx_out
                                   l_all_key
                          CHANGING l_vkorg_key.
    ENDAT.

    AT NEW refnr.
      PERFORM add_refnr_line USING lsx_out
                                   l_vkorg_key
                          CHANGING l_refnr_key.
    ENDAT.

    AT NEW kunnr.
      PERFORM add_servisan_line USING lsx_out
                                   l_refnr_key
                          CHANGING l_servisan_key.
    ENDAT.

    AT NEW vbeln.
      PERFORM add_vbeln_line USING lsx_out
                                   l_servisan_key
                          CHANGING l_vbeln_key.
    ENDAT.

    PERFORM add_complete_line USING ls_out
                                    l_vbeln_key
                           CHANGING l_last_key.
  ENDLOOP.


** precisamos de ter um num_prio mais abrangente do que só ao nivel do grupo(s) seleccionados.
  CLEAR: num_prio.

  DATA: lt_t311_x LIKE t311 OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE lt_t311_x
      FROM t311
      FOR ALL ENTRIES IN tab_out
          WHERE lgnum  = lgnum
            AND refnr = tab_out-refnr.
*    refnr IN refnr AND
*    refnt IN refnt AND
*    datum IN datum.

  SELECT  * INTO TABLE lt_zwm028 FROM  zwm028
  FOR ALL ENTRIES IN lt_t311_x
         WHERE  lgnum    = lgnum
         AND    refnr    = lt_t311_x-refnr
         AND    remessa  = space.

  LOOP AT  lt_zwm028 WHERE prioridade IS NOT INITIAL.
    IF lt_zwm028-total_paletes > lt_zwm028-paletes_carro AND
       lt_zwm028-total_paletes > lt_zwm028-paletes_pulmao.
      num_prio = num_prio + 1.
    ENDIF.
  ENDLOOP.


  not_first_time = 'X'.
ENDFORM.                    "create_hierarchy

*&--------------------------------------------------------------------*
*&      Form  add_all_line
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PS_OUT     text
*      -->P_RELAT_KEYtext
*      -->P_NODE_KEY text
*---------------------------------------------------------------------*
FORM add_all_line USING     ps_out TYPE t_out
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
*  ls_item_layout-t_image = '@3P@'.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
*  ls_item_layout-style   =
*                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

  MOVE ps_out-all TO ls_out-all.

** Paletes DRI, TRI, PRM e Picking
  CLEAR: paletes_dri, paletes_tri, paletes_prm, paletes_crd, paletes_aut.
  LOOP AT lt_paletes.
    paletes_dri = paletes_dri + lt_paletes-pal_dri.
    paletes_tri = paletes_tri + lt_paletes-pal_tri.
    paletes_prm = paletes_prm + lt_paletes-pal_prm.
    paletes_crd = paletes_crd + lt_paletes-pal_crd.
    paletes_aut = paletes_aut + lt_paletes-pal_aut.
  ENDLOOP.

  WRITE paletes_dri TO ls_out-paletes_dri DECIMALS 0.
  WRITE paletes_tri TO ls_out-paletes_tri DECIMALS 0.
  WRITE paletes_prm TO ls_out-paletes_prm DECIMALS 0.
  WRITE paletes_crd TO ls_out-paletes_crd DECIMALS 0.
  WRITE paletes_aut TO ls_out-paletes_aut DECIMALS 0.

  CLEAR lt_zwm026.
  REFRESH lt_zwm026.
  SELECT * INTO TABLE lt_zwm026
      FROM zwm026
        FOR ALL ENTRIES IN lt_paletes
            WHERE armazem = lgnum
              AND grupo = lt_paletes-refnr
              AND estado <> 'T'.
  IF NOT  lt_zwm026[] IS INITIAL.
    SORT lt_zwm026.
    DELETE ADJACENT DUPLICATES FROM lt_zwm026 COMPARING n_pal_picking.
    DESCRIBE TABLE lt_zwm026 LINES ls_out-paletes_picking.
  ENDIF.

  CLEAR: ls_out-prioridade.

* add node
  l_node_text =  ps_out-all.
  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    "add_all_line

*&--------------------------------------------------------------------*
*&      Form  add_refnr_line
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PS_OUT      text
*      -->P_RELAT_KEY text
*      -->P_NODE_KEY  text
*---------------------------------------------------------------------*
FORM add_refnr_line USING  ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        l_ctrl,
        ls_out      TYPE t_out.

  DATA: lv_special TYPE flag.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA lt_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  SORT lt_to_part BY refnr.
  READ TABLE lt_to_part WITH KEY refnr = ps_out-refnr
                        BINARY SEARCH.
  IF sy-subrc = 0.
    ls_item_layout-style   =
                       cl_gui_column_tree=>style_emphasized_c.
  ENDIF.

  APPEND ls_item_layout TO lt_item_layout.

  MOVE ps_out-all TO ls_out-all.

  MOVE ps_out-refnr TO ls_out-refnr.

  SORT lt_transportes BY refnr.
  READ TABLE lt_transportes WITH KEY refnr = ps_out-refnr
                            BINARY SEARCH.
  IF sy-subrc = 0.
    ls_out-icon_portaria = '@8P@'.
*    ICON_MESSAGE_INFORMATION_SM.
  ELSE.
    CLEAR ls_out-icon_portaria.
  ENDIF.
*  MOVE ps_out-zlock           TO ls_out-zlock.
*  MOVE ps_out-icon_lock       TO ls_out-icon_lock.
*  MOVE ps_out-desc_lock       TO ls_out-desc_lock.
  MOVE ps_out-palete_especial TO ls_out-palete_especial.
  CLEAR: ls_out-to_partida.
  MOVE ps_out-porta TO ls_out-porta.

  CLEAR: ls_out-total_paletes, ls_out-paletes_pulmao.

  CLEAR zwm028.
  SELECT SINGLE * FROM  zwm028
         WHERE  lgnum    = lgnum
         AND    refnr    = ps_out-refnr
         AND    remessa  = space.

  IF sy-subrc = 0.
    MOVE zwm028-zlock TO ls_out-zlock.

    PERFORM atribui_icon_lock_grupo USING zwm028-zlock
                                    CHANGING ls_out-icon_lock.

    PERFORM ler_texto_lock_grupo USING zwm028-zlock
                                 CHANGING ls_out-desc_lock.

    IF NOT zwm028-st_ppk IS INITIAL.
      CONCATENATE zwm028-st_ppk '+' zwm028-st_pul zwm028-pulmao1
                  zwm028-pulmao2
                  INTO ls_out-destino SEPARATED BY space.
    ELSE.
      ls_out-pul_side = zwm028-kober.

      CONCATENATE zwm028-st_pul zwm028-pulmao1 zwm028-pulmao2
                  INTO ls_out-destino SEPARATED BY space.
    ENDIF.

    MOVE zwm028-total_paletes  TO ls_out-total_paletes.
    MOVE zwm028-paletes_pulmao TO ls_out-paletes_pulmao.
    MOVE zwm028-paletes_carro  TO ls_out-paletes_carro.
    IF zwm028-st_dck = 'DCK'.
      l_ctrl = 'X'.
    ENDIF.
  ELSE.
    SELECT total_paletes INTO zwm028-total_paletes FROM  zwm028
           WHERE  lgnum    = lgnum
           AND    refnr    = ps_out-refnr.

      ADD zwm028-total_paletes TO ls_out-total_paletes.
      ADD zwm028-paletes_pulmao TO ls_out-paletes_pulmao.
    ENDSELECT.
  ENDIF.

** Paletes DRI, TRI, PRM e Picking

  CLEAR: paletes_dri, paletes_tri, paletes_prm, paletes_crd, paletes_aut.
  LOOP AT lt_paletes WHERE refnr = ps_out-refnr.
    paletes_dri = paletes_dri + lt_paletes-pal_dri.
    paletes_tri = paletes_tri + lt_paletes-pal_tri.
    paletes_prm = paletes_prm + lt_paletes-pal_prm.
    paletes_crd = paletes_crd + lt_paletes-pal_crd.
    paletes_aut = paletes_aut + lt_paletes-pal_aut.
  ENDLOOP.

  WRITE paletes_dri TO ls_out-paletes_dri DECIMALS 0.
  WRITE paletes_tri TO ls_out-paletes_tri DECIMALS 0.
  WRITE paletes_prm TO ls_out-paletes_prm DECIMALS 0.
  WRITE paletes_crd TO ls_out-paletes_crd DECIMALS 0.
  WRITE paletes_aut TO ls_out-paletes_aut DECIMALS 0.

  CLEAR lt_zwm026.
  REFRESH lt_zwm026.
  SELECT * INTO TABLE lt_zwm026
      FROM zwm026
          WHERE armazem = lgnum
            AND grupo = ps_out-refnr
            AND estado <> 'T'.
  IF NOT  lt_zwm026[] IS INITIAL.
    SORT lt_zwm026.
    DELETE ADJACENT DUPLICATES FROM lt_zwm026 COMPARING n_pal_picking.
    DESCRIBE TABLE lt_zwm026 LINES ls_out-paletes_picking.
  ENDIF.

  IF NOT zwm028-prioridade IS INITIAL.
    IF zwm028-total_paletes > zwm028-paletes_carro AND
       zwm028-total_paletes > zwm028-paletes_pulmao.
      num_prio = num_prio + 1.
    ENDIF.
  ENDIF.
  MOVE zwm028-prioridade TO ls_out-prioridade.

  MOVE zwm028-porta TO ls_out-porta.

  SORT lt_aux BY refnr.

  IF l_ctrl NE 'X'.
    READ TABLE lt_aux WITH KEY refnr = ls_out-refnr
                      BINARY SEARCH.
    IF sy-subrc EQ 0.
*      MOVE '@QD@' TO ls_out-icon_palete.
      lv_special = abap_true.
    ENDIF.

    IF lv_special EQ abap_false.
      IF ls_out-destino(3) = 'PUL' OR ls_out-destino(3) = 'PPK'.
        MOVE '@QC@' TO ls_out-icon_palete.
      ELSEIF ls_out-destino(3) = 'PLT'.
        ls_out-icon_palete = icon_ppe_pline.
      ELSEIF ls_out-destino(3) = 'PLM'.
        ls_out-icon_palete = icon_wf_workitem_completed.
      ENDIF.
    ELSE.
      IF ls_out-destino(3) = 'PUL' OR ls_out-destino(3) = 'PPK'.
        MOVE '@QD@' TO ls_out-icon_palete.
      ELSEIF ls_out-destino(3) = 'PLT'.
        ls_out-icon_palete = '@4T@'.
      ELSEIF ls_out-destino(3) = 'PLM'.
        ls_out-icon_palete = '@D3@'.
      ENDIF.
    ENDIF.

  ELSE.
    MOVE '@7Q@' TO ls_out-icon_palete.

    SELECT SINGLE *
      FROM zwm026
          WHERE armazem = lgnum
            AND grupo = ps_out-refnr.
*    IF sy-subrc = 0..
*      ls_out-icon_palete = '@K5@'.
*    ENDIF.

    READ TABLE lt_aux WITH KEY refnr = ls_out-refnr
                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-icon_palete = '@AD@'.
    ENDIF.
  ENDIF.



  CLEAR: l_ctrl.

* add node
  CLEAR l_node_text.
*  write ps_out-refnr to aux_refnr.
*  concatenate  aux_refnr '-' ps_out-refnt into l_node_text.

  MOVE ps_out-refnr_out TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    "add_refnr_out_line

*&--------------------------------------------------------------------*
*&      Form  add_vbeln_line
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PS_OUT     text
*      -->P_RELAT_KEYtext
*      -->P_NODE_KEY text
*---------------------------------------------------------------------*
FORM add_vbeln_line USING  ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  IF NOT ps_out-palete_especial IS INITIAL.
    MOVE '@QD@' TO ls_item_layout-t_image.
    ls_item_layout-style   =
                       cl_gui_column_tree=>style_emphasized_b.
  ELSE.
    MOVE '@QC@' TO ls_item_layout-t_image.
  ENDIF.
  APPEND ls_item_layout       TO lt_item_layout.

  CLEAR: ls_out-prioridade.

  MOVE ps_out-all             TO ls_out-all.
  MOVE ps_out-refnr           TO ls_out-refnr.

  MOVE ps_out-vbeln           TO ls_out-vbeln.

  MOVE ps_out-palete_especial TO ls_out-palete_especial.

*  MOVE ps_out-to_partida TO ls_out-to_partida.

  CLEAR: ls_out-total_paletes, ls_out-paletes_pulmao.

  CLEAR: zwm028-total_paletes, zwm028-paletes_pulmao.

  SELECT SINGLE * FROM  zwm028
         WHERE  lgnum    = lgnum
         AND    refnr    = ps_out-refnr
         AND    remessa  = ps_out-vbeln.

  MOVE zwm028-total_paletes  TO ls_out-total_paletes.
  MOVE zwm028-paletes_pulmao TO ls_out-paletes_pulmao.

  REFRESH lt_zwm026.
  CLEAR: lt_zwm026, paletes_dri, paletes_tri, paletes_prm, paletes_crd, paletes_aut.

  IF ps_out-servisan IS INITIAL.
** Paletes DRI, TRI, PRM e Picking
    LOOP AT lt_paletes WHERE refnr = zwm028-refnr
                         AND vbeln = zwm028-remessa.
      paletes_dri = paletes_dri + lt_paletes-pal_dri.
      paletes_tri = paletes_tri + lt_paletes-pal_tri.
      paletes_prm = paletes_prm + lt_paletes-pal_prm.
      paletes_crd = paletes_crd + lt_paletes-pal_crd.
      paletes_aut = paletes_aut + lt_paletes-pal_aut.
    ENDLOOP.

    WRITE paletes_dri TO ls_out-paletes_dri DECIMALS 0.
    WRITE paletes_tri TO ls_out-paletes_tri DECIMALS 0.
    WRITE paletes_prm TO ls_out-paletes_prm DECIMALS 0.
    WRITE paletes_crd TO ls_out-paletes_crd DECIMALS 0.
    WRITE paletes_aut TO ls_out-paletes_aut DECIMALS 0.

    SELECT * INTO TABLE lt_zwm026
        FROM zwm026
            WHERE armazem = lgnum
              AND grupo = ps_out-refnr
              AND remessa = ps_out-vbeln
              AND estado <> 'T'.

    IF NOT  lt_zwm026[] IS INITIAL.
      SORT lt_zwm026.
      DELETE ADJACENT DUPLICATES FROM lt_zwm026 COMPARING n_pal_picking.
      DESCRIBE TABLE lt_zwm026 LINES ls_out-paletes_picking.
    ENDIF.
  ENDIF.

  CLEAR ls_out-porta.

* add node
  CLEAR l_node_text.

  MOVE ls_out-vbeln TO l_node_text.

  CLEAR: ls_out-icon_lock,
         ls_out-desc_lock.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    "add_refnr_out_line

*&--------------------------------------------------------------------*
*&      Form  add_complete_line
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PS_OUT     text
*      -->P_RELAT_KEYtext
*      -->P_NODE_KEY text
*---------------------------------------------------------------------*
FORM add_complete_line USING   ps_out TYPE t_out
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
*  APPEND ls_item_layout TO lt_item_layout.

  IF NOT ps_out-to_partida IS INITIAL OR NOT ps_out-to_partida_pkl IS INITIAL.
    ls_item_layout-style   =
                       cl_gui_column_tree=>style_emphasized_c.
  ENDIF.

  APPEND ls_item_layout TO lt_item_layout.

  ls_out = ps_out.

  CLEAR: ls_out-prioridade.

  CLEAR: ls_out-icon_lock,
         ls_out-desc_lock,
         ls_out-total_paletes,
         ls_out-paletes_pulmao,
         ls_out-porta.

  l_node_text =  ps_out-tanum.
  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = ls_out
      i_node_text      = l_node_text
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                               " add_complete_line

*&---------------------------------------------------------------------*
*&      Form  register_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events.
* define the events which will be passed to the backend
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

* define the events which will be passed to the backend
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
*  append l_event to lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND l_event TO lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
*  append l_event to lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.

  CALL METHOD tree1->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
*    MESSAGE e074(zdialogrf).
  ENDIF.

* set Handler
  DATA: l_event_receiver TYPE REF TO lcl_tree_event_receiver.
  CREATE OBJECT l_event_receiver.

  SET HANDLER l_event_receiver->handle_item_double_click FOR tree1.

ENDFORM.                               " register_events

*&--------------------------------------------------------------------*
*&      Form  define_toolbar_excluding
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PT_TOOLBAR_textUDING
*---------------------------------------------------------------------*
FORM define_toolbar_excluding
  CHANGING pt_toolbar_excluding TYPE ui_functions.

*  append cl_alv_tree_base=>mc_fc_calculate
*    to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_calculate_avg
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_calculate_min
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_calculate_max
*      to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_calculate_sum
*      to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_expand
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_collapse
*      to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_col_optimize
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_col_invisible
*      to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_find
*      to pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_help
    TO pt_toolbar_excluding.

*  append cl_alv_tree_base=>mc_fc_current_variant
*    to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_load_variant
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_save_variant
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_maintain_variant
*      to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_print_back
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_print_back_all
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_print_prev
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_print_prev_all
*      to pt_toolbar_excluding.
ENDFORM.                               " define_toolbar_excluding

*&---------------------------------------------------------------------*
*&      Form  valida_parametros_entrada
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_parametros_entrada .

*  select single * from t300
*                    where
*                      lgnum = lgnum.
ENDFORM.                    "valida_parametros_entrada

*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       Cabecalho da ALV
*----------------------------------------------------------------------*
*      -->P_LT_LIST_COMMENTARY  text
*      -->P_L_LOGO  text
*----------------------------------------------------------------------*
FORM build_comment USING
      pt_list_commentary TYPE slis_t_listheader
      p_logo             TYPE sdydo_value.

  DATA: ls_line  TYPE slis_listheader,
        text(60),
        l_lines  LIKE sy-tabix,
        l_tabix  LIKE sy-tabix.

  CLEAR : text,
          aux_s_date.

* LIST HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = 'Monitor de Entregas'(t00).
  APPEND ls_line TO pt_list_commentary.

* STATUS LINE: TYPE S
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Armazém:'(t01).
  ls_line-info = lgnum.
  APPEND ls_line TO pt_list_commentary.

* Grupo: Inicio
  CLEAR ls_line.
  DESCRIBE TABLE refnr LINES l_lines.
  IF l_lines > 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Grupo(s):'(t02).
    CASE l_lines.
      WHEN 1.
        l_tabix = 1.
        PERFORM fill_title USING 'C'
                                 refnr-low
                                 refnr-high
                                 l_tabix
                        CHANGING ls_line-info.
      WHEN OTHERS.
        LOOP AT refnr.
          l_tabix = sy-tabix.
          PERFORM fill_title USING 'C'
                                   refnr-low
                                   refnr-high
                                   l_tabix
                          CHANGING ls_line-info.
        ENDLOOP.
    ENDCASE.
    APPEND ls_line TO pt_list_commentary.
  ENDIF.
* Grupo: Fim

* Descrição Grupo: Inicio
  CLEAR ls_line.
  DESCRIBE TABLE refnt LINES l_lines.
  IF l_lines > 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Descrição Grupo:'(t03).
    CASE l_lines.
      WHEN 1.
        l_tabix = 1.
        PERFORM fill_title USING 'C'
                                 refnt-low
                                 refnt-high
                                 l_tabix
                        CHANGING ls_line-info.
      WHEN OTHERS.
        LOOP AT refnt.
          l_tabix = sy-tabix.
          PERFORM fill_title USING 'C'
                                   refnt-low
                                   refnt-high
                                   l_tabix
                          CHANGING ls_line-info.
        ENDLOOP.
    ENDCASE.
    APPEND ls_line TO pt_list_commentary.
  ENDIF.
* Descrição Grupo: Fim

* Data: Inicio
  CLEAR ls_line.
  DESCRIBE TABLE datum LINES l_lines.
  IF l_lines > 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Data Criação da OT:'(t04).
    CASE l_lines.
      WHEN 1.
        l_tabix = 1.
        PERFORM fill_title USING 'D'
                                 datum-low
                                 datum-high
                                 l_tabix
                        CHANGING ls_line-info.
      WHEN OTHERS.
        LOOP AT datum.
          l_tabix = sy-tabix.
          PERFORM fill_title USING 'D'
                                   datum-low
                                   datum-high
                                   l_tabix
                          CHANGING ls_line-info.
        ENDLOOP.
    ENDCASE.
    APPEND ls_line TO pt_list_commentary.
  ENDIF.
* Data: Fim

* Indicador: Inicio
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Indicador Liberação:'(t05).
  IF rel = 'X'.
    ls_line-info = text-p02.
  ELSEIF notrel = 'X'.
    ls_line-info = text-p03.
  ELSE.
    ls_line-info = text-p04.
  ENDIF.
  APPEND ls_line TO pt_list_commentary.
* Indicador: Fim

* Indicador Em carga: Inicio
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Indicador Liberação:'(t07).
  IF p_curso = 'X'.
    ls_line-info = text-p05.
  ELSEIF p_concl = 'X'.
    ls_line-info = text-p06.
  ELSE.
    ls_line-info = text-p04.
  ENDIF.
  APPEND ls_line TO pt_list_commentary.
* Indicador Em carga: Fim


** ACTION LINE: TYPE A
  CLEAR ls_line.
  ls_line-typ  = 'A'.
  GET TIME.
  WRITE sy-datum TO aux_s_date.
* TEXT-T02 DATA DE EXECUÇÃO
  CONCATENATE text-t06 aux_s_date INTO text
                         SEPARATED BY space.
  ls_line-info = text.
  APPEND ls_line TO pt_list_commentary.

ENDFORM.                    "build_comment

*&--------------------------------------------------------------------*
*&      Form  fill_title
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_TIPO     text
*      -->F_LOW      text
*      -->F_HIGH     text
*      -->F_TABIX    text
*      -->F_INFO     text
*---------------------------------------------------------------------*
FORM fill_title USING f_tipo f_low f_high f_tabix f_info.

  DATA: l_text(60),
        l_text_low(60),
        l_text_high(60).

  DATA: l_len  TYPE i,
        l_offs TYPE i,
        l_ctrl TYPE i.

  CLEAR: l_text, l_text_low, l_text_high.
  IF f_low IS INITIAL.
    PERFORM trata_tipo USING f_tipo f_high
                    CHANGING l_text_high.
    CONCATENATE 'até' l_text_high INTO l_text SEPARATED BY space.
  ELSE.
    IF f_high IS INITIAL.
      PERFORM trata_tipo USING f_tipo f_low
                      CHANGING l_text_low.
      IF f_tabix = 1.
        MOVE l_text_low TO l_text.
      ELSE.
        CONCATENATE '/' l_text_low INTO l_text
                                    SEPARATED BY space.
      ENDIF.
    ELSE.
      PERFORM trata_tipo USING f_tipo f_low
                      CHANGING l_text_low.
      PERFORM trata_tipo USING f_tipo f_high
                      CHANGING l_text_high.
      IF f_tabix = 1.
        CONCATENATE 'de' l_text_low 'a' l_text_high INTO l_text
                                      SEPARATED BY space.
      ELSE.
        CONCATENATE '/ de' l_text_low 'a' l_text_high INTO l_text
                                      SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDIF.
  IF f_tabix = 1.
    MOVE l_text TO f_info.
  ELSE.
    l_offs = strlen( f_info ) + 1.
    l_len = strlen( l_text ).
    l_ctrl = l_offs + l_len.
    CHECK l_ctrl LE 60.
    MOVE l_text TO f_info+l_offs(l_len).
  ENDIF.
ENDFORM.                    "fill_title

*&--------------------------------------------------------------------*
*&      Form  trata_tipo
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_TYP      text
*      -->F_INPUT    text
*      -->F_OUTPUT   text
*---------------------------------------------------------------------*
FORM trata_tipo USING f_typ f_input f_output.
  CASE f_typ.
    WHEN 'D'.
      WRITE f_input TO f_output DD/MM/YYYY.
    WHEN 'C'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = f_input
        IMPORTING
          output = f_output.
    WHEN OTHERS.
      MOVE f_input TO f_output.
  ENDCASE.
ENDFORM.                    "trata_tipo

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  DATA: lv_2step  TYPE flag,
        lv_2spart TYPE flag.

  FREE all_tasks. CLEAR all_tasks.

** RL -> INS 06.04.2005 -----------------------------------------------
** Selecinar apenas os grupos
  REFRESH: itab_28, itab_serv, lt_t311, lt_zwm028, lt_t311a, r_id.
  CLEAR: itab_28, itab_serv, lt_t311, lt_zwm028, lt_t311a, r_id.

  CLEAR: gr_refnr,
         gr_refnr_2step,
         gr_refnr_2spart.


  SELECT * INTO TABLE lt_t311
      FROM t311
          WHERE lgnum  = lgnum AND
                refnr IN refnr AND
                refnt IN refnt AND
                datum IN datum.

  IF NOT lt_t311[] IS INITIAL.

    SELECT * INTO TABLE lt_t311a
        FROM t311a
            FOR ALL ENTRIES IN lt_t311
                WHERE lgnum = lt_t311-lgnum AND
                      refnr = lt_t311-refnr.

    SELECT * INTO TABLE itab_28
        FROM zwm028
            FOR ALL ENTRIES IN lt_t311
                WHERE lgnum = lt_t311-lgnum AND
                      refnr = lt_t311-refnr.

    DELETE itab_28 WHERE servisan NE 'X'.

  ENDIF.

** Get carros já registados na portaria
  CLEAR lt_transportes.
  REFRESH lt_transportes.
  LOOP AT lt_t311.
    CLEAR:   grupo, transporte.
    REFRESH: grupo, transporte.

    MOVE: 'I'           TO grupo-sign,
          'EQ'          TO grupo-option,
          lt_t311-refnr TO grupo-low.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 23.05.2012 11:32:50
*  Motivo: Valida tipo de Picking
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        is_t311  = lt_t311
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2step EQ abap_true AND
       lv_2spart EQ abap_false.
      APPEND grupo TO gr_refnr_2step.
    ELSEIF lv_2step EQ abap_true AND
           lv_2spart EQ abap_true.
      APPEND grupo TO gr_refnr_2spart.
    ELSE.
      APPEND grupo TO gr_refnr.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    APPEND grupo.

    CALL FUNCTION 'SD_SHIPMENT_FOR_DELGROUP'
      TABLES
        delgroup_i       = grupo
        shipment_o       = transporte
      EXCEPTIONS
        delgroup_i_empty = 1
        no_entries_found = 2
        OTHERS           = 3.

    CLEAR lt_zwm003.
    REFRESH lt_zwm003.
    SELECT SINGLE *
         FROM zwm003_aux
             WHERE armazem = lgnum
              AND n_transporte = transporte-low.

    IF sy-subrc = 0.
      CLEAR lt_transportes.
      lt_transportes-tknum = transporte-low.
      lt_transportes-refnr = lt_t311-refnr.
      APPEND lt_transportes.
    ELSE.
      SELECT SINGLE *
       FROM zwm006_aux
           WHERE armazem = lgnum
            AND n_transporte = transporte-low
            AND finalizada = ' '.
      IF sy-subrc = 0.
        CLEAR lt_transportes.
        lt_transportes-tknum = transporte-low.
        lt_transportes-refnr = lt_t311-refnr.
        APPEND lt_transportes.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF NOT itab_28[] IS INITIAL.
    r_id-sign = 'I'.
    r_id-option = 'EQ'.

    LOOP AT itab_28.
      r_id-low = itab_28-remessa.
      COLLECT r_id.
    ENDLOOP.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 23.05.2012 12:21:01
*  Motivo: Retorna OT's
*--------------------------------------------------------------------*
    PERFORM get_ots.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    all_tasks[] = itab_serv[].

** Obter os dados da tabela ZWM028
    LOOP AT all_tasks.
      CLEAR: zwm040.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 23.05.2012 12:00:52
*  Motivo: Picking 2 Passos
*--------------------------------------------------------------------*
      IF all_tasks-refnr IN gr_refnr_2step.
        SELECT SINGLE * FROM zwm040
        WHERE lgnum   EQ lgnum
          AND refnr   EQ all_tasks-refnr.

        all_tasks-pick_2step = abap_true.
      ELSE.
        SELECT SINGLE * FROM zwm040
        WHERE lgnum   EQ lgnum
          AND refnr   EQ all_tasks-refnr
          AND remessa EQ all_tasks-vbeln.

        all_tasks-pick_2step = abap_false.
      ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


      CLEAR: zwm028.
      SELECT SINGLE * FROM zwm028
      WHERE lgnum   EQ lgnum
        AND refnr   EQ all_tasks-refnr
        AND remessa EQ zwm040-id_servisan.

      CHECK sy-subrc EQ 0.

      all_tasks-st_pul     = zwm028-st_pul.
      all_tasks-pulmao1    = zwm028-pulmao1.
      all_tasks-pulmao2    = zwm028-pulmao2.
      all_tasks-kober      = zwm028-kober.
      all_tasks-st_ppk     = zwm028-st_ppk.
      all_tasks-pre_pick   = zwm028-pre_pick.
      all_tasks-st_dck     = zwm028-st_dck.
      all_tasks-porta      = zwm028-porta.
      all_tasks-zlock      = zwm028-zlock.
      all_tasks-prioridade = zwm028-prioridade.

      MODIFY all_tasks.
    ENDLOOP.
  ENDIF.
** RL <- INS 06.04.2005 -----------------------------------------------





  IF NOT gr_refnr_2spart IS INITIAL.
    SELECT a~lgnum a~tanum a~benum a~refnr a~kquit a~queue a~kgvnq
           b~refnt
           c~tapos c~vltyp c~vlpla c~matnr c~vsolm c~nsolm c~meins c~pquit
           c~qdatu c~qzeit c~qname c~pvqui c~edatu c~ezeit c~ename
           c~posty c~vlenr c~maktx c~posnr c~letyp c~vista
           d~st_pul d~pulmao1 d~pulmao2 d~st_ppk d~pre_pick d~st_dck
           d~porta d~zlock d~remessa
           d~prioridade d~paletes_carro
           e~num_recorrido f~vkorg e~sscc a~benum c~nltyp
    APPENDING CORRESPONDING FIELDS OF TABLE all_tasks
    FROM ( ( ( ( ( t311 AS b INNER JOIN ltak AS a
           ON b~lgnum = a~lgnum AND
              b~refnr = a~refnr )
               INNER JOIN ltap AS c
               ON c~lgnum = a~lgnum AND
                  c~tanum = a~tanum AND
                  c~vorga <> 'ST')
                  INNER JOIN zwm028 AS d
                  ON d~lgnum = a~lgnum AND
                     d~refnr = a~refnr AND
                     d~remessa = a~benum )
                     INNER JOIN likp AS f
                     ON f~vbeln = d~remessa )

                     LEFT OUTER JOIN zwm026 AS e
                     ON e~armazem = a~lgnum AND
                        e~grupo   = a~refnr AND
                        e~to_number = a~tanum )

    WHERE a~lgnum = lgnum  AND
          b~refnr IN gr_refnr_2spart AND
          b~refnt IN refnt AND
          b~datum IN datum AND
          f~vkorg IN vkorg AND
          b~kzdru IN released AND
          ( a~queue = picking_queue OR
            a~queue = tri_queue     OR
            a~queue = dri_queue     OR
            a~queue = picking_prm   OR
            a~queue = queue_crd ).

    LOOP AT all_tasks WHERE refnr IN gr_refnr_2spart.
      IF all_tasks-vltyp <> '815' AND all_tasks-nltyp EQ '916'.
        DELETE all_tasks INDEX sy-tabix.
        CONTINUE.
      ENDIF.

      all_tasks-vbeln = all_tasks-benum.
      MODIFY all_tasks INDEX sy-tabix.
    ENDLOOP.

  ELSE.
    MOVE: 'I'    TO grupo-sign,
          'EQ'   TO grupo-option,
          'NULL' TO grupo-low.

    APPEND grupo TO gr_refnr_2spart.
  ENDIF.



*--------------------------ins Mar2005 d~total_paletes d~paletes_pulmao
  SELECT a~lgnum a~tanum a~vbeln a~refnr a~kquit a~queue a~kgvnq
         b~refnt
        c~tapos c~vltyp c~vlpla c~matnr c~vsolm c~nsolm c~meins c~pquit
         c~qdatu c~qzeit c~qname c~pvqui c~edatu c~ezeit c~ename
         c~posty c~vlenr c~maktx c~posnr c~letyp c~vista
         d~st_pul d~pulmao1 d~pulmao2 d~st_ppk d~pre_pick d~st_dck
         d~porta d~zlock d~remessa
** RL -> INS 27.04.2005 -----------------------------------------------
         d~prioridade d~paletes_carro
** RL <- INS 27.04.2005 -----------------------------------------------
         e~num_recorrido f~vkorg e~sscc

** RL -> MOD 06.04.2005 -----------------------------------------------
*  INTO CORRESPONDING FIELDS OF TABLE all_tasks
  APPENDING CORRESPONDING FIELDS OF TABLE all_tasks
** RL <- MOD 06.04.2005 -----------------------------------------------

  FROM ( ( ( ( ( t311 AS b INNER JOIN ltak AS a
         ON b~lgnum = a~lgnum AND
            b~refnr = a~refnr )
             INNER JOIN ltap AS c
             ON c~lgnum = a~lgnum AND
                c~tanum = a~tanum AND
                c~vorga <> 'ST')
                INNER JOIN zwm028 AS d
                ON d~lgnum = a~lgnum AND
                   d~refnr = a~refnr AND
                   d~remessa = a~vbeln )
*----------------------------------------------------agrupar por vkorg
                   INNER JOIN likp AS f
                   ON f~vbeln = d~remessa )

                   LEFT OUTER JOIN zwm026 AS e
                   ON e~armazem = a~lgnum AND
                      e~grupo   = a~refnr AND
                      e~to_number = a~tanum )

  WHERE a~lgnum = lgnum  AND
        b~refnr IN refnr AND
        b~refnt IN refnt AND
        b~datum IN datum AND
        f~vkorg IN vkorg AND
        b~kzdru IN released AND
        ( a~queue = picking_queue OR
          a~queue = tri_queue     OR
          a~queue = dri_queue     OR
          a~queue = picking_prm   OR
          a~queue = queue_crd ).

*>>>>>>>> Begin of Insertion HP_212089 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
* TO items with POSTY = '1' are not deleted anymore from the database
* in case of two step confirmation during TO confirmation of first
* step. They should not be displayed. Therefor these entries are dele-
* ted from table TAP.
*  IF        t340-trtyp = con_anzeigen
*    AND NOT ltak-kgvnq IS INITIAL.
*    DELETE tap WHERE posty = con_posty_tb
*               AND   pquit = con_x.
*  ENDIF.
*>>>>>>>> End   of Insertion HP_212089 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  CLEAR t340.

  SELECT SINGLE * FROM t340 WHERE tcode = 'LT21'.

  IF t340-trtyp = 'A'.
    LOOP AT all_tasks  WHERE kgvnq = 'X'
                         AND posty = '1'
                         AND pquit = 'X'.
      DELETE all_tasks.
    ENDLOOP.
  ENDIF.

** Excluir consoante as opções de selecção
  IF rel = 'X'.
    IF p_curso = 'X'.
      DELETE all_tasks WHERE kquit EQ 'X'.
    ELSEIF p_concl = 'X'.
      DELETE all_tasks WHERE kquit NE 'X'.
    ENDIF.

  ENDIF.
*---------------------------------------------------end

ENDFORM.                    " select_data

*&--------------------------------------------------------------------*
*&      Form  tab_final
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM tab_final.
  TYPES: BEGIN OF lty_mat_pal_xp,
           kunnr TYPE kunnr,
           matnr TYPE matnr,
         END OF lty_mat_pal_xp.

  DATA: lt_t311a_calc  TYPE TABLE OF t311a,
        lt_t311a_calc2 TYPE TABLE OF t311a.

  DATA: aux_refnr LIKE t311-refnr,
        l_counter LIKE sy-tabix,
        l_kunnr   LIKE kna1-kunnr,
        l_adrnr   LIKE vbpa-adrnr,
        qtd_int   TYPE i.

  DATA: lt_zwm031     TYPE TABLE OF zwm031,
        lt_vbpa       TYPE TABLE OF vbpa,
        lt_zwm049     TYPE TABLE OF zwm049,
        ls_zwm049     TYPE zwm049,
        lt_mat_pal_xp TYPE TABLE OF lty_mat_pal_xp,
        ls_mat_pal_xp TYPE lty_mat_pal_xp.

  DATA: ls_vbpa TYPE vbpa.

  DATA: lv_2step   TYPE flag,
        lv_2spart  TYPE flag,
        lr_vbeln   TYPE RANGE OF vbeln,
        ls_r_vbeln LIKE LINE OF lr_vbeln,
        lv_meins   TYPE meins.

  DATA: ls_t311a TYPE t311a.

  FIELD-SYMBOLS: <ls_to_part> LIKE LINE OF lt_to_part.

  SORT all_tasks.
  CLEAR lt_paletes.
  REFRESH lt_paletes.
  CLEAR: lt_paletes, paletes_dri, paletes_tri, paletes_prm, paletes_crd, paletes_aut.

  SORT lt_t311a BY refnr.

  LOOP AT lt_t311.

    lt_t311a_calc = lt_t311a[].

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.06.2012 12:00:52
*  Motivo: Picking 2 Passos
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        is_t311  = lt_t311
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2step EQ abap_true AND
       lv_2spart EQ abap_false.
      DELETE ADJACENT DUPLICATES FROM lt_t311a_calc COMPARING refnr.
    ENDIF.

    LOOP AT lt_t311a_calc INTO ls_t311a WHERE refnr = lt_t311-refnr.

      CLEAR: lr_vbeln.
      IF lv_2step EQ abap_false OR
         lv_2spart EQ abap_true.
        ls_r_vbeln-low = ls_t311a-rbnum.
        ls_r_vbeln-sign   = 'I'.
        ls_r_vbeln-option = 'EQ'.
        APPEND ls_r_vbeln TO lr_vbeln.
      ENDIF.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


      LOOP AT all_tasks WHERE refnr = ls_t311a-refnr
                          AND vbeln IN lr_vbeln
                          AND pquit = ' '
                          AND pvqui = ' '.

        IF all_tasks-vltyp = 'DRI' OR
           all_tasks-vltyp = 'BLK' OR
           all_tasks-vltyp = 'TRI' OR
           all_tasks-vltyp = 'PRM' OR
           all_tasks-vltyp = 'CRD' OR
           all_tasks-vltyp = 'AUT'.

          CLEAR:  qtd_pal, qtd_res.
          SELECT SINGLE *
          FROM marm
          WHERE matnr = all_tasks-matnr AND
          meinh = 'PAL'.
          IF sy-subrc = 0.

            qtd_pal = marm-umrez / marm-umren.
            IF all_tasks-nsolm > all_tasks-vista.
              qtd_res = all_tasks-nsolm - all_tasks-vista.
            ELSE.
              qtd_res = all_tasks-nsolm.
            ENDIF.
            qtd_pal = qtd_res / qtd_pal.

            IF all_tasks-vltyp = 'DRI' OR
               all_tasks-vltyp = 'BLK' OR
               all_tasks-vltyp = 'TRI' OR
               all_tasks-vltyp = 'AUT'.
*              IF all_tasks-letyp = 'P2' OR all_tasks-letyp = 'P5'.
              IF z_wm_cl_management=>is_remontada( i_lgnum = all_tasks-lgnum i_letyp = all_tasks-letyp ) EQ abap_true.
                qtd_pal = qtd_pal / 2.
              ENDIF.
            ENDIF.
            IF all_tasks-vltyp = 'PRM'.
              CLEAR qtd_int.
              qtd_int = qtd_pal.
              qtd_pal = qtd_int.
            ENDIF.
            CLEAR lt_paletes.
            CASE all_tasks-vltyp.
              WHEN 'AUT'.
                paletes_aut = paletes_aut + qtd_pal.
              WHEN 'DRI' OR 'BLK'.
                paletes_dri = paletes_dri + qtd_pal.
              WHEN 'TRI'.
                paletes_tri = paletes_tri + qtd_pal.
              WHEN 'PRM'.
                paletes_prm = paletes_prm + qtd_pal.
              WHEN 'CRD'.
                paletes_crd = paletes_crd + qtd_pal.
            ENDCASE.

          ENDIF.
        ENDIF.
      ENDLOOP.
      lt_paletes-refnr = all_tasks-refnr.
      lt_paletes-vbeln = all_tasks-vbeln.
      lt_paletes-pal_dri = paletes_dri.
      lt_paletes-pal_tri = paletes_tri.
      lt_paletes-pal_prm = paletes_prm.
      lt_paletes-pal_crd = paletes_crd.
      lt_paletes-pal_aut = paletes_aut.
      COLLECT lt_paletes.
      CLEAR: lt_paletes, paletes_dri,
      paletes_tri, paletes_prm,
      paletes_crd, paletes_aut.
    ENDLOOP.
  ENDLOOP.


  CLEAR: total_itens, total_completed.

  FREE: itab_aux, itab_carga, tab_out, lt_aux, lt_to_part.
  CLEAR: itab_aux, itab_carga, tab_out, lt_aux, lt_to_part.

  CLEAR: l_index.

  LOOP AT all_tasks.

    PACK all_tasks-tanum TO tab_out-tanum.
    PACK all_tasks-tapos TO tab_out-tapos.
    PACK all_tasks-num_recorrido TO tab_out-num_recorrido.
    WRITE:
*          all_tasks-vbeln TO tab_out-vbeln,
           all_tasks-refnr TO tab_out-refnr,
           all_tasks-vltyp TO tab_out-vltyp,
           all_tasks-vlpla TO tab_out-vlpla,
           all_tasks-matnr TO tab_out-matnr,
           all_tasks-kzdru TO tab_out-kzdru,
           all_tasks-ename TO tab_out-user,
           all_tasks-zlock TO tab_out-zlock,
           all_tasks-prioridade TO tab_out-prioridade,
           'Org. de Vendas' TO tab_out-all,
           all_tasks-total_paletes  TO tab_out-total_paletes,
           all_tasks-paletes_pulmao TO tab_out-paletes_pulmao,
           all_tasks-porta TO tab_out-porta,
           all_tasks-vkorg TO tab_out-vkorg.

    MOVE all_tasks-vbeln TO tab_out-vbeln.
    MOVE all_tasks-posnr TO tab_out-posnr.

    PERFORM atribui_icon_lock_grupo
                    USING all_tasks-zlock
                 CHANGING tab_out-icon_lock.

    PERFORM ler_texto_lock_grupo
                    USING all_tasks-zlock
                 CHANGING tab_out-desc_lock.

    MOVE:  all_tasks-vsolm TO tab_out-vsolm,
           all_tasks-nsolm TO tab_out-nsolm,
           all_tasks-meins TO tab_out-meins,
           all_tasks-letyp TO tab_out-letyp.

    SELECT SINGLE * FROM  tvkot
           WHERE  spras  = sy-langu
           AND    vkorg  = all_tasks-vkorg.

    CONCATENATE  all_tasks-vkorg '-' tvkot-vtext INTO tab_out-vkorg_out.

    CLEAR aux_refnr.
    WRITE all_tasks-refnr TO aux_refnr.
    CONCATENATE  aux_refnr '-' all_tasks-refnt INTO tab_out-refnr_out.

    tab_out-maktx = all_tasks-maktx.

    IF NOT all_tasks-st_pul IS INITIAL.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 06.09.2012 16:06:44
*  Motivo: Lado do Pulmão
*--------------------------------------------------------------------*
      tab_out-pul_side = all_tasks-kober.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

      CONCATENATE all_tasks-st_pul all_tasks-pulmao1 all_tasks-pulmao2
                  INTO tab_out-destino SEPARATED BY space.
      IF NOT all_tasks-st_ppk IS INITIAL.

        IF all_tasks-vltyp = 'PCK' OR all_tasks-vltyp = 'PKB'.
          MOVE all_tasks-st_ppk TO tab_out-destino.
        ENDIF.
      ENDIF.
    ELSEIF NOT all_tasks-st_dck IS INITIAL.
      CONCATENATE all_tasks-st_dck all_tasks-porta
                      INTO tab_out-destino SEPARATED BY space.
    ENDIF.

** Verificar se existem to´s partidas das zonas de paletes completas.
    IF all_tasks-vltyp = 'DRI' OR all_tasks-vltyp = 'BLK' OR all_tasks-vltyp = 'TRI' OR
       all_tasks-vltyp = 'PRM' OR all_tasks-vltyp = 'AUT'.

      CLEAR qtd_pal.

      SELECT SINGLE *
          FROM marm
              WHERE matnr = all_tasks-matnr AND
                    meinh = 'PAL'.
      IF sy-subrc = 0.
        qtd_pal = marm-umrez / marm-umren.
        qtd_pal = all_tasks-nsolm MOD qtd_pal.
        IF qtd_pal NE 0.
*        IF qtd_pal <> all_tasks-vsolm.
          lt_to_part-refnr = all_tasks-refnr.
          lt_to_part-vbeln = all_tasks-vbeln.
          lt_to_part-tanum = all_tasks-tanum.
          lt_to_part-tapos = all_tasks-tapos.
          lt_to_part-to_partida = 'X'.
          APPEND lt_to_part.
          CLEAR lt_to_part.
        ENDIF.

      ENDIF.
    ENDIF.


    IF all_tasks-vltyp = 'DRI' OR all_tasks-vltyp = 'BLK' OR all_tasks-vltyp = 'TRI' OR
       all_tasks-vltyp = 'PRM' OR all_tasks-vltyp = 'AUT' OR
       all_tasks-vltyp = 'PCK' OR all_tasks-vltyp = 'PKB'.

      CLEAR qtd_pal.

      SELECT SINGLE meins FROM mara
                          INTO lv_meins
                          WHERE matnr =  all_tasks-matnr.

      SELECT SINGLE *
          FROM marm
              WHERE matnr = all_tasks-matnr AND
                    meinh = lv_meins.
      IF sy-subrc = 0.
        qtd_pal = marm-umrez / marm-umren.
        qtd_pal = all_tasks-nsolm MOD qtd_pal.
        IF qtd_pal <> 0.

          READ TABLE lt_to_part
            ASSIGNING <ls_to_part>
            WITH KEY refnr = all_tasks-refnr
                     vbeln = all_tasks-vbeln
                     tanum = all_tasks-tanum
                     tapos = all_tasks-tapos.

          IF sy-subrc <> 0.
            CLEAR: lt_to_part.
            lt_to_part-refnr = all_tasks-refnr.
            lt_to_part-vbeln = all_tasks-vbeln.
            lt_to_part-tanum = all_tasks-tanum.
            lt_to_part-tapos = all_tasks-tapos.
            APPEND lt_to_part ASSIGNING <ls_to_part>.
          ENDIF.

          CLEAR <ls_to_part>-to_partida.
          <ls_to_part>-to_partida_pkl = abap_true.

          CLEAR lt_to_part.
        ENDIF.

      ENDIF.
    ENDIF.

    IF all_tasks-pvqui = 'X' AND ( NOT all_tasks-vdifm IS INITIAL ).
      MOVE '@11@' TO tab_out-status.
    ELSEIF all_tasks-pquit = 'X'.
      MOVE '@08@' TO tab_out-status.
    ELSEIF all_tasks-pvqui = 'X' AND all_tasks-pquit = ' '.
      MOVE '@09@' TO tab_out-status.
      MOVE '1' TO  tab_out-pick.
      ADD 1 TO total_itens.
    ELSE.
      MOVE '@0A@' TO tab_out-status.
      MOVE '1' TO  tab_out-open.
      ADD 1 TO total_itens.
    ENDIF.

** DEL - BS 11.07.2006
** Alteração efectuado a pedido de Sr. Ferreira,
** a validação da paletização especial é feita SEMPRE
** pelo recebedor da mercadoria - tipo W1 ou WE
** REALIZADA MAIS ABAIXO
*    SELECT SINGLE kunnr INTO likp-kunnr
*        FROM likp
*            WHERE vbeln = all_tasks-vbeln.
*    IF sy-subrc = 0.
*      SELECT SINGLE * FROM  zwm031
*             WHERE lgnum  = all_tasks-lgnum
*               AND kunnr  = likp-kunnr
*               AND matnr  = all_tasks-matnr.
*      IF sy-subrc = 0.
*        tab_out-palete_especial = 'X'.
*        lt_aux-refnr           = all_tasks-refnr.
*        lt_aux-vbeln           = all_tasks-vbeln.
*        lt_aux-palete_especial = 'X'.
*        COLLECT lt_aux.
*      ENDIF.
*    ENDIF.
** DEL - BS 11.07.2006


*** RL -> INS
*** Obter Nome do Emissor da Ordem
*    CLEAR: vbpa, adrc, l_adrnr.
*    SELECT * FROM vbpa
*    WHERE vbeln EQ all_tasks-vbeln
*      AND parvw EQ 'AG'.
*      l_adrnr = vbpa-adrnr.
*** RL -> INS 06.04.2005 -----------------------------------------------
*      tab_out-kunnr = vbpa-kunnr.
*** RL <- INS 06.04.2005 -----------------------------------------------
*      EXIT.
*    ENDSELECT.
*
*    SELECT * FROM adrc
*    WHERE addrnumber EQ l_adrnr.
*      CONCATENATE  all_tasks-vbeln '-' adrc-name1
*              INTO tab_out-vbeln_out.
*** RL -> INS 06.04.2005 -----------------------------------------------
*      CLEAR: l_kunnr.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = tab_out-kunnr
*        IMPORTING
*          output = l_kunnr.
*
*      CONCATENATE l_kunnr '-' adrc-name1
*              INTO tab_out-serv_out.
*** RL <- INS 06.04.2005 -----------------------------------------------
*      EXIT.
*    ENDSELECT.

** INI - SG 17.11.2005
** Obter Nome do Emissor da Ordem
    CLEAR: vbpa, adrc, l_adrnr.
*  Alteração 2006.07.07 Ezequiel Ferreira a pedido
*  Gonçalo Guterres e após consulta Ricardo Lopes
*    SELECT SINGLE adrnr kunnr INTO (vbpa-adrnr, vbpa-kunnr)
*        FROM vbpa
*            WHERE vbeln EQ all_tasks-vbeln
*              AND posnr = '000000'
*              AND parvw EQ 'AG'.
*
*    l_adrnr = vbpa-adrnr.
*    tab_out-kunnr = vbpa-kunnr.
*

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 25.09.2012 14:42:59
*  Motivo: Cliente para grupo em 2 Passos
*--------------------------------------------------------------------*
    CLEAR: lr_vbeln, ls_r_vbeln.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = all_tasks-lgnum
        i_refnr  = all_tasks-refnr
        i_vbeln  = all_tasks-vbeln
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    CLEAR: lt_vbpa.

    IF lv_2step EQ abap_true AND
      lv_2spart EQ abap_false.

      lt_t311a_calc2 = lt_t311a[].
      DELETE lt_t311a_calc2 WHERE lgnum <> all_tasks-lgnum OR
                                  refnr <> all_tasks-refnr.

      IF NOT lt_t311a_calc2 IS INITIAL.
        SELECT * FROM vbpa
           INTO TABLE lt_vbpa
           FOR ALL ENTRIES IN lt_t311a_calc2
           WHERE vbeln = lt_t311a_calc2-rbnum AND
                 posnr = '000000' AND
                 ( parvw EQ 'W1' OR parvw EQ 'WE' ).
      ENDIF.

    ELSE.
      SELECT * FROM vbpa
         INTO TABLE lt_vbpa
              WHERE vbeln = all_tasks-vbeln AND
                    posnr = '000000' AND
                    parvw EQ 'W1'.

      IF sy-subrc <> 0.
        SELECT * FROM vbpa
           INTO TABLE lt_vbpa
           WHERE vbeln = all_tasks-vbeln AND
                 posnr = '000000' AND
                 parvw EQ 'WE'.
      ENDIF.

      READ TABLE lt_vbpa
            INTO ls_vbpa
            INDEX 1.

      vbpa-adrnr = ls_vbpa-adrnr.
      vbpa-kunnr = ls_vbpa-kunnr.

      l_adrnr = vbpa-adrnr.
      tab_out-kunnr = vbpa-kunnr.


*      SELECT SINGLE adrnr kunnr INTO (vbpa-adrnr, vbpa-kunnr)
*          FROM vbpa
*              WHERE vbeln = all_tasks-vbeln
*                AND posnr = '000000'
*                AND parvw EQ 'W1'.
*
*      IF sy-subrc EQ 0.
*        l_adrnr = vbpa-adrnr.
*        tab_out-kunnr = vbpa-kunnr.
*      ELSE.
*        SELECT SINGLE adrnr kunnr INTO (vbpa-adrnr, vbpa-kunnr)
*            FROM vbpa
*                WHERE vbeln = all_tasks-vbeln
*                  AND posnr = '000000'
*                  AND parvw EQ 'WE'.
*
*        l_adrnr = vbpa-adrnr.
*        tab_out-kunnr = vbpa-kunnr.
*      ENDIF.
    ENDIF.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


** INS BS - 11.07.2006
** Alteração efectuado a pedido de Sr. Ferreira,
** a validação da paletização especial é feita SEMPRE
** pelo recebedor da mercadoria - tipo W1 ou WE
*    SELECT SINGLE * FROM  zwm031
*           WHERE lgnum  = all_tasks-lgnum
*             AND kunnr  = tab_out-kunnr
*             AND matnr  = all_tasks-matnr.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 25.09.2012 16:29:04
*  Motivo: Material Por Cliente
*--------------------------------------------------------------------*
    SELECT * FROM  zwm031
       INTO TABLE lt_zwm031
        FOR ALL ENTRIES IN lt_vbpa
        WHERE lgnum  = all_tasks-lgnum AND
              kunnr  = lt_vbpa-kunnr AND
              matnr  = all_tasks-matnr.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    IF NOT lt_zwm031 IS INITIAL.
      tab_out-palete_especial = 'X'.
      lt_aux-refnr           = all_tasks-refnr.
      lt_aux-vbeln           = all_tasks-vbeln.
      lt_aux-palete_especial = 'X'.
      COLLECT lt_aux.
    ELSE.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 25.09.2012 15:56:07
*  Motivo: Select Para Remessas
*--------------------------------------------------------------------*
      IF NOT lt_vbpa IS INITIAL.
        CLEAR: lt_zwm049.
        SELECT * FROM zwm049
           INTO TABLE lt_zwm049
           FOR ALL ENTRIES IN lt_vbpa
           WHERE lgnum  = all_tasks-lgnum AND
                 kunnr  = lt_vbpa-kunnr.

        IF sy-subrc EQ 0.

          LOOP AT lt_zwm049 INTO ls_zwm049 WHERE NOT paltxt IS INITIAL.
            EXIT.
          ENDLOOP.

          IF sy-subrc = 0.
            tab_out-palete_especial = 'X'.
            lt_aux-refnr           = all_tasks-refnr.
            lt_aux-vbeln           = all_tasks-vbeln.
            lt_aux-palete_especial = 'X'.
            COLLECT lt_aux.
          ENDIF.

        ENDIF.
      ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*      CLEAR: zwm049.
*      SELECT SINGLE * FROM  zwm049
*            WHERE lgnum  = all_tasks-lgnum
*              AND kunnr  = tab_out-kunnr.
*      IF sy-subrc = 0 AND zwm049-paltxt IS NOT INITIAL.
*        tab_out-palete_especial = 'X'.
*        lt_aux-refnr           = all_tasks-refnr.
*        lt_aux-vbeln           = all_tasks-vbeln.
*        lt_aux-palete_especial = 'X'.
*        COLLECT lt_aux.
*      ENDIF.
    ENDIF.
** INS BS - 11.07.2006


*  Fim da alteração de 2006.07.07.
    SELECT SINGLE name1 INTO adrc-name1
        FROM adrc
            WHERE addrnumber EQ l_adrnr.

    WRITE all_tasks-vbeln TO tab_out-vbeln_out.
    CONCATENATE  tab_out-vbeln_out '-' adrc-name1
                  INTO tab_out-vbeln_out.

    CLEAR: l_kunnr.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.05.2012 16:31:04
*  Motivo: 2 Step picking - Multi Cliente
*--------------------------------------------------------------------*
    IF all_tasks-pick_2step EQ abap_true.

      tab_out-serv_out = '<< MULTI CLIENTE >>'(053).

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = tab_out-kunnr
        IMPORTING
          output = l_kunnr.

      CONCATENATE l_kunnr '-' adrc-name1
              INTO tab_out-serv_out.

    ENDIF.

** FIM - SG 17.11.2005

** Verificar se existe transporte para ordenação
    FREE: grupo, transporte.
    CLEAR: grupo, transporte.

    l_index = l_index + 1.
    tab_out-index = l_index.

    MOVE: 'I'             TO grupo-sign,
          'EQ'            TO grupo-option,
          tab_out-refnr   TO grupo-low.
    APPEND grupo.

    CALL FUNCTION 'SD_SHIPMENT_FOR_DELGROUP'
      TABLES
        delgroup_i       = grupo
        shipment_o       = transporte
      EXCEPTIONS
        delgroup_i_empty = 1
        no_entries_found = 2
        OTHERS           = 3.

    IF sy-subrc EQ 0.
      READ TABLE transporte INDEX 1.
      tab_out-transporte = transporte-low.

      CLEAR: zwm006_aux.
      SELECT SINGLE * FROM zwm006_aux
      WHERE armazem      EQ lgnum
        AND n_transporte EQ tab_out-transporte
        AND finalizada   EQ ' '.
*        EXIT.
*      ENDSELECT.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING zwm006_aux TO itab_aux.
      ELSE.
        itab_aux-mandt = sy-mandt.
        itab_aux-armazem = lgnum.
        itab_aux-n_transporte = tab_out-refnr.
      ENDIF.
    ELSE.
      itab_aux-mandt = sy-mandt.
      itab_aux-armazem = lgnum.
      itab_aux-n_transporte = tab_out-refnr.
    ENDIF.

    itab_aux-index = tab_out-index.
    APPEND itab_aux.
    CLEAR: itab_aux.
** RL <- INS
*-----------------------ins Mar2005-----------------selecção de posição
    IF all_tasks-pvqui NE space.
      IF all_tasks-vlenr NE space.
        SELECT posicao_pulmao INTO tab_out-posicao_pulmao FROM  zwm013
               WHERE  armazem  = lgnum
               AND    sscc     = all_tasks-vlenr.

        ENDSELECT.

      ELSEIF all_tasks-sscc NE space.

        SELECT posicao_pulmao INTO tab_out-posicao_pulmao FROM  zwm013
               WHERE  armazem  = lgnum
               AND    sscc     = all_tasks-sscc.

        ENDSELECT.

      ENDIF.
    ENDIF.

    IF sy-subrc NE 0.
      tab_out-posicao_pulmao = '--'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = all_tasks-meins
        language       = sy-langu
      IMPORTING
        output         = tab_out-meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 28.05.2012 16:38:56
*  Motivo: Remessa 2 Step Picking
*--------------------------------------------------------------------*
    IF all_tasks-pick_2step EQ abap_true.
      tab_out-vbeln = gc_multi_vbeln_txt.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*----------------------------------------------------------------------
    APPEND tab_out.
    CLEAR tab_out.

  ENDLOOP.

  SORT tab_out BY all vkorg refnr vbeln.

  SORT lt_aux BY vbeln.

  SORT itab_serv BY vkorg lgnum tanum vbeln refnr.

  SORT: lt_to_part, lt_paletes.
  CLEAR: l_index.

  FREE: itab_serv_aux.
  CLEAR: itab_serv_aux, l_counter.

  LOOP AT tab_out.
    l_index = sy-tabix.

    READ TABLE lt_aux WITH KEY vbeln = tab_out-vbeln
                               refnr = tab_out-refnr
                      BINARY SEARCH.
    IF sy-subrc = 0.
      tab_out-palete_especial = 'X'.
    ENDIF.

    IF all_tasks-pick_2step EQ abap_true.
      READ TABLE lt_to_part WITH KEY refnr = tab_out-refnr
                                     tanum = tab_out-tanum
                                     tapos = tab_out-tapos
                            BINARY SEARCH.
    ELSE.
      READ TABLE lt_to_part WITH KEY refnr = tab_out-refnr
                                     vbeln = tab_out-vbeln
                                     tanum = tab_out-tanum
                                     tapos = tab_out-tapos
                            BINARY SEARCH.
    ENDIF.

    IF sy-subrc = 0.
      tab_out-to_partida = lt_to_part-to_partida.
      tab_out-to_partida_pkl = lt_to_part-to_partida_pkl.
      IF tab_out-to_partida_pkl EQ abap_true.
        tab_out-to_partida = abap_false.
      ENDIF.
    ENDIF.

    READ TABLE itab_serv WITH KEY vkorg = tab_out-vkorg
                                  lgnum = lgnum
                                  tanum = tab_out-tanum
                                  vbeln = tab_out-vbeln
                                  refnr = tab_out-refnr
                         BINARY SEARCH.
    IF sy-subrc EQ 0.
** Remessa Servisan
      READ TABLE itab_serv_aux WITH KEY refnr = tab_out-refnr
                                        kunnr = tab_out-kunnr
                               BINARY SEARCH.
      IF sy-subrc NE 0.
        l_counter = l_counter + 1.
        tab_out-servisan = l_counter.

        itab_serv_aux-refnr = tab_out-refnr.
        itab_serv_aux-kunnr = tab_out-kunnr.
        itab_serv_aux-servisan = 1.
        COLLECT itab_serv_aux.
        CLEAR: itab_serv_aux.
        SORT: itab_serv_aux.
      ELSE.
        tab_out-servisan = l_counter.
      ENDIF.
    ENDIF.

    MODIFY tab_out INDEX l_index.

    AT END OF refnr.
      FREE: itab_serv_aux.
      CLEAR: itab_serv_aux, l_counter.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " tab_final

*---------------------------------------------------------------------*
*       FORM GET_PARAMETER                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(MODULE)                                                 *
*  -->  VALUE(PARAM)                                                  *
*  -->  VALOR                                                         *
*---------------------------------------------------------------------*
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
  READ TABLE ti_zwm001 WITH KEY armazem   = whs
                                processo  = module
                                parametro = param
                                BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE ti_zwm001 TO zwm001.
  ENDIF.
  MOVE zwm001-valor TO valor.

ENDFORM.                    " GET_PARAMETER


*&---------------------------------------------------------------------*
*&      Form  constroi_ranges
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM constroi_ranges.

* range para o release
  IF rel = 'X'.
    MOVE: 'I'   TO released-sign,
          'EQ'  TO released-option,
          'X'   TO released-low.
    APPEND released.
  ELSEIF notrel = 'X'.
    MOVE: 'I'   TO released-sign,
          'EQ'  TO released-option,
          ' '   TO released-low.
    APPEND released.
  ELSE. "both
    MOVE: 'I'   TO released-sign,
          'EQ'  TO released-option,
          'X'   TO released-low.
    APPEND released.

    MOVE: 'I'   TO released-sign,
          'EQ'  TO released-option,
          ' '   TO released-low.
    APPEND released.
  ENDIF.

ENDFORM.                    " constroi_ranges


*&--------------------------------------------------------------------*
*&      Form  event_double_click
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM event_double_click.
  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname.

  DATA: l_index         TYPE lvc_nkey.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  CASE l_item_name.
    WHEN '&Hierarchy'.
      READ TABLE gt_out INTO gs_out INDEX l_index.
      CHECK sy-subrc = 0.

      IF NOT gs_out-tanum  IS INITIAL.
        SET PARAMETER ID 'TAN' FIELD gs_out-tanum.
        CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.

      ELSEIF NOT gs_out-vbeln IS INITIAL.
        CHECK gs_out-vbeln <> gc_multi_vbeln_txt.

        SET PARAMETER ID 'VL' FIELD gs_out-vbeln.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

      ELSEIF NOT gs_out-refnr IS INITIAL.

        SET PARAMETER ID 'GRN' FIELD gs_out-refnr.
        CALL TRANSACTION 'VG03' AND SKIP FIRST SCREEN.

      ENDIF.
*------------------------------------------------------COM 17Mar
***      CHECK NOT gs_out-vbeln IS INITIAL.
***      SET PARAMETER ID 'VL' FIELD gs_out-vbeln.
***      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
*------------------------------------------------------end
*    when 'LOCK_GRUPO' or 'LOCKED_OUT'.
*      read table gt_out into gs_out index l_index.
*      check sy-subrc = 0.
*      check not gs_out-lock_grupo is initial.
*      g_flag_lock = 'X'.
*      move gs_out-refnr      to ecran-grupo.
*      move gs_out-lock_grupo to ecran-lock_grupo.
*      move gs_out-prioridade to ecran-prioridade.
*      perform ler_texto_lock_grupo
*                       using ecran-lock_grupo
*                    changing dd07d-ddtext.
*      clear g_flag_change.
*      call screen '0200' starting at 10 5  ending at 60 12.
*      if not g_flag_change is initial.
*        call method tree1->change_node
*          exporting
*            i_node_key    = l_selected_node
*            i_outtab_line = gs_out.
**       this method must be called to send the data to the frontend
*        call method tree1->frontend_update.
*      endif.
*    when 'PRIORIDADE'.
*      read table gt_out into gs_out index l_index.
*      check sy-subrc = 0.
*      check not gs_out-prioridade is initial.
*      clear g_flag_lock.
*      move gs_out-refnr      to ecran-grupo.
*      move gs_out-lock_grupo to ecran-lock_grupo.
*      move gs_out-prioridade to ecran-prioridade.
*      perform ler_texto_lock_grupo
*                       using ecran-lock_grupo
*                    changing dd07d-ddtext.
*      clear g_flag_change.
*      call screen '0200' starting at 10 5  ending at 60 12.
*      if not g_flag_change is initial.
*        call method tree1->change_node
*          exporting
*            i_node_key    = l_selected_node
*            i_outtab_line = gs_out.
**       this method must be called to send the data to the frontend
*        call method tree1->frontend_update.
*      endif.
    WHEN 'TANUM'.
      READ TABLE gt_out INTO gs_out INDEX l_index.
      CHECK sy-subrc = 0.
      CHECK NOT gs_out-tanum IS INITIAL.
      SET PARAMETER ID 'TAN' FIELD gs_out-tanum.
      SET PARAMETER ID 'TAP' FIELD ' '.
      SET PARAMETER ID 'LGN' FIELD lgnum.
      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.
    WHEN 'TAPOS'.
      READ TABLE gt_out INTO gs_out INDEX l_index.
      CHECK sy-subrc = 0.
      CHECK NOT gs_out-tapos IS INITIAL.
      SET PARAMETER ID 'TAN' FIELD gs_out-tanum.
      SET PARAMETER ID 'TAP' FIELD gs_out-tapos.
      SET PARAMETER ID 'LGN' FIELD lgnum.
      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.
    WHEN 'ORIGEM'.
*      read table gt_out into gs_out index l_index.
*      check sy-subrc = 0.
*      check not gs_out-origem is initial.
*      set parameter id 'LGP' field gs_out-origem+4(10).
*      set parameter id 'LGN' field lgnum.
*      set parameter id 'LGT' field gs_out-origem(3).
*      call transaction 'LS03N' and skip first screen.
    WHEN 'MATNR'.
      READ TABLE gt_out INTO gs_out INDEX l_index.
      CHECK sy-subrc = 0.
      CHECK NOT gs_out-matnr IS INITIAL.
      SET PARAMETER ID 'MAT' FIELD gs_out-matnr.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.                    "event_double_click

*&--------------------------------------------------------------------*
*&      Form  altera_lock
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_LOCK     text
*---------------------------------------------------------------------*
FORM altera_lock USING f_lock.
  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname.

  DATA: l_index TYPE lvc_nkey.

  DATA: l_subrc LIKE sy-subrc.

  DATA: lv_2chang TYPE flag.

  CLEAR:  st_destino, st_destino2, bin_destino1,
          bin_kober1, bin_destino2, ok_code_0400,
          texto0400.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  READ TABLE gt_out INTO gs_out INDEX l_index.
  CHECK sy-subrc = 0.
  CHECK NOT gs_out-zlock IS INITIAL.

  CHECK f_lock <> gs_out-zlock.

  READ TABLE lt_to_part WITH KEY refnr = gs_out-refnr
                        BINARY SEARCH.
  IF sy-subrc = 0.
*  IF NOT gs_out-to_partida IS INITIAL.
*   Não pode desbloquear pq existem OT´s partidas
    MESSAGE i222(zwmmsg001).
    EXIT.
  ENDIF.

  IF f_lock < gs_out-zlock.
*   Não pode colocar um estado inferior ao existente
    MESSAGE i155(zwmmsg001).
    EXIT.
  ENDIF.
** Não permitir passar do lock 2 para o 3
  IF gs_out-zlock = 2 AND f_lock = 3.
    MESSAGE i209(zwmmsg001).
    EXIT.
  ENDIF.
** Não permitir passar do lock 1 para o 5
  IF gs_out-zlock = 1 AND f_lock = 5.
    MESSAGE i209(zwmmsg001).
    EXIT.
  ENDIF.

  gs_out-zlock = f_lock.

  g_lock  = f_lock.
  g_grupo = gs_out-refnr.

  CLEAR l_subrc.
  PERFORM check_grupo USING g_grupo l_subrc.
  CHECK l_subrc IS INITIAL.

  CLEAR: g_ok_code, ok_code_0400.

  CLEAR caract_carga.
  PERFORM get_caract_carga.


  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = lgnum
      i_refnr  = g_grupo
    IMPORTING
      e_2chang = lv_2chang
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.



  IF zwm028-st_pul IS INITIAL.

    IF g_lock = 2.
      CHECK zwm028-st_ppk IS INITIAL.
    ENDIF.

    IF g_lock = 5.
      CHECK zwm028-st_dck IS INITIAL.
    ENDIF.

    IF NOT lt_zwm026[] IS INITIAL.
      CLEAR texto0400.
    ELSE.
      texto0400 = 'Característica de DCK!'.
      CLEAR caract_carga.
    ENDIF.

    IF lv_2chang EQ abap_true.
      texto0400 = 'Agrupamento de Remessa!'.
    ENDIF.

    CALL SCREEN '0400' STARTING AT 10 1 ENDING AT 61 12.
    CHECK g_ok_code = 'CONFIRMAR'.
  ELSE.
    IF g_lock = 5.
      CHECK zwm028-st_dck IS INITIAL.
    ENDIF.

    CALL SCREEN '0400' STARTING AT 10 1 ENDING AT 61 12.
    CHECK g_ok_code = 'CONFIRMAR'.
  ENDIF.

*  CALL SCREEN '0200' STARTING AT 10 1 ENDING AT 61 12.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'G_PORTARIA'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

*  CHECK g_ok_code = 'CONFIRMAR'.

  PERFORM atribui_icon_lock_grupo
                   USING f_lock
                CHANGING gs_out-icon_lock.

  PERFORM ler_texto_lock_grupo
                  USING f_lock
               CHANGING gs_out-desc_lock.

  CALL METHOD tree1->change_node
    EXPORTING
      i_node_key    = l_selected_node
      i_outtab_line = gs_out.

* this method must be called to send the data to the frontend
  CALL METHOD tree1->frontend_update.

ENDFORM.                    "altera_picking

*&--------------------------------------------------------------------*
*&      Form  atribui_icon_lock_grupo
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_LOCK     text
*      -->F_OUT      text
*---------------------------------------------------------------------*
FORM atribui_icon_lock_grupo USING f_lock f_out.
  CASE f_lock.
    WHEN 1.
      MOVE icon_unspecified_one   TO f_out.
    WHEN 2.
      MOVE icon_unspecified_two   TO f_out.
    WHEN 3.
      MOVE icon_unspecified_three TO f_out.
    WHEN 4.
      MOVE icon_unspecified_four  TO f_out.
    WHEN 5.
      MOVE icon_unspecified_five  TO f_out.
  ENDCASE.
ENDFORM.                    "atribui_icon_lock_grupo

*&--------------------------------------------------------------------*
*&      Form  ler_texto_lock_grupo
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_LOCK     text
*      -->F_TEXTO    text
*---------------------------------------------------------------------*
FORM ler_texto_lock_grupo USING f_lock f_texto.

  DATA: l_texto LIKE dd07v-ddtext,
        l_value LIKE dd07v-domvalue_l.

  CLEAR l_texto.

  l_value = f_lock.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = 'ZLOCK'
      i_domvalue = l_value
    IMPORTING
      e_ddtext   = l_texto
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  f_texto = l_texto.
ENDFORM.                    "ler_texto_ddic

*&--------------------------------------------------------------------*
*&      Form  print_pal_especial
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM print_pal_especial.
  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname.

  DATA: l_index         TYPE lvc_nkey.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  READ TABLE gt_out INTO gs_out INDEX l_index.
  CHECK sy-subrc = 0.
  CHECK NOT gs_out-refnr IS INITIAL.
*  Possibilitar imprimir sem ITEMS - apenas com dados de cabeçalho
*  CHECK NOT gs_out-palete_especial IS INITIAL.

  SUBMIT zwmrep0030
        WITH p_lgnum = lgnum
        WITH p_refnr = gs_out-refnr
                    AND RETURN.

ENDFORM.                    "print_pal_especial

*---------------------------------------------------------------------*
*       FORM CONFIRM_STEP                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_LINE1                                                       *
*  -->  F_LINE2                                                       *
*  -->  F_TITLE                                                       *
*  -->  F_ANSWER                                                      *
*---------------------------------------------------------------------*
FORM confirm_step USING f_line1 f_line2 f_title f_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
*     defaultoption  = ' '
      textline1 = f_line1
      textline2 = f_line2
      titel     = f_title
    IMPORTING
      answer    = f_answer
    EXCEPTIONS
      OTHERS    = 1.
ENDFORM.                    "confirm_step

*---------------------------------------------------------------------*
*       FORM popup                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM popup USING f_diag1 f_diag2 f_diag3
                 f_line1 f_line2 f_title
                 f_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
    EXPORTING
      defaultoption  = ' '
      diagnosetext1  = f_diag1
      diagnosetext2  = f_diag2
      diagnosetext3  = f_diag3
      textline1      = f_line1
      textline2      = f_line2
      titel          = f_title
    IMPORTING
      answer         = f_answer
    EXCEPTIONS
      titel_too_long = 01.
ENDFORM.                    "popup
*&---------------------------------------------------------------------*
*&      Form  nova_ordenacao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nova_ordenacao .

  DATA: itab_carga LIKE zwm003_aux OCCURS 0 WITH HEADER LINE.

  DATA: return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  DATA: l_count LIKE sy-tabix.

  DATA: itab_dados LIKE tab_out OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF itab_indices OCCURS 0,
          index(10),
        END OF itab_indices.

** Fazer a ordenação respectiva
  LOOP AT itab_aux.
    MOVE-CORRESPONDING itab_aux TO itab_carga.
    APPEND itab_carga.
    CLEAR: itab_carga.
  ENDLOOP.

  CALL FUNCTION 'ZWM_SORT_QUEUE_V1'
    EXPORTING
      armazem    = lgnum
    TABLES
      return_msg = return_msg
      l_zwm003   = itab_carga.

  LOOP AT itab_carga.
    LOOP AT itab_aux WHERE armazem = itab_carga-armazem AND
                           num_entrada = itab_carga-num_entrada.
      itab_indices-index = itab_aux-index.
      COLLECT itab_indices.
      CLEAR: itab_indices.
    ENDLOOP.
  ENDLOOP.

  CLEAR: l_index, l_count.
  LOOP AT itab_indices.
    l_count = sy-tabix.
    l_index = itab_indices-index.
    READ TABLE tab_out INDEX l_index.
    CHECK sy-subrc EQ 0.
    MOVE-CORRESPONDING tab_out TO itab_dados.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = itab_dados-meins
        language       = sy-langu
      IMPORTING
        output         = itab_dados-meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    itab_dados-nova_ordenacao = l_count.
    APPEND itab_dados.
    CLEAR: itab_dados.
  ENDLOOP.

  SORT itab_dados BY nova_ordenacao.

  FREE: tab_out, itab_indices, itab_carga, itab_aux.
  CLEAR: tab_out, itab_indices, itab_carga, itab_aux.

  tab_out[] = itab_dados[].

  FREE: itab_dados.
  CLEAR: itab_dados.

ENDFORM.                    " nova_ordenacao
*&---------------------------------------------------------------------*
*&      Form  add_vkorg_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_ALL_KEY  text
*      <--P_L_REFNR_KEY  text
*----------------------------------------------------------------------*
FORM add_vkorg_line USING  ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  MOVE ps_out-all TO ls_out-all.

  CLEAR: ls_out-prioridade.
  CLEAR: ls_out-to_partida.

  MOVE ps_out-vkorg TO ls_out-vkorg.

  CLEAR l_node_text.

  MOVE ps_out-vkorg_out TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " add_vkorg_line
*&---------------------------------------------------------------------*
*&      Form  get_costumizing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_costumizing .

  CLEAR valor.

  PERFORM get_parameter
          USING lgnum
                'GESTAO_FILAS'
                'FILA_SAIDA_PCK'
                valor.
  MOVE valor TO picking_queue.
  CLEAR valor.

  PERFORM get_parameter
         USING lgnum
               'GESTAO_FILAS'
               'FILA_SAIDA_DRI'
               valor.
  MOVE valor TO dri_queue.
  CLEAR valor.

  PERFORM get_parameter
         USING lgnum
               'GESTAO_FILAS'
               'FILA_SAIDA_TRI'
               valor.
  MOVE valor TO tri_queue.
  CLEAR valor.

  PERFORM get_parameter
          USING lgnum
                'GESTAO_FILAS'
                'FILA_S_PAL_REMONTADA'
                valor.
  MOVE valor TO picking_prm.

  PERFORM get_parameter
          USING lgnum
                'GESTAO_FILAS'
                'FILA_SAIDA_CRD'
                valor.
  MOVE valor TO queue_crd.

ENDFORM.                    " get_costumizing
*&---------------------------------------------------------------------*
*&      Form  GET_USER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_user_data .
* Read the user data from table lrf_wkqu ( for all the warehouses)
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = xuser
    EXCEPTIONS
      no_entry_found = 01.
  IF sy-subrc = 0.
*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:33:59
*/ Apenas considerar deposito ativo
    SORT xuser[] BY statu.
    DELETE xuser[] WHERE statu NE abap_true.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:33:59

    READ TABLE xuser INDEX 1.
    MOVE xuser-lgnum TO lgnum.

*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:44:00
*/ Comparar valor por defeito contra introduzido
    gv_init_lgnum = lgnum.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 12:44:00
  ENDIF.

ENDFORM.                    " GET_USER_DATA
*&---------------------------------------------------------------------*
*&      Form  add_servisan_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_REFNR_KEY  text
*      <--P_L_SERVISAN_KEY  text
*----------------------------------------------------------------------*
FORM add_servisan_line  USING ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  IF NOT ps_out-servisan IS INITIAL.
*    MOVE '@M7@' TO ls_item_layout-t_image.
*  ELSE.
    MOVE '@PQ@' TO ls_item_layout-t_image.
*    ls_item_layout-style = cl_gui_column_tree=>style_emphasized_b.
  ENDIF.

  APPEND ls_item_layout       TO lt_item_layout.

* add node
  CLEAR l_node_text.

  CLEAR: ls_out-prioridade.

  MOVE ps_out-serv_out TO l_node_text.

  CLEAR: ls_out-icon_lock, ls_out-desc_lock.

  CLEAR: ls_out-total_paletes, ls_out-paletes_pulmao,
         ls_out-paletes_carro, ls_out-paletes_dri,
         ls_out-paletes_tri, ls_out-paletes_prm,
         ls_out-paletes_crd.

  CLEAR: paletes_dri, paletes_tri, paletes_prm, paletes_crd, paletes_aut.

  CLEAR lt_zwm026.
  REFRESH lt_zwm026.

  SELECT * FROM zwm028
         WHERE lgnum   EQ lgnum
           AND refnr   EQ ps_out-refnr
           AND remessa NE ' '
           AND emissor EQ ps_out-kunnr.
    ADD zwm028-total_paletes  TO ls_out-total_paletes.
    ADD zwm028-paletes_pulmao TO ls_out-paletes_pulmao.
    ADD zwm028-paletes_carro  TO ls_out-paletes_carro.

** Paletes DRI, TRI, PRM e Picking
    IF ls_out-vkorg <> 'SER1'.
      IF NOT ps_out-servisan IS INITIAL.

        SELECT * FROM zwm040
            WHERE lgnum       = lgnum
              AND id_servisan = zwm028-remessa
              AND refnr       = zwm028-refnr.

          LOOP AT lt_paletes WHERE refnr = zwm040-refnr
                               AND vbeln = zwm040-remessa.
            paletes_dri = paletes_dri + lt_paletes-pal_dri.
            paletes_tri = paletes_tri + lt_paletes-pal_tri.
            paletes_prm = paletes_prm + lt_paletes-pal_prm.
            paletes_crd = paletes_crd + lt_paletes-pal_crd.
            paletes_aut = paletes_aut + lt_paletes-pal_aut.
          ENDLOOP.

          SELECT * APPENDING TABLE lt_zwm026
              FROM zwm026
                  WHERE armazem = lgnum
                    AND grupo = ps_out-refnr
                    AND remessa = zwm040-remessa
                    AND estado <> 'T'.

          WRITE paletes_dri TO ls_out-paletes_dri DECIMALS 0.
          WRITE paletes_tri TO ls_out-paletes_tri DECIMALS 0.
          WRITE paletes_prm TO ls_out-paletes_prm DECIMALS 0.
          WRITE paletes_crd TO ls_out-paletes_crd DECIMALS 0.
          WRITE paletes_aut TO ls_out-paletes_aut DECIMALS 0.

          IF NOT  lt_zwm026[] IS INITIAL.
            SORT lt_zwm026.
            DELETE ADJACENT DUPLICATES FROM lt_zwm026 COMPARING n_pal_picking.
            DESCRIBE TABLE lt_zwm026 LINES ls_out-paletes_picking.
          ENDIF.
        ENDSELECT.
      ELSE.
        LOOP AT lt_paletes WHERE refnr = zwm028-refnr
                             AND vbeln = zwm028-remessa.
          paletes_dri = paletes_dri + lt_paletes-pal_dri.
          paletes_tri = paletes_tri + lt_paletes-pal_tri.
          paletes_prm = paletes_prm + lt_paletes-pal_prm.
          paletes_crd = paletes_crd + lt_paletes-pal_crd.
          paletes_aut = paletes_aut + lt_paletes-pal_aut.
        ENDLOOP.

        SELECT * APPENDING TABLE lt_zwm026
            FROM zwm026
                WHERE armazem = lgnum
                  AND grupo = ps_out-refnr
                  AND remessa = ps_out-vbeln
                  AND estado <> 'T'.


        WRITE paletes_dri TO ls_out-paletes_dri DECIMALS 0.
        WRITE paletes_tri TO ls_out-paletes_tri DECIMALS 0.
        WRITE paletes_prm TO ls_out-paletes_prm DECIMALS 0.
        WRITE paletes_crd TO ls_out-paletes_crd DECIMALS 0.
        WRITE paletes_aut TO ls_out-paletes_aut DECIMALS 0.

        IF NOT  lt_zwm026[] IS INITIAL.
          SORT lt_zwm026.
          DELETE ADJACENT DUPLICATES FROM lt_zwm026 COMPARING n_pal_picking.
          DESCRIBE TABLE lt_zwm026 LINES ls_out-paletes_picking.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDSELECT.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " add_servisan_line
*&---------------------------------------------------------------------*
*&      Form  altera_prioridade
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM altera_prioridade .

  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname,
        l_index         TYPE lvc_nkey,
        l_subrc         LIKE sy-subrc.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  READ TABLE gt_out INTO gs_out INDEX l_index.
  CHECK sy-subrc = 0.

  IF     gs_out-status IS INITIAL AND
     NOT gs_out-zlock  IS INITIAL AND
     NOT gs_out-refnr  IS INITIAL.

    IF gs_out-zlock NE '5'.
      g_grupo = gs_out-refnr.

      CLEAR l_subrc.
      PERFORM check_grupo USING g_grupo l_subrc.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'G_PORTARIA'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CHECK l_subrc IS INITIAL.

      IF gs_out-prioridade = 0.
        CLEAR l_subrc.
        PERFORM check_paletes_producao USING l_subrc.
        CHECK l_subrc IS INITIAL.
      ENDIF.

*  CLEAR g_ok_code.
      CALL SCREEN '0300' STARTING AT 10 1 ENDING AT 31 5.

      CALL METHOD tree1->change_node
        EXPORTING
          i_node_key    = l_selected_node
          i_outtab_line = gs_out.

* this method must be called to send the data to the frontend
      CALL METHOD tree1->frontend_update.
    ELSE.
** ERRO Impossível definir prioridades para status 5
      MESSAGE i000 WITH 'Impossível definir prioridades para status 5'(055).
    ENDIF.
  ELSE.
** ERRO grupo inválido, ou selecionar nó do grupo
    MESSAGE i000 WITH 'Selecionar o nó do grupo'(056).
  ENDIF.

ENDFORM.                    " altera_prioridade

*&---------------------------------------------------------------------*
*&      Form  TIPO_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tipo_carga .

  DATA lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.

  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname,
        l_index         TYPE lvc_nkey,
        l_subrc         LIKE sy-subrc.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  READ TABLE gt_out INTO gs_out INDEX l_index.
  CHECK sy-subrc = 0.

  IF     gs_out-status IS INITIAL AND
     NOT gs_out-zlock  IS INITIAL AND
     NOT gs_out-refnr  IS INITIAL.

    CLEAR zwm028.
    SELECT SINGLE *
        FROM zwm028
            WHERE lgnum = lgnum AND
                  refnr = gs_out-refnr.

    IF NOT zwm028-st_pul IS INITIAL OR
       NOT zwm028-st_ppk IS INITIAL OR
       NOT zwm028-st_dck IS INITIAL.

      EXIT.
    ENDIF.


    SELECT SINGLE * FROM zwm026
    WHERE armazem   EQ lgnum
      AND grupo     EQ gs_out-refnr
      AND to_number NE space.

    IF sy-subrc = 0.

*    se tem paletes de picking
      CALL SCREEN 0001 STARTING AT 10 1 ENDING AT 61  7.

    ELSE.

*      senao tem paletes de picking
      CALL SCREEN 0003 STARTING AT 10 1 ENDING AT 61 7.
    ENDIF.

    CALL METHOD tree1->change_node
      EXPORTING
        i_node_key    = l_selected_node
        i_outtab_line = gs_out.

* this method must be called to send the data to the frontend
    CALL METHOD tree1->frontend_update.

  ELSE.
** ERRO grupo inválido, ou selecionar nó do grupo
    MESSAGE i000 WITH 'Selecionar o nó do grupo'(057).
  ENDIF.

ENDFORM.                    " TIPO_CARGA
*&---------------------------------------------------------------------*
*&      Form  processa_to_partida
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_to_partida .

  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname,
        l_index         TYPE lvc_nkey,
        l_subrc         LIKE sy-subrc.

  DATA: l_tanum  LIKE ltak-tanum,
        lv_tanum LIKE ltap-tanum,
        lv_tapos LIKE ltap-tapos,
        lv_vbeln TYPE vbeln.

  DATA: ls_ltap TYPE ltap.

  DATA l_ltap_cancl LIKE ltap_cancl OCCURS 0 WITH HEADER LINE.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  READ TABLE gt_out INTO gs_out INDEX l_index.
  CHECK sy-subrc = 0.

  IF NOT gs_out-to_partida IS INITIAL.

    CLEAR: lv_tanum, lv_tapos.
    lv_tanum = gs_out-tanum.
    lv_tapos = gs_out-tapos.

    SELECT SINGLE * FROM ltap
                    INTO ls_ltap
                    WHERE lgnum = lgnum AND
                          tanum = lv_tanum AND
                          tapos = lv_tapos.


    CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
      EXPORTING
        warehouse     = lgnum
        tanum         = lv_tanum
        tapos         = lv_tapos
      TABLES
        return_msg    = return_msg
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      DATA lv_lenum LIKE ltap-vlenr.

      CLEAR ltap.
      SELECT SINGLE * FROM ltap
          WHERE lgnum = lgnum AND
                tanum = lv_tanum AND
                tapos = lv_tapos.

*      IF ltap-letyp = 'P2' OR ltap-letyp = 'P5'.
      IF z_wm_cl_management=>is_remontada( i_lgnum = ltap-lgnum i_letyp = ltap-letyp ) EQ abap_true.
        CLEAR zwm020.
        SELECT SINGLE * FROM zwm020
            WHERE armazem = lgnum AND
                ( p1 = ltap-vlenr OR
                  p2 = ltap-vlenr ).
        IF ltap-vlenr = zwm020-p1.
          lv_lenum = zwm020-p2.
        ELSEIF ltap-vlenr = zwm020-p2.
          lv_lenum = zwm020-p1.
        ENDIF.

        CLEAR ltap.
        SELECT SINGLE * FROM ltap
          WHERE lgnum = lgnum AND
                vlenr = lv_lenum AND
                pquit = ' '.

        CLEAR: lv_tanum, lv_tapos.
        lv_tanum = ltap-tanum.
        lv_tapos = ltap-tapos.

        IF ltap-nltyp = '916'.
          CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
            EXPORTING
              warehouse     = lgnum
              tanum         = lv_tanum
              tapos         = lv_tapos
            TABLES
              return_msg    = return_msg
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.
        ELSE.

          CLEAR l_ltap_cancl.
          REFRESH l_ltap_cancl.

          l_ltap_cancl-tanum = lv_tanum.
          l_ltap_cancl-tapos = lv_tapos.
          APPEND l_ltap_cancl.

          CALL FUNCTION 'ZWM_CANCEL_TO'
            EXPORTING
              armazem      = lgnum
            TABLES
              t_ltap_cancl = l_ltap_cancl
            EXCEPTIONS
              error        = 1
              OTHERS       = 2.
        ENDIF.

      ENDIF.

      PERFORM reabastecimento_picking USING abap_false.

*      CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 11.07.2012 17:12:13
*  Motivo: Cria OT's
*--------------------------------------------------------------------*
      lv_vbeln = gs_out-vbeln.

      CALL FUNCTION 'ZWM_TO_CREATE_OUT'
        EXPORTING
          warehouse     = lgnum
          refnr         = gs_out-refnr
          vbeln         = lv_vbeln
          posnr         = gs_out-posnr
          vsola         = gs_out-nsolm
          meins         = gs_out-meins
          werks         = ls_ltap-werks
          lgort         = ls_ltap-lgort
          matnr         = ls_ltap-matnr
        TABLES
          return_msg    = return_msg
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
          EXPORTING
            i_lgnum = lgnum
            i_refnr = gs_out-refnr
            i_step  = 1.
      ENDIF.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    ENDIF.

    CALL METHOD tree1->change_node
      EXPORTING
        i_node_key    = l_selected_node
        i_outtab_line = gs_out.

* this method must be called to send the data to the frontend
    CALL METHOD tree1->frontend_update.

  ELSE.
** Selecionar OT Partida
    MESSAGE i000 WITH 'Selecionar OT Partida'(058).
  ENDIF.

  CLEAR flag_tree.
  not_first_time = 'X'.

ENDFORM.                    " processa_to_partida
*&---------------------------------------------------------------------*
*&      Module  user_exit_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_0300 INPUT.

  CLEAR: l_prioridade, ok_code_0300.
  SET SCREEN 0.
  LEAVE SCREEN.

ENDMODULE.                 " user_exit_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  SET PF-STATUS 'DESTINO'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE ok_code_0300.
    WHEN 'CONFIRMAR'.
** Actualização da prioridade

      GET TIME.

      UPDATE zwm028 SET  prioridade      = l_prioridade
                         user_prioridade = sy-uname
                         data_prioridade = sy-datum
                         hora_prioridade = sy-uzeit
                    WHERE lgnum   = lgnum AND
                          refnr   = g_grupo AND
                          remessa = space.

      COMMIT WORK AND WAIT.

      CALL FUNCTION 'Z_WMFR_IDOC_CHANGE_GRP_PRIOR'
        EXPORTING
          i_lgnum = lgnum
          i_refnr = g_grupo
          i_prior = l_prioridade.


      CLEAR flag_tree.
      not_first_time = 'X'.

      CLEAR: l_prioridade, ok_code_0300.
      SET SCREEN '0000'.
      LEAVE SCREEN.

  ENDCASE.

ENDMODULE.                 " user_command_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  exluir_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_EXCL_FUNC  text
*----------------------------------------------------------------------*
FORM exluir_toolbar  CHANGING ct_excl_func
                                     TYPE ui_functions.

  CLEAR ct_excl_func.

  APPEND '&CALC'             TO ct_excl_func.
*  APPEND '&GRAPH'               TO ct_excl_func.
*  APPEND '&REFRESH'             TO ct_excl_func.
*  APPEND '&INFO'                TO ct_excl_func.
*  APPEND '&LOCAL&PASTE'         TO ct_excl_func.
*  APPEND '&LOCAL&PASTE_NEW_ROW' TO ct_excl_func.
*  APPEND '&LOCAL&UNDO'          TO ct_excl_func.
*  APPEND '&LOCAL&APPEND'        TO ct_excl_func.
*  APPEND '&LOCAL&INSERT_ROW'    TO ct_excl_func.
*  APPEND '&LOCAL&DELETE_ROW'    TO ct_excl_func.
*  APPEND '&LOCAL&COPY_ROW'      TO ct_excl_func.
*  APPEND '&LOCAL&CUT'           TO ct_excl_func.
*  APPEND '&LOCAL&COPY'          TO ct_excl_func.

ENDFORM.                    " exluir_toolbar
*&---------------------------------------------------------------------*
*&      Form  check_bloqueios
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_bloqueios .

*CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
* EXPORTING
*   KEYWORD_             = 'G_ENTREGAS'
*   _SCOPE               = '1'
* EXCEPTIONS
*   FOREIGN_LOCK         = 1
*   SYSTEM_FAILURE       = 2
*   OTHERS               = 3.
*  IF SY-SUBRC <> 0.
*    CLEAR: p_mod.
*      p_vis = 'X'.
*  ENDIF.
*  exit.

  DATA: itab_enq LIKE seqg3 OCCURS 0 WITH HEADER LINE.

  DATA: lv_gtarg TYPE eqegtarg.

  CONCATENATE 'G_ENTREGAS_' lgnum INTO lv_gtarg.
  CONDENSE lv_gtarg NO-GAPS.

** Verifica se já existe bloqueio
  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      gclient               = sy-mandt
      gname                 = 'KEYWORD'
    TABLES
      enq                   = itab_enq
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT itab_enq WHERE gtarg EQ lv_gtarg.
      MESSAGE i000 WITH 'Prog. já está a ser processado pelo user'(059)
            itab_enq-guname 'apenas possibilidade de exibição.'(060).
      CLEAR: p_mod.
      p_vis = 'X'.
      EXIT.
    ENDLOOP.
  ENDIF.

** Ainda não existe bloqueio, vai criar o bloqueio
  CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = lv_gtarg
      _scope         = '1'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    IF itab_enq[] IS INITIAL.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      l_user = sy-msgv1.

      MESSAGE i000 WITH 'Prog. já está a ser processado pelo user'(061)
            l_user 'apenas possibilidade de exibição.'(062).
    ENDIF.
    CLEAR: p_mod.
    p_vis = 'X'.
  ENDIF.

ENDFORM.                    " check_bloqueios
*&---------------------------------------------------------------------*
*&      Form  reabastecimento_picking
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reabastecimento_picking USING uv_round_add TYPE flag.

  DATA: BEGIN OF lt_totais_lote OCCURS 0,
          matnr LIKE ltap-matnr,
          charg LIKE ltap-charg,
          vsola LIKE ltap-vsola,
        END OF lt_totais_lote,
        ls_totais_lote LIKE lt_totais_lote.

  DATA: lt_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE,
        t_lqua    LIKE lqua   OCCURS 0 WITH HEADER LINE,
        lt_ltak   LIKE ltak   OCCURS 0 WITH HEADER LINE,
        lt_ltap   LIKE ltap   OCCURS 0 WITH HEADER LINE.

  DATA: l_quant     LIKE zpalete_picking-uni_incompleta,
        l_quant_pkr LIKE zpalete_picking-uni_incompleta,
        l_quant_aux TYPE i,
        l_meins     LIKE lqua-meins.

  DATA: lv_vbeln TYPE vbeln.

  DATA: lt_return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        lt_sscc       LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: l_mov_type  LIKE ltak-bwlvs,
        l_mov_type1 LIKE ltak-bwlvs,
        l_mov_type2 LIKE ltak-bwlvs,
        l_mov       LIKE ltak-bwlvs,
        l_mov11     LIKE ltak-bwlvs,
        l_mov12     LIKE ltak-bwlvs,
        l_mov21     LIKE ltak-bwlvs,
        l_mov22     LIKE ltak-bwlvs,
        l_lgtyp     LIKE mlgt-lgtyp,
        l_plkpt     LIKE mlgn-plkpt,
        l_betyp     LIKE ltak-betyp,
        l_pstyv1    LIKE lips-pstyv,
        l_pstyv2    LIKE lips-pstyv.

  DATA: l_tanum LIKE ltak-tanum,
        l_werks LIKE lqua-werks,
        l_lgort LIKE lqua-lgort.

  DATA resto LIKE ltap-vsolm.

  SELECT * FROM zwm001 INTO TABLE lt_zwm001
                   WHERE armazem   = lgnum
                     AND processo  = 'REABASTECIMENTO'.
  IF lt_zwm001[] IS INITIAL.
*    Não existem parametros definidos para este processo (tabela &)
    MESSAGE i156(zwmmsg001) WITH 'ZWM001'.
    EXIT.
  ENDIF.

  CLEAR lt_zwm001.
  LOOP AT lt_zwm001.
    CASE lt_zwm001-parametro.
      WHEN 'ST_PCK'.
        l_lgtyp = lt_zwm001-valor.
      WHEN 'ST_BET'.
        l_betyp = lt_zwm001-valor.
      WHEN 'ST_PKB'.
        l_plkpt = lt_zwm001-valor.
      WHEN 'ST_MOV'.
        l_mov = lt_zwm001-valor.
      WHEN 'ST_MOV11'.
        l_mov11 = lt_zwm001-valor.
      WHEN 'ST_MOV12'.
        l_mov12 = lt_zwm001-valor.
      WHEN 'ST_MOV21'.
        l_mov21 = lt_zwm001-valor.
      WHEN 'ST_MOV22'.
        l_mov22 = lt_zwm001-valor.
      WHEN 'ST_PST1'.
        l_pstyv1 = lt_zwm001-valor.
      WHEN 'ST_PST2'.
        l_pstyv2 = lt_zwm001-valor.
    ENDCASE.
  ENDLOOP.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_out-matnr
    IMPORTING
      output = gs_out-matnr.


  lv_vbeln = gs_out-vbeln.
  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_refnr     = gs_out-refnr
      i_vbeln     = lv_vbeln
      i_matnr     = gs_out-matnr
      i_recall    = 'X'
      i_usewm     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
**   IMPORTING
**     ET_MESSAGES         =
    CHANGING
      c_lgnum     = lgnum
      c_werks     = l_werks
      c_lgort     = l_lgort
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.


*& Begin of Modification by Tiago Pateiro - ROFF @ 04.01.2016 14:46:16
*/ Adaptar determinaçao do centro a novos armazens - França
**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO l_werks
**    WHERE lgort EQ l_
**      AND lgnum EQ lgnum.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    l_werks = 'RENV'.
**  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 04.01.2016 14:46:16



  CLEAR l_meins.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input          = gs_out-meins
      language       = sy-langu
    IMPORTING
      output         = l_meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  l_mov_type = l_mov.  "972

  CLEAR: mlgn, mlgt, lqua.
  SELECT SINGLE * FROM mlgn
          WHERE matnr = gs_out-matnr
            AND lgnum = lgnum.
  IF mlgn-plkpt = l_plkpt.
    l_mov_type1 = l_mov21.  "976
    l_mov_type2 = l_mov22.  "975
  ELSE.
    l_mov_type1 = l_mov11.  "971
    l_mov_type2 = l_mov12.  "970
  ENDIF.
  SELECT SINGLE * FROM mlgt
          WHERE matnr = gs_out-matnr
            AND lgnum = lgnum
            AND lgtyp = l_lgtyp.
  CHECK sy-subrc = 0.
  IF mlgn-plkpt IS INITIAL.
    CHECK NOT mlgt-lgpla IS INITIAL.
  ENDIF.

** Verificar stock no picking fixo
  REFRESH t_lqua.
  CLEAR: t_lqua, l_quant.

  IF mlgn-plkpt IS INITIAL.
    SELECT * FROM lqua INTO TABLE t_lqua
            WHERE lgnum = mlgt-lgnum
              AND lgtyp = mlgt-lgtyp
              AND lgpla = mlgt-lgpla
              AND lenum = ' '.

    IF NOT t_lqua[] IS INITIAL.
      LOOP AT t_lqua.
        CLEAR l_quant_aux.
        l_quant_aux = trunc( t_lqua-verme ).
        l_quant = l_quant + l_quant_aux.
      ENDLOOP.
    ENDIF.

  ELSE.
** Verificar stock no picking variavel
    SELECT * FROM lqua INTO TABLE t_lqua
            WHERE lgnum = mlgt-lgnum
              AND lgtyp = 'PKB'
              AND matnr = gs_out-matnr
              AND lenum = ' '.

    IF NOT t_lqua[] IS INITIAL.
      LOOP AT t_lqua.
        CLEAR l_quant_aux.
        l_quant_aux = trunc( t_lqua-verme ).
        l_quant = l_quant + l_quant_aux.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CLEAR resto.
  IF gs_out-nsolm < mlgn-lhmg1.
    resto = gs_out-nsolm.
  ELSE.
    resto =  gs_out-nsolm MOD mlgn-lhmg1.
  ENDIF.

  CHECK l_quant < resto.

** Verfificar quantidade na material na reserva de picking (PKR)

  REFRESH t_lqua.
  CLEAR: t_lqua, l_quant_pkr, l_quant_aux.

  IF mlgn-plkpt IS INITIAL.
    SELECT * FROM lqua INTO TABLE t_lqua
            WHERE lgnum = mlgt-lgnum
              AND lgtyp = 'PKR'
              AND matnr = gs_out-matnr.

    IF NOT t_lqua[] IS INITIAL.
      LOOP AT t_lqua.
        CLEAR l_quant_aux.
        l_quant_aux = trunc( t_lqua-verme ).
        l_quant_pkr = l_quant_pkr + l_quant_aux.
      ENDLOOP.
    ENDIF.

  ELSE.
** Verificar stock na reserva do picking variavel
    SELECT * FROM lqua INTO TABLE t_lqua
            WHERE lgnum = mlgt-lgnum
              AND lgtyp = 'PRB'
              AND matnr = gs_out-matnr.

    IF NOT t_lqua[] IS INITIAL.
      LOOP AT t_lqua.
        CLEAR l_quant_aux.
        l_quant_aux = trunc( t_lqua-verme ).
        l_quant_pkr = l_quant_pkr + l_quant_aux.
      ENDLOOP.
    ENDIF.
  ENDIF.

  REFRESH: lt_return_msg, lt_sscc.
  CLEAR: lt_return_msg, lt_sscc.
  CLEAR l_tanum.

  lt_sscc-material      = gs_out-matnr.

** so vou pedir a diferenca do que quero com o que esta no picking
** no maximo o que estiver no PKR
  lt_sscc-quantidade    = resto - l_quant.

  IF lt_sscc-quantidade > l_quant_pkr.
    lt_sscc-quantidade  = l_quant_pkr.
  ENDIF.

  lt_sscc-uni           = l_meins.

  CLEAR mlgn.
  SELECT SINGLE *
      FROM mlgn
          WHERE matnr = gs_out-matnr AND
                lgnum = lgnum.
  IF sy-subrc = 0.
*    IF mlgn-lety1 = 'P2' OR mlgn-lety1 = 'P5'.
    IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
      IF l_mov_type1 = '971'.
        lt_sscc-quantidade    = 2.
        lt_sscc-uni           = 'PAL'.
      ENDIF.
    ENDIF.
  ENDIF.
  APPEND lt_sscc.

  CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse  = lgnum
      mov_type   = l_mov_type1                              "971 ou 976
      plant      = l_werks
      s_loc      = l_lgort
      req_number = gs_out-refnr
      req_type   = l_betyp
    IMPORTING
      to         = l_tanum
    TABLES
      return_msg = lt_return_msg
      sscc       = lt_sscc
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  IF NOT l_tanum IS INITIAL.

    CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
      EXPORTING
        i_lgnum = lgnum
        i_refnr = gs_out-refnr
        i_step  = 1.


    CLEAR ltak.
    SELECT * FROM ltak INTO TABLE lt_ltak
                        WHERE lgnum = lgnum
                          AND tanum = l_tanum
                          AND betyp = l_betyp
                          AND benum = gs_out-refnr
                          AND kquit = ' '.

    IF NOT lt_ltak[] IS INITIAL.

      SELECT * FROM ltap INTO TABLE lt_ltap
                  FOR ALL ENTRIES IN lt_ltak
                    WHERE lgnum = lt_ltak-lgnum
                      AND tanum = lt_ltak-tanum.

      DELETE lt_ltap WHERE matnr <> gs_out-matnr.
      DELETE lt_ltap WHERE pquit = 'X'.

      IF NOT lt_ltap[] IS INITIAL.
        LOOP AT lt_ltap.
*          IF lt_ltap-letyp = 'P2' OR lt_ltap-letyp = 'P5'.
          IF z_wm_cl_management=>is_remontada( i_lgnum = lt_ltap-lgnum i_letyp = lt_ltap-letyp ) EQ abap_true.
            IF lt_ltap-tapos = '0001'.
              l_quant_aux = trunc( lt_ltap-vsola ).
              l_quant = l_quant + l_quant_aux.
            ENDIF.
          ELSE.
            l_quant_aux = trunc( lt_ltap-vsola ).
            l_quant = l_quant + l_quant_aux.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR l_quant.
  ENDIF.

  IF l_quant < resto.

    l_quant = resto - l_quant.

    REFRESH: lt_return_msg, lt_sscc.
    CLEAR: lt_return_msg, lt_sscc.
    CLEAR l_tanum.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = l_meins
        language       = sy-langu
      IMPORTING
        output         = l_meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    lt_sscc-material      = gs_out-matnr.
    lt_sscc-quantidade    = l_quant.
    lt_sscc-uni           = l_meins.

    DATA: adicional TYPE lvs_lznum,
          qt_aux    LIKE lt_sscc-quantidade,
          qt_pal    LIKE mlgn-lhmg1,
          n_pal     TYPE i,
          qt_resto  TYPE i.

    CLEAR l_tanum.
    CLEAR mlgn.
    SELECT SINGLE *
        FROM mlgn
            WHERE matnr = gs_out-matnr AND
                  lgnum = lgnum.
    IF sy-subrc = 0.
*      IF mlgn-lety1 = 'P2' OR mlgn-lety1 = 'P5'.
      IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
        lt_sscc-quantidade    = 1.
        lt_sscc-uni           = 'PAL'.
        APPEND lt_sscc.
        lt_sscc-quantidade    = 1.
        lt_sscc-uni           = 'PAL'.
        n_pal = 2.
      ELSE.
        n_pal = 1.
      ENDIF.
    ENDIF.
    APPEND lt_sscc.

    CLEAR: qt_aux, qt_pal.
    IF mlgn-plkpt = 'PKB'.
      adicional = l_quant.
    ELSE.
      CLEAR adicional.
    ENDIF.

    qt_pal = mlgn-lhmg1 * n_pal.

    IF l_quant > qt_pal.
      CLEAR lt_sscc.
      REFRESH lt_sscc.

      qt_aux = l_quant DIV qt_pal.
      qt_resto = l_quant MOD qt_pal.

      IF NOT qt_resto IS INITIAL.
        IF mlgn-plkpt = 'PKB'.
          adicional = qt_resto.
        ELSE.
          CLEAR adicional.
        ENDIF.

*        IF mlgn-lety1 = 'P2' OR mlgn-lety1 = 'P5'.
        IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
          lt_sscc-material      = gs_out-matnr.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
          APPEND lt_sscc.
          lt_sscc-material      = gs_out-matnr.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
        ELSE.
          lt_sscc-material      = gs_out-matnr.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
        ENDIF.
        APPEND lt_sscc.

        IF uv_round_add EQ abap_true AND adicional IS NOT INITIAL.
          adicional = ceil( adicional ).
        ENDIF.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse      = lgnum
            mov_type       = l_mov_type "972
            plant          = l_werks
            s_loc          = l_lgort
            req_number     = gs_out-refnr
            req_type       = l_betyp
            sscc_adicional = adicional
          IMPORTING
            to             = l_tanum
          TABLES
            return_msg     = lt_return_msg
            sscc           = lt_sscc
          EXCEPTIONS
            error          = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
            EXPORTING
              i_lgnum = lgnum
              i_refnr = gs_out-refnr
              i_step  = 1.
        ENDIF.

        CLEAR lt_sscc.
        REFRESH lt_sscc.

      ENDIF.

      DO qt_aux TIMES.

        IF mlgn-plkpt = 'PKB'.
          adicional = mlgn-lhmg1.
        ELSE.
          CLEAR adicional.
        ENDIF.

*        IF mlgn-lety1 = 'P2' OR mlgn-lety1 = 'P5'.
        IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
          lt_sscc-material      = gs_out-matnr.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
          APPEND lt_sscc.
          lt_sscc-material      = gs_out-matnr.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
        ELSE.
          lt_sscc-material      = gs_out-matnr.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
        ENDIF.
        APPEND lt_sscc.

        IF uv_round_add EQ abap_true AND adicional IS NOT INITIAL.
          adicional = ceil( adicional ).
        ENDIF.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse      = lgnum
            mov_type       = l_mov_type "972
            plant          = l_werks
            s_loc          = l_lgort
            req_number     = gs_out-refnr
            req_type       = l_betyp
            sscc_adicional = adicional
          IMPORTING
            to             = l_tanum
          TABLES
            return_msg     = lt_return_msg
            sscc           = lt_sscc
          EXCEPTIONS
            error          = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
            EXPORTING
              i_lgnum = lgnum
              i_refnr = gs_out-refnr
              i_step  = 1.
        ENDIF.

        CLEAR lt_sscc.
        REFRESH lt_sscc.

      ENDDO.

    ELSE.
      IF uv_round_add EQ abap_true  AND adicional IS NOT INITIAL.
        adicional = ceil( adicional ).
      ENDIF.
      CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse      = lgnum
          mov_type       = l_mov_type "972
          plant          = l_werks
          s_loc          = l_lgort
          req_number     = gs_out-refnr
          req_type       = l_betyp
          sscc_adicional = adicional
        IMPORTING
          to             = l_tanum
        TABLES
          return_msg     = lt_return_msg
          sscc           = lt_sscc
        EXCEPTIONS
          error          = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
          EXPORTING
            i_lgnum = lgnum
            i_refnr = gs_out-refnr
            i_step  = 1.
      ENDIF.

    ENDIF.
  ENDIF.

  CLEAR ltak.
  SELECT * FROM ltak INTO TABLE lt_ltak
                      WHERE lgnum = lgnum
                        AND tanum = l_tanum
                        AND betyp = l_betyp
                        AND benum = gs_out-refnr
                        AND kquit = ' '.

  CHECK NOT lt_ltak[] IS INITIAL.

  SELECT * FROM ltap INTO TABLE lt_ltap
              FOR ALL ENTRIES IN lt_ltak
                WHERE lgnum = lt_ltak-lgnum
                  AND tanum = lt_ltak-tanum.

  DELETE lt_ltap WHERE matnr <> gs_out-matnr.
  DELETE lt_ltap WHERE pquit = 'X'.

  CHECK NOT lt_ltap[] IS INITIAL.


  REFRESH lt_totais_lote.
  SORT lt_ltak.
  LOOP AT lt_ltap.
    CLEAR ls_totais_lote.

*    IF lt_ltap-letyp = 'P2' OR lt_ltap-letyp = 'P5'.
    IF z_wm_cl_management=>is_remontada( i_lgnum = lt_ltap-lgnum i_letyp = lt_ltap-letyp ) EQ abap_true.
      IF lt_ltap-tapos = '0001'.
        READ TABLE lt_ltak WITH KEY lgnum = lt_ltap-lgnum
                                    tanum = lt_ltap-tanum
                                    BINARY SEARCH.
        IF sy-subrc = 0.
          IF NOT lt_ltak-lznum IS INITIAL.
            MOVE lt_ltak-lznum TO ls_totais_lote-vsola.
          ELSE.
            ls_totais_lote-vsola = lt_ltap-vsolm.
          ENDIF.
        ENDIF.

        ls_totais_lote-matnr = lt_ltap-matnr.
        ls_totais_lote-charg = lt_ltap-charg.
        COLLECT ls_totais_lote INTO lt_totais_lote.
      ENDIF.
    ELSE.
      READ TABLE lt_ltak WITH KEY lgnum = lt_ltap-lgnum
                                  tanum = lt_ltap-tanum
                         BINARY SEARCH.
      IF sy-subrc = 0.
        IF NOT lt_ltak-lznum IS INITIAL.
          MOVE lt_ltak-lznum TO ls_totais_lote-vsola.
        ELSE.
          ls_totais_lote-vsola = lt_ltap-vsolm.
        ENDIF.
      ENDIF.

      ls_totais_lote-matnr = lt_ltap-matnr.
      ls_totais_lote-charg = lt_ltap-charg.
      COLLECT ls_totais_lote INTO lt_totais_lote.

    ENDIF.
  ENDLOOP.

  LOOP AT lt_totais_lote.
    REFRESH: lt_return_msg, lt_sscc.
    CLEAR: lt_return_msg, lt_sscc.
    CLEAR l_tanum.


    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = l_meins
        language       = sy-langu
      IMPORTING
        output         = l_meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    lt_sscc-material      = lt_totais_lote-matnr.
    lt_sscc-quantidade    = lt_totais_lote-vsola.
    lt_sscc-uni           = l_meins.
    lt_sscc-lote_producao = lt_totais_lote-charg.
    APPEND lt_sscc.

    CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse  = lgnum
        mov_type   = l_mov_type2                            "970 ou 975
        plant      = l_werks
        s_loc      = l_lgort
        req_number = gs_out-refnr
        req_type   = l_betyp
      IMPORTING
        to         = l_tanum
      TABLES
        return_msg = lt_return_msg
        sscc       = lt_sscc
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    IF sy-subrc EQ 0.
      CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
        EXPORTING
          i_lgnum = lgnum
          i_refnr = gs_out-refnr
          i_step  = 1.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " reabastecimento_pickin

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'PORTA'.
  CLEAR: pulmao, pick_pre.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_0001 INPUT.

  IF ok_code_0001 EQ 'CANCE'.
    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " user_exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

*  DATA: i_zwm007 LIKE zwm007 OCCURS 0 WITH HEADER LINE,
*        i_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
*        i_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.
*
*  DATA: num_paletes(2), pulmao_aux(3), pulmao_ LIKE lagp-lgpla,
*        encontrou_pulmao(1), num_quantos TYPE i.
*
**  DATA: t_vbss LIKE vbss OCCURS 0 WITH HEADER LINE.
*
*  DATA: t_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.
*
*  CHECK NOT pulmao IS INITIAL.
*
*** RL -> INS 23.05.2005
*** Nova actualização da tabela ZWM028 tendo em conta eliminação
*** de items nas remessas
*
*  CALL FUNCTION 'ZWM_ACTUALIZA_PAL_GRUPO'
*    EXPORTING
*      lgnum                = it311-lgnum
*      refnr                = it311-refnr
*    EXCEPTIONS
*      actualizacao_zwm0028 = 1
*      OTHERS               = 2.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*** RL <- INS 23.05.2005
*
*** Actualizar o total de paletes a nivel do grupo
*
*  CLEAR t_zwm028.
*  REFRESH t_zwm028.
*
*  SELECT * INTO TABLE t_zwm028 FROM zwm028 WHERE refnr = it311-refnr.
*
*  CLEAR num_paletes.
*
*  LOOP AT t_zwm028.
*    CLEAR zwm028.
*    SELECT SINGLE *
*      FROM zwm028
*          WHERE lgnum = t_zwm028-lgnum AND
*                refnr = t_zwm028-refnr AND
*                remessa = t_zwm028-remessa.
*    IF sy-subrc = 0.
*      num_paletes = num_paletes + zwm028-total_paletes.
*    ENDIF.
*  ENDLOOP.
*
**  CLEAR t_vbss.
**  REFRESH t_vbss.
**
**  CLEAR num_paletes.
**  SELECT * INTO TABLE t_vbss FROM vbss WHERE sammg = it311-refnr.
**
**  LOOP AT t_vbss.
**    CLEAR zwm028.
**    SELECT SINGLE *
**      FROM zwm028
**          WHERE lgnum = it311-lgnum AND
**                refnr = t_vbss-sammg AND
**                remessa = t_vbss-vbeln.
**    IF sy-subrc = 0.
**      num_paletes = num_paletes + zwm028-total_paletes.
**    ENDIF.
**  ENDLOOP.
*
*
*  CLEAR i_zwm028.
*  REFRESH i_zwm028.
*
*  SELECT * INTO TABLE i_zwm028
*      FROM zwm028
*          WHERE lgnum = it311-lgnum AND
*                refnr = it311-refnr.
**** LOG ZWM028
**
**  LOOP AT i_zwm028.
**    CLEAR: zwm028_log, wa_log.
**    GET TIME.
**    MOVE-CORRESPONDING i_zwm028 TO wa_log.
**    wa_log-data = sy-datum.
**    wa_log-hora = sy-uzeit.
**    wa_log-ini_fim = 'INI'.
**    wa_log-user_tarefa = sy-uname.
**    wa_log-programa = sy-repid.
**
**    MODIFY zwm028_log FROM wa_log.
**    COMMIT WORK AND WAIT.
**    WAIT UP TO 2 SECONDS.
**  ENDLOOP.
**** LOG ZWM028
*
*  READ TABLE i_zwm028 INDEX 1.
*  CLEAR i_zwm028-remessa.
*  CLEAR i_zwm028-ordem.
*  i_zwm028-refnr = it311-refnr.
*  i_zwm028-total_paletes = num_paletes.
*** RL -> INS 14.04.2005 ----------------------------------------------
*** Servisan
*  CLEAR: i_zwm028-servisan, i_zwm028-emissor.
*** RL <- INS 14.04.2005 ----------------------------------------------
*
*  INSERT INTO zwm028 VALUES i_zwm028.
*  IF sy-subrc = 0.
*    COMMIT WORK AND WAIT.
*  ELSE.
*    ROLLBACK WORK.
*    ok_code_0001 = 'CANCE'.
*    CLEAR: pulmao, ok_code_0001.
*    SET  SCREEN '0000'.LEAVE SCREEN.
*  ENDIF.
*
*  WHILE 1 = 1.
*    SELECT SINGLE *
*        FROM zwm028
*            WHERE lgnum = it311-lgnum AND
*                  refnr = it311-refnr AND
*                  remessa = ' '.
*    IF sy-subrc = 0.
*      EXIT.
*    ELSE.
*      WAIT UP TO 1 SECONDS.
*    ENDIF.
*  ENDWHILE.
*
*  DATA: pos TYPE i, aux_pos.
*  CLEAR aux_pos.
*  pos = 1.
*
*  DELETE i_zwm028 WHERE remessa IS INITIAL.
*
*  SORT i_zwm028 BY ordem ASCENDING.
*
*  LOOP AT i_zwm028.
*
*    i_zwm028-posicao_ini_pul = pos.
*
*    MODIFY i_zwm028 INDEX sy-tabix.
*    pos = pos + i_zwm028-total_paletes.
*
*  ENDLOOP.
*
*  MODIFY zwm028 FROM TABLE i_zwm028.
*  COMMIT WORK AND WAIT.
*
**** LOG ZWM028
**  CLEAR i_zwm028.
**  REFRESH i_zwm028.
**
**  SELECT * INTO TABLE i_zwm028
**    FROM zwm028
**        WHERE lgnum = it311-lgnum AND
**              refnr = it311-refnr.
**
**  LOOP AT i_zwm028.
**    CLEAR: zwm028_log, wa_log.
**    GET TIME.
**    MOVE-CORRESPONDING i_zwm028 TO wa_log.
**    wa_log-data = sy-datum.
**    wa_log-hora = sy-uzeit.
**    wa_log-ini_fim = 'FIM'.
**    wa_log-user_tarefa = sy-uname.
**    wa_log-programa = sy-repid.
**
**    MODIFY zwm028_log FROM wa_log.
**    COMMIT WORK AND WAIT.
**    WAIT UP TO 2 SECONDS.
**  ENDLOOP.
**** LOG ZWM028
*
*  REFRESH i_zwm007.
  CASE pulmao.

    WHEN 'PUL'.

      CLEAR zwm028.
      IF pick_pre IS INITIAL.
        UPDATE zwm028
              SET st_pul = pulmao
                  WHERE lgnum = lgnum AND
                        refnr = gs_out-refnr.
      ELSE.
        UPDATE zwm028
              SET st_pul = pulmao
                  st_ppk = pick_pre
                  WHERE lgnum = lgnum AND
                        refnr = gs_out-refnr.
      ENDIF.

      COMMIT WORK AND WAIT.

  ENDCASE.

  CLEAR: pulmao, pick_pre, ok_code_0001.
  SET  SCREEN '0000'.LEAVE SCREEN.
ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.
  SET PF-STATUS 'PORTA'.
  CLEAR: porta_pul, pick_pre.

ENDMODULE.                 " STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.

*  REFRESH: i_zwm007,
*           i_zwm028 ,
*           i_lagp .
*
*  CLEAR: num_paletes, pulmao_aux, pulmao_ ,
*        encontrou_pulmao, num_quantos.
************************************
*  CHECK NOT porta_pul IS INITIAL.
*
*** RL -> INS 23.05.2005
*** Nova actualização da tabela ZWM028 tendo em conta eliminação
*** de items nas remessas
*  CALL FUNCTION 'ZWM_ACTUALIZA_PAL_GRUPO'
*    EXPORTING
*      lgnum                = it311-lgnum
*      refnr                = it311-refnr
*    EXCEPTIONS
*      actualizacao_zwm0028 = 1
*      OTHERS               = 2.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*** RL <- INS 23.05.2005
*
*** Actualizar o total de paletes a nivel do grupo
*  CLEAR t_zwm028.
*  REFRESH t_zwm028.
*
*  SELECT * INTO TABLE t_zwm028 FROM zwm028 WHERE refnr = it311-refnr.
*
*  CLEAR num_paletes.
*
*  LOOP AT t_zwm028.
*    CLEAR zwm028.
*    SELECT SINGLE *
*      FROM zwm028
*          WHERE lgnum = t_zwm028-lgnum AND
*                refnr = t_zwm028-refnr AND
*                remessa = t_zwm028-remessa.
*    IF sy-subrc = 0.
*      num_paletes = num_paletes + zwm028-total_paletes.
*    ENDIF.
*  ENDLOOP.
*
**  CLEAR t_vbss.
**  REFRESH t_vbss.
**
**  CLEAR num_paletes.
**  SELECT * INTO TABLE t_vbss FROM vbss WHERE sammg = it311-refnr.
**
**  LOOP AT t_vbss.
**    CLEAR zwm028.
**    SELECT SINGLE *
**      FROM zwm028
**          WHERE lgnum = it311-lgnum AND
**                refnr = t_vbss-sammg AND
**                remessa = t_vbss-vbeln.
**    IF sy-subrc = 0.
**      num_paletes = num_paletes + zwm028-total_paletes.
**    ENDIF.
**  ENDLOOP.
*
*  CLEAR i_zwm028.
*  REFRESH i_zwm028.
*  SELECT * INTO TABLE i_zwm028
*      FROM zwm028
*          WHERE lgnum = it311-lgnum AND
*                refnr = it311-refnr.
*
*  READ TABLE i_zwm028 INDEX 1.
*  CLEAR i_zwm028-remessa.
*  CLEAR i_zwm028-ordem.
*  i_zwm028-refnr = it311-refnr.
*  i_zwm028-total_paletes = num_paletes.
*
*** RL -> INS 14.04.2005 ----------------------------------------------
*** Servisan
*  CLEAR: i_zwm028-servisan, i_zwm028-emissor.
*** RL <- INS 14.04.2005 ----------------------------------------------
*
*  INSERT INTO zwm028 VALUES i_zwm028.
*  IF sy-subrc = 0.
*    COMMIT WORK AND WAIT.
*  ELSE.
*    ROLLBACK WORK.
*    ok_code_0001 = 'CANCE'.
*    CLEAR: porta_pul, ok_code_0001.
*    SET  SCREEN '0000'.LEAVE SCREEN.
*  ENDIF.
*
*  WHILE 1 = 1.
*    SELECT SINGLE *
*        FROM zwm028
*            WHERE lgnum = it311-lgnum AND
*                  refnr = it311-refnr AND
*                  remessa = ' '.
*    IF sy-subrc = 0.
*      EXIT.
*    ELSE.
*      WAIT UP TO 1 SECONDS.
*    ENDIF.
*  ENDWHILE.
*
*  CLEAR: aux_pos,pos.
*  pos = 1.
*
*  DELETE i_zwm028 WHERE remessa IS INITIAL.
*  COMMIT WORK AND WAIT.
*
*  SORT i_zwm028 BY ordem ASCENDING.
*
*  LOOP AT i_zwm028.
*    i_zwm028-posicao_ini_pul = pos.
*    MODIFY i_zwm028 INDEX sy-tabix.
*    pos = pos + i_zwm028-total_paletes.
*  ENDLOOP.
*
*  MODIFY zwm028 FROM TABLE i_zwm028.
*  COMMIT WORK AND WAIT.
*
*  REFRESH i_zwm007.
  break roffd.
  CLEAR zwm028.
  CASE porta_pul.
    WHEN 'DCK'.

      UPDATE zwm028
       SET st_dck = porta_pul
           WHERE lgnum = lgnum AND
                 refnr = gs_out-refnr.

    WHEN 'PUL'.

      UPDATE zwm028
       SET st_pul = porta_pul
           WHERE lgnum = lgnum AND
                 refnr = gs_out-refnr.

    WHEN 'PLT'.

      UPDATE zwm028
       SET st_pul = porta_pul
           WHERE lgnum = lgnum AND
                 refnr = gs_out-refnr.

  ENDCASE.
  SET  SCREEN '0000'.LEAVE SCREEN.
  CLEAR porta_pul.
ENDMODULE.                 " USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_pulmao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_pulmao INPUT.

  REFRESH posicoes.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE posicoes
      FROM lagp WHERE lgnum = lgnum AND lgtyp = 'PUL'.

*** --> [ROFF - SDF] João Dias - RENUPG00015 - 22.10.2020 15:03:54  ***

*  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
*    EXPORTING
**     CUCOL                        = 20
*      curow                        = 10
**     DISPLAY                      = ' '
*      selectfield                  = 'LGNUM'
*      tablename                    = 'ZWM027'
*      given_value                  = ' '
**     SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
*      titel                        = 'Posicoes'
*    IMPORTING
*      ind                          = index
*    TABLES
*      full_table                   = posicoes
*    EXCEPTIONS
*      no_tablefields_in_dictionary = 1
*      no_tablestructure_given      = 2
*      more_then_one_selectfield    = 3
*      no_selectfield               = 4
*      OTHERS                       = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE posicoes INDEX index.
*    IF sy-subrc = 0.
*      pulmao = posicoes-lgtyp.
*    ENDIF.
*  ENDIF.

  DATA ls_selection TYPE slis_selfield.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title          = 'Posicoes'
      i_selection      = 'X'
      i_tabname        = 'posicoes'
      i_structure_name = 'ZWM027'
    IMPORTING
      es_selfield      = ls_selection
    TABLES
      t_outtab         = posicoes
    EXCEPTIONS
      program_error    = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE posicoes INTO DATA(ls_posicoes) INDEX ls_selection-tabindex.
    IF sy-subrc = 0.
      pulmao = ls_posicoes-lgtyp.
    ENDIF.
  ENDIF.
*** <-- [ROFF - SDF] João Dias - RENUPG00015 - 22.10.2020 15:03:54 ***

ENDMODULE.                 " help_pulmao  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_porta_pul  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_porta_pul INPUT.

  REFRESH posicoes.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE posicoes
      FROM lagp WHERE lgnum = lgnum AND
                      ( lgtyp = 'DCK' OR
                        lgtyp = 'PUL' OR
                        lgtyp = 'PLT' ).

*** --> [ROFF - SDF] João Dias - 11.11.2020 17:05:35  ***

*  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
*    EXPORTING
**     CUCOL                        = 20
*      curow                        = 10
**     DISPLAY                      = ' '
*      selectfield                  = 'LGNUM'
*      tablename                    = 'ZWM027'
*      given_value                  = ' '
**     SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
*      titel                        = 'Posicoes'(067)
*    IMPORTING
*      ind                          = index
*    TABLES
*      full_table                   = posicoes
*    EXCEPTIONS
*      no_tablefields_in_dictionary = 1
*      no_tablestructure_given      = 2
*      more_then_one_selectfield    = 3
*      no_selectfield               = 4
*      OTHERS                       = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE posicoes INDEX index.
*    IF sy-subrc = 0.
*      porta_pul = posicoes-lgtyp.
*    ENDIF.
*  ENDIF.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title          = 'Posicoes'
      i_selection      = 'X'
      i_tabname        = 'posicoes'
      i_structure_name = 'ZWM027'
    IMPORTING
      es_selfield      = ls_selection
    TABLES
      t_outtab         = posicoes
    EXCEPTIONS
      program_error    = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE posicoes INTO ls_posicoes INDEX ls_selection-tabindex.
    IF sy-subrc = 0.
      porta_pul = ls_posicoes-lgtyp.
    ENDIF.
  ENDIF.

*** <-- [ROFF - SDF] João Dias - 11.11.2020 17:05:35 ***

ENDMODULE.                 " help_porta_pul  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_pick_pre  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_pick_pre INPUT.

  REFRESH posicoes.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE posicoes
      FROM lagp WHERE lgnum = lgnum AND lgtyp = 'PPK'.

*** --> [ROFF - SDF] João Dias - 11.11.2020 17:06:07  ***

*  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
*    EXPORTING
**     CUCOL                        = 20
*      curow                        = 10
**     DISPLAY                      = ' '
*      selectfield                  = 'LGNUM'
*      tablename                    = 'ZWM027'
*      given_value                  = ' '
**     SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
*      titel                        = 'Posicoes'(067)
*    IMPORTING
*      ind                          = index
*    TABLES
*      full_table                   = posicoes
*    EXCEPTIONS
*      no_tablefields_in_dictionary = 1
*      no_tablestructure_given      = 2
*      more_then_one_selectfield    = 3
*      no_selectfield               = 4
*      OTHERS                       = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE posicoes INDEX index.
*    IF sy-subrc = 0.
*      pick_pre = posicoes-lgtyp.
*    ENDIF.
*  ENDIF.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title          = 'Posicoes'
      i_selection      = 'X'
      i_tabname        = 'posicoes'
      i_structure_name = 'ZWM027'
    IMPORTING
      es_selfield      = ls_selection
    TABLES
      t_outtab         = posicoes
    EXCEPTIONS
      program_error    = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE posicoes INTO ls_posicoes INDEX ls_selection-tabindex.
    IF sy-subrc = 0.
      pick_pre = ls_posicoes-lgtyp.
    ENDIF.
  ENDIF.

*** <-- [ROFF - SDF] João Dias - 11.11.2020 17:06:07 ***

ENDMODULE.                 " help_pick_pre  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_pulmao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pulmao INPUT.
  CHECK NOT pulmao IS INITIAL.

  IF pulmao <> 'PUL'.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '118' WITH pulmao.
    CLEAR: pulmao.
  ENDIF.
ENDMODULE.                 " check_pulmao  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_pick_pre  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pick_pre INPUT.

  CLEAR: t_ltak, t_ltap.
  REFRESH: t_ltak, t_ltap.

  CHECK NOT pick_pre IS INITIAL.

  IF pick_pre <> 'PPK'.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '118' WITH  pick_pre.
    CLEAR: pick_pre.
  ENDIF.

** Verficar se tem paletes completas

  SELECT * INTO TABLE t_ltak
      FROM ltak
          WHERE lgnum = lgnum AND
                refnr = gs_out-refnr AND
                kquit = ' '.

  LOOP AT t_ltak.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_ltap
        FROM ltap
            WHERE lgnum = t_ltak-lgnum AND
                  tanum = t_ltak-tanum AND
                  vorga <> 'ST' AND
                  pquit = ' ' AND
                  ( vltyp = 'TRI' OR
                    vltyp = 'DRI' OR
                    vltyp = 'BLK' OR
                    vltyp = 'PRM' ).

  ENDLOOP.

  IF t_ltap[] IS INITIAL.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '208' .
    CLEAR: pick_pre.
  ENDIF.

ENDMODULE.                 " check_pick_pre  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_porta_pul  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_porta_pul INPUT.
  CHECK NOT porta_pul IS INITIAL.

  IF porta_pul = 'PUL' OR porta_pul = 'DCK' OR porta_pul = 'PLT'.

  ELSE.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '118' WITH  porta_pul.
    CLEAR: porta_pul.
  ENDIF.

ENDMODULE.                 " check_porta_pul  IN
*&---------------------------------------------------------------------*
*&      Form  CHECK_PALETES_PRODUCAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_paletes_producao USING f_subrc..

  DATA: lt_zwm045          LIKE zwm045         OCCURS 0 WITH HEADER LINE,
        lt_zwm_log_efacec  LIKE zwm_log_efacec  OCCURS 0 WITH HEADER LINE,
        lt_zwm_log_efacec1 LIKE zwm_log_efacec OCCURS 0 WITH HEADER LINE.

  DATA: min_time     LIKE sy-uzeit,
        max_time     LIKE sy-uzeit,
        min_date     LIKE sy-datum,
        max_date     LIKE sy-datum,
        time         LIKE sy-uzeit,
        num_get_mesa TYPE i,
        num_get_to   TYPE i,
        t_pal_tapete TYPE zminpal.

  CLEAR: time, min_time, max_time, num_get_mesa, num_get_to,
         lt_zwm045, lt_zwm_log_efacec, lt_zwm_log_efacec1,
         t_pal_tapete.
  REFRESH: lt_zwm045, lt_zwm_log_efacec, lt_zwm_log_efacec1.

  CLEAR valor.
  PERFORM get_parameter
          USING lgnum
                'MONITOR_ENTREGAS'
                'TIME'
                valor.

  time = valor * 60.

  CHECK NOT time IS INITIAL.

  break roffd.
  SELECT * INTO TABLE lt_zwm045
      FROM zwm045
          WHERE lgnum = lgnum.

  CHECK NOT lt_zwm045[] IS INITIAL.

  GET TIME.
  min_time = sy-uzeit - time.
  max_time = sy-uzeit.

  min_date = sy-datum - ( time / 240000 ).
  max_date = sy-datum.

  SELECT * INTO TABLE lt_zwm_log_efacec
      FROM zwm_log_efacec
          WHERE data BETWEEN min_date AND max_date
            AND hora BETWEEN min_time AND max_time
            AND processo = 'GET_MESA'.

*  CHECK NOT lt_zwm_log_efacec[] IS INITIAL.

  IF lt_zwm_log_efacec[] IS NOT INITIAL.

    SELECT * INTO TABLE lt_zwm_log_efacec1
        FROM zwm_log_efacec
            FOR ALL ENTRIES IN lt_zwm_log_efacec
                WHERE processo = 'GET_TO'
                  AND sscc = lt_zwm_log_efacec-sscc.

    DESCRIBE TABLE lt_zwm_log_efacec LINES num_get_mesa.
    DESCRIBE TABLE lt_zwm_log_efacec1 LINES num_get_to.
  ENDIF.

  t_pal_tapete = num_get_mesa - num_get_to.

  IF t_pal_tapete > 99.
    t_pal_tapete = 99.
  ENDIF.

  IF t_pal_tapete < 0.
    CLEAR: t_pal_tapete.
  ENDIF.

  DATA num_pri TYPE i.
  LOOP AT lt_zwm045 WHERE min_pal <= t_pal_tapete
                      AND max_pal >= t_pal_tapete.

    CLEAR num_pri.
    MOVE lt_zwm045-num_pri TO num_pri.
    IF num_pri <= num_prio.
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '256'
                             WITH t_pal_tapete lt_zwm045-num_pri .
      f_subrc = 4.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECK_PALETES_PROD
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'DESTINO'.

  IF st_destino IS INITIAL.
    IF zwm028-total_paletes <= 4.
      st_destino = 'PLM'.
    ENDIF.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name = 'ST_DESTINO'.
      IF st_destino <> 'PLM'.
        IF NOT st_destino IS INITIAL.
          screen-input = ' '.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF NOT bin_destino1 IS INITIAL.
          screen-input = ' '.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
    IF screen-name = 'ST_DESTINO2'.
      IF NOT texto0400 IS INITIAL OR
         st_destino = 'DCK'.
        screen-input = ' '.
        MODIFY SCREEN.
      ENDIF.
      IF NOT st_destino2 IS INITIAL.
        screen-input = ' '.
        MODIFY SCREEN.
      ENDIF.
      IF NOT zwm028-st_ppk IS INITIAL.
        st_destino2 = zwm028-st_ppk.
        screen-input = ' '.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_0400 INPUT.

  g_ok_code = ok_code_0400.
  IF ok_code_0400 EQ 'CANCE'.
    CLEAR:  st_destino, st_destino2, bin_destino1, bin_kober1,
            bin_destino2,ok_code_0400, texto0400.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " USER_EXIT_0400  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ST_DESTINO1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_st_destino1 INPUT.

  DATA: num_clientes TYPE i,
        aux_kunnr    LIKE vbpa-kunnr.

  DATA: lt_vbpa     LIKE vbpa OCCURS 0 WITH HEADER LINE,
        l_remessa   LIKE zwm028-remessa,
        l_kunnr     LIKE vbpa-kunnr,
        lt_vkorg_mp TYPE TABLE OF zwm001,
        lv_vkorg    TYPE vkorg.

  DATA: lv_virtual TYPE flag.

** Org de Vendas Com para DCK com Multi Prod
************************************************************************
  SELECT * FROM zwm001
           INTO TABLE lt_vkorg_mp
           WHERE armazem = lgnum AND
                 processo = 'CARGAS_DIRETAS_MULTI' AND
                 parametro = 'ORG_VENDAS' AND
                 valor <> ''.

  SORT lt_vkorg_mp BY valor.


************************************************************************

  CHECK NOT st_destino IS INITIAL.

  IF st_destino EQ 'PLV'.
    st_destino = 'PUL'.
    lv_virtual = abap_true.
  ENDIF.


  IF g_lock = 2.
    IF st_destino = 'DCK'.
      IF caract_carga IS INITIAL.
        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '257' WITH st_destino.
        CLEAR: ok_code_0400, st_destino.
        EXIT.
      ENDIF.
    ELSEIF st_destino = 'PLT' AND NOT texto0400 IS INITIAL.
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '257' WITH st_destino.
      CLEAR: ok_code_0400, st_destino.
      EXIT.
    ENDIF.
  ELSEIF g_lock = 5.
    IF st_destino <> 'DCK'.
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '257' WITH st_destino.
      CLEAR: ok_code_0400, st_destino.
      EXIT.
    ENDIF.
  ENDIF.

  IF st_destino = 'DCK'.
    CLEAR wa_out.
    LOOP AT gt_out INTO wa_out
                  WHERE refnr = zwm028-refnr
                    AND palete_especial = 'X'.
      EXIT.
    ENDLOOP.

** Remover Validação de Bloquear Carga Direta para Pal Esp
************************************************************************
*    IF sy-subrc = 0.
*      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '265'.
*      CLEAR: ok_code_0400, st_destino.
*      EXIT.
*    ENDIF.
  ENDIF.

*  IF st_destino = 'DCK' AND texto0400 IS INITIAL.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '257' WITH st_destino.
*    CLEAR: ok_code_0400, st_destino.
*    EXIT.
*  ENDIF.

  IF st_destino = 'PLM' AND zwm028-total_paletes > 4.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '257' WITH st_destino.
    CLEAR: ok_code_0400, st_destino.
    EXIT.
  ENDIF.

  IF st_destino = 'PLT'.
    SORT lt_transportes.
    READ TABLE lt_transportes WITH KEY refnr = zwm028-refnr.
    IF sy-subrc = 0.
      MESSAGE ID 'ZWMMSG001' TYPE 'W' NUMBER '272' WITH st_destino.
    ENDIF.
  ENDIF.

  IF st_destino = 'PLT' OR st_destino = 'DCK'.
    CLEAR: lt_zwm028,lt_vbpa, num_clientes.
    REFRESH: lt_zwm028, lt_vbpa.

    SELECT * INTO TABLE lt_zwm028
        FROM zwm028
            WHERE lgnum = lgnum
              AND refnr = zwm028-refnr
              AND remessa <> ' '.

** Cargas apenas com paletes completas não valida se é servisan
    IF texto0400 IS INITIAL.
      LOOP AT lt_zwm028 WHERE servisan = 'X'.
        EXIT.
      ENDLOOP.
*      IF sy-subrc = 0.
*        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '257' WITH st_destino.
*        CLEAR: ok_code_0400, st_destino.
*        EXIT.
*      ENDIF.
    ENDIF.

    SELECT * INTO TABLE lt_vbpa
        FROM vbpa
            FOR ALL ENTRIES IN lt_zwm028
                WHERE vbeln EQ lt_zwm028-remessa
                  AND posnr = '000000'
                  AND parvw EQ 'W1'.

    SORT lt_vbpa BY kunnr.
    CLEAR aux_kunnr.
    LOOP AT lt_vbpa.
      IF aux_kunnr <> lt_vbpa-kunnr.
        num_clientes = num_clientes + 1.
        aux_kunnr = lt_vbpa-kunnr.
      ENDIF.
    ENDLOOP.

    SELECT SINGLE vkorg FROM likp
                        INTO lv_vkorg
                        WHERE vbeln = lt_vbpa-vbeln.

    READ TABLE lt_vkorg_mp
          WITH KEY valor = lv_vkorg
          BINARY SEARCH
          TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.

**      IF num_clientes > 1 AND texto0400 IS INITIAL.
**        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '264'.
**        CLEAR: ok_code_0400, st_destino.
**        EXIT.
**      ENDIF.

    ENDIF.

    CLEAR wa_out.
    LOOP AT gt_out INTO wa_out
                   WHERE refnr = zwm028-refnr
                     AND palete_especial = 'X'.
      EXIT.
    ENDLOOP.
** Remover Validação de Bloquear Carga Direta para Pal Esp
************************************************************************
*    IF sy-subrc = 0.
*      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '265'.
*      CLEAR: ok_code_0400, st_destino.
*      EXIT.
*    ENDIF.
  ENDIF.


  IF st_destino <> 'DCK' AND
     st_destino <> 'PUL' AND
     st_destino <> 'PLT' AND
     st_destino <> 'PLM'.

    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '257' WITH st_destino.
    CLEAR: ok_code_0400, st_destino.
    EXIT.
  ENDIF.

  IF st_destino <> 'DCK'.

    CLEAR: bin_destino1, bin_destino2, bin_kober1.
    CALL FUNCTION 'ZWM_FIND_BIN'
      EXPORTING
        st_type             = st_destino
        num_paletes         = zwm028-total_paletes
        armazem             = lgnum
        refnr               = zwm028-refnr
        virtual             = lv_virtual
      IMPORTING
        posicao1            = bin_destino1
        posicao2            = bin_destino2
        e_kober             = bin_kober1
      EXCEPTIONS
        num_paletes_initial = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.

    ENDIF.

    IF bin_destino1 IS INITIAL AND bin_destino2 IS INITIAL.
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '205' WITH st_destino.
      CLEAR: ok_code_0400, st_destino.
      EXIT.
    ENDIF.
  ELSE.
    bin_destino1 = zwm028-porta.
  ENDIF.


ENDMODULE.                 " CHECK_ST_DESTINO1  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ST_DESTINO2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_st_destino2 INPUT.

  CHECK NOT st_destino2 IS INITIAL.

*  IF st_destino <> 'PUL'.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '258'.
*    CLEAR: ok_code_0400, st_destino2.
*    EXIT.
*  ENDIF.

  IF st_destino2 <> 'PPK'.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '257' WITH st_destino2.
    CLEAR: ok_code_0400, st_destino2.
    EXIT.
  ENDIF.

** Verficar se tem paletes completas

  CLEAR: t_ltak, t_ltap.
  REFRESH: t_ltak, t_ltap.
  SELECT * INTO TABLE t_ltak
      FROM ltak
          WHERE lgnum = zwm028-lgnum
            AND refnr = zwm028-refnr
            AND kquit = ' '.

  LOOP AT t_ltak.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_ltap
        FROM ltap
            WHERE lgnum = t_ltak-lgnum AND
                  tanum = t_ltak-tanum AND
                  vorga <> 'ST' AND
                  pquit = ' ' AND
                  ( vltyp = 'TRI' OR
                    vltyp = 'DRI' OR
                    vltyp = 'BLK' OR
                    vltyp = 'PRM' ).
  ENDLOOP.

  IF t_ltap[] IS INITIAL.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '208' .
    CLEAR: st_destino2.
    EXIT.
  ENDIF.

ENDMODULE.                 " CHECK_ST_DESTINO2  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  DATA t_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.
  DATA lv_werks TYPE werks_d. " << INS ROFF(SDF):TMGP:06.01.2016 08:53:03
  DATA lv_lgort TYPE lgort_d.
  DATA lv_ordem TYPE zwm028-ordem. " << INS ROFF(SDF):TMGP:20.01.2016 17:16:41
  DATA lv_valor TYPE zwm001-valor. " << INS ROFF(SDF):TMGP:20.01.2016 17:17:01
  DATA lv_vbeln TYPE vbeln.
  DATA lv_lgtyp TYPE lgtyp.
  DATA lv_activated_idoc TYPE flag.

  CLEAR: t_zwm028, lv_werks, lv_lgort.
  REFRESH: t_zwm028.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_activated_idoc
                      WHERE armazem   = lgnum AND
                            processo  = 'LIBERACAO_VIA_IDOC' AND
                            parametro = 'ACTIVAR'.



  IF g_lock = 2 AND st_destino2 = 'PPK'.
  ELSE.
    CHECK NOT st_destino IS INITIAL.
  ENDIF.
  CHECK ok_code_0400 = 'CONFIRMAR'.

  IF st_destino <> 'DCK' AND st_destino <> ' '.
    CHECK NOT bin_destino1 IS INITIAL.
  ENDIF.

  CLEAR g_desbloq.
  PERFORM get_desbloqueio_carga.
  CHECK NOT g_desbloq IS INITIAL.


  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_refnr       = gs_out-refnr
      i_recall      = 'X'
      i_usewm       = 'X'
      i_usemm       = 'X'
      i_useaut      = 'X'
      i_get_lgnum   = 'X'
      i_get_werks   = 'X'
      i_get_lgort   = 'X'
      i_first_werks = 'X'
      i_first_lgort = 'X'
* IMPORTING
*     ET_MESSAGES   =
    CHANGING
      c_lgnum       = lgnum
      c_werks       = lv_werks
      c_lgort       = lv_lgort
    EXCEPTIONS
      error         = 1
      user_back     = 2
      OTHERS        = 3.



  CASE st_destino.

    WHEN ' '.
      IF NOT st_destino2 IS INITIAL.
        IF g_desbloq = 'G'.
          UPDATE zwm028 SET zlock      = g_lock
                            st_ppk     = st_destino2
                            transporte = trans
                            tipo_lock  = g_desbloq
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr.
          COMMIT WORK AND WAIT.
        ELSEIF g_desbloq = 'R'.
          UPDATE zwm028 SET st_ppk     = st_destino2
                            transporte = trans
                            tipo_lock  = g_desbloq
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr.
          COMMIT WORK AND WAIT.

          SELECT * INTO TABLE t_zwm028
              FROM zwm028
                  WHERE lgnum = lgnum
                    AND refnr = gs_out-refnr.

          SORT t_zwm028 BY ordem.
          LOOP AT t_zwm028.

            UPDATE zwm028 SET zlock = g_lock
                          WHERE lgnum = lgnum
                            AND refnr = t_zwm028-refnr
                            AND remessa = t_zwm028-remessa.
            COMMIT WORK AND WAIT.

            IF t_zwm028-remessa IS NOT INITIAL AND
               t_zwm028-total_paletes <> t_zwm028-paletes_pulmao.
              EXIT.
            ENDIF.

          ENDLOOP.

        ENDIF.
      ENDIF.
    WHEN 'PUL'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 13:57:03
*  Motivo: Altera Ordem de Carga
*--------------------------------------------------------------------*
      CALL FUNCTION 'ZWM_MPUL_SEQ_CARGA_CHANGE'
        EXPORTING
          i_refnr = gs_out-refnr
          i_kober = bin_kober1.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

      CLEAR zwm028.
      IF st_destino2 IS INITIAL.
        IF g_desbloq = 'G'.
          UPDATE zwm028 SET zlock      = g_lock
                            st_pul     = st_destino
                            pulmao1    = bin_destino1
                            pulmao2    = bin_destino2
                            kober      = bin_kober1
                            transporte = trans
                            tipo_lock  = g_desbloq
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr.
          COMMIT WORK AND WAIT.
        ELSEIF g_desbloq = 'R'.
          UPDATE zwm028 SET st_pul     = st_destino
                            pulmao1    = bin_destino1
                            pulmao2    = bin_destino2
                            kober      = bin_kober1
                            transporte = trans
                            tipo_lock  = g_desbloq
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr.
          COMMIT WORK AND WAIT.

*& Begin of Modification by Tiago Pateiro - ROFF @ 20.01.2016 17:15:07
*/
          CLEAR lv_valor.
          CLEAR lv_ordem.

          SELECT valor UP TO 1 ROWS
            FROM zwm001 INTO lv_valor
            WHERE armazem EQ lgnum
              AND processo EQ 'ALTERA_GLOCK'
              AND parametro EQ 'ACTIVAR'.
          ENDSELECT.

          IF lv_valor EQ abap_false.
            lv_ordem  = '01'.
          ELSE.
            SELECT MAX( ordem )
              FROM zwm028 INTO lv_ordem
              WHERE lgnum EQ lgnum
                AND refnr EQ gs_out-refnr.
          ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 20.01.2016 17:15:08

          UPDATE zwm028 SET zlock = g_lock
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr
*                          AND ( remessa = ' ' OR ordem = '01' ). " << DEL ROFF(SDF):TMGP:20.01.2016 17:17:08
                          AND ( remessa = ' ' OR ordem = lv_ordem ).  " << INS ROFF(SDF):TMGP:20.01.2016 17:17:09
          COMMIT WORK AND WAIT.

          IF NOT st_destino IS INITIAL.
            lv_lgtyp = st_destino.
          ELSE.
            lv_lgtyp = st_destino2.
          ENDIF.

          CALL FUNCTION 'Z_WMFR_SET_TAPETE'
            EXPORTING
              i_lgnum = lgnum
              i_lgtyp = lv_lgtyp
              i_lgpla = bin_destino1
              i_refnr = gs_out-refnr.

          SELECT SINGLE remessa FROM zwm028
                                INTO lv_vbeln
                                WHERE lgnum = lgnum AND
                                      refnr = gs_out-refnr AND
                                      ordem = lv_ordem.

          CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
            EXPORTING
              i_lgnum = lgnum
              i_refnr = gs_out-refnr
              i_vbeln = lv_vbeln
              i_step  = 2.
        ENDIF.

      ELSE.
        IF g_desbloq = 'G'.
          UPDATE zwm028 SET zlock      = g_lock
                            st_pul     = st_destino
                            st_ppk     = st_destino2
                            pulmao1    = bin_destino1
                            pulmao2    = bin_destino2
                            kober      = bin_kober1
                            transporte = trans
                            tipo_lock  = g_desbloq
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr.
          COMMIT WORK AND WAIT.
        ELSEIF g_desbloq = 'R'.
          UPDATE zwm028 SET st_pul     = st_destino
                            st_ppk     = st_destino2
                            pulmao1    = bin_destino1
                            pulmao2    = bin_destino2
                            kober      = bin_kober1
                            transporte = trans
                            tipo_lock  = g_desbloq
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr.
          COMMIT WORK AND WAIT.

          SELECT * INTO TABLE t_zwm028
              FROM zwm028
                  WHERE lgnum = lgnum
                    AND refnr = gs_out-refnr.

          SORT t_zwm028 BY ordem.
          LOOP AT t_zwm028.

            UPDATE zwm028 SET zlock = g_lock
                          WHERE lgnum = lgnum
                          AND refnr = t_zwm028-refnr
                          AND remessa = t_zwm028-remessa.
            COMMIT WORK AND WAIT.

            IF t_zwm028-remessa IS NOT INITIAL AND
               t_zwm028-total_paletes <> t_zwm028-paletes_pulmao.
              EXIT.
            ENDIF.

          ENDLOOP.
*          UPDATE zwm028 SET zlock = g_lock
*                        WHERE lgnum = lgnum
*                          AND refnr = gs_out-refnr
*                          AND ( remessa = ' ' OR ordem = '01' ).
*          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

      CLEAR wa_lagpv.
      SELECT SINGLE * FROM lagp
      WHERE lgnum = lgnum
      AND lgtyp = st_destino
      AND lgpla = bin_destino1.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING lagp TO wa_lagpv.
        wa_lagpv-brand = 'X'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 06.06.2012 11:42:27
*  Motivo: Bloqueia Meio Pulmão
*--------------------------------------------------------------------*
        IF lgnum <> '150'.
          IF NOT wa_lagpv-kober IS INITIAL.
            wa_lagpv-kober = gc_mpul_lado_amb.
          ELSE.
            wa_lagpv-kober = bin_kober1.
          ENDIF.
        ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

        CALL FUNCTION 'L_LAGP_VERAENDERN'
          EXPORTING
            xlagpv = wa_lagpv.

        COMMIT WORK AND WAIT.
      ENDIF.

      IF NOT bin_destino2 IS INITIAL.
        CLEAR wa_lagpv.
        SELECT SINGLE * FROM lagp
        WHERE lgnum = lgnum
        AND lgtyp = st_destino
        AND lgpla = bin_destino2.

        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lagp TO wa_lagpv.
          wa_lagpv-brand = 'X'.
          CALL FUNCTION 'L_LAGP_VERAENDERN'
            EXPORTING
              xlagpv = wa_lagpv.

          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

    WHEN 'DCK'.

      SELECT SINGLE signi
      FROM vttk INTO vttk-signi
      WHERE tknum = zwm028-transporte.

      IF zwm028-porta IS INITIAL.
        g_lock = 1.
      ENDIF.

      IF g_lock = 5.
        UPDATE zwm028 SET zlock = g_lock
                          porta = bin_destino1
                          transporte = trans
                      WHERE lgnum = lgnum
                        AND refnr = gs_out-refnr.
        COMMIT WORK AND WAIT.
      ELSE.
        IF g_desbloq = 'G'.
          UPDATE zwm028 SET zlock      = g_lock
                            st_dck     = st_destino
                            porta      = bin_destino1
                            transporte = trans
                            tipo_lock  = g_desbloq
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr.
          COMMIT WORK AND WAIT.
        ELSEIF g_desbloq = 'R'.
          UPDATE zwm028 SET st_dck     = st_destino
                            porta      = bin_destino1
                            transporte = trans
                            tipo_lock  = g_desbloq
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr.
          COMMIT WORK AND WAIT.

          UPDATE zwm028 SET zlock = g_lock
                        WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr
                          AND ( remessa = ' ' OR ordem = '01' ).
          COMMIT WORK AND WAIT.


          IF NOT st_destino IS INITIAL.
            lv_lgtyp = st_destino.
          ELSE.
            lv_lgtyp = st_destino2.
          ENDIF.

          CALL FUNCTION 'Z_WMFR_SET_TAPETE'
            EXPORTING
              i_lgnum = lgnum
              i_lgtyp = lv_lgtyp
              i_lgpla = bin_destino1
              i_refnr = gs_out-refnr.

          SELECT SINGLE remessa FROM zwm028
                                INTO lv_vbeln
                                WHERE lgnum = lgnum AND
                                      refnr = gs_out-refnr AND
                                      ordem = '01'.

          CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
            EXPORTING
              i_lgnum = lgnum
              i_refnr = gs_out-refnr
              i_vbeln = lv_vbeln
              i_step  = 2.

**  Verificar se existem mais remessas com o mesmo recebedor de merc.
          IF NOT lt_vbpa[] IS INITIAL.

            SELECT SINGLE remessa INTO l_remessa
                FROM zwm028
                WHERE lgnum = lgnum
                  AND refnr = gs_out-refnr
                  AND ordem = '01'.

            LOOP AT lt_vbpa WHERE vbeln = l_remessa.
              l_kunnr = lt_vbpa-kunnr.
              EXIT.
            ENDLOOP.

            LOOP AT lt_vbpa WHERE vbeln <> l_remessa
                              AND kunnr = l_kunnr.
              IF lv_activated_idoc EQ abap_true.
                EXIT.
              ENDIF.

              UPDATE zwm028 SET zlock = g_lock
                  WHERE lgnum = lgnum
                    AND refnr = gs_out-refnr
                    AND remessa = lt_vbpa-vbeln.
              COMMIT WORK AND WAIT.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
      IF g_lock = '2' OR g_lock = '4'.

      ELSEIF g_lock = '5'.
**      carga directa nao cria OT de Carga
        CHECK zwm028-st_dck IS INITIAL.
        CHECK zwm028-ot IS INITIAL.
** Criar Ot de carga

        CLEAR: i_sscc, return_msg, certificado, to.
        REFRESH: i_sscc, return_msg.

        MOVE 'PORTA' TO i_sscc-material.
        MOVE '1' TO i_sscc-quantidade.
        MOVE 'UN' TO i_sscc-uni.
        i_sscc-tipo_su = 'P6'."por defeito pq nao interessa o tipo
        APPEND i_sscc.

        SELECT SINGLE lzone INTO certificado
        FROM lagp WHERE lgnum = lgnum AND
        lgtyp = 'DCK' AND
        lgpla = bin_destino1.

*& Begin of Modification by Tiago Pateiro - ROFF @ 06.01.2016 08:50:57
*/ Renova França - determinacao de centro por armazém
**        SELECT werks UP TO 1 ROWS
**          FROM t320 INTO lv_werks
**          WHERE lgort EQ 'CD'
**            AND lgnum EQ lgnum.
**        ENDSELECT.
**        IF sy-subrc NE 0.
**          lv_werks  = 'RENV'.
**        ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 06.01.2016 08:51:20

        IF lv_werks IS INITIAL.
          IF lgnum = '100'.
            lv_werks = 'RENV'.
          ELSEIF lgnum = '150'.
            lv_werks = 'RFRA'.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse   = lgnum
            mov_type    = '930'
            bin_origem  = bin_destino1
            bin_destino = bin_destino1
*           plant       = 'RENV' " << DEL ROFF(SDF):TMGP:06.01.2016 08:52:27
            plant       = lv_werks " << INS ROFF(SDF):TMGP:06.01.2016 08:52:31
            s_loc       = lv_lgort
            certificado = certificado
          IMPORTING
            to          = to
          TABLES
            return_msg  = return_msg
            sscc        = i_sscc
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
        IF sy-subrc <> 0.
          LOOP AT return_msg WHERE msgtyp = 'E'.
            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
            NUMBER return_msg-msgnr
            WITH return_msg-msgv1 return_msg-msgv2.
          ENDLOOP.
        ELSE.

          UPDATE zwm028 SET  ot = to
          zlock = g_lock
          WHERE lgnum = lgnum AND
          refnr = g_grupo.

          IF sy-subrc = 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.

        ENDIF.
      ELSE.

      ENDIF.

    WHEN 'PLT' OR 'PLM'.
      IF g_desbloq = 'G'.
        UPDATE zwm028 SET zlock      = g_lock
                          st_pul     = st_destino
                          pulmao1    = bin_destino1
                          pulmao2    = bin_destino2
                          transporte = trans
                          tipo_lock  = g_desbloq
                      WHERE lgnum = lgnum
                        AND refnr = gs_out-refnr.
        COMMIT WORK AND WAIT.
      ELSEIF g_desbloq = 'R'.
        UPDATE zwm028 SET st_pul     = st_destino
                          pulmao1    = bin_destino1
                          pulmao2    = bin_destino2
                          transporte = trans
                          tipo_lock  = g_desbloq
                      WHERE lgnum = lgnum
                        AND refnr = gs_out-refnr.
        COMMIT WORK AND WAIT.

        UPDATE zwm028 SET zlock = g_lock
                      WHERE lgnum = lgnum
                        AND refnr = gs_out-refnr
                        AND ( remessa = ' ' OR ordem = '01' ).
        COMMIT WORK AND WAIT.


        IF NOT st_destino IS INITIAL.
          lv_lgtyp = st_destino.
        ELSE.
          lv_lgtyp = st_destino2.
        ENDIF.

        CALL FUNCTION 'Z_WMFR_SET_TAPETE'
          EXPORTING
            i_lgnum = lgnum
            i_lgtyp = lv_lgtyp
            i_lgpla = bin_destino1
            i_refnr = gs_out-refnr.


        SELECT SINGLE remessa FROM zwm028
                              INTO lv_vbeln
                              WHERE lgnum = lgnum AND
                                    refnr = gs_out-refnr AND
                                    ordem = '01'.

        CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
          EXPORTING
            i_lgnum = lgnum
            i_refnr = gs_out-refnr
            i_vbeln = lv_vbeln
            i_step  = 2.

**  Verificar se existem mais remessas com o mesmo recebedor de merc.
        IF NOT lt_vbpa[] IS INITIAL.

          SELECT SINGLE remessa INTO l_remessa
              FROM zwm028
                  WHERE lgnum = lgnum
                    AND refnr = gs_out-refnr
                    AND ordem = '01'.

          LOOP AT lt_vbpa WHERE vbeln = l_remessa.
            l_kunnr = lt_vbpa-kunnr.
            EXIT.
          ENDLOOP.

          LOOP AT lt_vbpa WHERE vbeln <> l_remessa
                            AND kunnr = l_kunnr.
            UPDATE zwm028 SET zlock = g_lock
                          WHERE lgnum = lgnum
                          AND refnr = gs_out-refnr
                          AND remessa = lt_vbpa-vbeln.
            COMMIT WORK AND WAIT.
          ENDLOOP.
        ENDIF.
      ENDIF.

      CLEAR wa_lagpv.
      SELECT SINGLE * FROM lagp
      WHERE lgnum = lgnum
      AND lgtyp = st_destino
      AND lgpla = bin_destino1.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING lagp TO wa_lagpv.
        wa_lagpv-brand = 'X'.
        CALL FUNCTION 'L_LAGP_VERAENDERN'
          EXPORTING
            xlagpv = wa_lagpv.

        COMMIT WORK AND WAIT.
      ENDIF.

      IF NOT bin_destino2 IS INITIAL.

        CLEAR wa_lagpv.
        SELECT SINGLE * FROM lagp
        WHERE lgnum = lgnum
        AND lgtyp = st_destino
        AND lgpla = bin_destino2.

        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lagp TO wa_lagpv.
          wa_lagpv-brand = 'X'.
          CALL FUNCTION 'L_LAGP_VERAENDERN'
            EXPORTING
              xlagpv = wa_lagpv.

          COMMIT WORK AND WAIT.
        ENDIF.

      ENDIF.
  ENDCASE.

  CLEAR: st_destino, st_destino2, bin_destino1, bin_kober1, bin_destino2,
  texto0400.
  g_ok_code = ok_code_0400.
  SET  SCREEN '0000'.LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_TIPO_CARGA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_tipo_carga INPUT.

  REFRESH posicoes.
  SELECT lgnum lgtyp INTO CORRESPONDING FIELDS OF TABLE posicoes
      FROM lagp
          WHERE lgnum = lgnum
            AND ( lgtyp = 'PUL' OR
                  lgtyp = 'DCK' OR
                  lgtyp = 'PLM' OR
                  lgtyp = 'PLT' ).

  SORT posicoes.
  DELETE ADJACENT DUPLICATES FROM posicoes COMPARING lgtyp.

  IF g_lock = 2.
*    DELETE posicoes WHERE lgtyp = 'DCK'.
  ELSEIF g_lock = 3 OR g_lock = 4.
    IF texto0400 IS INITIAL.
*      DELETE posicoes WHERE lgtyp = 'DCK'.
    ENDIF.
  ELSEIF g_lock = 5.
    DELETE posicoes WHERE lgtyp = 'PUL'
                      AND lgtyp = 'PLM'
                      AND lgtyp = 'PLT'.
  ENDIF.

*** --> [ROFF - SDF] João Dias - 11.11.2020 16:02:20  ***

*  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
*    EXPORTING
*      curow                        = 10
*      selectfield                  = 'LGNUM'
*      tablename                    = 'ZWM027'
*      given_value                  = ' '
*      titel                        = 'Posicoes'
*    IMPORTING
*      ind                          = index
*    TABLES
*      full_table                   = posicoes
*    EXCEPTIONS
*      no_tablefields_in_dictionary = 1
*      no_tablestructure_given      = 2
*      more_then_one_selectfield    = 3
*      no_selectfield               = 4
*      OTHERS                       = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE posicoes INDEX index.
*    IF sy-subrc = 0.
*      st_destino = posicoes-lgtyp.
*    ENDIF.
*  ENDIF.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title          = 'Posicoes'
      i_selection      = 'X'
      i_tabname        = 'posicoes'
      i_structure_name = 'ZWM027'
    IMPORTING
      es_selfield      = ls_selection
    TABLES
      t_outtab         = posicoes
    EXCEPTIONS
      program_error    = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE posicoes INTO ls_posicoes INDEX ls_selection-tabindex.
    IF sy-subrc = 0.
      st_destino = ls_posicoes-lgtyp.
    ENDIF.
  ENDIF.

*** <-- [ROFF - SDF] João Dias - 11.11.2020 16:02:20 ***

ENDMODULE.                 " HELP_TIPO_CARGA  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DESBLOQUEIO_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_desbloqueio_carga .
  DATA lv_valor TYPE zwm001-valor. " << INS ROFF(SDF):TMGP:25.01.2016 17:10:37

  DATA: num_clientes TYPE i,
        aux_kunnr    LIKE vbpa-kunnr.

*  IF zwm028-tipo_lock IS INITIAL.
  DO 1 TIMES.

    IF st_destino = 'DCK' OR  st_destino = 'PLT'.

      LOOP AT tab_out WHERE refnr = zwm028-refnr.
        IF NOT tab_out-palete_especial IS INITIAL.
          g_desbloq = 'R'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF g_desbloq IS INITIAL.

        CLEAR: lt_zwm028, num_clientes.
        REFRESH lt_zwm028.

        SELECT * INTO TABLE lt_zwm028
          FROM zwm028
              WHERE lgnum = lgnum
                AND refnr = zwm028-refnr
                AND remessa <> ' '.

        LOOP AT lt_zwm028 WHERE servisan = 'X'.
        ENDLOOP.
        IF sy-subrc = 0.
          g_desbloq = 'R'.
          EXIT.
        ENDIF.

        CLEAR: lt_vbpa, num_clientes.
        REFRESH lt_vbpa.

        SELECT * INTO TABLE lt_vbpa
            FROM vbpa
                FOR ALL ENTRIES IN lt_zwm028
                    WHERE vbeln EQ lt_zwm028-remessa
                      AND posnr = '000000'
                      AND parvw EQ 'W1'.

        SORT lt_vbpa BY kunnr.
        CLEAR aux_kunnr.
        LOOP AT lt_vbpa.
          IF aux_kunnr <> lt_vbpa-kunnr.
            num_clientes = num_clientes + 1.
            aux_kunnr = lt_vbpa-kunnr.
          ENDIF.
        ENDLOOP.

        IF num_clientes <= 1.
          g_desbloq = 'G'.
        ELSE.
          g_desbloq = 'R'.
        ENDIF.
      ENDIF.

    ELSEIF st_destino = 'PUL'.
      IF zwm028-total_paletes > 17.
        g_desbloq = 'R'.
      ELSE.
        IF bin_destino1+8(2) = '01'.
          g_desbloq = 'R'.
        ELSEIF bin_destino1+8(2) = '02'.
          g_desbloq = 'G'.
        ENDIF.
      ENDIF.
    ELSEIF st_destino = 'PLM'.
      g_desbloq = 'G'.
    ENDIF.
    IF st_destino2 = 'PPK'.
      g_desbloq = 'R'.
    ENDIF.
*  ELSE.
*    g_desbloq = zwm028-tipo_lock.
*  ENDIF.

  ENDDO.

** Desbloqueio Por Ordem de Venda e Tipo de Carro
***********************************************************************
  DO  1 TIMES.
    CHECK NOT zwm028-transporte IS INITIAL.

    CALL FUNCTION 'Z_WM_IS_SPECIAL_PICK_TRANS_OV'
      EXPORTING
        i_lgnum = lgnum
        i_refnr = zwm028-refnr
        i_tknum = zwm028-transporte
        i_vbeln = zwm028-remessa
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.
    CHECK sy-subrc EQ 0.
    g_desbloq = 'G'.
    EXIT.
  ENDDO.





**********************************************************************

*& Begin of Modification by Tiago Pateiro - ROFF @ 25.01.2016 17:09:02
  SELECT valor UP TO 1 ROWS
    FROM zwm001 INTO lv_valor
    WHERE armazem EQ lgnum
      AND processo EQ 'FIXA_G_DESBLOQ'
      AND parametro EQ 'VALOR'.
  ENDSELECT.
  IF lv_valor IS NOT INITIAL.
    g_desbloq = lv_valor.
  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 25.01.2016 17:09:03
ENDFORM.                    " GET_DESBLOQUEIO_CARGA
*&---------------------------------------------------------------------*
*&      Form  GET_CARACT_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_caract_carga .

  CLEAR: lt_zwm028,lt_vbpa, num_clientes.
  REFRESH: lt_zwm028, lt_vbpa.

  SELECT * INTO TABLE lt_zwm028
      FROM zwm028
          WHERE lgnum = lgnum
            AND refnr = zwm028-refnr
            AND remessa <> ' '.

  LOOP AT lt_zwm028 WHERE servisan = 'X'.
  ENDLOOP.
  IF sy-subrc = 0.
    CLEAR caract_carga.
    EXIT.

  ENDIF.

  SELECT * INTO TABLE lt_vbpa
      FROM vbpa
          FOR ALL ENTRIES IN lt_zwm028
              WHERE vbeln EQ lt_zwm028-remessa
                AND posnr = '000000'
                AND parvw EQ 'W1'.

  SORT lt_vbpa BY kunnr.
  CLEAR aux_kunnr.
  LOOP AT lt_vbpa.
    IF aux_kunnr <> lt_vbpa-kunnr.
      num_clientes = num_clientes + 1.
      aux_kunnr = lt_vbpa-kunnr.
    ENDIF.
  ENDLOOP.

  IF num_clientes > 1 AND texto0400 IS INITIAL.
    CLEAR caract_carga.
    EXIT.
  ENDIF.

  CLEAR wa_out.
  LOOP AT gt_out INTO wa_out
                 WHERE refnr = zwm028-refnr
                   AND palete_especial = 'X'.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    CLEAR caract_carga.
    EXIT.
  ENDIF.

  caract_carga = 'Característica de PLT'(068).
ENDFORM.                    " GET_CARACT_CARGA
*&---------------------------------------------------------------------*
*&      Form  GET_OTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ots.
  DATA: lt_t311a           TYPE TABLE OF t311a,
        lt_likp            TYPE TABLE OF likp,
        lt_itab_serv_2step LIKE TABLE OF itab_serv.

  DATA: ls_t311a TYPE t311a,
        ls_likp  TYPE likp.

  FIELD-SYMBOLS: <ls_itab_serv_2step> LIKE itab_serv.

  FREE: itab_28.
  CLEAR: itab_28.

  IF NOT gr_refnr IS INITIAL.
** Dados SERVISAN
    SELECT a~lgnum a~tanum a~vbeln a~refnr a~kquit a~queue a~kgvnq
           b~refnt
           c~tapos c~vltyp c~vlpla c~matnr c~vsolm c~nsolm c~meins
           c~pquit c~letyp c~vista
           c~qdatu c~qzeit c~qname c~pvqui c~edatu c~ezeit c~ename
           c~posty c~vlenr c~maktx c~posnr c~nsolm
           d~remessa
           e~num_recorrido e~sscc
           f~vkorg
    INTO CORRESPONDING FIELDS OF TABLE itab_serv
    FROM ( ( ( ( ( t311 AS b INNER JOIN ltak AS a
           ON b~lgnum = a~lgnum AND
              b~refnr = a~refnr )
               INNER JOIN ltap AS c
               ON c~lgnum = a~lgnum AND
                  c~tanum = a~tanum AND
                  c~vorga <> 'ST')
                  INNER JOIN zwm040 AS d
                  ON d~lgnum = a~lgnum AND
                     d~refnr = a~refnr AND
                     d~remessa = a~vbeln )
                     INNER JOIN likp AS f
                     ON f~vbeln = d~remessa )
                     LEFT OUTER JOIN zwm026 AS e
                     ON e~armazem = a~lgnum AND
                        e~grupo   = a~refnr AND
                        e~to_number = a~tanum )
    WHERE a~lgnum       EQ lgnum
      AND b~refnr       IN gr_refnr
      AND b~refnt       IN refnt
      AND b~datum       IN datum
      AND d~id_servisan IN r_id
      AND f~vkorg       IN vkorg
      AND b~kzdru       IN released
      AND ( a~queue = picking_queue OR
            a~queue = tri_queue     OR
            a~queue = dri_queue     OR
            a~queue = picking_prm   OR
            a~queue = queue_crd ).
  ELSE.
    MOVE: 'I'    TO grupo-sign,
          'EQ'   TO grupo-option,
          'NULL' TO grupo-low.

    APPEND grupo TO gr_refnr.
  ENDIF.


  IF NOT gr_refnr_2step IS INITIAL.

    DO 1 TIMES.
      SELECT a~lgnum a~tanum a~vbeln a~refnr a~kquit a~queue a~kgvnq
             b~refnt
             c~tapos c~vltyp c~vlpla c~matnr c~vsolm c~nsolm c~meins
             c~pquit c~letyp c~vista
             c~qdatu c~qzeit c~qname c~pvqui c~edatu c~ezeit c~ename
             c~posty c~vlenr c~maktx c~posnr c~nsolm
             d~remessa
             e~num_recorrido e~sscc
      INTO CORRESPONDING FIELDS OF TABLE lt_itab_serv_2step
      FROM ( ( ( ( t311 AS b INNER JOIN ltak AS a
             ON b~lgnum = a~lgnum AND
                b~refnr = a~refnr )
                 INNER JOIN ltap AS c
                 ON c~lgnum = a~lgnum AND
                    c~tanum = a~tanum AND
                    c~vorga <> 'ST')
                    INNER JOIN zwm040 AS d
                    ON d~lgnum = a~lgnum AND
                       d~refnr = a~refnr )
                       LEFT OUTER JOIN zwm026 AS e
                       ON e~armazem = a~lgnum AND
                          e~grupo   = a~refnr AND
                          e~to_number = a~tanum )
      WHERE a~lgnum       EQ lgnum
        AND b~refnr       IN gr_refnr_2step
        AND b~refnt       IN refnt
        AND b~datum       IN datum
        AND d~id_servisan IN r_id
        AND b~kzdru       IN released
        AND ( a~queue = picking_queue OR
              a~queue = tri_queue     OR
              a~queue = dri_queue     OR
              a~queue = picking_prm   OR
              a~queue = queue_crd ).


      CHECK NOT lt_itab_serv_2step IS INITIAL.

      SELECT * FROM t311a
         INTO TABLE lt_t311a
         FOR ALL ENTRIES IN lt_itab_serv_2step
         WHERE lgnum = lgnum AND
               refnr = lt_itab_serv_2step-refnr.

      CHECK sy-subrc EQ 0.
      SORT lt_t311a BY refnr.

      SELECT * FROM likp
         INTO TABLE lt_likp
         FOR ALL ENTRIES IN lt_t311a
         WHERE vbeln = lt_t311a-rbnum.

      CHECK sy-subrc EQ 0.
      SORT lt_likp BY vbeln.


      LOOP AT lt_itab_serv_2step ASSIGNING <ls_itab_serv_2step>.
        READ TABLE lt_t311a
              WITH KEY refnr = <ls_itab_serv_2step>-refnr
              BINARY SEARCH
              TRANSPORTING NO FIELDS.
        CHECK sy-subrc EQ 0.

        LOOP AT lt_t311a INTO ls_t311a FROM sy-tabix.
          IF ls_t311a-refnr <> <ls_itab_serv_2step>-refnr.
            EXIT.
          ENDIF.

          READ TABLE lt_likp
                WITH KEY vbeln = ls_t311a-rbnum
                BINARY SEARCH
                TRANSPORTING NO FIELDS.
          CHECK sy-subrc EQ 0.
          LOOP AT lt_likp INTO ls_likp FROM sy-tabix.
            IF ls_likp-vbeln <> ls_t311a-rbnum.
              EXIT.
            ENDIF.

            IF <ls_itab_serv_2step>-vkorg IS INITIAL.
              <ls_itab_serv_2step>-vkorg = ls_likp-vkorg.
            ELSEIF <ls_itab_serv_2step>-vkorg <> ls_likp-vkorg.
              <ls_itab_serv_2step>-vkorg = gc_vkorg_multi.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

      APPEND LINES OF lt_itab_serv_2step TO itab_serv.

    ENDDO.

  ELSE.
    MOVE: 'I'    TO grupo-sign,
          'EQ'   TO grupo-option,
          'NULL' TO grupo-low.

    APPEND grupo TO gr_refnr_2step.
  ENDIF.

** Zera Grupos
***********************************************************************

ENDFORM.                    " GET_OTS
*&---------------------------------------------------------------------*
*&      Form  LOAD_CAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_car .
  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname.

  DATA: lv_return TYPE c.

  DATA lt_return_msg TYPE tab_bdcmsgcoll.

  DATA: ls_return_msg TYPE bdcmsgcoll.

  DATA lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  READ TABLE gt_out INTO gs_out INDEX l_index.
  CHECK sy-subrc = 0.
  CHECK gs_out-zlock EQ '5'.

  SELECT *  FROM zwm028
            INTO TABLE lt_zwm028
            WHERE lgnum = lgnum AND
                  refnr = gs_out-refnr.

** Satus de Bloqueio
**********************************************************************
  LOOP AT lt_zwm028 TRANSPORTING NO FIELDS WHERE zlock <> '5'.
    MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '299'.
  ENDLOOP.

** Retorna Paletes no Pulmão
**********************************************************************
  READ TABLE lt_zwm028 INDEX 1.

** Retorna OT DCK
**********************************************************************
  SELECT SINGLE * FROM ltap
                  WHERE lgnum = lgnum AND
                        tanum = lt_zwm028-ot.

  SELECT SINGLE *
      FROM ltak
          WHERE lgnum = lgnum
            AND tanum = lt_zwm028-ot.
  IF ltak-kquit = 'X'.
    MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '300'.
  ENDIF.

  PERFORM confirm_step USING 'Carregar Carro Automáticamente?' '' '' lv_return.
  CHECK lv_return EQ 'J'.


  CALL FUNCTION 'Z_WM_AUTO_LOAD_CAR'
    EXPORTING
      i_lgnum       = lgnum
      i_refnr       = gs_out-refnr
    IMPORTING
      et_return_msg = lt_return_msg
    EXCEPTIONS
      error         = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
**  Erro ao carregar carro
    MESSAGE s298 DISPLAY LIKE 'E'.
    LOOP AT lt_return_msg INTO ls_return_msg WHERE msgtyp = 'E'.
      MESSAGE ID ls_return_msg-msgid TYPE ls_return_msg-msgtyp
              NUMBER ls_return_msg-msgnr
              WITH ls_return_msg-msgv1 ls_return_msg-msgv2.
    ENDLOOP.
  ELSE.
    MESSAGE s297.
  ENDIF.


  CLEAR flag_tree.
  not_first_time = 'X'.
  LEAVE TO SCREEN sy-dynnr.
ENDFORM.                    " LOAD_CAR
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_user.
  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname,
        l_index         TYPE lvc_nkey,
        l_subrc         LIKE sy-subrc.

  DATA: lt_fields	TYPE TABLE OF sval.

  DATA: ls_field TYPE sval.

  DATA: lv_uname TYPE uname.

  DATA: lv_returncode TYPE c.


  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  READ TABLE gt_out INTO gs_out INDEX l_index.
  CHECK sy-subrc = 0.

  IF     gs_out-status IS INITIAL AND
     NOT gs_out-zlock  IS INITIAL AND
     NOT gs_out-refnr  IS INITIAL.

    IF gs_out-zlock NE '5'.
      g_grupo = gs_out-refnr.


**      UPDATE zwm028 SET  userass      = lv_uname
**                    WHERE lgnum   = lgnum AND
**                          refnr   = g_grupo AND
**                          remessa = space.

      SELECT  SINGLE userass INTO lv_uname
                             FROM zwm028
                             WHERE lgnum   = lgnum AND
                                   refnr   = g_grupo AND
                                   remessa = space.

      ls_field-tabname = 'TRDYSE01CM'.
      ls_field-fieldname = 'USERNAME'.
      ls_field-value = lv_uname.
      APPEND ls_field TO lt_fields.




      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          popup_title     = text-072
        IMPORTING
          returncode      = lv_returncode
        TABLES
          fields          = lt_fields
        EXCEPTIONS
          error_in_fields = 1
          OTHERS          = 2.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.


      CHECK lv_returncode <> 'A'.

      CLEAR: ls_field.
      READ TABLE lt_fields
            INTO ls_field
            INDEX 1.

      lv_uname = ls_field-value.


      UPDATE zwm028 SET  userass      = lv_uname
                    WHERE lgnum   = lgnum AND
                          refnr   = g_grupo AND
                          remessa = space.

      COMMIT WORK.

      CALL METHOD tree1->frontend_update.
    ELSE.
** ERRO Impossível definir prioridades para status 5
      MESSAGE i000 WITH 'Impossível definir user para status 5'(073).
    ENDIF.
  ELSE.
** ERRO grupo inválido, ou selecionar nó do grupo
    MESSAGE i000 WITH 'Selecionar o nó do grupo'(056).
  ENDIF.

ENDFORM.                    " ASSIGN_USER
*&---------------------------------------------------------------------*
*&      Form  PROCESA_TO_PKL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM procesa_to_pkl .

  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname,
        l_index         TYPE lvc_nkey,
        l_subrc         LIKE sy-subrc.

  DATA: l_tanum  LIKE ltak-tanum,
        lv_tanum LIKE ltap-tanum,
        lv_tapos LIKE ltap-tapos,
        lv_vbeln TYPE vbeln,
        lv_subrc TYPE subrc.

  DATA: ls_ltap TYPE ltap.

  DATA l_ltap_cancl LIKE ltap_cancl OCCURS 0 WITH HEADER LINE.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  READ TABLE gt_out INTO gs_out INDEX l_index.
  CHECK sy-subrc = 0.

  IF NOT gs_out-to_partida_pkl IS INITIAL.

    CLEAR: lv_tanum, lv_tapos.
    lv_tanum = gs_out-tanum.
    lv_tapos = gs_out-tapos.

    SELECT SINGLE * FROM ltap
                    INTO ls_ltap
                    WHERE lgnum = lgnum AND
                          tanum = lv_tanum AND
                          tapos = lv_tapos.


    CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
      EXPORTING
        warehouse     = lgnum
        tanum         = lv_tanum
        tapos         = lv_tapos
      TABLES
        return_msg    = return_msg
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      DATA lv_lenum LIKE ltap-vlenr.

      CLEAR ltap.
      SELECT SINGLE * FROM ltap
          WHERE lgnum = lgnum AND
                tanum = lv_tanum AND
                tapos = lv_tapos.

*      IF ltap-letyp = 'P2' OR ltap-letyp = 'P5'.
      IF z_wm_cl_management=>is_remontada( i_lgnum = ltap-lgnum i_letyp = ltap-letyp ) EQ abap_true.
        CLEAR zwm020.
        SELECT SINGLE * FROM zwm020
            WHERE armazem = lgnum AND
                ( p1 = ltap-vlenr OR
                  p2 = ltap-vlenr ).
        IF ltap-vlenr = zwm020-p1.
          lv_lenum = zwm020-p2.
        ELSEIF ltap-vlenr = zwm020-p2.
          lv_lenum = zwm020-p1.
        ENDIF.

        CLEAR ltap.
        SELECT SINGLE * FROM ltap
          WHERE lgnum = lgnum AND
                vlenr = lv_lenum AND
                pquit = ' '.

        CLEAR: lv_tanum, lv_tapos.
        lv_tanum = ltap-tanum.
        lv_tapos = ltap-tapos.

        IF ltap-nltyp = '916'.
**          CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
**            EXPORTING
**              warehouse     = lgnum
**              tanum         = lv_tanum
**              tapos         = lv_tapos
**            TABLES
**              return_msg    = return_msg
**            EXCEPTIONS
**              error_message = 1
**              OTHERS        = 2.
        ELSE.

          CLEAR l_ltap_cancl.
          REFRESH l_ltap_cancl.

          l_ltap_cancl-tanum = lv_tanum.
          l_ltap_cancl-tapos = lv_tapos.
          APPEND l_ltap_cancl.

          CALL FUNCTION 'ZWM_CANCEL_TO'
            EXPORTING
              armazem      = lgnum
            TABLES
              t_ltap_cancl = l_ltap_cancl
            EXCEPTIONS
              error        = 1
              OTHERS       = 2.
        ENDIF.

      ENDIF.

      PERFORM reabastecimento_pkl CHANGING lv_subrc.
      IF lv_subrc EQ 4.
        PERFORM reabastecimento_picking USING abap_true.
        PERFORM reabastecimento_pkl CHANGING lv_subrc.
        IF lv_subrc <> 0.
**        Erro a reabstecer PKL
          MESSAGE s350 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.

*      CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 11.07.2012 17:12:13
*  Motivo: Cria OT's
*--------------------------------------------------------------------*
      lv_vbeln = gs_out-vbeln.

      CALL FUNCTION 'ZWM_TO_CREATE_OUT'
        EXPORTING
          warehouse     = lgnum
          refnr         = gs_out-refnr
          vbeln         = lv_vbeln
          posnr         = gs_out-posnr
*         vsola         = gs_out-nsolm
          meins         = gs_out-meins
          werks         = ls_ltap-werks
          lgort         = ls_ltap-lgort
          matnr         = ls_ltap-matnr
        TABLES
          return_msg    = return_msg
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
          EXPORTING
            i_lgnum = lgnum
            i_refnr = gs_out-refnr
            i_step  = 1.
      ENDIF.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    ENDIF.

    CALL METHOD tree1->change_node
      EXPORTING
        i_node_key    = l_selected_node
        i_outtab_line = gs_out.

* this method must be called to send the data to the frontend
    CALL METHOD tree1->frontend_update.

  ELSE.
** Selecionar OT Partida PKL
    MESSAGE i000 WITH 'Selecionar OT Partida PKL'(075).
  ENDIF.

  CLEAR flag_tree.
  not_first_time = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REABASTECIMENTO_PKL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reabastecimento_pkl CHANGING cv_subrc.
  DATA: lt_lqua       TYPE TABLE OF lqua WITH HEADER LINE,
        lt_sscc       LIKE zwm_sscc OCCURS 0 WITH HEADER LINE,
        lt_return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        lt_zwm001     LIKE zwm001 OCCURS 0 WITH HEADER LINE.

  DATA: ls_ltap TYPE ltap,
        ls_marm TYPE marm,
        ls_lqua TYPE lqua.

  DATA: l_quant_pkl LIKE zpalete_picking-uni_incom_p,
        lv_qtd      TYPE menge_d,
        lv_verme    TYPE menge_d,
        l_betyp     LIKE ltak-betyp,
        l_movpkl    LIKE ltak-bwlvs,
        l_tanum     TYPE tanum.


  CLEAR: cv_subrc.


  SELECT * FROM zwm001 INTO TABLE lt_zwm001
                 WHERE armazem   = lgnum
                   AND processo  = 'REABASTECIMENTO'.
  IF lt_zwm001[] IS INITIAL.
*    Não existem parametros definidos para este processo (tabela &)
    MESSAGE i156(zwmmsg001) WITH 'ZWM001'.
    cv_subrc = 2.
    EXIT.
  ENDIF.

  CLEAR lt_zwm001.
  LOOP AT lt_zwm001.
    CASE lt_zwm001-parametro.
      WHEN 'ST_BET'.
        l_betyp = lt_zwm001-valor.
      WHEN 'MOV1'.
        l_movpkl = lt_zwm001-valor.
    ENDCASE.
  ENDLOOP.


  SELECT SINGLE * FROM ltap
                  INTO ls_ltap
                  WHERE lgnum = lgnum AND
                        tanum = gs_out-tanum AND
                        tapos = gs_out-tapos.

  SELECT * FROM lqua INTO TABLE lt_lqua
             WHERE lgnum = lgnum
               AND lgtyp = 'PKL'
              AND matnr = ls_ltap-matnr
              AND werks = ls_ltap-werks
              AND lgort = ls_ltap-lgort
              AND lenum = ' '.


**  IF ls_ltap-charg IS NOT INITIAL.
**    DELETE lt_lqua WHERE charg <> ls_ltap-charg.
**  ENDIF.

  DATA l_quant_aux_pkl LIKE zpalete_picking-uni_incom_p.

  CLEAR l_quant_pkl.
  IF NOT lt_lqua[] IS INITIAL.

    LOOP AT lt_lqua.

      CLEAR ls_marm.
      SELECT SINGLE * FROM marm
                INTO ls_marm
                WHERE matnr = lt_lqua-matnr AND
                      meinh = ls_ltap-altme.

      lv_qtd = ( lt_lqua-verme * ls_marm-umren ) / ls_marm-umrez.

      CALL FUNCTION 'ROUND'
        EXPORTING
          input         = lv_qtd
          sign          = '-'
        IMPORTING
          output        = lv_verme
        EXCEPTIONS
          input_invalid = 1
          overflow      = 2
          type_invalid  = 3
          OTHERS        = 4.

      IF lv_verme <= 1.
        CONTINUE.
      ENDIF.

      CLEAR l_quant_aux_pkl.
      l_quant_aux_pkl = lt_lqua-verme.
      l_quant_pkl = l_quant_pkl + l_quant_aux_pkl.
    ENDLOOP.
  ENDIF.

  CHECK l_quant_pkl <= ls_ltap-vsolm.

  lt_sscc-material      = ls_ltap-matnr.
  lt_sscc-quantidade    = 1.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = ls_ltap-meins
    IMPORTING
      output         = lt_sscc-uni
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  APPEND lt_sscc.

  CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse   = lgnum
      mov_type    = l_movpkl                          "979
      plant       = ls_ltap-werks
      s_loc       = ls_ltap-lgort
      certificado = 'X'
      req_number  = gs_out-refnr
      req_type    = l_betyp
    IMPORTING
      to          = l_tanum
    TABLES
      return_msg  = lt_return_msg
      sscc        = lt_sscc
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF l_tanum IS INITIAL.
    cv_subrc = 4.
    EXIT.
  ELSE.
    DO 10 TIMES.

      CLEAR: l_quant_pkl, lt_lqua.
      REFRESH lt_lqua.

      SELECT * FROM lqua INTO TABLE lt_lqua
      WHERE lgnum = lgnum
        AND lgtyp = 'PKL'
        AND matnr = ls_ltap-matnr
        AND werks = ls_ltap-werks
        AND lgort = ls_ltap-lgort
        AND lenum = ' '.

      IF ls_ltap-charg IS NOT INITIAL.
        DELETE lt_lqua WHERE charg <> ls_ltap-charg.
      ENDIF.

      IF NOT lt_lqua[] IS INITIAL.
        LOOP AT lt_lqua.

          CLEAR ls_marm.
          SELECT SINGLE * FROM marm
                    INTO ls_marm
                    WHERE matnr = lt_lqua-matnr AND
                          meinh = ls_ltap-meins.

          lv_qtd = ( lt_lqua-verme * ls_marm-umren ) / ls_marm-umrez.

          CALL FUNCTION 'ROUND'
            EXPORTING
              input         = lv_qtd
              sign          = '-'
            IMPORTING
              output        = lv_verme
            EXCEPTIONS
              input_invalid = 1
              overflow      = 2
              type_invalid  = 3
              OTHERS        = 4.

          IF lv_verme <= 1.
            CONTINUE.
          ENDIF.

          CLEAR l_quant_aux_pkl.
          l_quant_aux_pkl = lt_lqua-verme.
          l_quant_pkl = l_quant_pkl + l_quant_aux_pkl.
        ENDLOOP.
      ENDIF.

      IF l_quant_pkl < ls_ltap-vsolm.
        WAIT UP TO 1 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.
