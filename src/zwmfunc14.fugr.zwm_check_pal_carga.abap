FUNCTION zwm_check_pal_carga.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM
*"     VALUE(I_LGPLA) TYPE  LGTOR
*"     VALUE(I_SSCC) TYPE  EXIDV
*"     VALUE(I_SSCC2) TYPE  EXIDV
*"  EXPORTING
*"     VALUE(E_SUCESS) TYPE  FLAG
*"     VALUE(E_MSG) TYPE  ZWM_MSG
*"----------------------------------------------------------------------
  DATA: lv_pos    TYPE numc2.
  DATA: lv_tabix  TYPE sy-tabix.
  DATA: lv_dest   TYPE char14.
  DATA: ls_zwm013 TYPE zwm013.
  DATA: ls_zwm002 TYPE zwm002.
  DATA: ls_zwm006 TYPE zwm006_aux.
  DATA: ls_zwm028 TYPE zwm028.
  DATA: ls_zwm081 TYPE zwm081.
  DATA: lt_t311a  TYPE TABLE OF t311a  WITH HEADER LINE.
  DATA: lt_vttp   TYPE TABLE OF vttp   WITH HEADER LINE.
  DATA: lt_ltap   TYPE TABLE OF ltap   WITH HEADER LINE.
  DATA: lt_ltak   TYPE TABLE OF ltak   WITH HEADER LINE.
  DATA: lt_zwm081 TYPE TABLE OF zwm081 WITH HEADER LINE.
  DATA: lt_zwm013 TYPE TABLE OF zwm013 WITH HEADER LINE.

**********************************************************************
  "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
*  DATA lo_display_carga TYPE REF TO zcl_wm_gestao_cais_de_carga.
  DATA(lo_display_carga) = NEW zcl_wm_gestao_cais_de_carga( ).
  lo_display_carga->init( porta = i_lgpla ).
  "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)

** Validar Porta
**********************************************************************
  SELECT SINGLE *
    FROM zwm002 INTO ls_zwm002
    WHERE armazem = i_lgnum
    AND   porta   = i_lgpla.

  IF sy-subrc <> 0.

    " Porta & não é valida para o armazém &
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER 143 INTO e_msg WITH i_lgpla i_lgnum.

    "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
    lo_display_carga->ws_leitura_sscc( sscc    = i_sscc
                                       status  = 'E'
                                       message = CONV #( e_msg ) ).
    "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)

    EXIT.

  ELSEIF ls_zwm002-portico IS INITIAL.
*    EXIT.
  ENDIF.

  IF ls_zwm002-bloqueio_sscc EQ abap_true. "IS NOT INITIAL.

    " Porta & - Erro no pórtico - Bloqueio SSCC &
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER 141 INTO e_msg WITH i_lgpla ls_zwm002-sscc.
    "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
    lo_display_carga->ws_leitura_sscc( sscc    = i_sscc
                                       status  = 'E'
                                       message = CONV #( e_msg ) ).
    "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
    EXIT.

  ENDIF.

  IF ls_zwm002-num_entrada IS INITIAL.

    " Porta & não se encontra atribuída para carga
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER 144 INTO e_msg WITH i_lgpla.

    "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
    lo_display_carga->ws_leitura_sscc( sscc    = i_sscc
                                       status  = 'E'
                                       message = CONV #( e_msg ) ).
    "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)

    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zwm006_aux INTO ls_zwm006
    WHERE armazem     = i_lgnum
    AND   num_entrada = ls_zwm002-num_entrada.

  IF ls_zwm006-n_transporte IS INITIAL.

    " Porta & sem transporte atribuído
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER 145 INTO e_msg WITH i_lgpla.

    "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
    lo_display_carga->ws_leitura_sscc( sscc    = i_sscc
                                       status  = 'E'
                                       message = CONV #( e_msg ) ).
    "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)

    EXIT.
  ENDIF.


** Validar carga
**********************************************************************
  SELECT *
    FROM vttp INTO TABLE lt_vttp
    WHERE tknum = ls_zwm006-n_transporte.

  IF lt_vttp[] IS NOT INITIAL.
    SELECT *
      FROM t311a INTO TABLE lt_t311a
      FOR ALL ENTRIES IN lt_vttp
      WHERE lgnum = i_lgnum
      AND   rbtyp = 'L'
      AND   rbnum = lt_vttp-vbeln.
  ENDIF.

  READ TABLE lt_t311a INDEX 1.

  IF lt_t311a-refnr IS INITIAL.

    " Transporte & sem grupo definido
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER 146 INTO e_msg WITH ls_zwm006-n_transporte.

    "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
    lo_display_carga->ws_leitura_sscc( sscc    = i_sscc
                                       status  = 'E'
                                       message = CONV #( e_msg ) ).
    "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)

    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zwm028 INTO ls_zwm028
    WHERE lgnum = i_lgnum
    AND   refnr = lt_t311a-refnr.

  IF ls_zwm028 IS INITIAL.

    " Grupo & não está definido para carga
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER 147 INTO e_msg WITH lt_t311a-refnr.

    "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
    lo_display_carga->ws_leitura_sscc( refnr   = ls_zwm028-refnr
                                       sscc    = i_sscc
                                       status  = 'E'
                                       message = CONV #( e_msg ) ).
    "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)

    EXIT.
  ENDIF.

** Validar Palete
**********************************************************************
  SELECT SINGLE *
    FROM zwm013 INTO ls_zwm013
    WHERE armazem = i_lgnum
    AND   sscc    = i_sscc.

  IF sy-subrc = 0.
*   mESSAGE ID 'ZWM001' TYPE 'E' NUMBER '126' INTO e_msg.

    IF ls_zwm013-destino+4(10) <> ls_zwm002-pulmao_1.

      IF ls_zwm013-destino+4(10) <> ls_zwm002-pulmao_2.

        " Grupo & não está definido para carga
        MESSAGE ID 'ZWM001' TYPE 'E' NUMBER 148 INTO e_msg WITH i_sscc i_lgpla.

        "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
        lo_display_carga->ws_leitura_sscc( refnr   = ls_zwm028-refnr
                                           sscc    = i_sscc
                                           status  = 'E'
                                           message = CONV #( e_msg ) ).
        "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)

        EXIT.
      ENDIF.
    ENDIF.

  ELSE.

    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = i_lgnum
      AND   vlenr = i_sscc.

    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM ltak INTO TABLE lt_ltak
        FOR ALL ENTRIES IN lt_ltap
        WHERE lgnum = lt_ltap-lgnum
        AND   tanum = lt_ltap-tanum.
    ENDIF.

    DELETE lt_ltak WHERE refnr IS INITIAL.

    CLEAR lt_ltak.
    READ TABLE lt_ltak INDEX 1.

    IF lt_ltak-refnr <> ls_zwm028-refnr.

      " Palete & não pertence ao grupo &
      MESSAGE ID 'ZWM001' TYPE 'E' NUMBER 149 INTO e_msg WITH i_sscc ls_zwm028-refnr.

      "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
      lo_display_carga->ws_leitura_sscc( refnr   = ls_zwm028-refnr
                                         sscc    = i_sscc
                                         status  = 'E'
                                         message = CONV #( e_msg ) ).
      "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
      EXIT.
    ENDIF.

  ENDIF.

  SELECT SINGLE *
    FROM zwm081 INTO ls_zwm081
    WHERE armazem = i_lgnum
    AND   sscc    = i_sscc.

  IF sy-subrc = 0.

    " A Palete & já foi carregada no camião
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER 150 INTO e_msg WITH i_sscc.

    "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
    lo_display_carga->ws_leitura_sscc( refnr   = ls_zwm028-refnr
                                       sscc    = i_sscc
                                       status  = 'W'
                                       message = CONV #( e_msg ) ).
    "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
    EXIT.
  ENDIF.

** Validar Sequência
**********************************************************************
*  SELECT *
*    FROM zwm013 INTO TABLE lt_zwm013
*    WHERE armazem = i_lgnum
*    AND   destino = ls_zwm013-destino.
*
*  lv_dest = ls_zwm013-destino.
*
*  IF lv_dest+13(1) = '1'.
*    lv_dest+13(1) = '2'.
*
*  ELSEIF lv_dest+13(1) = '2'.
*    lv_dest+13(1) = '1'.
*  ENDIF.
*
*  SELECT *
*    FROM zwm013 APPENDING TABLE lt_zwm013
*    WHERE armazem = i_lgnum
*    AND   destino = lv_dest.
*
*  SELECT *
*    FROM zwm081 INTO TABLE lt_zwm081
*    WHERE armazem = i_lgnum
*    AND   refnr   = ls_zwm028-refnr.
*
*  IF sy-subrc <> 0.
*    CLEAR lv_pos.
*  ENDIF.
*
*** Paletes já lidas
*  LOOP AT lt_zwm013.
*
*    lv_tabix = sy-tabix.
*
*    READ TABLE lt_zwm081 WITH KEY sscc = lt_zwm013-sscc.
*    CHECK sy-subrc = 0.
*
*    DELETE lt_zwm013 INDEX lv_tabix.
*  ENDLOOP.
*
*  SORT lt_zwm013 BY destino posicao_pulmao.
*
*  READ TABLE lt_zwm013 INDEX 1.
*
*  IF lt_zwm013-destino <> ls_zwm013-destino.
*
*    e_msg = 'A Palete & com sequencia errada!'.
*
*    REPLACE FIRST OCCURRENCE OF '&' IN e_msg WITH i_sscc.
*    EXIT.
*
*  ELSE.
*    lv_pos = lt_zwm013-posicao_pulmao.
*
*    IF ls_zwm013-posicao_pulmao <> lv_pos.
*
*      e_msg = 'A Palete & com sequencia errada!'.
*
*      REPLACE FIRST OCCURRENCE OF '&' IN e_msg WITH i_sscc.
*      EXIT.
*    ENDIF.
*  ENDIF.

** Mensagem de Sucesso
**********************************************************************
  e_sucess = 'X'.

  " Palete & lida corretamente!
  MESSAGE ID 'ZWM001' TYPE 'S' NUMBER 151 INTO e_msg WITH i_sscc.

  "Ini-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)
  lo_display_carga->ws_leitura_sscc( refnr   = ls_zwm028-refnr
                                     sscc    = i_sscc
                                     status  = 'S'
                                     message = CONV #( e_msg ) ).
  "Fim-Inetum/AJM-01_08_24-Atualiza Display de Carga(Fiori)

  CLEAR ls_zwm081.

  GET TIME.

  MOVE-CORRESPONDING ls_zwm013 TO ls_zwm081.

  ls_zwm081-armazem = i_lgnum.
  ls_zwm081-sscc    = i_sscc.
  ls_zwm081-porta   = i_lgpla.
  ls_zwm081-refnr   = ls_zwm028-refnr.
  ls_zwm081-datum   = sy-datum.
  ls_zwm081-uzeit   = sy-uzeit.

  MODIFY zwm081 FROM ls_zwm081.
  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.

ENDFUNCTION.
