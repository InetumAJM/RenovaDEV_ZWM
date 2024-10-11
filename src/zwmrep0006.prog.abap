*&---------------------------------------------------------------------*
*& Report  ZWMREP0006                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0006 MESSAGE-ID zwmmsg001.
TABLES: zwm010.

INCLUDE:rlmobinc.

TABLES:zwm001.

DATA : BEGIN OF ti_zwm001 OCCURS 0.
         INCLUDE STRUCTURE zwm001.
       DATA: END OF ti_zwm001.

*dados gerais
DATA: ok_code_0001 LIKE sy-ucomm.
DATA: equipamento TYPE zwm010-equipamento.
DATA: text TYPE  bdcmsgcoll-msgv1.
DATA: cursorfield(20),
      return_msg TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: tab_zwm010 LIKE zwm010 OCCURS 0 WITH HEADER LINE.
DATA: ecran(4).
DATA: tab_zwm011 LIKE zwm011.




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
  SET PF-STATUS 'ZRF'.

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
  DATA: tipo, sentido.
  DATA: valor            TYPE zwm001-valor,
        equipamento_trie TYPE zwm001-valor,
        equipamento_tri  TYPE zwm001-valor,
        equipamento_con  TYPE zwm001-valor,
        equipamento_ret  TYPE zwm001-valor,
        equipamento_ppa  TYPE zwm001-valor,
        lv_equip_aut     TYPE zwm001-valor. " << INS ROFF(SDF):TMGP:27.01.2016 10:02:08

**  Maquinas para descargas
  DATA : equipamento_ppad TYPE zwm001-valor,
         equipamento_ppac TYPE zwm001-valor,
         equipamento_cond TYPE zwm001-valor,
         equipamento_retp TYPE zwm001-valor.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
                             'GERAL'
                             'EQUIPAMENTO_CON'
                              valor.
  WRITE valor TO equipamento_con LEFT-JUSTIFIED.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
                              'GERAL'
                              'EQUIPAMENTO_TRI'
                               valor.
  WRITE valor TO equipamento_tri LEFT-JUSTIFIED.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
                              'GERAL'
                              'EQUIPAMENTO_TRIE'
                               valor.
  WRITE valor TO equipamento_trie LEFT-JUSTIFIED.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
                              'GERAL'
                              'EQUIPAMENTO_RET'
                               valor.
  WRITE valor TO equipamento_ret LEFT-JUSTIFIED.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
        'GERAL'
        'EQUIPAMENTO_RETP'
        valor.
  WRITE valor TO equipamento_retp LEFT-JUSTIFIED.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
                              'GERAL'
                              'EQUIPAMENTO_PPA'
                               valor.
  WRITE valor TO equipamento_ppa LEFT-JUSTIFIED.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
                             'GERAL'
                             'EQUIPAMENTO_COND'
                              valor.
  WRITE valor TO equipamento_cond LEFT-JUSTIFIED.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
                              'GERAL'
                              'EQUIPAMENTO_PPAD'
                               valor.
  WRITE valor TO equipamento_ppad LEFT-JUSTIFIED.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
                              'GERAL'
                              'EQUIPAMENTO_PPAC'
                               valor.
  WRITE valor TO equipamento_ppac LEFT-JUSTIFIED.

*& Begin of Modification by Tiago Pateiro - ROFF @ 27.01.2016 10:01:19
*/ RNFROL00023
  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum 'GERAL' 'EQUIPAMENTO_AUT' valor.

  lv_equip_aut = valor.
  CONDENSE lv_equip_aut.
*& End of Modification by Tiago Pateiro - ROFF @ 27.01.2016 10:01:20

  CLEAR tipo.
  CASE ok_code_0001.
    WHEN 'NEXT'.

      IF NOT equipamento IS INITIAL.

        IF equipamento = equipamento_tri   OR
           equipamento = equipamento_trie.
***********************TRILATERAIS****************************

**        Sentido prioritario (Corredores esquerdos ou direitos)
          IF equipamento = equipamento_tri.
            sentido = 'D'.   " Direita
          ELSE.
            sentido = 'E'.   " Esquerda
          ENDIF.

          REFRESH return_msg.
          CALL FUNCTION 'ZWM_GET_TO_TRI'
            EXPORTING
              armazem           = xuser-lgnum
              tamanho           = lrf_wkqu-devty(5)
              sentido           = sentido
            IMPORTING
              nova_to           = tab_zwm011
              ecra              = ecran
              tipo_queue        = tipo
            TABLES
              l_zwm010          = tab_zwm010
              return_msg        = return_msg
            EXCEPTIONS
              no_equipment      = 1
              no_work_available = 2
              OTHERS            = 3.

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

              CLEAR equipamento.
              IF lrf_wkqu-devty(5) = '16X20'.
                SET SCREEN '0001'.LEAVE SCREEN.
              ELSE.
                SET SCREEN '0002'.LEAVE SCREEN.
              ENDIF.
            ENDIF.
          ELSE.
            CALL FUNCTION 'ZWM_CALL_TASK_TRI'
              EXPORTING
                armazem     = xuser-lgnum
                ecran       = ecran
                tab_zwm011  = tab_zwm011
                tipo_queue  = tipo
                equipamento = equipamento
                primeira    = 'X'.

          ENDIF.

        ELSEIF equipamento = equipamento_con  OR
               equipamento = equipamento_ppa  OR
               equipamento = equipamento_cond OR
               equipamento = equipamento_ppad OR
               equipamento = equipamento_ppac.

***********************CONVENCIONAL****************************
***********************Porta Paletes***************************
          CALL FUNCTION 'ZWM_GET_TO_RET'
            EXPORTING
              armazem           = xuser-lgnum
              tamanho           = lrf_wkqu-devty(5)
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
            READ TABLE return_msg INDEX 1.
            IF sy-subrc = 0.
              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = return_msg-msgid
                  message_lang   = sy-langu
                  message_type   = return_msg-msgtyp
                  message_number = return_msg-msgnr
                  message_var1   = return_msg-msgv1.
              CLEAR equipamento.
              IF lrf_wkqu-devty(5) = '16X20'.
                SET SCREEN '0001'.LEAVE SCREEN.
              ELSE.
                SET SCREEN '0002'.LEAVE SCREEN.
              ENDIF.
            ENDIF.

          ELSE.
            CALL FUNCTION 'ZWM_CALL_TASK_RET'
              EXPORTING
                armazem     = xuser-lgnum
                ecran       = ecran
                tab_zwm011  = tab_zwm011
                tipo_queue  = tipo
                equipamento = equipamento
                primeira    = 'X'.
          ENDIF.

        ELSEIF equipamento = equipamento_ret OR
               equipamento = equipamento_retp.
***********************RETRACTIL****************************

          CALL FUNCTION 'ZWM_GET_TO_PIC'
            EXPORTING
              armazem           = xuser-lgnum
              tamanho           = lrf_wkqu-devty(5)
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
            READ TABLE return_msg INDEX 1.
            IF sy-subrc = 0.
              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = return_msg-msgid
                  message_lang   = sy-langu
                  message_type   = return_msg-msgtyp
                  message_number = return_msg-msgnr
                  message_var1   = return_msg-msgv1.
              CLEAR equipamento.
              IF lrf_wkqu-devty(5) = '16X20'.
                SET SCREEN '0001'.LEAVE SCREEN.
              ELSE.
                SET SCREEN '0002'.LEAVE SCREEN.
              ENDIF.
            ENDIF.
          ELSE.

            CALL FUNCTION 'ZWM_CALL_TASK_RET'
              EXPORTING
                armazem     = xuser-lgnum
                ecran       = ecran
                tab_zwm011  = tab_zwm011
                tipo_queue  = tipo
                equipamento = equipamento
                primeira    = 'X'.

          ENDIF.
*& Begin of Modification by Tiago Pateiro - ROFF @ 27.01.2016 10:15:39
*/ RNFROL00023
        ELSEIF equipamento EQ lv_equip_aut.
          CALL FUNCTION 'Z_WMFR_RF_PICKING_AUT'
            EXPORTING
              is_xuser    = xuser
              i_equipment = equipamento.
*& End of Modification by Tiago Pateiro - ROFF @ 27.01.2016 10:15:40

*& Begin of Modification by Carlos Fernandes - ROFF @ 10.02.2016
*/ RNFROL00025
          CLEAR equipamento.
          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0001'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0002'.LEAVE SCREEN.
          ENDIF.
*& End of Modification by by Carlos Fernandes - ROFF @ 10.02.2016
        ENDIF.

        CLEAR equipamento.
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

  IF NOT equipamento IS INITIAL.
*    REFRESH TAB_ZWM010.
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
