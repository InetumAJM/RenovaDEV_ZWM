************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0064                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Arquivamento de registos de tabelas ZWM                  *
* Criado por: Fernando Lopes                                           *
* Criado em.: 11/01/2006                                               *
*                                                                      *
************************************************************************
REPORT zwmrep0064 MESSAGE-ID zwmmsg001.

************************************************************************
** Tabelas DD
************************************************************************
TABLES: t311.

************************************************************************
** Variáveis
************************************************************************
DATA: data_limite LIKE sy-datum,
      indice      LIKE sy-tabix,
      linhas      TYPE i,
      resposta.

DATA: BEGIN OF it_t311 OCCURS 0,
        lgnum LIKE t311-lgnum,
        refnr LIKE t311-refnr,
      END OF it_t311.

DATA: it_z005 LIKE zwm005     OCCURS 0 WITH HEADER LINE,
      it_z006 LIKE zwm006_aux OCCURS 0 WITH HEADER LINE,
      it_z026 LIKE zwm026     OCCURS 0 WITH HEADER LINE,
      it_z028 LIKE zwm028     OCCURS 0 WITH HEADER LINE,
      it_z040 LIKE zwm040     OCCURS 0 WITH HEADER LINE,
      it_efa  LIKE zwm_log_efacec OCCURS 0 WITH HEADER LINE.


************************************************************************
** Opções de selecção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum LIKE t311-lgnum    OBLIGATORY.
PARAMETERS: p_data  LIKE sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
PARAMETERS: p_grupo  RADIOBUTTON GROUP list DEFAULT 'X',
            p_portar RADIOBUTTON GROUP list,
            p_logefa RADIOBUTTON GROUP list.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-b03.
PARAMETERS: p_arq  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.

************************************************************************
** Initialization
************************************************************************
INITIALIZATION.

  CALL FUNCTION 'SUBTRACT_TIME_FROM_DATE'
    EXPORTING
      i_idate                     = sy-datum
      i_time                      = 1
      i_iprkz                     = '2'
*     I_RDMHD                     =
    IMPORTING
      o_idate                     = data_limite
    EXCEPTIONS
      invalid_period              = 1
      invalid_round_up_rule       = 2
      internal_error              = 3
      OTHERS                      = 4.

  IF sy-subrc <> 0.
    data_limite = sy-datum - 31.
  ENDIF.

  p_data = data_limite.

************************************************************************
** At Selection-screen.
************************************************************************
AT SELECTION-SCREEN ON p_data.

  IF p_data > data_limite.
    MESSAGE e000 WITH 'Só permitidas datas a mais de um mês'
                      'da data actual'.
  ENDIF.

************************************************************************
** Start-of-Selection
************************************************************************
START-OF-SELECTION.
  CLEAR:   it_t311, it_z005, it_z006, it_z026, it_z028, it_z040, it_efa.
  REFRESH: it_t311, it_z005, it_z006, it_z026, it_z028, it_z040, it_efa.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 15
      text       = 'A obter dados'.

  IF p_grupo EQ 'X'.

    SELECT lgnum refnr FROM t311
            INTO CORRESPONDING FIELDS OF TABLE it_t311
            WHERE datum <= p_data.

    DELETE it_t311 WHERE lgnum NE p_lgnum.

    IF NOT it_t311[] IS INITIAL.

*      SELECT * FROM zwm026
*               INTO TABLE it_z026
*                FOR ALL ENTRIES IN it_t311
*              WHERE armazem EQ p_lgnum
*                AND grupo   EQ it_t311-refnr.

      SORT it_t311 BY refnr.

      SELECT * FROM zwm026
               INTO TABLE it_z026
              WHERE armazem EQ p_lgnum.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 55
          text       = 'A obter dados'.

      SORT it_z026 BY grupo.

      LOOP AT it_z026.
        indice = sy-tabix.
        READ TABLE it_t311 WITH KEY refnr = it_z026-grupo BINARY SEARCH.
        IF sy-subrc NE 0.
          DELETE it_z026 INDEX indice.
        ENDIF.
      ENDLOOP.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 75
          text       = 'A obter dados'.

      SELECT * FROM zwm028
               INTO TABLE it_z028
                FOR ALL ENTRIES IN it_t311
              WHERE lgnum EQ p_lgnum
                AND refnr EQ it_t311-refnr.

      SELECT * FROM zwm040
               INTO TABLE it_z040
                FOR ALL ENTRIES IN it_t311
              WHERE lgnum EQ p_lgnum
                AND refnr EQ it_t311-refnr.

    ENDIF.

  ELSEIF p_portar EQ 'X'.

*    MESSAGE i000 WITH 'Opção de Portaria Desligada'.
*    EXIT.

    SELECT * FROM zwm005
             INTO TABLE it_z005
            WHERE armazem EQ p_lgnum
              AND data    <= p_data.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 55
        text       = 'A obter dados'.

    SELECT * FROM zwm006_aux
             INTO TABLE it_z006
            WHERE armazem  EQ p_lgnum
              AND data_reg <= p_data.

  ELSEIF p_logefa EQ 'X'.

    SELECT * FROM zwm_log_efacec
             INTO TABLE it_efa
            WHERE data <= p_data.

  ENDIF.

************************************************************************
END-OF-SELECTION.

*  IF p_arq EQ 'X'.
*** Confirma Opção
*
*    CLEAR resposta.
*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        titlebar                  = 'Arquivamento registos tabelas ZWM'
**       DIAGNOSE_OBJECT             = ' '
*        text_question               = 'Prosseguir com arquivamento ?'
*        text_button_1               = 'Sim'
**       ICON_BUTTON_1               = ' '
*        text_button_2               = 'Não'
**       ICON_BUTTON_2               = ' '
*        default_button              = '2'
**       DISPLAY_CANCEL_BUTTON       = 'X'
**       USERDEFINED_F1_HELP         = ' '
**       START_COLUMN                = 25
**       START_ROW                   = 6
**       POPUP_TYPE                  =
**       IV_QUICKINFO_BUTTON_1       = ' '
**       IV_QUICKINFO_BUTTON_2       = ' '
*      IMPORTING
*        answer                      = resposta.
**     TABLES
**       PARAMETER                   =
**     EXCEPTIONS
**       TEXT_NOT_FOUND              = 1
**       OTHERS                      = 2
*
*    IF resposta <> '1'.
*      CLEAR p_arq.
*    ENDIF.
*
*  ENDIF.

  IF p_arq EQ 'X'.

** Arquiva os registos para as respectivas tabelas de histórico

    IF NOT it_efa[] IS INITIAL.
      INSERT zwm_log_efacec_h FROM TABLE it_efa.
      IF sy-subrc EQ 0.
        DELETE zwm_log_efacec FROM TABLE it_efa.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE e000 WITH 'Erro arquivamento tabela ZWM_LOG_EFACEC'.
        ENDIF.
      ELSE.
        ROLLBACK WORK.
        MESSAGE e000 WITH 'Erro arquivamento tabela ZWM_LOG_EFACEC'.
      ENDIF.
    ENDIF.


    IF NOT it_z005[] IS INITIAL.
      INSERT zwm005h FROM TABLE it_z005.
      IF sy-subrc EQ 0.
        DELETE zwm005 FROM TABLE it_z005.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE e000 WITH 'Erro arquivamento tabela ZWM005'.
        ENDIF.
      ELSE.
        ROLLBACK WORK.
        MESSAGE e000 WITH 'Erro arquivamento tabela ZWM005'.
      ENDIF.
    ENDIF.

    IF NOT it_z006[] IS INITIAL.
      INSERT zwm006_auxh FROM TABLE it_z006.
      IF sy-subrc EQ 0.
        DELETE zwm006_aux FROM TABLE it_z006.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE e000 WITH 'Erro arquivamento tabela ZWM006_AUX'.
        ENDIF.
      ELSE.
        ROLLBACK WORK.
        MESSAGE e000 WITH 'Erro arquivamento tabela ZWM006_AUX'.
      ENDIF.
    ENDIF.

    IF NOT it_z026[] IS INITIAL.
      INSERT zwm026h FROM TABLE it_z026.
      IF sy-subrc EQ 0.
        DELETE zwm026 FROM TABLE it_z026.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE e000 WITH 'Erro arquivamento tabela ZWM026'.
        ENDIF.
      ELSE.
        ROLLBACK WORK.
        MESSAGE e000 WITH 'Erro arquivamento tabela ZWM026'.
      ENDIF.
    ENDIF.

    IF NOT it_z028[] IS INITIAL.
      MODIFY zwm028h FROM TABLE it_z028.
      IF sy-subrc EQ 0.
        DELETE zwm028 FROM TABLE it_z028.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE e000 WITH 'Erro arquivamento tabela ZWM028'.
        ENDIF.
      ELSE.
        ROLLBACK WORK.
        MESSAGE e000 WITH 'Erro arquivamento tabela ZWM028'.
      ENDIF.
    ENDIF.

    IF NOT it_z040[] IS INITIAL.
      INSERT zwm040h FROM TABLE it_z040.
      IF sy-subrc EQ 0.
        DELETE zwm040 FROM TABLE it_z040.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE e000 WITH 'Erro arquivamento tabela ZWM040'.
        ENDIF.
      ELSE.
        ROLLBACK WORK.
        MESSAGE e000 WITH 'Erro arquivamento tabela ZWM040'.
      ENDIF.
    ENDIF.

** Log
    IF p_portar EQ 'X'.

      DESCRIBE TABLE it_z005 LINES linhas.
      WRITE: / 'Registos da tabela ZWM005 arquivados:    ', linhas.
      DESCRIBE TABLE it_z006 LINES linhas.
      WRITE: / 'Registos da tabela ZWM006_AUX arquivados:', linhas.

    ELSEIF p_grupo EQ 'X'.

      DESCRIBE TABLE it_z026 LINES linhas.
      WRITE: / 'Registos da tabela ZWM026 arquivados:', linhas.
      DESCRIBE TABLE it_z028 LINES linhas.
      WRITE: / 'Registos da tabela ZWM028 arquivados:', linhas.
      DESCRIBE TABLE it_z040 LINES linhas.
      WRITE: / 'Registos da tabela ZWM040 arquivados:', linhas.

    ELSEIF p_logefa EQ 'X'.

      DESCRIBE TABLE it_efa LINES linhas.
      WRITE: / 'Registos da tabela ZWM_LOG_EFACEC arquivados:', linhas.

    ENDIF.

  ELSE.

** Apenas Log
    IF p_portar EQ 'X'.

      DESCRIBE TABLE it_z005 LINES linhas.
      WRITE: / 'Registos da tabela ZWM005 a arquivar:    ', linhas.
      DESCRIBE TABLE it_z006 LINES linhas.
      WRITE: / 'Registos da tabela ZWM006_AUX a arquivar:', linhas.

    ELSEIF p_grupo EQ 'X'.

      DESCRIBE TABLE it_z026 LINES linhas.
      WRITE: / 'Registos da tabela ZWM026 a arquivar:', linhas.
      DESCRIBE TABLE it_z028 LINES linhas.
      WRITE: / 'Registos da tabela ZWM028 a arquivar:', linhas.
      DESCRIBE TABLE it_z040 LINES linhas.
      WRITE: / 'Registos da tabela ZWM040 a arquivar:', linhas.

    ELSEIF p_logefa EQ 'X'.

      DESCRIBE TABLE it_efa LINES linhas.
      WRITE: / 'Registos da tabela ZWM_LOG_EFACEC arquivados:', linhas.


    ENDIF.

    WRITE: /.
    WRITE: / 'Seleccionar opção de arquivamento se desejar',
             'arquivar estes registos'.
  ENDIF.

  CLEAR: it_t311, it_z005, it_z006, it_z026, it_z028, it_z040.
  FREE:  it_t311, it_z005, it_z006, it_z026, it_z028, it_z040.
