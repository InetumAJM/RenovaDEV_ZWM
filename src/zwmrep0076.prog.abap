************************************************************************
*                                                                      *
*      ***********************************************************     *
*     *      ROFF - Consultoria em Tecnologia de Informação       *    *
*     *                                                           *    *
*     *                           SAP                             *    *
*      ***********************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP....: ZWMREP0076                                            *
* Cliente......: Renova                                                *
* Descrição....: Manutenção Tabela Interface Display Portaria          *
* Criado por...: Fernando Lopes (ROFF)                                 *
* Criado em....: 19/01/2006                                            *
* Tipo PRG.....: Executável                                            *
************************************************************************
REPORT  zwmrep0076 MESSAGE-ID zwmmsg001.

** Opções de Selecção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_tab   AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN ULINE.
PARAMETERS: p_displ AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.


** Inicialização
************************************************************************
INITIALIZATION.

** Start-of-Selection
************************************************************************
START-OF-SELECTION.

************************************************************************
END-OF-SELECTION.

  IF p_tab IS INITIAL AND p_displ IS INITIAL.
    MESSAGE s000 WITH 'Seleccione pelo menos uma opção'.
    EXIT.
  ENDIF.

  IF p_tab EQ 'X'.
** Chama a actualização da tabela

    CALL TRANSACTION 'ZWM104'.

  ENDIF.


  IF p_displ EQ 'X'.
** Comunica com o Display

    CALL FUNCTION 'ZWM_RFC_DISPLAY_PORTARIA'
      EXPORTING
        modo            = 'D'
        chamada_rfc     = ' '
*       PORTA           =
*     TABLES
*       REGISTOS        =
      EXCEPTIONS
        erro            = 1
        erro_modo       = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
** Apresenta o Log de erro

      CALL FUNCTION 'APPL_LOG_DISPLAY'
       EXPORTING
         object                               = 'ZWM001'
         subobject                            = 'ZWM001'
*        EXTERNAL_NUMBER                      = ' '
*        OBJECT_ATTRIBUTE                     = 0
*        SUBOBJECT_ATTRIBUTE                  = 0
*        EXTERNAL_NUMBER_ATTRIBUTE            = 0
*        date_from                            =
*        time_from                            =
*        date_to                              =
*        time_to                              =
*        TITLE_SELECTION_SCREEN               = ' '
*        TITLE_LIST_SCREEN                    = ' '
*        COLUMN_SELECTION                     = '11112221122   '
         suppress_selection_dialog            = 'X'
*        COLUMN_SELECTION_MSG_JUMP            = '1'
*        EXTERNAL_NUMBER_DISPLAY_LENGTH       = 20
*        I_S_DISPLAY_PROFILE                  =
*        I_VARIANT_REPORT                     = ' '
*      IMPORTING
*        number_of_protocols                  =
       EXCEPTIONS
         no_authority                         = 1
         OTHERS                               = 2.

    ELSE.
      MESSAGE s000 WITH 'Actualização Display efectuada com sucesso'.
    ENDIF.

  ENDIF.
