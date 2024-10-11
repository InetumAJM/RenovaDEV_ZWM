************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0075                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Visualização de Logs                                     *
* Criado por: Fernando Lopes                                           *
* Criado em.: 17/01/2006                                               *
*                                                                      *
************************************************************************
REPORT zwmrep0075 MESSAGE-ID zwmmsg001 .

** Constantes
************************************************************************
CONSTANTS: objecto TYPE balobj_d VALUE 'ZWM001'.  "Log ZWM

** Variáveis
************************************************************************
DATA: subobjecto  TYPE balsubobj,
      n_protocols LIKE sy-dbcnt,
      dialog.

** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(17) text-s01.
PARAMETERS p_data_f LIKE sy-datum OBLIGATORY.
PARAMETERS p_time_f LIKE sy-uzeit OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(17) text-s02.
PARAMETERS p_data_t LIKE sy-datum OBLIGATORY.
PARAMETERS p_time_t LIKE sy-uzeit OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.
PARAMETERS p_opcoes AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
PARAMETERS op1 RADIOBUTTON GROUP radi.
PARAMETERS op2 RADIOBUTTON GROUP radi.
SELECTION-SCREEN END OF BLOCK b2.


** Initialization
************************************************************************
INITIALIZATION.

  p_data_f = sy-datum.
  p_data_t = sy-datum.

  p_time_f = '000000'.
  p_time_t = '235959'. "sy-uzeit.


** Start-of-Selection
************************************************************************
START-OF-SELECTION.

** Valida opções de selecção
  IF p_data_f > p_data_t.
    MESSAGE s000 WITH 'Datas inválidas'.
    EXIT.
  ENDIF.
  IF p_data_f = p_data_t AND p_time_f > p_time_t.
    MESSAGE s000 WITH 'Horas inválidas'.
    EXIT.
  ENDIF.

** Subobjecto de Log
  IF op1 EQ 'X'.
    subobjecto = 'ZWM001'.
  ELSEIF op2 EQ 'X'.
    subobjecto = '*'.
  ENDIF.

** Opções extras de selecção
  IF p_opcoes EQ 'X'.
    CLEAR dialog.
  ELSE.
    dialog = 'X'.
  ENDIF.

** Apresenta Log
  CLEAR n_protocols.
  CALL FUNCTION 'APPL_LOG_DISPLAY'
   EXPORTING
     object                               = objecto
     subobject                            = subobjecto
*    EXTERNAL_NUMBER                      = ' '
*    OBJECT_ATTRIBUTE                     = 0
*    SUBOBJECT_ATTRIBUTE                  = 0
*    EXTERNAL_NUMBER_ATTRIBUTE            = 0
     date_from                            = p_data_f
     time_from                            = p_time_f
     date_to                              = p_data_t
     time_to                              = p_time_t
*    TITLE_SELECTION_SCREEN               = ' '
*    TITLE_LIST_SCREEN                    = ' '
*    COLUMN_SELECTION                     = '11112221122   '
     suppress_selection_dialog            = dialog
*    COLUMN_SELECTION_MSG_JUMP            = '1'
*    EXTERNAL_NUMBER_DISPLAY_LENGTH       = 20
*    I_S_DISPLAY_PROFILE                  =
*    I_VARIANT_REPORT                     = ' '
   IMPORTING
     number_of_protocols                  = n_protocols
   EXCEPTIONS
     no_authority                         = 1
     OTHERS                               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF n_protocols IS INITIAL.
    MESSAGE s000 WITH 'Sem Log a exibir'.
    EXIT.
  ENDIF.
