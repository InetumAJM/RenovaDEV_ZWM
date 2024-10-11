*&---------------------------------------------------------------------*
*& Report  ZWMREP0084
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zwmrep0084 MESSAGE-ID zwmmsg001.

INCLUDE: zwmrep0084top,
         zwmrep0084f01,
         zwm_constants.

TOP-OF-PAGE.

INITIALIZATION.

  PERFORM inicializacao.

AT SELECTION-SCREEN OUTPUT.
  PERFORM create_combobox.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.
  PERFORM get_variante CHANGING p_varia.



START-OF-SELECTION.
  PERFORM progresso USING '5'
                          text-003.

  lock_key = sy-uname.
  CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = lock_key
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'A transacção ja está a ser executado pelo user' sy-uname .
  ENDIF.

  PERFORM get_dados.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = lock_key
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  CLEAR lock_key.

END-OF-SELECTION.
  CHECK gv_exit IS INITIAL.
  PERFORM progresso USING '55'
                          text-005.
  PERFORM display_dados.
