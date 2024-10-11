FUNCTION zwm_get_refnr_from_tknum.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGNUM) LIKE  T311-LGNUM
*"     REFERENCE(TKNUM) LIKE  VTTK-TKNUM
*"     REFERENCE(SIGNI) LIKE  VTTK-SIGNI OPTIONAL
*"     REFERENCE(CHECK_TKNUM) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(REFNR) LIKE  T311-REFNR
*"     REFERENCE(TKNUM_OUT) LIKE  VTTK-TKNUM
*"  TABLES
*"      NEW_VTTP TYPE  VTTPVB_TAB
*"  EXCEPTIONS
*"      NO_ITEMS
*"      NO_GROUP
*"      NO_WAREHOUSE
*"      NO_PROCESS
*"      DUPLICATE_GROUP
*"----------------------------------------------------------------------

  TABLES: vttp, t311a, vttk.

  DATA: l_tknum   LIKE vttp-tknum,
        itab_vttk LIKE vttk OCCURS 0 WITH HEADER LINE.

  DATA: n_grupo TYPE i,
        aux_dpreg LIKE vttk-dpreg.

  DATA: BEGIN OF st OCCURS 0,
          lgnum LIKE t311a-lgnum,
          refnr LIKE t311a-refnr,
  END   OF st.

  DATA lt_t311a LIKE st OCCURS 0 WITH HEADER LINE.

  FREE: itab_vttk.
  CLEAR: refnr, tknum_out, itab_vttk, aux_dpreg.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = tknum
    IMPORTING
      output = l_tknum.

** Validar se existe algum grupo com a mesma matricula ainda não
** finalizado
  IF check_tknum = 'X'.

    CHECK NOT signi IS INITIAL.

    aux_dpreg = sy-datum - 60.
    CLEAR: vttk.
    SELECT SINGLE * FROM vttk
    WHERE tknum NE l_tknum
      AND signi EQ signi
      AND stten NE 'X'
      AND dpreg > aux_dpreg.

    IF sy-subrc EQ 0.
      tknum_out = vttk-tknum.
      RAISE no_process.
    ENDIF.
  ELSE.

    IF new_vttp[] IS INITIAL.
      RAISE no_items.
    ELSE.
      CLEAR: n_grupo, lt_t311a.
      REFRESH lt_t311a.

      LOOP AT new_vttp.

        CLEAR: t311a.
        SELECT * FROM t311a
        WHERE lgnum EQ lgnum
        AND rbnum EQ new_vttp-vbeln
        AND rbtyp EQ 'L'. "Fornecimentos
          MOVE-CORRESPONDING t311a TO lt_t311a.
          COLLECT lt_t311a.
          CLEAR lt_t311a.
        ENDSELECT.

      ENDLOOP.



      DESCRIBE TABLE lt_t311a LINES n_grupo.
      IF n_grupo > 1.
        RAISE duplicate_group.
      ENDIF.

      READ TABLE lt_t311a INDEX 1.
      IF sy-subrc NE 0.
        RAISE no_warehouse.
      ENDIF.
      refnr = lt_t311a-refnr.
      IF refnr IS INITIAL.
        RAISE no_group.
      ENDIF.

*      READ TABLE new_vttp INDEX 1.
*      CLEAR: t311a.
*      SELECT * FROM t311a
*      UP TO 1 ROWS
*      WHERE lgnum EQ lgnum
*        AND rbnum EQ new_vttp-vbeln
*        AND rbtyp EQ 'L'. "Fornecimentos
*        refnr = t311a-refnr.
*      ENDSELECT.
*      IF sy-subrc NE 0.
*        RAISE no_warehouse.
*      ELSEIF refnr IS INITIAL.
*        RAISE no_group.
*      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
