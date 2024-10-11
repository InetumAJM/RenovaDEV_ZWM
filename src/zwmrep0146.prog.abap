************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0146                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Reabastecimento Buffer Paletização Especial              *
* Criado por: Ricardo Sousa (INETUM)                                   *
* Criado em.: 26/10/2023                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0146 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0146_top.
INCLUDE zwmrep0146_c01.
INCLUDE zwmrep0146_o01.
INCLUDE zwmrep0146_i01.
INCLUDE zwmrep0146_f01.

DATA: ls_makt  TYPE makt.
DATA: lv_menge TYPE numc3.
DATA: lt_zwm031 TYPE TABLE OF zwm031 WITH HEADER LINE.
DATA: lt_marm   TYPE TABLE OF marm   WITH HEADER LINE.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN.
  PERFORM user_command_1000.

START-OF-SELECTION.
  PERFORM enqueue.

*  SELECT *
*    FROM zwm031 INTO TABLE lt_zwm031
*    WHERE lgnum = p_lgnum.
*
*  DELETE lt_zwm031 WHERE remontada IS NOT INITIAL.
*
*  IF lt_zwm031[] IS NOT INITIAL.
*    SELECT *
*      FROM marm INTO TABLE lt_marm
*      FOR ALL ENTRIES IN lt_zwm031
*      WHERE matnr = lt_zwm031-matnr
*      AND   meinh = 'PAL'.
*  ENDIF.
*
*  SORT lt_zwm031 BY matnr.
*
*  TYPES: BEGIN OF st_list,
*           matnr    TYPE matnr,
*           menge    TYPE numc3,
*           umrez    TYPE umrez,
*           unporpal TYPE zunporpal,
*           niveis   TYPE zniveis,
*           sup      TYPE xfeld,
*         END OF st_list.
*
*  DATA: lt_list TYPE TABLE OF st_list WITH HEADER LINE.
*
*  LOOP AT lt_zwm031.
*
*    READ TABLE lt_marm WITH KEY matnr = lt_zwm031-matnr.
*
*    IF lt_zwm031-lastro IS NOT INITIAL.
*      lv_menge = lt_marm-umrez / lt_zwm031-lastro.
*    ENDIF.
*
*    IF lv_menge <> lt_zwm031-niveis.
*      CLEAR lt_list.
*      lt_list-matnr    = lt_zwm031-matnr.
*      lt_list-umrez    = lt_marm-umrez.
*      lt_list-menge    = lv_menge.
*      lt_list-unporpal = lt_zwm031-unporpal.
*      lt_list-niveis   = lt_zwm031-niveis.
*
*      lv_menge = lt_marm-umrez - lt_zwm031-unporpal.
*
*      IF lv_menge > lt_zwm031-unporpal.
*        lt_list-sup = 'X'.
*      ENDIF.
*
*      APPEND lt_list.
*    ENDIF.
*
*
*
*  ENDLOOP.
*
*  SORT lt_list BY menge DESCENDING.
**  WRITE : / 'MATERIAL', 'DESCRIÇÃO', 'QTD. PAL', 'NIVEIS PAL', '|', 'QTD. ESP', 'NIVEIS ESP'.
*
*  LOOP AT lt_list.
*    SELECT SINGLE *
*      FROM makt INTO ls_makt
*      WHERE matnr = lt_list-matnr
*      AND   spras = sy-langu.
*
*    WRITE : / lt_list-matnr, ls_makt-maktx, lt_list-umrez, lt_list-menge, '|',  lt_list-unporpal, lt_list-niveis, lt_list-sup.
*  ENDLOOP.
*
*  EXIT.

  PERFORM get_data.

END-OF-SELECTION.
  PERFORM process_data.
  PERFORM dequeue.
