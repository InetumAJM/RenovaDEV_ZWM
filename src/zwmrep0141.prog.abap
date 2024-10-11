************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0141                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: RF - Transferência de Stock de Material                  *
* Criado por: Ricardo Sousa                                            *
* Criado em.: 26/11/2021                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0141 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0141_top.
INCLUDE zwmrep0141_o01.
INCLUDE zwmrep0141_i01.
INCLUDE zwmrep0141_f01.

START-OF-SELECTION.
  PERFORM find_whs.
  PERFORM get_customizing.

END-OF-SELECTION.

*  DATA: lt_lqua   TYPE TABLE OF lqua WITH HEADER LINE.
*  DATA: lt_lqua_h TYPE TABLE OF lqua WITH HEADER LINE.
*  DATA: lv_lines TYPE i.
*
*  SELECT *
*    FROM lqua INTO TABLE lt_lqua
*    WHERE lgnum = '100'
*    AND   lgtyp = 'PKL'.
*
*  SORT lt_lqua BY matnr lgpla.
*  DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING matnr lgpla.
*
*  lt_lqua_h[] = lt_lqua[].
*
*  SORT lt_lqua_h BY matnr.
*  DELETE ADJACENT DUPLICATES FROM lt_lqua_h COMPARING matnr.
*
*  LOOP AT lt_lqua_h.
*
*    CLEAR lv_lines.
*    LOOP AT lt_lqua WHERE matnr = lt_lqua_h-matnr.
*      lv_lines = lv_lines + 1.
*    ENDLOOP.
*
*    IF lv_lines > 1.
*      WRITE: / lt_lqua_h-matnr.
*    ENDIF.
*
*  ENDLOOP.
*
*  EXIT.
  PERFORM set_screen.
