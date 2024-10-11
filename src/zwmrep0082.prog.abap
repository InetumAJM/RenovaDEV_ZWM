************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0082                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Impressão SSCC por Grupo                                 *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 04/06/2008                                               *
*                                                                      *
************************************************************************
REPORT zwmrep0082 MESSAGE-ID zwmmsg001.

************************************************************************
** Tabelas DD
************************************************************************
TABLES: ltak, ltap, zwm026.

************************************************************************
** Variáveis
************************************************************************

DATA: lt_ltak    LIKE ltak         OCCURS 0 WITH HEADER LINE,
      lt_ltap    LIKE ltap         OCCURS 0 WITH HEADER LINE,
      lt_zwm026  LIKE zwm026       OCCURS 0 WITH HEADER LINE,
      t_sscc     LIKE zwm_ean128   OCCURS 0 WITH HEADER LINE,
      lt_zwm069  LIKE zwm069       OCCURS 0 WITH HEADER LINE.

************************************************************************
** Opções de selecção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum  LIKE t311a-lgnum   OBLIGATORY.
PARAMETERS: p_refnr  LIKE t311a-refnr   OBLIGATORY.
PARAMETERS: p_vbeln  LIKE t311a-rbnum.
PARAMETERS: p_print  LIKE nast-ldest    OBLIGATORY.
PARAMETERS: p_copies LIKE itcpo-tdcopies OBLIGATORY DEFAULT 3.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
** Initialization
************************************************************************
INITIALIZATION.

************************************************************************
** Start-of-Selection
************************************************************************
START-OF-SELECTION.

  IF p_lgnum <> '999'.

    CLEAR:   lt_ltak, lt_ltap, t_sscc, lt_zwm026.
    REFRESH: lt_ltak, lt_ltap, t_sscc, lt_zwm026.

    SELECT * INTO TABLE lt_ltak
        FROM ltak
            WHERE lgnum = p_lgnum
              AND refnr = p_refnr.

    IF NOT p_vbeln IS INITIAL.
      DELETE lt_ltak WHERE vbeln <> p_vbeln.
    ENDIF.

    CHECK NOT lt_ltak[] IS INITIAL.

    SELECT * INTO TABLE lt_ltap
       FROM ltap
           FOR ALL ENTRIES IN lt_ltak
               WHERE lgnum = lt_ltak-lgnum
                 AND tanum = lt_ltak-tanum.

    DELETE lt_ltap WHERE vorga = 'ST' OR
                         vlenr IS INITIAL.

    SELECT * INTO TABLE lt_zwm026
        FROM zwm026
            WHERE armazem = p_lgnum
              AND grupo   = p_refnr.

    IF NOT p_vbeln IS INITIAL.
      DELETE lt_zwm026 WHERE remessa <> p_vbeln.
    ENDIF.

    SORT lt_zwm026 BY sscc.

    DELETE ADJACENT DUPLICATES FROM lt_zwm026 COMPARING sscc.

  ENDIF.

END-OF-SELECTION.

  IF p_lgnum <> '999'.

    CLEAR t_sscc.
    REFRESH t_sscc.

    LOOP AT lt_ltap.
      MOVE lt_ltap-vlenr TO t_sscc-sscc.
      APPEND t_sscc.
    ENDLOOP.

    LOOP AT lt_zwm026.
      MOVE lt_zwm026-sscc TO t_sscc-sscc.
      APPEND t_sscc.
    ENDLOOP.
  ELSE.

    CLEAR lt_zwm069.
    REFRESH lt_zwm069.
    SELECT * INTO TABLE lt_zwm069
        FROM zwm069
            WHERE refnr = p_refnr.

    IF p_vbeln IS NOT INITIAL.
      DELETE lt_zwm069 WHERE vbeln <> p_vbeln.
    ENDIF.

    LOOP AT lt_zwm069.
      MOVE lt_zwm069-sscc TO t_sscc-sscc.
      APPEND t_sscc.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'ZWM_IMPRIME_EAN128'
    EXPORTING
      lgnum                    = p_lgnum
      printer                  = p_print
      copies                   = p_copies
    TABLES
      sscc                     = t_sscc
    EXCEPTIONS
      impressora_nao_existe    = 1
      sscc_nao_existe          = 2
      sscc_com_impressao_grupo = 3
      OTHERS                   = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
