************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0009                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Impressão de form auxiliar - Terceiros                   *
* Criado por: Bruno Simões                                             *
* Criado em.: 02/12/2003                                               *
* Tipo PRG..: Report                                                   *
************************************************************************

REPORT  ZWMREP0009.

TABLES : ZWM017,
         MARM,
         MAKT,
         MARA.


DATA : BEGIN OF L_ZWM017 OCCURS 0.
        INCLUDE STRUCTURE ZWM017.
DATA: MAKTX LIKE MAKT-MAKTX,
      EAN11 LIKE MARM-EAN11,
      PED_ITEM(15).
DATA : END OF L_ZWM017.


DATA : LVS_ITCPO LIKE ITCPO.

PARAMETERS: P_ARMAZ LIKE ZWM017-ARMAZEM OBLIGATORY,
            P_TALAO LIKE ZWM017-NUM_ENTRADA OBLIGATORY.

START-OF-SELECTION.

  IF NOT P_TALAO IS INITIAL AND NOT P_ARMAZ IS INITIAL.
    PERFORM CARREGA_INFO.
    PERFORM IMPRIME_TALAO.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CARREGA_INFO.

** Dados entrados pelo responsável de armazém
  SELECT * FROM ZWM017 INTO TABLE L_ZWM017
           WHERE ARMAZEM = P_ARMAZ AND
                 NUM_ENTRADA = P_TALAO and
                 material <> 'PCHEP' and
                 material <> 'PROUGE'.
  IF SY-SUBRC <> 0.
    EXIT.
  ELSE.
    SORT L_ZWM017 BY ARMAZEM EBELN EBELP.
  ENDIF.


  LOOP AT L_ZWM017.
** Descrição do material
    SELECT SINGLE MAKTX FROM MAKT INTO L_ZWM017-MAKTX
                        WHERE MATNR = L_ZWM017-MATERIAL AND
                              SPRAS = SY-LANGU.
** EAN11
    SELECT SINGLE MEINS FROM MARA INTO MARA-MEINS
                        WHERE MATNR = L_ZWM017-MATERIAL.
    IF SY-SUBRC = 0.
      SELECT SINGLE EAN11 FROM MARM INTO L_ZWM017-EAN11
                          WHERE MATNR = L_ZWM017-MATERIAL AND
                                MEINH = MARA-MEINS.
    ENDIF.

** Concatenação do pedido de compra e do respectivo item
    CONCATENATE L_ZWM017-EBELN L_ZWM017-EBELP INTO L_ZWM017-PED_ITEM.
    MODIFY L_ZWM017.
  ENDLOOP.

ENDFORM.                    " CARREGA_INFO


*&---------------------------------------------------------------------*
*&      Form  IMPRIME_TALAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIME_TALAO.

*** Actualização dos parâmetros da impressão
*      lvs_itcpo-tdnoprev = 'X'.
*      lvs_itcpo-tdcopies = 3.
  LVS_ITCPO-TDIMMED = 'X'.
  LVS_ITCPO-TDDELETE = 'X'.


** Criação do formulário
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      DEVICE                      = 'PRINTER'
      DIALOG                      = 'X'
      FORM                        = 'ZWMFORM002'
      LANGUAGE                    = SY-LANGU
      OPTIONS                     = LVS_ITCPO
    EXCEPTIONS
      CANCELED                    = 1
      DEVICE                      = 2
      FORM                        = 3
      OPTIONS                     = 4
      UNCLOSED                    = 5
      MAIL_OPTIONS                = 6
      ARCHIVE_ERROR               = 7
      INVALID_FAX_NUMBER          = 8
      MORE_PARAMS_NEEDED_IN_BATCH = 9
      SPOOL_ERROR                 = 10
      OTHERS                      = 11.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  CALL FUNCTION 'START_FORM'
    EXPORTING
      FORM        = 'ZWMFORM002'
      LANGUAGE    = SY-LANGU
      PROGRAM     = 'ZWMREP0009'
    EXCEPTIONS
      FORM        = 1
      FORMAT      = 2
      UNENDED     = 3
      UNOPENED    = 4
      UNUSED      = 5
      SPOOL_ERROR = 6
      OTHERS      = 7.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  LOOP AT L_ZWM017.

** Impressão do cabeçalho no item
    AT NEW EBELN.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          ELEMENT                  = 'ITEMS_HEADER'
          WINDOW                   = 'MAIN'
        EXCEPTIONS
          ELEMENT                  = 1
          FUNCTION                 = 2
          TYPE                     = 3
          UNOPENED                 = 4
          UNSTARTED                = 5
          WINDOW                   = 6
          BAD_PAGEFORMAT_FOR_PRINT = 7
          SPOOL_ERROR              = 8
          OTHERS                   = 9.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDAT.

** Impressão dos items

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        ELEMENT                  = 'ITEMS'
        WINDOW                   = 'MAIN'
      EXCEPTIONS
        ELEMENT                  = 1
        FUNCTION                 = 2
        TYPE                     = 3
        UNOPENED                 = 4
        UNSTARTED                = 5
        WINDOW                   = 6
        BAD_PAGEFORMAT_FOR_PRINT = 7
        SPOOL_ERROR              = 8
        OTHERS                   = 9.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'END_FORM'
    EXCEPTIONS
      UNOPENED                 = 1
      BAD_PAGEFORMAT_FOR_PRINT = 2
      SPOOL_ERROR              = 3
      OTHERS                   = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

** fecho do formulario
  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      UNOPENED                 = 1
      BAD_PAGEFORMAT_FOR_PRINT = 2
      SEND_ERROR               = 3
      SPOOL_ERROR              = 4
      OTHERS                   = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " IMPRIME_TALAO
