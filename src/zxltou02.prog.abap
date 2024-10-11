*&---------------------------------------------------------------------*
*&  Include           ZXLTOU02                                         *
*&---------------------------------------------------------------------*


DATA: ls_ltap TYPE ltap_vb.
DATA: lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.
DATA: lt_ltak   LIKE ltak   OCCURS 0 WITH HEADER LINE.
DATA: lt_ltap   LIKE ltap   OCCURS 0 WITH HEADER LINE.

DATA: lv_refnr TYPE lvs_refnr.
**********************************************************************

*BREAK-POINT.

** NS . WM - filtrar á cabeça pelo aramzem wm de MP


** Armazém França
**********************************************************************
IF i_ltak_vb-lgnum = '150'.

** Remontadas em Armazem Automatico
  CALL FUNCTION 'Z_WMFR_EXIT_CONFIRM_ARMAUT_REM'
    EXPORTING
      is_ltak_vb = i_ltak_vb
      it_ltap_vb = t_ltap_vb[].

  IF i_ltak_vb-bwlvs = '972'.

    READ TABLE t_ltap_vb
          INTO ls_ltap
          INDEX 1.

    CHECK ls_ltap IS NOT INITIAL.

** Garante que so passa 1 vez na OT
    CHECK ls_ltap-pquit IS NOT INITIAL.

    CALL FUNCTION 'L_TO_CREATE_SINGLE' IN BACKGROUND TASK AS SEPARATE UNIT
      EXPORTING
        i_lgnum = ls_ltap-lgnum
        i_bwlvs = '999'
        i_matnr = ls_ltap-matnr
        i_werks = ls_ltap-werks
        i_lgort = ls_ltap-lgort
        i_charg = ls_ltap-charg
        i_anfme = ls_ltap-nsola
        i_altme = ls_ltap-altme
        i_squit = 'X'
        i_vltyp = ls_ltap-nltyp
        i_vlpla = ls_ltap-nlpla
        i_nltyp = 'INT'
        i_nlpla = '000-000-01'.

  ENDIF.

  CHECK i_ltak_vb-refnr IS NOT INITIAL.

  READ TABLE t_ltap_vb
        INTO ls_ltap
        INDEX 1.

  CHECK ls_ltap-vltyp = 'AUT'.

  CLEAR lt_zwm028.
  REFRESH lt_zwm028.
  SELECT * INTO TABLE lt_zwm028
      FROM zwm028
          WHERE lgnum = i_ltak_vb-lgnum
            AND refnr = i_ltak_vb-refnr
            AND tipo_lock = 'R'.

  CHECK lt_zwm028[] IS NOT INITIAL.

  CLEAR lt_ltak.
  REFRESH lt_ltak.
  SELECT * INTO TABLE lt_ltak
      FROM ltak
          WHERE lgnum = i_ltak_vb-lgnum
            AND refnr = i_ltak_vb-refnr.

  DELETE lt_ltak WHERE kquit = 'X'.

  DELETE lt_ltak WHERE lgnum = i_ltak_vb-lgnum
                   AND tanum = i_ltak_vb-tanum.

  CHECK lt_ltak[] IS NOT INITIAL.

  SELECT * INTO TABLE lt_ltap
      FROM ltap
          FOR ALL ENTRIES IN lt_ltak
              WHERE lgnum = lt_ltak-lgnum
                AND tanum = lt_ltak-tanum.

  LOOP AT lt_ltap TRANSPORTING NO FIELDS WHERE vltyp EQ 'DRI' OR vltyp EQ 'BLK'.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    DELETE lt_ltap WHERE kzsub IS INITIAL AND vltyp EQ 'AUT'.
    DELETE lt_ltap WHERE pvqui IS NOT INITIAL AND vltyp EQ 'AUT'.
  ELSE.
    DELETE lt_ltap WHERE kzsub IS INITIAL.
    DELETE lt_ltap WHERE pvqui IS NOT INITIAL.
  ENDIF.

  CHECK lt_ltap[] IS INITIAL.

  DELETE lt_zwm028 WHERE zlock <> '1'.

  CHECK lt_zwm028[] IS NOT INITIAL.

  READ TABLE lt_zwm028 INDEX 1.

  CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK' IN BACKGROUND TASK AS SEPARATE UNIT
    EXPORTING
      i_lgnum = i_ltak_vb-lgnum
      i_refnr = lt_zwm028-refnr
      i_vbeln = lt_zwm028-remessa
*     I_COMMIT       = 'X'
*     I_STEP  =
    .

** Armazém Torres Novas
**********************************************************************
ELSEIF i_ltak_vb-lgnum = '100'.

  READ TABLE t_ltap_vb INDEX 1.

**  Split de PRM
  CALL FUNCTION 'ZWM_EXIT_AUT_SPLIT_PRM' IN BACKGROUND TASK
    EXPORTING
      is_ltak_vb = i_ltak_vb
      it_ltap_vb = t_ltap_vb[].

** Estorno OT de Armazém Automático (WCS)
  IF t_ltap_vb-vorga = 'ST' AND t_ltap_vb-kzsub = 'X'.
    CALL FUNCTION 'ZWM_CREATE_IDOC_TO_CANCEL' IN BACKGROUND TASK
      EXPORTING
        i_lgnum   = i_ltak_vb-lgnum
        i_tanum   = i_ltak_vb-tanum
      TABLES
        t_ltap_vb = t_ltap_vb
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
  ENDIF.


  LOOP AT t_ltap_vb WHERE qdatu IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    IF t_ltap_vb-vorga <> 'ST'.

      "  Free de Pulmão
      IF i_ltak_vb-refnr IS NOT INITIAL.
        CALL FUNCTION 'ZWM_EXIT_FREE_PULMAO' IN BACKGROUND TASK
          EXPORTING
            is_ltak_vb = i_ltak_vb
            it_ltap_vb = t_ltap_vb[].
      ENDIF.

      " Confirmações no Armazém Automático (WCS)
      CALL FUNCTION 'ZWM_CHECK_TO_CONFIRM_WCS' IN BACKGROUND TASK
        EXPORTING
          i_ltak_vb = i_ltak_vb
        TABLES
          t_ltap_vb = t_ltap_vb.
    ENDIF.
  ENDIF.

** Armazém Beta Matéria Primas
**********************************************************************
ELSEIF i_ltak_vb-lgnum = '400'.
*  DATA: i TYPE i.
*  DO.
*    IF i = 1.
*      EXIT.
*    ENDIF.
*  ENDDO.

  CALL FUNCTION 'ZWMMP_INSREMOVE_QTD_INTERFACE' IN BACKGROUND TASK AS SEPARATE UNIT
    EXPORTING
      i_ltak = i_ltak_vb
    TABLES
      i_ltap = t_ltap_vb.


** Armazém França Matérias Primas
**********************************************************************
ELSEIF i_ltak_vb-lgnum EQ '450'.

*& Begin of Modification by Tiago Pateiro - ROFF @ 04.12.2015 10:37:20
*/ Ajustar criação de TO's/consumos/mascaras para França
  CALL FUNCTION 'Z_WMFR_TO_ZXLTOU02_INSREM_QTY'
    IN BACKGROUND TASK AS SEPARATE UNIT
    EXPORTING
      is_ltak = i_ltak_vb
      it_ltap = t_ltap_vb[].

*  CALL FUNCTION 'ZWMMP_INSREMOVE_QTD_INTERFACE'
*    IN BACKGROUND TASK AS SEPARATE UNIT
*    EXPORTING
*      i_ltak = i_ltak_vb
*    TABLES
*      i_ltap = t_ltap_vb[].
*& End of Modification by Tiago Pateiro - ROFF @ 04.12.2015 10:37:37
ENDIF.

** Só para TO's de reabastecimento do PKL
IF i_ltak_vb-bwlvs = '979'.

  READ TABLE t_ltap_vb
        INTO ls_ltap
        INDEX 1.

  CHECK ls_ltap IS NOT INITIAL.

** Garante que so passa 1 vez na OT
  CHECK ls_ltap-pquit IS NOT INITIAL.

  CALL FUNCTION 'L_TO_CREATE_SINGLE' IN BACKGROUND TASK AS SEPARATE UNIT
    EXPORTING
      i_lgnum = ls_ltap-lgnum
      i_bwlvs = '999'
*     I_BETYP = ' '
*     I_BENUM = ' '
      i_matnr = ls_ltap-matnr
      i_werks = ls_ltap-werks
      i_lgort = ls_ltap-lgort
      i_charg = ls_ltap-charg
*     I_BESTQ = ' '
*     I_SOBKZ = ' '
*     I_SONUM = ' '
*     I_LETYP = ' '
      i_anfme = ls_ltap-nsola
      i_altme = ls_ltap-altme
*     I_WDATU = INIT_DATUM
*     I_VFDAT = INIT_DATUM
*     I_ZEUGN = ' '
*     I_LZNUM = ' '
      i_squit = 'X'
*     I_NIDRU = ' '
*     I_DRUKZ = ' '
*     I_LDEST = ' '
*     I_WEMPF = ' '
*     I_ABLAD = ' '
      i_vltyp = ls_ltap-nltyp
*     I_VLBER = ' '
      i_vlpla = ls_ltap-nlpla
*     I_VPPOS = ' '
*     I_VLENR = ' '
*     I_VLQNR = ' '
      i_nltyp = 'INT'
*     I_NLBER = ' '
      i_nlpla = '000-000-01'
*     I_NPPOS = ' '
*     I_NLENR = ' '
*     I_NLQNR = ' '
*     I_RLTYP = ' '
*     I_RLBER = ' '
*     I_RLPLA = ' '
*     I_RLQNR = ' '
*     I_UPDATE_TASK               = ' '
*     I_COMMIT_WORK               = 'X'
*     I_BNAME = SY-UNAME
*     I_KOMPL = 'X'
*     I_SOLEX = 0
*     I_PERNR = 0
*     I_AUSFB = ' '
*     I_REFNR = ' '
*     I_L2SKA = ' '
*     I_INVENT                    = ' '
*  IMPORTING
*     E_TANUM =
*     E_LTAP  =
*  TABLES
*     T_LTAK  =
*     T_LTAP_VB                   =
*  EXCEPTIONS
*     NO_TO_CREATED               = 1
*     BWLVS_WRONG                 = 2
*     BETYP_WRONG                 = 3
*     BENUM_MISSING               = 4
*     BETYP_MISSING               = 5
*     FOREIGN_LOCK                = 6
*     VLTYP_WRONG                 = 7
*     VLPLA_WRONG                 = 8
*     VLTYP_MISSING               = 9
*     NLTYP_WRONG                 = 10
*     NLPLA_WRONG                 = 11
*     NLTYP_MISSING               = 12
*     RLTYP_WRONG                 = 13
*     RLPLA_WRONG                 = 14
*     RLTYP_MISSING               = 15
*     SQUIT_FORBIDDEN             = 16
*     MANUAL_TO_FORBIDDEN         = 17
*     LETYP_WRONG                 = 18
*     VLPLA_MISSING               = 19
*     NLPLA_MISSING               = 20
*     SOBKZ_WRONG                 = 21
*     SOBKZ_MISSING               = 22
*     SONUM_MISSING               = 23
*     BESTQ_WRONG                 = 24
*     LGBER_WRONG                 = 25
*     XFELD_WRONG                 = 26
*     DATE_WRONG                  = 27
*     DRUKZ_WRONG                 = 28
*     LDEST_WRONG                 = 29
*     UPDATE_WITHOUT_COMMIT       = 30
*     NO_AUTHORITY                = 31
*     MATERIAL_NOT_FOUND          = 32
*     LENUM_WRONG                 = 33
*     OTHERS  = 34
    .
* IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
* ENDIF.

ENDIF.
