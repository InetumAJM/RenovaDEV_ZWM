*&---------------------------------------------------------------------*
*&  Include           ZXLTOU19
*&---------------------------------------------------------------------*

DATA aut_qmat LIKE lqua_vb OCCURS 0 WITH HEADER LINE.

DATA: umrez       LIKE marm-umrez,
      umren       LIKE marm-umren,
      paletizacao TYPE i.

CALL FUNCTION 'L_QMAT_SFIFO_CREATE'
  TABLES
    t_qmat_sfifo_cus = t_qmat
    t_bdbatch_cus    = t_bdbatch.

CHECK NOT t_qmat[] IS INITIAL.

CHECK i_ltak-lgnum = '150'.

CHECK i_ltak-refnr IS NOT INITIAL OR ( i_ltak-betyp = 'Z' AND i_ltak-benum IS NOT INITIAL ).

DELETE t_qmat WHERE lgtyp <> 'AUT' AND lgtyp <> 'DRI' AND lgtyp <> 'BLK'.

DELETE t_qmat WHERE lgtyp EQ i_ltap-nltyp.   " Elimina o proprio destino

DELETE t_qmat WHERE lgpla IS INITIAL.

DELETE t_qmat WHERE skzue NE ' '
                 OR skzua NE ' '
                 OR skzsi NE ' '.

CHECK NOT t_qmat[] IS INITIAL.

IF  i_ltak-betyp = 'Z' AND i_ltak-benum IS NOT INITIAL.

  IF i_mlvs-ltkza = 'D'.
    SORT t_qmat BY lgtyp DESCENDING verme ASCENDING lgpla ASCENDING.
  ELSE.
    SORT t_qmat BY lgtyp ASCENDING verme ASCENDING lgpla ASCENDING.
  ENDIF.

ELSE.
** Obtem a paletização
  CLEAR: umrez,umren.
  SELECT SINGLE umrez umren FROM marm
           INTO (umrez,umren)
          WHERE matnr EQ i_ltap-matnr
            AND meinh EQ 'PAL'.

  CHECK sy-subrc EQ 0 AND NOT umren IS INITIAL.

  paletizacao = umrez / umren.

  DELETE t_qmat WHERE verme < paletizacao.

  IF i_mlvs-ltkza = 'D'.
    SORT t_qmat BY lgtyp DESCENDING lgpla ASCENDING.
  ELSE.
    SORT t_qmat BY lgtyp ASCENDING lgpla ASCENDING.
  ENDIF.

  READ TABLE t_qmat INDEX 1.
  IF t_qmat-lgtyp = 'AUT'.

    i_ltap-vltyp = 'AUT'.

    CLEAR aut_qmat.
    REFRESH aut_qmat.
    LOOP AT  t_qmat WHERE lgtyp = 'AUT'.
      MOVE-CORRESPONDING t_qmat TO aut_qmat.
      APPEND aut_qmat.
    ENDLOOP.

    CALL FUNCTION 'Z_WMFR_EXIT_SAIDA_ARM_AUTO'
      EXPORTING
        is_ltak  = i_ltak
        is_ltap  = i_ltap
        is_mlvs  = i_mlvs
        is_mgef  = i_mgef
        is_t331  = i_t331
        is_t333  = i_t333
        is_t340d = i_t340d
        i_anfml  = i_anfml
        i_anfme  = i_anfme
        i_vorga  = i_vorga
      CHANGING
        ct_tqmat = aut_qmat[]
      EXCEPTIONS
        error    = 1
        not_exit = 2
        OTHERS   = 3.

    IF sy-subrc <> 2.
      i_ltap-vltyp = '+++'.

      IF aut_qmat[] IS INITIAL.
        DELETE t_qmat WHERE lgtyp = 'AUT'.
      ELSE.
        CLEAR t_qmat.
        REFRESH t_qmat.
        t_qmat[] = aut_qmat[].
      ENDIF.

      EXIT.
    ENDIF.

    i_ltap-vltyp = '+++'.

  ENDIF.
ENDIF.
