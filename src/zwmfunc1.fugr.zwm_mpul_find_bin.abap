FUNCTION zwm_mpul_find_bin .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LGTYP) TYPE  LGTYP
*"     REFERENCE(I_NUM_PALETES) TYPE  NUMC2
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_POSICAO) TYPE  LGPLA
*"     REFERENCE(E_KOBER) TYPE  KOBER
*"----------------------------------------------------------------------
  DATA: lt_zwm001   TYPE TABLE OF zwm001,
        lt_zwm007   TYPE TABLE OF zwm007,
        lt_lagp     TYPE TABLE OF lagp,
        lt_kober    TYPE TABLE OF kober,
        lt_t311a    TYPE TABLE OF t311a,
        lt_likp     TYPE TABLE OF likp,
        lt_vbpa     TYPE TABLE OF vbpa,
        lt_zwm049   TYPE TABLE OF zwm049,
        lt_lagp_get TYPE TABLE OF lagp.

  DATA: ls_zwm007 TYPE zwm007,
        ls_lagp   TYPE lagp.

  DATA: lv_lgtyp_pul_aux TYPE char20,
        lv_lgtyp_pul     TYPE lgtyp,
        lv_pulmao        TYPE lgpla,
        lv_kober         TYPE kober,
        lv_try           TYPE i,
        lv_sort_asc      TYPE flag.

  DATA: lv_mpul_size_portao  TYPE i,
        lv_mpul_size_armazem TYPE i.

  CLEAR: e_posicao, e_kober.


  PERFORM get_parameter USING i_lgnum
                             'TROCA_ATRIBUICAO_PUL'
                             'ACTIVAR'
                              lv_sort_asc.

** Tipo de depósito PUL
  PERFORM get_parameter USING i_lgnum
                             'MPUL'
                             'MPUL_SIZE_PORTAO'
                              lv_mpul_size_portao.


  PERFORM get_parameter USING i_lgnum
                             'MPUL'
                             'MPUL_SIZE_ARMAZEM'
                              lv_mpul_size_armazem.



  CHECK i_num_paletes <= lv_mpul_size_portao.

** Parametrizações
***********************************************************************
  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs       = i_lgnum
    TABLES
      ti_zwm001 = lt_zwm001.

** Tipo de depósito PUL
  PERFORM get_parameter USING i_lgnum
                             'ENTRADA_ARMAZEM'
                             'ST_PUL'
                              lv_lgtyp_pul_aux.

  MOVE lv_lgtyp_pul_aux TO lv_lgtyp_pul.

  CHECK lv_lgtyp_pul EQ i_lgtyp.

** Valida se o Grupo de Paletezação Expecial
***********************************************************************
  DO 1 TIMES.
    CHECK NOT i_refnr IS INITIAL.

    SELECT * FROM t311a
       INTO TABLE lt_t311a
       WHERE lgnum = i_lgnum AND
             refnr = i_refnr.

    CHECK sy-subrc EQ 0.

    SELECT * FROM likp
       INTO TABLE lt_likp
       FOR ALL ENTRIES IN lt_t311a
       WHERE vbeln = lt_t311a-rbnum.

    CHECK sy-subrc EQ 0.

    SELECT * FROM vbpa
       INTO TABLE lt_vbpa
       FOR ALL ENTRIES IN lt_likp
       WHERE vbeln = lt_likp-vbeln AND
             posnr = '000000' AND
             parvw IN ('W1','WE').

    CHECK sy-subrc EQ 0.


    SELECT * FROM zwm049
       INTO TABLE lt_zwm049
       FOR ALL ENTRIES IN lt_vbpa
       WHERE kunnr = lt_vbpa-kunnr.

    CHECK sy-subrc EQ 0.

**  Grupo & contem paletização especial, não irá ser determinado Meio Pulmão
    MESSAGE i011(zwmsg001) DISPLAY LIKE 'W' WITH i_refnr.
    RETURN.
  ENDDO.

** Retorna Pulmões
***********************************************************************
  SELECT * FROM zwm007
     INTO TABLE lt_zwm007
          WHERE armazem = i_lgnum AND
                tipo <> 'D'.

  CHECK sy-subrc EQ 0.

  IF lv_sort_asc EQ abap_false.
    SORT lt_zwm007 BY porta DESCENDING.
  ELSE.
    SORT lt_zwm007 BY porta ASCENDING.
  ENDIF.

** Retorna Posições Livres (Sem estarem totalmente Ocupadas)
***********************************************************************
  SELECT * FROM lagp
     INTO TABLE lt_lagp
     WHERE lgnum = i_lgnum AND
           lgtyp = i_lgtyp AND
           kober <> gc_mpul_lado_amb.

  SORT lt_lagp BY lgpla.

** Parametros de Lado de Pulmão a serem usados
***********************************************************************
  IF i_num_paletes <= lv_mpul_size_armazem.
    APPEND gc_mpul_lado_armazem TO lt_kober.
    APPEND gc_mpul_lado_portao TO lt_kober.
  ELSEIF i_num_paletes > lv_mpul_size_armazem AND i_num_paletes <= lv_mpul_size_portao.
    APPEND gc_mpul_lado_portao TO lt_kober.
  ENDIF.

** Ultima solução pulmões vazios
***********************************************************************
  APPEND '' TO lt_kober.

** Leitura de Todos os Pulmões
***********************************************************************
  DO.
    READ TABLE lt_kober
          INTO lv_kober
          INDEX sy-index.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    LOOP AT lt_zwm007 INTO ls_zwm007.
      DO 12 TIMES VARYING lv_pulmao
                     FROM ls_zwm007-pulmao1
                     NEXT ls_zwm007-pulmao2.

        CHECK NOT lv_pulmao IS INITIAL.

        CLEAR ls_lagp.
        READ TABLE lt_lagp
              INTO ls_lagp
              WITH KEY lgpla = lv_pulmao
              BINARY SEARCH.

        CHECK sy-subrc EQ 0.

        IF ls_lagp-kober IS INITIAL AND
           ls_lagp-brand EQ 'X'.
*         Ocupada - Pulmão completo
          CONTINUE.
        ENDIF.

        IF ls_lagp-kober IS INITIAL AND
           NOT lv_kober IS INITIAL.
*         Prioricade a Meios Pulmões
          CONTINUE.
        ENDIF.

        IF lv_kober IS INITIAL AND
           ls_lagp-brand EQ 'X'.
          CONTINUE.
        ENDIF.

        IF ls_lagp-kober <> lv_kober OR
           ls_lagp-kober IS INITIAL.

          IF ls_lagp-kober IS INITIAL.
*           Pulmão era completo passa a meio

            IF i_num_paletes <= lv_mpul_size_armazem.
              lv_kober = gc_mpul_lado_armazem.
            ELSEIF i_num_paletes > lv_mpul_size_armazem AND i_num_paletes <= lv_mpul_size_portao.
              lv_kober = gc_mpul_lado_portao.
            ENDIF.

          ENDIF.

          APPEND ls_lagp TO lt_lagp_get.

**          e_kober   = lv_kober.
**          e_posicao = ls_lagp-lgpla.
**          EXIT.
        ENDIF.
      ENDDO.

**      IF NOT e_posicao IS INITIAL.
**        EXIT.
**      ENDIF.
    ENDLOOP.

**    IF NOT e_posicao IS INITIAL.
**      EXIT.
**    ENDIF.
  ENDDO.

  PERFORM filter_select_pul USING i_lgnum i_lgtyp CHANGING lt_lagp_get.
  CHECK NOT lt_lagp_get IS INITIAL.

  READ TABLE lt_lagp_get
        INTO ls_lagp
        INDEX 1.


  e_posicao = ls_lagp-lgpla.

ENDFUNCTION.
