DATA lv_count TYPE i. " << INS ROFF(SDF):TMGP:08.01.2016 15:05:22

****************************************************************************
****** Especial armazem materias primas   **********************************
****************************************************************************

IF i_ltak-lgnum = '400'.
  CONSTANTS: cons_uni_kg TYPE meins VALUE 'KG'.
  CONSTANTS: cons_max_menge TYPE menge_d VALUE '50'.
  CONSTANTS: cons_max_uni TYPE menge_d VALUE '60'.
  DATA: lt_zwmmpt001 LIKE zwmmpt001 OCCURS 0 WITH HEADER LINE.
  DATA: lt_lagp_mp LIKE lagp OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua LIKE lqua OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwmmpt006 LIKE zwmmpt006 OCCURS 0 WITH HEADER LINE.
  DATA: ws_mlgn  TYPE mlgn.
  DATA: ws_mlgt  TYPE mlgt.
  DATA: ws_t334b TYPE t334b.
  DATA: lv_conv_menge LIKE plfh-mgvgw.
  DATA: lv_peso_estimado LIKE lagp-lgewi.
  DATA: lv_buf_lgtyp TYPE lgtyp.
  DATA: wa_lagp TYPE lagp.
  DATA: wa_mlgn TYPE mlgn.
  RANGES: r_pesados FOR mlgn-lgbkz.
  DATA: lv_norm LIKE ws_t334b-lgbe1.
  DATA: lv_chao LIKE ws_t334b-lgbe1.
  DATA: lv_casqgrd LIKE mlgn-ltkze.
  DATA: lv_geral_blk  LIKE mlgn-ltkze.
  DATA: lv_casqpos LIKE lagp-lgpla.
  DATA: lv_interface_optmiza.
  DATA: lv_tabix TYPE sytabix.
  DATA: ls_tap TYPE ltap_vb.

  FIELD-SYMBOLS: <lt_tap> TYPE tt_ltap_vb.



  IF i_ltap-nltyp EQ 'BLK'. " Destino Interface
**    Este Tipo de deposito ja nao está em uso, foi "engulido" pelo tipo de deposito INT (interface)


**    Isto so deve ser valido enquanto o tipo de deposito bloco
**    so tiver uma posição e essa posicao permite armazenagem mista
    SELECT SINGLE * INTO wa_mlgn FROM mlgn WHERE lgnum = i_ltap-lgnum AND
                                                 matnr = i_ltap-matnr.
    IF sy-subrc = 0 AND
      wa_mlgn-lhmg1 = i_ltap-vsolm.

      SELECT SINGLE * INTO wa_lagp FROM lagp WHERE lgnum = i_ltap-lgnum AND
                                                   lgtyp = i_ltap-nltyp.
      IF sy-subrc = 0.
        MOVE wa_lagp-lgpla TO e_nlpla.
      ENDIF.
    ENDIF.
  ENDIF.

  IF i_ltap-nltyp EQ 'BK2'. " Destino Arm 2
    SELECT SINGLE * INTO wa_lagp FROM lagp WHERE lgnum = i_ltap-lgnum AND
                                                 lgtyp = i_ltap-nltyp.
    IF sy-subrc = 0.
      MOVE wa_lagp-lgpla TO e_nlpla.
    ENDIF.
  ENDIF.


  IF i_ltap-nltyp = 'INT'. " Destino Interface


    SELECT * INTO TABLE lt_zwmmpt001 FROM zwmmpt001 WHERE armazem = i_ltap-lgnum AND
                                                    processo = 'GERAL'.
    r_pesados-sign = 'I'.
    r_pesados-option = 'EQ'.
    LOOP AT lt_zwmmpt001.
      IF  lt_zwmmpt001-parametro = 'AREA_MAT_CHAO'.
        MOVE lt_zwmmpt001-valor TO r_pesados-low.
        APPEND r_pesados.
      ELSEIF lt_zwmmpt001-parametro = 'BUFF_INTERF_LGTYP'.
        MOVE lt_zwmmpt001-valor TO lv_buf_lgtyp.
*      elseif lt_zwmmpt001-parametro = 'AREA_POS_NORMAL'.
*        move lt_zwmmpt001-valor to lv_norm.
      ELSEIF lt_zwmmpt001-parametro = 'AREA_POS_CHAO'.
        MOVE lt_zwmmpt001-valor TO lv_chao.
      ELSEIF  lt_zwmmpt001-parametro = 'COD_CAS_R3'.
        MOVE lt_zwmmpt001-valor TO lv_casqgrd.
      ELSEIF  lt_zwmmpt001-parametro = 'COD_MAT_BLK'.
        MOVE lt_zwmmpt001-valor TO  lv_geral_blk.
      ELSEIF  lt_zwmmpt001-parametro = 'EXIT_ENT_INT_ACTIV'.
        MOVE lt_zwmmpt001-valor TO lv_interface_optmiza.
      ENDIF.
    ENDLOOP.


    SELECT SINGLE * INTO wa_mlgn FROM mlgn WHERE lgnum = i_ltap-lgnum AND
                                                 matnr = i_ltap-matnr.
    IF  wa_mlgn-ltkze = lv_casqgrd OR
        wa_mlgn-ltkze = lv_geral_blk .
      SELECT SINGLE * INTO ws_mlgt FROM mlgt WHERE lgnum = i_ltap-lgnum AND
                                                   lgtyp = i_ltap-nltyp AND
                                                   matnr = i_ltap-matnr.
      IF sy-subrc = 0 AND
        ws_mlgt-lgpla IS NOT INITIAL.
        MOVE ws_mlgt-lgpla TO e_nlpla.
        EXIT.
      ENDIF.
    ENDIF.


    SELECT * INTO TABLE lt_lagp_mp FROM lagp WHERE
       lgnum = i_ltak-lgnum AND
       lgtyp = 'INT' AND
       skzua = '' AND
       skzue = '' AND
       skzsa = '' AND
       skzse = '' AND
       anzqu = 0.
    IF sy-subrc = 0.
*      tabela de reservas de posicao para picking do cacifo para o interface.
      SELECT * INTO TABLE lt_zwmmpt006 FROM zwmmpt006 WHERE lgnum = i_ltak-lgnum.
      DELETE lt_zwmmpt006 WHERE aloc_nlpla IS INITIAL.
      LOOP AT lt_zwmmpt006.
        DELETE lt_lagp_mp WHERE lgpla = lt_zwmmpt006-aloc_nlpla.
      ENDLOOP.
    ENDIF.

    IF i_ltak-bwlvs = '319' AND i_ltap-vltyp = 'RK'.
      DELETE lt_lagp_mp WHERE lgpla = '001-001-00'.
    ENDIF.

    SELECT SINGLE * INTO ws_mlgn FROM mlgn WHERE matnr = i_ltap-matnr AND
                                                 lgnum  = i_ltak-lgnum.
    IF sy-subrc = 0.
      SELECT SINGLE * INTO ws_t334b FROM t334b
      WHERE
        lgnum = i_ltak-lgnum AND
        lgtyp = i_ltap-nltyp AND
        lgbkz = ws_mlgn-lgbkz.
    ENDIF.

***    se o material nao vem do buffer
**    então se for pesado nao pode ir para a estantaria
**
    IF ws_mlgn-lgbkz IN r_pesados AND r_pesados[] IS NOT INITIAL AND
      i_ltap-vltyp <> lv_buf_lgtyp.
      IF ws_t334b-lgbe0 = lv_norm.
        CLEAR: ws_t334b-lgbe0.
      ENDIF.
      IF ws_t334b-lgbe1 = lv_norm.
        CLEAR: ws_t334b-lgbe1.
      ENDIF.
      IF ws_t334b-lgbe2 = lv_norm.
        CLEAR: ws_t334b-lgbe2.
      ENDIF.
      IF ws_t334b-lgbe3 = lv_norm.
        CLEAR: ws_t334b-lgbe3.
      ENDIF.
      IF ws_t334b-lgbe4 = lv_norm.
        CLEAR: ws_t334b-lgbe4.
      ENDIF.
      IF ws_t334b-lgbe5 = lv_norm.
        CLEAR: ws_t334b-lgbe5.
      ENDIF.
    ENDIF.

****
*    Excluir posicoes invalida (devido a vizinhos grandes)
    IF  lv_interface_optmiza IS NOT INITIAL.

      CALL FUNCTION 'ZWMMP_INTERFACE_SORT_POS'
        EXPORTING
          i_lgnum = i_ltap-lgnum
          i_lgtyp = i_ltap-nltyp
          i_letyp = i_ltap-letyp
          i_matnr = i_ltap-matnr
        TABLES
          i_lagp  = lt_lagp_mp
          o_lagp  = lt_lagp_mp.

    ENDIF.

    DO 1 TIMES.
      ASSIGN ('(SAPML03T)TAP[]') TO <lt_tap>.
      CHECK <lt_tap> IS ASSIGNED.
      CHECK NOT <lt_tap> IS INITIAL.

      LOOP AT lt_lagp_mp.
        lv_tabix = sy-tabix.

        READ TABLE <lt_tap>
          WITH KEY nltyp = lt_lagp_mp-lgtyp
                   nlpla = lt_lagp_mp-lgpla
          TRANSPORTING NO FIELDS.
        CHECK sy-subrc EQ 0.

        DELETE lt_lagp_mp INDEX lv_tabix.
      ENDLOOP.
    ENDDO.

****
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe0.
    IF sy-subrc = 0 AND ws_t334b-lgbe0 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe1.
    IF sy-subrc = 0 AND ws_t334b-lgbe1 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe2.
    IF sy-subrc = 0 AND ws_t334b-lgbe2 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe3.
    IF sy-subrc = 0 AND ws_t334b-lgbe3 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe4.
    IF sy-subrc = 0 AND ws_t334b-lgbe4 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe5.
    IF sy-subrc = 0 AND ws_t334b-lgbe5 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe6.
    IF sy-subrc = 0 AND ws_t334b-lgbe6 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe7.
    IF sy-subrc = 0 AND ws_t334b-lgbe7 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe8.
    IF sy-subrc = 0 AND ws_t334b-lgbe8 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.
    READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe9.
    IF sy-subrc = 0 AND ws_t334b-lgbe9 IS NOT INITIAL.
      MOVE lt_lagp_mp-lgpla TO e_nlpla.
      EXIT.
    ENDIF.


*    read table lt_lagp_mp index 1.
*    if sy-subrc = 0 and ws_t334b-LGBE0 is not initial.
*      move lt_lagp_mp-lgpla to e_nlpla.
*    endif.

  ENDIF.

  IF i_ltap-nltyp = 'CFO'. "Destino Cafico

    DATA: BEGIN OF lt_max_peso OCCURS 0,
            lgpla TYPE lgpla,
          END OF lt_max_peso.

    DATA: BEGIN OF lt_max_uni OCCURS 0,
            lgpla TYPE lgpla,
          END OF lt_max_uni.

    DATA: lv_max_menge TYPE menge_d.
    DATA: lv_max_menge_str(20) .
    DATA: lv_max_uni TYPE i.
    DATA: lv_max_uni_str(4) .
    DATA: lv_curr_unis TYPE menge_d.
    DATA: lv_uni_estimado TYPE menge_d.


    SELECT * INTO TABLE lt_lagp_mp FROM lagp WHERE
       lgnum = i_ltak-lgnum AND
       lgtyp = 'CFO' AND
       skzua = '' AND
       skzue = '' AND
       skzsa = '' AND
       skzse = ''.

    IF sy-subrc = 0.
      SELECT * INTO TABLE lt_lqua FROM lqua
       FOR ALL ENTRIES IN lt_lagp_mp WHERE
         lgnum =  lt_lagp_mp-lgnum AND
         lgtyp = lt_lagp_mp-lgtyp AND
         lgpla = lt_lagp_mp-lgpla.
    ENDIF.

*
** CAFICO TEM UM MAXIMO DE PESO POR POSICAO (50Kg)
** este valor nao ficou no registo mestre posicoes pq a criacao automatica de OT
** mesmo parameterizado sem verificação de peso cortava quantidade da OT se passasse o limite maximo

    SELECT SINGLE valor INTO lv_max_menge_str FROM zwmmpt001 WHERE
      armazem = i_ltap-lgnum AND
      processo = 'GERAL' AND
      parametro = 'LIMITE_CACIFO_PESO' .
    MOVE lv_max_menge_str TO lv_max_menge.
    IF lv_max_menge IS INITIAL.
      MOVE cons_max_menge TO lv_max_menge.
    ENDIF.


    SELECT SINGLE valor INTO lv_max_uni_str FROM zwmmpt001 WHERE
      armazem = i_ltap-lgnum AND
      processo = 'GERAL' AND
      parametro = 'LIMITE_CACIFO_UNI' .
    MOVE lv_max_uni_str TO lv_max_uni.
    IF lv_max_uni IS INITIAL.
      MOVE cons_max_uni TO lv_max_uni.
    ENDIF.

    ASSIGN ('(SAPML03T)TAP[]') TO <lt_tap>.

    LOOP AT lt_lagp_mp.

*        if I_MLVS-GEWEI <>  lt_lagp_mp-GEWEI.


      IF i_mlvs-gewei <>  cons_uni_kg.
*          necessidade de converter unidades de peso
        lv_conv_menge = i_mlvs-brgew.
        CALL FUNCTION 'CF_UT_UNIT_CONVERSION'
          EXPORTING
            unit_new_imp  = cons_uni_kg
            unit_old_imp  = i_mlvs-gewei
            value_old_imp = lv_conv_menge
          IMPORTING
            value_new_exp = lv_conv_menge
          EXCEPTIONS
            overflow      = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
        ENDIF.
*          ltap-BRGEW = lv_conv_menge.
        i_mlvs-brgew =  lv_conv_menge.
      ENDIF.

      lv_peso_estimado = lt_lagp_mp-mgewi + ( i_mlvs-brgew *  i_ltap-vsolm ).

      DO 1 TIMES.
        CHECK <lt_tap> IS ASSIGNED.

        LOOP AT <lt_tap> INTO ls_tap WHERE matnr = i_mlvs-matnr AND
                                           nlpla = lt_lagp_mp-lgpla.
          lv_peso_estimado = lv_peso_estimado + ( i_mlvs-brgew *  ls_tap-vsolm ).
        ENDLOOP.
      ENDDO.


      IF lv_peso_estimado > lv_max_menge.
        CLEAR: lt_max_peso.
        MOVE lt_lagp_mp-lgpla TO lt_max_peso-lgpla.
        APPEND lt_max_peso.
*        delete lt_lagp_mp index sy-tabix.
      ENDIF.

*      quantas unidades estão na posicao?
      IF i_ltap-meins = 'UN'.
        CLEAR: lv_curr_unis.
        LOOP AT lt_lqua WHERE lgpla = lt_lagp_mp-lgpla.
          ADD lt_lqua-gesme TO lv_curr_unis.
          ADD lt_lqua-einme TO lv_curr_unis.
        ENDLOOP.
        lv_uni_estimado = lv_curr_unis + i_ltap-vsolm.

        DO 1 TIMES.
          CHECK <lt_tap> IS ASSIGNED.

          LOOP AT <lt_tap> INTO ls_tap WHERE matnr = i_mlvs-matnr AND
                                             nlpla = lt_lagp_mp-lgpla.
            lv_uni_estimado = lv_uni_estimado + ls_tap-vsolm.
          ENDLOOP.
        ENDDO.

        IF lv_uni_estimado > lv_max_uni.
          CLEAR: lt_max_uni.
          MOVE lt_lagp_mp-lgpla TO lt_max_uni-lgpla.
          APPEND lt_max_uni.
        ENDIF.
      ENDIF.

    ENDLOOP.


**      prioridade às posições já com o material
*    if lt_lagp_mp[] is not initial.
**      prioridade às posições já com o material
*
*      select * into table lt_lqua from lqua
*      for all entries in lt_lagp_mp where
*        lgnum =  lt_lagp_mp-lgnum and
*        lgtyp = lt_lagp_mp-lgtyp and
*        lgpla = lt_lagp_mp-lgpla.
*
*      read table lt_lqua with key matnr = i_ltap-matnr.
*      if sy-subrc = 0.
*        delete lt_lqua where matnr <> i_ltap-matnr.
*        sort lt_lqua by gesme ascending.
*        read table lt_lqua index 1.
*        move lt_lqua-lgpla to e_nlpla.
*      else.
*        delete lt_lagp_mp where anzqu is not initial.
*        sort lt_lagp_mp by lgpla.
**        sort lt_lagp_mp by ANZQU lgpla.
*        read table lt_lagp_mp index 1.
*        if sy-subrc = 0.
*          move lt_lagp_mp-lgpla to e_nlpla.
*        endif.
*      endif.
*    endif.


    SELECT SINGLE * INTO ws_mlgn FROM mlgn WHERE matnr = i_ltap-matnr AND
                                                 lgnum  = i_ltak-lgnum.
    IF sy-subrc = 0.
      SELECT SINGLE * INTO ws_t334b FROM t334b
      WHERE
        lgnum = i_ltak-lgnum AND
        lgtyp = i_ltap-nltyp AND
        lgbkz = ws_mlgn-lgbkz.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDIF.


    CASE ws_mlgn-lgbkz.
      WHEN 'MP2' OR 'MP3' OR 'MP4'.
        LOOP AT lt_max_uni.
          DELETE lt_lagp_mp WHERE lgpla = lt_max_uni-lgpla.
        ENDLOOP.
        LOOP AT lt_max_peso.
          DELETE lt_lagp_mp WHERE lgpla = lt_max_peso-lgpla.
        ENDLOOP.

      WHEN 'MP5' OR 'MP7'.
        LOOP AT lt_max_peso.
          DELETE lt_lagp_mp WHERE lgpla = lt_max_peso-lgpla.
        ENDLOOP.
    ENDCASE.

    IF lt_lagp_mp[] IS NOT INITIAL.
*      prioridade às posições já com o material

*      clear: lt_lqua. refresh: lt_lqua.
      LOOP AT lt_lqua.
        READ TABLE lt_lagp_mp WITH KEY lgnum = lt_lqua-lgnum
                                       lgtyp = lt_lqua-lgtyp
                                       lgpla = lt_lqua-lgpla.
        IF sy-subrc <> 0.
          DELETE lt_lqua WHERE  lgtyp = lt_lqua-lgtyp AND
                                lgpla = lt_lqua-lgpla.
        ENDIF.
      ENDLOOP.


      READ TABLE lt_lqua WITH KEY matnr = i_ltap-matnr.
      IF sy-subrc = 0.
        DELETE lt_lqua WHERE matnr <> i_ltap-matnr.
        SORT lt_lqua BY gesme ASCENDING.
        READ TABLE lt_lqua INDEX 1.
        MOVE lt_lqua-lgpla TO e_nlpla.
      ELSE.

        DELETE lt_lagp_mp WHERE anzqu IS NOT INITIAL.
        SORT lt_lagp_mp BY lgpla.

*         if ws_mlgn-lgbkz in r_pesados and r_pesados[] is not initial and
*          i_ltap-vltyp <> lv_buf_lgtyp.
*
        READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe0.
        IF sy-subrc = 0.
          MOVE lt_lagp_mp-lgpla TO e_nlpla.
          EXIT.
        ENDIF.

        READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe1.
        IF sy-subrc = 0.
          MOVE lt_lagp_mp-lgpla TO e_nlpla.
          EXIT.
        ENDIF.

        READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe2.
        IF sy-subrc = 0.
          MOVE lt_lagp_mp-lgpla TO e_nlpla.
          EXIT.
        ENDIF.
        READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe3.
        IF sy-subrc = 0.
          MOVE lt_lagp_mp-lgpla TO e_nlpla.
          EXIT.
        ENDIF.
        READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe4.
        IF sy-subrc = 0.
          MOVE lt_lagp_mp-lgpla TO e_nlpla.
          EXIT.
        ENDIF.
        READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe5.
        IF sy-subrc = 0.
          MOVE lt_lagp_mp-lgpla TO e_nlpla.
          EXIT.
        ENDIF.
        READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe6.
        IF sy-subrc = 0.
          MOVE lt_lagp_mp-lgpla TO e_nlpla.
          EXIT.
        ENDIF.
        READ TABLE lt_lagp_mp WITH KEY lgber = ws_t334b-lgbe7.
        IF sy-subrc = 0.
          MOVE lt_lagp_mp-lgpla TO e_nlpla.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
    EXIT.
  ENDIF. "fim logica do cacifo

  EXIT.

  "fim logica do armazem MP 400
***********************************************************************************
ELSEIF i_ltak-lgnum = '500'.
**
  DATA: lv_lines     TYPE i.
  DATA: ls_mlgn      TYPE mlgn.
  DATA: lt_lagp      LIKE lagp OCCURS 0 WITH HEADER LINE.
  DATA: ls_t334b     TYPE t334b.
  DATA: lv_lgbe(14)  TYPE c.
  DATA: lv_index     TYPE c.
  DATA: lt_range     TYPE RANGE OF t334b-lgbe0.
  DATA: ls_range     LIKE LINE OF lt_range.
  DATA: lv_zona      LIKE mlgn-ltkze.
  FIELD-SYMBOLS: <fs_lgbe> TYPE any.

** Obter posição
  SELECT SINGLE *
    FROM mlgn INTO ls_mlgn
    WHERE matnr = i_ltap-matnr AND
          lgnum = i_ltap-lgnum.

  IF sy-subrc = 0.
    SELECT *
      FROM lagp INTO TABLE lt_lagp
      WHERE lgnum = i_ltap-lgnum AND
            lgtyp = i_ltap-nltyp.

**  Separado por áreas
    IF ls_mlgn-lgbkz IS NOT INITIAL.
      SELECT SINGLE * FROM t334b INTO ls_t334b
        WHERE lgnum = i_ltak-lgnum  AND
              lgtyp = i_ltap-nltyp  AND
              lgbkz = ls_mlgn-lgbkz.
      IF sy-subrc = 0.


        DO 30 TIMES VARYING lv_zona FROM
                             ls_t334b-lgbe0 NEXT
                             ls_t334b-lgbe1.

          ls_range-sign   = 'I'.
          ls_range-option = 'EQ'.
          ls_range-low    = lv_zona.
          APPEND ls_range TO lt_range.
        ENDDO.

*        DO 29 TIMES.
*
*          CONCATENATE 'LS_T334B-LGBE' lv_index INTO lv_lgbe.
*          ASSIGN lv_lgbe TO <fs_lgbe>.
*          IF ( sy-subrc NE 0 ).
*            EXIT.
*          ENDIF.
*          ls_range-sign   = 'I'.
*          ls_range-option = 'EQ'.
*          ls_range-low    = <fs_lgbe>.
*
*          APPEND ls_range TO lt_range.
*          ADD 1 TO lv_index .
*        ENDDO.
        DELETE lt_lagp WHERE lgber NOT IN lt_range.
      ENDIF.
*      DELETE lt_lagp WHERE lgber <> ls_mlgn-lgbkz.
    ENDIF.
  ENDIF.

  DESCRIBE TABLE lt_lagp LINES lv_lines.

  IF lv_lines > 1.
*   Estratégia posição vazia
    DELETE lt_lagp WHERE anzqu IS NOT INITIAL.
  ENDIF.

  SORT lt_lagp BY lgpla.

  CLEAR lt_lagp.
  READ TABLE lt_lagp INDEX 1.
  IF sy-subrc = 0.
    e_nlpla = lt_lagp-lgpla.
  ENDIF.

  EXIT.
*& Begin of Modification by Tiago Pateiro - ROFF @ 08.01.2016 15:03:05
ELSEIF i_ltak-lgnum EQ '450'.
  SELECT COUNT(*)
    FROM lagp INTO lv_count
    WHERE lgnum EQ i_ltak-lgnum
      AND lgtyp EQ i_ltap-nltyp.
  IF lv_count EQ 1.
    SELECT lgpla UP TO 1 ROWS
      FROM lagp INTO e_nlpla
      WHERE lgnum EQ i_ltak-lgnum
        AND lgtyp EQ i_ltap-nltyp.
    ENDSELECT.
  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 08.01.2016 15:03:05

ELSEIF i_ltak-lgnum EQ '150'.

  SELECT COUNT(*)
    FROM lagp INTO lv_count
    WHERE lgnum EQ i_ltak-lgnum
      AND lgtyp EQ i_ltap-nltyp.
  IF lv_count EQ 1.
    SELECT lgpla UP TO 1 ROWS
      FROM lagp INTO e_nlpla
      WHERE lgnum EQ i_ltak-lgnum
        AND lgtyp EQ i_ltap-nltyp.
    ENDSELECT.
  ENDIF.

ELSEIF i_ltak-lgnum = '100' AND i_ltap-nltyp = 'PKL'.

  DATA lv_prdha LIKE mara-prdha.

** Obter posição
  SELECT * FROM lagp INTO TABLE lt_lagp
      WHERE lgnum = i_ltak-lgnum AND
            lgtyp = i_ltap-nltyp AND
            kzler = 'X' AND
            skzue = ' ' AND
            skzsi = ' ' AND
            skzse = ' '.

  SELECT SINGLE prdha INTO lv_prdha FROM mara WHERE matnr = i_ltap-matnr.

** Volumes pequenos
  IF lv_prdha = 'PAPTISTRFGUAADC020'.
    SORT lt_lagp BY lgber DESCENDING sorlp ASCENDING lgpla ASCENDING.
  ELSE.
** Volumes Grandes
    DELETE lt_lagp WHERE lgber = '002'.
    SORT lt_lagp BY lgber ASCENDING sorlp ASCENDING lgpla ASCENDING.
  ENDIF.

  CLEAR lt_lagp.
  READ TABLE lt_lagp INDEX 1.
  IF sy-subrc = 0.
    e_nlpla = lt_lagp-lgpla.
  ENDIF.

  EXIT.
ENDIF. "fim logica do armazem Delta 500
***********************************************************************************
** Logica de Armazem 150
***********************************************************************
CALL FUNCTION 'Z_WMFR_EXIT_ENTRADA_ARM_AUTO'
  EXPORTING
    is_ltak  = i_ltak
    is_ltap  = i_ltap
    is_mlvs  = i_mlvs
    is_mgef  = i_mgef
    is_t331  = i_t331
    is_t333  = i_t333
    is_t340d = i_t340d
    i_vorga  = i_vorga
  IMPORTING
    e_nlpla  = e_nlpla
  EXCEPTIONS
    error    = 1
    not_exit = 2
    OTHERS   = 3.

IF sy-subrc <> 2.
  EXIT.
ENDIF.
***********************************************************************


TABLES : zwm014,
         zwm020,
         t334p,
         t334b,
         mlgn.

DATA : BEGIN OF l_lagp OCCURS 0.
         INCLUDE STRUCTURE lagp.
       DATA : END OF l_lagp.

DATA : BEGIN OF lagp_aux OCCURS 0.
DATA : peso.
DATA : zona.
        INCLUDE STRUCTURE lagp.
DATA : peso_garfo.
DATA : END OF lagp_aux.

DATA : BEGIN OF tipo_pal_aux OCCURS 0.
DATA : tipo_pal LIKE t334p-lpty0.
DATA : peso LIKE sy-tabix.
DATA : END OF tipo_pal_aux.

DATA : BEGIN OF zona_aux OCCURS 0.
DATA : zona LIKE mlgn-ltkze.
DATA : peso LIKE sy-tabix.
DATA : END OF zona_aux.

DATA wa_zwm014 LIKE zwm014.

DATA : corredor(4),
       corredor_ant(4),
       su_aux              TYPE lenum,
       bin_aux             TYPE lgpla,
       tipo_palete         LIKE t334p-lpty0,
       n_mensulas_ocupadas TYPE i,
       zona                LIKE t334b-lgbe0,
       save_index          LIKE sy-tabix,
       s_index             LIKE sy-tabix,
       peso                LIKE sy-tabix.

*** Quando se fizer as transferencias é preciso alterar o exit

** Range dos vários tipos de palete
RANGES tipo_pal FOR t334p-lpty0 OCCURS 0.

** Range das zonas
RANGES zonas FOR t334b-lgbe0 OCCURS 0.

CLEAR : corredor, e_nlpla, su_aux, bin_aux,tipo_pal, tipo_palete,
        n_mensulas_ocupadas, save_index, peso,
        lagp_aux, zonas, zona_aux, wa_zwm014, l_lagp.

REFRESH : tipo_pal, lagp_aux, tipo_pal_aux, zonas, zona_aux, l_lagp.

*& Begin of Modification by Tiago Pateiro - ROFF @ 08.01.2016 15:03:05
IF i_ltak-lgnum EQ '450'.
  SELECT COUNT(*)
    FROM lagp INTO lv_count
    WHERE lgnum EQ i_ltak-lgnum
      AND lgtyp EQ i_ltap-nltyp.
  IF lv_count EQ 1.
    SELECT lgpla UP TO 1 ROWS
      FROM lagp INTO e_nlpla
      WHERE lgnum EQ i_ltak-lgnum
        AND lgtyp EQ i_ltap-nltyp.
    ENDSELECT.
  ENDIF.
ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 08.01.2016 15:03:05

*    Erro no exit na ocupacao de mensula
*    Desbloquear as mensulas que tenham ficado ocupadas mas
*    posteriormente existiu um erro

UPDATE zwm014 SET estado = ' ' bin = bin_aux
   WHERE armazem = i_ltak-lgnum AND
         su = su_aux AND estado = 'X'.

** Se criar a OT manualmente com o mov '999' não faz nada
CHECK NOT i_ltak-bwlvs = '999'.

** Achar os vários tipos de palete para o item da to q está a entrar
CLEAR t334p.
SELECT SINGLE * FROM t334p
                WHERE lgnum = i_ltap-lgnum AND
                      letyp = i_ltap-letyp.
IF sy-subrc = 0.
  CLEAR peso.
  DO 30 TIMES VARYING tipo_palete FROM
                     t334p-lpty0 NEXT
                     t334p-lpty1.
    IF NOT tipo_palete IS INITIAL.
      tipo_pal-sign   = 'I'.
      tipo_pal-option = 'EQ'.
      tipo_pal-low    = tipo_palete.
      APPEND tipo_pal.

      peso = peso + 1.
      tipo_pal_aux-tipo_pal = tipo_palete.
      tipo_pal_aux-peso = peso.
      APPEND tipo_pal_aux.

    ELSEIF tipo_palete IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.
ENDIF.


** Descobrir qual é a zona preferencial associada ao material
SELECT SINGLE * FROM mlgn
                    WHERE matnr = i_ltap-matnr AND
                          lgnum = i_ltap-lgnum.

** Calcular AS OUTRAS ZONAS MENOS PREFERENCIAIS associadas ao material
CLEAR t334b.
SELECT SINGLE * FROM t334b
                WHERE lgnum = i_ltap-lgnum AND
                      lgtyp = 'TRI' AND
                      lgbkz = mlgn-ltkze.
IF sy-subrc = 0.
  CLEAR peso.
  DO 30 TIMES VARYING zona FROM
                     t334b-lgbe0 NEXT
                     t334b-lgbe1.
    IF NOT zona IS INITIAL.
      zonas-sign   = 'I'.
      zonas-option = 'EQ'.
      zonas-low    = zona.
      APPEND zonas.

      peso = peso + 1.
      zona_aux-zona = zona.
      zona_aux-peso = peso.
      APPEND zona_aux.

    ELSEIF zona IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.
ENDIF.

** No caso de realocacoes a su vem no campo origem
IF i_ltak-bwlvs = '916'.
  i_ltap-nlenr = i_ltap-vlenr.
ENDIF.

** Temos de fazer distinção entre PALETES SIMPLES E PALETES REMONTADAS

SELECT SINGLE * FROM zwm020
                WHERE armazem = i_ltap-lgnum AND
                      ( p1 = i_ltap-nlenr OR
                        p2 = i_ltap-nlenr ) .

IF sy-subrc = 0.
** Palete Remontada

** Primeira palete remontada
  IF zwm020-bin IS INITIAL.

** 1 - CARREGAR TODAS AS MENSULAS QUE SE ENCONTRAM VAZIAS
    SELECT * INTO CORRESPONDING FIELDS OF TABLE l_lagp
      FROM lagp AS l INNER JOIN zwm014 AS z
          ON l~lgnum = z~armazem AND
             l~lgpla = z~mensula
          WHERE l~lgtyp = 'MEN' AND
                l~skzue = ' ' AND              " mensula desbloqueada
                z~estado = ' '.

** Achou mensulas vazias
    IF sy-subrc = 0.
      SORT l_lagp BY lgpla DESCENDING.
    ENDIF.

** Achar a primeira posição vazia tendo em conta a mensula
    CLEAR corredor_ant.
    LOOP AT l_lagp.
      CLEAR corredor.
      CONCATENATE l_lagp-lgpla(3) '%' INTO corredor.

      IF corredor <> corredor_ant.
        SELECT * FROM lagp APPENDING CORRESPONDING FIELDS OF TABLE
        lagp_aux
                 WHERE lgnum = i_ltak-lgnum AND
                       lgtyp = 'TRI' AND
                       lgpla LIKE corredor AND
                       lptyp IN tipo_pal AND
                       lgber IN zonas AND
                       kzler = 'X' AND
                       kzvol = ' ' AND
                       anzqu = 0   AND
                       skzue = ' ' AND
                       skzsi = ' ' AND
                       skzse = ' '.

        corredor_ant = corredor.
      ENDIF.
    ENDLOOP.


    LOOP AT lagp_aux.
      CLEAR s_index.
      s_index = sy-tabix.

      SORT tipo_pal_aux BY tipo_pal.
      READ TABLE tipo_pal_aux WITH KEY tipo_pal = lagp_aux-lptyp.
      IF sy-subrc = 0.
        lagp_aux-peso = tipo_pal_aux-peso.
      ENDIF.

      SORT zona_aux BY zona.
      READ TABLE zona_aux WITH KEY zona = lagp_aux-lgber.
      IF sy-subrc = 0.
        lagp_aux-zona = zona_aux-peso.
      ENDIF.

      IF l_lagp-brand = lagp_aux-brand.
        lagp_aux-peso_garfo = 1.
      ELSEIF l_lagp-brand <> lagp_aux-brand.
        lagp_aux-peso_garfo = 2.
      ENDIF.

      MODIFY lagp_aux INDEX s_index.
    ENDLOOP.

    SORT lagp_aux BY peso ASCENDING
                     zona ASCENDING
                     sorlp ASCENDING
                     lgpla ASCENDING
                     peso_garfo ASCENDING.

    READ TABLE lagp_aux INDEX 1.
    IF sy-subrc = 0.
      e_nlpla = lagp_aux-lgpla.
    ENDIF.

    SORT l_lagp BY lgpla brand.

    IF NOT e_nlpla IS INITIAL.

      CLEAR: wa_zwm014, zwm014.

      READ TABLE l_lagp WITH KEY lgpla(3) = e_nlpla(3)
                                 brand    = lagp_aux-brand.
      IF sy-subrc <> 0.
        READ TABLE l_lagp WITH KEY lgpla(3) = e_nlpla(3).
      ENDIF.

      SELECT SINGLE *
          FROM zwm014
              WHERE armazem = i_ltak-lgnum AND
                    estado = ' ' AND
                    mensula = l_lagp-lgpla.
      IF sy-subrc = 0.

        MOVE-CORRESPONDING zwm014 TO wa_zwm014.
        wa_zwm014-armazem = i_ltak-lgnum.
        wa_zwm014-mensula = l_lagp-lgpla.
        wa_zwm014-su = i_ltap-nlenr.
        wa_zwm014-estado = 'X'.
        wa_zwm014-bin = e_nlpla.
        MODIFY zwm014 FROM wa_zwm014.

** verificar se neste corredor se está a ocupar
** a terceira mensula do corredor ...se sim colocar
** nas TOs que pertencem ao corredor prioridade máxima

        CONCATENATE l_lagp-lgpla(3) '%' INTO corredor.

        SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                        WHERE armazem = i_ltak-lgnum AND
                              mensula LIKE corredor AND
                              estado = 'X'.

        IF corredor(3) = '001'.

          UPDATE zwm014 SET prioridade = 9
                         WHERE armazem = i_ltak-lgnum AND
                               mensula LIKE corredor AND
                               estado = 'X'.

        ENDIF.

        IF n_mensulas_ocupadas >= 3.

          UPDATE zwm014 SET prioridade = 9
                        WHERE armazem = i_ltak-lgnum AND
                              mensula LIKE corredor AND
                              estado = 'X'.

        ENDIF.
        CLEAR : corredor.
      ENDIF.
    ELSE.

    ENDIF.

** Actualizar dados de paletes remontadas
    zwm020-bin = e_nlpla.
    MODIFY zwm020.

  ELSEIF NOT zwm020-bin IS INITIAL.

    e_nlpla = zwm020-bin.

  ENDIF.

ELSE.
** 1 - CARREGAR TODAS AS MENSULAS QUE SE ENCONTRAM VAZIAS
  SELECT * INTO CORRESPONDING FIELDS OF TABLE l_lagp
    FROM lagp AS l INNER JOIN zwm014 AS z
        ON l~lgnum = z~armazem AND
           l~lgpla = z~mensula
        WHERE l~lgtyp = 'MEN' AND
              l~skzue = ' ' AND              " mensula desbloqueada
              z~estado = ' '.

** Achou mensulas vazias
  IF sy-subrc = 0.
    SORT l_lagp BY lgpla DESCENDING.
  ENDIF.

  CLEAR corredor_ant.
  LOOP AT l_lagp.
    CLEAR corredor.
    CONCATENATE l_lagp-lgpla(3) '%' INTO corredor.

    IF corredor <> corredor_ant.
      SELECT * FROM lagp APPENDING CORRESPONDING FIELDS OF TABLE
      lagp_aux
               WHERE lgnum = i_ltak-lgnum AND
                     lgtyp = 'TRI' AND
                     lgpla LIKE corredor AND
                     lptyp IN tipo_pal AND
                     lgber IN zonas AND
                     kzler = 'X' AND
                     kzvol = ' ' AND
                     anzqu = 0   AND
                     skzue = ' ' AND
                     skzsi = ' ' AND
                     skzse = ' '.

      corredor_ant = corredor.
    ENDIF.

  ENDLOOP.

*  BREAK-POINT.
  CLEAR s_index.
  LOOP AT lagp_aux.
    s_index = sy-tabix.
    SORT tipo_pal_aux BY tipo_pal.
    READ TABLE tipo_pal_aux WITH KEY tipo_pal = lagp_aux-lptyp.
    IF sy-subrc = 0.
      lagp_aux-peso = tipo_pal_aux-peso.
    ENDIF.

    SORT zona_aux BY zona.
    READ TABLE zona_aux WITH KEY zona = lagp_aux-lgber.
    IF sy-subrc = 0.
      lagp_aux-zona = zona_aux-peso.
    ENDIF.

    IF l_lagp-brand = lagp_aux-brand.
      lagp_aux-peso_garfo = 1.
    ELSEIF l_lagp-brand <> lagp_aux-brand.
      lagp_aux-peso_garfo = 2.
    ENDIF.

    MODIFY lagp_aux INDEX s_index.
  ENDLOOP.

  SORT lagp_aux BY peso ASCENDING
                   zona ASCENDING
                   sorlp ASCENDING
                   lgpla ASCENDING
                   peso_garfo ASCENDING.

  READ TABLE lagp_aux INDEX 1.
  IF sy-subrc = 0.
    e_nlpla = lagp_aux-lgpla.
  ENDIF.

  SORT l_lagp BY lgpla brand.

  IF NOT e_nlpla IS INITIAL.

** Actualizar dados da mensula
    CLEAR: wa_zwm014, zwm014.

    READ TABLE l_lagp WITH KEY lgpla(3) = e_nlpla(3)
                               brand    = lagp_aux-brand.
    IF sy-subrc <> 0.
      READ TABLE l_lagp WITH KEY lgpla(3) = e_nlpla(3).
    ENDIF.

    SELECT SINGLE *
        FROM zwm014
            WHERE armazem = i_ltak-lgnum AND
                  estado = ' ' AND
                  mensula = l_lagp-lgpla.
    IF sy-subrc = 0.

      MOVE-CORRESPONDING zwm014 TO wa_zwm014.
      wa_zwm014-armazem = i_ltak-lgnum.
      wa_zwm014-mensula = l_lagp-lgpla.
      wa_zwm014-su = i_ltap-nlenr.
      wa_zwm014-estado = 'X'.
      wa_zwm014-bin = e_nlpla.
      MODIFY zwm014 FROM wa_zwm014.

** verificar se neste corredor se está a ocupar
** a terceira mensula do corredor ...se sim colocar
** nas TOs que pertencem ao corredor prioridade máxima

      CONCATENATE l_lagp-lgpla(3) '%' INTO corredor.

      SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                      WHERE armazem = i_ltak-lgnum AND
                            mensula LIKE corredor AND
                            estado = 'X'.

      IF corredor(3) = '001'.

        UPDATE zwm014 SET prioridade = 9
                       WHERE armazem = i_ltak-lgnum AND
                             mensula LIKE corredor AND
                             estado = 'X'.

      ENDIF.

      IF n_mensulas_ocupadas >= 3.

        UPDATE zwm014 SET prioridade = 9
                      WHERE armazem = i_ltak-lgnum AND
                            mensula LIKE corredor AND
                            estado = 'X'.

      ENDIF.
      CLEAR : corredor.
    ENDIF.
  ELSE.

  ENDIF.

ENDIF.
***********************************************************
************** Versão em Produtivo ************************
***********************************************************
*TABLES : zwm014,
*         zwm020,
*         t334p,
*         mlgn.
*
*DATA : BEGIN OF l_lagp OCCURS 0.
*        INCLUDE STRUCTURE lagp.
*DATA : END OF l_lagp.
*
*DATA : BEGIN OF lagp_aux OCCURS 0.
*DATA : peso.
*DATA : zona.
*        INCLUDE STRUCTURE lagp.
*DATA : END OF lagp_aux.
*
*
*DATA : BEGIN OF tipo_pal_aux OCCURS 0.
*DATA : tipo_pal LIKE t334p-lpty0.
*DATA : peso LIKE sy-tabix.
*DATA : END OF tipo_pal_aux.
*
*DATA : corredor(4),
*       su_aux TYPE lenum,
*       bin_aux TYPE lgpla,
*       tipo_palete LIKE t334p-lpty0,
*       n_mensulas_ocupadas TYPE i,
*       zona1 LIKE mlgn-ltkze,
*       zona2 LIKE mlgn-ltkze,
*       zona3 LIKE mlgn-ltkze,
*       zona4 LIKE mlgn-ltkze,
*       save_index LIKE sy-tabix,
*       peso LIKE sy-tabix.
*
**** Quando se fizer as transferencias é preciso alterar o exit
*
*** Range dos vários tipos de palete
*RANGES tipo_pal FOR t334p-lpty0 OCCURS 0.
*
*CLEAR : corredor, e_nlpla, su_aux, bin_aux,tipo_pal, tipo_palete,
*        n_mensulas_ocupadas,zona1,zona2,zona3,zona4, save_index, peso,
*        lagp_aux.
*REFRESH : tipo_pal, lagp_aux, tipo_pal_aux.
*
**    Erro no exit na ocupacao de mensula
**    Desbloquear as mensulas que tenham ficado ocupadas mas
**    posteriormente existiu um erro
*
*UPDATE zwm014 SET estado = ' ' bin = bin_aux
*   WHERE armazem = i_ltak-lgnum AND
*         su = su_aux AND estado = 'X'.
*
*** Achar os vários tipos de palete para o item da to q está a entrar
*CLEAR t334p.
*SELECT SINGLE * FROM t334p
*                WHERE lgnum = i_ltap-lgnum AND
*                      letyp = i_ltap-letyp.
*IF sy-subrc = 0.
*  CLEAR peso.
*  DO 30 TIMES VARYING tipo_palete FROM
*                     t334p-lpty0 NEXT
*                     t334p-lpty1.
*    IF NOT tipo_palete IS INITIAL.
*      tipo_pal-sign   = 'I'.
*      tipo_pal-option = 'EQ'.
*      tipo_pal-low    = tipo_palete.
*      APPEND tipo_pal.
*
*      peso = peso + 1.
*      tipo_pal_aux-tipo_pal = tipo_palete.
*      tipo_pal_aux-peso = peso.
*      APPEND tipo_pal_aux.
*
*    ELSEIF tipo_palete IS INITIAL.
*      EXIT.
*    ENDIF.
*  ENDDO.
*ENDIF.
*
*
*** Descobrir qual é a zona preferencial associada ao material
*SELECT SINGLE ltkze FROM mlgn INTO zona1
*                    WHERE matnr = i_ltap-matnr AND
*                          lgnum = i_ltap-lgnum.
*
*** Calcular AS OUTRAS ZONAS MENOS PREFERENCIAIS associadas ao material
*CASE zona1.
*  WHEN 'A'.
*    zona1 = '001'.
*    zona2 = 'A'.
*    zona3 = 'B'.
*    zona4 = 'C'.
*  WHEN 'B'.
*    zona1 = '001'.
*    zona2 = 'B'.
*    zona3 = 'C'.
*    zona4 = 'A'.
*  WHEN 'C'.
*    zona1 = '001'.
*    zona2 = 'C'.
*    zona3 = 'A'.
*    zona4 = 'B'.
*  WHEN OTHERS.
*ENDCASE.
*
*** No caso de realocacoes a su vem no campo origem
*IF i_ltak-bwlvs = '916'.
*  i_ltap-nlenr = i_ltap-vlenr.
*ENDIF.
*
*** Temos de fazer distinção entre PALETES SIMPLES E PALETES REMONTADAS
*
*SELECT SINGLE * FROM zwm020
*                WHERE armazem = i_ltap-lgnum AND
*                      ( p1 = i_ltap-nlenr OR
*                        p2 = i_ltap-nlenr ) .
*
*IF sy-subrc = 0.
*** Palete Remontada
*
*** Primeira palete remontada
*  IF zwm020-bin IS INITIAL.
*
*** 1 - CARREGAR TODAS AS MENSULAS QUE SE ENCONTRAM VAZIAS
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE l_lagp
*      FROM lagp AS l INNER JOIN zwm014 AS z
*          ON l~lgnum = z~armazem AND
*             l~lgpla = z~mensula
*          WHERE l~lgtyp = 'MEN' AND
*                l~skzue = ' ' AND              " mensula desbloqueada
*                z~estado = ' '.
*
*** Achou mensulas vazias
*    IF sy-subrc = 0.
*
*      SORT l_lagp BY lgpla DESCENDING.
*** Achar a primeira posição vazia tendo em conta a mensula
*      LOOP AT l_lagp.
*        CONCATENATE l_lagp-lgpla(3) '%' INTO corredor.
*
*        SELECT * FROM lagp INTO CORRESPONDING FIELDS OF TABLE lagp_aux
*                 WHERE lgnum = i_ltak-lgnum AND
*                       lgtyp = 'TRI' AND
*                       lgpla LIKE corredor AND
*                       lptyp IN tipo_pal AND
**                       brand = l_lagp-brand AND
*                       kzler = 'X' AND
*                       skzue = ' ' AND
*                       skzsi = ' ' AND
*                       skzse = ' '.
*        IF sy-subrc = 0.
*          CLEAR save_index.
*          LOOP AT lagp_aux.
*            save_index = sy-tabix.
*            IF lagp_aux-lgber = zona1.
*              lagp_aux-zona = 1.
*            ELSEIF lagp_aux-lgber = zona2.
*              lagp_aux-zona = 2.
*            ELSEIF lagp_aux-lgber = zona3.
*              lagp_aux-zona = 3.
*            ELSEIF lagp_aux-lgber = zona4.
*              lagp_aux-zona = 4.
*            ENDIF.
*
*            SORT tipo_pal_aux BY tipo_pal.
*            READ TABLE tipo_pal_aux WITH KEY tipo_pal = lagp_aux-lptyp.
*            IF sy-subrc = 0.
*              lagp_aux-peso = tipo_pal_aux-peso.
*            ENDIF.
*            MODIFY lagp_aux INDEX save_index.
*          ENDLOOP.
*
*          SORT lagp_aux BY peso ASCENDING
*                           zona ASCENDING
*                           sorlp ASCENDING.
*
*          READ TABLE lagp_aux INDEX 1.
*          IF sy-subrc = 0.
*            e_nlpla = lagp_aux-lgpla.
*            EXIT.
*          ENDIF.
*        ELSE.
*          CLEAR : corredor.
*        ENDIF.
*
*        IF sy-subrc = 0.
*          CLEAR : lagp_aux, zona1, zona2, zona3, zona4.
*          REFRESH : lagp_aux.
*          EXIT.
*        ELSE.
*          CLEAR : lagp_aux.
*          REFRESH : lagp_aux.
*          CONTINUE.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*    IF NOT e_nlpla IS INITIAL.
*
*** Actualizar dados da mensula
*      DATA wa_zwm014 LIKE zwm014.
*
*      CLEAR: wa_zwm014, zwm014.
*
*
*      SELECT SINGLE *
*          FROM zwm014
*              WHERE armazem = i_ltak-lgnum AND
*                    estado = ' ' AND
*                    mensula = l_lagp-lgpla.
*      IF sy-subrc = 0.
*
*        MOVE-CORRESPONDING zwm014 TO wa_zwm014.
*        wa_zwm014-armazem = i_ltak-lgnum.
*        wa_zwm014-mensula = l_lagp-lgpla.
*        wa_zwm014-su = i_ltap-nlenr.
*        wa_zwm014-estado = 'X'.
*        wa_zwm014-bin = e_nlpla.
*        MODIFY zwm014 FROM wa_zwm014.
**        COMMIT WORK.
*
*      ENDIF.
*
*** verificar se neste corredor se está a ocupar
*** a terceira mensula do corredor ...se sim colocar
*** nas TOs que pertencem ao corredor prioridade máxima
*      SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
*                      WHERE armazem = i_ltak-lgnum AND
*                            mensula LIKE corredor AND
*                            estado = 'X'.
*
*      IF corredor(3) = '001'.
*
*        UPDATE zwm014 SET prioridade = 9
*                       WHERE armazem = i_ltak-lgnum AND
*                             mensula LIKE corredor AND
*                             estado = 'X'.
*
*      ENDIF.
*
*      IF n_mensulas_ocupadas >= 3.
*
*        UPDATE zwm014 SET prioridade = 9
*                      WHERE armazem = i_ltak-lgnum AND
*                            mensula LIKE corredor AND
*                            estado = 'X'.
*
*      ENDIF.
*      CLEAR : corredor.
*    ELSE.
*
*    ENDIF.
*
*** Actualizar dados de paletes remontadas
*    zwm020-bin = e_nlpla.
*    MODIFY zwm020.
*
*  ELSEIF NOT zwm020-bin IS INITIAL.
*
*    e_nlpla = zwm020-bin.
*
*  ENDIF.
*
*
*ELSE.
*** 1 - CARREGAR TODAS AS MENSULAS QUE SE ENCONTRAM VAZIAS
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE l_lagp
*    FROM lagp AS l INNER JOIN zwm014 AS z
*        ON l~lgnum = z~armazem AND
*           l~lgpla = z~mensula
*        WHERE l~lgtyp = 'MEN' AND
*              l~skzue = ' ' AND              " mensula desbloqueada
*              z~estado = ' '.
*
*** Achou mensulas vazias
*  IF sy-subrc = 0.
*
*    SORT l_lagp BY lgpla DESCENDING.
*** Achar a primeira posição vazia tendo em conta a mensula
*    LOOP AT l_lagp.
*      CONCATENATE l_lagp-lgpla(3) '%' INTO corredor.
*      SELECT * FROM lagp INTO CORRESPONDING FIELDS OF TABLE lagp_aux
*               WHERE lgnum = i_ltak-lgnum AND
*                     lgtyp = 'TRI' AND
*                     lgpla LIKE corredor AND
*                     lptyp IN tipo_pal AND
**                     brand = l_lagp-brand AND
*                     kzler = 'X' AND
*                     skzue = ' ' AND
*                     skzsi = ' ' AND
*                     skzse = ' '.
*      IF sy-subrc = 0.
*** Actualização das zonas preferenciais para os materiais
*        CLEAR save_index.
*        LOOP AT lagp_aux.
*          save_index = sy-tabix.
*          IF lagp_aux-lgber = zona1.
*            lagp_aux-zona = 1.
*          ELSEIF lagp_aux-lgber = zona2.
*            lagp_aux-zona = 2.
*          ELSEIF lagp_aux-lgber = zona3.
*            lagp_aux-zona = 3.
*          ELSEIF lagp_aux-lgber = zona4.
*            lagp_aux-zona = 4.
*          ENDIF.
*
*          SORT tipo_pal_aux BY tipo_pal.
*          READ TABLE tipo_pal_aux WITH KEY tipo_pal = lagp_aux-lptyp.
*          IF sy-subrc = 0.
*            lagp_aux-peso = tipo_pal_aux-peso.
*          ENDIF.
*          MODIFY lagp_aux INDEX save_index.
*        ENDLOOP.
*
*        SORT lagp_aux BY peso ASCENDING
*                         zona ASCENDING
*                         sorlp ASCENDING.
*
*        READ TABLE lagp_aux INDEX 1.
*        IF sy-subrc = 0.
*          e_nlpla = lagp_aux-lgpla.
*          EXIT.
*        ENDIF.
*      ELSE.
*        CLEAR : corredor.
*      ENDIF.
*
*      IF sy-subrc = 0.
*        CLEAR : lagp_aux, zona1, zona2, zona3, zona4.
*        REFRESH : lagp_aux.
*        EXIT.
*      ELSE.
*        CLEAR : lagp_aux.
*        REFRESH : lagp_aux.
*        CONTINUE.
*      ENDIF.
*    ENDLOOP.
*
*    IF NOT e_nlpla IS INITIAL.
*
*** Actualizar dados da mensula
*
*      CLEAR: wa_zwm014, zwm014.
*
*
*      SELECT SINGLE *
*          FROM zwm014
*              WHERE armazem = i_ltak-lgnum AND
*                    estado = ' ' AND
*                    mensula = l_lagp-lgpla.
*      IF sy-subrc = 0.
*
*        MOVE-CORRESPONDING zwm014 TO wa_zwm014.
*        wa_zwm014-armazem = i_ltak-lgnum.
*        wa_zwm014-mensula = l_lagp-lgpla.
*        wa_zwm014-su = i_ltap-nlenr.
*        wa_zwm014-estado = 'X'.
*        wa_zwm014-bin = e_nlpla.
*        MODIFY zwm014 FROM wa_zwm014.
*
*** verificar se neste corredor se está a ocupar
*** a terceira mensula do corredor ...se sim colocar
*** nas TOs que pertencem ao corredor prioridade máxima
*
*        SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
*                        WHERE armazem = i_ltak-lgnum AND
*                              mensula LIKE corredor AND
*                              estado = 'X'.
*
*        IF corredor(3) = '001'.
*
*          UPDATE zwm014 SET prioridade = 9
*                         WHERE armazem = i_ltak-lgnum AND
*                               mensula LIKE corredor AND
*                               estado = 'X'.
*
*        ENDIF.
*
*        IF n_mensulas_ocupadas >= 3.
*
*          UPDATE zwm014 SET prioridade = 9
*                        WHERE armazem = i_ltak-lgnum AND
*                              mensula LIKE corredor AND
*                              estado = 'X'.
*
*        ENDIF.
*        CLEAR : corredor.
*
*      ENDIF.
*    ELSE.
*
*    ENDIF.
*
*  ENDIF.

*ENDIF.
