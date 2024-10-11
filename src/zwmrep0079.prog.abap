************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0079                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Paletização Especial                                     *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 01/08/2007                                               *
*                                                                      *
* Alterado por Paulo Sousa 2011.05.30. Adicionado parâmetro Num.Copias *
************************************************************************
REPORT zwmrep0079 MESSAGE-ID zwmmsg001.

************************************************************************
** Tabelas DD
************************************************************************
TABLES: t311a, ltak, ltap, zwm026, zwm044, vbpa, zwm031, zwm065,likp.

************************************************************************
** Variáveis
************************************************************************

DATA: lt_t311a   LIKE t311a  OCCURS 0 WITH HEADER LINE,
      lt_vbuk    LIKE vbuk   OCCURS 0 WITH HEADER LINE,
      lt_ltak    LIKE ltak   OCCURS 0 WITH HEADER LINE,
      lt_ltap    LIKE ltap   OCCURS 0 WITH HEADER LINE,
      lt_lips    LIKE lips   OCCURS 0 WITH HEADER LINE,
      lt_zwm026  LIKE zwm026 OCCURS 0 WITH HEADER LINE,
      l_zwm026   LIKE zwm026 OCCURS 0 WITH HEADER LINE,
      lt_zwm044  LIKE zwm044 OCCURS 0 WITH HEADER LINE,
*      lt_zwm031  LIKE zwm031 OCCURS 0 WITH HEADER LINE,
      items      LIKE zwm_items_hu OCCURS 0 WITH HEADER LINE,
      items2     LIKE zwm_items_hu OCCURS 0 WITH HEADER LINE,
      return_msg TYPE bdcmsgcoll   OCCURS 0 WITH HEADER LINE,
      t_sscc     LIKE zwm_ean128   OCCURS 0 WITH HEADER LINE,
      l_sscc     LIKE zwm_ean128   OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF lt_zwm031 OCCURS 0.
         INCLUDE STRUCTURE zwm031.
         DATA: vbeln LIKE lips-vbeln.
DATA : END OF lt_zwm031.

DATA : BEGIN OF lt_qty OCCURS 0,
         refnr LIKE t311-refnr,
         vbeln LIKE lips-vbeln,
         matnr LIKE lips-matnr,
         charg LIKE ltap-charg,
         vsolm LIKE ltap-vsolm,
         meins LIKE ltap-meins.
DATA : END OF lt_qty.

DATA : BEGIN OF lt_resto OCCURS 0,
         refnr LIKE t311-refnr,
         vbeln LIKE lips-vbeln,
         matnr LIKE lips-matnr,
         charg LIKE ltap-charg,
         vsolm LIKE ltap-vsolm,
         meins LIKE ltap-meins,
         werks LIKE ltap-werks,
         lgort LIKE ltap-lgort.
DATA :END OF lt_resto.

DATA : BEGIN OF lt_total OCCURS 0,
         refnr LIKE t311-refnr,
         vbeln LIKE lips-vbeln,
         matnr LIKE lips-matnr,
         vsolm LIKE ltap-vsolm,
         meins LIKE ltap-meins.
DATA : END OF lt_total.

DATA: qtd_total  LIKE ltap-vsolm,
      qtd        LIKE ltap-vsolm,
      new_sscc   LIKE vekp-exidv,
      qtd_resto  LIKE ltap-vsolm,
      n_pal      LIKE zwm026-n_pal_picking,
      e_subrc    LIKE sy-subrc,
      save_index LIKE sy-tabix,
      resto      TYPE i,
      lv_2step   TYPE flag,
      lv_2spart  TYPE flag.

************************************************************************
** Opções de selecção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum   LIKE t311-lgnum     OBLIGATORY.
PARAMETERS: p_refnr   LIKE t311-refnr     OBLIGATORY.
PARAMETERS: p_pmatn   LIKE mara-matnr     OBLIGATORY.
PARAMETERS: p_print   LIKE nast-ldest     OBLIGATORY.
PARAMETERS: p_copies  LIKE itcpp-tdcopies OBLIGATORY DEFAULT 3.
PARAMETERS: p_com     TYPE char1 AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
** Initialization
************************************************************************
INITIALIZATION.

************************************************************************
** Start-of-Selection
************************************************************************
START-OF-SELECTION.

  DATA lv_benum TYPE lvs_benum.

  CLEAR:   lt_t311a, lt_ltak, lt_ltap, l_sscc, lt_vbuk,
           lt_zwm026, l_zwm026, lt_zwm044, lt_zwm031, items.
  REFRESH: lt_t311a, lt_ltak, lt_ltap, l_sscc, lt_vbuk,
           lt_zwm026, l_zwm026,lt_zwm044, lt_zwm031, items.


  SELECT SINGLE * FROM zwm044 WHERE lgnum = p_lgnum AND refnr = p_refnr.

  IF sy-subrc = 0.
    MESSAGE i000 WITH 'Paletização já foi efectuada'.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE lt_t311a
    FROM t311a
        WHERE lgnum = p_lgnum
          AND refnr = p_refnr.

  CHECK NOT lt_t311a[] IS INITIAL.

  SELECT * INTO TABLE lt_vbuk
      FROM vbuk
          FOR ALL ENTRIES IN lt_t311a
              WHERE vbeln = lt_t311a-rbnum.

  CLEAR e_subrc.
  LOOP AT lt_vbuk WHERE pkstk = 'C'.
    MESSAGE i000 WITH ' Operação invalida ! Embalamento Efectuado'.
    e_subrc = 4.
    EXIT.
  ENDLOOP.
  IF e_subrc <> 0.
    EXIT.
  ENDIF.

  CLEAR e_subrc.
  LOOP AT lt_vbuk WHERE wbstk = 'C'.
    MESSAGE i000 WITH ' Operação invalida ! Saida de Mercadoria já efectuada. '.
    e_subrc = 4.
    EXIT.
  ENDLOOP.
  IF e_subrc <> 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = p_lgnum
      i_refnr  = p_refnr
    IMPORTING
      e_2step  = lv_2step
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE lt_ltak
      FROM ltak
          WHERE lgnum = p_lgnum
            AND refnr = p_refnr.

  CHECK NOT lt_ltak[] IS INITIAL.

  DELETE lt_ltak WHERE bwlvs = '940'.

  SELECT * INTO TABLE lt_ltap
     FROM ltap
         FOR ALL ENTRIES IN lt_ltak
             WHERE lgnum = lt_ltak-lgnum
               AND tanum = lt_ltak-tanum.

  DELETE lt_ltap WHERE vorga = 'ST'.

  IF lv_2step EQ 'X' AND
     lv_2spart EQ ' '.

    LOOP AT lt_ltap.
      lt_ltap-vbeln = '9999999999'.
      MODIFY lt_ltap.
    ENDLOOP.

    LOOP AT lt_ltak.
      lt_ltak-benum = '9999999999'.
      MODIFY lt_ltak.
    ENDLOOP.

  ELSEIF lv_2step EQ 'X' AND
         lv_2spart EQ 'X'.

    LOOP AT lt_ltak WHERE benum IS NOT INITIAL OR
                          vbeln IS NOT INITIAL.
      LOOP AT lt_ltap WHERE tanum = lt_ltak-tanum AND
                            vbeln IS INITIAL.

        IF NOT lt_ltak-benum  IS INITIAL.
          lt_ltap-vbeln = lt_ltak-benum.
        ELSE.
          lt_ltap-vbeln = lt_ltak-vbeln.
        ENDIF.

        MODIFY lt_ltap.
      ENDLOOP.
    ENDLOOP.

  ENDIF.

  LOOP AT lt_ltap.
    IF lt_ltap-vltyp = '815' AND lt_ltap-nltyp = '916'.
      DELETE lt_ltap WHERE tanum = lt_ltap-tanum.
      DELETE lt_ltak WHERE tanum = lt_ltap-tanum.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_t311a.
    save_index = sy-tabix.

    IF lv_2step EQ 'X' AND
        lv_2spart EQ ' '.
      lv_benum = '9999999999'.
    ELSE.
      lv_benum = lt_t311a-rbnum.
    ENDIF.

    LOOP AT lt_ltak WHERE benum = lv_benum.
      EXIT.
    ENDLOOP.
    IF sy-subrc <> 0.
      DELETE lt_t311a INDEX save_index.
      CONTINUE.
    ENDIF.

    CLEAR vbpa.
    SELECT SINGLE kunnr INTO vbpa-kunnr
        FROM vbpa
            WHERE vbeln EQ lt_t311a-rbnum
              AND posnr = '000000'
              AND parvw EQ 'W1'.

    IF sy-subrc NE 0.
      SELECT SINGLE kunnr INTO vbpa-kunnr
          FROM vbpa
              WHERE vbeln EQ lt_t311a-rbnum
                AND posnr = '000000'
                AND parvw EQ 'WE'.
    ENDIF.

    CHECK NOT vbpa-kunnr IS INITIAL.

*    IF lv_2step EQ 'X' AND
*       lv_2spart EQ ' '.
    CLEAR zwm065.
    SELECT SINGLE * FROM zwm065
        WHERE lgnum = p_lgnum
          AND kunnr = vbpa-kunnr.
*      IF sy-subrc <> 0.

    CLEAR likp.
    SELECT SINGLE * FROM likp WHERE vbeln = lt_t311a-rbnum.

    LOOP AT lt_ltak WHERE benum = lv_benum.
      LOOP AT lt_ltap WHERE lgnum = lt_ltak-lgnum AND tanum = lt_ltak-tanum.
        IF lt_ltap-vbeln IS INITIAL.
          IF zwm065-kunnr IS NOT INITIAL.
            lt_ltap-vbeln = lt_t311a-rbnum.
            MODIFY lt_ltap.
          ELSE.
            lt_ltap-vbeln = '9999999999'.
            MODIFY lt_ltap.

            IF likp-vkorg <> 'SER1'.
              lt_t311a-rbnum = '9999999999'.
              MODIFY lt_t311a INDEX save_index.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  SORT lt_t311a.
  DELETE ADJACENT DUPLICATES FROM lt_t311a COMPARING refnr rbnum.

*    LOOP AT lt_ltap.
*      lt_ltap-vbeln = '9999999999'.
*      MODIFY lt_ltap.
*    ENDLOOP.
*  ENDIF.

  IF p_com IS INITIAL.
    SELECT * INTO TABLE lt_zwm026
        FROM zwm026
            WHERE armazem = p_lgnum
              AND grupo   = p_refnr.
  ENDIF.

** INI
** Alteração para verificar se só se cria novas HU´s
** para as paletes completas ou para completas /incompletas
  CLEAR lt_total.
  REFRESH lt_total.
  LOOP AT lt_t311a.

    IF lv_2step EQ 'X' AND
        lv_2spart EQ ' '.
      lv_benum = '9999999999'.
    ELSE.
      lv_benum = lt_t311a-rbnum.
    ENDIF.

    CLEAR vbpa.
    SELECT SINGLE kunnr INTO vbpa-kunnr
        FROM vbpa
            WHERE vbeln EQ lt_t311a-rbnum
              AND posnr = '000000'
              AND parvw EQ 'W1'.

    IF sy-subrc NE 0.
      SELECT SINGLE kunnr INTO vbpa-kunnr
          FROM vbpa
              WHERE vbeln EQ lt_t311a-rbnum
                AND posnr = '000000'
                AND parvw EQ 'WE'.
    ENDIF.

    CHECK NOT vbpa-kunnr IS INITIAL.

*    IF lv_2step EQ 'X' AND
*     lv_2spart EQ ' '.
*      lt_t311a-rbnum = '9999999999'.
*    ENDIF.

    LOOP AT lt_ltap WHERE lgnum = p_lgnum
                      AND vbeln = lv_benum.

      SELECT SINGLE * FROM  zwm031
          WHERE lgnum  = p_lgnum
            AND kunnr  = vbpa-kunnr
            AND matnr  = lt_ltap-matnr.

      CHECK sy-subrc = 0.
** Material com paletização especial

** Paletes Completas
      IF NOT lt_ltap-vlenr IS INITIAL.
        lt_total-refnr = p_refnr.
        lt_total-vbeln = lt_t311a-rbnum.
        lt_total-matnr = lt_ltap-matnr.
        lt_total-vsolm = lt_ltap-vsolm.
        lt_total-meins = lt_ltap-meins.
        COLLECT lt_total.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  SORT lt_total.
** FIM

  LOOP AT lt_t311a.

    IF lv_2step EQ 'X' AND
        lv_2spart EQ ' '.
      lv_benum = '9999999999'.
    ELSE.
      lv_benum = lt_t311a-rbnum.
    ENDIF.

    CLEAR vbpa.
    SELECT SINGLE kunnr INTO vbpa-kunnr
        FROM vbpa
            WHERE vbeln EQ lt_t311a-rbnum
              AND posnr = '000000'
              AND parvw EQ 'W1'.

    IF sy-subrc NE 0.
      SELECT SINGLE kunnr INTO vbpa-kunnr
          FROM vbpa
              WHERE vbeln EQ lt_t311a-rbnum
                AND posnr = '000000'
                AND parvw EQ 'WE'.
    ENDIF.

    CHECK NOT vbpa-kunnr IS INITIAL.

*    IF lv_2step EQ 'X' AND
*       lv_2spart EQ ' '.
*      lt_t311a-rbnum = '9999999999'.
*    ENDIF.
    LOOP AT lt_ltap WHERE lgnum = p_lgnum
                      AND vbeln = lv_benum.

      SELECT SINGLE * FROM  zwm031
          WHERE lgnum  = p_lgnum
            AND kunnr  = vbpa-kunnr
            AND matnr  = lt_ltap-matnr.

      CHECK sy-subrc = 0.
** Material com paletização especial

      MOVE-CORRESPONDING zwm031 TO lt_zwm031.
      MOVE lt_t311a-rbnum TO lt_zwm031-vbeln.
      APPEND lt_zwm031.

** Paletes Completas
      IF NOT lt_ltap-vlenr IS INITIAL.
        MOVE p_lgnum       TO lt_zwm044-lgnum.
        MOVE p_refnr       TO lt_zwm044-refnr.
        MOVE lt_ltap-vbeln TO lt_zwm044-vbeln.
        MOVE lt_ltap-vlenr TO lt_zwm044-exidv.
        MOVE 'I'           TO lt_zwm044-status.
        COLLECT lt_zwm044.

        lt_qty-refnr = p_refnr.
        lt_qty-vbeln = lt_ltap-vbeln.
        lt_qty-matnr = lt_ltap-matnr.
        lt_qty-charg = lt_ltap-charg.
        lt_qty-vsolm = lt_ltap-vsolm.
        lt_qty-meins = lt_ltap-meins.
        COLLECT lt_qty.

      ELSE.
** Paletes Incompletas
*        READ TABLE lt_total WITH KEY refnr = p_refnr
*                              vbeln = lt_ltap-vbeln
*                              matnr = lt_ltap-matnr.
*        IF sy-subrc = 0.
*          CLEAR resto.
*          resto = lt_total-vsolm MOD zwm031-unporpal.
*        ELSEIF sy-subrc <> 0.
*          CLEAR resto.
*        ENDIF.
*
*        CHECK NOT resto IS INITIAL.

        CLEAR n_pal.
        LOOP AT lt_zwm026 WHERE armazem   = p_lgnum
                            AND grupo     = p_refnr
                            AND remessa   = lt_ltap-vbeln
                            AND to_number = lt_ltap-tanum.

          n_pal = lt_zwm026-n_pal_picking.
        ENDLOOP.

        CLEAR save_index.
        LOOP AT lt_zwm026 WHERE armazem   = p_lgnum
                            AND grupo     = p_refnr
                            AND remessa   = lt_ltap-vbeln
                            AND n_pal_picking = n_pal.

          save_index = sy-tabix.
          IF lt_zwm026-material  = lt_ltap-matnr.

            IF lt_zwm026-lote = lt_ltap-charg.

              MOVE p_lgnum           TO lt_zwm044-lgnum.
              MOVE p_refnr           TO lt_zwm044-refnr.
              MOVE lt_zwm026-remessa TO lt_zwm044-vbeln.
              MOVE lt_zwm026-sscc    TO lt_zwm044-exidv.
              MOVE 'I'               TO lt_zwm044-status.
              COLLECT lt_zwm044.

              lt_qty-refnr = p_refnr.
              lt_qty-vbeln = lt_ltap-vbeln.
              lt_qty-matnr = lt_ltap-matnr.
              lt_qty-charg = lt_ltap-charg.
*              lt_qty-vsolm = lt_ltap-vsolm.
              lt_qty-vsolm = lt_zwm026-quantidade.
              lt_qty-meins = lt_ltap-meins.
              COLLECT lt_qty.

              DELETE lt_zwm026 INDEX save_index.
            ENDIF.
          ELSE.

            SELECT SINGLE * FROM  zwm031
                WHERE lgnum  = p_lgnum
                  AND kunnr  = vbpa-kunnr
                  AND matnr  = lt_zwm026-material.

            IF sy-subrc <> 0.
              l_sscc-sscc = lt_zwm026-sscc.
              COLLECT l_sscc.

              MOVE-CORRESPONDING lt_zwm026 TO l_zwm026.
              APPEND l_zwm026.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF lt_zwm031[] IS INITIAL.
    MESSAGE i000 WITH ' Não existem materiais sujeitos a Paletização Especial'.
    EXIT.
  ENDIF.

  IF NOT lt_zwm044[] IS INITIAL.
    INSERT zwm044 FROM TABLE lt_zwm044.
    COMMIT WORK.
  ENDIF.


END-OF-SELECTION.
  DATA lv_werks TYPE werks_d. " << INS ROFF(SDF):TMGP:18.01.2016 10:21:46
  DATA lv_lgort TYPE lgort_d.

  CLEAR lt_zwm044.
  REFRESH lt_zwm044.

  SORT lt_zwm031 BY lgnum vbeln kunnr matnr.
  DELETE ADJACENT DUPLICATES FROM lt_zwm031 COMPARING lgnum vbeln kunnr matnr.

** Obter dados da Remessa
  IF lt_t311a[] IS NOT INITIAL.
    REFRESH: lt_lips.

    SELECT *
      FROM lips INTO TABLE lt_lips
      FOR ALL ENTRIES IN lt_t311a
      WHERE vbeln = lt_t311a-rbnum.

    SORT lt_lips BY vbeln matnr charg.
  ENDIF.


*& Begin of Modification by Tiago Pateiro - ROFF @ 18.01.2016 10:24:17
**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO lv_werks
**    WHERE lgort EQ 'CD'
**      AND lgnum EQ p_lgnum.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    lv_werks  = 'RENV'.
**  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 18.01.2016 10:24:17

  LOOP AT lt_t311a.

*    CLEAR: lv_werks, lv_lgort.
*    CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
*     EXPORTING
*       i_user              = sy-uname
*       i_refnr             = p_refnr
*       i_vbeln             = lt_t311a-rbnum
*       i_recall            = 'X'
*       i_usewm             = 'X'
*       i_usemm             = 'X'
*       i_useaut            = 'X'
**   IMPORTING
**     ET_MESSAGES         =
*     CHANGING
*       c_lgnum             = p_lgnum
*       c_werks             = lv_werks
*       c_lgort             = lv_lgort
*     EXCEPTIONS
*       error               = 1
*       user_back           = 2
*       OTHERS              = 3.




*    IF lv_2step EQ 'X' AND
*     lv_2spart EQ ' '.
*      lt_t311a-rbnum = '9999999999'.
*    ENDIF.
    LOOP AT lt_zwm031 WHERE vbeln = lt_t311a-rbnum.

      CLEAR: lv_werks, lv_lgort.
      CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
        EXPORTING
          i_user    = sy-uname
          i_matnr   = lt_zwm031-matnr
          i_refnr   = p_refnr
          i_vbeln   = lt_t311a-rbnum
          i_recall  = 'X'
          i_usewm   = 'X'
          i_usemm   = 'X'
          i_useaut  = 'X'
*   IMPORTING
*         ET_MESSAGES         =
        CHANGING
          c_lgnum   = p_lgnum
          c_werks   = lv_werks
          c_lgort   = lv_lgort
        EXCEPTIONS
          error     = 1
          user_back = 2
          OTHERS    = 3.

      IF lv_2step EQ 'X' AND
          lv_2spart EQ ' '.
        lv_benum = '9999999999'.
      ELSE.
        lv_benum = lt_t311a-rbnum.
      ENDIF.

      IF lv_werks IS INITIAL.
        IF p_lgnum = '100'.
          lv_werks = 'RENV'.
        ELSEIF p_lgnum = '150'.
          lv_werks = 'RFRA'.
        ENDIF.
      ENDIF.

      LOOP AT lt_qty WHERE refnr = lt_t311a-refnr
                       AND vbeln = lv_benum
                       AND matnr = lt_zwm031-matnr.

*      CLEAR lt_zwm031.
*      READ TABLE lt_zwm031 WITH KEY lgnum = p_lgnum
*                                    matnr = lt_qty-matnr.
*      CHECK sy-subrc = 0.

        CLEAR qtd_total.
        WHILE qtd_total < lt_qty-vsolm.

          CLEAR qtd.
          qtd = lt_qty-vsolm - qtd_total.
          IF qtd >= lt_zwm031-unporpal.
            qtd = lt_zwm031-unporpal.
          ELSE.
** resto de quantidade
            MOVE-CORRESPONDING lt_qty TO lt_resto.
            lt_resto-vsolm = qtd.
            APPEND lt_resto.
            EXIT.
          ENDIF.

          qtd_total = qtd_total + qtd.

          CLEAR items.
          REFRESH items.

          items-material = lt_qty-matnr.
          items-quantity = qtd.
          items-unit     = lt_qty-meins.
          items-batch    = lt_qty-charg.
          COLLECT items.
          CLEAR items.

          " Obter Centro e Depósito da Remessa
          READ TABLE lt_lips WITH KEY vbeln = lt_t311a-rbnum
                                      matnr = lt_qty-matnr
                                      charg = lt_qty-charg.
          IF sy-subrc <> 0.
            READ TABLE lt_lips WITH KEY vbeln = lt_t311a-rbnum
                                        matnr = lt_qty-matnr.
          ENDIF.

          IF sy-subrc = 0.
            lv_werks = lt_lips-werks.
            lv_lgort = lt_lips-lgort.
          ENDIF.

          CLEAR new_sscc.
          CALL FUNCTION 'ZWM_CREATE_HU'
            EXPORTING
              warehouse                  = p_lgnum
*             plant                      = 'RENV' " << DEL ROFF(SDF):TMGP:18.01.2016 10:25:30
              plant                      = lv_werks " << INS ROFF(SDF):TMGP:18.01.2016 10:25:27
              s_loc                      = lv_lgort
              packing_material           = p_pmatn
            IMPORTING
              hukey                      = new_sscc
            TABLES
              return_msg                 = return_msg
              items                      = items
            EXCEPTIONS
              empty_table                = 1
              reference_document_differs = 2
              empty_delivery_item        = 3
              item_not_found             = 4
              OTHERS                     = 5.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            MOVE p_lgnum       TO lt_zwm044-lgnum.
            MOVE p_refnr       TO lt_zwm044-refnr.
            MOVE lt_qty-vbeln  TO lt_zwm044-vbeln.
            MOVE new_sscc      TO lt_zwm044-exidv.
            MOVE 'F'           TO lt_zwm044-status.
            COLLECT lt_zwm044.

            INSERT zwm044 FROM TABLE lt_zwm044.
            COMMIT WORK.
            CLEAR lt_zwm044.
            REFRESH lt_zwm044.

          ENDIF.
        ENDWHILE.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  CLEAR items.
  REFRESH items.

  IF NOT lt_resto[] IS INITIAL.

    SORT lt_resto BY matnr vsolm DESCENDING.
    LOOP AT lt_t311a.


*      CLEAR: lv_werks, lv_lgort.
*      CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
*        EXPORTING
*          i_user    = sy-uname
*          i_refnr   = p_refnr
*          i_vbeln   = lt_t311a-rbnum
*          i_recall  = 'X'
*          i_usewm   = 'X'
*          i_usemm   = 'X'
*          i_useaut  = 'X'
**   IMPORTING
**         ET_MESSAGES         =
*        CHANGING
*          c_lgnum   = p_lgnum
*          c_werks   = lv_werks
*          c_lgort   = lv_lgort
*        EXCEPTIONS
*          error     = 1
*          user_back = 2
*          OTHERS    = 3.


      CLEAR qtd_total.
*      IF lv_2step EQ 'X' AND
*       lv_2spart EQ ' '.
*        lt_t311a-rbnum = '9999999999'.
*      ENDIF.
      LOOP AT lt_zwm031 WHERE vbeln = lt_t311a-rbnum.

        CLEAR: lv_werks, lv_lgort.
        CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
          EXPORTING
            i_user    = sy-uname
            i_matnr   = lt_zwm031-matnr
            i_refnr   = p_refnr
            i_vbeln   = lt_t311a-rbnum
            i_recall  = 'X'
            i_usewm   = 'X'
            i_usemm   = 'X'
            i_useaut  = 'X'
*   IMPORTING
*           ET_MESSAGES         =
          CHANGING
            c_lgnum   = p_lgnum
            c_werks   = lv_werks
            c_lgort   = lv_lgort
          EXCEPTIONS
            error     = 1
            user_back = 2
            OTHERS    = 3.

        IF lv_werks IS INITIAL.
          IF p_lgnum = '100'.
            lv_werks = 'RENV'.
          ELSEIF p_lgnum = '150'.
            lv_werks = 'RFRA'.
          ENDIF.
        ENDIF.

        IF lv_2step EQ 'X' AND
            lv_2spart EQ ' '.
          lv_benum = '9999999999'.
        ELSE.
          lv_benum = lt_t311a-rbnum.
        ENDIF.

        LOOP AT lt_resto WHERE refnr = lt_t311a-refnr
                           AND vbeln = lv_benum
                           AND matnr = lt_zwm031-matnr.

          " Obter Centro e Depósito da Remessa
          READ TABLE lt_lips WITH KEY vbeln = lt_t311a-rbnum
                                      matnr = lt_resto-matnr
                                      charg = lt_resto-charg.
          IF sy-subrc <> 0.
            READ TABLE lt_lips WITH KEY vbeln = lt_t311a-rbnum
                                        matnr = lt_resto-matnr.
          ENDIF.

          IF sy-subrc = 0.
            lv_werks = lt_lips-werks.
            lv_lgort = lt_lips-lgort.
          ENDIF.

*        CLEAR lt_zwm031.
*        READ TABLE lt_zwm031 WITH KEY lgnum = p_lgnum
*                                      matnr = lt_resto-matnr.
*        CHECK sy-subrc = 0.
          CLEAR qtd.
          qtd =  qtd_total + lt_resto-vsolm.

          IF qtd < lt_zwm031-unporpal.
            qtd_total = qtd_total + qtd.

            items-material = lt_resto-matnr.
            items-quantity = lt_resto-vsolm.
            items-unit     = lt_resto-meins.
            items-batch    = lt_resto-charg.
            COLLECT items.
            CLEAR items.

          ELSEIF qtd > lt_zwm031-unporpal.

            items-material = lt_resto-matnr.
            items-quantity = lt_resto-vsolm - ( qtd - lt_zwm031-unporpal ).
            items-unit     = lt_resto-meins.
            items-batch    = lt_resto-charg.
            COLLECT items.

            qtd_resto = lt_resto-vsolm - items-quantity.
            CLEAR items.

            items-material = lt_resto-matnr.
            items-quantity = qtd_resto.
            items-unit     = lt_resto-meins.
            items-batch    = lt_resto-charg.
            COLLECT items.
            CLEAR items.

            qtd_total = qtd_resto.

          ELSEIF qtd = lt_zwm031-unporpal.

            items-material = lt_resto-matnr.
            items-quantity = lt_resto-vsolm.
            items-unit     = lt_resto-meins.
            items-batch    = lt_resto-charg.
            COLLECT items.
            CLEAR items.

            CLEAR items2.
            REFRESH items2.
            LOOP AT items WHERE material = lt_zwm031-matnr.
              MOVE-CORRESPONDING items TO items2.
              APPEND items2.
            ENDLOOP.

            DELETE items WHERE material = lt_zwm031-matnr.

            CLEAR new_sscc.
            CALL FUNCTION 'ZWM_CREATE_HU'
              EXPORTING
                warehouse                  = p_lgnum
                plant                      = lv_werks
                s_loc                      = lv_lgort
                packing_material           = p_pmatn
              IMPORTING
                hukey                      = new_sscc
              TABLES
                return_msg                 = return_msg
                items                      = items2
              EXCEPTIONS
                empty_table                = 1
                reference_document_differs = 2
                empty_delivery_item        = 3
                item_not_found             = 4
                OTHERS                     = 5.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ELSE.
              MOVE p_lgnum       TO lt_zwm044-lgnum.
              MOVE p_refnr       TO lt_zwm044-refnr.
              MOVE lt_resto-vbeln  TO lt_zwm044-vbeln.
              MOVE new_sscc      TO lt_zwm044-exidv.
              MOVE 'F'           TO lt_zwm044-status.
              COLLECT lt_zwm044.

              INSERT zwm044 FROM TABLE lt_zwm044.
              COMMIT WORK.
              CLEAR lt_zwm044.
              REFRESH lt_zwm044.

            ENDIF.

          ELSEIF qtd = lt_zwm031-unporpal.

            items-material = lt_resto-matnr.
            items-quantity = lt_resto-vsolm.
            items-unit     = lt_resto-meins.
            items-batch    = lt_resto-charg.
            COLLECT items.
            CLEAR items.

**            CLEAR new_sscc.
**            CALL FUNCTION 'ZWM_CREATE_HU'
**              EXPORTING
**                warehouse                  = p_lgnum
**                plant                      = 'RENV'
**                s_loc                      = 'CD'
**                packing_material           = p_pmatn
**              IMPORTING
**                hukey                      = new_sscc
**              TABLES
**                return_msg                 = return_msg
**                items                      = items
**              EXCEPTIONS
**                empty_table                = 1
**                reference_document_differs = 2
**                empty_delivery_item        = 3
**                item_not_found             = 4
**                OTHERS                     = 5.
**            IF sy-subrc <> 0.
**              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**            ELSE.
**              MOVE p_lgnum       TO lt_zwm044-lgnum.
**              MOVE p_refnr       TO lt_zwm044-refnr.
**              MOVE lt_resto-vbeln  TO lt_zwm044-vbeln.
**              MOVE new_sscc      TO lt_zwm044-exidv.
**              MOVE 'F'           TO lt_zwm044-status.
**              COLLECT lt_zwm044.
**
**              INSERT zwm044 FROM TABLE lt_zwm044.
**              COMMIT WORK.
**              CLEAR lt_zwm044.
**              REFRESH lt_zwm044.
**              CLEAR items.
**              REFRESH items.
**
**
**            ENDIF.

            CLEAR qtd_total.

          ENDIF.
        ENDLOOP.

        IF NOT items[] IS INITIAL.

          CLEAR new_sscc.
          CALL FUNCTION 'ZWM_CREATE_HU'
            EXPORTING
              warehouse                  = p_lgnum
              plant                      = lv_werks
              s_loc                      = lv_lgort
              packing_material           = p_pmatn
            IMPORTING
              hukey                      = new_sscc
            TABLES
              return_msg                 = return_msg
              items                      = items
            EXCEPTIONS
              empty_table                = 1
              reference_document_differs = 2
              empty_delivery_item        = 3
              item_not_found             = 4
              OTHERS                     = 5.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            MOVE p_lgnum       TO lt_zwm044-lgnum.
            MOVE p_refnr       TO lt_zwm044-refnr.
            MOVE lt_resto-vbeln  TO lt_zwm044-vbeln.
            MOVE new_sscc      TO lt_zwm044-exidv.
            MOVE 'F'           TO lt_zwm044-status.
            COLLECT lt_zwm044.

            INSERT zwm044 FROM TABLE lt_zwm044.
            COMMIT WORK.
            CLEAR lt_zwm044.
            REFRESH lt_zwm044.
            CLEAR items.
            REFRESH items.
            CLEAR qtd_total.
          ENDIF.
        ENDIF.

        CLEAR qtd_total.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  CLEAR lt_zwm044.
  REFRESH lt_zwm044.

**  CLEAR items.
**  REFRESH items.

  SORT l_zwm026.
  DELETE ADJACENT DUPLICATES FROM l_zwm026.
  LOOP AT l_sscc.
    CLEAR: lv_werks, lv_lgort.
    LOOP AT l_zwm026 WHERE sscc = l_sscc-sscc.

      CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
        EXPORTING
          i_user    = sy-uname
          i_refnr   = l_zwm026-grupo
          i_vbeln   = l_zwm026-remessa
          i_recall  = 'X'
          i_usewm   = 'X'
          i_usemm   = 'X'
          i_useaut  = 'X'
        CHANGING
          c_lgnum   = p_lgnum
          c_werks   = lv_werks
          c_lgort   = lv_lgort
        EXCEPTIONS
          error     = 1
          user_back = 2
          OTHERS    = 3.

      IF lv_werks IS INITIAL.
        IF p_lgnum = '100'.
          lv_werks = 'RENV'.
        ELSEIF p_lgnum = '150'.
          lv_werks = 'RFRA'.
        ENDIF.
      ENDIF.

      items-material = l_zwm026-material.
      items-quantity = l_zwm026-quantidade.
      items-unit     = l_zwm026-unidade.
      items-batch    = l_zwm026-lote.

      " Obter Centro e Depósito da Remessa
      READ TABLE lt_lips WITH KEY vbeln = l_zwm026-remessa
                                  matnr = l_zwm026-material
                                  charg = l_zwm026-lote.
      IF sy-subrc <> 0.
        READ TABLE lt_lips WITH KEY vbeln = l_zwm026-remessa
                                    matnr = l_zwm026-material.
      ENDIF.

      IF sy-subrc = 0.
        lv_werks = lt_lips-werks.
        lv_lgort = lt_lips-lgort.
      ENDIF.

*      IF sy-subrc = 0.
*        items-werks = lt_lips-werks.
*        items-lgort = lt_lips-lgort.
*      ENDIF.

      COLLECT items.
      CLEAR items.

    ENDLOOP.

    CLEAR new_sscc.
    CALL FUNCTION 'ZWM_CREATE_HU'
      EXPORTING
        warehouse                  = p_lgnum
*       plant                      = 'RENV' " << DEL ROFF(SDF):TMGP:18.01.2016 10:25:51
        plant                      = lv_werks " << INS ROFF(SDF):TMGP:18.01.2016 10:25:50
        s_loc                      = lv_lgort
        packing_material           = p_pmatn
      IMPORTING
        hukey                      = new_sscc
      TABLES
        return_msg                 = return_msg
        items                      = items
      EXCEPTIONS
        empty_table                = 1
        reference_document_differs = 2
        empty_delivery_item        = 3
        item_not_found             = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MOVE p_lgnum           TO lt_zwm044-lgnum.
      MOVE p_refnr           TO lt_zwm044-refnr.
      MOVE l_zwm026-remessa  TO lt_zwm044-vbeln.
      MOVE new_sscc          TO lt_zwm044-exidv.
      MOVE 'F'               TO lt_zwm044-status.
      COLLECT lt_zwm044.

      INSERT zwm044 FROM TABLE lt_zwm044.
      COMMIT WORK.
      CLEAR lt_zwm044.
      REFRESH lt_zwm044.
      CLEAR items.
      REFRESH items.

    ENDIF.
  ENDLOOP.

  IF NOT items[] IS INITIAL.
    CLEAR new_sscc.
    CALL FUNCTION 'ZWM_CREATE_HU'
      EXPORTING
        warehouse                  = p_lgnum
        plant                      = lv_werks " << INS ROFF(SDF):TMGP:18.01.2016 10:25:50
*       plant                      = 'RENV'
        s_loc                      = lv_lgort
        packing_material           = p_pmatn
      IMPORTING
        hukey                      = new_sscc
      TABLES
        return_msg                 = return_msg
        items                      = items
      EXCEPTIONS
        empty_table                = 1
        reference_document_differs = 2
        empty_delivery_item        = 3
        item_not_found             = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MOVE p_lgnum       TO lt_zwm044-lgnum.
      MOVE p_refnr       TO lt_zwm044-refnr.
      MOVE lt_resto-vbeln  TO lt_zwm044-vbeln.
      MOVE new_sscc      TO lt_zwm044-exidv.
      MOVE 'F'           TO lt_zwm044-status.
      COLLECT lt_zwm044.

      INSERT zwm044 FROM TABLE lt_zwm044.
      COMMIT WORK.
      CLEAR lt_zwm044.
      REFRESH lt_zwm044.
      CLEAR items.
      REFRESH items.
      CLEAR qtd_total.
    ENDIF.
  ENDIF.
  CLEAR lt_zwm044.
  REFRESH lt_zwm044.
  SELECT * INTO TABLE lt_zwm044
      FROM zwm044
          WHERE lgnum = p_lgnum
            AND refnr = p_refnr.

** Validar Paletes Remontadas
**********************************************************************
  PERFORM check_pal_remontadas USING lt_zwm044[]
                                     lt_ltap[].

** Impressão de Etiquetas
**********************************************************************
  LOOP AT lt_t311a.

    CLEAR t_sscc.
    REFRESH t_sscc.
    IF lv_2step EQ 'X' AND
        lv_2spart EQ ' '.
      lv_benum = '9999999999'.
    ELSE.
      lv_benum = lt_t311a-rbnum.
    ENDIF.
    LOOP AT lt_zwm044 WHERE vbeln = lv_benum AND status = 'F'.
      t_sscc-sscc = lt_zwm044-exidv.
      APPEND t_sscc.
    ENDLOOP.

    CALL FUNCTION 'ZWM_IMPRIME_EAN128'
      EXPORTING
        printer                  = p_print
        lbltype                  = 'L0'
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

  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  CHECK_PAL_REMONTADAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZWM044[]  text
*----------------------------------------------------------------------*
FORM check_pal_remontadas USING pt_zwm044 TYPE ANY TABLE
                                pt_ltap   TYPE ANY TABLE.

  DATA: lv_kunnr    TYPE kunnr.
  DATA: ls_mlgn     TYPE mlgn.

  DATA: lt_vekp     TYPE TABLE OF vekp   WITH HEADER LINE.
  DATA: lt_vepo     TYPE TABLE OF vepo   WITH HEADER LINE.
  DATA: lt_zwm044   TYPE TABLE OF zwm044 WITH HEADER LINE.
  DATA: lt_zwm044_h TYPE TABLE OF zwm044 WITH HEADER LINE.
  DATA: lt_zwm031   TYPE TABLE OF zwm031 WITH HEADER LINE.
  DATA: lt_zwm020   TYPE TABLE OF zwm020 WITH HEADER LINE.
  DATA: lt_ltap     TYPE TABLE OF ltap   WITH HEADER LINE.

  DATA: lv_vbeln TYPE vbeln.

** Check paletes remontadas
**********************************************************************
  lt_zwm044[]   = pt_zwm044[].
  lt_zwm044_h[] = pt_zwm044[].
  lt_ltap[]     = pt_ltap[].

  SORT lt_zwm044_h BY vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_zwm044_h COMPARING vbeln.

  LOOP AT lt_zwm044_h.


    IF lv_2step EQ 'X' AND
       lv_2spart EQ ' '.
      SELECT SINGLE rbnum FROM t311a
                          INTO lv_vbeln
                          WHERE lgnum = p_lgnum AND
                                refnr = lt_zwm044_h-refnr.
    ELSE.
      lv_vbeln = lt_zwm044_h-vbeln.
    ENDIF.

    CLEAR lv_kunnr.
    SELECT SINGLE kunnr INTO lv_kunnr
        FROM vbpa
            WHERE vbeln EQ lt_zwm044_h-vbeln
              AND posnr = '000000'
              AND parvw EQ 'W1'.

    IF sy-subrc NE 0.
      SELECT SINGLE kunnr INTO lv_kunnr
          FROM vbpa
              WHERE vbeln EQ lt_zwm044_h-vbeln
                AND posnr = '000000'
                AND parvw EQ 'WE'.
    ENDIF.

    CLEAR:   lt_vekp, lt_vepo, lt_zwm020.
    REFRESH: lt_vekp, lt_vepo, lt_zwm020.

    LOOP AT lt_zwm044 WHERE vbeln = lt_zwm044_h-vbeln AND status = 'F'.
      lt_vekp-exidv = lt_zwm044-exidv.
      APPEND lt_vekp.
    ENDLOOP.

    IF lt_vekp[] IS NOT INITIAL.
      SELECT *
        FROM vekp INTO TABLE lt_vekp
        FOR ALL ENTRIES IN lt_vekp
        WHERE exidv = lt_vekp-exidv.
    ENDIF.

    IF lt_vekp[] IS NOT INITIAL.
      SELECT *
        FROM vepo INTO TABLE lt_vepo
        FOR ALL ENTRIES IN lt_vekp
        WHERE venum = lt_vekp-venum.
    ENDIF.

    IF lt_vepo[] IS NOT INITIAL.
      SELECT *
        FROM zwm031 INTO TABLE lt_zwm031
        FOR ALL ENTRIES IN lt_vepo
        WHERE lgnum = p_lgnum
        AND   kunnr = lv_kunnr
        AND   matnr = lt_vepo-matnr.
    ENDIF.

    SORT lt_zwm031 BY matnr.

** Validar Palete remontadas
    LOOP AT lt_vekp.

      CLEAR lt_vepo.
      READ TABLE lt_vepo INDEX 1.

      CLEAR lt_zwm031.
      READ TABLE lt_zwm031 WITH KEY matnr = lt_vepo-matnr.

      IF lt_zwm031-remontada IS NOT INITIAL.

        IF lt_zwm020-p1 IS INITIAL.
          lt_zwm020-p1 = lt_vekp-exidv.
        ELSEIF lt_zwm020-p2 IS INITIAL.
          lt_zwm020-p2 = lt_vekp-exidv.
        ENDIF.

      ENDIF.

      IF lt_zwm020-p1 IS NOT INITIAL AND
         lt_zwm020-p2 IS NOT INITIAL.

        lt_zwm020-armazem = p_lgnum.

        APPEND lt_zwm020.
        CLEAR lt_zwm020.
      ENDIF.
    ENDLOOP.

** Validar Meias Paletes
    CLEAR lt_zwm020.

    LOOP AT lt_ltap WHERE vbeln = lt_zwm044_h-vbeln.

      CLEAR ls_mlgn.
      SELECT SINGLE *
        FROM mlgn INTO ls_mlgn
        WHERE matnr = lt_ltap-matnr
        AND   lgnum = lt_ltap-lgnum.

*      IF ls_mlgn-lety1 = 'P2' OR
*         ls_mlgn-lety1 = 'P5'.

      IF z_wm_cl_management=>is_remontada( is_data = ls_mlgn ) EQ abap_true.

        IF lt_zwm020-p1 IS INITIAL.
          lt_zwm020-p1 = lt_ltap-vlenr.
        ELSEIF lt_zwm020-p2 IS INITIAL.
          lt_zwm020-p2 = lt_ltap-vlenr.
        ENDIF.

      ENDIF.

      IF lt_zwm020-p1 IS NOT INITIAL AND
         lt_zwm020-p2 IS NOT INITIAL.

        lt_zwm020-armazem = p_lgnum.

        APPEND lt_zwm020.
        CLEAR lt_zwm020.
      ENDIF.

    ENDLOOP.

    " Guardar Paletes Remontadas
    IF lt_zwm020[] IS NOT INITIAL.
      MODIFY zwm020 FROM TABLE lt_zwm020.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHECK_PAL_REMONTADAS
