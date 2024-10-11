************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0028                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Reabastecimento da Reserva de Picking                    *
* Criado por: Luís Rocha                                               *
* Criado em.: 02/12/2004                                               *
* Tipo PRG..: Report                                                   *
************************************************************************

REPORT zwmrep0028 LINE-COUNT 100.

TABLES: mlgt,
        mara,
        lqua,
        ltak,
        ltap,
        t300,
        zwm001,
        mlgn,
        zwm020,
        mard.

DATA: gt_mlgt LIKE mlgt OCCURS 0 WITH HEADER LINE,
      gt_lqua LIKE lqua OCCURS 0 WITH HEADER LINE,
      gt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE,
      t_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE,
      t_ltap_cancl LIKE ltap_cancl OCCURS 0 WITH HEADER LINE.

DATA: gt_return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: gt_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

DATA: g_werks  LIKE ltap-werks,
      g_lgort  LIKE ltap-lgort,
      g_tanum  LIKE ltak-tanum,
      g_lgtyp1 LIKE mlgt-lgtyp,
      g_lgtyp2 LIKE lqua-lgtyp,
      g_bwlvs  LIKE ltak-bwlvs,
      num_pal_reab_v TYPE i,
      num_pal_reab_f TYPE i.

DATA: g_num   LIKE sy-tabix,
      g_lines LIKE sy-tabix,
      n_pal TYPE i,
      to LIKE ltap-tanum.

DATA n_posi TYPE i.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum LIKE mlgn-lgnum OBLIGATORY MEMORY ID lgn.
SELECT-OPTIONS: s_matnr FOR mlgt-matnr.
SELECTION-SCREEN END OF BLOCK b1.

* Initialization
INITIALIZATION.
  DATA: xuser LIKE lrf_wkqu OCCURS 0 WITH HEADER LINE.
* Read the user data from table lrf_wkqu ( for all the warehouses)
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = xuser
    EXCEPTIONS
      no_entry_found = 01.
  IF sy-subrc = 0.
    READ TABLE xuser INDEX 1.
    MOVE xuser-lgnum TO p_lgnum.
  ENDIF.

* Start-of-Selection
START-OF-SELECTION.
  DATA lv_werks TYPE werks_d. " << INS ROFF(SDF):TMGP:30.12.2015 17:34:54

  DATA lt_lgtyp TYPE mawm_int_tt_lgtyp. " << INS ROFF(SDF):TMGP:30.12.2015 13:47:52

  DATA lr_lgtyp TYPE RANGE OF lgtyp. " << INS ROFF(SDF):TMGP:30.12.2015 13:47:52

  FIELD-SYMBOLS <fs_lgtyp>  LIKE LINE OF lt_lgtyp[]. " << INS ROFF(SDF):TMGP:30.12.2015 13:47:52
  FIELD-SYMBOLS <fs_rlgty>  LIKE LINE OF lr_lgtyp[]. " << INS ROFF(SDF):TMGP:30.12.2015 13:47:52

* Valida parametros de entrada
  PERFORM valida_parametros_entrada.
  IF sy-subrc <> 0.
*   ERRO: Nº Depósito Inválido !
    MESSAGE s146(zwmmsg001).
    EXIT.
  ENDIF.

* Ler parametros
  PERFORM ler_parametros.

* Processo
  SELECT * FROM mlgt INTO TABLE gt_mlgt
          WHERE matnr IN s_matnr
            AND lgnum = p_lgnum
            AND lgtyp = g_lgtyp1                   "PCK ou
            AND lgpla <> ' '.

  CHECK NOT gt_mlgt[] IS INITIAL.

  LOOP AT gt_mlgt.





    CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
     EXPORTING
       i_user              = sy-uname
       i_matnr             = gt_mlgt-matnr
       i_recall            = 'X'
       i_usewm             = 'X'
       i_userf             = 'X'
       i_usemm             = 'X'
       i_useaut            = 'X'
       i_get_lgnum         = 'X'
       i_get_werks         = 'X'
       i_get_lgort         = 'X'
*   IMPORTING
*     ET_MESSAGES         =
     CHANGING
       c_lgnum             = p_lgnum
       c_werks             = g_werks
       c_lgort             = g_lgort
     EXCEPTIONS
       error               = 1
       user_back           = 2
       OTHERS              = 3.


    CLEAR n_posi.

    SELECT COUNT(*) FROM lagp INTO n_posi
        WHERE lgnum = p_lgnum AND
              lgtyp = 'REP' AND
              kzler = 'X' AND
              kzvol = ' ' AND
              anzqu = 0   AND
              skzue = ' ' AND
              skzsi = ' ' AND
              skzse = ' '.


    IF n_posi <= 6.
      EXIT.
    ENDIF.

    REFRESH: gt_lqua, gt_ltap.
    SELECT * FROM lqua INTO TABLE gt_lqua
            WHERE lgnum = p_lgnum
              AND ( lgtyp = g_lgtyp2 OR lgtyp = 'PRB' )     "PKR ou PRB
              AND matnr = gt_mlgt-matnr.

    DESCRIBE TABLE gt_lqua LINES g_lines.

    CLEAR mlgn.
    SELECT SINGLE *
        FROM mlgn
            WHERE matnr = gt_mlgt-matnr AND
                  lgnum = p_lgnum.

    IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.
      num_pal_reab_v = num_pal_reab_v * 2.
      num_pal_reab_f = num_pal_reab_f * 2.
    ENDIF.

    IF mlgn-plkpt = 'PKB'.
      CHECK g_lines < num_pal_reab_v.
    ELSE.
      CHECK g_lines < num_pal_reab_f.
    ENDIF.

    SELECT * FROM ltap INTO TABLE gt_ltap
                  WHERE lgnum = p_lgnum
                    AND matnr = gt_mlgt-matnr
                    AND pquit = ' '
                    AND werks = g_werks.
    IF NOT gt_ltap[] IS INITIAL.
      LOOP AT gt_ltap.
        CLEAR ltak.
        SELECT SINGLE * FROM  ltak
               WHERE lgnum = gt_ltap-lgnum
                AND tanum = gt_ltap-tanum.

        IF sy-subrc = 0 AND ltak-bwlvs = g_bwlvs
                        AND ltak-kquit = ' '.
          g_lines = g_lines + 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF mlgn-plkpt = 'PKB'.
      CHECK g_lines < num_pal_reab_v.
      g_num = num_pal_reab_v - g_lines.
    ELSE.
      CHECK g_lines < num_pal_reab_f.
      g_num = num_pal_reab_f - g_lines.
    ENDIF.

*& Begin of Modification by Tiago Pateiro - ROFF @ 30.12.2015 17:33:02
*/ Adaptar determinação do centro para França
    SELECT werks UP TO 1 ROWS
      FROM t320 INTO lv_werks
      WHERE lgort EQ 'CD'
        AND lgnum EQ p_lgnum.
    ENDSELECT.
    IF sy-subrc NE 0.
      lv_werks = 'RENV'.
    ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 30.12.2015 17:33:11

    SELECT SINGLE *
        FROM mard
            WHERE matnr = gt_mlgt-matnr AND
                  werks = lv_werks AND " << INS ROFF(SDF):TMGP:30.12.2015 17:35:09
                  lgort = 'CD'.

    CHECK NOT mard-labst IS INITIAL.

    DATA lt_lqua LIKE lqua OCCURS 0 WITH HEADER LINE.
    DATA num_quantos TYPE i.

    CLEAR num_quantos.

*& Begin of Modification by Tiago Pateiro - ROFF @ 30.12.2015 17:35:46
*/ Adaptar logica para contemplar Renova França
    CALL FUNCTION 'Z_WM_T334T_GET_LGTYP'
      EXPORTING
        i_lgnum  = p_lgnum
        i_bwref  = g_bwlvs
        i_matnr  = gt_mlgt-matnr
      IMPORTING
        et_lgtyp = lt_lgtyp[].
    IF lt_lgtyp[] IS NOT INITIAL.
      LOOP AT lt_lgtyp[] ASSIGNING <fs_lgtyp>.
        APPEND INITIAL LINE TO lr_lgtyp[] ASSIGNING <fs_rlgty>.
        <fs_rlgty>-sign   = 'I'.
        <fs_rlgty>-option = 'EQ'.
        <fs_rlgty>-low    = <fs_lgtyp>.
      ENDLOOP.

      SELECT COUNT(*)
        FROM lqua INTO num_quantos
        WHERE matnr EQ gt_mlgt-matnr
          AND lgnum EQ p_lgnum
          AND lgtyp IN lr_lgtyp[] " partial index
          AND verme GT 0.
    ELSE.
*& End of Modification by Tiago Pateiro - ROFF @ 30.12.2015 17:35:47
      SELECT COUNT(*) FROM lqua INTO num_quantos
              WHERE lgnum = p_lgnum AND
                    matnr = gt_mlgt-matnr AND
                    ( lgtyp = 'TRI' OR
                      lgtyp = 'DRI' OR lgtyp = 'BLK' ) AND
                      verme > 0.
*& Begin of Modification by Tiago Pateiro - ROFF @ 30.12.2015 17:36:21
*/ Adaptar logica para contemplar Renova França
    ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 30.12.2015 17:36:21

    CHECK g_num <= num_quantos.

    DO g_num TIMES.
*   Cria TO
      REFRESH: gt_return_msg, gt_sscc.
      CLEAR: gt_return_msg, gt_sscc.
      CLEAR g_tanum.

      SELECT SINGLE * FROM mara WHERE matnr = gt_mlgt-matnr.
      CHECK sy-subrc = 0.

      gt_sscc-material   = gt_mlgt-matnr.
      gt_sscc-quantidade = 1.

      SELECT SINGLE *
          FROM mlgn
              WHERE lgnum = p_lgnum AND matnr =  gt_mlgt-matnr.

      gt_sscc-tipo_su = mlgn-lety1.

      IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.
        gt_sscc-quantidade = 1.
        gt_sscc-uni        = 'PAL'.
        APPEND gt_sscc.
        gt_sscc-quantidade = 1.
        gt_sscc-uni        = 'PAL'.
      ELSE.
        gt_sscc-uni        = mara-meins.
      ENDIF.

      APPEND gt_sscc.

      CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse  = p_lgnum
          mov_type   = g_bwlvs                  "827
          plant      = g_werks                  "RENV
          s_loc      = g_lgort                  "CD
        IMPORTING
          to         = g_tanum
        TABLES
          return_msg = gt_return_msg
          sscc       = gt_sscc
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.

      IF sy-msgty EQ 'S'.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*** Verificar se é remontada
      DATA t_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.
      DATA c_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.
      CLEAR: t_ltap, c_ltap.
      REFRESH: t_ltap, c_ltap.

      SELECT * APPENDING TABLE t_ltap
          FROM ltap
              WHERE lgnum = p_lgnum AND
                    tanum = g_tanum.

      LOOP AT t_ltap
          WHERE vltyp = 'TRI' AND
                ( letyp in z_wm_cl_management=>r_letyp_remontada( p_lgnum ) ) AND
                vbeln = ' '.

        CLEAR zwm020.
        SELECT SINGLE * FROM zwm020
            WHERE armazem = t_ltap-lgnum AND
                  ( p1 = t_ltap-vlenr OR
                    p2 = t_ltap-vlenr ).

        IF sy-subrc = 0.
          CLEAR c_ltap.
          REFRESH c_ltap.
          MOVE-CORRESPONDING t_ltap TO c_ltap.
          APPEND c_ltap.
          CLEAR c_ltap.

          IF zwm020-p1 = t_ltap-vlenr.
** Verificar se existem duas to´s para as duas paletes remontadas

** Se tiverem as duas paletes tem de se estornar as duas tos e voltar a
** criar as duas to's sendo a primeira a da to de baixo

            READ TABLE t_ltap WITH KEY vlenr = zwm020-p2.
            IF sy-subrc = 0.
              MOVE-CORRESPONDING t_ltap TO c_ltap.
              APPEND c_ltap.
              CLEAR c_ltap.
            ENDIF.

          ELSEIF zwm020-p2 = t_ltap-vlenr.

            READ TABLE t_ltap WITH KEY vlenr = zwm020-p1.
            IF sy-subrc = 0.
              MOVE-CORRESPONDING t_ltap TO c_ltap.
              APPEND c_ltap.
              CLEAR c_ltap.
            ENDIF.
          ENDIF.
        ENDIF.

** Verificar se estao as duas paletes remontadas para a saida.
** Se for as duas paletes para saida verificar se ja estao criadas pela
** ordem correcta
        CLEAR n_pal.

        DESCRIBE TABLE c_ltap LINES n_pal.

        IF n_pal = 2.
          SORT c_ltap BY lgnum tanum tapos.
          READ TABLE c_ltap INDEX 1.
          IF c_ltap-vlenr = zwm020-p1.
            CONTINUE.
          ENDIF.
        ENDIF.

        CLEAR t_sscc.
        REFRESH t_sscc.
        LOOP AT c_ltap.

**************************
**   Cancelar as OT
**************************

          CLEAR t_ltap_cancl.
          REFRESH t_ltap_cancl.

          t_ltap_cancl-tanum = c_ltap-tanum.
          t_ltap_cancl-tapos = c_ltap-tapos.
          APPEND t_ltap_cancl.
          CLEAR t_ltap_cancl.

          CALL FUNCTION 'ZWM_CANCEL_TO'
            EXPORTING
              armazem      = c_ltap-lgnum
            TABLES
              t_ltap_cancl = t_ltap_cancl
            EXCEPTIONS
              error        = 1
              OTHERS       = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDLOOP.

**************************
**   Tabela para Criar Novas OT´s
**************************
        break roffd.
        READ TABLE c_ltap WITH KEY vlenr = zwm020-p1.
        IF sy-subrc = 0.
          t_sscc-sscc = c_ltap-vlenr.
          t_sscc-tipo_su = c_ltap-letyp.
          t_sscc-material = c_ltap-matnr.
          t_sscc-quantidade = c_ltap-vsola.
          t_sscc-uni = c_ltap-altme.
          t_sscc-lote_producao = c_ltap-charg.
          APPEND t_sscc.
          CLEAR t_sscc.
        ENDIF.

        CLEAR c_ltap.
        READ TABLE c_ltap WITH KEY vlenr = zwm020-p2.
        IF sy-subrc = 0.
          t_sscc-sscc = c_ltap-vlenr.
          t_sscc-tipo_su = c_ltap-letyp.
          t_sscc-material = c_ltap-matnr.
          t_sscc-quantidade = c_ltap-vsola.
          t_sscc-uni = c_ltap-altme.
          t_sscc-lote_producao = c_ltap-charg.
          APPEND t_sscc.
          CLEAR t_sscc.
        ENDIF.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse   = p_lgnum
            mov_type    = '827'
            st_type_o   = c_ltap-vltyp
            bin_origem  = c_ltap-vlpla
            st_type_d   = c_ltap-nltyp
            bin_destino = c_ltap-nlpla
            plant       = c_ltap-werks
            s_loc       = c_ltap-lgort
            origem      = 'X'
          IMPORTING
            to          = to
          TABLES
            return_msg  = gt_return_msg
            sscc        = t_sscc
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.

*        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.

        IF sy-msgty EQ 'S'.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CLEAR t_sscc.
        REFRESH t_sscc.

*    ENDLOOP.

        DELETE t_ltap WHERE vlenr = zwm020-p1 OR vlenr = zwm020-p2.
        CLEAR c_ltap.
        REFRESH c_ltap.

      ENDLOOP.


***************************
    ENDDO.
  ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  valida_parametros_entrada
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_parametros_entrada .

  SELECT SINGLE * FROM t300
                    WHERE
                      lgnum = p_lgnum.
ENDFORM.                    "valida_parametros_entrada


*&--------------------------------------------------------------------*
*&      Form  ler_parametros
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM ler_parametros.
  CLEAR: g_werks, g_lgort, g_lgtyp1, g_lgtyp2, g_bwlvs.

**  SELECT SINGLE * FROM  zwm001
**         WHERE armazem   = p_lgnum
**           AND processo  = 'GERAL'
**           AND parametro = 'PLANT'.
**  IF sy-subrc = 0.
**    g_werks = zwm001-valor.
**  ENDIF.
**  SELECT SINGLE * FROM  zwm001
**         WHERE armazem   = p_lgnum
**           AND processo  = 'GERAL'
**           AND parametro = 'LGORT'.
**  IF sy-subrc = 0.
**    g_lgort = zwm001-valor.
**  ENDIF.
  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = p_lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'MOV'.
  IF sy-subrc = 0.
    g_bwlvs = zwm001-valor.
  ENDIF.
  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = p_lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'ST_PCK'.
  IF sy-subrc = 0.
    g_lgtyp1 = zwm001-valor.
  ENDIF.
  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = p_lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'ST_PKR'.
  IF sy-subrc = 0.
    g_lgtyp2 = zwm001-valor.
  ENDIF.


  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = p_lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'PKB'.
  IF sy-subrc = 0.
    num_pal_reab_v = zwm001-valor.
  ENDIF.


  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = p_lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'PKR'.
  IF sy-subrc = 0.
    num_pal_reab_f = zwm001-valor.
  ENDIF.


ENDFORM.                    "ler_parametros
