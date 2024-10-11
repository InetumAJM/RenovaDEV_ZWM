FUNCTION z_wmfr_create_picking_to.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"     REFERENCE(E_SUBRC) TYPE  SYSUBRC
*"     REFERENCE(E_TANUM) TYPE  TANUM
*"----------------------------------------------------------------------

  DATA : BEGIN OF sscc OCCURS 0.
          INCLUDE STRUCTURE zwm_sscc.
  DATA : END OF sscc.

  DATA lt_messages TYPE tab_bdcmsgcoll.

  DATA ls_lqua TYPE lqua.
  DATA ls_ltak TYPE ltak.
  DATA ls_ltap TYPE ltap.
  DATA ls_mlgn TYPE mlgn.
  DATA ls_mlgt TYPE mlgt.

  DATA adicional   LIKE ltak-lznum.
  DATA bin_o       LIKE lagp-lgpla.
  DATA certificado LIKE ltap-zeugn.
  DATA lgort       LIKE ltap-lgort.
  DATA lv_subrc    TYPE sysubrc.
  DATA lv_tanum    TYPE tanum.
  DATA movimento   LIKE ltak-bwlvs.
  DATA plant       LIKE ltap-werks.
  DATA st_type     LIKE ltap-vltyp.
  DATA st_type_d   LIKE ltap-vltyp.
  DATA st_type_o   LIKE ltap-vltyp.
  DATA valor       LIKE zwm001-valor.

  WAIT UP TO 1 SECONDS.

** Storage type associado ao pulmão de reposição
  CLEAR valor.
  PERFORM f_get_parameter
    USING i_lgnum
          'ENTRADA_ARMAZEM'
          'ST_REP'
    CHANGING valor.
  WRITE valor TO st_type LEFT-JUSTIFIED.

** Movimento para TO de picking
  CLEAR valor.
  PERFORM f_get_parameter
    USING i_lgnum
         'PICKING'
         'MOV_WM'
    CHANGING valor.
  WRITE valor TO movimento LEFT-JUSTIFIED.

** dados de item
  CLEAR ls_ltap.
  SELECT *
    FROM ltap
    INTO ls_ltap
    WHERE lgnum EQ i_lgnum
      AND tanum EQ i_tanum.

    IF ls_ltap-nltyp EQ st_type.
      st_type_o = ls_ltap-nltyp.
      bin_o = ls_ltap-nlpla.
      plant = ls_ltap-werks.
      lgort = ls_ltap-lgort.

      SELECT SINGLE lzone
        FROM lagp
        INTO certificado
        WHERE lgnum EQ i_lgnum
          AND lgtyp EQ ls_ltap-nltyp
          AND lgpla EQ ls_ltap-nlpla.

      sscc-sscc = ls_ltap-nlenr.
      sscc-tipo_su = ls_ltap-letyp.
      sscc-material = ls_ltap-matnr.
      CLEAR sscc-variante.
      sscc-quantidade = ls_ltap-vsola.
      sscc-uni = ls_ltap-altme.
      CLEAR sscc-altura.
      sscc-lote_producao = ls_ltap-charg.
      APPEND sscc.

    ELSE.
      EXIT.
    ENDIF.
  ENDSELECT.

  CLEAR st_type_d.
  IF NOT sscc[] IS INITIAL.
    SELECT SINGLE *
      FROM ltak
      INTO ls_ltak
      WHERE lgnum EQ ls_ltap-lgnum
        AND tanum EQ ls_ltap-tanum.

    IF ls_ltak-betyp NE 'Z'. " Sem referencia a um Grupo
** Movimento para TO de picking
      CLEAR valor.
      PERFORM f_get_parameter
        USING i_lgnum
              'PICKING'
              'MOV_WM'
        CHANGING valor.
      WRITE valor TO movimento LEFT-JUSTIFIED. "919

** Verificar se tem quantidade no PCK
      SELECT SINGLE *
        FROM mlgt
        INTO ls_mlgt
        WHERE matnr EQ ls_ltap-matnr
          AND lgnum EQ ls_ltap-lgnum
          AND lgtyp EQ 'PCK'.

      IF NOT ls_mlgt-lgpla IS INITIAL.
        SELECT SINGLE *
          FROM lqua
          INTO ls_lqua
          WHERE lgnum EQ ls_mlgt-lgnum
            AND matnr EQ ls_mlgt-matnr
            AND lgtyp EQ ls_mlgt-lgtyp
            AND lgpla EQ ls_mlgt-lgpla.
        IF sy-subrc EQ 0.
          st_type_d = 'PKR'.
        ELSE.
          st_type_d = ls_mlgt-lgtyp.
        ENDIF.
      ELSE.
        SELECT SINGLE *
          FROM mlgn
          INTO ls_mlgn
          WHERE matnr EQ ls_ltap-matnr
            AND lgnum EQ ls_ltap-lgnum.

        IF ls_mlgn-plkpt EQ 'PKB'.
** Movimento para TO de reabastecimento picking variavel
          CLEAR valor.
          PERFORM f_get_parameter
            USING i_lgnum
                  'PICKING_REAB_VA'
                  'MOV_WM1'
            CHANGING valor.
          WRITE valor TO movimento LEFT-JUSTIFIED. "828
        ENDIF.
      ENDIF.
    ELSE.   " com referencia a um grupo
      CLEAR adicional.
      adicional = ls_ltak-lznum.

      SELECT SINGLE *
        FROM mlgn
        INTO ls_mlgn
        WHERE matnr EQ ls_ltap-matnr
          AND lgnum EQ ls_ltap-lgnum.

      IF ls_mlgn-plkpt EQ 'PKB'.
** Movimento para TO de reabastecimento picking variavel
        CLEAR: valor, movimento.
        PERFORM f_get_parameter
          USING i_lgnum
                'PICKING_REAB_VA'
                'MOV_WM'
          CHANGING valor.
        WRITE valor TO movimento LEFT-JUSTIFIED. "977
      ELSE.
** Movimento para TO de reabastecimento picking fixo
        CLEAR: valor, movimento.
        PERFORM f_get_parameter
          USING i_lgnum
                'PICKING_REAB'
                'MOV_WM'
          CHANGING valor.
        WRITE valor TO movimento LEFT-JUSTIFIED. "973
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse      = i_lgnum
        mov_type       = movimento
        st_type_o      = st_type_o
        bin_origem     = bin_o
        st_type_d      = st_type_d
        plant          = plant
        s_loc          = lgort
        certificado    = certificado
        origem         = 'X'
        req_number     = ls_ltak-benum
        req_type       = ls_ltak-betyp
        sscc_adicional = adicional
      IMPORTING
        to             = lv_tanum
      TABLES
        return_msg     = lt_messages
        sscc           = sscc
      EXCEPTIONS
        error          = 1
        OTHERS         = 2.
    IF sy-subrc NE 0.
      lv_subrc = sy-subrc.
    ELSE.
      IF NOT ls_ltak-tapri IS INITIAL.
        CALL FUNCTION 'ZWM_TAPRI_CHANGE'
          EXPORTING
            i_lgnum     = i_lgnum
            i_tanum     = e_tanum
            i_tapri     = ls_ltak-tapri
            i_commit    = abap_true
          IMPORTING
            et_messages = lt_messages
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
        IF sy-subrc NE 0.
          lv_subrc = sy-subrc.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  et_messages[] = lt_messages.
  e_subrc = lv_subrc.
  e_tanum = lv_tanum.
ENDFUNCTION.
