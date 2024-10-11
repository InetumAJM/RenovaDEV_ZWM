FUNCTION zwm_create_to_chd_wcs_in .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LZNUM) TYPE  LZNUM OPTIONAL
*"     REFERENCE(I_LINHA) TYPE  FEVOR OPTIONAL
*"     REFERENCE(I_ZEUGN) TYPE  LVS_ZEUGN OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_TO_DUMMY) TYPE  TANUM
*"  TABLES
*"      T_RETURN STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  CONSTANTS: lc_object    TYPE balhdr-object    VALUE 'ZWCS'.
  CONSTANTS: lc_subobject TYPE balhdr-subobject VALUE 'ZWCS_01'.

  DATA: lv_sscc          TYPE exidv.
  DATA: lv_tabix         TYPE sy-tabix.
  DATA: lv_sscc2         TYPE exidv.
  DATA: lv_matnr         TYPE matnr.
  DATA: lv_maabc         TYPE maabc.
  DATA: lv_lznum         TYPE lznum.
  DATA: lv_to_dummy      TYPE tanum.
  DATA: lv_pick_aut_ocup TYPE numc2.
  DATA: lv_cap_max_aut   TYPE int4.
  DATA: lv_cap_ocup      TYPE int4.
  DATA: lv_cap           TYPE menge_d.
  DATA: lv_umrez         TYPE umrez.
  DATA: lv_subobject     TYPE balhdr-subobject.
  DATA: ls_vekp          TYPE vekp.
  DATA: ls_zwm009        TYPE zwm009.
  DATA: ls_mlgt          TYPE mlgt.
  DATA: lt_vepo          TYPE TABLE OF vepo   WITH HEADER LINE.
  DATA: lt_lqua          TYPE TABLE OF lqua   WITH HEADER LINE.
  DATA: lt_zwm020        TYPE TABLE OF zwm020 WITH HEADER LINE.

** Obter dados
**********************************************************************
  lv_sscc = i_lznum.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = lv_sscc
*    IMPORTING
*      output = lv_sscc.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = lv_sscc
*    IMPORTING
*      output = lv_sscc2.

** Aviso de Entrada no WCS
*  IF i_linha IS NOT INITIAL.
*    CONCATENATE i_linha '#' lv_sscc INTO lv_lznum.
*  ENDIF.

** Parâmetros
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'WCS'
      i_parametro = 'PICOS_AUTO_OCUP'
    IMPORTING
      e_valor     = lv_pick_aut_ocup
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'WCS'
      i_parametro = 'CAP_MAX_AUTO_OCUP'
    IMPORTING
      e_valor     = lv_cap_max_aut
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

** Aviso de Entrada Produção
**********************************************************************
  lv_subobject = lc_subobject.

  " Linha
  IF i_linha IS NOT INITIAL.
    CLEAR t_return.
    t_return-msgtyp = 'S'.
    t_return-msgid  = 'ZWMMSG001'.
    t_return-msgnr  = '000'.
    t_return-msgv1  = 'Linha'.
    t_return-msgv2  = i_linha.
    APPEND t_return.
  ENDIF.

  " Obter SSCC
  CLEAR t_return.
  t_return-msgtyp = 'S'.
  t_return-msgid  = 'ZWMMSG001'.
  t_return-msgnr  = '000'.
  t_return-msgv1  = 'SSCC'.
  t_return-msgv2  = lv_sscc.
  APPEND t_return.

** Obter dados da HU
  SELECT SINGLE *
    FROM vekp INTO ls_vekp
    WHERE exidv = lv_sscc.

  IF sy-subrc = 0.
    SELECT *
      FROM vepo INTO TABLE lt_vepo
      WHERE venum = ls_vekp-venum.
  ENDIF.

** Validar Rotação Material para obter ponto destino do WCS
  READ TABLE lt_vepo INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE maabc
      FROM marc INTO lv_maabc
      WHERE matnr = lt_vepo-matnr
      AND   werks = lt_vepo-werks.
  ENDIF.

** Validar se é uma palete para o picking
  SELECT SINGLE umrez
    FROM marm INTO lv_umrez
    WHERE matnr EQ lt_vepo-matnr
    AND   meinh EQ 'PAL'.

  IF lt_vepo-vemng < lv_umrez.

    SELECT SINGLE *
      FROM mlgt INTO ls_mlgt
      WHERE matnr = lt_vepo-matnr
      AND   lgnum = i_lgnum
      AND   lgtyp = 'PCK'.

    " Com posição de picking definida - Saida no manual
    IF ls_mlgt-lgpla IS NOT INITIAL.
      lv_maabc = 'Z'.

    ELSE.

      " Automático
      IF ls_zwm009-wcs_point = 'AUT'.

        " Validar % de ocupação do Automático
        SELECT *
          FROM lqua INTO TABLE lt_lqua
          WHERE lgnum = i_lgnum
          AND   lgtyp = 'AUT'
          AND   lgpla = 'AUT'.

        DELETE lt_lqua WHERE lenum IS INITIAL.

        SORT lt_lqua BY lenum.
        DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING lenum.

        " Retirar paletes remontadas
        IF lt_lqua[] IS NOT INITIAL.
          SELECT *
            FROM zwm020 INTO TABLE lt_zwm020
            FOR ALL ENTRIES IN lt_lqua
            WHERE armazem = i_lgnum
            AND   p2      = lt_lqua-lenum.
        ENDIF.

        IF lt_zwm020[] IS NOT INITIAL.
          LOOP AT lt_lqua.
            lv_tabix = sy-tabix.

            READ TABLE lt_zwm020 WITH KEY p2 = lt_lqua-lenum.
            IF sy-subrc = 0.
              DELETE lt_lqua INDEX lv_tabix.
            ENDIF.
          ENDLOOP.
        ENDIF.

        DESCRIBE TABLE lt_lqua LINES lv_cap_ocup.

        lv_cap = ( lv_cap_ocup / lv_cap_max_aut ) * 100.

        " Excedeu a % de capacidade para arrumar paletes de picking
        IF lv_cap >= lv_pick_aut_ocup.
          lv_maabc = 'Z'. " Saída Manual
        ENDIF.

      ELSE.
        lv_maabc = 'Z'. " Saída Manual
      ENDIF.
    ENDIF.
  ENDIF.

** Rotação do material
  SELECT SINGLE *
    FROM zwm009 INTO ls_zwm009
    WHERE abc = lv_maabc.

  IF ls_zwm009-wcs_point IS INITIAL.

    lv_matnr = lt_vepo-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = lv_matnr
      IMPORTING
        output = lv_matnr.

    " Rotação de Material & sem Ponto WCS definido (ZWM010).
    CLEAR t_return.
    t_return-msgtyp = 'E'.
    t_return-msgid  = 'ZWMMSG001'.
    t_return-msgnr  = '351'.
    t_return-msgv1  = lv_matnr.
    APPEND t_return.
  ENDIF.

** OT de Aviso de entrada
  lv_lznum = i_lznum.

**********************************************************************
  "INI-05/06/2024-Inetum-AJM- Armazem automatico - Modo Noite
  IF ls_zwm009-wcs_point NE 'AUT'.
    FIELD-SYMBOLS <lfs_zwm030> TYPE zwm030.
    ASSIGN ('(SAPLZWMFUNC4)GS_ZWM030') TO <lfs_zwm030>.
    IF <lfs_zwm030> IS ASSIGNED.
      SELECT *
        FROM zwm083
        INTO TABLE @DATA(lt_zwm083)
        WHERE linha EQ @i_linha
          AND aufnr EQ @<lfs_zwm030>-aufnr.
      IF sy-subrc IS NOT INITIAL.
        DATA(lr_lety) = zcl_wm_utils=>get_t334e_letyp_range( ).
        SELECT SINGLE matnr,
                      lgnum,
                      lety1
          FROM mlgn
          INTO @DATA(ls_mlgn)
          WHERE matnr EQ @<lfs_zwm030>-matnr
            AND lgnum EQ @i_lgnum
            AND lety1 IN @lr_lety.
        IF sy-subrc IS INITIAL.
          DATA lv_tstamp TYPE zwm082-tstamp_ini.
          lv_tstamp = sy-datum && sy-uzeit.
          DATA lv_day TYPE scal-indicator.
          CALL FUNCTION 'DATE_COMPUTE_DAY'
            EXPORTING
              date = sy-datum
            IMPORTING
              day  = lv_day.
          SELECT SINGLE *
            FROM zwm082
            INTO @DATA(ls_zwm082)
            WHERE ativo EQ @abap_true
              AND ( ( tstamp_ini LE @lv_tstamp
                  AND tstamp_fim GE @lv_tstamp )
                  OR weekday EQ @lv_day ).
          IF sy-subrc IS INITIAL.
            ls_zwm009-wcs_point = 'AUT'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  "Fim-05/06/2024-Inetum-AJM- Armazem automatico - Modo Noite
**********************************************************************

  CALL FUNCTION 'ZWM_CREATE_TO_CHD'
    EXPORTING
      i_lgnum    = i_lgnum
      i_lgpla    = ls_zwm009-wcs_point
      i_lznum    = lv_lznum
      i_linha    = i_linha
      I_ZEUGN    = I_ZEUGN
      i_type_mov = 'E'
    IMPORTING
      e_to_dummy = e_to_dummy
    TABLES
      t_return   = t_return
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'ZWM_MSG_LOG_WCS'
      EXPORTING
        i_object    = lc_object
        i_subobject = lv_subobject
        i_state     = 'A'
      TABLES
        t_log2      = t_return
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    RAISE error.
  ENDIF.

  CALL FUNCTION 'ZWM_MSG_LOG_WCS'
    EXPORTING
      i_object    = lc_object
      i_subobject = lv_subobject
      i_state     = 'A'
    TABLES
      t_log2      = t_return
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

ENDFUNCTION.
