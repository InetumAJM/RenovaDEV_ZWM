class ZCL_WM_GESTAO_CAIS_DE_CARGA definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_zwm028_single,
        refnr         TYPE zwm_s002-refnr,
        nro_clientes  TYPE zwm_s002-nro_clientes,
        origem_carga  TYPE zwm_s002-origem_carga,
        origem_cargat TYPE zwm_s002-origem_cargat,
      END OF ty_zwm028_single .
  types:
    BEGIN OF name_mapping,
        abap TYPE abap_compname,
        json TYPE string,
      END OF name_mapping .
  types:
    name_mappings_ex TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY json .
  types:
    name_mappings    TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY abap .
  types:
    BEGIN OF ty_action,
        upd_porta         TYPE boolean,
        leitura_sscc      TYPE boolean,
        desbloqueio_porta TYPE boolean,
      END OF ty_action .

  constants:
    BEGIN OF cs_bloqueio_sscc,
                 bloquear    VALUE 'X',
                 desbloquear VALUE '-',
               END OF cs_bloqueio_sscc .

  class-methods GET_DEST_NAME
    returning
      value(RV_DEST) type RFCDEST .
  class-methods GET_DETALHE_PORTAS
    importing
      !LGNUM type ZWM002-ARMAZEM default '100'
      !PORTA type ZWM002-PORTA optional
      !INC_ZWM006_AUX type BKK_YESNO default 'X'
      !INC_INF_ADC_TRANSP type BKK_YESNO default 'X'
      !INC_T_ZWM028 type BKK_YESNO default 'X'
    exporting
      !ET_DETALHE_PORTAS type ZWM_TT_002
    returning
      value(RS_DETALHE_PORTA) type ZWM_S002 .
  class-methods GET_PORTAS
    importing
      !LGNUM type ZWM002-ARMAZEM default '100'
      !PORTA type ZWM002-PORTA optional
    exporting
      !ET_PORTAS type ZWM_TT_003
    returning
      value(RS_PORTA) type ZWM002 .
  class-methods GET_ZWM028
    importing
      !LGNUM type ZWM002-ARMAZEM default '100'
      !TRANSPORTE type ZWM028-TRANSPORTE
    exporting
      !ES_ZWM028 type ZWM_S005
    returning
      value(RT_ZWM028) type ZWM_TT_001 .
  class-methods GET_ZWM028_INFO
    importing
      !LGNUM type ZWM002-ARMAZEM default '100'
      !TRANSPORTE type ZWM028-TRANSPORTE
    returning
      value(RS_ZWM028_SINGLE) type TY_ZWM028_SINGLE .
  class-methods GET_ZWM006_AUX
    importing
      !LGNUM type ZWM002-ARMAZEM default '100'
      !NUM_ENTRADA type ZWM006_AUX-NUM_ENTRADA optional
    exporting
      !ET_ZWM006_AUX type ZWM_TT_004
    returning
      value(RS_ZWM006_AUX) type ZWM006_AUX .
  class-methods GET_INF_ADC_TRANSP
    importing
      !TRANSPORTE type ZWM028-TRANSPORTE
    exporting
      !ET_DETALHE_CLIENTE type ZWM_TT_005
    returning
      value(RS_DADOS_ADICIONAIS) type ZWM_S003 .
  class-methods GET_DETALHE_CLIENTE
    importing
      !TRANSPORTE type ZWM028-TRANSPORTE
    returning
      value(RT_DETALHE_CLIENTE) type ZWM_TT_005 .
  class-methods SET_BLOQUEIO_PORTA_SSCC
    importing
      !LGNUM type ZWM002-ARMAZEM default '100'
      !SSCC type ZWM002-SSCC optional
      !PORTA type ZWM002-PORTA
      !BLOQUEIO type BOOLEAN
    exporting
      !ES_DETALHE_PORTA type ZWM_S002 .
  class-methods IS_MULTI_CLIENTE
    importing
      !SDABW type VTTK-SDABW
    returning
      value(RV_IS_MUL_CLIENTE) type BKK_YESNO .
  class-methods SET_ENH_PRETTY_NAME_ACTIVE
    importing
      !IV_ON_OFF type BOOLEAN .
  class-methods IS_ENH_PRETTY_NAME_ACTIVE
    returning
      value(RV_ACTIVE) type BOOLEAN .
  class-methods GET_PRETTY_NAME
    importing
      !IN type CSEQUENCE
    changing
      !MT_NAME_MAPPINGS type NAME_MAPPINGS
      !MT_NAME_MAPPINGS_EX type NAME_MAPPINGS_EX
    returning
      value(OUT) type STRING .
  class-methods WS_LEITURA_SSCC
    importing
      !REFNR type ZWM028-REFNR optional
      !SSCC type EXIDV
      !STATUS type MSGTY
      !MESSAGE type BAPIRET2-MESSAGE .
  class-methods WS_UPD_TRANSPORTE_PORTA
    importing
      !LGNUM type ZWM002-ARMAZEM default '100'
      !PORTA type ZWM002-PORTA .
  class-methods INIT
    importing
      !LGNUM type ZWM002-ARMAZEM default '100'
      !PORTA type ZWM002-PORTA .
protected section.
private section.

  class-data GV_LGNUM type ZWM002-ARMAZEM value '100' ##NO_TEXT.
  class-data GV_PORTA type ZWM002-PORTA .
  class-data GV_ENH_PRETTY_NAME_ACT type BOOLEAN .

  class-methods GET_TEXT_SSCC_STATUS
    importing
      !STATUS type MSGTY
    returning
      value(RS_DETAIL) type ZWM_S006 .
  class-methods SEND_WS_MESSAGE
    importing
      !MENSAGEM type ANY .
ENDCLASS.



CLASS ZCL_WM_GESTAO_CAIS_DE_CARGA IMPLEMENTATION.


METHOD get_dest_name.

*|FIDCLNT030_WS|
  SELECT SINGLE valor
    FROM zwm001
    INTO rv_dest
    WHERE "ARMAZEM   eq '' and
          processo  EQ 'GESTAO_DE_CARGAS' AND
          parametro EQ 'RFCDEST'          .

ENDMETHOD.


METHOD get_detalhe_cliente.

  SELECT
  vttk~tknum,
  vttk~tplst,
   vttp~tpnum,
   vttp~vbeln,
    vbpa~posnr,
    vbpa~parvw,
    vbpa~kunnr,
    vbpa~lifnr,
    vbpa~adrnr,
      adrc~addrnumber,
      adrc~date_from,
      adrc~nation,
      adrc~date_to,
      adrc~title,
      adrc~name1,
      adrc~name2,
      adrc~name3,
      adrc~name4,
      adrc~name_text,
      adrc~name_co,
      adrc~city1,
      adrc~city2,
      adrc~city_code,
      adrc~cityp_code,
      adrc~home_city,
      adrc~cityh_code,
      adrc~post_code1,
      adrc~country,
      adrc~langu,
      adrc~region
    FROM vttk
      INNER JOIN vttp
      ON vttp~tknum EQ vttk~tknum
       INNER JOIN vbpa
       ON vbpa~vbeln EQ vttp~vbeln
        INNER JOIN adrc
        ON adrc~addrnumber EQ vbpa~adrnr
    INTO TABLE @rt_detalhe_cliente
     WHERE vttp~tknum EQ @transporte
       AND vbpa~parvw IN ( 'WE',   "Recebedor Mercadoria
                           'W1' ). "Destino
  .

  SELECT vttk~tknum,
         vttk~tplst,
          tvfptz~tdlnr,
          tvfptz~vsart,
          tvfptz~land1,
          tvfptz~fpstlz,
          tvfptz~tpstlz,
          tvfptz~trfzn,
           tvftzt~bezei
    INTO TABLE @DATA(lt_tvfptz)
     FROM vttk
    INNER JOIN tvfptz
    ON tvfptz~tplst EQ vttk~tplst
      LEFT OUTER JOIN tvftzt
      ON  tvftzt~spras EQ @sy-langu
      AND tvftzt~trfzn EQ tvfptz~trfzn
         WHERE vttk~tknum   EQ @transporte
           AND tvfptz~vsart EQ '01'.
  SORT lt_tvfptz BY tplst
                    land1
                    fpstlz.

  LOOP AT rt_detalhe_cliente ASSIGNING FIELD-SYMBOL(<lfs_det_cliente>).
    LOOP AT lt_tvfptz INTO DATA(ls_tvfptz)
      WHERE tplst  EQ <lfs_det_cliente>-tplst
        AND land1  EQ <lfs_det_cliente>-country
        AND fpstlz LE <lfs_det_cliente>-post_code1
        AND tpstlz GE <lfs_det_cliente>-post_code1.
      EXIT.
    ENDLOOP.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    <lfs_det_cliente>-trfzn   = ls_tvfptz-trfzn.
    <lfs_det_cliente>-trfznt  = ls_tvfptz-bezei.

  ENDLOOP.

ENDMETHOD.


METHOD get_detalhe_portas.

  get_portas( EXPORTING porta = porta
              IMPORTING et_portas = DATA(lt_portas) ).

  LOOP AT lt_portas INTO DATA(ls_porta).
    APPEND CORRESPONDING #( ls_porta )
      TO et_detalhe_portas
        ASSIGNING FIELD-SYMBOL(<lfs_porta>).
    <lfs_porta>-porta2 = <lfs_porta>-porta+1.

    IF <lfs_porta>-bloqueio_sscc EQ cs_bloqueio_sscc-bloquear.
      <lfs_porta>-sscc_info         = get_text_sscc_status( 'E' ).
*      <lfs_porta>-sscc_info-exidv        = ''.
      <lfs_porta>-sscc_info-message      = 'Porta Bloqueada - A Aguardar desbloqueio'.
*    ELSEIF ls_porta-bloqueio_sscc EQ cs_bloqueio_sscc-desbloquear.
*      CLEAR <lfs_porta>-bloqueio_sscc .
    ENDIF.

    <lfs_porta>-no_transport = abap_true.

*Error       - Message is an error
*Information - Message should be just an information
*None        - Message has no specific level
*Success     - Message is a success message
*Warning     - Message is a warning - sap.ui.core.MessageType.Warning
    CONSTANTS: BEGIN OF lc_status,
                 suc TYPE text20 VALUE 'Success',
                 err TYPE text20 VALUE 'Error',
                 inf TYPE text20 VALUE 'Information',
               END OF lc_status.
    <lfs_porta>-tile_highlight = lc_status-inf.
    IF ls_porta-bloqueio_sscc EQ abap_true.
      <lfs_porta>-tile_highlight = lc_status-err.
    ELSEIF <lfs_porta>-num_entrada IS INITIAL.
      <lfs_porta>-tile_highlight = lc_status-inf.
    ELSE.
      <lfs_porta>-tile_highlight = lc_status-suc.
    ENDIF.
    <lfs_porta>-zwm028_h-perc_text  = ''.
    <lfs_porta>-zwm028_h-perc_value = '0'.
    <lfs_porta>-zwm028_h-perc_state = 'None'.

*PORTA_ICON
*PORTA_STATE
*        switch (status) {
*          case "Success":
*            oIcon = "sap-icon://status-negative";
*            break;
*          case "Error":
*            oIcon = "sap-icon://status-negative";
*            break;
*          case "Information":
*            oIcon = "sap-icon://status-positive";
*            break;
*        }

*switch (status) {
*          case "Success":
*            oState = "Error";
*            break;
*          case "Error":
*            oState = "Error";
*            break;
*          case "Information":
*            oState = "Success";
*            break;
*        }
    <lfs_porta>-porta_icon  = 'sap-icon://status-positive'.
    <lfs_porta>-porta_state = lc_status-suc.
    CASE <lfs_porta>-tile_highlight.
      WHEN lc_status-suc.
        <lfs_porta>-porta_icon  = 'sap-icon://status-negative'.
        <lfs_porta>-porta_state = lc_status-err.
      WHEN lc_status-err.
        <lfs_porta>-porta_icon  = 'sap-icon://status-negative'.
        <lfs_porta>-porta_state = lc_status-err.
      WHEN lc_status-inf.
        <lfs_porta>-porta_icon  = 'sap-icon://status-positive'.
        <lfs_porta>-porta_state = lc_status-suc.
    ENDCASE.
    IF <lfs_porta>-num_entrada IS INITIAL.
      CONTINUE.
    ENDIF.

    IF inc_zwm006_aux EQ abap_true.
      <lfs_porta>-zwm006_aux = get_zwm006_aux( num_entrada = <lfs_porta>-num_entrada ).
      <lfs_porta>-tknum = <lfs_porta>-zwm006_aux-n_transporte.
      IF <lfs_porta>-tknum IS NOT INITIAL.
        CLEAR <lfs_porta>-no_transport.
*        <lfs_porta>-no_transport = '-'.
      ENDIF.

      IF <lfs_porta>-tknum IS NOT INITIAL.
        DATA(ls_zwm028_single)    = get_zwm028_info( <lfs_porta>-tknum ).
        <lfs_porta>-refnr         = ls_zwm028_single-refnr       .
        <lfs_porta>-nro_clientes  = ls_zwm028_single-nro_clientes.
        <lfs_porta>-origem_carga  = ls_zwm028_single-origem_carga.
        <lfs_porta>-origem_cargat = ls_zwm028_single-origem_cargat.

        IF inc_t_zwm028 EQ abap_true.
          <lfs_porta>-t_zwm028 = get_zwm028( EXPORTING transporte = <lfs_porta>-tknum
                                             IMPORTING es_zwm028  = <lfs_porta>-zwm028_h ).
        ENDIF.

        IF inc_inf_adc_transp EQ abap_true.
          get_inf_adc_transp(
            EXPORTING
              transporte          = <lfs_porta>-tknum     " Nº transporte
            IMPORTING
              et_detalhe_cliente  = <lfs_porta>-t_detalhe_cliente    " Informação Recebedor Merc (WE) e Destino Final (W1)
            RECEIVING
              rs_dados_adicionais = <lfs_porta>-inf_adc_transp    " Dados Adicionais
          ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

**  delete et_detalhe_portas WHERE num_entrada is INITIAL. "temp

  READ TABLE et_detalhe_portas INTO rs_detalhe_porta INDEX 1.

ENDMETHOD.


METHOD get_inf_adc_transp.

  SELECT SINGLE
          vttk~tknum,
          vttk~vbtyp,
          vttk~shtyp,
          vttk~tplst,
          vttk~sdabw,
            tvsakt~bezei AS sdabwt
    FROM vttk
          LEFT OUTER JOIN tvsakt
      ON  tvsakt~spras EQ @sy-langu
      AND tvsakt~sdabw EQ vttk~sdabw
    INTO @rs_dados_adicionais
    WHERE tknum EQ @transporte.

  rs_dados_adicionais-multi_cliente = is_multi_cliente( rs_dados_adicionais-sdabw ).

*  DATA(lt_det_cliente) = get_detalhe_cliente( transporte ).
  et_detalhe_cliente = get_detalhe_cliente( transporte ).
  READ TABLE et_detalhe_cliente INTO DATA(ls_vbpa_adrc)
    WITH KEY parvw = 'WE'.

  rs_dados_adicionais-trfzn        = ls_vbpa_adrc-trfzn.
  rs_dados_adicionais-trfznt       = ls_vbpa_adrc-trfznt.
  rs_dados_adicionais-trfznt       = replace( val  = rs_dados_adicionais-trfznt
                                              sub  = 'Tarifa Para ' with = '' ).
  rs_dados_adicionais-trfznt       = replace( val  = rs_dados_adicionais-trfznt
                                              sub  = 'Tarifa para ' with = '' ).

ENDMETHOD.


METHOD get_portas.

  DATA lr_porta TYPE RANGE OF zwm002-porta.

  REFRESH lr_porta.
  IF porta IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = porta ) TO lr_porta.
  ENDIF.

  SELECT *
    FROM zwm002
    INTO TABLE @et_portas
      WHERE armazem EQ @lgnum
        AND porta   IN @lr_porta.

  READ TABLE et_portas INTO rs_porta INDEX 1.

  IF et_portas IS INITIAL.
    IF strlen( porta ) LT 3.
      DATA(lv_porta) = porta.
      lv_porta = '0' && lv_porta.
      get_portas(
        EXPORTING
          lgnum     = lgnum    " Nº do depósito/complexo de depósito
          porta     = lv_porta " Portão para o nº do depósito
        IMPORTING
          et_portas = et_portas    " ZWM002
        RECEIVING
          rs_porta  = rs_porta    " Get Detalhe Portas
      ).
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD get_pretty_name.

  DATA: tokens TYPE TABLE OF char128,
        cache  LIKE LINE OF mt_name_mappings.

  FIELD-SYMBOLS: <token> LIKE LINE OF tokens,
                 <cache> LIKE LINE OF mt_name_mappings.

  READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
  IF sy-subrc IS INITIAL.
    out = <cache>-json.
  ELSE.
    out = in.

    REPLACE ALL OCCURRENCES OF `__` IN out WITH `*`.

    TRANSLATE out TO LOWER CASE.
    TRANSLATE out USING `/_:_~_`.
    SPLIT out AT `_` INTO TABLE tokens.
    LOOP AT tokens ASSIGNING <token>. " FROM 2. <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO out.
    REPLACE ALL OCCURRENCES OF `*` IN out WITH `_`.

    cache-abap  = in.
    cache-json = out.
    INSERT cache INTO TABLE mt_name_mappings.
    INSERT cache INTO TABLE mt_name_mappings_ex.
  ENDIF.

ENDMETHOD.


METHOD get_text_sscc_status.

  rs_detail-status       = status.
  CASE status.
    WHEN 'S'.
      rs_detail-status_color = '#90EE90'. "light green "'green'.
      rs_detail-status_text  = 'Palete Correta'.
      rs_detail-status_icon  = 'displaycaisdecarga/images/icon_yes.png'. "'sap-icon://accept'.
    WHEN 'E'.
      rs_detail-status_color = 'red'.
      rs_detail-status_text  = 'Palete Errada'.
      rs_detail-status_icon  = 'displaycaisdecarga/images/icon_no.png'. "'sap-icon://decline'.
    WHEN 'W'.
      rs_detail-status_color = 'yellow'.
      rs_detail-status_text  = 'Palete Já Carregada'.
      rs_detail-status_icon  = 'displaycaisdecarga/images/icon_stop.png'. "'sap-icon://warning'.
    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.


METHOD get_zwm006_aux.

  DATA lr_num_entrada TYPE RANGE OF zwm006_aux-num_entrada.

  REFRESH lr_num_entrada.
  IF num_entrada IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = num_entrada ) TO lr_num_entrada.
  ENDIF.

  SELECT *
    FROM zwm006_aux
    INTO TABLE @et_zwm006_aux
      WHERE armazem     EQ @lgnum
        AND num_entrada IN @lr_num_entrada.

  READ TABLE et_zwm006_aux INTO rs_zwm006_aux INDEX 1.

ENDMETHOD.


METHOD get_zwm028.

  DATA lr_transporte TYPE RANGE OF zwm028-transporte.

  REFRESH lr_transporte.
  IF transporte IS NOT INITIAL.
    DATA(lv_transp) = transporte.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_transp
      IMPORTING
        output = lv_transp.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_transp ) TO lr_transporte.
  ENDIF.

  SELECT zwm028~*,
          kna1~name1 AS emissor_t,
          kna1~land1,
            t005~lkvrz,
              vttk~sdabw
    FROM zwm028
      LEFT OUTER JOIN kna1
      ON kna1~kunnr EQ zwm028~emissor
        LEFT OUTER JOIN t005
        ON t005~land1 EQ kna1~land1
          LEFT OUTER JOIN vttk
          ON vttk~tknum EQ zwm028~transporte
    INTO TABLE @rt_zwm028
      WHERE lgnum      EQ @lgnum
        AND transporte IN @lr_transporte.

  DATA: lv_perc TYPE p DECIMALS 0.
  LOOP AT rt_zwm028 ASSIGNING FIELD-SYMBOL(<lfs_zwm028>).
    <lfs_zwm028>-multi_cliente = is_multi_cliente( <lfs_zwm028>-sdabw ).
    DATA(lv_multi_cliente) = <lfs_zwm028>-multi_cliente.

*    <lfs_zwm028>-perc_text  = '2 de 4'.
    DATA(lv_total_paletes) = CONV i( <lfs_zwm028>-total_paletes ).
    <lfs_zwm028>-perc_text  = |{ <lfs_zwm028>-paletes_carro } de { lv_total_paletes }|.
    CLEAR lv_perc.
    IF <lfs_zwm028>-total_paletes IS NOT INITIAL.
      lv_perc = ( <lfs_zwm028>-paletes_carro / <lfs_zwm028>-total_paletes ) * 100.
    ENDIF.
    <lfs_zwm028>-perc_value = lv_perc. "'50'.
    <lfs_zwm028>-perc_state = 'Success'.
  ENDLOOP.

  READ TABLE rt_zwm028 INTO es_zwm028
    WITH KEY remessa = space.
  IF sy-subrc IS INITIAL.
    DELETE rt_zwm028 INDEX sy-tabix.
    IF <lfs_zwm028> IS ASSIGNED.
      es_zwm028-land1 = <lfs_zwm028>-land1.
      es_zwm028-lkvrz = <lfs_zwm028>-lkvrz.
      IF <lfs_zwm028>-land1 IS NOT INITIAL.
        es_zwm028-url_bandeira = |https://flagcdn.com/w320/{ <lfs_zwm028>-land1 }.png|.
      ENDIF.
      IF lines( rt_zwm028[] ) GT 1.
        es_zwm028-emissor   = ''.
        es_zwm028-emissor_t = 'Multi-Cliente'.
      ELSE.
        READ TABLE rt_zwm028 INTO DATA(ls_cliente) INDEX 1.
        es_zwm028-emissor   = ls_cliente-emissor.
        es_zwm028-emissor_t = ls_cliente-emissor_t.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lv_multi_cliente EQ abap_true.
    REFRESH rt_zwm028.
    APPEND INITIAL LINE TO rt_zwm028 ASSIGNING <lfs_zwm028>.
    <lfs_zwm028>-emissor   = 'X'.
    <lfs_zwm028>-emissor_t = 'MULTICLIENTE'.
  ENDIF.

  SORT rt_zwm028 BY ordem.

ENDMETHOD.


METHOD get_zwm028_info.

  DATA lr_transporte TYPE RANGE OF zwm028-transporte.

  REFRESH lr_transporte.
  IF transporte IS NOT INITIAL.
    DATA(lv_transp) = transporte.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_transp
      IMPORTING
        output = lv_transp.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_transp ) TO lr_transporte.
  ENDIF.

  SELECT lgnum,
         refnr,
         remessa,
         st_pul,
         st_dck
    FROM zwm028
    INTO TABLE @DATA(lt_zwm028)
      WHERE lgnum      EQ @lgnum
        AND transporte IN @lr_transporte.

  LOOP AT lt_zwm028 INTO DATA(ls_zwm028).
    IF ls_zwm028-remessa IS NOT INITIAL.
      rs_zwm028_single-nro_clientes = rs_zwm028_single-nro_clientes + 1.
    ELSE.
      "ls_zwm028-st_dck.
      rs_zwm028_single-origem_carga = COND #( WHEN ls_zwm028-st_pul IS NOT INITIAL
                                              THEN ls_zwm028-st_pul
                                              ELSE ls_zwm028-st_dck ).
    ENDIF.
    rs_zwm028_single-refnr        = ls_zwm028-refnr.
  ENDLOOP.

  IF rs_zwm028_single-origem_carga IS NOT INITIAL.
    SELECT SINGLE ltypt
      INTO rs_zwm028_single-origem_cargat
      FROM t301t
      WHERE spras EQ sy-langu
        AND lgnum EQ lgnum
        AND lgtyp EQ rs_zwm028_single-origem_carga.
  ENDIF.

ENDMETHOD.


METHOD INIT.

  gv_lgnum = lgnum.
  gv_porta = porta.

ENDMETHOD.


METHOD is_enh_pretty_name_active.

  rv_active = gv_enh_pretty_name_act.

ENDMETHOD.


METHOD is_multi_cliente.

  CLEAR rv_is_mul_cliente.

  "Código de processamento especial
  DATA lr_sdabw TYPE RANGE OF vttk-sdabw.
  lr_sdabw = VALUE #( sign = 'I' option = 'EQ'
                      ( low = '0005' ) "carro à palete
                      ( low = '0010' ) "carro ao peso
                      ).

  IF sdabw IN lr_sdabw.
    rv_is_mul_cliente = abap_true.
  ENDIF.

ENDMETHOD.


METHOD send_ws_message.

  set_enh_pretty_name_active( abap_true ).
  DATA(lv_json) = /ui2/cl_json=>serialize(
    EXPORTING
      data             = mensagem
      compress         = abap_true
      numc_as_string   = abap_true
      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      conversion_exits = abap_true ).
  set_enh_pretty_name_active( abap_false ).

  REPLACE ALL OCCURRENCES OF '":[' IN lv_json WITH '":{"results":['.
  REPLACE ALL OCCURRENCES OF ']' IN lv_json WITH ']}'.

*  DATA(lv_dest_fiori) = |FIDCLNT030_RFC|.
*  DATA(lv_dest_fiori) = |FIDCLNT030_WS|.
  DATA(lv_dest_fiori) = get_dest_name( ).
  DATA lv_msg TYPE string.
  CALL FUNCTION 'ZWM_WS_UPD_DISPLAY_CAIS_CARGA'
    DESTINATION lv_dest_fiori
    EXPORTING
      iv_msg                = lv_json
    EXCEPTIONS
      communication_failure = 90 "MESSAGE lv_msg
      system_failure        = 92 "MESSAGE lv_msg
      OTHERS                = 99.

ENDMETHOD.


METHOD set_bloqueio_porta_sscc.
  DATA: BEGIN OF ls_json_msg.
          INCLUDE TYPE ty_action.
*          INCLUDE TYPE zwm002.
          INCLUDE TYPE zwm_s002.
          DATA:
          lgnum TYPE zwm028-lgnum,
        END OF ls_json_msg.

  UPDATE zwm002
    SET bloqueio_sscc = bloqueio "abap_true
        sscc          = sscc
      WHERE armazem EQ lgnum
        AND porta   EQ porta.

  COMMIT WORK AND WAIT.

  IF es_detalhe_porta IS REQUESTED.
    es_detalhe_porta = CORRESPONDING #( zcl_wm_gestao_cais_de_carga=>get_detalhe_portas(
                                        porta = porta " Portão para o nº do depósito
                      ) ).
  ENDIF.

  IF bloqueio EQ abap_false OR
     bloqueio EQ cs_bloqueio_sscc-desbloquear.
    ls_json_msg = CORRESPONDING #( es_detalhe_porta ).

    ls_json_msg-desbloqueio_porta = abap_true.
    ls_json_msg-lgnum             = lgnum.
    ls_json_msg-porta             = porta.

    send_ws_message( ls_json_msg ).
  ENDIF.
ENDMETHOD.


METHOD SET_ENH_PRETTY_NAME_ACTIVE.
  gv_enh_pretty_name_act = iv_on_off.
ENDMETHOD.


METHOD ws_leitura_sscc.

  DATA: BEGIN OF ls_json_msg.
          INCLUDE TYPE ty_action.
          INCLUDE TYPE zwm_s002.
        DATA:
                lgnum	TYPE zwm028-lgnum.
*                porta      TYPE zwm002-porta,
*                refnr      TYPE zwm028-refnr,
*                zwm028_h  TYPE zwm_s005,
*                sscc_info TYPE zwm_s006.
  DATA: END OF ls_json_msg.

  IF status EQ 'E'.
    set_bloqueio_porta_sscc( EXPORTING porta    = gv_porta    " Portão para o nº do depósito
                                       sscc     = sscc
                                       bloqueio = zcl_wm_gestao_cais_de_carga=>cs_bloqueio_sscc-bloquear "abap_true
                             IMPORTING  es_detalhe_porta = DATA(ls_zwm_s002) ).
  ELSE.
    ls_zwm_s002 = CORRESPONDING #( zcl_wm_gestao_cais_de_carga=>get_detalhe_portas(
                                     porta = gv_porta ) ). " Portão para o nº do depósito

  ENDIF.

  ls_json_msg = CORRESPONDING #( ls_zwm_s002 ).

  ls_json_msg-leitura_sscc      = abap_true.
  ls_json_msg-lgnum             = gv_lgnum.
  ls_json_msg-porta             = gv_porta.
  ls_json_msg-refnr             = refnr.

  ls_json_msg-sscc_info         = get_text_sscc_status( status ).
  ls_json_msg-sscc_info-exidv   = sscc.
  ls_json_msg-sscc_info-message = message.

  IF refnr IS NOT INITIAL.
    SELECT SINGLE transporte
      FROM zwm028
      INTO @DATA(lv_tknum)
      WHERE lgnum   EQ @gv_lgnum
        AND refnr   EQ @refnr.

    get_zwm028( EXPORTING transporte = lv_tknum
                IMPORTING es_zwm028  = ls_json_msg-zwm028_h ).

  ENDIF.

  send_ws_message( ls_json_msg ).

ENDMETHOD.


METHOD ws_upd_transporte_porta.

  CHECK porta IS NOT INITIAL.

  DATA: BEGIN OF ls_json_msg.
          INCLUDE TYPE ty_action.
          INCLUDE TYPE zwm_s002.
        DATA: END OF ls_json_msg.

  DATA(ls_det_porta) = get_detalhe_portas(
      lgnum              = lgnum    " Nº do depósito/complexo de depósito
      porta              = porta    " Portão para o nº do depósito
  ).

  IF ls_det_porta-no_transport IS INITIAL.
    ls_det_porta-no_transport = '-'.
  ENDIF.
  IF ls_det_porta-bloqueio_sscc IS INITIAL.
    ls_det_porta-bloqueio_sscc = '-'.
  ENDIF.

  ls_json_msg = CORRESPONDING #( ls_det_porta ).
  ls_json_msg-upd_porta = abap_true.

  send_ws_message( ls_json_msg ).

ENDMETHOD.
ENDCLASS.
