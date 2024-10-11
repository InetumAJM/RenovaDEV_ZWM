class ZCL_IM__LE_SHIPMENT definition
  public
  final
  create public .

*"* public components of class ZCL_IM__LE_SHIPMENT
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_BADI_LE_SHIPMENT .
protected section.
*"* protected components of class ZCL_IM__LE_SHIPMENT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM__LE_SHIPMENT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM__LE_SHIPMENT IMPLEMENTATION.


METHOD if_ex_badi_le_shipment~at_save .

  break roffd.

  DATA wa_zwm028 TYPE zwm028.

  DATA lt_zwm028 TYPE STANDARD TABLE OF zwm028 .

  DATA: wa_vttk       TYPE cxshipment,
        wa_new_vttk   TYPE vttkvb_tab,
        line_vttk     TYPE LINE OF vttkvb_tab,
        wa_new_vttp   TYPE vttpvb_tab,
        wa_zwm006_aux TYPE zwm006_aux,
        wa_zwm003     TYPE zwm003.

  DATA: l_refnr TYPE lvs_refnr,
        l_tknum TYPE tknum,
        lv_lgnum  TYPE lgnum, " << INS ROFF(SDF):TMGP:28.12.2015 11:06:25
        aux_refnr TYPE lvs_refnr.

  FIELD-SYMBOLS <fs_vttp> LIKE LINE OF wa_new_vttp[]. " << INS ROFF(SDF):TMGP:28.12.2015 11:06:50

  CLEAR lt_zwm028.
  REFRESH lt_zwm028.

  wa_vttk = im_shipments_at_save.

  wa_new_vttk = wa_vttk-new_vttk.
  wa_new_vttp = wa_vttk-new_vttp.

  LOOP AT wa_new_vttk INTO line_vttk.
*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 11:00:04
*/ Passar armazem associado ao grupo T311
    CLEAR lv_lgnum.

    READ TABLE wa_new_vttp[] ASSIGNING <fs_vttp>
      WITH KEY tknum = line_vttk-tknum. " obter primeiro item do transporte
    IF sy-subrc EQ 0.
      SELECT lgnum UP TO 1 ROWS
        FROM t311a INTO lv_lgnum
        WHERE rbnum EQ <fs_vttp>-vbeln
          AND rbtyp EQ 'L'. "Fornecimentos
      ENDSELECT.
    ENDIF.

    IF lv_lgnum IS INITIAL.
      lv_lgnum  = '100'.
    ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 11:00:04

** Validar se o transporte é para o sistema de depósito 100
    CALL FUNCTION 'ZWM_GET_REFNR_FROM_TKNUM'
      EXPORTING
*        lgnum           = '100'  " << DEL ROFF(SDF):TMGP:28.12.2015 11:07:08
        lgnum           = lv_lgnum  " << INS ROFF(SDF):TMGP:28.12.2015 11:07:16
        tknum           = line_vttk-tknum
      IMPORTING
        refnr           = l_refnr
        tknum_out       = l_tknum
      TABLES
        new_vttp        = wa_new_vttp
      EXCEPTIONS
        no_items        = 1
        no_group        = 2
        no_warehouse    = 3
        no_process      = 4
        duplicate_group = 5
        OTHERS          = 6.

    CHECK sy-subrc EQ 0 OR
          sy-subrc GT 3.

** Preencher a data de registo planeado
    IF line_vttk-dpreg IS INITIAL.
      MESSAGE i000(zwmmsg001) WITH
          'Obrigatório preencher a data planejada do registro'.
      RAISE error_with_message.
    ENDIF.

    aux_refnr = l_refnr.
    IF line_vttk-stten IS INITIAL.
** Validar se o existe algum transporte não finalizado para a matricula
      CALL FUNCTION 'ZWM_GET_REFNR_FROM_TKNUM'
        EXPORTING
*          lgnum           = '100'  " << DEL ROFF(SDF):TMGP:28.12.2015 11:07:28
          lgnum           = lv_lgnum " << INS ROFF(SDF):TMGP:28.12.2015 11:07:30
          tknum           = line_vttk-tknum
          signi           = line_vttk-signi
          check_tknum     = 'X'
        IMPORTING
          refnr           = l_refnr
          tknum_out       = l_tknum
        TABLES
          new_vttp        = wa_new_vttp
        EXCEPTIONS
          no_items        = 1
          no_group        = 2
          no_warehouse    = 3
          no_process      = 4
          duplicate_group = 5
          OTHERS          = 6.

      IF sy-subrc = 4.
        IF NOT l_tknum IS INITIAL.
          MESSAGE i000(zwmmsg001) WITH
              'A matrícula' line_vttk-signi
              'já está a ser utilizada no transporte' l_tknum.
          RAISE error_with_message.
        ENDIF.
      ENDIF.
    ENDIF.
*** Actualizar o transporte no grupo
*
*    break roffd.
*    SELECT * INTO TABLE lt_zwm028
*        FROM zwm028
*            WHERE lgnum = '100' AND
*                  refnr = aux_refnr.
*
*
*    LOOP AT lt_zwm028 INTO wa_zwm028.
*      CHECK wa_zwm028-transporte IS INITIAL.
*      wa_zwm028-transporte = line_vttk-tknum.
*      MODIFY lt_zwm028 FROM wa_zwm028.
*    ENDLOOP.
*
*    CLEAR wa_zwm028.
*    LOOP AT lt_zwm028 INTO wa_zwm028.
*      MODIFY zwm028 FROM wa_zwm028.
*      CLEAR wa_zwm028.
*    ENDLOOP.

** Actualizar se a matricula está preenchida
    IF NOT line_vttk-signi IS INITIAL.
      CALL FUNCTION 'ZWM_ACTUALIZA_ZWM006_ZWM003'
        EXPORTING
*          lgnum = '100'  " << DEL ROFF(SDF):TMGP:28.12.2015 11:06:09
          lgnum = lv_lgnum " << INS ROFF(SDF):TMGP:28.12.2015 11:06:05
          tknum = line_vttk-tknum
          signi = line_vttk-signi.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD if_ex_badi_le_shipment~before_update.

  DATA wa_zwm028 TYPE zwm028.

  DATA lt_zwm028 TYPE STANDARD TABLE OF zwm028 .

  DATA: wa_vttk       TYPE cxshipment,
        wa_new_vttk   TYPE vttkvb_tab,
        line_vttk     TYPE LINE OF vttkvb_tab,
        wa_new_vttp   TYPE vttpvb_tab,
        wa_zwm006_aux TYPE zwm006_aux,
        wa_zwm003     TYPE zwm003.

  DATA: l_refnr TYPE lvs_refnr,
        l_tknum TYPE tknum,
        lv_lgnum  TYPE lgnum, " << INS ROFF(SDF):TMGP:28.12.2015 10:51:00
        aux_refnr TYPE lvs_refnr.

  FIELD-SYMBOLS <fs_vttp> LIKE LINE OF wa_new_vttp[]. " << INS ROFF(SDF):TMGP:28.12.2015 11:00:35

  break roffd.
  CLEAR lt_zwm028.
  REFRESH lt_zwm028.

  wa_vttk = im_shipments_before_update.

  wa_new_vttk = wa_vttk-new_vttk.
  wa_new_vttp = wa_vttk-new_vttp.

  LOOP AT wa_new_vttk INTO line_vttk.
*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 11:00:04
*/ Passar armazem associado ao grupo T311
    CLEAR lv_lgnum.

    READ TABLE wa_new_vttp[] ASSIGNING <fs_vttp>
      WITH KEY tknum = line_vttk-tknum. " obter primeiro item do transporte
    IF sy-subrc EQ 0.
      SELECT lgnum UP TO 1 ROWS
        FROM t311a INTO lv_lgnum
        WHERE rbnum EQ <fs_vttp>-vbeln
          AND rbtyp EQ 'L'. "Fornecimentos
      ENDSELECT.
    ENDIF.

    IF lv_lgnum IS INITIAL.
      lv_lgnum  = '100'.
    ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 11:00:04

** Validar se o transporte é para o sistema de depósito 100
    CALL FUNCTION 'ZWM_GET_REFNR_FROM_TKNUM'
      EXPORTING
*        lgnum           = '100'  " << DEL ROFF(SDF):TMGP:28.12.2015 11:03:07
        lgnum           = lv_lgnum " << INS ROFF(SDF):TMGP:28.12.2015 11:03:04
        tknum           = line_vttk-tknum
      IMPORTING
        refnr           = l_refnr
        tknum_out       = l_tknum
      TABLES
        new_vttp        = wa_new_vttp
      EXCEPTIONS
        no_items        = 1
        no_group        = 2
        no_warehouse    = 3
        no_process      = 4
        duplicate_group = 5
        OTHERS          = 6.

    CHECK sy-subrc EQ 0 OR
          sy-subrc GT 3.

*** Preencher a data de registo planeado
*    IF line_vttk-dpreg IS INITIAL.
*      MESSAGE i000(zwmmsg001) WITH
*          'Obrigatório preencher a data planejada do registro'.
*      RAISE error_with_message.
*    ENDIF.

    aux_refnr = l_refnr.

** Actualizar o transporte no grupo

    SELECT * INTO TABLE lt_zwm028
        FROM zwm028
            WHERE lgnum = '100' AND
                  refnr = aux_refnr.
*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 10:51:15
*/ Passar armazem associado ao grupo T311
    IF sy-subrc NE 0.
      SELECT *
        FROM zwm028 INTO TABLE lt_zwm028[]
        WHERE lgnum EQ lv_lgnum
          AND refnr EQ aux_refnr.
    ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 10:51:16

    LOOP AT lt_zwm028 INTO wa_zwm028.
      wa_zwm028-transporte = line_vttk-tknum.
      MODIFY lt_zwm028 FROM wa_zwm028.
    ENDLOOP.

    CLEAR wa_zwm028.
    LOOP AT lt_zwm028 INTO wa_zwm028.
      MODIFY zwm028 FROM wa_zwm028.
      CLEAR wa_zwm028.
    ENDLOOP.

** Actualizar se a matricula está preenchida
    IF NOT line_vttk-signi IS INITIAL.
      CALL FUNCTION 'ZWM_ACTUALIZA_ZWM006_ZWM003'
        EXPORTING
*          lgnum = '100'  " << DEL ROFF(SDF):TMGP:28.12.2015 10:53:20
          lgnum = lv_lgnum  " << INS ROFF(SDF):TMGP:28.12.2015 10:53:15
          tknum = line_vttk-tknum
          signi = line_vttk-signi.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD if_ex_badi_le_shipment~in_update .

*  break roffd.

ENDMETHOD.
ENDCLASS.
