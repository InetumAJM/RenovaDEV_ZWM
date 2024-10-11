class ZCL_ZWM_WORK_EPE_ODATA_DPC_EXT definition
  public
  inheriting from ZCL_ZWM_WORK_EPE_ODATA_DPC
  create public .

public section.
protected section.

  methods FINALIZESET_CREATE_ENTITY
    redefinition .
  methods OBTAINSET_GET_ENTITY
    redefinition .
  methods STATIONSHSET_GET_ENTITYSET
    redefinition .
  methods TRANSFERSET_CREATE_ENTITY
    redefinition .
  methods WAREHOUSESHSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZWM_WORK_EPE_ODATA_DPC_EXT IMPLEMENTATION.


  METHOD finalizeset_create_entity.

* Obter os valores dos parâmetros
    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).


* Chamar o MF ZWM_FIN_WORK_EPE
    CALL FUNCTION 'ZWM_FIN_WORK_EPE'
      EXPORTING
        i_lgnum       = er_entity-lgnum
        i_pos         = er_entity-pos
        i_lenum       = er_entity-lenum
      IMPORTING
        e_work_finish = er_entity-work_finish
        e_sucess      = er_entity-sucess
        e_msg         = er_entity-msg.

  ENDMETHOD.


  METHOD obtainset_get_entity.

    DATA: ls_work_epe TYPE zwm_013,
          lt_pal_epe  TYPE ztt_zwm_012.

    DATA(lt_key) = io_tech_request_context->get_keys( ).

* Obter os valores das chaves
    READ TABLE lt_key INTO DATA(ls_key) WITH KEY name = 'LGNUM'.
    DATA(lv_lgnum) = CONV lgnum( ls_key-value ).

    CLEAR ls_key.

    READ TABLE lt_key INTO ls_key WITH KEY name = 'EPE'.
    DATA(lv_epe) = CONV lgpla( ls_key-value ).

    CLEAR ls_key.

    READ TABLE lt_key INTO ls_key WITH KEY name = 'SIM'.
    DATA(lv_sim) = CONV flag( ls_key-value ).

* Chamar o MF ZWM_GET_WORK_EPE
    CALL FUNCTION 'ZWM_GET_WORK_EPE'
      EXPORTING
        i_lgnum     = lv_lgnum
        i_epe       = lv_epe
        i_sim       = lv_sim
      IMPORTING
        es_work_epe = ls_work_epe
        et_pal_epe  = lt_pal_epe.

* Converter para JSON a estrutura ls_work_epe
    DATA(ls_work_epe_json) = /ui2/cl_json=>serialize(
                              data = ls_work_epe
                              compress = abap_false
                              pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).


* Converter para JSON a tabela lt_pal_epe
    DATA(lt_pal_epe_json) = /ui2/cl_json=>serialize(
                              data = lt_pal_epe
                              compress = abap_false
                              pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).


* Atribuir a estrutura e tabela à entidade externa
    er_entity-lgnum     = lv_lgnum.
    er_entity-epe       = lv_epe.
    er_entity-sim       = lv_sim.
    er_entity-work_epe  = ls_work_epe_json.
    er_entity-pal_epe   = lt_pal_epe_json.

  ENDMETHOD.


  method STATIONSHSET_GET_ENTITYSET.

    SELECT DISTINCT equipamento FROM zwm011 INTO TABLE @DATA(lt_equipamento).

    LOOP AT lt_equipamento INTO DATA(ls_equipamento).
      APPEND ls_equipamento TO et_entityset.
    ENDLOOP.

  endmethod.


  method TRANSFERSET_CREATE_ENTITY.

* Obter os valores dos parâmetros
    io_data_provider->read_entry_data( IMPORTING es_data = ER_ENTITY ).


* Chamar o MF ZWM_GET_WORK_EPE
    CALL FUNCTION 'ZWM_TRF_WORK_EPE'
      EXPORTING
        i_lgnum  = ER_ENTITY-lgnum
        i_pos    = ER_ENTITY-pos
        i_lenum  = ER_ENTITY-lenum
      IMPORTING
        e_sucess = ER_ENTITY-sucess
        e_msg    = ER_ENTITY-msg.

  endmethod.


  METHOD warehouseshset_get_entityset.

    SELECT DISTINCT armazem FROM zwm001 INTO TABLE @DATA(lt_armazem).

    LOOP AT lt_armazem INTO DATA(ls_armazem).
      APPEND ls_armazem TO et_entityset.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
