class ZWM_PT_WCS_CO_HOST_REC_SOAP definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods ZWMPT_WS_ASYNCHRONOUS_LITE
    importing
      !INPUT type ZWMPT_WS_ASYNCHRONOUS_LITE_SO1
    exporting
      !OUTPUT type ZWMPT_WS_ASYNCHRONOUS_LITE_SOA
    raising
      CX_AI_SYSTEM_FAULT .
  methods ZWMPT_WS_STOCK_PICTURE
    importing
      !INPUT type ZWMPT_WS_STOCK_PICTURE_SOAP_IN
    exporting
      !OUTPUT type ZWMPT_WS_STOCK_PICTURE_SOAP_OU
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZWM_PT_WCS_CO_HOST_REC_SOAP IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZWM_PT_WCS_CO_HOST_REC_SOAP'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method ZWMPT_WS_ASYNCHRONOUS_LITE.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'ZWMPT_WS_ASYNCHRONOUS_LITE'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method ZWMPT_WS_STOCK_PICTURE.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'ZWMPT_WS_STOCK_PICTURE'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
