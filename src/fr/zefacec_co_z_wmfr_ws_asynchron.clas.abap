class ZEFACEC_CO_Z_WMFR_WS_ASYNCHRON definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

*"* public components of class ZEFACEC_CO_Z_WMFR_WS_ASYNCHRON
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods ZWMFR_WS_ASYNCHRONOUS_LITE
    importing
      !INPUT type ZEFACEC_ZWMFR_WS_ASYNCHRONOUS1
    exporting
      !OUTPUT type ZEFACEC_ZWMFR_WS_ASYNCHRONOUS
    raising
      CX_AI_SYSTEM_FAULT
      CX_AI_APPLICATION_FAULT .
protected section.
*"* protected components of class ZEFACEC_CO_Z_WMFR_WS_ASYNCHRON
*"* do not include other source files here!!!
private section.
*"* private components of class ZEFACEC_CO_Z_WMFR_WS_ASYNCHRON
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZEFACEC_CO_Z_WMFR_WS_ASYNCHRON IMPLEMENTATION.


method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZEFACEC_CO_Z_WMFR_WS_ASYNCHRON'
    logical_port_name   = logical_port_name
  ).

endmethod.


method ZWMFR_WS_ASYNCHRONOUS_LITE.

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
      method_name = 'ZWMFR_WS_ASYNCHRONOUS_LITE'
    changing
      parmbind_tab = lt_parmbind
  ).

endmethod.
ENDCLASS.
