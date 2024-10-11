*&---------------------------------------------------------------------*
*&  Include           ZWMREP0003TOP                                    *
*&---------------------------------------------------------------------*
PROGRAM zwmrep003 MESSAGE-ID zwmmsg001.

TYPE-POOLS: slis.
INCLUDE <icon>.

INCLUDE rlmobinc.
TABLES: zwm001,
        zwm002,
        zwm003,
        t300t,
        tvrot,
        zwm004,
        ekko,
        ekpo,
        vbfa,
        zwm005,
        zwm006,
        zwm007,
        vttp,
        kna1,
        zwm016,
        zwm017,
        zwm018,
        zwm019,
        vbrk,
        zwm025,
        vbsk,
        vbss,
        zwm028,
        t001l,
        mch1,
        zwm036.

DATA: itab_dynp LIKE dynpread OCCURS 0 WITH HEADER LINE,
      itab_return LIKE ddshretval OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF lcarga,
      simbolo(4),
      num_entrada   LIKE zwm006-num_entrada,
      dia           TYPE sy-datum,
      hora          TYPE sy-uzeit,
      porta         TYPE zwm006-porta,
      matricula     LIKE zwm006-matricula,
      tipo          TYPE zwm006-tipo_camiao,
      transportador LIKE lfa1-name1,
      observacoes   TYPE zwm003-observacoes,
      destino(20),
      cliente       LIKE kna1-kunnr,
      nome(20),
      dia_carga     TYPE sy-datum,
      hora_carga    TYPE sy-uzeit,
      transporte    TYPE vttk-tknum,
      documento     TYPE ebeln,
      remessa       TYPE ebeln,
      dia_entrega   TYPE sy-datum,
      hora_entrega  TYPE sy-uzeit,
      flag          TYPE i,
END OF lcarga.

TYPES: BEGIN OF ldescarga,
    simbolo(4),
    num_entrada TYPE zwm005-num_entrada,
    data TYPE zwm005-data,
    hora_entrada TYPE zwm005-hora_entrada,
    porta TYPE zwm005-porta,
    matricula TYPE zwm005-matricula,
    tipo_camiao TYPE zwm005-tipo_camiao,
    desc_transp TYPE zwm005-desc_transp,
    observacoes TYPE zwm005-observacoes,
    ord_compra TYPE zwm005-ord_compra,
END OF ldescarga.


DATA : BEGIN OF ti_zwm001 OCCURS 0.
        INCLUDE STRUCTURE zwm001.
DATA: END OF ti_zwm001.

*tabelas internas
DATA: carga1 TYPE lcarga OCCURS 0 WITH HEADER LINE. "Cargas
DATA: izwm003 TYPE zwm003 OCCURS 0 WITH HEADER LINE. "Fila de espera
DATA: izwm002 TYPE zwm002 OCCURS 0 WITH HEADER LINE. "Portas
DATA: izwm005 TYPE ldescarga OCCURS 0 WITH HEADER LINE. "Descargas
DATA: tab_zwm005 TYPE zwm005 OCCURS 0 WITH HEADER LINE. "Descargas
DATA: izwm006 TYPE zwm006 OCCURS 0 WITH HEADER LINE. "Cargas
DATA: zwm_return_truck TYPE zwm_return_truck OCCURS 0 WITH HEADER LINE.


*dados gerais
DATA: ok_code_0001 LIKE sy-ucomm,
      ok_code_0002 LIKE sy-ucomm,
      ok_code_0003 LIKE sy-ucomm,
      ok_code_0004 LIKE sy-ucomm,
      ok_code_0005 LIKE sy-ucomm,
      ok_code_0006 LIKE sy-ucomm,
      ok_code_0007 LIKE sy-ucomm,
      ok_code_0008 LIKE sy-ucomm,
      ok_code_0009 LIKE sy-ucomm,
      ok_code_0010 LIKE sy-ucomm,
      ok_code_0011 LIKE sy-ucomm,
      ok_code_0012 LIKE sy-ucomm,
      ok_code_0013 LIKE sy-ucomm,
      ok_code_0014 LIKE sy-ucomm,
      cursorfield(20),
      num_entrada LIKE zwm003-num_entrada,
      return_msg TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

*dados ecran 2
DATA: matricula LIKE zwm003-matricula,
      t_armazem(38).

*dados ecran 3
DATA: doc_compra     TYPE ekko-ebeln,
      tipo_camiao    LIKE vttk-sdabw,
      transportador  LIKE vttk-tdlnr,
      observacao     LIKE vttk-tpbez,
      nome_transp    LIKE lfa1-name1.

DATA: porta LIKE zwm002-porta.

** Para colocar várias ordens de compra em simultâneo
DATA : BEGIN OF ordens OCCURS 0,
        ebeln LIKE ekko-ebeln.
DATA : END OF ordens.

*dados ecran 4
DATA: doc_carga TYPE vttk-tknum.

*dados  ecran 5
DATA: indice_porta TYPE sy-tabix,
      indice_fila  TYPE sy-tabix.

TYPES: BEGIN OF portas1,
      index(1).
        INCLUDE STRUCTURE zwm_return_truck.
TYPES:  flag(1).
TYPES  END OF portas1.

DATA: tab_portas1 TYPE portas1 OCCURS 0 WITH HEADER LINE. "Portas

DATA: BEGIN OF fila_porta OCCURS 0,
      index(1),
      talao      TYPE zwm003-num_entrada,
      matricula  TYPE zwm003-matricula,
      observacao TYPE zwm003-observacoes,
      operacao   TYPE zwm003-operacao,
      chegada    TYPE zwm003-hora_entrada,
      flag(1),
      END OF fila_porta.

*dados ecran 6
DATA: matricula_d TYPE zwm006-matricula,
      matricula_o TYPE zwm006-matricula,
      porta_d TYPE zwm006-porta,
      tipo_porta(14).

*dados ecran 7
DATA: numero_transporte TYPE vttk-tknum.

*ecran 1
DATA: aux_s_date(10),
      v1(4),
      v2(4),
      v3(4),
      v5(4),
      v4(4),
      v6(4),
      v7(4).

DATA: BEGIN OF tab_f OCCURS 0,
      armazem TYPE zwm002-armazem,
      porta TYPE zwm002-porta,
      estado(4),
      matricula TYPE vttk-signi,
      tipo(8),
      estado_interno TYPE zwm002-estado,
      col TYPE slis_t_specialcol_alv,
      END OF tab_f.

*ecran 7
DATA: BEGIN OF help OCCURS 0,
      matricula TYPE zwm005-matricula,
      END OF help.

* ecran 8
DATA: pedido LIKE ekpo-ebeln,
      item LIKE ekpo-ebelp,
      material LIKE ekpo-matnr,
      descricao LIKE makt-maktx,
      quantidade LIKE ekpo-menge,
      uni LIKE ekpo-meins,
      talao(5),
      lote LIKE mch1-charg,
      certificado LIKE ltap-zeugn.

*ecran 9
DATA : doc_compra1 TYPE ekko-ebeln,
       doc_compra2 TYPE ekko-ebeln,
       doc_compra3 TYPE ekko-ebeln,
       doc_compra4 TYPE ekko-ebeln,
       doc_compra5 TYPE ekko-ebeln,
       doc_compra6 TYPE ekko-ebeln,
       doc_compra7 TYPE ekko-ebeln,
       doc_compra8 TYPE ekko-ebeln,
       doc_compra9 TYPE ekko-ebeln,
       doc_compra10 TYPE ekko-ebeln.

* ecran 10
DATA: talao_saida(5).

DATA: percentagem TYPE p DECIMALS 2,
      indice_tab TYPE lvc_index.
* ecran 11
DATA: remessa LIKE lips-vbeln,
      item_r LIKE lips-posnr,
      cliente LIKE kna1-kunnr,
      descricao_cliente(40),
      doc_renova LIKE likp-vbeln,
      l_teste like likp-vbeln,
      cod_dev LIKE zwm036-cod_dev,
      l_text  LIKE dd07v-ddtext,
      cod_mot LIKE zwm036-cod_motivo,
      motivo  LIKE zwm036-motivo.

* ecran 12
DATA: material1 LIKE mara-matnr,
      material2 LIKE mara-matnr,
      material3 LIKE mara-matnr,
      des1 LIKE makt-maktx,
      des2 LIKE makt-maktx,
      des3 LIKE makt-maktx,
      uni1 LIKE mara-meins,
      uni2 LIKE mara-meins,
      uni3 LIKE mara-meins,
      qtd1(2),
      qtd2(2),
      qtd3(2).

* ecran 13

DATA: deposito LIKE mard-lgort.
******************* ALV data declare
CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw DEFINITION LOAD.

DATA: tree  TYPE REF TO cl_gui_alv_tree_simple.



CONSTANTS: fcode_disp   LIKE sy-ucomm VALUE 'DISP'.

DATA: gt_fieldcatalog TYPE lvc_t_fcat,
      gt_sort         TYPE lvc_t_sort,
      gt_tab          LIKE tab_f OCCURS 0.


DATA:   g_repid LIKE sy-repid.
DATA:   ok_code         LIKE sy-ucomm,   "OK-Code
        g_user_command TYPE slis_formname VALUE 'USER_COMMAND',
        g_status TYPE slis_formname VALUE 'STANDARD_FULLSCREEN',
        grid          TYPE REF TO cl_gui_alv_grid,
        g_custom_container TYPE REF TO cl_gui_custom_container.

DATA: l_matnr    LIKE mara-matnr,
      l_lote     LIKE mch1-charg,
      l_cliente  LIKE kna1-kunnr,
      l_material LIKE mara-matnr,
      l_vbeln    LIKE likp-vbeln,
      l_cod      LIKE dd07v-domvalue_l.
