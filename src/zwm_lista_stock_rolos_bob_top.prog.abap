*&---------------------------------------------------------------------*
*&  Include           ZWM_LISTA_STOCK_ROLOS_BOB_TOP
*&---------------------------------------------------------------------*


**************************************************************
*                   Tabelas
**************************************************************

TABLES: lqua, mara, kna1, z02rpmateriais.


**************************************************************
*                   Tipos
**************************************************************

TYPES: BEGIN OF ty_lqua,
         lgnum TYPE lqua-lgnum,
         lqnum TYPE lqua-lqnum,
         matnr TYPE lqua-matnr,
         werks TYPE lqua-werks,
         charg TYPE lqua-charg,
         lgpla TYPE lqua-lgpla,
         gesme TYPE lqua-gesme,
         lenum TYPE lqua-lenum,
       END OF ty_lqua,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
         bismt TYPE mara-bismt,
         breit TYPE mara-breit,
         prdha TYPE mara-prdha,
       END OF ty_mara,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,

       BEGIN OF ty_z02rpmateriais,
         matnr TYPE z02rpmateriais-matnr,
         cor   TYPE z02rpmateriais-cor,
       END OF ty_z02rpmateriais,

       BEGIN OF ty_alv,
         lgnum        TYPE lqua-lgnum,
         werks        TYPE lqua-werks,
         lgpla        TYPE lqua-lgpla,
         matnr        TYPE lqua-matnr,
         maktx        TYPE makt-maktx,
         lenum        TYPE lqua-lenum,
         gesme        TYPE lqua-gesme,
         numero_total TYPE i,
         valor_medio  TYPE lqua-gesme,
         charg        TYPE lqua-charg,
         mtart        TYPE mara-mtart,
         prdha        TYPE mara-prdha,
         kunnr        TYPE z02rpprolos-kunnr,
         name1        TYPE kna1-name1,
         vbeln        TYPE z02rpprolos-vbeln,
         refcli       TYPE z02rpprolos-refcli,
         norma        TYPE z02rpprolos-norma,
         dimint       TYPE z02rpprolos-dimint,
         dimext       TYPE z02rpprolos-dimext,
         largura      TYPE z02rpprolos-largura,
         quebra       TYPE z02rpprolos-quebra,
         veloce       TYPE z02rpprolos-veloce,
         rol_bob_seq  TYPE z02rpprolos-rolosq,
         embalagem(3) TYPE c,
         cor          TYPE z02rpmateriais-cor,
         bismt        TYPE mara-bismt,
         lznum        TYPE ltak-lznum,
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Inicio
         densidade    TYPE z02rpprolos-densidade,
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Fim
       END OF ty_alv.


**************************************************************
*                   Tabelas Internas
**************************************************************

DATA: gt_lqua           TYPE STANDARD TABLE OF ty_lqua,
      gt_mara           TYPE STANDARD TABLE OF ty_mara,
      gt_kna1           TYPE STANDARD TABLE OF ty_kna1,
      gt_makt           TYPE STANDARD TABLE OF makt,
      gt_z02rpprolos    TYPE STANDARD TABLE OF z02rpprolos,
      gt_z02rppbobinap  TYPE STANDARD TABLE OF z02rppbobinap,
      gt_z02rppbobinak  TYPE STANDARD TABLE OF z02rppbobinak,
      gt_z02rpmateriais TYPE STANDARD TABLE OF ty_z02rpmateriais,
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Inicio
      gt_z02rpconsprodl TYPE STANDARD TABLE OF z02rpconsprodl,
      gt_z02rpconsprodh TYPE STANDARD TABLE OF z02rpconsprodh,
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Fim
* INETUM - NR - 15.05.2024 - RENPRJ00037 - Inicio
      gt_z02rpebobinak  TYPE STANDARD TABLE OF z02rpebobinak,
* INETUM - NR - 15.05.2024 - RENPRJ00037 - Fim
      gt_alv            TYPE STANDARD TABLE OF ty_alv.


**************************************************************
*                   Estruturas
**************************************************************


**************************************************************
*                   Vaariáveis
**************************************************************


**************************************************************
*                   Constantes
**************************************************************


**************************************************************
*                   Objetos
**************************************************************

DATA: go_salv TYPE REF TO cl_salv_table.
