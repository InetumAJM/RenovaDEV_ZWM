FUNCTION-POOL ZWMFUNC7 MESSAGE-ID zwmmsg001.

INCLUDE zwminc000.
INCLUDE rlmobinc.

TABLES : zwm001,
         zwm002,
*         zwm003,
         zwm004,
         zwm005,
*         zwm006,
         zwm007," parametrização das portas de Carga/descarga
         zwm008,
         zwm009,
         zwm010,
         zwm011,
*         zwm012,
         zwm013,
         zwm014,
         zwm015,
         zwm016,
         zwm017,
         zwm018,
         zwm019,
         zwm020,
         zwm021,
         zwm022,
         zwm023,
         zwm024,
         zwm025,
         zwm026,
         zwm027,
         zwm028,
         zwm032,
         zwm040,
         zwm042,
         vbfa,
         vttp,
         vbuk,
         lqua,
         mch1,
         marc,
         marm,
         mkpf,
         mard,
         mean,
         ekko,
         t307,
         mchb,
         mlgt,
         vbsk,
         vbss,
         mcha,
         ekpo,
         tsp03,
         zwm006_aux,
         zwm003_aux.
*        zwm002_aux

** Variaveis para impressão de SSCC
DATA: itab_sscc LIKE zwm_ean128 OCCURS 0 WITH HEADER LINE,
      ean128    TYPE ean128,
      l_printer LIKE nast-ldest,
      barcode   TYPE barcode_flat.

DATA : tipo LIKE zwm010-tipo,
       resposta.
** Tabelas globais ao grupo de funções
DATA : BEGIN OF ti_zwm001 OCCURS 0.
        INCLUDE STRUCTURE zwm001.
DATA: END OF ti_zwm001.

DATA : BEGIN OF ti_zwm010 OCCURS 0.
        INCLUDE STRUCTURE zwm010.
DATA: END OF ti_zwm010.


DATA: text TYPE  bdcmsgcoll-msgv1,
      text1 TYPE  bdcmsgcoll-msgv1,
      text2 TYPE  bdcmsgcoll-msgv2.
DATA:     cursorfield(20).
DATA:return_msg TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: tab_zwm011 LIKE zwm011.
DATA: tab_zwm010 LIKE zwm010 OCCURS 0 WITH HEADER LINE.
DATA: wa_log LIKE zwm_log_efacec.

**dados gerais
DATA: bin_input(14),
      bin_output_o(14),
      bin_output_d(14),
      quant_in  LIKE ltap-vsola,
      quant_out LIKE ltap-vsola,
      bin_input_r(14), "Bin de Retorno
      bin_output_r(14), "Bin de Retorno
      store_destino TYPE ltap-nltyp,
      store_origem TYPE ltap-vltyp,
      proximo_bin LIKE ltap-nlpla,
      mov TYPE bwlvs.

DATA: l_tanum  LIKE ltap-tanum,
      l_tapos  LIKE ltap-tapos,
      l_vlpla  LIKE ltap-vlpla,
      l_vltyp  LIKE ltap-vltyp,
      l_letyp  LIKE ltap-letyp,
      l_nltyp  LIKE ltap-nltyp,
      l_vlenr  LIKE ltap-vlenr,
      l_linhas LIKE sy-tabix,
      l_remontada,
      l_sscc LIKE zwm020-p2.

DATA: itab_lqua LIKE lqua OCCURS 0 WITH HEADER LINE.

**dados ecran 0001
DATA: ok_code_0001 LIKE sy-ucomm,
      ok_code_0027 LIKE sy-ucomm.

DATA: su             TYPE ltap-vlenr,
      su1            TYPE ltap-vlenr,
      descricao(20),
      armazem_ret        TYPE ltap-lgnum,
      queue          TYPE ltak-queue,
      bin_origem     TYPE ltap-vlpla,
      equipamento_   TYPE zwm010-equipamento,
      bin_destino1   TYPE ltap-nlpla,
      bin_destino    TYPE ltap-nlpla,
      to_ret         TYPE ltap-tanum,
      to_ret2        TYPE ltap-tanum,
      to_ret3        TYPE ltap-tanum,
      item_ret       TYPE ltap-tapos,
      grupo          TYPE t311-refnr,
      remessa        LIKE ltak-vbeln,
      texto_processo(20),
      f3_activo,
      posicao_pulmao1(2),
      posicao_pulmao2(2).

DATA i_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.
DATA c_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.
**dados ecran 0003
DATA: ok_code_0003   LIKE sy-ucomm,
      armazem_tri    TYPE ltap-lgnum,
      to_tri         TYPE ltap-tanum,
      item_tri       TYPE ltap-tapos,
      tipo_tri(1), "tipo de queue E (entrada) S (saida)
      atribuida(4).

**dados ecran 0005 e 0006
DATA: ok_code_0005   LIKE sy-ucomm,
      porta1(14),
      l_porta TYPE lgtor,
      l_lgpla TYPE lgpla,
      porta2(14),
      num_palete(2).

**dados de ecran 0007
DATA : ok_code_0007 LIKE sy-ucomm,
       pos(4) VALUE '0',
       de(4),
       pulmao1(14),
       pulmao2(14),
       ped_compra(15),
       pedido LIKE ekko-ebeln,
       item LIKE ekpo-ebelp,
       to_terceiros LIKE ltak-tanum.

DATA : BEGIN OF l_su OCCURS 0,
        su LIKE lein-lenum.
DATA : END OF l_su.

** dados de ecran 0009
DATA : ok_code_0009 LIKE sy-ucomm,
       incidencia LIKE zwm022-incidencia,
       desc_incidencia LIKE zwm022-descricao,
       sscc_errado LIKE ltap-vlenr,
       st_type_tri LIKE ltap-vltyp,
       st_type_men LIKE ltap-vltyp,
       st_type_dri LIKE ltap-vltyp.

DATA: itab_return LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF ltap_cancel OCCURS 0.
        INCLUDE STRUCTURE ltap_cancl.
DATA : END OF ltap_cancel.

DATA : BEGIN OF sscc OCCURS 0.
        INCLUDE STRUCTURE zwm_sscc.
DATA : END OF sscc.


DATA : st_type_o LIKE ltap-vltyp,
       bin_o LIKE lagp-lgpla,
       bin_d LIKE lagp-lgpla,
       st_type_d LIKE ltap-vltyp,
       st_type LIKE ltap-vltyp,
       mensula LIKE ltap-vlpla,
       plant LIKE ltap-werks,
       lgort LIKE ltap-lgort,
       to LIKE ltap-tanum.

** dados de ecran 11 e 12
DATA : BEGIN OF tab OCCURS 0,
       ean11 LIKE mean-ean11,
       matnr LIKE mara-matnr,
END OF tab.

DATA: material_output LIKE mara-matnr.

** dados de ecran 13 e 14
DATA: ok_code_0013 LIKE sy-ucomm,
      ok_code_0014 LIKE sy-ucomm.
DATA: material LIKE ltap-matnr,
      tipo_palete LIKE ltap-letyp,
      posicao_pulmao(2),
      unidade LIKE ltap-meins,
      ecra_chamador(4),
      incidencia_terceiros LIKE zwm022-incidencia.
DATA  paletes_dif TYPE i.
DATA  pack_material LIKE mara-matnr.
DATA  quantidade LIKE vepo-vemng.
DATA  lote LIKE vepo-charg.


** dados de ecran 15 e 16
DATA : ok_code_0015 LIKE sy-ucomm,
       ok_code_0016 LIKE sy-ucomm,
       ean11 LIKE marm-ean11,
      n_etiq(2),
      hukey LIKE bapihukey-hu_exid.

DATA t_mchb LIKE mchb OCCURS 0 WITH HEADER LINE.
** Dados ecra 17 e 18
DATA:  ok_code_0017 LIKE sy-ucomm.
DATA: plant_o TYPE werks_d,
      sloc_o TYPE lgort_d.
** Dados ecra 19 e 20
DATA:  ok_code_0019 LIKE sy-ucomm.

**dados ecran 0021
DATA: ok_code_0021   LIKE sy-ucomm.

** dados ecran 23 e 24
DATA  ok_code_0023 LIKE sy-ucomm.

** dados ecran 25
DATA  ok_code_0025 LIKE sy-ucomm.

** dados ecran 29
DATA: ok_code_0029 LIKE sy-ucomm,
      sscc1 LIKE lein-lenum,
      sscc2 LIKE lein-lenum,
      desc LIKE makt-maktx,
      qtd LIKE ltap-vsolm,
      uni LIKE ltap-meins,
      bin_origem_o(14),
      bin_dest1(14),
      bin_dest2(14).

** Para a criação da HU
DATA : BEGIN OF items_hu OCCURS 0.
        INCLUDE STRUCTURE zwm_items_hu.
DATA : END OF items_hu.

** Dados para atribuição de TO ao operário
DATA : BEGIN OF l_ltap OCCURS 0.
        INCLUDE STRUCTURE ltap.
DATA : queue LIKE ltak-queue,
       tipo LIKE zwm010-tipo,
       prioridade_queue,
*       prioridade_to(5),
       prioridade_to TYPE p DECIMALS 0,
       refnr LIKE ltak-refnr,
       pos_remessa LIKE zwm028-ordem,
       pos_pulmao LIKE zwm013-posicao_pulmao,
       pulmao LIKE zwm013-destino.
** RL -> INS 27.04.2005
** Prioridades
*DATA: prioridade LIKE zwm028-prioridade.
** RL <- INS 27.04.2005
DATA : END OF l_ltap.

** RL -> INS 27.04.2005
** Prioridades
DATA: l_index LIKE sy-tabix.
** RL <- INS 27.04.2005

DATA : BEGIN OF l_ltap_inv OCCURS 0.
        INCLUDE STRUCTURE ltap.
DATA : sorlp LIKE lagp-sorlp.
DATA : END OF l_ltap_inv.

** Dados globais
DATA : movimento LIKE ltak-bwlvs.

DATA: to3,
      s_loc TYPE lgort_d,
      valor LIKE zwm001-valor,
      to_remontada LIKE ltap-tanum,
      pal_remontada LIKE lein-lenum.

DATA :  corredor(4),
         n_mensulas_ocupadas TYPE i.

*-------------Reformulação
  DATA : ti_zwm003 LIKE zwm003_aux OCCURS 0 WITH HEADER LINE,
         ti_zwm002 LIKE zwm002     OCCURS 0 WITH HEADER LINE.
*-----------------------

  DATA : tipo_camiao TYPE ztipo_camiao,
         porta LIKE zwm002-porta,
         index LIKE sy-tabix.

** Fila de espera
  DATA : BEGIN OF l_zwm003 OCCURS 0.
          INCLUDE STRUCTURE zwm003_aux.
  DATA : atribuida.
  DATA : END OF l_zwm003.
** Estado das portas
  DATA : BEGIN OF l_zwm002 OCCURS 0.
          INCLUDE STRUCTURE zwm002.
  DATA : END OF l_zwm002.

** Contem as portas de DESCARGA válidas caso existam
  DATA : BEGIN OF l_portas_descarga OCCURS 0.
          INCLUDE STRUCTURE zwm002.
  DATA : tipo LIKE zwm007-tipo,
         pulmao1 LIKE zwm007-pulmao1,
         pulmao2 LIKE zwm007-pulmao2,
         pulmao3 LIKE zwm007-pulmao3,
         pulmao4 LIKE zwm007-pulmao4,
         pulmao5 LIKE zwm007-pulmao5,
         pulmao6 LIKE zwm007-pulmao6,
         pulmao7 LIKE zwm007-pulmao7,
         pulmao8 LIKE zwm007-pulmao8,
         pulmao9 LIKE zwm007-pulmao9,
         pulmao10 LIKE zwm007-pulmao10,
         pulmao11 LIKE zwm007-pulmao11,
         pulmao12 LIKE zwm007-pulmao12,
         atribuida,
         peso.
  DATA : END OF l_portas_descarga.


** Contem as portas de CARGA DIRECTA válidas caso existam
  DATA : BEGIN OF l_portas_cargad OCCURS 0.
          INCLUDE STRUCTURE zwm002.
  DATA : tipo LIKE zwm007-tipo,
         pulmao1 LIKE zwm007-pulmao1,
         pulmao2 LIKE zwm007-pulmao2,
         pulmao3 LIKE zwm007-pulmao3,
         pulmao4 LIKE zwm007-pulmao4,
         pulmao5 LIKE zwm007-pulmao5,
         pulmao6 LIKE zwm007-pulmao6,
         pulmao7 LIKE zwm007-pulmao7,
         pulmao8 LIKE zwm007-pulmao8,
         pulmao9 LIKE zwm007-pulmao9,
         pulmao10 LIKE zwm007-pulmao10,
         pulmao11 LIKE zwm007-pulmao11,
         pulmao12 LIKE zwm007-pulmao12,
         atribuida,
         peso.
  DATA : END OF l_portas_cargad.


** Contem as portas de CARGA PULMÃO válidas caso existam
  DATA : BEGIN OF l_portas_cargap OCCURS 0.
          INCLUDE STRUCTURE zwm002.
  DATA : tipo LIKE zwm007-tipo,
         pulmao1 LIKE zwm007-pulmao1,
         pulmao2 LIKE zwm007-pulmao2,
         pulmao3 LIKE zwm007-pulmao3,
         pulmao4 LIKE zwm007-pulmao4,
         pulmao5 LIKE zwm007-pulmao5,
         pulmao6 LIKE zwm007-pulmao6,
         pulmao7 LIKE zwm007-pulmao7,
         pulmao8 LIKE zwm007-pulmao8,
         pulmao9 LIKE zwm007-pulmao9,
         pulmao10 LIKE zwm007-pulmao10,
         pulmao11 LIKE zwm007-pulmao11,
         pulmao12 LIKE zwm007-pulmao12,
         atribuida,
         peso.
  DATA : END OF l_portas_cargap.

** Contem as remessas associadas ao transporte
  DATA : BEGIN OF remessas OCCURS 0,
         vbeln LIKE likp-vbeln.
  DATA : END OF remessas.

** Items das remessas
  DATA : l_lips LIKE lips OCCURS 0 WITH HEADER LINE.

  DATA t_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.

  DATA : pulmao LIKE zwm007-pulmao1,
         num_quantos TYPE i,
         indice LIKE sy-tabix,
         incompleta,
         encontrou_porta,
         pulmao_aux LIKE lagp-lgber,
         encontrou_pulmao,
         num_pulmao TYPE i,
         s_tabix LIKE sy-tabix.


  DATA : BEGIN OF t_portas OCCURS 0.
          INCLUDE STRUCTURE zwm003_aux.
  DATA : END OF t_portas.
