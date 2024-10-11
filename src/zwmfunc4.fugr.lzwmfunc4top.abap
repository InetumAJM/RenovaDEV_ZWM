FUNCTION-POOL zwmfunc4.                     "MESSAGE-ID ..

TYPE-POOLS: abap.

TABLES: mean,
        marm,
        marc,
        mara,
        makt,
        mseg,
        mlgn,
        afpo,
        afko,
        vekp,
        vepo,
        ltak,
        ltap,
        mch1,
        t100,
        lein,
        zwm_log_efacec,
        zwm001,
        zwm009,
        zwm013,      "Tabela que guarda os bloqueios dos documentos
        zwm020,      "Paletes Remontadas
        zwm030,      "Tabela de Controlo para Interface de Produção
        zwm043,      "Latout de etiqueta por produto
        zwm_if_prod. "Estrutura para Interface de Produção

DATA: gt_zwm030   LIKE zwm030 OCCURS 0 WITH HEADER LINE,
      gs_zwm030   LIKE zwm030,
      gt_registo  LIKE zwm_if_prod,
      wa_log      LIKE zwm_log_efacec,
      l_msg       LIKE zwm_log_efacec-msg,
      itab_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gs_reg,
        matnr      LIKE mara-matnr,
        aufnr      LIKE afko-aufnr,
        charg      LIKE afpo-charg,
        lgnum      LIKE mseg-lgnum,
        bwart      LIKE mseg-bwart,
        mblnr      LIKE mkpf-mblnr,
        mjahr      LIKE mkpf-mjahr,
        werks      LIKE mseg-werks,
        lgort      LIKE mseg-lgort,
        pack       LIKE mara-matnr,
        code       LIKE bapi2017_gm_code,
        hukey1     LIKE bapihukey-hu_exid,
        hukey2     LIKE bapihukey-hu_exid,
        quantidade LIKE zwm_if_prod-quantidade,
        unidade    LIKE mara-meins,
        maktx      LIKE makt-maktx,
        makt2      LIKE makt-maktx,
        makt3      LIKE makt-maktx,
        vfdat      LIKE mch1-vfdat,
        meins      LIKE mara-meins,
        end_str(1),
      END OF gs_reg.

DATA: g_hukey      LIKE bapihukey-hu_exid,
      g_matnr(18),
      g_quantidade(4).

DATA: w_num LIKE sy-tabix,
      w_subrc LIKE sy-subrc,
      w_retorno LIKE sy-subrc,
      g_mblnr LIKE mseg-mblnr,
      g_mjahr LIKE mseg-mjahr,
      x_mblnr LIKE mseg-mblnr,
      x_mjahr LIKE mseg-mjahr,
      l_porta TYPE ablad.

DATA : BEGIN OF return_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF return_msg.
