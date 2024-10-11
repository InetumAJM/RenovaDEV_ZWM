FUNCTION-POOL zwmfunc11.                    "MESSAGE-ID ..

TYPE-POOLS: abap , zwm01, zrf01.

INCLUDE zwm_constants.

TYPES: BEGIN OF gty_data_consolidation,
         exidv    TYPE exidv,
         menge_l  TYPE menge_d, "Quantidade Lida
         lenum    TYPE lenum,
         menge    TYPE menge_d, "Quantidade a Ler
         menge_f  TYPE menge_d, "Quantidade em falta
         s_mlgn_f TYPE mlgn,
         s_vepo   TYPE vepo,
         s_stpo   TYPE stpo,
         s_lqua   TYPE lqua,
         s_mchb   TYPE mchb,
         index    TYPE sytabix,
       END OF gty_data_consolidation.

TYPES: gty_t_data_consolidation TYPE TABLE OF gty_data_consolidation.

TYPES: gty_t_mseg TYPE TABLE OF mseg.
