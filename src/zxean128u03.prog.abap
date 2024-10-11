*&---------------------------------------------------------------------*
*&  Include           ZXEAN128U03                                      *
*&---------------------------------------------------------------------*

DATA: BEGIN OF aux_e128_ai_relation
         ,ai         TYPE t313daival
         ,aisub      TYPE t313daisub
         ,aiusage(1) TYPE c           "allowed values: gc_aiusage
         ,field      TYPE fieldname
         ,unitfield  TYPE fieldname
         ,unit       TYPE unit
      ,END  OF aux_e128_ai_relation.

IF is_ean128_spec-aityp = 'EAN128'.
*  aux_e128_ai_relation-ai = '91'.
*  aux_e128_ai_relation-aisub = 0.
*  aux_e128_ai_relation-aiusage = '12'.
*  aux_e128_ai_relation-field   = 'MATNR'.
*  INSERT aux_e128_ai_relation INTO TABLE ct_ean128_ai_relation.
*  aux_e128_ai_relation-ai = '20'.
*  aux_e128_ai_relation-aisub = 0.
*  aux_e128_ai_relation-aiusage = '12'.
*  aux_e128_ai_relation-field   = 'MATNR'.
*  INSERT aux_e128_ai_relation INTO TABLE ct_ean128_ai_relation.
*  aux_e128_ai_relation-ai = '333'.
*  aux_e128_ai_relation-aisub = 0.
*  aux_e128_ai_relation-aiusage = '12'.
*  aux_e128_ai_relation-field   = 'HOEHE'.
*  aux_e128_ai_relation-unitfield   = 'MEABM'.
*  INSERT aux_e128_ai_relation INTO TABLE ct_ean128_ai_relation.
ENDIF.


aux_e128_ai_relation-ai      = '240'.
aux_e128_ai_relation-aisub   = 0.
aux_e128_ai_relation-aiusage = '2'.
aux_e128_ai_relation-FIELD   = 'MATNR'.
INSERT aux_e128_ai_relation INTO TABLE ct_ean128_ai_relation.
