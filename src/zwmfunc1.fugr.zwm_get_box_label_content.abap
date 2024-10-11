FUNCTION ZWM_GET_BOX_LABEL_CONTENT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(SSCC) TYPE  EXIDV
*"  TABLES
*"      LABELS STRUCTURE  ZWM_PRINT_LABEL_DETAILS
*"----------------------------------------------------------------------
** Paulo Sousa 2012.02.28
** Função que devolve detalhes dos volumes contidos numa palete, para posterior impressão
** de labels com SSCC por caixa.

 tables: adrc, zsystemu.

 data: begin of it_vepo occurs 0.
         include structure VEPOVB.
 data: end of it_vepo.

 data: wa_vepo type HUM_HU_ITEM_T.
 data: wa_vekp  TYPE hum_hu_header_t.

 data: hu_VENUM type HUM_VENUM_T.
 data: sytabix like sy-tabix,
        boxes type i,
        actual type i,
        pedido like VBAK-BSTNK,
        zadrnr type ADRNR,
        encomenda type vbeln,
        item_enc type posnr, zkun like kna1-kunnr, gln(13) type c.

 DATA: t_makt LIKE makt OCCURS 0 WITH HEADER LINE.
 data: anterior like zwm_print_label_details-recebedor.

  refresh labels.
  select single * from vekp where exidv = sscc.

  refresh: hu_venum, it_vepo, labels.
  clear: hu_venum, it_vepo, labels.

  append vekp-venum to hu_venum.

  CALL FUNCTION 'HU_GET_HUS'
     EXPORTING
         IT_VENUM                 = hu_venum
     IMPORTING
         ET_HEADER                = wa_vekp
         ET_ITEMS                 = wa_vepo.

** Guardar em it_vepo entradas com P_MATERIAL nao nulo (SSCC de caixa)
  loop at wa_vepo into it_vepo.
       if it_vepo-p_material <> ''.
          append it_vepo.
       endif.
  endloop.

loop at it_vepo.
     move-corresponding vekp to labels.
     move-corresponding it_vepo to labels.

     labels-delivery = it_vepo-vbeln.
     labels-delivery_item = it_vepo-posnr.
     labels-lote = it_vepo-charg.
     labels-recebedor = it_vepo-spe_idplate.
     append labels.
  endloop.

  describe table labels lines boxes.
  actual = 0.
  loop at labels.
    sytabix = sy-tabix.
**  dados de documentos de vendas
    CALL FUNCTION 'Z_GET_ENCOMENDA_DA_REMESSA'
      EXPORTING
        REMESSA              = labels-delivery
        ITEM_REMESSA         = labels-delivery_item
     IMPORTING
       ENCOMENDA            = encomenda
*   TIPO_ENCOMENDA       =
       PEDIDO               = pedido
*   DATAPEDIDO           =
       ITEM_ENCOMENDA       = item_enc.

    labels-po_number = pedido.
    select single * from likp where vbeln = labels-delivery.
    labels-delivery_date = likp-lfdat.
** dados logisticos do material
    select single * from marm where matnr = labels-matnr and meinh = labels-vemeh.
    if sy-subrc = 0.
      labels-brgew = labels-vemng * marm-brgew.
      labels-ntgew = labels-brgew.
      labels-ntvol = labels-vemng * marm-volum.
      labels-matean = marm-ean11.
    endif.

** moradas
   clear zadrnr.
   select single adrnr into zadrnr from vbpa where vbeln = encomenda
                                               and posnr = item_enc
                                               and parvw = 'EN'.
   if sy-subrc <> 0.
       select single adrnr into zadrnr from vbpa where vbeln = encomenda
                                                   and posnr = item_enc
                                                   and parvw = 'W1'.
   endif.
   if sy-subrc = 0.
      select single * from adrc where addrnumber = zadrnr.
      if sy-subrc = 0.
         labels-we_name1 = adrc-name1.
         labels-we_name2 = adrc-name2.
         labels-we_name3 = adrc-name3.
         labels-we_name4 = adrc-name4.
         labels-we_street = adrc-street.
         labels-we_post_code1 = adrc-post_code1.
         labels-we_city1 = adrc-city1.
         select single landx into labels-country from t005t where spras = adrc-langu
                                                              and land1 = adrc-country.
       endif.
    endif.

** designacoes
    refresh t_makt.
    SELECT * INTO TABLE t_makt FROM makt WHERE matnr = labels-matnr.

    READ TABLE t_makt INDEX 1.
    IF sy-subrc = 0.
      labels-desc1 = t_makt-maktx.
    ENDIF.

    READ TABLE t_makt INDEX 2.
    IF sy-subrc = 0.
      labels-desc2 = t_makt-maktx.
    ENDIF.

    READ TABLE t_makt INDEX 3.
    IF sy-subrc = 0.
      labels-desc3 = t_makt-maktx.
    ENDIF.
    READ TABLE t_makt INDEX 4.
    IF sy-subrc = 0.
      labels-desc4 = t_makt-maktx.
    ENDIF.

** Coisas especificas do cliente System-U
    clear zkun.
    select single kunnr into zkun from vbpa where vbeln = encomenda
                                                   and posnr = item_enc
                                                   and parvw = 'EN'.
    if zkun <> ''.
       select single * from kna1 where kunnr = zkun.
       concatenate kna1-bbbnr kna1-bbsnr kna1-bubkz into gln.
       select single * from zsystemu where gln = gln.
       if sy-subrc = 0.
          concatenate 'Code Plateforme: ' zsystemu-code_platform into labels-lin1 separated by space.
          concatenate 'Code Associe: ' zsystemu-code_pdv into labels-lin2 separated by space.
          concatenate 'Jour de liv. PDV: ' zsystemu-jour_liv into labels-lin3 separated by space.
       endif.
    endif.
    modify labels index sytabix.
    sy-tabix = sytabix.
  endloop.

** Calcular total de boxes por recebedor, e numero de sequencia

  clear anterior.
  sort labels by recebedor.
  loop at labels.
     sytabix = sy-tabix.
     if labels-recebedor <> anterior.
**      determinar numero de volumes por destino
        anterior = labels-recebedor.
        actual = 0.
        boxes = 0.
        loop at labels where recebedor = anterior.
          boxes = boxes + 1.
        endloop.
     endif.
     sy-tabix = sytabix.
     read table labels index sytabix.
     actual = actual + 1.
    labels-boxnum = actual.
    labels-totalboxes = boxes.
    modify labels index sytabix.
  endloop.

ENDFUNCTION.
