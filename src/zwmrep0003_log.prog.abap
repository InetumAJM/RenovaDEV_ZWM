*&---------------------------------------------------------------------*
*& Report  ZWMREP0003_LOG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZWMREP0003_LOG LINE-SIZE 245 NO STANDARD PAGE HEADING..

tables: zwm006_aux, vttk, lfa1.

SELECTION-SCREEN: BEGIN OF BLOCK selecione WITH FRAME TITLE text-001.




SELECT-OPTIONS: p_talao for zwm006_aux-num_entrada,
                p_tknum FOR zwm006_aux-n_transporte,
                p_arm   for zwm006_aux-num_entrada DEFAULT '100'.
SELECTION-SCREEN skip.
SELECTION-SCREEN END OF BLOCK selecione.



SELECTION-SCREEN BEGIN OF BLOCK procurar WITH FRAME TITLE text-002.



SELECT-OPTIONS:                p_tdlnr for vttk-tdlnr.

parameters: p_desde type datum default sy-datum,
            p_ate   type datum  default sy-datum.
parameters: p_search type char20.

SELECTION-SCREEN END OF BLOCK procurar.


data: lv_search  type string,
      cursor(20).

data: BEGIN OF lista occurs 1.
        include: structure zwm006_aux.
        data: tdlnr type tdlnr,
        name1 type name1_gp.
data: END OF lista.

if p_talao is not initial.
  " Descargas
  select * from zwm005
    APPENDING CORRESPONDING FIELDS OF TABLE lista where num_entrada in p_talao.
  " Cargas
  select * from zwm006_aux INNER JOIN vttk on zwm006_aux~n_transporte = vttk~tknum
    APPENDING CORRESPONDING FIELDS OF TABLE lista where num_entrada in p_talao.
endif.
if p_tknum is not initial.

  select * from zwm006_aux INNER JOIN vttk on zwm006_aux~n_transporte = vttk~tknum
    APPENDING CORRESPONDING FIELDS OF TABLE lista where n_transporte in p_tknum.
endif.


if p_talao is initial and p_tknum is initial.

  if p_search is initial.
    lv_search = '%'.
  else.
    concatenate '%' p_search '%' into lv_search.
  endif.
  TRANSLATE lv_search to UPPER CASE.

  select * from zwm006_aux INNER JOIN vttk on zwm006_aux~n_transporte = vttk~tknum
                           INNER JOIN lfa1 on vttk~tdlnr = lfa1~lifnr
    APPENDING CORRESPONDING FIELDS OF TABLE lista where
              vttk~tdlnr in p_tdlnr
          and zwm006_aux~armazem in p_arm
          and data_reg >= p_desde
          and data_reg <= p_ate
    and ( ( matricula like lv_search ) OR ( observacoes like lv_search ) OR ( observacoes2 like lv_search ) ).
endif.


perform display_lista.

at LINE-SELECTION.
  get cursor field cursor.
  if cursor = 'LISTA-NUM_ENTRADA'.
    " Vizualizar talao
    submit zwmrep0004_pdf with p_talao = lista-num_entrada AND RETURN.
  endif.



form display_lista.

  data: wa         type zwm006_aux,
        hh         type char2,
        mm         type char2,
        hin        type char5,
        hout       type char5,
        pesini(5)  type p decimals 0,
        pesfim(5)  type p decimals 0,
        difkg(5)   type p decimals 0,
        difp(4)    type p decimals 1,
        absdifp(4) type p decimals 1.

  constants: maxcol type i value 194.

  sort lista by num_entrada DESCENDING.

  uline at /(maxcol).
  format color col_heading INTENSIFIED on.
  write:/ '| Talão | Entrada          | Saida | Transporte | Transportador', 98'| Matricula', 120' | Contacto', 144'| KG Entrada', 157'| KG Saída', 170'| Dif. (KG)  | Dif. (%)', 194'|'.
  uline at /(maxcol).

  loop at lista. "into wa.
    format color col_normal INTENSIFIED off.
    hh = lista-hora_reg(2).
    mm = lista-hora_reg+2(2).
    concatenate: hh mm into hin separated by ':'.

    hh = lista-hora_saida(2).
    mm = lista-hora_saida+2(2).
    concatenate: hh mm into hout separated by ':'.

    write:/ '|'.
    format color col_key INTENSIFIED on.
    write: lista-num_entrada, '|'.
    format color col_normal INTENSIFIED off.

    pesini = lista-pesini.
    pesfim = lista-pesfim.
    difkg = lista-difkg.
    difp = lista-difp.

    write: lista-data_reg, hin, '|', hout, '|', lista-n_transporte, '|', lista-tdlnr, lista-name1, '|', lista-matricula, '|', lista-observacoes, '|', pesini, '|', pesfim, '|', difkg, '|'.
    absdifp = abs( difp ).
    if absdifp >= 5.
      format color col_total INTENSIFIED off.
    endif.
    write: difp, 194'|'.
    format color col_normal INTENSIFIED off.
    hide lista.
  endloop.
  uline at /(maxcol).
endform.
