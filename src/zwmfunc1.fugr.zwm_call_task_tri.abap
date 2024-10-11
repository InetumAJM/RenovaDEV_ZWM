FUNCTION zwm_call_task_tri .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"     REFERENCE(ECRAN) TYPE  CHAR4
*"     REFERENCE(TAB_ZWM011) LIKE  ZWM011 STRUCTURE  ZWM011
*"     REFERENCE(TIPO_QUEUE) TYPE  CHAR1
*"     VALUE(EQUIPAMENTO) TYPE  ZWM010-EQUIPAMENTO OPTIONAL
*"     VALUE(PRIMEIRA) TYPE  CHAR1 DEFAULT ' '
*"----------------------------------------------------------------------
  DATA : st_men LIKE lagp-lgtyp.

  CLEAR f3_activo.

  PERFORM user_own_data.

** Função que carrega tabelas de parametrização para memória
  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs = tab_zwm011-armazem.

** Dados de tabelas de parametrização


** Tipo de depósito das mensulas
  CLEAR : st_men.

  PERFORM get_parameter USING armazem
                          'ENTRADA_ARMAZEM'
                          'ST_MEN'
                           st_men.

*LIMPA os campos PARA TODOS OS ECRANS.
  CLEAR:to_tri,item_tri,su,su1,descricao,bin_origem,texto_processo,
        bin_destino,bin_destino1,equipamento_,store_destino,matnr,
        store_origem,bin_input,bin_output_d,bin_output_o,cursorfield,
        bin_output_kober_d.


  MOVE equipamento TO equipamento_.
  MOVE tipo_queue TO tipo_tri.

  CLEAR ltap.
*preenche os campos PARA TODOS OS ECRANS.
  SELECT SINGLE * FROM ltap
  WHERE lgnum = tab_zwm011-armazem AND
        tanum = tab_zwm011-to_number AND
        tapos = tab_zwm011-to_item.

  CHECK sy-subrc = 0.

  MOVE : ltap-nlenr           TO su1,
         ltap-vlpla           TO bin_origem,
         ltap-nlpla           TO bin_destino1,
         ltap-nltyp           TO store_destino,
         ltap-vltyp           TO store_origem,
         ltap-lgnum           TO armazem_tri,
         tab_zwm011-to_number TO to_tri,
         tab_zwm011-to_item   TO item_tri,
         ltap-maktx(20)       TO descricao,
         ltap-matnr           TO matnr.

  IF tipo_tri = 'E'.

    CLEAR zwm014.
    SELECT SINGLE * FROM zwm014
        WHERE armazem     = xuser-lgnum AND
            su = ltap-nlenr.
    IF sy-subrc <> 0.
      CLEAR zwm014.
      SELECT SINGLE * FROM zwm014
        WHERE armazem     = xuser-lgnum AND
              su_transito = ltap-nlenr.
      CHECK sy-subrc = 0.

    ENDIF.

*  é de entrada, logo origem = mensula
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = st_men
        lgpla = zwm014-mensula
      IMPORTING
        bin   = bin_output_o.

*o destino é o destino dado na TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_destino
        lgpla = bin_destino1
      IMPORTING
        bin   = bin_output_d.


  ELSEIF tipo_tri = 'S'.

*é de saida logo a origem é a origem da TO
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = store_origem
        lgpla = bin_origem
      IMPORTING
        bin   = bin_output_o.

*** Verificar se é remontada para colocar no campo da su a palete de
*** baixo

    SELECT SINGLE *
        FROM zwm020
            WHERE armazem = ltap-lgnum AND
                  ( p1 = ltap-vlenr OR
                    p2 = ltap-vlenr ).
    IF sy-subrc = 0.
      MOVE zwm020-p1 TO su1.
    ELSE.
** SU nas saidas
      MOVE ltap-vlenr TO su1.
    ENDIF.

*e o destino é a mensula
    CLEAR zwm014.
    SELECT SINGLE * FROM zwm014
        WHERE armazem     = xuser-lgnum AND
            su = su1.
    IF sy-subrc <> 0.
      CLEAR zwm014.
      SELECT SINGLE * FROM zwm014
        WHERE armazem     = xuser-lgnum AND
              su_transito = su1.
    ENDIF.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = st_men
        lgpla = zwm014-mensula
      IMPORTING
        bin   = bin_output_d.
  ENDIF.

  CLEAR bin_input.

  IF lrf_wkqu-devty(5) = '16X20'.
    PERFORM call_screen USING primeira '0003'.
  ELSE.
    PERFORM call_screen USING primeira '0004'.
  ENDIF.

ENDFUNCTION.
