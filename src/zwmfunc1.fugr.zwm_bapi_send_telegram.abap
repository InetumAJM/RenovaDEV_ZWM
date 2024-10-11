FUNCTION zwm_bapi_send_telegram.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(SSCC) TYPE  LENUM
*"     REFERENCE(MESA) TYPE  CHAR14
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
  DATA : mat_struct LIKE zwm_material OCCURS 0 WITH HEADER LINE,
         x_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE,
         result_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  DATA: to LIKE ltak-tanum,
        mov LIKE ltak-bwlvs,
        st_type TYPE lgtyp,
        plant LIKE mard-werks,
        lgort LIKE mard-lgort,
        letyp LIKE lein-letyp,
        su_invalida(1),
        lgtyp LIKE lagp-lgtyp,
        lgpla LIKE lagp-lgpla,
        certificado LIKE ltap-zeugn.

* check se user esta ou nao associado ao armazem
  PERFORM user_own_data.
  IF xuser-lgnum IS INITIAL.
    return-type = 'E'.
    return-id = 'ZWMMSG001'.
    return-number = '999'.
    return-message_v1 = 'User não esta no armazem'.
    APPEND return.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs = xuser-lgnum.


  PERFORM get_parameter USING xuser-lgnum
                              'ENTRADA_ARMAZEM'
                              'MOV_WM'
                              mov.

  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = mesa
    IMPORTING
      lgtyp = lgtyp
      lgpla = lgpla.

  SELECT SINGLE lzone INTO certificado
    FROM lagp WHERE lgnum = xuser-lgnum AND
                    lgtyp = lgtyp AND
                    lgpla = lgpla.


* verificar se é palete remontada
  SELECT SINGLE *
      FROM zwm020
          WHERE armazem = xuser-lgnum AND
                p1 = sscc.
* Palete Remontada
  IF sy-subrc = 0.

    SELECT SINGLE p~vemng p~vemeh p~matnr
                    p~werks p~lgort p~charg
          INTO (x_sscc-quantidade, x_sscc-uni, x_sscc-material,
                plant, lgort, x_sscc-lote_producao)
             FROM vekp AS k INNER JOIN vepo AS p
                  ON  k~venum = p~venum
                      WHERE  p~vepos = '000001' AND
                             k~exidv = zwm020-p1.

    x_sscc-sscc = zwm020-p1.
    SELECT SINGLE tipo_palete INTO x_sscc-tipo_su
        FROM zwm013
          WHERE armazem = xuser-lgnum AND
                sscc = zwm020-p1.
    APPEND x_sscc.
    CLEAR x_sscc.

    SELECT SINGLE p~vemng p~vemeh p~matnr
                   p~werks p~lgort p~charg
       INTO (x_sscc-quantidade, x_sscc-uni, x_sscc-material,
              plant, lgort,x_sscc-lote_producao)
          FROM vekp AS k INNER JOIN vepo AS p
               ON  k~venum = p~venum
                   WHERE  p~vepos = '000001' AND
                          k~exidv = zwm020-p2.

    x_sscc-sscc = zwm020-p2.
    SELECT SINGLE tipo_palete INTO x_sscc-tipo_su
        FROM zwm013
            WHERE armazem = xuser-lgnum AND
                  sscc = zwm020-p1.
    APPEND x_sscc.
    CLEAR x_sscc.

* criar TO
    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse   = xuser-lgnum
        mov_type    = mov
        st_type_o   = st_type
        plant       = plant
        s_loc       = lgort
        certificado = certificado
        req_number  = lgpla
        req_type    = 'E'
      IMPORTING
        to          = to
      TABLES
        return_msg  = result_msg
        sscc        = x_sscc
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      return-type = 'E'.
      return-id = 'ZWMMSG001'.
      return-number = '999'.
      return-message_v1 = 'Erro na criacao da to '.
      APPEND return.
      EXIT.
    ELSE.
*       verificar se a zona ou armazem cheio.
      SELECT SINGLE *
        FROM ltap
          WHERE lgnum = xuser-lgnum AND tanum = to.

      IF ltap-nltyp = 'CMA' AND ltap-nlpla = 'C-MAX'.
        return-type = 'E'.
        return-id = 'ZWMMSG001'.
        return-number = '088'.
        APPEND return.
      ENDIF.
    ENDIF.

* Não é Palete Remontada
  ELSE.
    SELECT SINGLE p~vemng p~vemeh p~matnr p~charg
                    p~werks p~lgort
              INTO (x_sscc-quantidade, x_sscc-uni, x_sscc-material,
                    x_sscc-lote_producao, plant, lgort)
                 FROM vekp AS k INNER JOIN vepo AS p
                      ON  k~venum = p~venum
                          WHERE  p~vepos = '000001' AND
                                 k~exidv = sscc.

    x_sscc-sscc = sscc.
    SELECT SINGLE tipo_palete INTO x_sscc-tipo_su
        FROM zwm013
          WHERE armazem = xuser-lgnum AND
                sscc = sscc.
    APPEND x_sscc.
    CLEAR x_sscc.

    READ TABLE x_sscc INDEX 1.
    mat_struct-material = x_sscc-material.
    mat_struct-menge = x_sscc-quantidade.
    mat_struct-meins = x_sscc-uni.
    mat_struct-charg = x_sscc-lote_producao.
    APPEND mat_struct.


    SELECT SINGLE *
       FROM marm
           WHERE matnr = x_sscc-material AND
                 meinh = x_sscc-tipo_su.

    IF x_sscc-quantidade <> marm-umrez.
*    a palete é incompleta
      PERFORM get_parameter USING xuser-lgnum
                               'ENTRADA_ARMAZEM'
                               'ST_REP'
                               st_type.
    ENDIF.

    CALL FUNCTION 'ZWM_CREATE_STORAGE_UNIT'
      EXPORTING
        warehouse   = xuser-lgnum
        mov_type    = mov
        st_type     = st_type
        plant       = plant
        s_loc       = lgort
        su_type     = x_sscc-tipo_su
        certificado = certificado
        mat_struct  = mat_struct
        req_number  = lgpla
        req_type    = 'E'
      IMPORTING
        to          = to
      TABLES
        result_msg  = result_msg
      CHANGING
        su_number   = x_sscc-sscc
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc <> 0.
      return-type = 'E'.
      return-id = 'ZWMMSG001'.
      return-number = '999'.
      return-message_v1 = 'Erro na criacao da to '.
      APPEND return.
      EXIT.
    ELSE.
*       verificar se a zona ou armazem cheio.
      SELECT SINGLE *
        FROM ltap
          WHERE lgnum = xuser-lgnum AND tanum = to.

      IF ltap-nltyp = 'CMA' AND ltap-nlpla = 'C-MAX'.
        return-type = 'E'.
        return-id = 'ZWMMSG001'.
        return-number = '088'.
        APPEND return.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
