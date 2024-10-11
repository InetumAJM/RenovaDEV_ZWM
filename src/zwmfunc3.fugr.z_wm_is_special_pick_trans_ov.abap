FUNCTION z_wm_is_special_pick_trans_ov.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"     REFERENCE(I_TKNUM) TYPE  TKNUM
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_zwm074 TYPE zwm074.

  DATA: lv_sdabw TYPE sdabw,
        lv_vkorg TYPE vkorg.

** Desbloqueio Por Ordem de Venda e Tipo de Carro
***********************************************************************
  DO  1 TIMES.
    CHECK NOT i_tknum IS INITIAL.


    IF NOT i_vbeln IS INITIAL.

      SELECT SINGLE vkorg FROM likp
                          INTO lv_vkorg
                          WHERE vbeln = i_vbeln.

    ELSE.

      SELECT SINGLE vkorg FROM likp AS a
                          INNER JOIN t311a AS b ON a~vbeln = b~rbnum
                          INTO lv_vkorg
                          WHERE b~lgnum = i_lgnum AND
                                b~refnr = i_refnr.

    ENDIF.

    CHECK sy-subrc EQ 0.
    CHECK NOT lv_vkorg IS INITIAL.

    SELECT SINGLE sdabw FROM vttk
                        INTO lv_sdabw
                        WHERE tknum = i_tknum.
    CHECK sy-subrc EQ 0.
    CHECK NOT lv_sdabw  IS INITIAL.

    SELECT SINGLE * FROM zwm074
                    INTO ls_zwm074
                    WHERE lgnum = i_lgnum AND
                          vkorg = lv_vkorg AND
                          sdabw = lv_sdabw.
    CHECK sy-subrc EQ 0.

    RETURN.
  ENDDO.


  RAISE error.
ENDFUNCTION.
