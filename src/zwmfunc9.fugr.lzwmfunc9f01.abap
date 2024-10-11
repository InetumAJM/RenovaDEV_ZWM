*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC9F01 .
*----------------------------------------------------------------------*

*{   INSERT         DEVK939519                                        1
*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WAREHOUSE  text
*      -->P_0035   text
*      -->P_0036   text
*      -->P_HOST   text
*----------------------------------------------------------------------*
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

  DATA: ls_zwm001 TYPE zwm001.

  SELECT SINGLE *
    FROM zwm001 INTO ls_zwm001
    WHERE armazem   = whs    AND
          processo  = module AND
          parametro = param.

  IF sy-subrc = 0.
    MOVE ls_zwm001-valor TO valor.
  ELSE.
    CLEAR valor.
  ENDIF.

ENDFORM.                    " GET_PARAMETER
*}   INSERT
