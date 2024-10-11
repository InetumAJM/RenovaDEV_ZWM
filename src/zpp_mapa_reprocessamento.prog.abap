*----------------------------------------------------------------------
* Report      : Z02RP_RP_REPDITA_V2
* Description : Mapa Reprocessamento DITA
* ---------------------------------------------------------------------
* Author: Patrícia Alves / João C. Oliveira
* User: ROFFD
* Request: RENSAF00022 / RNFROL00005
* Creation date: 12.08.2015
* TR: DEVK922313
*&---------------------------------------------------------------------*
* NOTA: Para eventuais alterações verificar os REMARKS (comentário no código)

INCLUDE zpp_mapa_reprocessamento_top.
INCLUDE zpp_mapa_reprocessamento_cls.
INCLUDE zpp_mapa_reprocessamento_scr.
INCLUDE zpp_mapa_reprocessamento_o01.
INCLUDE zpp_mapa_reprocessamento_i01.
INCLUDE zpp_mapa_reprocessamento_f01.
INCLUDE <icon>.

* ---------------------------------------------------------------------
* START-OF-SELECTION
* ---------------------------------------------------------------------
START-OF-SELECTION.
  CLEAR lv_erro.
  PERFORM check_werks_screen.
  PERFORM check_area_screen.
  PERFORM check_linha_screen.
  PERFORM check_area.

  CHECK lv_erro <> 'X'.
  PERFORM get_sessoes.

* ---------------------------------------------------------------------
* END-OF-SELECTION
* ---------------------------------------------------------------------

  IF gt_z02rpsessaodita[] IS NOT INITIAL.
    PERFORM fill_output_alv.

    IF gt_sessaodita[] IS NOT INITIAL.
      CALL SCREEN '0400'.
    ELSE.
      MESSAGE text-054 TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE text-054 TYPE 'E'.
  ENDIF.

END-OF-SELECTION.
