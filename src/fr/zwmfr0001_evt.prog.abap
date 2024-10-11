*----------------------------------------------------------------------*
* Include: ZWMFR0001_EVT
*----------------------------------------------------------------------*
* Description: Monitor de planeamento do abastecimento à produção
*----------------------------------------------------------------------*
* Author........: [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date: 2015-10-23
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_set_soptions.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layo.
  PERFORM f_get_layout CHANGING p_layo.

START-OF-SELECTION.
  PERFORM f_data_free USING abap_false.
  PERFORM f_data_init USING abap_false.
  PERFORM f_data_query.
  PERFORM f_data_prepare.

END-OF-SELECTION.
  PERFORM f_data_display.
