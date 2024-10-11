"Name: \PR:SAPLL03B\FO:DATEN_LTAK_TA_MANUELL\SE:END\EI
ENHANCEMENT 0 ZWM_003.

DATA: lv_refnr TYPE lvs_refnr,
      lv_l2ska TYPE LTAK_L2SKA.

DATA: ls_t311 TYPE t311.

FIELD-SYMBOLS: <lv_group> TYPE ANY,
               <lv_l2ska> TYPE ANY.

DO 1 TIMES.
** Indicação de Grupo
  ASSIGN ('I_REFNR')  TO <lv_group>.
  CHECK <lv_group> IS ASSIGNED.

  lv_refnr = <lv_group>.
  CLEAR <lv_group>.

  CHECK NOT lv_refnr IS INITIAL AND ltak-refnr IS INITIAL.

  ltak-refnr = lv_refnr.

** Indicação de Pck em 2 passos
  ASSIGN ('I_L2SKA')  TO <lv_l2ska>.
  CHECK <lv_l2ska> IS ASSIGNED.

  lv_l2ska = <lv_l2ska>.
  CLEAR <lv_l2ska>.

  CHECK NOT lv_l2ska IS INITIAL.

  ltak-l2ska = lv_l2ska.
ENDDO.

ENDENHANCEMENT.
