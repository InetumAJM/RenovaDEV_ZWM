class ZCL_WM_UTILS definition
  public
  final
  create public .

public section.

  types:
    ty_lvs_letyp_range TYPE RANGE OF lvs_letyp .

  class-methods GET_T334E_LETYP_RANGE
    importing
      !LGNUM type T334E-LGNUM default '100'
      !LGTYP type T334E-LGTYP default 'AUT'
    returning
      value(R_LETYP_RANGE) type TY_LVS_LETYP_RANGE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WM_UTILS IMPLEMENTATION.


METHOD get_t334e_letyp_range.

*  DATA lr_lety TYPE RANGE OF mlgn-lety1.
  REFRESH r_letyp_range.
  SELECT SINGLE *
    FROM t334e
    INTO @DATA(ls_t334e)
    WHERE lgnum EQ '100'
      AND lgtyp EQ 'AUT'.
  DATA lv_index TYPE sy-tabix VALUE 3.
  DO .
    ADD 1 TO lv_index.
    ASSIGN COMPONENT lv_index
      OF STRUCTURE ls_t334e
        TO FIELD-SYMBOL(<lfs_lety>).
    IF sy-subrc IS NOT INITIAL OR
       <lfs_lety> IS NOT ASSIGNED OR
       <lfs_lety> IS INITIAL.
      EXIT.
    ENDIF.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <lfs_lety> ) TO r_letyp_range.
  ENDDO.

ENDMETHOD.
ENDCLASS.
