class ZCL_IM_WM_CL_DELIVERY definition
  public
  final
  create public .

*"* public components of class ZCL_IM_WM_CL_DELIVERY
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_SMOD_V50B0001 .
protected section.
*"* protected components of class ZCL_IM_WM_CL_DELIVERY
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_WM_CL_DELIVERY
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_WM_CL_DELIVERY IMPLEMENTATION.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50I_001.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50I_002.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50I_003.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50I_004.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50I_009.
endmethod.


METHOD if_ex_smod_v50b0001~exit_saplv50i_010.
  DATA: ls_zwm_007    TYPE zwm_007,
        ls_extension1 TYPE bapiextc.

  READ TABLE extension1
        INTO ls_extension1
        INDEX 1.

  CHECK sy-subrc EQ 0.

  ls_zwm_007 = ls_extension1-field1.

  cs_vbkok-lifex = ls_zwm_007-lifex.
ENDMETHOD.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50K_005.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50K_006.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50K_007.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50K_008.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50K_011.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50K_012.
endmethod.


method IF_EX_SMOD_V50B0001~EXIT_SAPLV50K_013.
endmethod.
ENDCLASS.
