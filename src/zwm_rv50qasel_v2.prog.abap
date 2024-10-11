*----------------------------------------------------------------------*
*   INCLUDE RV50QASEL                                                  *
*----------------------------------------------------------------------*

************************************************************************
* selection screen
************************************************************************

data: if_uncha like mv50l-uncha.                            "AIP

ranges: it_bestk for vbuk-bestk,                            "AIP
        it_abeln for vbak-vbeln,
        it_ebeln for ekko-ebeln,
        it_vgsys for lips-vgsys,
        it_vlstk for likp-vlstk.

selection-screen begin of block orgdata with frame title text-c04.
select-options: it_vkorg   for  vepvg-vkorg.
select-options: it_vtweg   for  vepvg-vtweg.
select-options: it_spart   for  vepvg-spart.
*select-options: if_vstel   for  likp-vstel.
selection-screen end of block orgdata.

selection-screen begin of block create with frame title text-c10.
select-options: it_ernam   for  vbak-ernam.
select-options: it_erdat   for  vbak-erdat.
selection-screen end of block create.

*selection-screen begin of block timedata with frame title text-c09.
*select-options: it_kodat   for  likp-kodat.
*select-options: it_lddat   for  likp-lddat.
*select-options: it_tddat   for  likp-tddat.
*select-options: it_wadat   for  likp-wadat.
*select-options: it_wtist   for  likp-wadat_ist.
*select-options: it_lfdat   for  likp-lfdat.
*selection-screen end of block timedata.

*selection-screen begin of block komidata with frame title text-c01.
*parameters      if_fl_wm   like mv50l-kzfes no-display.
*parameters      if_fl_fi   like mv50l-kzlvs no-display.
*parameters      if_fl_be   like mv50l-kzkom default 'X' no-display.
*select-options  it_lgnum   for  likp-lgnum.
*parameters      if_lhead   like mv50l-kzhead no-display.
*parameters      if_lgpos like mv50l-kzlgpos default 'X' no-display.
*selection-screen begin of line.
*parameters      if_refnu   like mv50l-kzgwm.
*selection-screen comment (45) text-ref for field if_refnu.
*selection-screen end of line.
*select-options  it_lgort   for  lips-lgort.
*select-options  it_lgtor   for  likp-lgtor.
*selection-screen end of block komidata.

selection-screen begin of block salesord with frame title text-c08.
select-options: it_vbeln   for  vbak-vbeln matchcode object vmva.
select-options: it_auart   for  vbak-auart.
select-options: it_audat   for  vbak-audat.
selection-screen end of block salesord.

*selection-screen begin of block billdoc with frame title text-c09.
*select-options: it_vbelf   for  vbrk-vbeln matchcode object vmcf.
*select-options: it_fkart   for  vbrk-fkart.
*select-options: it_fkdat   for  vbrk-fkdat.
*parameters: if_billd as checkbox default 'X'.
*selection-screen end of block billdoc.

selection-screen begin of block groupcarg with frame title text-c06.
select-options: it_sammg   for  vbsk-sammg matchcode object vbsg.
selection-screen end of block groupcarg.

selection-screen begin of block material with frame title text-c13.
select-options: it_matnr   for  vbap-matnr.
select-options: it_charg   for  vbap-charg.                 "46A BEY
selection-screen end of block material.

*selection-screen begin of block produktion with frame title text-hu1.
*select-options: it_prvbe   for  likp-prvbe.
*select-options: it_aufnr   for  lips-aufnr.
*selection-screen end of block produktion.

selection-screen begin of block partner with frame title text-c07.
*select-options: it_spdnr   for  vepvg-spdnr matchcode object kred.
select-options: it_kunag   for  vepvg-kunnr matchcode object debi.
select-options: it_kunwe   for  vepvg-kunwe matchcode object debi.
select-options: it_kdgrp   for  knvv-kdgrp.
selection-screen end of block partner.

*selection-screen begin of block addpartner with frame title text-c12.
*parameters:     if_parvw   like vbpa-parvw.
**select-options: it_partn   for  lipov-partn.
*selection-screen end of block addpartner.

selection-screen begin of block transp with frame title text-c06.
*select-options: it_route   for  mv50l-route matchcode object vmtr.
*select-options: it_aulwe   for  likp-aulwe modif id aul.
*select-options: it_lstel   for  likp-lstel.
*select-options: it_ablad   for  likp-ablad.
*select-options: it_tknum   for  vttk-tknum.
*select-options: it_exidv   for  vekp-exidv.      "46A BEY
select-options: it_vsbed   for  vbak-vsbed.
*select-options: it_lprio   for  likp-lprio.
selection-screen end of block transp.

*selection-screen begin of block status with frame title text-c16.
*select-options: it_pkstk   for  vbuk-pkstk.
*select-options: it_kostk   for  vbuk-kostk.
*select-options: it_koquk   for  vbuk-koquk.
*select-options: it_fkstk   for  vbuk-fkstk.
*select-options: it_wbstk   for  vbuk-wbstk.
*select-options: it_lvstk   for  vbuk-lvstk.
*select-options: it_trsta   for  vbuk-trsta.
*selection-screen end of block status.

*selection-screen begin of block viewopt with frame title text-c11.
*parameters      if_refgw   like mv50l-ugew.
*parameters      if_refvo   like mv50l-uvol.
*parameters      if_item    like mv50l-posda default 'X'.
*parameters      if_anzpo   like mv50l-anzpos.
*parameters      if_spd_a   like mv50l-fa_display.
*selection-screen end of block viewopt.

*selection-screen begin of block handle with frame title text-c14.
*parameters      if_alakt   like  rl03t-alakt default 'X'.
*parameters      if_komim   like  rl03t-komim.
*selection-screen end of block handle.

*selection-screen begin of block message with frame title text-c02.
*select-options: i_kschl    for mv50l-kschl.
*select-options: i_kschln   for mv50l-na_kschl.
*select-options: i_kschl5   for mv50l-kschl_v5.
*selection-screen end of block message.
