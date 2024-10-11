"Name: \PR:SAPLL03B\FO:TBP_BEARBEITEN\SE:END\EI
ENHANCEMENT 0 ZWMFR_001.
*& Begin of Modification by Tiago Pateiro - ROFF @ 21.01.2016 14:28:01
*/ Passar valor do ZEUGN para TO - RF ZWMFR0002C
  LOOP AT ct_tbp[] ASSIGNING <fs_tbp>.
    CLEAR ls_trite.

    READ TABLE it_trite[] INTO ls_trite
      WITH KEY tbpos = <fs_tbp>-tbpos
      BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    <fs_tbp>-zeugn  = ls_trite-zzzeugn.
  ENDLOOP.
*& End of Modification by Tiago Pateiro - ROFF @ 21.01.2016 14:28:17
ENDENHANCEMENT.
