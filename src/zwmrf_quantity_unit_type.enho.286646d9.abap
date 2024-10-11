"Name: \PR:SAPLL03A\FO:MENGE_PRO_LETYP_BESTIMMEN\SE:BEGIN\EI
ENHANCEMENT 0 ZWMRF_QUANTITY_UNIT_TYPE.

 DATA: LF_KZFME1 LIKE RL03T-KZFME,
       LF_CHARG1 LIKE LTAP-CHARG,
       LF_LHMGE1 LIKE MLVS-LHMG1.

  CLEAR: P_SUBRC,
         P_LHMGE,
         P_LHMEI.

  IF P_LETYP IS INITIAL.
    P_SUBRC = R_LETYP_UNBEKANNT.
    EXIT.
  ENDIF.

constants: lc_anfml1(22)  type c          value '()RL03T-ANFML'.

field-symbols: <lv_anfml1>.

" Armazém Automático (WCS) - Excepto Movimentos de Reabastecimentos
IF t331-lgnum = '100' AND t331-lgtyp = 'AUT' AND ( ltak-bwlvs <> '972' AND ltak-bwlvs <> '979' ).
  if t331-lenvw = con_x and t331-stein = con_stein_b.     "mod_550889

*   Set SUT
    i_mlvs-lety1 = p_letyp.

    assign (lc_anfml1) to <lv_anfml1>.

    if <lv_anfml1> <> space.
*     Processing has been invoked from a normal on-line transaction such
*     as LT03 therefore the pick qty should be obtained from the screen.
      i_mlvs-lhmg1 = <lv_anfml1>.

    elseif ltap-vsolm <> space.
*     Processing has been invoked from an RF transaction.

      i_mlvs-lhmg1 = ltap-vsolm.

    elseif i_rl03a-anfml <> space.
*     Processing has been invoked from a non RF but still background
*     transaction - such as LT10.
      i_mlvs-lhmg1 = i_rl03a-anfml.
    endif.
  endif.                                                  "mod_550889
endif.

  CASE P_LETYP.
    WHEN I_MLVS-LETY1. P_LHMNG = I_MLVS-LHMG1.
    WHEN I_MLVS-LETY2. P_LHMNG = I_MLVS-LHMG2.
    WHEN I_MLVS-LETY3. P_LHMNG = I_MLVS-LHMG3.
    WHEN OTHERS.     P_SUBRC = R_LETYP_UNBEKANNT.
  ENDCASE.

*........Produktmengeneinheiten.........................................

  IF I_MLVS-KZWSM EQ CON_KZWSM_PROD.

    CASE P_LETYP.
      WHEN I_MLVS-LETY1.
        If I_MLVS-LHME1 <> LTAP-ALTME OR
           LTAP-ALTME    = LTAP-MEINS.
          P_LHMEI = LTAP-MEINS.
          P_LHMGE = P_LHMNG.
          EXIT.
        ENDIF.

        PERFORM ALTME_PRUEFEN(SAPFL000) USING I_MLVS-LHME1
                                                 HLP_UMREZ
                                                 HLP_UMREN
                                                 I_MLVS IT340D
                                                 CON_TRUE  LF_KZFME1.
        IF LF_KZFME1 = CON_KZFME_PROD.
          PERFORM UMRECHNEN_MEINS_ALTME(SAPFL000) USING
                                               LTAP-MATNR
                                               LTAP-WERKS   LF_CHARG1
                                               I_MLVS-LHMG1 LTAP-MEINS
                                               LF_LHMGE1    I_MLVS-LHME1
                                               LTAP-UMREZ   LTAP-UMREN.
          P_LHMGE = LF_LHMGE1.
          P_LHMEI = I_MLVS-LHME1.
        ELSE.
          P_LHMEI = LTAP-MEINS.
        ENDIF.

      WHEN I_MLVS-LETY2.
        If I_MLVS-LHME2 <> LTAP-ALTME OR
           LTAP-ALTME    = LTAP-MEINS.
          P_LHMEI = LTAP-MEINS.
          P_LHMGE = P_LHMNG.
          EXIT.
        ENDIF.

        PERFORM ALTME_PRUEFEN(SAPFL000) USING I_MLVS-LHME2
                                                 HLP_UMREZ
                                                 HLP_UMREN
                                                 I_MLVS IT340D
                                                 CON_TRUE  lf_KZFME1.
        IF LF_KZFME1 = CON_KZFME_PROD.
          PERFORM UMRECHNEN_MEINS_ALTME(SAPFL000) USING
                                              LTAP-MATNR
                                              LTAP-WERKS   LF_CHARG1
                                              I_MLVS-LHMG2 LTAP-MEINS
                                              LF_LHMGE1    I_MLVS-LHME2
                                              LTAP-UMREZ   LTAP-UMREN.

          P_LHMGE = LF_LHMGE1.
          P_LHMEI = I_MLVS-LHME2.
        ELSE.
          P_LHMEI = LTAP-MEINS.
        ENDIF.

      WHEN I_MLVS-LETY3.
        If I_MLVS-LHME3 <> LTAP-ALTME OR
           LTAP-ALTME    = LTAP-MEINS.
          P_LHMEI = LTAP-MEINS.
          P_LHMGE = P_LHMNG.
          EXIT.
        ENDIF.

        PERFORM ALTME_PRUEFEN(SAPFL000) USING I_MLVS-LHME3
                                                 HLP_UMREZ
                                                 HLP_UMREN
                                                 I_MLVS IT340D
                                                 CON_TRUE  lf_KZFME1.
        IF LF_KZFME1 = CON_KZFME_PROD.
          PERFORM UMRECHNEN_MEINS_ALTME(SAPFL000) USING
                                              LTAP-MATNR
                                              LTAP-WERKS   LF_CHARG1
                                              I_MLVS-LHMG3 LTAP-MEINS
                                              LF_LHMGE1    I_MLVS-LHME3
                                              LTAP-UMREZ   LTAP-UMREN.

          P_LHMGE = LF_LHMGE1.
          P_LHMEI = I_MLVS-LHME3.
        ELSE.
          P_LHMEI = LTAP-MEINS.
        ENDIF.


    ENDCASE.
  ENDIF.

 EXIT.

ENDENHANCEMENT.
