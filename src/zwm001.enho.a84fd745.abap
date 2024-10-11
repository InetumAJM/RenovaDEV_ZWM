"Name: \PR:SAPML05S\FO:T311_LESEN\SE:END\EI
ENHANCEMENT 0 ZWM001.



* Referenznummer ist bereits gedruckt
  IF T311-L2SKR EQ CON_REFNR_NO_REL.
    IF T311-KZDRU NE SPACE.
    leave to SCREEN 0.
    ENDIF.
  ELSE.
* Bei 2-stufiger Kommi
    IF T311-KZDRU NE SPACE.
    leave to SCREEN 0.
    ELSE.
* Die Prüfung für die einzelnen Schritte der 2-stufigen Kommissionierung
* erfolgt im  FB: 'l_ref_multiple_process_release', da dort auch
*                  Selektion nach Aufteilung/Direkt erfolgt
    ENDIF.
  ENDIF.

* Referenznummer hat Sperr-KZ gesetzt
  IF T311-KZSPE NE SPACE.
    leave to SCREEN 0.
  ENDIF.

*........Beim Start eines Sammelgangs:.................................
*........Fehler, wenn Referenznummer erledigt...........................
*........Warnung, wenn Referenznummer bereits aktiv.....................

  IF VORGA <> CON_VORGA_FR AND NOT T311-KZERL IS INITIAL.
    leave to SCREEN 0.
  ENDIF.

  IF T311-KZAKT NE SPACE AND VORGA <> CON_VORGA_FR.
    leave to SCREEN 0.
  ENDIF.

*........Bei der Freigabe eines Sammelgangs: Prüfen, daß die Referenz-..
*........nummer aktiv ist. .............................................

  IF T311-KZAKT IS INITIAL AND VORGA = CON_VORGA_FR.
    leave to SCREEN 0.
  ENDIF.


ENDENHANCEMENT.
