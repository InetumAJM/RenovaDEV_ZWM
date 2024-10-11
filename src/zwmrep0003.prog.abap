
**&SPWizard: Data incl. inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE ZWMREP0003TOP .
INCLUDE ZWMREP003O01.  " PBO-Modules
INCLUDE ZWMREP003I01.  " PAI-Modules
INCLUDE ZWMREP003F01.  " FORM-Routines

START-OF-SELECTION.
  PERFORM USER_OWN_DATA.
*check se user esta ou nao associado ao armazem
  IF XUSER-LGNUM IS INITIAL.
    MESSAGE E003.
  ENDIF.
** Refresh automático
END-OF-SELECTION.

*** Ecrã
*  SET SCREEN '0001'.
