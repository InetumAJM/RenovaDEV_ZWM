
*INCLUDE ZWMREP003TOP.    " global Data

**&SPWizard: Data incl. inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zwmrep0003top_v2.
**&SPWizard: Include inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zwmrepo003_v2 .
INCLUDE zwmrepi003_v2 .
INCLUDE zwmrep0003o01_v2.
INCLUDE zwmrep0003i01_v2.
INCLUDE zwmrep0003f01_v2.
INCLUDE zwmrep003o01_v2.
INCLUDE zwmrep003i01_v2.
INCLUDE zwmrep003f01_v2.

START-OF-SELECTION.
  PERFORM user_own_data.
*check se user esta ou nao associado ao armazem
  IF xuser-lgnum IS INITIAL.
    MESSAGE e003.
  ENDIF.

END-OF-SELECTION.

** Ecrã
***  SET SCREEN '0001'.
  SET SCREEN '9001'.
