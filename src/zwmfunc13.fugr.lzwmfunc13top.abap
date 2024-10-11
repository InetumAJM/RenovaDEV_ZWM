FUNCTION-POOL zwmfunc13 MESSAGE-ID zwm001.

* INCLUDE LZWMFUNC13D...                     " Local class definition
*** Types and Table Types
TYPES: BEGIN OF ty_zwmt001,
         processo  TYPE zwmmpt001-processo,
         parametro TYPE zwmmpt001-parametro,
         item      TYPE zwmmpt001-item,
         valor     TYPE zwmmpt001-valor,
       END OF ty_zwmt001.
TYPES: ty_st_zwmt001  TYPE SORTED TABLE OF ty_zwmt001 WITH UNIQUE KEY processo parametro item.

*** Internal Tables
DATA gt_zwmt001     TYPE ty_st_zwmt001.

*** Ranges

*** Work Areas

*** Field Symbols

*** Variables

*** Object Instances

*** OLE Objects

**********************************************************************
* Z_WMFR_WS_ASYNCHRONOUS_LITE
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Constants
CONSTANTS c_edi_partyp_ls         TYPE edi4sndprt             VALUE 'LS'.
CONSTANTS c_process_edi_wm_auto   TYPE zwmmpt001-processo     VALUE 'AUTOMATICO_EDI'.
CONSTANTS c_param_edi_wm_sndpor   TYPE zwmmpt001-parametro    VALUE 'LOG_SYS'.
CONSTANTS c_param_edi_wm_rcvpor   TYPE zwmmpt001-parametro    VALUE 'ENVIRONMENT'.
CONSTANTS c_balobj_zwm_pt         TYPE balobj_d               VALUE 'ZWM_FR'.
CONSTANTS c_balsubobj_interface   TYPE balsubobj              VALUE 'INTERFACE'.
