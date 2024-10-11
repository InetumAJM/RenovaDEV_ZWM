REPORT zrm08nast.

INCLUDE mrm_types_nast.
INCLUDE mrm_const_nast.
INCLUDE fv61bf00.      "Routinen zur Symbolersetzung bei Mailnachrichten

* implizite Übergabe beim Aufruf der Verarbeitungsproramme
INCLUDE rvadtabl.

* Feldleisten für Symbolersetzung bei Mail
TABLES: rbkp,
        rseg,
        lfa1,
        addr3_val,
        vf_kred,
        drseg.

*---------------------------------------------------------------------*
*       FORM ENTRY_ERS                                                *
*---------------------------------------------------------------------*
*       Ausgabe eines "ERS-Schreibens" der Logistik-Rechnungsprüfung
*       (ab Release 4.0) mittels eines NAST-Satzes, "getriggert" von
*       der Nachrichtensteuerung/RSNAST00
*---------------------------------------------------------------------*
*  -->  XSCREEN  Flag für Bildschirmausgabe. Wird ignoriert!
*  <--  RETCODE  Returncode (0=ok, 1=Fehler)
*  --->  NAST  (implizit)
*  --->  TNAPR (implizit)
*---------------------------------------------------------------------*
FORM entry_ers USING retcode LIKE sy-subrc
                     xscreen LIKE boole-boole.

* archive
  arc_params-sap_object = nast-objtype.
  toa_dara-sap_object   = nast-objtype.
  CLEAR toa_dara-object_id.
  toa_dara-object_id    = nast-objky+4(14).

  CALL FUNCTION 'Z_MRM_ENTRY_ERS'
    EXPORTING
      i_nast       = nast
      i_fonam      = tnapr-fonam
      i_xscreen    = xscreen
      i_arc_params = arc_params
      i_toa_dara   = toa_dara
    EXCEPTIONS
      OTHERS       = 1.
  retcode = sy-subrc.                  "entweder 0 oder 1

  IF ( retcode <> 0 ) AND              "Fehler
     ( xscreen = 'X' ).                "Ausgabe auf Bildschirm
    MESSAGE ID     sy-msgid
            TYPE   sy-msgty
            NUMBER sy-msgno
            WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "ENTRY_ERS
*** RENSAF00018 - ROFF SDF (MDA) - 29.12.2014 16:16:26 - BEGIN --->
FORM entry_ers_pdf USING retcode LIKE sy-subrc
                     xscreen LIKE boole-boole.

* archive
  arc_params-sap_object = nast-objtype.
  toa_dara-sap_object   = nast-objtype.
  CLEAR toa_dara-object_id.
  toa_dara-object_id    = nast-objky+4(14).

  CALL FUNCTION 'Z_MRM_ENTRY_ERS_PDF'
    EXPORTING
      i_nast       = nast
      i_fonam      = tnapr-fonam
      i_xscreen    = xscreen
      i_arc_params = arc_params
      i_toa_dara   = toa_dara
    EXCEPTIONS
      OTHERS       = 1.
  retcode = sy-subrc.                  "entweder 0 oder 1

  IF ( retcode <> 0 ) AND              "Fehler
     ( xscreen = 'X' ).                "Ausgabe auf Bildschirm
    MESSAGE ID     sy-msgid
            TYPE   sy-msgty
            NUMBER sy-msgno
            WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "ENTRY_ERS_PDF
*** RENSAF00018 - ROFF SDF (MDA) - 29.12.2014 16:16:26 -  END  <---
