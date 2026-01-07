CLASS lhc_ZE_INSP_LOT_EMAIL DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ze_insp_lot_email RESULT result.

ENDCLASS.

CLASS lhc_ZE_INSP_LOT_EMAIL IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.
