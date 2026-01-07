CLASS lhc_ze_tax_gst_prcnt DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ze_tax_gst_prcnt RESULT result.

ENDCLASS.

CLASS lhc_ze_tax_gst_prcnt IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.
