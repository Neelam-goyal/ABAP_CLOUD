CLASS lhc_ZE_NUM_TO_STRING DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ze_num_to_string RESULT result.

ENDCLASS.

CLASS lhc_ZE_NUM_TO_STRING IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.
