CLASS zcl_mrm_badi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_mrm_header_data_default .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MRM_BADI IMPLEMENTATION.


  METHOD if_ex_mrm_header_data_default~default_header_data.
  if 1 = 2.

  ENDIF.
  ENDMETHOD.
ENDCLASS.
