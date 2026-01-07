CLASS ycl_miro_vald DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

data:
          lv_char15 TYPE c LENGTH 15.

    CLASS-METHODS:
      validate_qlty
        IMPORTING
          im_docnum like lv_char15
          im_eobj   like lv_char15
          RETURNING VALUE(es_data) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_MIRO_VALD IMPLEMENTATION.


  METHOD validate_qlty.
    es_data = abap_true.
  ENDMETHOD.
ENDCLASS.
