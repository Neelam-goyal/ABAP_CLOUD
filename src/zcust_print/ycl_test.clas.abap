CLASS ycl_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  data:
          lv_char10 TYPE c LENGTH 10,
          lv_char120 TYPE c LENGTH 120.

     DATA:
       gt_final TYPE TABLE OF zstr_grn_data.

     METHODS:
       get_test_data
         IMPORTING
                   iv_vbeln        like lv_char10
                   iv_action       like lv_char10
         RETURNING VALUE(et_final) LIKE gt_final,

       prep_xml_test_print
         IMPORTING
                   it_final             LIKE gt_final
                   iv_action            like lv_char10
         RETURNING VALUE(iv_xml_base64) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS YCL_TEST IMPLEMENTATION.


METHOD get_test_data.

ENDMETHOD.


METHOD prep_xml_test_print.

ENDMETHOD.
ENDCLASS.
