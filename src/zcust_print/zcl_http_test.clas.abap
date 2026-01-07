CLASS zcl_http_test DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: xt_final        TYPE TABLE OF zi_sale_reg,
          ls_xml_base64   TYPE string,
          lv_access_token TYPE string.

    DATA: lo_client TYPE REF TO YCL_TEST,
          lo_ads    TYPE REF TO zcl_ads_service.

    DATA: lv_vbeln      TYPE c LENGTH 10,
          rv_response   TYPE string,
          rv_resp_signd TYPE string,
          lv_action     TYPE c LENGTH 10.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_TEST IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA:
      xt_final TYPE TABLE OF zstr_grn_data.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    CLEAR: ls_action .
    READ TABLE lt_input INTO ls_action WITH KEY name = 'salesdoc'.
    IF sy-subrc EQ 0.
      lv_vbeln  = ls_action-value.
    ENDIF.

    "********Creation of object**************
    CREATE OBJECT lo_client.
    CREATE OBJECT lo_ads.


    ""*****Calling Methods to get PDF code base64
    lv_access_token = lo_ads->get_ads_access_token(  ).


    xt_final      = lo_client->get_test_data( iv_vbeln = lv_vbeln iv_action = lv_action ).

    ls_xml_base64 = lo_client->prep_xml_test_print( it_final = xt_final[] iv_action = lv_action ).

    rv_response = lo_ads->get_ads_api_toget_base64(
      im_access_token  = lv_access_token
      im_template_name = 'ZTEST_FORM/Z_TEST_PRINT' "'ZPP_LABEL_PRINT/ZTEST_LABEL_PRINT' "
      im_xml_base64    = ls_xml_base64 ).


    ""**Setiing response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = rv_response ).


  ENDMETHOD.
ENDCLASS.
