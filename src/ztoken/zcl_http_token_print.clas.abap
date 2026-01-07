CLASS zcl_http_token_print DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: xt_final        TYPE TABLE OF zi_token_data,
          ls_xml_base64   TYPE string,
          lv_access_token TYPE string.

    DATA: lv_tokennum TYPE c LENGTH 10,
          rv_response TYPE string,
          lv_action   TYPE c LENGTH 10.

    DATA: lo_client TYPE REF TO zcl_token_print,
          lo_ads    TYPE REF TO zcl_ads_service.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_TOKEN_PRINT IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'tokennum'.
    IF sy-subrc EQ 0.
      lv_tokennum = ls_input-value.
    ENDIF.
    "*lv_tokennum = |{ lv_tokennum ALPHA = IN }| .

    "********Creation of object**************
    CREATE OBJECT lo_client.
    CREATE OBJECT lo_ads.

    DATA:
      lv_resp_new TYPE string.

    ""*****Calling Methods to get PDF code base64
    lv_access_token = lo_ads->get_ads_access_token(  ).

    IF lv_action = 'tokenprint'. "Token Print

      xt_final      = lo_client->get_token_data(
                        im_tokennum = lv_tokennum
                        im_action   = lv_action
                        ).

      ls_xml_base64 = lo_client->prep_xml_token_print( it_final = xt_final[] im_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
        im_access_token  = lv_access_token
        im_template_name = 'ZMM_FORM_TOKEN/ZMM_BLV_TOKEN_PRINT'
        im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    ""**Setiing response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = rv_response ).

  ENDMETHOD.
ENDCLASS.
