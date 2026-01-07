CLASS zcl_http_hdfc_bank_file DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_HDFC_BANK_FILE IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA: lo_client  TYPE REF TO zcl_bank_data_file,
          miw_string TYPE string,
          gt_file    TYPE TABLE OF zstr_hsbc_bank_file, "zstr_hdfc_bank_file,
          lv_input_str TYPE string.

    "Get inbound data
    DATA(lv_request_body) = request->get_text( ).
    lv_input_str = lv_request_body.

    CREATE OBJECT lo_client.
    gt_file[]  = lo_client->get_accounting_data( im_input_str = lv_input_str ).

    DATA(json) = /ui2/cl_json=>serialize(
      data             = gt_file[]
      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      ).

    miw_string = json .

    ""**Setting response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = miw_string ).

  ENDMETHOD.
ENDCLASS.
