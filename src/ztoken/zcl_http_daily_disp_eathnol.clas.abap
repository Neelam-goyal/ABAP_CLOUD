class ZCL_HTTP_DAILY_DISP_EATHNOL definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HTTP_DAILY_DISP_EATHNOL IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.

    DATA: lo_client  TYPE REF TO ycl_daily_disp_eathnol,
          miw_string TYPE string,
          gt_data    TYPE TABLE OF zstr_daily_dispatch_ethanol,
          lv_input_str TYPE string,
          lv_action  TYPE c LENGTH 10.

    "Get inbound data
    DATA(lv_request_body) = request->get_text( ).
    lv_input_str = lv_request_body.

    CREATE OBJECT lo_client.
    gt_data[]  = lo_client->get_rep_data( im_input_str = lv_input_str im_action = lv_action ).

    DATA(json) = /ui2/cl_json=>serialize(
      data             = gt_data[]
      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      ).

    miw_string = json .

    ""**Setting response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = miw_string ).

  endmethod.
ENDCLASS.
