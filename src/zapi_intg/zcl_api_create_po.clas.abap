CLASS zcl_api_create_po DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_CREATE_PO IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA:
      xt_final TYPE TABLE OF zstr_po_create_hdr,
      xt_hdr   TYPE TABLE OF zstr_po_create_hdr,
      xs_hdr   TYPE zstr_po_create_hdr,
      xt_item  TYPE TABLE OF zstr_po_create_item,
      xs_item  TYPE zstr_po_create_item.


    DATA: lo_client    TYPE REF TO zcl_po_process,
          miw_string   TYPE string,
          lv_input_str TYPE string.

    "Get inbound data
    DATA(lv_request_body) = request->get_text( ).
    lv_input_str = lv_request_body.

    CREATE OBJECT lo_client.
    xt_final[]  = lo_client->create_purchase_order( im_input_str = lv_input_str ).

    DATA(json) = /ui2/cl_json=>serialize(
      data             = xt_final[]
      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      ).

    miw_string = json .

    ""**Setting response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = miw_string ).


    """******
*    xs_hdr-purchaseorder          = '123456'.
*    xs_hdr-purchaseordertype      = 'NB'.
*    xs_hdr-CompanyCode            = '1000'.
*    xs_hdr-purchasingorganization = '2000'.
*    xs_hdr-purchasinggroup        = '001'.
*    xs_hdr-supplier               = '1000088'.
*    xs_hdr-postatus               = ''.
*
*    xs_item-purchaseorderitem = '10'.
*    xs_item-material          = 'MAT10005'.
*    xs_item-plant             = '1001'.
*    xs_item-orderquantity     = '50'.
*    xs_item-netpriceamount    = '999'.
*    APPEND xs_item TO xt_item.
*
*    INSERT LINES OF xt_item INTO TABLE xs_hdr-gt_item.
*    APPEND xs_hdr TO xt_hdr.
*
*    DATA(json) = /ui2/cl_json=>serialize(
*      data             = xt_hdr[]
*      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
*      ).
    """******
  ENDMETHOD.
ENDCLASS.
