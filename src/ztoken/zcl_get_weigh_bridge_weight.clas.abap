CLASS zcl_get_weigh_bridge_weight DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char120 TYPE c LENGTH 120,
      lv_char4   TYPE c LENGTH 4,
      lv_char5   TYPE c LENGTH 5.

    METHODS:
      get_wb_weight
        IMPORTING
                  im_tokennum      LIKE lv_char10
                  im_plant         LIKE lv_char4
                  im_kata          LIKE lv_char5
        RETURNING VALUE(es_weight) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_get_weigh_bridge_weight IMPLEMENTATION.


  METHOD get_wb_weight.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    IF im_plant = '1001'.
      url = 'https://blvexports.mawaiweb.com'.
    ELSEIF im_plant = '2001'.
      url = 'https://blfblvexports.mawaiweb.com'.
    ELSEIF im_plant = '4001'.
      IF im_kata = 'kata1'.
        url = 'https://bapl1.mawaiweb.com'.
      ELSEIF im_kata = 'kata2'.
        url = 'https://bapl2.mawaiweb.com'.
      ENDIF.
    ELSEIF im_plant = '3001'.
      url = 'https://blvexportskandla.mawaiweb.com'.   "added by neelam goyal: Raj kumar mishra
    ELSEIF im_plant = '1002'.
      url = 'https://blvexportskandla.mawaiweb.com'.   "added by neelam goyal: Raj kumar mishra
    ENDIF.

    TRY.
        DATA(dest1) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest1 ).

        DATA(lo_request1) = lo_http_client->get_http_request( ).
        lo_request1->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'Host' ##NO_TEXT
            i_value = url
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>get ).
        DATA(response_body1) = lo_response1->get_text( ).
        es_weight   = response_body1.

*    es_weight   =
*    '{'
*    && '"status": "success",'
*    && '"data": "5950"'
*    && '}'.

        """"*****************************************************************************************

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
