CLASS zcl_http_pack_advice DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_PACK_ADVICE IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA: lv_action1 TYPE c LENGTH 10,
          lv_action2 TYPE c LENGTH 10,
          lv_vbeln   TYPE c LENGTH 10,
          lv_packnum TYPE c LENGTH 10,
          lv_date    TYPE c LENGTH 10,
          lv_erdate  TYPE zstr_pack_adv_hdr-entrydate,
          gt_item    TYPE TABLE OF zstr_pack_adv_itm,
          gt_pack    TYPE TABLE OF zstr_pack_adv_hdr,
          gs_pack    TYPE zstr_pack_adv_hdr.

    DATA: lo_client TYPE REF TO ycl_pack_adv.

    DATA: miw_string TYPE string.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname1'.
    IF sy-subrc EQ 0.
      lv_action1 = ls_action-value.
    ENDIF.

    READ TABLE lt_input INTO DATA(ls_action2) WITH KEY name = 'actionname2'.
    IF sy-subrc EQ 0.
      lv_action2 = ls_action2-value.
    ENDIF.

    READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'salesdocument'.
    IF sy-subrc EQ 0.
      lv_vbeln = ls_input-value.
    ENDIF.
    lv_vbeln = |{ lv_vbeln ALPHA = IN }| .


    READ TABLE lt_input INTO DATA(ls_pack) WITH KEY name = 'packingno'.
    IF sy-subrc EQ 0.
      lv_packnum = ls_pack-value.
    ENDIF.
    lv_packnum = |{ lv_packnum ALPHA = IN }| .


    READ TABLE lt_input INTO DATA(ls_date) WITH KEY name = 'date'.
    IF sy-subrc EQ 0.
      lv_date   = ls_date-value.
      lv_erdate = lv_date+6(4) && lv_date+3(2) && lv_date+0(2).
    ENDIF.

    "********Creation of object**************
    CREATE OBJECT lo_client.
    CLEAR: miw_string.

    IF lv_action1 = 'create' AND lv_action2 = 'getdata'.

      gt_pack[]  = lo_client->get_sales_data( im_vbeln = lv_vbeln ).

      DATA(json) = /ui2/cl_json=>serialize(
        data             = gt_pack[]
        compress         = abap_true
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        ).

      miw_string = json .

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action1 = 'create' AND lv_action2 = 'save'.

      "Get inbound data
      DATA(lv_request_body) = request->get_text( ).

      /ui2/cl_json=>deserialize(
                      EXPORTING json = lv_request_body
                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                         CHANGING data = gt_pack
                   ).

      DATA(lv_pack_num) = lo_client->save_data_get_packnum( im_action = lv_action1 xt_pack = gt_pack ).
      miw_string = lv_pack_num.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).



    ENDIF.

    IF lv_action1 = 'change' AND lv_action2 = 'getdata'.

      SHIFT lv_packnum  LEFT DELETING LEADING '0'.
      gt_pack[]  = lo_client->get_pack_change_data( im_packnum = lv_packnum ).

      DATA(lv_json) = /ui2/cl_json=>serialize(
        data             = gt_pack[]
        compress         = abap_true
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        ).

      miw_string = lv_json .

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action1 = 'change' AND lv_action2 = 'save'.

      "Get inbound data
      lv_request_body = request->get_text( ).

      /ui2/cl_json=>deserialize(
                      EXPORTING json = lv_request_body
                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                         CHANGING data = gt_pack
                   ).

      lv_pack_num = lo_client->save_data_get_packnum( im_action = lv_action1 xt_pack = gt_pack ).
      miw_string = lv_pack_num.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
