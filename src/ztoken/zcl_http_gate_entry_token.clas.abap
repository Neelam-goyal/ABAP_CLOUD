CLASS zcl_http_gate_entry_token DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_GATE_ENTRY_TOKEN IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA:
      gt_getoken TYPE TABLE OF zstr_token_gate_entry,
      gs_getoken TYPE zstr_token_gate_entry.

    DATA:
      lo_client TYPE REF TO ycl_token_gate_entry.

    DATA: lv_tokennum TYPE c LENGTH 10,
          lv_genum    TYPE c LENGTH 10,
          lv_plant    TYPE c LENGTH 4,
          lv_kata     TYPE c LENGTH 5,
          lv_action1  TYPE c LENGTH 10,
          lv_action2  TYPE c LENGTH 10,
          lv_lot_ok   TYPE c.

    DATA: miw_string TYPE string.
    CLEAR: miw_string.

    "Get inbound data
    DATA(lv_request_body) = request->get_text( ).

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname1'.
    IF sy-subrc EQ 0.
      lv_action1 = ls_action-value.
    ENDIF.

    CLEAR: ls_action.
    READ TABLE lt_input INTO ls_action WITH KEY name = 'actionname2'.
    IF sy-subrc EQ 0.
      lv_action2 = ls_action-value.
    ENDIF.

    CLEAR: ls_action.
    READ TABLE lt_input INTO ls_action WITH KEY name = 'tokennum'.
    IF sy-subrc EQ 0.
      lv_tokennum = ls_action-value.
    ENDIF.
    "lv_genum = |{ lv_genum ALPHA = IN }| .

    CLEAR: ls_action.
    READ TABLE lt_input INTO ls_action WITH KEY name = 'gateentryno'.
    IF sy-subrc EQ 0.
      lv_genum = ls_action-value.
    ENDIF.

    CLEAR: ls_action.
    READ TABLE lt_input INTO ls_action WITH KEY name = 'wbplant'.
    IF sy-subrc EQ 0.
      lv_plant = ls_action-value.
    ENDIF.

    CLEAR: ls_action.
    READ TABLE lt_input INTO ls_action WITH KEY name = 'kataname'.
    IF sy-subrc EQ 0.
      lv_kata = ls_action-value.
    ENDIF.
    ""****Creating Object**********************
    CREATE OBJECT lo_client.

    IF lv_action1 = 'create' AND lv_action2 = 'getdata'.

      gt_getoken = lo_client->get_token_data(
        EXPORTING
          im_tokennum = lv_tokennum
      ).

      DATA(json) = /ui2/cl_json=>serialize(
        data             = gt_getoken[]
        pretty_name      = /ui2/cl_json=>pretty_mode-none
        ).

      miw_string = json.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action1 = 'create' AND lv_action2 = 'save'.

      "Get inbound data
      CLEAR: lv_request_body.
      lv_request_body = request->get_text( ).

      /ui2/cl_json=>deserialize(
                      EXPORTING json = lv_request_body
                         pretty_name = /ui2/cl_json=>pretty_mode-none
                         CHANGING data = gt_getoken
                   ).

*      IF gt_getoken[] IS NOT INITIAL.
*        READ TABLE gt_getoken INTO DATA(gs_getoken_new) INDEX 1.
*      ENDIF.

*      clear: lv_lot_ok.
*      LOOP AT gs_getoken_new-item_data INTO DATA(ls_item_new).
*
*        IF ls_item_new-insplotno IS INITIAL.
*         lv_lot_ok = abap_true.
*         exit.
*        ENDIF.
*
*        CLEAR: ls_item_new.
*      ENDLOOP.
*
*      IF lv_lot_ok IS INITIAL.
*        DATA(lv_gate_num) = lo_client->save_data_get_genum( im_action = lv_action1 xt_gedata = gt_getoken ).
*        miw_string = lv_gate_num.
*      ELSE.
*        miw_string = 'Please provide inspection lot number'.
*      ENDIF.

      DATA(lv_gate_num) = lo_client->save_data_get_genum( im_action = lv_action1 xt_gedata = gt_getoken ).
      miw_string = lv_gate_num.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action1 = 'change' AND lv_action2 = 'getdata'.

      DATA(gt_final) = lo_client->get_ge_change_data(
        EXPORTING
          im_genum = lv_genum
      ).

      CLEAR: json.
      json = /ui2/cl_json=>serialize(
        data             = gt_final[]
        compress         = abap_true
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        ).

      miw_string = json.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).


    ENDIF.


    IF lv_action1 = 'change' AND lv_action2 = 'save'.

      "Get inbound data
      CLEAR: lv_request_body.
      lv_request_body = request->get_text( ).

      /ui2/cl_json=>deserialize(
                      EXPORTING json = lv_request_body
                         pretty_name = /ui2/cl_json=>pretty_mode-none
                         CHANGING data = gt_getoken
                   ).

      lv_gate_num = lo_client->save_data_get_genum( im_action = lv_action1 xt_gedata = gt_getoken ).
      miw_string  = lv_gate_num.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action1 = 'fetchwgt' .

      DATA:
        lo_wgt     TYPE REF TO zcl_get_weigh_bridge_weight.

      CREATE OBJECT lo_wgt.

      lo_wgt->get_wb_weight(
        EXPORTING
          im_tokennum = lv_action1
          im_plant    = lv_plant
          im_kata     = lv_kata
        RECEIVING
          es_weight   = DATA(lv_wgt_res)
      ).

      miw_string = lv_wgt_res.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
