CLASS zcl_http_token_process DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_TOKEN_PROCESS IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA:
      gt_data  TYPE TABLE OF zstr_token_data,
      gs_data  TYPE zstr_token_data,
      gt_final TYPE TABLE OF zmm_token_data,
      gs_final TYPE zmm_token_data,
      it_item  TYPE TABLE OF zstr_token_item,
      is_item  TYPE zstr_token_item.

    DATA:
      lo_client TYPE REF TO ycl_token_process.

    DATA: lv_tokennum TYPE c LENGTH 10,
          lv_action   TYPE c LENGTH 10,
          lv_action1  TYPE c LENGTH 10,
          miw_string  TYPE string.

    "Get inbound data
    DATA(lv_request_body) = request->get_text( ).

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'. "create
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    CLEAR: ls_action.
    READ TABLE lt_input INTO ls_action WITH KEY name = 'actionname1'.
    IF sy-subrc EQ 0.
      lv_action1 = ls_action-value.
    ENDIF.

    CLEAR: ls_action.
    READ TABLE lt_input INTO ls_action WITH KEY name = 'tokennum'.
    IF sy-subrc EQ 0.
      lv_tokennum = ls_action-value.
    ENDIF.
    "lv_genum = |{ lv_genum ALPHA = IN }| .

    ""****Creating Object**********************
    CREATE OBJECT lo_client.

    IF lv_action = 'create' AND lv_action1 = 'save'.

      "Get inbound data
      CLEAR: lv_request_body.
      lv_request_body = request->get_text( ).

      /ui2/cl_json=>deserialize(
                      EXPORTING json = lv_request_body
                         pretty_name = /ui2/cl_json=>pretty_mode-none
                         CHANGING data = gt_data
                   ).


      READ TABLE gt_data INTO gs_data INDEX 1.
      gs_final = CORRESPONDING #( gs_data ).
      LOOP AT gs_data-item_data INTO DATA(ls_item).
        gs_final-ponum    = ls_item-ebeln.
        gs_final-poitem   = ls_item-ebelp.
        gs_final-supplier = ls_item-supplier.
        gs_final-opqty    = ls_item-opqty.
        gs_final-docdate  = ls_item-docdate.
        gs_final-netprice = ls_item-netprice.
        gs_final-material       = ls_item-matnr.
        gs_final-material_group = ls_item-productgroup.
        gs_final-material_type  = ls_item-producttype.
        gs_final-material_uom   = ls_item-uom.
        gs_final-material_desc  = ls_item-maktx.
        gs_final-insplotno      = ls_item-insplotno.
        APPEND gs_final TO gt_final.
        CLEAR: ls_item.
      ENDLOOP.

      DATA(lv_token_num) = lo_client->save_data_get_tokennum( im_action = lv_action it_data = gt_final ).

      miw_string = lv_token_num.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action = 'change' AND lv_action1 = 'getdata'.

      gt_final[] = lo_client->get_token_change_data(
        EXPORTING
          im_token_num = lv_tokennum
      ).

      IF gt_final[] IS NOT INITIAL.

        READ TABLE gt_final INTO gs_final INDEX 1.
        gs_data = CORRESPONDING #( gs_final ).
        LOOP AT gt_final INTO DATA(gs_item).

          is_item-ebeln    = gs_item-ponum.
          is_item-ebelp    = gs_item-poitem.
          is_item-opqty    = gs_item-opqty.
          is_item-docdate  = gs_item-docdate.
          is_item-netprice = gs_item-netprice.
          is_item-matnr       = gs_item-material.
          is_item-productgroup  = gs_item-material_group.
          is_item-producttype   = gs_item-material_type.
          is_item-uom      = gs_item-material_uom.
          is_item-maktx   = gs_item-material_desc .
          is_item-insplotno  = gs_item-insplotno .
          is_item-supplier   = gs_item-supplier.
          APPEND is_item TO it_item.

          CLEAR: gs_final.
        ENDLOOP.

        INSERT LINES OF it_item INTO TABLE gs_data-item_data.
        APPEND gs_data TO gt_data.

      ENDIF.

      DATA(json) = /ui2/cl_json=>serialize(
        data             = gt_data[]
        pretty_name      = /ui2/cl_json=>pretty_mode-none
        ).

      miw_string = json.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action = 'change' AND lv_action1 = 'save'.

      "Get inbound data
      CLEAR: lv_request_body.
      lv_request_body = request->get_text( ).

      /ui2/cl_json=>deserialize(
                      EXPORTING json = lv_request_body
                         pretty_name = /ui2/cl_json=>pretty_mode-none
                         CHANGING data = gt_data
                   ).

      READ TABLE gt_data INTO gs_data INDEX 1.
      gs_final = CORRESPONDING #( gs_data ).
      LOOP AT gs_data-item_data INTO ls_item.
        gs_final-ponum    = ls_item-ebeln.
        gs_final-poitem   = ls_item-ebelp.
        gs_final-opqty    = ls_item-opqty.
        gs_final-docdate  = ls_item-docdate.
        gs_final-netprice = ls_item-netprice.
        gs_final-material       = ls_item-matnr.
        gs_final-material_group = ls_item-productgroup.
        gs_final-material_type  = ls_item-producttype.
        gs_final-material_uom   = ls_item-uom.
        gs_final-material_desc  = ls_item-maktx.
        gs_final-insplotno      = ls_item-insplotno.
        gs_final-supplier       = ls_item-supplier.
        APPEND gs_final TO gt_final.
        CLEAR: ls_item.
      ENDLOOP.

      lv_token_num = lo_client->save_data_get_tokennum( im_action = lv_action it_data = gt_final ).

      miw_string = lv_token_num.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action = 'delete' AND lv_action1 = 'save'.

      "Get inbound data
      CLEAR: lv_request_body.
      lv_request_body = request->get_text( ).

      /ui2/cl_json=>deserialize(
                      EXPORTING json = lv_request_body
                         pretty_name = /ui2/cl_json=>pretty_mode-none
                         CHANGING data = gt_data
                   ).

      READ TABLE gt_data INTO gs_data INDEX 1.
      gs_final = CORRESPONDING #( gs_data ).
      LOOP AT gs_data-item_data INTO ls_item.
        gs_final-ponum    = ls_item-ebeln.
        gs_final-poitem   = ls_item-ebelp.
        gs_final-opqty    = ls_item-opqty.
        gs_final-docdate  = ls_item-docdate.
        gs_final-netprice = ls_item-netprice.
        gs_final-material       = ls_item-matnr.
        gs_final-material_group = ls_item-productgroup.
        gs_final-material_type  = ls_item-producttype.
        gs_final-material_uom   = ls_item-uom.
        gs_final-material_desc  = ls_item-maktx.

        APPEND gs_final TO gt_final.
        CLEAR: ls_item.
      ENDLOOP.

      lv_token_num = lo_client->save_data_get_tokennum( im_action = lv_action it_data = gt_final ).

      miw_string = lv_token_num.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
