CLASS zcl_http_update_delivery_seria DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      lo_srl       TYPE REF TO zcl_update_delvry_srl,
      gt_srl       TYPE TABLE OF zstr_dlvry_serial,
      rt_srl       TYPE TABLE OF zstr_dlvry_serial,
      lv_input_str TYPE string.

    DATA:
      lv_action  TYPE c LENGTH 10,
      lv_date    TYPE c LENGTH 10,
      miw_string TYPE string.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_UPDATE_DELIVERY_SERIA IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA:
      lv_csrf_token   TYPE string,
      lv_action       TYPE c LENGTH 10,
      lv_srl_upd_stat TYPE string,
      miw_string      TYPE string,
      lv_save_resp    TYPE string.

    "********Creation of object**************
    CREATE OBJECT lo_srl.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.


    DATA(lv_request_body) = request->get_text( ).
    lv_input_str = lv_request_body.

    IF lv_input_str IS NOT INITIAL.

      /ui2/cl_json=>deserialize(
        EXPORTING json = lv_input_str
           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
           CHANGING data = gt_srl
                                   ).

    ENDIF.

    IF lv_action = 'savedata'.

      lo_srl->save_serial_data(
        EXPORTING
          im_action    = lv_action
          it_srl       = gt_srl
          im_input_str = lv_input_str
        RECEIVING
          es_save_stat = lv_save_resp
      ).

      ""**Setting response
      response->set_text(
        EXPORTING
          i_text = lv_save_resp ).

    ENDIF.


    IF lv_action = 'getdata'.

      READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'uploaddate'.
      IF sy-subrc EQ 0.
        lv_date = ls_input-value.
      ENDIF.

      lo_srl->fetch_serial_data(
        EXPORTING
          im_action = lv_action
          im_date   = lv_date
        RECEIVING
          et_srl    = gt_srl
      ).

      DATA(lv_json) = /ui2/cl_json=>serialize(
        data             = gt_srl[]
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        ).

      miw_string = lv_json .

      ""**Setting response
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.


    IF lv_action = 'updsrl'.

      IF gt_srl[] IS NOT INITIAL.

        lo_srl->update_serial_data(
          EXPORTING
            im_action     = lv_action
            im_csrf_token = lv_csrf_token
            im_mode       = 'NCG'
            xt_srl        = gt_srl
          RECEIVING
            et_srl_status = rt_srl

        ).

        lv_json = /ui2/cl_json=>serialize(
          data             = rt_srl[]
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
          ).

        miw_string = lv_json.

        ""**Setting response
        response->set_text(
          EXPORTING
            i_text = miw_string ).

      ENDIF.

    ENDIF.


  ENDMETHOD.
ENDCLASS.
