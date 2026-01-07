CLASS zcl_http_suplier_perform DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      lv_action      TYPE c LENGTH 10,
      lv_action_type TYPE c LENGTH 10,
      lv_input_str   TYPE string,
      miw_string     TYPE string,
      lo_obj         TYPE REF TO zcl_supplier_perform,
      gt_final       TYPE TABLE OF zpp_suplr_perfor,
      gt_status      TYPE TABLE OF zpp_suplr_perfor.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_SUPLIER_PERFORM IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    "********Creation of object**************
    CREATE OBJECT lo_obj.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    CLEAR: ls_action.
    READ TABLE lt_input INTO ls_action WITH KEY name = 'actiontype'.
    IF sy-subrc EQ 0.
      lv_action_type = ls_action-value.
    ENDIF.

    DATA(lv_request_body) = request->get_text( ).
    lv_input_str = lv_request_body.

    IF lv_action = 'getdata'.

      lo_obj->get_supplier_data(
        EXPORTING
          im_action      = lv_action
          im_action_type = lv_action_type
          im_input_str   = lv_input_str
        RECEIVING
          et_final       = gt_final
      ).

      DATA(lv_json)  = /ui2/cl_json=>serialize(
        data             = gt_final[]
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        ).

      miw_string = lv_json .

      ""**Setting response
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action = 'savedata'.

      lo_obj->save_supplier_data(
        EXPORTING
          im_action      = lv_action
          im_action_type = lv_action_type
          im_input_str   = lv_input_str
        RECEIVING
          es_save_status = miw_string
      ).

      ""**Setting response
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action = 'sendmail'.

      DATA:
        gt_input TYPE TABLE OF zpp_suplr_perfor,
        gs_input TYPE zpp_suplr_perfor.

      /ui2/cl_json=>deserialize(
        EXPORTING json = lv_input_str
           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
           CHANGING data = gt_input
                                   ).

      IF gt_input[] IS NOT INITIAL.

        lo_obj->send_mail(
          EXPORTING
            xt_data      = gt_input
            im_mode      = 'NBG'
            im_action    = lv_action
            im_action_type = lv_action_type
          RECEIVING
            et_mail_stat = gt_status
        ).

        lv_json = /ui2/cl_json=>serialize(
          data             = gt_status[]
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
          ).

        miw_string = lv_json.

      ENDIF.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.


  ENDMETHOD.
ENDCLASS.
