CLASS zcl_http_co11n_conf DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      lv_action    TYPE c LENGTH 10,
      lo_conf      TYPE REF TO zcl_co11n_conf,
      gt_odr       TYPE TABLE OF zpp_order_conf,
      xt_odr       TYPE TABLE OF zpp_order_conf,
      lt_prop      TYPE TABLE OF zstr_goodsmvt_prop,
      lv_input_str TYPE string,
      lv_date      TYPE c LENGTH 10,
      miw_string   TYPE string.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_CO11N_CONF IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA:
       lv_save_resp TYPE string.

    "********Creation of object**************
    CREATE OBJECT lo_conf.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    IF lv_action = 'savedata'.

      DATA(lv_request_body) = request->get_text( ).
      lv_input_str = lv_request_body.

      IF lv_input_str IS NOT INITIAL.

        lo_conf->save_order_data(
          EXPORTING
            im_action    = lv_action
            im_input_str = lv_input_str
          RECEIVING
            es_save_stat = lv_save_resp
        ).

        ""**Setting response
        response->set_text(
          EXPORTING
            i_text = lv_save_resp ).

      ENDIF.

    ENDIF.

    IF lv_action = 'getdata'.

      READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'orderdate'.
      IF sy-subrc EQ 0.
        lv_date = ls_input-value.
      ENDIF.

      lo_conf->fetch_order_data(
        EXPORTING
          im_action = lv_action
          im_date   = lv_date
        RECEIVING
          et_odr    = gt_odr
      ).

      DATA(lv_json) = /ui2/cl_json=>serialize(
        data             = gt_odr[]
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        ).

      miw_string = lv_json .

      ""**Setting response
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.


    IF lv_action = 'confodr'.

      DATA:
        gt_input TYPE TABLE OF zpp_order_conf,
        gs_input TYPE zpp_order_conf.

      lv_request_body = request->get_text( ).
      lv_input_str = lv_request_body.

      /ui2/cl_json=>deserialize(
        EXPORTING json = lv_input_str
           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
           CHANGING data = gt_input
                                   ).
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<lfs_input>).


*      lo_conf->get_prod_order_proposal(
*        EXPORTING
*          im_action     = lv_action
*          it_odr        = gt_odr
*        RECEIVING
*          et_prop       = lt_prop
*      ).
*
*      lo_conf->conf_prod_order(
*        EXPORTING
*          im_action     = lv_action
*          it_odr        = gt_odr
*          it_prop       = lt_prop
*        RECEIVING
*          et_odr_status = xt_odr
*      ).

        IF sy-tabix = 1.
          <lfs_input>-status = 'Error in confirmation !' ##NO_TEXT.
        ELSE.
          <lfs_input>-status = 'Success' ##NO_TEXT.
        ENDIF.

      ENDLOOP.


      lv_json = /ui2/cl_json=>serialize(
        data             = gt_input[] "xt_odr[]
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        ).

      miw_string = lv_json .

      ""**Setting response
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
