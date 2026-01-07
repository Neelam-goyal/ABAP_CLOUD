CLASS zcl_http_eway_noirn_proc_clrtx DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      lo_eway         TYPE REF TO zcl_eway_noirn_process_clrtx,
      lv_access_token TYPE string,
      miw_string      TYPE string,
      lv_eway_json    TYPE string,
      lv_eway_resp    TYPE string,
      lv_canc_resp    TYPE string,
      gt_final        TYPE TABLE OF zstr_einv_data,
      gt_eway         TYPE TABLE OF zstr_eway_data,
      gs_eway         TYPE zstr_eway_data,
      lv_input_str    TYPE string.


    DATA: lv_vbeln  TYPE c LENGTH 10,
          lv_date   TYPE c LENGTH 10,
          lv_bukrs  TYPE c LENGTH 4,
          lv_plant  TYPE c LENGTH 4,
          lv_fyear  TYPE c LENGTH 4,
          lv_etype  TYPE c LENGTH 10,
          lv_module TYPE c LENGTH 10,
          lv_action TYPE c LENGTH 10.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_EWAY_NOIRN_PROC_CLRTX IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'billingdocument'.
    IF sy-subrc EQ 0.
      lv_vbeln = ls_input-value.
    ENDIF.
    lv_vbeln = |{ lv_vbeln ALPHA = IN }| .

    CLEAR: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'billingdate'.
    IF sy-subrc EQ 0 AND ls_input-value IS NOT INITIAL.
      lv_date = ls_input-value. "2024-06-13
      lv_date = ls_input-value+0(4) && ls_input-value+5(2) && ls_input-value+8(2).
    ENDIF.

    CLEAR: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'compcode'.
    IF sy-subrc EQ 0.
      lv_bukrs = ls_input-value.
    ENDIF.

    CLEAR: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'fisyear'.
    IF sy-subrc EQ 0.
      lv_fyear = ls_input-value.
    ENDIF.

    CLEAR: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'plant'.
    IF sy-subrc EQ 0.
      lv_plant = ls_input-value.
    ENDIF.

    CLEAR: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'einvtype'.
    IF sy-subrc EQ 0.
      lv_etype = ls_input-value.
    ENDIF.

    CLEAR: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'businessmodule'.
    IF sy-subrc EQ 0.
      lv_module = ls_input-value.
    ENDIF.

    DATA:
      gt_input TYPE TABLE OF zstr_eway_data,
      gs_input TYPE zstr_eway_data.

    DATA(lv_request_body) = request->get_text( ).
    lv_input_str = lv_request_body.

    IF lv_input_str IS NOT INITIAL.

      /ui2/cl_json=>deserialize(
        EXPORTING json = lv_input_str
           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
           CHANGING data = gs_input
                                   ).

      lv_action = gs_input-actionname.

    ENDIF.

    "********Creation of object**************
    CREATE OBJECT lo_eway.

    IF lv_action = 'fetchdata'.

      lo_eway->fetch_billing_data(
        EXPORTING
          im_vbeln    = lv_vbeln
          im_bukrs    = lv_bukrs
          im_fyear    = lv_fyear
          im_plant    = lv_plant
          im_date     = lv_date
          iv_action   = lv_action
          im_etype    = lv_etype
        RECEIVING
          r_eway_data = gt_final
      ).

      DATA(lv_json) = /ui2/cl_json=>serialize(
        data             = gt_final[]
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        ).

      miw_string = lv_json .

      ""**Setting response
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action = 'showjson'.

      lo_eway->prepare_eway_json_clrtx(
        EXPORTING
          im_vbeln    = lv_vbeln
          im_bukrs    = lv_bukrs
          im_fyear    = lv_fyear
          im_plant    = lv_plant
          im_date     = lv_date
          iv_action   = lv_action
          im_access_token = lv_access_token
          im_input_str    = lv_input_str
        RECEIVING
          r_eway_json  = lv_eway_json
      ).

      ""**Setting response
      miw_string = lv_eway_json.
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

    IF lv_action = 'geneway'.

      lo_eway->prepare_eway_json_clrtx(
        EXPORTING
          im_vbeln    = lv_vbeln
          im_bukrs    = lv_bukrs
          im_fyear    = lv_fyear
          im_plant    = lv_plant
          im_date     = lv_date
          iv_action   = lv_action
          im_access_token = lv_access_token
          im_input_str    = lv_input_str
        RECEIVING
          r_eway_json  = lv_eway_json
      ).

      IF lv_eway_json IS NOT INITIAL.

        lo_eway->gen_eway_from_clrtx(
          EXPORTING
            im_action       = lv_action
            im_eway_json    = lv_eway_json
            im_input_str    = lv_input_str
          RECEIVING
            r_gen_eway      = lv_eway_resp
        ).

        """if success save data in to ztable
        lo_eway->save_eway_data_clrtx(
          EXPORTING
          im_vbeln     = lv_vbeln
          im_bukrs     = lv_bukrs
          im_fyear     = lv_fyear
          im_plant     = lv_plant
          im_date      = lv_date
          im_eway_resp = lv_eway_resp
          im_input_str = lv_input_str
          RECEIVING
          r_saved_resp = lv_eway_resp
        ).

      ENDIF.

      ""**Setting response
      miw_string = lv_eway_resp.
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

*    IF lv_action = 'canceleinv'.
*
*      lo_einv->get_token_from_clrtx(
*        EXPORTING
*          im_action      = lv_action
*        RECEIVING
*          r_access_token = lv_access_token
*      ).
*
*      lo_einv->cancel_einv_irn_clrtx(
*        EXPORTING
*           im_vbeln    = lv_vbeln
*           im_bukrs    = lv_bukrs
*           im_fyear    = lv_fyear
*           im_plant    = lv_plant
*           im_date     = lv_date
*           iv_action   = lv_action
*           im_access_token = lv_access_token
*        RECEIVING
*          r_einv_canc     =  lv_canc_resp
*      ).
*
*      ""**Setting response
*      miw_string = lv_canc_resp.
*      response->set_text(
*        EXPORTING
*          i_text = miw_string ).
*
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
