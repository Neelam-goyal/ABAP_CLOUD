CLASS zcl_api_vend_mas DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_VEND_MAS IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA: miw_string TYPE string,
          lv_date    TYPE c LENGTH 10,
          iv_date    TYPE d.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_date) WITH KEY name = 'date'.
    IF sy-subrc EQ 0.
      lv_date = ls_date-value. "20-09-2023
    ENDIF.

    DATA:
      sys_date     TYPE d.

    sys_date = cl_abap_context_info=>get_system_date( ).

    SELECT * FROM zi_supplier_address
  WHERE creationdate = @lv_date
    INTO TABLE @DATA(gt_vend).

    DATA(json) = /ui2/cl_json=>serialize(
      data             = gt_vend[]
      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      ).

    miw_string = json .

    ""**Setting response for TP
    response->set_text(
      EXPORTING
        i_text = miw_string ).

  ENDMETHOD.
ENDCLASS.
