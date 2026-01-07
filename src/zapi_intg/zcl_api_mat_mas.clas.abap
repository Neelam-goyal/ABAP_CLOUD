CLASS zcl_api_mat_mas DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_MAT_MAS IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA:
      gt_mat TYPE TABLE OF zstr_mat_mas,
      gs_mat TYPE zstr_mat_mas.

    TYPES : BEGIN OF ty_dat,
              date TYPE c LENGTH 10,
            END OF ty_dat.

    DATA: miw_string TYPE string,
          t_inp      TYPE TABLE OF ty_dat,
          r_date     TYPE RANGE OF ty_dat-date,
          rw_date    LIKE LINE OF r_date,
          r_inp      TYPE ty_dat,
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

    SELECT * FROM zi_prod_master
    WHERE creationdate = @lv_date
    INTO TABLE @DATA(lt_mat).

    IF lt_mat[] IS NOT INITIAL.
      gt_mat = CORRESPONDING #( lt_mat[] ).
    ENDIF.

    DATA(json) = /ui2/cl_json=>serialize(
      data             = gt_mat[]
      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      ).

    miw_string = json .

    ""**Setting response for TP
    response->set_text(
      EXPORTING
        i_text = miw_string ).

  ENDMETHOD.
ENDCLASS.
