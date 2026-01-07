CLASS ycl_daily_disp_eathnol DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_data TYPE TABLE OF zstr_daily_dispatch_ethanol,
      gs_data TYPE zstr_daily_dispatch_ethanol.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char120 TYPE c LENGTH 120,
      lv_char4   TYPE c LENGTH 4.

    METHODS:
      get_rep_data
        IMPORTING
                  im_input_str   TYPE string
                  im_action      LIKE lv_char10
        RETURNING VALUE(et_data) LIKE gt_data.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS YCL_DAILY_DISP_EATHNOL IMPLEMENTATION.


  METHOD get_rep_data.

    TYPES:
      BEGIN OF gty_bukrs,
        bukrs_low TYPE c LENGTH 4,
      END OF gty_bukrs,

      BEGIN OF gty_werks,
        werks_low TYPE c LENGTH 4,
      END OF gty_werks,

      BEGIN OF gty_kunnr,
        kunnr_low TYPE i_customer-customer,
      END OF gty_kunnr.

*      BEGIN OF gty_budat,
*        budat_low TYPE c LENGTH 10,
*      END OF gty_budat.

    DATA:
      gt_kunnr TYPE TABLE OF gty_kunnr,
      gt_bukrs TYPE TABLE OF gty_bukrs,
      gt_werks TYPE TABLE OF gty_werks,
      gs_kunnr TYPE gty_kunnr,
      gs_matnr TYPE gty_bukrs,
      gs_werks TYPE gty_werks.
*      gt_budat TYPE TABLE OF gty_budat,
*      gs_budat TYPE gty_budat.

    TYPES: BEGIN OF gty_input,
             bukrs_low  LIKE gt_bukrs,
             werks_low  LIKE gt_werks,
             budat_low  TYPE c LENGTH 10,
             budat_high TYPE c LENGTH 10,
             kunnr_low  LIKE gt_kunnr,
             kunnr_high TYPE i_customer-customer,
           END OF gty_input.

    DATA:
      gt_input TYPE TABLE OF gty_input,
      gs_input TYPE gty_input.

    DATA :
      r_budat  TYPE RANGE OF zi_dc_note-postingdate,
      rs_budat LIKE LINE OF  r_budat,

      r_kunnr  TYPE RANGE OF i_customer-customer,
      rs_kunnr LIKE LINE OF r_kunnr,

      r_bukrs  TYPE RANGE OF zi_dc_note-companycode,
      rs_bukrs LIKE LINE OF r_bukrs,

      r_werks  TYPE RANGE OF zi_dc_note-plant,
      rs_werks LIKE LINE OF r_werks.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gt_input
                                 ).

    READ TABLE gt_input INTO gs_input INDEX 1.
    CLEAR: r_budat, r_kunnr, r_werks, r_bukrs.

    ""***Preparing range for posting date
    IF gs_input-budat_high IS INITIAL.

      rs_budat-low     = gs_input-budat_low+6(4) && gs_input-budat_low+3(2)  && gs_input-budat_low+0(2).
      "*rs_budat-high    = '' .
      rs_budat-option  = 'EQ' .
      rs_budat-sign    = 'I' .
      APPEND rs_budat TO r_budat.

    ELSE.

      rs_budat-low     = gs_input-budat_low+6(4) && gs_input-budat_low+3(2)  && gs_input-budat_low+0(2).
      rs_budat-high    = gs_input-budat_high+6(4) && gs_input-budat_high+3(2)  && gs_input-budat_high+0(2).
      rs_budat-option  = 'BT'.
      rs_budat-sign    = 'I'.
      APPEND rs_budat TO r_budat.

    ENDIF.

    ""***Preparing range for customer code
    IF gs_input-kunnr_high IS INITIAL.

      LOOP AT gs_input-kunnr_low INTO DATA(ls_kunnr).

        rs_kunnr-low     = ls_kunnr-kunnr_low.
        rs_kunnr-high    = '' .
        rs_kunnr-option  = 'EQ' .
        rs_kunnr-sign    = 'I' .
        APPEND rs_kunnr TO r_kunnr.

        CLEAR: ls_kunnr.
      ENDLOOP.

    ELSE.

      READ TABLE gs_input-kunnr_low INTO ls_kunnr INDEX 1.
      rs_kunnr-low     = ls_kunnr-kunnr_low.
      rs_kunnr-high    = gs_input-kunnr_high.
      rs_kunnr-option  = 'BT' .
      rs_kunnr-sign    = 'I' .
      APPEND rs_kunnr TO r_kunnr.

    ENDIF.

    ""***Preparing range for company code
    LOOP AT gs_input-bukrs_low INTO DATA(ls_bukrs).

      rs_bukrs-low     = ls_bukrs-bukrs_low.
      rs_bukrs-high    = '' .
      rs_bukrs-option  = 'EQ' .
      rs_bukrs-sign    = 'I' .
      APPEND rs_bukrs TO r_bukrs.

      CLEAR: ls_bukrs.
    ENDLOOP.

    ""***Preparing range for plant
    LOOP AT gs_input-werks_low INTO DATA(ls_werks).

      rs_werks-low     = ls_werks-werks_low.
      rs_werks-high    = '' .
      rs_werks-option  = 'EQ' .
      rs_werks-sign    = 'I' .
      APPEND rs_werks TO r_werks.

      CLEAR: ls_werks.
    ENDLOOP.

    SELECT * FROM zi_daily_dispatch_ethanol
             WHERE plant IN @r_werks
             AND soldtoparty IN @r_kunnr
             AND billingdocumentdate IN @r_budat
             INTO TABLE @DATA(it_data).

    IF it_data[] IS NOT INITIAL.
      LOOP AT it_data INTO DATA(ls_data).

        gs_data-plant           = ls_data-plant.
        gs_data-customer        = ls_data-customer.
        gs_data-customer_name   = ls_data-customername.
        gs_data-customer_city   = ls_data-cityname.
        gs_data-ponumber        = ls_data-purchaseorderbycustomer.
        gs_data-totqty          = ls_data-totbilledqty - ls_data-totcancelledqty.
        gs_data-soqty           = ls_data-soqtyf2 - ls_data-soqtys1.
        gs_data-balanceqty      = gs_data-soqty - gs_data-totqty.

        gs_data-day_1           = ls_data-billedqty_1  - ls_data-cancelledqty_1 .
        gs_data-day_2           = ls_data-billedqty_2  - ls_data-cancelledqty_2 .
        gs_data-day_3           = ls_data-billedqty_3  - ls_data-cancelledqty_3 .
        gs_data-day_4           = ls_data-billedqty_4  - ls_data-cancelledqty_4 .
        gs_data-day_5           = ls_data-billedqty_5  - ls_data-cancelledqty_5 .
        gs_data-day_6           = ls_data-billedqty_6  - ls_data-cancelledqty_6 .
        gs_data-day_7           = ls_data-billedqty_7  - ls_data-cancelledqty_7 .
        gs_data-day_8           = ls_data-billedqty_8  - ls_data-cancelledqty_8 .
        gs_data-day_9           = ls_data-billedqty_9  - ls_data-cancelledqty_9 .
        gs_data-day_10          = ls_data-billedqty_10 - ls_data-cancelledqty_10.
        gs_data-day_11          = ls_data-billedqty_11 - ls_data-cancelledqty_11.
        gs_data-day_12          = ls_data-billedqty_12 - ls_data-cancelledqty_12.
        gs_data-day_13          = ls_data-billedqty_13 - ls_data-cancelledqty_13.
        gs_data-day_14          = ls_data-billedqty_14 - ls_data-cancelledqty_14.
        gs_data-day_15          = ls_data-billedqty_15 - ls_data-cancelledqty_15.
        gs_data-day_16          = ls_data-billedqty_16 - ls_data-cancelledqty_16.
        gs_data-day_17          = ls_data-billedqty_17 - ls_data-cancelledqty_17.
        gs_data-day_18          = ls_data-billedqty_18 - ls_data-cancelledqty_18.
        gs_data-day_19          = ls_data-billedqty_19 - ls_data-cancelledqty_19.
        gs_data-day_20          = ls_data-billedqty_20 - ls_data-cancelledqty_20.
        gs_data-day_21          = ls_data-billedqty_21 - ls_data-cancelledqty_21.
        gs_data-day_22          = ls_data-billedqty_22 - ls_data-cancelledqty_22.
        gs_data-day_23          = ls_data-billedqty_23 - ls_data-cancelledqty_23.
        gs_data-day_24          = ls_data-billedqty_24 - ls_data-cancelledqty_24.
        gs_data-day_25          = ls_data-billedqty_25 - ls_data-cancelledqty_25.
        gs_data-day_26          = ls_data-billedqty_26 - ls_data-cancelledqty_26.
        gs_data-day_27          = ls_data-billedqty_27 - ls_data-cancelledqty_27.
        gs_data-day_28          = ls_data-billedqty_28 - ls_data-cancelledqty_28.
        gs_data-day_29          = ls_data-billedqty_29 - ls_data-cancelledqty_29.
        gs_data-day_30          = ls_data-billedqty_30 - ls_data-cancelledqty_30.
        gs_data-day_31          = ls_data-billedqty_31 - ls_data-cancelledqty_31.

        APPEND gs_data TO gt_data.

        CLEAR: ls_data.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
