CLASS zcl_co11n_conf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    DATA:
      gt_odr    TYPE TABLE OF zpp_order_conf,
      gt_prop   TYPE TABLE OF zstr_goodsmvt_prop,
      lv_char10 TYPE c LENGTH 10,
      lv_char2  TYPE c LENGTH 2.


    METHODS:
      fetch_order_data
        IMPORTING
                  im_action     LIKE lv_char10
                  im_date       LIKE lv_char10
        RETURNING VALUE(et_odr) LIKE gt_odr,

      get_prod_order_proposal
        IMPORTING
                  im_action      LIKE lv_char10
                  it_odr         LIKE gt_odr
        RETURNING VALUE(et_prop) LIKE gt_prop,

      conf_prod_order
        IMPORTING
                  im_action            LIKE lv_char10
                  it_odr               LIKE gt_odr
                  it_prop              LIKE gt_prop
        RETURNING VALUE(et_odr_status) LIKE gt_odr,

      save_order_data
        IMPORTING
                  im_action     LIKE lv_char10
                  im_input_str  TYPE string
        RETURNING VALUE(es_save_stat) TYPE string.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CO11N_CONF IMPLEMENTATION.


  METHOD conf_prod_order.

    "API endpoint for API sandbox
    DATA: lv_url      TYPE string,
          lv_url1     TYPE string,
          lv_url2     TYPE string,
          lv_api_pass TYPE string,
          response    TYPE string,
          miw_string  TYPE string.

    DATA: lv_sysid   TYPE zsd_sysid-sysid,
          lv_obj_val TYPE zsd_sysid-objvalue,
          lv_sys_url TYPE zsd_sysid-objvalue.

    DATA:
      lv_str_hdr       TYPE string,
      lv_str_itm       TYPE string,
      lv_str_othr1     TYPE string,
      lv_str_othr2     TYPE string,
      miw_string_final TYPE string.

    DATA:
      lv_OrderID        TYPE string,
      lv_OrderOperation TYPE string,
      lv_Sequence       TYPE string.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'APIUSRPSS' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid_pass).

    IF sy-subrc EQ 0.
      lv_obj_val = ls_sysid_pass-objvalue.
    ENDIF.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'SYSURL' AND sysid = @sy-sysid
                    INTO @DATA(ls_sys_url).

    IF sy-subrc EQ 0.
      lv_sys_url = ls_sys_url-objvalue.
      CONDENSE lv_sys_url.
    ENDIF.

    """"***Start: Data/JSON Prepration***************************
    miw_string =
    '{'
        && '"OrderID": "5000003",'
        && '"Sequence": "0",'
        && '"OrderOperation": "0010",'
        && '"Material": "TESTSFG",'
        && '"ConfirmationYieldQuantity": "10",'
        && '"to_ProdnOrdConfMatlDocItm": {'
            && '"results": ['
                && '{'
                    && '"OrderID": "5000003",'
                    && '"Material": "TESTRM",'
                    && '"Plant": "1101",'
                    && '"StorageLocation": "S001",'
                    && '"Batch": "0000000301",'
                    && '"GoodsMovementType": "261",'
                    && '"EntryUnit": "EA",'
                    && '"QuantityInEntryUnit": "1"'
                && '},'
                && '{'
                    && '"OrderID": "5000003",'
                    && '"OrderItem": "1",'
                    && '"Material": "TESTSFG",'
                    && '"Plant": "1101",'
                    && '"StorageLocation": "S001",'
                    && '"Batch": "0000000302",'
                    && '"GoodsMovementType": "101",'
                    && '"EntryUnit": "EA",'
                    && '"GoodsMovementRefDocType": "F",'
                    && '"QuantityInEntryUnit": "1"'
                && '}'
            && ']'
        && '}'
    && '}'.

    miw_string_final = miw_string.
    """"***End: Data/JSON Prepration*****************************

    IF lv_sys_url IS NOT INITIAL.

      lv_url     = lv_sys_url && '/sap/opu/odata/sap/API_PROD_ORDER_CONFIRMATION_2_SRV/ProdnOrdConf2'.
      lv_api_pass = lv_obj_val.

    ENDIF.

    TRY.
        "create http destination by url; API endpoint for API sandbox
        DATA(lo_http_destination) = cl_http_destination_provider=>create_by_url( lv_url ).

        "create HTTP client by destination
        DATA(lo_web_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ).

        "adding headers with API Key for API Sandbox
        DATA(lo_web_http_request) = lo_web_http_client->get_http_request( ).

        lo_web_http_request->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        lo_web_http_request->set_authorization_basic( i_username = 'api_user'
                                              i_password = lv_api_pass ).

        lo_web_http_request->append_text(
          EXPORTING
            data = miw_string_final
        ).

        """*****Calling Proposal API to get CSRF Token**************************
        lo_web_http_request->set_header_fields( VALUE #(
        (  name = 'APIKey' value = 'auKKAVy7tpKvqcJq8JchjOflWWliK571' )
        (  name = 'DataServiceVersion' value = '2.0' )
        (  name = 'Accept' value = 'application/json' )
        (  name = 'x-csrf-token' value = 'fetch' )
*        (  name = 'If-Match' value = '*' )
         ) ) ##NO_TEXT.

        "set request method and execute request
        DATA(lo_web_http_response12) = lo_web_http_client->execute( if_web_http_client=>head ).
        DATA(lv_response_csrf) = lo_web_http_response12->get_header_field(
            i_name  = 'x-csrf-token'
        ).

        """*****Calling Proposal API to get Proposal**************************
        lo_web_http_request->set_header_fields( VALUE #(
        (  name = 'APIKey' value = 'auKKAVy7tpKvqcJq8JchjOflWWliK571' )
        (  name = 'DataServiceVersion' value = '2.0' )
        (  name = 'Accept' value = 'application/json' )
        (  name = 'x-csrf-token' value = lv_response_csrf )
*        (  name = 'If-Match' value = '*' )
         ) ) ##NO_TEXT.

*        CATCH cx_web_message_error.
        DATA(lo_web_http_response) = lo_web_http_client->execute( if_web_http_client=>post ).
        DATA(lv_response) = lo_web_http_response->get_text( ).

        ""***Start: Converting response in internal table*****************************
        DATA:
          lr_data     TYPE REF TO data.

        FIELD-SYMBOLS:
          <lt_table> TYPE STANDARD TABLE.

        FREE: lr_data.
        /ui2/cl_json=>deserialize(
                EXPORTING json = lv_response
                   pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                   CHANGING data = lr_data
             ).

        ASSIGN lr_data->* TO FIELD-SYMBOL(<ls_data>).
        " Map the ADD_TEXT field
        ASSIGN COMPONENT 'D' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<ld_add>).

*        IF <ld_add> IS ASSIGNED.
*          ls_srl-updatestatus = 'Success'.
*          UNASSIGN <ld_add>.
*          UNASSIGN <ls_data>.
*        ELSE.
*          ls_srl-updatestatus = 'Error in updation'.
*        ENDIF.
        ""***End: Converting response in internal table*****************************

      CATCH cx_http_dest_provider_error cx_web_http_client_error cx_web_message_error ##NO_HANDLER.
        "error handling
    ENDTRY.

  ENDMETHOD.


  METHOD fetch_order_data.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM zpp_order_conf
             WHERE erdat EQ @sys_date AND
                   update_status eq ''
             INTO TABLE @DATA(gt_odr).

    et_odr[] = gt_odr[].

  ENDMETHOD.


  METHOD get_prod_order_proposal.

    """**Start:Response JSON Structure******************

    TYPES: BEGIN OF ts_metadata,
             type TYPE c LENGTH 120,
           END OF ts_metadata.

    TYPES: BEGIN OF ts_res,
             batch                   TYPE c LENGTH 40,
             entryunit               TYPE c LENGTH 40,
             goodsmovementrefdoctype TYPE c LENGTH 40,
             goodsmovementtype       TYPE c LENGTH 40,
             material                TYPE c LENGTH 40,
             orderid                 TYPE c LENGTH 40,
             orderitem               TYPE c LENGTH 40,
             plant                   TYPE c LENGTH 40,
             quantityinentryunit     TYPE c LENGTH 40,
             __metadata              TYPE ts_metadata,
           END OF ts_res.

    DATA: lt_res TYPE TABLE OF ts_res.

    TYPES: BEGIN OF ts_meta,
             __metadata TYPE ts_metadata,
             results    LIKE lt_res,
           END OF ts_meta.

    TYPES: BEGIN OF ts_json,
             d TYPE ts_meta,
           END OF ts_json.

    DATA ls_data TYPE ts_json.

    TYPES: BEGIN OF ts_prop,
             batch                   TYPE c LENGTH 40,
             entryunit               TYPE c LENGTH 40,
             goodsmovementrefdoctype TYPE c LENGTH 40,
             goodsmovementtype       TYPE c LENGTH 40,
             material                TYPE c LENGTH 40,
             orderid                 TYPE c LENGTH 40,
             orderitem               TYPE c LENGTH 40,
             plant                   TYPE c LENGTH 40,
             quantityinentryunit     TYPE c LENGTH 40,
           END OF ts_prop.

    DATA:
      gt_prop TYPE TABLE OF ts_prop,
      gs_prop TYPE ts_prop.

    """**End:Response JSON Structure******************

    "API endpoint for API sandbox
    DATA: lv_url      TYPE string,
          lv_url1     TYPE string,
          lv_url2     TYPE string,
          lv_api_pass TYPE string,
          response    TYPE string,
          miw_string  TYPE string.

    DATA: lv_sysid   TYPE zsd_sysid-sysid,
          lv_obj_val TYPE zsd_sysid-objvalue,
          lv_sys_url TYPE zsd_sysid-objvalue.

    DATA:
      lv_OrderID        TYPE string,
      lv_OrderOperation TYPE string,
      lv_Sequence       TYPE string.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'APIUSRPSS' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid_pass).

    IF sy-subrc EQ 0.
      lv_obj_val = ls_sysid_pass-objvalue.
    ENDIF.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'SYSURL' AND sysid = @sy-sysid
                    INTO @DATA(ls_sys_url).

    IF sy-subrc EQ 0.
      lv_sys_url = ls_sys_url-objvalue.
      CONDENSE lv_sys_url.
    ENDIF.


    IF lv_sys_url IS NOT INITIAL.

      lv_url1     = lv_sys_url && '/sap/opu/odata/SAP/API_PROD_ORDER_CONFIRMATION_2_SRV/GetGdsMvtProposal?'.

      lv_OrderID         = '5000003'.
      lv_OrderOperation  = '0010'.
      lv_Sequence        = '0'.

      lv_url2 = |OrderID='{ lv_OrderID }'&OrderOperation='{ lv_OrderOperation }'&Sequence='{ lv_Sequence }'|.

      lv_url      = lv_url1 && lv_url2.
      lv_api_pass = lv_obj_val.

    ENDIF.

    TRY.
        "create http destination by url; API endpoint for API sandbox
        DATA(lo_http_destination) = cl_http_destination_provider=>create_by_url( lv_url ).

        "create HTTP client by destination
        DATA(lo_web_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ).

        "adding headers with API Key for API Sandbox
        DATA(lo_web_http_request) = lo_web_http_client->get_http_request( ).

*        lo_web_http_request->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        lo_web_http_request->set_authorization_basic( i_username = 'api_user'
                                              i_password = lv_api_pass ).

        """*****Calling Proposal API to get CSRF Token**************************
        lo_web_http_request->set_header_fields( VALUE #(
        (  name = 'APIKey' value = 'auKKAVy7tpKvqcJq8JchjOflWWliK571' )
        (  name = 'DataServiceVersion' value = '2.0' )
        (  name = 'Accept' value = 'application/json' )
        (  name = 'x-csrf-token' value = 'fetch' )
        (  name = 'If-Match' value = '*' )
         ) ) ##NO_TEXT.

        "set request method and execute request
        DATA(lo_web_http_response12) = lo_web_http_client->execute( if_web_http_client=>head ).
        DATA(lv_response_csrf) = lo_web_http_response12->get_header_field(
            i_name  = 'x-csrf-token'
        ).

        """*****Calling Proposal API to get Proposal**************************
        lo_web_http_request->set_header_fields( VALUE #(
        (  name = 'APIKey' value = 'auKKAVy7tpKvqcJq8JchjOflWWliK571' )
        (  name = 'DataServiceVersion' value = '2.0' )
        (  name = 'Accept' value = 'application/json' )
        (  name = 'x-csrf-token' value = lv_response_csrf )
        (  name = 'If-Match' value = '*' )
         ) ) ##NO_TEXT.

*        CATCH cx_web_message_error.
        DATA(lo_web_http_response) = lo_web_http_client->execute( if_web_http_client=>post ).
        DATA(lv_response) = lo_web_http_response->get_text( ).

        /ui2/cl_json=>deserialize(
          EXPORTING
            json = lv_response
          CHANGING
            data = ls_data
        ).

        IF ls_data IS NOT INITIAL.
          DATA(lt_prop) = ls_data-d-results[].
          et_prop[] = lt_prop[].
        ENDIF.

      CATCH cx_http_dest_provider_error cx_web_http_client_error cx_web_message_error ##NO_HANDLER.
        "error handling
    ENDTRY.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA:
      lv_action TYPE c LENGTH 10,
      lv_date   TYPE c LENGTH 10,
      lt_odr    TYPE TABLE OF zpp_order_conf,
      lt_prop   TYPE TABLE OF zstr_goodsmvt_prop.

    me->fetch_order_data(
      EXPORTING
        im_action = lv_action
        im_date   = lv_date
      RECEIVING
        et_odr    = lt_odr
    ).

    me->get_prod_order_proposal(
      EXPORTING
        im_action     = lv_action
        it_odr        = lt_odr
      RECEIVING
        et_prop = lt_prop
    ).

    me->conf_prod_order(
      EXPORTING
        im_action     = lv_action
        it_odr        = lt_odr
        it_prop       = lt_prop
      RECEIVING
        et_odr_status = lt_odr
    ).

  ENDMETHOD.


  METHOD save_order_data.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    DATA:
      gt_input     TYPE TABLE OF zpp_order_conf,
      gs_input     TYPE zpp_order_conf.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gt_input
                                 ).

    if gt_input[] is NOT INITIAL.

     LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<lfs_input>).

      <lfs_input>-erdat = sys_date.
      <lfs_input>-uname = sys_uname.
      <lfs_input>-uzeit = sys_time.

     ENDLOOP.

     MODIFY zpp_order_conf FROM TABLE @gt_input.
     if sy-subrc eq 0.
      es_save_stat = 'Data saved successfully in SAP' ##NO_TEXT.
     ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
