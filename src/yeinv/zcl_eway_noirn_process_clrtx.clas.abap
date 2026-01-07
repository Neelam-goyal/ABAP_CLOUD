CLASS zcl_eway_noirn_process_clrtx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_eway   TYPE TABLE OF zstr_eway_data,
      gs_eway   TYPE zstr_eway_data,
      gt_final  TYPE TABLE OF zstr_einv_data,
      gs_final  TYPE zstr_einv_data,
      lv_char10 TYPE c LENGTH 10,
      lv_char20 TYPE c LENGTH 20,
      lv_char4  TYPE c LENGTH 4.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    METHODS:
      fetch_billing_data
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_etype           LIKE lv_char10
        RETURNING VALUE(r_eway_data) LIKE gt_final,

      get_token_from_clrtx
        IMPORTING
                  im_action             LIKE lv_char10
        RETURNING VALUE(r_access_token) TYPE string,

      prepare_eway_json_clrtx
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_access_token    TYPE string
                  im_input_str       TYPE string
        RETURNING VALUE(r_eway_json) TYPE string,

      gen_eway_from_clrtx
        IMPORTING
                  im_action         LIKE lv_char10
                  im_eway_json      TYPE string
                  im_input_str      TYPE string
        RETURNING VALUE(r_gen_eway) TYPE string,

      save_eway_data_clrtx
        IMPORTING
                  im_vbeln            LIKE lv_char10
                  im_bukrs            LIKE lv_char4
                  im_fyear            LIKE lv_char4
                  im_plant            LIKE lv_char4
                  im_date             LIKE lv_char10
                  im_eway_resp        TYPE string
                  im_input_str        TYPE string
        RETURNING VALUE(r_saved_resp) TYPE string,

      cancel_eway_clrtx
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_plant_gstin     LIKE lv_char20
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_access_token    TYPE string
                  im_input_str       TYPE string
        RETURNING VALUE(r_eway_canc) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EWAY_NOIRN_PROCESS_CLRTX IMPLEMENTATION.


  METHOD cancel_eway_clrtx.

    TYPES: BEGIN OF gty_resp,
             ownerid      TYPE string,
             gstin        TYPE string,
             irn          TYPE string,
             ewbnumber    TYPE string,
             ewbstatus    TYPE string,
             errordetails TYPE string,
           END OF gty_resp.
    DATA:
      ls_resp   TYPE gty_resp,
      cw_string TYPE string.

    DATA:
      gt_input TYPE TABLE OF zstr_eway_data,
      gs_input TYPE zstr_eway_data.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    ""***Start: Data Preparation***************************
    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gs_input
                                 ).

    IF gs_input IS INITIAL.
      gs_input-billingdocument =  im_vbeln.
      gs_input-companycode     =  im_bukrs.
      gs_input-plant           =  im_plant.
      gs_input-plant_gstin     =  im_plant_gstin.
    ENDIF.

    SELECT SINGLE * FROM zsd_einv_data
    WHERE billingdocument = @gs_input-billingdocument AND
          companycode     = @gs_input-companycode  AND
          fiscalyear      = @lv_fis_year AND
          plant           = @gs_input-plant AND
          plant_gstin     = @gs_input-plant_gstin AND
          ewbno           NE '' AND
          canc_status_eway EQ ''
          INTO @DATA(cs_einv) .               "#EC CI_ALL_FIELDS_NEEDED

    CLEAR: cw_string.
    cw_string = '{'
    && '"ewbNo":' && '"' && cs_einv-ewbno && '",'
    && '"cancelRsnCode": "DATA_ENTRY_MISTAKE",'
    && '"cancelRmrk": "DATA_ENTRY_MISTAKE"'
    && '}'.

    ""***End: Data Preparation***************************

    """"Start:Cancellation:*****************************************************************************************
    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string,
          lv_auth_token  TYPE string,
          lv_owner_id    TYPE string,
          lv_gstin       TYPE string.

*    url = 'https://einvoicing.internal.cleartax.co/v2/eInvoice/ewaybill/cancel'.
*    lv_auth_token = '1.b085ee0a-13b5-4e67-ba49-44ca634cc0af_c5fff1119d7c2772d6aa0172db20c8cc74eeeef1797f43dc46325ac83125412b'.
*    lv_owner_id   = 'b30507d9-49b5-436e-86fd-a1731f30ed3a'.
*    lv_gstin      = '09AAACI4265L1ZA'.

    DATA:
      r_dq  TYPE RANGE OF sy-sysid,
      rw_dq LIKE LINE OF r_dq.

    rw_dq-low    = 'M1C' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    rw_dq-low    = 'PFE' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    IF sy-sysid IN r_dq.
      lv_gstin = '06AAFCD5862R017' ##NO_TEXT.
    ELSE.
      lv_gstin = cs_einv-plant_gstin.
    ENDIF.

    TRY.

        DATA(dest1) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest1 ).

        DATA(lo_request1) = lo_http_client->get_http_request( ).
        lo_request1->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: miw_string.
        miw_string = cw_string.

        lo_request1->append_text(
          EXPORTING
            data = miw_string
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'Host' ##NO_TEXT
            i_value = url
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'X-Cleartax-Auth-Token' ##NO_TEXT
            i_value = lv_auth_token
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'X-Cleartax-Product' ##NO_TEXT
            i_value = 'EInvoice'
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'owner_id' ##NO_TEXT
            i_value = lv_owner_id
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'gstin' ##NO_TEXT
            i_value = lv_gstin
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body1) = lo_response1->get_text( ).
        r_eway_canc = response_body1.

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.
    """"End:Cancellation**********************************************************************

    """**Start: Update table for cancelled document*******************************************
    CLEAR: miw_string.
    miw_string = r_eway_canc.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = miw_string
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_resp
    ).

    IF ls_resp-ewbstatus = 'CANCELLED'.

      cs_einv-cancel_date_eway   = sys_date.
      cs_einv-canc_status_eway   = abap_true.
      cs_einv-cancel_time_eway   = sys_time.
      MODIFY zsd_einv_data FROM @cs_einv.
      COMMIT WORK.

    ENDIF.
    """**End: Update table for cancelled document*********************************************

  ENDMETHOD.


  METHOD fetch_billing_data.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    DATA:
      r_vbeln  TYPE RANGE OF zi_sale_reg-billingdocument,
      r_date   TYPE RANGE OF zi_sale_reg-billingdocumentdate,
      r_plant  TYPE RANGE OF zi_sale_reg-plant,
      wr_vbeln LIKE LINE OF r_vbeln,
      wr_date  LIKE LINE OF r_date,
      wr_plant LIKE LINE OF r_plant.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    IF im_vbeln IS NOT INITIAL.
      wr_vbeln-low    = im_vbeln.
      wr_vbeln-high   = im_vbeln.
      wr_vbeln-sign   = 'I'.
      wr_vbeln-option = 'BT'.
      APPEND wr_vbeln TO r_vbeln.
    ENDIF.

    IF im_date IS NOT INITIAL.
      wr_date-low    = im_date.
      wr_date-high   = im_date.
      wr_date-sign   = 'I'.
      wr_date-option = 'BT'.
      APPEND wr_date TO r_date.
    ENDIF.

    IF im_plant IS NOT INITIAL.
      wr_plant-low    = im_plant.
      wr_plant-high   = im_plant.
      wr_plant-sign   = 'I'.
      wr_plant-option = 'BT'.
      APPEND wr_plant TO r_plant.
    ENDIF.

    SELECT * FROM zi_sale_reg
    WHERE billingdocument     IN @r_vbeln AND
          billingdocumentdate IN @r_date  AND
          plant               IN @r_plant AND
          billingdocumentiscancelled = ''
          AND billingdocumenttype IN ( 'F2','F5','JSN', 'JVR','JDC' , 'F8' )
    INTO TABLE @DATA(it_final) .              "#EC CI_ALL_FIELDS_NEEDED

    IF it_final IS NOT INITIAL.

      SELECT
billingdocument    ,
companycode        ,
fiscalyear         ,
plant_gstin        ,
billingtype        ,
docdate            ,
plant              ,
status             ,
businessmodule     ,
irn                ,
ackno              ,
ackdt              ,
ewayackno          ,
ewayackdt          ,
ewbno              ,
ewbdt              ,
signedinvoice      ,
signedqrcode       ,
ewbvalidtill       ,
qrcodeurl          ,
erdat              ,
uname              ,
uzeit              ,
cancel_status      ,
cancel_date        ,
cancel_time        ,
canc_status_eway   ,
cancel_date_eway   ,
cancel_time_eway   ,
transporterid      ,
modeoftransp       ,
transpdocnum       ,
transdocdate       ,
vehiclenum         ,
vehicletype        ,
transpname         ,
datasource         ,
distance
      FROM zsd_einv_data
      FOR ALL ENTRIES IN @it_final
      WHERE billingdocument  = @it_final-billingdocument AND
            companycode      = @it_final-companycode  AND
            fiscalyear       = @lv_fis_year AND
            plant            = @it_final-plant
            "canc_status_eway = ''
      INTO TABLE @DATA(it_einv) ##SELECT_FAE_WITH_LOB[SIGNEDINVOICE] ##SELECT_FAE_WITH_LOB[SIGNEDQRCODE]. "#EC CI_NO_TRANSFORM

      LOOP AT it_final INTO DATA(ls_final).

        gs_final-billingdocument  = ls_final-billingdocument.
        gs_final-companycode      = ls_final-companycode.
        gs_final-fiscalyear       = lv_fis_year.
        gs_final-docdate          = ls_final-billingdocumentdate.
        gs_final-plant            = ls_final-plant.
        gs_final-plant_gstin      = ls_final-plant_gstin.
        gs_final-status           = ''.

        READ TABLE it_einv INTO DATA(ls_einv) WITH KEY
                           billingdocument   = ls_final-billingdocument
                           companycode       = ls_final-companycode
                           fiscalyear        = lv_fis_year.
        IF sy-subrc EQ 0.

          IF ls_einv-cancel_status = abap_false.
            gs_final-status     = 'Generated' ##NO_TEXT.
            gs_final-success    = 'Y' ##NO_TEXT.
          ELSE.
            gs_final-status     = 'Cancelled' ##NO_TEXT.
            gs_final-success    = 'N' ##NO_TEXT.
          ENDIF.

          gs_final-irn        = ls_einv-irn.
          gs_final-ackno      = ls_einv-ackno.
          gs_final-ackdt      = ls_einv-ackdt.
          gs_final-ewbno      = ls_einv-ewbno.
          gs_final-ewbdt      = ls_einv-ewbdt.

        ENDIF.

        APPEND gs_final TO gt_final.

        CLEAR: ls_final, ls_einv, gs_final.
      ENDLOOP.

      IF im_etype = 'new' ##NO_TEXT.
        DELETE gt_final WHERE status NE ''.
      ELSEIF im_etype = 'generated' ##NO_TEXT.
        DELETE gt_final WHERE status EQ 'Cancelled' ##NO_TEXT.
      ENDIF.

      SORT gt_final BY billingdocument.
      DELETE ADJACENT DUPLICATES FROM gt_final COMPARING billingdocument.
      r_eway_data[] = gt_final[].

    ENDIF.

  ENDMETHOD.


  METHOD gen_eway_from_clrtx.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string,
          lv_auth_token  TYPE string,
          lv_owner_id    TYPE string,
          lv_gstin       TYPE string,
          gs_input       TYPE zstr_eway_data.

    DATA:
      r_dq  TYPE RANGE OF sy-sysid,
      rw_dq LIKE LINE OF r_dq.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gs_input
                                 ).

    rw_dq-low    = 'M1C' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    rw_dq-low    = 'PFE' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    IF sy-sysid IN r_dq.
      lv_gstin = '06AAFCD5862R017' ##NO_TEXT.
    ELSE.
      lv_gstin = gs_input-plant_gstin.
    ENDIF.

    SELECT SINGLE
      gstin,
      ownerid ,
      authtoken,
      url_auth_token,
      url_einv_gen,
      url_einv_canc,
      url_qrb2c,
      url_eway_gen,
      url_eway_irn,
      url_eway_partb,
      url_eway_canc
    FROM zsd_cred_clr
    WHERE gstin = @lv_gstin
    INTO @DATA(ls_clr_cred).                                "#EC WARNOK

    IF sy-subrc EQ 0.
      url           = ls_clr_cred-url_eway_gen.
      lv_auth_token = ls_clr_cred-authtoken.
      lv_gstin      = ls_clr_cred-gstin.
      lv_owner_id   = ls_clr_cred-ownerid.
    ENDIF.

*    DATA: lr_cscn TYPE if_com_scenario_factory=>ty_query-cscn_id_range.
*
*    " find CA by scenario
*    lr_cscn = VALUE #( ( sign = 'I' option = 'EQ' low = 'YY1_MY_COMM' ) ).
*    DATA(lo_factory) = cl_com_arrangement_factory=>create_instance( ).
*    lo_factory->query_ca(
*      EXPORTING
*        is_query           = VALUE #( cscn_id_range = lr_cscn )
*      IMPORTING
*        et_com_arrangement = DATA(lt_ca) ).
*
*    IF lt_ca IS INITIAL.
*      EXIT.
*    ENDIF.
*
*    " take the first one
*    READ TABLE lt_ca INTO DATA(lo_ca) INDEX 1.

    TRY.

        DATA(dest1) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest1 ).

*        DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
*                 comm_scenario  = 'YY1_MY_COMM'
*                 service_id     = 'YY1_API_CALL_REST'
*                 comm_system_id = lo_ca->get_comm_system_id( ) ).
*
*        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).

        DATA(lo_request1) = lo_http_client->get_http_request( ).
        lo_request1->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: miw_string.
        miw_string = im_eway_json.

        lo_request1->append_text(
          EXPORTING
            data = miw_string
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'Host' ##NO_TEXT
            i_value = url
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'X-Cleartax-Auth-Token' ##NO_TEXT
            i_value = lv_auth_token
        ).

*        lo_request1->set_header_field(
*          EXPORTING
*            i_name  = 'X-Cleartax-Product'
*            i_value = 'EInvoice'
*       ).
*
*        lo_request1->set_header_field(
*         EXPORTING
*       i_name  = 'owner_id'
*         i_value = lv_owner_id
*        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'gstin' ##NO_TEXT
            i_value = lv_gstin
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>put ).
        DATA(response_body1) = lo_response1->get_text( ).
        r_gen_eway = response_body1.
        """"*****************************************************************************************

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.


  ENDMETHOD.


  METHOD get_token_from_clrtx.

  ENDMETHOD.


  METHOD prepare_eway_json_clrtx.

    DATA:
      miw_string        TYPE string,
      miw_eway_json_hdr TYPE string,
      miw_eway_json_itm TYPE string,
      lv_tranct_type       TYPE string,
      lv_doc_type       TYPE string,
      lv_subs_type       TYPE string.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    DATA:
      r_vbeln  TYPE RANGE OF zi_sale_reg-billingdocument,
      r_date   TYPE RANGE OF zi_sale_reg-billingdocumentdate,
      r_plant  TYPE RANGE OF zi_sale_reg-plant,
      r_bukrs  TYPE RANGE OF zi_sale_reg-companycode,
      wr_bukrs LIKE LINE OF r_bukrs,
      wr_vbeln LIKE LINE OF r_vbeln,
      wr_date  LIKE LINE OF r_date,
      wr_plant LIKE LINE OF r_plant.

    DATA:
      gt_input TYPE TABLE OF zstr_eway_data,
      gs_input TYPE zstr_eway_data,
      gv_json  TYPE string.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gs_input
                                 ).

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    IF gs_input-billingdocument IS NOT INITIAL.
      wr_vbeln-low    = gs_input-billingdocument.
      wr_vbeln-high   = gs_input-billingdocument.
      wr_vbeln-sign   = 'I'.
      wr_vbeln-option = 'BT'.
      APPEND wr_vbeln TO r_vbeln.
    ENDIF.

    IF im_date IS NOT INITIAL.
      wr_date-low    = im_date.
      wr_date-high   = im_date.
      wr_date-sign   = 'I'.
      wr_date-option = 'BT'.
      APPEND wr_date TO r_date.
    ENDIF.

    IF gs_input-plant IS NOT INITIAL.
      wr_plant-low    = gs_input-plant.
      wr_plant-high   = gs_input-plant.
      wr_plant-sign   = 'I'.
      wr_plant-option = 'BT'.
      APPEND wr_plant TO r_plant.
    ENDIF.

    IF gs_input-companycode IS NOT INITIAL.
      wr_bukrs-low    = gs_input-companycode.
      wr_bukrs-high   = gs_input-companycode.
      wr_bukrs-sign   = 'I'.
      wr_bukrs-option = 'BT'.
      APPEND wr_bukrs TO r_bukrs.
    ENDIF.

    SELECT * FROM zi_sale_reg
    WHERE billingdocument     IN @r_vbeln AND
          billingdocumentdate IN @r_date  AND
          plant               IN @r_plant AND
          companycode         IN @r_bukrs  AND
          billingdocumentiscancelled = ''
    INTO TABLE @DATA(it_final) .              "#EC CI_ALL_FIELDS_NEEDED


    DATA:
      lv_seller_addrs1 TYPE string,
      lv_seller_addrs2 TYPE string,
      lv_buyer_addrs1  TYPE string,
      lv_buyer_addrs2  TYPE string,
      lv_buyer_stcode  TYPE string,
      lv_shipto_addrs1 TYPE string,
      lv_shipto_addrs2 TYPE string,
      lv_buyer_gstin   TYPE c LENGTH 20,
      lv_shipto_gstin  TYPE c LENGTH 20,
      itm_count        TYPE sy-tabix,
      lv_neg_val       TYPE c LENGTH 40.

    DATA:
      lv_val_total_assess       TYPE c LENGTH 40,
      lv_val_total_cgst         TYPE c LENGTH 40,
      lv_val_total_sgst         TYPE c LENGTH 40,
      lv_val_total_igst         TYPE c LENGTH 40,
      lv_val_total_cess         TYPE c LENGTH 40,
      lv_val_total_cess_non     TYPE c LENGTH 40,
      lv_val_other_charge       TYPE c LENGTH 40,
      lv_val_total_invoice      TYPE c LENGTH 40,
      lv_val_total_cess_v_state TYPE c LENGTH 40,
      lv_val_dt_roundoff        TYPE c LENGTH 40,
      lv_val_dt_tot_inv_ac      TYPE c LENGTH 40,
      lv_val_discount           TYPE c LENGTH 40,
      lv_gst_rate               TYPE c LENGTH 40,
      lv_insurance_amt          TYPE c LENGTH 40,
      lv_trans_type             TYPE c LENGTH 4.

    DATA:
      r_dq  TYPE RANGE OF sy-sysid,
      rw_dq LIKE LINE OF r_dq.

    rw_dq-low    = 'M1C' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    rw_dq-low    = 'PFE' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    READ TABLE it_final INTO DATA(ls_final_hdr) INDEX 1. "#EC CI_NOORDER
    DATA(line_itm) = lines( it_final[] ).

    DATA : lvdt TYPE c LENGTH 10 .
    lvdt = |{ ls_final_hdr-billingdocumentdate+6(2) }/{ ls_final_hdr-billingdocumentdate+4(2) }/{ ls_final_hdr-billingdocumentdate+0(4) }| .


    CLEAR: miw_string, miw_eway_json_hdr, miw_eway_json_itm.
    ls_final_hdr-doc_type = 'CHN' ##NO_TEXT.

    ls_final_hdr-billingdocument = |{ ls_final_hdr-billingdocument ALPHA = OUT }| .


    """""" FOR TESTING PURPOSE ....
    IF sy-sysid IN r_dq.
      ls_final_hdr-plant_gstin = '06AAFCD5862R017' ##NO_TEXT.
    ENDIF.
    """""" FOR TESTING PURPOSE ....

    SELECT SINGLE * FROM zsd_disp_from
    WHERE storagelocation = @ls_final_hdr-storagelocation
    INTO @DATA(ls_disp_from).
    IF sy-subrc NE 0.

      ls_disp_from-dispatchfrmname  = ls_final_hdr-plantname.
      ls_disp_from-dispatchfrmaddr1 = ls_final_hdr-streetname.
      ls_disp_from-dispatchfrmaddr2 = ls_final_hdr-streetname  && ls_final_hdr-street.
      ls_disp_from-dispatchfrmloc   = ls_final_hdr-cityname.
      ls_disp_from-dispatchfrmpin   = ls_final_hdr-postalcode.
      ls_disp_from-dispatchfrmstcd  = ls_final_hdr-plant_gstin+0(2).

    ENDIF.

    IF ls_final_hdr-customerpricegroup = 'C2'.
      lv_doc_type  = 'BOS'.
      lv_subs_type = 'supply'.
    ELSE.
      lv_doc_type  = 'Delivery Challan'.
      lv_subs_type = 'Others'.
    ENDIF.

    IF ( ls_final_hdr-ship_to_party NE ls_final_hdr-bill_to_party ) .
      lv_trans_type = 'SHP' .
    ENDIF .
    IF lv_trans_type EQ 'SHP'.
     lv_tranct_type = 'BillToShipTo'.
    else.
     lv_tranct_type = 'Regular'.
    ENDIF.

    IF ls_final_hdr-billingdocumenttype = 'F8'.
       ls_final_hdr-we_name = ls_final_hdr-re_name.
       ls_final_hdr-we_street1 = ls_final_hdr-re_street1.
       ls_final_hdr-we_street = ls_final_hdr-re_street.
       ls_final_hdr-we_street1 = ls_final_hdr-re_street1.
       SELECT SINGLE districtname from i_customer  where customer = @ls_final_hdr-bill_to_party into @ls_final_hdr-re_city.
       ls_final_hdr-we_city = ls_final_hdr-re_city.
       ls_final_hdr-we_pin = ls_final_hdr-re_pin.
       ls_final_hdr-we_tax+0(2) = ls_final_hdr-re_tax+0(2).
       lv_tranct_type = 'Regular'.
    ENDIF.


    miw_eway_json_hdr =
    '{'
       && '"DocumentNumber":'   && '"' && ls_final_hdr-billingdocument && '",'
        && '"DocumentType":'    && '"' && lv_doc_type  && '",'
        && '"DocumentDate":'    && '"' && lvdt && '",'
        && '"SupplyType": "Outward",'
        && '"SubSupplyType":'    && '"' && lv_subs_type  && '",'
        && '"SubSupplyTypeDesc": "NA",'
        && '"TransactionType":'    && '"' && lv_tranct_type  && '",'

        && '"BuyerDtls": {'
            && '"Gstin":'      && '"' && ls_final_hdr-re_tax && '",'
            && '"LglNm":'      && '"' && ls_final_hdr-re_name && '",'
            && '"TrdNm":'      && '"' && ls_final_hdr-re_name && '",'
            && '"Addr1":'      && '"' && ls_final_hdr-re_street1 && '",'
            && '"Addr2":'      && '"' && ls_final_hdr-re_street && ls_final_hdr-re_street1 && '",'
            && '"Loc":'        && '"' && ls_final_hdr-re_city && '",'
            && '"Pin":'        &&        ls_final_hdr-re_pin && ','
            && '"Stcd":'       && '"' && ls_final_hdr-re_tax+0(2) && '"'
        && '},'

        && '"SellerDtls": {'
            && '"Gstin":' && '"' && ls_final_hdr-plant_gstin && '",'
            && '"LglNm":' && '"' && ls_final_hdr-plantname && '",'
            && '"TrdNm":' && '"' && ls_final_hdr-plantname && '",'
            && '"Addr1":' && '"' && ls_final_hdr-streetname && '",'
            && '"Addr2":' && '"' && ls_final_hdr-street && '",'
            && '"Loc":'   && '"' && ls_final_hdr-cityname && '",'
            && '"Pin":'   && ls_final_hdr-postalcode && ','
            && '"Stcd":'  && '"' && ls_final_hdr-plant_gstin+0(2) && '"'
        && '},' ##NO_TEXT.

    miw_eway_json_hdr = miw_eway_json_hdr
          && '"DispDtls": {'
          && '"Nm":'      && '"' && ls_disp_from-dispatchfrmname && '",'
          && '"Addr1":'   && '"' && ls_disp_from-dispatchfrmaddr1 && '",'
          && '"Addr2":'   && '"' && ls_disp_from-dispatchfrmaddr2 && '",'
          && '"Loc":'     && '"' && ls_disp_from-dispatchfrmloc && '",'
          && '"Pin":'     && '"' && ls_disp_from-dispatchfrmpin && '",'
          && '"Stcd":'    && '"' && ls_disp_from-dispatchfrmstcd && '"'
          && '},'.

    IF  lv_trans_type EQ 'SHP'.
      miw_eway_json_hdr = miw_eway_json_hdr
      && '"ExpShipDtls": {'
      && '"LglNm":'   && '"' && ls_final_hdr-we_name && '",'
      && '"Addr1":'   && '"' && ls_final_hdr-we_street1 && '",'
      && '"Addr2":'   && '"' && ls_final_hdr-we_street && ls_final_hdr-we_street1 && '",'
      && '"Loc":'     && '"' && ls_final_hdr-we_city && '",'
      && '"Pin":'     && ls_final_hdr-we_pin && ','
      && '"Stcd":'    && '"' && ls_final_hdr-we_tax+0(2) && '"'
      && '},'.
    ENDIF.

    miw_eway_json_hdr = miw_eway_json_hdr
    && '"ItemList":' && '['
    &&   '{'.                                           "#EC CI_NOORDER

    lv_val_total_cess          = '0'.
    lv_val_total_cess_v_state  = '0'.
    lv_val_discount            = '0'.
    lv_val_other_charge        = '0'.
    lv_val_dt_roundoff         = '0'.
    lv_val_total_cess_non      = '0'.

    CLEAR: itm_count.
    LOOP AT it_final INTO DATA(ls_itm).

      itm_count   = itm_count + 1.

      IF ls_itm-baseunit = 'EA' .
        ls_itm-baseunit = 'NOS' .
      ELSEIF ls_itm-baseunit = 'L' .
        ls_itm-baseunit = 'LTR' .
      ELSEIF ls_itm-baseunit = 'KG'.
        ls_itm-baseunit = 'KGS' .
      ELSEIF ls_itm-baseunit = 'M'  .
        ls_itm-baseunit = 'MTR' .
      ELSEIF ls_itm-baseunit = 'ST' .
        ls_itm-baseunit = 'NOS' .
      ENDIF .

      DATA:
        lv_bill_qty TYPE p DECIMALS 2.

      lv_bill_qty     = ls_itm-netweight / 100. "ls_itm-billingquantity.
      ls_itm-baseunit = 'QTL'.

      ls_itm-item_assessableamount_inr = ls_itm-item_assessableamount_inr.
      "ls_itm-item_insurance_amt.

      ls_itm-item_othercharge = ls_itm-item_othercharge +
                                ls_itm-item_freight + ls_itm-item_insurance_amt.

      miw_eway_json_itm = miw_eway_json_itm

                  && '"ProdName":'         && '"' && ls_itm-billingdocumentitem && '",'
                  && '"ProdDesc":'         && '"' && ls_itm-billingdocumentitemtext && '",'
                  && '"HsnCd":'            && '"' && ls_itm-hsn && '",'
                  && '"Qty":'              && lv_bill_qty && ','
                  && '"Unit":'             && '"' && ls_itm-baseunit && '",'
                  && '"AssAmt":'           && ls_itm-item_assessableamount_inr && ','
                  && '"CgstRt":'           && ls_itm-item_cgstrate && ','
                  && '"CgstAmt":'          && ls_itm-item_cgstamount_inr && ','
                  && '"SgstRt":'           && ls_itm-item_sgstrate && ','
                  && '"SgstAmt":'          && ls_itm-item_sgstamount_inr && ','
                  && '"IgstRt":'           && ls_itm-item_igstrate && ','
                  && '"IgstAmt":'          && ls_itm-item_igstamount_inr && ','
                  && '"CesRt": 0,'
                  && '"CesAmt": 0,'
                  && '"OthChrg":'          && ls_itm-item_othercharge && ','
                  && '"CesNonAdvAmt": 0'.               "#EC CI_NOORDER

      IF line_itm EQ itm_count.
        miw_eway_json_itm = miw_eway_json_itm &&  '}'.  "#EC CI_NOORDER
      ELSE.
        miw_eway_json_itm = miw_eway_json_itm &&  '},{'. "#EC CI_NOORDER
      ENDIF.

      lv_val_total_assess        = lv_val_total_assess + ls_itm-item_assessableamount_inr .
      lv_val_total_cgst          = lv_val_total_cgst + ls_itm-item_cgstamount_inr.
      lv_val_total_sgst          = lv_val_total_sgst + ls_itm-item_sgstamount_inr.
      lv_val_total_igst          = lv_val_total_igst + ls_itm-item_igstamount_inr.
      lv_val_other_charge        = lv_val_other_charge + ls_itm-item_othercharge .

*      lv_neg_val = ls_itm-item_roundoff.
*      CONDENSE lv_neg_val.
*      IF lv_neg_val CA '-'.
*        ls_itm-item_roundoff = ls_itm-item_roundoff * -1.
*      ENDIF.

      lv_val_dt_roundoff         = lv_val_dt_roundoff + ls_itm-item_roundoff .

      CLEAR: ls_itm, lv_neg_val.
    ENDLOOP.

    lv_val_total_invoice = lv_val_total_assess + lv_val_total_cgst + lv_val_dt_roundoff +
                           lv_val_total_sgst + lv_val_total_igst + lv_val_other_charge .

    clear lv_neg_val.
    lv_neg_val = lv_val_dt_roundoff.
    CONDENSE lv_neg_val.
    IF lv_neg_val CA '-'.
       lv_val_dt_roundoff = lv_val_dt_roundoff * -1.
    ENDIF.

    miw_eway_json_itm = miw_eway_json_itm && '],'.

    miw_eway_json_hdr = miw_eway_json_hdr && miw_eway_json_itm

        && '"TotalInvoiceAmount":'      && lv_val_total_invoice && ','
        && '"TotalCgstAmount":'         && lv_val_total_cgst  && ','
        && '"TotalSgstAmount":'         && lv_val_total_sgst  && ','
        && '"TotalIgstAmount":'         && lv_val_total_igst  && ','
        && '"TotalCessAmount":'         && lv_val_total_cess  && ','
        && '"TotalCessNonAdvolAmount":' && lv_val_total_cess_non  && ','
        && '"TotalAssessableAmount":'   && lv_val_total_assess  && ','
        && '"OtherAmount":'             && lv_val_other_charge  && ','
        && '"OtherTcsAmount":'          && lv_val_dt_roundoff  && ','

        && '"TransId":'     && '"' && gs_input-transporterid && '",'
        && '"TransName":'   && '"' && gs_input-transpname && '",'
        && '"TransMode":'   && '"' && gs_input-modeoftransp && '",'  """"""ROAD",
        && '"Distance":'    && '"' && gs_input-distance && '",'      """"""""0,'
        && '"TransDocNo":'  && '"' && gs_input-transpdocnum && '",'
        && '"TransDocDt":'  && '"' && gs_input-transdocdate && '",'
        && '"VehNo":'       && '"' && gs_input-vehiclenum && '",'
        && '"VehType":'     && '"' && gs_input-vehicletype && '"'    """"REGULAR"'

    && '}'.                                             "#EC CI_NOORDER

    miw_string = miw_eway_json_hdr.
*      && miw_eway_json_itm.

    r_eway_json =  miw_string.

  ENDMETHOD.


  METHOD save_eway_data_clrtx.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    DATA:
      miw_string TYPE string.

    DATA:
      gt_input TYPE TABLE OF zstr_eway_data,
      gs_input TYPE zstr_eway_data.

    TYPES: BEGIN OF gty_resp,
             success      TYPE string,
             status       TYPE string,
             ewbno        TYPE string,
             ewbdt        TYPE string,
             ewbvalidtill TYPE string,
             alert        TYPE string,
           END OF gty_resp.

    DATA:
      ls_resp TYPE gty_resp,
      gt_data TYPE TABLE OF zsd_einv_data,
      gs_data TYPE zsd_einv_data.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gs_input
                                 ).

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    SELECT SINGLE * FROM zsd_einv_data
    WHERE billingdocument = @gs_input-billingdocument AND
          companycode     = @gs_input-companycode  AND
          fiscalyear      = @lv_fis_year AND
          plant           = @gs_input-plant AND
          plant_gstin     = @gs_input-plant_gstin AND
          ewbno           EQ ''
          INTO @DATA(ls_einv) .               "#EC CI_ALL_FIELDS_NEEDED

    """*****Deserialize response******************************

    SPLIT im_eway_resp AT '"govt_response":' INTO DATA(lv_resp_1) DATA(lv_resp_2).
    SPLIT lv_resp_2    AT '},' INTO DATA(lv_resp_3) DATA(lv_resp_4).
    lv_resp_3 = lv_resp_3 && '}'.

    miw_string = lv_resp_3.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = miw_string
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_resp
    ).

    IF ls_resp-success = 'Y'.

      ls_einv-billingdocument = gs_input-billingdocument.
      ls_einv-companycode     = gs_input-companycode.
      ls_einv-fiscalyear      = lv_fis_year.
      ls_einv-plant_gstin     = gs_input-plant_gstin.
      ls_einv-plant           = gs_input-plant.
      ls_einv-erdat           = sys_date.
      ls_einv-uname           = sys_uname.
      ls_einv-uzeit           = sys_time.

      ls_einv-transporterid = gs_input-transporterid.
      ls_einv-modeoftransp  = gs_input-modeoftransp.
      ls_einv-transpdocnum  = gs_input-transpdocnum.
      ls_einv-transdocdate  = gs_input-transdocdate.
      ls_einv-vehiclenum    = gs_input-vehiclenum.
      ls_einv-vehicletype   = gs_input-vehicletype.
      ls_einv-transpname    = gs_input-transpname.
      ls_einv-datasource    = gs_input-datasource.
      ls_einv-distance      = gs_input-distance.
      ls_einv-ewbno         = ls_resp-ewbno.
      ls_einv-ewbdt         = sys_date.
      ls_einv-ewayackno     = ''.
      ls_einv-ewayackdt     = ''.

      MODIFY zsd_einv_data FROM @ls_einv.
      IF sy-subrc EQ 0.
        COMMIT WORK.
      ENDIF.
      CLEAR: ls_einv.

      r_saved_resp = miw_string.

    ELSE.

      r_saved_resp = im_eway_resp.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
