CLASS zcl_einv_process_clrtx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
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

    DATA:
      gt_fidata TYPE TABLE OF zstr_fi_debit_note,
      gs_dbnote TYPE zstr_fi_debit_note.

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
                  im_module          LIKE lv_char10
        RETURNING VALUE(r_einv_data) LIKE gt_final,

      fetch_finance_data
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_etype           LIKE lv_char10
                  im_module          LIKE lv_char10
        RETURNING VALUE(r_einv_data) LIKE gt_final,

      prepare_einv_json_clrtx
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_module          LIKE lv_char10
                  im_access_token    TYPE string
        RETURNING VALUE(r_einv_json) TYPE string,

      prepare_fi_einv_data
        IMPORTING
                  im_vbeln         LIKE lv_char10
                  im_bukrs         LIKE lv_char4
                  im_fyear         LIKE lv_char4
                  im_plant         LIKE lv_char4
                  im_date          LIKE lv_char10
                  iv_action        LIKE lv_char10
                  im_module        LIKE lv_char10
                  im_access_token  TYPE string
        RETURNING VALUE(et_fidata) LIKE gt_fidata,

      prepare_fi_einv_json_clrtx
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_module          LIKE lv_char10
                  im_access_token    TYPE string
        RETURNING VALUE(r_einv_json) TYPE string,

      get_token_from_clrtx
        IMPORTING
                  im_action             LIKE lv_char10
        RETURNING VALUE(r_access_token) TYPE string,

      gen_einv_from_clrtx
        IMPORTING
                  im_action         LIKE lv_char10
                  im_gstin          LIKE lv_char20
                  im_einv_json      TYPE string
        RETURNING VALUE(r_gen_einv) TYPE string,

      save_irn_data_clrtx
        IMPORTING
                  im_vbeln            LIKE lv_char10
                  im_bukrs            LIKE lv_char4
                  im_fyear            LIKE lv_char4
                  im_plant            LIKE lv_char4
                  im_date             LIKE lv_char10
                  im_module           LIKE lv_char10
                  im_einv_resp        TYPE string
        RETURNING VALUE(r_saved_resp) TYPE string,

      cancel_einv_irn_clrtx
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_access_token    TYPE string
        RETURNING VALUE(r_einv_canc) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EINV_PROCESS_CLRTX IMPLEMENTATION.


  METHOD cancel_einv_irn_clrtx.

    TYPES: BEGIN OF gty_resp1,
             success TYPE string,
             ackno   TYPE string,
             ackdt   TYPE string,
             irn     TYPE string,
             status  TYPE string,
           END OF gty_resp1.

    DATA:
      einv_resp     TYPE gty_resp1,
      ls_resp_final TYPE gty_resp1.

    TYPES: BEGIN OF gty_resp,
             custom_fields   TYPE c LENGTH 20,
             deleted         TYPE c LENGTH 20,
             document_status TYPE c LENGTH 20,
             error_response  TYPE c LENGTH 20,
             errors          TYPE c LENGTH 20,
             govt_response   LIKE einv_resp,
           END OF gty_resp.

    DATA: ls_einv_resp TYPE gty_resp,
          lt_einv_resp TYPE TABLE OF gty_resp.

    ""***Start: Data Preparation***************************
    DATA:
      cw_string TYPE string.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT SINGLE * FROM zsd_einv_data
    WHERE billingdocument = @im_vbeln AND
          companycode     = @im_bukrs  AND
          fiscalyear      = @im_fyear AND
*          plant           = @im_plant AND
          cancel_status   = ''
          INTO @DATA(cs_einv) . "#EC WARNOK              "#EC CI_ALL_FIELDS_NEEDED

    CLEAR: cw_string.
    cw_string = '['
    && '{'
    && '"irn":' && '"' && cs_einv-irn && '",'
    && '"CnlRsn": "1",'
    && '"CnlRem": "Wrong"'
    && '}'
    && ']'.

    ""***End: Data Preparation***************************

    """"Start:Cancellation:*****************************************************************************************
    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string,
          lv_auth_token  TYPE string,
          lv_owner_id    TYPE string,
          lv_gstin       TYPE string.

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

    SELECT SINGLE *
    FROM zsd_cred_clr
    WHERE gstin = @lv_gstin
    INTO @DATA(ls_clr_cred). "#EC WARNOK                 "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      url           = ls_clr_cred-url_einv_canc.
      lv_auth_token = ls_clr_cred-authtoken.
      lv_gstin      = ls_clr_cred-gstin.
      lv_owner_id   = ls_clr_cred-ownerid.
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
            i_value = 'EInvoice' ##NO_TEXT
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
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>put ).
        DATA(response_body1) = lo_response1->get_text( ).
        "*r_einv_canc = response_body1.

        /ui2/cl_json=>deserialize(
                        EXPORTING json = response_body1
                           pretty_name = /ui2/cl_json=>pretty_mode-none
                           CHANGING data = lt_einv_resp
                     ).

        IF lt_einv_resp[] IS NOT INITIAL.

          READ TABLE lt_einv_resp INTO ls_einv_resp INDEX 1.

          IF ls_einv_resp-govt_response IS NOT INITIAL.

            ls_resp_final = CORRESPONDING #( ls_einv_resp-govt_response ).

            IF ls_resp_final-success = 'Y'.

              DATA(lv_json) = /ui2/cl_json=>serialize(
                  data             = ls_resp_final
                  pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
                  ).

              r_einv_canc = lv_json.

            ELSE.

              r_einv_canc = response_body1.

            ENDIF.
          ENDIF.
        ENDIF.

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.
    """"End:Cancellation**********************************************************************

    """**Start: Update table for cancelled document*******************************************
    IF ls_resp_final-success = 'Y'.
      cs_einv-cancel_date   = sys_date.
      cs_einv-cancel_status = abap_true.
      cs_einv-cancel_time   = sys_time.
      MODIFY zsd_einv_data FROM @cs_einv.
      COMMIT WORK.

    ENDIF.
    """**End: Update table for cancelled document*********************************************

  ENDMETHOD.


  METHOD fetch_billing_data.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear, "zi_dc_note-FiscalYear,
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
          AND billingdocumenttype IN ( 'F2','G2','L2', 'CBRE','CBAR', 'JSTO' )
    INTO TABLE @DATA(it_final) .              "#EC CI_ALL_FIELDS_NEEDED

    IF it_final IS NOT INITIAL.

      SELECT
        billingdocument ,
        companycode     ,
        fiscalyear      ,
        plant_gstin     ,
        billingtype         ,
        docdate             ,
        plant               ,
        status              ,
        businessmodule      ,
        irn                 ,
        ackno               ,
        ackdt               ,
        ewayackno           ,
        ewayackdt           ,
        ewbno               ,
        ewbdt               ,
        signedinvoice       ,
        signedqrcode        ,
        ewbvalidtill        ,
        qrcodeurl           ,
        erdat               ,
        uname               ,
        uzeit               ,
        cancel_status       ,
        cancel_date         ,
        cancel_time         ,
        canc_status_eway    ,
        cancel_date_eway    ,
        cancel_time_eway    ,
        transporterid       ,
        modeoftransp        ,
        transpdocnum        ,
        transdocdate        ,
        vehiclenum          ,
        vehicletype         ,
        transpname          ,
        datasource          ,
        distance
      FROM zsd_einv_data
      FOR ALL ENTRIES IN @it_final
      WHERE billingdocument   = @it_final-billingdocument AND
            companycode   = @it_final-companycode  AND
            fiscalyear = @lv_fis_year AND
            plant   = @it_final-plant
            "cancel_status   = ''
      INTO TABLE @DATA(it_einv) ##SELECT_FAE_WITH_LOB[SIGNEDINVOICE] ##SELECT_FAE_WITH_LOB[SIGNEDQRCODE]. "#EC CI_NO_TRANSFORM

      SELECT
      billingdocument,
      companycode,
      accountingdocument,
      fiscalyear,
      billingdocumenttype,
      accountingtransferstatus
      FROM i_billingdocument
      FOR ALL ENTRIES IN @it_final
      WHERE billingdocument   = @it_final-billingdocument AND
            companycode   = @it_final-companycode  AND
            fiscalyear = @lv_fis_year
      INTO TABLE @DATA(it_bill).                   "#EC CI_NO_TRANSFORM

      LOOP AT it_final INTO DATA(ls_final).

        READ TABLE it_bill INTO DATA(ls_bill) WITH KEY
                           billingdocument   = ls_final-billingdocument
                           companycode       = ls_final-companycode
                           fiscalyear        = lv_fis_year.

        IF ls_bill-accountingtransferstatus = 'C'.

          gs_final-businessmodule   = im_module.
          gs_final-billingdocument  = ls_final-billingdocument.
          gs_final-companycode      = ls_final-companycode.
          gs_final-fiscalyear       = lv_fis_year.
          gs_final-docdate          = ls_final-billingdocumentdate.
          gs_final-plant            = ls_final-plant.
          gs_final-plant_gstin      = ls_final-plant_gstin.
          gs_final-status           = ''.

          gs_final-invoicenature    = ls_final-billingdocumenttypename.

          READ TABLE it_einv INTO DATA(ls_einv) WITH KEY
                             billingdocument   = ls_final-billingdocument
                             companycode       = ls_final-companycode
                             fiscalyear        = lv_fis_year.
          IF sy-subrc EQ 0.

            IF ls_einv-cancel_status = abap_false.
              gs_final-status     = 'Generated' ##NO_TEXT.
            ELSE.
              gs_final-status     = 'Cancelled' ##NO_TEXT.
            ENDIF.

            gs_final-irn        = ls_einv-irn.
            gs_final-ackno      = ls_einv-ackno.
            gs_final-ackdt      = ls_einv-ackdt.
            gs_final-ewbno      = ls_einv-ewbno.
            gs_final-ewbdt      = ls_einv-ewbdt.

          ENDIF.

          APPEND gs_final TO gt_final.

        ENDIF.

        CLEAR: ls_final, ls_einv, gs_final, ls_bill.
      ENDLOOP.

      IF im_etype = 'new' ##NO_TEXT.
        DELETE gt_final WHERE status NE ''.
      ELSEIF im_etype = 'generated' ##NO_TEXT.
        DELETE gt_final WHERE status EQ ''.
        DELETE gt_final WHERE status EQ 'Cancelled' ##NO_TEXT.
      ENDIF.

      SORT gt_final BY billingdocument.
      DELETE ADJACENT DUPLICATES FROM gt_final COMPARING billingdocument.
      r_einv_data[] = gt_final[].

    ENDIF.

  ENDMETHOD.


  METHOD fetch_finance_data.

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

    SELECT
companycode,
accountingdocument,
fiscalyear,
accountingdocumentitem,
financialaccounttype,
chartofaccounts,
accountingdocumentitemtype,
postingkey,
product,
plant,
postingdate,
documentdate,
debitcreditcode,
taxcode,
taxitemgroup,
transactiontypedetermination,
glaccount,
customer,
supplier,
purchasingdocument,
purchasingdocumentitem,
purchaseorderqty,
profitcenter,
documentitemtext,
amountincompanycodecurrency,
amountintransactioncurrency,
cashdiscountbaseamount,
netpaymentamount,
assignmentreference,
invoicereference,
invoicereferencefiscalyear,
invoiceitemreference,
quantity,
baseunit,
materialpriceunitqty,
taxbaseamountintranscrcy,
clearingjournalentry,
clearingdate,
clearingcreationdate,
clearingjournalentryfiscalyear,
clearingitem,
housebank,
bpbankaccountinternalid,
housebankaccount,
in_hsnorsaccode,
costcenter,
accountingdocumenttype,
netduedate,
offsettingaccount,
transactioncurrency,
paymentterms,
businessplace,
valuedate,
paymentmethod,
specialglcode,
specialgltransactiontype,
documentreferenceid,
alternativereferencedocument,
accountingdocumentheadertext,
companycodename,
addressid,
supplierfullname,
customerfullname
    FROM zi_dc_note
    WHERE accountingdocument  IN @r_vbeln AND
          postingdate         IN @r_date  AND
          plant               IN @r_plant AND
          accountingdocumenttype IN ( 'DG','DR' )
    INTO TABLE @DATA(it_final) .              "#EC CI_ALL_FIELDS_NEEDED

    IF it_final IS NOT INITIAL.

      SELECT
        billingdocument ,
        companycode     ,
        fiscalyear      ,
        plant_gstin     ,
        billingtype         ,
        docdate             ,
        plant               ,
        status              ,
        businessmodule      ,
        irn                 ,
        ackno               ,
        ackdt               ,
        ewayackno           ,
        ewayackdt           ,
        ewbno               ,
        ewbdt               ,
        signedinvoice       ,
        signedqrcode        ,
        ewbvalidtill        ,
        qrcodeurl           ,
        erdat               ,
        uname               ,
        uzeit               ,
        cancel_status       ,
        cancel_date         ,
        cancel_time         ,
        canc_status_eway    ,
        cancel_date_eway    ,
        cancel_time_eway    ,
        transporterid       ,
        modeoftransp        ,
        transpdocnum        ,
        transdocdate        ,
        vehiclenum          ,
        vehicletype         ,
        transpname          ,
        datasource          ,
        distance
      FROM zsd_einv_data
      FOR ALL ENTRIES IN @it_final
      WHERE billingdocument   = @it_final-accountingdocument AND
            companycode   = @it_final-companycode  AND
            fiscalyear = @lv_fis_year AND
            plant   = @it_final-plant
            "cancel_status   = ''
      INTO TABLE @DATA(it_einv)  ##SELECT_FAE_WITH_LOB[SIGNEDINVOICE] ##SELECT_FAE_WITH_LOB[SIGNEDQRCODE]. "#EC CI_NO_TRANSFORM

      LOOP AT it_final INTO DATA(ls_final).

        gs_final-businessmodule   = im_module.
        gs_final-billingdocument  = ls_final-accountingdocument.
        gs_final-companycode      = ls_final-companycode.
        gs_final-fiscalyear       = lv_fis_year.
        gs_final-docdate          = ls_final-postingdate.
        gs_final-plant            = ls_final-plant.
        gs_final-plant_gstin      = ''. "ls_final-plant_gstin.
        gs_final-status           = ''.

        READ TABLE it_einv INTO DATA(ls_einv) WITH KEY
                           billingdocument   = ls_final-accountingdocument
                           companycode       = ls_final-companycode
                           fiscalyear        = lv_fis_year.
        IF sy-subrc EQ 0.

          IF ls_einv-cancel_status = abap_false.
            gs_final-status     = 'Generated' ##NO_TEXT.
          ELSE.
            gs_final-status     = 'Cancelled' ##NO_TEXT.
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
        DELETE gt_final WHERE status EQ ''.
        DELETE gt_final WHERE status EQ 'Cancelled' ##NO_TEXT.
      ENDIF.

      SORT gt_final BY billingdocument.
      DELETE ADJACENT DUPLICATES FROM gt_final COMPARING billingdocument.
      r_einv_data[] = gt_final[].

    ENDIF.

  ENDMETHOD.


  METHOD gen_einv_from_clrtx.

    TYPES: BEGIN OF gty_resp1,
             success       TYPE string,
             ackno         TYPE string,
             ackdt         TYPE string,
             irn           TYPE string,
             signedinvoice TYPE string,
             signedqrcode  TYPE string,
             status        TYPE string,
           END OF gty_resp1.

    DATA:
      einv_resp     TYPE gty_resp1,
      ls_resp_final TYPE gty_resp1.

    TYPES: BEGIN OF gty_resp,
             custom_fields   TYPE c LENGTH 20,
             deleted         TYPE c LENGTH 20,
             document_status TYPE c LENGTH 20,
             error_response  TYPE c LENGTH 20,
             errors          TYPE c LENGTH 20,
             govt_response   LIKE einv_resp,
           END OF gty_resp.

    DATA: ls_einv_resp TYPE gty_resp,
          lt_einv_resp TYPE TABLE OF gty_resp.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string,
          lv_auth_token  TYPE string,
          lv_owner_id    TYPE string,
          lv_gstin       TYPE string.

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

    """"Generation:*****************************************************************************************
*    url = 'https://einvoicing.internal.cleartax.co/v1/govt/api/Invoice'.
*    lv_auth_token = '1.b085ee0a-13b5-4e67-ba49-44ca634cc0af_c5fff1119d7c2772d6aa0172db20c8cc74eeeef1797f43dc46325ac83125412b'.
*    lv_gstin      = '09AAACI4265L1ZA' .
*    lv_owner_id   = 'b30507d9-49b5-436e-86fd-a1731f30ed3a'.

    IF sy-sysid IN r_dq.
      lv_gstin = '06AAFCD5862R017' ##NO_TEXT.
    ELSE.
      lv_gstin = im_gstin.
    ENDIF.

    SELECT SINGLE *
    FROM zsd_cred_clr
    WHERE gstin = @lv_gstin
    INTO @DATA(ls_clr_cred). "#EC WARNOK                 "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      url           = ls_clr_cred-url_einv_gen.
      lv_auth_token = ls_clr_cred-authtoken.
      lv_gstin      = ls_clr_cred-gstin.
      lv_owner_id   = ls_clr_cred-ownerid.
    ENDIF.

    TRY.
        DATA(dest1) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest1 ).

        DATA(lo_request1) = lo_http_client->get_http_request( ).
        lo_request1->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: miw_string.
        miw_string = im_einv_json.

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
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>put ).
        DATA(response_body1) = lo_response1->get_text( ).
        "*r_gen_einv = response_body1.
        """"*****************************************************************************************

        /ui2/cl_json=>deserialize(
                        EXPORTING json = response_body1
                           pretty_name = /ui2/cl_json=>pretty_mode-none
                           CHANGING data = lt_einv_resp
                     ).

        IF lt_einv_resp[] IS NOT INITIAL.

          READ TABLE lt_einv_resp INTO ls_einv_resp INDEX 1.

          IF ls_einv_resp-govt_response IS NOT INITIAL.

            ls_resp_final = CORRESPONDING #( ls_einv_resp-govt_response ).

            IF ls_resp_final-success = 'Y'.

              DATA(lv_json) = /ui2/cl_json=>serialize(
                  data             = ls_resp_final
                  pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
                  ).

              r_gen_einv = lv_json.

            ELSE.

              r_gen_einv = response_body1.

            ENDIF.
          ENDIF.
        ENDIF.

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.


  METHOD get_token_from_clrtx.

  ENDMETHOD.


  METHOD prepare_einv_json_clrtx.

    DATA:
      miw_string        TYPE string,
      miw_einv_json_hdr TYPE string,
      miw_einv_json_itm TYPE string.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n,
          lv_neg_val      TYPE c LENGTH 20.

    DATA:
      r_vbeln  TYPE RANGE OF zi_sale_reg-billingdocument,
      r_date   TYPE RANGE OF zi_sale_reg-billingdocumentdate,
      r_plant  TYPE RANGE OF zi_sale_reg-plant,
      r_bukrs  TYPE RANGE OF zi_sale_reg-companycode,
      wr_bukrs LIKE LINE OF r_bukrs,
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

    IF im_bukrs IS NOT INITIAL.
      wr_bukrs-low    = im_bukrs.
      wr_bukrs-high   = im_bukrs.
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
      itm_count        TYPE sy-tabix.

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
      lv_item_assvl_amt         TYPE c LENGTH 40.

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

    lv_seller_addrs1 =  ls_final_hdr-street && ls_final_hdr-streetname .
    lv_seller_addrs2 = ''.

    lv_buyer_addrs1         = ls_final_hdr-sp_street && ls_final_hdr-sp_street1 .
    lv_buyer_addrs2         = ''.

    DATA : lvdt TYPE c LENGTH 10 .
    lvdt = |{ ls_final_hdr-billingdocumentdate+6(2) }/{ ls_final_hdr-billingdocumentdate+4(2) }/{ ls_final_hdr-billingdocumentdate+0(4) }| .

    """""" FOR TESTING PURPOSE ....
    IF sy-sysid IN r_dq.
      ls_final_hdr-plant_gstin = '06AAFCD5862R017' ##NO_TEXT.
    ENDIF.
    """""" FOR TESTING PURPOSE ....

    ls_final_hdr-billingdocument = |{ ls_final_hdr-billingdocument ALPHA = OUT }| .

    IF ls_final_hdr-billingdocumenttype = 'JSTO'.
      ls_final_hdr-ship_to_party = ls_final_hdr-bill_to_party.
      ls_final_hdr-doc_type      = 'INV'.
    ENDIF.

    IF ls_final_hdr-billingdocumenttype = 'F2' AND
       ls_final_hdr-distributionchannel = '40'.

      IF ls_final_hdr-item_igstamount = ''.
        ls_final_hdr-supply_type = 'EXPWOP'.
      ELSE.
        ls_final_hdr-supply_type = 'EXPWP'.
      ENDIF.

    ENDIF.

    lv_buyer_stcode = ls_final_hdr-re_tax+0(2).

    IF ls_final_hdr-distributionchannel = '20'.

      ls_final_hdr-re_pin = '999999'.

      lv_buyer_stcode     = '96'.
      CONDENSE lv_buyer_stcode.

    ENDIF.

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

    CLEAR: miw_string, miw_einv_json_hdr, miw_einv_json_itm.
    miw_einv_json_hdr = '[{'
    && '"transaction": {'
    && '"Version":"1.1",'
    && '"TranDtls":{'
    && '"TaxSch":"GST",'
    && '"SupTyp":' && '"' && ls_final_hdr-supply_type && '",'
    && '"RegRev":"N"'
    && '},'
    && '"DocDtls":{'
    && '"Typ":'   && '"' && ls_final_hdr-doc_type && '",'
    && '"No":'    && '"' && ls_final_hdr-billingdocument && '",'
    && '"Dt":'    && '"' && lvdt && '"'
    && '},'.

    IF ls_final_hdr-distributionchannel = '30'.
      miw_einv_json_hdr = miw_einv_json_hdr
      && '"ExpDtls": {'
      && '"ExpCat": null,'
      && '"WthPay": null,'
      && '"ShipBNo": null,'
      && '"ShipBDt": null,'
      && '"Port": null,'
      && '"InvForCur": 0,'
      && '"ForCur":'  && '"' && ls_final_hdr-transactioncurrency && '",'
      && '"CntCode":' && '"' && ls_final_hdr-re_country && '"'
      && '},'.
    ENDIF.

    miw_einv_json_hdr = miw_einv_json_hdr
        && '"SellerDtls":{'
        && '"Gstin":' && '"' && ls_final_hdr-plant_gstin && '",'
        && '"LglNm":' && '"' && ls_final_hdr-plantname && '",'
        && '"TrdNm":' && '"' && ls_final_hdr-plantname && '",'
        && '"Addr1":' && '"' && ls_final_hdr-streetname && '",'
        && '"Addr2":' && '"' && ls_final_hdr-streetname  && ls_final_hdr-street && '",'
        && '"Loc":'   && '"' && ls_final_hdr-cityname && '",'
        && '"Pin":'   && ls_final_hdr-postalcode && ','
        && '"Stcd":'  && '"' && ls_final_hdr-plant_gstin+0(2) && '",'
        && '"Ph":"9999999999",'
        && '"Em":'     && '"' && ls_final_hdr-plant_email && '"'
        && '},'

        && '"BuyerDtls":{'
        && '"Gstin":'      && '"' && ls_final_hdr-re_tax && '",'
        && '"LglNm":'      && '"' && ls_final_hdr-re_name && '",'
        && '"Pos":'        && '"' && lv_buyer_stcode && '",'
        && '"Addr1":'      && '"' && ls_final_hdr-re_street1 && '",'
        && '"Addr2":'      && '"' && ls_final_hdr-re_street && ls_final_hdr-re_street1 && '",'
        && '"Loc":'        && '"' && ls_final_hdr-re_city && '",'
        && '"Pin":'        &&        ls_final_hdr-re_pin && ','
        && '"Stcd":'       && '"' && lv_buyer_stcode && '",'
        && '"Ph":'         && '"' && ls_final_hdr-re_phone4 && '",'
        && '"Em":'         && '"' && ls_final_hdr-re_email && '"'
        && '},'.

    miw_einv_json_hdr = miw_einv_json_hdr
          && '"DispDtls": {'
          && '"Nm":'      && '"' && ls_disp_from-dispatchfrmname && '",'
          && '"Addr1":'   && '"' && ls_disp_from-dispatchfrmaddr1 && '",'
          && '"Addr2":'   && '"' && ls_disp_from-dispatchfrmaddr2 && '",'
          && '"Loc":'     && '"' && ls_disp_from-dispatchfrmloc && '",'
          && '"Pin":'     && '"' && ls_disp_from-dispatchfrmpin && '",'
          && '"Stcd":'    && '"' && ls_disp_from-dispatchfrmstcd && '"'
          && '},'.

    DATA :lv_trans_type TYPE c LENGTH 4 .
    IF ls_final_hdr-ship_to_party NE ls_final_hdr-bill_to_party .
      lv_trans_type = 'SHP' .
    ENDIF .

***"SOC for ship details changes
    IF  lv_trans_type EQ 'SHP'.
      miw_einv_json_hdr = miw_einv_json_hdr
      && '"ShipDtls":'
      && '{'
      && '"Gstin":'      && '"' && ls_final_hdr-we_tax && '",'
      && '"LglNm":'      && '"' && ls_final_hdr-we_name && '",'
      && '"TrdNm":'      && '"' && ls_final_hdr-we_name && '",'
      && '"Addr1":'      && '"' && ls_final_hdr-we_street1 && ls_final_hdr-we_street && '",' "ls_final_hdr-we_street && '",'
      && '"Addr2":'      && '"' && ls_final_hdr-we_street1 && ls_final_hdr-we_street && '",'
      && '"Loc":'        && '"' && ls_final_hdr-we_city && '",'
      && '"Pin":'        && ls_final_hdr-we_pin && ','
      && '"Stcd":'       && '"' && ls_final_hdr-we_region && '"'
      && '},'.
    ENDIF.
***"EOC for ship details changes

    lv_val_total_cess          = '0'.
    lv_val_total_cess_v_state  = '0'.
    lv_val_discount            = '0'.
    lv_val_other_charge        = '0'.
    lv_val_dt_roundoff         = '0'.

    miw_einv_json_hdr = miw_einv_json_hdr
    && '"ItemList":' && '['
    &&   '{'.

    CLEAR: itm_count.
    LOOP AT it_final INTO DATA(ls_itm).

      itm_count   = itm_count + 1.

      lv_gst_rate = '0'.
      IF ls_itm-item_cgstrate IS NOT INITIAL.
        lv_gst_rate = ls_itm-item_sgstrate + ls_itm-item_cgstrate.
      ELSEIF ls_itm-item_igstrate IS NOT INITIAL.
        lv_gst_rate = ls_itm-item_igstrate.
      ENDIF.

      DATA : lv_service TYPE c .
      CLEAR : lv_service .
      IF ls_itm-hsn+0(2) = '99' .
        lv_service = 'Y' .
      ELSE .
        lv_service = 'N' .
      ENDIF .

      IF ls_itm-baseunit  = 'KL' OR ls_itm-baseunit  = 'BL'.
        ls_itm-baseunit  = 'KLR'.
      ELSEIF ls_itm-baseunit  = 'TO'.
        ls_itm-baseunit  = 'TON'.
      ELSEIF ls_itm-baseunit  = 'KG'.
        ls_itm-baseunit  = 'KGS'.
      ELSE.
        ls_itm-baseunit  = 'NOS'.
      ENDIF.

      ls_itm-item_unitprice = ls_itm-item_unitprice * ls_itm-accountingexchangerate.

      IF ls_itm-distributionchannel = '20'.
        ls_itm-item_unitprice       = ls_itm-item_pcip_amt.
        ls_itm-item_totalamount_inr = ls_itm-item_assessableamount.
      ENDIF.

      lv_item_assvl_amt = ls_itm-item_totalamount_inr.
*                          ls_itm-item_othercharge +
*                          ls_itm-item_freight +
*                          ls_itm-item_insurance_amt.

      lv_item_assvl_amt = lv_item_assvl_amt - ls_itm-item_discountamount_inr.

      ls_itm-item_grandtotalamount_inr = lv_item_assvl_amt +
                                         ls_itm-item_igstamount_inr +
                                         ls_itm-item_sgstamount_inr +
                                         ls_itm-item_cgstamount_inr.
*                                         ls_itm-item_othercharge +
*                                         ls_itm-item_freight +
*                                         ls_itm-item_insurance_amt.

      "*ls_itm-item_totalamount_inr = ls_itm-item_totalamount_inr + ls_itm-item_freight.

      DATA:
        lv_bill_qty TYPE p DECIMALS 2.

      lv_bill_qty     = ls_itm-netweight / 100. "ls_itm-billingquantity.
      ls_itm-baseunit = 'QTL'.

      miw_einv_json_itm = miw_einv_json_itm
          && '"SlNo":'             && '"' && ls_itm-billingdocumentitem && '",'
          && '"IsServc":"'         && lv_service && '",'
          && '"PrdDesc":'          && '"' && ls_itm-billingdocumentitemtext && '",'
          && '"HsnCd":'            && '"' && ls_itm-hsn && '",'
          && '"Qty":'              &&  lv_bill_qty && ','
          && '"Unit":'             && '"' && ls_itm-baseunit && '",'
          && '"UnitPrice":'        && ls_itm-item_unitprice && ','
          && '"TotAmt":'           && ls_itm-item_totalamount_inr && ','
          && '"Discount":'         && ls_itm-item_discountamount_inr && ','
          && '"AssAmt":'           && lv_item_assvl_amt && ','
          && '"GstRt":'            && lv_gst_rate && ','
          && '"SgstAmt":'          && ls_itm-item_sgstamount_inr && ','
          && '"IgstAmt":'          && ls_itm-item_igstamount_inr && ','
          && '"CgstAmt":'          && ls_itm-item_cgstamount_inr && ','
          && '"CesRt":0,'
          && '"CesAmt":0,'
          && '"CesNonAdvlAmt":0,'
          && '"StateCesRt":0,'
          && '"StateCesAmt":0,'
          && '"StateCesNonAdvlAmt":0,'
          && '"OthChrg": 0,' "&& ls_itm-item_freight && ','
          && '"TotItemVal":'       && ls_itm-item_grandtotalamount_inr && ''. "#EC CI_NOORDER

      IF line_itm EQ itm_count.
        miw_einv_json_itm = miw_einv_json_itm &&  '}'.
      ELSE.
        miw_einv_json_itm = miw_einv_json_itm &&  '},{'.
      ENDIF.

      lv_val_total_assess        = lv_val_total_assess + lv_item_assvl_amt. "ls_itm-item_totalamount_inr.
      lv_val_total_cgst          = lv_val_total_cgst + ls_itm-item_cgstamount_inr.
      lv_val_total_sgst          = lv_val_total_sgst + ls_itm-item_sgstamount_inr.
      lv_val_total_igst          = lv_val_total_igst + ls_itm-item_igstamount_inr.

      lv_val_other_charge        = lv_val_other_charge +
                                   ls_itm-item_othercharge +
                                   ls_itm-item_freight +
                                   ls_itm-item_insurance_amt.


      lv_neg_val = ls_itm-item_roundoff.
      CONDENSE lv_neg_val.
      IF lv_neg_val CA '-'.
        ls_itm-item_roundoff = ls_itm-item_roundoff * -1.
      ENDIF.
      lv_val_dt_roundoff         = lv_val_dt_roundoff + ls_itm-item_roundoff .

      lv_val_total_invoice = lv_val_total_invoice +
                             ls_itm-item_grandtotalamount_inr +
                             ls_itm-item_roundoff.


      CLEAR: ls_itm, lv_neg_val.
    ENDLOOP.

*    lv_val_total_invoice = lv_val_total_assess +
*                           lv_val_total_cgst +
*                           lv_val_total_sgst +
*                           lv_val_total_igst.

    lv_val_other_charge = 0.

    miw_einv_json_itm = miw_einv_json_itm
        && '],'
        && '"ValDtls":{'
        && '"AssVal":'     && lv_val_total_assess  && ','
        && '"CgstVal":'    && lv_val_total_cgst  && ','
        && '"SgstVal":'    && lv_val_total_sgst  && ','
        && '"IgstVal":'    && lv_val_total_igst  && ','
        && '"CesVal":'     && lv_val_total_cess  && ','
        && '"StCesVal":'   && lv_val_total_cess_v_state  && ','
        && '"Discount":'   && lv_val_discount  && ','
        && '"OthChrg":'    && lv_val_other_charge  && ','
        && '"RndOffAmt":'  && lv_val_dt_roundoff  && ','
        && '"TotInvVal":'  && lv_val_total_invoice && ''
        && '}'
        && '}}]'.

    miw_string = miw_einv_json_hdr
      && miw_einv_json_itm.

    r_einv_json =  miw_string.

    IF 1 = 2 .
      miw_string = '{'
      && '"Version":"1.03",'
      && '"TranDtls":{'
      && '"TaxSch":"GST",'
      && '"SupTyp":"B2B",'
      && '"RegRev":"N"'
      && '},'
      && '"DocDtls":{'
      && '"Typ":"INV",'
      && '"No":' && '"' && im_vbeln && '",'  """"2009000493",'
      && '"Dt":"30/05/2024"'
      && '},'
      && '"SellerDtls":{'
      && '"Gstin":"09AAACI4265L1ZA",'
      && '"LglNm":"Intex Technologies (I) Ltd-M/W Noida",'
      && '"TrdNm":"Intex Technologies (I) Ltd-M/W Noida",'
      && '"Addr1":"A-12 Housiery Complex Noida",'
      && '"Addr2":"A-12 Housiery Complex Noida",'
      && '"Loc":"Gautam Budh Nagar",'
      && '"Pin":201305,'
      && '"Stcd":"09",'
      && '"Ph":"9999999999",'
      && '"Em":"pankaj.verma@intex.in"'
      && '},'
      && '"BuyerDtls":{'
      && '"Gstin":"09AEKPJ0181R2ZW",'
      && '"LglNm":"TOP N TOWN",'
      && '"Pos":"09",'
      && '"Addr1":"10 Akashdeep Complex",'
      && '"Addr2":"10 Akashdeep Complex",'
      && '"Loc":"Meerut",'
      && '"Pin":250002,'
      && '"Stcd":"09",'
      && '"Ph":"9990791400",'
      && '"Em":"kamaljain101@gmail.com"'
      && '},'
      && '"ItemList":['
      && '{'
      && '"SlNo":"000010",'
      && '"IsServc":"N",'
      && '"PrdDesc":"Multimedia Speaker 2.1 XM 2590 SUFB",'
      && '"HsnCd":"85182200",'
      && '"Qty":1.000,'
      && '"Unit":"NOS",'
      && '"UnitPrice":1000.00,'
      && '"TotAmt":1000.00,'
      && '"Discount":0,'
      && '"AssAmt":1000,'
      && '"GstRt":18,'
      && '"SgstAmt":90.00,'
      && '"IgstAmt":0,'
      && '"CgstAmt":90.00,'
      && '"CesRt":0,'
      && '"CesAmt":0,'
      && '"CesNonAdvlAmt":0,'
      && '"StateCesRt":0,'
      && '"StateCesAmt":0,'
      && '"StateCesNonAdvlAmt":0,'
      && '"OthChrg":0,'
      && '"TotItemVal":1180'
      && '}'
      && '],'
      && '"ValDtls":{'
      && '"AssVal":1000,'
      && '"CgstVal":90,'
      && '"SgstVal":90,'
      && '"IgstVal":0,'
      && '"CesVal":0,'
      && '"StCesVal":0,'
      && '"Discount":0,'
      && '"OthChrg":0,'
      && '"RndOffAmt":0,'
      && '"TotInvVal":1180'
      && '}'
      && '}'.

      r_einv_json =  miw_string.
    ENDIF .

  ENDMETHOD.


  METHOD prepare_fi_einv_data.

    DATA:
      gt_item TYPE TABLE OF zstr_fi_debit_note_item,
      gs_item TYPE zstr_fi_debit_note_item,
      xt_item TYPE TABLE OF zstr_fi_debit_note_item.

    DATA:
      lo_amt_words     TYPE REF TO zcl_amt_words,
      lv_total_value   TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_frt_amt   TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_cgst_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_sgst_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_igst_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_tcs_amt   TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_load_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_rndf_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_other_amt TYPE p LENGTH 16 DECIMALS 2,
      lv_grand_total   TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_gst_amt   TYPE p LENGTH 16 DECIMALS 2,
      lv_amount_neg    TYPE c LENGTH 20,
      region_desc      TYPE c LENGTH 20.

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

    IF im_bukrs IS NOT INITIAL.
      wr_bukrs-low    = im_bukrs.
      wr_bukrs-high   = im_bukrs.
      wr_bukrs-sign   = 'I'.
      wr_bukrs-option = 'BT'.
      APPEND wr_bukrs TO r_bukrs.
    ENDIF.

    SELECT * FROM zi_dc_note
    WHERE accountingdocument  IN @r_vbeln AND
          postingdate         IN @r_date  AND
          plant               IN @r_plant AND
          companycode         IN @r_bukrs AND
          accountingdocumenttype IN ( 'DG','DR' )
    INTO TABLE @DATA(lt_acc) .                "#EC CI_ALL_FIELDS_NEEDED

    IF lt_acc[] IS NOT INITIAL.

      SELECT * FROM zi_dc_note
      WHERE accountingdocument  IN @r_vbeln AND
            postingdate         IN @r_date  AND
            plant               IN @r_plant AND
            companycode         IN @r_bukrs AND
            accountingdocumenttype IN ( 'DG','DR' )
            AND transactiontypedetermination IN ( 'WRX', 'BSX', 'EGK', 'PRD' )
               INTO TABLE @DATA(lt_wrx_bsx).  "#EC CI_ALL_FIELDS_NEEDED

      SELECT * FROM zi_dc_note
      WHERE accountingdocument  IN @r_vbeln AND
            postingdate         IN @r_date  AND
            plant               IN @r_plant AND
            companycode         IN @r_bukrs AND
            accountingdocumenttype IN ( 'DG','DR' )
            AND transactiontypedetermination EQ ''
            APPENDING TABLE @lt_wrx_bsx.      "#EC CI_ALL_FIELDS_NEEDED

    ENDIF.


    DATA(xt_acc) = lt_acc[].
    SORT xt_acc BY companycode accountingdocument fiscalyear.
    DELETE ADJACENT DUPLICATES FROM xt_acc COMPARING companycode accountingdocument fiscalyear.

    LOOP AT xt_acc INTO DATA(xs_acc).

      """******Header Data
      gs_dbnote-companycode          = xs_acc-companycode.
      gs_dbnote-accountingdocument   = xs_acc-accountingdocument.
      gs_dbnote-fiscalyear           = xs_acc-fiscalyear.
      gs_dbnote-postingdate          = xs_acc-postingdate.
      gs_dbnote-documentdate         = xs_acc-documentdate.


      DATA(lt_acc_plant) = lt_acc[].
      DELETE lt_acc_plant WHERE plant EQ ''.
      IF lt_acc_plant[] IS INITIAL.
        lt_acc_plant[] = lt_acc[].
        DELETE lt_acc_plant WHERE businessplace EQ ''.
      ENDIF.

      READ TABLE lt_acc_plant INTO DATA(ls_acc_plant) INDEX 1. "#EC CI_NOORDER
      IF sy-subrc EQ 0.

        SELECT SINGLE * FROM zi_plant_address
        WHERE plant = @ls_acc_plant-businessplace INTO @DATA(ls_plant_adrs). "#EC CI_NOORDER

        SELECT SINGLE
        companycode,
        businessplace,
        businessplacedescription,
        addressid,
        in_gstidentificationnumber
        FROM i_in_businessplacetaxdetail
        WHERE companycode = @ls_acc_plant-companycode AND businessplace = @ls_acc_plant-businessplace
        INTO @DATA(ls_bus_place).

        gs_dbnote-suppl_code         = ls_acc_plant-businessplace. "ls_acc_plant-Plant.
        gs_dbnote-suppl_name         = ls_plant_adrs-addresseefullname. "ls_plant_adrs-PlantName.

        IF ls_plant_adrs-streetprefixname2 IS NOT INITIAL.
          gs_dbnote-suppl_addr1        = ls_plant_adrs-streetprefixname1 && ',' && ls_plant_adrs-streetprefixname2.
        ELSE.
          gs_dbnote-suppl_addr1        = ls_plant_adrs-streetprefixname1.
        ENDIF.

        gs_dbnote-suppl_addr2        = ls_plant_adrs-streetname &&  ',' && ls_plant_adrs-streetsuffixname1. "&&  ',' && ls_plant_adrs-DistrictName.

        IF ls_plant_adrs-region EQ 'HR'.
          region_desc = 'Haryana' ##NO_TEXT.
        ENDIF.

        gs_dbnote-suppl_addr3        = ls_plant_adrs-cityname &&  ',' &&  region_desc && ',' && ls_plant_adrs-postalcode .
        gs_dbnote-suppl_pin          = ls_plant_adrs-postalcode.
        gs_dbnote-suppl_cin          = 'U31909DL2020PLC360123' ##NO_TEXT.
        gs_dbnote-suppl_gstin        = ls_bus_place-in_gstidentificationnumber ##NO_TEXT. "for plant 1001
        gs_dbnote-suppl_pan          = gs_dbnote-suppl_gstin+0(10).

        SELECT SINGLE * FROM zi_regiontext WHERE region = @ls_plant_adrs-region AND language = 'E' AND country = @ls_plant_adrs-country
         INTO @DATA(lv_st_nm).                "#EC CI_ALL_FIELDS_NEEDED

        gs_dbnote-suppl_stat_code    = lv_st_nm-regionname. "ls_plant_adrs-Region.
        gs_dbnote-suppl_phone        = ''.
        gs_dbnote-suppl_email        = 'sapadmin@sterlinggtake.com'.

      ENDIF.

      READ TABLE lt_acc INTO DATA(ls_cust) WITH KEY transactiontypedetermination = 'AGD'.
      SELECT SINGLE * FROM zi_customer_address
      WHERE customer = @ls_cust-customer INTO @DATA(ls_supplier).

      IF sy-subrc EQ 0.

        SELECT SINGLE * FROM zi_countrytext   WHERE country = @ls_supplier-country AND language = 'E'
        INTO @DATA(lv_cn_name_we).            "#EC CI_ALL_FIELDS_NEEDED

        SELECT SINGLE * FROM zi_regiontext  WHERE region = @ls_supplier-regio AND language = 'E' AND country = @ls_supplier-country
        INTO @DATA(lv_st_name_we).            "#EC CI_ALL_FIELDS_NEEDED

        gs_dbnote-billto_code        = xs_acc-supplier.
        gs_dbnote-billto_name        = ls_supplier-addresseefullname.
        gs_dbnote-billto_addr1       = ls_supplier-streetprefixname1 && ',' && ls_supplier-streetprefixname2.
        gs_dbnote-billto_addr2       = ls_supplier-streetname &&  ',' && ls_supplier-streetsuffixname1 &&  ',' && ls_supplier-districtname.
        gs_dbnote-billto_addr3       = ls_supplier-cityname &&  ',' && ls_supplier-postalcode &&  ',' && lv_cn_name_we-countryname.
        gs_dbnote-billto_pin         = ls_supplier-postalcode.
        gs_dbnote-billto_loc         = lv_st_name_we-regionname.
        gs_dbnote-billto_cin         = ''.
        gs_dbnote-billto_gstin       = ls_supplier-taxnumber3.
        gs_dbnote-billto_pan         = ''.
        gs_dbnote-billto_stat_code   = ls_supplier-taxnumber3+0(2). "lv_st_name_we-RegionName. "ls_supplier-regio.
        gs_dbnote-billto_phone       = ls_supplier-phonenumber1.
        gs_dbnote-billto_email       = ''.

        gs_dbnote-shipto_code        = xs_acc-supplier.
        gs_dbnote-shipto_name        = ls_supplier-addresseefullname.
        gs_dbnote-shipto_addr1       = ls_supplier-streetprefixname1 && ',' && ls_supplier-streetprefixname2.
        gs_dbnote-shipto_addr2       = ls_supplier-streetname &&  ',' && ls_supplier-streetsuffixname1 &&  ',' && ls_supplier-districtname.
        gs_dbnote-shipto_addr3       = ls_supplier-cityname &&  ',' && ls_supplier-postalcode &&  ',' && lv_cn_name_we-countryname.
        gs_dbnote-shipto_cin         = ''.
        gs_dbnote-shipto_gstin       = ls_supplier-taxnumber3.
        gs_dbnote-shipto_pan         = ls_supplier-taxnumber3+2(10).
        gs_dbnote-shipto_stat_code   = lv_st_name_we-regionname. "ls_supplier-regio.
        gs_dbnote-shipto_phone       = ls_supplier-phonenumber1.
        gs_dbnote-shipto_email       = ''.
        gs_dbnote-shipto_place_supply  = ''.

        CLEAR: lv_amount_neg.
        lv_amount_neg = xs_acc-amountincompanycodecurrency.
        CONDENSE lv_amount_neg.
        IF lv_amount_neg CA '-'.
          lv_grand_total = xs_acc-amountincompanycodecurrency * -1.
        ELSE.
          lv_grand_total = xs_acc-amountincompanycodecurrency.
        ENDIF.

      ENDIF.

      gs_dbnote-veh_no               = ''.
      gs_dbnote-trnas_mode           = ''.
      gs_dbnote-inv_no               = xs_acc-accountingdocument.
      gs_dbnote-inv_date             = xs_acc-postingdate+6(2) && '.' && xs_acc-postingdate+4(2) && '.' && xs_acc-postingdate+0(4).
      gs_dbnote-inv_ref_no           = xs_acc-documentreferenceid. "InvoiceReference.
      gs_dbnote-tax_payable_rev      = ''.
      gs_dbnote-remark               = xs_acc-documentitemtext.
      gs_dbnote-trans_curr           = xs_acc-transactioncurrency.

      """******Item Data
      CLEAR: gs_item.
      LOOP AT lt_acc INTO DATA(ls_acc)
                     WHERE companycode  = xs_acc-companycode AND
                     accountingdocument = xs_acc-accountingdocument AND
                     fiscalyear = xs_acc-fiscalyear AND
                     ( transactiontypedetermination = 'JIC' OR transactiontypedetermination = 'JIS' OR
                       transactiontypedetermination = 'JII' OR transactiontypedetermination = 'FR1' OR
                       transactiontypedetermination = 'RND' OR transactiontypedetermination = 'LOD' OR
                       transactiontypedetermination = 'OTH' OR transactiontypedetermination = 'TCS' OR
                       transactiontypedetermination = 'JOC' OR transactiontypedetermination = 'JOS' OR
                       transactiontypedetermination = 'JOI'
                     ).


        gs_item-companycode            = ls_acc-companycode.
        gs_item-accountingdocument     = ls_acc-accountingdocument.
        gs_item-fiscalyear             = ls_acc-fiscalyear.
        gs_item-accountingdocumentitem = ls_acc-accountingdocumentitem.
        gs_item-taxitemgroup           = ls_acc-taxitemgroup.

        CLEAR: lv_amount_neg.
        lv_amount_neg = ls_acc-amountincompanycodecurrency.
        CONDENSE lv_amount_neg.
        IF lv_amount_neg CA '-'.
          ls_acc-amountincompanycodecurrency = ls_acc-amountincompanycodecurrency * -1.
        ENDIF.

        IF ( ls_acc-transactiontypedetermination = 'JIC' OR ls_acc-transactiontypedetermination = 'JOC' ).

          gs_item-cgst_amt     = ls_acc-amountincompanycodecurrency.
          lv_sum_cgst_amt      = lv_sum_cgst_amt  + ls_acc-amountincompanycodecurrency.
          gs_item-cgst_rate    = ''.

        ELSEIF ( ls_acc-transactiontypedetermination = 'JIS' OR ls_acc-transactiontypedetermination = 'JOS' ).

          gs_item-sgst_amt            = ls_acc-amountincompanycodecurrency.
          lv_sum_sgst_amt  = lv_sum_sgst_amt + ls_acc-amountincompanycodecurrency.
          gs_item-sgst_rate           = ''.

        ELSEIF ( ls_acc-transactiontypedetermination = 'JII' OR ls_acc-transactiontypedetermination = 'JOI' ).

          gs_item-igst_amt     = ls_acc-amountincompanycodecurrency.
          lv_sum_igst_amt  = lv_sum_igst_amt + ls_acc-amountincompanycodecurrency.
          gs_item-igst_rate           = ''.

        ELSEIF ls_acc-transactiontypedetermination = 'FR1'.

          gs_item-frt_amt             = ls_acc-amountincompanycodecurrency.
          lv_sum_frt_amt  = lv_sum_frt_amt + ls_acc-amountincompanycodecurrency.

        ELSEIF ls_acc-transactiontypedetermination = 'RND'.

          gs_item-rndf_amt            = ls_acc-amountincompanycodecurrency.
          lv_sum_rndf_amt  = lv_sum_rndf_amt + ls_acc-amountincompanycodecurrency.

        ELSEIF ls_acc-transactiontypedetermination = 'LOD'.

          gs_item-load_amt            = ls_acc-amountincompanycodecurrency.
          lv_sum_load_amt  = lv_sum_load_amt + ls_acc-amountincompanycodecurrency.

        ELSEIF ls_acc-transactiontypedetermination = 'OTH'.

          gs_item-othr_amt            = ls_acc-amountincompanycodecurrency.
          lv_sum_other_amt  = lv_sum_other_amt + ls_acc-amountincompanycodecurrency.

        ELSEIF ls_acc-transactiontypedetermination = 'TCS'.

          gs_item-tcs_amt            = ls_acc-amountincompanycodecurrency.
          lv_sum_tcs_amt  = lv_sum_tcs_amt + ls_acc-amountincompanycodecurrency.

        ENDIF.
        APPEND gs_item TO xt_item.

        CLEAR: ls_acc, gs_item.
      ENDLOOP.

      gs_dbnote-sum_cgst_amt1         = lv_sum_cgst_amt.
      gs_dbnote-sum_sgst_amt1         = lv_sum_sgst_amt.
      gs_dbnote-sum_igst_amt1         = lv_sum_igst_amt.
      lv_sum_gst_amt = lv_sum_cgst_amt + lv_sum_sgst_amt + lv_sum_igst_amt.

      CLEAR: ls_acc, gs_item.
      CLEAR: lv_sum_cgst_amt, lv_sum_sgst_amt, lv_sum_igst_amt.
      DATA(lt_bsx) = lt_wrx_bsx[].
      DATA(lt_prd) = lt_wrx_bsx[].
      DELETE lt_bsx WHERE transactiontypedetermination NE 'BSX'.
      DELETE lt_prd WHERE transactiontypedetermination NE 'PRD'.

      IF lt_wrx_bsx[] IS NOT INITIAL.
        DATA(lv_wrx_bsx_line) = lines( lt_wrx_bsx ).
        IF lv_wrx_bsx_line GT 1.

          READ TABLE lt_wrx_bsx INTO DATA(lvs_egk) WITH KEY transactiontypedetermination = 'EGK'.
          IF sy-subrc NE 0.

            READ TABLE lt_wrx_bsx INTO DATA(cs_wrx) WITH KEY transactiontypedetermination = 'WRX'.
            IF sy-subrc EQ 0.
              DELETE lt_wrx_bsx WHERE transactiontypedetermination NE 'WRX'.
            ELSE.
              DELETE lt_wrx_bsx WHERE transactiontypedetermination NE 'BSX'.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.

      LOOP AT lt_wrx_bsx INTO ls_acc
                     WHERE companycode  = xs_acc-companycode AND
                     accountingdocument = xs_acc-accountingdocument AND
                     fiscalyear = xs_acc-fiscalyear. "AND
        "TransactionTypeDetermination ne 'BSX'.

        READ TABLE lt_bsx INTO DATA(ls_bsx) INDEX 1.    "#EC CI_NOORDER
        READ TABLE lt_prd INTO DATA(ls_prd) INDEX 1.    "#EC CI_NOORDER

        IF ls_acc-transactiontypedetermination = 'EGK'.

          READ TABLE lt_acc INTO DATA(cs_acc) WITH KEY companycode  = xs_acc-companycode
                                                 accountingdocument = xs_acc-accountingdocument
                                                         fiscalyear = xs_acc-fiscalyear
                                                         transactiontypedetermination = ''.

          ls_acc-product      = cs_acc-product.
          ls_acc-baseunit     = cs_acc-baseunit.
          ls_acc-quantity     = cs_acc-quantity.
          ls_acc-taxitemgroup = cs_acc-taxitemgroup.
          ls_acc-plant        = cs_acc-plant.
          ls_acc-amountincompanycodecurrency = cs_acc-amountincompanycodecurrency .
          ls_acc-in_hsnorsaccode = cs_acc-in_hsnorsaccode.
          gs_dbnote-remark = cs_acc-documentitemtext.

        ENDIF.

        gs_item-companycode            = ls_acc-companycode.
        gs_item-accountingdocument     = ls_acc-accountingdocument.
        gs_item-fiscalyear             = ls_acc-fiscalyear.
        gs_item-accountingdocumentitem = ls_acc-accountingdocumentitem.
        gs_item-trans_curr             = ls_acc-transactioncurrency.

        SELECT SINGLE * FROM i_productdescription
                        WHERE product = @ls_acc-product AND language = 'E'
                        INTO @DATA(ls_maktx). "#EC CI_ALL_FIELDS_NEEDED

        SELECT SINGLE * FROM i_glaccounttext
                        WHERE glaccount = @ls_acc-glaccount AND language = 'E'
                        INTO @DATA(ls_gltext). "#EC CI_ALL_FIELDS_NEEDED

        SELECT SINGLE
        product,
        plant,
        consumptiontaxctrlcode
        FROM i_productplantbasic
        WHERE product = @ls_acc-product AND plant = @ls_acc-plant
        INTO @DATA(ls_hsn).

        CLEAR: lv_amount_neg.
        lv_amount_neg = ls_acc-amountincompanycodecurrency.
        CONDENSE lv_amount_neg.
        IF lv_amount_neg CA '-'.
          ls_acc-amountincompanycodecurrency = ls_acc-amountincompanycodecurrency * -1.
        ENDIF.

        CLEAR: lv_amount_neg.
        lv_amount_neg = ls_bsx-amountincompanycodecurrency.
        CONDENSE lv_amount_neg.
        IF lv_amount_neg CA '-'.
          ls_bsx-amountincompanycodecurrency = ls_bsx-amountincompanycodecurrency * -1.
        ENDIF.

        CLEAR: lv_amount_neg.
        lv_amount_neg = ls_prd-amountincompanycodecurrency.
        CONDENSE lv_amount_neg.
        IF lv_amount_neg CA '-'.
          ls_prd-amountincompanycodecurrency = ls_prd-amountincompanycodecurrency * -1.
        ENDIF.

        gs_item-itemcode            = ls_acc-product.
        gs_item-itemdesc            = ls_maktx-productdescription. "ls_gltext-GLAccountName.
        gs_item-hsncode             = ls_acc-in_hsnorsaccode. "ls_hsn-ConsumptionTaxCtrlCode.
        gs_item-uom                 = ls_acc-baseunit.
        gs_item-itmqty              = ls_acc-quantity.

        IF ls_acc-transactiontypedetermination NE 'BSX'.
          gs_item-amount              = ls_acc-amountincompanycodecurrency + ls_bsx-amountincompanycodecurrency + ls_prd-amountincompanycodecurrency.
          gs_item-taxable_amt         = ls_acc-amountincompanycodecurrency + ls_bsx-amountincompanycodecurrency + ls_prd-amountincompanycodecurrency.
        ELSE.
          gs_item-amount              = ls_acc-amountincompanycodecurrency + ls_prd-amountincompanycodecurrency.
          gs_item-taxable_amt         = ls_acc-amountincompanycodecurrency + ls_prd-amountincompanycodecurrency.
        ENDIF.

        gs_item-discount            = ''.


        lv_total_value              = lv_total_value + gs_item-taxable_amt.
        IF gs_item-itmqty IS NOT INITIAL.
          gs_item-unit_rate           = gs_item-amount / gs_item-itmqty.
        ENDIF.

        LOOP AT xt_item INTO DATA(xs_item) WHERE taxitemgroup = ls_acc-taxitemgroup.

          IF xs_item-cgst_amt IS NOT INITIAL.
            gs_item-cgst_amt   = xs_item-cgst_amt.
            lv_sum_cgst_amt    = lv_sum_cgst_amt + gs_item-cgst_amt.
          ENDIF.

          IF xs_item-sgst_amt IS NOT INITIAL.
            gs_item-sgst_amt   = xs_item-sgst_amt.
            lv_sum_sgst_amt    = lv_sum_sgst_amt + gs_item-sgst_amt.
          ENDIF.

          IF xs_item-igst_amt IS NOT INITIAL.
            gs_item-igst_amt   = xs_item-igst_amt.
            lv_sum_igst_amt    = lv_sum_igst_amt + gs_item-igst_amt.
          ENDIF.

          CLEAR: xs_item.
        ENDLOOP.

        IF gs_item-cgst_amt IS NOT INITIAL.
          gs_item-cgst_rate    = ( gs_item-cgst_amt * 100 ) / gs_item-taxable_amt.
        ENDIF.

        IF gs_item-sgst_amt IS NOT INITIAL.
          gs_item-sgst_rate    = ( gs_item-sgst_amt * 100 ) / gs_item-taxable_amt.
        ENDIF.

        IF gs_item-igst_amt IS NOT INITIAL.
          gs_item-igst_rate    = ( gs_item-igst_amt * 100 ) / gs_item-taxable_amt.
        ENDIF.

        IF ls_acc-taxcode = 'C1' OR
           ls_acc-taxcode = 'C2' OR
           ls_acc-taxcode = 'C3' OR
           ls_acc-taxcode = 'C4' OR
           ls_acc-taxcode = 'C5' OR
           ls_acc-taxcode = 'C6' OR
           ls_acc-taxcode = 'C7' OR
           ls_acc-taxcode = 'C8' OR
           ls_acc-taxcode = 'D1' OR
           ls_acc-taxcode = 'D2' OR
           ls_acc-taxcode = 'D3' OR
           ls_acc-taxcode = 'D4' OR
           ls_acc-taxcode = 'D5' OR
           ls_acc-taxcode = 'D6' OR
           ls_acc-taxcode = 'D7' OR
           ls_acc-taxcode = 'D8'.

          gs_dbnote-revrs_yes_no = 'Yes'.

        ELSE.

          gs_dbnote-revrs_yes_no = 'No'.

        ENDIF.

        APPEND gs_item TO gt_item.
        CLEAR: ls_acc, gs_item.
      ENDLOOP.

      lv_grand_total = lv_total_value +
                       lv_sum_frt_amt +
                       lv_sum_cgst_amt +
                       lv_sum_sgst_amt +
                       lv_sum_igst_amt +
                       lv_sum_load_amt +
                       lv_sum_rndf_amt.

      gs_dbnote-sum_frt_amt          = lv_sum_frt_amt.
      gs_dbnote-sum_other_amt        = lv_sum_frt_amt. "lv_sum_other_amt.
      gs_dbnote-sum_cgst_amt         = lv_sum_cgst_amt.
      gs_dbnote-sum_sgst_amt         = lv_sum_sgst_amt.
      gs_dbnote-sum_igst_amt         = lv_sum_igst_amt.
      gs_dbnote-sum_tcs_amt          = lv_sum_tcs_amt.
      gs_dbnote-sum_load_amt         = lv_sum_load_amt.
      gs_dbnote-sum_rndf_amt         = lv_sum_rndf_amt.
      gs_dbnote-sum_gst_amt          = lv_sum_gst_amt.
      gs_dbnote-total_value          = lv_total_value.
      gs_dbnote-grand_total          = lv_grand_total.

      INSERT LINES OF gt_item INTO TABLE gs_dbnote-gt_item.
      APPEND gs_dbnote TO et_fidata. "gt_dbnote.

    ENDLOOP.


  ENDMETHOD.


  METHOD prepare_fi_einv_json_clrtx.

    DATA:
      miw_string        TYPE string,
      miw_einv_json_hdr TYPE string,
      miw_einv_json_itm TYPE string.

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
      itm_count        TYPE sy-tabix.

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
      lv_item_total             TYPE c LENGTH 40.

    DATA:
      lt_fidata TYPE TABLE OF zstr_fi_debit_note.

    DATA:
      r_dq  TYPE RANGE OF sy-sysid,
      rw_dq LIKE LINE OF r_dq.

    rw_dq-low    = 'C3O' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    rw_dq-low    = 'J8W' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    me->prepare_fi_einv_data(
      EXPORTING
        im_vbeln        = im_vbeln
        im_bukrs        = im_bukrs
        im_fyear        = im_fyear
        im_plant        = im_plant
        im_date         = im_date
        iv_action       = iv_action
        im_module       = im_module
        im_access_token = im_access_token
      RECEIVING
        et_fidata       =  lt_fidata
    ).

    READ TABLE lt_fidata INTO DATA(ls_fidata) INDEX 1.

    lv_seller_addrs1 = ls_fidata-suppl_addr2.
    lv_seller_addrs2 = ls_fidata-suppl_addr2.
    lv_buyer_addrs1  = ls_fidata-billto_addr1.
    lv_buyer_addrs2  = ls_fidata-billto_addr2.

    """""" FOR TESTING PURPOSE ....
    IF sy-sysid IN r_dq.
      ls_fidata-suppl_gstin  = '06AAFCD5862R017' ##NO_TEXT.
    ENDIF.
    """""" FOR TESTING PURPOSE ....
    """""" FOR TESTING PURPOSE ....

    DATA:
      lv_supply_type TYPE c LENGTH 40,
      lv_doc_type    TYPE c LENGTH 40,
      lv_document    TYPE c LENGTH 40,
      lv_date        TYPE c LENGTH 10.

    lv_supply_type = 'B2B'.
    lv_doc_type    = 'INV'.
    lv_document    = ls_fidata-accountingdocument.
    lv_date        = |{ ls_fidata-postingdate+6(2) }/{ ls_fidata-postingdate+4(2) }/{ ls_fidata-postingdate+0(4) }| .

    lv_buyer_stcode = ls_fidata-billto_stat_code.

    CLEAR: miw_string, miw_einv_json_hdr, miw_einv_json_itm.
    miw_einv_json_hdr = '{'
    && '"Version":"1.03",'
    && '"TranDtls":{'
    && '"TaxSch":"GST",'
    && '"SupTyp":' && '"' && lv_supply_type && '",'
    && '"RegRev":"N"'
    && '},'
    && '"DocDtls":{'
    && '"Typ":'   && '"' && lv_doc_type && '",'
    && '"No":'    && '"' && lv_document && '",'
    && '"Dt":'    && '"' && lv_date && '"'
    && '},'.

    miw_einv_json_hdr = miw_einv_json_hdr
        && '"SellerDtls":{'
        && '"Gstin":' && '"' && ls_fidata-suppl_gstin && '",'
        && '"LglNm":' && '"' && ls_fidata-suppl_name && '",'
        && '"TrdNm":' && '"' && ls_fidata-suppl_name && '",'
        && '"Addr1":' && '"' && ls_fidata-suppl_addr2 && '",'
        && '"Addr2":' && '"' && ls_fidata-suppl_addr2 && '",'
        && '"Loc":'   && '"' && ls_fidata-suppl_addr3 && '",'
        && '"Pin":'   && ls_fidata-suppl_pin && ','
        && '"Stcd":'  && '"' && ls_fidata-suppl_gstin+0(2) && '",'
        && '"Ph":"9999999999",'
        && '"Em":'     && '"' && 'sapadmin@sterlinggtake.com' && '"'
        && '},'
        && '"BuyerDtls":{'
        && '"Gstin":'      && '"' && ls_fidata-billto_gstin && '",'
        && '"LglNm":'      && '"' && ls_fidata-billto_name && '",'
        && '"Pos":'        && '"' && lv_buyer_stcode && '",'
        && '"Addr1":'      && '"' && lv_buyer_addrs1 && '",'
        && '"Addr2":'      && '"' && lv_buyer_addrs2 && '",'
        && '"Loc":'        && '"' && ls_fidata-billto_loc && '",'
        && '"Pin":'        &&        ls_fidata-billto_pin && ','
        && '"Stcd":'       && '"' && lv_buyer_stcode && '",'
        && '"Ph":'         && '"' && ls_fidata-billto_phone && '",'
        && '"Em":'         && '"' && ls_fidata-billto_email && '"'
        && '},'.

    lv_val_total_cess          = '0'.
    lv_val_total_cess_v_state  = '0'.
    lv_val_discount            = '0'.
    lv_val_other_charge        = '0'.
    lv_val_dt_roundoff         = '0'.

    miw_einv_json_hdr = miw_einv_json_hdr
    && '"ItemList":' && '['
    &&   '{'.

    DATA(it_itm) = ls_fidata-gt_item[].
    DATA(line_itm) = lines( it_itm[] ).

    CLEAR: itm_count.
    LOOP AT it_itm INTO DATA(ls_itm).

      itm_count   = itm_count + 1.

      lv_gst_rate = '0'.

      IF ls_itm-sgst_rate IS NOT INITIAL.
        lv_gst_rate = ls_itm-sgst_rate.
      ELSEIF ls_itm-igst_rate IS NOT INITIAL.
        lv_gst_rate = ls_itm-igst_rate.
      ENDIF.

      DATA : lv_service TYPE c .
      CLEAR : lv_service .
      IF ls_itm-hsncode IS NOT INITIAL .
        IF ls_itm-hsncode+0(2) = '99' .
          lv_service = 'Y' .
        ELSE .
          lv_service = 'N' .
        ENDIF .
      ELSE .
        lv_service = 'N' .
      ENDIF .

      ls_itm-uom = 'NOS'.

      CLEAR: lv_item_total.
      lv_item_total = ls_itm-amount + ls_itm-sgst_amt + ls_itm-igst_amt + ls_itm-cgst_amt.

      miw_einv_json_itm = miw_einv_json_itm
          && '"SlNo":'             && '"' && ls_itm-accountingdocumentitem && '",'
          && '"IsServc":"'         && lv_service && '",'
          && '"PrdDesc":'          && '"' && ls_itm-itemdesc && '",'
          && '"HsnCd":'            && '"' && ls_itm-hsncode && '",'
          && '"Qty":'              && ls_itm-itmqty && ','
          && '"Unit":'             && '"' && ls_itm-uom && '",'
          && '"UnitPrice":'        && ls_itm-unit_rate && ','
          && '"TotAmt":'           && ls_itm-amount && ','
          && '"Discount":0,'
          && '"AssAmt":'           && ls_itm-taxable_amt && ','
          && '"GstRt":'            && lv_gst_rate && ','
          && '"SgstAmt":'          && ls_itm-sgst_amt && ','
          && '"IgstAmt":'          && ls_itm-igst_amt && ','
          && '"CgstAmt":'          && ls_itm-cgst_amt && ','
          && '"CesRt":0,'
          && '"CesAmt":0,'
          && '"CesNonAdvlAmt":0,'
          && '"StateCesRt":0,'
          && '"StateCesAmt":0,'
          && '"StateCesNonAdvlAmt":0,'
          && '"OthChrg":0,'
          && '"TotItemVal":'       && lv_item_total && ''   .

      IF line_itm EQ itm_count.
        miw_einv_json_itm = miw_einv_json_itm &&  '}'.
      ELSE.
        miw_einv_json_itm = miw_einv_json_itm &&  '},{'.
      ENDIF.

      lv_val_total_assess        = lv_val_total_assess + ls_itm-taxable_amt .
      lv_val_total_cgst          = lv_val_total_cgst + ls_itm-cgst_amt.
      lv_val_total_sgst          = lv_val_total_sgst + ls_itm-sgst_amt.
      lv_val_total_igst          = lv_val_total_igst + ls_itm-igst_amt.
      lv_val_other_charge        = lv_val_other_charge + ls_itm-othr_amt.
      lv_val_dt_roundoff         = lv_val_dt_roundoff + ls_itm-rndf_amt.

      CLEAR: ls_itm.
    ENDLOOP.

    lv_val_total_invoice = lv_val_total_assess + lv_val_total_cgst + lv_val_dt_roundoff +
                           lv_val_total_sgst + lv_val_total_igst + lv_val_other_charge .

    miw_einv_json_itm = miw_einv_json_itm
        && '],'
        && '"ValDtls":{'
        && '"AssVal":'     && lv_val_total_assess  && ','
        && '"CgstVal":'    && lv_val_total_cgst  && ','
        && '"SgstVal":'    && lv_val_total_sgst  && ','
        && '"IgstVal":'    && lv_val_total_igst  && ','
        && '"CesVal":'     && lv_val_total_cess  && ','
        && '"StCesVal":'   && lv_val_total_cess_v_state  && ','
        && '"Discount":'   && lv_val_discount  && ','
        && '"OthChrg":'    && lv_val_other_charge  && ','
        && '"RndOffAmt":'  && lv_val_dt_roundoff  && ','
        && '"TotInvVal":'  && lv_val_total_invoice && ''
        && '}'
        && '}'.

    miw_string = miw_einv_json_hdr
      && miw_einv_json_itm.

    r_einv_json =  miw_string.

  ENDMETHOD.


  METHOD save_irn_data_clrtx.

    DATA:
      miw_string TYPE string.

    TYPES: BEGIN OF gty_resp,
             success       TYPE string,
             ackno         TYPE string,
             ackdt         TYPE string,
             irn           TYPE string,
             signedinvoice TYPE string,
             signedqrcode  TYPE string,
             status        TYPE string,
           END OF gty_resp.

    DATA:
      ls_resp TYPE gty_resp,
      gt_data TYPE TABLE OF zsd_einv_data,
      gs_data TYPE zsd_einv_data.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM zi_sale_reg
    WHERE billingdocument     = @im_vbeln AND
          "BillingDocumentDate = @im_date  AND
          plant               = @im_plant AND
          companycode         = @im_bukrs AND
          billingdocumentiscancelled = ''
    INTO TABLE @DATA(it_final) .              "#EC CI_ALL_FIELDS_NEEDED

    miw_string = im_einv_resp.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = miw_string
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_resp
    ).

    IF ls_resp-success = 'Y'.

      READ TABLE it_final INTO DATA(ls_final) INDEX 1.  "#EC CI_NOORDER

      IF im_module = 'SD'.
        gs_data-businessmodule = 'SD'.
      ELSEIF im_module = 'FI'.
        gs_data-businessmodule = 'FI'.
      ENDIF.

      gs_data-billingdocument   = im_vbeln.
      gs_data-companycode       = im_bukrs.
      gs_data-fiscalyear        = im_fyear.
      gs_data-plant_gstin       = ls_final-plant_gstin. "'09AAACI4265L1ZA'.
      gs_data-docdate           = im_date.
      gs_data-plant             = im_plant.
      gs_data-billingtype       = ls_final-billingdocumenttype.
      gs_data-status            = 'Success' ##NO_TEXT.
      gs_data-irn               = ls_resp-irn.
      gs_data-ackno             = ls_resp-ackno.
      gs_data-ackdt             = ls_resp-ackdt.
      gs_data-ewbno             = ''.
      gs_data-ewbdt             = ''.
      gs_data-signedinvoice     = ls_resp-signedinvoice.
      gs_data-signedqrcode      = ls_resp-signedqrcode.
      gs_data-ewbvalidtill      = ''.
      gs_data-qrcodeurl         = ''.
      gs_data-erdat             = sys_date.
      gs_data-uname             = sys_uname.
      gs_data-uzeit             = sys_time.
      gs_data-cancel_status     = ''.
*      gs_data-cancel_date       = ''.
*      gs_data-cancel_time       = ''.
      APPEND gs_data TO gt_data.

      IF gt_data[] IS NOT INITIAL.
        MODIFY zsd_einv_data FROM TABLE @gt_data.
        COMMIT WORK.
      ENDIF.

    ENDIF.

    r_saved_resp = im_einv_resp.

  ENDMETHOD.
ENDCLASS.
