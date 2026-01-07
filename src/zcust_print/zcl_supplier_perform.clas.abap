CLASS zcl_supplier_perform DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA : lv_char10  TYPE c LENGTH 10,
           lv_char20  TYPE c LENGTH 20,
           lv_char120 TYPE c LENGTH 120,
           gt_final   TYPE TABLE OF zpp_suplr_perfor,
           gs_final   TYPE zpp_suplr_perfor.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    METHODS:
      get_supplier_data
        IMPORTING
                  im_action       LIKE lv_char10
                  im_action_type  LIKE lv_char10
                  im_input_str    TYPE string
        RETURNING VALUE(et_final) LIKE gt_final,

      save_supplier_data
        IMPORTING
                  im_action             LIKE lv_char10
                  im_action_type        LIKE lv_char10
                  im_input_str          TYPE string
        RETURNING VALUE(es_save_status) TYPE string,

      send_mail
        IMPORTING
                  xt_data             LIKE gt_final
                  im_mode             LIKE lv_char10
                  im_action           LIKE lv_char10
                  im_action_type      LIKE lv_char10
        RETURNING VALUE(et_mail_stat) LIKE gt_final.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SUPPLIER_PERFORM IMPLEMENTATION.


  METHOD get_supplier_data.

    DATA : r_vend  TYPE RANGE OF I_Supplier-Supplier,
           rw_vend LIKE LINE OF  r_vend.

    TYPES: BEGIN OF gty_vend,
             vend_low TYPE I_Supplier-Supplier,
           END OF gty_vend.

    DATA:
      gt_vend TYPE TABLE OF gty_vend.

    TYPES: BEGIN OF gty_input,
             vend_high  TYPE I_Supplier-Supplier,
             vend_low   LIKE gt_vend,
             month_low  TYPE n LENGTH 2,
             year_low   TYPE n LENGTH 4,
             actionname TYPE c LENGTH 10,
           END OF gty_input.

    DATA:
      gt_input TYPE TABLE OF gty_input,
      gs_input TYPE gty_input.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gt_input
                                 ).

    IF gt_input[] IS NOT INITIAL.

      READ TABLE gt_input INTO gs_input INDEX 1.

      IF gs_input-vend_high IS INITIAL.

        LOOP AT gs_input-vend_low INTO DATA(lss_vend).

          rw_vend-low     = lss_vend-vend_low.
          rw_vend-high    = '' .
          rw_vend-option  = 'EQ' .
          rw_vend-sign    = 'I' .
          APPEND rw_vend TO r_vend.

          CLEAR: lss_vend.
        ENDLOOP.

      ELSE.

        READ TABLE gs_input-vend_low INTO DATA(ls_vend) INDEX 1.
        rw_vend-low     = ls_vend-vend_low.
        rw_vend-high    = gs_input-vend_high.
        rw_vend-option  = 'BT' .
        rw_vend-sign    = 'I' .
        APPEND rw_vend TO r_vend.

      ENDIF.
    ENDIF.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    IF im_action_type = 'newentry' ##NO_TEXT.

      SELECT
      Supplier,
      SupplierAccountGroup,
      SupplierName,
      SupplierFullName
      FROM I_Supplier
      WHERE Supplier IN @r_vend
      INTO TABLE @DATA(lt_lfa1).

      IF lt_lfa1[] IS NOT INITIAL.
        LOOP AT lt_lfa1 INTO DATA(ls_lfa1).
          gs_final-entry_month  = gs_input-month_low.
          gs_final-entry_year   = gs_input-year_low.
          gs_final-suppliercode = ls_lfa1-Supplier.
          gs_final-suppliername = ls_lfa1-SupplierName.
          APPEND gs_final TO et_final.
          CLEAR: ls_lfa1.
        ENDLOOP.
      ENDIF.

    ELSEIF im_action_type = 'oldentry' ##NO_TEXT.

      SELECT * FROM zpp_suplr_perfor
      WHERE suppliercode IN @r_vend AND
            entry_year   = @gs_input-year_low AND
            entry_month  = @gs_input-month_low
      INTO TABLE @DATA(lt_suppl).

      et_final = CORRESPONDING #( lt_suppl[] ).

    ENDIF.


  ENDMETHOD.


  METHOD save_supplier_data.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    DATA:
      gt_input TYPE TABLE OF zpp_suplr_perfor,
      gs_input TYPE zpp_suplr_perfor.

    IF im_input_str IS NOT INITIAL.

      /ui2/cl_json=>deserialize(
        EXPORTING json = im_input_str
           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
           CHANGING data = gt_input
                                   ).

      IF gt_input[] IS NOT INITIAL.

        sys_date  = cl_abap_context_info=>get_system_date( ).
        sys_time  = cl_abap_context_info=>get_system_time( ).
        sys_uname = cl_abap_context_info=>get_user_technical_name( ).

        LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<lfs_input>).
          <lfs_input>-erdat = sys_date.
          <lfs_input>-uzeit = sys_time.
          <lfs_input>-uname = sys_uname.
        ENDLOOP.

        MODIFY zpp_suplr_perfor FROM TABLE @gt_input.
        IF sy-subrc EQ 0.
          es_save_status = 'Data successfully saved in SAP..!' ##NO_TEXT.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD send_mail.

    DATA:
      gt_data       TYPE TABLE OF zpp_suplr_perfor,
      gs_data       TYPE zpp_suplr_perfor,
      lv_docnum     TYPE c LENGTH 15,
      lv_rating     TYPE c LENGTH 15,
      lv_action_req TYPE c LENGTH 255,
      lv_score      TYPE i.

    IF im_mode NE 'BCG'.

      IF xt_data[] IS NOT INITIAL.

        gt_data[] = xt_data[].

        SELECT * FROM zpp_suplr_perfor
        FOR ALL ENTRIES IN @xt_data
        WHERE suppliercode = @xt_data-suppliercode AND
              entry_month  = @xt_data-entry_month
        INTO TABLE @DATA(lt_month_data).

        SELECT * FROM zpp_suplr_perfor
        FOR ALL ENTRIES IN @xt_data
        WHERE suppliercode = @xt_data-suppliercode AND
              entry_year  = @xt_data-entry_year
        INTO TABLE @DATA(lt_year_data).

      ENDIF.

    ELSE.

    ENDIF.

    DATA: lv_email_add(512) TYPE c,
          lv_table1_data    TYPE string,
          lv_final_table1   TYPE string,
          lv_table2_data    TYPE string,
          lv_final_table2   TYPE string,
          lv_val_01         TYPE c LENGTH 20,
          lv_val_02         TYPE c LENGTH 20,
          lv_val_03         TYPE c LENGTH 20,
          lv_val_04         TYPE c LENGTH 20,
          lv_val_05         TYPE c LENGTH 20,
          lv_val_06         TYPE c LENGTH 20,
          lv_val_07         TYPE c LENGTH 20,
          lv_val_08         TYPE c LENGTH 20,
          lv_val_09         TYPE c LENGTH 20,
          lv_val_10         TYPE c LENGTH 20,
          lv_val_11         TYPE c LENGTH 20,
          lv_val_12         TYPE c LENGTH 20.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    DATA:
      lv_supplier TYPE zi_supplier_address-Supplier.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
      IF <lfs_data> IS ASSIGNED.

        TRY.

            ""**Creation of Instance
            DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).

            ""**Start:Preparation of receiver*********************
            CLEAR: lv_email_add.
            lv_email_add = 'rupadhyay@seniorauto.co.in'.

            IF <lfs_data>-suppliercode IS NOT INITIAL.

              lv_supplier = <lfs_data>-suppliercode.
              lv_supplier = |{ lv_supplier ALPHA = IN }| .

              SELECT SINGLE
              Supplier,
              SupplierName,
              EmailAddress
              FROM zi_supplier_address
              WHERE Supplier = @lv_supplier
              INTO @DATA(ls_supplr).

              IF ls_supplr-EmailAddress IS NOT INITIAL.
                lv_email_add = ls_supplr-EmailAddress.
              ENDIF.

            ENDIF.

            IF lv_email_add IS NOT INITIAL.
              lo_mail->add_recipient( lv_email_add ).
            ENDIF.

            lv_email_add = 'aman.sharma@mawaimail.com'.
            lo_mail->add_recipient( iv_address = lv_email_add iv_copy = cl_bcs_mail_message=>cc ).

            lv_email_add = 'rupadhyay@seniorauto.co.in'.
            lo_mail->add_recipient( iv_address = lv_email_add iv_copy = cl_bcs_mail_message=>cc ).

            lv_email_add = 'nrana@seniorauto.co.in'.
            lo_mail->add_recipient( iv_address = lv_email_add iv_copy = cl_bcs_mail_message=>cc ).

*    LOOP AT lt_email INTO DATA(ls_email).
*
*      lv_email_add = ls_email-emailid.
*      IF ls_email-to_cc = 'TO'.
*
*        IF gs_matdoc-supll_email IS INITIAL.
*          lo_mail->add_recipient( lv_email_add ).
*        ENDIF.

*      ELSEIF ls_email-to_cc = 'CC'.
*
*      ENDIF.
*
*    ENDLOOP.

            ""**End:Preparation of receiver*********************

            ""**E-Mail Subject
            lv_docnum = <lfs_data>-entry_month && <lfs_data>-entry_year.
            lo_mail->set_subject( |Supplier Performance Rating for the month of - | && lv_docnum ) ##NO_TEXT.

            DATA(lv_mail_body) = '<p>Dear Sir,</p>'
            && '<p>Based on the supplies received from you and assessment of the same for Quality and on time Delivery.</p>'.


            DATA(lv_table1_header) =
                    '<table border="1"'
                    && 'width="1100" ' && ' "overflow-y:scroll" ' && '><tbody><tr>'
                    && |<th>QUALITY SCORE</th>|
                    && |<th>DELIVERY SCORE</th>|
                    && |<th>OVERALL RATING</th>|
                    && '</tr>'.

            READ TABLE lt_month_data INTO DATA(ls_month) WITH KEY suppliercode = <lfs_data>-suppliercode.
            lv_table1_data  = lv_table1_data
            && '<tr>'
            && '<td>' && ls_month-qualityscore && '</td>'
            && '<td>' && ls_month-deliveryscore && '</td>'
            && '<td>' && ls_month-overallrating && '</td>'
            && '</tr>'.

            lv_table1_data  = lv_table1_data  && '</tbody></table>'.
            lv_final_table1 = lv_table1_header && lv_table1_data && '<br>'.

            DATA(lv_mail_body_2) = '<p>Now, Based on the supplies received trend of yearly performance rating.</p>'
            && '<br>'
            && '<br>'.

            DATA(lv_table2_header) =
                    '<table border="1"'
                    && 'width="1100" ' && ' "overflow-y:scroll" ' && '><tbody><tr>'
                    && |<th>JAN</th>|
                    && |<th>FEB</th>|
                    && |<th>MAR</th>|
                    && |<th>APR</th>|
                    && |<th>MAY</th>|
                    && |<th>JUN</th>|
                    && |<th>JUL</th>|
                    && |<th>AUG</th>|
                    && |<th>SEP</th>|
                    && |<th>OCT</th>|
                    && |<th>NOV</th>|
                    && |<th>DEC</th>|
                    && '</tr>'.

            CLEAR:
            lv_val_01,
            lv_val_02,
            lv_val_03,
            lv_val_04,
            lv_val_05,
            lv_val_06,
            lv_val_07,
            lv_val_08,
            lv_val_09,
            lv_val_10,
            lv_val_11,
            lv_val_12.

            LOOP AT lt_year_data INTO DATA(ls_year_data) WHERE suppliercode = <lfs_data>-suppliercode.
              IF ls_year_data-entry_month = '01'.
                lv_val_01 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '02'.
                lv_val_02 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '03'.
                lv_val_03 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '04'.
                lv_val_04 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '05'.
                lv_val_05 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '06'.
                lv_val_06 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '07'.
                lv_val_07 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '08'.
                lv_val_08 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '09'.
                lv_val_09 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '10'.
                lv_val_10 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '11'.
                lv_val_11 = ls_year_data-overallrating.
              ELSEIF ls_year_data-entry_month = '12'.
                lv_val_12 = ls_year_data-overallrating.
              ENDIF.
              CLEAR: ls_year_data.
            ENDLOOP.

            lv_table2_data  = lv_table2_data
            && '<tr>'
            && '<td>' && lv_val_01 && '</td>'
            && '<td>' && lv_val_02 && '</td>'
            && '<td>' && lv_val_03 && '</td>'
            && '<td>' && lv_val_04 && '</td>'
            && '<td>' && lv_val_05 && '</td>'
            && '<td>' && lv_val_06 && '</td>'
            && '<td>' && lv_val_07 && '</td>'
            && '<td>' && lv_val_08 && '</td>'
            && '<td>' && lv_val_09 && '</td>'
            && '<td>' && lv_val_10 && '</td>'
            && '<td>' && lv_val_11 && '</td>'
            && '<td>' && lv_val_12 && '</td>'
            && '</tr>'.

            lv_table2_data  = lv_table2_data  && '</tbody></table>'.
            lv_final_table2 = lv_table2_header && lv_table2_data && '<br>'.

            lv_rating = ls_month-overallrating.
            lv_score  = lv_rating.

            IF lv_score > 91 AND lv_score <= 100.

              lv_action_req = 'Good: Good, Sustain and control 4M changes.' ##NO_TEXT.

            ELSEIF lv_score > 81 AND lv_score <= 90.

              lv_action_req = 'Average: Work for better by improving process controls.' ##NO_TEXT.

            ELSEIF lv_score > 71 AND lv_score <= 80.

              lv_action_req = 'Below Average: Time bount action plan required for improvement.' ##NO_TEXT.

            ELSE.

              lv_action_req = 'Poor: Immediate attention required from top management . Share corrective action plan.' ##NO_TEXT.

            ENDIF.

            DATA(lv_body_data_3) = |<p></p>|
            && |<p>You are requested to take necessary actions as under.</p>|
            && |<p>RATING: { lv_rating } </p>|
            && |<p>ACTION REQUIRED: { lv_action_req } </p>|
            && |<p>We look forward to a mutually beneficial relationship based on continually improvement in the performance rating and shall appreciate a line of conformation.</p>|
            && |<p></p>|
            && '<br>'.

            DATA(lv_footer) = '<B>With Best Regards,,</B>' && '<br>'
            && 'For Senior India Private Limited' && '<br>'
            && 'Purchase HOD/QA HOD' && '<br>'
            && '<p>**** This is an auto generated Notification by SAP, please do not reply****</p>' ##NO_TEXT.

            DATA(lv_final_mail_body)  = lv_mail_body
                                        && lv_final_table1
                                        && lv_mail_body_2
                                        && lv_final_table2
                                        && lv_body_data_3
                                        && lv_footer.

            lo_mail->set_main( cl_bcs_mail_textpart=>create_text_html( lv_final_mail_body ) ).


            lo_mail->send( IMPORTING et_status = DATA(lt_status) ).

            IF sy-subrc = 0.
              <lfs_data>-status = 'Email sent successfully..!' ##NO_TEXT.
              APPEND <lfs_data> TO et_mail_stat.
            ENDIF.

            "*CATCH cx_web_http_conversion_failed.
          CATCH cx_bcs_mail INTO DATA(lx_mail).
            "handle exceptions here

            <lfs_data>-status = 'Error in sending E-mail...!' ##NO_TEXT.
            APPEND <lfs_data> TO et_mail_stat.

        ENDTRY.

      ENDIF.
      CLEAR: ls_month, lv_final_mail_body, lv_mail_body, lv_final_table1, lv_mail_body_2, lv_final_table2, lv_body_data_3, lv_footer.
      clear: lv_table1_data, lv_table1_header, lv_table2_header, lv_table2_data.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
