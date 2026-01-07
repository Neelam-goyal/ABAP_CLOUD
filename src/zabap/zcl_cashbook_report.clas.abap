CLASS zcl_cashbook_report DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CASHBOOK_REPORT IMPLEMENTATION.


  METHOD if_rap_query_provider~select.
    IF io_request->is_data_requested(  ).

      DATA : lt_response TYPE TABLE OF zi_cashbook_report.
      DATA : ls_response LIKE LINE OF lt_response.

      DATA : lt_responseout LIKE lt_response.
      DATA : ls_responseout LIKE LINE OF lt_responseout.

      DATA(lt_clause) = io_request->get_paging(  )->get_page_size( ).
      DATA(lt_parameter) = io_request->get_parameters(  ).
      DATA(lt_filter) = io_request->get_requested_elements(  ).
      DATA(lt_sort) = io_request->get_sort_elements(  ).

      DATA(lv_top) = io_request->get_paging(  )->get_page_size(  ).
      DATA(lv_skip) = io_request->get_paging(  )->get_offset(  ).
      DATA(lv_max_raws) = COND #( WHEN lv_top = if_rap_query_paging=>page_size_unlimited THEN 0 ELSE lv_top ).

      TRY .
          DATA(lt_filter_cond) = io_request->get_filter(  )->get_as_ranges(  ).
        CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_sel_option) ##NO_HANDLER.
      ENDTRY.

      LOOP AT lt_filter_cond INTO DATA(ls_filter_cond).

        IF ls_filter_cond-name = 'COMPANYCODE'.
          DATA(ls_companycode) = ls_filter_cond-range[].
        ENDIF.

        IF ls_filter_cond-name = 'ACCOUNTINGDOCUMENT'.
          DATA(ls_accountingdocument) = ls_filter_cond-range[].
        ENDIF.

        IF ls_filter_cond-name = 'ACCOUNTINGDOCUMENTTYPE'.
          DATA(ls_accountingdocumenttype) = ls_filter_cond-range[].
        ENDIF.

        IF ls_filter_cond-name = 'FISCALYEAR'.
          DATA(ls_fiscalyear) = ls_filter_cond-range[].
        ENDIF.

        IF ls_filter_cond-name = 'POSTINGDATE'.
          DATA(ls_postingdate) = ls_filter_cond-range[].
        ENDIF.

      ENDLOOP.

***      LOOP AT ls_postingdate ASSIGNING FIELD-SYMBOL(<ls_pd>).
***        REPLACE ALL OCCURRENCES OF '/' IN <ls_pd>-low WITH ''.
***        REPLACE ALL OCCURRENCES OF '/' IN <ls_pd>-high WITH ''.
***      ENDLOOP.

      DATA(lv_companycode)     = ls_companycode[ 1 ]-low.
      DATA(lv_fiscalyear)      = ls_fiscalyear[ 1 ]-low.
      DATA(lv_postingdate_low) = ls_postingdate[ 1 ]-low.
      DATA(lv_postingdate_high) = ls_postingdate[ 1 ]-high.
      DATA: lv_opening_balance TYPE p DECIMALS 2 .

      SELECT je~sourceledger,
             je~ledgergllineitem,
             je~companycode,
             je~accountingdocument,
             je~accountingdocumenttype,
             je~fiscalyear,
             je~postingdate,
             je~documentdate,
             je~assignmentreference,
             je~documentitemtext,
             je~reversalreferencedocument,
             je~glaccount,
             je~supplier,
             sup~suppliername AS suppliername,
             je~customer,
             cust~customername AS customername,
             je~costcenter,
             je~fiscalperiod,
             je~invoicereference,
             je~ledger,
             je~netduedate,
             je~taxcode,
             je~referencedocumenttype,
             je~profitcenter,
             je~debitcreditcode,
             je~creditamountincocodecrcy,
             je~debitamountincocodecrcy,
             je~housebank,
             je~housebankaccount,
             je~isreversal,
             je~isreversed,
             je~amountincompanycodecurrency,
             gl~glaccountname
               FROM i_journalentryitem AS je
               LEFT OUTER JOIN i_supplier AS sup
                 ON je~supplier = sup~supplier
               LEFT OUTER JOIN i_customer AS cust
                 ON je~customer = cust~customer
               LEFT OUTER JOIN i_glaccounttext AS gl
                 ON je~glaccount = gl~glaccount
                AND je~chartofaccounts = gl~chartofaccounts
               WHERE je~companycode        IN @ls_companycode
                 AND je~accountingdocument     IN @ls_accountingdocument
                 AND je~accountingdocumenttype IN @ls_accountingdocumenttype
                 AND je~fiscalyear             IN @ls_fiscalyear
                 AND je~postingdate            BETWEEN @lv_postingdate_low AND @lv_postingdate_high
                 AND je~glaccount              <> '0000213000'
                 AND je~referencedocumenttype  =  'CAJO'
                 AND je~ledger                 =  '0L'
               INTO TABLE @DATA(it_final).


*--------------------------------------------------------------------*
********************  Calculate Opening Balance***********************
*--------------------------------------------------------------------*


      SELECT SUM( amountincompanycodecurrency )
      FROM i_journalentryitem
      WHERE companycode IN @ls_companycode
      AND ledger      = '0L'
      AND glaccount  <> '0000213000'
      AND referencedocumenttype  =  'CAJO'
      AND postingdate LE @lv_postingdate_low
      INTO @lv_opening_balance.

      IF lv_opening_balance IS INITIAL.
        lv_opening_balance = 0.
      ENDIF.

*--------------------------------------------------------------------*
*  Insert Opening Balance Line
*--------------------------------------------------------------------*

      ls_response-documentitemtext = 'Opening Balance' ##NO_TEXT.
      ls_response-openingbalance   = lv_opening_balance.
      APPEND ls_response TO lt_response.
*--------------------------------------------------------------------*
*  Calculate Running Balance
*--------------------------------------------------------------------*
      DATA(lv_running_balance) = lv_opening_balance.

      LOOP AT it_final INTO DATA(wa_final).
        CLEAR: ls_response.

        MOVE-CORRESPONDING wa_final TO ls_response.

        DATA(lv_debit)  = COND #( WHEN wa_final-debitamountincocodecrcy  IS NOT INITIAL
                                  THEN wa_final-debitamountincocodecrcy
                                  ELSE 0 ).
        DATA(lv_credit) = COND #( WHEN wa_final-creditamountincocodecrcy IS NOT INITIAL
                                  THEN wa_final-creditamountincocodecrcy
                                  ELSE 0 ).

        IF wa_final-debitcreditcode = 'S'. " Debit → Add
          lv_running_balance = lv_running_balance + lv_debit.
        ELSEIF wa_final-debitcreditcode = 'H'. " Credit → Subtract
          lv_running_balance = lv_running_balance + lv_credit.
        ENDIF.

        ls_response-posting_date             = |{ wa_final-postingdate+6(2) }.{ wa_final-postingdate+4(2) }.{ wa_final-postingdate+0(4) }|.
        ls_response-documentdate             = |{ wa_final-documentdate+6(2) }.{ wa_final-documentdate+4(2) }.{ wa_final-documentdate+0(4) }|.
        ls_response-netduedate               = |{ wa_final-netduedate+6(2) }.{ wa_final-netduedate+4(2) }.{ wa_final-netduedate+0(4) }|.
        ls_response-sourceledger             = wa_final-sourceledger.
        ls_response-ledgergllineitem         = wa_final-ledgergllineitem.
        ls_response-companycode              = wa_final-companycode.
        ls_response-accountingdocument       = wa_final-accountingdocument.
        ls_response-accountingdocumenttype   = wa_final-accountingdocumenttype.
        ls_response-fiscalyear               = wa_final-fiscalyear.
        ls_response-assignmentreference      = wa_final-assignmentreference.
        ls_response-documentitemtext         = wa_final-documentitemtext.
        ls_response-reversreferencedocument  = wa_final-reversalreferencedocument.
        ls_response-glaccount                = wa_final-glaccount.
        ls_response-glaccountlongname        = wa_final-glaccountname.
        ls_response-supplier                 = wa_final-supplier.
        ls_response-customer                 = wa_final-customer.
        ls_response-customername             = wa_final-customername.
        ls_response-suppliername             = wa_final-suppliername.
        ls_response-costcenter               = wa_final-costcenter.
        ls_response-fiscalperiod             = wa_final-fiscalperiod.
        ls_response-invoicereference         = wa_final-invoicereference.
        ls_response-ledger                   = wa_final-ledger.
        ls_response-taxcode                  = wa_final-taxcode.
        ls_response-referencedocumenttype    = wa_final-referencedocumenttype.
        ls_response-profitcenter             = wa_final-profitcenter.
        ls_response-debitcreditcodename      = wa_final-debitcreditcode.
        ls_response-creditamountincocodecrcy = wa_final-creditamountincocodecrcy.
        ls_response-debitamountincocodecrcy  = wa_final-debitamountincocodecrcy .
        ls_response-housebank                = wa_final-housebank.
        ls_response-housebankaccount         = wa_final-housebankaccount.
        ls_response-isreversal               = wa_final-isreversal.
        ls_response-isreversed               = wa_final-isreversed .
        ls_response-openingbalance           = '0'.

        ls_response-runingbalance           =  lv_running_balance.

        APPEND ls_response TO lt_response.

      ENDLOOP.

      " ----- Replace the simple CLEAR / APPEND blank row with this -----

      ls_response-posting_date             = ''.
      ls_response-documentdate             = ''.
      ls_response-netduedate               = ''.
      ls_response-sourceledger             = ''.
      ls_response-ledgergllineitem         = ''.
      ls_response-accountingdocument       = ''.
      ls_response-accountingdocumenttype   = ''.
      ls_response-assignmentreference      = ''.
      ls_response-reversreferencedocument  = ''.
      ls_response-glaccount                = ''.
      ls_response-glaccountlongname        = ''.
      ls_response-supplier                 = ''.
      ls_response-customer                 = ''.
      ls_response-customername             = ''.
      ls_response-suppliername             = ''.
      ls_response-costcenter               = ''.
      ls_response-fiscalperiod             = ''.
      ls_response-invoicereference         = ''.
      ls_response-ledger                   = ''.
      ls_response-taxcode                  = ''.
      ls_response-referencedocumenttype    = ''.
      ls_response-profitcenter             = ''.
      ls_response-debitcreditcodename      = ''.
      ls_response-creditamountincocodecrcy = ''.
      ls_response-debitamountincocodecrcy  = ''.
      ls_response-housebank                = ''.
      ls_response-housebankaccount         = ''.
      ls_response-isreversal               = ''.
      ls_response-isreversed               = ''.

      " fill all CDS KEY fields (must be non-initial)
      ls_response-sourceledger       = 'ZZ'.                 " SourceLedger  (char2)
      ls_response-companycode        = lv_companycode.       " CompanyCode   (char4)
      ls_response-fiscalyear         = lv_fiscalyear.        " FiscalYear    (char4)
      ls_response-accountingdocument = ''.        " AccountingDocument (char10) - padded to 10
      ls_response-ledgergllineitem   = '999999'.             " LedgerGLLineItem (char6)
      ls_response-documentitemtext = 'Closing Balance' ##NO_TEXT.
      ls_response-closingbalance   = ls_response-runingbalance .

      " set other numeric fields to 0 (types must match)
      ls_response-openingbalance     = 0.
      ls_response-runingbalance      = 0.

      APPEND ls_response TO lt_response.
      " -------------------------------------------------------------------


      " Paging logic
      IF lv_top = if_rap_query_paging=>page_size_unlimited.
        lt_responseout = lt_response.
      ELSE.
        LOOP AT lt_response ASSIGNING FIELD-SYMBOL(<fs_line>) "#EC CI_NOORDER
             FROM ( lv_skip + 1 ) TO ( lv_skip + lv_top ).
          APPEND <fs_line> TO lt_responseout.
        ENDLOOP.
      ENDIF.

      io_response->set_total_number_of_records( lines( lt_response ) ).
      io_response->set_data( lt_responseout ).


***      lv_max_raws = lv_skip + lv_top.
***      IF lv_skip > 0 .
***        lv_skip = lv_skip + 1 .
***      ENDIF.
***
***      CLEAR lt_responseout.
***      LOOP AT lt_response ASSIGNING FIELD-SYMBOL(<lfs_out_line_item>) FROM lv_skip TO lv_max_raws.
***        ls_responseout = <lfs_out_line_item>.
***        APPEND ls_responseout TO lt_responseout.
***      ENDLOOP.
***
***      io_response->set_total_number_of_records( lines( lt_response ) ).
***      io_response->set_data( lt_responseout ).


    ENDIF.
  ENDMETHOD.
ENDCLASS.
