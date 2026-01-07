CLASS zcl_bank_data_file DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_file TYPE TABLE OF zstr_hsbc_bank_file , "zstr_hdfc_bank_file,
      gs_file TYPE zstr_hsbc_bank_file. "zstr_hdfc_bank_file.

    METHODS:
      get_accounting_data
        IMPORTING
                  im_input_str   TYPE string
        RETURNING VALUE(et_file) LIKE gt_file.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_BANK_DATA_FILE IMPLEMENTATION.


  METHOD get_accounting_data.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20,
      lv_amout_neg TYPE c LENGTH 20.

    TYPES: BEGIN OF gty_belnr,
             belnr_low TYPE c LENGTH 10,
           END OF gty_belnr.

    DATA:
      gt_belnr TYPE TABLE OF gty_belnr,
      gs_belnr TYPE gty_belnr.

    TYPES: BEGIN OF gty_input,
             CompanyCode     TYPE zi_dc_note-CompanyCode,
             PostingDateLow  TYPE c LENGTH 10,
             PostingDateHigh TYPE c LENGTH 10,
             belnr_high      TYPE c LENGTH 10,
             belnr_low       LIKE gt_belnr,
           END OF gty_input.

    DATA:
      gt_input TYPE TABLE OF gty_input,
      gs_input TYPE gty_input.

    DATA : r_belnr        TYPE RANGE OF zi_dc_note-AccountingDocument,
           rw_belnr       LIKE LINE OF  r_belnr,
           r_budat        TYPE RANGE OF zi_dc_note-PostingDate,
           rw_budat       LIKE LINE OF  r_budat,
           lv_FiscalYear  TYPE zi_dc_note-FiscalYear,
           lv_CompanyCode TYPE zi_dc_note-CompanyCode.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gt_input
                                 ).

    READ TABLE gt_input INTO gs_input INDEX 1.
    lv_CompanyCode = gs_input-companycode.
    "lv_FiscalYear  = gs_input-fiscalyear.

    IF gs_input-PostingDateHigh IS INITIAL.


      rw_budat-low     = gs_input-PostingDateLow+0(4) && gs_input-PostingDateLow+5(2) && gs_input-PostingDateLow+8(2).
      "*rw_budat-high    = '' .
      rw_budat-option  = 'EQ' .
      rw_budat-sign    = 'I' .
      APPEND rw_budat TO r_budat.

    ELSE.

      rw_budat-low     = gs_input-PostingDateLow+0(4) && gs_input-PostingDateLow+5(2) && gs_input-PostingDateLow+8(2).
      rw_budat-high    = gs_input-PostingDateHigh+0(4) && gs_input-PostingDateHigh+5(2) && gs_input-PostingDateHigh+8(2).
      rw_budat-option  = 'BT' .
      rw_budat-sign    = 'I' .
      APPEND rw_budat TO r_budat.

    ENDIF.

    IF gs_input-belnr_high IS INITIAL.

      LOOP AT gs_input-belnr_low INTO DATA(lss_belnr).

        rw_belnr-low     = lss_belnr-belnr_low.
        rw_belnr-high    = '' .
        rw_belnr-option  = 'EQ' .
        rw_belnr-sign    = 'I' .
        APPEND rw_belnr TO r_belnr.

        CLEAR: lss_belnr.
      ENDLOOP.

    ELSE.

      READ TABLE gs_input-belnr_low INTO DATA(ls_belnr) INDEX 1.
      rw_belnr-low     = ls_belnr-belnr_low.
      rw_belnr-high    = gs_input-belnr_high.
      rw_belnr-option  = 'BT'.
      rw_belnr-sign    = 'I'.
      APPEND rw_belnr TO r_belnr.

    ENDIF.

*    rw_belnr-low     = '2300000029'.
*    rw_belnr-high    = '' .
*    rw_belnr-option  = 'EQ' .
*    rw_belnr-sign    = 'I' .
*    APPEND rw_belnr TO r_belnr.
*
*    lv_CompanyCode = '1000'.
*    lv_FiscalYear  = '2023'.

    SELECT * FROM zi_bank_payment
             WHERE CompanyCode = @lv_CompanyCode AND
                   PostingDate IN @r_budat AND "FiscalYear  = @lv_FiscalYear AND
                   AccountingDocument IN @r_belnr AND
                   AccountingDocumentType EQ 'ZP'
             INTO TABLE @DATA(gt_acc) .       "#EC CI_ALL_FIELDS_NEEDED

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    """"*****************************************************************************************************
    TRY.

        CALL METHOD cl_numberrange_runtime=>number_get
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZBANK_NUM'
          IMPORTING
            number      = DATA(file_num)
            returncode  = DATA(rcode).
      CATCH cx_nr_object_not_found ##NO_HANDLER.
      CATCH cx_number_ranges ##NO_HANDLER.
    ENDTRY.

    "**Prepare File Header data
    SHIFT file_num LEFT DELETING LEADING '0'.
    DATA(lv_file_num)  = 'SENIOR_' && sys_date && file_num.
    DATA(lv_file_date) = sys_date+0(4) && '/' && sys_date+4(2) && '/' && sys_date+6(2).
    DATA(lv_file_time) = sys_time+0(2) && ':' && sys_time+2(2) && ':' && sys_time+4(2).

    CLEAR: gs_file.
    gs_file-column1  = 'IFH' ##NO_TEXT.                    "Record Type
    gs_file-column2  = 'IFILE' ##NO_TEXT.                  "File Format
    gs_file-column3  = 'CSV' ##NO_TEXT.                    "File Type
    gs_file-column4  = 'ABC34016001' ##NO_TEXT.            "HSBC Connect Customer ID
    gs_file-column5  = 'GBHBEUGB1008397369' ##NO_TEXT.     "HSBCnet Customer ID
    gs_file-column6  = lv_file_num.              "File Reference
    gs_file-column7  = lv_file_date.             "File Creation Date "'2024/04/19'
    gs_file-column8  = lv_file_time.             "File Creation Time "'12:09:17'
    gs_file-column9  = 'P' ##NO_TEXT.                      "Authorization Type
    gs_file-column10 = '1.0' ##NO_TEXT.                    "File Version
    gs_file-column11 = ''.                       "Record Count
    APPEND gs_file TO gt_file.

    """"*****************************************************************************************************
    DATA(ct_acc) = gt_acc[].
    DELETE gt_acc WHERE HouseBank EQ ''.
    DATA(mt_acc) = gt_acc[].
    DELETE gt_acc WHERE FinancialAccountType NE 'K'.

    SORT gt_acc BY CompanyCode FiscalYear AccountingDocument.
    DELETE ADJACENT DUPLICATES FROM gt_acc COMPARING CompanyCode FiscalYear AccountingDocument.

    DATA:
      lv_amt_trns_curr TYPE zi_bank_payment-AmountInTransactionCurrency.

    LOOP AT gt_acc ASSIGNING FIELD-SYMBOL(<lfs_new_acc>).

      CLEAR: lv_amt_trns_curr.

      IF <lfs_new_acc> IS ASSIGNED.

        LOOP AT mt_acc INTO DATA(ms_acc) WHERE CompanyCode = <lfs_new_acc>-CompanyCode AND
                                               FiscalYear  = <lfs_new_acc>-FiscalYear AND
                                               AccountingDocument = <lfs_new_acc>-AccountingDocument AND
                                               TransactionTypeDetermination = 'ZAH'.

          lv_amout_neg = ms_acc-AmountInTransactionCurrency.
          IF lv_amout_neg CA '-'.
            lv_amt_trns_curr = lv_amt_trns_curr + ms_acc-AmountInTransactionCurrency * -1.
          ELSE.
            lv_amt_trns_curr = lv_amt_trns_curr + ms_acc-AmountInTransactionCurrency.
          ENDIF.

          CLEAR: ms_acc, lv_amout_neg.
        ENDLOOP.

        <lfs_new_acc>-AmountInTransactionCurrency = lv_amt_trns_curr.

      ENDIF.

    ENDLOOP.

    LOOP AT gt_acc INTO DATA(gs_acc).

      READ TABLE ct_acc INTO DATA(cs_acc) WITH KEY
                     CompanyCode = gs_acc-CompanyCode
                     FiscalYear  = gs_acc-FiscalYear
                     AccountingDocument = gs_acc-AccountingDocument
                     FinancialAccountType = 'S'.

      "lv_amtintranscurr = lv_amtintranscurr + gs_acc-AmountInTransactionCurrency.

      "*Prepare: Batch Header and First Party Details Record
      CLEAR: gs_file.
      gs_file-column1  = 'BATHDR' ##NO_TEXT.  "*Record Type
      gs_file-column2  = 'LTR' ##NO_TEXT.     "*Instruction Type
      gs_file-column3  = '1' ##NO_TEXT.       "*Total number of instructions in batch
      gs_file-column4  = ''.        "*Batch Reference
      gs_file-column5  = ''.        "*Filler
      gs_file-column6  = ''.        "*Filler
      gs_file-column7  = ''.        "*Filler
      gs_file-column8  = ''.        "*Filler
      gs_file-column9  = ''.        "*Filler
      gs_file-column10 = '@1ST@' ##NO_TEXT.   "*Constant Eye Catcher
      gs_file-column11 = cs_acc-ValueDate.        "*Value Date (YYYYMMDD) --------------->sheo
      gs_file-column12 = gs_acc-BankAccount.        "*First Party Account   --------------->sheo
      gs_file-column13 = 'INR' ##NO_TEXT.     "*Transaction Currency*
      gs_file-column14 = gs_acc-AmountInTransactionCurrency.        "*Transaction Amount*   --------------->sheo
      CONDENSE gs_file-column14.
      gs_file-column15 = ''.        "*Template Mode
      gs_file-column16 = ''.        "*Batch Template ID
      gs_file-column17 = 'IN' ##NO_TEXT.     "*First Party Account Country
      gs_file-column18 = 'HBAP' ##NO_TEXT.    "*First Party Account Institution
      gs_file-column19 = 'INR' ##NO_TEXT.     "*First Party Account Currency*
      gs_file-column20 = ''.        "*Payment Amount in Debit account currency*
      gs_file-column21 = ''.        "*First Party Name
      gs_file-column22 = ''.        "*First Party Information Line 1
      gs_file-column23 = ''.        "*First Party Information Line 2
      gs_file-column24 = ''.        "*First Party Information Line 3
      gs_file-column25 = ''.        "*First Party Information Line 4
      gs_file-column26 = ''.        "*Payment Code
      gs_file-column27 = gs_acc-SupplierName+0(30). "gs_acc-AccountingDocument && gs_acc-FiscalYear.        "*Reference Line 1 --------------->sheo
      APPEND gs_file TO gt_file.

      "**Second Party Details Record for HVP and LVP payments
      CLEAR: gs_file.
      gs_file-column1  = 'SECPTY' ##NO_TEXT.                              "*Record Type
      gs_file-column2  = gs_acc-BenefBankAccount.               "*Second Party Account Number    """---Sheo
      gs_file-column3  = gs_acc-BankAccountHolderName.          "*Second Party Name              """---Sheo
      gs_file-column4  = ''.                                    "*Second Party Identifier
      gs_file-column5  = ''.                                    "*Beneficiary Bank Number / ID
      gs_file-column6  = ''.                                    "*Beneficiary Branch Number
      gs_file-column7  = ''.                                    "*Transaction Code
      gs_file-column8  = gs_acc-AmountInTransactionCurrency.    "*Second Party Transaction Amount """----Sheo
      CONDENSE gs_file-column8.
      gs_file-column9  = cs_acc-ValueDate.                      "*Entry Value Date (YYYYMMDD)     """----Sheo
      gs_file-column10 = ''.                                    "*Second Party Reference
      gs_file-column11 = ''.                                    "*Information Line 1
      gs_file-column12 = ''.                                    "*Information Line 2
      gs_file-column13 = ''.                                    "*Information Line 3
      gs_file-column14 = ''.                                    "*Information Line 4
      gs_file-column15 = 'Y' ##NO_TEXT.                                   "*Advice Indicator
      gs_file-column16 = 'N' ##NO_TEXT.                                   "*WHT Indicator
      gs_file-column17 = ''.                                    "*Filler
      gs_file-column18 = ''.                                    "*Filler
      gs_file-column19 = ''.                                    "*Filler
      gs_file-column20 = ''.                                    "*Filler
      gs_file-column21 = ''.                                    "*Filler
      gs_file-column22 = '@HVP@' ##NO_TEXT.                               "*Constant Eye Catcher
      gs_file-column23 = gs_acc-SupplierName+0(30). "gs_acc-AccountingDocument && gs_acc-FiscalYear.  "*First Party Reference ""---Sheo
      CONDENSE gs_file-column23.
      gs_file-column24 = 'INR' ##NO_TEXT.                                 "*Payment Currency
      gs_file-column25 = ''.                                    "*Template ID
      gs_file-column26 = ''.                                    "*Exchange Rate
      gs_file-column27 = ''.                                    "*Intermediary Institution Bank ID/SWIFT Address Code
      gs_file-column28 = ''.                                    "*Intermediary Institution Bank Name
      gs_file-column29 = ''.                                    "*Intermediary Institution Bank Address Line 1
      gs_file-column30 = ''.                                    "*Intermediary Institution Bank Address Line 2
      gs_file-column31 = ''.                                    "*Intermediary Institution Bank Address Line 3
      gs_file-column32 = ''.                                    "*Intermediary Institution Bank Address Line 4
      gs_file-column33 = ''.                                    "*Intermediary Institution Bank ID/SWIFT Address
      gs_file-column34 = ''.                                    "*Intermediary Institution Account Number
      gs_file-column35 = 'BCD' ##NO_TEXT.                                 "*Beneficiary Bank ID/SWIFT Address Code
      gs_file-column36 = gs_acc-BankNumber.                     "*Beneficiary Bank ID/SWIFT Address ""---Sheo
      gs_file-column37 = ''.                                    "*Beneficiary Bank Name
      gs_file-column38 = ''.                                    "*Beneficiary Bank Address Line 1
      gs_file-column39 = ''.                                    "*Beneficiary Bank Address Line 2
      gs_file-column40 = ''.                                    "*Beneficiary Bank Address Line 3
      gs_file-column41 = ''.                                    "*Beneficiary Bank Address Line 4
      gs_file-column42 = 'IN' ##NO_TEXT.                                  "*Beneficiary Bank Country
      gs_file-column43 = ''.                                    "*Beneficiary CHIPS UID
      gs_file-column44 = ''. "gs_acc-AccountingDocument && gs_acc-FiscalYear.     "*Payment Details Line 1               """------Sheo
      gs_file-column45 = 'PAYMENT AGAINST PENDING BILLSSENI' ##NO_TEXT.   "*Payment Details Line 2
      gs_file-column46 = 'OR INDIA' ##NO_TEXT.                            "*Payment Details Line 3
      gs_file-column47 = ''.                                    "*Payment Details Line 4
      gs_file-column48 = 'OUR' ##NO_TEXT.                                 "*Details of Charges
      gs_file-column49 = ''.                                    "*Sender Charges
      gs_file-column50 = ''.                                    "*Charges Account
      gs_file-column51 = ''.                                    "*Instruction Code Line 1
      gs_file-column52 = ''.                                    "*Instruction Code Line 2
      gs_file-column53 = ''.                                    "*Instruction Code Line 3
      gs_file-column54 = ''.                                    "*Instruction Code Line 4
      gs_file-column55 = ''.
      IF gs_acc-AmountInTransactionCurrency LT 200000.
        gs_file-column56 = '/ACC/NEFT' ##NO_TEXT.                           "*Bank to Bank Information Line 1
      ELSE.
        gs_file-column56 = '/ACC/RTGS' ##NO_TEXT.                           "*Bank to Bank Information Line 1
      ENDIF.
      APPEND gs_file TO gt_file.

      "**Advising Record
      SELECT SINGLE * FROM zi_supplier_address
      WHERE Supplier = @gs_acc-Supplier INTO @DATA(ls_supplier). "#EC CI_ALL_FIELDS_NEEDED

      SPLIT ls_supplier-EmailAddress AT '@' INTO DATA(part1) DATA(part2).

      CLEAR: gs_file.
      gs_file-column1  = 'ADV' ##NO_TEXT.                 "*Record Type
      gs_file-column2  = ''.                    "*Advice Recipient ID
      gs_file-column3  = 'R' ##NO_TEXT.                   "*Action Flag
      gs_file-column4  = ''.                    "*Recipient template description
      gs_file-column5  = ''.                    "*User ID
      gs_file-column6  = ''.                    "*User First Name
      gs_file-column7  = ''.                    "*User Last Name
      gs_file-column8  = '2' ##NO_TEXT.                   "*Total Number of Recipient (1 – 6)  "----Sheo
      gs_file-column9  = '1' ##NO_TEXT.                   "*Recipient Item No      "----Sheo
      gs_file-column10 = part1.     "*Recipient Name "----Sheo
      gs_file-column11 = ''.                    "*Recipient Title Flag
      gs_file-column12 = ''.                    "*Recipient Title Description
      gs_file-column13 = ''.                    "*Action Code
      gs_file-column14 = ''.                    "*Template ID
      gs_file-column15 = ''.                    "*Template Status
      gs_file-column16 = ''.                    "*Template Timestamp
      gs_file-column17 = 'F' ##NO_TEXT.                   "*Advice Format
      gs_file-column18 = 'Y' ##NO_TEXT.                   "*Email channel select flag
      gs_file-column19 = '1' ##NO_TEXT.                   "*Email Format
      gs_file-column20 = ls_supplier-EmailAddress.  "*Email Address "----Sheo
      gs_file-column21 = ''.                           "*Alternate Email Address
      gs_file-column22 = 'IN' ##NO_TEXT.                         "*Domicile of email recipient
      APPEND gs_file TO gt_file.
      CLEAR: part1, part2.


      "**Advising Record
      CLEAR: gs_file.
      gs_file-column1  = 'ADV' ##NO_TEXT.                 "*Record Type
      gs_file-column2  = ''.                    "*Advice Recipient ID
      gs_file-column3  = 'R' ##NO_TEXT.                   "*Action Flag
      gs_file-column4  = ''.                    "*Recipient template description
      gs_file-column5  = ''.                    "*User ID
      gs_file-column6  = ''.                    "*User First Name
      gs_file-column7  = ''.                    "*User Last Name
      gs_file-column8  = '2' ##NO_TEXT.                   "*Total Number of Recipient (1 – 6)  "----Sheo
      gs_file-column9  = '2' ##NO_TEXT.                   "*Recipient Item No      "----Sheo
      gs_file-column10 = 'accounts' ##NO_TEXT.     "*Recipient Name "----Sheo
      gs_file-column11 = ''.                    "*Recipient Title Flag
      gs_file-column12 = ''.                    "*Recipient Title Description
      gs_file-column13 = ''.                    "*Action Code
      gs_file-column14 = ''.                    "*Template ID
      gs_file-column15 = ''.                    "*Template Status
      gs_file-column16 = ''.                    "*Template Timestamp
      gs_file-column17 = 'F' ##NO_TEXT.                   "*Advice Format
      gs_file-column18 = 'Y' ##NO_TEXT.                   "*Email channel select flag
      gs_file-column19 = '1' ##NO_TEXT.                   "*Email Format
      gs_file-column20 = 'accounts@ims.co.in' ##NO_TEXT.  "*Email Address "----Sheo
      gs_file-column21 = ''.                           "*Alternate Email Address
      gs_file-column22 = 'IN' ##NO_TEXT.                         "*Domicile of email recipient
      APPEND gs_file TO gt_file.

      "*Advice Details - Table Header
      "*ADV-TBLTXT,5,20,L,INV NO,10,L,TDS DED.,15,L,PARTICULARS,10,L,OTHER DED.,35,L,REMARKS
      CLEAR: gs_file.
      gs_file-column1  = 'ADV-TBLTXT' ##NO_TEXT.    "*Record Type
      gs_file-column2  = '5' ##NO_TEXT.             "*Number of column
      gs_file-column3  = '20' ##NO_TEXT.            "*1st Column width *
      gs_file-column4  = 'L' ##NO_TEXT.             "*1st Column justification
      gs_file-column5  = 'inv no' ##NO_TEXT.        "*1st Column header
      gs_file-column6  = '10' ##NO_TEXT.            "*2nd Column width *
      gs_file-column7  = 'L' ##NO_TEXT.             "*2nd Column justification
      gs_file-column8  = 'TDS DED' ##NO_TEXT.       "*2nd Column header
      gs_file-column9  = '15' ##NO_TEXT.            "*3rd Column width *
      gs_file-column10 = 'L' ##NO_TEXT.             "*3rd Column justification
      gs_file-column11 = 'PARTICULARS' ##NO_TEXT.   "*3rd Column header
      gs_file-column12 = '10' ##NO_TEXT.            "*4th Column width *
      gs_file-column13 = 'L' ##NO_TEXT.             "*4th Column justification
      gs_file-column14 = 'OTHER DED' ##NO_TEXT.     "*4th Column header
      gs_file-column15 = '35' ##NO_TEXT.            "*5th Column width *
      gs_file-column16 = 'L' ##NO_TEXT.             "*5th Column justification
      gs_file-column17 = 'AMOUNT' ##NO_TEXT.        "*5th Column header
      gs_file-column18 = ''.              "*6th Column width *
      gs_file-column19 = ''.              "*6th Column justification
      gs_file-column20 = ''.              "*6th Column header
      APPEND gs_file TO gt_file.

      SELECT
      *
      FROM zi_dc_note
      WHERE
      companycode = @gs_acc-CompanyCode AND ClearingJournalEntry = @gs_acc-AccountingDocument AND ClearingJournalEntryFiscalYear = @gs_acc-FiscalYear
      INTO TABLE @DATA(lt_acc). "#EC CI_ALL_FIELDS_NEEDED "#EC CI_NO_TRANSFORM

      IF lt_acc[] IS NOT INITIAL.

        DELETE lt_acc WHERE AccountingDocument = gs_acc-AccountingDocument.

      ENDIF.


      "*Advice Details - Table Body (one row per record)
      LOOP AT lt_acc INTO DATA(ls_acc_new).

        CLEAR: gs_file.
        gs_file-column1  = 'ADV-TBLBDY'.    "*Record Type
        gs_file-column2  = ls_acc_new-DocumentReferenceID.           "*Table entry column 1 detail * "----Sheo
        gs_file-column3  = '20'.            "*Table entry column 1 length
        gs_file-column4  = ''.              "*Table entry column 2 detail *
        gs_file-column5  = '10'.            "*Table entry column 2 length
        gs_file-column6  = ''.              "*Table entry column 3 detail *
        gs_file-column7  = '15'.            "*Table entry column 3 length
        gs_file-column8  = ''.              "*Table entry column 4 detail *
        gs_file-column9  = '10'.            "*Table entry column 4 length
        gs_file-column10 = ls_acc_new-AmountInTransactionCurrency * -1.     "*Table entry column 5 detail *  "----Sheo
        CONDENSE gs_file-column10.
        gs_file-column11 = '35'.            "*Table entry column 5 length
        gs_file-column12 = ''.              "*Table entry column 6 detail *
        gs_file-column13 = ''.              "*Table entry column 6 length
        APPEND gs_file TO gt_file.

        CLEAR: ls_acc_new.
      ENDLOOP.

*      gs_file-customer_name     = ''.
*      gs_file-transaction_type  = gs_acc-PaymentMethodDescription.
*      gs_file-reference_number  = gs_acc-AccountingDocument && gs_acc-FiscalYear.
*      gs_file-dr_account_no     = cs_acc-BankAccount.
*      gs_file-payment_narration = ''.
*      gs_file-beneficiary_name  = gs_acc-BankAccountHolderName.
*      gs_file-bene_add_1        = ''.
*      gs_file-bene_add_2        = ''.
*      gs_file-bene_add_3        = ''.
*      gs_file-payment_location  = ''.
*      gs_file-cheque_no         = ''.
*      gs_file-value_date        = cs_acc-ValueDate.
*      gs_file-amount            = cs_acc-AmountInTransactionCurrency.
*      gs_file-print_branch_location = ''.
*      gs_file-email_add_1           = gs_acc-EmailAddress.
*      gs_file-email_add_2           = ''.
*      gs_file-email_add_3           = ''.
*      gs_file-freetext              = ''.
*      gs_file-na1                   = ''.
*      gs_file-na2                   = ''.
*      gs_file-na3                   = ''.
*      gs_file-na4                   = ''.
*      gs_file-na5                   = ''.
*      gs_file-bene_bank_account     = gs_acc-BenefBankAccount.
*      gs_file-bene_bank_ifsc        = gs_acc-BankNumber.
*      gs_file-na6                   = ''.
*      gs_file-deliver_to            = ''.
*      gs_file-ordering_party_name   = ''.
*      gs_file-ordering_party_add1   = ''.
*      gs_file-ordering_party_add2   = ''.
*      gs_file-ordering_party_add3   = ''.
*      gs_file-ordering_party_account = ''.
*      gs_file-bank_name              = ''.
*      gs_file-bank_to_bank_info      = ''.
*      APPEND gs_file TO gt_file.

      CLEAR: gs_acc, lt_acc, ls_supplier.
    ENDLOOP.

    READ TABLE gt_file ASSIGNING FIELD-SYMBOL(<lfs_file>) INDEX 1. "Counting total number of record
    DATA(lv_line) = Lines( gt_file[] ).
    <lfs_file>-column11 = lv_line.
    CONDENSE <lfs_file>-column11.

*    READ TABLE gt_file ASSIGNING FIELD-SYMBOL(<lfs_file1>) INDEX 2. "Counting total number of supplier
*    DATA(lv_line1) = Lines( gt_acc[] ).
*    <lfs_file1>-column3  = lv_line1.
*    CONDENSE <lfs_file1>-column3.


    IF gt_file[] IS NOT INITIAL.
      et_file[] = gt_file[].
    ENDIF.

  ENDMETHOD.
ENDCLASS.
