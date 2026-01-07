@EndUserText.label: 'CACHBOOK_REPORT'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_CASHBOOK_REPORT'
@UI.headerInfo : { typeName : 'Cashbook Report' , typeNamePlural: 'Cashbook Report'}

define custom entity ZI_cashbook_report
{

      //////      @UI.facet     : [{
      //////      id            : 'HeaderInfo',
      //////      purpose       : #STANDARD,
      //////      type          : #IDENTIFICATION_REFERENCE,
      //////      label         : 'General Info',
      //////      targetQualifier            : 'Main'
      //////      }]



      //  @UI.selectionField        : [{ position: 1  }]
      //   @UI.lineItem              : [{ position : 1 , label : 'Source Ledgerr' }]
      //   @UI.identification        : [{ position: 1, label: 'Purchase Order' ,  qualifier: 'Main' }]
  key SourceLedger             : abap.char(2);

      @UI.selectionField       : [{ position: 1  }]
      @UI.lineItem             : [{ position : 1 , label : 'Company Code' }]
      @UI.identification       : [{ position: 1, label: 'Company Code' ,  qualifier: 'Main' }]
      @Consumption.filter.mandatory: true
  key CompanyCode              : abap.char(4);

      @UI.selectionField       : [{ position: 4  }]
      @UI.lineItem             : [{ position : 4 , label : 'Fiscal Year' }]
      //  @UI.identification        : [{ position: 10, label: 'Purchase Or' }]
      @Consumption.filter.mandatory: true
  key FiscalYear               : abap.char(4);

      @UI.selectionField       : [{ position: 2  }]
      @UI.lineItem             : [{ position : 2 , label : 'Accounting Document' }]
  key AccountingDocument       : abap.char(10);

      //    @UI.lineItem              : [{ position : 5 , label : 'Ledger GL LineItem' }]
  key LedgerGLLineItem         : abap.char(6);


      @UI.selectionField       : [{ position: 3  }]
      @UI.lineItem             : [{ position : 3 , label : 'Accounting Document Type' }]
      //  @UI.identification        : [{ position: 10, label: 'Purchase Or' }]
      AccountingDocumentType   : abap.char(2);

      @UI.selectionField       : [{ position: 5  }]
      @Consumption.filter.mandatory: true
      PostingDate              : abap.dats;

      @UI.lineItem             : [{ position : 5 , label : 'Posting Date' }]
      Posting_Date             : abap.char(10);


      @UI.lineItem             : [{ position : 6 , label : 'Document Date' }]
      //  @UI.identification        : [{ position: 10, label: 'Purchase Or' }]
      DocumentDate             : abap.char(10);

      @UI.lineItem             : [{ position : 7 , label : 'Assignment Reference,' }]
      //  @UI.identification        : [{ position: 10, label: 'Purchase Or' }]
      AssignmentReference      : abap.char(10);

      @UI.lineItem             : [{ position :  8, label : 'Document ItemText' }]
      //  @UI.identification        : [{ position: 10, label: 'Purchase Or' }]
      DocumentItemText         : abap.char(50);

      @UI.lineItem             : [{ position : 9 , label : 'Reversal Reference Document' }]
      ReversReferenceDocument  : abap.char(10);

      //  @UI.selectionField         : [{ position: 6 }]
      @UI.lineItem             : [{ position : 10 , label : 'GL Account' }]
      //  @UI.identification        : [{ position: 10, label: 'Purchase Or' }]
      GLAccount                : abap.char(10);

      @UI.lineItem             : [{ position : 11 , label : 'GL Account LongName' }]
      //  @UI.identification        : [{ position: 10, label: 'Purchase Or' }]
      GLAccountLongName        : abap.char(50);

      // @UI.selectionField         : [{ position: 5  }]
      @UI.lineItem             : [{ position : 12 , label : 'Supplier' }]
      Supplier                 : abap.char(10);

      @UI.lineItem             : [{ position : 13 , label : 'Supplier Name' }]
      SupplierName             : abap.char(50);

      @UI.lineItem             : [{ position : 14 , label : 'Customer' }]
      Customer                 : abap.char(10);

      @UI.lineItem             : [{ position : 15 , label : 'Customer Name' }]
      CustomerName             : abap.char(50);

      @UI.lineItem             : [{ position : 16 , label : 'Cost Center' }]
      CostCenter               : abap.char(10);

      @UI.lineItem             : [{ position : 17 , label : 'Fiscal Period' }]
      FiscalPeriod             : abap.char(3);

      @UI.lineItem             : [{ position : 18 , label : 'Invoice Reference' }]
      InvoiceReference         : abap.char(10);

      @UI.lineItem             : [{ position : 19 , label : 'Ledger' }]
      Ledger                   : abap.char(2);

      @UI.lineItem             : [{ position : 20 , label : 'NetDueDate' }]
      NetDueDate               : abap.char(10);

      @UI.lineItem             : [{ position : 21 , label : 'TaxCode' }]
      TaxCode                  : abap.char(2);

      @UI.lineItem             : [{ position : 22 , label : 'Reference Document Type' }]
      ReferenceDocumentType    : abap.char(5);

      @UI.lineItem             : [{ position : 23 , label : 'Profit Center' }]
      ProfitCenter             : abap.char(10);

      @UI.lineItem             : [{ position : 24 , label : 'Debit Credit Code Name' }]
      DebitCreditCodeName      : abap.char(50);

      @UI.lineItem             : [{ position : 25 , label : 'Credit Amount In CoCodeCrcy' }]
      CreditAmountInCoCodeCrcy : abap.dec( 13, 2 );

      @UI.lineItem             : [{ position : 26 , label : 'Debit Amount In CoCodeCrcy' }]
      DebitAmountInCoCodeCrcy  : abap.dec( 13, 2 );


      @UI.lineItem             : [{ position : 27 , label : 'House Bank Key' }]
      HouseBank                : abap.char(5);

      @UI.lineItem             : [{ position : 28 , label : 'House Bank Account' }]
      HouseBankAccount         : abap.char(5);

      @UI.lineItem             : [{ position : 29 , label : 'IsReversal' }]
      IsReversal               : abap.char(1);

      @UI.lineItem             : [{ position : 30 , label : 'IsReversed' }]
      IsReversed               : abap.char(1);

      @UI.lineItem             : [{ position : 31 , label : 'Opening Balance' }]
      OpeningBalance           : abap.dec( 13, 2 );

      @UI.lineItem             : [{ position : 32 , label : 'Runing Balance' }]
      RuningBalance            : abap.dec( 13, 2 );

      @UI.lineItem             : [{ position : 33 , label : 'Closing Balance' }]
      ClosingBalance           : abap.dec( 13, 2 );


}
