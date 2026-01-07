@AbapCatalog.sqlViewName: 'ZV_VEND_AGE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Vendor ageinging'
define view ZI_VEND_AGE

  with parameters
    @Environment.systemField: #SYSTEM_DATE
    P_KeyDate : sydate

  as select from    ZI_OPEN_LINE_ITEM(P_KeyDate : $parameters.P_KeyDate) as op

    left outer join I_JournalEntry                                       as jour on  jour.AccountingDocument = op.AccountingDocument
                                                                                 and jour.FiscalYear         = op.FiscalYear
                                                                                 and jour.CompanyCode        = op.CompanyCode

    left outer join I_JournalEntryItem                                   as jouritm on  jouritm.AccountingDocument = op.AccountingDocument
                                                                                    and jouritm.FiscalYear         = op.FiscalYear
                                                                                    and jouritm.CompanyCode        = op.CompanyCode
                                                                                    and jouritm.Ledger             = '0L'
                                                                                    and ( jouritm.TransactionTypeDetermination = 'WRX' or jouritm.TransactionTypeDetermination = 'FR3' )
                                                                                 
    left outer join I_Supplier                                           as supl on supl.Supplier = op.Supplier
    
    left outer join I_PaymentTermsText as paytmtxt on paytmtxt.PaymentTerms = op.PaymentTerms and paytmtxt.Language = 'E'
{

  key op.CompanyCode,
  key op.AccountingDocument,
  key op.FiscalYear,
      //      op.ClearingDate,
      //      op.ClearingAccountingDocument,
      op.AssignmentReference,
      op.DocumentItemText,
      //      op.CostCenter,
      op.OperationalGLAccount,
      op.GLAccount,
      op.Supplier,
      supl.SupplierName,
      op.ProfitCenter,
      op.BusinessPlace,
      op.PostingDate,
      op.DocumentDate,
      op.AccountingDocumentType,
      op.PaymentTerms,
      paytmtxt.PaymentTermsDescription,
      op.NetDueDate,
      op.CompanyCodeCurrency,
      op.AmountInCompanyCodeCurrency,
      op.TransactionCurrency,
      op.AmountInTransactionCurrency,
      jouritm.PurchasingDocument,
      jour.DocumentReferenceID,
      op.nodays,

      @EndUserText.label: 'days_0_30'
      case when op.nodays >= 0 and op.nodays <= 30
      then op.AmountInCompanyCodeCurrency * - 1
      end as days_1_30,

      @EndUserText.label: 'days_31_60'
      case when op.nodays >= 31 and op.nodays <= 60
      then op.AmountInCompanyCodeCurrency * - 1
      end as days_31_60,

      @EndUserText.label: 'days_61_90'
      case when op.nodays >= 61 and op.nodays <= 90
      then op.AmountInCompanyCodeCurrency * - 1
      end as days_61_90,

      @EndUserText.label: 'days_91_120'
      case when op.nodays >= 91 and op.nodays <= 120
      then op.AmountInCompanyCodeCurrency * - 1
      end as days_91_120,

      @EndUserText.label: 'days_121_150'
      case when op.nodays >= 121 and op.nodays <= 150
      then op.AmountInCompanyCodeCurrency * - 1
      end as days_121_150,

      @EndUserText.label: 'days_151_180'
      case when op.nodays >= 151 and op.nodays <= 180
      then op.AmountInCompanyCodeCurrency * - 1
      end as days_151_180,

      @EndUserText.label: 'days_ge_180'
      case when op.nodays >= 180
      then op.AmountInCompanyCodeCurrency * - 1
      end as days_ge_180,

      @EndUserText.label: 'Not due amount'
      case when op.NetDueDate > $parameters.P_KeyDate
      then op.AmountInCompanyCodeCurrency * - 1
      end as not_due_amount
                
}
