@AbapCatalog.sqlViewName: 'ZV_BANK_PAYMENT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bank Payment Data'
define view ZI_BANK_PAYMENT
  as select from    ZI_DC_NOTE                as dc

    left outer join I_BusinessPartnerBank     as pbnk    on pbnk.BusinessPartner = dc.Supplier

    left outer join I_BusinessPartner         as bptnr   on bptnr.BusinessPartner = dc.Supplier

    left outer join ZI_SUPPLIER_ADDRESS       as adrc    on adrc.Supplier = dc.Supplier

    left outer join I_SuplrBankDetailsByIntId as intid   on intid.Supplier = dc.Supplier
    
    left outer join I_Bank_2 as bank2
    on bank2.BankCountry = intid.BankCountry and bank2.BankInternalID = intid.Bank

    left outer join I_HouseBankAccountLinkage as blink   on  blink.CompanyCode      = dc.CompanyCode
                                                         and blink.HouseBank        = dc.HouseBank
                                                         and blink.HouseBankAccount = dc.HouseBankAccount

    left outer join I_PaymentMethodText       as paymeth on  paymeth.PaymentMethod = dc.PaymentMethod
                                                         and paymeth.Country       = 'IN'
                                                         and paymeth.Language      = 'E'
{

  key dc.CompanyCode,
  key dc.AccountingDocument,
  key dc.FiscalYear,
      dc.AccountingDocumentItem,
      dc.FinancialAccountType,
      dc.ChartOfAccounts,
      dc.AccountingDocumentItemType,
      dc.PostingKey,
      dc.Product,
      dc.Plant,
      dc.PostingDate,
      dc.DocumentDate,
      dc.DebitCreditCode,
      dc.TaxCode,
      dc.TaxItemGroup,
      dc.TransactionTypeDetermination,
      dc.GLAccount,
      dc.Customer,
      dc.Supplier,
      dc.PurchasingDocument,
      dc.PurchasingDocumentItem,
      dc.PurchaseOrderQty,
      dc.ProfitCenter,
      dc.DocumentItemText,
      dc.AmountInCompanyCodeCurrency,
      dc.AmountInTransactionCurrency,
      dc.CashDiscountBaseAmount,
      dc.NetPaymentAmount,
      dc.AssignmentReference,
      dc.InvoiceReference,
      dc.InvoiceReferenceFiscalYear,
      dc.InvoiceItemReference,
      dc.Quantity,
      dc.BaseUnit,
      dc.MaterialPriceUnitQty,
      dc.TaxBaseAmountInTransCrcy,
      dc.ClearingJournalEntry,
      dc.ClearingDate,
      dc.ClearingCreationDate,
      dc.ClearingJournalEntryFiscalYear,
      dc.ClearingItem,
      dc.HouseBank,
      dc.BPBankAccountInternalID,
      dc.HouseBankAccount,
      dc.IN_HSNOrSACCode,
      dc.CostCenter,
      dc.AccountingDocumentType,
      dc.NetDueDate,
      dc.OffsettingAccount,
      dc.TransactionCurrency,
      dc.PaymentTerms,
      dc.BusinessPlace,
      dc.DocumentReferenceID,
      dc.ValueDate,
      dc.PaymentMethod,
      bptnr.BusinessPartner,
      bptnr.BusinessPartnerName,
      pbnk.BankAccount as BenefBankAccount,
      pbnk.BankNumber,
      pbnk.BankAccountHolderName,
      pbnk.BankCountryKey,
      pbnk.SWIFTCode,
      pbnk.BankControlKey,
      pbnk.CityName,
      adrc.EmailAddress,
      adrc.SupplierName,
      blink.BankAccount,
      paymeth.PaymentMethodDescription,
      bank2.BankName

}
