@AbapCatalog.sqlViewName: 'ZV_OL_BASIC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic OL'
define view ZI_OL_BASIC 

 with parameters
    @Environment.systemField: #SYSTEM_DATE
    P_KeyDate : sydate
    
as select from I_OperationalAcctgDocItem as op
{
    
 key op.CompanyCode,
 key op.AccountingDocument,
 key op.FiscalYear,
 key op.AccountingDocumentItem,
 op.ChartOfAccounts,
 op.AccountingDocumentItemType,
 op.ClearingDate,
 op.ClearingCreationDate,
 op.ClearingJournalEntryFiscalYear,
 op.ClearingJournalEntry,
 op.PostingKey,
 op.FinancialAccountType,
 op.SpecialGLCode,
 op.SpecialGLTransactionType,
 op.DebitCreditCode,
 op.BusinessArea,
 op.PartnerBusinessArea,
 op.TaxCode,
 op.TaxCountry,
 op.TaxRateValidityStartDate,
 op.WithholdingTaxCode,
 op.TaxType,
 op.TaxItemGroup,
 op.TransactionTypeDetermination,
 op.ValueDate,
 op.AssignmentReference,
 op.DocumentItemText,
 op.ControllingArea,
 op.CostCenter,
 op.IsSalesRelated,
 op.LineItemDisplayIsEnabled,
 op.IsOpenItemManaged,
 op.OperationalGLAccount,
 op.GLAccount,
 op.Customer,
 op.Supplier,
 op.BranchAccount,
 op.IsBalanceSheetAccount,
 op.ProfitLossAccountType,
 op.SpecialGLAccountAssignment,
 op.DueCalculationBaseDate,
 op.PaymentTerms,
 op.InvoiceReference,
 op.InvoiceReferenceFiscalYear,
 op.HouseBank,
 op.NetPaymentDays,
 op.Product,
 op.Plant,
 op.PurchasingDocument,
 op.PurchasingDocumentItem,
 op.MaterialPriceControl,
 op.ValuationArea,
 op.ProfitCenter,
 op.BusinessPlace,
 op.TaxSection,
 op.FunctionalArea,
 op.PartnerSegment,
 op.PartnerFunctionalArea,
 op.HouseBankAccount,
 op.CostElement,
 op.ClearingIsLedgerGroupSpecific,
 op.TaxItemAcctgDocItemRef,
 op.ReferenceDocumentType,
 op.OriginalReferenceDocument,
 op.FiscalPeriod,
 op.AccountingDocumentCategory,
 op.PostingDate,
 op.DocumentDate,
 op.AccountingDocumentType,
 op.NetDueDate,
 op.OffsettingChartOfAccounts,
 op.CompanyCodeCurrency,
 op.AmountInCompanyCodeCurrency,
 op.FunctionalCurrency,
 op.AmountInFunctionalCurrency,
 op.TaxAmountInCoCodeCrcy,
 op.TaxBaseAmountInCoCodeCrcy,
 op.TransactionCurrency,
 op.AmountInTransactionCurrency,
 op.OriginalTaxBaseAmount,
 op.TaxAmount,
 op.TaxBaseAmountInTransCrcy, 
 op.BalanceTransactionCurrency,
 op.AmountInBalanceTransacCrcy,
 op.AdditionalCurrency1,
 op.ValuationDiffAmtInAddlCrcy1,
 op.AmountInAdditionalCurrency1,
 op.IN_GSTPartner,
 op.IN_GSTPlaceOfSupply,
 op.IN_HSNOrSACCode,
 op.OriglTaxBaseAmountInCoCodeCrcy,
 op.OriginalTaxBaseAmtInAddlCrcy1,
 op.OriginalTaxBaseAmtInAddlCrcy2,
 dats_days_between(op.NetDueDate, :P_KeyDate) as nodays
    
} 
where 
op.PostingDate <= $parameters.P_KeyDate and
op.ClearingJournalEntry = '' and
op.FinancialAccountType = 'K' and
op.AccountingDocumentCategory <> 'S' and
op.AccountingDocumentCategory <> 'V'
