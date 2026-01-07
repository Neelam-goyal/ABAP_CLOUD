@AbapCatalog.sqlViewName: 'ZI_DED_N'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BARDANA'
define view ZI_DED
  as select from I_OperationalAcctgDocItem as a
{
  key    a.CompanyCode,
  key    a.AccountingDocument,
  key    a.FiscalYear,
        a.AdditionalCurrency2,
        @Aggregation.default: #SUM
        @Semantics: { amount : {currencyCode: 'AdditionalCurrency2'} }
         //  @Semantics.amount.currencyCode: ''
         sum(
           a.AmountInCompanyCodeCurrency ) as BardanaDeduction

}
where
     a.FinancialAccountType         = 'K' and 
  ( a.TransactionTypeDetermination = 'WRX'
   or  a.TransactionTypeDetermination = 'BSX'
   or  a.TransactionTypeDetermination = 'PRD' )
//and a.IN_InvoiceReferenceNumber <> ' '
group by
  a.CompanyCode,
  a.AccountingDocument,
  a.FiscalYear,
  a.AdditionalCurrency2
