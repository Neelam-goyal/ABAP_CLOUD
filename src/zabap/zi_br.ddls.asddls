@AbapCatalog.sqlViewName: 'ZI_BRN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'deduction'
@Analytics.dataCategory: #CUBE
define view ZI_br
  as select from I_OperationalAcctgDocItem as a
{
  key    a.CompanyCode,
  key    a.AccountingDocument,
  key    a.FiscalYear,
           @Semantics.amount.currencyCode: ''
     //    @DefaultAggregation: #SUM
         sum(
           a.AmountInCompanyCodeCurrency ) as Deduction
}
where
   a.FinancialAccountType    = 'K' and
   a.GLAccount               = '0000301015'
  or  a.GLAccount            = '0000301010'
  or  a.GLAccount            = '0000301011'
  or  a.GLAccount            = '0000301012'
  or  a.GLAccount            = '0000301013'
  or  a.GLAccount            = '0000301014'
  or  a.GLAccount            = '0000301001'
  or  a.GLAccount            = '0000407025'
//  and a.InvoiceReference <> ''
group by
  a.CompanyCode,
  a.AccountingDocument,
  a.FiscalYear
