@AbapCatalog.sqlViewName: 'ZV_SUM_CLR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sum cleared line item'
define view ZI_SUM_CLR 

 with parameters
    @Environment.systemField: #SYSTEM_DATE
    P_KeyDate : sydate
    
as select from ZI_OL_BASE(P_KeyDate : $parameters.P_KeyDate) as base 
left outer join I_OperationalAcctgDocItem as op
on op.AccountingDocument = base.ClearingJournalEntry and
   op.FiscalYear         = base.ClearingJournalEntryFiscalYear
{

  key op.CompanyCode,
  key op.AccountingDocument,
  key op.FiscalYear,
  key op.AccountingDocumentItem,
  sum(op.AmountInCompanyCodeCurrency) as clramount
     
}
group by
op.CompanyCode,
op.AccountingDocument,
op.FiscalYear,
op.AccountingDocumentItem
