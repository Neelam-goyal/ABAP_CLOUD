@AbapCatalog.sqlViewName: 'ZI_BILLINGV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Billing data with month'
define view ZI_BILLING_DATA
  as select from    I_BillingDocumentItem as a
    left outer join I_BillingDocument     as b on a.BillingDocument = b.BillingDocument
{
  key a.SalesDocument,
  key a.SalesDocumentItem,
  key a.BillingDocument,
  key a.BillingDocumentItem,
      substring(b.BillingDocumentDate,5,2) as Mon,
      substring(b.BillingDocumentDate,1,4) as yyear,
      @Semantics.unitOfMeasure: true
      a.BillingQuantityUnit,
      @Semantics.quantity.unitOfMeasure: 'BillingQuantityUnit'
      a.BillingQuantity


}
where
      a.BillingQuantity            >  0
  and a.BillingDocumentType        != 'S1'
  and b.BillingDocumentIsCancelled =  ''
