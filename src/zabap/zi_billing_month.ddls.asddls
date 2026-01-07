@AbapCatalog.sqlViewName: 'ZI_BILLINGMV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Monthly billing data'
define view ZI_BILLING_MONTH
  as select from ZI_BILLING_DATA
{
  key SalesDocument,
  key SalesDocumentItem,
      Mon,
      @Semantics.unitOfMeasure: true
      BillingQuantityUnit,
      @Semantics.quantity.unitOfMeasure: 'BillingQuantityUnit'
      sum( BillingQuantity ) as BillingQty

} 
group by
  SalesDocument,
  SalesDocumentItem,
  Mon,
  BillingQuantityUnit
