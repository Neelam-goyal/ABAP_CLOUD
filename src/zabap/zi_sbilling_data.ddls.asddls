@AbapCatalog.sqlViewName: 'ZI_SBILLINGV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sum Billing Data'
define view ZI_SBILLING_DATA as select from I_BillingDocumentItem as a left outer join I_BillingDocument as b
on a.BillingDocument = b.BillingDocument
{
    key a.SalesDocument,
    key a.SalesDocumentItem,
    @Semantics.unitOfMeasure: true
    a.BillingQuantityUnit,
    @Semantics.quantity.unitOfMeasure: 'BillingQuantityUnit'
    sum( a.BillingQuantity )  as BillingQty
} where a.BillingQuantity > 0 and a.BillingDocumentType != 'S1' and
 a.BillingDocumentType != 'F5'  and a.BillingDocumentType != 'F8'    and b.BillingDocumentIsCancelled = ''
 group by a.SalesDocument,a.SalesDocumentItem,a.BillingQuantityUnit
