@AbapCatalog.sqlViewName: 'ZI_SBILLINGAV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sum Billing Data'
define view ZI_SBILLING_ADATA as select from I_BillingDocumentItem as a left outer join I_BillingDocument as b
on a.BillingDocument = b.BillingDocument
{
    key a.SalesDocument,
    key a.SalesDocumentItem,
    @Semantics.currencyCode: true
    a.TransactionCurrency as currency,
    @Semantics.amount.currencyCode: 'currency'
    sum( a.NetAmount ) as BNetAmount
} where a.BillingQuantity > 0 and a.BillingDocumentType != 'S1' and
 a.BillingDocumentType != 'F5'  and a.BillingDocumentType != 'F8' and b.BillingDocumentIsCancelled = ''
 group by a.SalesDocument,a.SalesDocumentItem,a.TransactionCurrency


