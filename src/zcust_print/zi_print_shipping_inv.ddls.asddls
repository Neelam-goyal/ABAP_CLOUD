@AbapCatalog.sqlViewName: 'ZV_SHIP_INV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Shipping inv print'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_PRINT_SHIPPING_INV 
as select from I_BillingDocument as bl
{
    key bl.BillingDocument,
    bl.BillingDocumentType,
    bl.BillingDocumentDate,
    bl.DistributionChannel,
    bl.SoldToParty

}
where ( bl.BillingDocumentType = 'F5' or 
        
        bl.BillingDocumentType = 'JVR' )
and 
( 
  bl.DistributionChannel = '20' 
 )
  
and bl.BillingDocumentIsCancelled = ''
