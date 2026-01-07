@AbapCatalog.sqlViewName: 'ZV_DLV_CHLN '
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Delivery Challan'
define view ZI_PRINT_DLV_CHLN 
as select from I_BillingDocument as bl
{
    key bl.BillingDocument,
    bl.BillingDocumentType,
    bl.BillingDocumentDate,
    bl.DistributionChannel,
    bl.SoldToParty

}
where ( bl.BillingDocumentType = 'JDC' or 
        bl.BillingDocumentType = 'JSN' or 
        bl.BillingDocumentType = 'F8' or 
        bl.BillingDocumentType = 'JVR' )
and 
( 
  bl.DistributionChannel = '50' 
 )
  
and bl.BillingDocumentIsCancelled = ''
