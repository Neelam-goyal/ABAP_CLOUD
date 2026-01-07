@AbapCatalog.sqlViewName: 'ZV_BILL_SUPPLY'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bill of supply'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_BILL_SUPPLY 
as select from I_BillingDocument as bl
{
    key bl.BillingDocument,
    bl.BillingDocumentType,
    bl.BillingDocumentDate,
    bl.DistributionChannel,
    bl.SoldToParty

}
where bl.BillingDocumentIsCancelled = ''

//where bl.BillingDocumentType = 'F2' 
//and 
//( bl.DistributionChannel = '10' or 
//  bl.DistributionChannel = '30' or
//  bl.DistributionChannel = '40' or 
//  bl.DistributionChannel = '50' or 
//  bl.DistributionChannel = '60' 
//  )
//  and bl.BillingDocumentIsCancelled = ''
