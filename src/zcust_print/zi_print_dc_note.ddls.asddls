@AbapCatalog.sqlViewName: 'ZV_DC_NOTE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SD debit/credit note'
define view ZI_PRINT_DC_NOTE 
as select from I_BillingDocument as bl
{
    key bl.BillingDocument,
    bl.BillingDocumentType,
    bl.BillingDocumentDate,
    bl.DistributionChannel,
    bl.SoldToParty

}
where ( bl.BillingDocumentType = 'G2' or 
        bl.BillingDocumentType = 'CBRE' or 
        bl.BillingDocumentType = 'L2' )
and 
( bl.DistributionChannel = '10' or 
  bl.DistributionChannel = '20' or 
  bl.DistributionChannel = '30' or 
  bl.DistributionChannel = '40' or 
  bl.DistributionChannel = '50' or 
  bl.DistributionChannel = '60' )
  
and bl.BillingDocumentIsCancelled = ''
