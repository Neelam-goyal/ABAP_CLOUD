@AbapCatalog.sqlViewName: 'ZV_TAX_INV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tax Invoice data'
define view ZI_PRINT_TAX_INV 
as select from I_BillingDocument as bl
{
    key bl.BillingDocument,
    bl.BillingDocumentType,
    bl.BillingDocumentDate,
    bl.DistributionChannel,
    bl.SoldToParty

}
where bl.BillingDocumentType = 'F2' 
and 
( bl.DistributionChannel = '10' or 
  bl.DistributionChannel = '30' or
  bl.DistributionChannel = '40' or 
  bl.DistributionChannel = '50' or 
  bl.DistributionChannel = '60' 
  )
  and bl.BillingDocumentIsCancelled = ''
