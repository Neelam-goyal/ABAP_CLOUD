@AbapCatalog.sqlViewName: 'ZV_AFT_MKT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'After Market'
define view ZI_PRINT_AFT_MKT 
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
( bl.DistributionChannel = '11' or 
  bl.DistributionChannel = '12'
  )
  and bl.BillingDocumentIsCancelled = ''
