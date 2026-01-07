@AbapCatalog.sqlViewName: 'ZV_EXP_TAX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Export Tax Invoice'
define view ZI_PRINT_EXP_TAX_INV 
as select from I_BillingDocument as bl
{
    key bl.BillingDocument,
    bl.BillingDocumentType,
    bl.BillingDocumentDate,
    bl.DistributionChannel,
    bl.SoldToParty

}
where ( bl.BillingDocumentType = 'F2' or 
        bl.BillingDocumentType = 'F8' )
and 
bl.DistributionChannel = '20'
and bl.BillingDocumentIsCancelled = ''

