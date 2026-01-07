@AbapCatalog.sqlViewName: 'ZV_VENDOR_CODE'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Vendor code'
@Metadata.allowExtensions: true
define root view ZI_VENDOR_CODE as select from zsd_vendor_code as ven
{
 
 key ven.customer,
 key ven.plant,
     ven.vendor
    
}
