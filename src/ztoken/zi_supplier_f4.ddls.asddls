@AbapCatalog.sqlViewName: 'ZV_SUPPLIER_F4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Supplier F4'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SUPPLIER_F4 as select from    I_Supplier  as lfa1
    left outer join I_Address_2 as adrc on lfa1.AddressID = adrc.AddressID
{

  key lfa1.Supplier,
      lfa1.SupplierName,
      lfa1.SupplierFullName,
      lfa1.TaxNumber3,
      lfa1.BusinessPartnerPanNumber,
      adrc.CityName

}
