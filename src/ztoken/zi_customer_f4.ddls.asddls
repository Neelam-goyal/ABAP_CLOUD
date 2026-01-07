@AbapCatalog.sqlViewName: 'ZV_CUSTOMER_F4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer F4'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_CUSTOMER_F4 
as select from    I_Customer  as kna1
left outer join I_Address_2 as adrc on kna1.AddressID = adrc.AddressID
{

  key kna1.Customer,
      kna1.CustomerName,
      kna1.CustomerFullName,
      adrc.CityName

}
