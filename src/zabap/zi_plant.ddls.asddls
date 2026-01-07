@AbapCatalog.sqlViewName: 'ZI_PLANTV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Plant Data'
define view ZI_PLANT as select from I_Plant as A left outer join  I_Address_2
as B on A.AddressID = B.AddressID and B.AddressPersonID = '' and B.AddressRepresentationCode = ''
left outer join I_CountryText as c on B.Country = c.Country and c.Language = 'E'
{
  key A.Plant,
  B.AddressID,
  B.AddressPersonID,
  B.AddressRepresentationCode,
  A.PlantName,
  B.CityName,
  B.Street,
  B.StreetName,
  B.Country,
  B.Region,
  c.CountryName
  
}
