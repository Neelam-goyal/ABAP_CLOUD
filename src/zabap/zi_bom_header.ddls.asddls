@AbapCatalog.sqlViewName: 'ZV_BOM_HEADER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BOM Header'
define view ZI_BOM_HEADER 
 with parameters
    @Environment.systemField: #SYSTEM_DATE
    P_KeyDate : sydate
as select from I_MaterialBOMLink as BHDR
left outer join I_BillOfMaterialWithKeyDate(P_KeyDate: $parameters.P_KeyDate) as BHKDATE 
on BHKDATE.BillOfMaterial = BHDR.BillOfMaterial
left outer join I_Product as mara
on mara.Product = BHDR.Material
left outer join I_ProductText as makt
on makt.Product = BHDR.Material and makt.Language = 'E'
{
  key BHDR.Plant,
  key BHDR.Material,
      BHDR.BillOfMaterial,
      makt.ProductName,
      BHKDATE.BillOfMaterialCategory,
      BHKDATE.BOMExplosionApplication,
      BHKDATE.HeaderValidityStartDate,
      BHKDATE.HeaderValidityEndDate,      
      BHKDATE.BOMHeaderBaseUnit,
      BHKDATE.BOMHeaderQuantityInBaseUnit,  
      mara.ProductType,
      mara.ProductGroup,
      mara.BaseUnit                 
} 


