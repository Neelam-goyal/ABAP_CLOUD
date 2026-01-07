@AbapCatalog.sqlViewName: 'ZV_BOM_COMP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BOM Component'
define view ZI_BOM_COMP 
with parameters
@Environment.systemField: #SYSTEM_DATE
P_KeyDate : sydate
    
as select from I_BillOfMaterialItemBasic as bcomp
left outer join ZI_BOM_HEADER(P_KeyDate: $parameters.P_KeyDate) as BHDR
on BHDR.BillOfMaterial = bcomp.BillOfMaterial

left outer join I_Product as mara
on mara.Product = bcomp.BillOfMaterialComponent 

left outer join I_ProductText as makt
on makt.Product = bcomp.BillOfMaterialComponent and makt.Language = 'E'

left outer join ZI_MATERIAL_STOCK as mard
on mard.Material = bcomp.BillOfMaterialComponent and mard.Plant = BHDR.Plant

//left outer join I_ProductStorageLocationBasic as ploc
//on ploc.Product = bcomp.BillOfMaterialComponent and ploc.Plant = BHDR.Plant
{
 
     key BHDR.Plant,
     key BHDR.Material,
     key BHDR.BillOfMaterial,
     key bcomp.BillOfMaterialComponent,
     BHDR.ProductName,
     BHDR.BillOfMaterialCategory,
     BHDR.BOMExplosionApplication,
     BHDR.HeaderValidityStartDate,
     BHDR.HeaderValidityEndDate,
     BHDR.BOMHeaderBaseUnit,
     BHDR.BOMHeaderQuantityInBaseUnit,
     BHDR.ProductType,
     BHDR.ProductGroup,
     BHDR.BaseUnit,  
     
     bcomp.BillOfMaterialItemCategory,
     bcomp.BillOfMaterialItemNumber,
     bcomp.BillOfMaterialItemUnit,
     bcomp.BillOfMaterialItemQuantity,
     bcomp.IsProductionRelevant,
     bcomp.BOMItemIsCostingRelevant,
     bcomp.ProdOrderIssueLocation,
     
//   ploc.StorageLocation,
     
     mara.ProductType  as comp_ProductType,
     mara.ProductGroup as comp_ProductGroup,
     mara.ExternalProductGroup as comp_ExtProductGroup,
     mara.BaseUnit as comp_BaseUnit,
     
     makt.ProductName as COMP_MAT_NAME,
     
     mard.COMP_STOCK
           
}
