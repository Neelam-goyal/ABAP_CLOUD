@AbapCatalog.sqlViewName: 'ZV_BOM_REPORT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BOM Report'

define root view ZI_BOM_REPORT 
with parameters
@Environment.systemField: #SYSTEM_DATE
P_KeyDate : sydate
as select from ZI_BOM_COMP (P_KeyDate: $parameters.P_KeyDate) as BCOMP
{

    key BCOMP.Plant,
    @EndUserText.label: 'Header Material'
    key BCOMP.Material,
    key BCOMP.BillOfMaterial,
    key BCOMP.BillOfMaterialComponent,
    BCOMP.Material as EXPLOD_MATNR,
    BCOMP.ProductType,
    @EndUserText.label: 'Header Material Description'
    BCOMP.ProductName,
    BCOMP.BillOfMaterialCategory,
    BCOMP.BOMExplosionApplication,
    BCOMP.HeaderValidityStartDate,
    BCOMP.HeaderValidityEndDate,
    BCOMP.BOMHeaderBaseUnit,
    BCOMP.BOMHeaderQuantityInBaseUnit,
    BCOMP.BillOfMaterialItemCategory,
    BCOMP.BillOfMaterialItemNumber,
    BCOMP.BillOfMaterialItemUnit,
    BCOMP.BillOfMaterialItemQuantity,
    BCOMP.IsProductionRelevant,
    BCOMP.BOMItemIsCostingRelevant, 
    @EndUserText.label: 'Component Mat. Type'
    BCOMP.comp_ProductType,
    BCOMP.comp_ProductGroup,
    @EndUserText.label: 'Product Family'
    BCOMP.comp_ExtProductGroup,
    BCOMP.comp_BaseUnit,
    @EndUserText.label: 'Component Material Description'
    BCOMP.COMP_MAT_NAME,
    @EndUserText.label: 'Stock at Plant'
    BCOMP.COMP_STOCK,
    //BCOMP.StorageLocation,
    BCOMP.ProdOrderIssueLocation,
    case
    when ( BCOMP.BillOfMaterialItemQuantity is not null and 
           BCOMP.BOMHeaderQuantityInBaseUnit is not null )
    then division( BCOMP.BillOfMaterialItemQuantity, BCOMP.BOMHeaderQuantityInBaseUnit, 2 )    
    end as bom_qpa
         
}
