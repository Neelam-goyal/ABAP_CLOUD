@AbapCatalog.sqlViewName: 'ZV_STOCK_REP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exceed stock report'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_EXCEED_STOCK_REP as select from I_ProductPlantBasic as marc

left outer join ZI_MATERIAL_STOCK_SLOC as mard
on mard.Material = marc.Product and mard.Plant = marc.Plant

left outer join I_Product as product
on  mard.Material = product.Product

left outer join ZE_MIN_STOCK as safety
on  mard.Material = safety.material and mard.Plant = safety.plant

{
  
  @EndUserText.label: 'Material Code'
  key marc.Product,
  @EndUserText.label: 'Plant'
  key marc.Plant,
  @EndUserText.label: 'Storage Location'
  key mard.StorageLocation,
  
 //@EndUserText.label: 'Unrestricted Stock'
  mard.COMP_STOCK,

  @EndUserText.label: 'Safety Stock'
  safety.safetystock
            
   
}
