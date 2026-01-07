@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View entity'
@Metadata.allowExtensions: true
define root view entity ZE_MIN_STOCK
  as select from zmm_min_stock as mins
{

  key mins.plant,
  key mins.material,
      mins.safetystock,
      mins.storagelocation,
      mins.materialdesc,
      mins.uom,
      mins.createdby,
      mins.createdon
      
}

