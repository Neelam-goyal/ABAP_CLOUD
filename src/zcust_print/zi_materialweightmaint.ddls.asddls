@EndUserText.label: 'Material Weight Maintain'
@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
define view entity ZI_MaterialWeightMaint
  as select from ZMAT_WT_DATA
  association to parent ZI_MaterialWeightMaint_S as _MaterialWeightMaAll on $projection.SingletonID = _MaterialWeightMaAll.SingletonID
{
  key MATERIAL as Material,
  GROSS_WT as GrossWt,
  GROSS_WT_UOM as GrossWtUom,
  NET_WT as NetWt,
  NET_WT_UOM as NetWtUom,
  @Semantics.user.createdBy: true
  LOCAL_CREATED_BY as LocalCreatedBy,
  @Semantics.systemDateTime.createdAt: true
  LOCAL_CREATED_AT as LocalCreatedAt,
  @Consumption.hidden: true
  LOCAL_LAST_CHANGED_BY as LocalLastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  @Consumption.hidden: true
  LOCAL_LAST_CHANGED_AT as LocalLastChangedAt,
  @Semantics.systemDateTime.lastChangedAt: true
  LAST_CHANGE_AT as LastChangeAt,
  @Consumption.hidden: true
  1 as SingletonID,
  _MaterialWeightMaAll
  
}
