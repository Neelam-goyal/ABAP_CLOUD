@EndUserText.label: 'Material Weight Maintain Singleton'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.semanticKey: [ 'SingletonID' ]
@UI: {
  headerInfo: {
    typeName: 'MaterialWeightMaAll'
  }
}
define root view entity ZI_MaterialWeightMaint_S
  as select from I_Language
    left outer join ZMAT_WT_DATA on 0 = 0
  association [0..*] to I_ABAPTransportRequestText as _ABAPTransportRequestText on $projection.TransportRequestID = _ABAPTransportRequestText.TransportRequestID
  composition [0..*] of ZI_MaterialWeightMaint as _MaterialWeightMaint
{
  @UI.facet: [ {
    id: 'ZI_MaterialWeightMaint', 
    purpose: #STANDARD, 
    type: #LINEITEM_REFERENCE, 
    label: 'Material Weight Maintain', 
    position: 1 , 
    targetElement: '_MaterialWeightMaint'
  } ]
  @UI.lineItem: [ {
    position: 1 
  } ]
  key 1 as SingletonID,
  _MaterialWeightMaint,
  @UI.hidden: true
  max( ZMAT_WT_DATA.LAST_CHANGE_AT ) as LastChangedAtMax,
  @ObjectModel.text.association: '_ABAPTransportRequestText'
  @UI.identification: [ {
    position: 2 , 
    type: #WITH_INTENT_BASED_NAVIGATION, 
    semanticObjectAction: 'manage'
  } ]
  @Consumption.semanticObject: 'CustomizingTransport'
  cast( '' as SXCO_TRANSPORT) as TransportRequestID,
  _ABAPTransportRequestText
  
}
where I_Language.Language = $session.system_language
