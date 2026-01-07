@AbapCatalog.sqlViewName: 'ZV_AFWI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'AFWI Data'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_AFWI as select from I_MfgOrderDocdGoodsMovement as mseg
left outer join I_MfgOrderConfirmation as conf
on mseg.ManufacturingOrder = conf.ManufacturingOrder
{

      key mseg.GoodsMovement,
      key mseg.GoodsMovementYear,
      key mseg.Batch,
      key conf.MfgOrderConfirmation,    
      conf.MfgOrderConfirmationGroup,  
      mseg.GoodsMovementItem,
      mseg.ManufacturingOrder,
      mseg.GoodsMovementType,
      mseg.BaseUnit,
      mseg.GoodsMovementRefDocType,
      mseg.PostingDate,
      mseg.QuantityInBaseUnit,
      conf.MaterialDocument,
      conf.MaterialDocumentYear
          
} where mseg.ManufacturingOrder = '000001000340'
