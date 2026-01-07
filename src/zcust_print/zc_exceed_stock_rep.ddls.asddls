@AbapCatalog.sqlViewName: 'ZV_EXCEED_REP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumpson view for exceed stock sreport'
@Metadata.ignorePropagatedAnnotations: true
define view ZC_EXCEED_STOCK_REP
  as select from ZI_EXCEED_STOCK_REP
{

  key Product,
  key Plant,
      StorageLocation,
      safetystock,
//    colorstats,
      
      @UI.lineItem: [{ position: 40, label: 'Unrestricted Stock', 
                       criticality: 'col_color', 
                       criticalityRepresentation : #WITHOUT_ICON  }]
      @UI.fieldGroup: [{ qualifier: 'Xinfo', position: 10 }]
//    @UI.lineItem: [{ position: 40 , label: 'Unrestricted Stock', type: #AS_DATAPOINT}]
//    @UI.dataPoint: { visualization: #PROGRESS ,
//                   targetValueElement: 'COMP_STOCK' ,
//                   criticality: 'col_color'}
      COMP_STOCK,

     
      // map productive status to criticality value
      @Consumption.filter.hidden: true
      case 
      when COMP_STOCK > safetystock then 1
      when COMP_STOCK < safetystock then 3
      else 2
      end as col_color

}
