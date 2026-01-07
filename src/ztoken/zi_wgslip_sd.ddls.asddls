@AbapCatalog.sqlViewName: 'ZV_WGSLIP_SD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SD weigh bridge slip'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_WGSLIP_SD 
  as select from zsd_gout_data as gout
{

  key gout.gentry_num         as GentryNum,
  key gout.gentry_year        as GentryYear,
  key gout.saledocument       as Saledocument,
      gout.erdat              as Erdat
    
}
