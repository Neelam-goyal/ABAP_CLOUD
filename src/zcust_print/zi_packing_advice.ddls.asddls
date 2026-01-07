@AbapCatalog.sqlViewName: 'ZPACK_ADVICE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Packing Advice Print'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_PACKING_ADVICE
  as select distinct from zsd_pack_adv as pack_adv
{
  key pack_adv.packadvnum as pack_num,
  key pack_adv.saleorderno,
  key pack_adv.erdate
}
