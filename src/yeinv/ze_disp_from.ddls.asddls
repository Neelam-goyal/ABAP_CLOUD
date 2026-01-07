@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View entity'
@Metadata.allowExtensions: true
define root view entity ZE_DISP_FROM
  as select from zsd_disp_from as disp
{

  key disp.storagelocation,
      disp.dispatchfrmname,
      disp.dispatchfrmaddr1,
      disp.dispatchfrmaddr2,
      disp.dispatchfrmloc,
      disp.dispatchfrmpin,
      disp.dispatchfrmstcd

}
