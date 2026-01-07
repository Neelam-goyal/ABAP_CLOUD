@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View entity'
@Metadata.allowExtensions: true
define root view entity ze_txtid_field
  as select from zsd_txtid_field as txtid
{

  key txtid.documentnumber,
  key txtid.documentitem,
      txtid.buyer_oth_consinee,
      txtid.notify_party1,
      txtid.notify_party2,
      txtid.pre_carig_by,
      txtid.pre_carrier,
      txtid.vessal,
      txtid.port_of_loading,
      txtid.port_of_discharge,
      txtid.final_destination,
      txtid.marks_no,
      cntry_of_fdest
}
