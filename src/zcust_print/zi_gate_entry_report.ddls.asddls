@AbapCatalog.sqlViewName: 'ZV_GE_REPORT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Gate entry report'
define view ZI_GATE_ENTRY_REPORT
  as select from    zmm_ge_data     as ge
    left outer join ZI_MIGO_MIRO    as preg   on  preg.MigoNumber             = ge.mblnr
                                              and preg.MigoYear               = ge.mjahr
                                              and preg.PurchasingDocument     = ge.ponum
                                              and preg.PurchasingDocumentItem = ge.poitem
    left outer join ZI_INSP_LOT_REJ as ud     on  ud.MaterialDocument       = ge.mblnr
                                              and ud.MaterialDocumentYear   = ge.mjahr
                                              and ud.PurchasingDocument     = ge.ponum
                                              and ud.PurchasingDocumentItem = ge.poitem
  //  inner join ZI_TOKEN_GE as getok on  ge.gentry_num  = getok.gentry_num
    left outer join ZI_TOKEN_GE     as getok  on  ge.gentry_num  = getok.gentry_num
                                              and ge.gentry_year = getok.gentry_year
                                              and ge.ponum       = getok.ponum
                                              and ge.poitem      = getok.poitem
  //   addedby neelam
    left outer join ZI_GE_DATA      as getok1 on  ge.gentry_num  = getok1.gentry_num
                                              and ge.gentry_year = getok1.gentry_year
                                              and ge.ponum       = getok1.ponum
                                              and ge.poitem      = getok1.poitem
{

  key ge.gentry_num,
  key ge.gentry_year,
  key ge.ponum,
  key ge.poitem,
      ge.mblnr,
      ge.mjahr,
      ge.created_on,
      ge.created_time,
      ge.out_date,
      ge.out_time,
      ge.in_date,
      ge.in_time,
      ge.werks,
      ge.lifnr,
      ge.billnum,
      ge.billdate,
      ge.ewaybill_num,
      ge.vechnum,
      ge.transporter,
      ge.driver_name,
      ge.driver_num,
      ge.trans_mode,
      ge.lr_num,
      ge.lr_date,
      ge.gross_wgt,
      ge.tare_wgt,
      ge.net_wgt,
      getok.tokennum,
      getok.bagtype,
      getok.supp_loadwht,
      getok.whmt_charges,
      getok.invqtybag,
      getok.bardanawht,
      getok.supervisor,
      getok.contractor,
      getok.noofbag,
      getok.totalbardana,
      getok.totaldeduc,
      getok.deduction,
      getok.actualqty,
      ge.weight_uom,
      ge.check_rc,
      ge.check_pollt,
      ge.check_tripal,
      ge.check_insur,
      ge.check_dl,
      ge.uname,
      ge.matnr,
      ge.maktx,
      ge.itemdesc,
      ge.docdate,
      ge.poqty,
      ge.uom,
      ge.ovrtol,
      ge.netprice,
      ge.currcy,
      ge.perqty,
      ge.openqty,
      ge.valtyp,
      ge.challnqty,
      ge.delnoteqty,
      ge.sloc,
      ge.mweight,
      ge.erdat,
      ge.uzeit,
      ge.cuname,
      preg.MiroNumber,
      preg.MiroYear,
      preg.MiroQty,
      preg.TaxableValue,
      preg.PostingDate,
      preg.InvoiceValue,
      ud.InspLotQtyToFree,
      ud.InspLotQtyToBlocked,
      ud.InspectionLotUsageDecisionCode,
      ud.InspectionLotHasUsageDecision


}
where
  ge.gedeleted = ''
