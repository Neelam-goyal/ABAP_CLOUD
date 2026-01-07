@AbapCatalog.sqlViewName: 'ZV_PARTNER_ADRS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Partner Address'
define view ZI_PARTNER_ADDRESS as select from I_BillingDocumentPartner as BP
left outer join I_Address_2 as adrs on adrs.AddressID  = BP.AddressID


// I_BillingDocumentPartner

left outer join I_BillingDocItemPartner as bp_item on bp_item.Customer = BP.Customer
left outer join I_Address_2 as adrs_i on adrs_i.AddressID  = bp_item.AddressID

left outer join I_Businesspartnertaxnumber as tax on tax.BusinessPartner  = BP.Customer
left outer join I_Businesspartnertaxnumber as tax_i on tax_i.BusinessPartner  = bp_item.Customer



left outer join I_BillingDocumentItem     as b_itm on b_itm.BillingDocument  = BP.BillingDocument

left outer join I_SalesDocumentPartner  as so_partner on so_partner.SalesDocument  = b_itm.SalesDocument 
                                  and so_partner.PartnerFunction = 'WE'

left outer join I_Address_2 as adrs_i_so on adrs_i_so.AddressID  = so_partner.AddressID

left outer join I_SalesDocumentPartner  as so_partner_es on so_partner_es.SalesDocument  = b_itm.SalesDocument 
                                  and so_partner_es.PartnerFunction = 'ES'
                                  
left outer join I_SalesDocumentPartner as so_partner_PS on so_partner_es.SalesDocument  = b_itm.SalesDocument 
                                  and so_partner_es.PartnerFunction = 'PS'                                  

left outer join I_Address_2 as adrs_i_so_es on adrs_i_so_es.AddressID  = so_partner_es.AddressID

left outer join I_Businesspartnertaxnumber as tax_so on tax_so.BusinessPartner  = so_partner.Customer
left outer join I_Businesspartnertaxnumber as tax_i_so on tax_i_so.BusinessPartner  = so_partner.Customer
left outer join I_Businesspartnertaxnumber as tax_i_so_es on tax_i_so_es.BusinessPartner  = so_partner.Customer

//I_SalesDocumentPartner

{
  key BP.BillingDocument,
   
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.AddresseeFullName end )  as WE_NAME ,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.CityName end )  as WE_CITY ,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.PostalCode end )  as WE_PIN ,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.Street end )  as WE_STREET ,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.StreetName end )  as WE_STREET1 ,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.HouseNumber end )  as WE_HOUSE_NO ,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.Country end )  as WE_COUNTRY ,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.Region end )  as WE_REGION ,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.FormOfAddress end )  as WE_FROM_OF_ADD,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so._EmailAddress.EmailAddress end )  as WE_EMAIL,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so._PhoneNumber.PhoneAreaCodeSubscriberNumber end )  as WE_PHONE4,  
  max( case so_partner.PartnerFunction when 'WE' then tax_i_so.BPTaxNumber end )  as WE_TAX,
  max( case so_partner.PartnerFunction when 'WE' then substring( tax_i_so.BPTaxNumber , 3 , 10 ) end )  as WE_PAN,  
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.StreetPrefixName1 end )  as WE_StreetPrefixName1,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.StreetPrefixName2 end )  as WE_StreetPrefixName2,
  max( case so_partner.PartnerFunction when 'WE' then adrs_i_so.StreetSuffixName1 end )  as WE_StreetSuffixName1,
  max( case so_partner.PartnerFunction when 'WE' then so_partner.Customer end ) as ship_to_party , 

  max( case BP.PartnerFunction when 'AG' then adrs.AddresseeFullName end )  as AG_NAME ,
  max( case BP.PartnerFunction when 'AG' then adrs.CityName end )  as AG_CITY ,
  max( case BP.PartnerFunction when 'AG' then adrs.PostalCode end )  as AG_PIN ,
  max( case BP.PartnerFunction when 'AG' then adrs.Street end )  as AG_STREET ,
  max( case BP.PartnerFunction when 'AG' then adrs.StreetName end )  as AG_STREET1 ,
  max( case BP.PartnerFunction when 'AG' then adrs.HouseNumber end )  as AG_HOUSE_NO ,
  max( case BP.PartnerFunction when 'AG' then adrs.Country end )  as AG_COUNTRY ,
  max( case BP.PartnerFunction when 'AG' then adrs.Region end )  as AG_REGION ,
  max( case BP.PartnerFunction when 'AG' then adrs.FormOfAddress end )  as AG_FROM_OF_ADD,
  max( case BP.PartnerFunction when 'AG' then adrs._EmailAddress.EmailAddress end )  as AG_EMAIL,
  max( case BP.PartnerFunction when 'AG' then adrs._PhoneNumber.PhoneAreaCodeSubscriberNumber end )  as AG_PHONE4,  
  max( case BP.PartnerFunction when 'AG' then tax.BPTaxNumber end )  as AG_TAX,
  max( case BP.PartnerFunction when 'AG' then substring( tax.BPTaxNumber , 3 , 10 ) end )  as AG_PAN, 
  max( case BP.PartnerFunction when 'AG' then adrs.StreetPrefixName1 end )  as AG_StreetPrefixName1,
  max( case BP.PartnerFunction when 'AG' then adrs.StreetPrefixName2 end )  as AG_StreetPrefixName2,
  max( case BP.PartnerFunction when 'AG' then adrs.StreetSuffixName1 end )  as AG_StreetSuffixName1,
  max( case BP.PartnerFunction when 'AG' then BP.Customer end ) as sold_to_party ,

   max( case BP.PartnerFunction when 'RG' then adrs.AddresseeFullName end )  as RG_NAME ,
   max( case BP.PartnerFunction when 'RG' then adrs.CityName end )  as RG_CITY ,
   max( case BP.PartnerFunction when 'RG' then adrs.PostalCode end )  as RG_PIN ,
   max( case BP.PartnerFunction when 'RG' then adrs.Street end )  as RG_STREET ,
   max( case BP.PartnerFunction when 'RG' then adrs.StreetName end )  as RG_STREET1 ,
   max( case BP.PartnerFunction when 'RG' then adrs.HouseNumber end )  as RG_HOUSE_NO ,
   max( case BP.PartnerFunction when 'RG' then adrs.Country end )  as RG_COUNTRY ,
   max( case BP.PartnerFunction when 'RG' then adrs.Region end )  as RG_REGION ,
   max( case BP.PartnerFunction when 'RG' then adrs.FormOfAddress end )  as RG_FROM_OF_ADD ,
   max( case BP.PartnerFunction when 'RG' then adrs._EmailAddress.EmailAddress end )  as RG_EMAIL,
   max( case BP.PartnerFunction when 'RG' then adrs._PhoneNumber.PhoneAreaCodeSubscriberNumber end )  as RG_PHONE4,
   max( case BP.PartnerFunction when 'RG' then tax.BPTaxNumber end )  as RG_TAX,
   max( case BP.PartnerFunction when 'RG' then substring( tax.BPTaxNumber , 3 , 10 ) end )  as RG_PAN, 
   max( case BP.PartnerFunction when 'RG' then adrs.StreetPrefixName1 end )  as RG_StreetPrefixName1,
   max( case BP.PartnerFunction when 'RG' then adrs.StreetPrefixName2 end )  as RG_StreetPrefixName2,
   max( case BP.PartnerFunction when 'RG' then adrs.StreetSuffixName1 end )  as RG_StreetSuffixName1,   
   max( case BP.PartnerFunction when 'RG' then BP.Customer end ) as payer_code ,
   
   max( case BP.PartnerFunction when 'RE' then adrs.AddresseeFullName end )  as RE_NAME ,
   max( case BP.PartnerFunction when 'RE' then adrs.CityName end )  as RE_CITY ,
   max( case BP.PartnerFunction when 'RE' then adrs.PostalCode end )  as RE_PIN ,
   max( case BP.PartnerFunction when 'RE' then adrs.Street end )  as RE_STREET ,
   max( case BP.PartnerFunction when 'RE' then adrs.StreetName end )  as RE_STREET1 ,
   max( case BP.PartnerFunction when 'RE' then adrs.HouseNumber end )  as RE_HOUSE_NO ,
   max( case BP.PartnerFunction when 'RE' then adrs.Country end )  as RE_COUNTRY ,
   max( case BP.PartnerFunction when 'RE' then adrs.Region end )  as RE_REGION ,
   max( case BP.PartnerFunction when 'RE' then adrs.FormOfAddress end )  as RE_FROM_OF_ADD , 
   max( case BP.PartnerFunction when 'RE' then adrs._EmailAddress.EmailAddress end )  as RE_EMAIL , 
   max( case BP.PartnerFunction when 'RE' then adrs._PhoneNumber.PhoneAreaCodeSubscriberNumber end )  as RE_PHONE4,
   max( case BP.PartnerFunction when 'RE' then tax.BPTaxNumber end )  as RE_TAX,
   max( case BP.PartnerFunction when 'RE' then substring( tax.BPTaxNumber , 3 , 10 ) end )  as RE_PAN,
   max( case BP.PartnerFunction when 'RE' then adrs.StreetPrefixName1 end )  as RE_StreetPrefixName1,
   max( case BP.PartnerFunction when 'RE' then adrs.StreetPrefixName2 end )  as RE_StreetPrefixName2,
   max( case BP.PartnerFunction when 'RE' then adrs.StreetSuffixName1 end )  as RE_StreetSuffixName1,    
   max( case BP.PartnerFunction when 'RE' then BP.Customer end ) as bill_to_party,

   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.AddresseeFullName end )  as ES_NAME ,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.CityName end )  as ES_CITY ,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.PostalCode end )  as ES_PIN ,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.Street end )  as ES_STREET ,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.StreetName end )  as ES_STREET1 ,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.HouseNumber end )  as ES_HOUSE_NO ,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.Country end )  as ES_COUNTRY ,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.Region end )  as ES_REGION ,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.FormOfAddress end )  as ES_FROM_OF_ADD ,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es._EmailAddress.EmailAddress end )  as ES_EMAIL,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es._PhoneNumber.PhoneAreaCodeSubscriberNumber end )  as ES_PHONE4,
   max( case so_partner_es.PartnerFunction when 'ES' then tax_i_so_es.BPTaxNumber end )  as ES_TAX,
   max( case so_partner_es.PartnerFunction when 'ES' then substring( tax_i_so_es.BPTaxNumber , 3 , 10 ) end )  as ES_PAN, 
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.StreetPrefixName1 end )  as ES_StreetPrefixName1,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.StreetPrefixName2 end )  as ES_StreetPrefixName2,
   max( case so_partner_es.PartnerFunction when 'ES' then adrs_i_so_es.StreetSuffixName1 end )  as ES_StreetSuffixName1,   
   max( case so_partner_es.PartnerFunction when 'ES' then so_partner_es.Customer end ) as ES_code ,
   
   max( case BP.PartnerFunction when 'SP' then adrs.AddresseeFullName end )  as SP_NAME ,
   max( case BP.PartnerFunction when 'SP' then adrs.CityName end )  as SP_CITY ,
   max( case BP.PartnerFunction when 'SP' then adrs.PostalCode end )  as SP_PIN ,
   max( case BP.PartnerFunction when 'SP' then adrs.Street end )  as SP_STREET ,
   max( case BP.PartnerFunction when 'SP' then adrs.StreetName end )  as SP_STREET1 ,
   max( case BP.PartnerFunction when 'SP' then adrs.HouseNumber end )  as SP_HOUSE_NO ,
   max( case BP.PartnerFunction when 'SP' then adrs.Country end )  as SP_COUNTRY ,
   max( case BP.PartnerFunction when 'SP' then adrs.Region end )  as SP_REGION ,
   max( case BP.PartnerFunction when 'SP' then adrs.FormOfAddress end )  as SP_FROM_OF_ADD ,
   max( case BP.PartnerFunction when 'SP' then adrs._EmailAddress.EmailAddress end )  as SP_EMAIL,
   max( case BP.PartnerFunction when 'SP' then adrs._PhoneNumber.PhoneAreaCodeSubscriberNumber end )  as SP_PHONE4,
   max( case BP.PartnerFunction when 'SP' then tax.BPTaxNumber end )  as SP_TAX,
   max( case BP.PartnerFunction when 'SP' then substring( tax.BPTaxNumber , 3 , 10 ) end )  as SP_PAN, 
   max( case BP.PartnerFunction when 'SP' then adrs.StreetPrefixName1 end )  as SP_StreetPrefixName1,
   max( case BP.PartnerFunction when 'SP' then adrs.StreetPrefixName2 end )  as SP_StreetPrefixName2,
   max( case BP.PartnerFunction when 'SP' then adrs.StreetSuffixName1 end )  as SP_StreetSuffixName1,   
   max( case BP.PartnerFunction when 'SP' then BP.Customer end ) as SP_code 
                 
}   
    group by BP.BillingDocument ,  BP.Customer
    

