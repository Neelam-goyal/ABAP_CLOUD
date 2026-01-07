@AbapCatalog.sqlViewName: 'ZV_DDISP_ETHANOL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Daily dispatch ethanol report'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_DAILY_DISPATCH_ETHANOL
  as select distinct from ZI_DDE_BASE    as base

    left outer join ZI_DDE_SUM_QTY  as sumqtyf2 on  base.Plant                = sumqtyf2.Plant
                                             and base.SoldToParty             = sumqtyf2.SoldToParty
                                             and base.PurchaseOrderByCustomer = sumqtyf2.PurchaseOrderByCustomer

    left outer join ZI_DDE_SUM_QTY_S1 as sumqtys1 on  base.Plant               = sumqtys1.Plant
                                             and base.SoldToParty             = sumqtys1.SoldToParty
                                             and base.PurchaseOrderByCustomer = sumqtys1.PurchaseOrderByCustomer
                                             
    left outer join ZI_DAY_SUM_QTY as daysumf2 on  base.Plant                 = daysumf2.Plant
                                             and base.SoldToParty             = daysumf2.SoldToParty
                                             and base.PurchaseOrderByCustomer = daysumf2.PurchaseOrderByCustomer


    left outer join ZI_DAY_SUM_QTY_S1 as daysumS1 on  base.Plant              = daysumS1.Plant
                                             and base.SoldToParty             = daysumS1.SoldToParty
                                             and base.PurchaseOrderByCustomer = daysumS1.PurchaseOrderByCustomer
{

  key base.Plant,
  key base.SoldToParty,
  key base.PurchaseOrderByCustomer,
      base.CustomerPurchaseOrderDate,
      base.BillingDocumentDate,
      base.Customer,
      base.CustomerName,
      base.CityName,
      sumqtyf2.totbilledqty,
      sumqtys1.totcancelledqty,
      sumqtyf2.soqtyf2,
      sumqtys1.soqtys1,
        
      case base.daycount   
      when '1'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_1,

      case base.daycount  
      when '1'
      then daysumS1.totcancelledqty  
      else 0 end as cancelledqty_1,
      
      case base.daycount  
      when '2'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_2,

      case base.daycount  
      when '2'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_2,
            
      case base.daycount  
      when '3'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_3,

      case base.daycount 
      when '3'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_3,
            
      case base.daycount   
      when '4'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_4,

      case base.daycount  
      when '4'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_4,
      
      case base.daycount  
      when '5'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_5,

      case base.daycount   
      when '5'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_5,
      
      case base.daycount
      when '6'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_6,

      case base.daycount    
      when '6'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_6,
            
      case base.daycount 
      when '7'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_7,

      case base.daycount 
      when '7'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_7,
            
      case base.daycount  
      when '8'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_8,

      case base.daycount   
      when '8'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_8,
      
      case base.daycount   
      when '9'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_9,

      case base.daycount  
      when '9'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_9,
            
      case base.daycount 
      when '10'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_10,            

      case base.daycount
      when '10'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_10,
      
      case base.daycount    
      when '11'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_11,            

      case base.daycount    
      when '11'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_11,

      case base.daycount    
      when '12'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_12,            

      case base.daycount    
      when '12'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_12,

      case base.daycount    
      when '13'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_13,            

      case base.daycount    
      when '13'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_13,
            
      case base.daycount   
      when '14'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_14,            

      case base.daycount     
      when '14'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_14,  

      case base.daycount    
      when '15'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_15,            

      case base.daycount    
      when '15'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_15,

      case base.daycount    
      when '16'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_16,            

      case base.daycount    
      when '16'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_16,

      case base.daycount    
      when '17'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_17,            

      case base.daycount    
      when '17'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_17,

      case base.daycount    
      when '18'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_18,            

      case base.daycount    
      when '18'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_18,

      case base.daycount    
      when '19'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_19,            

      case base.daycount    
      when '19'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_19,

      case base.daycount    
      when '20'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_20,            

      case base.daycount    
      when '20'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_20,

      case base.daycount    
      when '21'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_21,            

      case base.daycount    
      when '21'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_21,
  
                                                    
      case base.daycount    
      when '22'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_22,            

      case base.daycount  
      when '22'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_22,  
     
      case base.daycount    
      when '23'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_23,            

      case base.daycount    
      when '23'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_23,

      case base.daycount    
      when '24'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_24,            

      case base.daycount    
      when '24'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_24,      

      case base.daycount    
      when '25'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_25,            

      case base.daycount    
      when '25'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_25,

      case base.daycount    
      when '26'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_26,            

      case base.daycount    
      when '26'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_26,

      case base.daycount    
      when '27'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_27,            

      case base.daycount    
      when '27'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_27,

      case base.daycount    
      when '28'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_28,            

      case base.daycount    
      when '28'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_28,

      case base.daycount    
      when '29'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_29,            

      case base.daycount    
      when '29'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_29,

      case base.daycount    
      when '30'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_30,            

      case base.daycount    
      when '30'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_30,

      case base.daycount    
      when '31'
      then daysumf2.totbilledqty 
      else 0 end as billedqty_31,            

      case base.daycount    
      when '31'
      then daysumS1.totcancelledqty 
      else 0 end as cancelledqty_31
                                                                                                            
}
//group by
//base.Plant,
//base.SoldToParty,
//base.PurchaseOrderByCustomer,
//base.daycount
