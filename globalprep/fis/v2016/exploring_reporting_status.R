dt_taxon 
dt_data
dt_results



#1. subset the allocation data to year i
data_yearly <- dt_data[Year==2010,]

#2. Now we want it ordered by UniversalDataID
setkey(data_yearly,UniversalDataID)

#3. Get unique UniversalDataID

udi <- unique(data_yearly$UniversalDataID)

#4. Subset results

results_sub <- dt_results[UniversalDataID %in% udi]

setkey(results_sub,UniversalDataID) #sort by UDI



all_data <- results_sub%>%
                left_join(data_yearly)%>%
                left_join(dt_taxon)%>%
                left_join(dt_entity)%>%
                left_join(cells_df_water)%>%
                mutate(catch_prop = AllocatedCatch * area,
                       year = i)%>%
                group_by(rgn_id,fao_id, Scientific_Name, Common_Name, TaxonKey,catch_type_name,reporting_status_name)%>%
                summarise(catch = sum(catch_prop))%>%
                ungroup()%>%
                mutate(year     = 2010,
                       stock_id = gsub(" ", "_", paste(Scientific_Name, fao_id, sep='-'), fixed=TRUE))%>%
                rename(fao_rgn  = fao_id,
                       tons     = catch)


# wahoo example

# The maldives (region 39) has reported and unreported catch. A couple options, calculate total amount of their catch or total of all catch 


wahoo <- all_data%>%
          filter(stock_id=='Acanthocybium_solandri-57')%>%
          group_by(rgn_id)%>%
          mutate(total_catch = sum(tons))%>%
          ungroup()%>%
          group_by(rgn_id,reporting_status_name)%>%
          mutate(catch = sum(tons))%>%
          ungroup()%>%
          mutate(prop_unreported = ifelse(reporting_status_name=='Unreported',catch/total_catch,0))%>%
          dplyr::select(rgn_id,Common_Name,reporting_status_name,catch,stock_id,total_catch,prop_unreported)


bato <- all_data%>%
            filter(stock_id=='Batoidea-57')%>%
            group_by(rgn_id)%>%
            mutate(total_catch = sum(tons))%>%
            ungroup()%>%
            group_by(rgn_id,reporting_status_name)%>%
            mutate(catch = sum(tons))%>%
            ungroup()%>%
            mutate(prop_unreported = ifelse(reporting_status_name=='Unreported',catch/total_catch,0))%>%
            dplyr::select(rgn_id,Common_Name,reporting_status_name,catch,stock_id,total_catch,prop_unreported)


cni <- all_data%>%
          filter(stock_id=='Cnidaria-27')%>%
          group_by(rgn_id)%>%
          mutate(total_catch = sum(tons))%>%
          ungroup()%>%
          group_by(rgn_id,reporting_status_name)%>%
          mutate(catch = sum(tons))%>%
          ungroup()%>%
          mutate(prop_unreported = ifelse(reporting_status_name=='Unreported',catch/total_catch,0))%>%
          dplyr::select(rgn_id,Common_Name,reporting_status_name,catch,stock_id,total_catch,prop_unreported)


misc <- all_data%>%
          filter(TaxonKey<100340)%>%
          group_by(stock_id)%>%
            mutate(total_catch = sum(tons))%>%
            ungroup()%>%
            group_by(stock_id,reporting_status_name)%>%
            mutate(catch = sum(tons))%>%
            ungroup()%>%
            mutate(prop_unreported = ifelse(reporting_status_name=='Unreported',catch/total_catch,0))%>%
            dplyr::select(Common_Name,reporting_status_name,catch,stock_id,total_catch,prop_unreported)%>%
            distinct()

#plot proportion catch unreported (marine fishes nei)

p <- misc%>%
      filter(Common_Name == 'Marine fishes nei' & reporting_status_name == 'Unreported')%>%
      dplyr::arrange(desc(prop_unreported))

ggplot(p,aes(prop_unreported))+
    geom_histogram(bins=7)


#debugging
misc <- all_data%>%
  filter(stock_id == 'Miscellaneous_aquatic_invertebrates-57')%>%
  group_by(rgn_id,stock_id)%>%
  mutate(total_catch = sum(tons))%>%
  ungroup()%>%
  group_by(rgn_id,stock_id,reporting_status_name)%>%
  mutate(catch = sum(tons))%>%
  ungroup()%>%
  mutate(prop_unreported = ifelse(reporting_status_name=='Unreported',catch/total_catch,0))%>%
  dplyr::select(rgn_id,Common_Name,reporting_status_name,catch,stock_id,total_catch,prop_unreported)%>%
  distinct()


