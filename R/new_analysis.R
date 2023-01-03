library(tidyverse)
library(fmsb)
library(corrplot)

data <- qasis


# rename for efficient addressing

data <-
    data %>%
    rename(project_name = `Project Name`) %>%
    rename(project_manager = `Project Manager`) %>%
    rename(site_name = `Site Name`) %>%
    rename(audit_date = `Audit Date`) %>%
    rename(audit_type = `Audit Type`) %>%
    rename(numeric_rating = `Numeric Rating`) %>%
    rename(supplementary_item = `Supplementary Item`) %>% 
    rename(country = Country) %>% 
    rename(customer = Customer) %>% 
    rename(audit = Audit) %>% 
    rename(auditor = Auditor) %>% 
    rename(equipment = Equipment) %>% 
    rename(asp = ASP) %>% 
    rename(category = Category) %>% 
    rename(sub_category = Subcategory) %>% 
    rename(description = Description) %>% 
    rename(rating = Rating)

# look at the categorical data

categorical <- function(x){
    cbind(freq=table(x), percentage=prop.table(table(x))*100)
}

categorical(data$category)
categorical(data$sub_category)
categorical(data$supplementary_item)
categorical(data$rating)
categorical(data$country)
categorical(data$auditor)


#  TODO Kan auditören påverka resultatet?
categorical(data$auditor)
data %>% 
    group_by(auditor) %>% 
    summarize(antal = n()) %>% 
    arrange(-antal) %>% 
    head(n=20) %>% 
    ggplot( aes(x= reorder(auditor, antal), y = antal)) +
    geom_col() +
    coord_flip()

 
#  TODO kan landet påverka resultatet
categorical(data$country)
data %>% 
    group_by(country, site_name) %>% 
    summarize(antal = n()) %>% view
    arrange(-antal) %>% 
    head(n=20) %>% 
    ggplot( aes(x= reorder(country, antal), y = antal)) +
    geom_col() +
    coord_flip()

#  TODO kan typ av utrustning påverka resultatet
categorical(data$equipment )
data %>%
    group_by(equipment) %>%
    summarize(antal = n()) %>%
    arrange(-antal) %>%
    head(n = 20) %>%
    ggplot(aes(x = reorder(equipment, antal), y = antal)) +
    geom_col() +
    coord_flip()

# TODO Investigate faults and equipment correlation
# TODO Investigate country and fault correlation also per equipment
# TODO Is the equimpment fault correlation different in different countries
# TODO Inviestigate the ASP fault correlation, also with respect to Major, Minor and Observations
# TODO Investigate the number of faults found per audit event over time


# Correlation analysis

#  Giving categorical values numbers
    #  This did not turn out so well 
    {
    # Start with convering names to numbers and add to database
    
    eqmt <- 
        data %>% 
        select(equipment) %>% 
        filter( !duplicated(equipment)) %>% 
        mutate(eqmtId = 1:nrow(.))
    
    audit <- 
        data %>% 
        select(auditor) %>% 
        filter( !duplicated(auditor)) %>% 
        mutate(auditId = 1:nrow(.))
    
    cntry <- 
        data %>% 
        select(country) %>% 
        filter( !duplicated(country)) %>% 
        mutate(cntryId = 1:nrow(.))
    
    asp <- 
        data %>% 
        select(asp) %>% 
        filter( !duplicated(asp)) %>% 
        mutate(aspId = 1:nrow(.))
    
    proj <- 
        data %>% 
        select(project_name) %>% 
        filter( !duplicated(project_name)) %>% 
        mutate(projId = 1:nrow(.))
    
    rate <- 
        data %>% 
        select(rating) %>% 
        filter( !duplicated(rating)) %>% 
        mutate(rateId = 1:nrow(.))
    
    rate <- 
        bind_cols( rating = c("Critical", "Major", "Minor", "Observation"), rateId = 1:4)
    
    
    data <-
        data %>%
        full_join(y = eqmt, by = "equipment") %>% 
        full_join( y = audit, by = "auditor") %>% 
        full_join( y = cntry, by = "country") %>% 
        full_join( y = asp, by = "asp") %>% 
        full_join( y = proj, by = "project_name") %>% 
        full_join( y = rate, by = "rating")
        
    
    corrData <- 
        data %>% 
        select(contains("Id"))
    
    
    correlations <- cor(corrData)
    corrplot(correlations, method="circle")
    
    # remove the ones that seem correlated
    
    corrData <- 
        corrData %>% 
        select(-auditId)
        
    correlations <- cor(corrData)
    corrplot(correlations, method="circle")
    
    corrData <- 
        corrData %>% 
        select(-projId)
    
    correlations <- cor(corrData)
    corrplot(correlations, method="circle")
    
    corrData <- 
        corrData %>% 
        select(-cntryId)
    
    correlations <- cor(corrData)
    corrplot(correlations, method="circle")
    
    
    # Conclusion, Faults are strongest correlated to ASP and ASP is strongest correlated to Country and Project
    
    corrData <- 
        data %>% 
        select(contains("Id")) %>% 
        filter(rateId %in% c( 2, 4)) 
    correlations <- cor(corrData)
    corrplot(correlations, method="circle")
    
    }

# Next up Chi-Square Test of Independence, test between two variables

    #  Version ett av test med Chi-square, visare ingen korrelation
    {
        
        
        data.1 <-
            data %>%
            select(rating, country) %>%
            group_by(country, rating) %>%
            summarize(res = n(), .groups = "drop") %>%
            pivot_wider(names_from = rating, values_from = res, values_fill = 0) %>%
            # mutate(country = stringr::str_replace_all(country, "[:space:]" , "_")) %>%
            as.data.frame() 
        
        rownames(data.1) <-  data.1$country
        
        as.table(data.1)
         
        data.1 %>% 
            select(-country) %>% 
            as.matrix() %>% 
            as.table() %>% 
            chisq.test()
    
    
        vector <- c("country", "equipment", "asp", "auditor")
        pair.Corr <- function(df, var1, var2){
            var1 <- enquo(var1)
            var2 <- enquo(var2)
            print( as.character(var2))
            res <- 
            df %>% 
                select( !!var2, !!var1) %>% 
                table() 
            
            chisq.test(res)
           
        }
    
        
        
        lapply(vector, pair.Corr,  df = data, var1 = "rating")
        pair.Corr(data, rating, country)  
    
    }

    #  now lets change column and rows to see if this alters the values

    #  This does not give any improvement in analysis

    {

    vector <- c("country", "equipment", "asp", "auditor")
    pair.Corr <- function(df, var1, var2){
        var1 <- enquo(var1)
        var2 <- enquo(var2)
        print( as.character(var2))
        res <- 
            df %>% 
            select( !!var2, !!var1) %>% 
            table() 
        
        chisq.test(res)
        
    }
    
    
    
    lapply(vector, pair.Corr,  df = data, var2 = "rating")
    pair.Corr(data, rating, country)  
    }   


#  At this point we can either decide that they are uncorrelated or maybe
#  address som fundamentals of the data. One fundamental is that the dataset
#  covers all countries on the globe. Maybe if we select a country we will get
#  better results

#  results slightly better but the conclusion has to be that they are uncorrelated

    {sweden <- 
    data %>% 
    filter( country == "Sweden" )

lapply(vector, pair.Corr,  df = sweden, var2 = "rating")
}


#  Faults per audit over time

#  this is interesting. We can see countries with good quality, countries with
#  improved Q and countries with random quality

    {data %>% 
    group_by(country, audit, audit_date ) %>% 
    summarize( n = n(), .groups = "drop") %>%
    mutate(scale_n = scale(n)) %>% 
    ggplot( aes(x = audit_date, y = scale_n)) +
    geom_line() +
    facet_wrap(~country)


#  Faults found per ASP over time 

asp_list <- 
    data %>% 
    group_by(asp) %>% 
    summarize(n = n(), .groups= "drop") %>% 
    filter(n > 20) %>% 
    select(asp)

data %>% 
    filter( asp %in% asp_list$asp) %>% 
    group_by(asp, audit_date) %>% 
    summarize(n = n(), .groups = "keep") %>% 
    mutate(asp_avg = mean(n)) %>% 
    ungroup() %>% 
    group_by(asp, audit_date ) %>% 
       arrange(-avg_scale_n) %>%
    ungroup() %>% 
    head(n = nrow(.)/4) %>% 
    ggplot( aes(x = audit_date, y = scale_n)) +
    geom_line() +
    geom_abline(intercept = asp_avg)+
    facet_wrap( ~asp )}

 # How can we visualize progress and what metric is best describing achievements

#  visualize using spider diagram's
#  look at countries and faults

    {
        spider_data <-
    data %>%
    select(country, equipment, rating, category)}

spider_data_prep <- function(spider_data) {
    x <-
        spider_data %>%
        select(rating, country) %>%
        table() %>%
        as_tibble() %>%
        pivot_wider(names_from = rating, values_from = n) %>%
        # scale(center = F) %>%
        bind_rows(summarise_all(., ~ if (is.numeric(.)) max(.) else "Max")) %>%
        bind_rows(summarise_all(., ~ if (is.numeric(.)) min(.) else "Min"))
    
    y1 <- x[1, ]
    y2 <- x[2, ]

    x[1, ] <- x[nrow(x) - 1, ]
    x[2, ] <- x[nrow(x) , ]

    x[nrow(x) - 1, ] <- y1
    x[nrow(x), ] <- y2

    x <-
        x %>%
        column_to_rownames(var = "country")
    x
}
    
        
radarchart(spider_data_prep(spider_data))
}



