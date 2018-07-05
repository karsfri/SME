# To do -------------------------------------------------------------------

"
Allar tölur yfir á verðlag 2017

Financial ration á atvinnugreinar
sjá keldan: http://www.keldan.is/market/financeref
- Svæði
- Ísat
- Ár
- bil




beta

Dept / Equity


skoða correlation innan svæðis, atvinnugreinar, etc

Bera saman return  við market return



"

library(tidyverse)
library(magrittr)
library(forcats)
library(ggjoy)

# Lesa inn og hreinsa -----------------------------------------------------

# relevel

relevel_bil <- function(x) {
    x  %>% mutate(BIL = fct_relevel(BIL, "0-10", "10-50", "50-100", "100-250", "250-500", "500-1000", ">1000")) %>% 
        dplyr::rename(n = `COUNT(MAXNUM1.TEKJUAR)`)
}

# Lesa inn


df_postnr <- readxl::read_xlsx("2018_04_27_RECONOMICS2_TOFLUR.xlsx", sheet = "POSTNUMER") %>% relevel_bil()
df_svaedi <- readxl::read_xlsx("2018_04_27_RECONOMICS2_TOFLUR.xlsx", sheet = "SVÆÐIBIL") %>% relevel_bil()
df_bil <- readxl::read_xlsx("2018_04_27_RECONOMICS2_TOFLUR.xlsx", sheet = "BIL") %>% relevel_bil()
df_svaedi_allt <- readxl::read_xlsx("2018_04_27_RECONOMICS2_TOFLUR.xlsx", sheet = "SVAEDI1") %>% 
    dplyr::rename(n = `COUNT(MAXNUM1.TEKJUAR)`)


isat_count <- readxl::read_xlsx("2018_05_04_RECONOMICS_ISAT.xlsx", sheet = "isat", range = "B6:L28")
isat_svaedi <- readxl::read_xlsx("2018_05_04_RECONOMICS_ISAT.xlsx", sheet = "Sheet1")  %>% 
    dplyr::rename(n = `COUNT(MAXNUM1.TEKJUAR)`)
isat_balkur <- readxl::read_xlsx("2018_05_04_RECONOMICS_ISAT.xlsx", sheet = "Sheet4") %>% relevel_bil()

vextir_a_skuldabref <- read_csv("vextir_a_skuldabref.csv") %>% 
    dplyr::mutate(TEKJUAR = TIME, vextir_a_skuldabref = Value / 100) %>% 
    dplyr::select(TEKJUAR, vextir_a_skuldabref)

allt <- list(df_postnr, df_svaedi, df_bil, df_svaedi_allt, isat_count,
             isat_svaedi, isat_balkur)


df_postnr %>% filter(as.numeric(n) %>% between(5, 20)) %>%  ggplot(aes(x = as.numeric(n))) + geom_histogram()  
df_svaedi %>% filter(as.numeric(n) %>% between(5, 20)) %>%  ggplot(aes(x = as.numeric(n))) + geom_histogram()  
df_bil %>% filter(as.numeric(n) %>% between(5, 20)) %>%  ggplot(aes(x = as.numeric(n))) + geom_histogram()  
df_svaedi_allt %>% filter(as.numeric(n) %>% between(5, 20)) %>%  ggplot(aes(x = as.numeric(n))) + geom_histogram()  
isat_count %>% filter(as.numeric(n) %>% between(5, 20)) %>%  ggplot(aes(x = as.numeric(n))) + geom_histogram()  
isat_svaedi %>% filter(as.numeric(n) %>% between(5, 20)) %>%  ggplot(aes(x = as.numeric(n))) + geom_histogram()  
isat_balkur %>% filter(as.numeric(n) %>% between(5, 20)) %>%  ggplot(aes(x = as.numeric(n))) + geom_histogram()  


isat_svaedi %>% count(SVAEDI)

AUSTURLAND
, NORDURLAND_EYSTRA
, NORDURLAND_VESTRA
, REYKJANES
, REYKJAVIK
, SUDURLAND
, SUDURNES
, VESTFIRDIR
, VESTURLAND        




# explore
vars <- allt %>% 
    map(names) %>% 
    map(as_tibble) %>% 
    reduce(union)

df_bil %>% select(TEKJUAR, BIL, n, HREINAR_TEKJUR, hre)


# Scale per firm ----------------------------------------------------------

# df_postnr %<>%  mutate(n = as.numeric(n)) %>% mutate_at(vars(HREINAR_TEKJUR:SAMTALS_STADA_LOK_ARS), funs(. / n)) 
# df_bil %<>%  mutate(n = as.numeric(n)) %>% mutate_at(vars(HREINAR_TEKJUR:SAMTALS_STADA_LOK_ARS), funs(. / n)) 
# df_svaedi %<>%  mutate(n = as.numeric(n)) %>% mutate_at(vars(HREINAR_TEKJUR:SAMTALS_STADA_LOK_ARS), funs(. / n)) 
# df_svaedi_allt %<>%  mutate(n = as.numeric(n)) %>% mutate_at(vars(HREINAR_TEKJUR:SAMTALS_STADA_LOK_ARS), funs(. / n)) 
# isat_balkur %<>%  mutate(n = as.numeric(n)) %>% mutate_at(vars(HREINAR_TEKJUR:SAMTALS_STADA_LOK_ARS), funs(. / n)) 
# isat_svaedi %<>%  mutate(n = as.numeric(n)) %>% mutate_at(vars(HREINAR_TEKJUR:SAMTALS_STADA_LOK_ARS), funs(. / n)) 

# Plot Stærð------------------------------------------------------------

df_svaedi %>% 
    filter(TEKJUAR == 2017) %>%
    ggplot(aes(x = SVAEDI, y = as.numeric(n))) +
    geom_line()



# Hitamyndir --------------------------------------------------------------



## Hitamyndir ##

# Hagnaður á ft, isat og svæði
isat_svaedi %>% 
    filter(TEKJUAR == "2016") %>% 
    filter(!is.na(HAGNADUR_TAP)) %>% 
    ggplot(aes(x = ISAT2008_BALKURLYSING, y = as.factor(SVAEDI), fill = HAGNADUR_TAP_ARSREIKN / n)) +
    geom_tile() +
    scale_fill_continuous(trans = "log") +
    theme_intellecon() +
    vertical_x_label

my_trans <- function(x) sign(x) * log10(abs(x))
my_inverse_trans <- function(x) sign(x) * 10 ^ abs(x)

# Hagnaðarhlutfall, isat og svæði
isat_svaedi %>% 
    filter(TEKJUAR == "2016") %>% 
    filter(!(ISAT2008_BALKURLYSING %in% c("NÁMUGRÖFTUR OG VINNSLA HRÁEFNA ÚR JÖRÐU"))) %>% 
    filter(!is.na(HAGNADUR_TAP)) %>% 
    ggplot(aes(x = ISAT2008_BALKURLYSING, fill = as.factor(SVAEDI), y = HAGNADUR_TAP_ARSREIKN / HREINAR_TEKJUR)) +
    geom_col() +
    scale_y_continuous(trans = trans_new(name = "my_log", 
                                         transform = my_trans, inverse = my_inverse_trans),
                       breaks = 10^ (-8:8)) +
    # scale_fill_continuous(trans = "log", low = "red", high = "green") +
    theme_intellecon() +
    vertical_x_label

# Hagnaðarhlutfall, bil
isat_balkur %>% 
    filter(TEKJUAR == "2016") %>% 
    filter(!(ISAT2008_BALKURLYSING %in% c("NÁMUGRÖFTUR OG VINNSLA HRÁEFNA ÚR JÖRÐU"))) %>% 
    filter(!is.na(HAGNADUR_TAP),
           BIL != "NULL") %>% 
    mutate(hagnHlutfall = REKSTRARTEK_MIN_REKSTRARGJ / REKSTRARTEKJUR_SAMTALS) %>% 
    ggplot(aes(x = fct_reorder(f = BIL, x = hagnHlutfall, fun = "median", .desc = TRUE), y = hagnHlutfall)) +
    geom_boxplot(outlier.colour = intellecon_pal_discrete[2]) +
    # scale_y_continuous(trans = trans_new(name = "my_log", 
    #                                      transform = my_trans, inverse = my_inverse_trans),
    #                    breaks = 10^ (-8:8 * 2)) +
    # scale_fill_continuous(trans = "log", low = "red", high = "green") +
    theme_intellecon() +
    vertical_x_label 

# Hagnaðarhlutfall, isat 
isat_svaedi %>% 
    filter(TEKJUAR == "2016") %>% 
    filter(!(ISAT2008_BALKURLYSING %in% c("NÁMUGRÖFTUR OG VINNSLA HRÁEFNA ÚR JÖRÐU"))) %>% 
    mutate(hagnHlutfall = REKSTRARTEK_MIN_REKSTRARGJ / REKSTRARTEKJUR_SAMTALS) %>% 
    filter(hagnHlutfall > -1) %>% 
    ggplot(aes(x = fct_reorder(f = ISAT2008_BALKURLYSING, x = hagnHlutfall, fun = "median", .desc = TRUE), y = hagnHlutfall)) +
    geom_boxplot(outlier.colour = intellecon_pal_discrete[2]) +
    # scale_y_continuous(trans = trans_new(name = "my_log", 
    #                                      transform = my_trans, inverse = my_inverse_trans),
    #                    breaks = 10^ (-8:8 * 2)) +
    # scale_fill_continuous(trans = "log", low = "red", high = "green") +
    theme_intellecon() +
    vertical_x_label 

# Hagnaðarhlutfall, svæði 
isat_svaedi %>% 
    filter(TEKJUAR == "2016") %>% 
    filter(!(ISAT2008_BALKURLYSING %in% c("NÁMUGRÖFTUR OG VINNSLA HRÁEFNA ÚR JÖRÐU"))) %>% 
    mutate(hagnHlutfall = REKSTRARTEK_MIN_REKSTRARGJ / REKSTRARTEKJUR_SAMTALS) %>% 
    filter(hagnHlutfall > -1) %>% 
    ggplot(aes(x = fct_reorder(f = SVAEDI, x = hagnHlutfall, fun = "median", .desc = TRUE), y = hagnHlutfall)) +
    geom_boxplot(outlier.colour = intellecon_pal_discrete[2]) +
    # scale_y_continuous(trans = trans_new(name = "my_log", 
    #                                      transform = my_trans, inverse = my_inverse_trans),
    #                    breaks = 10^ (-8:8 * 2)) +
    # scale_fill_continuous(trans = "log", low = "red", high = "green") +
    theme_intellecon() +
    vertical_x_label 


# Hagnaðarhlutfall, isat og bil
isat_balkur %>% 
    filter(TEKJUAR == "2016") %>% 
    filter(!is.na(HAGNADUR_TAP)) %>% 
    ggplot(aes(x = ISAT2008_BALKURLYSING, y = as.factor(BIL), fill = HAGNADUR_TAP_ARSREIKN / HREINAR_TEKJUR)) +
    geom_tile() +
    scale_fill_continuous(trans = "log", low = "red", high = "green") +
    theme_intellecon() +
    vertical_x_label

# Fjöldi fyrirtækja, isat og svæði
isat_svaedi %>% 
    filter(TEKJUAR == "2016") %>% 
    filter(!is.na(HAGNADUR_TAP)) %>% 
    ggplot(aes(x = ISAT2008_BALKURLYSING, y = as.factor(SVAEDI), colour = n)) +
    geom_tile() +
    scale_fill_continuous(trans = "log", low = "red", high = "green") +
    theme_intellecon() +
    vertical_x_label


# Tekjur - Súlurit --------------------------------------------------------




# Tekjur á ft,
isat_svaedi %>%
    mutate(TEKJUAR = as.integer(TEKJUAR)) %>%
    group_by(TEKJUAR, ISAT2008_BALKURBALKUR, ISAT2008_BALKURLYSING) %>%
    summarise(n = sum(n, na.rm = TRUE),
              HREINAR_TEKJUR = sum(HREINAR_TEKJUR, na.rm = TRUE)) %>%
    filter(HREINAR_TEKJUR != 0,
           TEKJUAR == 2016) %>%
    ggplot(aes(x = fct_reorder(ISAT2008_BALKURLYSING, HREINAR_TEKJUR / n, .desc = TRUE), y = HREINAR_TEKJUR / n)) +
    geom_col() +
    # scale_y_log10() +
    theme_intellecon() +
    vertical_x_label

isat_svaedi %>% 
    mutate(TEKJUAR = as.integer(TEKJUAR)) %>% 
    group_by(TEKJUAR, ISAT2008_BALKURBALKUR, ISAT2008_BALKURLYSING) %>% 
    summarise(n = sum(n, na.rm = TRUE),
              HREINAR_TEKJUR = sum(HREINAR_TEKJUR, na.rm = TRUE)) %>% 
    filter(HREINAR_TEKJUR != 0,
           TEKJUAR == 2016) %>% 
    ggplot(aes(x = fct_reorder(ISAT2008_BALKURLYSING, HREINAR_TEKJUR, .desc = TRUE), y = HREINAR_TEKJUR)) +
    geom_col() +
    # scale_y_log10() +
    theme_intellecon() +
    vertical_x_label



# Launakostnaður ----------------------------------------------------------





# Skuldir -----------------------------------------------------------------


df_svaedi %>%
    mutate(TEKJUAR = as.integer(TEKJUAR)) %>% 
    filter(TEKJUAR > 1997) %>% 
    group_by(TEKJUAR) %>% 
    mutate(skuldir = 
               # ADRAR_SKAMMTIMASK +
               ADRAR_LANGTIMASKULDIR #+
               # ERLENDAR_LANGTIMASK +
               # SK_V_TENGD_ADILA_VAXTAREIK
           ) %>% 
    summarize(VAXTAGJOLD = sum(VAXTAGJOLD, na.rm = TRUE),
              skuldir = sum(skuldir, na.rm = TRUE),
              # REKSTRARTEK_MIN_REKSTRARGJ = sum(REKSTRARTEK_MIN_REKSTRARGJ, na.rm = TRUE),
              # REKSTRARTEKJUR_SAMTALS = sum(REKSTRARTEKJUR_SAMTALS, na.rm = TRUE)
              ) %>%
    mutate(vextir = VAXTAGJOLD / skuldir
           # hagnHlutfall = REKSTRARTEK_MIN_REKSTRARGJ / REKSTRARTEKJUR_SAMTALS,
           # hagnHlutfallHaerriVextir = (REKSTRARTEK_MIN_REKSTRARGJ + 
           #                                 (VAXTAGJOLD - VAXTAGJOLD * (vextir + 3/100)  / vextir)) / REKSTRARTEKJUR_SAMTALS,
    ) %>% 
    left_join(vextir_a_skuldabref) %>% 
    ungroup %>% 
    ggplot(aes(x = TEKJUAR, y = vextir)) +
    geom_col() +
    geom_line(aes(y = vextir_a_skuldabref))
    # geom_point(aes(y = hagnHlutfallHaerriVextir)) +
    theme_intellecon() +
    scale_y_continuous(labels = scales::percent)





# Myndir ------------------------------------------------------------------



# df_s %>% names %>% View

# df_s %>%
#     filter(TEKJUAR == 2016,
#            !is_null(BIL) & BIL != "NULL") %>% 
#     mutate(launahlutfall = LAUN / REKSTRARTEKJUR_SAMTALS) %>% 
#     ggplot(aes(x = SVAEDI, y = launahlutfall, fill = BIL)) +
#     geom_col(position = "dodge") +
#     theme_intellecon() %+replace%
#     theme(axis.text.x = element_text(angle = 10))

df_s %>% dplyr::select(starts_with("HAGN")) %>% View
View(df_s[ ,192:196])


df_s %>% dplyr::select(1:4, VAXTAGJOLD, VIDSKIPTASKULDIR)
df_s %>% dplyr::select(contains("skuld"))
df_s %>% mutate(RAT = ADRAR_LANGTIMASKULDIR/SKULDIR_SAMTALS, VAXT = VAXTAGJOLD / ADRAR_LANGTIMASKULDIR,
           VAXT2 = VAXTAGJOLD / SKULDIR_SAMTALS) %>% View

df_svaedi %>%
    mutate(TEKJUAR = as.integer(TEKJUAR)) %>% 
    filter(TEKJUAR > 1997) %>% 
    group_by(TEKJUAR) %>% 
    mutate(skuldir = 
               ADRAR_SKAMMTIMASK +
               ADRAR_LANGTIMASKULDIR +
               ERLENDAR_LANGTIMASK +
               SK_V_TENGD_ADILA_VAXTAREIK
           ) %>% 
    summarize(VAXTAGJOLD = sum(VAXTAGJOLD, na.rm = TRUE),
              skuldir = sum(skuldir, na.rm = TRUE)) %>%
    mutate(vextir = VAXTAGJOLD / skuldir) %>% 
    left_join(vextir_a_skuldabref) %>% 
    ggplot(aes(x = TEKJUAR, y = vextir)) +
    geom_col(position = "dodge") +
    geom_line(aes(y = vextir_a_skuldabref)) +
    theme_intellecon() +
    scale_y_continuous(labels = scales::percent)

# B/E ratio ---------------------------------------------------------------

isat_svaedi %>% 
    filter(TEKJUAR == 2011) %>% 
    mutate(BE = EIGID_FE_SAMTALS / HAGNADUR_SKV_ARSREIKNINGI) %>% 
    filter(between(BE, 0, 20)) %>%
    ggplot(aes(x = n, BE, color = ISAT2008_BALKURLYSING)) +
    geom_point() +
    scale_x_continuous()+
    scale_y_log10()  +
    geom_smooth(se = FALSE, method = "lm") +
    facet_wrap(~ ISAT2008_BALKURLYSING, nrow = 4)



# Kennitölur --------------------------------------------------------------

add_kt <- function(x){
    x %>% mutate(    
        # Arðsemi eigin fjár (e.return on equity,ROE) = Hagnaður ⁄ Meðalstaða eigin fjár
        roe = HAGNADUR_TAP_ARSREIKN / EIGID_FE_SAMTALS,
        # Arðsemi heildareigna (e.return on assets,ROA) = Hagnaður ⁄ (Meðalstaða eigna)
        ROA = HAGNADUR_TAP_ARSREIKN / EIGNIR_SAMTALS,
        # Hagnaðarhlutfall (e.profit margin) = Hagnaður ⁄ Sala
        sala = (VSK_SKYLD_SALA_SAMT + VELTA_UNDANTHEGIN_VSK),
        profit_margin = HAGNADUR_TAP_ARSREIKN / sala,
        # Brúttó ágóða hlutfall (e.gross profit margin) = (Brúttó ágóði) ⁄ Sala
        gross_margin = (sala - SOLUKOSTNADUR) / sala,
        # Arðgreiðsluhlutfall (e.payout ratio) = Arður ⁄ Hagnaður
        UTHLUTADUR_ARDUR / HAGNADUR_TAP_ARSREIKN,
        # Arðgreiðsluhlutfall (e.payout ratio) = Arður ⁄ Hagnaður
        payout_ratio = UTHLUTADUR_ARDUR / HAGNADUR_TAP_ARSREIKN,
        
        ### Greiðsluhæfi ###
        
        # Veltufjárhlutfall (e.current ratio) = Veltufjármunir ⁄ Skammtímaskuldir
        veltufjarmunir = INNEIGN_VIRDISAUKASKATTS + VERDBREF + HANDB_FE,
        skammtimaskuldir = ADRAR_SKAMMTIMASK + OGREIDDUR_VSK + NAESTA_ARS_AFB_LANGTIMALAN+
            VIDSKIPTASKULDIR + ERLENDAR_SKAMMTIMASK + ADR_FYRIRFRAM_INNH_TEKJUR,
        current_ratio  = veltufjarmunir / skammtimaskuldir,
        # Lausafjárhlutfall (e.quick ratio,acid-test) = (Kvikir veltufjármunir) ⁄ Skammtímaskuldir
        # Kvikir veltufjármunir = Veltufjármunir – Vörubirgðir
        quick_ratio = (veltufjarmunir - BIRGDIR_FULLUNN_VARA) / skammtimaskuldir,
        # Hlutfall handbærs fjár (e.cash ratio) =(Handbært fé + skammtímaverðbréf) / Skammtímaskuldir
        # cash_ratio = HANDB_FE + 
        
        # Sjóðstreymi á móti skammtímaskuldum 
        # (e.current cash debt coverage ratio) =(Handbært fé frá rekstri) ⁄ (Meðalstaða skammtímaskulda)
        current_cash_dept_coverage_ratio = HANDB_FE / skammtimaskuldir,
        # Gæði hagnaðar (e.quality of earnings) = (Handbært fé frá rekstri) ⁄ Hagnaður
        quality_of_earnings = HANDB_FE / HAGNADUR_TAP_ARSREIKN,
        
        ### Skuldsetningarhlutföll (e. coverage ratios) ###
        
        # Hlutfall skulda á móti eignum (e.debt to assets ratio) = Heildarskuldir ⁄ Heildareignir
        dept_asset_ratio = SKULDIR_SAMTALS / EIGNIR_SAMTALS,
        # Eiginfjárhlutfall (e.equity ratio) = (Eigið fé) ⁄ Heildareignir
        equity_ratio = EIGID_FE_SAMTALS / EIGNIR_SAMTALS,
        # Vaxtaþekja (e.times interest earned,TIE) = (Rekstrarhagnaður (EBIT)) ⁄ Vaxtagjöld
        tie = REKSTRARTEK_MIN_REKSTRARGJ / VAXTAGJOLD,
        # Skuldaþekja handbærs fjár frá rekstri 
        # (e.cash debt coverage ratio) = (Handbært fé frá rekstri) ⁄ (Meðalstaða heildarskulda)
        cash_debt_coverage_ratio = HANDB_FE / SKULDIR_SAMTALS
        # Veltuhraði birgða (e.inventory turnover ratio) = (Kostnaðarverð seldra vara) ⁄ Meðalstaða birgða
        # Ekki hægt!!!!!!!!!                          
    )
    
}

df_postnr: bil og póstnúmer
df_svaedi: bil og svæði
df_bil: bil
df_svaedi_allt: svæði
isat_svaedi: svæði og isat
isat_balkur: bil og isat


my_kennitalas <- c("roe", "ROA", "profit_margin", "gross_margin", "payout_ratio", 
                   "current_ratio", "quick_ratio", 
                   "current_cash_dept_coverage_ratio", "quality_of_earnings", "dept_asset_ratio", 
                   "equity_ratio", "tie", "cash_debt_coverage_ratio")

my_titles <- c("roe" = "Arðsemi eigin fjár (e.return on equity,ROE)",
               "ROA" = "Arðsemi heildareigna (e.return on assets,ROA)",
               "profit_margin" = "Hagnaðarhlutfall (e.profit margin)",
               "gross_margin" = "Brúttó ágóða hlutfall (e.gross profit margin)",
               "payout_ratio" = "Arðgreiðsluhlutfall (e.payout ratio)",
               "current_ratio" = "veltufjarmunir / skammtimaskuldir",
               "quick_ratio" = "Lausafjárhlutfall (e.quick ratio,acid-test)",
               "current_cash_dept_coverage_ratio" = "Sjóðstreymi á móti skammtímaskuldum",
               "quality_of_earnings" = "Gæði hagnaðar (e.quality of earnings)",
               "dept_asset_ratio" = "Hlutfall skulda á móti eignum (e.debt to assets ratio)",
               "equity_ratio" = "Eiginfjárhlutfall (e.equity ratio)",
               "tie" = "Vaxtaþekja (e.times interest earned,TIE)",
               "cash_debt_coverage_ratio" = "Skuldaþekja handbærs fjár frá rekstri")


wrapit <- function(text) {
    wtext <- paste(strwrap(text,width=40), collapse = " \n ")
    return(wtext)
}


change_label <- function(text){
    sprintf("%0.2f", round(text, digits = 2))
}

plot_kennitala <- function(df, x_var, y_var, facet_var){
    
    ylimits <- df[y_var] %>% as.data.frame() %>% quantile(c(0.1, 0.9), na.rm = TRUE)
    label_loc <- ylimits[1] + 0.1 * (ylimits[2] - ylimits[1])
    
    df %>% 
        filter(roe != 0) %>% 
        mutate(my_label = change_label(roe)) %>%
        ggplot(aes_string(x = x_var, y = y_var), fill = intellecon_pal_discrete[2]) +
        # geom_col(position = "dodge", fill = intellecon_pal_discrete[2]) +
        coord_cartesian(ylim = ylimits) +
        # geom_text(aes(label = my_label), y = label_loc, size = 2, color = intellecon_pal_discrete[1]) +
        facet_wrap(as.formula(paste("~", facet_var)), ncol = 3) +
        theme_intellecon() +
        list(theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
                   text = element_text(size = 7))) +
        labs(title = my_titles[y_var])
}





isat_svaedi <- isat_svaedi %>% 
    add_kt 

isat_svaedi %>% 
    filter(!is.na(roe), !is.na(SVAEDI)) %>%
    ungroup %>%
    ggplot(aes(x = roe, TEKJUAR)) +
    geom_joy2()


for(kt in my_kennitalas){
    isat_svaedi %>% 
        filter(TEKJUAR == 2017) %>% 
        group_by(ISAT2008_BALKURLYSING) %>% 
        mutate(isat = wrapit(first(ISAT2008_BALKURLYSING))) %>% ungroup %>% 
        plot_kennitala("SVAEDI", kt, "isat") %>% 
        print()
}


    

df_bil %>% 
    filter(TEKJUAR == max(TEKJUAR),
           BIL != "NULL") %>% 
    ggplot(aes(x = BIL, y = TEKJUF_EFTIRGJOF_SKULDA)) +
    geom_col() +
    theme_intellecon()
    
df_svaedi %>% 
    # filter(#TEKJUAR == 2016,
    #        BIL != "NULL") %>% 
    ggplot(aes(x = TEKJUAR, y = TEKJUF_EFTIRGJOF_SKULDA)) +
    geom_col() +
    theme_intellecon()
    
df_svaedi %>% 
    # filter(#TEKJUAR == 2016,
    #        BIL != "NULL") %>% 
    ggplot(aes(x = TEKJUAR, y = TEK_EFTIRG_SKULD_SKILMBR)) +
    geom_col() +
    theme_intellecon()