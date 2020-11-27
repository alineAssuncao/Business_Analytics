# Analisando Tráfego de Web Sites

# Pacotes
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("riverplot")
install.packages("RColorBrewer")
library(gridExtra)  
library(ggplot2)  
library(lubridate) 
library(riverplot)  
library(RColorBrewer) 

# Criando função para converter hh:mm:ss para segundos
make_seconds <- function(hhmmss) {
    hhmmss_list <- strsplit(hhmmss, split = ":")
    3600 * as.numeric(hhmmss_list[[1]][1]) +
        60 * as.numeric(hhmmss_list[[1]][2]) +
        as.numeric(hhmmss_list[[1]][3])
    }    


# Carregando os dados de acesso ao web site de uma loja online
dados <- read.csv("dados_site_loja_online.csv", stringsAsFactors = FALSE)

# Examinando o dataframe
print(head(dados))
print(str(dados))

# Transformando a data em, um objeto do tipo data
dados$date <- parse_date_time(dados$date, "mdy")

# Convertendo a duração da sessão para segundos
dados$ave_session_seconds <- numeric(nrow(dados))
for (i in seq(along = dados$ave_session_duration)) 
  dados$ave_session_seconds[i] <- 
        make_seconds(dados$ave_session_duration[i])

# Calculando o total de segundos em todas as sessões em um dia de acesso ao web site
dados$total_session_seconds <-
  dados$ave_session_seconds * dados$sessions

# 161 dias = 23 semanas, portanto podemos indexar por semanas
week <- NULL
for (i in 1:23) week <- c(week, rep(i, times = 7))
dados$week <- week

# Calculando número de sessões por tipo de browser
dados$other_browser <- dados$sessions -
  dados$chrome - dados$safari -
  dados$firefox - dados$internet_explorer
    
# Calculando número de sessões por tipo de sistema operacional
dados$other_system <- dados$sessions -
  dados$windows - dados$macintosh -
  dados$ios - dados$android    

# Extraindo acessos diários e totais
dados_daily <- dados[, 
    c("date", "week", "sessions", "users", "pageviews", "scroll_videopromo",    
      "scroll_whatstoutbay", "scroll_howitworks", "scroll_faq",           
       "scroll_latestfeeds", "internet_explorer", "chrome", "firefox", 
       "safari", "other_browser", "windows", "macintosh", "ios", "android",              
       "other_system", "total_session_seconds")]

# Examinando o datafreme criado
print(str(dados_daily))
print(head(dados_daily))

# Agregando por semana usando sum()
dados_weekly <- 
    aggregate(dados_daily[, setdiff(names(dados_daily), c("date","week"))], 
    by = list(dados_daily$week), FUN = sum)
names(dados_weekly)[1] <- "week"  

# Calculando a duração média das sessões no web site em segundos
dados_weekly$ave_session_seconds <- 
  dados_weekly$total_session_seconds / dados_weekly$sessions

# Examinando o datafreme criado
print(str(dados_weekly))
print(head(dados_weekly))

# Formatando os dados sobre Browser para o Plot
Browser <- "IE"
Count <- sum(dados_weekly$internet_explorer)
browser_data_frame <- data.frame(Browser, Count)
Browser <- "Chrome"
Count <- sum(dados_weekly$chrome)
browser_data_frame <- rbind(browser_data_frame, data.frame(Browser, Count))
Browser <- "Firefox"
Count <- sum(dados_weekly$firefox)
browser_data_frame <- rbind(browser_data_frame, data.frame(Browser, Count))
Browser <- "Safari"
Count <- sum(dados_weekly$safari)
browser_data_frame <- rbind(browser_data_frame, data.frame(Browser, Count))    
Browser <- "Other"
Count <- sum(dados_weekly$other_browser)
browser_data_frame <- rbind(browser_data_frame, data.frame(Browser, Count))

# Plot
pdf(file = "graf01_dados_user_browsers_R.pdf", width = 11, height = 8.5)
browser_bar_plot <- ggplot(data = browser_data_frame,
    aes(x = Browser, y = Count)) + 
    geom_bar(stat = "identity", width = 0.75, colour = "black", fill = "darkblue") + ylim(0, 225) +
    theme(axis.title.y = element_text(size = 15, colour = "black")) +
    theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(size = 15, colour = "black")) +
    annotate("text", x = 1, y = browser_data_frame$Count[1] + 5, 
        label = paste(as.character(round(100 * browser_data_frame$Count[1]/
            sum(browser_data_frame$Count), digits = 0)), "%", sep = "")) +
    annotate("text", x = 2, y = browser_data_frame$Count[2] + 5, 
        label = paste(as.character(round(100 * browser_data_frame$Count[2]/
            sum(browser_data_frame$Count), digits = 0)), "%", sep = "")) +   
    annotate("text", x = 3, y = browser_data_frame$Count[3] + 5, 
        label = paste(as.character(round(100 * browser_data_frame$Count[3]/
            sum(browser_data_frame$Count), digits = 0)), "%", sep = "")) +
    annotate("text", x = 4, y = browser_data_frame$Count[4] + 5, 
        label = paste(as.character(round(100 * browser_data_frame$Count[4]/
            sum(browser_data_frame$Count), digits = 0)), "%", sep = "")) +
    annotate("text", x = 5, y = browser_data_frame$Count[5] + 5, 
        label = paste(as.character(round(100 * browser_data_frame$Count[5]/
            sum(browser_data_frame$Count), digits = 0)), "%", sep = ""))            
print(browser_bar_plot)
dev.off()

# Formatando os dados sobre Sistema Operacional para o Plot
System <- "Windows"
Count <- sum(dados_weekly$windows)
system_data_frame <- data.frame(System, Count)
System <- "Macintosh"
Count <- sum(dados_weekly$macintosh)
system_data_frame <- rbind(system_data_frame, data.frame(System, Count))
System <- "iOS"
Count <- sum(dados_weekly$ios)
system_data_frame <- rbind(system_data_frame, data.frame(System, Count))
System <- "Android"
Count <- sum(dados_weekly$android)
system_data_frame <- rbind(system_data_frame, data.frame(System, Count))    
System <- "Other"
Count <- sum(dados_weekly$other_system)
system_data_frame <- rbind(system_data_frame, data.frame(System, Count))

pdf(file = "graf02_dados_user_systems_R.pdf", width = 11, height = 8.5)
system_bar_plot <- ggplot(data = system_data_frame,
    aes(x = System, y = Count)) + 
    geom_bar(stat = "identity", width = 0.75, 
        colour = "black", fill = "darkblue") +
    ylim(0, max(system_data_frame$Count + 15)) +
    theme(axis.title.y = element_text(size = 15, colour = "black")) +
    theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(size = 15, colour = "black")) +
    annotate("text", x = 1, y = system_data_frame$Count[1] + 5, 
        label = paste(as.character(round(100 * system_data_frame$Count[1]/
            sum(system_data_frame$Count), digits = 0)), "%", sep = "")) +
    annotate("text", x = 2, y = system_data_frame$Count[2] + 5, 
        label = paste(as.character(round(100 * system_data_frame$Count[2]/
            sum(system_data_frame$Count), digits = 0)), "%", sep = "")) +   
    annotate("text", x = 3, y = system_data_frame$Count[3] + 5, 
        label = paste(as.character(round(100 * system_data_frame$Count[3]/
            sum(system_data_frame$Count), digits = 0)), "%", sep = "")) +
    annotate("text", x = 4, y = system_data_frame$Count[4] + 5, 
        label = paste(as.character(round(100 * system_data_frame$Count[4]/
            sum(system_data_frame$Count), digits = 0)), "%", sep = "")) +
    annotate("text", x = 5, y = system_data_frame$Count[5] + 5, 
        label = paste(as.character(round(100 * system_data_frame$Count[5]/
            sum(system_data_frame$Count), digits = 0)), "%", sep = ""))            
print(system_bar_plot)
dev.off()

# Plot Multiple Time Series para sessões, pageviews e duração das sessões
pdf(file = "graf03_dados_site_stats_R.pdf", width = 8.5, height = 11)
sessions_plot <- ggplot(data = dados_weekly,
    aes(x = week, y = sessions)) + geom_line()  +
    ylab("Sessions") +
    theme(axis.title.x = element_blank()) +
    annotate("rect", xmin = 11.75, xmax = 13.25, 
        ymin = 0, ymax = max(dados_weekly$sessions), 
        fill = "blue", alpha = 0.4) 
  
pageviews_plot <- ggplot(data = dados_weekly,
    aes(x = week, y = pageviews)) + geom_line() +
    ylab("Page Views") +
    theme(axis.title.x = element_blank()) +
    annotate("rect", xmin = 11.75, xmax = 13.25, 
        ymin = 0, ymax = max(dados_weekly$pageviews), 
        fill = "blue", alpha = 0.4) 

duration_plot <- ggplot(data = dados_weekly,
    aes(x = week, y = ave_session_seconds)) + geom_line() +
    xlab("Semana de Operação") +
    ylab("Segundos") +
    theme(axis.title.x = element_text(size = 15, colour = "black")) +
    annotate("rect", xmin = 11.75, xmax = 13.25, 
        ymin = 0, ymax = max(dados_weekly$ave_session_seconds), 
        fill = "blue", alpha = 0.4) +
    annotate("text", x = 12.5, y = 20, size = 4, colour = "white",
        label = "UseR!") 

mts_plot <- grid.arrange(sessions_plot, pageviews_plot,
    duration_plot, ncol = 1, nrow = 3)
print(mts_plot)
dev.off()
    
# Diagrama Sankey para home page scrolling
pdf(file = "graf04_dados_sankey_R.pdf", width = 8.5, height = 11)
nodes <- data.frame(ID = c("A","B","C","D","E","F","G"),
    x = c(1, 2, 3, 4, 5, 6, 6),
    y = c(7, 7.5, 8, 8.5, 9, 9.5, 6),
    labels = c("Top",
               "Video",
               "What?",
               "How?",
               "FAQ",
               "News",
               "Exit"),
               stringsAsFactors = FALSE, 
               row.names = c("A","B","C","D","E","F","G"))
edges <- data.frame(N1 = c("A","B","C","D","E",
                           "A","B","C","D","E"),
                    N2 = c("G","G","G","G","G",
                           "B","C","D","E","F"),
    Value = c(
    sum(dados_weekly$sessions)- sum(dados_weekly$scroll_videopromo),
    sum(dados_weekly$scroll_videopromo) - 
        sum(dados_weekly$scroll_whatstoutbay),
    sum(dados_weekly$scroll_whatstoutbay) - 
        sum(dados_weekly$scroll_howitworks),
    sum(dados_weekly$scroll_howitworks) - sum(dados_weekly$scroll_faq),
    sum(dados_weekly$scroll_faq) - sum(dados_weekly$scroll_latestfeeds),
    sum(dados_weekly$scroll_videopromo),
    sum(dados_weekly$scroll_whatstoutbay),
    sum(dados_weekly$scroll_howitworks),
    sum(dados_weekly$scroll_faq),
    sum(dados_weekly$scroll_latestfeeds)), row.names = NULL)
    
selected_pallet <- brewer.pal(9, "Blues")
river_object <- makeRiver(nodes, edges,  
    node_styles = 
    list(A = list(col = selected_pallet[9], textcol = "white"),
    B = list(col = selected_pallet[8], textcol = "white"),
    C = list(col = selected_pallet[7], textcol = "white"),
    D = list(col = selected_pallet[6], textcol = "white"),
    E = list(col = selected_pallet[5], textcol = "white"),
    F = list(col = selected_pallet[5], textcol = "white"),
    G = list(col = "darkred", textcol = "white")))
plot(river_object, nodewidth = 4, srt = TRUE) 
dev.off()



