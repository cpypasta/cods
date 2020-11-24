library(shiny)
library(xts)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(plotly)
library(gplots)
library(ggcorrplot)
library(scales)
library(ggthemes)

source("utils.R")

# trim points at beginning
# script to create data

cod_measures <- readRDS("data/cod_measures.rds")
cod_measures_long <- readRDS("data/cod_measures_long.rds")
cod_granger <- readRDS("data/cod_granger.rds")
cod_predictions <- readRDS("data/cod_predictions.rds")
usa_states <- readRDS("data/usa_states.rds")

cod_statecauses <- readRDS('data/statecausesfullpop.rds')
cod_statecauses <- cod_statecauses %>% filter(upstate != 'Puerto Rico')

cod_causes <- levels(cod_measures_long$measure)
covid_causes <- c("covid", "covid_multiple")
non_covid_causes <- cod_causes[!(cod_causes %in% covid_causes)]
create_palette <- colorRampPalette(brewer.pal(n = 8, name = "Dark2"))
cod_palette <- create_palette(length(cod_causes))

cod_years <- unique(cod_statecauses$Year)
cod_regions <- unique(cod_statecauses$region)

covid_map_data <- cod_statecauses %>%
    transmute(state, year = Year, measure = Measure, value = Value, pop = Pop, value_capita = value / pop) %>%
    filter(measure %in% c("Covid", "CovidMultiple"), value > 0) %>%
    group_by(state) %>%
    summarize(value = sum(value), value_capita = sum(value_capita)) %>%    
    inner_join(usa_states, by = c("state" = "region")) %>%
    select(-subregion)

ui <- fluidPage(
    titlePanel("U.S. COVID-19 Causes of Death Analysis"),
    img(src="covid.jpg", align = "left"),
    p("COVID-19 deaths in the United States have surpassed 256,000, and the coronavirus is now the third leading cause of 
      death in this country, after heart disease and cancer. Before the pandemic, the U.S. already had a high overall 
      mortality rate, and the gap has widened in the last few decades. In this analysis, we put the pandemic's toll into 
      perspective by comparing where COVID-19 falls as a leading cause of death (COD) in the U.S., and how it has affected the 
      number of deaths in the other 12 causes. Causes of death analysis attempts to understand the burden of mortality that 
      are directly or indirectly attributed to COVID-19."), 
    p("The CDC National Center for Health Statistics (NCHS) collects weekly counts of deaths by state and of select causes 
      that are categorized by underlying cause of death listed in the standardized health care grouping of ICD-10 codes. 
      From 2014 to the current period, there are 12 main listed causes, including leading U.S. killers such as diseases of 
      the heart, diabetes, and lower respiratory. With the onset of COVID-19, two more causes were added: COVID-19 Multiple 
      Cause of Death and COVID-19 Underlying Cause of Death."),
    p(strong("Our project seeks to find if COVID-19 had any affect on the number of deaths in the other 12 causes."), "We combined", 
      a("Weekly Counts of Deaths by State and Select Causes, 2014-2018", href="https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/3yf8-kanr"),  
      "and",
      a("Weekly Counts of Deaths by State and Select Causes, 2019-2020", href="https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6"), 
      "datasets into one tidy dataset to represent provisional counts of deaths by the week the deaths occurred, by state of occurrence, and by select underlying causes of death from 2014-2020.",
      "We also used the", a("U.S. 2020 Population Density", href="https://worldpopulationreview.com/state-rankings/state-densities"), "dataset to calculate deaths per capita."),
    p("Our overall finding from our analysis is that COVID-19 most certainly affected the death counts for other COD, but we 
      cannot be sure how exactly they are correlated. It may simply be due to misclassified COD, or it could be due to a true causal interaction. A few COD have
      pretty signficant statistical support for a correlation with COVID-19: diabetes and heart disease. These COD show
      significance with Granger causality and Pearson correlations. Moreover, Alzheimer's disease shows signficant Pearson
      correlation, a strong Granger causality (though less than signficant), and clearly shows a prediction anomaly in 2020.
      More intensive analysis would need to be done to strengthen and expand on these findings."),
    p("The code for this Shiny app can be found on", a("GitHub.", href="https://github.com/rollerb/cods"), "There will find all the code for this Shiny app as well as all the preprocessing logic for reproducing the results shown here."),
    br(),
    tabsetPanel(
        type = "tabs",
        tabPanel("Correlations", 
                 h2("Causes of Death by Time"),
                 p("The main focus of our analysis was comparing the COD over time.
                   In other words, we looked for a correlation between one COD over time and another,
                   with special focus being placed on COVID-19. We used four strategies to address this question:",
                 a("Perason correlation coefficients,", href = "https://inspirationalfreethought.wordpress.com/2014/05/16/spurious-correlations-with-time-series-what-we-can-learn/"),
                 a("Granger causality,", href="https://en.wikipedia.org/wiki/Granger_causality"),
                 a("predicted deaths", href="https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm"),
                 "(shown on the \"Predictions\" tab), and basic descriptive visualizations like time series plots."),
                 p("On this tab, you can see all 13 COD, which you can hide or show individually. We also have a couple 
                   correlation buttons that will select only those COD that are significant based on the time frame you 
                   have selected and the statistical calculations. You can control the years shown by moving the \"Years\" 
                   slider. A couple notes about the statistical calculations: the Perason correlation is done on the
                   time series difference to remove any non-stationarity characteristics; and the Granger causality only
                   looks at the weeks where there are COVID-19 deaths." ),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         helpText("Customize the time series plots using these options:"),
                         lapply(1:length(cod_causes), function(x) {
                             n <- length(cod_causes)
                             col <- col2hex(cod_palette[x])
                             css_col <- paste0("#cods div.checkbox:nth-child(",x,
                                               ") span{color: ", col,"; ")
                             tags$style(type="text/css", css_col)
                         }),
                         checkboxGroupInput(
                             "cods",
                             "Causes of Death",
                             cod_causes,
                             cod_causes
                         ),
                         p(
                             actionButton("select_all", "Select All"),
                             actionButton("deselect_all", "Deselect All")
                         ),
                         p(
                             strong("Significant COVID-19 Correlations"),
                             style = "margin-top: 15px"
                         ),
                         p(
                             actionButton("select_granger", "Granger Causality"),
                             actionButton("select_pearson", "Pearson Correlation")
                         ),
                         sliderInput(
                             "years",
                             "Years",
                             min = 2014, max = 2020,
                             value = c(2019,2020),
                             sep = ""
                         )
                     ),
                     mainPanel(
                         tabsetPanel(
                             type = "tabs",
                             tabPanel("Timeline", plotlyOutput("cod_timelines"), style = "margin-top: 10px"),
                             tabPanel("Stacked", plotlyOutput("stacked_cod"), style = "margin-top: 10px")
                         ),
                         
                         fluidRow(
                             column(6, plotOutput("granger_plot")),
                             column(6, plotOutput("pearson_corr"))
                         )
                     )
                 )        
        ),
        tabPanel("Predictions",
                 h2("Causes of Death by Predictions"),
                 p("Another way of looking at COD abnormalities is to look at the expected death count versus actual count.
                   We would expect the counts to not go outside of the 95% confidence interval, and if there are counts that
                   are beyond those bounds, we would consider them statistically unusual. We can't come to any conclusions
                   about what caused these abnormalities, but we can show when it happens and can speculate on cause.
                   Of particular interest to us, is any abnormal changes during the COVID-19 pandemic, which there are quite
                   a few anomalies in the plots below. This type of analysis is usually done to identify \"excess deaths\",
                   but we are using it in a more general sense, not just to identify total COVID-19 counts."),
                 p("The predicted death counts were predicted using",
                 a("ARIMA", href="https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average"),
                 "models and were developed using the data from 2015-2019."),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         helpText("Select cause of death to see 2020 forecast:"),
                         selectInput(
                             "predict_cause",
                             "Cause of Death",
                             c("ALL", non_covid_causes),
                             selected = "ALL"
                         ),
                         sliderInput(
                             "past_years",
                             "Past Years",
                             min = 2015, max = 2020,
                             value = 2020,
                             sep = ""
                         )
                     ),
                     mainPanel(
                         plotOutput("predict_plot")
                     )
                 )        
        ),
        tabPanel("Regions",
                 h2("Causes of Death by Region"),
                 p("Here we see the COD per capita (of the population) by region, state, and year. Determining
                   the dispersement of how the causes of death are different by region, state, and year allows for a more 
                   targeted view of how COVID-19 has affected each COD and to notice patterns. For the most
                   part we see a fairly consistent pattern from year to year for cause of death prior to 2020."),
                 fluidRow(
                     column(12, plotlyOutput("map_cod"))
                 ),
                 p("In the charts below, the fewer years chosen, the easier it is to see a difference, particlulary in 
                   the South. Additionally, with 2020 (and COVID-19) we begin to see a drop in things like
                   heart disease, cancer, and Alzheimer's. Heart disease and cancer are known risk factors for death 
                   from COVID-19, but Alzheimer's is not. We can hypothesize for possible reasons, but overall this 
                   indicates an area that will need more research."), 
                   p("Note: While it is possible to select all 
                   years for all regions, it is advised to select no more than 4 years to be able to easily see 
                   differences between the charts."),
                 sidebarLayout(
                     sidebarPanel(
                         helpText("Causes of Death by Region with Population Normalized"),
                         checkboxGroupInput(
                             "cdc_years",
                             "Years",
                             cod_years,
                             c(2017,2018,2019,2020)
                         ),
                         checkboxGroupInput(
                             "regions",
                             "Region",
                             cod_regions,
                             cod_regions
                         )
                     ),
                     mainPanel(
                         plotOutput("location_plot", height = "800px")
                     )
                 )
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$select_all, {
        updateCheckboxGroupInput(session, "cods", choices = cod_causes, selected = cod_causes)
    })
    
    observeEvent(input$deselect_all, {
        updateCheckboxGroupInput(session, "cods", choices = cod_causes, selected = c())
    })
    
    observeEvent(input$select_granger, {
        covid_fields <- c("covid", "covid_multiple")
        sig_granger <- cod_granger %>%
            filter((result %in% covid_fields | predictor %in% covid_fields) & granger <= 0.05) %>%
            mutate(across(where(is.factor), as.character))
        selected = unique(c(sig_granger$result, sig_granger$predictor))
        updateCheckboxGroupInput(session, "cods", choices = cod_causes, selected = selected)
    })
    
    observeEvent(input$select_pearson, {
        updateCheckboxGroupInput(session, "cods", choices = cod_causes, selected = get_pearson_names(cod_measures, cod_causes, input$years))
    })
    
    output$cod_timelines <- renderPlotly({
        validate(
            need(length(input$cods) > 0, "Please select causes of death in order to see plots.")
        )
        
        cods <- input$cods
        data <- cod_measures_long %>% filter(measure %in% cods) %>% rename(cause = measure, deaths = value)
        date_bounds <- find_date_bounds(data$week_end, input$years)
        min_date <- date_bounds$min_date
        max_date <- date_bounds$max_date
        date_bounds_title <- paste0(date_bounds$min_date, " to ", date_bounds$max_date)
        selected_palette <- cod_palette[which(cod_causes %in% cods)]
        
        cod_plot <- ggplot(data, aes(x = week_end, y = deaths, color = cause)) +
            geom_line(size = .5) +
            xlim(min_date, max_date) +
            scale_color_manual(values = selected_palette) +
            scale_y_continuous(labels = comma) +
            labs(y = "Weekly Deaths") +
            theme(legend.position = "none", axis.title.x = element_blank())
        ggplotly(cod_plot, tooltip = c("colour", "x", "y")) %>%
            layout(margin = list(t = 40), title = list(x = 0.1, y = 0.96, text = paste0("Causes of Death Over Time <br><sup>", date_bounds_title, "</sup>")))
    })
    
    output$granger_plot <- renderPlot({
        req(input$cods)
        cod_granger %>%
            filter(result %in% input$cods & predictor %in% input$cods) %>%
            slice_min(granger, n = 10) %>%
            ggplot(aes(x = reorder(granger_formula, granger), y = granger, fill = granger)) +
            geom_col() +
            coord_flip() +
            labs(y = "Granger p-value", title = "Top 10 Granger Causality*", caption = "*Only considers weeks with COVID deaths") +
            theme(axis.title.y = element_blank(), legend.position = "none") +
            scale_fill_gradient2(mid = "grey", high = "brown") +
            scale_y_continuous(expand = c(0,0)) +
            geom_hline(yintercept = 0.05, color = "black", size = 0.4, linetype = "dashed")
    })
    
    output$pearson_corr <- renderPlot({
        req(input$cods, input$years)
        date_bounds <- find_date_bounds(cod_measures$week_end, input$years)
        corr <- get_pearson(cod_measures, input$cods, input$years)
        ggcorrplot(corr,
                   lab = TRUE,
                   lab_size = 3,
                   show.legend = FALSE,
                   title = "Pearson Correlation") +
            labs(subtitle = paste0(date_bounds$min_date, " to ", date_bounds$max_date))
    })
    
    output$predict_plot <- renderPlot({
        predict_all <- input$predict_cause == "ALL"
        data <- if (predict_all) cod_predictions else cod_predictions %>% filter(disease == input$predict_cause)
        
        has_past_data <- input$past_years < 2020
        
        data <- if (has_past_data) {
            past_data <- cod_measures_long %>%
                filter(year(week_end) >= input$past_years & year(week_end) < 2020, !(measure %in% covid_causes))
            
            if (!predict_all) {
                past_data <- past_data %>% filter(measure == input$predict_cause)   
            }
            
            past_data %>%
                rename(disease = measure, actual = value, date = week_end) %>%
                bind_rows(data)
        } else {
            data
        }
        
        ggpredict <- ggplot(data) +
            geom_line(aes(x = date, y = actual)) +
            geom_line(aes(x = date, y = upper), color = "blue") +
            geom_line(aes(x = date, y = lower), color = "blue") +
            geom_line(aes(x = date, y = mean), color = "red", linetype = "dashed") +
            geom_ribbon(aes(x = date, ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
            scale_y_continuous(labels = comma) +
            labs(title = paste(input$predict_cause, "expected death count in 2020"), subtitle = "showing 95% confidence interval", x = "Week", y = "Deaths")
        
        if (predict_all) {
            ggpredict + facet_wrap(vars(disease), scales = "free")
        } else {
            ggpredict
        }
    })
    
    output$stacked_cod <- renderPlotly({
        req(input$cods)
        cods <- input$cods
        data <- cod_measures_long %>% filter(measure %in% cods) %>% rename(cause = measure, deaths = value)
        date_bounds <- find_date_bounds(data$week_end, input$years)
        min_date <- date_bounds$min_date
        max_date <- date_bounds$max_date
        date_bounds_title <- paste0(date_bounds$min_date, " to ", date_bounds$max_date)
        selected_palette <- cod_palette[which(cod_causes %in% cods)]
        
        cod_plot <- ggplot(data = data, aes(x = week_end, y = deaths, fill = cause)) +
            geom_bar(position = "fill", stat = "identity") +
            scale_fill_manual(values = selected_palette) +
            xlim(min_date, max_date) +
            theme(legend.position = "none", axis.title.x = element_blank()) +
            ylab("Weekly Deaths Percent")
        ggplotly(cod_plot, tooltip = c("fill", "x", "y")) %>%
            layout(margin = list(t = 40), title = list(x = 0.1, y = 0.96, text = paste0("Causes of Death Percentage Over Time <br><sup>", date_bounds_title, "</sup>")))
    })
    
    output$map_cod <- renderPlotly({
        req(input$cods)
        
        covid_map_plot <- covid_map_data %>% arrange(group, order) %>%
            ggplot(mapping = aes(x = long, y = lat, group = group, text = paste0("</br>State: ", str_to_title(state), "</br>Deaths: ", format(value, big.mark = ","), "</br>Per Capita: ", round(value_capita, 5)))) +
            geom_polygon(aes(fill = value_capita), color = "black", size = 0.1) + 
            theme_map() +
            ggtitle("COVID-19 Deaths per Capita") +
            coord_fixed(1.3) +
            theme(legend.position = "none") +
            scale_fill_gradient2(midpoint = median(covid_map_data$value_capita), 
                                 low = muted("blue"),
                                 mid = "white",
                                 high = muted("red"))
        ggplotly(covid_map_plot, tooltip = "text")
    })
    
    output$location_plot <- renderPlot({
        req(input$cdc_years, input$regions)
        data <- cod_statecauses %>% filter(Year == input$cdc_years)
        data <- data %>% filter(region == input$regions) %>% mutate(NormalizedPopulation = ValuePop/100000)
        barColourCount = length(unique(cod_statecauses$Measure))
        getPalette = colorRampPalette(brewer.pal(12, "Paired"))
        
        
        ggplot(data = data,  aes(x=Cause, y = NormalizedPopulation, fill=Measure)) +
            geom_bar(stat="identity") +
            facet_wrap(~Year + region, ncol=length(input$regions)) +
            scale_fill_manual(values = getPalette(barColourCount)) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))
        
    })
}

shinyApp(ui = ui, server = server)
