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
non_covid_causes <- cod_causes[!(cod_causes %in% c("covid", "covid_multiple"))]
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
    titlePanel("Causes of Death"),
    p("Motivation: COVID-19 deaths in the United States have surpassed 256,000, and the coronavirus is now the third leading cause of death in this country, after heart disease and cancer. Before the pandemic, the U.S. already had a high overall mortality rate, and the gap has widened in the last few decades.In this analysis, we put the pandemic's toll into perspective by comparing where COVID-19 falls as a leading cause of death in the U.S. and how it has affected the number of deaths in other 12 causes. Causes of death analysis attempt to understand the burden of mortality that are directly or indirectly attributed to COVID-19."), 
    p("The CDC National Center for Health Statistics (NCHS) collects weekly counts of deaths by state and of select causes that are categorized by underlying cause of death listed in the standardized health care grouping of ICD-10 codes. From 2014 - to current period, there were 12 main listed causes, including leading USA killers such as diseases of heart, diabetes, and chronic lower respiratory diseases. With the onset of COVID-19, two more causes were added: COVID-19 Multiple Cause of Death and COVID-19 Underlying Cause of Death."),
    p("Our project seeks to find if COVID-19 had any affect on the number of deaths in the other 12 causes. We combined \"Weekly Counts of Deaths by State and Select Causes, 2014-2018\" and \"Weekly Counts of Deaths by State and Select Causes, 2019-2020\" datasets into one tidy dataset to represent provisional counts of deaths by the week the deaths occurred, by state of occurrence, and by select underlying causes of death from 2014-2020. The dataset also includes weekly provisional counts of death for COVID-19, coded to ICD-10 code U07.1 as an underlying or multiple cause of death."),
    tabsetPanel(
        type = "tabs",
        tabPanel("Correlations", 
            h2("CODs Correlations Over Time"),
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
             h2("Causes of Death in 2020: Predicted vs. Actual"),
             p("Here we compare what the predicted death count for a COD compared to the actual death counts."),
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
             fluidRow(
                 column(12, plotlyOutput("map_cod"))
             ),
             p("Here we see the CODs by region."),
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

        has_past_data <- !predict_all & input$past_years < 2020

        past_data <- if (has_past_data) {
            cod_measures_long %>%
                filter(year(week_end) >= input$past_years & year(week_end) < 2020) %>%
                filter(measure == input$predict_cause)
        }

        ggpredict <- ggplot(data) +
            geom_line(aes(x = date, y = actual)) +
            geom_line(aes(x = date, y = upper), color = "blue") +
            geom_line(aes(x = date, y = lower), color = "blue") +
            geom_line(aes(x = date, y = mean), color = "red", linetype = "dashed") +
            geom_ribbon(aes(x = date, ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
            scale_y_continuous(labels = comma) +
            labs(title = paste(input$predict_cause, "expected death count in 2020"), subtitle = "showing 95% confidence interval", x = "Week", y = "Deaths")

        ggpredict <- if (has_past_data) {
            ggpredict + geom_line(data = past_data, aes(x = week_end, y = value))
        } else {
            ggpredict
        }

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
