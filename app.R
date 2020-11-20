library(shiny)
library(xts)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(plotly)
library(gplots)
library(ggcorrplot)
library(scales)

source("utils.R")

# trim points at beginning
# script to create data

cod_measures <- readRDS("data/cod_measures.rds")
cod_measures_long <- readRDS("data/cod_measures_long.rds")
cod_granger <- readRDS("data/cod_granger.rds")
cod_predictions <- readRDS("data/cod_predictions.rds")

cod_causes <- levels(cod_measures_long$measure)
non_covid_causes <- cod_causes[!(cod_causes %in% c("covid", "covid_multiple"))]
create_palette <- colorRampPalette(brewer.pal(n = 8, name = "Dark2"))
cod_palette <- create_palette(length(cod_causes))


ui <- fluidPage(
    titlePanel("Causes of Death"),
    p("Here we explore the causes of death from the CDC."),
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
    ),
    h2("Causes of Death in 2020: Predicted vs. Actual", style = "margin-top: 50px"),
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
}

shinyApp(ui = ui, server = server)
