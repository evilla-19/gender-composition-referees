########################################
########## libraries ###################
########################################


library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(chron)
library(plotly)
library(RColorBrewer)
library(ggalluvial)


########################################
########## Read in data ################
########################################

#setwd('/Users/benito/Documents/SourceData/GitHub/gender-composition-referees')
data = read.delim(file = 'EMBOJ_Track_record_2017_annotated.txt', header = TRUE, sep = '\t')

data$gender = factor(data$gender, levels = c('male', 'female', 'None'))

head(data)
# ## basic plot # 1: percentage of male/female referees overall
# df = as.data.frame(table(data$gender))

# g = ggplot(df, aes(x = '', y= Freq, fill = Var1))
# g = g + geom_bar(stat = 'identity', width = 1, position = 'fill') + theme_minimal()
# g = g + scale_fill_manual(values = c('orange', 'blue', 'darkgray'))

# g + coord_polar('y', start = 0)

# ## basic plot # 2: percentage of papers with all-male, all-female or mixed referee teams


# gender_balance_by_ms = data %>% group_by(Manuscript) %>%
# summarise(total = n(), male_count = sum(gender == 'male'), female_count = sum(gender == 'female'), none_count = sum(gender == 'None')) %>%
# mutate(perc_male = male_count / total, perc_female = female_count / total, perc_none = none_count / total)


# head(gender_balance_by_ms)

# all_male = table(gender_balance_by_ms$perc_male)['1']
# all_female = table(gender_balance_by_ms$perc_female)['1']
# mixed = length(gender_balance_by_ms$total) - all_male - all_female

# df2 = data.frame(Var1 = c('all_male', 'all_female', 'mixed'), Freq = c(all_male, all_female, mixed))

# g = ggplot(df2, aes(x = '', y= Freq, fill = Var1))
# g = g + geom_bar(stat = 'identity', width = 1) + theme_minimal()
# g = g + scale_fill_manual(values = c('orange', 'blue', 'darkgray'))
# g
# g + coord_polar('y')


# ## basic plot # 3: subset the 'mixed' teams and see composition of those teams


# df3 = data %>% group_by(Manuscript, gender) %>% summarise(count = n())

# # mixed_df = gender_balance_by_ms %>% filter(perc_male < 1) %>% filter(perc_female < 1)
# theme_settings = theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank())

# ggplot(df3, aes(x = Manuscript, y = count, fill = gender)) + geom_bar(stat = 'identity', width = 0.3, position = 'fill') + theme_void() + scale_fill_manual(values = c('blue', 'orange', 'darkgray')) + theme_settings 

# df3$gender = factor(df3$gender, levels = c('male', 'female', 'None')) ## reorder factor

# # stacked area plot
# ggplot(df3, aes(x = as.numeric(Manuscript), y = count, fill = gender)) + geom_area(alpha = 0.5, position = 'fill') + scale_fill_manual(values = c('blue', 'orange', 'darkgray')) + theme_settings 

# # stacked pie chart
# ggplot(df3, aes(x = as.numeric(Manuscript), y = count, fill = gender)) + geom_area(alpha = 0.5, position = 'fill', width = 0.01) + scale_fill_manual(values = c('blue', 'orange', 'darkgray')) + theme_settings + coord_polar('y', start = 0)



# # df4 = df3 %>% group_by(Manuscript) %>% mutate(total = sum(count), perc = (count/total)*100) # calculating percentage, does not really change much



########################################
########## UI layout ###################
########################################

ui = shinyUI(
  fluidPage(theme = "styles.css",
    tags$div(class = 'top-panel',
      h1(class = 'title-top', 'Visualize EMBO J referee gender distribution')
              ),
    # titlePanel('Referee gender distribution'),
      tags$div(class = 'main-title',
        wellPanel(class = 'title-well',
          img(class = 'logo', src = 'EMBO_logo_RGBblack_outlined.gif', height = 60, width = 100),
          h1("Referee gender distribution for 2017")
            )),
      sidebarLayout(
        sidebarPanel(width = 4,class = 'side-panel',
          h1('An example app for visualizing referee gender distribution'),
          
          br(), br(), br(),  
          tags$span(class = 'glyphicon glyphicon-info-sign'),
          p('This is small demo app to see the kinds of visualizations we could develop for referee team gender composition. It is based on the internship project from Silvia La Porta and extended by Eva.'),

          br(), br(),
          p(class = 'main-conclusions', 'The general conclusion for year 2017 is that there is a strong gender imbalance, with an ovepowering number of male referees overall, as well as a higher proportion of male-only referee teams. Although the biggest proportion of referee teams was mixed, within those, most of the teams were imbalanced with a clear shift towards male-dominated teams. The fate of those manuscripts is depicted below:'),

          br(), br(),

          wellPanel(class = 'alluvium',
            plotOutput(outputId = 'alluvium')
                  ),
          # verbatimTextOutput(outputId = 'alluvium'),

          tags$div(class = 'info-div',  
            br(), br(), br(),  
            tags$span(class = 'glyphicon glyphicon-envelope'),
            p('   If you are interested in this app, please contact Eva at '),
            a(href = 'mailto:eva.benito@embo.org', 'eva.benito@embo.org')
                  ),

            tags$div(class = 'bottom-side-panel',
              p('Developed with'),
              img(src = 'shinylogo.jpeg', width = 40, height = 40, alt = 'shiny logo'),
              p('and'),
              img(src = 'R_logo.png', width = 40, height = 35, alt = 'R logo')
                    ), 

            tags$div(class = 'slider-input',
              wellPanel(
                sliderInput(
                  inputId = 'animation',
                  label = 'Please select how many papers to visualize for the two plots on the right',
                  min = 10,
                  value = 100,
                  max = 351,
                  # step = 10,
                  # animate = animationOptions(interval = 300, loop = TRUE)
                            )
                        )
                      )
        ),
        mainPanel(width = 8,
          wellPanel(class = 'main-panel',
          column(
            width = 6,
            h3('Percentage of male/female referees overall'),
            plotOutput(outputId = 'global_gender')
            ),
          column(
            width = 6,
            h3('Percentage of male-only/female-only/mixed referee teams'),
            plotOutput(outputId = 'perc_teams')
          ),
          column(
            width = 6,
            h3('Gender distribution of referee teams - stacked bar plot'),
            plotOutput(outputId = 'perc_gender_indiv_stacked')          
          ),
          column(
            width = 6,
            h3('Gender distribution of referee teams - stacked pie chart'),
            plotOutput(outputId = 'perc_gender_indiv_pie')
          )
          )
        )
      )
    ))
    # fluidRow(class = 'side-container',
    #   column(
    #     width = 4,
        
    #         ),
    # fluidRow(class = 'main-container',
    #   h1(class = 'title', 'Basic statistics for 2017'),
    #   column(width = 4,
    #     h3('Percentage of male/female referees overall'),
    #     wellPanel(
    #       plotOutput(outputId = 'global_gender')
    #               )
    #       ),
    #   column(width = 4,
    #     h3('Percentage of male-only/female-only/mixed referee teams'),
    #     wellPanel(
    #       plotOutput(outputId = 'perc_teams')
    #               )
    #       ),
    #   column(width = 4,
    #     h3('Gender distribution of referee teams'),
    #     wellPanel(
    #       plotOutput(outputId = 'perc_gender_indiv')
    #             )
    # )
# )))

########################################
########## Server ######################
########################################

server = function(input, output) {

    ##### Block 1, for graph # 1
    df = as.data.frame(table(data$gender))
    colnames(df)[1] = 'gender'

    output$global_gender = renderPlot(
      {
        g = ggplot( df, aes(x = '', y= Freq, fill = gender)) +
                    geom_bar(stat = 'identity', width = 1, position = 'fill') + 
                    theme_minimal() + 
                    scale_fill_manual(values = c('darkblue', 'orange', 'darkgray')) +
                    coord_polar('y', start = 0) +
                    xlab('') + ylab('') + theme_void()
      g
      }
                                      )
    ##### Block 2, for graph # 2
    gender_balance_by_ms = data %>% group_by(Manuscript) %>%
    summarise(total = n(), male_count = sum(gender == 'male'), female_count = sum(gender == 'female'), none_count = sum(gender == 'None')) %>%
    mutate(perc_male = male_count / total, perc_female = female_count / total, perc_none = none_count / total)

    all_male = table(gender_balance_by_ms$perc_male)['1']
    all_female = table(gender_balance_by_ms$perc_female)['1']
    mixed = length(gender_balance_by_ms$total) - all_male - all_female

    df2 = data.frame(gender = c('all_male', 'all_female', 'mixed'), Freq = c(all_male, all_female, mixed))
    colnames(df2)[1] = 'gender'
    df2$gender = factor(df2$gender, levels = c('all_male', 'all_female', 'mixed'))

    output$perc_teams = renderPlot(
      {
        g = ggplot(df2, aes(x = '', y= Freq, fill = gender))
        g = g + geom_bar(stat = 'identity', width = 1) + theme_minimal()
        g = g + scale_fill_manual(values = c('darkblue', 'orange', 'darkgray'))
        g
        g + coord_polar('y') +  xlab('') + ylab('') + theme_void()
      }
                                  )

    ##### Block 3, for graph # 3
    df3 = data %>% group_by(Manuscript, gender) %>% summarise(count = n())

    main_data = reactive({
            df3[1:input$animation , ]
                  })


    # mixed_df = gender_balance_by_ms %>% filter(perc_male < 1) %>% filter(perc_female < 1)
    theme_settings = theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank())  

    output$perc_gender_indiv_stacked = renderPlot(
      {
        ggplot(main_data(), aes(x = as.numeric(Manuscript), y = count, fill = gender)) + geom_bar( position = 'fill', width = 0.1, stat = 'identity') + scale_fill_manual(values = c('darkblue', 'orange', 'darkgray')) + theme_settings + xlab('Manuscript')
      }
                                          )


    ##### Block 4, for graph # 4
    output$perc_gender_indiv_pie = renderPlot(
      {
        ggplot(main_data(), aes(x = as.numeric(Manuscript), y = count, fill = gender)) + geom_bar(stat = 'identity', width = 1, position = 'fill') + scale_fill_manual(values = c('darkblue', 'orange', 'darkgray')) + theme_settings + coord_polar('y', start = 0) + xlab('Manuscript') + ylab('')
      }
                                          )

    ##### Block 5, for graph # 5, which goes on the sidebarpanel

    # renderPrint(head(data))
    final_data_alluvium = data %>% group_by(Manuscript) %>% 
    mutate(male_count = sum(gender == 'male'), female_count = sum(gender == 'female'), none_count = sum(gender == 'None'), total= n(), perc_male = (male_count / total) * 100, perc_female = (female_count/ total) * 100) %>% 
    mutate(team_composition = ifelse(perc_male == 100, 'male_only', ifelse(perc_female == 100, 'female_only', 'mixed'))) %>% group_by(team_composition, Final.Decision.Type) %>%
    select(team_composition, Final.Decision.Type) %>%
    mutate(freq = n()) %>% unique()

    print(final_data_alluvium)

    output$alluvium = renderPlot({
      ggplot(final_data_alluvium,
      aes(y = freq, axis1 = team_composition, axis2 = Final.Decision.Type)) +
      geom_alluvium(aes(fill = Final.Decision.Type), width = 1/12) +
      geom_stratum(width = 1/12, fill = 'lightgray', color = 'black') +
      geom_label(stat = "stratum", label.strata = TRUE) +
      scale_x_discrete(limits = c("referee group composition", "final decision"), expand = c(.05, .05)) +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      theme_minimal() +
            theme(legend.position = 'bottom', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank())
    })

}




########################################
########## App #########################
########################################



shinyApp(ui = ui, server = server)

# shiny::runApp(display.mode="showcase")


