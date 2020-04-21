#
# Interactive app for data filtering
#
# (c) 2020 Jean-Olivier Irisson, GNU General Public License v3

library("shiny")
library("dplyr")
library("ggplot2")
library("stringr")
library("slider")
library("parallel")

# plot theme
theme_set(
  theme_light(12) +
  theme(
    panel.background=element_blank(),
    panel.grid=element_blank(),
    strip.background=element_rect(fill="grey90", colour="grey70"),
    strip.text=element_text(colour="black"),
    axis.title.y=element_blank()
  )
)

# load data
projects <- list.files("projs") %>% str_replace(".rds", "")
current_profiles <- c()

# Define UI for application
ui <- function(request) {
  fluidPage(
    tags$script(HTML("$(function(){
        $(document).keyup(function(e) {
        if (e.which == 85 && e.altKey) {
          $('#update').click()
        }
      });
    })")),
    tags$style(HTML("
      body { font-family: Arial, sans-serif; font-size: 12px }
      .help-block { font-size: 0.9em; font-style: italic }
    ")),
    fluidRow(
      column(3,
        h3("UVP profiles check"),
        # redraw_button("Update"),
        actionButton("update", "Update [Alt+U]", width="100%", class="btn btn-primary"),
        div(style="height: 10px"),

        selectInput(inputId="proj", label="Project", choices=projects,  width="100%"),
        uiOutput("profiles_selector"),
        helpText("NB: You can type in the boxes to filter projects or profiles."),

        sliderInput(inputId="quant", label="Quantile", width="100%",
                    value=0.7, min=0.5, max=0.95, step=0.05),
        helpText("Quantile of nb of particles to consider as correct; 0.5=median, 0.95=high."),

        sliderInput(inputId="window", label="Window size", width="100%",
                    value=10, min=2, max=30, step=1),
        helpText("Size of *one side* of the moving window on which the quantile and the spread around it are computed."),

        sliderInput(inputId="smult", label="Spread multiplier", width="100%",
                    value=3, min=1, max=8, step=0.2),
        sliderInput(inputId="qmult", label="Quantile multiplier", width="100%",
                    value=0.6, min=0.5, max=1, step=0.05),
        helpText(HTML("The <strong>spread</strong> multiplier is relevant for <strong>small</strong> numbers of particles, the <strong>quantile</strong> multiplier is relevant for <strong>large</strong> numbers of particles. The limit of erroneous values (blue line on the plots) is computed as</br>
    min(</br>
        quantile - spread * spread_multiplier,</br>
        quantile * quantile_multiplier</br>
    )</br>
  ")),

        div(style="height: 1px")
      ),

      column(9,
        # sliderInput(inputId="depths", label="Depth range on plot", width="100%",
        #             value=c(0,10000), min=0, max=7000, step=100),
        helpText("Use this slider to zoom over the y axis of the plots. You can move the handles and then move the blue bar directly to scan over the profile."),
        sliderInput(inputId="imgs", label=NULL, width="100%",
                    value=c(0,1000), min=0, max=1000, step=100),
        # uiOutput("imgs_selector"),
        plotOutput("profile_plot", height="auto"),
        div(style="height: 1px")
      )
    )
  )
}

# Define computational logic
server <- function(input, output, session) {
  message("START")

  read_project <- reactive({
    message("start - read profiles from ", input$proj)
    # if (length(input$proj) == 0) { stop("Please select at least one project") }
    out <- readRDS(paste0("projs/", input$proj, ".rds"))
    message("end - read profiles from project")
    out
  })

  output$profiles_selector <- renderUI({
    message("start - list profiles")
    profile_names <- names(read_project())
    out <- selectInput("profiles", label="Profiles", choices=profile_names, multiple=TRUE, width="100%")
    message("end - list profiles")
    out
  })

  extract_profiles <- eventReactive(input$update, {
  # extract_profiles <- reactive({
    message("start - extract selected profiles")
    proj <- read_project()
    # validate( need(length(input$profiles) > 0, "Select at least one profile and update.") )
    message("  ", paste0(input$profiles, collapse=","))
    out <- proj[input$profiles]
    message("end - extract selected profiles")
    out
  })

  compute_outliers <- eventReactive(input$update, {
  # compute_outliers <- reactive({
    message("start - compute outliers")

    # read settings locally, to be able to pass them as static variables
    # to the parallel subprocesses
    iquant  <- force(input$quant)
    iwindow <- force(input$window)
    ismult  <- force(input$smult)
    iqmult  <- force(input$qmult)
    # compute outliers in parallel
    out <- extract_profiles() %>%
      mclapply(function(p, quant=iquant, window=iwindow,
                           smult=ismult, qmult=iqmult) {
        mutate(p,
          mv_quant = slide_dbl(n, quantile, prob=quant, na.rm=TRUE, .before=window, .after=window),
          anom = abs(n - mv_quant),
          mv_anom = anom %>%
            slide_dbl(median, na.rm=TRUE, .before=window*2, .after=window*2) %>%
            slide_dbl(mean  , na.rm=TRUE, .before=window  , .after=window  ),
          # thresh = mv_quant - (smult * mv_anom),
          # thresh = mv_quant * qmult,
          thresh = pmin(mv_quant - (smult * mv_anom), mv_quant * qmult),
          thresh = ifelse(thresh < 0, 0, thresh)
        )
      }, mc.cores=3) %>%
      bind_rows(.id="profile")
    message("end - compute outliers")
    out
  })

  # update zoom slider
  observeEvent(input$update, {
    if (! setequal(input$profiles, current_profiles)) {
      message("start - update imgs slider")
      img_max <- sapply(extract_profiles(), function(x) {x$i[nrow(x)]}) %>% max()
      updateSliderInput(session, "imgs", value=c(0, img_max), max=img_max)
      current_profiles <<- input$profiles
      message("end - update imgs slider")
    }
  })

  get_plot_data <- reactive({
    message("start - get plot data")
    message("  range:", input$imgs[1], " ", input$imgs[2])
    out <- compute_outliers() %>%
      filter(i >= input$imgs[1], i <= input$imgs[2])
    message("end - get plot data")
    out
  })

  plot_height <- reactive({
    message("start - define plot height")
    unit_px_height <- 500
    n_plots <- unique(get_plot_data()$profile) %>% length()
    out <- ceiling(n_plots / 4) * unit_px_height + 50
    message("end - define plot height")
    out
  })

  output$profile_plot <- renderPlot({
    message("start - plot selected data")
    d <- get_plot_data()

    # make transparency and size depend on the number of plotted points
    n_img <- diff(input$imgs)
    alpha <- size <- 0.9 - 0.75 * (n_img / (n_img + 500))
    size <- size*1.5

    # compute proportion of images removed in this data
    stats <- d %>%
      group_by(profile) %>%
      summarise(n_tot=n(), n_out=sum(n<thresh)) %>%
      ungroup() %>%
      # make that into a nice label
      mutate(label=str_c(profile, " - ", n_out, "/", n_tot, " (", round(n_out/n_tot, 1)*100, "%)"))
    # prepare a labelling function for the plot
    stat_label <- function(df) {
      list(profile=left_join(df, stats, by="profile")$label)
    }
    out <- ggplot(d) +
      facet_wrap(~profile, scales="free", ncol=4, labeller=stat_label) +
      # decision boundary
      geom_path(aes(x=thresh, y=i), colour="dodgerblue", size=0.25, alpha=0.2) +
      # points
      geom_point(aes(x=n, y=i, colour=n<thresh, alpha=n<thresh), size=size, shape=16) +
      scale_colour_manual(guide="none", values=c("black", "red")) +
      scale_alpha_manual(guide="none", values=c(alpha, 0.95)) +
      # nice scales
      scale_y_reverse(labels=function(x) {ifelse(x<1000, paste0(x), paste0(x/1000, "k"))}) +
      labs(x="Nb of particles")
    message("end - plot selected data")
    out
  }, height=plot_height)
}

shinyApp(ui=ui, server=server)
