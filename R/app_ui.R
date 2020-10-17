#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # List the first level UI elements here 
    bs4DashPage(
      title = "ShinySaurus",
      sidebar_collapsed = FALSE,
      
      # navigation bar
      navbar = bs4DashNavbar(
        skin = "dark",
        status = "primary"
      ),
      
      # left sidebar
      sidebar = bs4DashSidebar(
        skin = "dark",
        status = "primary",
        title = "ShinySaurus",
        brandColor = "primary",
        #src = "some_image.png",
        elevation = 3,
        opacity = 0.8,
        
        # left sidebar menu
        bs4SidebarMenu(
          bs4SidebarMenuItem(
            "Welcome",
            tabName = "welcome",
            icon = 'info'
          ),
          bs4SidebarMenuItem(
            "Explore",
            tabName = "explore",
            icon = 'search'
          )
        )
      ),
      
      # main body
      body = bs4DashBody(
        bs4TabItems(
          bs4TabItem(
            tabName = "welcome",
            mod_welcome_ui("welcome_ui_1")
          ),
          bs4TabItem(
            tabName = "explore",
            mod_explore_ui("explore_ui_1")
          )
        )
      ),
      
      # footer
      footer = bs4DashFooter(
        copyrights = a(
          href = "https://r-podcast.org",
          target = "_blank",
          "The R-Podcast"
        ),
        right_text = "2020"
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shinysaurus'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

