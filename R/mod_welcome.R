#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import bs4Dash
mod_welcome_ui <- function(id){
  ns <- NS(id)
  tagList(
    # the big jumbotrom!
    fluidRow(
      col_12(
        bs4Jumbotron(
          title = "Welcome to ShinySaurus!",
          lead = includeMarkdown(app_sys("app", "docs", "welcome.md")),
          btn_name = "Visit GitHub Repository",
          href = "https://github.com/rpodcast/shinysaurus"
        )
      )
    ),
    
    # Acknowledgments
    fluidRow(
      col_12(
        bs4Callout(
          title = "Thank you!",
          width = 12,
          status = "primary",
          "This application would not be possible without the brilliant R Community!"
        )
      )
    ),
    
    fluidRow(
      col_4(
        bs4UserCard(
          title = "Eric Nantz",
          subtitle = "Application Developer",
          status = "info",
          width = 12,
          src = "www/pic_with_r_logo_github.jpg",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "The R-Podcast",
              type = "action",
              src = "https://r-podcast.org"
            ),
            bs4ListGroupItem(
              "Shiny Developer Series",
              type = "action",
              src = "https://shinydevseries.com"
            ),
            bs4ListGroupItem(
              "R-Weekly Highlights Podcast",
              type = "action",
              src = "https://rweekly.fireside.fm"
            ),
            bs4ListGroupItem(
              "Twitter: @thercast",
              type = "action",
              src = "https://twitter.com/thercast"
            ),
            bs4ListGroupItem(
              "Github: rpodcast",
              type = "action",
              src = "https://github.com/rpodcast"
            )
          )
        )
      ),
      col_4(
        bs4UserCard(
          title = "Elio Campitelli",
          subtitle = "metamer package author",
          status = "info",
          width = 12,
          src = "www/elio_campitelli_pic.jpeg",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "Statistical Metamerism",
              type = "action",
              src = "https://eliocamp.github.io/codigo-r/en/2019/01/statistical-metamerism"
            ),
            bs4ListGroupItem(
              "Twitter: @d_olivaw",
              type = "action",
              src = "https://twitter.com/d_olivaw"
            ),
            bs4ListGroupItem(
              "Github: eliocamp",
              type = "action",
              src = "https://github.com/eliocamp"
            )
          )
        )
      ),
      col_4(
        bs4UserCard(
          title = "Steph Locke",
          subtitle = "datasauRus package author",
          status = "info",
          width = 12,
          src = "www/stephlocke.jpg",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "Locke Data",
              type = "action",
              src = "https://itsalocke.com"
            ),
            bs4ListGroupItem(
              "Twitter: @theStephLocke",
              type = "action",
              src = "https://twitter.com/theStephLocke"
            ),
            bs4ListGroupItem(
              "Github: stephlocke",
              type = "action",
              src = "https://github.com/stephlocke"
            )
          )
        )
      )
    ),
    fluidRow(
      col_3(
        bs4UserCard(
          title = "David Granjon",
          subtitle = "bs4Dash package author",
          status = "info",
          width = 12,
          src = "www/david_granjon.jpeg",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "RinteRface",
              type = "action",
              src = "https://rinterface.com"
            ),
            bs4ListGroupItem(
              "Twitter: @divadnojnarg",
              type = "action",
              src = "https://twitter.com/divadnojnarg"
            ),
            bs4ListGroupItem(
              "Github: DivadNojnarg",
              type = "action",
              src = "https://github.com/DivadNojnarg"
            )
          )
        )
      ),
      col_3(
        bs4UserCard(
          title = "Colin Fay",
          subtitle = "golem package author",
          status = "info",
          width = 12,
          src = "www/colin_fay.jpeg",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "Engineering Production Shiny Apps",
              type = "action",
              src = "https://engineering-shiny.org/"
            ),
            bs4ListGroupItem(
              "Blog",
              type = "action",
              src = "https://colinfay.me"
            ),
            bs4ListGroupItem(
              "Twitter: @_colinfay",
              type = "action",
              src = "https://twitter.com/_colinfay"
            ),
            bs4ListGroupItem(
              "Github: ColinFay",
              type = "action",
              src = "https://github.com/ColinFay"
            )
          )
        )
      ),
      col_3(
        bs4UserCard(
          title = "Carson Sievert",
          subtitle = "plotly package author",
          status = "info",
          width = 12,
          src = "www/carson_seivert.jpeg",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "Plotly-R Book",
              type = "action",
              src = "https://plotly-r.com"
            ),
            bs4ListGroupItem(
              "Twitter: @cpsievert",
              type = "action",
              src = "https://twitter.com/cpsievert"
            ),
            bs4ListGroupItem(
              "Github: cpsievert",
              type = "action",
              src = "https://github.com/cpsievert"
            )
          )
        )
      ),
      col_3(
        bs4UserCard(
          title = "John (JP) Coene",
          subtitle = "waiter & tippy package author",
          status = "info",
          width = 12,
          src = "www/john_coene.jpeg",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "Personal Site",
              type = "action",
              src = "https://john-coene.com/"
            ),
            bs4ListGroupItem(
              "Twitter: @jdatap",
              type = "action",
              src = "https://twitter.com/jdatap"
            ),
            bs4ListGroupItem(
              "Github: JohnCoene",
              type = "action",
              src = "https://github.com/JohnCoene"
            )
          )
        )
      )
    )
  )
}
    
#' welcome Server Function
#'
#' @noRd 
mod_welcome_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_ui_1")
    
## To be copied in the server
# callModule(mod_welcome_server, "welcome_ui_1")
 
