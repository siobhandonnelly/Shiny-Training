library(shiny)
library(shiny.router)

home_page <- div(
  titlePanel("Dashboard"),
  p("This is a dashboard page")
)

work_page <- div(
  titlePanel("Work"),
  p("This is a work page")
)

Study_page <- div(
  titlePanel("Study"),
  p("This is a Study page")
)

Personal_Charecteristics_page <- div(
  titlePanel("Personal_Charecteristics"),
  p("This is a Personal Charecteristics")
)

router <- make_router(
  route("/", home_page),
  route("Work", work_page),
  route("Study", Study_page),
  route("Personal_Charecteristics", Personal_Charecteristics_page)
)