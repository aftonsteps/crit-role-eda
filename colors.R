## Colors
library(RColorBrewer)
library(rstudioapi)
library(fBasics)
library(grDevices)
library(ggplot2)

## Test data
## Test discrete colors
test_discrete <- function(pal) {
  test_data <- data.frame(val = rep(1, 9),
                          g = as.factor(seq(1, 9)))
  
  ggplot(data = test_data, aes(x = g, 
                               y = val, 
                               group = g,
                               fill = g)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = pal)
}

## Mollymauk Tealeaf
dark_purple <- c("#0a0124", "#12042e", "#000417", "#020721", "#10051c")
light_purple <- c("#afa4ca", "#af9dde", "#c89ae3")
v_light_purple <- c("#e9d1f3")
red <- c("#5b1a1e", "#590f20", "#540714")
green <- c("#40463e")
teal <- c("#172932")
light_green <- c("#9fbb96")
gold <- c("#d59252")
gray_blue <- c("#2e2c3c")
mollymauk_pal <- c("#5b1a1e", 
                   "#0a0124", 
                   "#afa4ca", 
                   "#e9d1f3",
                   "#e0e0eb",
                   "#003366",
                   "#172932", 
                   "#9fbb96", 
                   "#ffffe6")

jester_pal <- c("#0f1f3d",
                "#002b80",
                "#6699ff",
                "#cc00cc",
                "#ffccff",
                "#206040",
                "#d9f2e6",
                "#804000",
                "#f9ecf2")

mighty_nein <- c("#78a05a",
                 "#072d7f",
                 "#b36817",
                 #"#b34817", #old caleb
                 "#525530",
                 "#5882b8",
                 "#5b417d",
                 "#26806a",
                 "#535154",
                 "#0e0717",
                 "#c47e9d")

mighty_nein_medium <- c("#cadabe",
                        "#9ebbfa",
                        "#f3bea5",
                        "#d8dabe",
                        "#b8c9e0",
                        "#cabcdc",
                        "#b0e8da",
                        "#cccbcd",
                        "#c8b1e7",
                        "#dfb9ca") ## TODO get medium tone here

mighty_nein_light <- c("#f2f6ef",
                       "#e7eefe",
                       "#fcefe8",
                       "#f5f6ef",
                       "#edf2f7",
                       "#f2eef6",
                       "#ebf9f6",
                       "#f2f2f3",
                       "#f1ebf9",
                       "#efdce5")

mighty_nein_dark_to_light <- 
  as.data.frame(as.matrix(rbind(as.character(mighty_nein_light),
                                as.character(mighty_nein))))

mighty_nein_dark_to_medium <- 
  as.data.frame(as.matrix(rbind(as.character(mighty_nein_medium),
                                as.character(mighty_nein))))
colnames(mighty_nein_dark_to_light) <-
  colnames(mighty_nein_dark_to_medium) <-
  c("Travis",
    "Marisha",
    "Liam",
    "Sam",
    "Laura",
    "Taliesin",
    "Taliesin2",
    "Ashley",
    "Matt",
    "Taliesin3")

discrete_mighty_nein_colors <-
  c("Travis0" = as.character(mighty_nein_dark_to_medium$Travis[1]),
    "Travis1" = as.character(mighty_nein_dark_to_medium$Travis[2]),
    "Marisha0" = as.character(mighty_nein_dark_to_medium$Marisha[1]),
    "Marisha1" = as.character(mighty_nein_dark_to_medium$Marisha[2]),
    "Liam0" = as.character(mighty_nein_dark_to_medium$Liam[1]),
    "Liam1" = as.character(mighty_nein_dark_to_medium$Liam[2]),
    "Sam0" = as.character(mighty_nein_dark_to_medium$Sam[1]),
    "Sam1" = as.character(mighty_nein_dark_to_medium$Sam[2]),
    "Laura0" = as.character(mighty_nein_dark_to_medium$Laura[1]),
    "Laura1" = as.character(mighty_nein_dark_to_medium$Laura[2]),
    "Taliesin0" = as.character(mighty_nein_dark_to_medium$Taliesin[1]),
    "Taliesin1" = as.character(mighty_nein_dark_to_medium$Taliesin[2]),
    "Taliesin3" = as.character(mighty_nein_dark_to_medium$Taliesin3[1]),
    "Taliesin4" = as.character(mighty_nein_dark_to_medium$Taliesin3[2]),
    "Ashley0" = as.character(mighty_nein_dark_to_medium$Ashley[1]),
    "Ashley1" = as.character(mighty_nein_dark_to_medium$Ashley[2]))

discrete_mighty_nein_colors_by_char <-
  c("Fjord0" = as.character(mighty_nein_dark_to_medium$Travis[1]),
    "Fjord1" = as.character(mighty_nein_dark_to_medium$Travis[2]),
    "Beau0" = as.character(mighty_nein_dark_to_medium$Marisha[1]),
    "Beau1" = as.character(mighty_nein_dark_to_medium$Marisha[2]),
    "Caleb0" = as.character(mighty_nein_dark_to_medium$Liam[1]),
    "Caleb1" = as.character(mighty_nein_dark_to_medium$Liam[2]),
    "Nott0" = as.character(mighty_nein_dark_to_medium$Sam[1]),
    "Nott1" = as.character(mighty_nein_dark_to_medium$Sam[2]),
    "Jester0" = as.character(mighty_nein_dark_to_medium$Laura[1]),
    "Jester1" = as.character(mighty_nein_dark_to_medium$Laura[2]),
    "Molly0" = as.character(mighty_nein_dark_to_medium$Taliesin[1]),
    "Molly1" = as.character(mighty_nein_dark_to_medium$Taliesin[2]),
    "Caduceus0" = as.character(mighty_nein_dark_to_medium$Taliesin3[1]),
    "Caduceus1" = as.character(mighty_nein_dark_to_medium$Taliesin3[2]),
    "Yasha0" = as.character(mighty_nein_dark_to_medium$Ashley[1]),
    "Yasha1" = as.character(mighty_nein_dark_to_medium$Ashley[2]))

single_mighty_nein_colors <-
  c("Fjord" = as.character(mighty_nein_dark_to_medium$Travis[2]),
    "Beau" = as.character(mighty_nein_dark_to_medium$Marisha[2]),
    "Caleb" = as.character(mighty_nein_dark_to_medium$Liam[2]),
    "Nott" = as.character(mighty_nein_dark_to_medium$Sam[2]),
    "Jester" = as.character(mighty_nein_dark_to_medium$Laura[2]),
    "Molly" = as.character(mighty_nein_dark_to_medium$Taliesin[2]),
    "Caduceus" = as.character(mighty_nein_dark_to_medium$Taliesin3[2]),
    "Yasha" = as.character(mighty_nein_dark_to_medium$Ashley[2]))


test_discrete(mollymauk_pal)
test_discrete(jester_pal)
test_discrete(mighty_nein)
test_discrete(mighty_nein_light)
