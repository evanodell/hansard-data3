

x <- debate %>% filter(year == 2020)

# x$speech


y <- hunspell::hunspell(x$speech, dict = hunspell::dictionary("en_GB"))

"uk.org.publicwhip/debate/1983-03-10a.980.0"


"itisproducing"