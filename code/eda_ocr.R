library(tesseract)
library(magick)


# hospital chart only:
url2 <- "https://www.health.gov.au/resources/current-covid-19-cases-in-hospitals-and-intensive-care-units-icus"

# Set tesseract to only detect numbers
num_only <- tesseract::tesseract(
  options = list(tessedit_char_whitelist = c(".0123456789 "))
)


image <- "hosp_chart_raw.png"

im_proc <- image_read(image) %>%
  image_resize("2000") %>%
  image_convert(colorspace = 'gray') %>%
  image_trim() %>% 
  image_crop("800x555+210+720") %>%  # width, height, , x start, y start # "950x555+60+720", just the graph: "800x555+210+720"
  image_trim() #%>% 
  # image_transparent(color = "white", fuzz=00) %>% 
  # image_background("white") %>% 
  # image_negate() %>%
  # image_morphology(method = "Thinning", kernel = "Rectangle:20x1") %>%
  # image_negate() #%>% # back to white
  tesseract::ocr(engine = num_only)

  

dat <-   
  image_data(image_negate(im_proc))[1,,] %>%
  as.data.frame() %>%
  as_tibble() %>% 
  mutate(Row = 1:nrow(.)) %>%
  select(Row, everything()) %>%
  mutate_all(as.character) %>%
  gather(key = Column, value = value, 2:ncol(.)) %>%
  mutate(Column = as.numeric(gsub("V", "", Column)),
         Row = as.numeric(Row),
         value = ifelse(value == "00", NA, 1)) %>%
  filter(!is.na(value))


ggplot(data = dat, aes(x = Row, y = Column, colour = (Column < 300))) +
  geom_point() +
  theme(legend.position = "off") + 
  scale_y_continuous(trans = "reverse")



text <- ocr(image = "hosp_chart_raw.png")
