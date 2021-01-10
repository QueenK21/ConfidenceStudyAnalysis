root_dir = "/data"
setwd(root_dir)

library(readxl)
library(ggplot2)
library(ggnetwork)

my_data = read_excel("error_dist.xlsx")
                
#df = data.frame(my_data)

ggplot(my_data, aes(x = Bin, y = Freq4)) + 
  geom_bar(stat = "identity", fill = "#808080") +
  theme_light() + 
  ylim(-0.03, 0.14)+
  scale_x_continuous(breaks = c(0,45, 90, -45)) +
  coord_polar(theta = "x", start = 135.17)+
  geom_hline(yintercept = seq(0, 0.2, by = 0.05), colour = "black", size = 0.3)+
  geom_vline(xintercept = c(0, 45, 90, -45), colour = "black", size = 0.3)+
  theme(legend.position="none", panel.grid=element_blank())

ggsave("histogram6.jpg", dpi = 200)
