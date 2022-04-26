library(pacman)
p_load(tidyverse, magrittr)

# Load the data
samples = read_csv("function_020samplestrat.csv")

samples %<>% filter(attention == "puppy")

samples %>% filter(seaName == "rough_1") %>% ggplot(aes(x = `x`,
                                                        y = sample_num,
                                                        colour = sample_num,
                                                        group = sample_num)) +
  geom_jitter() +
  # Peak of function
  stat_function(fun = function(x) 0.5*dbeta(x, 131, 21), 
                colour = "red", size = 2, linetype = "dashed")

samples %>% filter(seaName == "rough_2") %>% ggplot(aes(x = `x`,
                                                        y = sample_num,
                                                        colour = sample_num,
                                                        group = sample_num)) +
  geom_jitter() +
  # Peak of function
  # geom_vline(xintercept = 3/14) +
  stat_function(fun = function(x) 0.5*dbeta(x, 31, 111), 
                colour = "red", size = 2, linetype = "dashed")

samples %>% filter(seaName == "rough_3") %>% ggplot(aes(x = `x`,
                                                        y = sample_num,
                                                        colour = sample_num,
                                                        group = sample_num)) +
  geom_jitter() +
  # Peak of function
  stat_function(fun = function(x) 0.5*dbeta(x, 141, 121), 
                colour = "red", size = 2, linetype = "dashed")

samples %>% filter(seaName == "rough_4") %>% ggplot(aes(x = `x`,
                                                        y = sample_num,
                                                        colour = sample_num,
                                                        group = sample_num)) +
 geom_jitter() +
  # Peak of function
  stat_function(fun = function(x) 0.5*dbeta(x, 131, 41), 
                colour = "red", size = 2, linetype = "dashed")

samples %>% filter(grepl("rough", seaName)) -> rough_samples

rough_samples %<>% mutate(distance_from_expectation = case_when(
  seaName == "rough_1" ~ abs(x - (13/15)),
  seaName == "rough_2" ~ abs(x - (3/14)),
  seaName == "rough_3" ~ abs(x - (14/26)),
  seaName == "rough_4" ~ abs(x - (13/17))))

rough_samples %>% ggplot(aes(x = seaName,
                             group = seaName, 
                             fill = seaName, 
                             colour = seaName,
                             y = distance_from_expectation)) +
  geom_boxplot(alpha = 0.2) +
  geom_jitter(size = 0.5) +
  facet_grid(cb~sample_num)




samples %>% filter(seaName == "smooth_2") %>% ggplot(aes(x = sample_num,
                                                        y = `x`,
                                                        group = name)) +
  geom_line()


preds = read_csv("function_020preds.csv")

preds %>% filter(seaName != "test") %>%
  ggplot(aes(x = x, y = y, group = name)) +
  geom_line(width = 0.5, alpha = 0.2) +
  facet_wrap(vars(seaName))

argmax_r1 = dbeta(13/15, 131, 21)
argmax_r2 = dbeta(3/14, 31, 111)
argmax_r3 = dbeta(14/26, 141, 121)
argmax_r4 = dbeta(0.78, 131, 41)

argmax_s1 = dbeta(0, 1, 3)
argmax_s2 = dbeta(2 / 3, 3, 2)
argmax_s3 = dbeta(1, 2.5, 1)
argmax_s4 = dbeta(1/4, 1.5, 2.5)


vlines= c(13/15,3/14, 14/26, 0.78, 0, 2/3,1, 1/4, NA)


true_func = function(x, seaName){
  val = case_when(
        seaName == "rough_1" ~ (dbeta(x, 131, 21) * 2/3) / argmax_r1 + 0.08333,
        seaName == "rough_2" ~ (dbeta(x, 31, 111) * 2/3) / argmax_r2 + 0.16666,
        seaName == "rough_3" ~ (dbeta(x, 141, 121) * 2/3) / argmax_r3 + 0.243333,
        seaName == "rough_4" ~ (dbeta(x, 131, 40) * 2/3) / argmax_r4 + 0.3333,
        seaName == "smooth_1" ~ (dbeta(x, 1, 3) * 2/3) / argmax_s1 + 0.08333,
        seaName == "smooth_2" ~ (dbeta(x, 3, 2) * 2/3) / argmax_s2 + 0.16666,
        seaName == "smooth_3" ~ (dbeta(x, 2.5, 1) * 2/3) / argmax_s3 + 0.243333,
        seaName == "smooth_4" ~ (dbeta(x, 1.5, 2.5) * 2/3) / argmax_s4 + 0.3333,
        seaName == "flat" ~ 0.5,
        seaName == "test" ~ (0.4 * x + 0.2))
  return(val)
}

library(ggpubr)

func_list = c("rough_1", "rough_2", "rough_3", "rough_4",
              "smooth_1", "smooth_2", "smooth_3", "smooth_4", "flat")

plot_list = list()
plot <- preds %>% filter(seaName == "rough_1") %>%
    ggplot(aes(x = x, y = y, group = name)) +
    geom_line(size = 0.5, alpha = 0.2) +
    stat_function(fun = function(x){true_func(x, "rough_1")}, ,
                  colour = "red", size = 1, linetype = "dashed") +
    ggtitle("rough_1")
plot_list = append(plot_list, list(plot))

plot <- preds %>% filter(seaName == "rough_2") %>%
  ggplot(aes(x = x, y = y, group = name)) +
  geom_line(size = 0.5, alpha = 0.2) +
  stat_function(fun = function(x){true_func(x, "rough_2")}, ,
                colour = "red", size = 1, linetype = "dashed") +
  ggtitle("rough_2")
plot_list = append(plot_list, list(plot))

plot <- preds %>% filter(seaName == "rough_3") %>%
  ggplot(aes(x = x, y = y, group = name)) +
  geom_line(size = 0.5, alpha = 0.2) +
  stat_function(fun = function(x){true_func(x, "rough_3")}, ,
                colour = "red", size = 1, linetype = "dashed") +
  ggtitle("rough_3")
plot_list = append(plot_list, list(plot))

plot <- preds %>% filter(seaName == "rough_4") %>%
  ggplot(aes(x = x, y = y, group = name)) +
  geom_line(size = 0.5, alpha = 0.2) +
  stat_function(fun = function(x){true_func(x, "rough_4")}, ,
                colour = "red", size = 1, linetype = "dashed") +
  ggtitle("rough_4")
plot_list = append(plot_list, list(plot))

plot <- preds %>% filter(seaName == "smooth_1") %>%
  ggplot(aes(x = x, y = y, group = name)) +
  geom_line(size = 0.5, alpha = 0.2) +
  stat_function(fun = function(x){true_func(x, "smooth_1")}, ,
                colour = "red", size = 1, linetype = "dashed") +
  ggtitle("smooth_1")
plot_list = append(plot_list, list(plot))

plot <- preds %>% filter(seaName == "smooth_2") %>%
  ggplot(aes(x = x, y = y, group = name)) +
  geom_line(size = 0.5, alpha = 0.2) +
  stat_function(fun = function(x){true_func(x, "smooth_2")}, ,
                colour = "red", size = 1, linetype = "dashed") +
  ggtitle("smooth_2")
plot_list = append(plot_list, list(plot))

plot <- preds %>% filter(seaName == "smooth_3") %>%
  ggplot(aes(x = x, y = y, group = name)) +
  geom_line(size = 0.5, alpha = 0.2) +
  stat_function(fun = function(x){true_func(x, "smooth_3")}, ,
                colour = "red", size = 1, linetype = "dashed") +
  ggtitle("smooth_3")
plot_list = append(plot_list, list(plot))

plot <- preds %>% filter(seaName == "smooth_4") %>%
  ggplot(aes(x = x, y = y, group = name)) +
  geom_line(size = 0.5, alpha = 0.2) +
  stat_function(fun = function(x){true_func(x, "smooth_4")}, ,
                colour = "red", size = 1, linetype = "dashed") +
  ggtitle("smooth_4")
plot_list = append(plot_list, list(plot))

plot <- preds %>% filter(seaName == "flat") %>%
  ggplot(aes(x = x, y = y, group = name)) +
  geom_line(size = 0.5, alpha = 0.2) +
  stat_function(fun = function(x){true_func(x, "flat")}, ,
                colour = "red", size = 1, linetype = "dashed") +
  ggtitle("flat")
plot_list = append(plot_list, list(plot))

ggarrange(plotlist = plot_list, nrow = 3, ncol = 3)


preds %<>% mutate(t = true_func(x, seaName), err = abs(y - t))

preds %>% group_by(name, seaName) %>% summarise(sum_err = sum(abs(y - t)), num_preds = max(pred_num)) -> sum_preds

sum_preds %<>% mutate(avg_error = sum_err/num_preds)  

sum_preds %>% filter(seaName != "test") %>% 
  ggplot(aes(x = seaName, y = avg_error, fill = seaName, colour = seaName)) + 
  # geom_jitter(size = 1) + 
  geom_boxplot(alpha = 0.2)


sum_preds %<>% mutate(isRough = grepl("rough", seaName))

p_load(lme4, lmerTest)

sum_preds %>% filter(seaName != "test") %>% lmer(avg_error ~ isRough + seaName + (1|name), data = .) %>% summary








# # # # # # # # # # # #
# Clustering analysis #
# # # # # # # # # # # #

library(mclust)

samples<-read_csv("function_020samplestrat.csv")
samples %>% filter(seaName == "flat") %>% select(name, sample_num, x) %>% spread(key = sample_num, value = x) -> clust_data

name <- clust_data$name
X <- clust_data[,-1]
clPairs(X, name)
BIC <- mclustBIC(X)
# plot(BIC)
# summary(BIC)
mod1 <- Mclust(X, x = BIC)
table(mod1$classification)
plot(mod1, what = "classification")
clust_data$classification <- mod1$classification

samples <- merge(samples, clust_data %>% select(name, classification), by.x = "name", by.y = "name") %>% rename(flat_c = classification)




clust_data %>% gather(key = "sample_num", value = "x", c(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`)) -> clust_gather

clust_gather %>% ggplot(aes(x = sample_num,
                            y = `x`, 
                            group = as.factor(classification), 
                            color = as.factor(classification))) + 
  stat_summary(fun = mean, geom = "line", size = 2) + 
  geom_line(alpha = 0.2, inherit.aes = "false", aes(x = sample_num, y = `x`, group = name, color = as.factor(classification))) +
  coord_flip() +
  labs(x = "Order of sample chosen",
       y = "X value of sample",
         color = "Cluster",
         group = "Cluster")



names_functions<-c("flat" = "Constant", "rough_1"="Peaked 1",
                   "rough_2"="Peaked 2", 
                   "rough_3"="Peaked 3",
                   "rough_4"="Peaked 4",
                   "smooth_1"="Smooth 1",
                   "smooth_2"="Smooth 2",
                   "smooth_3"="Smooth 3",
                   "smooth_4"="Smooth 4")

data_vline <- data.frame(seaName = unique(samples$seaName),  # Create data for lines
                         vline = vlines)
data_vline   

 plot_samples<- ggplot(data = samples, aes(x = sample_num, y = `x`, group = sampling_policy, color = sampling_policy)) +
    stat_summary(fun = mean, geom = 'line',size=1)+
    facet_wrap(vars(seaName), labeller = as_labeller(names_functions)) +
    coord_flip() +
    labs(x = "Order of Sample Chosen",
         y = "Value of Sample on the x axis",
         color = "Strategy",
         group = "Strategy")+theme_minimal()
 
 samples
 data_vline
 
 total <- merge(samples,data_vline,by="seaName")
 
 plot_samples+ geom_hline(aes(yintercept=vline), data=total,linetype="dashed", 
                          color = "red", size=1, alpha=0.5 )

  
ids = levels(as.factor(samples$name))





samples<-samples %>% mutate(sampling_policy = 
                                 case_when(
                                                sampling_policy == "BP" ~ "Binary Partition",
                                                sampling_policy == "BP1" ~ "Binary Partition",
                                                sampling_policy == "BP2" ~ "Binary Partition",
                                                sampling_policy == "EQ1" ~ "Equidistant",
                                                sampling_policy == "EQ2" ~ "Equidistant",
                                                sampling_policy == "EQ3" ~ "Equidistant",
                                                sampling_policy == "RES" ~ "Resample+Equidistant",
                                                sampling_policy == "RES1" ~ "Resample+Equidistant",
                                                sampling_policy == "RES2" ~ "Resample+Equidistant",
                                                sampling_policy == "UNS" ~ "Misc.",
                                                sampling_policy == "UNS1" ~ "Misc.",
                                                sampling_policy == "UNS2" ~ "Misc.",
                                                sampling_policy == "FLAT" ~ "Flat",
                                              ))



samples %>%
  filter(name == sample(ids, 1)) %>% mutate(sampling_policy = 
                                              case_when(
                                                sampling_policy == "BP" ~ "BP",
                                                sampling_policy == "BP1" ~ "BP",
                                                sampling_policy == "BP2" ~ "BP",
                                                sampling_policy == "EQ1" ~ "EQ",
                                                sampling_policy == "EQ2" ~ "EQ",
                                                sampling_policy == "EQ3" ~ "EQ",
                                                sampling_policy == "RES" ~ "RES",
                                                sampling_policy == "RES1" ~ "RES",
                                                sampling_policy == "RES2" ~ "RES",
                                                sampling_policy == "UNS" ~ "UNS",
                                                sampling_policy == "UNS1" ~ "UNS",
                                                sampling_policy == "UNS2" ~ "UNS",
                                                sampling_policy == "FLAT" ~ "FLAT",
                                              ), cb_order = if_else(cb == 0, "rough_first", "smooth_first")) %>%
  ggplot(aes(x = sample_num, y = `x`, group = sampling_policy, color = sampling_policy)) +
  stat_summary(fun = mean, geom = "line") +
  facet_wrap(vars(seaName, cb_order)) +
  coord_flip() +
  labs(x = "Order of sample chosen",
       y = "X value of sample",
       color = "Cluster",
       group = "Cluster")


unique(samples$sampling_policy)



library(ggpubr)
ggarrange(rough_1_plot, rough_2_plot, rough_3_plot, rough_4_plot, smooth_1_plot, smooth_2_plot, smooth_3_plot, smooth_4_plot, ncol = 3, nrow = 3)+
  scale_color_discrete(labels = c(`1` = "Unspecified",
                                  `2` = "Equidistant I", 
                                  `3` = "Equidistant II", 
                                  `4` = "Equidistant III")) +
  ggtitle("smooth_4 sampling clusters") -> smooth_4_plot+
  scale_color_discrete(labels = c(`1` = "Unspecified I", 
                                  `2` = "Binary partition", 
                                  `3` = "Resample after equidistant I", 
                                  `4` = "Equidistant I", 
                                  `5` = "Equidistant II", 
                                  `6` = "Equidistant III",
                                  `7` = "Resample after equidistant II")) +
  ggtitle("smooth_3 sampling clusters") -> smooth_3_plot+
  scale_color_discrete(labels = c(`1` = "Unspecified I", 
                                  `2` = "Unspecified II", 
                                  `3` = "Binary partition I", 
                                  `4` = "Resample after equidistant", 
                                  `5` = "Binary partition II", 
                                  `6` = "Equidistant I",
                                  `7` = "Equidistant II")) +
  ggtitle("smooth_2 sampling clusters") -> smooth_2_plot+
  scale_color_discrete(labels = c(`1` = "Unspecified", 
                                  `2` = "Equidistant I", 
                                  `3` = "Equidistant II", 
                                  `4` = "Resample after equidistant I", 
                                  `5` = "Resample after equidistant II")) +
  ggtitle("smooth_1 sampling clusters") -> smooth_1_plot+
  scale_color_discrete(labels = c(`1` = "Unspecified", 
                                  `2` = "Binary partition", 
                                  `3` = "Equidistant I", 
                                  `4` = "Equidistant II")) +
  ggtitle("rough_1 sampling clusters") -> rough_4_plot+
  scale_color_discrete(labels = c(`1` = "Resample after equidistant I", 
                                  `2` = "Binary partition I", 
                                  `3` = "Equidistant I", 
                                  `4` = "Binary partition II", 
                                  `5` = "Resample after equidistant II",
                                  `6` = "Equidistant II", 
                                  `7` = "Equidistant III",
                                  `8` = "Unspecified")) +
  ggtitle("rough_1 sampling clusters") -> rough_3_plot+
  scale_color_discrete(labels = c(`1` = "Resample after equidistant", 
                                  `2` = "Binary partition", 
                                  `3` = "Unspecified", 
                                  `4` = "Equidistant I", 
                                  `5` = "Equidistant II", 
                                  `6` = "Equidistant III")) +
  ggtitle("rough_1 sampling clusters") -> rough_1_plot+
scale_color_discrete(labels = c(`1` = "Unspecified", 
                                  `2` = "Resample after equidistant", 
                                  `3` = "Binary partition", 
                                  `4` = "Equidistant I", 
                                  `5` = "Equidistant II", 
                                  `6` = "Equidistant III")) +
  ggtitle("rough_2 sampling clusters") -> rough_2_plot
  
  
 


samples %<>% mutate(
  rough_1_c = case_when(
    rough_1_c == 1 ~ "RES",
    rough_1_c == 2 ~ "BP",
    rough_1_c == 3 ~ "UNS",
    rough_1_c == 4 ~ "EQ1",
    rough_1_c == 5 ~ "EQ2",
    rough_1_c == 6 ~ "EQ3",
  ),
  rough_2_c = case_when(
    rough_2_c == 1 ~ "UNS",
    rough_2_c == 2 ~ "RES",
    rough_2_c == 3 ~ "BP",
    rough_2_c == 4 ~ "EQ1",
    rough_2_c == 5 ~ "EQ2",
    rough_2_c == 6 ~ "EQ3",
  ),
  rough_3_c = case_when(
    rough_3_c == 1 ~ "RES1",
    rough_3_c == 2 ~ "BP1",
    rough_3_c == 3 ~ "EQ1",
    rough_3_c == 4 ~ "BP2",
    rough_3_c == 5 ~ "RES2",
    rough_3_c == 6 ~ "EQ2",
    rough_3_c == 7 ~ "EQ3",
    rough_3_c == 8 ~ "UNS",
  ),
  rough_4_c = case_when(
    rough_4_c == 1 ~ "UNS",
    rough_4_c == 2 ~ "BP",
    rough_4_c == 3 ~ "EQ1",
    rough_4_c == 4 ~ "EQ2",
  ),
  smooth_1_c = case_when(
    smooth_1_c == 1 ~ "UNS",
    smooth_1_c == 2 ~ "EQ1",
    smooth_1_c == 3 ~ "EQ2",
    smooth_1_c == 4 ~ "RES1",
    smooth_1_c == 5 ~ "RES2",
  ),
  smooth_2_c = case_when(
    smooth_2_c == 1 ~ "UNS1",
    smooth_2_c == 2 ~ "UNS2",
    smooth_2_c == 3 ~ "BP1",
    smooth_2_c == 4 ~ "RES",
    smooth_2_c == 5 ~ "BP2",
    smooth_2_c == 6 ~ "EQ1",
    smooth_2_c == 7 ~ "EQ2",
  ),
  smooth_3_c = case_when(
    smooth_3_c == 1 ~ "UNS",
    smooth_3_c == 2 ~ "BP",
    smooth_3_c == 3 ~ "RES1",
    smooth_3_c == 4 ~ "EQ1",
    smooth_3_c == 5 ~ "EQ1",
    smooth_3_c == 6 ~ "EQ3",
    smooth_3_c == 7 ~ "RES2",
  ),
  smooth_4_c = case_when(
    smooth_4_c == 1 ~ "UNS1",
    smooth_4_c == 2 ~ "EQ1",
    smooth_4_c == 3 ~ "EQ2",
    smooth_4_c == 4 ~ "EQ3",
  )
)

samples %<>%
  mutate(sampling_policy = case_when(
    seaName == "rough_1" ~ rough_1_c,
    seaName == "rough_2" ~ rough_2_c,
    seaName == "rough_3" ~ rough_3_c,
    seaName == "rough_4" ~ rough_4_c,
    seaName == "smooth_1" ~ smooth_1_c,
    seaName == "smooth_2" ~ smooth_2_c,
    seaName == "smooth_3" ~ smooth_3_c,
    seaName == "smooth_4" ~ smooth_4_c,
    seaName == "flat" ~ "FLAT",
    seaName == "test" ~ "TEST",
  ))




samples %>% filter(sample_num == 0, seaName != c("flat", "test")) %>% mutate(sampling_policy = 
                                                                               case_when(
                                                                                 sampling_policy == "BP" ~ "BP",
                                                                                 sampling_policy == "BP1" ~ "BP",
                                                                                 sampling_policy == "BP2" ~ "BP",
                                                                                 sampling_policy == "EQ1" ~ "EQ",
                                                                                 sampling_policy == "EQ2" ~ "EQ",
                                                                                 sampling_policy == "EQ3" ~ "EQ",
                                                                                 sampling_policy == "RES" ~ "RES",
                                                                                 sampling_policy == "RES1" ~ "RES",
                                                                                 sampling_policy == "RES2" ~ "RES",
                                                                                 sampling_policy == "UNS" ~ "UNS",
                                                                                 sampling_policy == "UNS1" ~ "UNS",
                                                                                 sampling_policy == "UNS2" ~ "UNS",
                                                                               )) %>% with(table(name, sampling_policy, cb)) -> strats


samples %>% filter(sample_num == 0, seaName != c("flat", "test")) %>% mutate(sampling_policy = 
                                                                               case_when(
                                                                                 sampling_policy == "BP" ~ "BP",
                                                                                 sampling_policy == "BP1" ~ "BP",
                                                                                 sampling_policy == "BP2" ~ "BP",
                                                                                 sampling_policy == "EQ1" ~ "EQ",
                                                                                 sampling_policy == "EQ2" ~ "EQ",
                                                                                 sampling_policy == "EQ3" ~ "EQ",
                                                                                 sampling_policy == "RES" ~ "RES",
                                                                                 sampling_policy == "RES1" ~ "RES",
                                                                                 sampling_policy == "RES2" ~ "RES",
                                                                                 sampling_policy == "UNS" ~ "UNS",
                                                                                 sampling_policy == "UNS1" ~ "UNS",
                                                                                 sampling_policy == "UNS2" ~ "UNS",
                                                                               )) %>% 
  ggplot(aes(y = if_else(sampling_policy == "EQ", 1, 0), x = as.factor(cb))) + 
    stat_summary(fun = mean, geom = "bar") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
    facet_wrap(vars(seaName)) + 
    labs(y = "Proportion of equidistant sampling policy use",
         x = "Counterbalance (0 = rough first, 1 = smooth first)")








joint_samples <- merge(sum_preds %>% filter(seaName != "test"),
                       samples %>% filter(sample_num == 0, seaName != "test") %>% dplyr::select(name, seaName, cb, sampling_policy), by = c("name", "seaName"), all.x = TRUE)
joint_samples %>% filter(seaName != "flat") %>% mutate(sampling_policy = 
                                                         case_when(
                                                           sampling_policy == "BP" ~ "BP",
                                                           sampling_policy == "BP1" ~ "BP",
                                                           sampling_policy == "BP2" ~ "BP",
                                                           sampling_policy == "EQ1" ~ "EQ",
                                                           sampling_policy == "EQ2" ~ "EQ",
                                                           sampling_policy == "EQ3" ~ "EQ",
                                                           sampling_policy == "RES" ~ "RES",
                                                           sampling_policy == "RES1" ~ "RES",
                                                           sampling_policy == "RES2" ~ "RES",
                                                           sampling_policy == "UNS" ~ "UNS",
                                                           sampling_policy == "UNS1" ~ "UNS",
                                                           sampling_policy == "UNS2" ~ "UNS",
                                                         ), is_rough = if_else(startsWith(seaName, "rough"), "rough", "smooth"),
                                                       cb_order = if_else(cb == 0, "rough_first", "smooth_first")) %>% 
  ggplot(aes(y = avg_error, x = sampling_policy, group = sampling_policy, color = sampling_policy, fill = sampling_policy)) +
  facet_wrap(vars(is_rough, cb_order)) +
  geom_jitter(width = 0.05, height = 0) +
  geom_boxplot(alpha = 0.2)

joint_samples %>% filter(seaName != "flat") %>% mutate(sampling_policy = 
                                                         case_when(
                                                           sampling_policy == "BP" ~ "BP",
                                                           sampling_policy == "BP1" ~ "BP",
                                                           sampling_policy == "BP2" ~ "BP",
                                                           sampling_policy == "EQ1" ~ "EQ",
                                                           sampling_policy == "EQ2" ~ "EQ",
                                                           sampling_policy == "EQ3" ~ "EQ",
                                                           sampling_policy == "RES" ~ "RES",
                                                           sampling_policy == "RES1" ~ "RES",
                                                           sampling_policy == "RES2" ~ "RES",
                                                           sampling_policy == "UNS" ~ "UNS",
                                                           sampling_policy == "UNS1" ~ "UNS",
                                                           sampling_policy == "UNS2" ~ "UNS",
                                                         ), is_rough = if_else(startsWith(seaName, "rough"), "rough", "smooth"),
                                                       cb_order = if_else(cb == 0, "rough_first", "smooth_first")) %>% 
  lmer(formula = avg_error ~ is_rough * cb_order + (1|name), data = .) %>% summary


# write_csv(samples, "~/Documents/GitHub/function-learning/analysis/function_020samplestrat.csv")







