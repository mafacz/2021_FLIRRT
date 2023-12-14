##REPEATED MEASUREMENTS##
##i included the possibility to plot multiple groups, in our case this could be e.g. high and low UF, especially when we look at the effects it has on patients e.g MAP or others

require(tidyverse)

#Line Plot#
p1 <- dataset %>% #dataset should be in long format (more than one row per patient)
	select(patient_id, moment_of_measurement, value, group) %>% #patient_id is the unique ID of the patient; moment_of_measurement is the moment of measurement; value is the value of the dependent variable; group is group
	group_by(moment_of_measurement, group) %>%
	summarise(mean_cl_normal(value), count = sum(!is.na(value))) %>%
	ggplot(aes(x = moment_of_measurement, y = y, color = factor(group))) +
	geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5,  position = position_dodge(0.00), size = 0.5) +
    geom_point(size = 2) +
	geom_line(size = 0.5) +
	scale_color_manual(name = "", values = c("blue","red"), labels = c("Group 1","Group 2")) +
	xlab("Days") +
	ylab("XXX") +
	scale_x_continuous(expand = c(0.02, 0.02)) +
	scale_y_continuous(limits=c(8,20), breaks = seq(6,20,2), expand=c(0,0)) + #adjust according to the variable unit
	geom_text(data = function(x) subset(x, group==1), aes(label = count, y = 8.9),  position=position_dodge(0.9), size = 3.5) +
	geom_text(data = function(x) subset(x, group==0), aes(label = count, y = 8.3),  position=position_dodge(0.9), size = 3.5) +
	theme(panel.grid.major.y = element_line(colour = "gray"), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(colour = "black", size = 15), axis.text = element_text(color = "black", size = 13), legend.key=element_blank(), legend.background=element_blank(), legend.text=element_text(size=13), legend.position = c(0.20, 0.95), plot.title = element_text(hjust = 0.5, face="bold", size = 15))
