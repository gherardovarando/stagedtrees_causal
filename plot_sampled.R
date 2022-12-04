library(ggplot2)
library(data.table)
library(plyr)

ps <- c(2, 3, 4, 5, 6) ## num variables
Ns <- c(100, 250, 500, 1000, 2000, 3000, 5000, 10000) ## sample size
ks <- c(2, 3, 4) ## complexity (stages per stratum or prob_edges = k / (p-1))
ls <- c(2, 3, 4) ## maximum number of levels per variable 

sampling_methods_names <- c("random_sevt") 
			    #"random_sevt" ,"random_bnsevt")
dtall <- NULL
for (nm in sampling_methods_names){
  for (p in ps){
    for (N in Ns){
      for (k in ks){
        for (l in ls){
          fnam <- paste0("results/",  nm, "/","p", p, "N", N, "k", k, 'l', l, "results.rds")
          if (file.exists(fnam)){
            results <- readRDS(fnam)
            bic.df <- as.data.frame(t(sapply(results, getElement, 'bic')))
            dist.df <- as.data.frame(t(sapply(results, getElement, 'distance')))
            cid.df <- as.data.frame(t(sapply(results, getElement, 'cid')))
            time.df <- as.data.frame(t(sapply(results, getElement, 'time')))
            dist.dt <- melt(as.data.table(dist.df), measure.vars = colnames(dist.df), variable.name = "algorithm", value.name = "distance")
            cid.dt <- melt(as.data.table(cid.df), measure.vars = colnames(cid.df), variable.name = "algorithm", value.name = "cid")
            bic.dt <- melt(as.data.table(bic.df), measure.vars = colnames(bic.df), variable.name = "algorithm", value.name = 'bic')
            time.dt <- melt(as.data.table(time.df), measure.vars = colnames(time.df), variable.name = "algorithm", value.name = 'time')
            dt <- cbind(dist.dt, cid = cid.dt$cid, bic = bic.dt$bic, time = time.dt$time, p = p, N = N, k = paste0("k=",k), l = paste0("l=",l), nm = nm)
            if (is.null(dtall)){
              dtall <- dt
            }else{
              dtall <- rbind(dtall, dt)
            }  
          }
        }
      }
    }
  }
  
}

selected <- c( "best_bhc", "best_kmeans", "bn_tabu", "bn_mmhc")
path <- "plot"
dir.create(path)
dt1 <- data.table(ddply(dtall, .(p, N, k, l, nm, algorithm), summarise, 
                      time.stde = sd(time, na.rm = TRUE) / sum(!is.na(time)),
                      time = mean(time, na.rm = TRUE),
             KD = mean(distance, na.rm = TRUE), 
             KD.stde = sd(distance, na.rm = TRUE)/ sum(!is.na(distance)), 
             CID = mean(cid, na.rm = TRUE),
             CID.stde = sd(cid, na.rm = TRUE) / sum(!is.na(cid))
)
)

dt1 <- dt1[algorithm %in% selected & l == "l=2"] 


dt1.a <- melt(dt1, id.vars = c("algorithm", "p", "N", "k", "nm", "l"))


for (pp in ps){
PLGG <- ggplot(dt1.a[p == pp & nm == "random_sevt" 
                     & variable %in% c("CID", "KD")], 
               aes(x=N, y=value, group = algorithm, color = algorithm, fill = algorithm)) +
  geom_line() + geom_point() + 
  #geom_linerange(aes(x = N, ymin = KD - 3*KD.stde, ymax = KD + 3*KD.stde)) +
  facet_grid(cols = vars(k), rows = vars(variable), scales = "free_y") +
  theme_bw() + 
  guides(color = guide_legend(nrow = 2)) +
  scale_x_log10(breaks = Ns[c(2,4,6,8)]) + ylab("")+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 7), 
        #legend.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7), legend.margin = margin(),
        legend.box.margin = margin(), legend.title = element_blank(), 
        text = element_text(size = 9)) 

ggsave(
  file = paste0("plot_",pp,"var_random_sevt.pdf"),
  plot = PLGG,
  path = path,
  width = 3,
  height = 3.5,
  units = "in"
)
}



dt2 <- ddply(dt1, c("p", "algorithm"), .fun =  summarise, time = mean(time, na.rm=TRUE))

PLGG <- ggplot(dt2, aes(x=p, y=time, 
				    group = algorithm, color = algorithm, fill = algorithm)) +
   geom_point() + geom_line()+ scale_y_log10()+ theme_bw() + 
   scale_x_continuous(breaks = unique(dt2$p)) + 
   theme(legend.position = "bottom") + 
   ylab("time (s)")  


ggsave(
        file = "plot_time.pdf",
	plot = PLGG,
        path = path,
        width = 5.5,
        height = 3.5,
        units = "in"
      )

   

