# http://www.istat.it/it/files/2014/11/C11.pdf
hh_wt_fixline <- data.frame(year = c(2009, 2010, 2011, 2012, 2013),
                            hh = c(72.8, 70.7, 68.8, 66.2, 64))
# require(zoo)
# require(ggplot2)
# ggplot(hh_wt_fixline, aes(x=year,y=hh)) +
#   geom_point() +
#   geom_smooth(method='lm')

mod <- lm(hh ~ year, data = hh_wt_fixline)
prediction2016 <- predict(mod, data.frame(year=2016))

observed2011 <- hh_wt_fixline[hh_wt_fixline$year==2011,"hh"]
hh_perc_correction <- prediction2016 / observed2011
# housecleaning
rm(observed2011, prediction2016, mod, hh_wt_fixline)