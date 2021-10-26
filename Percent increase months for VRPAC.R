
### use edutor_color.R for data
after <- sum(df$cases[df$age_group%in%c("0-4 Years", "5-11 Years", "12-15 Years", "16-17 Years") & df$week>="2021-08-07"])
before <- sum(df$cases[df$age_group%in%c("0-4 Years", "5-11 Years", "12-15 Years", "16-17 Years") & df$week<"2021-08-07" & df$week>="2021-06-05"])


(after - before)/before



after <- sum(df$cases[df$age_group%in%c("5-11 Years") & df$week>="2021-08-07"])
before <- sum(df$cases[df$age_group%in%c("5-11 Years") & df$week<"2021-08-07" & df$week>="2021-06-05"])


(after - before)/before
