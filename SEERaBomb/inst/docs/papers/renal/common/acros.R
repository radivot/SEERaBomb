gp=geom_point()
gl=geom_line()
geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
# ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
gh=geom_hline(yintercept=1)
# svts=scale_x_continuous(breaks=c(0,5,10))#surv times
sbb=theme(strip.background=element_blank())
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
ltp=theme(legend.position="top")
# lh=theme(legend.direction="horizontal")
jco=scale_color_jco()
tc=function(sz) theme_classic(base_size=sz);
gx=xlab("Years Since Diagnosis")

agts=scale_x_continuous(breaks=c(25,50,75))#age times
gxi=xlab("Age (Years)")
gyi=ylab(quote(paste("Cases per ",10^5," Person Years")))
sy=scale_y_log10()


# svts=scale_x_continuous(breaks=seq(0,50,10))#surv times

# myt=theme(legend.text=element_text(size=12),strip.text=element_text(size=12))
