library(tidyverse)
# input <- list(
#   sensitivity1 = 100,
#   specificity1 = 93,
#   sensitivity2 = 97,
#   specificity2 = 98
# )
#   
# makePPV_black <- function(prev, sens=input$sensitivity1/100, spec=input$specificity1/100) {
#   prev <- 1/prev
#   (sens * prev) / ({sens * prev} + {(1-spec)*(1-prev)})
# }
# 
# makePPV_red <- function(prev, sens=input$sensitivity2/100, spec=input$specificity2/100) {
#   prev <- 1/prev
#   (sens * prev) / ({sens * prev} + {(1-spec)*(1-prev)})
# }
#   
#   tibble(
#     prev = c(seq(1, 9, by=2), seq(10, 490, by=10), seq(500, 2500, by=100))
#   ) %>% 
#     mutate(PPV = makePPV_black(prev)) %>%
#     mutate(PPV2 = makePPV_red(prev)) %>%
#     ggplot(aes(x=prev)) + 
#     geom_line(aes(y=PPV), color="black") +
#     geom_line(aes(y=PPV2), color="red") +
#     xlim(c(NA,2500)) +
#     scale_y_continuous(labels=percent, limits=c(0,1)) +
#     labs(x="1/Prevalence")
#   
  
  
calcNumbers <- function(pretest, sens=1, spec=.93, n=1) {
    # prev <- 1/prev
    # (sens * prev) / ({sens * prev} + {(1-spec)*(1-prev)})
    
    TP <- pretest * sens * n
    FN <- pretest * (1-sens) * n
    TN <- (1-pretest) * spec * n
    FP <- (1-pretest) * (1-spec) * n
    haveDz <- (TP + FN) 
    noDz   <- (FP + TN) 
    
    PPV <- (sens * pretest) / ({sens * pretest} + {(1-spec)*(1-pretest)})
    
    list(pretest=pretest, sens=sens, spec=spec, 
         TP=TP, TN=TN, FP=FP, FN=FN,
         haveDz=haveDz, noDz=noDz, 
         PPV=PPV, n=n)
    
    tibble(pretest=pretest, PPV=PPV, TP=TP, FP=FP, TN=TN, FN=FN)
}

c(seq(5, 100, by=10)/100,.025) %>%

# c(.95, .9, .75, .5, .25, .1, .5, .01) %>%
  # c(.95, .9, .75, .5, .25, .1, .5, .01) %>%
  calcNumbers() %>%
  
  pivot_longer(cols=c(TP, TN, FP, FN)) %>%
  filter(name %in% c("TP", "FP")) %>%
  ggplot(aes(x=pretest)) +
  geom_line(aes(y=PPV)) +
  geom_col(aes(y=value, fill=name), position = "dodge")


# (seq(5, 100, by=10)/100) %>%
  
# c(seq(5, 100, by=10)/100,.025) %>%
c(.9, .75, .5, .25, .1, .05, 0.025, .01) %>%
  calcNumbers() %>%
  
  pivot_longer(cols=c(TP, TN, FP, FN)) %>%
  filter(name %in% c("TP", "FP")) %>%
  ggplot(aes(x=pretest)) +
  geom_line(aes(y=PPV)) +
  geom_col(aes(y=value, fill=name), position = "dodge") 

c(.9, .75, .5, .25, .1, .05, 0.025, .01) %>%
  calcNumbers() %>%
  
  pivot_longer(cols=c(TP, TN, FP, FN)) %>%
  filter(name %in% c("TP", "FP")) %>%
  ggplot(aes(x=pretest)) +
  geom_line(aes(y=PPV)) +
  geom_col(aes(y=value, fill=name), position = "dodge") +
  scale_x_log10()
  
"#14A44D" 
"#dcf1e4"
"#DC4C64"
"#fae4e8"
"#3B71CA"
"#e2eaf7"

  

pre_to_post <- function(pr_pre, sens=1, spec=.93, pos_LR=T){
  positive_LR <- sens / (1-spec)
  negative_LR <- (1-sens) / spec
  
  odds_pre <- pr_pre / (1-pr_pre)
  
  if(pos_LR) odds_post <- odds_pre * positive_LR
  if(!pos_LR) odds_post <- odds_pre * negative_LR
  
  pr_pre <- odds_post / (1 + odds_post)
  pr_pre
  
  
}

# Confirm with: https://jamaevidence.mhmedical.com/data/calculators/LR_nomogram.html
pre_to_post(.1)
pre_to_post(.4)



df_labs <- tibble(occ = c(10,20,50,100,1000)) %>%
  mutate(pre = 1/occ,
         post = pre_to_post(pre, sens=1, spec=.931),
         label = str_glue("1 in {occ}")) 

tibble(pre = c(.001, .01, .05, .10, .25, .5, .75, .9) ) %>%
  mutate(post = pre_to_post(pre)) %>%
  ggplot(aes(x=pre, y=post)) +
  geom_line() +
  ggrepel::geom_label_repel(aes(x=pre, y=post, label=label), 
                            data=df_labs,
                            nudge_y = .1) +
  scale_x_log10()

# Used for case 1
tibble(pre = seq(1, 1000)/1000 ) %>%
  # summary()
  mutate(post = pre_to_post(pre)) %>%
  ggplot(aes(x=pre, y=post)) +
  geom_line() +
  ggrepel::geom_label_repel(aes(x=pre, y=post, label=label), 
                            data=df_labs,
                            nudge_y = .1) +
  scale_x_continuous(labels=scales::percent) +
  scale_x_log10(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="Pre-test", y="Post-test", color="" )
  

## Illustrates varying levels
tibble(pre = seq(1, 1000)/1000 ) %>%
  # summary()
  mutate(spec93 = pre_to_post(pre),
         spec97 = pre_to_post(pre, spec = .97),
         spec90 = pre_to_post(pre, spec = .90),
         spec99 = pre_to_post(pre, spec = .99),
         ) %>%
  pivot_longer(cols = starts_with("spec")) %>%
  ggplot(aes(x=pre)) +
  geom_line(aes(y=value, group=name, color=name)) +
  ggrepel::geom_label_repel(aes(x=pre, y=post, label=label), 
                            data=df_labs,
                            nudge_y = .1) +
  scale_x_continuous(labels=scales::percent) +
  scale_x_log10(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="Pre-test", y="Post-test", color="" )
  



## Work in progress
tibble(pre = seq(1, 1000)/1000 ) %>%
  # summary()
  mutate(sens93 = pre_to_post(pre),
         sens97 = pre_to_post(pre, spec = .97),
         sens90 = pre_to_post(pre, spec = .90),
         sens99 = pre_to_post(pre, spec = .99),
  ) %>%
  pivot_longer(cols = starts_with("sens")) %>%
  ggplot(aes(x=pre)) +
  geom_line(aes(y=value, group=name, color=name)) +
  # ggrepel::geom_label_repel(aes(x=pre, y=post, label=label), 
  #                           data=df_labs,
  #                           nudge_y = .1) +
  scale_x_continuous(labels=scales::percent) +
  scale_x_log10(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="Pre-test", y="Post-test", color="" )



tibble(pre = seq(1, 1000)/1000 ) %>%
  # summary()
  mutate(`s90` = pre_to_post(pre, sens=.9, spec = .9, pos_LR = F),
         `s80` = pre_to_post(pre, sens=.8, spec = .8, pos_LR = F),
         `s95` = pre_to_post(pre, sens=.95, spec = .95, pos_LR = F),
         `s97` = pre_to_post(pre, sens=.97, spec = .97, pos_LR = F),

  ) %>%
  # mutate(`CF_80` = pre_to_post(pre, sens=.8, spec = .95, pos_LR = T),
  #        `CF_95` = pre_to_post(pre, sens=.95, spec = .95, pos_LR = T),
  #        `ID_70` = pre_to_post(CF_80, sens=.9, spec = .95, pos_LR = F)
  #        # `s95` = pre_to_post(pre, sens=.95, spec = .95, pos_LR = F),
  #        # `s97` = pre_to_post(pre, sens=.97, spec = .97, pos_LR = F),
  #        
  # ) %>%
  pivot_longer(cols = starts_with("s")) %>%
  ggplot(aes(x=pre)) +
  geom_line(aes(y=value, group=name, color=name)) +

  scale_x_continuous(labels=scales::percent) +
  # scale_x_log10(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="Pre-test", y="Post-test", color="" )



#########


sens
spec

odds_pre <- pr_pre / (1-pr_pre)

LR_pos <- sens / (1-spec)
LR_neg <- (1-sens) / spec

pr_pre <- odds_post / (1 + odds_post)


pre_to_post <- function(pr_pre, sens=1, spec=.93, pos_LR=T){
  positive_LR <- sens / (1-spec)
  negative_LR <- (1-sens) / spec
  
  odds_pre <- pr_pre / (1-pr_pre)
  
  if(pos_LR) odds_post <- odds_pre * positive_LR
  if(!pos_LR) odds_post <- odds_pre * negative_LR
  
  odds_post / (1 + odds_post)
  
}

get_LR <- function(sens, spec){
  # if (type=="+") LR <- sens / (1-spec) 
  # if (type=="-") LR <- (1-sens) / spec
  
  LR <- list(pos = sens / (1-spec),
             neg = (1-sens) / spec)
  
  return(LR)
}

convert_pre_post <- function(Pr_pre, LR){
  # Convert pretest-prob to pretest odds
  odds_pre <- Pr_pre / (1-Pr_pre)
  
  # Find the posttest-odds
  odds_post <- odds_pre * LR
  
  # Convert back to probability (specifically post-test probability)
  odds_post / (1 + odds_post)
  
}

get_LR(.93, .95)

tibble(pre = seq(1, 1000)/1000 ) %>%
  mutate(post_pos = convert_pre_post(pre, get_LR(.93, .95)$pos),
         post_neg = convert_pre_post(pre, get_LR(.93, .95)$neg)) %>%
  pivot_longer(cols = starts_with("post")) %>%
  ggplot(aes(x=pre)) +
  geom_line(aes(y=value, group=name, color=name)) +
  
  scale_x_continuous(labels=scales::percent) +
  # scale_x_log10(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x="Pre-test", y="Post-test", color="" )

