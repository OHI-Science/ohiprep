#### Understanding the California Current fisheries equations so I can answer this question:
### Just another question , sorry for bothering you but I was wondering if the formula in the PlosONE US assessment paper is correct, it is the last one in the Equation S1c, where you calculate (2.5-(F/FMSY)/1.3 . I was wondering if 1.3 is correct (should this not be 0.8) and why 1.3 and also why 2.5 ?  
## email on 11/12/2015

library(dplyr)

## overfished (bbmsy < 0.8)

bbmsy <- seq(0,0.7, by=0.1)
ffmsy <- seq(0, 3, by=0.1)

data_overfished <- expand.grid(bbmsy=bbmsy, ffmsy=ffmsy)

data_overfished <- data_overfished %>%
  mutate(calc = ifelse(ffmsy >= (bbmsy + 1.5), "a",
                       ifelse(ffmsy < (bbmsy - 0.2), 'b',
                          ifelse(ffmsy >= (bbmsy + 0.2) & ffmsy < (bbmsy + 1.5), 'c',
                                 ifelse(ffmsy >= (bbmsy - 0.2) & ffmsy < (bbmsy + 0.2), 'd', NA)))))

data_overfished <- data_overfished %>%
  mutate(f_prime = ifelse(calc == 'a', 0,
                          ifelse(calc == 'b', ffmsy/(bbmsy-0.2),
                                 ifelse(calc == 'c', (bbmsy + 1.5 - ffmsy)/1.5, 1))))

## ok/underfished (bbmsy > 0.8)

bbmsy <- seq(0.8, 3.5, by=0.1)
ffmsy <- seq(0, 3, by=0.1)

data_ok_under <- expand.grid(bbmsy=bbmsy, ffmsy=ffmsy)

data_ok_under <- data_ok_under %>%
  mutate(calc = ifelse(ffmsy < 0.8, "e",
                       ifelse(ffmsy >= 0.8 & ffmsy < 1.2, 'f', 
                              ifelse(ffmsy >= 1.2, 'g', NA))))

data_ok_under <- data_ok_under %>%
  mutate(f_prime = ifelse(calc == 'e', ffmsy/0.8,
                          ifelse(calc == 'f', 1, (2.5 - ffmsy)/1.3)))


data <- rbind(data_overfished, data_ok_under)


ggplot(data, aes(x=bbmsy, y=ffmsy, fill=f_prime)) +
  geom_tile() +
  scale_fill_gradient(low="red", high="green") +
  geom_vline(xintercept = 0.75, col="red") +
  theme_bw()



####

bbmsy <- seq(0, 3, by=0.1)
ffmsy <- seq(0, 3, by=0.1)

data2 <- expand.grid(bbmsy=bbmsy, ffmsy=ffmsy)

data2 <- data2 %>%
  mutate(score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy+1.5), 0, NA),
         score = ifelse(bbmsy < 0.8 & ffmsy < (bbmsy - 0.2), ffmsy/(bbmsy-0.2), score),
         score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy + 0.2) & ffmsy < (bbmsy + 1.5), (bbmsy + 1.5 - ffmsy)/1.5, score),
         score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy - 0.2) & ffmsy < (bbmsy + 0.2), 1, score)) %>%
  mutate(score = ifelse(bbmsy >= 0.8 & ffmsy < 0.8, ffmsy/0.8, score),
         score = ifelse(bbmsy >= 0.8 & ffmsy >= 0.8 & ffmsy < 1.2, 1, score),
         score = ifelse(bbmsy >= 0.8 & ffmsy >= 1.2, (2.5 - ffmsy)/1.3, score)) %>%
  mutate(score = ifelse(score < 0, 0, score))

ggplot(data2, aes(x=bbmsy, y=ffmsy, fill=score)) +
  geom_tile() +
  scale_fill_gradient(low="red", high="green") +
  geom_vline(xintercept = 0.75, col="red") +
  theme_bw()

