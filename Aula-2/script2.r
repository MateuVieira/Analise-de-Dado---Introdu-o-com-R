duracao <- rename(duracao, replace = c("user_id"="aluno", "course_id"="curso", "timeToFinish"="dias"))

plot(duracao$dias)

png("histograma.png")
hist(duracao$dias, breaks = 20, main = "Histograma do Tempo",
     ylab = "Quantidade", xlab = "Tempo", ylim = c(0, 2000), xlim = c(0,600), col = "blue") 

dev.off()

mean(duracao$dias, na.rm = T)

median(duracao$dias, na.rm = T)

summary(duracao$dias)

dim(duracao)[1]

sumario_estatistico <- aggregate(duracao$dias, list(duracao$curso), mean, na.rm = T)



sumario_estatistico <-  rename(sumario_estatistico, replace = c("Group.1"="curso","x"="dias"))

Popularidade <- rename(Popularidade, c("course_id"="curso", "freq"="popularidade"))

popularidade_e_duracao <- merge(sumario_estatistico,Popularidade, "curso")

plot(popularidade_e_duracao$dias, popularidade_e_duracao$popularidade)

modelo.linear <- lm(popularidade_e_duracao$popularidade ~ popularidade_e_duracao$dias)

abline(lm(popularidade_e_duracao$popularidade ~ popularidade_e_duracao$dias))

scatter.smooth(popularidade_e_duracao$dias, popularidade_e_duracao$popularidade, pch = 21, col = "blue")

install.packages("ggplot2")
library(ggplot2)

grafico <- ggplot(popularidade_e_duracao, aes(dias, popularidade))
grafico <- grafico + geom_point()
grafico <- grafico + geom_smooth()
