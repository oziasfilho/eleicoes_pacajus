library(electionsBR)
library(tidyverse)
library(data.table)
library(janitor)
library(extrafont)
dados<-fread("consulta_cand_2020_CE.csv")
dados<- dados %>% filter(NM_UE=="PACAJUS")
dados %>% count(DS_CARGO)
vereador<- dados %>% filter(DS_CARGO=="VEREADOR")
prefeito<- dados %>% filter(DS_CARGO=="PREFEITO")
prefeito_vice<- dados %>% filter(DS_CARGO %in% c("PREFEITO","VICE-PREFEITO"))

raca<- dados %>% count(DS_CARGO,DS_COR_RACA)
raca_total<- dados %>% tabyl(DS_COR_RACA)
raca_prefeito<- prefeito_vice %>% tabyl(DS_COR_RACA)
raca_vereador<- vereador %>% tabyl(DS_COR_RACA)

str(grafico)



###### GRÁFICO RAÇA TOTAL #####
raca_total %>% ggplot(aes(x=reorder(DS_COR_RACA,percent),y=percent,label = scales::percent(percent,accuracy = .01))) +
  geom_bar(stat = "identity",fill="darkgreen") +
  coord_flip() +
  xlab(" ") +
  ylab("Percentual de Candidaturas") +
  geom_text(vjust = -0.05, hjust=-0.5,    # nudge above top of bar
            size = 3) + 
  ggtitle("Percentual de Candidaturas por Raça") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::percent,limits = c(0,1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                     panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

###### GRÁFICO RAÇA PREFEITO E VICE #####
raca_prefeito %>% ggplot(aes(x=reorder(DS_COR_RACA,percent),y=percent,label = scales::percent(percent,accuracy = .01))) +
  geom_bar(stat = "identity",fill="darkgreen") +
  coord_flip() +
  xlab(" ") +
  ylab("Percentual de Candidaturas") +
  geom_text(vjust = -0.05, hjust=-0.5,    # nudge above top of bar
            size = 3) + 
  ggtitle("Percentual de Candidatos por Raça \n(Prefeitura)") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::percent,limits = c(0,1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))


###### GRÁFICO - RAÇA VEREADOR ######
raca_vereador %>% ggplot(aes(x=reorder(DS_COR_RACA,percent),y=percent,label = scales::percent(percent,accuracy = .01))) +
  geom_bar(stat = "identity",fill="darkgreen") +
  coord_flip() +
  xlab(" ") +
  ylab("Percentual de Candidaturas") +
  geom_text(vjust = -0.05, hjust=-0.5,    # nudge above top of bar
            size = 3) + 
  ggtitle("Percentual de Candidaturas por Raça \n(Câmara Municipal)") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels = scales::percent,limits = c(0,1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))


####### GRAU DE INSTRUÇÃO ######
gs_vereador<-vereador %>% tabyl(DS_GRAU_INSTRUCAO)
gs_prefeito<- prefeito_vice %>% tabyl(DS_GRAU_INSTRUCAO)
gs_total<- dados %>% tabyl(DS_GRAU_INSTRUCAO)
gs_total$DS_GRAU_INSTRUCAO<-factor(gs_total$DS_GRAU_INSTRUCAO, levels=c("LÊ E ESCREVE",
                                                                              "ENSINO FUNDAMENTAL INCOMPLETO","ENSINO FUNDAMENTAL COMPLETO",
                                                                              "ENSINO MÉDIO INCOMPLETO","ENSINO MÉDIO COMPLETO",
                                                                              "SUPERIOR INCOMPLETO","SUPERIOR COMPLETO"))

gs_prefeito$DS_GRAU_INSTRUCAO<-factor(gs_prefeito$DS_GRAU_INSTRUCAO, levels=c("LÊ E ESCREVE",
                                                                        "ENSINO FUNDAMENTAL INCOMPLETO","ENSINO FUNDAMENTAL COMPLETO",
                                                                        "ENSINO MÉDIO INCOMPLETO","ENSINO MÉDIO COMPLETO",
                                                                        "SUPERIOR INCOMPLETO","SUPERIOR COMPLETO"))

gs_vereador$DS_GRAU_INSTRUCAO<-factor(gs_vereador$DS_GRAU_INSTRUCAO, levels=c("LÊ E ESCREVE",
                                                                              "ENSINO FUNDAMENTAL INCOMPLETO","ENSINO FUNDAMENTAL COMPLETO",
                                                                              "ENSINO MÉDIO INCOMPLETO","ENSINO MÉDIO COMPLETO",
                                                                              "SUPERIOR INCOMPLETO","SUPERIOR COMPLETO"))
#### GRAFICO PREFEITO E VICE #####
gs_prefeito %>% ggplot(aes(x=DS_GRAU_INSTRUCAO,y=percent,label = scales::percent(percent,accuracy = .01))) +
  geom_bar(stat = "identity",fill="yellow2") +
  coord_flip() +
  xlab(" ") +
  ylab("Percentual de Candidaturas") +
  geom_text(vjust = -0.01, hjust=-0.05,    # nudge above top of bar
            size = 3) + 
  ggtitle("Percentual de Candidaturas por Grau de Instrução \n(Prefeitura)") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::percent,limits = c(0,1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))


###### GRÁFICO TOTAL ######
gs_total %>% ggplot(aes(x=DS_GRAU_INSTRUCAO,y=percent,label = scales::percent(percent,accuracy = .01))) +
  geom_bar(stat = "identity",fill="yellow2") +
  coord_flip() +
  xlab(" ") +
  ylab("Percentual de Candidaturas") +
  geom_text(vjust = -0.01, hjust=-0.05,    # nudge above top of bar
            size = 3) + 
  ggtitle("Percentual de Candidaturas por Grau de Instrução") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::percent,limits = c(0,1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

###### GRÁFICO VEREADORES
gs_vereador %>% ggplot(aes(x=DS_GRAU_INSTRUCAO,y=percent,label = scales::percent(percent,accuracy = .01))) +
  geom_bar(stat = "identity",fill="yellow2") +
  coord_flip() +
  xlab(" ") +
  ylab("Percentual de Candidaturas") +
  geom_text(vjust = -0.01, hjust=-0.05,    # nudge above top of bar
            size = 3) + 
  ggtitle("Percentual de Candidaturas por Grau de Instrução \n(Câmara Municipal)") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::percent,limits = c(0,1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))


###### SEXO ######
sexo<-dados %>% group_by(SG_PARTIDO,DS_GENERO) %>% count()
sexo<-sexo %>% pivot_wider(names_from = DS_GENERO,values_from=n)                          
sexo<- sexo %>% mutate(TOTAL=FEMININO+MASCULINO)
sexo<- sexo %>% mutate(PERC_FEM=FEMININO/TOTAL,PERC_MASC=MASCULINO/TOTAL)
sexo<- sexo %>% select(PERC_FEM,PERC_MASC)
names(sexo)<-c("SG_PARTIDO","FEMININO","MASCULINO")
sexo<- sexo %>% pivot_longer(!SG_PARTIDO,names_to="DS_GENERO",values_to="PERC")

position = position_dodge(width = .75)
width = .75


sexo %>% ggplot(aes(x=SG_PARTIDO,y=PERC,fill=DS_GENERO,label = scales::percent(PERC))) +
  geom_bar(stat = "identity",position=position) + 
  coord_flip() +
  xlab(" ") +
  ylab("Percentual de Candidaturas") +
  geom_text(hjust=-0.05,    # nudge above top of bar
            size = 3,position = position) + 
  ggtitle("Percentual de Candidaturas por Sexo em cada Partido") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::percent,limits = c(0,1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  geom_hline(yintercept=0.3, linetype="dashed", color = "brown")



###### RECEITAS_CANDIDATOS ######
receitas<- fread("receitas_candidatos_2020_CE.csv",dec = ",") %>% filter(NM_UE=="PACAJUS")
receitas_pref<- receitas %>% filter(DS_CARGO=="Prefeito")
pref_rec<-receitas_pref %>% group_by(DS_ORIGEM_RECEITA) %>% summarise(Total=sum(VR_RECEITA)) %>% arrange(desc(Total))


pref_rec %>% ggplot(aes(x=reorder(DS_ORIGEM_RECEITA,Total),y=Total,label = scales::dollar(Total,prefix = "R$",big.mark = "."))) +
  geom_bar(stat = "identity",position=position) + 
  coord_flip() +
  xlab(" ") +
  ylab("Receitas") +
  geom_text(hjust=-0.05,    # nudge above top of bar
            size = 3,position = position) + 
  ggtitle("Receitas por Origem (Até 12/11) \n(Prefeitura)") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::dollar_format(prefix = "R$"),limits = c(0,200000)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

receitas_ver<- receitas %>% filter(DS_CARGO=="Vereador")
rec_ver<- receitas_ver %>% group_by(DS_ORIGEM_RECEITA) %>% summarise(Total=sum(VR_RECEITA)) %>% arrange(desc(Total))

rec_ver %>% ggplot(aes(x=reorder(DS_ORIGEM_RECEITA,Total),y=Total,label = scales::dollar(Total,prefix = "R$",big.mark = ".",decimal.mark = ","))) +
  geom_bar(stat = "identity",position=position) + 
  coord_flip() +
  xlab(" ") +
  ylab("Receitas") +
  geom_text(hjust=-0.05,    # nudge above top of bar
            size = 3,position = position) + 
  ggtitle("Receitas por Origem (Até 12/11) \n(Câmara Municipal)") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::dollar_format(prefix = "R$"),limits = c(0,170000)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

####### DESPESAS #####
despesas<-fread("despesas_contratadas_candidatos_2020_CE.csv",dec = ",") %>% filter(NM_UE=="PACAJUS")
despesas_pref<- despesas %>% filter(DS_CARGO=="Prefeito")
desp_pref<- despesas_pref %>% group_by(DS_ORIGEM_DESPESA) %>% summarise(Total=sum(VR_DESPESA_CONTRATADA)) %>% arrange(desc(Total))
despesas_ver<- despesas %>% filter(DS_CARGO=="Vereador")
desp_ver<- despesas_ver %>% group_by(DS_ORIGEM_DESPESA) %>% summarise(Total=sum(VR_DESPESA_CONTRATADA)) %>% arrange(desc(Total)) %>% top_n(15)

desp_pref %>% ggplot(aes(x=reorder(DS_ORIGEM_DESPESA,Total),y=Total,label = scales::dollar(Total,prefix = "R$",big.mark = ".",decimal.mark = ","))) +
  geom_bar(stat = "identity",position=position) + 
  coord_flip() +
  xlab(" ") +
  ylab("Despesas") +
  geom_text(hjust=-0.05,    # nudge above top of bar
            size = 3,position = position) + 
  ggtitle("Valor das Despesas Contratadas até 12/11 \n(Prefeitura)") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::dollar_format(prefix = "R$"),limits = c(0,300000)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

desp_ver %>% ggplot(aes(x=reorder(DS_ORIGEM_DESPESA,Total),y=Total,label = scales::dollar(Total,prefix = "R$",big.mark = ".",decimal.mark = ","))) +
  geom_bar(stat = "identity",position=position) + 
  coord_flip() +
  xlab(" ") +
  ylab("Despesas") +
  geom_text(hjust=-0.05,    # nudge above top of bar
            size = 3,position = position) + 
  ggtitle("Valor das Despesas Contratadas até 12/11 \n(Câmara Municipal)") + 
  theme(text=element_text(size=8,  family="Arial Narrow",face = "bold")) + 
  labs(subtitle = "Pacajus - CE") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),labels=scales::dollar_format(prefix = "R$"),limits = c(0,80000)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))



veread_rec<-c(rec_ver$NM_CANDIDATO)
veread_desp<-c(desp_ver$NM_CANDIDATO)
intersect(veread_rec,veread_desp)
setdiff(veread_rec,veread_desp)
