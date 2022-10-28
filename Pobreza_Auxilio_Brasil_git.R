# Pacotes -----------------------------------------------------------------

library(PNADcIBGE)

# Dados -------------------------------------------------------------------

pnad <- get_pnadc(year = 2021, 
                  interview = 5,
                  defyear = 2022,
                  defperiod = 1,
                  deflator = T,
                  vars = c("V2007", # Sexo
                           "V2009", # Idade
                           "V2010", # Raça
                           "V1016", # Nº da entrevista
                           "V2005", # Condicao no domicilio
                           "V5002A", # PBF
                           "V5002A2", # Valor PBF
                           "V5003A", # Recebeu rendimentos de outros programas sociais
                           "V5003A2", # Valor de outros programas sociais
                           "V5004A", # Aposentadoria e pensões INSS
                           "V5004A2", # Valor aposentadoria e pensões INSS
                           "V5005A", # Seguro-desemprego
                           "V5005A2", # Valor seguro-desemprego
                           "V5001A", # BPC
                           "V5001A2", # Valor BPC
                           "VD2003", # Numero de pessoas no domicilio
                           "VD4002", # Condição de ocupação
                           "VD4009", # Posição na ocupação e categoria do emprego do trabalho principal
                           "VD4019", # Rendimento habitual de todos os trabalhos
                           "VD4048", # Rendimento efetivo de outras fontes
                           "VD5007", # Rendimento domiciliar habitual todos os trabalhos + efetivo todas as fontes (exclusive trabalhadores domésticos)
                           "VD5008")) # Rendimento domiciliar PC habitual todos os trabalhos + efetivo todas as fontes (exclusive trabalhadores domésticos)

#pnadsalva<-pnad

# Análise -----------------------------------------------------------------

# Somar valor total por família de PBF e Outros benefícios do 4º trimestre

pnad$variables <- merge(cbind(pnad$variables, "PesoInd" = pnad$pweights),
                        aggregate(data.frame("PBF" = pnad$variables$V5002A2,
                                             "NPBF" = as.numeric(pnad$variables$V5002A) == 2,
                                             "OutBen" = pnad$variables$V5003A2,
                                             "OutBen4T" = pnad$variables$V5003A2 * 
                                               (as.numeric(pnad$variables$V5002A) == 2) * (pnad$variables$Trimestre == 4),
                                             "benef_out"=(pnad$variables$V5003A=="Sim") * 
                                               (as.numeric(pnad$variables$V5002A) == 2) * (pnad$variables$Trimestre == 4),
                                             "criancas"=( (pnad$variables$V2009 <= 6) & (as.numeric(pnad$variables$V2005)%in%1:16)),
                                             "PesoDom" = pnad$pweights),
                                  by = list("UPA" = pnad$variables$UPA,
                                            "V1008" = pnad$variables$V1008),
                                  FUN = sum,
                                  na.rm = T),
                        by = c("UPA", "V1008"),
                        all.x = T)

# Criar base para família

pnad$base.familia <- pnad$variables[as.numeric(pnad$variables$V2005) == 1,]

# Obter outros benefícios que não AB no 4 Tri

pnad$base.familia$OutBef.ExcAB <- pnad$base.familia$V5003A2

pnad$base.familia$OutBef.ExcAB[is.na(pnad$base.familia$OutBef.ExcAB)] <- 0

pnad$base.familia$OutBef.ExcAB[pnad$base.familia$OutBen4T > 0] <- ifelse(pnad$base.familia$OutBef.ExcAB[pnad$base.familia$OutBen4T > 0] < 224,
                                                                         0,
                                                                         pnad$base.familia$OutBef.ExcAB[pnad$base.familia$OutBen4T > 0] - 224)

### Simulação AB R$ 400 em 2022

pnad$base.familia$AB400 <- 0

# Benefício para famílias que recebem PBF e AB no 4 Tri

pnad$base.familia$AB400[pnad$base.familia$PBF > 0 | pnad$base.familia$OutBen4T > 0] <- 400

### Simulação AE R$ 600 em 2022

# Criação da renda per capita 

pnad$base.familia$Ypc <- pnad$base.familia$VD5008 - ((pnad$base.familia$PBF + pnad$base.familia$OutBen)/pnad$base.familia$VD2003)

# Dados para expansão de beneficiários por UF

beneficarios <- read.csv("~/diferencas_beneficiarios2122.csv")

beneficarios.uf <- aggregate(pnad$base.familia$PesoInd[pnad$base.familia$PBF > 0 | pnad$base.familia$OutBen4T > 0],
                             by = list("UF" = pnad$base.familia$UF[pnad$base.familia$PBF > 0 | pnad$base.familia$OutBen4T > 0]),
                             FUN = sum,
                             na.rm = T)

beneficarios.uf <- merge(beneficarios.uf,
                         beneficarios[c("Localidade", "expansao")],
                         by.x = c("UF"),
                         by.y = c("Localidade"))

beneficarios.uf$Projecao <- beneficarios.uf$x*beneficarios.uf$expansao

# Aleatorizar a ordem das observações

pnad$base.familia <- pnad$base.familia[sample.int(nrow(pnad$base.familia)),]

# Conferir se tem potenciais beneficiários suficentes para serem incluídos

beneficarios.uf <- merge(beneficarios.uf,
                         aggregate(data.frame("Peso.UF" = pnad$base.familia$PesoInd[pnad$base.familia$Ypc < 210 &
                                                                                      pnad$base.familia$PBF == 0 &
                                                                                      pnad$base.familia$OutBen4T == 0]),
                                   by = list("UF" = pnad$base.familia$UF[pnad$base.familia$Ypc < 210 &
                                                                           pnad$base.familia$PBF == 0 &
                                                                           pnad$base.familia$OutBen4T == 0]),
                                   FUN = sum),
                         by = c("UF"))

beneficarios.uf$dif <- beneficarios.uf$Peso.UF - beneficarios.uf$Projecao

# Regra de seleção de novos beneficiários

pnad$base.familia$NovoBene <- 0

for (uf in setdiff(unique(as.character(pnad$base.familia$UF)), "Piauí")) {
  
  novos.benef <- 0
  
  i <- 1
  
  while (novos.benef <= beneficarios.uf$Projecao[as.character(beneficarios.uf$UF) == uf]) {
    
    novos.benef <- novos.benef + pnad$base.familia$PesoInd[as.character(pnad$base.familia$UF) == uf &
                                                             pnad$base.familia$Ypc < 210 &
                                                             pnad$base.familia$PBF == 0 &
                                                             pnad$base.familia$OutBen4T == 0][i]
    
    pnad$base.familia$NovoBene[as.character(pnad$base.familia$UF) == uf &
                                 pnad$base.familia$Ypc < 210 &
                                 pnad$base.familia$PBF == 0 &
                                 pnad$base.familia$OutBen4T == 0][i] <- 1
    
    i <- i + 1
    
  }
  
}

# Atribuir para todos Piauí

pnad$base.familia$NovoBene[as.character(pnad$base.familia$UF) == "Piauí" &
                             pnad$base.familia$Ypc < 210 &
                             pnad$base.familia$PBF == 0 &
                             pnad$base.familia$OutBen4T == 0] <- 1


# Atribuir para aquilo que faltou no Piauí

novos.benef <- 0

i <- 1

while (novos.benef <= -(beneficarios.uf$dif[as.character(beneficarios.uf$UF) == "Piauí"])) {
  
  novos.benef <- novos.benef + pnad$base.familia$PesoInd[pnad$base.familia$Ypc < 210 &
                                                           pnad$base.familia$PBF == 0 &
                                                           pnad$base.familia$OutBen4T == 0 &
                                                           pnad$base.familia$NovoBene == 0][i]
  
  pnad$base.familia$NovoBene[pnad$base.familia$Ypc < 210 &
                               pnad$base.familia$PBF == 0 &
                               pnad$base.familia$OutBen4T == 0 &
                               pnad$base.familia$NovoBene == 0][i] <- 1
  
  i <- i + 1
  
}

# Auxilio R$ 600 para todos 

pnad$base.familia$AB600 <- 0

pnad$base.familia$AB600[pnad$base.familia$AB400 > 0] <- 600

pnad$base.familia$AB600[pnad$base.familia$NovoBene == 1] <- 600
pnad$base.familia$AB400_exp<-0
pnad$base.familia$AB400_exp[pnad$base.familia$AB600 == 600] <- 400
pnad$base.familia$ABlula=pnad$base.familia$AB600+pnad$base.familia$criancas*150

# Taxas de pobreza --------------------------------------------------------

# Rendas per capita por cenário

pnad$base.familia$Ypc.AB400 <- pnad$base.familia$Ypc + pnad$base.familia$OutBef.ExcAB + (pnad$base.familia$AB400/pnad$base.familia$VD2003)
pnad$base.familia$Ypc.AB400_exp <- pnad$base.familia$Ypc + pnad$base.familia$OutBef.ExcAB + (pnad$base.familia$AB400_exp/pnad$base.familia$VD2003)

pnad$base.familia$Ypc.AB600 <- pnad$base.familia$Ypc + pnad$base.familia$OutBef.ExcAB + (pnad$base.familia$AB600/pnad$base.familia$VD2003)

pnad$base.familia$Ypc.ABlula <- pnad$base.familia$Ypc + pnad$base.familia$OutBef.ExcAB + (pnad$base.familia$ABlula/pnad$base.familia$VD2003)

# Taxa de pobreza por benefício

renda <- c("VD5008", "Ypc.AB400","Ypc.AB400_exp", "Ypc.AB600","Ypc.ABlula")

pobreza <- data.frame()

for (y in renda) {
  
  taxa <- sum((pnad$base.familia[y] <= 193)*pnad$base.familia$PesoDom)/sum(pnad$base.familia$PesoDom)
  
  pobreza <- rbind(pobreza,
                   data.frame("Renda" = y,
                              "Tx.Pobreza" = taxa))
  
}
renda <- c("VD5008", "Ypc.AB400", "Ypc.AB600","Ypc.ABlula")

dif.pobrezaextrema <--sum((pnad$base.familia$Ypc.ABlula <= 193)*pnad$base.familia$PesoDom)+sum((pnad$base.familia$Ypc.AB400_exp <= 193)*pnad$base.familia$PesoDom)
dif.pobreza<--sum((pnad$base.familia$Ypc.ABlula <= 416)*pnad$base.familia$PesoDom)+sum((pnad$base.familia$Ypc.AB400_exp <= 416)*pnad$base.familia$PesoDom)
dif.pobrezaextrema
dif.pobreza




