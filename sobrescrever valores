getwd() #Shows the default working directory 
setwd('C:\\Users\\dir\\vR') #muda o diretorio


#install.packages("readxl") #instala a biblioteca
library("readxl")
dados<- read_excel("tabela_transformada_R.xlsx") #le o arquivo excel
options(max.print=1000000)
dados = as.data.frame(dados)
#dados[ , 5] achar o index, não será utilizado... 
# #VAR SX 1ignorado - 2017 ---------------------------------------------------
dados$`1ignorado_SX`[dados$`1ignorado_SX`=='F'] <- '11'
dados$`1ignorado_SX`[dados$`1ignorado_SX`=='M'] <- '12'

dados$`2000_SX`[dados$`2000_SX`=='F'] <- '11'
dados$`2000_SX`[dados$`2000_SX`=='M'] <- '12'

# VAR PS 1ignorado - 2017 ----------------------------------------------------
xnew <- dados$`1ignorado_PS`
xnew[xnew>=2500] <-13
xnew[xnew>= 1500 & xnew<=2499] <-14
xnew[xnew<=1499&xnew!=14&xnew!=13]<-15
dados$`1ignorado_PS` <- xnew


xnew <- dados$`2000_PS`
xnew[xnew>=2500] <-13
xnew[xnew>= 1500 & xnew<=2499] <-14
xnew[xnew<=1499&xnew!=14&xnew!=13]<-15
dados$`2000_PS` <- xnew



# VAR GST 1ignorado - 2017 ------------------------------------------------
dados$`1ignorado_GST`[dados$`1ignorado_GST`=='>42semanas'] <- '16'
dados$`1ignorado_GST`[dados$`1ignorado_GST`=='37-41semanas'] <- '16'
dados$`1ignorado_GST`[dados$`1ignorado_GST`=='<22semanas'] <- '17'
dados$`1ignorado_GST`[dados$`1ignorado_GST`=='22-27semanas'] <- '17'
dados$`1ignorado_GST`[dados$`1ignorado_GST`=='28-31semanas'] <- '17'
dados$`1ignorado_GST`[dados$`1ignorado_GST`=='32-36semanas'] <- '17'
dados$`1ignorado_GST`[dados$`1ignorado_GST`=='8'] <- 'NA'
dados$`1ignorado_GST`[dados$`1ignorado_GST`=='ignorado'] <- 'NA'

dados$`2000_GST`[dados$`2000_GST`=='>42semanas'] <- '16'
dados$`2000_GST`[dados$`2000_GST`=='37-41semanas'] <- '16'
dados$`2000_GST`[dados$`2000_GST`=='<22semanas'] <- '17'
dados$`2000_GST`[dados$`2000_GST`=='22-27semanas'] <- '17'
dados$`2000_GST`[dados$`2000_GST`=='28-31semanas'] <- '17'
dados$`2000_GST`[dados$`2000_GST`=='32-36semanas'] <- '17'
dados$`2000_GST`[dados$`2000_GST`=='8'] <- 'NA'
dados$`2000_GST`[dados$`2000_GST`=='ignorado'] <- 'NA'

# VAR LN 1ignorado - 2017 -------------------------------------------------------------
dados$`1ignorado_LN`[dados$`1ignorado_LN`=='hospital']<-'18'
dados$`1ignorado_LN`[dados$`1ignorado_LN`=='domicilio']<-'19'
dados$`1ignorado_LN`[dados$`1ignorado_LN`=='outros_saude']<-'20'
dados$`1ignorado_LN`[dados$`1ignorado_LN`=='outros']<-'20'
dados$`1ignorado_LN`[dados$`1ignorado_LN`=='ignorado']<-'NA'

dados$`2000_LN`[dados$`2000_LN`=='hospital']<-'18'
dados$`2000_LN`[dados$`2000_LN`=='domicilio']<-'19'
dados$`2000_LN`[dados$`2000_LN`=='outros_saude']<-'20'
dados$`2000_LN`[dados$`2000_LN`=='outros']<-'20'
dados$`2000_LN`[dados$`2000_LN`=='ignorado']<-'NA'


# VAR IDADE 1ignorado - 2017 ------------------------------------------------
ynew <- dados$`1ignorado_IDADE`
ynew[ynew<=19]<-21 #até 19
ynew[ynew>=20&ynew<=35]<-22 #20 até 35 anos
ynew[ynew>=36]<-23 #>36 anos
dados$`1ignorado_IDADE` <- ynew

ynew <- dados$`2000_IDADE`
ynew[ynew<=19]<-21 #até 19
ynew[ynew>=20&ynew<=35]<-22 #20 até 35 anos
ynew[ynew>=36]<-23 #>36 anos
dados$`2000_IDADE` <- ynew

ynew <- dados$`2001_IDADE`
ynew[ynew<=19]<-21 #até 19
ynew[ynew>=20&ynew<=35]<-22 #20 até 35 anos
ynew[ynew>=36]<-23 #>36 anos
dados$`2001_IDADE` <- ynew

# VAR ESTCIV 1ignorado - 2017 -----------------------------------------------
dados$`1ignorado_ESTICV`[dados$`1ignorado_ESTICV`=='casada']<-'24'
dados$`1ignorado_ESTICV`[dados$`1ignorado_ESTICV`=='un_estavel']<-'24'
dados$`1ignorado_ESTICV`[dados$`1ignorado_ESTICV`=='separada']<-'25'
dados$`1ignorado_ESTICV`[dados$`1ignorado_ESTICV`=='solteira']<-'25'
dados$`1ignorado_ESTICV`[dados$`1ignorado_ESTICV`=='viuva']<-'25'
dados$`1ignorado_ESTICV`[dados$`1ignorado_ESTICV`=='ignorada']<-'ignorado'
dados$`1ignorado_ESTICV`[dados$`1ignorado_ESTICV`=='NA']<-'ignorado'

dados$`2000_ESTICV`[dados$`2000_ESTICV`=='casada']<-'24'
dados$`2000_ESTICV`[dados$`2000_ESTICV`=='un_estavel']<-'24'
dados$`2000_ESTICV`[dados$`2000_ESTICV`=='separada']<-'25'
dados$`2000_ESTICV`[dados$`2000_ESTICV`=='solteira']<-'25'
dados$`2000_ESTICV`[dados$`2000_ESTICV`=='viuva']<-'25'
dados$`2000_ESTICV`[dados$`2000_ESTICV`=='ignorada']<-'ignorado'
dados$`2000_ESTICV`[dados$`2000_ESTICV`=='NA']<-'ignorado'

# VAR ESCA 1ignorado - 2017 -------------------------------------------------
dados$`1ignorado_ESC`[dados$`1ignorado_ESC`=='nenhum']<-'26'
dados$`1ignorado_ESC`[dados$`1ignorado_ESC`=='1_3anos']<-'26'
dados$`1ignorado_ESC`[dados$`1ignorado_ESC`=='4_7anos']<-'27'
dados$`1ignorado_ESC`[dados$`1ignorado_ESC`=='8_11anos']<-'28'
dados$`1ignorado_ESC`[dados$`1ignorado_ESC`=='>12anos']<-'28'
dados$`1ignorado_ESC`[dados$`1ignorado_ESC`=='ignorado']<-'ignorado'
dados$`1ignorado_ESC`[dados$`1ignorado_ESC`=='NA']<-'ignorado'

dados$`2000_ESC`[dados$`2000_ESC`=='nenhum']<-'26'
dados$`2000_ESC`[dados$`2000_ESC`=='1_3anos']<-'26'
dados$`2000_ESC`[dados$`2000_ESC`=='4_7anos']<-'27'
dados$`2000_ESC`[dados$`2000_ESC`=='8_11anos']<-'28'
dados$`2000_ESC`[dados$`2000_ESC`=='>12anos']<-'28'
dados$`2000_ESC`[dados$`2000_ESC`=='ignorado']<-'ignorado'
dados$`2000_ESC`[dados$`2000_ESC`=='NA']<-'ignorado'

# VAR d_risco 1ignorado - 2017 --------------------------------------------------------------
dados$`1ignorado_d_risco`[dados$`1ignorado_d_risco`=='risco']<-'29'
dados$`1ignorado_d_risco`[dados$`1ignorado_d_risco`=='nao_risco']<-'30'
dados$`1ignorado_d_risco`[dados$`1ignorado_d_risco`=='ignorado']<-'ignorado'
dados$`1ignorado_d_risco`[dados$`1ignorado_d_risco`=='NA']<-'ignorado'

dados$`2000_d_risco`[dados$`2000_d_risco`=='risco']<-'29'
dados$`2000_d_risco`[dados$`2000_d_risco`=='nao_risco']<-'30'
dados$`2000_d_risco`[dados$`2000_d_risco`=='ignorado']<-'ignorado'
dados$`2000_d_risco`[dados$`2000_d_risco`=='NA']<-'ignorado'

# VAR GR 1ignorado - 2017 ------------------------------------------------
dados$`1ignorado_GR`[dados$`1ignorado_GR`=='1']<-'36'
dados$`1ignorado_GR`[dados$`1ignorado_GR`=='2']<-'37'
dados$`1ignorado_GR`[dados$`1ignorado_GR`=='3']<-'38'
dados$`1ignorado_GR`[dados$`1ignorado_GR`=='>=3'&dados$`1ignorado_GR`=='<=8']<-'38'
dados$`1ignorado_GR`[dados$`1ignorado_GR`=='ignorado']<-'ignorado'
dados$`1ignorado_GR`[dados$`1ignorado_GR`=='9']<-'ignorado'
dados$`1ignorado_GR`[is.na(dados$`1ignorado_GR`)] = ignorado

dados$`2000_GR`[dados$`2000_GR`=='1']<-'36'
dados$`2000_GR`[dados$`2000_GR`=='2']<-'37'
dados$`2000_GR`[dados$`2000_GR`=='3']<-'38'
dados$`2000_GR`[dados$`2000_GR`=='>=3'&dados$`2000_GR`=='<=8']<-'38'
dados$`2000_GR`[dados$`2000_GR`=='ignorado']<-'ignorado'
dados$`2000_GR`[dados$`2000_GR`=='9']<-'ignorado'
dados$`2000_GR`[is.na(dados$`2000_GR`)] = ignorado

dados$`2001_GR`[dados$`2001_GR`=='1']<-'36'
dados$`2001_GR`[dados$`2001_GR`=='2']<-'37'
dados$`2001_GR`[dados$`2001_GR`=='3']<-'38'
dados$`2001_GR`[dados$`2001_GR`=='>=3'&dados$`2001_GR`=='<=8']<-'38'
dados$`2001_GR`[dados$`2001_GR`=='ignorado']<-'ignorado'
dados$`2001_GR`[dados$`2001_GR`=='9']<-'ignorado'
dados$`2001_GR`[is.na(dados$`2001_GR`)] = ignorado

# VAR PRT 1ignorado - 2017 ---------------------------------------------------
dados$`1ignorado_PRT`[dados$`1ignorado_PRT`=='vaginal']<-'31'
dados$`1ignorado_PRT`[dados$`1ignorado_PRT`=='cesareo']<-'32'
dados$`1ignorado_PRT`[dados$`1ignorado_PRT`=='ignorado']<-'ignorado'
dados$`1ignorado_PRT`[dados$`1ignorado_PRT`=='NA']<-'ignorado'

dados$`2000_PRT`[dados$`2000_PRT`=='vaginal']<-'31'
dados$`2000_PRT`[dados$`2000_PRT`=='cesareo']<-'32'
dados$`2000_PRT`[dados$`2000_PRT`=='ignorado']<-'ignorado'
dados$`2000_PRT`[dados$`2000_PRT`=='NA']<-'ignorado'

# VAR CST 1ignorado - 2017 -----------------------------------------------
dados$`1ignorado_CST`[dados$`1ignorado_CST`=='nenhuma']<-'33'
dados$`1ignorado_CST`[dados$`1ignorado_CST`=='1_3']<-'34'
dados$`1ignorado_CST`[dados$`1ignorado_CST`=='4_6']<-'34'
dados$`1ignorado_CST`[dados$`1ignorado_CST`=='7_e_mais']<-'35'
dados$`1ignorado_CST`[dados$`1ignorado_CST`=='ignorado']<-'ignorado'
dados$`1ignorado_CST`[dados$`1ignorado_CST`=='NA']<-'ignorado'
dados$`1ignorado_CST`[dados$`1ignorado_CST`=='8']<-'ignorado'

dados$`2000_CST`[dados$`2000_CST`=='nenhuma']<-'33'
dados$`2000_CST`[dados$`2000_CST`=='1_3']<-'34'
dados$`2000_CST`[dados$`2000_CST`=='4_6']<-'34'
dados$`2000_CST`[dados$`2000_CST`=='7_e_mais']<-'35'
dados$`2000_CST`[dados$`2000_CST`=='ignorado']<-'ignorado'
dados$`2000_CST`[dados$`2000_CST`=='NA']<-'ignorado'
dados$`2000_CST`[dados$`2000_CST`=='8']<-'ignorado'


# VAR RC 1ignorado - 2017 ----------------------------------------------
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='branca']<-'39'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='amarela']<-'39'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='preta']<-'40'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='parda']<-'40'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='amarela']<-'40'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='NA']<-'ignorado'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='ignorado']<-'ignorado'
dados$`1ignorado_RC`[is.na(dados$`1ignorado_RC`)] = ignorado

dados$`2000_RC`[dados$`2000_RC`=='branca']<-'39'
dados$`2000_RC`[dados$`2000_RC`=='amarela']<-'39'
dados$`2000_RC`[dados$`2000_RC`=='preta']<-'40'
dados$`2000_RC`[dados$`2000_RC`=='parda']<-'40'
dados$`2000_RC`[dados$`2000_RC`=='amarela']<-'40'
dados$`2000_RC`[dados$`2000_RC`=='NA']<-'ignorado'
dados$`2000_RC`[dados$`2000_RC`=='ignorado']<-'ignorado'
dados$`2000_RC`[is.na(dados$`2000_RC`)] = ignorado


# VAR RC 1ignorado - 2017 -------------------------------------------------
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='branca']<-'41'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='amarela']<-'41'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='preta']<-'42'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='parda']<-'42'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='amarela']<-'42'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='NA']<-'ignorado'
dados$`1ignorado_RC`[dados$`1ignorado_RC`=='ignorado']<-'ignorado'
dados$`1ignorado_RC`[is.na(dados$`1ignorado_RC`)] = ignorado

dados$`2000_RC`[dados$`2000_RC`=='branca']<-'41'
dados$`2000_RC`[dados$`2000_RC`=='amarela']<-'41'
dados$`2000_RC`[dados$`2000_RC`=='preta']<-'42'
dados$`2000_RC`[dados$`2000_RC`=='parda']<-'42'
dados$`2000_RC`[dados$`2000_RC`=='amarela']<-'42'
dados$`2000_RC`[dados$`2000_RC`=='NA']<-'ignorado'
dados$`2000_RC`[dados$`2000_RC`=='ignorado']<-'ignorado'
dados$`2000_RC`[is.na(dados$`2000_RC`)] = ignorado

# salvar ------------------------------------------------------------------
write.csv((dados.file = 'C:\\Users\\novodir\\vR\\dados.csv'))
install.packages('lessR')
library(lessR)
save(dados, file = "dados.RData")
dados = as.data.frame(dados)
Write("dados", format="Excel")
write.csv(dados, file = "dados.csv")
