rm(list=ls())

#para usar qualquer comando ? so dar ctrl + enter na linha, que ele ira executar o comando da linha
install.packages('tidyverse')

#so precisa instalar uma vez por computador
#ai esse comando aqui em baixo testa pra ver se tem, se n?o tiver ele vai dizer que nao tem

library(tidyverse)

#esse comando aqui vc escolhe a base que vai usar

base <- read.csv(file.choose(),header = TRUE, sep = ';', dec = ',')

#isso vai ajudar a eu tratar a base, tem que rodar tamb?m
attach(base)

#vou fazer recursivamente(n sei explicar isso sem usar os termos tecnicos), mas vou usar essa base tratada
#e bom tbm que ai vc pode ver as diferenças dela olhando pra base sem tratamento tbm
baseTratada <- base


#primeiro, pego as informações que seram necessarias para contagem, e coloco separadas.
baseParaContagemDeCodFamiliar <- data.frame(d.cod_familiar_fam,p.cod_parentesco_rf_pessoa)

#agora eu uso um funcao que faz as informacoes virarem um table, usando a caracteristica
#da table de contar coisas repetidas pra isso ja
contagemDeCodFamiliarTable <- table(d.cod_familiar_fam)

#agora eu coloco isso em um data frame, ele vai organizar em duas colunas, uma e os codFamiliares
# a outra vai ser a frequencia com que se repetem
codFamiliarContados <- data.frame(contagemDeCodFamiliarTable) 

#aqui eu troco o nome da coluna de Var1 que e automatico do data frame pra codFamiliar dnv
colnames(codFamiliarContados)[1] = 'd.cod_familiar_fam'

#agora eu dou um merge (em R merge e o procv), para deixar tudo do mesmo tamanho dnv
codFamiliaresContadosRepetidos <- merge(baseParaContagemDeCodFamiliar, codFamiliarContados, by = "d.cod_familiar_fam")


#agora eu tenho um data frame com todos os cod familiar contados, so que ele se repetem
#juntos com os codFamiliares contados, ai apartir disso eu so vou querer agora
#uma Coluna com o valor contado indo apenas para o RF
#Aqui eu faço a coluna para o numero de Cod contados ir apenas paras os RF, que nao for recebe
# 1 apenas
codFamiliaresApenasNosRFs <- codFamiliaresContadosRepetidos %>%
  mutate(PessoasContadas = case_when(p.cod_parentesco_rf_pessoa == 1 & Freq > 1 ~ Freq,
                                     T ~ 1))
#agora so colocar essa coluna na base... e é isso ;)
baseTratada <- baseTratada %>%
  mutate(PESSOAS_CONTADAS_POR_COD_FAMILIAR = codFamiliaresApenasNosRFs$PessoasContadas)


#so pra facilitar dnv
attach(baseTratada)

#agora vou estar pegando as informacoes pra fazer a base de mae solo(sem conjuge homem, RF e com mais alguem no cadastro)
baseParaMaesSolos <- data.frame(codFamiliaresApenasNosRFs, p.cod_sexo_pessoa) 

#agora eu vou fazer uma coluna pra procurar quem e conjuge
conjuges <- baseParaMaesSolos %>%
  mutate(homensConjuges = case_when(
    baseParaMaesSolos$p.cod_parentesco_rf_pessoa == 2 & baseParaMaesSolos$p.cod_sexo_pessoa == 1 ~ "Conjuge",
    T ~ 'nao conjuge(ignorar)'
  ))

#agora eu vou colocar os codFamiliares e os conjuges em outra base pra facilitar mexer depois
codFamiliarComConjuges <- data.frame(conjuges$d.cod_familiar_fam , conjuges$homensConjuges)


#agora eu vou colocar so os codigos familiares QUE TEM conjuges, todos aqui tem conjuges
apenasCodFamiliaresQueTemConjuges <- filter(codFamiliarComConjuges,codFamiliarComConjuges$conjuges.homensConjuges == 'Conjuge')

#mudando o nome das colunas para cod familiar dnv para poder usar o "by", nos left join ou merge(que e a mesma coisa)
colnames(codFamiliarComConjuges)[1] = 'd.cod_familiar_fam'
colnames(apenasCodFamiliaresQueTemConjuges)[1] = 'd.cod_familiar_fam'


#aqui eu vou colocar todos os conjuges em todos os codigos, para deixar a coluna certa, se tiver o conjuge masculino, ele vai mostrar isso pra qualquer - 
# - codigo familiar.
baseParaMaesSolos <- left_join(baseParaMaesSolos, apenasCodFamiliaresQueTemConjuges, by = 'd.cod_familiar_fam', multiple = "first")

#trocando o nome da coluna que tava muito estranho pelo leftjoin
colnames(baseParaMaesSolos)[6] = 'familiasComHomemsConjuge'


#aqui eu troco so pra nao deixaro <NA> mesmo
baseParaMaesSolos$familiasComHomemsConjuge[is.na(baseParaMaesSolos$familiasComHomemsConjuge)] <- 'sem conjuge'

#um print so pra mostrar se ta indo tudo certo
baseParaMaesSolos

#Aqui vou começar pegando os codigos de quem e filho
codigoComDoisFilhos <- baseTratada %>%
  select(d.cod_familiar_fam, p.cod_parentesco_rf_pessoa) %>%
  filter(p.cod_parentesco_rf_pessoa == 3)

#agora vou contar quantos filhos tem em cada familia
filhosContados <- table(codigoComDoisFilhos$d.cod_familiar_fam)
filhosContados <- data.frame(filhosContados)

#agora e so pegar os codigos familiares que tem 2 OU MAIS filhos
filhosContados <- filhosContados %>%
  filter(Freq > 1) %>%
  mutate(doisOuMaisFilhos = "SIM") %>%
  select(-Freq) 

#mudando o tipo da variavel pra fazer o left join
filhosContados$Var1 <- as.character(filhosContados$Var1)
filhosContados$Var1 <- as.numeric(filhosContados$Var1)

#colocando a informaçao de dois ou mais filhos na base de maeSolo
baseParaMaesSolos <- left_join(baseParaMaesSolos, filhosContados, by = c("d.cod_familiar_fam" = "Var1"))


#agora sim, com todos os dados contemplados, vou fazer as condicoes necessarias
#para ser mae solo, se for, ele vai colocar mae solo, caso nao, vai colocar "empty" n sabia oq colocar, pois em geral é uma informação
#que so precisar saber se e mae solo, saber que nao e meio que tanto faz.
baseParaMaesSolos <- baseParaMaesSolos %>%
  mutate(MaeSolo = case_when(
    baseParaMaesSolos$PessoasContadas > 1 & 
      baseParaMaesSolos$p.cod_sexo_pessoa == 2 &
      baseParaMaesSolos$familiasComHomemsConjuge == 'sem conjuge' &
      baseParaMaesSolos$doisOuMaisFilhos == "SIM"
    ~ 'MAE SOLO', T ~ 'NAO'
  ))

#com tudo pronto, agora so botar a coluna na base, e ta feito ;)
#(isso de mae solo foi mt dificil slk passei 2 dias nisso)
baseTratada <- baseTratada %>%
  mutate(MAE_SOLO = baseParaMaesSolos$MaeSolo)

#Agora vou fazer a coluna "Ta atualizado", ela mostra se o cadastro da pessoas esta atualizado
#de acordo com o mes de referencia disponibilizado na base, bem util para cruzamento
#de programas sociais ;)

#Aqui eu transformo a coluna em arquivo data
baseTratada$d.ref_cad <- as.Date(baseTratada$d.ref_cad)
#ai aqui eu diminuo os 2 anos, caso for uma referencia diferente com o novo governo
#so trocar ali no numero a quantidade de anos que voce quer diminuir
baseTratada$d.ref_cad <-baseTratada$d.ref_cad %m-% years(2)

#ai eu transformo em data tbm a data de atualizacao da familia
baseTratada$d.dat_atual_fam <- as.Date(baseTratada$d.dat_atual_fam)

#agora so criar a coluna "ta atualizado", com base na logica de cadastro atualizado ;) 
baseTratada <- baseTratada %>%
  mutate(CADASTRO_ATUALIZADO = case_when(
    baseTratada$d.dat_atual_fam > baseTratada$d.ref_cad ~ 'SIM',
    T ~ 'NAO'
  ))

#################################################################################
  #COLOCANDO INFORMACAO QUE RECEBE PBF
#################################################################################

#mesmo esquema da base do cadUnico so pegar ela pra ler aqui
basePab <- read.csv(file.choose(),header = TRUE, sep = ';', dec = ',')

#isso aqui facilita eu nao ter que ficar escrevendo muito
attach(basePab)

#peguei so a informacao dos NIS e dos valores, se quiser codFamiliar so botar ai tbm
basePabTratada <-  select(basePab,NIS, VLRTOTAL)

#aqui vou tratar, pegando so os RF's (apenas quem recebe o valor), e criar a coluna para fazer o merge com a base
basePabTratada <- basePabTratada %>%
  filter(VLRTOTAL > 0) %>% 
  rename(p.num_nis_pessoa_atual = NIS) %>%
  mutate(RECEBE_PBF = "SIM") %>%
  select(-VLRTOTAL)


#agora trazendo so a coluna de quem recebe pros Rf's apenas, pelo nis.
baseTratada <- left_join(baseTratada, basePabTratada, by = 'p.num_nis_pessoa_atual', multiple = "first") 

#trocando os <NA> por nao
baseTratada$RECEBE_PBF[is.na(baseTratada$RECEBE_PBF)] <- 'NAO'

#################################################################################
#                   COLOCANDO INFORMACAO SE RECEBE CMAIS
#################################################################################

baseCmais <- read_excel(file.choose(), sheet = 'Planilha1')

#CASO TENHA UMA BASE SO COM ATIVOS
#ONDE CPF...2 É O AJUSTE QUE O PROPRIO R FAZ CASO AS DUAS COLUNAS SEJAM "CPF", CASO RESOLVAM TROCAR
#DE NOME (OQ EU INDICO), SO MUDAR ALI PRO NOME NOVO DA COLUNA
baseCmais <- baseCmais %>%
  select(CPF...2 , `GRUPO ESPECÍFICO`) %>%
  rename(p.num_cpf_pessoa = CPF...2) %>%
  rename(GRUPO_CMAIS = `GRUPO ESPECÍFICO`)
  
baseCmais$p.num_cpf_pessoa <- as.numeric(baseCmais$p.num_cpf_pessoa)


baseTratada <- left_join(baseTratada, baseCmais, by = "p.num_cpf_pessoa", multiple = "all")

baseTratada$GRUPO_CMAIS[is.na(baseTratada$GRUPO_CMAIS)] <- 'NAO RECEBE CMAIS'



###################
###CASO TENHA UMA BASE COM ATIVOS E CANCELADOS
baseCmais <- select(baseCmais, CPF , NIS , `CÓDIGO FAMILIAR`, `GRUPO ESPECÍFICO`,`STATUS DA CONTA`, )

baseCmais <- baseCmais %>%
  filter(`STATUS DA CONTA` != "CANCELADO" & `STATUS DA CONTA` != "CANCELADO - CARTÃO DEVOLVIDO" & `STATUS DA CONTA` != "CANCELADO - POR SOLICITAÇÃO" ) 

baseCmais <- baseCmais %>%
  select(CPF, `GRUPO ESPECÍFICO`) %>%
  rename(p.num_cpf_pessoa = CPF...2)
as.numeric(p.num_cpf_pessoa)

baseTratada <- left_join(baseTratada, baseCmais, by = "p.num_cpf_pessoa", multiple = "all")

baseTratada$`GRUPO ESPECÍFICO`[is.na(baseTratada$`GRUPO ESPECÍFICO`)] <- 'NAO RECEBE CMAIS'

#############################

#################################################################################
#                   COLOCANDO INFORMACAO SE RECEBE FUNCEP
#################################################################################

baseFuncep <- read_excel(file.choose(), sheet = 'Planilha2')


baseFuncep <- baseFuncep %>%
  select(`CÓDIGO FAMILIAR` , `MÃO AMIGA`) %>%
  rename(d.cod_familiar_fam = `CÓDIGO FAMILIAR`)

baseFuncep$d.cod_familiar_fam <-as.numeric(baseFuncep$d.cod_familiar_fam)


baseTratada <- left_join(baseTratada, baseFuncep, by = "d.cod_familiar_fam", multiple = "first")

baseTratada$`MÃO AMIGA`[is.na(baseTratada$`MÃO AMIGA`)] <- 'NAO RECEBE FUNCEP'




################################################################################
# COLOCANDO COLUNAS DE CRIANCA 0-3, 0-6 E IDOSOS
################################################################################



baseTratada$p.dta_nasc_pessoa <- as.Date(baseTratada$p.dta_nasc_pessoa)


baseTratada$idade = as.numeric(difftime(Sys.Date(),baseTratada$p.dta_nasc_pessoa, units = "weeks"))/52.20

#baseTratada <- baseTratada %>%
 # add_column( idade = age_calc(baseTratada$p.dta_nasc_pessoa, enddate = hoje, units = "years", precise = TRUE)) 
  
baseTratada$idade <- floor(baseTratada$idade)

baseTratada <- baseTratada %>%
  mutate(CRIANCA_0_A_3 = case_when(
    idade <= 3 ~ 'SIM', T ~ 'NAO'))%>%
  mutate(CRIANCA_0_A_6 = case_when(
    idade <= 6 ~ 'SIM', T ~ 'NAO'))%>%
    mutate(IDOSO = case_when(
      idade >= 60 ~ 'SIM',T ~  'NAO'
    ))

################################################################################
# COLOCANDO MUNICIPIO 
################################################################################

baseMunicipio <- read_excel(file.choose(), sheet = 'Planilha1')

baseTratada <- baseTratada %>%
  rename(IBGE = d.cd_ibge)

baseTratada <- left_join(baseTratada, baseMunicipio, by = "IBGE", multiple = "all")

##################################################################################
# NOVO PERFIL POBREZA
##################################################################################

baseTratada <- baseTratada %>%
  mutate(NOVA_POBREZA = case_when(
    d.vlr_renda_media_fam <= 218 ~ 'POBREZA', T ~ 'NAO'
  ))



###########################################################################################################
# A PARTE DE LOGICA E COLOCAR COLUNAS ACABOU, APARTIR DAQUI SO VOU TROCANDO INFORMACOES E LIMPANDO A BASE
###########################################################################################################

#apartir daqui eu vou come?ar a ir limpando, concatenando e substituindo informa??es na base
baseTratada <- baseTratada %>%
  mutate_at(c("d.cod_familia_indigena_fam", "d.ind_familia_quilombola_fam","p.cod_deficiencia_memb"), funs(recode(.,
                                                                                          '1' = 'SIM',
                                                                                          '2' = 'NAO'
                                                                                                      )))
baseTratada <- baseTratada %>%
  mutate_at(c("p.marc_sit_rua"), funs(recode(.,
                                             '0' = 'NAO',
                                              '1' = 'SIM'
                                              )))





baseTratada <- baseTratada %>%
  mutate_at(c("p.cod_parentesco_rf_pessoa"), funs(recode(.,
'1' = 'RESPONSÁVEL PELA UNIDADE FAMILIAR - RF',
'2' = 'CÔNJUGE OU COMPANHEIRO(A)',
'3' = 'FILHO(A)',
'4' = 'ENTEADO(A)',
'5' = 'NETO(A) OU BISNETO(A)',
'6' = 'PAI OU MÃE',
'7' = 'SOGRO(A)',
'8' = 'IRMÃO OU IRMÃ',
'9' = 'GENRO OU NORA',
'10' = 'OUTRO PARENTE',
'11' = 'NÃO PARENTE'
  )))


baseTratada <- baseTratada %>%
  mutate_at(c("p.cod_raca_cor_pessoa"), funs(recode(.,
'1' = 'BRANCA',
'2' = 'PRETA',
'3' = 'AMARELA',
'4' = 'PARDA',
'5' = 'INDÍGENA',
  )))


baseTratada <- baseTratada %>%
  mutate_at(c("p.cod_sexo_pessoa"), funs(recode(.,
 '1' = 'MASCULINO',
 '2' = 'FEMININO',
  )))







#colocando os parenteses no ddd
baseTratada$d.num_ddd_contato_1_fam <- paste0("(",d.num_ddd_contato_1_fam,")")

#concatenando o ddd (agora com os parenteses) com o telefone.

baseTratada <- baseTratada %>%
  unite(col = TELEFONE
        ,d.num_ddd_contato_1_fam,d.num_tel_contato_1_fam, sep = "")  %>%
  rename(BAIRRO = d.nom_localidade_fam) %>%
  unite(col = LOGRADOURO,
        d.nom_tip_logradouro_fam, d.nom_titulo_logradouro_fam, d.nom_logradouro_fam, sep = " ")


baseTratada <- baseTratada %>%
  rename(CODIGO_FAMILIAR = d.cod_familiar_fam, 
         CPF = p.num_cpf_pessoa,
         NIS = p.num_nis_pessoa_atual,
         PARENTESCO = p.cod_parentesco_rf_pessoa,
         N_DO_LOGRADOURO = d.num_logradouro_fam,
         DATA_DE_NASCIMENTO = p.dta_nasc_pessoa,
         RENDA_MEDIA_FAM = d.vlr_renda_media_fam,
         SEXO = p.cod_sexo_pessoa,
         INDIGENA = d.cod_familia_indigena_fam,
         QUILOMBOLA = d.ind_familia_quilombola_fam,
         DEFICIENCIA = p.cod_deficiencia_memb,
         RACA = p.cod_raca_cor_pessoa,
         SITUACAO_DE_RUA = p.marc_sit_rua,
         IDENTIDADE =  p.num_identidade_pessoa,
         NOME = p.nom_pessoa)
         
 
  
################################################################################
# FILTRANDO E COLOCANDO NAS BASES PARA EXCEL 
################################################################################
 

Novos_perfis_de_Pobreza <- baseTratada %>%
  filter(PARENTESCO == "RESPONSÁVEL PELA UNIDADE FAMILIAR - RF" & NOVA_POBREZA == 'POBREZA')%>%
  select(CPF , NOME , `MÃO AMIGA`, GRUPO_CMAIS , RECEBE_PBF , RENDA_MEDIA_FAM, CRIANCA_0_A_3, CRIANCA_0_A_6, IDOSO, MAE_SOLO , PESSOAS_CONTADAS_POR_COD_FAMILIAR , CODIGO_FAMILIAR, NIS, 
        IDENTIDADE , SEXO , RACA , INDIGENA , QUILOMBOLA , DEFICIENCIA , SITUACAO_DE_RUA , MUNICÍPIO , BAIRRO , LOGRADOURO , N_DO_LOGRADOURO , TELEFONE, PARENTESCO, 
        CADASTRO_ATUALIZADO) 

Extrema_Pobreza_e_Pobreza <- baseTratada %>%
  filter(RENDA_MEDIA_FAM <= 218) %>%
  select(CPF , NOME , `MÃO AMIGA`, GRUPO_CMAIS , RECEBE_PBF , RENDA_MEDIA_FAM, CRIANCA_0_A_3, CRIANCA_0_A_6, IDOSO, MAE_SOLO , PESSOAS_CONTADAS_POR_COD_FAMILIAR , CODIGO_FAMILIAR, NIS, 
       IDENTIDADE , SEXO , RACA , INDIGENA , QUILOMBOLA , DEFICIENCIA , SITUACAO_DE_RUA , MUNICÍPIO , BAIRRO , LOGRADOURO , N_DO_LOGRADOURO , TELEFONE, PARENTESCO, 
       CADASTRO_ATUALIZADO) 

Demais_perfis <- baseTratada %>%
  filter(d.fx_rfpc == 3 | d.fx_rfpc == 4)%>%
  select(CPF , NOME , `MÃO AMIGA`, GRUPO_CMAIS , RECEBE_PBF , RENDA_MEDIA_FAM, CRIANCA_0_A_3, CRIANCA_0_A_6, IDOSO, MAE_SOLO , PESSOAS_CONTADAS_POR_COD_FAMILIAR , CODIGO_FAMILIAR, NIS, 
         IDENTIDADE , SEXO , RACA , INDIGENA , QUILOMBOLA , DEFICIENCIA , SITUACAO_DE_RUA , MUNICÍPIO , BAIRRO , LOGRADOURO , N_DO_LOGRADOURO , TELEFONE, PARENTESCO, 
         CADASTRO_ATUALIZADO)



#################################################################################
# COLOCANDO AGORA NA PASTA
#################################################################################



write.csv2(Novos_perfis_de_Pobreza, "//172.28.64.134/doc/DADS/2023/DIRETORIA_VIGSUAS_CADUNICO/2.4 COORDENACAO_CADUNICO_PAB/CRUZAMENTOS_2023/SouRuimDeNome/Novos_perfis_de_Pobreza.csv"
           , row.names=FALSE)
  

write.csv2(Extrema_Pobreza_e_Pobreza, "//172.28.64.134/doc/DADS/2023/DIRETORIA_VIGSUAS_CADUNICO/2.4 COORDENACAO_CADUNICO_PAB/CRUZAMENTOS_2023/SouRuimDeNome/Extrema_Pobreza_e_Pobreza.csv"
           , row.names=FALSE)


write.csv2(Demais_perfis, "//172.28.64.134/doc/DADS/2023/DIRETORIA_VIGSUAS_CADUNICO/2.4 COORDENACAO_CADUNICO_PAB/CRUZAMENTOS_2023/SouRuimDeNome/Demais_perfis.csv"
           , row.names=FALSE)




################################################################################
#AGORA O APTO A RECEBER CMAIS 
################################################################################

base <- read.csv(file.choose(),header = TRUE, sep = ';', dec = ',')
Extrema_Pobreza_e_Pobreza <- base

Extrema_Pobreza_e_Pobreza <- Extrema_Pobreza_e_Pobreza %>%
  mutate(aptoParaCmais = case_when(
    PARENTESCO == "RESPONSÁVEL PELA UNIDADE FAMILIAR - RF" &
      GRUPO_CMAIS == 'NAO RECEBE CMAIS' &
      MÃO.AMIGA == 'NAO RECEBE FUNCEP'&
      CADASTRO_ATUALIZADO == 'SIM' ~ 'APTO', T ~ 'NAO'
    
  ))

apto_a_receber_cmais <- Extrema_Pobreza_e_Pobreza %>%
  filter(aptoParaCmais == "APTO" & PESSOAS_CONTADAS_POR_COD_FAMILIAR > 1 )


write.csv2(apto_a_receber_cmais, "//172.28.64.134/doc/DADS/2023/DIRETORIA_VIGSUAS_CADUNICO/2.4 COORDENACAO_CADUNICO_PAB/CRUZAMENTOS_2023/SouRuimDeNome/Perfis_aptos_a_receber_cmais.csv"
           , row.names=FALSE)


