






	pollutantmean <- function(directory, x, id){

    fl <- list.files(directory, full.names = TRUE) 
    #cria um objeto com o path dos arquivos csv,
    d <- data.frame()
    #para colar linhas ou colunas, é necessário uma data.frame base. Criei uma data frame vazia para esse fim,
    for(i in id){
        d <- rbind(d, read.csv(fl[i]))
    }
    #com a ajuda de um for-loop, usei a função rbind(colar linhas) para colar linha de uma data.frame com outras,
    mean(d[,x], na.rm = TRUE)
    #média de acordo com a coluna selecionada. 
	}





	complete <- function(directory, id){

    fl <- list.files(directory, full.names = TRUE) 
    d <- data.frame()
    for(i in id){
        d <- rbind(d, read.csv(fl[i]))
    }
	
    bad <- is.na(d$sulfate & d$nitrate)
    e <- d[!bad,]
    #removi todos os na do arquivo,
    
    v <- numeric(length(id))
    k <- 1
    #criei um vetor v vazio com extenção igual ao id e um indexador k para que os números saiam nas ordens,
    
    for(j in id) {
        row.of.df <- nrow(e[e$ID == j,])
        #data frame para guardar os resultados,
        row.vec <- unlist(row.of.df)
        #unlist para guardar os resultados como vetor,
        v[k] <- row.vec
        k <- k + 1 
        #guardei os resultados no vetor vazio e indexado por k,
    }
    
    x <- data.frame(id, complete.case = v)
    x
    #resultado em data frame.
	}


	corr <- function(directory, t) {
    fl <- list.files(directory, full.names = TRUE)
    id <- cases[cases$complete.case > t, "id"]
    #cases é um arquivo que contem o id e o complete.case(número de observações sem na's),
    d <- data.frame()
    #data.frame vazio,
    
    for(i in id) {
        d <- rbind(d, read.csv(fl[i]))
    }
    #une as data.frames,
    
    good <- complete.cases(d)
    d <- d[good,]
    #remove as na's,
    
    v <- numeric(length(id))
    k <- 1
    
    for(i in id){
        correlation <- cor(d[d$ID == i, "nitrate"], d[d$ID == i,"sulfate"])
        #correlação entre as colunas de d.
        cor.unlisted <- unlist(correlation)
        v[k] <- cor.unlisted
        k <- k + 1
    }
    print(v)
}

