#############################################################
#
#   Program: query_functions.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, MD, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Write some standard functions for logic checks
#            missing values, and out of range.
#
#   INPUT: 
#   OUTPUT: standard R functions
#
#   Notes: File should be sourced by the query checks.
#
#   Created: 31 October 2012
#   Revisions: 
#     
#############################################################

check <- function(x) {
  nacols <- which(sapply(x, FUN=function(i) all(is.na(i))))
  if(any(nacols)) x <- x[,-nacols]
  df <- match("dataframe", x[,1])
  tbl <- match("tablename", x[,1])
  id <- match("id", x[,1])
  exp <- match("expected", x[,1])
  opt <- match("optional", x[,1])
  if(is.na(df) || is.na(tbl) || is.na(exp)) stop('missing parameters')
  col.ix <- na.omit(c(df,tbl,id,exp,opt))
  if(is.na(id)) {
    id <- 'patient'
  } else {
    id <- x[id,2]
  }
  df <- get(x[df,2])
  tbl <- x[tbl,2]
  exp <- as.character(x[exp,-1])
  exp <- exp[seq(min(which(nchar(exp) == 0), length(exp)+1)-1)]
  if(!is.na(opt)) {
    opt <- as.character(x[opt,-1])
    opt <- opt[seq(min(which(nchar(opt) == 0), length(opt)+1)-1)]
  }
  ops <- x[-col.ix,]
  ### need to remove LOOP segments
  loop.ix <- grep('loop', ops[,1])
  nloop <- length(loop.ix)
  if(nloop) {
    if(nloop %% 2 == 0 && all(ops[loop.ix[seq(2, nloop, by=2)],2] == 'END')) {
      loop.seg <- vector('list', nloop/2)
      for(i in seq(1, nloop, by=2)) {
        loopop <- ops[loop.ix[i],]
        loopVector <- eval(parse(text=ops[loop.ix[i],3]))
        looprepl <- loopop[rep(1, length(loopVector) * (loop.ix[i+1] - loop.ix[i] - 1)),]
        looprepl[,] <- ''
        looprows <- ops[seq(loop.ix[i]+1, loop.ix[i+1]-1),]
        for(vi in seq_along(loopVector)) {
          for(i1 in seq(nrow(looprows))) {
            for(i2 in seq(ncol(looprows))) {
              looprepl[(vi-1)*nrow(looprows)+i1,i2] <- sub(ops[loop.ix[i],2], loopVector[vi], looprows[i1,i2])
            }
          }
        }
        ops[loop.ix[i],1] <- 'LOOPREPLACE'
        ops[seq(loop.ix[i]+1, loop.ix[i+1]),] <- NA
        loop.seg[[floor(i/2)+1]] <- looprepl
      }
      ops <- ops[!is.na(ops[,1]),]
      nRepl <- 0
      while(any(grepl('LOOPREPLACE', ops[,1]))) {
        repl.ix <- grep('LOOPREPLACE', ops[,1])[1]
        nRepl <- nRepl + 1
        replacement <- loop.seg[[nRepl]]
        if(repl.ix == 1) {
          ops <- rbind(replacement, ops[-repl.ix,])
        } else if(repl.ix == nrow(ops)) {
          ops <- rbind(ops[-repl.ix,], replacement)
        } else {
          ops <- rbind(ops[seq(1,repl.ix-1),], replacement, ops[seq(repl.ix+1,nrow(ops)),])
        }
      }
      if(nRepl != length(loop.seg)) stop('invalid loop syntax')
    } else {
      stop('invalid loop syntax')
    }
  }
  nops <- nrow(ops)
  msg <- vector("list", nops)
  skip <- FALSE
  for(i in seq(nops)) {
    type <- ops[i,1]
    fun <- ops[i,2]
    ## branching logic does not allow multiple branches
    if(type == 'branch') {
      if(fun == 'END') {
        skip <- FALSE
      } else {
        skip <- !eval(parse(text=fun))
      }
      next
    }
    if(skip) {
      next
    }
    size <- min(which(nchar(ops[i,]) == 0), ncol(x)+1)-1
    args <- NULL
    namedargs <- NULL
    if(size >= 3) {
      args <- as.character(ops[i, seq(3, size)])
      n.ix <- grep("::", args)
      if(length(n.ix)) {
        namedargs <- sub("^.*::", "", args[n.ix])
        names(namedargs) <- sub("::.*$", "", args[n.ix])
        args <- args[-c(n.ix)]
      }
    }
    if(type == 'validate') {
      if(fun == 'extravar') {
        args <- list(na.omit(c(exp, opt)))
      } else if(fun == 'missvar') {
        args <- list(exp)
      }
      if("table" %in% names(namedargs)) {
        t.ix <- match('table', names(namedargs))
        mytab <- get(namedargs[t.ix])
        namedargs <- namedargs[-c(t.ix)]
      } else {
        mytab <- df
      }
      msg[[i]] <- do.call(fun, c(args, list(table=mytab, tablename=tbl, id=id), namedargs))
    } else if(type == 'transform') {
      if(fun %in% c('convertdate', 'forcenumber')) {
        makeconv <- FALSE
        if(length(args) == 1 && exists(args[1], df)) {
          var1 <- 'df'
          var2 <- df[,args[1]]
          makeconv <- TRUE
        } else if(exists(args[2])) {
          df2 <- get(args[2])
          if(exists(args[1], df2)) {
            var1 <- args[2]
            var2 <- df2[,args[1]]
            makeconv <- TRUE
          }
        }
        if(makeconv) {
          varname <- sprintf("%s[,'%s']", var1, args[1])
          assign(varname, do.call(fun, list(var2)))
        }
      } else if(fun == 'merge') {
        if(exists(args[2]) && length(args) > 3 && all(sapply(args[-c(1,2)], exists, get(args[2])))) {
          df2 <- get(args[2])
          # this should only exist within the function
          assign(args[1], merge(df, df2[,args[-c(1,2)]], all.x=TRUE))
        } else {
          stop('merge failed')
        }
      } else if(fun == 'assign') {
        if(length(args) != 2) stop('assignment failed')
        assign(args[1], eval(parse(text=args[2])))
      }
      if(exists(args[1], df)) {
        df[,args[1]] <- do.call(fun, list(df[,args[1]]))
      }
    } else {
      stop(sprintf("unknown operation %s", type))
    }
  }
  do.call('rbind', msg)
}

## WRITE FUNCTION TO CHECK FOR VARIABLES THAT SHOULD NOT BE THERE 
extravar <- function(expectednames, table, tablename, ...) {
  err <- setdiff(names(table), expectednames)
  query <- NULL
  if(length(err)) {
    query <- data.frame('', tablename, err, "Table Structure", "Unexpected Variable", "", stringsAsFactors=FALSE)
    names(query) <- names(emptyquery)
  }
  query
}

# extravar <- function(expectednames,table=parent.frame()){
#   vars <- table[!(names(table) %in% expectednames)]
#   if(ncol(vars)>0){
#     query <- data.frame("",tablename,names(vars),
#                                                                                 "Table Structure",
#                                                                                 "Unexpected Variable","",
#                                                                                 stringsAsFactors=FALSE)
#     names(query) <- names(emptyquery)
#     assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
#   }
# }
## WRITE FUNCTION TO CHECK FOR VARIABLES THAT SHOULD BE THERE
missvar <- function(expectednames, table, tablename, ...) {
  err <- setdiff(expectednames, names(table))
  query <- NULL
  if(length(err)) {
    query <- data.frame('', tablename, err, "Table Structure", "Missing Variable", "", stringsAsFactors=FALSE)
    names(query) <- names(emptyquery)
  }
  query
}
# missvar <- function(expectednames,table=parent.frame()){
#   vars <- expectednames[!(expectednames %in% names(table))]
#   if(length(vars)>0){
#     query <- data.frame("",tablename,vars,
#                                                                                 "Table Structure",
#                                                                                 "Missing Variable","",
#                                                                                 stringsAsFactors=FALSE)
#     names(query) <- names(emptyquery)
#     assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
#   }
# }
## WRITE FUNCTION TO CHECK FOR MISSING PATIENT VALUES
missingvalue <- function(var, table, tablename, id='patient', ...) {
  query <- NULL
  if(exists(var, table)) {
    varA <- sprintf("%s_a", var)
    miserr <- is.na(table[,var])
    if(exists(varA, table)) {
      myvarA <- table[,varA]
      miserr <- miserr & !(!is.na(myvarA) & myvarA == "U")
    }
    if(any(miserr)) {
      query <- data.frame(table[miserr,id], tablename, var, "Missing Value", "Missing Value", "", stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}

# missingvalue <- function(var,table=parent.frame(),id=patient){
#   var <- deparse(substitute(var))
#   id <- deparse(substitute(id))
#   if(exists(var,table)) {
#     varA <- sprintf("%s_a", var)
#     if(!exists(varA,table)) {
#       miserr <- is.na(get(var,table))
#       if(any(miserr)) {
#         query <- data.frame(get(id,table)[miserr],
#                                                                                         tablename,
#                                                                                         var,
#                                                                                         "Missing Value",
#                                                                                         "Missing Value","",
#                                                                                         stringsAsFactors=FALSE)
#         names(query) <- names(emptyquery)
#         assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
#       }
#     }
#     ## THIS SECOND LOOP IS PERTINENT FOR DATE VARIABLES WITH FLAG THAT THE DATE IS UNKNOWN *_D_A="U"
#     if(exists(varA,table)) {
#       myvarA <- get(varA, table)
#       miserr <- !(!is.na(myvarA) & myvarA == "U") & is.na(get(var,table))
#       if(any(miserr)) {
#         query <- data.frame(get(id,table)[miserr],
#                                                                                         tablename,
#                                                                                         var,
#                                                                                         "Missing Value",
#                                                                                         "Missing Value","",
#                                                                                         stringsAsFactors=FALSE)
#         names(query) <- names(emptyquery)
#         assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
#       }
#     }
#   }
# }
## WRITE FUNCTION TO CHECK FOR DATES OCCURRING IN FUTURE
futuredate <- function(var, table, tablename, id='patient', ...) {
  query <- NULL
  if(exists(var, table)) {
    datevar <- table[,var]
    if(class(datevar) != 'Date') datevar <- convertdate(datevar)
    futerr <- !is.na(datevar) & datevar > databaseclose
    ## IN THE CASE THAT A FLAG HAS BEEN PROVIDED FOR DATE VARIABLES, THEN CHECK FOR APPROPRIATE WINDOW AND DO NOT QUERY UNKNOWN DATES
    varflag <- sprintf("%s_a", var)
    if(exists(varflag, table)) {
      flag <- table[,varflag]
      ## might want to use difftime function if variables are actual dates
      ## dist <- difftime(databaseclose, table[,var], units='days')
      dist <- databaseclose - as.numeric(datevar)
      futerr[!is.na(flag) & ((flag == 'Y' & dist > -365) | (flag == 'M' & dist > -183) | (flag == 'D' & dist > -16) | flag == 'U' | flag == '<')] <- FALSE
    }
    if(any(futerr)) {
      errmsg <- sprintf("databaseclose=%s&%s=%s", format(as.Date(databaseclose, origin="1970-01-01"), format="%Y-%m-%d"), var, table[futerr, var])
      query <- data.frame(table[futerr, id], tablename, var, "Logic", "Date in the Future", errmsg, stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}
# futuredate <- function(var,table=parent.frame(),id=patient){
#   var <- deparse(substitute(var))
#   if(exists(var,table)){
#     futerr <- !is.na(get(var,table)) & get(var,table)>databaseclose
#     ## IN THE CASE THAT A FLAG HAS BEEN PROVIDED FOR DATE VARIABLES, THEN CHECK FOR APPROPRIATE WINDOW AND DO NOT QUERY UNKNOWN DATES
#     if(exists(paste(var,"_a",sep=""),table)){
#       flag <- get(paste(var,"_a",sep=""),table)
#       futerr[!is.na(flag) & flag=="Y" & databaseclose - as.numeric(get(var,table)) > -365] <- FALSE
#       futerr[!is.na(flag) & flag=="M" & databaseclose - as.numeric(get(var,table)) > -183] <- FALSE
#       futerr[!is.na(flag) & flag=="D" & databaseclose - as.numeric(get(var,table)) > -16] <- FALSE
#       futerr[!is.na(flag) & flag=="U"] <- FALSE
#       futerr[!is.na(flag) & flag=="<"] <- FALSE
#     }
#     query <- emptyquery
#     if(any(futerr)) {
#       query<-data.frame(get(deparse(substitute(id)),table)[futerr],
#                                                                         tablename,var,"Logic",
#                                                                         "Date in the Future",
#                                                                         paste("databaseclose=",format(as.Date(databaseclose,origin="1970-01-01"),format="%Y-%m-%d"),"&",var,"=",get(var,table)[futerr],sep=""),
#                                                                         stringsAsFactors=FALSE)
#     }
#     names(query) <- names(emptyquery)
#     if(nrow(query)>0){assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1}
#   }
# }
## WRITE FUNCTION TO CHECK FOR UNEXPECTED CODES
badcodes <- function(var, codelist, table, tablename, id='patient') {
  query <- NULL
  codelist <- eval(parse(text=codelist))
  if(exists(var, table)) {
    coderr <- !is.na(table[,var]) & !(table[,var] %in% codelist)
    if(any(coderr)) {
      query <- data.frame(table[coderr, id], tablename, var, "Value Error", "Unexpected Code", sprintf("%s=%s", var, table[coderr, var]), stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}

# badcodes <- function(var,codelist,table=parent.frame(),id=patient){
#   var <- deparse(substitute(var))
#   if(exists(var,table)){
#     coderr <- !is.na(get(var,table)) & !(get(var,table) %in% codelist)
#     if(any(coderr)) {
#       query<-data.frame(get(deparse(substitute(id)),table)[coderr],
#                                                                         tablename,var,"Value Error",
#                                                                         "Unexpected Code",
#                                                                         paste(var,"=",get(var,table)[coderr],sep=""),
#                                                                         stringsAsFactors=FALSE)
#       names(query) <- names(emptyquery)
#       assign(paste("query",index,sep=""),query,envir=globalenv())
#       index <<- index + 1
#     }
#   }
# }
## WRITE FUNCTION TO CHECK FOR DUPLICATES OF UNIQUEID
queryduplicates <- function(uniqueid, date=date, subsettext="", table, tablename, id='patient', ...) {
  subvar <- sub("=.*$", "", subsettext)
  query <- NULL
  if(exists(uniqueid, table)) {
    if(!missing(date)) {
      pid <- sprintf("%s&%s", table[,uniqueid], table[,date])
    } else {
      pid <- table[,uniqueid]
    }
    if(anyDuplicated(pid)) {
      duperr <- duplicated(pid)
      if(missing(date)) {
        idmsg <- id
        errmsg <- sprintf("%s=%s", id, table[duperr, id])
      } else {
        idmsg <- sprintf("%s&%s%s", id, date, subvar)
        errmsg <- sprintf("%s=%s&%s=%s%s", id, table[duperr, id], date, table[duperr, date], subsettext)
      }
      query <- data.frame(table[duperr, id], tablename, idmsg, "Logic", "Duplicate Record", errmsg, stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}

# queryduplicates <- function(uniqueid,table=parent.frame(),date=date,subsettext="",id=patient){
#   uniqueid <- deparse(substitute(uniqueid))
#   id <- deparse(substitute(id))
#   subvar <- unlist(strsplit(subsettext,"="))[1]
#   if(exists(uniqueid,table)) {
#     if(!missing(date)){
#       date <- deparse(substitute(date))
#       pid <- paste(get(uniqueid,table),"&",get(date,table))
#     }
#     if(missing(date)){pid <- get(uniqueid,table)}
#     if(any(duplicated(pid))){
#       duperr <- duplicated(pid)
#       if(missing(date)) {
#         query <- data.frame(get(id,table)[duperr],
#                                                                                 tablename,
#                                                                                 id,
#                                                                                 "Logic",
#                                                                                 "Duplicate Record",
#                                                                                 paste(id," =",get(id,table)[duperr]),
#                                                                                 stringsAsFactors=FALSE)
#       } else {
#         query <- data.frame(get(id,table)[duperr],
#                                                                                 tablename,
#                                                                                 ifelse(!is.na(subvar),paste(id,"&",date,subvar,sep=""),
#                                                                                                           paste(id,"&",date,sep="")),
#                                                                                 "Logic",
#                                                                                 "Duplicate Record",
#                                                                                 paste(id,"=",get(id,table)[duperr],"&",
#                                                                                                         date,"=",get(date,table)[duperr],subsettext,sep=""),
#                                                                                 stringsAsFactors=FALSE)
#       }
#       names(query) <- names(emptyquery)
#       assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
#     }
#   }
# }
## WRITE FUNCTION FOR OUT OF ORDER DATES, DATE2 IS SUPPOSED TO OCCUR ON OR AFTER DATE1
outoforder <- function(date1, date2, table2, table, tablename, id='patient') {
  query <- NULL
  ######## what is the purpose of table2?  should date2 use it when present?
  if(!missing(table2)) {
    tablename <- paste(tablename, "&", table2)
  }
  if(exists(date1, table) && exists(date2, table)) {
    datevar1 <- table[,date1]
    datevar2 <- table[,date2]
    if(class(datevar1) != 'Date') datevar1 <- convertdate(datevar1)
    if(class(datevar2) != 'Date') datevar2 <- convertdate(datevar2)
    logicerr <- !is.na(datevar1) & !is.na(datevar2) & datevar1 > datevar2
    d1f <- sprintf("%s_a", date1)
    d2f <- sprintf("%s_a", date2)
    if(exists(d1f, table)) {
      flag1 <- table[,d1f]
      logicerr[!is.na(flag1) & flag1 %in% c("U",">")] <- FALSE
    }
    if(exists(d2f, table)) {
      flag2 <- table[,d2f]
      logicerr[!is.na(flag2) & flag2 %in% c("U","<")] <- FALSE
    }
    if(any(logicerr)) {
      errmsg <- sprintf("%s=%s&%s=%s", date1, table[logicerr, date1], date2, table[logicerr, date2])
      query <- data.frame(table[logicerr, id], tablename, sprintf("%s&%s", date1, date2), "Logic", "Out of Order", errmsg, stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}

# outoforder <- function(date1,date2,table=parent.frame(),table2=table2,id=patient){
#   date1 <- deparse(substitute(date1))
#   date2 <- deparse(substitute(date2))
# 	if(!missing(table2)){tablename <- paste(tablename,"&",table2)}
#   if(exists(date1,table) & exists(date2,table)){
#      logicerr <- !is.na(get(date1,table)) & !is.na(get(date2,table)) & get(date1,table) > get(date2,table)
#      if(exists(paste(date1,"_a",sep=""),table)){
# 	flag1 <- get(paste(date1,"_a",sep=""),table)
# 	logicerr[!is.na(flag1) & flag1 %in% c("U",">")] <- FALSE
#      }
#      if(exists(paste(date2,"_a",sep=""),table)){
# 	flag2 <- get(paste(date2,"_a",sep=""),table)
# 	logicerr[!is.na(flag2) & flag2 %in% c("U","<")] <- FALSE
#      }
#      if(any(logicerr)){
#       query <- data.frame(get(deparse(substitute(id)),table)[logicerr],
#      										 tablename,paste(date1,"&",date2,sep=""),
#      										 "Logic",
#      										 "Out of Order",
#      										 paste(date1,"=",get(date1,table)[logicerr],"&",date2,"=",get(date2,table)[logicerr],sep=""),
#      										 stringsAsFactors=FALSE)
#      names(query) <- names(emptyquery)
#      assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
#      }
#   }
# }
## CONVERT DATE VARIABLES AND PERFORM LOGIC CHECK AGAINST DATE OF BIRTH
convertdate <- function(date) {
  if(all(is.numeric(date))) {
    return(as.Date(date, origin='1970-01-01'))
  }
  return(as.Date(date, format="%Y-%m-%d"))
}
## WRITE FUNCTION TO CHECK FOR OUT OF RANGE DATA
upperrangecheck <- function(var, value, subsettext="", table, tablename, id='patient') {
  subvar <- sub("=.*$", "", subsettext)
  query <- NULL
  if(exists(var, table)) {
    coderr <- !is.na(table[,var]) & as.numeric(table[,var]) > as.numeric(value)
    if(any(coderr)) {
      query <- data.frame(table[coderr, id], tablename, sprintf("%s%s", var, subvar), "Logic", "Out of Range", sprintf("%s=%s%s", var, table[coderr, var], subsettext), stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}
# upperrangecheck <- function(var,value,table=parent.frame(),subsettext="",id=patient){
#   var <- deparse(substitute(var))
#   subvar <- unlist(strsplit(subsettext,"="))[1]
#   if(exists(var,table)) {
#     coderr <- !is.na(get(var,table)) & get(var,table) > value
#     if(any(coderr)) {
#       query<-data.frame(get(deparse(substitute(id)),table)[coderr],
#                                                                         tablename,ifelse(!is.na(subvar),paste(var,subvar,sep=""),var),"Logic",
#                                                                         "Out of Range",
#                                                                         paste(var,"=",get(var,table)[coderr],subsettext,sep=""),
#                                                                         stringsAsFactors=FALSE)
#       names(query) <- names(emptyquery)
#       assign(paste("query",index,sep=""),query,envir=globalenv())
#       index <<- index + 1
#     }
#   }
# }
## WRITE FUNCTION TO CHECK FOR OUT OF RANGE DATA
lowerrangecheck <- function(var, value, subsettext="", table, tablename, id='patient') {
  subvar <- sub("=.*$", "", subsettext)
  query <- NULL
  if(exists(var, table)) {
    coderr <- !is.na(table[,var]) & as.numeric(table[,var]) < as.numeric(value)
    if(any(coderr)) {
      query <- data.frame(table[coderr, id], tablename, sprintf("%s%s", var, subvar), "Logic", "Out of Range", sprintf("%s=%s%s", var, table[coderr, var], subsettext), stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}

# lowerrangecheck <- function(var,value,table=parent.frame(),subsettext="",id=patient){
#   var <- deparse(substitute(var))
#   subvar <- unlist(strsplit(subsettext,"="))[1]
#   if(exists(var,table)){
#     coderr <- !is.na(get(var,table)) & get(var,table) < value
#     if(any(coderr)) {
#       query<-data.frame(get(deparse(substitute(id)),table)[coderr],
#                                                 tablename,ifelse(!is.na(subvar),paste(var,subvar,sep=""),var),"Logic",
#                                                 "Out of Range",
#                                                 paste(var,"=",get(var,table)[coderr],subsettext,sep=""),
#                                                 stringsAsFactors=FALSE)
#       names(query) <- names(emptyquery)
#       assign(paste("query",index,sep=""),query,envir=globalenv())
#       index <<- index + 1
#     }
#   }
# }
## WRITE FUNCTION TO CHECK FOR UNEXPECTED VARIABLE TYPE
notnumeric <- function(var, table, tablename, id='patient') {
  query <- NULL
  if(exists(var,table)) {
    numerr <- grepl("[:alpha:]", table[,var])
    if(any(numerr)) {
      query <- data.frame(table[numerr, id], tablename, var, "Value Error", "Unexpected Type", sprintf("%s=%s&type=numeric", var, table[numerr, var]), stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}

# notnumeric <- function(var,table=parent.frame(),id=patient){
#   var <- deparse(substitute(var))
#   if(exists(var,table)){
#     numerr <- grepl("[:alpha:]",get(var,table))
#     if(any(numerr)) {
#       query<-data.frame(get(deparse(substitute(id)),table)[numerr],
#                                                 tablename,var,"Value Error",
#                                                 "Unexpected Type",
#                                                 paste(var,"=",get(var,table)[numerr],"&type=numeric",sep=""),
#                                                 stringsAsFactors=FALSE)
#       names(query) <- names(emptyquery)
#       assign(paste("query",index,sep=""),query,envir=globalenv())
#       index <<- index + 1
#     }
#   }
# }
## FORCE NUMBER will take a factor/character to a number without issuing the warnings
forcenumber <- function(var) {
  setting <- options('warn')
  options(warn = -1)
  x <- as.numeric(gsub("[^0-9.]", "", as.character(var)))
  options(setting)
  return(x)
}

## WRITE FUNCTION TO CHECK FOR UNEXPECTED VARIABLE TYPE
notdate <- function(var, table, tablename, id='patient', ...) {
  query <- NULL
  if(exists(var, table)) {
    datavar <- table[,var]
    ## must be in YYYY-MM-DD format
    numerr <- !is.na(datavar) & !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", datavar)
## ADDED TO FLAG BAD DATES AS VALUE ERRORS - 3/29/2013
    ## bad dates will be forced to missing by as.Date(), eg 2010-07-32
    dateform <- as.Date(datavar,"%Y-%m-%d")
    ## if missing the formatted date and not the original date, then we know something was wrong with one component (avoids programming this check seperately)
    numerr[!is.na(datavar) & is.na(dateform)] <- TRUE
    if(any(numerr)) {
      query <- data.frame(table[numerr,id], tablename, var, "Value Error", "Unexpected Type", sprintf("%s=%s&type=date", var, datavar[numerr]), stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}
# notdate <- function(var,table=parent.frame(),id=patient){
#   var <- deparse(substitute(var))
#   if(exists(var,table)){
#     datavar <- get(var,table)
#     ## must be in YYYY-MM-DD format
#     numerr <- !is.na(datavar) & !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", datavar)
# ## ADDED TO FLAG BAD DATES AS VALUE ERRORS - 3/29/2013
#     ## bad dates will be forced to missing by as.Date(), eg 2010-07-32
#     dateform <- as.Date(datavar,"%Y-%m-%d")
#     ## if missing the formatted date and not the original date, then we know something was wrong with one component (avoids programming this check seperately)
#     numerr[!is.na(datavar) & is.na(dateform)] <- TRUE
#     if(any(numerr)){
#       query<-data.frame(get(deparse(substitute(id)),table)[numerr],
#       									tablename,var,"Value Error",
#       									"Unexpected Type",
#       									paste(var,"=",datavar[numerr],"&type=date",sep=""),
#       									stringsAsFactors=FALSE)
#       names(query) <- names(emptyquery)
#       assign(paste("query",index,sep=""),query,envir=globalenv())
#       index <<- index + 1
#     }
#   }
# }

## WRITE FUNCTION TO CHECK FOR RECORDS THAT EXIST WHEN NOT PROPERLY INDICATED (eg, tblBAS)
badrecord <- function(uniqueid, subset, superset, subsettext="", table, tablename, id='patient', ...) {
  query <- NULL
  subvar <- gsub("!", "", sub("=.*$", "", subsettext))
  if(is.character(subset)) subset <- get(subset)
  if(is.character(superset)) superset <- get(superset)
  if(exists(uniqueid, subset) && exists(uniqueid, superset)) {
    recerr <- !(subset[,uniqueid] %in% superset[,uniqueid])
    if(any(recerr)) {
      varerr <- sprintf("%s%s", uniqueid, subvar)
      errmsg <- sprintf("%s = %s%s", uniqueid, subset[recerr, uniqueid], subsettext)
      query <- data.frame(subset[recerr, id], tablename, varerr, "Logic", "Unexpected Record", errmsg, stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}
# badrecord <- function(uniqueid, subset=parent.frame(), superset=parent.frame(), subsettext="", id=patient) {
#   uniqueid <- deparse(substitute(uniqueid))
#   subvar <- unlist(strsplit(subsettext,"="))[1]
#   subvar <- gsub("!","",subvar)
#   if(exists(uniqueid,subset) & exists(uniqueid,superset)){
#     if(any(!(get(uniqueid,subset) %in% get(uniqueid,superset)))){
#       recerr <- !(get(uniqueid,subset) %in% get(uniqueid,superset))
#       	query <- data.frame(get(deparse(substitute(id)),subset)[recerr],
#       										tablename,
#       										ifelse(!is.na(subvar),paste(uniqueid,subvar,sep=""),uniqueid),
#       										"Logic",
#       										"Unexpected Record",
#       										paste(paste(uniqueid,"=",get(uniqueid,subset)[recerr]),subsettext,sep=""),
#       										stringsAsFactors=FALSE)
#       names(query) <- names(emptyquery)
#       assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
#     }
#   }
# }

## WRITE FUNCTION TO CHECK FOR RECORDS THAT SHOULD EXIST WHEN NOT PROPERLY INDICATED (eg, tblBAS)
missrecord <- function(uniqueid, subset, superset, subsettext="", table, tablename, id='patient', ...) {
  query <- NULL
  subvar <- gsub("!", "", sub("=.*$", "", subsettext))
  if(is.character(subset)) subset <- get(subset)
  if(is.character(superset)) superset <- get(superset)
  if(exists(uniqueid, subset) && exists(uniqueid, superset)) {
    recerr <- !(subset[,uniqueid] %in% superset[,uniqueid])
    if(any(recerr)) {
      varerr <- sprintf("%s%s", uniqueid, subvar)
      errmsg <- sprintf("%s = %s%s", uniqueid, subset[recerr, uniqueid], subsettext)
      query <- data.frame(subset[recerr, id], tablename, varerr, "Logic", "Missing Record", errmsg, stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}
# missrecord <- function(uniqueid,subset=parent.frame(),superset=parent.frame(),subsettext="",id=patient){
#   uniqueid <- deparse(substitute(uniqueid))
#   subvar <- unlist(strsplit(subsettext,"="))[1]
#   subvar <- gsub("!","",subvar)
#   if(exists(uniqueid,subset) & exists(uniqueid,superset)){
#     if(any(!(get(uniqueid,subset) %in% get(uniqueid,superset)))){
#       recerr <- !(get(uniqueid,subset) %in% get(uniqueid,superset))
#         query <- data.frame(get(deparse(substitute(id)),subset)[recerr],
#                                                                                 tablename,
#                                                                                 ifelse(!is.na(subvar),paste(uniqueid,subvar,sep=""),uniqueid),
#                                                                                 "Logic",
#                                                                                 "Missing Record",
#                                                                                 paste(paste(uniqueid,"=",get(uniqueid,subset)[recerr]),subsettext,sep=""),
#                                                                                 stringsAsFactors=FALSE)
#       names(query) <- names(emptyquery)
#       assign(paste("query",index,sep=""),query,envir=globalenv()); index <<- index + 1
#     }
#   }
# }

## QUERY IF VARIABLE IS MONOTONIC INCREASING/DECREASING
monotonic <- function(var, date, increasing=TRUE, table, tablename, id='patient') {
  query <- NULL
  if(exists(var,table)) {
    qv <- table[order(table[,id], table[,date]), c(id, var, date)]
    qv <- qv[!is.na(qv[,var]),]
    qv$delta <- unlist(tapply(qv[,var], qv[,id], FUN=function(x) c(0, diff(x))))
    if(increasing) {
      recerr <- which(qv$delta < 0)
    } else {
      recerr <- which(qv$delta > 0)
    }
    if(length(recerr)) {
      errmsg <- sprintf("%s=%s&%s=%s&%s=%s&%s=%s", date, qv[recerr-1, date], var, qv[recerr-1, var], date, qv[recerr, date], var, qv[recerr, var])
      query <- data.frame(qv[recerr, id], tablename, var, "Logic", "Out of Range", errmsg, stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}

## QUERY ANY EVENT THAT IS RECORDED TWICE, using DATE within DIST of each other
distance <- function(var, date, dist, table, tablename, id='patient') {
  query <- NULL
  if(exists(var, table) && exists(date, table)) {
    qv <- table[order(table[,id], table[,var], table[,date]), c(id, var, date)]
    qv <- qv[!is.na(qv[,var]) & !is.na(qv[,date]),]
    qv[,'grp'] <- paste(qv[,id], qv[,var], sep='-')
    qv$delta <- unlist(tapply(qv[,date], qv[,'grp'], FUN=function(x) c(NA, diff(x))))
    recerr <- which(!is.na(qv$delta) & qv$delta < as.numeric(dist))
    if(length(recerr)) {
      errmsg <- sprintf("%s=%s&%s=%s&%s=%s&%s=%s", date, qv[recerr-1, date], var, qv[recerr-1, var], date, qv[recerr, date], var, qv[recerr, var])
      query <- data.frame(qv[recerr, id], tablename, var, "Logic", "Out of Range", errmsg, stringsAsFactors=FALSE)
      names(query) <- names(emptyquery)
    }
  }
  query
}
