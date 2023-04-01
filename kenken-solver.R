## Brute-force recursive solver for kenken puzzles.
##
## Copyright 2017-2023 George Helffrich

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

op  <- function(v) v[if(v[1]>v[2]) c(1,2) else c(2,1)] ## ordered pair
mod <- function(v){v[1] %%  v[2]}      ## apply() won't accept `%%`
idv <- function(v){v[1] %/% v[2]}      ## apply() won't accept `%/%`

div <- function(m){                    ## / op
   od <- apply(m,1,op)
   ov <- apply(od,2,idv)
   ov[ apply(od,2,mod) != 0 ] <- 0
   ov
}

sub <- function(m)abs(apply(m,1,diff)) ## - op

mul <- function(m)apply(m,1,prod)      ## x op

add <- function(m)apply(m,1,sum)       ## + op

board<-function(file,N=1) {
   ## Read in kenken description.  Returns list of groups with attribute 'grid'
   ## which represents the initial board state.
   ## grp list(key='name', n=n, row=c(...), col=c(...), op=x, opn=m, bag=NULL)
   ##   n - # squares
   ##   row[1..n], col[1..n] - row, col for square
   ##   op - '+', '-', 'x', '/' - operation
   ##   opn - operation result
   ##   bag - certain digits in this group derived from known grid values,
   ##         constraints from other groups in same row & column; unordered.
   txt<-readLines(file,n=-1)

   ## Extract number of individual puzzles.
   KK <- '.KK' == substr(txt,1,3)
   n <- sum(KK)
   if ((n == 0 && N != 1) || (n > 0 && n < N)) {
      m <- ifelse(n==0,1,n)
      stop(
         sprintf("**Can't locate puzzle %d in file (%d found)",N,m),
         call.=FALSE
      )
   }

   ## Extract desired puzzle (or the only one).
   ix <- cumsum(KK)
   if (n > 0) txt <- txt[which(ix == N)]

   ## ID is first arg on .KK macro or file name.
   ID <- sprintf('"%s"',file)
   if ('.KK' == substr(txt[1],1,3)) {
      ID <- regexpr('"[^"]*"',txt[1])
      if (ID > 0) ID <- substr(txt[1],ID,ID+attr(ID,'match.length')-1)
      txt <- txt[-1]
   }

   ## Remove extraneous punctuation and blank lines.
   nblnk <- nchar(txt) > 0
   for(i in 1:length(txt)) {               ## Tidy up text
      if (1 == regexpr('^[A-Za-z]  *[1-9][0-9]*[+x/-]',txt[i])) break
      txt[i] <- base::sub('#.*',"",txt[i]) ## Strip comments
      txt[i] <- gsub(' ',"",txt[i])        ## Delete blanks
     #txt[i] <- gsub('[^0-9]','_',txt[i])  ## Non-digits become underscores
      nblnk[i] <- nchar(txt[i]) > 0
   }
   txt<-txt[nblnk]                     ## Remove blank lines

   ## Parse grid layout description and check format.
   n <- nchar(txt[1])
   for(i in 1:n) if (nchar(txt[i]) != n) {
      cat(paste(
	ifelse(nchar(txt[i]) > n,'Too many','Not enough'),
	'characters in grid line',i),'\n')
      return(NA)
   }
   pat <- sprintf('[A-Za-z1-%d]{%d}',n,n)
   for(i in 1:n) if (1 != regexpr(pat,txt[i])) {
      cat(paste('Invalid character in grid line',i),'\n')
      return(NA)
   }

   grps <- list()
   attr(grps,'ID') <- ID
   m <- matrix(unlist(strsplit(txt[1:n],NULL)),n,n,byrow=TRUE)
   for(i in seq_along(txt)[-(1:n)]) {  ## Parse groups
      l <- regexpr(
         '[A-Za-z]  *(?<val>[1-9][0-9]*)(?<op>[+x/-])', txt[i], perl=TRUE
      )
      if (l != 1) {
         cat(sprintf('Group %d not recognized: "%s"',i-n,txt[i]),'\n')
         return(NA)
      }
      key <- substr(txt[i],1,1)
      val <- as.integer(substring(txt[i],
         attr(l,'capture.start')[,'val'],
         attr(l,'capture.start')[,'val']+attr(l,'capture.length')[,'val']-1
      ))
      op <- substring(txt[i],
         attr(l,'capture.start')[,'op'],
         attr(l,'capture.start')[,'op']
      )
      nk <- which(m == key, arr.ind=TRUE)
      if (nrow(nk) == 0) {
         cat(paste('No key called',key,'in grid'),'\n')
         return(NA)
      }
      grps[[i-n]] <- list(
         key=key,n=nrow(nk),row=nk[,'row'],col=nk[,'col'],op=op,opn=val
      )
   }
   grid <- matrix(NA,n,n)              ## Initial grid
   for(key in c('1','2','3','4','5','6','7','8','9')[1:n]){
      if(any(m == key)) {              ## Populate with fixed digits
         nk <- which(m == key, arr.ind=TRUE)
         grid[nk] <- as.integer(key)
      }
   }
   attr(grps,'grid') <- grid
   grps
}

gpbox <- function(grp,grid){           ## Return values in boxes for this group
   vapply(1:grp$n,function(i)grid[grp$row[i],grp$col[i]],1)
}

sel <- function(grp, grid, nn=1:nrow(grid), cnst=NULL){
   cmb <- list()
   for(i in 1:grp$n){
      if (!is.na(grid[grp$row[i],grp$col[i]])) {
         cmb[[i]] <- grid[grp$row[i],grp$col[i]]
      } else {
         rowv <- c( grid[grp$row[i],] , cnst$row[[grp$row[i]]] )
         colv <- c( grid[,grp$col[i]] , cnst$col[[grp$col[i]]] )
         cmb[[i]] <- if (!is.null(grp$bag))
            grp$bag  ## nn[ (nn %in% grp$bag) ]
         else
            nn[ !(nn %in% c(rowv,colv)) ]
         if (length(cmb[[i]]) == 0) return(matrix(nrow=0,ncol=grp$n))
      }
   }
   tmp <- as.matrix(expand.grid(cmb, KEEP.OUT.ATTRS = FALSE))

   ## Now winnow choices using constraints in each row/col
   free <- is.na(gpbox(grp,grid))
   mask <- rep(FALSE,nrow(tmp))
   for(row in unique(grp$row)) mask <- mask |
      apply(
         tmp[,free & grp$row == row, drop=FALSE],
         1,
         function(v)any(v %in% cnst$row[[row]] & !(v %in% grp$bag))
      )
   for(col in unique(grp$col)) mask <- mask |
      apply(
         tmp[,free & grp$col == col, drop=FALSE],
         1,
         function(v)any(v %in% cnst$col[[col]] & !(v %in% grp$bag))
      )
   tmp[!mask,,drop=FALSE]
}

genfill <- function(grp, grid, cnst=NULL){
   choices <- sel(grp, grid, cnst=cnst)
   if (!nrow(choices)) return(matrix(nrow=0,ncol=grp$n))
   chk <- switch(grp$op,                          ## these fit op constraint
      '+' = add(choices)
   ,
      '-' = sub(choices)
   ,
      'x' = mul(choices)
   ,
      '/' = div(choices) 
   ) == grp$opn

   ## now do selection consistency checks: no numbers in same row or col
   comb <- combn(grp$n,2)                         ## pairwise combinations
   rowc <- matrix(grp$row[comb],nrow=2)           ## row #s of each pair
   colc <- matrix(grp$col[comb],nrow=2)           ## col #s of each pair
   rows <- apply(rowc,2,function(v)v[1]==v[2])    ## these combs have same row
   cols <- apply(colc,2,function(v)v[1]==v[2])    ## these combs have same col
   dups <- rep(FALSE,length(chk))
   for(i in (1:ncol(rowc))[rows]) dups <- dups |  ## duplicate in same row
      (choices[,comb[1,i]] == choices[,comb[2,i]])
   for(i in (1:ncol(colc))[cols]) dups <- dups |  ## duplicate in same col
      (choices[,comb[1,i]] == choices[,comb[2,i]])

   choices[chk & !dups,,drop=FALSE]               ## these are OK
}

ksolve <- function(file,N=1,trace=FALSE){
   bd <- board(file,N)                ## Read board
   if (is.na(bd[1])) stop("**Bad puzzle description",call.=FALSE)
   gr <- attr(bd,'grid')              ## Get initial grid
   ix <- sort(                        ## Order groups into largest first
      vapply(seq_along(bd),function(i)bd[[i]]$n,1),
      dec=TRUE,
      index=TRUE
   )$ix
   n <<- 0

   cat(sprintf("Solving %s...\n",attr(bd,'ID')))
   ok <-recurse(bd,gr,ix,trace)       ## Attempt solution
   if (ok){                           ## Print out solution
      cat(sprintf("Solution after %d positions examined: \n",n))
      print(attr(ok,'grid'))
   } else
      cat(sprintf("*** No solution after %d tries for puzzle in %s.\n",n,file))
}

## Recursive backtrack algorithm.  Called via:
##
##    ok <- recurse(board, grid, gix)
##
## with:
##
##    board -- Board layout (static).  This is a list of lists, one
##             list element for each group of boxes yielding the
##             arithmetic retuls.  Each box group has
##             key= ID of box from board tableau (A-Z)
##             n= number of boxes in group
##             row[1:n]=, col[1:n]= row and col of each box
##             op= character operation (+, -, x, /)
##             opn= value of operation
##    grid  -- nxn matrix of grid entries (NA if not yet set)
##    gix   -- m(<=n) - vector of group index of choices being tried
##
## Function result is TRUE or FALSE.  The return value if TRUE has the
## attribute 'grid' which gives the solved board position.

recurse <- function(board, grid, gix, dbg=FALSE){
   n <<- n + 1
   gp <- board[[ gix[1] ]]            ## Get group for current group index
   choices <- genfill(gp,grid)        ## Generate choices based on current grid
   ok <- nrow(choices) > 0
   if (ok) {
      for(i in 1:nrow(choices)){      ## For each choice ...
         gnew <- grid
         for(k in 1:ncol(choices)){   ## ... fill the grid
            gnew[gp$row[k],gp$col[k]] <- choices[i,k]
         }
         if (dbg) {
            print(gnew)
            cat(sprintf("Try group %d, choice %d (pos. %d):\n",gix[1],i,n))
         }
         if (length(gix) <= 1) break
         ok <- recurse(board, gnew, gix[-1], dbg)
         if (ok) break                ## If a viable (or last) choice, use it
         ## Otherwise, try next choice until all exhausted.
      }
      if (ok && length(gix) <= 1) attr(ok,'grid') <- gnew
   }
   ok
}
