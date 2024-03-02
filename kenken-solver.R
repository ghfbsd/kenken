## Brute-force recursive solver for kenken puzzles.
##
## Copyright 2017-2024 George Helffrich

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

div <- function(m){                    ## / op

   # This function redefines itself after calculating look-up table for
   # unordered integer division.  It uses the lookup table to speed up the
   # result, slightly (by ~0.6%).
   uid <- function(m,n){               ## unordered integer division
      mn <- m %/% n; mn[ m%%n != 0 | m <  n] <- 0L # result when m >= n or 0
      nm <- n %/% m; nm[ n%%m != 0 | n <= m] <- 0L # result when m <  n or 0
      mn + nm
   }

   DIV <- outer(1:9,1:9,uid)           ## defines lookup table
   div <<- function(m)                 ## (re)defines function
      apply(m,1,function(m)DIV[m[1],m[2]])
   environment(div)$DIV <-             ## saves compact lookup table
      matrix(DIV,9,9)
   apply(m,1,                          ## gives first answer
      function(m)DIV[m[1],m[2]]
   )
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

ksolve <- function(file,N=1,trc=FALSE,odo=TRUE,rev=FALSE,all=FALSE){
   ## file  - file name with grid description
   ## N     - which grid in file to solve (first is default)
   ## trc   - controls dump of grid backtracking (voluminous)
   ## odo   - controls odometer display
   ## rev   - controls whether biggest groups in grid solved first (TRUE) or
   ##         last (FALSE).  Last seems to be the best choice on average based
   ##         on solutions of 7x7 grids.
   ## all   - find ALL solutions, not just first

   bd <- board(file,N)                 ## Read board
   if (is.na(bd[1])) stop("**Bad puzzle description",call.=FALSE)
   gr <- attr(bd,'grid')               ## Get initial grid
   ix <- sort(                         ## Order groups largest/smallest
      vapply(seq_along(bd),function(i)bd[[i]]$n,1),
      dec=rev,                         ## Smallest group first seems faster
      index=TRUE
   )$ix

   ## Set up call count and odometer display if requested (and not tracing)
   n <<- 0
   if (is.na(trc)) trc <- FALSE
   odo <- c(ifelse(odo & !trc,1,0),length(ix))
   rsol <<- list()

   cat(sprintf("Solving %s...\n",attr(bd,'ID')))
   ok <- recurse(bd,gr,ix,trc,odo,all) ## Attempt solution
   if (ok || (all && length(rsol)>0)){ ## Print out solution
      odo <- ifelse(odo[1]>0,'\n','')
      cat(sprintf("%sSolution after %d positions examined: \n",odo,n))
      for(g in rsol) print(g)
   } else
      cat(sprintf("*** No solution after %d tries for puzzle in %s.\n",
                  n,file))
}

## Recursive backtrack algorithm.  Called via:
##
##    ok <- recurse(board, grid, gix, trace, odo, all)
##
## with:
##
##    board -- Board layout (static).  This is a list of lists, one
##             list element for each group of boxes yielding the
##             arithmetic result.  Each box group has
##             key= ID of box from board tableau (A-Z)
##             n= number of boxes in group
##             row[1:n]=, col[1:n]= row and col of each box
##             op= character operation (+, -, x, /)
##             opn= value of operation
##    grid  -- nxn matrix of grid entries (NA if not yet set)
##    gix   -- m(<=n) - vector of group index of choices being tried
##    dbg   -- logical whether to display grid at each stage
##    odo   -- odometer display of progress towards grid solution
##    all   -- find all solutions, not just the first encountered
##
## Function result is TRUE or FALSE.  If the return value is TRUE,
## the solution(s) are in the global list var "rsol".

recurse <- function(board, grid, gix, dbg, odo=NA, all=NA){
   n <<- n + 1
   if (odo[1]>0){                     ## Odometer display
      level <- paste(strrep(">",odo[1]),strrep("-",diff(odo)),'|',sep='')
      cat(sprintf("%s %d\r",level,n))
   }
   gp <- board[[ gix[1] ]]            ## Get group for current group index
   choices <- genfill(gp,grid)        ## Generate choices based on current grid
   ok <- nrow(choices) > 0
   if (ok) {
      for(i in 1:nrow(choices)) {     ## For each choice ...
         gnew <- grid
         for(k in 1:ncol(choices)) {  ## ... fill the grid
            gnew[gp$row[k],gp$col[k]] <- choices[i,k]
         }
         if (dbg) {                   ## Print board if tracing
            print(gnew)
            cat(sprintf("Try group %d, choice %d (pos. %d):\n",gix[1],i,n))
         }
         if (length(gix) <= 1) {      ## Found a solution; add to list
            rsol[[length(rsol)+1]] <<- gnew
            break
         }
         ok <- recurse(board, gnew, gix[-1], dbg, odo+c(1,0), all)
         if (ok && !all) break
         ## Otherwise, try next choice until all exhausted.
      }
      if (all && length(gix) <= 1) ok <- length(rsol) > 0
   }
   ok
}
