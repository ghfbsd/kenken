## Logic based solver for kenken puzzles.
##
## Copyright 2024-2024 George Helffrich

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
   # result.
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
   ID <- sprintf('%s',file)
   if ('.KK' == substr(txt[1],1,3)) {
      ID <- regexpr('"[^"]*"',txt[1])
      if (ID > 0) ID <- substr(txt[1],ID+1,ID+attr(ID,'match.length')-2)
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
      ix <- sort(nk[,'row'],index=TRUE)$ix
      grps[[i-n]] <- list(
         key=key,n=nrow(nk),row=nk[ix,'row'],col=nk[ix,'col'],op=op,opn=val
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
   attr(grps,'map') <- m
   grps
}

pgr <- function(n,map=NA,gps=NA,grid=NA,wait=FALSE){
   # Print graphical description of the grid.
   # n - size of grid
   # map - grid map (nxn matrix of characters)
   # gps - the board layout, a list of grp elements
   # grid - the number of possibilities at each point on the grid
   #    grid[i,j,k] 1 <= i,j <=n; 2 <= k <= n+1.
   #       grid[i,j,1] is number of possibilities,
   #       grid[i,j,2:(1+grid[i,j,1]) are what numbers are possible.

   # Coordinate system: (1,1) on top left, (n,n) on bottom right (like a matrix)

   ## grp list(key='name', n=n, row=c(...), col=c(...), op=x, opn=m, bag=NULL)
   ##   n - # squares
   ##   row[1..n], col[1..n] - row, col for square
   ##   op - '+', '-', 'x', '/' - operation
   ##   opn - operation result
   ##   bag - certain digits in this group derived from known grid values,
   ##         constraints from other groups in same row & column; unordered.

   plot(NA,NA,                            # define board
      bty='n', xaxt='n', yaxt='n',
      xlim=c(0,1), ylim=c(0,1),
      xlab='', ylab='',main=if(exists('ID')) ID else ''
   )

   for(l in 0:n){                         # draw grid lines
      lwd <- ifelse(l %in% c(0,n), 5, 1)
      lines(rep(l/n,2),c(0,1),lwd=lwd)
      lines(c(0,1),rep(l/n,2),lwd=lwd)
   }

   if (!is.na(map[1])) for(i in 1:n){     # draw group boundaries
      for(j in 1:n){
         if (i > 1 && map[i-1,j] != map[i,j])
            lines((j-c(0,1))/n,rep((n-i+1)/n,2),lwd=3)
         if (j > 1 && map[i,j-1] != map[i,j])
            lines(rep((j-1)/n,2),(n-i+c(0,1))/n,lwd=3)
      }
   }

   if (!is.na(gps[1])) for(i in 1:n){     # draw group ops
      for(j in 1:n){
         org <- c(j-1,n-i+1)/n + c(0.01,-0.01)
         lbl <- if (!is.na(map[1]) && map[i,j] %in% 1:9)
            text(org[1], org[2], map[i,j], adj=c(0,1))
         else for(k in seq_along(gps)) {
            if (gps[[k]]$row[1] != i || gps[[k]]$col[1] != j) next
            text(org[1], org[2], sprintf("%s%s",gps[[k]]$opn,gps[[k]]$op),
               adj=c(0,1)
            )
         }
      }
   }

   for(i in 1:n-1){                       # possible values
      for(j in 1:n-1){
         ctr <- c((j+0.5)/n, 1 - (i+0.5)/n)
         poss <- if (!is.na(grid[1]))
            grid[i+1,j+1,1+1:grid[i+1,j+1,1]]
         else
            1:n
         if (length(poss) == 1)           # only one - that's solved
            text(ctr[1], ctr[2], poss[1], adj=c(0.5,0.5), cex=3.8)
         else for(k in 1:n){              # max n possibilities, show if present
            ang <- 2*pi*(k-1)/n 
            r <- 0.55/(2*n)
            if (k %in% poss) text(ctr[1] + r*cos(ang), ctr[2] + r*sin(ang), k,
               adj=c(0.5,0.5), cex=0.80
            )
         }
      }
   }
   
   if (wait) readline('waiting...')
}

igrid <- function(grid){
   ## Load initial grid with values
   ##   grid - grid state

   ## Returns (modified) state of grid that has all values possible
   ## except those that have a fixed initial digit.

   n <- dim(grid)[1]
   stopifnot(all(dim(grid) == n))
   new <- array(0L,dim=c(n,n,n+1))
   for(i in 1:n)for(j in 1:n) new[i,j,] <- c(n,1:n)
   ix <- which(!is.na(grid),arr=TRUE)
   for(i in seq_along(ix[,1])){
      new[ix[i,1],ix[i,2],1] <- 1L
      new[ix[i,1],ix[i,2],2] <- grid[ix[i,1],ix[i,2]]
   }
   new
}

grpname <- function(grp, board) {
   ## Give a unique name to a group
   ##   grp -    group to name
   ##   board -  the description of the grid (see board description above)

   ## Returns character string describing the group uniquely

   dups <- unlist(lapply(board,function(g)grp$op == g$op && grp$opn == g$opn))
   sprintf("%s%s group%s",grp$opn,grp$op,
      ifelse(sum(dups) > 1,sprintf(" at (%s,%s)",grp$row[1],grp$col[1]),"")
   )
}

winnow <- function(grp,choices) {
   # Winnows group given choices for numbers in each cell of the group.
   #   Only those which satisfy the arithmetic constraint survive.
   # grp - group under examination
   # choices - (m x n) matrix of possible combinations of values in the group
   #   m - number of combinations
   #   n - number of cells in the group

   # Returns a shrunken choices array with any row deleted which has an
   #   infeasible combination of digit assigments to the cells in the group,
   #   because either:
   #   1) it doesn't obey the arithmetic constraint, or
   #   2) that it replicates digits in the row(s) or column(s) of the group.

   chk <- switch(grp$op,                       ## these fit op constraint
      '+' = add(choices)
   ,
      '-' = sub(choices)
   ,
      'x' = mul(choices)
   ,
      '/' = div(choices) 
   ) == grp$opn


   ## now do selection consistency checks: no numbers in same row or col
   comb <- combn(grp$n,2)                      ## pairwise combinations
   rowc <- matrix(grp$row[comb],nrow=2)        ## row #s of each pair
   colc <- matrix(grp$col[comb],nrow=2)        ## col #s of each pair
   rows <- apply(rowc,2,function(v)v[1]==v[2]) ## these combs have same row
   cols <- apply(colc,2,function(v)v[1]==v[2]) ## these combs have same col
   dups <- rep(FALSE,length(chk))
   for(i in (1:ncol(rowc))[rows]) dups <- dups |  ## duplicate in same row
      (choices[,comb[1,i]] == choices[,comb[2,i]])
   for(i in (1:ncol(colc))[cols]) dups <- dups |  ## duplicate in same col
      (choices[,comb[1,i]] == choices[,comb[2,i]])

   choices[chk & !dups,,drop=FALSE]            ## these are OK
}

sel <- function(st,...) {
   ## Select which combinations make sense for the grid in its present state
   ##   st -  state of grid
   ##   ... - 1 or more groups to consider given the grid state

   ## Returns an updates grid state (may or may not be different from original)
   ##   given the arithmetical constraints in each group, and the interactions
   ##   between the other groups.

   n <- dim(st)[1]

   rown <- coln <- 1:n
   rowd <- cold <- integer(0)
   for(k in 1:...length()) {                   ## preliminary checks
      grp <- ...elt(k)
      rown <- intersect(rown,grp$row)          ## a pair: shared rows or columns
      coln <- intersect(coln,grp$col)
      rowd <- union(rowd,                      ## >=3: form transitive closure
         grp$row[grp$row %in% unlist(lapply(list(...) %$% 'row',unique)[-k])]
      )
      cold <- union(cold,                      ## >=3: form transitive closure
         grp$col[grp$col %in% unlist(lapply(list(...) %$% 'col',unique)[-k])]
      )
   }

   if (length(rown) == 0 &&                    ## can't if no intersection
       length(coln) == 0 &&            
       ...length() <= 2) return(st)            ## ...for a pair
   if (length(rowd) == 0 &&                    ## futile if no overlap at all
       length(cold) == 0 &&
       ...length() > 2) return(st)             ## ...among 3 or more

   clist <- list()
   for(k in 1:...length()) {                   ## start the heavy work
      grp <- ...elt(k)
      m <- list()
      for(i in 1:grp$n) m[[i]] <-              ## extract possibilities
         getij(st,grp$row[i],grp$col[i])
      choices <- as.matrix(expand.grid(m))

      if (!nrow(choices)) return(st)           ## no possibilities, no change

      clist <- c(clist,list(
         winnow(grp, choices)                  ## these are OK for the group
      ))
   }

   if (...length() > 1) {                      ## winnow further

      if (...length() > 2) {
         rown <- rowd                          ## these become rows/cols of
         coln <- cold                          ## interest
      }

      ## The ses array has a cell for each of the combinations of choices in
      ##   (k1 for clist[[1]], k2 for clist[[2]], ... k[n] for clist[[n]].
      ##   The cell is TRUE if the choices are compatible.  The code is
      ##   somewhat ponderous due to the need to generalize it to n possible
      ##   choices (where n <= 4 at present).  Conceptually, it is like doing
      ##   outer(1:k1, 1:k2, function(i,j){calculate answer for cell(i,j)})
      ##   but for up to 4 dimensions as opposed to 2 for outer().

      ngp <- ...length()
      gpn <- vapply(clist,nrow,1L)
      gp1n <- lapply(1:ngp,function(i)1:gpn[i])
      gijall <- expand.grid(gp1n)
      ses <- array(TRUE,dim=gpn)
      for (row in rown) {                      ## this does ses() for dims > 2
         glst <- lapply(1:ngp,function(i)...elt(i)$row == row)
         for(i in 1:nrow(gijall)) {
            inij <- FALSE
            q <-                               ## start with first cell(s)
               clist[[1]][gijall[i,1],][glst[[1]]]
            for(j in 2:ngp){                   ## any overlap with these cell(s)
               qij <- clist[[j]][gijall[i,j],][glst[[j]]]
               inij <- inij | any(q %in% qij)  ## overlap check
               q <- union(q, qij)              ## round 'em up
            }
            ij <-                              ## index in an array of ? dims
               matrix(unlist(gijall[i,]),nrow=1)
            ses[ij] <- ses[ij] & !inij         ## the answer...
         }
      }
      if (!all(ses)) {                         ## any duds from row constraints?
         ok <- which(ses,arr=TRUE)             ## accumulate digits in each
         for(k in 1:ngp){                      ## cell for successful combs
            grp <- ...elt(k)
            for(i in 1:grp$n){
               poss <- unique(clist[[k]][ok[,k],i])
               st[grp$row[i],grp$col[i],] <-   ## that's the cell's possibles
                  c(length(poss),poss,rep(0L,n-length(poss)))
            }
         }
      }
       
      ses <- array(TRUE,dim=gpn)
      for (col in coln) {                      ## this does ses() for dims > 2
         glst <- lapply(1:ngp,function(i)...elt(i)$col == col)
         for(i in 1:nrow(gijall)) {
            inij <- FALSE
            q <-                               ## start with first cell(s)
               clist[[1]][gijall[i,1],][glst[[1]]]
            for(j in 2:ngp){                   ## any overlap with these cell(s)
               qij <- clist[[j]][gijall[i,j],][glst[[j]]]
               inij <- inij | any(q %in% qij)  ## overlap check
               q <- union(q, qij)              ## round 'em up
            }
            ij <- matrix(unlist(gijall[i,]),nrow=1)
            ses[ij] <- ses[ij] & !inij         ## the answer...
         }
      }
      if (!all(ses)) {                         ## any duds from col constraints?
         ok <- which(ses,arr=TRUE)             ## accumulate digits in each
         for(k in 1:ngp){                      ## cell for successful combs
            grp <- ...elt(k)
            for(i in 1:grp$n){
               poss <- unique(clist[[k]][ok[,k],i])
               st[grp$row[i],grp$col[i],] <-   ## that's the cell's possibles
                  c(length(poss),poss,rep(0L,n-length(poss)))
            }
         }
      }
   } else {                                    ## simple case of just one group
      grp <- ...elt(1)
      choices <- clist[[1]]
      for(i in 1:grp$n) {
         poss <- unique(choices[,i])
         st[grp$row[i],grp$col[i],] <-         ## those are the cell's possibles
            c(length(poss),poss,rep(0L,n-length(poss)))
      }
   }
   st
}

rmvij <- function(st,ir,jc,q) {
   ## Remove set items in a grid cell
   ##   st - state of grid
   ##   ir - row
   ##   jc - column
   ##   q  - set of items to remove

   ## Returns updated state of grid

   stopifnot(typeof(st) == 'integer')
   n <- dim(st)[1]
   nn <- st[ir,jc,1]
   res <- setdiff(st[ir,jc,1+1:nn],q)
   m <- length(res)
   if (m<=0) {cat('***BUG*** (0 choices) '); browser()}
   st[ir,jc,] <- c(m,res,rep(0L,n-m))
   st
}

getij <- function(st,ir,jc) {
   ## Get set of items in a grid cell
   ##   st - state of grid
   ##   ir - row
   ##   jc - column

   ## Returns vector of possibilities in the cell

   stopifnot(typeof(st) == 'integer')
   st[ir,jc,1+1:st[ir,jc,1]]
}

allij <- function(st,ir,jc) {
   ## Get set items from one or more grid cells
   ##   st - state of grid
   ##   ir - row (may be vector)
   ##   jc - column (may be vector)

   ## Returns list of vectors of possibilities for each cell

   stopifnot(length(ir) == length(jc))
   lapply(seq_along(ir),function(i)st[ir[i],jc[i],1+1:st[ir[i],jc[i],1]])
}

numij <- function(st,ir,jc) {
   ## Get the number of possibilities in a grid cell
   ##   st - state of grid
   ##   ir - row
   ##   jc - column

   ## Returns a number (1 <= i <= n)

   stopifnot(typeof(st) == 'integer')
   st[ir,jc,1]
}

numst <- function(st) {
   ## Get the number of possibilities in all the cells of the grid
   ##   st - state of grid

   ## Returns an nxn matrix of numbers, one number for each cell

   st[,,1]
}

numgr <- function(grp,st) {
   ## Get the number of possibilities in each of the cells of a group
   ##   grp - group (list; see board description)
   ##   st  - state of grid

   ## Returns a vector containing a number for each cell in the group

   vapply(1:grp$n, function(i) numij(st,grp$row[i],grp$col[i]), 0L)
}

allgi <- function(bd,ir) {
   ## Which groups are only in a row?
   ##   bd - board (list of groups; see board description)
   ##   ir - row

   ## Returns a vector, one element for each group, saying if all the cells
   ##    are in row ir

   vapply(bd, function(grp) all(grp$row == ir), TRUE)
}

anygi <- function(bd,ir) {
   ## Which groups are in a row?
   ##   bd - board (list of groups; see board description)
   ##   ir - row

   ## Returns a vector, one element for each group, saying if any cell
   ##   is in row ir

   vapply(bd, function(grp) any(grp$row == ir), TRUE)
}

allgj <- function(bd,jc) {
   ## Which groups are only in a column?
   ##   bd - board (list of groups; see board description)
   ##   jc - column

   ## Returns a vector, one element for each group, saying if all the cells
   ##   are in column jc

   vapply(bd, function(grp) all(grp$col == jc), TRUE)
}

anygj <- function(bd,jc) {
   ## Which groups are in a column?
   ##   bd - board (list of groups; see board description)
   ##   jc - column

   ## Returns a vector, one element for each group, saying if any cell
   ##   is in column jc

   vapply(bd, function(grp) any(grp$col == jc), TRUE)
}

`%$%` <- function(grp, key) {
   ## Gets key= value from a list of lists
   ##   implements the binary operator L %$% 'key'
   ##   where L is a list or lists which you want the element 'key' from

   ## Returns a vector of length(L) with the value for key

   sapply(grp, `[[`, as.character(as.list(match.call()$key)))
}

gotu <- function(st,ij) {
   ## Cell ij has a single value in it.
   ## Eliminate it from all of the same row & col in grid
   ##   st - grid state
   ##   ij - vector of length 2 having row & col index of cell

   ## Returns updated grid state

   n <- dim(st)[1]                       ## that's the size

   q <- st[ij[1],ij[2],2]                ## that's the value

   qin <- function(grp){q %in% grp}
   row <- apply(st[ij[1],-ij[2],1+1:n],1,qin)
   if (any(row))
      for(ic in (1:n)[-ij[2]][row]) st <- rmvij(st,ij[1],ic,q)
   col <- apply(st[-ij[1],ij[2],1+1:n],1,qin)
   if (any(col))
      for(ir in (1:n)[-ij[1]][col]) st <- rmvij(st,ir,ij[2],q)
   st
}

update <- function(new,old,bd,why=NA,wait=FALSE,q='') {
   ## Update - update new grid state from old
   ##   new  - new grid state array
   ##   old  - old grid state array
   ##   bd   - board layout list
   ##   wait - whether to display new grid state
   ##          if NA, just display the solution (answer mode)
   ##          if TRUE stop before each change (tutorial mode)
   ##          if FALSE progressively show each change to the grid (movie mode)

   ## If prompt made, any non-blank response will quit processing

   stopifnot(typeof(new) == 'integer')

   if (all(new[,,1] == old[,,1]))      ## if nothing changes, no grid redrawing
      return(new)

   .N. <<- .N. + 1                     ## count steps
   if (is.na(wait)) {
      cat('\rStep',.N.)
      return(new)
   }
   if (!is.na(why) && wait)            ## write out reason for change to grid
      q <- readline(paste(why,'...'))  ##    wait for go-ahead
   else
      if (!is.na(why)) cat(why,'\n')   ## otherwise, just say why and go on
   if (q != '')                        ## stops if any non-blank response
      stop('Quit',call.=FALSE)

   m <- attr(bd,'map')
   pgr(dim(m)[1],m,bd,new)             ## draw new grid
   new
}

ksolve <- function(file,N=1,trc=TRUE) {
   ## file  - file name with grid description
   ## N     - which grid in file to solve (first is default)
   ## trc   - controls whether there is a pause before making each
   ##         solution step after writing an explanation for it

   ## With this solution algorithm, the number of steps required to solve an
   ## nxn grid goes according to the formula,
   ##
   ##  log(steps) = 1.7164 + 0.3925 * n
   ##
   ## e.g., solving a 4x4 grid takes around 20 steps
   ##               a 6x6 grid takes around 60 steps
   ##               a 9x9 grid takes around 180 steps


   bd <- board(file,N)                    ## Read board
   if (is.na(bd[1]))
      stop("**Bad puzzle description",call.=FALSE)
   if (is.na(trc))                        ## Report what we're solving
      cat(sprintf('Solving "%s"...\n',attr(bd,'ID')))
   gr <- attr(bd,'grid')                  ## Get initial grid
   m <- attr(bd,'map')                    ## get grid map
   environment(pgr)$ID <- attr(bd,'ID')   ## get grid ID
   st <- igrid(gr)                        ## load initial state
   n <- dim(m)[1]

   if (!is.na(trc)) pgr(n,m,bd,st)        ## display initial grid layout

   .N. <<- 0
   repeat {                               ## cycle through solution steps
      chg <- numst(st)

      # ***Rule 1*** Lone digits
      ix <- which(st[,,1] == 1, arr=TRUE) ## Winnow based on solved squares
      if (nrow(ix)>0) for(i in 1:nrow(ix)) {
         ij <- ix[i,]
         nw <- gotu(st,ij)
         st <- update(nw,st,bd,
            why=sprintf("know %s is at (%s,%s)",st[ij[1],ij[2],2],ij[1],ij[2]),
            wait=trc
         )
         next                             ## Restart if square solved
      }

      # ***Rule 2*** Arithmetical constraints
      for(grp in bd) {                    ## Winnow based on numerical ops
         nw <- sel(st,grp)
         st <- update(nw,st,bd, why=grpname(grp,bd),wait=trc)
      }
      if (any(numst(st) - chg != 0)) next ## Re-start if something changed

      jtest <- function(ij,col=NA) {      ## Check column for identical pairs
         if (!all(st[ij,col,1] == length(ij))) return(FALSE)
         q <- getij(st,ij[1],col)
         res <- vapply(
            2:length(ij),
            function(i)setequal(q, getij(st,ij[i],col)),
            TRUE
         )
         all(res)
      }

      itest <- function(ij,row=NA) {      ## Check row for identical pairs
         if(!all(st[row,ij,1] == length(ij))) return(FALSE)
         q <- getij(st,row,ij[1])
         res <- vapply(
            2:length(ij),
            function(i)setequal(q, getij(st,row,ij[i])),
            TRUE
         )
         all(res)
      }

      # ***Rule 3*** Naked tuples
      for(tuple in 2:(n-1)){              ## Check for repeated pairs, triples,
         pairs <- combn(n,tuple)          ##   ... in a row or col and reduce
         for(i in 1:n) {                  ## Reduce tuples in a row or column
            same <- apply(pairs,2,jtest,col=i)
            if (any(same)) {              ## Examine column i
               old <- st
               ij <- pairs[,same]
               q <- getij(st,ij[1],i)
               for(ir in setdiff(1:n,pairs[,same])) st <- rmvij(st,ir,i,q)
               update(st,old,bd,
                  why=sprintf('removing (%s) from col %s',paste(q,collapse=' '),i),
                  wait=trc
               )
            }
            same <- apply(pairs,2,itest,row=i)
            if (any(same)) {              ## Examine row i
               old <- st
               ij <- pairs[,same]
               q <- getij(st,i,ij[1])
               for(jc in setdiff(1:n,pairs[,same])) st <- rmvij(st,i,jc,q)
               update(st,old,bd,
                  why=sprintf('removing (%s) from row %s',paste(q,collapse=' '),i),
                  wait=trc
               )
            }
         }
         if (any(numst(st)-chg != 0)) break
      }
      if (any(numst(st)-chg != 0)) next

      # ***Rule 4*** Lonely digits
      chkr <- function(ir,q)
         vapply(1:n,function(i) q %in% getij(st,ir,i), TRUE)
      chkc <- function(jc,q)
         vapply(1:n,function(i) q %in% getij(st,i,jc), TRUE)

      for(digit in 1:n) {                 ## Search for only possible digit loc
         for(ir in 1:n) {                 ## Search for loner in each row
            got <- chkr(ir,digit)
            if (sum(got) == 1) {
               jc <- (1:n)[got]
               new <- rmvij(st,ir,jc,as.integer(setdiff(getij(st,ir,jc),digit)))
               why <- sprintf('only %s in row %s is in col %s',digit,ir,jc)
               st <- update(new,st,bd,why=why,wait=trc)
            }
         }

         for(jc in 1:n) {                 ## Search for loner in each col
            got <- chkc(jc,digit)
            if (sum(got) == 1) {
               ir <- (1:n)[got]
               new <- rmvij(st,ir,jc,as.integer(setdiff(getij(st,ir,jc),digit)))
               why <- sprintf('only %s in col %s is in row %s',digit,jc,ir)
               st <- update(new,st,bd,why=why,wait=trc)
            }
         }
      }

      if (any(numst(st)-chg != 0)) next

      if (all(numst(st) == 1)) break      ## might be done now

      # ***Rule 5*** Pair interactions
      gpr <- combn(1:length(bd),2)        ## group - group pairwise interaction
      for(i in 1:length(gpr[1,])){
         gp1 <- bd[[gpr[1,i]]]
         gp2 <- bd[[gpr[2,i]]]
         rint <- intersect(gp1$row,gp2$row)
         cint <- intersect(gp1$col,gp2$col)
         if (length(rint)>0 || length(cint)>0) {
            if (all(numgr(gp1,st) == 1) ||   ## skip solved groups
                all(numgr(gp2,st) == 1)) next
            new <- sel(st,gp1,gp2)
            why <- sprintf("%s and %s interactions",
               grpname(gp1,bd), grpname(gp2,bd)
            )
            if (any(chg - numst(new) != 0)) break
         }
      }
      st <- update(new,st,bd, why=why, wait=trc)

      if (any(numst(st)-chg != 0)) next

      # ***Rule 6*** Triplet interactions
      gpr <- combn(1:length(bd),3)        ## group triplet interaction
      for(i in 1:length(gpr[1,])){        ## some heuristics could help here
         gp1 <- bd[[gpr[1,i]]]
         gp2 <- bd[[gpr[2,i]]]
         gp3 <- bd[[gpr[3,i]]]
         new <- sel(st,gp1,gp2,gp3)
         why <- sprintf("%s, %s and %s interactions",
            grpname(gp1,bd), grpname(gp2,bd), grpname(gp3,bd)
         )
         if (any(chg - numst(new) != 0)) break
      }
      st <- update(new,st,bd, why=why, wait=trc)

      if (any(numst(st)-chg != 0)) next

      # ***Rule 7*** Quadruplet interactions
      gpr <- combn(1:length(bd),4)        ## this is getting ridiculous, but
      for(i in 1:length(gpr[1,])){        ## I've seen one 6x6 that needs this
         gp1 <- bd[[gpr[1,i]]]
         gp2 <- bd[[gpr[2,i]]]
         gp3 <- bd[[gpr[3,i]]]
         gp4 <- bd[[gpr[4,i]]]
         new <- sel(st,gp1,gp2,gp3,gp4)
         why <- sprintf("%s, %s, %s and %s interactions",
            grpname(gp1,bd), grpname(gp2,bd), grpname(gp3,bd), grpname(gp4,bd)
         )
         if (any(chg - numst(new) != 0)) break
      }
      st <- update(new,st,bd, why=why, wait=trc)

      if (all(numst(st)-chg == 0)) break
   }

   if (all(numst(st) == 1)) {
      cat("\nSolved in",.N.,"steps\n")
      if (is.na(trc)) print(st[,,2])
   }
   if (any(numst(st) != 1)) cat('\n***STUCK!*** after',.N.,'steps\n')
}