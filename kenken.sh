#!/bin/sh
## kenken -- describe grid and turn it into a printable form using groff
##    One or more grids on standard input; groff output on std. output; use
##    -me -t to process, e.g. kenken < grid.in | groff -me -t.  Use .KK to add
##    a title or to print multiple grids in the same printed output.
## Original:  6 Oct. 2017; Last update: 25 Mar. 2023

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

## Grids of any size up to 9x9 can be printed; they only depend on the size of
## the tableau made of the letters A-Z and 1-9.  Examples describing various
## grids:
##   .KK "First one (6x6)"
##   A1BBBC
##   ADDEEC
##   ADFFGH
##   AIIIGH
##   JJKKKL
##   JMMKLL
##   
##   A 48x
##   B 15+
##   C 2/
##   D 30x
##   E 7+
##   F 1-
##   G 4-
##   H 9+
##   I 9+
##   J 14+
##   K 17+
##   L 4+
##   M 1-

##   .KK "Second one (4x4)"
##   XXYY
##   2BYW
##   ABBW
##   ACCW
##   X 3-
##   Y 6+
##   B 9+
##   W 8+
##   A 1-
##   C 1-

awk 'BEGIN{n=0; pre=1; bs="\\h`-0.5n`"; su="\\s-4\\u"; sd="\\d\\s0"
   title="This is a kenken grid."
}
func ch(i,j){ return substr(line[i],j,1) }
func pch(i,j){
   if(chp[ch(i,j)] == "" || ch(i,j) ~ ("^" pat "$")){
      chp[ch(i,j)] = ch(i,j)
      if (j==1) return bs su key[ch(i,j)] sd; else return su key[ch(i,j)] sd
   }
   return ""
}
func err(msg){ printf "**%s (line %d)\n",msg,NR > "/dev/tty"; exit 1 }
NF==1 && /^[0-9A-Za-z][0-9A-Za-z]*$/{ ## input grid - n characters per line
   gl=length($1)
   if (n && gs!=gl) err(sprintf("Mixing %dx%d and %dx%d grid!",gs,gl))
   if (!n) {gs = gl; if(gl>9) err("Max grid size is 9x9")
      pat = "["; for(i=1;i<=gl;i++) pat=pat i; pat=pat "]";
      gap = substr("::::::::::",1,gl-1)
   }
   n+=1; line[n] = $0
}
func proc(){
   if (n!=gs) err(sprintf("Need %d lines, got %d",gs,n))

   ## Check that all keys defined and define single digit keys
   for(i=1;i<=gs;i++){
      for(j=1;j<=gs;j++){
         if(key[ch(i,j)]==""){
	    if (ch(i,j) ~ pat) key[ch(i,j)] = ch(i,j)
	    else err("missing op for " ch(i,j))
	 }
      }
   }

   ## Print out preamble
   if (pre){
      print ".nr pp 16"
      print ".lp"
      print ".ad l"
      pre = 0
   }
   print ".nr g% \\n%"
   print ".mk gT"
   print ".TS"
   print "doublebox,tab(:);"

   ## Print out key lines
   for(i=1;i<=gs;i++){
      str="lw(2v)e"; dup="l"
      for(j=1;j<=gs-1;j++){
         str = str "1|"; dup = dup "|"
         if (ch(i,j)!=ch(i,j+1)){str = str "|"; dup = dup "|"}
	 str = str "le"; dup = dup "l"
      }
      if (i==gs) dup = dup "."
      print str; print dup
      if (i<gs){
	 str = ""
         for(j=1;j<=gs;j++){
	    if (ch(i,j) == ch(i+1,j)) str = str "_"; else str = str "="
	    if (j<gs && ch(i,j) != ch(i,j+1)) str = str "||"; else str = str " "
	 }
	 print str
      }
   }

   ## Print out data lines
   str = bs su key[ch(1,1)] sd
   for(j=2;j<=gs;j++){
      if (ch(1,j-1) == ch(1,j)) str = str ":"; else str = str ":" pch(1,j)
   }
   print str; print gap
   for(i=2;i<=gs;i++){
      if (ch(i,1) == ch(i-1,1)) str = ":"; else str = pch(i,1) ":"
      for(j=2;j<=gs;j++){
         if (ch(i,j-1) == ch(i,j)) str = str ":"
	 else if (ch(i,j) == ch(i,j-1)) str = str ":"
	 else if (ch(i,j) == ch(i-1,j)) str = str ":"
	 else str = str pch(i,j) ":"
      }
      print str; print gap
   }

   ## Print out trailer
   wid = 0.7*gs
   print ".TE"
#  print "\\u\\s-8\\(co2017-2024 G. Helffrich\\s0\\d"
#  print ".br"
   print ".sp"
   print ".mk gB"
   print ".ie \\n%=\\n(g% .sp |\\n(gTu"  ## On same page? Move back
   print ".el .sp |\\n(tmu"              ## Otherwise move to page top
   printf ".in %.1fi\n%s\n",wid,title
   print ".in 0"
   print ".sp |\\n(gBu"
}
NF==2 && $1 != ".KK"{
   got=0;for(i=1;i<=n && !got;i++)for(j=1;j<=gs && !got;j++)got = $1 == ch(i,j)
   if(!got) err($1 " is not in the table")
   if(key[$1]!="") err($1 " is already defined")
   if($2 ~ /\/$/)key[$1] = substr($2,1,length($2)-1) "\\(di"     ## divide sym
   else if($2 ~ /x$/)key[$1] = substr($2,1,length($2)-1) "\\(mu" ## multiply sym
   else if($2 ~ /[+-]$/) key[$1]=$2 
   else err($2 " is an invalid expression")
}
$1 == ".KK"{
   if (n>0) proc()
   if (NF>1) {
      str = $2; for(i=3;i<=NF;i++) str = str " " $(i)
      if (str ~ /^".*"$/) str = substr(str,2,length(str)-2)
   } else
      str = "This is a kenken grid."
   title = str; n = 0; delete chp; delete key
}
END{
   if (n>0) proc();
}'
