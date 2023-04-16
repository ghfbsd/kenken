#!/bin/sh
## bash shell script to download Kenken puzzles from www.kenkenpuzzle.com
## and transcribe them into grid representation used in this package, or
## write out the puzzle's solution.

## Original:  1 Apr. 2023

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

## Parts of this derived from https://github.com/plohkoon/kenkensolver.
## Contributions acknowledged with thanks.

mode=${2:-parsed} tmp=/tmp/tmp$$
trap "/bin/rm -f $tmp" EXIT
usage() {
   [ $# -gt 0 ] && echo "$*" >&2
   echo "usage: $0 PUZZLE_ID { raw | cooked | parsed | solved }" >&2
   exit 1
}

decode() {
   # Strip JSON of everything except grid data and break into lines
   sed -e 's/"data":"//' -e 's/",//' -e 's/\\r\\n/\
/g'
}

getdat() {
   # Find grid data in JSON stream
   awk '/\"data\":\"[^\"]*\",/{
      if (match($0,/\"data\":\"[^\"]*\",/) > 0){
         print substr($0,RSTART,RLENGTH)
      }
   }' $tmp
}

translate() {
   # Translation of kenkenpuzzle.com grid to our own representation
   awk 'BEGIN{
         LETTERS="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         DOTS="........."
         ID="'"$1"'"
      }
      func err(txt){printf "%s (line %d)\n",txt,NR > "/dev/tty" ; exit 1}
      func pgrid(i,j,l){
         for(i=1;i<=gs;i++){
            l = ""; for(j=1;j<=gs;j++) l = l g[i,j]
            print l
         }
      }
      /[ATSVH]/{pt=$1; i=0}
      pt=="A" && /[0-9] [0-9 ]*/{
         ## Process answer part
         i += 1
         if (i == 1) {
            gs = NF
            for(j=1; j<=gs; j++){
               g[j,j] = "."
               for(k=j+1; k<=gs; k++){ g[j,k] = "."; g[k,j] = "."}
            }
         }
         if (i > 1 && NF != gs) err("**A inconsistent")
         ans = ans " " $0
      }
      pt=="T" && /[0-9][0-9 ]*/{
         ## Process op value part
         if (NF != gs) err("**T line does not have " gs " elements")
         if (i == 0) gn = 0
         i += 1;
         for(n=1;n<=NF;n++){
            if (0 != $(n)) {
               gn += 1; gi = substr(LETTERS,gn,1)
               gr[gi] = i; gc[gi] = n; gv[gi] = $(n)
            }
         }
      }
      pt=="S" && /[0+*\/1-] [0+*\/1- ]*/{
         ## Process op name part
         if (NF != gs) err("**S line does not have " gs " elements")
         if (i == 0) gn = 0
         i += 1;
         for(n=1;n<=NF;n++){
            k = $(n); if (k == "*") k = "x"
            if ("0" != k) {
               gn += 1; gi = substr(LETTERS,gn,1)
               if (gr[gi] != i || gc[gi] != n) err("**T/S mismatch line " i)
               if (k == "1") g[i,n] = gv[gi]; else g[i,n] = gi
               go[gi] = k
            }
         }
      }
      pt=="V" && /[01] [01 ]*/{
         ## Vertical bars separating cells in row i
         i += 1; if (NF != gs-1) err("**V line has " NF " entries, not " gs-1)
         for(n=1;n<=NF;n++){
            if ("0" == $(n)) g[i,n+1] = g[i,n]
            vv[i,n] = $(n)
         }
      }
      pt=="H" && /[01] [01 ]*/{
         ## Horizontal bars separating cells in col i
         i += 1; if (NF != gs-1) err("**H line has " NF " entries, not " gs-1)
         for(n=1;n<=NF;n++){
            if ("0" == $(n)) g[n+1,i] = g[n,i]
            hh[i,n] = $(n)
         }
      }
      END{
         ## Finish up cell allocation in grid for odd shapes
         any = 1
         while (any) {
            any = 0
            for(i=1;i<=gn;i++){
               for(j=1;j<=gn;j++){
                  if(g[i,j] == "." && j>1 && vv[i,j-1] == "0"){
                     g[i,j] = g[i,j-1]  # copy grid value from left
                  }
                  if(g[i,j] == "." && j<gn&& vv[i,j] == "0"){
                     # copy grid value from right, skipping unassigned cells
                     for(k=j+1;k<gn && g[i,k] == "." && vv[i,k] == "0";k++){
                        continue
                     }
                     g[i,j] = g[i,k]
                  }
                  if(g[i,j] == "." && i>1 && hh[j,i-1] == "0"){
                     g[i,j] = g[i-1,j]  # copy grid value from above
                  }
                  if (g[i,j] == ".") any = 1
               }
            }
         }
         ## Print out completed grid
         printf ".KK \"%dx%d grid %s\"\n",gs,gs,ID; pgrid()
         print ""
         for(i=1; i<=gn; i++){
            l = substr(LETTERS,i,1)
            if(go[l] != "1") printf "%s %s%s\n", l, gv[l], go[l]
         }
      }'
}

## Script actions begin here

[ $# -ge 1 ] || usage

ok=$(echo "$1" |
   awk 'BEGIN{res="BAD"} /[0-9][0-9]*/{res="OK"} END{print res}'
)

[ $ok = "OK" ] || usage "**Bad puzzle ID (should be a number)"

## Get authorization token from web site, save for future use
if [ ! -e ".auth.txt" ]; then
   curl -s -L -b .cookies.txt -c .cookies.txt "https://www.kenkenpuzzle.com/play_now" |
   sed -nE '/input.*authenticity_token/{s:.*auth.*value="([0-9a-zA-Z+/=]*)".*:\1:;p;q;}' \
   > .auth.txt
fi

## Download puzzle
curl -s -L -b .cookies.txt -c .cookies.txt \
   --data-urlencode "utf8=✓" \
   --data-urlencode "authenticity_token=$(cat .auth.txt)" \
   --data-urlencode "puzzle_id=$1" \
   --data-urlencode "x=0" \
   --data-urlencode "y=0" \
   "https://www.kenkenpuzzle.com/find_puzzle" |
  sed -nE '/base64/{s:.*'"'"'([0-9a-zA-Z+/=]*)'"'"'.*:\1:;p;q;}' |
  base64 -D > $tmp

## Process:

if [ "$mode" = "raw" ]; then
   ## raw is the raw JSON text from the web site
   cat $tmp

elif [ "$mode" = "cooked" ]; then
   ## cooked is stripped of JSON formatting
   getdat |
   decode

elif [ "$mode" = "solved" ]; then
   ## solved is the solution
   getdat |
   decode |
   awk '/[ASTHV]/{pt=$1}
      pt=="A" && /[1-9] [1-9 ]*/{print $0}'

elif [ "$mode" = "parsed" ]; then
   ## parsed is the puzzle grid in our format
   ID=$(awk '/\"id\":[0-9][0-9]*,/{
      if (match($0,/\"id\":[0-9]*/) > 0){
         print substr($0,RSTART+5,RLENGTH-5),"kenkenpuzzle.com"
      } else
         print "kenkenpuzzle.com"
   }' $tmp)

   getdat |
   decode |
   translate "${ID}"

else
   ## Anything else is an error
   usage "**Don't recognize arg \"${mode}\""
fi
