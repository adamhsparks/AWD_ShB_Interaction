#!/bin/bash
#
###########################################################
# Usage: imgDetails.sh
#
# Script will save to ./imgDetails_(PID).html
# with the details of images it came across in the current
# directory which the script was run.
# Taken from: http://hints.macworld.com/article.php?story=20041220171133228
############################################################
#
declare -i COUNT=0
declare -i SUCCESS=0
declare -r OUT=./imgDetails.html
declare -ar ALLOWED=(*.jpg *.JPG *.jpeg *.GIF *.gif *.png *.PNG)

rm imgDetails.html

for ITEM in ${ALLOWED[@]}; do
  if [ -f $ITEM ]; then
    declare -i POS=0
    declare -a PROPS=()
    for PROP in $($CMD "$ITEM"|tail -2|sed 's/ //g'|awk -F':' '{print $2}')
    do
      echo $PROP | egrep '[0-9]+'>/dev/null 2>&1
      if [ $? == 0 ]; then
        PROPS[$POS]=$PROP
        POS=POS+1
      fi
    done
    if [ -n ${PROPS[0]} -a -n ${PROPS[1]} ]; then
      echo "<img src=\"${ITEM}\" alt=\"${ITEM}\" />" | tee -a $OUT
      [ $? == 0 ] && SUCCESS=SUCCESS+1
    fi
    COUNT=COUNT+1
  fi
done

echo -e "\nAttempted to process (${COUNT}) files."
echo -e "Successfully processed (${SUCCESS}) files."

[ -f $OUT ]

exit 0
