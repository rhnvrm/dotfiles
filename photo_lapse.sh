#!/bin/bash

#Take photo for photolapse

#Take photo
fswebcam -d /dev/video0 --jpeg 90 -p YUYV -F 2 /home/rhnvrm/test.jpg -r 1920x1080

#Move to webcam photo folder
mv /home/rhnvrm/test.jpg /home/rhnvrm/Pictures/Webcam/$(date +\%Y-\%m-\%d-\%H\%M\%S).jpg


