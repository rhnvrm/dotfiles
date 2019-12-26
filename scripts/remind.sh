echo 'reminder: ' $1
echo 'time:' $2
echo 'notify-send -u "critical" "'$1'"' | at $2
