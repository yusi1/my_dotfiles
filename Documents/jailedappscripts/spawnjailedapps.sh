#!/bin/bash

termname="Alacritty"
term="alacritty"
dnsname="FDNS"
dns="127.1.1.1"
customclass="sandboxed"
class="$termname,$customclass"

echo "Enter Application To Launch DNS=$dns:"
read execapp

appname=$execapp

notify-send "Started Running Private Jailed $appname Process (DNS = $dnsname $dns)"

#if [[ $(procs | grep "fdns") == () ]]; then
#b=$(fdns --monitor & disown)
#fi

#$term --class $class -e $b 
#$term --class $class -e firejail --dns=$dns /usr/bin/$execapp
firejail --dns=$dns /usr/bin/$execapp
#$term --class $class -e firejail --private --dns=$dns /usr/bin/$execapp

notify-send "Finished Running Private Jailed $appname Process"

exit
