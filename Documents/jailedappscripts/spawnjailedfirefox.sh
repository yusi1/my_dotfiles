#!/bin/bash

termname="Alacritty"
term="alacritty"
appname="Firefox"
execapp="firefox"
dnsname="FDNS"
dns="127.1.1.1"
customclass="sandboxed"
class="$termname,$customclass"

notify-send "Started Running Private Jailed $appname Process (DNS = $dnsname $dns)"

$term --class $class -e fdns --monitor & disown
$term --class $class -e firejail --private --dns=$dns /usr/bin/$execapp

notify-send "Finished Running Private Jailed $appname Process"
