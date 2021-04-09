#!/bin/bash

execapp="betterdiscord"
dnsname="fdns"
dns="127.1.1.1"
cmd="firejail --profile=electron --dns=$dns /usr/bin/$execapp"


if [[ a=$(procs $dnsname) != *"$dnsname"* ]]; then
    #notify-send "launching $dnsname daemon...";
    pkexec sudo fdns --daemonize & disown; sleep 5;
    $cmd
else
    #notify-send "$dnsname process exists, launching $execapp without relaunching $dnsname"
    $cmd
fi
