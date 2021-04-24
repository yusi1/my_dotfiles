#!/bin/bash

execapp="spotify"
dnsname="fdns"
dns="127.1.1.1"
cmd="firejail --dns=$dns /usr/bin/$execapp"


if [[ a=$(procs $dnsname) != *"$dnsname"* ]]; then
    #notify-send "launching $dnsname daemon...";
    pkexec sudo fdns --daemonize; sleep 5;
    $cmd
else
    #notify-send "$dnsname process exists, launching $execapp without relaunching $dnsname"
    $cmd
fi
