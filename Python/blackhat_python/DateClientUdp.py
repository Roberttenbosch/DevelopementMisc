#!/bin/python

import socket
import threading

host = "127.0.0.1"
port= 4950

client = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

#client.connect((host,port))
client.sendto("bpe",(host,port))
data, addr = client.recvfrom(4096)

print data
print addr
