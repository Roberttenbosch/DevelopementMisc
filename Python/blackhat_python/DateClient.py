#!/usr/bin/python

import socket
import threading

host = "127.0.0.1"
port= 9034

client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

client.connect((host,port))
client.send("send")
i =0
while True:
    response = client.recv(4096)
    print response + " " + str(i)
    i = i + 1
    # client.close()
    # client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    # client.connect((host,port))
    # client.send("send this")

