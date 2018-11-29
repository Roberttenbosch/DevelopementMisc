#!/usr/bin/python3
import sys

print("Program arguments", sys.argv)

for place in sys.path:
    print(place)