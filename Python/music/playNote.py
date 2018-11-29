#!/usr/bin/python3
from music import legacy

C4 = 60
QN = 1.0
note = legacy.Note(C4, QN)
legacy.Play.midi(note)