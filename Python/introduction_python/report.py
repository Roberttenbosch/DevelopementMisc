#!/usr/bin/python3


def get_desc():
    """Return random weather"""
    from random import choice
    poss = ["rain", "show", "sleet", "boe"]
    return choice(poss)