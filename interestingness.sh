#!/bin/bash
DIR="/Users/kunjeong/Docs/Development/Research/PLRG/p4cherry"
$DIR/p4spectec covers $DIR/spec/*.watsup -pid 0 -pid 769 -pid 771 -p ./program.p4
