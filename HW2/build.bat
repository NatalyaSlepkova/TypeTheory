del hw2.exe
ocamlc hw22_unify.mli
ocamlc opal.ml hw22_unify.ml -o hw2.exe
@echo off
if exist hw2.exe (
    hw2.exe
    pause
) 