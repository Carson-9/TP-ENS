ocamlopt main.ml -o main
rm -rf *.cmx
rm -rf *.cmi
echo -e "\n"
./main
echo -e "\n"