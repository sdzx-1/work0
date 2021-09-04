# ubuntu build
## install libray
sudo apt install graphviz  
sudo apt install libsdl2-dev   
sudo apt install libsdl2-gfx-dev  
sudo apt install libsdl2-ttf-dev
## build
cabal build  
# run example0
cabal run work0 -- work/example0
# run example1
cabal run work0 -- work/example0