HC:= ghc
LIBS:=aeson req
SRC_PATHS:=src
FILES:=$(foreach PATH,$(SRC_PATHS), $(wildcard $(PATH)/*.hs)) 
PACKAGES:=$(foreach LIB,$(LIBS), -package $(LIB))
GHC_FLAGS:=$(PACKAGES) -outputdir bin -o cligmm

all:
	$(HC) $(FILES) $(GHC_FLAGS)