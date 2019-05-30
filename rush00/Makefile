NAME = Tictactoe

SOURCEDIR = src/
SOURCE = Board.mli Cell.mli Ai.mli Game.mli Board.ml Cell.ml Ai.ml Game.ml Tictactoe.ml
SOURCE := $(addprefix $(SOURCEDIR), $(SOURCE))
CAMLBUILD = ocamlbuild

LIBS = $(WITHGRAPHICS)
WITHGRAPHICS = -package Graphics

all: $(NAME)

$(NAME): opt byt
	ln -fs $(NAME).opt $(NAME)

$(NAME).opt: $(SOURCE)
	$(CAMLBUILD) -I $(SOURCEDIR) $(NAME).native $(LIBS)
	mv $(NAME).native $(NAME).opt

$(NAME).byt: $(SOURCE)
	$(CAMLBUILD) -I $(SOURCEDIR) $(NAME).byte $(LIBS)
	mv $(NAME).byte $(NAME).byt

opt: $(NAME).opt
byt: $(NAME).byt

clean:
	$(CAMLBUILD) -clean
	rm -f $(NAME)

re:
	$(MAKE) clean
	$(MAKE) all

.PHONY: all clean fclean re
