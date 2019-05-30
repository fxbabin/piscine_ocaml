
NAME			:=			doctor

SRC_DIR			:=			./

SRC_MLI			:=			

SRCS 			:=			people.ml		\
							doctor.ml		\
							main.ml



PATHMLI				:=			$(addprefix $(SRC_DIR)/,$(SRC_MLI))
PATHML				:=			$(addprefix $(SRC_DIR)/,$(SRCS))



BO				:=			$(addprefix $(SRC_DIR)/,$(SRCS:.ml=.o))
BI				:=			$(addprefix $(SRC_DIR)/,$(SRCS:.ml=.cmi))
BX				:=			$(addprefix $(SRC_DIR)/,$(SRCS:.ml=.cmx))

R			=	\033[31m
G			=	\033[32m
B			=	\033[34m
W			=	\033[0m
O			=	\033[33m


all: $(NAME)

$(NAME) : $(PATHMLI) $(PATHML)
	@ocamlfind opt $(PATHMLI) $(PATHML) -o $(NAME) -I $(SRC_DIR)
	@printf '\033[40m %s\n\033[0m$(W)' "           Compilation of $(NAME) is done.          "
	@printf "\n"

clean:
	@rm -f $(BO) $(BI) $(BX)
	@printf '\033[40m %s\n\033[0m$(W)' "           Cleaning all the bloat file is done.     "
	@printf "\n"

fclean: clean
	@rm -f $(NAME)
	@printf '\033[40m %s\n\033[0m$(W)' "           Removing the binary is done.             "
	@printf "\n"

re: fclean all

.PHONY: all clean fclean re

